"""
Enhanced redistricting analysis with partisan optimization capabilities.
This script provides functions for both neutral redistricting and 
partisan gerrymandering using GerryChain's optimization methods.
"""

import os
import geopandas as gpd
import pandas as pd
import numpy as np
import random
from gerrychain import (GeographicPartition, Partition, Graph, MarkovChain,
                       proposals, updaters, constraints, accept, Election)
from gerrychain.updaters import Tally, cut_edges
from gerrychain.tree import recursive_tree_part, recursive_seed_part, bipartition_tree
from gerrychain.constraints import contiguous
from gerrychain.proposals import recom
from gerrychain.metrics import efficiency_gap, mean_median, partisan_bias, partisan_gini
from gerrychain.optimization import SingleMetricOptimizer
from functools import partial
import matplotlib.pyplot as plt


def run_redistricting_analysis(
    shapefile_path, 
    output_dir, 
    num_steps=1000, 
    pop_deviation=0.05, 
    target_pop=None, 
    num_districts=None
  ):
    
    """
    Run neutral redistricting analysis using GerryChain.
    
    Parameters:
    -----------
    shapefile_path : str
        Path to the precinct shapefile
    output_dir : str
        Directory to save output files
    num_steps : int
        Number of steps for the Markov chain
    pop_deviation : float
        Population deviation allowed (as percentage)
    target_pop : float, optional
        Target population for each district. If None, calculated from data.
    num_districts : int, optional
        Number of districts to create
    """
    # Create output directory if it doesn't exist
    os.makedirs(output_dir, exist_ok=True)
    
    # Load shapefile
    print(f"Loading shapefile from {shapefile_path}")
    gdf = gpd.read_file(shapefile_path)
    
    # Check for required columns
    required_columns = ["pct_id", "pop", "pct_min", "dem_vsh"]
    missing_columns = [col for col in required_columns if col not in gdf.columns]
    
    if missing_columns:
        error_msg = f"Error: The following required columns are missing: {', '.join(missing_columns)}"
        print(error_msg)
        raise ValueError(error_msg)
    
    # Ensure the GeoDataFrame has a unique index
    gdf = gdf.reset_index(drop=True)
    
    # Rename variables to standard names used in the script
    column_mapping = {
        "pct_id": "precinct_id", 
        "pop": "population", 
        "pct_min": "per_minority", 
        "dem_vsh": "dem_voteshare"
    }
    
    gdf = gdf.rename(columns=column_mapping)
    
    # Calculate additional columns if needed
    if "per_minority" in gdf.columns and "population" in gdf.columns:
        gdf['bvap'] = np.round(gdf['per_minority'] * gdf['population']).astype(int)
        gdf['wvap'] = gdf['population'] - gdf['bvap']
    
    if "dem_voteshare" in gdf.columns:
        gdf['rep_voteshare'] = 1 - gdf['dem_voteshare']
    
    # Create graph from GeoDataFrame
    print("Creating graph from GeoDataFrame")
    graph = Graph.from_geodataframe(gdf)
    
    # Add population data to graph
    for node in graph.nodes():
        graph.nodes[node]["population"] = gdf.loc[node, "population"]
        if "dem_voteshare" in gdf.columns:
            graph.nodes[node]['dem_voteshare'] = gdf.loc[node, 'dem_voteshare']
            graph.nodes[node]['rep_voteshare'] = gdf.loc[node, 'rep_voteshare']
    
    # Calculate total population
    total_population = sum(gdf['population'])
    
    # Determine number of districts and ideal population
    if num_districts is not None:
        num_districts = int(num_districts)
        ideal_population = total_population / num_districts
    elif target_pop is not None:
        num_districts = int(round(total_population / target_pop))
        ideal_population = total_population / num_districts
    else:
        raise ValueError("Either num_districts or target_pop must be specified")
    
    print(f"Total population: {total_population}")
    print(f"Number of districts: {num_districts}")
    print(f"Ideal population per district: {ideal_population}")
    
    # Set up Election
    election = Election("ELECTION", {"Dem": "dem_voteshare", "Rep": 'rep_voteshare'})
    
    # Set up updaters
    my_updaters = {
        'population': Tally('population'),
        'cut_edges': cut_edges,
        'ELECTION': election
    }
    
    # Create initial partition
    initial_partition = create_initial_partition(graph, num_districts, ideal_population, 
                                               pop_deviation, my_updaters)
    
    # Print initial district populations
    print("\nInitial district populations:")
    for district in sorted(initial_partition["population"].keys()):
        pop = initial_partition["population"][district]
        deviation = (pop - ideal_population) / ideal_population * 100
        print(f"District {district}: {pop:,.0f} (deviation: {deviation:+.1f}%)")
    
    # Write initial district assignment
    gdf['init_CD'] = [initial_partition.assignment[node] for node in graph.nodes()]
    
    # Set up constraints
    pop_constraint = constraints.within_percent_of_ideal_population(initial_partition, pop_deviation)
    compactness_bound = constraints.UpperBound(
        lambda p: len(p["cut_edges"]),
        2*len(initial_partition["cut_edges"])
    )
    
    # Set up proposal function
    proposal = partial(recom,
                      pop_col="population",
                      pop_target=ideal_population,
                      epsilon=pop_deviation,
                      node_repeats=5,
                      method=partial(bipartition_tree,
                                   max_attempts=5000,
                                   allow_pair_reselection=True,
                                   warn_attempts=500))
    
    # Set up the chain
    print(f"\nSetting up Markov chain with {num_steps} steps")
    chain = MarkovChain(
        proposal=proposal,
        constraints=[
            constraints.contiguous,
            pop_constraint,
            compactness_bound
        ],
        accept=accept.always_accept,
        initial_state=initial_partition,
        total_steps=num_steps
    )
    
    # Run the chain
    print("Running Markov chain")
    metrics = []
    CD_assignments = []
    
    try:
        for i, partition in enumerate(chain):
            if i % 100 == 0:
                print(f"Step {i}")
            
            metrics.append({
                "step": i,
                "cut_edges": len(partition["cut_edges"]),
                "efficiency_gap": efficiency_gap(partition["ELECTION"]),
                "mean_median": mean_median(partition["ELECTION"]),
                "dem_seats": partition["ELECTION"].wins("Dem"),
                "rep_seats": partition["ELECTION"].wins("Rep")
            })
            
            CD_assignments.append({
                'step': i,
                'district_id': [partition.assignment[node] for node in graph.nodes()]
            })
    except KeyboardInterrupt:
        print("\nChain interrupted by user. Saving progress...")
    except Exception as e:
        print(f"\nError during chain execution: {e}")
        print("Saving progress up to this point...")
    
    # Save results
    save_neutral_results(metrics, CD_assignments, gdf, output_dir)
    
    return {
        "num_districts": num_districts,
        "ideal_population": ideal_population,
        "steps_completed": len(CD_assignments)
    }

def create_initial_partition(graph, num_districts, ideal_population, epsilon, updaters):
    """
    Create an initial partition using various methods.
    """
    initial_partition = None
    
    # First attempt with recursive_seed_part
    try:
        # print("Attempting to create initial partition using recursive_seed_part...")
        initial_assignment = recursive_seed_part(
            graph,
            parts=range(num_districts),
            pop_target=ideal_population,
            pop_col="population",
            epsilon=epsilon,
            node_repeats=10,
            method=partial(bipartition_tree, 
                          max_attempts=10000,
                          allow_pair_reselection=True,
                          warn_attempts=1000)
        )
        
        initial_partition = Partition(
            graph,
            assignment=initial_assignment,
            updaters=updaters
        )
        # print("Successfully created initial partition using recursive_seed_part")
        
    except Exception as e:
        # print(f"recursive_seed_part failed: {e}")
        # print("Falling back to from_random_assignment...")
        
        # Try with progressively larger epsilon values
        epsilon_values = [epsilon, epsilon * 1.5, epsilon * 2, 0.25, 0.30, 0.40]
        
        for eps in epsilon_values:
            try:
                # print(f"Trying from_random_assignment with epsilon={eps:.3f}")
                initial_partition = Partition.from_random_assignment(
                    graph, 
                    n_parts=num_districts, 
                    epsilon=eps,
                    pop_col="population",
                    updaters=updaters
                )
                # print(f"Successfully created initial partition with epsilon={eps:.3f}")
                break
            except Exception as e:
                # print(f"Failed with epsilon={eps:.3f}: {e}")
                continue
    
    if initial_partition is None:
        raise RuntimeError("Could not create initial partition with any method")
    
    return initial_partition


def save_neutral_results(metrics, CD_assignments, gdf, output_dir):
    """
    Save results from neutral redistricting analysis.
    """
    if metrics:
        metrics_df = pd.DataFrame(metrics)
        metrics_df.to_csv(os.path.join(output_dir, "metrics.csv"), index=False)
        print(f"Saved metrics for {len(metrics)} steps")
    
    # Save district assignments
    num_steps_completed = len(CD_assignments)
    if num_steps_completed > 0:
        df_assignments = pd.DataFrame(CD_assignments)
        num_units = len(df_assignments['district_id'].iloc[0])
        all_assignments = np.zeros((num_units, num_steps_completed), dtype=int)
        
        for i, row in enumerate(df_assignments['district_id']):
            all_assignments[:, i] = row
        
        df_pivoted = pd.DataFrame(all_assignments, columns=[f'step_{i+1}' for i in range(num_steps_completed)])
        df_pivoted['precinct_id'] = gdf['precinct_id']
        df_pivoted['init_CD'] = gdf['init_CD']
        
        plan_output_path = os.path.join(output_dir, "CD_plans.csv")
        df_pivoted.to_csv(plan_output_path, index=False)
        print(f"Wrote CD plans to {plan_output_path}")
    
    print("\nAnalysis complete!")


def create_biased_ensemble(
    shapefile_path,
    output_dir,
    ensemble_size=10,
    bias_type="republican",
    pop_deviation=0.05,
    num_districts=None,
    # --- Burst schedule (explicit) ---
    dev_mode=False,
    burst_length=250,          # steps per burst
    num_bursts=20,             # total bursts
    patience_bursts=8,         # early stop after this many bursts w/o improvement
    # --- Geometry speed knob ---
    simplify_tolerance=None,   # e.g., 0.0005 CRS units; None = off
    # --- Scoring knobs ---
    use_soft_score=True,       # preferred name (kept)
    use_soft_scoring=None,     # optional alias; if provided, overrides use_soft_score
    soft_k=60.0                # logistic steepness for soft-seat score
):
    """
    Build a partisan-biased ensemble using GerryChain short_bursts with explicit controls:
      - ensemble_size: how many maps to output
      - num_bursts:    how many iterations of short_bursts
      - burst_length:  steps per burst
      - patience_bursts: early stop after N consecutive bursts without improvement

    Notes:
      * Early stopping is measured in BURSTS (not steps).
      * Planned steps = num_bursts * burst_length.
    """

    # ---- soft-score flag alias handling ----
    if use_soft_scoring is not None:
        use_soft_score = bool(use_soft_scoring)

    os.makedirs(output_dir, exist_ok=True)

    # -------- Load shapefile & normalize columns --------
    gdf = gpd.read_file(shapefile_path)
    if simplify_tolerance is not None:
        try:
            gdf["geometry"] = gdf.geometry.simplify(simplify_tolerance, preserve_topology=True)
        except Exception:
            pass

    rename_map = {}
    if "pct_id" in gdf.columns and "precinct_id" not in gdf.columns:
        rename_map["pct_id"] = "precinct_id"
    if "pop" in gdf.columns and "population" not in gdf.columns:
        rename_map["pop"] = "population"
    gdf = gdf.rename(columns=rename_map)

    required = {"precinct_id", "population", "dem_v", "rep_v"}
    missing = [c for c in required if c not in gdf.columns]
    assert not missing, f"Shapefile missing required columns: {missing}"

    # -------- Build graph --------
    graph = Graph.from_geodataframe(gdf)
    for node in graph.nodes():
        graph.nodes[node]["population"] = int(gdf.iloc[node]["population"])
        graph.nodes[node]["dem_v"]      = int(gdf.iloc[node]["dem_v"])
        graph.nodes[node]["rep_v"]      = int(gdf.iloc[node]["rep_v"])

    if num_districts is None:
        raise ValueError("num_districts must be provided")

    total_pop = int(gdf["population"].sum())
    ideal_population = total_pop / num_districts

    # Updaters (vote COUNTS)
    E0 = Election("E0", {"D": "dem_v", "R": "rep_v"})
    updaters = {"population": Tally("population"), "cut_edges": cut_edges, "E0": E0}

    # RECOM proposal
    node_repeats = 1 if dev_mode else 3
    max_attempts = 800 if dev_mode else 5000
    proposal = partial(
        recom,
        pop_col="population",
        pop_target=ideal_population,
        epsilon=pop_deviation,
        node_repeats=node_repeats,
        method=partial(
            bipartition_tree,
            max_attempts=max_attempts,
            allow_pair_reselection=True,
            warn_attempts=max_attempts // 2,
        ),
    )

    # Simple seat counters
    def seats_D(p): return p["E0"].wins("D")
    def seats_R(p): return p["E0"].wins("R")

    # Soft surrogate for seat count (logistic around 50%)
    def soft_seat_score(part, party="democratic", k=soft_k):
        shares_D = np.array(part["E0"].percents("D"))
        if party == "democratic":
            s = 1.0 / (1.0 + np.exp(-k * (shares_D - 0.5)))
            tie = 0.01 * (shares_D.mean() - 0.5)
        else:
            shares_R = 1.0 - shares_D
            s = 1.0 / (1.0 + np.exp(-k * (shares_R - 0.5)))
            tie = 0.01 * (shares_R.mean() - 0.5)
        return float(s.sum() + tie)

    # Party-specific objective
    if use_soft_score:
        if bias_type == "democratic":
            metric_func = lambda p: soft_seat_score(p, "democratic", soft_k)
        else:
            metric_func = lambda p: soft_seat_score(p, "republican", soft_k)
        metric_name = f"soft_seats({bias_type[:3]})"
    else:
        metric_func = seats_D if bias_type == "democratic" else seats_R
        metric_name = f"raw_{'D' if bias_type=='democratic' else 'R'}_seats"

    planned_steps = int(num_bursts) * int(burst_length)
    print(f"[Optimizer] bias={bias_type} | metric={metric_name} | dev_mode={dev_mode} | eps={pop_deviation:.3f}")
    print(f"[Schedule] num_bursts={num_bursts}, burst_length={burst_length}, "
          f"patience_bursts={patience_bursts}, planned_steps={planned_steps})")

    # -------- Helper: seed a valid initial plan --------
    def seed_initial_valid(max_tries=25):
        for _ in range(max_tries):
            try:
                assign = recursive_seed_part(
                    graph,
                    parts=range(num_districts),
                    pop_target=ideal_population,
                    pop_col="population",
                    epsilon=pop_deviation,
                    node_repeats=10,
                    method=partial(
                        bipartition_tree,
                        max_attempts=max_attempts * 2,
                        allow_pair_reselection=True,
                        warn_attempts=max_attempts
                    ),
                )
                part = Partition(graph, assignment=assign, updaters=updaters)
                pc = constraints.within_percent_of_ideal_population(part, pop_deviation)
                if pc(part):
                    return part
            except Exception:
                pass
        for _ in range(max_tries):
            try:
                part = Partition.from_random_assignment(
                    graph, n_parts=num_districts, epsilon=pop_deviation,
                    pop_col="population", updaters=updaters
                )
                pc = constraints.within_percent_of_ideal_population(part, pop_deviation)
                if pc(part):
                    return part
            except Exception:
                pass
        raise RuntimeError("Could not seed a valid initial partition within population bounds.")

    # -------- Storage --------
    plan_assignments = []
    metrics_records  = []

    print(f"Creating ensemble of {ensemble_size} {bias_type} maps...")

    # -------- Build each biased map --------
    for i in range(int(ensemble_size)):
        if i % 10 == 0 or i == ensemble_size - 1:
            print(f"Progress: {i+1}/{ensemble_size} maps completed")

        random.seed(i)
        np.random.seed(i)

        initial = seed_initial_valid()
        print(f"bias_type={bias_type} | init seats -> D={seats_D(initial)}, R={seats_R(initial)}")

        # Constraints
        pop_constraint      = constraints.within_percent_of_ideal_population(initial, pop_deviation)
        compactness_bound   = constraints.UpperBound(lambda p: len(p["cut_edges"]), 3 * len(initial["cut_edges"]))
        contiguity_required = constraints.contiguous
        assert pop_constraint(initial), "Initial partition violates population bounds for optimizer."

        optimizer = SingleMetricOptimizer(
            proposal=proposal,
            constraints=[contiguity_required, pop_constraint, compactness_bound],
            initial_state=initial,
            optimization_metric=metric_func,
            maximize=True
        )

        # ----- Run short_bursts with burst-based patience -----
        best_at_prev_boundary = -1e18
        bursts_without_improve = 0
        step_total = 0
        burst_idx = 0

        try:
            for part in optimizer.short_bursts(burst_length=int(burst_length), num_bursts=int(num_bursts)):
                step_total += 1

                # periodic logging
                if step_total == 1 or step_total % 250 == 0 or step_total % int(burst_length) == 0:
                    print(f"  step {step_total:5d}: best={optimizer.best_score:.3f} | "
                          f"curr D={seats_D(part)}, R={seats_R(part)}")

                # burst boundary reached?
                if step_total % int(burst_length) == 0:
                    burst_idx += 1
                    improved = optimizer.best_score > best_at_prev_boundary + 1e-12
                    best_at_prev_boundary = optimizer.best_score
                    if improved:
                        bursts_without_improve = 0
                    else:
                        bursts_without_improve += 1
                    print(f"  [Burst {burst_idx}/{num_bursts}] best={optimizer.best_score:.3f} "
                          f"| no-improve-bursts={bursts_without_improve}")

                    if patience_bursts is not None and bursts_without_improve >= int(patience_bursts):
                        print("  Early stop: patience (bursts) reached.")
                        break
        except Exception as e:
            print(f"  WARNING: chain error, using best-so-far (map {i}): {e}")

        # choose the best partition observed
        best_part = getattr(optimizer, "best_part", initial)

        # Record plan + metrics
        plan_assignments.append([best_part.assignment[n] for n in graph.nodes()])
        metrics_records.append({
            "map_idx": i,
            "bias_type": bias_type,
            "dem_seats": seats_D(best_part),
            "rep_seats": seats_R(best_part),
            "optimization_score": float(getattr(optimizer, "best_score", np.nan)),
            "efficiency_gap": float(efficiency_gap(best_part["E0"])),
            "mean_median": float(mean_median(best_part["E0"]))
        })

    # -------- Write outputs --------
    if plan_assignments:
        plans_df = pd.DataFrame(plan_assignments).T
        plans_df.columns = [f"{bias_type}_map_{i}" for i in range(len(plan_assignments))]
        plans_df["precinct_id"] = gdf["precinct_id"].values
        plans_df = plans_df[["precinct_id"] + [c for c in plans_df.columns if c != "precinct_id"]]
        plans_df.to_csv(os.path.join(output_dir, "CD_plans.csv"), index=False)

        metrics_df = pd.DataFrame(metrics_records)
        metrics_df.to_csv(os.path.join(output_dir, "metrics.csv"), index=False)

        print(
            f"Completed {bias_type} ensemble: "
            f"best_score={metrics_df['optimization_score'].max():.3f}, "
            f"mean_score={metrics_df['optimization_score'].mean():.3f}"
        )
        return plans_df, metrics_df

    return None, None






# Make sure all functions are accessible when imported
__all__ = [
  'run_redistricting_analysis',
  'save_neutral_results',
  'create_biased_ensemble',
  'create_initial_partition',
]
