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


def run_redistricting_analysis(shapefile_path, output_dir, num_steps=1000, 
                              pop_deviation=0.05, target_pop=None, num_districts=None):
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


def run_biased_redistricting_analysis(shapefile_path, output_dir, num_steps=1000, 
                                     pop_deviation=0.05, target_pop=None, num_districts=None,
                                     bias_metric="republican_seats", maximize_bias=True,
                                     optimization_method="short_bursts"):
    """
    Run redistricting analysis optimizing for biased maps.
    
    Parameters:
    -----------
    shapefile_path : str
        Path to the precinct shapefile
    output_dir : str
        Directory to save output files
    num_steps : int
        Number of steps for the optimization
    pop_deviation : float
        Population deviation allowed (as percentage)
    target_pop : float, optional
        Target population for each district
    num_districts : int, optional
        Number of districts to create
    bias_metric : str
        Which bias metric to optimize ("efficiency_gap", "mean_median", "partisan_bias", 
        "republican_seats", "democratic_seats")
    maximize_bias : bool
        Whether to maximize (True) or minimize (False) the bias metric
    optimization_method : str
        Method to use ("short_bursts", "simulated_annealing", "tilted_run")
    """
    # Create output directory
    os.makedirs(output_dir, exist_ok=True)
    
    # Load and process data (same as regular analysis)
    print(f"Loading shapefile from {shapefile_path}")
    gdf = gpd.read_file(shapefile_path)
    
    # Check for required columns
    required_columns = ["pct_id", "pop", "pct_min", "dem_vsh"]
    missing_columns = [col for col in required_columns if col not in gdf.columns]
    
    if missing_columns:
        error_msg = f"Error: The following required columns are missing: {', '.join(missing_columns)}"
        print(error_msg)
        raise ValueError(error_msg)
    
    # Rename columns
    column_mapping = {
        "pct_id": "precinct_id", 
        "pop": "population", 
        "pct_min": "per_minority", 
        "dem_vsh": "dem_voteshare"
    }
    gdf = gdf.rename(columns=column_mapping)
    
    # Calculate additional columns
    if "per_minority" in gdf.columns and "population" in gdf.columns:
        gdf['bvap'] = np.round(gdf['per_minority'] * gdf['population']).astype(int)
        gdf['wvap'] = gdf['population'] - gdf['bvap']
    
    if "dem_voteshare" in gdf.columns:
        gdf['rep_voteshare'] = 1 - gdf['dem_voteshare']
    
    # Create graph
    print("Creating graph from GeoDataFrame")
    graph = Graph.from_geodataframe(gdf)
    
    # Add population data to graph
    for node in graph.nodes():
        graph.nodes[node]["population"] = gdf.loc[node, "population"]
        if "dem_voteshare" in gdf.columns:
            graph.nodes[node]['dem_voteshare'] = gdf.loc[node, 'dem_voteshare']
            graph.nodes[node]['rep_voteshare'] = gdf.loc[node, 'rep_voteshare']
    
    # Calculate districts and population
    total_population = sum(gdf['population'])
    
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
    
    # Set up Election and updaters
    election = Election("ELECTION", {"Dem": "dem_voteshare", "Rep": 'rep_voteshare'})
    
    my_updaters = {
        'population': Tally('population'),
        'cut_edges': cut_edges,
        'ELECTION': election
    }
    
    # Create initial partition
    print("\nCreating initial partition...")
    initial_partition = create_initial_partition(graph, num_districts, ideal_population,
                                               pop_deviation, my_updaters)
    
    # Set up constraints
    pop_constraint = constraints.within_percent_of_ideal_population(initial_partition, pop_deviation)
    compactness_bound = constraints.UpperBound(
        lambda p: len(p["cut_edges"]),
        2*len(initial_partition["cut_edges"])
    )
    
    # Set up proposal
    proposal = partial(recom,
                      pop_col="population",
                      pop_target=ideal_population,
                      epsilon=pop_deviation,
                      node_repeats=5,
                      method=partial(bipartition_tree,
                                   max_attempts=5000,
                                   allow_pair_reselection=True,
                                   warn_attempts=500))
    
    # Define bias metrics
    bias_metrics = {
        "efficiency_gap": lambda p: abs(efficiency_gap(p["ELECTION"])),
        "mean_median": lambda p: abs(mean_median(p["ELECTION"])),
        "partisan_bias": lambda p: abs(partisan_bias(p["ELECTION"])),
        "partisan_gini": lambda p: partisan_gini(p["ELECTION"]),
        "republican_seats": lambda p: p["ELECTION"].wins("Rep"),
        "democratic_seats": lambda p: p["ELECTION"].wins("Dem")
    }
    
    metric_func = bias_metrics.get(bias_metric, bias_metrics["republican_seats"])
    
    # Create optimizer
    print(f"\nOptimizing for {bias_metric} ({'maximum' if maximize_bias else 'minimum'})")
    optimizer = SingleMetricOptimizer(
        proposal=proposal,
        constraints=[constraints.contiguous, pop_constraint, compactness_bound],
        initial_state=initial_partition,
        optimization_metric=metric_func,
        maximize=maximize_bias
    )
    
    # Run optimization
    metrics = []
    CD_assignments = []
    best_scores = []
    
    try:
        if optimization_method == "short_bursts":
            print("Running short bursts optimization...")
            chain_iter = optimizer.short_bursts(
                burst_length=100,
                num_bursts=num_steps // 100
            )
        elif optimization_method == "simulated_annealing":
            print("Running simulated annealing...")
            chain_iter = optimizer.simulated_annealing(
                num_steps=num_steps,
                beta_function=lambda t: t/num_steps,  # Linear cooling
                beta_magnitude=1.0
            )
        elif optimization_method == "tilted_run":
            print("Running tilted optimization...")
            chain_iter = optimizer.tilted_run(
                num_steps=num_steps,
                p=0.1  # 10% chance of accepting worse maps
            )
        else:
            raise ValueError(f"Unknown optimization method: {optimization_method}")
        
        for i, partition in enumerate(chain_iter):
            if i % 100 == 0:
                print(f"Step {i}, Best score: {optimizer.best_score:.4f}")
            
            # Track metrics
            metrics.append({
                "step": i,
                "cut_edges": len(partition["cut_edges"]),
                "efficiency_gap": efficiency_gap(partition["ELECTION"]),
                "mean_median": mean_median(partition["ELECTION"]),
                "partisan_bias": partisan_bias(partition["ELECTION"]),
                "dem_seats": partition["ELECTION"].wins("Dem"),
                "rep_seats": partition["ELECTION"].wins("Rep"),
                "optimization_score": metric_func(partition)
            })
            
            CD_assignments.append({
                'step': i,
                'district_id': [partition.assignment[node] for node in graph.nodes()]
            })
            
            best_scores.append(optimizer.best_score)
            
    except KeyboardInterrupt:
        print("\nOptimization interrupted by user. Saving progress...")
    except Exception as e:
        print(f"\nError during optimization: {e}")
        print("Saving progress up to this point...")
    
    # Save results
    save_biased_results(metrics, CD_assignments, best_scores, optimizer, 
                       gdf, output_dir, bias_metric, graph)
    
    return {
        "num_districts": num_districts,
        "ideal_population": ideal_population,
        "steps_completed": len(CD_assignments),
        "best_score": optimizer.best_score if hasattr(optimizer, 'best_score') else None
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


def save_biased_results(metrics, CD_assignments, best_scores, optimizer, gdf, output_dir, bias_metric, graph):
    """
    Save results from biased redistricting analysis.
    """
    if metrics:
        metrics_df = pd.DataFrame(metrics)
        metrics_df['best_score'] = best_scores[:len(metrics)]
        metrics_df.to_csv(os.path.join(output_dir, "optimization_metrics.csv"), index=False)
        print(f"Saved metrics for {len(metrics)} steps")
        
        # Save best partition details
        if hasattr(optimizer, 'best_part'):
            best_partition = optimizer.best_part
            best_details = {
                "efficiency_gap": efficiency_gap(best_partition["ELECTION"]),
                "mean_median": mean_median(best_partition["ELECTION"]),
                "partisan_bias": partisan_bias(best_partition["ELECTION"]),
                "dem_seats": best_partition["ELECTION"].wins("Dem"),
                "rep_seats": best_partition["ELECTION"].wins("Rep"),
                "optimization_score": optimizer.best_score
            }
            
            with open(os.path.join(output_dir, "best_partition_details.txt"), 'w') as f:
                f.write(f"Best {bias_metric} score: {optimizer.best_score:.4f}\n")
                f.write(f"Direction: Maximized\n\n")
                for key, value in best_details.items():
                    f.write(f"{key}: {value}\n")
    
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
        
        # Add best partition assignment if available
        if hasattr(optimizer, 'best_part'):
            best_assignment = [optimizer.best_part.assignment[node] for node in graph.nodes()]
            df_pivoted['best_CD'] = best_assignment
        
        plan_output_path = os.path.join(output_dir, "CD_plans_optimized.csv")
        df_pivoted.to_csv(plan_output_path, index=False)
        print(f"Wrote optimized CD plans to {plan_output_path}")
    
    print("\nOptimization complete!")


# def create_biased_ensemble(shapefile_path, output_dir, ensemble_size=10, 
#                           bias_type="republican", pop_deviation=0.05, 
#                           num_districts=None, optimization_steps=1000):
#     """
#     Create an ensemble of biased maps using different starting points.
#     
#     Parameters:
#     -----------
#     ensemble_size : int
#         Number of biased maps to create
#     bias_type : str
#         Either "republican" or "democratic"
#     optimization_steps : int
#         Number of optimization steps per map
#     """
#     
#     all_plans = []
#     metrics_summary = []
#     
#     bias_metric = "republican_seats" if bias_type == "republican" else "democratic_seats"
#     
#     for i in range(ensemble_size):
#         print(f"\nCreating {bias_type} map {i+1}/{ensemble_size}")
#         
#         # Run optimization with different random seed
#         random.seed(i)
#         np.random.seed(i)
#         
#         result = run_biased_redistricting_analysis(
#             shapefile_path=shapefile_path,
#             output_dir=os.path.join(output_dir, f"map_{i}"),
#             num_steps=optimization_steps,
#             pop_deviation=pop_deviation,
#             num_districts=num_districts,
#             bias_metric=bias_metric,
#             maximize_bias=True,
#             optimization_method="short_bursts"
#         )
#         
#         # Read the best plan
#         plans_df = pd.read_csv(os.path.join(output_dir, f"map_{i}", "CD_plans_optimized.csv"))
#         if 'best_CD' in plans_df.columns:
#             best_plan = plans_df['best_CD'].values
#         else:
#             best_plan = plans_df.iloc[:, -1].values  # Last column
#         
#         all_plans.append(best_plan)
#         
#         # Read metrics
#         metrics_df = pd.read_csv(os.path.join(output_dir, f"map_{i}", "optimization_metrics.csv"))
#         final_metrics = metrics_df.iloc[-1]
#         
#         metrics_summary.append({
#             'map_id': i,
#             'bias_type': bias_type,
#             'dem_seats': final_metrics['dem_seats'],
#             'rep_seats': final_metrics['rep_seats'],
#             'efficiency_gap': final_metrics['efficiency_gap'],
#             'mean_median': final_metrics['mean_median']
#         })
#     
#     # Combine all plans into single dataframe
#     ensemble_df = pd.DataFrame(all_plans).T
#     ensemble_df.columns = [f'{bias_type}_map_{i}' for i in range(ensemble_size)]
#     
#     # Add precinct ID
#     plans_df = pd.read_csv(os.path.join(output_dir, "map_0", "CD_plans_optimized.csv"))
#     ensemble_df['precinct_id'] = plans_df['precinct_id']
#     
#     # Reorder columns
#     cols = ['precinct_id'] + [col for col in ensemble_df.columns if col != 'precinct_id']
#     ensemble_df = ensemble_df[cols]
#     
#     # Save ensemble
#     ensemble_path = os.path.join(output_dir, f"{bias_type}_ensemble.csv")
#     ensemble_df.to_csv(ensemble_path, index=False)
#     print(f"\nSaved {bias_type} ensemble to {ensemble_path}")
#     
#     # Save metrics summary
#     metrics_summary_df = pd.DataFrame(metrics_summary)
#     metrics_path = os.path.join(output_dir, f"{bias_type}_ensemble_metrics.csv")
#     metrics_summary_df.to_csv(metrics_path, index=False)
#     
#     return ensemble_df, metrics_summary_df


# Additional helper function for diagnostics
def diagnose_population_distribution(gdf, num_districts, epsilon):
    """Diagnostic function to check if balanced districting is possible"""
    total_pop = sum(gdf['population'])
    ideal_pop = total_pop / num_districts
    min_allowed = ideal_pop * (1 - epsilon)
    max_allowed = ideal_pop * (1 + epsilon)
    
    print(f"\nPopulation diagnostics:")
    print(f"Total population: {total_pop:,.0f}")
    print(f"Number of districts: {num_districts}")
    print(f"Ideal population per district: {ideal_pop:,.0f}")
    print(f"Allowed range: [{min_allowed:,.0f}, {max_allowed:,.0f}]")
    print(f"Allowed deviation: +/-{epsilon*100:.1f}%")
    
    # Check precinct populations
    precinct_pops = gdf['population'].values
    print(f"\nPrecinct population stats:")
    print(f"Number of precincts: {len(precinct_pops)}")
    print(f"Min: {min(precinct_pops):,.0f}")
    print(f"Max: {max(precinct_pops):,.0f}")
    print(f"Mean: {np.mean(precinct_pops):,.0f}")
    print(f"Median: {np.median(precinct_pops):,.0f}")
    
    # Check if any single precinct exceeds the allowed range
    oversized = sum(1 for p in precinct_pops if p > max_allowed)
    if oversized > 0:
        print(f"\nWARNING: {oversized} precincts exceed the maximum allowed district population!")
        print("This makes balanced districting impossible with current parameters.")
    
    return {
        'total_pop': total_pop,
        'ideal_pop': ideal_pop,
        'min_allowed': min_allowed,
        'max_allowed': max_allowed,
        'oversized_precincts': oversized
    }


def create_biased_ensemble(shapefile_path, output_dir, ensemble_size=10, 
                          bias_type="republican", pop_deviation=0.05, 
                          num_districts=None, optimization_steps=1000):
    """
    Create an ensemble of biased maps 
    Only saves final results, not intermediate steps
    """
    
    # Load and process data once 
    gdf = gpd.read_file(shapefile_path)
    
    # Data processing (same as before)
    required_columns = ["pct_id", "pop", "pct_min", "dem_vsh"]
    column_mapping = {
        "pct_id": "precinct_id", 
        "pop": "population", 
        "pct_min": "per_minority", 
        "dem_vsh": "dem_voteshare"
    }
    gdf = gdf.rename(columns=column_mapping)
    
    if "per_minority" in gdf.columns and "population" in gdf.columns:
        gdf['bvap'] = np.round(gdf['per_minority'] * gdf['population']).astype(int)
        gdf['wvap'] = gdf['population'] - gdf['bvap']
    
    if "dem_voteshare" in gdf.columns:
        gdf['rep_voteshare'] = 1 - gdf['dem_voteshare']
    
    # Create graph once
    graph = Graph.from_geodataframe(gdf)
    
    # Add population data to graph
    for node in graph.nodes():
        graph.nodes[node]["population"] = gdf.loc[node, "population"]
        if "dem_voteshare" in gdf.columns:
            graph.nodes[node]['dem_voteshare'] = gdf.loc[node, 'dem_voteshare']
            graph.nodes[node]['rep_voteshare'] = gdf.loc[node, 'rep_voteshare']
    
    # Calculate districts and population
    total_population = sum(gdf['population'])
    if num_districts is not None:
        num_districts = int(num_districts)
        ideal_population = total_population / num_districts
    
    # Set up Election and updaters
    election = Election("ELECTION", {"Dem": "dem_voteshare", "Rep": 'rep_voteshare'})
    my_updaters = {
        'population': Tally('population'),
        'cut_edges': cut_edges,
        'ELECTION': election
    }
    
    # Set up constraints and proposal (same for all maps)
    proposal = partial(recom,
                      pop_col="population",
                      pop_target=ideal_population,
                      epsilon=pop_deviation,
                      node_repeats=5,
                      method=partial(bipartition_tree,
                                   max_attempts=5000,
                                   allow_pair_reselection=True,
                                   warn_attempts=500))
    
    # Define bias metric
    bias_metric = "republican_seats" if bias_type == "republican" else "democratic_seats"
    bias_metrics = {
        "republican_seats": lambda p: p["ELECTION"].wins("Rep"),
        "democratic_seats": lambda p: p["ELECTION"].wins("Dem")
    }
    metric_func = bias_metrics[bias_metric]
    
    # Store results for all maps
    all_plans = []
    ensemble_metrics = []
    
    # SINGLE STATUS MESSAGE
    print(f"Creating ensemble of {ensemble_size} {bias_type} maps...")
    
    for i in range(ensemble_size):
        # ONLY print progress every 10 maps
        if i % 10 == 0 or i == ensemble_size - 1:
            print(f"Progress: {i+1}/{ensemble_size} maps completed")
        
        # Set different random seed for each map
        random.seed(i)
        np.random.seed(i)
        
        # Create initial partition for this map 
        initial_partition = create_initial_partition(graph, num_districts, ideal_population, pop_deviation, my_updaters)
        
        # Set up constraints for this partition
        pop_constraint = constraints.within_percent_of_ideal_population(initial_partition, pop_deviation)
        compactness_bound = constraints.UpperBound(
            lambda p: len(p["cut_edges"]),
            2*len(initial_partition["cut_edges"])
        )
        
        # Create optimizer
        optimizer = SingleMetricOptimizer(
            proposal=proposal,
            constraints=[constraints.contiguous, pop_constraint, compactness_bound],
            initial_state=initial_partition,
            optimization_metric=metric_func,
            maximize=True
        )
        
        # Run optimization - QUIETLY
        final_partition = initial_partition
        
        try:
            chain_iter = optimizer.short_bursts(
                burst_length=100,
                num_bursts=optimization_steps // 100
            )
            
            # Run the optimization WITHOUT printing every step
            for partition in chain_iter:
                final_partition = partition
            
            # Get final metrics for this map
            final_metrics = {
                'map_id': i,
                'bias_type': bias_type,
                'dem_seats': final_partition["ELECTION"].wins("Dem"),
                'rep_seats': final_partition["ELECTION"].wins("Rep"),
                'efficiency_gap': efficiency_gap(final_partition["ELECTION"]),
                'mean_median': mean_median(final_partition["ELECTION"]),
                'optimization_score': optimizer.best_score if hasattr(optimizer, 'best_score') else 0
            }
            
            ensemble_metrics.append(final_metrics)
            
            # Store the BEST partition assignment
            if hasattr(optimizer, 'best_part'):
                best_assignment = [optimizer.best_part.assignment[node] for node in graph.nodes()]
            else:
                best_assignment = [final_partition.assignment[node] for node in graph.nodes()]
            
            all_plans.append(best_assignment)
            
        except Exception as e:
            # Only print errors, not regular status
            if i % 10 == 0:  # Only show errors for every 10th map to avoid spam
                print(f"Note: Map {i} used fallback method")
            
            # Store the current partition as fallback
            best_assignment = [final_partition.assignment[node] for node in graph.nodes()]
            all_plans.append(best_assignment)
            
            ensemble_metrics.append({
                'map_id': i,
                'bias_type': bias_type,
                'dem_seats': final_partition["ELECTION"].wins("Dem"),
                'rep_seats': final_partition["ELECTION"].wins("Rep"),
                'efficiency_gap': efficiency_gap(final_partition["ELECTION"]),
                'mean_median': mean_median(final_partition["ELECTION"]),
                'optimization_score': 0
            })
    
    # Create final ensemble DataFrame - ONLY SAVE FINAL RESULTS
    if all_plans:
        ensemble_df = pd.DataFrame(all_plans).T
        ensemble_df.columns = [f'{bias_type}_map_{i}' for i in range(len(all_plans))]
        ensemble_df['precinct_id'] = gdf['precinct_id']
        
        # Reorder columns
        cols = ['precinct_id'] + [col for col in ensemble_df.columns if col != 'precinct_id']
        ensemble_df = ensemble_df[cols]
        
        # Save ensemble plans
        ensemble_path = os.path.join(output_dir, "CD_plans.csv")
        ensemble_df.to_csv(ensemble_path, index=False)
        
        # Save summary metrics
        if ensemble_metrics:
            metrics_df = pd.DataFrame(ensemble_metrics)
            metrics_path = os.path.join(output_dir, "metrics.csv")
            metrics_df.to_csv(metrics_path, index=False)
        
        # SINGLE COMPLETION MESSAGE
        best_score = max([m['optimization_score'] for m in ensemble_metrics])
        mean_score = np.mean([m['optimization_score'] for m in ensemble_metrics])
        print(f"Completed {bias_type} ensemble: best={best_score:.1f}, mean={mean_score:.1f} seats")
        
        return ensemble_df, metrics_df
    
    return None, None

# Make sure all functions are accessible when imported
__all__ = [
    'run_redistricting_analysis',
    'run_biased_redistricting_analysis',
    'create_biased_ensemble',
    'create_initial_partition',
    'diagnose_population_distribution'
]
