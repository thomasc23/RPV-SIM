import os
import geopandas as gpd
import pandas as pd
import numpy as np
import random
import math
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

# This is the main function that R will call
def run_redistricting_analysis(shapefile_path, output_dir, num_steps=1000, pop_deviation=0.05, target_pop=None, num_districts=None):
    """
    Run redistricting analysis using GerryChain.
    
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
    if "per_minority" in gdf.columns and "population" in gdf.columns and "bvap" not in gdf.columns:
        gdf['bvap'] = np.round(gdf['per_minority'] * gdf['population']).astype(int)
        gdf['wvap'] = gdf['population'] - gdf['bvap']
    
    if "dem_voteshare" in gdf.columns and "rep_voteshare" not in gdf.columns:
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
        # Ensure num_districts is an integer
        num_districts = int(num_districts)
        ideal_population = total_population / num_districts
    elif target_pop is not None:
        # If target population is specified, calculate districts
        num_districts = int(round(total_population / target_pop))
        ideal_population = total_population / num_districts
    else:
        # Default: You need to specify either num_districts or target_pop
        raise ValueError("Either num_districts or target_pop must be specified")
    
    print(f"Total population: {total_population}")
    print(f"Number of districts: {num_districts}")
    print(f"Ideal population per district: {ideal_population}")
    
    # Run diagnostics before attempting to create partition
    diag_results = diagnose_population_distribution(gdf, num_districts, pop_deviation)
    
    # Set up Election
    election = Election("ELECTION", {"Dem": "dem_voteshare", "Rep": 'rep_voteshare'})
    
    # Set up updaters
    my_updaters = {
        'population': Tally('population'),
        'cut_edges': cut_edges,
        'ELECTION': election
    }
    
    # Try creating initial partition with recursive_seed_part first (more robust)
    initial_partition = None
    
    # First attempt with recursive_seed_part
    try:
        print("\nAttempting to create initial partition using recursive_seed_part...")
        initial_assignment = recursive_seed_part(
            graph,
            parts=range(num_districts),
            pop_target=ideal_population,
            pop_col="population",
            epsilon=pop_deviation,
            node_repeats=10,
            method=partial(bipartition_tree, 
                          max_attempts=10000,
                          allow_pair_reselection=True,
                          warn_attempts=1000)
        )
        
        initial_partition = Partition(
            graph,
            assignment=initial_assignment,
            updaters=my_updaters
        )
        print("Successfully created initial partition using recursive_seed_part")
        
    except Exception as e:
        print(f"recursive_seed_part failed: {e}")
        print("Falling back to from_random_assignment...")
        
        # Try with progressively larger epsilon values
        epsilon_values = [pop_deviation, pop_deviation * 1.5, pop_deviation * 2, 0.25, 0.30, 0.40]
        
        for eps in epsilon_values:
            try:
                print(f"\nTrying from_random_assignment with epsilon={eps:.3f}")
                initial_partition = Partition.from_random_assignment(
                    graph, 
                    n_parts=num_districts, 
                    epsilon=eps,
                    pop_col="population",
                    updaters=my_updaters
                )
                print(f"Successfully created initial partition with epsilon={eps:.3f}")
                break
            except Exception as e:
                print(f"Failed with epsilon={eps:.3f}: {e}")
                continue
    
    if initial_partition is None:
        # Last resort: try manual assignment based on geography
        print("\nAttempting geographic-based initial partition...")
        try:
            initial_partition = create_geographic_partition(graph, gdf, num_districts, ideal_population, pop_deviation, my_updaters)
            print("Successfully created geographic-based initial partition")
        except Exception as e:
            print(f"Geographic partition failed: {e}")
            raise RuntimeError("Could not create initial partition with any method")
    
    # Print out the population of each district
    print("\nInitial district populations:")
    for district in sorted(initial_partition["population"].keys()):
        pop = initial_partition["population"][district]
        deviation = (pop - ideal_population) / ideal_population * 100
        print(f"District {district}: {pop:,.0f} (deviation: {deviation:+.1f}%)")
    print(f"Ideal population: {ideal_population:,.0f}")
    
    # Write initial district assignment back to data
    gdf['init_CD'] = [initial_partition.assignment[node] for node in graph.nodes()]
    
    # Set up constraints
    pop_constraint = constraints.within_percent_of_ideal_population(initial_partition, pop_deviation)
    compactness_bound = constraints.UpperBound(
        lambda p: len(p["cut_edges"]),
        2*len(initial_partition["cut_edges"])
    )
    
    # Set up proposal function with pair reselection enabled
    proposal = partial(recom,
                      pop_col="population",
                      pop_target=ideal_population,
                      epsilon=pop_deviation,
                      node_repeats=5,  # Increased for better chance of finding valid cuts
                      method=partial(bipartition_tree,
                                   max_attempts=5000,
                                   allow_pair_reselection=True,
                                   warn_attempts=500)
                     )
    
    # Set up the chain
    print(f"\nSetting up Markov chain with {num_steps} steps")
    print(f"Population constraint: ±{pop_deviation*100:.1f}%")
    
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
                "mean_median": mean_median(partition["ELECTION"])
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
    
    # Convert metrics to DataFrame and save
    if metrics:
        metrics_df = pd.DataFrame(metrics)
        metrics_df.to_csv(os.path.join(output_dir, "metrics.csv"), index=False)
        print(f"Saved metrics for {len(metrics)} steps")
    
    # Write CD assignments back to data
    num_steps_completed = len(CD_assignments)
    if num_steps_completed > 0:
        df_assignments = pd.DataFrame(CD_assignments)
        num_units = len(df_assignments['district_id'].iloc[0])
        all_assignments = np.zeros((num_units, num_steps_completed), dtype=int)
        
        for i, row in enumerate(df_assignments['district_id']):
            all_assignments[:, i] = row
        
        # make dataframe and add precinct_id
        df_pivoted = pd.DataFrame(all_assignments, columns=[f'step_{i+1}' for i in range(num_steps_completed)])
        df_pivoted['precinct_id'] = gdf['precinct_id']
        df_pivoted['init_CD'] = gdf['init_CD']
        
        # Write the DataFrame to a CSV file
        plan_output_path = os.path.join(output_dir, "CD_plans.csv")
        df_pivoted.to_csv(plan_output_path, index=False)
        print(f"Wrote CD plans to {plan_output_path}")
    else:
        print("No plans were generated")
        plan_output_path = None
    
    print("\nAnalysis complete!")
    return {
        "num_districts": num_districts,
        "ideal_population": ideal_population,
        "metrics_file": os.path.join(output_dir, "metrics.csv") if metrics else None,
        "plans_file": plan_output_path,
        "steps_completed": num_steps_completed
    }


def run_biased_redistricting_analysis(shapefile_path, output_dir, num_steps=1000, 
                                     pop_deviation=0.05, target_pop=None, num_districts=None,
                                     bias_metric="efficiency_gap", maximize_bias=True,
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
    if "per_minority" in gdf.columns and "population" in gdf.columns and "bvap" not in gdf.columns:
        gdf['bvap'] = np.round(gdf['per_minority'] * gdf['population']).astype(int)
        gdf['wvap'] = gdf['population'] - gdf['bvap']
    
    if "dem_voteshare" in gdf.columns and "rep_voteshare" not in gdf.columns:
        gdf['rep_voteshare'] = 1 - gdf['dem_voteshare']
        
    # Reconstruct counts if not present in shapefile
    if "n_min" in gdf.columns:
        gdf['n_minority'] = gdf['n_min']
        gdf['n_majority'] = gdf['n_maj']
    else:
        # Fallback if counts not in shapefile
        gdf['n_minority'] = np.round(gdf['per_minority'] * gdf['population']).astype(int)
        gdf['n_majority'] = gdf['population'] - gdf['n_minority']
    
    if "dem_v" in gdf.columns:
        gdf['dem_votes'] = gdf['dem_v']
        gdf['rep_votes'] = gdf['rep_v']
    else:
        # Fallback if counts not in shapefile
        gdf['dem_votes'] = np.round(gdf['dem_voteshare'] * gdf['population']).astype(int)
        gdf['rep_votes'] = gdf['population'] - gdf['dem_votes']
    
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
    
    # Create initial partition (same as regular analysis)
    print("\nCreating initial partition...")
    try:
        initial_assignment = recursive_seed_part(
            graph,
            parts=range(num_districts),
            pop_target=ideal_population,
            pop_col="population",
            epsilon=pop_deviation,
            node_repeats=10,
            method=partial(bipartition_tree, 
                          max_attempts=10000,
                          allow_pair_reselection=True,
                          warn_attempts=1000)
        )
        initial_partition = Partition(graph, assignment=initial_assignment, updaters=my_updaters)
    except:
        print("Falling back to random assignment...")
        initial_partition = Partition.from_random_assignment(
            graph, n_parts=num_districts, epsilon=pop_deviation,
            pop_col="population", updaters=my_updaters
        )
    
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
    
    metric_func = bias_metrics.get(bias_metric, bias_metrics["efficiency_gap"])
    
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
    if metrics:
        metrics_df = pd.DataFrame(metrics)
        metrics_df['best_score'] = best_scores[:len(metrics)]
        metrics_df.to_csv(os.path.join(output_dir, "optimization_metrics.csv"), index=False)
        print(f"Saved metrics for {len(metrics)} steps")
        
        # Save best partition details
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
            f.write(f"Direction: {'Maximized' if maximize_bias else 'Minimized'}\n\n")
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
        
        # Add best partition assignment
        best_assignment = [optimizer.best_part.assignment[node] for node in graph.nodes()]
        df_pivoted['best_CD'] = best_assignment
        
        plan_output_path = os.path.join(output_dir, "CD_plans_optimized.csv")
        df_pivoted.to_csv(plan_output_path, index=False)
        print(f"Wrote optimized CD plans to {plan_output_path}")
    
    print("\nOptimization complete!")
    return {
        "num_districts": num_districts,
        "ideal_population": ideal_population,
        "metrics_file": os.path.join(output_dir, "optimization_metrics.csv"),
        "plans_file": plan_output_path if num_steps_completed > 0 else None,
        "steps_completed": num_steps_completed,
        "best_score": optimizer.best_score,
        "best_partition_details": best_details if metrics else None
    }


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
    print(f"Allowed deviation: ±{epsilon*100:.1f}%")
    
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
        # Show the largest precincts
        largest_precincts = sorted(precinct_pops, reverse=True)[:5]
        print(f"Largest precincts: {[f'{p:,.0f}' for p in largest_precincts]}")
    
    # Check average precincts per district
    avg_precincts_per_district = len(precinct_pops) / num_districts
    print(f"\nAverage precincts per district: {avg_precincts_per_district:.1f}")
    
    return {
        'total_pop': total_pop,
        'ideal_pop': ideal_pop,
        'min_allowed': min_allowed,
        'max_allowed': max_allowed,
        'oversized_precincts': oversized
    }


def create_geographic_partition(graph, gdf, num_districts, ideal_population, epsilon, updaters):
    """
    Create an initial partition based on geographic proximity.
    This is a fallback method when other methods fail.
    """
    from sklearn.cluster import KMeans
    
    # Get centroids of precincts
    centroids = gdf.geometry.centroid
    coords = np.array([[p.x, p.y] for p in centroids])
    
    # Use k-means clustering to create initial groups
    kmeans = KMeans(n_clusters=num_districts, random_state=0)
    initial_assignment = kmeans.fit_predict(coords)
    
    # Create assignment dictionary
    assignment_dict = {i: int(initial_assignment[i]) for i in range(len(gdf))}
    
    # Create partition
    partition = Partition(graph, assignment_dict, updaters)
    
    # Check if all districts are contiguous
    if not constraints.contiguous(partition):
        # Fix contiguity issues
        print("Initial geographic partition has contiguity issues. Attempting to fix...")
        # This would require more sophisticated fixing, but for now we'll just raise an error
        raise ValueError("Geographic partition is not contiguous")
    
    return partition
