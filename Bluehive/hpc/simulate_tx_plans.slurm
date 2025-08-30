#!/usr/bin/env bash
#SBATCH -J rpv_tx_plans
#SBATCH -t 5-00:00:00
#SBATCH -p standard
#SBATCH -N 1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=100G
#SBATCH -o TX/logs/rpv_tx_%j.out
#SBATCH -e TX/logs/rpv_tx_%j.err
#SBATCH --signal=B:USR1@180

set -euo pipefail

# -------- Paths (under /scratch by default) --------
export BASE_DIR="${BASE_DIR:-/scratch/$USER/RPV_Draw_Plans}"
export CONDA_ENV_PATH="${CONDA_ENV_PATH:-$BASE_DIR/conda_envs/rpv_py310}"
export R_LIBS_USER="${R_LIBS_USER:-$BASE_DIR/Rlib/4.4}"

# Ensure runtime dirs exist (does not affect SBATCH -o/-e — create those dirs before sbatch)
mkdir -p "$BASE_DIR" "$BASE_DIR/TX/logs" "$BASE_DIR/TX/Output"

# -------- Modules & geo runtime --------
set +e
source /software/modules/init/bash 2>/dev/null || true
module purge 2>/dev/null || true
module load r/4.4.0/b1 udunits2/2.2.20 2>/dev/null || true
set -e

# -------- Reticulate / GDAL / PROJ wiring --------
export PATH="$CONDA_ENV_PATH/bin:$PATH"
export RETICULATE_PYTHON="$CONDA_ENV_PATH/bin/python"
export LD_LIBRARY_PATH="/software/udunits2/2.2.20/lib:$CONDA_ENV_PATH/lib:${LD_LIBRARY_PATH:-}"
export GDAL_DATA="$CONDA_ENV_PATH/share/gdal"
export PROJ_LIB="$CONDA_ENV_PATH/share/proj"
export OMP_NUM_THREADS="${SLURM_CPUS_PER_TASK:-4}"

# Optional: quiet matplotlib font cache chatter
export MPLCONFIGDIR="$BASE_DIR/.matplotlib"
mkdir -p "$MPLCONFIGDIR"

# -------- R script parameters (env-driven) --------
# Keep the original simulation in Output/; Texas goes to TX/Output
export OUTPUT_BASE_DIR="${OUTPUT_BASE_DIR:-TX/Output}"

# Core engine knobs (overridable on sbatch --export=…)
export N_PLANS="${N_PLANS:-1000}"
export ENSEMBLE_SIZE="${ENSEMBLE_SIZE:-100}"
export BURST_LENGTH="${BURST_LENGTH:-500}"
export NUM_BURSTS="${NUM_BURSTS:-30}"
export PATIENCE_BURSTS="${PATIENCE_BURSTS:-10}"
export SOFT_K="${SOFT_K:-60}"
# Per your request:
export DEV_MODE="${DEV_MODE:-FALSE}"
export SIMPLIFY_TOL="${SIMPLIFY_TOL:-0.5}"
export RANDOM_SEED="${RANDOM_SEED:-123}"
export SAVE_EVERY_STEPS="${SAVE_EVERY_STEPS:-50}"

# File inputs/outputs (overridable)
export SHAPEFILE_IN="${SHAPEFILE_IN:-data/tx_24.shp}"
export SHAPEFILE_FOR_PIPE="${SHAPEFILE_FOR_PIPE:-$OUTPUT_BASE_DIR/state_map.shp}"

# EI hyperparameters (exposed)
export EI_MEAN_WHITE="${EI_MEAN_WHITE:-0.33}"
export EI_MEAN_BLACK="${EI_MEAN_BLACK:-0.73}"
export EI_MEAN_HISP="${EI_MEAN_HISP:-0.49}"
export EI_SD_WHITE="${EI_SD_WHITE:-0.14}"
export EI_SD_BLACK="${EI_SD_BLACK:-0.26}"
export EI_SD_HISP="${EI_SD_HISP:-0.31}"
export EI_RHO_WB="${EI_RHO_WB:--0.43}"
export EI_RHO_WH="${EI_RHO_WH:--0.35}"
export EI_RHO_BH="${EI_RHO_BH:-0.068}"

# Ensure the pipe target dir exists
mkdir -p "$BASE_DIR/$(dirname "$SHAPEFILE_FOR_PIPE")"

# -------- Sanity prints --------
echo "Job started: $(date)"
echo "Node: ${SLURMD_NODENAME:-$(hostname)}"
echo "CPUs: ${SLURM_CPUS_PER_TASK:-4}  MEM: ${SLURM_MEM_PER_NODE:-100G}"
echo "BASE_DIR: $BASE_DIR"
echo "RETICULATE_PYTHON: $RETICULATE_PYTHON"
echo "OUTPUT_BASE_DIR: $OUTPUT_BASE_DIR"
echo "Params: N_PLANS=$N_PLANS ENSEMBLE_SIZE=$ENSEMBLE_SIZE BURST_LENGTH=$BURST_LENGTH NUM_BURSTS=$NUM_BURSTS SOFT_K=$SOFT_K DEV_MODE=$DEV_MODE"
echo "Files:  SHAPEFILE_IN=$SHAPEFILE_IN  SHAPEFILE_FOR_PIPE=$SHAPEFILE_FOR_PIPE"
echo "EI: means=($EI_MEAN_WHITE,$EI_MEAN_BLACK,$EI_MEAN_HISP) sds=($EI_SD_WHITE,$EI_SD_BLACK,$EI_SD_HISP) rhos=($EI_RHO_WB,$EI_RHO_WH,$EI_RHO_BH)"

# -------- Run --------
cd "$BASE_DIR"
if [[ ! -f "$SHAPEFILE_IN" ]]; then
  echo "ERROR: Missing input shapefile: $SHAPEFILE_IN" >&2
  exit 2
fi

Rscript 01b_simulate_tx.R

echo "Job finished: $(date)"

