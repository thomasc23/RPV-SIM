#!/usr/bin/env bash
#SBATCH -J rpv_plans_small
#SBATCH --array=0-2
#SBATCH -t 5-00:00:00
#SBATCH -p standard
#SBATCH -N 1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=100G
#SBATCH -o logs/rpv_%A_%a.out
#SBATCH -e logs/rpv_%A_%a.err
#SBATCH --signal=B:USR1@180   # send SIGUSR1 180s before time limit

set -euo pipefail

# Which level am I?
LEVELS=(low medium high)
SEG_LEVEL="${LEVELS[$SLURM_ARRAY_TASK_ID]}"

# Base paths (all under /scratch)
export BASE_DIR="${BASE_DIR:-/scratch/$USER/RPV_Draw_Plans}"
export CONDA_ENV_PATH="${CONDA_ENV_PATH:-$BASE_DIR/conda_envs/rpv_py310}"
export R_LIBS_USER="${R_LIBS_USER:-$BASE_DIR/Rlib/4.4}"

mkdir -p "$BASE_DIR/logs" "$BASE_DIR/Output"

# --- Modules & geo runtime (same recipe that worked interactively) ---
# Temporarily disable `-e` because module init runs `unalias sudo` and returns non-zero.
set +e
# If your site uses a different init path, keep it; otherwise this is harmless.
source /software/modules/init/bash 2>/dev/null || true
module purge 2>/dev/null || true
module load r/4.4.0/b1 udunits2/2.2.20 2>/dev/null || true
set -e
# --------------------------------------------------------------------

export RETICULATE_PYTHON="$CONDA_ENV_PATH/bin/python"
export LD_LIBRARY_PATH="/software/udunits2/2.2.20/lib:$CONDA_ENV_PATH/lib:${LD_LIBRARY_PATH:-}"
export GDAL_DATA="$CONDA_ENV_PATH/share/gdal"
export PROJ_LIB="$CONDA_ENV_PATH/share/proj"

# (Optional) avoids matplotlib font-cache chatter per job
export MPLCONFIGDIR="$BASE_DIR/.matplotlib"
mkdir -p "$MPLCONFIGDIR"

# Parameters you requested
export SEG_LEVELS="$SEG_LEVEL"
export N_PLANS="${N_PLANS:-1000}"             # neutral: number of steps -> ~plans
export ENSEMBLE_SIZE="${ENSEMBLE_SIZE:-100}" # partisan: number of optimized maps
export BURST_LENGTH="${BURST_LENGTH:-500}"
export NUM_BURSTS="${NUM_BURSTS:-30}"
export PATIENCE_BURSTS="${PATIENCE_BURSTS:-10}"
export DEV_MODE="${DEV_MODE:-TRUE}"
export OUTPUT_BASE_DIR="${OUTPUT_BASE_DIR:-Output/}"
export SAVE_EVERY_STEPS="${SAVE_EVERY_STEPS:-50}"

echo "Job started: $(date)"
echo "Node: ${SLURMD_NODENAME:-$(hostname)}"
echo "Array ID: $SLURM_ARRAY_TASK_ID | Level: $SEG_LEVEL"
echo "Params: N_PLANS=$N_PLANS ENSEMBLE_SIZE=$ENSEMBLE_SIZE BURST_LENGTH=$BURST_LENGTH NUM_BURSTS=$NUM_BURSTS DEV_MODE=$DEV_MODE"

cd "$BASE_DIR"
Rscript 01_simulate_plans.R

echo "Job finished: $(date)"
