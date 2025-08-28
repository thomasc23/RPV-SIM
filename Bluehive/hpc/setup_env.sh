#!/usr/bin/env bash
#SBATCH --job-name=segmaps
#SBATCH --partition=debug
#SBATCH --time=00:15:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=12G
#SBATCH --output=logs/rpv_%j.out
#SBATCH --error=logs/rpv_%j.err
#SBATCH --export=NONE
#SBATCH --mail-type=begin
#SBATCH --mail-type=end
#SBATCH --mail-user=tchambe6@ur.rochester.edu

set -euo pipefail

# --- Make 'module' available without login shell ---
if ! type module &>/dev/null; then
  set +e
  source /etc/profile.d/modules.sh 2>/dev/null || \
  source /usr/share/Modules/init/bash 2>/dev/null || \
  source /software/modules/3.2.10/init/bash 2>/dev/null
  set -e
fi

module purge
module load r/4.4.0/b1
module load anaconda3/2023.07-2

# --- All paths under /scratch ---
export BASE_DIR=/scratch/tchambe6/RPV_Draw_Plans
cd "$BASE_DIR"

# --- Bootstrap env (idempotent, safe every run) ---
bash hpc/setup_env.sh

# Mirror key vars that R/reticulate and runtime need
export R_LIBS_USER="$BASE_DIR/Rlib/4.4"
export RETICULATE_PYTHON="$BASE_DIR/conda_envs/rpv_py310/bin/python"

# Prefer conda GDAL/PROJ at runtime for sf/gdal-based ops
export LD_LIBRARY_PATH="$BASE_DIR/conda_envs/rpv_py310/lib:${LD_LIBRARY_PATH:-}"
export GDAL_DATA="$BASE_DIR/conda_envs/rpv_py310/share/gdal"
export PROJ_LIB="$BASE_DIR/conda_envs/rpv_py310/share/proj"
export PROJ_NETWORK=ON

# Reasonable BLAS/OMP thread caps on a single node
export OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK:-4}
export MKL_NUM_THREADS=${SLURM_CPUS_PER_TASK:-4}
export OPENBLAS_NUM_THREADS=${SLURM_CPUS_PER_TASK:-4}
export NUMEXPR_NUM_THREADS=${SLURM_CPUS_PER_TASK:-4}

echo "Job started: $(date)"
echo "Node: $(hostname)"
echo "Job ID: ${SLURM_JOB_ID}"
echo "PWD: $(pwd)"

# --- Your simulation knobs (env-driven) ---
export N_PLANS=30
export ENSEMBLE_SIZE=6
export BURST_LENGTH=80
export NUM_BURSTS=5
export PATIENCE_BURSTS=3
export SOFT_K=60
export RANDOM_SEED=42
export OUTPUT_BASE_DIR="$BASE_DIR/Output/Debug/"

# --- Run ---
Rscript 01_simulate_plans.R

echo "Job finished: $(date)"

