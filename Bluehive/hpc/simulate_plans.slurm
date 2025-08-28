#!/bin/bash
#SBATCH -J segmaps
#SBATCH -p debug
#SBATCH -t 00:15:00
#SBATCH -N 1
#SBATCH -o logs/rpv_%j.out
#SBATCH -e logs/rpv_%j.err

set -u
umask 007

module purge
module load r/4.4.0/b1
module load udunits2/2.2.20

export BASE_DIR="${BASE_DIR:-/scratch/$USER/RPV_Maps}"
export CONDA_ENV_PATH="$BASE_DIR/conda_envs/rpv_py310"

mkdir -p "$BASE_DIR/logs" "$BASE_DIR/Output"

# Conda env for GDAL/GEOS/PROJ + Python
export PATH="$CONDA_ENV_PATH/bin:$PATH"
export LD_LIBRARY_PATH="/software/udunits2/2.2.20/lib:$CONDA_ENV_PATH/lib:${LD_LIBRARY_PATH:-}"
export R_LIBS_USER="$BASE_DIR/Rlib/4.4"
export RETICULATE_PYTHON="$CONDA_ENV_PATH/bin/python"
export GDAL_DATA="$CONDA_ENV_PATH/share/gdal"
export PROJ_LIB="$CONDA_ENV_PATH/share/proj"
export PKG_CONFIG_PATH="$CONDA_ENV_PATH/lib/pkgconfig:/software/udunits2/2.2.20/lib/pkgconfig:${PKG_CONFIG_PATH:-}"

echo "Job started: $(date)"
echo "Node: $(hostname)"
echo "Job ID: ${SLURM_JOB_ID:-N/A}"
echo "R_LIBS_USER=$R_LIBS_USER"
echo "RETICULATE_PYTHON=$RETICULATE_PYTHON"
echo "GDAL_DATA=$GDAL_DATA"
echo "PROJ_LIB=$PROJ_LIB"
module list || true

# IMPORTANT: define POP_DEVIATION used by 01_simulate_plans.R
export POP_DEVIATION="${POP_DEVIATION:-0.02}"

Rscript --vanilla 01_simulate_plans.R
rc=$?

echo "Job finished: $(date) (rc=$rc)"
exit $rc
