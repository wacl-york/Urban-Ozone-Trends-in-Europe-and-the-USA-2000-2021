#!/usr/bin/env bash
#SBATCH --job-name=toar_regression_mda8_main               # Job name
#SBATCH --ntasks=1                      # Number of MPI tasks to request
#SBATCH --cpus-per-task=1               # Number of CPU cores per MPI task
#SBATCH --mem=500M                      # Total memory to request
#SBATCH --time=0-48:00:00               # Time limit (DD-HH:MM:SS)
#SBATCH --account=chem-cmde-2019        # Project account to use
#SBATCH --mail-type=END,FAIL            # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=wsd500@york.ac.uk   # Where to send mail
#SBATCH --output=%x_log/%a/%x-%j.log       # Standard output log
#SBATCH --error=%x_err/%a/%x-%j.err        # Standard error log
# Abort if any command fails
set -e

# purge any existing modules
module purge

# Load modules
module load R/4.4.0-gfbf-2023b

# Commands to run
Rscript --vanilla /users/wsd500/scratch/TOAR_paper/hpc_scripts/run_all_mda8.R
