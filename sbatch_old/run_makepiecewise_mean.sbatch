#!/usr/bin/env bash
#SBATCH --job-name=toar_make_piecewise_mean # Job name
#SBATCH --ntasks=1                      # Number of MPI tasks to request
#SBATCH --cpus-per-task=8               # Number of CPU cores per MPI task
#SBATCH --mem=16G                      # Total memory to request
#SBATCH --time=0-01:00:00               # Time limit (DD-HH:MM:SS)
#SBATCH --account=chem-cmde-2019        # Project account to use
#SBATCH --mail-type=END,FAIL            # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=wsd500@york.ac.uk   # Where to send mail
#SBATCH --output=%x_log/%a/%x-%j.log       # Standard output log
#SBATCH --error=%x_err/%a/%x-%j.err        # Standard error log
#SBATCH --array=0-686          # Array range
# Abort if any command fails
set -e

# purge any existing modules
module purge

# Load modules
module load R/4.4.0-gfbf-2023b

# Commands to run
Rscript --vanilla /mnt/scratch/users/wsd500/TOAR_paper/scripts/5_3_0_create_piecewise/5_3_0_4_a_create_piecewise_mean.R $SLURM_ARRAY_TASK_ID
