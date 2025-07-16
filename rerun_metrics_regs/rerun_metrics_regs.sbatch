#!/usr/bin/env bash
#SBATCH --job-name=toar_regression_metrics_rerun
#SBATCH --ntasks=1                      # Number of MPI tasks to request
#SBATCH --cpus-per-task=1               # Number of CPU cores per MPI task
#SBATCH --mem=1G                      # Total memory to request
#SBATCH --time=0-01:00:00               # Time limit (DD-HH:MM:SS)
#SBATCH --account=chem-cmde-2019        # Project account to use
#SBATCH --mail-type=END,FAIL            # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=bsn502@york.ac.uk   # Where to send mail
#SBATCH --output=toar_regression_mda8/%x_log/%a/%x-%j.log       # Standard output log
#SBATCH --error=toar_regression_mda8/%x_err/%a/%x-%j.err        # Standard error log
#SBATCH --array=0-173          # Array range
# Abort if any command fails
set -e

# purge any existing modules
module purge

# Load modules
module load R/4.4.0-gfbf-2023b

# Commands to run
Rscript --vanilla /mnt/scratch/users/bsn502/TOAR_paper/rerun_metrics_regs/5_0_3_qr_scenario_jobscript_metrics_rerun.R $SLURM_ARRAY_TASK_ID  /mnt/scratch/projects/chem-cmde-2019/toar/data/regressions_metrics/
