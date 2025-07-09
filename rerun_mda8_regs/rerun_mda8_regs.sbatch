#!/usr/bin/env bash
#SBATCH --job-name=toar_regression_mda8_rerun
#SBATCH --ntasks=1                      # Number of MPI tasks to request
#SBATCH --cpus-per-task=1               # Number of CPU cores per MPI task
#SBATCH --mem=1G                      # Total memory to request
#SBATCH --time=0-00:30:00               # Time limit (DD-HH:MM:SS)
#SBATCH --account=chem-cmde-2019        # Project account to use
#SBATCH --mail-type=END,FAIL            # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=wsd500@york.ac.uk   # Where to send mail
#SBATCH --output=toar_regression_mda8/%x_log/%a/%x-%j.log       # Standard output log
#SBATCH --error=toar_regression_mda8/%x_err/%a/%x-%j.err        # Standard error log
#SBATCH --array=0-1216          # Array range
# Abort if any command fails
set -e

# purge any existing modules
module purge

# Load modules
module load R/4.4.0-gfbf-2023b

# Commands to run
Rscript --vanilla /mnt/scratch/users/wsd500/TOAR_paper/rerun_mda8_regs/5_0_1_qr_scenario_jobscript_mda8_rerun.R $SLURM_ARRAY_TASK_ID  /mnt/scratch/projects/chem-cmde-2019/toar/data/regressions_mda8_o3/
