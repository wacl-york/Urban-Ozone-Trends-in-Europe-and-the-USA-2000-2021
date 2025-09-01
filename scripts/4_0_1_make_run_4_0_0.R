library(here)

source(here::here('functions','utils.R'))

user = system("echo $USER", intern = T)

outputFile = here(data_path(), "cluster_meancvi", "logs","%x_%j_%a.log")
errFile = here(data_path(), "cluster_meancvi", "logs","%x_%j_%a.err")

message = c("#!/usr/bin/env bash",
            "#SBATCH --job-name=run_time_series_cluster_meancvi # Job name",
            "#SBATCH --ntasks=1                      # Number of MPI tasks to request",
            "#SBATCH --cpus-per-task=16               # Number of CPU cores per MPI task",
            "#SBATCH --mem=16G                      # Total memory to request",
            "#SBATCH --time=0-02:00:00               # Time limit (DD-HH:MM:SS)",
            "#SBATCH --account=chem-cmde-2019        # Project account to use",
            "#SBATCH --mail-type=END,FAIL            # Mail events (NONE, BEGIN, END, FAIL, ALL)",
            paste0("#SBATCH --mail-user=",user,"@york.ac.uk   # Where to send mail"),
            paste0("#SBATCH --output=",outputFile,"       # Standard output log"),
            paste0("#SBATCH --error=",errFile,"        # Standard error log"),
            paste0("#SBATCH --array=0-6          # Array range"), # 7 taus
            "# Abort if any command fails",
            "set -e",
            "",
            "# purge any existing modules",
            "module purge",
            "",
            "# Load modules",
            "module load R/4.4.0-gfbf-2023b",
            "",
            "# Commands to run",
            paste0('Rscript --vanilla /mnt/scratch/users/',user,'/TOAR_paper/scripts/4_0_0_cluster_time_series_meancvi.R $SLURM_ARRAY_TASK_ID')
)

data_file = file(paste0('/mnt/scratch/users/',user,'/TOAR_paper/sbatch/run_4_0_0.sbatch'), open = "wt")
writeLines(message, con = data_file)
close(data_file)
