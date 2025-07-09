library(DBI)
library(here)
library(dplyr)

con = dbConnect(duckdb::duckdb(),
                dbdir = here(readLines(here("data_config.txt"),n = 1),"data","db.duckdb"), read_only = TRUE)

name_station = tbl(con, "name_station") |>
  collect() |>
  filter(name == "o3") |>
  arrange(station_id)

dbDisconnect(con, shutdown = T)

user = system("echo $USER", intern = T)

message = c("#!/usr/bin/env bash",
            "#SBATCH --job-name=toar_make_piecewise # Job name",
            "#SBATCH --ntasks=1                      # Number of MPI tasks to request",
            "#SBATCH --cpus-per-task=1               # Number of CPU cores per MPI task",
            "#SBATCH --mem=2G                      # Total memory to request",
            "#SBATCH --time=0-01:00:00               # Time limit (DD-HH:MM:SS)",
            "#SBATCH --account=chem-cmde-2019        # Project account to use",
            "#SBATCH --mail-type=END,FAIL            # Mail events (NONE, BEGIN, END, FAIL, ALL)",
            paste0("#SBATCH --mail-user=",user,"@york.ac.uk   # Where to send mail"),
            "#SBATCH --output=%x_log/%a/%x-%j.log       # Standard output log",
            "#SBATCH --error=%x_err/%a/%x-%j.err        # Standard error log",
            paste0("#SBATCH --array=0-",nrow(name_station)-1,"          # Array range"),
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
            paste0('Rscript --vanilla /mnt/scratch/users/',user,'/TOAR_paper/scripts/5_3_0_2_create_piecewise_mda8_anom.R $SLURM_ARRAY_TASK_ID')
)

data_file = file(paste0('/mnt/scratch/users/',user,'/TOAR_paper/sbatch/run_makepiecewise_mda8_anom.sbatch'), open = "wt")
writeLines(message, con = data_file)
close(data_file)
