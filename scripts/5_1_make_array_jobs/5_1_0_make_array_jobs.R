library(DBI)
library(here)
library(dplyr)
library(duckdb)

user = system("echo $USER", intern = T)

con = dbConnect(duckdb::duckdb(),
                dbdir = here(readLines(here("data_config.txt"),n = 1),"data","db.duckdb"), read_only = FALSE)

scenarios = tbl(con, "regression_scenarios") |>
  collect()

dbDisconnect(con, shutdown = T)

maxJobsPerBatch = 9999

fullArray = nrow(scenarios) %/% maxJobsPerBatch
arrayRemain = (nrow(scenarios) %% maxJobsPerBatch)-1

if(!dir.exists("reg_batch")){
  dir.create("reg_batch")
}

# Make Individual Jobs ----------------------------------------------------

fileNameList = list()

for(i in 1:(fullArray+1)){
  array_idx_offset = ((i-1)*maxJobsPerBatch)+1

  fileOut = file.path("reg_batch", paste0("toar_regression_",array_idx_offset,".sbatch"))

  fileNameList[[i]] = fileOut



  jobname = paste0("#SBATCH --job-name=toar_regression_sub_",array_idx_offset," # Job name")

  message = c("#!/usr/bin/env bash",
              jobname,
              "#SBATCH --ntasks=1                      # Number of MPI tasks to request",
              "#SBATCH --cpus-per-task=1               # Number of CPU cores per MPI task",
              "#SBATCH --mem=1G                      # Total memory to request",
              "#SBATCH --time=0-00:30:00               # Time limit (DD-HH:MM:SS)",
              "#SBATCH --account=chem-cmde-2019        # Project account to use",
              "#SBATCH --mail-type=END,FAIL            # Mail events (NONE, BEGIN, END, FAIL, ALL)",
              paste0("#SBATCH --mail-user=",user,"@york.ac.uk   # Where to send mail"),
              "#SBATCH --output=%x_log/%a/%x-%j.log       # Standard output log",
              "#SBATCH --error=%x_err/%a/%x-%j.err        # Standard error log")

  if(i != (fullArray+1)){
    message = c(message,paste0("#SBATCH --array=0-",maxJobsPerBatch-1,"          # Array range"))
  }else{
    message = c(message,paste0("#SBATCH --array=0-",arrayRemain,"          # Array range"))
  }

  message = c(message,
              c("# Abort if any command fails",
                "set -e",
                "",
                "# purge any existing modules",
                "module purge",
                "",
                "# Load modules",
                "module load R/4.4.0-gfbf-2023b",
                "",
                "# Commands to run",
                paste0('Rscript --vanilla /mnt/scratch/users/',user,'/toar/scripts/5_0_0_qr_scenario_jobscript.R $SLURM_ARRAY_TASK_ID ',
                       array_idx_offset,' /mnt/scratch/projects/chem-cmde-2019/toar/data/regressions/')))
  data_file = file(fileOut, open = "wt")
  writeLines(message, con = data_file)
  close(data_file)
}


# make runall  ------------------------------------------------------------



message = c("#!/usr/bin/env bash",
            "#SBATCH --job-name=toar_regression_main               # Job name",
            "#SBATCH --ntasks=1                      # Number of MPI tasks to request",
            "#SBATCH --cpus-per-task=1               # Number of CPU cores per MPI task",
            "#SBATCH --mem=500M                      # Total memory to request",
            "#SBATCH --time=0-48:00:00               # Time limit (DD-HH:MM:SS)",
            "#SBATCH --account=chem-cmde-2019        # Project account to use",
            "#SBATCH --mail-type=END,FAIL            # Mail events (NONE, BEGIN, END, FAIL, ALL)",
            paste0("#SBATCH --mail-user=",user,"@york.ac.uk   # Where to send mail"),
            "#SBATCH --output=%x_log/%a/%x-%j.log       # Standard output log",
            "#SBATCH --error=%x_err/%a/%x-%j.err        # Standard error log",
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
	    paste0("Rscript --vanilla /users/",user,"/scratch/toar/run_all.R")
            )



data_file = file("runall_reg.sbatch", open = "wt")
writeLines(message, con = data_file)
close(data_file)

