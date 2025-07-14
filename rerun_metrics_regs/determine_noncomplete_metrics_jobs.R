library(dplyr)
library(stringr)

get_job_status = function(array_id, format = "jobid%-30,jobname%-40,state"){

  jobStatus = system(paste0("sacct -X -j ",array_id, " --format ", format), intern = T) |>
    stringr::str_trim() |>
    stringr::str_split("\\s+")

  header = jobStatus[[1]] |>
    unlist()

  jobStatus[3:length(jobStatus)] |>
    lapply(\(x) {
      x |>
        as.data.frame() |>
        t() |>
        as.data.frame()
    }) |>
    dplyr::bind_rows() |>
    setNames(header) |>
    tibble::tibble()

}


jobs = c(
  21150935,
  21171967,
  21189082
)

jobStatus = purrr::map_df(jobs, get_job_status)


jobNotComplete = jobStatus |>
  mutate(
    array_offset = JobName |>
      str_remove("toar_regression_metrics_sub_") |>
      as.numeric(),
    slurm_array_task_id = word(JobID, 2, sep = "_")
  ) |>
  filter(State != "COMPLETED")

write.csv(jobNotComplete, "rerun_metrics_regs/metrics_jobNotComplete.csv", row.names = F)

# write the sbatch file to run the array based on jobNotComplete
# will require an alternate version of 5_0_1_qr_scenario_jobscript.R

user = system("echo $USER", intern = T)

message = c(
  "#!/usr/bin/env bash",
  "#SBATCH --job-name=toar_regression_metrics_rerun", # Job name",
  "#SBATCH --ntasks=1                      # Number of MPI tasks to request",
  "#SBATCH --cpus-per-task=1               # Number of CPU cores per MPI task",
  "#SBATCH --mem=1G                      # Total memory to request",
  "#SBATCH --time=0-00:30:00               # Time limit (DD-HH:MM:SS)",
  "#SBATCH --account=chem-cmde-2019        # Project account to use",
  "#SBATCH --mail-type=END,FAIL            # Mail events (NONE, BEGIN, END, FAIL, ALL)",
  paste0("#SBATCH --mail-user=",user,"@york.ac.uk   # Where to send mail"),
  "#SBATCH --output=toar_regression_mda8/%x_log/%a/%x-%j.log       # Standard output log",
  "#SBATCH --error=toar_regression_mda8/%x_err/%a/%x-%j.err        # Standard error log",
  paste0("#SBATCH --array=0-",nrow(jobNotComplete)-1,"          # Array range"),
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
  paste0('Rscript --vanilla /mnt/scratch/users/',user,'/TOAR_paper/rerun_metrics_regs/5_0_3_qr_scenario_jobscript_metrics_rerun.R $SLURM_ARRAY_TASK_ID ',
         ' /mnt/scratch/projects/chem-cmde-2019/toar/data/regressions_metrics/')
            )


writeLines(message, "rerun_metrics_regs/rerun_metrics_regs.sbatch")
