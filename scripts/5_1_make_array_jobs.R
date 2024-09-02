write_batch = function(i){
  array_idx_offset = ((i-1)*10000)+1

  fileOut = file.path("reg_batch", paste0("toar_regression_",array_idx_offset,".sbatch"))

  data_file = file(fileOut, open = "wt")
  on.exit(close(data_file))

  jobname = paste0("#SBATCH --job-name=TOAR_regression_test_",array_idx_offset," # Job name")

  message = c("#!/usr/bin/env bash",
              jobname,
              "#SBATCH --ntasks=1                      # Number of MPI tasks to request",
              "#SBATCH --cpus-per-task=1               # Number of CPU cores per MPI task",
              "#SBATCH --mem=500M                      # Total memory to request",
              "#SBATCH --time=0-00:15:00               # Time limit (DD-HH:MM:SS)",
              "#SBATCH --account=chem-cmde-2019        # Project account to use",
              "#SBATCH --mail-type=END,FAIL            # Mail events (NONE, BEGIN, END, FAIL, ALL)",
              "#SBATCH --mail-user=bsn502@york.ac.uk   # Where to send mail",
              "#SBATCH --output=%x_log/%a/%x-%j.log       # Standard output log",
              "#SBATCH --error=%x_err/%a/%x-%j.err        # Standard error log")

  if(i != 13){
    message = c(message,"#SBATCH --array=0-9999          # Array range")
  }else{
    message = c(message,"#SBATCH --array=0-9842          # Array range")
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
                paste0('Rscript --vanilla 5_qr_scenario_jobscript.R $SLURM_ARRAY_TASK_ID ',
                       array_idx_offset,' "/mnt/scratch/users/bsn502/TOAR/regressions/"')))

  writeLines(message, con = data_file)
}

for(i in 1:13){
  write_batch(i)
}


system("ls reg_batch/ > send_many_sbatch.sh")
system("sed -i 's,toar,sbatch reg_batch/toar,g' send_many_sbatch.sh")
#system("chmod +x send_many_sbatch.sh")



