library(stringr)

user = system("echo $USER", intern = T)

files = list.files(paste0("/users/",user,"/scratch/TOAR_paper/reg_batch_mean/"), full.names = T)


for(i in 1:length(files)){

  jobId = system(paste0("sbatch --parsable ",files[i]), intern = T)
  print(paste0("array ",i,"'s job ID is :",jobId))
  jobRunning = TRUE

  while(jobRunning){
    Sys.sleep(60)
    jobStatus = system(paste0("sacct -n -X -j ",jobId," --format=State"), intern = T) |>
      str_trim() |>
      unique()

    print(jobStatus)
    # if there are more states than specified here, it will be > 0 so something is still RUNNING or PENDING
    if(sum(!jobStatus %in% c("COMPLETED", "TIMEOUT", "FAILED","OUT_OF_ME+")) > 0){
      next
    }else{
      jobRunning = FALSE
    }

  }
}


