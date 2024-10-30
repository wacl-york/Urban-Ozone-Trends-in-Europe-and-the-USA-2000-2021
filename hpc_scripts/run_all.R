library(stringr)

files = list.files("reg_batch/", full.names = T)


for(i in 16:length(files)){

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
		if(sum(!jobStatus %in% c("COMPLETED", "TIMEOUT", "FAILED")) > 0){ 
			next
		}else{
			jobRunning = FALSE
		}

	}	
}
	

