read_viking_log = function(fp){

  rawlog = readLines(fp)

  if(length(rawlog) != 20){
    return(NULL)
  }

  log_split = rawlog[6:length(rawlog)] |>
    lapply(function(x) stringr::str_split_1(x,":"))


  nms = lapply(log_split, function(x) stringr::str_trim(x[[1]])) |>
    unlist()


  joblog = lapply(log_split,function(x) stringr::str_trim(paste0(x[-1],collapse = ":"))) |>
    setNames(nms)

  joblog$mem_eff_perc = as.numeric(stringr::str_split_1(joblog$`Memory Efficiency`,"%")[1])
  joblog$wall_eff_perc = as.numeric(stringr::str_split_1(joblog$`Wall clock time efficiency`,"%")[1])

  joblog
}

args = commandArgs(trailingOnly = TRUE)

files = list.files(args[1],full.names = T)

fileName = basename(files)

logList = lapply(files, read_viking_log) |>
  setNames(fileName)

saveRDS(args[2])






