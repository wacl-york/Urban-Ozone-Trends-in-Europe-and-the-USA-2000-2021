data_path = function(...){

  here::here(readLines(here::here("data_config.txt"), n = 1), ...)

}
