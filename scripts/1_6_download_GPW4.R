urls = c(
  "https://dljsq618eotzp.cloudfront.net/sedac-popdensity-yeargrid5yr-v4.11/gpw_v4_population_density_rev11_2000_30_sec_2000.tif",
  "https://dljsq618eotzp.cloudfront.net/sedac-popdensity-yeargrid5yr-v4.11/gpw_v4_population_density_rev11_2020_30_sec_2020.tif"
  )

dirOut = here::here(readLines(here::here('data_config.txt')), "data", "GPW4")

if(!dir.exists(dirOut)){
  dir.create(dirOut)
}

for(i in 1:length(urls)){
  download.file(urls[i], file.path(dirOut,basename(urls[i])))
}
