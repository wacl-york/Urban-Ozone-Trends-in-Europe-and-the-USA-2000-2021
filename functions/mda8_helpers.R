calc_hour8_o3 = function(x){

  y = x[!is.na(x)]

  if(length(y) < 6){ # An MDA8 is valid if 6 of 8 hours are present in period
    return(NA)
  }

  mean(x, na.rm = T)

}

# Function to account for days where we have no mda8 data
# Avoids the "no non-missing arguments to max" warnings, just incase something else is happening
max_or_empty = function(x){

  y = x[!is.na(x)]

  if(length(y) == 0){
    NA
  }else{
    max(y, na.rm = T)
  }

}
