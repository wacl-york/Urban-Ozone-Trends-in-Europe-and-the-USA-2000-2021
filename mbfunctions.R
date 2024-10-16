mbfun = function(formula,
                 data,
                 tau # the quantile to do qr on.
){

  n = nrow(data) # length of timeseries
  b = ceiling(n^0.25) # block length
  nblocks = ceiling(n/b)

  # create a list of sequences that are the length of a block, and overlap with the subsequent block for b-1 of the values
  # the values in the sequences correspond to row ids in the data. eventually used to select values from the data
  # the list contains enough elements such that the last sequence in the list has its final value == n
  # (n-b+1, but the values in the sequence run to b-1, so the final value == n)
  blocks = lapply(seq_len(n-b+1), function(i) seq(i, i+b-1))

  # create a vector of randomly sampled block ids - i.e. values between 1 and nblocks.
  bn = sample(1:length(blocks), nblocks, replace = T)

  # blocks[bn] randomly sample (with replacement) the blocks of data.
  # unlisting this produces a vector that will rearrange the "blocks" of the timeseries

  samp_data = data[unlist(blocks[bn]),]
  mod = quantreg::rq(formula, data = samp_data, tau = tau)

  coef(mod)
}

# -------------------------------------------------------------------------

# Take bootstrap samples of the timeseries and return quantile regression coefficients.
# used to test significance of the trend in the measured timeseries.
# updated to reassign x before doing calculating qr trend so that the bootstrapped data is more like a timeseries
mbfun2 = function(formula,
                  data,
                  tau # the quantile to do qr on.
){

  n = nrow(data) # length of timeseries
  b = ceiling(n^0.25) # block length
  nblocks = ceiling(n/b)

  # create a list of sequences that are the length of a block, and overlap with the subsequent block for b-1 of the values
  # the values in the sequences correspond to row ids in the data. eventually used to select values from the data
  # the list contains enough elements such that the last sequence in the list has its final value == n
  # (n-b+1, but the values in the sequence run to b-1, so the final value == n)
  blocks = lapply(seq_len(n-b+1), function(i) seq(i, i+b-1))

  # create a vector of randomly sampled block ids - i.e. values between 1 and nblocks.
  bn = sample(1:length(blocks), nblocks, replace = T)

  # blocks[bn] randomly sample (with replacement) the blocks of data.
  # unlisting this produces a vector that will rearrange the "blocks" of the timeseries

  samp_data = data[unlist(blocks[bn]),]

  # when you do the QR, it will use the indices in samp_data, so the fact that the data was "rearranged" above does not affect the outcome
  # unless we do this to reassign the time index.
  samp_data$x = 1:nrow(samp_data)

  mod = quantreg::rq(formula, data = samp_data, tau = tau)

  coef(mod)
}
