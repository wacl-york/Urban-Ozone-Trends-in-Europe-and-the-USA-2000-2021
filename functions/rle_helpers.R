tidy_rle = function(rle_obj) {
  rle_df = dplyr::tibble(lengths = rle_obj$lengths,
                         values = rle_obj$values)

  dplyr::mutate(rle_df, idx_start = cumsum(c(1, lengths))[1:(nrow(rle_df))],
                idx_end = cumsum(lengths))

}

index_from_tidy_rle = function(tidyRle){

  tidyRle |>
    dplyr::mutate(id = dplyr::row_number()) |>
    purrr::pmap_df(~data.frame(idx = ..3:..4,id = ..5))

}
