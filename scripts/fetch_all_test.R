out[[1]] <- airtabler::air_get(base, table_name, combined_result = FALSE)
offset <- airtabler::get_offset(out[[1]])
while (!is.null(offset)) {
  out <- c(out, list(airtabler::air_get(base, table_name, combined_result = FALSE, offset = offset)))
  offset <- airtabler::get_offset(out[[length(out)]])
}

out_col_names <- purrr::reduce(out, ~union(.x, names(.y)), .init = character(0))
df_list <- out # For debugging standardize_list_columns
out <- purrr::reduce(out_col_names, ~standardize_list_columns(.x, .y), .init = out)
out <- dplyr::bind_rows(out)
