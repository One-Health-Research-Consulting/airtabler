#' Standardize list columns across a list of data frames
#'
#' This function ensures consistent types for a specified column across multiple data frames.
#' It handles nested data frames recursively and converts mixed type columns to a consistent list format.
#' Particularly useful for handling Airtable API responses where nested fields may have inconsistent types.
#' The function detects and handles error objects like {error: "#ERROR!"} by converting them to lists.
#'
#' @author Nathan C. Layman
#'
#' @param df_list List of data frames to standardize
#' @param column Character. Name of the column to standardize
#'
#' @return List of data frames with standardized column types
#' @export
#' 
#' @examples
#' \dontrun{
#' # Create sample data with mixed types
#' df1 <- tibble(id = 1, value = list(c("a", "b")), field = tibble(name = "test1"))
#' df2 <- tibble(id = 2, value = "c", name = c("test2", "test3")) |> nest("field" = name)
#' df_list <- list(df1, df2)
#'
#' # Then apply standardize_list_columns to each column
#' standardized_df_list <- purrr::reduce(
#'   c("id", "value", "field"),
#'   ~standardize_list_columns(.x, .y),
#'   .init = df_list
#' )
#'
#' # Now the data frames can be safely combined
#' result <- dplyr::bind_rows(standardized_df_list)
#' }
#' 
standardize_list_columns <- function(df_list, column) {

  # Check column types across all dataframes
  types <- purrr::map_chr(df_list, function(df) {
    if(nrow(df) > 0 && ncol(df) > 0 && column %in% names(df)) {
      return(class(df[[column]])[1])
    }
    return(NA_character_)
  })
  
  # Check for data.frame type columns
  if(all(types == "data.frame", na.rm = TRUE)) {


    message(glue::glue("{column} detected as data.frame, recursively standardizing columns."))
    
    # 1. Extract all nested dataframes for this column
    nested_dfs <- purrr::map_if(
      df_list, 
      ~(column %in% names(.x) && inherits(.x[[column]], "data.frame")), 
      ~.x[[column]]
    )
    # nested_dfs <- purrr::compact(nested_dfs)  # Remove NULL elements
    
    # If there are no nested dataframes, just return early
    if(length(nested_dfs) == 0) {
      return(df_list)
    }
  
    # 2. Get all unique column names across all nested dataframes
    nested_cols <- purrr::map(nested_dfs, names) |>
      purrr::reduce(union)
    
    # 3. Apply standardization to each column across all nested dataframes
    for(nested_col in nested_cols) {
      nested_dfs <- standardize_list_columns(nested_dfs, nested_col)
    }
    
    # 4. Map standardized nested dataframes back to original positions
    df_list <- purrr::map(df_list, function(df) {
      if(column %in% names(df) && inherits(df[[column]], "data.frame")) {

        # Find the index of this nested df in our processed list
        idx <- which(purrr::map_lgl(df_list, 
                                   ~(column %in% names(.x) && 
                                     inherits(.x[[column]], "data.frame") && 
                                     identical(.x[[column]], df[[column]]))))
        if(length(idx) > 0) {
          # Replace with standardized version
          df[[column]] <- nested_dfs[[idx[1]]]
        }
      }
      return(df)
    })
  }  else if(any(types == "list", na.rm = TRUE) || length(unique(types[!is.na(types)])) > 1) {
    # If any dataframe has the column as a list OR we have mixed types, standardize to list
    message(glue::glue("Standardizing column: {column} to list type"))
    
    df_list <- purrr::map(df_list, function(df) {
      if(column %in% names(df)) {
        df <- df |> 
          dplyr::mutate(!!column := purrr::map(!!rlang::sym(column), function(x) {
            if(is.list(x)) return(x)
            list(x)
          }))
      }
      return(df)
    })
  }

  return(df_list)
}
