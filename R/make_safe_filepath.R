make_safe_filepath <- function(string) {
  safe_string <- string
  
  # Replace problematic characters with underscores
  safe_string <- stringr::str_replace_all(safe_string, "[\\/:*?\"<>|]", "_")
  
  # Replace multiple consecutive underscores with a single one
  safe_string <- stringr::str_replace_all(safe_string, "_+", "_")
  
  # Trim whitespace from both ends
  safe_string <- stringr::str_trim(safe_string)
  
  # Remove leading/trailing underscores
  safe_string <- stringr::str_replace_all(safe_string, "^_+|_+$", "")
  
  # Remove underscores before file extension
  safe_string <- stringr::str_replace_all(safe_string, "_(\\.[^.]+)$", "\\1")
  
  # Optional: collapse internal whitespace
  safe_string <- stringr::str_squish(safe_string)
  
  return(safe_string)
}
