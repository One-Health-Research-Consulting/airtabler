air_download_attachments <- function(x, field, base_path = NULL, attachment_folder = "downloads", 
                                     include_attachment_id = TRUE, table_name = NULL, 
                                     organize_by_table_field = TRUE, use_relative_paths = TRUE,
                                     remove_original_field = FALSE, verbose = TRUE, overwrite = FALSE, ...) {
  
  if(!is.data.frame(x)){
    rlang::abort("x is not a dataframe")
  }
  
  if(!field %in% names(x)){
    error_msg <- glue::glue("{field} not found in names(x). Check the name of the column
                            used to store attachments in airtable")
    
    rlang::abort(error_msg)
  }
  
  # Construct the full base directory path
  if(!is.null(base_path)) {
    dir_name <- file.path(base_path, attachment_folder)
  } else {
    dir_name <- attachment_folder
  }
  
  # If organize_by_table_field is TRUE and table_name is provided, organize files by table/field
  if(organize_by_table_field && !is.null(table_name)) {
    # Create a path that includes table name and field name
    download_dir <- file.path(dir_name, table_name, field)
  } else {
    download_dir <- dir_name
  }
  
  if(!is.list(x[,field])){
    error_msg <- glue::glue("Field '{field}' in table '{table_name}' is not of class list. 
                            This field may not be properly configured as an attachment field in Airtable.")
    rlang::warn(error_msg)
    
    field_file_paths <- sprintf("%s.file_paths", field)  # Changed from "_file_paths" to ".file_paths"
    
    x$file_path <- NA
    
    # using dynamic names in case a base has multiple file attachment
    # columns
    x <- dplyr::rename(x, {{field_file_paths}} := file_path)
    return(x)
  }
  
  ### subset to necessary records ----
  
  # get files
  xfield <- purrr::pluck(x, field)
  
  ### get files ----
  dir.create(path = download_dir, recursive = TRUE, showWarnings = FALSE)  # Added showWarnings=FALSE
  
  xlist <- purrr::map(xfield, function(x){
    # Skip NULL entries or entries without URLs
    if(is.null(x) || length(x) == 0){
      return(NULL)
    }
    
    if(is.null(x$url)){
      ID <- x$id
      if(verbose) warning(sprintf("Record ID %s is null", ID))
      return(NULL)
    }
    
    # prepending attachment id in case the file naming convention
    # of the user does not preclude duplicate file names for files
    # with different contents
    
    filename_dest <- x$filename
    
    if(include_attachment_id){
      filename_dest <- sprintf("%s_%s", x$id, filename_dest)
    } else {
      if(verbose) {
        message("include_attachment_id = FALSE. If file names are repeated,
        only the first file with that name will be downloaded.")
      }
    }
    
    # Full path for download destination
    dest_full <- file.path(download_dir, filename_dest)
    
    # sometimes the same file is attached multiple times
    # if the file is already downloaded, don't add it again
    
    if(all(file.exists(dest_full)) && !overwrite){
      if(verbose) {
        not_downloaded_message <- glue::glue("\nFile already exists, not downloaded\n{dest_full}\n.")
        print(not_downloaded_message)
      }
      
      # Create and return appropriate path
      if(use_relative_paths && !is.null(base_path)) {
        # Return path relative to base_path
        rel_path <- file.path(attachment_folder, table_name, field, filename_dest)
        return(rel_path)
      } else {
        return(dest_full)
      }
    }
    
    # wrap in a map so that it works on linux systems where urls must explicitly
    # be a length one character vector
    purrr::map2(x$url, dest_full, function(url_item, dest_item){
      if(is.character(url_item) & length(url_item) == 1){
        a <- suppressWarnings(utils::download.file(url = url_item, destfile = dest_item, quiet = !verbose))
        if(verbose) print(a)
      } else {
        if(verbose) {
          print("url item not character or length greater than one")
          print(url_item)
        }
      }
    })
    
    # Create and return appropriate path
    if(use_relative_paths && !is.null(base_path)) {
      # Return path relative to base_path
      rel_path <- file.path(attachment_folder, table_name, field, filename_dest)
      return(rel_path)
    } else {
      return(dest_full)
    }
  })
  
  if(verbose) {
    down_load_message <- glue::glue("Files downloaded to {download_dir}")
    message(down_load_message)
  }
  
  # Changed from "_file_paths" to ".file_paths"
  field_file_paths <- sprintf("%s.file_paths", field)
  
  x$file_path <- xlist
  
  # using dynamic names in case a base has multiple file attachment
  # columns
  x <- dplyr::rename(x, {{field_file_paths}} := file_path)
  
  # Remove the original field if requested
  if(remove_original_field) {
    x <- x[, !names(x) %in% field, drop = FALSE]
  }
  
  return(x)
}
