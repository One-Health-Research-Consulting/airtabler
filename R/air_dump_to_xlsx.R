#' Export Airtable Dump to XLSX
#'
#' Takes the output from air_dump() and creates an Excel workbook with a worksheet
#' for each table. This makes it easy to reimport data into Airtable.
#'
#' @author Nathan C. Layman
#'
#' @param base_dump List. Output from air_dump() containing dataframes for each table
#' @param output_file String. Path to the output Excel file. If NULL, uses the base_name
#' @param base_name String. Name of the Airtable base, used in the filename if output_file is NULL
#' @param preserve_ids Logical. Whether to include ID columns to maintain relationships
#' @param clean_attachment_cols Logical. Whether to clean attachment columns for import
#' @param exclude_tables Character vector. Tables to exclude from the Excel file
#'
#' @return Path to the created Excel file
#' @export
#'
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
air_dump_to_xlsx <- function(base_dump,
                             output_file = NULL,
                             base_name = "airtable_base",
                             preserve_ids = TRUE,
                             clean_attachment_cols = TRUE,
                             exclude_tables = c()) {

  # Check if openxlsx is installed
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is needed for this function to work. Please install it.")
  }

  # Set output file path if not provided
  if (is.null(output_file)) {
    # Clean base_name for file system
    clean_base_name <- gsub("[^a-zA-Z0-9_-]", "_", base_name) |> 
    stringr::str_replace_all("_+", "_") |>
    stringr::str_squish()
    output_file <- paste0(clean_base_name, "_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
  }

  # Create a new workbook
  wb <- openxlsx::createWorkbook()

  # Process each table in the dump data
  for (table_name in names(base_dump)) {
    # Skip excluded tables
    if (table_name %in% exclude_tables) {
      next
    }

    # Skip if not a dataframe (might be metadata information)
    if (!is.data.frame(base_dump[[table_name]])) {
      message(paste("Skipping", table_name, "- not a dataframe"))
      next
    }

    # Clone the dataframe to avoid modifying the original
    # Format the data for export
    table_data <- process_table_for_excel(base_dump[[table_name]])

    # Clean sheet name for Excel (max 31 chars, no special chars)
    sheet_name <- gsub("[^a-zA-Z0-9_-]", "_", table_name) |>
    stringr::str_replace_all("_+", "_") |>
    stringr::str_squish() |>
    stringr::str_trunc(30)

    # Create worksheet and write data
    tryCatch(
      {
        openxlsx::addWorksheet(wb, sheet_name)
        openxlsx::writeData(wb, sheet_name, table_data)
        message(paste("Added worksheet for", table_name))
      },
      error = function(e) {
        warning(paste("Failed to add worksheet for", table_name, ":", e$message))
      }
    )
  }

  # Create directory if it doesn't exist
  output_dir <- dirname(output_file)
  if (!dir.exists(output_dir) && output_dir != ".") {
    dir_created <- dir.create(output_dir, recursive = TRUE)
    if (dir_created) {
      message(paste("Created directory:", output_dir))
    } else {
      warning(paste("Failed to create directory:", output_dir))
    }
  }

  # Save the workbook
  tryCatch(
    {
      openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)
      message(paste("Excel file saved to", output_file))
    },
    error = function(e) {
      stop(paste("Failed to save workbook:", e$message))
    }
  )

  return(output_file)
}
