devtools::install_github("n8layman/airtabler")
library(airtabler)
library(tidyverse)

# Get list of bases your token has access to
eha_bases <- air_list_bases() |> pluck("bases")
name <- eha_bases[35,]$name
id <- eha_bases[35,]$id

base_output_dir <- "/Users/nathanlayman/EHA Dropbox/Nathan Layman/airtable_export/bases/"

pwalk(eha_bases, function(id, name, permissionLevel) {
  print(stringr::str_squish(name))
  cleaned_base_name <- gsub(" ", "_", stringr::str_squish(name))
  output_dir <- paste0(base_output_dir, cleaned_base_name, "_", id)

  base_metadata <- air_generate_metadata_from_api(id)
  base_dump <- air_dump(id,
                        metadata = base_metadata,
                        base_path = output_dir,
                        attachment_folder = "attachments",
                        overwrite = FALSE, # Whether to overwrite attachment files or not
                        remove_original_field = TRUE, # Re-write attachment column as relative file location and clean up raw column
                        organize_by_table_field = TRUE) # All attachments saved in one folder or nested by table and field

  output_file <- paste0(output_dir, "/", cleaned_base_name)
  
  # Save dump as an .xlsx file
  # Note: excel has a ~30,000 character limit per cell which can crop things like sequence data
  # The next two options are better for large data
  air_dump_to_xlsx(base_dump, output_file = paste0(output_file, ".xlsx"), base_name = name)

  # Save dump as an R object for easier import
  saveRDS(base_dump, file = paste0(output_file, ".rds"))

  # Save dump as a series of flat csv files in a nested folder
  air_dump_to_csv(base_dump,
                  output_dir = output_dir,
                  attachments_dir = NULL,
                  overwrite = TRUE,
                  output_id = paste(cleaned_base_name, "csv_files", sep = "_"),
                  names_to_snake_case = TRUE)

})


# # Render a plain Markdown file to PDF
# rmarkdown::render("your_document.md", 
#                   output_format = "pdf_document",
#                   output_file = "your_document.pdf")
