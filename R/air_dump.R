#' Get items that differ between x and y
#'
#' Unlike setdiff, this function creates the union of x and y then
#' removes values that are in the intersect, providing values
#' that are unique to X and values that are unique to Y.
#'
#' @param x a set of values.
#' @param y a set of values.
#'
#' @return Unique values from X and Y, NULL if no unique values.
#' @export
#'
#' @examples
#' a <- 1:3
#' b <- 2:4
#'
#' set_diff(a,b)
#' # returns 1,4
#'
#' x <- 1:3
#' y <- 1:3
#'
#' set_diff(x,y)
#' # returns NULL
#'
set_diff <- function(x,y){
  u <- union(x,y)
  i <- intersect(x,y)
  j <- (u %in% i)

  if(all(j)){
    return(NULL)
  }

  diff <- u[!(j)]
  return(diff)
}


#' Create a new structural metadata table in the base
#'
#' @details Structural metadata describes the contents of your base and how they are linked. Structural
#' metadata can largely be derived from the base schema.
#'
#' @param base String. Base id
#' @param meta_data Data frame. Contains metadata records. From air_generate_metadata*
#' @param table_name String. name of the metadata table. default is "Meta Data"
#' @param field_descriptions Character vector. Descriptions of metadata table fields
#' @param type Character vector. Column types for metadata table fields. see https://airtable.com/developers/web/api/field-model
#' @param options Data frame. Options for fields in metadata table.
#'
#' @return List with outcome from creating the table and inserting the records
#' @export air_create_metadata_table
#'
#' @examples
#'\dontrun{
#' # set base id
#' base <- "appXXXXXXXX"
#' # create metadata from api
#' metadata  <- air_generate_metadata_from_api(base)
#' # add Meta Data table to base -- will not work if base already has a metadata
#' # table
#' log <- air_create_metadata_table(base,metadata)
#'
#'}
#'
#'
air_create_metadata_table <- function(base,meta_data, table_name = "Meta Data",  field_descriptions = NA,
                                      type = "singleLineText", options = NA){

  # check for meta data table
  ## if exists, stop
  schema <- air_get_schema(base)

  if(table_name %in% schema$tables$name){
    msg <- glue::glue("{table_name} already exists in the base {base}.
                      Please use air_update_metadata_table to update the metadata table
                      or delete the table and re-run the function")
    stop(msg)
  }


  # create fields_df
  # add description for standard names


  #
  if(setequal(names(meta_data), c("field_name", "table_name", "field_desc",
                                  "field_type", "field_id",  "table_id",
                                  "field_opts",  "primary_key"))){

    # create description object
    field_descriptions <- c("https://schema.org/name",
                            "https://schema.org/name",
                            "https://schema.org/description",
                            "https://schema.org/category",
                            "https://schema.org/identifier",
                            "https://schema.org/identifier",
                            "https://schema.org/option",
                            "https://schema.org/Boolean"
    )


  }

  fields_df <- air_fields_df_template(name = names(meta_data),
                                      description = field_descriptions,
                                      type = type,
                                      options = options)

  # create list describing table

  table_list <- air_table_template(table_name = table_name,
                                   description = "structural metadata for the base",
                                   fields_df = fields_df)
  # create table

  outcome_create_table <- air_create_table(base, table_list)

  # insert data

  outcome_insert_data <-   tryCatch(
    air_insert_data_frame(base = base,table_name = table_name,records = meta_data),
    error=function(cond) {

      warning(cond)

      return("data not inserted")
    })

  if(is.character(outcome_insert_data)){
    stop("Table created but data not inserted. Check field types then use
         air_insert_data_frame or air_update_metadata_table to add metadata
         records.")
  }


  return(list("create_table" = outcome_create_table,
              "insert_data" = outcome_insert_data))

}

# add the description table to the base

#' Create the descriptive metadata table for the base
#'
#' Descriptive metadata provides information about the base as a whole, who created it,
#' why, when, where can data be accessed, keywords, what license governs data use, etc.
#' Descriptive metadata facilitates data reuse by providing a point of contact for
#' future users, as well as attributes that allow the data to be entered into searchable
#' catalogs or archives.
#'
#' @details DCMI terms can be found here \url{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/}
#'
#' @param base String. Base id
#' @param description Data frame. Description from air_get_base_description* or air_generate_base_description
#' @param table_name String. Name of description table
#' @param field_descriptions Character vector. Descriptions of metadata table fields. If NA, DCMI terms will be used where possible.
#' @param type Character vector. Column types for metadata table fields. see \url{https://airtable.com/developers/web/api/field-model}
#' @param options Data frame. Options for fields in metadata table.
#'
#'
#' @return List. Outputs from creating the table and inserting the records
#' @export air_create_description_table
#'
#' @examples
#'\dontrun{
#' base = "appVjIfAo8AJlfTkx"
#' table_name= "description"
#'
#' description <- air_generate_base_description(title = "Example Base",
#'                                              creator = "Collin Schwantes")
#'
#' air_create_description_table(base,description,table_name)
#'}
#'
air_create_description_table <- function(base,
                                         description,
                                         table_name = "Description",
                                         field_descriptions = NA,
                                         type = "singleLineText",
                                         options = NA){

  # check for description table
  ## if exists, stop
  schema <- air_get_schema(base)

  if(table_name %in% schema$tables$name){
    msg <- glue::glue("{table_name} already exists in the base {base}.
                      Please use air_update_description_table to update the description table
                      or delete the table and re-run the function")
    stop(msg)
  }


  # create fields_df
  # add description for dcmi terms
  # check for dcmi terms
  if(all(is.na(field_descriptions))){
    # create a placeholder dataframe
    description_terms_df <- data.frame(col_names = names(description), dcmi_term = NA)

    # setup sprintf pattern
    dcmi_uri <- "http://purl.org/dc/terms/%s"

    # check for a match for each dcmi term
    description_terms_df_2 <- purrr::map_dfr(deposits::dcmi_terms(), function(dcmi_term){

      # get index position for a term
      dcmi_term_pos  <- stringr::str_which(description_terms_df$col_names,pattern =dcmi_term,negate = FALSE)

      if(!rlang::is_empty(dcmi_term_pos)){

        description_terms_df[dcmi_term_pos,"dcmi_term"] <-  sprintf(dcmi_uri,dcmi_term)

        return(description_terms_df[dcmi_term_pos,])
      }

      return(NULL)
    })

    # get undescribed terms

    description_terms_df_3 <- dplyr::anti_join(description_terms_df,description_terms_df_2,"col_names")

    description_terms_df_4 <- rbind(description_terms_df_2,description_terms_df_3)

    description_terms_df_5 <- description_terms_df_4[match(names(description),description_terms_df_4$col_names),]

    field_descriptions <- description_terms_df_5$dcmi_term

  }

  fields_df <- air_fields_df_template(name = names(description),
                                      description = field_descriptions,
                                      type = type,
                                      options = options)

  # create list describing table

  table_list <- air_table_template(table_name = table_name,
                                   description = "descriptive metadata for the base",
                                   fields_df = fields_df)
  # create table

  outcome_create_table <- air_create_table(base, table_list)

  # insert data

  outcome_insert_data <-   tryCatch(
    air_insert_data_frame(base = base,table_name = table_name,records = description),
    error=function(cond) {

      warning(cond)

      return("data not inserted")
    })

  if(is.character(outcome_insert_data)){
    stop("Table created but data not inserted. Check field types then use
         air_insert_data_frame or air_update_description_table to add metadata
         records.")
  }


  return(list("create_table" = outcome_create_table,
              "insert_data" = outcome_insert_data))

}


#' Update the structural metadata table
#'
#' @param base String. Base id
#' @param meta_data Data frame. Contains metadata records. From air_generate_metadata*
#' @param table_name String. Name of metadata table
#' @param join_field String. Name of field to join new and current metadata. Likely \code{field_id}
#' @param record_id_field String. Name of record id field. Like \code{id}
#'
#' @return List. Log of results for updating metadata
#' @export air_update_metadata_table
#'
#' @examples
#'\dontrun{
#' base = "appVjIfAo8AJlfTkx"
#' metadata <- air_generate_metadata_from_api(base = base)
#' air_update_metadata_table(base,metadata)
#'}
#'
air_update_metadata_table <- function(base,meta_data,table_name = "Meta Data", join_field = "field_id", record_id_field = "id"){

  schema <- air_get_schema(base)

  # check for Meta Data table
  check_for_md_table <-  schema$tables$name %in% table_name
  ## if no meta data table, stop


  if(!any(check_for_md_table)){
    msg <- glue::glue("No table called {table_name} in base {base}.
                      Please use air_create_metadata_table to create the metadata table
                      or create it manually.")
    stop(msg)
  }

  ## get table_id

  table_id <- schema$tables[schema$tables$name == table_name,"id"]

  # pull down current meta data table

  current_metadata_table <- fetch_all(base, table_name)

  #create any new fields from meta_data

  message("creating log")
  update_log <- list(
    fields_created = NA,
    records_updated = NA,
    records_inserted = NA,
    records_deleted = NA
  )


  message("checking if any fields need to be added")

  # use schema in case table is empty
  current_col_names <- schema$tables$fields[check_for_md_table][[1]]$name
  col_check <- !names(meta_data) %in% current_col_names

  if(all(col_check)){
    msg <- glue::glue("The meta_data object and the metadata table in your base, {table_name}, share
                      no fields.")
    warning(msg)
  }

  if(any(col_check)){
    message("creating missing fields")
    cols_to_create <- names(meta_data)[col_check]

    fields_created  <- air_create_field(base = base,
                                        table_id = table_id,
                                        name = cols_to_create)

    message(fields_created)

    update_log$fields_created  <- fields_created

  }

  # if the current metadata table is empty, then insert records
  if(is.character(current_metadata_table)){
    message("added new records")
    records_to_insert <- meta_data

    records_inserted <- air_insert_data_frame(base, table_name,records_to_insert)

    update_log$records_inserted <- records_inserted

    return(update_log)
  }

  # compare with updated values
  ## use field ids
  message("checking which fields need to be updated")
  ## assumes a certain structure for metadata

  min_update_df <- current_metadata_table[,c(join_field,record_id_field)]

  records_to_update <- dplyr::inner_join(meta_data,min_update_df,by = join_field )

  # update records
  message("updating records")
  records_updated <- air_update_data_frame(base, table_name, records_to_update$id,records_to_update)

  update_log$records_updated <- records_updated

  # insert new records
  message("added new records")
  records_to_insert <- dplyr::anti_join(meta_data,min_update_df,by = join_field)

  records_inserted <- air_insert_data_frame(base, table_name,records_to_insert)

  update_log$records_inserted <- records_inserted


  # drop records no longer in meta data
  message("Checking for records no longer in the base")

  records_to_delete <- dplyr::anti_join(min_update_df,meta_data,by = join_field)


  if(nrow(records_to_delete) >0){
    message("Deleting records no longer in the base")
    records_deleted <- purrr::map(records_to_delete$id, function(id){
      air_delete(base, table_name,id)
    })
  } else {
    message("No Records deleted")
    records_deleted <- "No records deleted"
  }

  update_log$records_deleted <- records_deleted


  return(update_log)
}


#' Update the description table
#'
#' Update the descriptive metadata table in airtable
#'
#' @param base String. Base id
#' @param description Data frame. Contains updated description
#' @param table_name  String. Name of description table
#' @param join_field String. Field to perform join on
#' @param record_id_field String. Name of the record id field
#'
#' @return list that logs updates
#' @export air_update_description_table
#'
#' @examples
#'
#' \dontrun{
#'
#' base <- "appXXXXXXXX"
#' table_name <- "Description"
#' # get description from table
#' description  <- air_get_base_description_from_table(base, table_name)
#' # update the identifier field
#' description$identifier <- "fake.doi.xyz/029940"
#' # update the table
#' air_update_description_table(base,description)
#'
#'
#' }
#'
#'
air_update_description_table <- function(base,description, table_name = "Description", join_field = "title", record_id_field = "id"){


  # check for description
  schema <- air_get_schema(base)

  # check for Description table
  ## if no table, stop

  if(!table_name %in% schema$tables$name){
    msg <- glue::glue("No table called {table_name} in base {base}.
                      Please use air_create_description_table to create the descriptive
                      metadata table or create it manually.")
    stop(msg)
  }

  ## create empty update log list

  update_log <- list(
    fields_created = NA,
    records_updated = NA,
    records_inserted = NA,
    records_deleted = NA
  )


  ## get table_id

  table_id <- schema$tables[schema$tables$name == table_name,"id"]
  table_pos <- which(schema$tables$id == table_id)
  # get column names from schema because empty columns are pulled
  table_columns <- schema$tables$fields[[table_pos]]$name
  ## check columns
  col_check <- !names(description) %in% table_columns

  if(all(col_check)){
    msg <- glue::glue("The description object and the metadata table in your base, {table_name}, share
                      no fields.")
    warning(msg)
  }
  #create any new fields from description
  if(any(col_check)){
    cols_to_create <- names(description)[col_check]

    fields_created  <- air_create_field(base = base,
                                        table_id = table_id,
                                        name = cols_to_create)

    message(fields_created)

    update_log$fields_created  <- fields_created

  }

  # pull down current table

  current_metadata_table <- fetch_all(base, table_name)
  # convert to tibble for more consistent behavior in joins
  current_metadata_table<- tibble::as_tibble(current_metadata_table)
  current_metadata_table <- current_metadata_table |>
    dplyr::select(-createdTime)

  # compare with updated values
  ## use field ids
  ## assumes a certain structure for metadata
  min_fields <- unique(join_field,record_id_field)

  min_update_df <- current_metadata_table[,min_fields]

  records_to_update <- dplyr::inner_join(description,min_update_df,by = join_field )

  # update records

  records_updated <- air_update_data_frame(base, table_name, records_to_update$id,records_to_update)

  update_log$records_updated <- records_updated

  # insert new records -- each table describes a single base so there should be
  # no records to insert

  records_to_insert <- dplyr::anti_join(description,min_update_df,by = join_field)

  if(nrow(records_to_insert) > 0){
    warning("Inserting a new record into the base. Check that the value in {join_field} matches
            between current and updated description")
  }

  records_inserted <- air_insert_data_frame(base, table_name,records_to_insert)

  update_log$records_inserted <- records_inserted


  # drop records no longer in meta data

  records_to_delete <- dplyr::anti_join(min_update_df,description,by = join_field)


  if(nrow(records_to_delete) >0){
    records_deleted <- purrr::map(records_to_delete$id, function(id){
      air_delete(base, table_name,id)
    })
  } else {
    records_deleted <- "No records deleted"
  }

  update_log$records_deleted <- records_deleted


  return(update_log)

}


#' Pull the metadata table from Airtable
#'
#' Airtable allows all users to access the metadata API.
#' The recommended workflow for creating this table is to use
#' air_generate_metadata_from_api to extract the structural metadata from the base
#' schema and then use air_create_metadata_table to add the table to your
#' base.
#'
#' For information about creating metadata tables in your base see the
#' \href{https://ecohealthalliance.github.io/eha-ma-handbook/8-airtable.html#managing-data}{EHA MA Handbook}
#'
#' @details Requires the following fields: table_name, field_name
#'
#' @param base String. ID for your base from Airtable. Generally 'appXXXXXXXXXXXXXX'
#' @param table_name String. Name of structural metadata table - the metadata that
#' describes how tables and fields fit together.
#' @param add_id_field Logical. If true, an "id" field is added to each table
#' @param field_names_to_snakecase Logical. If true, values in the field_names
#' column and the field in the metadata table themselves are are converted to snake_case
#'
#' @return data.frame with metadata table
#' @export air_get_metadata_from_table
#'
air_get_metadata_from_table <- function(base, table_name, add_id_field = FALSE, field_names_to_snakecase = TRUE){
  # get structural metadata table
  str_metadata <- fetch_all(base, table_name)

  # get original table names
  str_md_names <- names(str_metadata)

  ## check for table_name, field_name
  names(str_metadata) <- snakecase::to_snake_case(str_md_names)

  required_fields <- c("table_name","field_name")
  if(!all(required_fields %in% names(str_metadata))){
    stop(glue::glue("metadata table must contain the
                    following fields: {required_fields}. Note
                    that field names are converted to snakecase
                    before check."))
  }

  ## make field names snake_case
  if(field_names_to_snakecase){
    str_metadata$field_name <- snakecase::to_snake_case(str_metadata$field_name)
  }

  ## add id field to all tables
  if(add_id_field){

    tables <- dplyr::distinct(.data = str_metadata,table_name,.keep_all = TRUE)
    tables$field_desc <- "unique id assigned by airtable"
    tables$field_type <- "singleLineText"
    tables$field_id <- NA
    tables$field_opts <- NA

    str_metadata <- rbind(str_metadata,tables)

  }

  if(!field_names_to_snakecase){
    names(str_metadata) <- str_md_names
  }

  return(str_metadata)
}


#' Generate structural metadata from the api
#'
#' Structural metadata describes the contents of your base and how they are linked.
#' The structural metadata are created from the base schema. The nested schema
#' structure is flattened into a more user-friendly table which can then be
#' inserted as a table into the base with \code{air_created_metadata_table} and/or
#' used in a data export with \code{air_dump}.
#'
#' @details This function requires that the api token has the ability to read
#' the base schema.
#'
#' @param base String. Base id
#' @param metadata_table_name String. Name of exisiting structural metadata table if it exists
#' @param include_metadata_table Logical. Should the structural metadata table be included in the metadata?
#' @param field_names_to_snake_case Logical. Should the field names in the metadata table be snake_case?
#'
#' @return A data frame with metadata
#' @export air_generate_metadata_from_api
#'
#' @examples
#'
#' \dontrun{
#'
#' base <- "appXXXXXXXX"
#' metadata  <- air_generate_metadata_from_api(base)
#'
#' }

air_generate_metadata_from_api <- function(base,
                                           metadata_table_name = "Meta Data",
                                           include_metadata_table = FALSE,
                                           field_names_to_snake_case = TRUE){

  # get base schema
  schema <- air_get_schema(base)

  tables_df <- schema$tables

  if(!include_metadata_table){
    tables_df <- tables_df[stringr::str_detect(tables_df$name,
                                               pattern = metadata_table_name,
                                               negate = TRUE),]
  }

  # parse base schema to populate metadata table

  #split by table id to parse with purrr
  schema_list <- split(tables_df,f = tables_df$id)

  metadata_df <- purrr::map_dfr(schema_list, function(x){

    # create metadata table skeleton
    fields_df <-x$fields[[1]]

    fields_df$choices <- ""
    fields_df$linkedTableID <- ""

    # get flattened choice names
    if(!rlang::is_empty(fields_df$options$choices)){
      fields_df$choices <- purrr::map_chr(fields_df$options$choices,function(x){
        if(is.null(x)){
          return("")
        } else {
          return(paste(x$name,collapse = ", "))
        }
      })
    }

    # get linked table id
    if(!rlang::is_empty(fields_df$options$linkedTableId)){
      fields_df$linkedTableID <- fields_df$options$linkedTableId
    }


    fields_df <- fields_df |>
      dplyr::mutate(field_opts =
                      dplyr::case_when(
                        type == "multipleSelects" | type == "singleSelect" ~ choices,
                        type == "multipleRecordLinks" ~ linkedTableID,
                        TRUE ~ ""
                      )
      )





    # check that descriptions arent empty

    if(rlang::is_empty(fields_df$description)){
      fields_df$description <- ""
    }

    md_df <- data.frame(field_name = fields_df$name,
                        table_name = x$name,
                        field_desc = fields_df$description,
                        field_type = fields_df$type,
                        field_id = fields_df$id,
                        table_id = x$id,
                        field_opts = fields_df$field_opts,
                        primary_key = as.character(x$primaryFieldId == fields_df$id)
    )

    if(!field_names_to_snake_case){
      names(md_df) <- c("Field Name","Table Name", "Field Desc","Field Type",
                        "Field ID", "Table ID", "Field Opts", "Primary Key")
    }

    return(md_df)
  })

  return(metadata_df)
}

##air_insert

#' Generated Metadata from table names
#'
#' Deprecated: Use \code{air_generate_metadata_from_api}
#'
#' Generates a structural metadata table - the metadata that
#' describes how tables and fields fit together. Does not
#' include field types.
#'
#' @details For information about creating metadata tables in your base see the
#' \href{https://ecohealthalliance.github.io/eha-ma-handbook/8-airtable.html#managing-data}{EHA MA Handbook}
#'
#' @param base String. ID for your base from Airtable. Generally 'appXXXXXXXXXXXXXX'
#' @param table_names Vector of strings. The names of your tables. eg c("table 1","table 2", etc.)
#' @param limit Number from 1-100. How many rows should we pull from each table to create the metdata?
#'  Keep in mind that the airtable api will not return fields with "empty" values - "", false, or [].
#'  Code runs faster if fewer rows are pulled.
#'
#' @return data.frame with structural metadata.
#' @export air_generate_metadata_from_tables

air_generate_metadata_from_tables <- function(base, table_names,limit=1){
  warning('Deprecated: For more complete results, use air_generate_metadata_from_api.
  Airtable does not return fields with empty values - "", false, or [].')
  meta_data_table <- purrr::map_dfr(table_names,function(x){
    table_x <- air_get(base,x,limit = limit )
    fields_x <- names(table_x)

    ## guess record types?

    md_df <- data.frame( field_name = fields_x, table_name = x, field_desc = "", field_type = "")

    return(md_df)
  })

  return(meta_data_table)
}

#' Get base description from table
#'
#' Pull a table that has descriptive metadata.
#' Requires the following fields:
#' "title","primary_contact","email","description"
#'
#' @param base String. ID for your base from Airtable. Generally 'appXXXXXXXXXXXXXX'
#' @param table_name String. Name of descriptive metadata table - the metadata that
#' describes the base and provides attribution
#' @param field_names_to_snakecase Logical. Should field names be converted to snakecase?
#'
#' @return data.frame with descriptive metadata.
#' @export air_get_base_description_from_table
#'
#' @examples
#' \dontrun{
#' base <- "appXXXXXXXX"
#' table_name <- "Description"
#' air_get_base_description_from_table(base, table_name)
#' }
air_get_base_description_from_table<- function(base, table_name,field_names_to_snakecase = TRUE){
  #fetch table
  desc_table <- fetch_all(base, table_name)
  # to snake case
  if(field_names_to_snakecase){
    names(desc_table) <- snakecase::to_snake_case(names(desc_table))
  }

  required_fields <- c("title","primary_contact","email","description")
  if(all(required_fields %in% names(desc_table))){
    return(desc_table)
  } else {

    missing_rf <- required_fields[!required_fields %in% names(desc_table)]

    desc_table[missing_rf] <- ""
    return(desc_table)
  }

}

#' Generate descriptive metadata
#'
#' Creates a data.frame that describes the base. Descriptive metadata provides
#' information about the base as a whole: who created it,
#' why, when, where can data be accessed, keywords, what license governs data use, etc.
#' Descriptive metadata facilitates data reuse by providing a point of contact for
#' future users, as well as attributes that allow the data to be entered into searchable
#' catalogs or archives.
#'
#' @details See  \href{https://www.dublincore.org/resources/userguide/creating_metadata/}{dublin core} for inspiration about additional attributes.
#'
#' @param title String. Title is a property that refers to the name or names by
#' which a resource is formally known.
#' @param creator String. Person or people who created the base
#' @param primary_contact String.  Person or entity primarily responsible for
#' making the content of a resource
#' @param email String. Email of primary_contact
#' @param description String. This property refers to the description of
#' the content of a resource. The description is a potentially rich source of
#' indexable terms and assist the users in their selection of an appropriate
#' resource.
#' @param contributor String. An entity responsible for making contributions to the resource.
#' @param identifier String. An unambiguous reference to the resource within a given context.
#' @param license String. A legal document giving official permission to do something with the resource. "CC BY 4.0"
#' @param ... String. Additional descriptive metadata elements. See details.
#' Additional elements can be added as name pair values e.g.
#' \code{ isPartOf = "https://doi.org/00.00000/MyPaper01", isReferencedBy = "https://doi.org/10.48321/MyDMP01"}
#' @param created String. When was the base created?
#'
#' @return data.frame with descriptive metadata
#' @export
#'
#' @examples
#'
#' air_generate_base_description(title = "My Awesome Base" ,
#'  primary_contact= "Base Creator/Maintainer",
#'  email = "email@@example.com",
#'  base_description = "This base contains my awesome data
#'  from a project studying XXX in YYY. Data in the base were collected
#'  from 1900-01-01 to 1990-01-01 by researchers at Some Long Term Project.",
#'  is_part_of = "https://doi.org/10.48321/MyDMP01",
#'  isReferencedBy = "https://doi.org/10.5072/zenodo_sandbox.1062705"
#'  )
#'
air_generate_base_description <- function(title = NA,
                                          creator= NA,
                                          created=NA,
                                          primary_contact=NA,
                                          email = NA,
                                          description = NA,
                                          contributor = NA,
                                          identifier =NA,
                                          license = NA,...){
  desc_table <- tibble::tibble(title = title,
                               creator= creator,
                               created=created,
                               primary_contact=primary_contact,
                               email = email,
                               description = description,
                               contributor = contributor,
                               identifier =identifier,
                               license = license,
                               ...)
  return(desc_table)
}

### extract_base - returns a named list

#' Dump all tables from a base into R
#'
#' @param base String. ID for your base from Airtable. Generally 'appXXXXXXXXXXXXXX'
#' @param metadata Data.frame.Data frame with structural metadata - describes relationship between tables and fields.
#' Can be left as NULL if base already contains a table called meta data.
#' @param description Data.frame. Data frame with descriptive metadata - describes whats in your base and who made it.
#' Can be left as NULL if base already contains a table called description.
#' @param add_missing_fields Logical. Should fields described in the metadata data.frame be added to corresponding tables?
#' @param download_attachments Logical. Should attached files be downloaded?
#' @param ... Additional arguments to pass to air_download_attachments
#' @param attachment_fields Optional. character vector.
#' What field(s) should files be downloaded from? Default is to download all fields
#' with type multipleAttachments in metadata.
#' @param field_names_to_snakecase Logical. Should field names be
#'  converted to snake case?
#' @param polite_downloads Logical. Use if downloading many files. Sets a delay
#' so that server is not overwhelmed by requests.
#'
#' @return List of data.frames. All tables from metadata plus the
#' description and metadata tables.
#' @export air_dump
#'
#' @note To facilitate joining on ids, see purrr::as_vector for converting list
#'  type columns to vectors and
#' tidyr::unnest for expanding list columns.
#'
air_dump <- function(base, metadata = NULL, description = NULL,
                     add_missing_fields = TRUE,
                     download_attachments = TRUE,
                     attachment_fields = NULL,
                     polite_downloads = TRUE,
                     field_names_to_snakecase = TRUE,
                     ...) {
  
  # If no metadata is provided, attempt to retrieve it from the base
  if(is.null(metadata)){
    message("No metadata provided. Metadata will be retrieved from the metadata table or generated via API")
    
    # Get the schema structure of the Airtable base
    base_schema <- air_get_schema(base)
    
    # Extract all table names from the schema
    all_table_names <- base_schema$tables$name
    
    # Look for a metadata table in the base (case insensitive)
    metadata_table_exists <- grepl("meta data", all_table_names, ignore.case = TRUE)
    
    if(any(metadata_table_exists)){
      # If metadata table exists, fetch it from the base
      message("Retrieving metadata from table")
      metadata <- fetch_all(base = base, table_name = all_table_names[metadata_table_exists])
    } else {
      # If no metadata table exists, generate metadata using the API
      message("Generating metadata from api with air_generate_metadata_from_api")
      metadata <- air_generate_metadata_from_api(base = base)
    }
  }
  
  # Convert all metadata column names to snake_case for consistency
  names(metadata) <- snakecase::to_snake_case(names(metadata))
  if(field_names_to_snakecase) metadata$field_name <- snakecase::to_snake_case(metadata$field_name)
  
  # Define and check for required fields in the metadata
  required_metadata_fields <- c("table_name", "field_name")
  
  if(!all(required_metadata_fields %in% names(metadata))){
    stop(glue::glue("metadata table must contain the 
                    following fields: {required_metadata_fields}. Note
                    that field names are converted to snakecase
                    before check."))
  }
  
  # Get unique table names from the metadata
  base_table_names <- unique(metadata$table_name)
  
  # Process each table in the base and create a named list of tables
  table_list <- base_table_names |>
    purrr::set_names() |>
    purrr::map(function(table_name) {
      
      # Fetch all records from the current table
      current_table <- fetch_all(base, table_name)
      if(is.null(current_table)) return(NULL)

      # Get expected field names for this table from metadata
      expected_fields <- metadata[metadata$table_name == table_name, "field_name"]

      # Convert field names to snake_case if option is enabled
      if(field_names_to_snakecase) {
        names(current_table) <- snakecase::to_snake_case(names(current_table))
        expected_fields <- snakecase::to_snake_case(expected_fields)
      }
      
      # If table is empty or doesn't exist, create an empty dataframe with an id column
      if(!is.data.frame(current_table)){
        current_table <- data.frame(id = character())
      }
      
      # Identify observed fields (column names) in the fetched table
      observed_fields <- names(current_table)
      
      # Find differences between expected and observed fields
      fields_difference <- setdiff(expected_fields, observed_fields)
      
      # Handle field discrepancies if any exist
      if(!is.null(fields_difference)){
        # Check for fields in table but not in metadata (observed but not expected)
        fields_in_table_not_in_metadata <- setdiff(observed_fields, expected_fields)
        
        # Define system fields that are allowed to exist without being in metadata
        system_fields <- c("id", "createdTime", "created_time")
        system_fields_pattern <- paste(system_fields, collapse = "|")
        
        # If there are non-system fields in the table that aren't in metadata, throw an error
        if(length(fields_in_table_not_in_metadata) != 0 && 
           !all(fields_in_table_not_in_metadata %in% system_fields)){
          
          # Filter out system fields from the list of missing metadata fields
          undocumented_fields <- fields_in_table_not_in_metadata[
            !grepl(system_fields_pattern, fields_in_table_not_in_metadata, ignore.case = FALSE)
          ]
          
          # Format the list of undocumented fields for the error message
          undocumented_fields_formatted <- paste(undocumented_fields, collapse = "\n")
          
          # Stop execution with informative error message
          stop(glue::glue('The metadata table is missing the following fields from table {table_name}:

                          {undocumented_fields_formatted}

                          Please update the metadata table via R with air_update_metadata_table
                          or manually at https://airtable.com/{base}'))
        }
        
        # Add fields that exist in metadata but not in table, if requested
        if(add_missing_fields){
          fields_in_metadata_not_in_table <- setdiff(expected_fields, observed_fields)
          
          # Add empty columns for fields that are missing from the table
          current_table[fields_in_metadata_not_in_table] <- list(character(0))
        }
      }
      
      # Process attachments if enabled
      if(download_attachments){
        
         #Create a local copy of attachment_fields for this table iteration
        table_attachment_fields <- attachment_fields

          # Verify we can identify which fields contain attachments
        if(rlang::is_empty(table_attachment_fields) && !"field_type" %in% names(metadata)){
          rlang::abort("Unclear which fields contain attachments.
                      Either use the attachment_fields argument or
                      supply a metadata dataframe with field_types == 'multipleAttachments' for
                      fields that should be downloaded")
        } 

        # Determine which fields contain attachments
        if(is.null(table_attachment_fields)) {
        
          table_metadata <- metadata[metadata$table_name == table_name, c("field_name", "field_type")]
          
          # Extract fields of type 'multipleAttachments'
          table_attachment_fields <- table_metadata |>
            dplyr::filter(field_type == "multipleAttachments") |>
            dplyr::pull(field_name)
          
          # Inform user if no attachment fields are found
          if(rlang::is_empty(table_attachment_fields)) {
            rlang::inform("No fields of type multipleAttachment. No files to download")
            return(current_table)
          }
        }
        
        # Verify the attachment fields exist in the table
        if(is.character(table_attachment_fields)){
          if(!any(table_attachment_fields %in% names(current_table))) {
            return(current_table)
          }
        }

        # Configure download delay for polite API usage
        download_delay <- 0
        if(polite_downloads) {
          download_delay <- 0.01  # 10ms delay between downloads
        }
        
        # Process each attachment field
        for(attachment_field in table_attachment_fields){
          Sys.sleep(download_delay)
          # Download attachments for the current field
          current_table <- air_download_attachments(current_table,
                                                    field = attachment_field,
                                                    table_name = snakecase::to_snake_case(table_name), 
                                                    ...)
        }
      }
      
      return(current_table)
    })
  
  # Add metadata to the result list
  table_list$metadata <- metadata
  
  # Check if there's a description table in the results
  has_description_table <- grepl(pattern = "description", x = names(table_list), ignore.case = TRUE)
  
  # Handle description data based on parameters and existing data
  if(!is.null(description)){
    if(any(has_description_table)){
      warning("Base has a description table and a description data.frame was supplied to
              this function. Inserting description data.frame at $description. Table
              extract may be overwritten.")
    }
    table_list$description <- description
  } else {
    # If no description provided and none exists in the results, generate a default one
    if(all(!has_description_table)){
      table_list$description <- air_generate_base_description()
    }
  }
  
  # Return the complete list of tables, metadata, and description
  return(table_list)
}

#' Flatten list columns to character
#'
#' Similar in spirit to purrr::flatten_chr except
#' that it can handle NULL values in lists and returns outputs
#' that can be written to csv.
#'
#' @details
#' Because the outputs are intended for use in CSV files, we must use
#' double quotes to indicate that the commas separating list values do
#' not delimit cells. This conforms to RFC 4180 standard for CSVs.
#' \url{https://datatracker.ietf.org/doc/html/rfc4180}
#'
#'
#' @param data_frame a data frame, tibble or other data frame like object
#'
#' @return data_frame with list columns converted to character vectors.
#' @export
#'
#' @examples
#'
#' data_frame <- data.frame(a = I(list(list("Hello"),
#' list("Aloha"),
#' NULL,
#' list("Hola","Bonjour","Merhaba")
#' )),
#' b = 1:4,
#' c = letters[1:4],
#' d = I(data.frame(id = 1:4, name = "bob", email = "bob@@example.com"))
#' )
#'
#' test_df <- flatten_col_to_chr(data_frame)
#'
#' str(test_df)
#'
flatten_col_to_chr <- function(data_frame){
  for(i in names(data_frame)){
    #browser()
    # get column values
    col_from_df <- data_frame[[i]]

    if(is.list(col_from_df)){
      ## create an object to hold character values
      chr_col <- as.character()
      if(is.data.frame(col_from_df)){

        n_r <- nrow(col_from_df)

        for(j in 1:n_r){
          list_element<- col_from_df[j,]
          if(is.null(list_element)){
            list_element <- ""
          }

          row_value<- sprintf('"%s"',paste(list_element,collapse = ","))

          chr_col <- append(chr_col,row_value)
        }

      } else {
        n <- length(col_from_df)

        for(j in 1:n){
          list_element<- col_from_df[[j]]
          if(is.null(list_element)){
            list_element <- ""
          }
          row_value<- sprintf('"%s"',paste(list_element,collapse = ","))

          chr_col <- append(chr_col,row_value)
        }

      }


      data_frame[i] <- chr_col
    }
  }
  return(data_frame)
}



#' Save air_dump output to csv
#'
#' Saves data.frames from air_dump to csv files. File names are determined by
#' the names of the list objects from air_dump. Files will be saved in folder
#' with a unique name, inside the folder specified by \code{output_dir}. The
#' unique name is generated from a hash of the air_dump output.
#'
#' @param table_list List. List of data.frames output from \code{air_dump}
#' @param output_dir String. Folder containing output files
#' @param overwrite Logical. Should outputs be overwritten if they already exist?
#' @param attachments_dir String. What folder are base attachments stored in?
#' @param output_id String. Optional identifier for the data set - if NULL an
#' ID will be generated using a hash of the data.
#' @param names_to_snake_case Logical. Should field and table names be converted to snake_case?
#'
#' @return Vector of file paths
#' @export
air_dump_to_csv <- function(table_list, output_dir = "outputs",
                            attachments_dir = NULL,
                            overwrite = FALSE,
                            output_id = NULL,
                            names_to_snake_case = TRUE) {
  
  # create a unique id for the data
  if (is.null(output_id)) {
    output_id <- rlang::hash(table_list)
  }
  
  # check if data already exist
  output_dir_path_final <- sprintf("%s/%s", output_dir, output_id)
  if (all(dir.exists(output_dir_path_final)) && !overwrite) {
    message("Data already exists, files not written. Set overwrite to TRUE")
    return(list.files(output_dir_path_final, full.names = TRUE))
  }
  
  # create temp dir
  temp_path <- tempdir()
  output_dir_path <- sprintf("%s/%s", temp_path, output_id)
  
  # Create temp directory if it doesn't exist
  if (!dir.exists(output_dir_path)) {
    dir.create(output_dir_path, recursive = TRUE)
  }
  
  if (!dir.exists(output_dir_path)) {
    stop(glue::glue("Failed to create temporary directory: {output_dir_path}"))
  }
  
  # Process each table
  file_paths <- purrr::map2_chr(table_list, names(table_list), function(x_table, y_table_name) {
    
    # MINIMAL FIX: Skip if x_table is NULL or names(x_table) is NULL
    if (is.null(x_table) || is.null(names(x_table)) || !is.data.frame(x_table)) {
      message(paste("Skipping", y_table_name, "- not a valid data frame or missing column names"))
      return(NA_character_)
    }

    if (names_to_snake_case) {
      ##  clean table name
      y_table_name_snake <- snakecase::to_snake_case(y_table_name)
      ## clean up field names in table
      x_names_snake <- snakecase::to_snake_case(names(x_table))
      
      ## check that we didn't create duplicate field names
      dup_check <- duplicated(x_names_snake)
      
      if (any(dup_check)) {
        warn_msg <- glue::glue("The following field names in table {y_table_name} are duplicated after converting to snake_case:
                             \n
                             {paste(names(x_table)[which(dup_check)], collapse = ', ')}\n
                             attempting to fix automatically")
        print(warn_msg)
        warning(warn_msg)
        
        # Use make.unique to create unique names while preserving the original names where possible
        x_names_snake <- make.unique(x_names_snake, sep = ".")
      }
      
      y_table_name <- y_table_name_snake
      names(x_table) <- x_names_snake
    }
    
    # Sanitize table name further by replacing problematic characters
    y_table_name_sanitized <- gsub("[/ ]", "_", y_table_name)
    
    x_table_flat <- flatten_col_to_chr(x_table)
    
    ## export to CSV
    output_file_path <- sprintf("%s/%s.csv", output_dir_path, y_table_name_sanitized)
    
    # Create parent directory if needed
    parent_dir <- dirname(output_file_path)
    if (!dir.exists(parent_dir)) {
      dir.create(parent_dir, recursive = TRUE)
    }
    
    tryCatch({
      write.csv(x_table_flat, output_file_path, row.names = FALSE)
      message(glue::glue("Successfully created temporary file for {y_table_name_sanitized}"))
    }, error = function(e) {
      warning(glue::glue("Failed to write {y_table_name_sanitized} to temporary file: {e$message}"))
    })
    
    return(output_file_path)
  })
  
  ## Create final output directory
  if (!all(dir.exists(output_dir_path_final))) {
    dir.create(output_dir_path_final, recursive = TRUE)
    
    if (!all(dir.exists(output_dir_path_final))) {
      stop(glue::glue("Failed to create output directory: {output_dir_path_final}"))
    }
  }
  
  ## Copy from temp to final
  outputs_list <- list.files(output_dir_path, full.names = TRUE)
  
  # Copy files and handle errors
  copied_files <- sapply(outputs_list, function(src_file) {
    dest_file <- file.path(output_dir_path_final, basename(src_file))
    
    if (file.exists(dest_file) && !overwrite) {
      message(glue::glue("File {dest_file} already exists and overwrite=FALSE. Skipping."))
      return(dest_file)
    }
    
    copy_result <- tryCatch({
      file.copy(from = src_file, to = dest_file, overwrite = overwrite)
      message(glue::glue("Successfully copied {basename(src_file)} to final location"))
      dest_file
    }, error = function(e) {
      warning(glue::glue("Failed to copy {basename(src_file)}: {e$message}"))
      NA
    })
    
    return(copy_result)
  })
  
  ## Copy attachments into folder if specified
  if (!is.null(attachments_dir)) {
    if (!dir.exists(attachments_dir)) {
      warning(glue::glue("Attachments directory {attachments_dir} does not exist. Skipping attachment copy."))
    } else {
      message("Copying attachments")
      attachments_dest <- file.path(output_dir_path_final, basename(attachments_dir))
      
      tryCatch({
        file.copy(from = attachments_dir, to = output_dir_path_final, recursive = TRUE, copy.mode = TRUE)
        message(glue::glue("Successfully copied attachments to {attachments_dest}"))
      }, error = function(e) {
        warning(glue::glue("Failed to copy attachments: {e$message}"))
      })
    }
  }
  
  # Return list of all files in the final directory
  final_files <- list.files(output_dir_path_final, full.names = TRUE)
  return(final_files)
}


### extract_base - returns a named list

#' Dump all tables from a base into json files
#'
#' Essentially air_get without converting to Rs. Does not add fields with empty
#' values.
#'
#' @param base String. ID for your base from Airtable. Generally 'appXXXXXXXXXXXXXX'
#' @param metadata Data.frame.Data frame with structural metadata - describes relationship between tables and fields.
#' @param description Data.frame. Data frame with descriptive metadata - describes whats in your base and who made it.
#' Can be left as NULL if base already contains a table called description
#' @param output_dir String. Where should json files be saved?
#' @param overwrite  Logical. If data are not unique, should files be overwritten?
#'
#' @return List of data.frames. All tables from metadata plus the
#' description and metadata tables.
#' @export air_dump_to_json
#'
air_dump_to_json <- function(base, metadata, description = NULL, output_dir = "outputs", overwrite = FALSE) {
  
  names(metadata) <- snakecase::to_snake_case(names(metadata))
  
  ## check for required fields
  required_fields <- c("table_name")
  
  if (!all(required_fields %in% names(metadata))) {
    stop(glue::glue("metadata table must contain the
                    following field: {required_fields}. Note
                    that field names are converted to snakecase
                    before check."))
  }
  
  base_table_names <- unique(metadata$table_name)
  
  print(base_table_names)
  
  json_list <- base_table_names |>
    purrr::set_names() |>
    purrr::map(function(x) {
      ### no expected fields, just json
      ### see air_make_json for refactor
      x_json <- fetch_all_json(base, x)
      return(x_json)
    })
  
  json_list$metadata <- jsonlite::toJSON(metadata)
  
  # check for description table
  named_description <- grepl(pattern = "description", x = names(json_list), ignore.case = TRUE)
  
  if (!is.null(description)) {
    if (any(named_description)) {
      warning("Base has a description table and a description data.frame was supplied to
              this function. Inserting description data.frame at $description. Table
              extract may be overwritten.")
    }
    json_list$description <- jsonlite::toJSON(description)
  }
  
  ## does not add a description table if not present
  
  ## write to files
  output_id <- rlang::hash(json_list)
  
  output_dir_path <- sprintf("%s/%s", output_dir, output_id)
  
  # Create directory if it doesn't exist
  if (!dir.exists(output_dir_path)) {
    dir.create(output_dir_path, recursive = TRUE)
  }
  
  if (!dir.exists(output_dir_path)) {
    stop(glue::glue("Failed to create directory: {output_dir_path}"))
  }
  
  file_paths <- purrr::map2_chr(json_list, names(json_list), function(x_table, y_table_name) {
    # Sanitize table names by replacing problematic characters
    sanitized_table_name <- gsub("[/ ]", "_", y_table_name)
    
    output_file_path <- sprintf("%s/%s.json", output_dir_path, sanitized_table_name)
    
    # Check if file exists and respect overwrite parameter
    if (file.exists(output_file_path) && !overwrite) {
      message(glue::glue("File {output_file_path} already exists and overwrite=FALSE. Skipping."))
      return(output_file_path)
    }
    
    # Create parent directories if they don't exist
    parent_dir <- dirname(output_file_path)
    if (!dir.exists(parent_dir)) {
      dir.create(parent_dir, recursive = TRUE)
    }
    
    tryCatch({
      jsonlite::write_json(x_table, path = output_file_path)
      message(glue::glue("Successfully wrote {sanitized_table_name} to {output_file_path}"))
    }, error = function(e) {
      warning(glue::glue("Failed to write {sanitized_table_name} to {output_file_path}: {e$message}"))
    })
    
    return(output_file_path)
  })
  
  return(file_paths)
}



### write to db

### recover from metadata - JS code to regenerate tables
#
# air_js_for_tables <- function(metadata){
#   names(metadata) <- snakecase::to_snake_case(names(metadata))
#   table_names <- unique(metadata$table_name)
#
#  ## not sufficient for all field types, need to think more deeply about this
#   purrr::map(table_names, function(x){
#     field_names <- metadata[metadata$table_name == x,"field_name"]
#     field_types <- metadata[metadata$table_name == x,"field_type"]
#     create_field <- sprintf('{name: "%s", type: "%s"}',field_name,field_type)
#   })
#
# }



