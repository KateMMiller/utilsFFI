#' @title importData
#'
#' @description Imports data from FFI SQL Server database or csvs of FFI database
#' tables in the 'dbo' schema. Currently can only import from a local installation
#' of a park's FFI database in SQL Server Management Studio (SSMS), but the goal is to
#' add an option for importing directly from the SQL Server where the production
#' FFI databases are housed. Currently only works with one park/project at a time,
#' but could be extended to import multiple parks/projects (see importData function
#' in katemmiller/plantcomNGPN).
#'
#' @importFrom dplyr collect mutate rename tbl
#' @importFrom purrr flatten map set_names
#'
#' @param type Indicate how to import the database tables
#' \describe{
#' \item{"local"}{Import tables in 'dbo' schema from the local installation of an
#' FFI database in SQL Server Management Studio (SSMS).}
#' \item{"server"}{[**NOT YET ENABLED**] Import tables in 'dbo' schema from the
#' FFI database on the production SQL Server).}
#' \item{"csv"}{Import a zip file of csv FFI tables.}
#'  }
#'
#' @param dbname Quoted name of database matching the name of the database as it
#' appears in SQL Server Management Studio
#' (eg. "FFI_RA_AGFO").
#'
#' @param import_path Quoted string to import a zipped file of csvs if type = 'csv'.
#' The name of the zipped file should be included in the path.
#'
#' @param new_env If TRUE (Default), will assign tables to an environment called FFI_tables.
#' If FALSE, will assign tables to the global environment.
#'
#' @examples
#' \dontrun{
#' #--- From Local install of FFI SQL databases
#' # Import data for AGFO
#' library(utilsFFI)
#' importData(dbname = "FFI_RA_AGFO")
#'
#' # Import data into global environment instead of FFI_tables environment
#' importData(dbname = "FFI_RA_BADL", new_env = F)
#'
#' # Import data as a zip file of csvs
#' importData(type = "csv", import_path = "C:/temp/FFI_table_export_20250624.zip")
#'
#' # Check that the import worked by listing names of tables in FFI_tables
#' names(FFI_tables)
#'
#' # View data in the MacroPlot table
#' head(FFI_tables$MacroPlot)
#' str(FFI_tables$MacroPlot)
#'
#' # Make the macro table in FFI_RA_AGFO a separate data frame
#' macro <- get("MacroPlot", envir = FFI_tables)
#' head(macro)
#' }
#'
#' @returns An environment with database tables as data frames for each imported database.
#'
#' @export
#'

importData <- function(type = "local", server = NA, dbname = "FFI_RA_AGFO",
                       import_path = NA, new_env = T){
  #---- Bug Handling ----
  type <- match.arg(type, c("local", "server", 'csv'))
  if(length(dbname) > 1){stop("Can only import 1 database at a time.")}
  if(any(is.na(dbname))){stop("Must specify a dbname to import tables from SMSS.")}
  stopifnot(is.logical(new_env))
  #++++++ Update as more features are added ++++++
  if(type %in% c("server")){stop(paste0("Sorry, type = ", type, " is not yet enabled."))}
  if(type == "server" & is.na(server)){stop("Must specify a server address if type = 'server'")}

  if(type == 'csv'){
    if(!any(file.exists(import_path))){
      stop(paste0("Specified import_path does not exist. ",
                  ifelse(any(grepl("sharepoint", import_path)), " Note that file paths from Sharepoint or Teams are not accessible.",
                         "")))}

    if(all(is.na(import_path))){stop("Must specify an import_path for type = 'csv'.")}
    if(any(!grepl(".zip$", import_path))){stop("Must include the name of the zip file in import_path.")} # add / to end of path if doesn't exist
    if(any(!file.exists(import_path))){stop("Specified import_path directory does not exist.")}
    # Normalize filepath for zip
    import_pathn <- normalizePath(import_path)
  }

  #--- Start the import ---
  if(new_env == TRUE){FFI_tables <<- new.env()}
  env <- if(new_env == TRUE){FFI_tables} else {.GlobalEnv}

  if(type == "local"){
    error_mess <- paste0("Unable to connect to specified SQL database. Make sure you have a local installation of the database in SSMS, ",
                         "and check that the database name is correct.")
      tryCatch(
        con <- odbc::dbConnect(odbc::odbc(),
                               Driver = "ODBC Driver 17 for SQL Server",
                               Server = "localhost\\SQLEXPRESS",
                               Database = dbname,
                               Trusted_Connection = "Yes"),
        error = function(e){stop(error_mess)},
        warning = function(w){stop(error_mess)})

      tbls <- DBI::dbListTables(con, schema = "dbo")

      # Setup progress bar
      pb <- txtProgressBar(min = 0, max = length(tbls), style = 3)

      # Import views using their names and show progress bar
      tbl_import <- lapply(seq_along(tbls), function(x){
        setTxtProgressBar(pb, x)
        tbl <- tbls[x]
        tab <- dplyr::tbl(con, dbplyr::in_schema("dbo", tbl)) |> dplyr::collect() |>
          as.data.frame() |> dplyr::mutate(datasource = dbname)
        return(tab)})

      tbl_import <- setNames(tbl_import, tbls)
      tbl_import2 <- tbl_import[sort(names(tbl_import))]
      # remove empty tables
      tbl_import3 <- tbl_import2[sapply(tbl_import2, nrow) > 0]

      list2env(tbl_import3, envir = env)
      DBI::dbDisconnect(con)
  } # end of type = local

  if(type == "server"){
    # NOT ENABLED
  }

  if(type == "csv"){
    # Pulling in only tables commonly used across NGPN parks
    csv_list1 <- c('AuxSpecies', 'Cover_Frequency_metric_Attribute', 'Cover_Frequency_metric_Sample',
                   'Cover_Points_metric_Attribute', 'Cover_Points_metric_Sample', 'Cover_SpeciesComposition_metric_Attribute',
                   'Cover_SpeciesComposition_metric_Sample', 'DataGridViewSettings', 'Density_Belts_metric_Attribute',
                   'Density_Belts_metric_Sample', 'Density_Quadrats_metric_Attribute', 'Density_Quadrats_metric_Sample',
                   'DisturbanceHistory_Attribute', 'DisturbanceHistory_Sample', 'FuelConstants_CWD', 'FuelConstants_DL',
                   'FuelConstants_ExpDL', 'FuelConstants_FWD', 'FuelConstants_Veg', 'LU_Contact', 'LU_DataLevel', 'LU_DataType',
                   'LU_LifeCycle', 'LU_LifeForm', 'LU_MacroPlot_Type', 'LU_Shape', 'LU_Unit', 'Last_Modified_Date', 'LocalSpecies',
                   'MM_LocalSpecies_SpeciesPickList', 'MM_Method_Reference',
                   'MM_MonitoringStatus_SampleEvent', 'MM_Organization_Method', 'MM_ProjectUnit_MacroPlot',
                   'MM_Project_Protocol', 'MM_Protocol_Method', 'MM_SampleEvent_Protocol', 'MSchange_tracking_history',
                   'MacroPlot', 'MasterSpecies', 'MasterSpecies_LastModified', 'Method', 'MethodAttribute', 'MethodAttributeCode',
                   'MethodVersion', 'MonitoringStatus', 'Organization', 'OrganizationGroup', 'PostBurnSeverity_metric_Attribute',
                   'PostBurnSeverity_metric_Sample', 'Program', 'Project', 'ProjectUnit', 'Protocol', 'ProtocolVersion', 'RegistrationUnit',
                   'SampleAttribute', 'SampleAttributeCode', 'SampleEvent', 'SchemaVersions', 'Schema_Version', 'Settings',
                   'SpeciesPickList', 'SurfaceFuels_1000Hr_Attribute', 'SurfaceFuels_1000Hr_Sample', 'SurfaceFuels_Duff_Litter_Attribute',
                   'SurfaceFuels_Duff_Litter_Sample', 'SurfaceFuels_Fine_Attribute', 'SurfaceFuels_Fine_Sample',
                   #"SurfaceFuels_Hr_Attribute", "SurfaceFuels_Hr_Sample",
                   'Trees_Individuals_metric_Attribute', 'Trees_Individuals_metric_Sample')

    file_name1 <- sort(sub(".*/", "", import_path, perl = T))
    file_name2 <- gsub("[[:digit:]]+|.zip", "", file_name1)
    file_name <- gsub("_$","", file_name2)

    # Check if can read files within the zip file
    tryCatch(
      {zfiles = utils::unzip(import_path, list = T)$Name},
      error = function(e){stop(paste0("Unable to import specified zip file."))})

    z_list = sort(zfiles[grepl(paste0(csv_list1, collapse = "|"), zfiles)])

    # Drop date stamp (if it exists) from file name if exists in 2 steps
    z_list_names <- gsub("[[:digit:]]+|.csv", "", z_list)
    z_list_names <- gsub("./", "", z_list_names)
    z_list_names <- gsub("_$","", z_list_names)

    # Drop csvs from csv_list not in z_list
    csv_list <- csv_list1[csv_list1 %in% z_list_names]

    miss_tbls <- setdiff(z_list_names, csv_list) # currently circular. Once I know the tables that should always be included,
    # I'll update csv_list1 above and use the same tables for each park.

    # Check for missing views
    if(length(miss_tbls) > 0){warning("The following tables are not included in specified database: ",
                                      paste0(miss_tbls, collapse = ", "))}

    # Since the missing test passed, clean up files so only includes names in view_list, but
    # maintain order in files

    # Import views now that all tests passed
    pb <- txtProgressBar(min = 0, max = length(z_list), style = 3)

    tbls1 <- unzip(import_path, junkpaths = TRUE, exdir = tempdir())
    tbls2 <- sort(tbls1[grepl(".csv", tbls1)])

    tbls <- sort(tbls2[grepl(paste0(csv_list, collapse = "|"), tbls2)])

    tbl_import <-
      lapply(seq_along(tbls), function(x){
        setTxtProgressBar(pb,x)
        tbl <- tbls[x]
        tab <- read.csv(tbls[x], na.string = c("NA", "NULL"), check.names = FALSE) |>
          dplyr::mutate(datasource = file_name)
        return(tab)})

    tbl_import <- setNames(tbl_import, z_list_names)
    tbl_import2 <- tbl_import
    list2env(tbl_import, envir = env)

    # Close progress bar
    close(pb)


  } # end of type = 'csv'

} # end of function

