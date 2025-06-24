#' @title exportData
#'
#' @description Exports data tables that were imported from the FFI database
#' tables in the 'dbo' schema as a zip file of csvs. Currently only works with one
#' park/project at a time, but could be extended to import multiple parks/projects
#' (see importData function in katemmiller/vegcomNGPN).
#'
#' @param zip_name Quoted string that names the exported zip file (eg. "FFI_RA_AGFO").
#' If not specified, default name will be "FFI_table_export_YYYYMMDD.zip", with the
#' YYYYMMMDD being a date stamp for when the data were exported. Best practice would
#' be to name it the same as the dbname used for the importData() step.
#'
#' @param export_path Quoted string to export zipped csvs to if export = TRUE.
#' If not specified, will export to the working directory.
#'
#' @examples
#' \dontrun{
#' #--- From Local install of FFI SQL databases
#' # Import data for AGFO
#' library(utilsFFI)
#' importData(dbname = "FFI_RA_AGFO")
#'
#' # Export data using default settings:
#' exportData()
#'
#' # Export data to a zip file named FFI_RA_AGFO to your working directory.
#' exportData(zip_name = "FFI_RA_AGFO")
#'
#' # Export data to a named zip file and a specific directory
#' exportData(zip_name = "FFI_RA_AGFO", path = "C:/temp")
#' }
#'
#' @returns Saves a zip file of database tables in an imported FFI database
#'
#' @export
#'

exportData <- function(zip_name = "FFI_table_export", path = getwd()){
  #--- Bug handling ---
  if(!requireNamespace("zip", quietly = TRUE)){
    stop("Package 'zip' needed for this function. Please install it.", call. = FALSE)}
  if(!grepl("/$", path)){path <- paste0(path, "/")} # add / to end of path if doesn't exist
  if(!dir.exists(path)){stop("Specified export_path directory does not exist.")}
  pathn <- normalizePath(path)

  #--- Set up tables to be exported ---
  tbls <- sort(names(FFI_tables))

  dir.create(tmp <- tempfile())

  invisible(lapply(seq_along(tbls), function(x){
    tbl_name <- tbls[x]
    temp_tbl = get(tbl_name, envir = FFI_tables)
    write.csv(temp_tbl,
              paste0(tmp, "\\", tbls[x], ".csv"),
              row.names = FALSE)
  }))

  file_list <- list.files(tmp)
  zip_name2 = paste0(zip_name, "_", format(Sys.Date(), "%Y%m%d"), ".zip")

  zip::zipr(zipfile = paste0(pathn, "\\", zip_name2),
            root = tmp,
            files = file_list)

  noquote(paste0('Export complete. FFI tables saved to: ', pathn, "\\", zip_name2))

} # end of function

