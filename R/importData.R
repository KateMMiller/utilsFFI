#' @title importData
#'
#' @description Imports data from FFI SQL Server database or csvs of FFI database
#' tables in the 'dbo' schema. Currently can only import from a local installation
#' of a park's FFI database in SQL Server Management Studio (SSMS), but the goal is to
#' add an option for importing directly from the SQL Server where the production
#' FFI databases are housed. Currently only works with one park/project at a time,
#' but could be extended to import multiple parks/projects (see importData function
#' in katemmiller/vegcomNGPN).
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
#'  }
#'
#' @param dbname Quoted name of database matching the name of the database as it
#' appears in SQL Server Management Studio
#' (eg. "FFI_RA_AGFO").
#'
#' @param new_env Logical. If TRUE (default), will import tables to an environment
#' named FFI_tables. If FALSE, will import tables to global environment.
#'
#' @examples
#' \dontrun{
#' #--- From Local install of FFI SQL databases
#' # Import data for AGFO
#' library(utilsFFI)
#' importData(dbname = "FFI_RA_AGFO")
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
#' @returns Either an environment with database tables as data frames for each imported database, or database
#' tables directly in the global environment.
#'
#' @export
#'

importData <- function(type = "local", server = NA, dbname = "FFI_RA_AGFO", new_env = T){
  #---- Bug Handling ----
  type <- match.arg(type, c("local", "server", 'csv'))
  if(length(dbname) > 1){stop("Can only import 1 database at a time.")}
  stopifnot(is.logical(new_env))
  if(any(is.na(dbname))){stop("Must specify a dbname to import tables from SMSS.")}
  #++++++ Update as more features are added ++++++
  if(type %in% c("server")){stop(paste0("Sorry, type = ", type, " is not yet enabled."))}
  if(type == "server" & is.na(server)){stop("Must specify a server address if type = 'server'")}

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
      #VIEWS_NGPN <<- new.env()
      list2env(tbl_import3, envir = env)
      DBI::dbDisconnect(con)
  } # end of type = local
} # end of function

