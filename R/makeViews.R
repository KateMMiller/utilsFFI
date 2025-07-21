#' @title makeViews
#'
#' @description Makes flattened datasets (i.e. views) that join together macroplot, sample event,
#' sample data and sample attribute data for Cover Points (metric), Cover Species Composition,
#' Density Belts (metric), Density Quadrats (metric), Disturbance History, Surface Fuels 1000Hr, Fine, and Duff,
#' and Trees (metric). The intent of these views is for them to be stand alone, analysis ready
#' datasets. Must import FFI data tables first via importData() function. Note that this has only been
#' tested for a few parks, and may need to be tweaked to accommodate other FFI datasets.
#'
#' @importFrom dplyr filter inner_join left_join right_join select
#' @importFrom tidyr pivot_wider
#'
#' @param new_env Logical. If TRUE (default), will assign views into an environment called FFI_views.
#' If FALSE, will assign views to global environment.
#'
#' @param export_views Logical. If TRUE, will export a zip file of csvs to specified export_path for
#' the flattened views of the data. Default is FALSE.
#'
#' @param export_path Quoted string to export zipped csvs to if export = TRUE. If not specified,
#' will export to the working directory.
#'
#' @param zip_name Quoted string that names the exported zip file (eg. "FFI_RA_AGFO_views").
#' If not specified, default name will be "FFI_view_export_YYYYMMDD.zip", with the
#' YYYYMMMDD being a date stamp for when the data were exported. Best practice would
#' be to name it the same as the dbname used for the importData() step.
#'
#' @examples
#' \dontrun{
#' #++++ RUN FIRST ++++
#' library(utilsFFI)
#' importData(dbname = "FFI_RA_BADL")
#'
#' # generate views for BADL data using defaults
#' makeViews()
#'
#' # generate views and export zip to working directory
#' makeViews(export_views = T)
#'
#' # generate views and export zip to specified path and name
#' makeViews(export_views = T, export_path = "C:/temp", zip_name = "FFI_BADL_view_export")
#'
#' }
#'
#'
#' @returns Returns flattened views of FFI data by protocol
#'
#' @export

makeViews <- function(new_env = T, export_views = F, export_path = NA, zip_name = "FFI_view_export"){
  #---- Bug handling ----
  stopifnot(is.logical(new_env))
  stopifnot(is.logical(export_views))
  stopifnot(!is.na(zip_name))

  if(!requireNamespace("zip", quietly = TRUE) & export_views == TRUE){
    stop("Package 'zip' needed for this function. Please install it.", call. = FALSE)}

  if(export_views == TRUE){
    if(is.na(export_path)){
      export_path <- getwd()
      print(paste0("No export_path specified. Output saved to working directory: ", getwd()), quote = FALSE)}
    if(!grepl("/$", export_path)){export_path <- paste0(export_path, "/")} # add / to end of path if doesn't exist
    if(!dir.exists(export_path)){stop("Specified export_path directory does not exist.")}
    # Normalize filepath for zip
    export_pathn <- normalizePath(export_path)
  }

  if(new_env == TRUE){FFI_views <<- new.env()}
  env <- if(exists("FFI_tables")){FFI_tables} else {.GlobalEnv} # for the tables
  env_views <- if(new_env == TRUE){FFI_views} else {.GlobalEnv} # for the views

  pb <- txtProgressBar(min = 0, max = 11, style = 3)

  setTxtProgressBar(pb,1)
  #---- MacroPlot View -----
  #### Compile MacroPlot data ####
  tryCatch(
    macro_orig <- get("MacroPlot", envir = env),
    error = function(e){stop("MacroPlot table not found. Please import FFI data.")})
  tryCatch(
    mm_projunit <- get("MM_ProjectUnit_MacroPlot", envir = env),
    error = function(e){stop("MM_ProjectUnit_MacroPlot table not found. Please import FFI data tables.")})
  tryCatch(
    projunit <- get("ProjectUnit", envir = env),
    error = function(e){stop("ProjectUnit table not found. Please import FFI data tables.")})
  tryCatch(
    regunit <- get("RegistrationUnit", envir = env),
    error = function(e){stop("RegistrationUnit table not found. Please import FFI data tables.")})

  # Clean up purpose (using NPGN data. May have other cleaning to do for other administrative units/projects)
  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose == "Panel 9"] <- "Panel9"
  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in%
                                 c("FX", "Fx", "FX Monitoring", "FX monitoring", "Fire Effects Monitoring")] <- "FX Monitoring"
  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in% c("FX Dual", "FX_Dual")] <- "FX Dual"
  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in% c("Research", "research")] <- "Research"
  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in% c("Determine Strategies for efficient early detection",
                                                                   "Determine strategies for efficient early detection",
                                                                   "Early Invasives Detection")] <- "Early Detection"
  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in% c("FMH Grass Plot", "FMH Grass Plot ")] <- "FMH Grass Plot"
  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in% c("FIRE_Intensive", "FIRE_intensive", "FX_Intensive",
                                                                   "FX_ Intensive", "FIRE_intesive")] <- "FX Intensive"
  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in% c("FIRE_Extensive")] <- "FX Extensive"
  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in% c("Lafferty Plot", "Lafferty Plot ")] <- "Lafferty Plot"
  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in% c("invasive research", "Invasives Research",
                                                                   "Invasvies Research")] <- "Invasives Research"
  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in% c("Modified Shrub Plot", "Modified Shrub Plot ")] <- "Modified Shrub Plot"
  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in% c("Pre- and Post- treatment of fuels",
                                                                   "pre- and post-treatment forest and fuels")] <- "Pre and post fuels treatment"
  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in% c("FS")] <- "ForestStructure" # KNRI_PCM_038

  projunit$ProjectUnit_Name[projunit$ProjectUnit_Name %in% c("IN-ACTIVE", "In-Active", "Inactive")] <- "INACTIVE"

  # cleanup project and projectunit data
  projunit$ProjectUnit_Agency <- "NPS"
  # NGPN_plots <- macro_orig$MacroPlot_Name[grepl("_PCM_|_LPCM_|_FPCM_|_RCM_", macro_orig$MacroPlot_Name)]
  # macro <- macro_orig[macro_orig$MacroPlot_Name %in% NGPN_plots,]

  # Joining macroplot-relevant tables
  macro1 <- left_join(macro_orig, mm_projunit,
                      by = c("MacroPlot_GUID" = "MM_MacroPlot_GUID", "datasource"))
  macro2 <- left_join(macro1, regunit, by = c("MacroPlot_RegistrationUnit_GUID" = "RegistrationUnit_GUID", "datasource"))
  macro3 <- left_join(macro2, projunit,
                      by = c("MacroPlot_RegistrationUnit_GUID" = "ProjectUnit_RegistrationUnitGUID",
                             "MM_ProjectUnit_GUID" = "ProjectUnit_GUID",
                             "datasource"))

  # hacky way to keep tblname_UV1 as is
  names(macro3)[names(macro3) == "MacroPlot_UV1"] <- "MacroPlotUV1"
  names(macro3)[names(macro3) == "MacroPlot_UV2"] <- "MacroPlotUV2"
  names(macro3)[names(macro3) == "MacroPlot_UV3"] <- "MacroPlotUV3"
  names(macro3)[names(macro3) == "MacroPlot_UV4"] <- "MacroPlotUV4"
  names(macro3)[names(macro3) == "MacroPlot_UV5"] <- "MacroPlotUV5"
  names(macro3)[names(macro3) == "MacroPlot_UV6"] <- "MacroPlotUV6"
  names(macro3)[names(macro3) == "MacroPlot_UV7"] <- "MacroPlotUV7"
  names(macro3)[names(macro3) == "MacroPlot_UV8"] <- "MacroPlotUV8"
  names(macro3)[names(macro3) == "MacroPlot_GUID"] <- "MacroPlotGUID"
  names(macro3)[names(macro3) == "MacroPlot_Comment"] <- "MacroPlotComment"
  names(macro3)[names(macro3) == "MacroPlot_Name"] <- "MacroPlotName"
  names(macro3)[names(macro3) == "MacroPlot_Purpose"] <- "MacroPlotPurpose"
  names(macro3)[names(macro3) == "MacroPlot_Type"] <- "MacroPlotType"
  names(macro3)[names(macro3) == "ProjectUnit_Name"] <- "ProjectUnitName"
  names(macro3)[names(macro3) == "MM_ProjectUnit_GUID"] <- "MM_ProjectUnitGUID"
  names(macro3)[names(macro3) == "RegistrationUnit_GUID"] <- "RegUnitGUID"

  # Drop table names from most column names for easier coding
  names(macro3) <-
    gsub("^MacroPlot_|^ProjectUnit_|^Registration", "", names(macro3))

  # Add the _ back
  names(macro3) <- gsub("MacroPlot", "MacroPlot_", names(macro3))
  names(macro3) <- gsub("ProjectUnit", "ProjectUnit_", names(macro3))
  names(macro3) <- gsub("RegUnit", "RegistrationUnit_", names(macro3))

  # Compile final dataset
  keep_cols_macro <-
    c("MacroPlot_Name", "Unit_Name", "MacroPlot_Purpose", "MacroPlot_Type",
      "ProjectUnit_Name", "Agency", "UTM_X", "UTM_Y", "UTMzone", "Datum",
      "DD_Lat", "DD_Long", "Elevation", "ElevationUnits", "Azimuth", "Aspect",
      "SlopeHill", "SlopeTransect", "MacroPlot_UV1", "MacroPlot_UV2", "MacroPlot_UV3",
      "MacroPlot_UV4", "MacroPlot_UV5", "MacroPlot_UV6", "MacroPlot_UV7", "MacroPlot_UV8",
      "Metadata", "StartPoint", "Directions", "MacroPlot_Comment","Unit_Comment", #"Description",
      "MacroPlot_GUID", "RegistrationUnit_GUID", #"MM_ProjectUnit_GUID",
      "datasource")

  # Had to drop description and MM_ProjectUnit_GUID to make macroplot rows unique
  macro4 <- macro3[,keep_cols_macro]
  macro4$sampled <- 1

  # Replace spaces and . with "_", so easier to query
  macro4$ProjectUnit_Name <- gsub(" ", "_", macro4$ProjectUnit_Name)

  # make project column wide, so more efficient shape
  macro5 <- macro4 |> pivot_wider(names_from = "ProjectUnit_Name",
                                  values_from = "sampled",
                                  values_fill = 0,
                                  names_prefix = "ProjectUnit_") |>
    data.frame()

  colnames(macro5) <- gsub(" ", "_", colnames(macro5))

  # order projectunit columns
  macro_names <- names(macro5[!grepl("ProjectUnit_", names(macro5))])
  proj_names1 <- names(macro5[grepl("ProjectUnit_", names(macro5))])
  proj_names <-
    if(any(names(macro5) %in% "ProjectUnit_Park")){
      c("ProjectUnit_Park", sort(proj_names1[!proj_names1 %in% "ProjectUnit_Park"]))
    } else {sort(proj_names1)}

  MacroPlots <- data.frame(macro5[order(macro5$MacroPlot_Name),
                                  c(macro_names, proj_names)])

  #---- SampleEvents View ----
  #### Compile Sample Event Data ####
  tryCatch(
    monstat <- get("MonitoringStatus", envir = env) |> select(-datasource),
    error = function(e){stop("MonitoringStatus table not found. Please import FFI data tables.")})
  tryCatch(
    mm_monstat_se <- get("MM_MonitoringStatus_SampleEvent", envir = env) |> select(-datasource),
    error = function(e){stop("MM_MonitoringStatus_SampleEvent table not found. Please import FFI data tables.")})
  tryCatch(
    sampev <- get("SampleEvent", envir = env) |> select(-datasource),
    error = function(e){stop("SampleEvent table not found. Please import FFI data tables.")})

  # Use to make some tables smaller before join
  macro_guids <- unique(MacroPlots$MacroPlot_GUID)

  # Fix typos in MonitoringStatus_Name and MonitoringStatus_Base (cleanup for NGPN, but may want more for other dbs)
  sampev2 <- left_join(MacroPlots, sampev, by = c("MacroPlot_GUID" = "SampleEvent_Plot_GUID"),
                       relationship = 'many-to-many') #MM b/c plots are used for multiple projects

  monstat_join <- inner_join(mm_monstat_se, monstat, by = c("MM_MonitoringStatus_GUID" = "MonitoringStatus_GUID"))

  sampev3 <- left_join(sampev2, monstat_join, by = c("SampleEvent_GUID" = "MM_SampleEvent_GUID"),
                       relationship = 'many-to-many')

  sampev3$SampleEvent_Date <-
    format(as.Date(sampev3$SampleEvent_Date, format = "%Y-%m-%d %H:%m:%s"),
           "%Y-%m-%d")
  sampev3$year <- format(as.Date(sampev3$SampleEvent_Date, format = "%Y-%m-%d"), "%Y")
  sampev3$month <- format(as.Date(sampev3$SampleEvent_Date, format = "%Y-%m-%d"), "%m")
  sampev3$doy <- format(as.Date(sampev3$SampleEvent_Date, format = "%Y-%m-%d"), "%j")

  # drop plots with no associated sample events
  # unique(sampev4$MacroPlot_Name[is.na(sampev4$SampleEvent_GUID)]) # Plots with no sample events
  sampev4 <- sampev3[!is.na(sampev3$SampleEvent_GUID),]

  names(sampev4)[names(sampev4) == "SampleEvent_GUID"] <- "SampleEventGUID"
  names(sampev4)[names(sampev4) == "SampleEvent_Date"] <- "SampleEventDate"
  names(sampev4)[names(sampev4) == "SampleEvent_UV1"] <- "SampleEventUV1"
  names(sampev4)[names(sampev4) == "SampleEvent_Comment"] <- "SampleEventComment"

  # Drop table names from most column names for easier coding
  names(sampev4) <-
    gsub("^SampleEvent_", "", names(sampev4))

  # Add the _ back
  names(sampev4) <- gsub("SampleEvent", "SampleEvent_", names(sampev4))

  keep_cols_samp <-
    c("MacroPlot_Name", "Unit_Name", "MacroPlot_Purpose", "MacroPlot_Type",
      "SampleEvent_Date", "year", "month", "doy",
      "UTM_X", "UTM_Y", "UTMzone", "Elevation", "Azimuth", "Aspect",
      "SlopeHill", "SlopeTransect", "SampleEvent_UV1", "DefaultMonitoringStatus",
      "TreatmentUnit", "MonitoringStatus_Prefix", "MonitoringStatus_Base",
      "MonitoringStatus_Suffix", "MonitoringStatus_Name",
      "MonitoringStatus_Comment", "SampleEvent_Comment",
      "SampleEvent_GUID", "MM_MonitoringStatus_GUID", "RegistrationUnit_GUID", "MacroPlot_GUID")

  SampleEvents <- data.frame(sampev4[order(sampev4$MacroPlot_Name, sampev4$SampleEvent_Date),
                                     keep_cols_samp])

  sampev_unique <- SampleEvents |> select(-MonitoringStatus_Comment, -MM_MonitoringStatus_GUID) |> unique()
  # Prevents some duplication of data until monitoring status is fixed in database.
  sampev_guids <- unique(sampev_unique$SampleEvent_GUID)

  #---- Taxa_Table View----
  setTxtProgressBar(pb,2)
  tryCatch(localspp <- get("LocalSpecies", envir = env),
           error = function(e){stop("LocalSpecies table not found. Please import FFI data tables.")})
  tryCatch(mastspp <- get("MasterSpecies", envir = env),
           error = function(e){stop("MasterSpecies table not found. Please import FFI data tables.")})
  tryCatch(auxspp <- get("AuxSpecies", envir = env),
           error = function(e){stop("AuxSpecies table not found. Please import FFI data tables.")})
  tryCatch(lifeform <- get("LU_LifeForm", envir = env),
           error = function(e){stop("LU_LifeForm table not found. Please import FFI data tables.")})
  # NGPN does not appear to use the SpeciesPickList, so not including here.
  # lifecycle doesn't appear to be used much by NGPN, so not including it here.

  # Table joins
  locspp_reg <- left_join(localspp, regunit,
                          by = c("LocalSpecies_RegistrationUnitGUID" = "RegistrationUnit_GUID", "datasource"))

  spp1 <- left_join(locspp_reg, auxspp,
                    by = c("LocalSpecies_AuxSpeciesGUID" = "AuxSpecies_GUID",
                           "LocalSpecies_RegistrationUnitGUID" = "AuxSpecies_RegistrationUnitGUID",
                           "datasource"))

  spp2 <- left_join(spp1, mastspp,
                    by = c("LocalSpecies_MasterSpeciesGUID" = "MasterSpecies_GUID", 'datasource'))

  spp3 <- left_join(spp2, lifeform,
                    by = c("LocalSpecies_PreferedLifeForm_GUID" = "LU_LifeForm_GUID", "datasource"))

  # Merge Master and Aux species list to return complete species list.
  spp3$ScientificName <- ifelse(is.na(spp3$MasterSpecies_ScientificName), spp3$AuxSpecies_ScientificName,
                                spp3$MasterSpecies_ScientificName)
  spp3$ITIS_TSN <- ifelse(is.na(spp3$MasterSpecies_ITIS_TSN), spp3$AuxSpecies_ITIS_TSN,
                          spp3$MasterSpecies_ITIS_TSN)

  spp3$Family <- ifelse(is.na(spp3$MasterSpecies_Family), spp3$AuxSpecies_Family,
                        spp3$MasterSpecies_Family)

  spp3$Genus <- ifelse(is.na(spp3$MasterSpecies_Genus), spp3$AuxSpecies_Genus,
                       spp3$MasterSpecies_Genus)

  spp3$Symbol <- ifelse(is.na(spp3$MasterSpecies_Symbol), spp3$AuxSpecies_Symbol,
                        spp3$MasterSpecies_Symbol)

  spp3$NotBiological <- ifelse(is.na(spp3$MasterSpecies_NotBiological), spp3$AuxSpecies_NotBiological,
                               spp3$MasterSpecies_NotBiological)

  spp3$Nativity <- ifelse(is.na(spp3$LocalSpecies_Nativity), spp3$MasterSpecies_Nativity,
                          spp3$LocalSpecies_Nativity)

  spp3$CommonName <- ifelse(is.na(spp3$LocalSpecies_CommonName), spp3$MasterSpecies_CommonName,
                            spp3$LocalSpecies_CommonName)

  spp4 <- spp3 |> select(-MasterSpecies_ScientificName, -AuxSpecies_ScientificName,
                         -MasterSpecies_ITIS_TSN, -AuxSpecies_ITIS_TSN,
                         -MasterSpecies_Family, -AuxSpecies_Family,
                         -MasterSpecies_Genus, -AuxSpecies_Genus,
                         -MasterSpecies_Symbol, -LocalSpecies_Symbol, -AuxSpecies_Symbol,
                         -MasterSpecies_NotBiological, -AuxSpecies_NotBiological,
                         -LocalSpecies_Nativity, -MasterSpecies_Nativity,
                         -LocalSpecies_CommonName, -MasterSpecies_CommonName)

  # hacky way to keep LocalSpecies_UV1 as is
  names(spp4)[names(spp4) == "LocalSpecies_GUID"] <- "Spp_GUID"
  names(spp4)[names(spp4) == "LocalSpecies_UV1"] <- "Species_UV1"
  names(spp4)[names(spp4) == "LocalSpecies_Description"] <- "Species_Description"
  names(spp4)[names(spp4) == "LocalSpecies_Comment"] <- "Species_Comment"

  # Drop table names from column names for easier coding
  names(spp4) <-
    gsub("^MacroPlot_|^LocalSpecies_|^MasterSpecies_|^LU_|^AuxSpecies_|^Registration", "", names(spp4))

  names(spp4)[names(spp4) == "UnitGUID"] <- "RegistrationUnitGUID"

  keep_cols_taxa <- c("Symbol", "ITIS_TSN", "ScientificName", "CommonName", "Family", "Genus",
                      "Nativity", "Invasive", "Cultural", "Concern", "LifeCycle",
                      "LifeForm_Name", "NotBiological", "UserAdded", "Species_UV1",
                      "IsUnknown", "IsUnlisted", "Species_Description", "Species_Comment", "Unit_Name",
                      "SymbolKey", "Synonym_SymbolKey", "Spp_GUID", "RegistrationUnitGUID",
                      "MasterSpeciesGUID", "AuxSpeciesGUID", "PLANTS_GUID")

  Taxa_Table <- data.frame(spp4[order(spp4$Symbol, spp4$Unit_Name), keep_cols_taxa])

  #---- Cover_Points_Metric View ----
  setTxtProgressBar(pb,3)
  covpts_samp1 <-   tryCatch(get("Cover_Points_metric_Sample", envir = env),
                             error = function(e){NULL})

  covpts_attr1 <- tryCatch(get("Cover_Points_metric_Attribute", envir = env),
                           error = function(e){NULL})

  if(class(covpts_samp1) == "data.frame"){

    # Making tables smaller before joins
    covpts_samp <- covpts_samp1[covpts_samp1$SampleData_SampleEvent_GUID %in% sampev_guids,]
    samprow_guids <- unique(covpts_samp$SampleData_SampleRow_GUID)
    covpts_attr2 <- covpts_attr1[covpts_attr1$AttributeData_SampleRow_GUID %in% samprow_guids,]
    # Drop records where Index is blank b/c causes issues in the join
    #covpts_attr <- covpts_attr2[!is.na(covpts_attr2$Index),]

    samp_covs1 <- left_join(sampev_unique, covpts_samp,
                            by = c("SampleEvent_GUID" = "SampleData_SampleEvent_GUID"))

    # drop records with blank SampleData_SampleRow_GUID
    samp_covs <- samp_covs1[!is.na(samp_covs1$SampleData_SampleRow_GUID),] # works same as Visited == T

    samp_cova <- left_join(samp_covs, covpts_attr2,
                           by = c("SampleData_SampleRow_GUID" = "AttributeData_SampleRow_GUID",
                                  "datasource"),
                           relationship = 'many-to-many') # b/c multiple projects/macroplot

    samp_cov_spp <- left_join(samp_cova, Taxa_Table,
                              by = c("Spp_GUID", "Unit_Name", "RegistrationUnit_GUID" = "RegistrationUnitGUID"))

    cols_view_start <- c("MacroPlot_Name", "Unit_Name", "MacroPlot_Purpose",
                         "UTM_X", "UTM_Y", "UTMzone", "Elevation",
                         "Azimuth", "Aspect", "SlopeHill", "SlopeTransect", "SampleEvent_Date",
                         "year", "month", "doy")
    cols_view_end <- c("UV1Desc", "UV2Desc", "UV3Desc", "SaComment",
                       "DefaultMonitoringStatus", "MonitoringStatus_Base",
                       "MacroPlot_GUID", "SampleEvent_GUID", #"MM_MonitoringStatus_GUID",
                       "RegistrationUnit_GUID",
                       "Spp_GUID")
    cols_taxa_start <- c("Symbol", "ITIS_TSN", "ScientificName", "CommonName")
    cols_taxa_end <- c("Nativity", "Invasive", "Cultural", "Concern", "LifeCycle", "LifeForm_Name",
                       "NotBiological", "Species_Comment")
    cols_covpt <- c("Visited", "NumTran", "TranLen", 'NumPtsTran', "Offset",
                    "Index", "Transect", "Point", "Tape", "Order", "Height",
                    "CanopyLayer", "Status", "Comment")

    Cover_Points_metric <- data.frame(
      samp_cov_spp[order(samp_cov_spp$MacroPlot_Name, samp_cov_spp$year,
                         samp_cov_spp$Index, samp_cov_spp$ScientificName),
                   c(cols_view_start, cols_taxa_start,
                     cols_covpt,
                     cols_taxa_end, cols_view_end)])
  }
  #---- Cover_Species_Composition View ----
  setTxtProgressBar(pb,4)
  covcomp_samp1 <-   tryCatch(get("Cover_SpeciesComposition_metric_Sample", envir = env),
                              error = function(e){NULL})
  covcomp_attr1 <- tryCatch(get("Cover_SpeciesComposition_metric_Attribute", envir = env),
                            error = function(e){NULL})

  if(class(covcomp_samp1) == "data.frame"){
    # Making tables smaller before joins
    covcomp_samp <- covcomp_samp1[covcomp_samp1$SampleData_SampleEvent_GUID %in% sampev_guids,]
    samprow_guids <- unique(covcomp_samp$SampleData_SampleRow_GUID)
    covcomp_attr2 <- covcomp_attr1[covcomp_attr1$AttributeData_SampleRow_GUID %in% samprow_guids,]

    samp_comps1 <- left_join(sampev_unique, covcomp_samp,
                             by = c("SampleEvent_GUID" = "SampleData_SampleEvent_GUID"))

    # drop records with blank SampleData_SampleRow_GUID
    samp_comps <- samp_comps1[!is.na(samp_comps1$SampleData_SampleRow_GUID),] # works same as Visited == T too

    samp_compa <- left_join(samp_comps, covcomp_attr2,
                            by = c("SampleData_SampleRow_GUID" = "AttributeData_SampleRow_GUID",
                                   "datasource"),
                            relationship = 'many-to-many') # b/c multiple projects/macroplot

    samp_comp_spp <- left_join(samp_compa, Taxa_Table,
                               by = c("Spp_GUID", "Unit_Name", "RegistrationUnit_GUID" = "RegistrationUnitGUID"))

    cols_covcomp <- c("Visited", "Index", "Status", "SizeCl", "AgeCl", "Cover", "Height",
                      "Comment", "UV1", "UV2", "UV3")

    Cover_Species_Composition <- unique(
      data.frame(
        samp_comp_spp[order(samp_comp_spp$MacroPlot_Name, samp_comp_spp$year,
                            samp_comp_spp$Index, samp_comp_spp$ScientificName),
                      c(cols_view_start, cols_taxa_start,
                        cols_covcomp,
                        cols_taxa_end, cols_view_end)]))
  }
  #---- Density_Belts_Metric View ----
  setTxtProgressBar(pb,5)
  densbelt_samp1 <-   tryCatch(get("Density_Belts_metric_Sample", envir = env),
                               error = function(e){NULL})
  densbelt_attr1 <- tryCatch(get("Density_Belts_metric_Attribute", envir = env),
                             error = function(e){NULL})

  if(class(densbelt_samp1) == "data.frame"){
    # Making tables smaller before joins
    densbelt_samp <- densbelt_samp1[densbelt_samp1$SampleData_SampleEvent_GUID %in% sampev_guids,]
    samprow_guids <- unique(densbelt_samp$SampleData_SampleRow_GUID)
    densbelt_attr2 <- densbelt_attr1[densbelt_attr1$AttributeData_SampleRow_GUID %in% samprow_guids,]

    samp_densbs1 <- left_join(sampev_unique, densbelt_samp,
                              by = c("SampleEvent_GUID" = "SampleData_SampleEvent_GUID"))

    # drop records with blank SampleData_SampleRow_GUID
    samp_densbs <- samp_densbs1[!is.na(samp_densbs1$SampleData_SampleRow_GUID),] # works same as Visited == T

    samp_densba <- left_join(samp_densbs, densbelt_attr2,
                             by = c("SampleData_SampleRow_GUID" = "AttributeData_SampleRow_GUID",
                                    "datasource"),
                             relationship = 'many-to-many') # b/c multiple projects/macroplot

    samp_densb_spp <- left_join(samp_densba, Taxa_Table,
                                by = c("Spp_GUID", "Unit_Name",
                                       "RegistrationUnit_GUID" = "RegistrationUnitGUID"))

    cols_densbelt <- c("Visited", "NumTran", "NumSubbelt", "TranLen", "TranWid", "Area",
                       "Index", "Transect", "Subbelt", "Status", "SizeCl", "AgeCl",
                       "Count", "Height", "SubFrac", "Comment", "UV1", "UV2", "UV3")

    Density_Belts_metric <- data.frame(
      samp_densb_spp[order(samp_densb_spp$MacroPlot_Name, samp_densb_spp$year,
                           samp_densb_spp$Index, samp_densb_spp$ScientificName),
                     c(cols_view_start, cols_taxa_start,
                       cols_densbelt,
                       cols_taxa_end, cols_view_end)])
  }
  #---- Density_Quadrats_Metric View ----
  setTxtProgressBar(pb,6)

  densquad_samp1 <- tryCatch(get("Density_Quadrats_metric_Sample", envir = env),
                             error = function(e){NULL})
  densquad_attr1 <- tryCatch(get("Density_Quadrats_metric_Attribute", envir = env),
                             error = function(e){NULL})

  if(class(densquad_samp1) == "data.frame"){
    # Making tables smaller before joins
    densquad_samp <- densquad_samp1[densquad_samp1$SampleData_SampleEvent_GUID %in% sampev_guids,]
    samprow_guids <- unique(densquad_samp$SampleData_SampleRow_GUID)
    densquad_attr2 <- densquad_attr1[densquad_attr1$AttributeData_SampleRow_GUID %in% samprow_guids,]

    samp_densqs1 <- left_join(sampev_unique, densquad_samp,
                              by = c("SampleEvent_GUID" = "SampleData_SampleEvent_GUID"))

    # drop records with blank SampleData_SampleRow_GUID
    samp_densqs <- samp_densqs1[!is.na(samp_densqs1$SampleData_SampleRow_GUID),] # works same as Visited == T

    samp_densqa <- left_join(samp_densqs, densquad_attr2,
                             by = c("SampleData_SampleRow_GUID" = "AttributeData_SampleRow_GUID",
                                    "datasource"),
                             relationship = 'many-to-many') # b/c multiple projects/macroplot

    samp_densq_spp <- left_join(samp_densqa, Taxa_Table,
                                by = c("Spp_GUID", "Unit_Name",
                                       "RegistrationUnit_GUID" = "RegistrationUnitGUID"))

    cols_densquad <- c("Visited", "NumTran", "NumQuadTran", "QuadLen", "QuadWid", "Area",
                       "Index", "Transect", "Quadrat", "Status", "SizeCl", "AgeCl",
                       "Count", "Height", "SubFrac", "Comment", "UV1", "UV2", "UV3")

    Density_Quadrats_metric <- data.frame(
      samp_densq_spp[order(samp_densq_spp$MacroPlot_Name, samp_densq_spp$year,
                           samp_densq_spp$Index, samp_densq_spp$ScientificName),
                     c(cols_view_start, cols_taxa_start,
                       cols_densquad,
                       cols_taxa_end, cols_view_end)])
  }
  #---- Disturbance_History View ----
  setTxtProgressBar(pb,7)
  disthist_samp1 <-   tryCatch(get("DisturbanceHistory_Sample", envir = env),
                               error = function(e){NULL})
  disthist_attr1 <- tryCatch(get("DisturbanceHistory_Attribute", envir = env),
                             error = function(e){NULL})

  if(class(disthist_samp1) == "data.frame"){
    # Making tables smaller before joins
    disthist_samp <- disthist_samp1[disthist_samp1$SampleData_SampleEvent_GUID %in% sampev_guids,]
    samprow_guids <- unique(disthist_samp$SampleData_SampleRow_GUID)
    disthist_attr2 <- disthist_attr1[disthist_attr1$AttributeData_SampleRow_GUID %in% samprow_guids,]

    samp_dists1 <- left_join(sampev_unique, disthist_samp,
                             by = c("SampleEvent_GUID" = "SampleData_SampleEvent_GUID"))

    # drop records with blank SampleData_SampleRow_GUID
    samp_dists <- samp_dists1[!is.na(samp_dists1$SampleData_SampleRow_GUID),] # works same as Visited == T

    samp_dista <- left_join(samp_dists, disthist_attr2,
                            by = c("SampleData_SampleRow_GUID" = "AttributeData_SampleRow_GUID",
                                   "datasource"),
                            relationship = 'many-to-many') # b/c multiple projects/macroplot

    cols_dist <- c("Visited",
                   "Index", "ChAgent", "SevCode", "StartYr", "StartMo", "StartDy",
                   "EndYr", "EndMo", "EndDy", "DatePrec", "ChgDesc", "Comment",
                   "UV1", "UV2", "UV3")

    cols_view_end_nospp <- cols_view_end[!cols_view_end %in% "Spp_GUID"]

    Disturbance_History <- data.frame(
      samp_dista[order(samp_dista$MacroPlot_Name, samp_dista$year,
                       samp_dista$Index),
                 c(cols_view_start, cols_dist, cols_view_end_nospp)]
    )
  }
  #---- Surface_Fuels_1000Hr View ----
  setTxtProgressBar(pb,8)
  surf1000_samp1 <- tryCatch(get("SurfaceFuels_1000Hr_Sample", envir = env),
                             error = function(e){NULL})
  surf1000_attr1 <- tryCatch(get("SurfaceFuels_1000Hr_Attribute", envir = env),
                             error = function(e){NULL})

  if(class(surf1000_samp1) == "data.frame"){
    # Making tables smaller before joins
    surf1000_samp <- surf1000_samp1[surf1000_samp1$SampleData_SampleEvent_GUID %in% sampev_guids,]
    samprow_guids <- unique(surf1000_samp$SampleData_SampleRow_GUID)
    surf1000_attr2 <- surf1000_attr1[surf1000_attr1$AttributeData_SampleRow_GUID %in% samprow_guids,]

    samp_surf1000s1 <- left_join(sampev_unique, surf1000_samp,
                                 by = c("SampleEvent_GUID" = "SampleData_SampleEvent_GUID"))

    samp_surf1000s <- samp_surf1000s1[!is.na(samp_surf1000s1$SampleData_SampleRow_GUID == TRUE),]

    samp_surf1000a <- left_join(samp_surf1000s, surf1000_attr2,
                                by = c("SampleData_SampleRow_GUID" = "AttributeData_SampleRow_GUID",
                                       "datasource"),
                                relationship = 'many-to-many') # b/c multiple projects/macroplot

    cols_surf1000 <- c("Visited", "NumTran", "TranLen", "Index", "Transect", "Slope", "LogNum", "Dia",
                       "DecayCl", "CWDFuConSt", "Comment", "UV1", "UV2", "UV3")

    Surface_Fuels_1000Hr <- data.frame(
      samp_surf1000a[order(samp_surf1000a$MacroPlot_Name, samp_surf1000a$year,
                           samp_surf1000a$Index),
                     c(cols_view_start, cols_surf1000, cols_view_end_nospp)])
  }
  #---- Surface_Fuels_Fine View ----
  setTxtProgressBar(pb,9)
  surffine_samp1 <-   tryCatch(get("SurfaceFuels_Fine_Sample", envir = env),
                               error = function(e){NULL})
  surffine_attr1 <- tryCatch(get("SurfaceFuels_Fine_Attribute", envir = env),
                             error = function(e){NULL})

  if(class(surffine_samp1) == "data.frame"){

    # Making tables smaller before joins
    surffine_samp <- surffine_samp1[surffine_samp1$SampleData_SampleEvent_GUID %in% sampev_guids,]
    samprow_guids <- unique(surffine_samp$SampleData_SampleRow_GUID)
    surffine_attr2 <- surffine_attr1[surffine_attr1$AttributeData_SampleRow_GUID %in% samprow_guids,]

    samp_surffines1 <- left_join(sampev_unique, surffine_samp,
                                 by = c("SampleEvent_GUID" = "SampleData_SampleEvent_GUID"))

    samp_surffines <- samp_surffines1[!is.na(samp_surffines1$SampleData_SampleRow_GUID == TRUE),]

    samp_surffinea <- left_join(samp_surffines, surffine_attr2,
                                by = c("SampleData_SampleRow_GUID" = "AttributeData_SampleRow_GUID",
                                       "datasource"),
                                relationship = 'many-to-many') # b/c multiple projects/macroplot
    names(samp_surffinea)[names(samp_surffinea) == "Azimuth.x"] <- "Azimuth"
    names(samp_surffinea)[names(samp_surffinea) == "Azimuth.y"] <- "Azimuth_Fuels"

    cols_surffine <- c("Visited", "NumTran", "OneHrTranLen", "TenHrTranLen", "HunHrTranLen",
                       "Index", "Transect", "Azimuth_Fuels", "Slope", "OneHr", "TenHr", "HunHr", "FWDFuConSt",
                       "Comment", "UV1", "UV2", "UV3")

    Surface_Fuels_Fine <- data.frame(
      samp_surffinea[order(samp_surffinea$MacroPlot_Name, samp_surffinea$year,
                           samp_surffinea$Index),
                     c(cols_view_start, cols_surffine, cols_view_end_nospp)])

  }

  #---- Surface_Fuels_Duff View ----
  setTxtProgressBar(pb,10)
  surfduff_samp1 <-   tryCatch(get("SurfaceFuels_Duff_Litter_Sample", envir = env),
                               error = function(e){NULL})
  surfduff_attr1 <- tryCatch(get("SurfaceFuels_Duff_Litter_Attribute", envir = env),
                             error = function(e){NULL})

  if(class(surfduff_samp1) == "data.frame"){
    # Making tables smaller before joins
    surfduff_samp <- surfduff_samp1[surfduff_samp1$SampleData_SampleEvent_GUID %in% sampev_guids,]
    samprow_guids <- unique(surfduff_samp$SampleData_SampleRow_GUID)
    surfduff_attr2 <- surfduff_attr1[surfduff_attr1$AttributeData_SampleRow_GUID %in% samprow_guids,]

    samp_surfduffs1 <- left_join(sampev_unique, surfduff_samp,
                                 by = c("SampleEvent_GUID" = "SampleData_SampleEvent_GUID"))

    samp_surfduffs <- samp_surfduffs1[!is.na(samp_surfduffs1$SampleData_SampleRow_GUID == TRUE),]


    samp_surfduffa <- left_join(samp_surfduffs, surfduff_attr2,
                                by = c("SampleData_SampleRow_GUID" = "AttributeData_SampleRow_GUID",
                                       "datasource"),
                                relationship = 'many-to-many') # b/c multiple projects/macroplot

    cols_surfduff <- c("Visited", "NumTran", "Index", "Transect", "SampLoc", "OffSet", "LittDep",
                       "DuffDep", "FuelbedDep", "DLFuConSt", "Comment", "UV1", "UV2", "UV3")

    Surface_Fuels_Duff <- data.frame(
      samp_surfduffa[order(samp_surfduffa$MacroPlot_Name, samp_surfduffa$year,
                           samp_surfduffa$Index),
                     c(cols_view_start, cols_surfduff, cols_view_end_nospp)])
  }
  #---- Trees_Metric ----
  setTxtProgressBar(pb,11)
  tree_samp1 <- tryCatch(get("Trees_Individuals_metric_Sample", envir = env),
                         error = function(e){NULL})
  tree_attr1 <- tryCatch(get("Trees_Individuals_metric_Attribute", envir = env),
                         error = function(e){NULL})

  if(class(tree_attr1) == "data.frame"){
    # Making tables smaller before joins
    tree_samp <- tree_samp1[tree_samp1$SampleData_SampleEvent_GUID %in% sampev_guids,]
    samprow_guids <- unique(tree_samp$SampleData_SampleRow_GUID)
    tree_attr <- tree_attr1[tree_attr1$AttributeData_SampleRow_GUID %in% samprow_guids,]

    # Not all parks/plots have tree data associated, making the left_joins bring in a bunch of blank rows.
    # Using all plots with a tree recorded in the tree_samp1 to filter out non-tree plots
    samp_treesrj <- right_join(sampev_unique, tree_samp,
                               by = c("SampleEvent_GUID" = "SampleData_SampleEvent_GUID"))
    samp_treearj <- right_join(samp_treesrj, tree_attr,
                               by = c("SampleData_SampleRow_GUID" = "AttributeData_SampleRow_GUID",
                                      "datasource"),
                               relationship = 'many-to-many') # b/c multiple projects/macroplot
    tree_samp_plots <- sort(unique(samp_treearj$MacroPlot_Name))

    samp_trees1 <- left_join(sampev_unique, tree_samp,
                             by = c("SampleEvent_GUID" = "SampleData_SampleEvent_GUID")) |>
      filter(MacroPlot_Name %in% tree_samp_plots)

    samp_trees <- samp_trees1[!is.na(samp_trees1$SampleData_SampleRow_GUID == TRUE),]

    samp_treea <- left_join(samp_trees, tree_attr,
                            by = c("SampleData_SampleRow_GUID" = "AttributeData_SampleRow_GUID",
                                   "datasource"),
                            relationship = 'many-to-many') # b/c multiple projects/macroplot

    samp_tree_spp <- left_join(samp_treea, Taxa_Table,
                               by = c("Spp_GUID", "Unit_Name", "RegistrationUnit_GUID" = "RegistrationUnitGUID"))

    cols_tree <- c("Visited", "MacroPlotSize", "SnagPlotSize", "BrkPntDia",
                   "QTR", "SubFrac", "TagNo", "Status", "DBH", "CrwnCl", "LiCrBHt",
                   "CrwnRad", "DRC", "Comment", "UV1", "UV2", "UV3")

    # tree columns not used by NGPN
    #c("Ht", "CrwnRto", "CrFuBHt", "Age", "GrwthRt", "Mort", "DecayCl", "LaddBaseHt", "LaddMaxHt",
    # "NuLiStems", "NuDeStems", "EqDia", "XCoord", "YCoord", "CKR", "CharHt",
    # "ScorchHt", "CrScPct", "DamCd1", "DamSev1", "DamCd2", "DamSev2", "DamCd3",
    # "DamSev3", "DamCd4", "DamSev4", "DamCd5", "DamSev5")

    Trees_metric <- data.frame(
      samp_tree_spp[order(samp_densq_spp$MacroPlot_Name, samp_densq_spp$year,
                          samp_densq_spp$Index, samp_densq_spp$ScientificName),
                    c(cols_view_start, cols_taxa_start,
                      cols_tree,
                      cols_taxa_end, cols_view_end)])

  }

  #---- Add views to FFI_views ----
  view_names <- c("Cover_Points_metric", "Cover_Species_Composition", "Density_Belts_metric",
                  "Density_Quadrats_metric", "Disturbance_History", "MacroPlots", "SampleEvents",
                  "Surface_Fuels_1000Hr", "Surface_Fuels_Fine", "Surface_Fuels_Duff", "Taxa_Table",
                  "Trees_metric")

  if(exists("Cover_Points_metric")){
    assign("Cover_Points_metric", Cover_Points_metric, envir = env_views)}
  if(exists("Cover_Species_Composition")){
    assign("Cover_Species_Composition", Cover_Species_Composition, envir = env_views)}
  if(exists("Density_Belts_metric")){
    assign("Density_Belts_metric", Density_Belts_metric, envir = env_views)}
  if(exists("Density_Quadrats_metric")){
    assign("Density_Quadrats_metric", Density_Quadrats_metric, envir = env_views)}
  if(exists("Disturbance_History")){
    assign("Disturbance_History", Disturbance_History, envir = env_views)}
  assign("MacroPlots", MacroPlots, envir = env_views)
  assign("SampleEvents", SampleEvents, envir = env_views)
  if(exists("Surface_Fuels_1000Hr")){
    assign("Surface_Fuels_1000Hr", Surface_Fuels_1000Hr, envir = env_views)}
  if(exists("Surface_Fuels_Fine")){
    assign("Surface_Fuels_Fine", Surface_Fuels_Fine, envir = env_views)}
  if(exists("Surface_Fuels_Duff")){
    assign("Surface_Fuels_Duff", Surface_Fuels_Duff, envir = env_views)}
  assign("Taxa_Table", Taxa_Table, envir = env_views)
  if(exists("Trees_metric")){
    assign("Trees_metric", Trees_metric, envir = env_views)}

  views_final <- view_names[view_names %in% names(env_views)]

  if(length(views_final) != length(view_names)){
    warning(paste0("The following views were not created because specified database did not include relevant tables: ",
                   paste0(view_names[!view_names %in% names(env_views)], collapse = ", ")))
  }

  close(pb)

  if(export_views == TRUE){
    dir.create(tmp <- tempfile())
    invisible(lapply(seq_along(views_final), function(x){
      temp_tbl = get(views_final[x], envir = env_views)
      write.csv(temp_tbl,
                paste0(tmp, "\\", views_final[x], ".csv"),
                row.names = FALSE)
    }))

    view_list <- list.files(tmp)

    zip_name2 = paste0(zip_name, "_", format(Sys.Date(), "%Y%m%d"), ".zip")

    zip::zipr(zipfile = paste0(export_pathn, "\\", zip_name2),
              root = tmp,
              files = view_list)
    noquote(paste0("Export of views complete and saved to ", export_pathn, "\\", zip_name2))
  }

}

