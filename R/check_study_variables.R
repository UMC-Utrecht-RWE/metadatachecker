#' Compare variable consistency in two csv files - exact match
#'
#' Given the paths of two csv files (";" separated) and the column names,
#' this function returns the list of "not found" variables that are not
#' present in the reference file (first submitted file), but present in the second
#' submitted file
#'
#' @param path_study_vars_reference file path of reference meta data file (.csv)
#' e.g. "p_meta_data/PfizerRF_study_variables.csv"
#' @param ref_colname column name to be checked from the reference meta data csv
#' e.g. "VarName"
#' @param path_metadata file path of meta data file of interest (.csv)
#' e.g. "Pfizer_algorithms.csv"
#' @param metadata_colname column name to be checked from the meta data csv of interest
#' e.g. "CONCEPT"
#'
#'
#' @return a list of "not found" variables
#' @export
#'

check_study_variables <- function(path_study_vars_reference,
                                  ref_colname,
                                  path_metadata,
                                  metadata_colname){

  study_vars_df<-utils:: read.csv(path_study_vars_reference,
                         stringsAsFactors = F,
                         na.strings = c(""," ","NA","NULL"),
                         sep = ";")
  reference_vars <- study_vars_df[,ref_colname]

  metadata_df <- utils::read.csv(path_metadata,
                            stringsAsFactors = F,
                            strip.white = T,
                            sep = ";")
  metadata_vars <- metadata_df[,metadata_colname]

  # Search metadata variables in reference list

  # Find common set of variables between two columns
  common_set_vars <- intersect(reference_vars, metadata_vars)

  # Find the list of variables that are not present in study_vars
  not_present_in_ref_vars_exact <- setdiff(metadata_vars, reference_vars)

  # # Convert both sets to upper case to see if that is the issue
  # not_present_in_ref_vars <- setdiff(toupper(metadata_vars), toupper(reference_vars))


  return(not_present_in_ref_vars_exact)
}


