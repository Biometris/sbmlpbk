#' Load PBK model parameter values from a CSV
#'
#' This function reads a CSV file of parameter values and returns a named
#' numeric vector. Only rows matching the specified `idInstance` are used.
#'
#' The input CSV file must contain the following columns:
#' \describe{
#'   \item{\code{idModelInstance}}{A character string identifying the model instance. Used for filtering.}
#'   \item{\code{Parameter}}{The name of the parameter to be loaded. These will become the names of the output vector.}
#'   \item{\code{Value}}{The numeric value associated with each parameter.}
#' }
#'
#' @param model An object of class \code{sbmlModel} for which the parametrisations should be loaded.
#' @param filename Path to the CSV file containing parameters.
#' @param param_instance The instance ID used to filter parameters. When not specified, it is assumed that the file contains only one parametrisation.
#' @param ... Additional arguments passed to \code{read.csv}.
#'
#' @return A named numeric vector with parameter values.
#'
#' @examples
#' model_file <- system.file("extdata/", "simple_oral.sbml", package = "sbmlpbk")
#' model <- load_sbml(model_file)
#' params_file <- system.file("extdata/", "simple_oral_params.csv", package = "sbmlpbk")
#' params <- load_params(model, params_file, 'simple_PARAM')
#' head(params)
#'
#' # Example CSV structure:
#' # idModelInstance,Parameter,Value
#' # "model_1",a,0.5
#' # "model_2",a,1.0
#' # "model_2",b,2.5
#'
#' @importFrom utils head
#' @importFrom stats setNames
#' @importFrom utils read.csv
#' @export
load_params <- function(model, filename, param_instance, ...) {
  df <- read.csv(filename, ...)
  df$Value <- as.numeric(df$Value)
  if (any(grepl('idModelInstance', names(df)))) {
    df$idModelInstance <- as.character(df$idModelInstance)
    df <- df[df$idModelInstance == param_instance, ]
  }

  # Check for unknown parameters
  unknown_params <- setdiff(df$Parameter, names(model$params))
  if (length(unknown_params) > 0) {
    stop(paste0("Unknown parameter(s) in file: ", paste(unknown_params, collapse = ", ")))
  }

  # Create a named numeric vector
  params <- setNames(df$Value, as.character(df$Parameter))

  # Fill missing parameters from default model params
  missing_params <- setdiff(names(model$params), names(params))
  if (length(missing_params) > 0) {
    params[missing_params] <- model$params[missing_params]
  }

  # Reorder to match params order
  params <- params[names(model$params)]

  return(params)
}
