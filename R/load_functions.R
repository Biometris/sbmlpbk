#' Loads the functions from the SBML model
#'
#' @param model SBML model S3 class.
#' @param envir Environment in which to load the functions. Default the
#' the environment from which the function is called.
#'
#' @returns An object of class \code{sbmlModel}.
#' @export
load_functions <- function(model, envir = parent.frame()) {
  if (length(model$function_defs) > 0) {
    for (eqn in model$function_defs) {
      eval(parse(text = eqn), envir = envir)  # evaluate in local environment
    }
  }
}
