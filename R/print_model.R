#' Summary function for the class \code{sbmlModel}
#'
#' Gives a summary for an object of S3 class \code{sbmlModel}.
#'
#' @param object An object of class \code{sbmlModel}.
#' @param ... Not used.
#'
#' @returns A list with summary items.
#'
#' @export
summary.sbmlModel <- function(object, ...) {
  stopifnot(inherits(object, "sbmlModel"))
  summary_list <- list(
    compartment_count = length(object$compartment),
    compartment_ids = names(object$compartment),
    species_count = length(object$species),
    species_ids = names(object$species),
    param_count = length(object$params),
    param_ids = names(object$params),
    function_count = length(object$function_defs),
    function_ids = names(object$function_defs),
    assignment_rule_count = length(object$assignment_rules),
    rate_rule_count = length(object$rate_rules),
    reaction_count = length(object$reactions),
    time_unit = if (!is.na(object$model_units$time))
      create_unit_string(object$unit_defs[[object$model_units$time]])
        else 'not specified',
    amount_unit = if (!is.na(object$model_units$amount))
      create_unit_string(object$unit_defs[[object$model_units$amount]])
        else 'not specified',
    volume_unit = if (!is.na(object$model_units$volume))
      create_unit_string(object$unit_defs[[object$model_units$volume]])
        else 'not specified'
  )
  class(summary_list) <- "summary.sbmlModel"
  return(summary_list)
}

#' Printing summarized objects of class sbmlModel
#'
#' \code{print} method for object of class summary.sbmlModel created by summarizing
#' objects of class sbmlModel.
#'
#' @param x An object of class \code{sbmlModel}.
#' @param ... Not used.
#'
#' @noRd
#' @export
print.summary.sbmlModel <- function(x, ...) {
  print_summary(x, ...)
  invisible()
}

#' Print an \code{sbmlModel} Object
#'
#' This is a print method for objects of class `sbmlModel`. It supports multiple
#' output modes depending on the `type` argument.
#'
#' @param x An object of class `sbmlModel`.
#' @param ... Additional arguments. The following argument can be passed via \code{...}:
#'   \describe{
#'     \item{\code{type}}{A character string, either `"summary"` (default) or `"equations"`.
#'     Controls whether a high-level summary or the model's differential equations are printed.}
#'   }
#'
#' @examples
#' file <- system.file("extdata/", "simple_oral.sbml", package = "sbmlpbk")
#' model <- load_sbml(file)
#' print(model, type = "summary")
#' print(model, type = "equations")
#'
#' @export
print.sbmlModel <- function(x, ...) {
  args <- list(...)
  type <- if ("type" %in% names(args)) args$type else "summary"
  type <- match.arg(type, choices = c("summary", "equations"))
  switch(
    type,
    summary = {
      print(summary(x))
    },
    equations = {
      print_equations(x)
    }
  )

  invisible()
}

#' @importFrom utils head
#' @noRd
#' @keywords internal
print_summary <- function(x, ...) {
  cat("==================\n")
  cat("SBML Model Summary\n")
  cat("==================\n\n")

  cat("Compartments:\t", x$compartment_count, "\t[ ")
  cat(paste(head(x$compartment_ids, 5), collapse = ", "))
  if (length(x$compartment_ids) > 5) cat(", ...")
  cat(" ]\n")

  cat("Species:\t", x$species_count, "\t[ ")
  cat(paste(head(x$species_ids, 5), collapse = ", "))
  if (length(x$species_ids) > 5) cat(", ...")
  cat(" ]\n")

  cat("Parameters:\t", x$param_count, "\t[ ")
  cat(paste(head(x$param_ids, 5), collapse = ", "))
  if (length(x$param_ids) > 5) cat(", ...")
  cat(" ]\n")

  cat("Functions:\t", x$function_count)
  if (length(x$function_ids) > 0) {
    cat("\t[ ")
    cat(paste(head(x$function_ids, 5), collapse = ", "))
    if (length(x$function_ids) > 5) {
      cat(", ...")
    }
    cat(" ]")
  }
  cat("\n")

  cat("Assignment rules:\t", x$assignment_rule_count, "\n")
  cat("Rate rules:\t", x$rate_rule_count, "\n")
  cat("Reactions:\t", x$reaction_count, "\n")
  cat("\n")

  cat("Time unit:\t", x$time_unit, "\n")
  cat("Amount unit:\t", x$amount_unit, "\n")
  cat("Volume unit:\t", x$volume_unit, "\n")
  cat("\n")

  invisible()
}

#' @noRd
#' @keywords internal
print_equations <- function(model) {
  if (length(model$function_defs) > 0) {
    cat("# --- Functions ---\n")
    sapply(model$function_defs, FUN = function(x) cat(paste0(x, "\n", sep="")))
  }
  cat("\n")
  cat("# --- Assignment rules ---\n")
  if (length(model$assignment_rules) > 0) {
    sapply(model$assignment_rules, FUN = function(x) cat(paste0(x, "\n", sep="")))
  }
  cat("\n")
  if (length(model$rate_rules) > 0) {
    cat("# --- Rate rules ---\n")
    sapply(model$rate_rules, FUN = function(x) cat(paste0(x, "\n", sep="")))
  }
  cat("\n")
  cat("# --- Transfer ODEs ---\n")
  sapply(model$odes, FUN = function(x) cat(paste0(x, "\n", sep="")))
  cat("\n")

  invisible()
}
