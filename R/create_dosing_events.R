#' Create deSolve-Compatible Events from specified dosing scenarios
#'
#' Converts a list of structured dosing scenarios into an event data frame
#' suitable for use with the \pkg{deSolve} package in differential equation
#' modeling.
#'
#' This function interprets each dosing scenario (specified as a named list)
#' based on its type (e.g., bolus vs. continuous, single vs. repeated) and
#' generates corresponding time-stamped events for use in PBK or PK simulations.
#'
#' @param dosing_scenarios A list of dosing scenarios, where each element is a
#'   named list following a fixed structure. Each scenario is described by a
#'   scenario \code{type} describing the type of the dosing pattern (single vs.
#'   repeated and bolus vs. continuous) and a \code{target}. Furthermore, all
#'   scenarios must have a \code{time} and an \code{amount}, and may include
#'   the an \code{interval}, a \code{duration}, and an \code{until}
#'   specification, depending on the dosing type.
#'
#' @return A \code{data.frame} formatted for use as the \code{events} argument
#'   in \code{deSolve::ode()}, with columns:
#'   \describe{
#'     \item{\code{var}}{Name of the state variable to which the dose applies (e.g., "A_compartment").}
#'     \item{\code{time}}{Numeric time of the event (in hours).}
#'     \item{\code{value}}{Amount added to the state variable.}
#'     \item{\code{method}}{Typically \code{"add"} for bolus or \code{"rate"} for continuous infusion.}
#'   }
#'
#' @details
#' Recognized \code{type} values:
#' \itemize{
#'   \item \code{"single_bolus"} – single dose at \code{time}.
#'   \item \code{"repeated_bolus"} – repeated bolus doses between \code{time} and \code{until} at interval \code{interval}.
#'   \item \code{"single_continuous"} – continuous infusion from \code{time} to \code{time + duration}.
#'   \item \code{"repeated_continuous"} – multiple infusions each lasting \code{duration}, repeating every \code{interval} until \code{until}.
#' }
#'
#' The target compartment/state variable for each dose is assumed to be specified
#' in a field like \code{target} or by default as \code{"A_Gut"} if unspecified.
#'
#' @examples
#' dosing_scenarios <- list(
#'   list(target = "AGut", dose_type = "single_bolus", amount = 100, time = 0),
#'   list(
#'     target = "AGut",
#'     dose_type = "repeated_bolus",
#'     amount = 50,
#'     time = 12,
#'     interval = 24,
#'     until = 96
#'   ),
#'   list(target = "AAir", dose_type = "single_continuous", amount = 20, time = 0, duration = 24),
#'   list(
#'     target = "AAir",
#'     dose_type = "repeated_continuous",
#'     amount = 20,
#'     time = 56,
#'     duration = 12,
#'     interval = 24,
#'     until = 96
#'   )
#' )
#'
#' events <- create_dosing_events(dosing_scenarios)
#' head(events)
#'
#' @importFrom utils head
#' @export
create_dosing_events <- function(dosing_scenarios) {
  bolus_list <- list()
  continuous_list <- list()

  for (event in dosing_scenarios) {
    if (is.null(event$amount)) {
      warning("Skipping event with missing amount.")
      next
    }
    if (is.null(event$target)) {
      warning("Skipping event with missing target.")
      next
    }
    if (is.null(event$time)) {
      warning("Skipping event with missing time.")
      next
    }
    if (event$dose_type == "single_bolus") {
      bolus_list[[length(bolus_list) + 1]] <- data.frame(
        var = event$target,
        time = event$time,
        value = event$amount,
        method = "add",
        stringsAsFactors = FALSE
      )
    } else if (event$dose_type == "repeated_bolus") {
      if (is.null(event$interval)) {
        warning("Skipping repeated event with missing interval.")
        next
      }
      if (is.null(event$until)) {
        warning("Skipping repeated event with missing until.")
        next
      }
      times <- seq(event$time, event$until, by = event$interval)
      for (t in times) {
        bolus_list[[length(bolus_list) + 1]] <- data.frame(
          var = event$target,
          time = t,
          value = event$amount,
          method = "add",
          stringsAsFactors = FALSE
        )
      }
    } else if (event$dose_type == "single_continuous") {
      if (is.null(event$duration)) {
        warning("Skipping continuous event with missing duration.")
        next
      }
      continuous_df <- data.frame(
        var = rep(event$target, 2),
        time = c(event$time, event$time + event$duration),
        value = c(event$amount, 0),
        method = rep("replace", 2),
        stringsAsFactors = FALSE
      )
      continuous_list[[length(continuous_list) + 1]] <- continuous_df
    } else if (event$dose_type == "repeated_continuous") {
      if (is.null(event$interval)) {
        warning("Skipping repeated event with missing interval.")
        next
      }
      if (is.null(event$until)) {
        warning("Skipping repeated event with missing until.")
        next
      }
      if (is.null(event$duration)) {
        warning("Skipping continuous event with missing duration.")
        next
      }
      times <- seq(event$time, event$until, by = event$interval)
      for (t in times) {
        continuous_df <- data.frame(
          var = rep(event$target, 2),
          time = c(t, t + event$duration),
          value = c(event$amount, 0),
          method = rep("replace", 2),
          stringsAsFactors = FALSE
        )
        continuous_list[[length(continuous_list) + 1]] <- continuous_df
      }
    }
  }

  event_data <- do.call(rbind, c(bolus_list, continuous_list))
  event_data <- event_data[order(event_data$time), ]
  rownames(event_data) <- NULL
  return(event_data)
}
