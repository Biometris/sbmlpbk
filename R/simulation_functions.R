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

#' Create a deSolve-compatible model function
#'
#' This function generates a closure that can be used as the `func` argument in
#' \code{\link[deSolve]{ode}} or related solvers. The returned function computes
#' derivatives from a model specification (ODEs and assignment rules), while
#' optionally including selected compartment and parameter values in the output.
#'
#' @param model SBML model S3 class.
#' @param outputs Optional named list of equations (character strings). Each element
#'   defines a derived output, e.g. concentrations or calculated parameters:
#'   \code{list(CGut = "Agut / Gut", BW = "BW")}.
#'
#' @return A function with signature \code{function(time, state, parameters)} suitable
#' for use with \code{\link[deSolve]{ode}}. The returned function produces a list with:
#'   \itemize{
#'     \item First element: vector of state derivatives (in the same order as \code{state}).
#'     \item Subsequent elements: named numeric values of selected compartments
#'       and/or parameters (if requested).
#'   }
#'
#' @examples
#' library(deSolve)
#'
#' # Load example model
#' model_file <- system.file("extdata/", "simple_oral.sbml", package = "sbmlpbk")
#' model <- load_sbml(model_file)
#'
#' # Create deSolve-compatible function
#' func <- create_desolve_func(
#'   model,
#'   list(
#'     CGut = "AGut / Gut",
#'     BW = "BW"
#'   )
#' )
#'
#' # Set initial states
#' initial_states <- setNames(rep(0, length(model$species)), names(model$species))
#'
#' # Set input events (single unit bolus at time 1)
#' eventdat <- data.frame(var = c("AGut"), time = c(1), value = c(1), method = c("add"))
#'
#' # Run simulation
#' out <- ode(
#'   y = initial_states,
#'   times = seq(0, 24, by = 1),
#'   func = func,
#'   parms = model$params,
#'   events = list(data = eventdat)
#' )
#' head(out)
#'
#' @seealso \code{\link[deSolve]{ode}}
#' @export
create_desolve_func <- function(
    model,
    outputs = NULL
) {
  function(time, state, parameters) {
    with(as.list(c(state, parameters)), {

      # Evaluate assignment rules and assign to local variables
      if (length(model$assignment_rules) > 0) {
        for (eqn in model$assignment_rules) {
          # Evaluate in local environment
          eval(parse(text = eqn), envir = environment())
        }
      }

      # Evaluate derivatives from ODEs
      derivs <- lapply(
        model$odes,
        function(eqn) eval(parse(text = eqn), envir = environment())
      )
      names(derivs) <- names(model$odes)

      # Handle rate rules
      if (length(model$rate_rules) > 0) {
        # Evaluate derivatives from rate rules
        dy_rate_rules <- lapply(
          model$rate_rules,
          function(eqn) eval(parse(text = eqn), envir = environment())
        )
        names(dy_rate_rules) <- names(model$rate_rules)

        all_vars <- union(names(dy_rate_rules), names(derivs))

        # Merge: rate rules take precedence
        derivs[names(dy_rate_rules)] <- dy_rate_rules
        derivs <- derivs[all_vars]
      }

      # Evaluate requested outputs
      if (!is.null(outputs)) {
        output_values <- lapply(
          outputs,
          function(expr) eval(parse(text = expr), envir = environment())
        )
        names(output_values) <- names(outputs)
      } else {
        output_values <- list()
      }

      return(list(derivs, do.call(c, output_values)))
    })
  }
}

#' Create a deSolve-compatible times sequence
#'
#' Creates a \code{deSolve} compliant \code{times} sequence that can be used as
#' the `times` argument in \code{\link[deSolve]{ode}}, specifying the times for
#' which simulation output is wanted.
#'
#' @param model An object of class \code{sbmlModel}.
#' @param duration A number describing the total duration of the simulation.
#' @param step A number describing the resolution in terms of step size.
#' @param unit An optional character string describing the time unit of the
#' specified duration and step length. Timings are aligned with the
#' time unit of the \code{model}. Current options are "h" for hours (default),
#' "d" for days, or "model" when assuming the specified duration and step length
#' are considered in line with the time unit of the \code{model}.
#'
#' @return A \code{vector} of simulation output times formatted for use as the
#' \code{times} argument in \code{\link[deSolve]{ode}}.
#'
#' @examples
#' model_file <- system.file("extdata/", "simple_oral.sbml", package = "sbmlpbk")
#' model <- load_sbml(model_file)
#'
#' times <- create_desolve_times(
#'   model,
#'   duration = 10, # 10 days
#'   step = 1/24,   # every hour
#'   unit = 'd'     # times in days
#' )
#'
#' head(times)
#'
#' @importFrom utils head
#' @seealso \code{\link[deSolve]{ode}}
#' @export
create_desolve_times <- function(
    model,
    duration,
    step,
    unit = 'h'
) {
  time_conversion_factor = 1
  if (unit != 'model') {
    if (is.na(model$model_units$time)) {
      stop("Model time unit not specified.")
    }
    time_conversion_factor = 1 / get_time_conversion_factor(
      model$unit_defs[[model$model_units$time]],
      unit
    )
  }

  times <- seq(
    0,
    time_conversion_factor * duration,
    time_conversion_factor * step
  )
  return(times)
}

#' Create a deSolve-compatible events data frame
#'
#' Converts a list of structured dosing scenarios into an event data frame
#' that can be used as the `events` argument for running PBK model simulations
#' using \code{\link[deSolve]{ode}}.
#'
#' This function interprets each dosing scenario (specified as a named list)
#' based on its type (e.g., bolus vs. continuous, single vs. repeated) and
#' generates corresponding time-stamped events for use in PBK or PK simulations.
#'
#' @param model An object of class \code{sbmlModel} for which the
#' dosing events should be created.
#' @param dosing_events A list of dosing scenarios, where each element is a
#'   named list following a fixed structure. Each scenario is described by a
#'   scenario \code{type} describing the type of the dosing pattern (single vs.
#'   repeated and bolus vs. continuous) and a \code{target}. Furthermore, all
#'   scenarios must have a \code{time} and an \code{amount}, and may include
#'   the an \code{interval}, a \code{duration}, and an \code{until}
#'   specification, depending on the dosing type.
#' @param time_unit An optional character string describing the time unit
#' of the specified \code{dosing_events}. Event timings are aligned with the
#' time unit of the \code{model}. Current options are "h" for hours (default),
#' "d" for days, or "model" when assuming the timings of the specified events are
#' already aligned with the time unit of the \code{model}.
#' @param amount_unit An optional character string describing the amount unit
#' of the specified \code{dosing_events}. Amounts of the dosing events are
#' aligned with the amount unit of the \code{model}. Current options are "ng",
#' "ug" (default), "mg", or "model" (assuming the timings of the specified
#' events are already aligned with the time unit of the \code{model}).
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
#' model_file <- system.file("extdata/", "simple_oral.sbml", package = "sbmlpbk")
#' model <- load_sbml(model_file)
#' dosing_events <- list(
#'   list(target = "AGut", dose_type = "single_bolus", amount = 100, time = 0),
#'   list(
#'     target = "AGut",
#'     dose_type = "repeated_bolus",
#'     amount = 50,
#'     time = 12,
#'     interval = 24,
#'     until = 96
#'   ),
#'   list(
#'     target = "AAir",
#'     dose_type = "single_continuous",
#'     amount = 20,
#'     time = 0,
#'     duration = 24
#'   ),
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
#' events <- create_desolve_events(model, dosing_events)
#' head(events)
#'
#' @importFrom utils head
#' @seealso \code{\link[deSolve]{ode}}
#' @export
create_desolve_events <- function(
  model,
  dosing_events,
  time_unit = 'h',
  amount_unit = 'ug'
) {
  bolus_list <- list()
  continuous_list <- list()

  # Conversion factor to align events time unit with model time unit
  time_conversion_factor = 1
  if (time_unit != 'model') {
    if (is.na(model$model_units$time)) {
      stop("Model time unit not specified.")
    }
    time_conversion_factor = 1 / get_time_conversion_factor(
      model$unit_defs[[model$model_units$time]],
      time_unit
    )
  }

  # Conversion factor to align events amount unit with model amount unit
  amount_conversion_factor = 1
  if (amount_unit != 'model') {
    if (is.na(model$model_units$amount)) {
      stop("Model time amount not specified.")
    }
    amount_conversion_factor = 1 / get_amount_conversion_factor(
      model$unit_defs[[model$model_units$amount]],
      amount_unit
    )
  }

  # Iterate over events
  for (event in dosing_events) {
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
        value = event$amount * amount_conversion_factor,
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
          time = t * time_conversion_factor,
          value = event$amount * amount_conversion_factor,
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
        time = c(event$time, event$time + event$duration) * time_conversion_factor,
        value = c(event$amount * amount_conversion_factor, 0),
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
          time = c(t, t + event$duration) * time_conversion_factor,
          value = c(event$amount * amount_conversion_factor, 0),
          method = rep("replace", 2),
          stringsAsFactors = FALSE
        )
        continuous_list[[length(continuous_list) + 1]] <- continuous_df
      }
    }
  }

  event_data <- do.call(rbind, c(bolus_list, continuous_list))
  if (length(event_data) > 0) {
    event_data <- event_data[order(event_data$time), ]
  }
  rownames(event_data) <- NULL

  return(event_data)
}

