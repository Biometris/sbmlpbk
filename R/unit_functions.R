# SI prefix map
si_prefix_string <- list(
  "3"  = "k",
  "2"  = "h",
  "1"  = "da",
  "0"  = "",
  "-1" = "d",
  "-2" = "c",
  "-3" = "m",
  "-6" = "u",
  "-9" = "n",
  "-12" = "p"
)

# Time unit multipliers
time_unit_multipliers <- list(
  "1"       = "s",
  "60"      = "min",
  "3600"    = "h",
  "86400"   = "d",
  "31557600"= "y"
)

# Base unit strings
base_unit_strings <- list(
  dimensionless = "",
  second        = "s",
  gram          = "g",
  liter         = "L",
  litre         = "L",
  meter         = "m",
  metre         = "m",
  mole          = "mol",
  kelvin        = "K"
)

#' Build unit string from parsed unit definition structure.
#'
#' @noRd
#' @keywords internal
create_unit_string <- function(unit_def) {
  unit_string_parts <- character()

  for (i in seq_along(unit_def$units)) {
    u <- unit_def$units[[i]]
    part <- create_unit_part_string(u, is_first = (i == 1))
    if (nzchar(part)) {
      unit_string_parts <- c(unit_string_parts, part)
    }
  }

  if (length(unit_string_parts) == 0) {
    return("dimensionless")
  } else {
    return(paste0(unit_string_parts, collapse = ""))
  }
}

#' Helper function to build one unit part string.
#'
#' @noRd
#' @keywords internal
create_unit_part_string <- function(u, is_first = FALSE) {
  kind <- tolower(u$kind)
  scale <- ifelse(is.na(u$scale), 0, u$scale)
  exponent <- ifelse(is.na(u$exponent), 1, u$exponent)
  multiplier <- ifelse(is.na(u$multiplier), 1, u$multiplier)

  # Special handling for time
  if (kind == "second" && as.character(multiplier) %in% names(time_unit_multipliers)) {
    base_unit_str <- time_unit_multipliers[[as.character(multiplier)]]
    multiplier <- 1
  } else {
    base_unit_str <- base_unit_strings[[kind]]
    if (is.null(base_unit_str)) base_unit_str <- kind # fallback
  }

  scale_str <- si_prefix_string[[as.character(scale)]]
  if (is.null(scale_str)) scale_str <- "" # fallback

  if (exponent >= 0) {
    operator_str <- if (is_first) "" else "."
  } else {
    operator_str <- "/"
    exponent <- -exponent
  }

  exponent_str <- if (exponent != 1) paste0("^", exponent) else ""
  multiplier_str <- if (multiplier != 1) paste0(multiplier, ".") else ""

  ucum_unit <- paste0(operator_str, multiplier_str, scale_str, base_unit_str, exponent_str)
  return(ucum_unit)
}

#' @noRd
#' @keywords internal
is_amount_unit <- function(unit_def) {
  # Recognized amount units
  amount_kinds <- c("mole", "gram")

  # Amount unit definition should only have one part
  if (length(unit_def$units) != 1) {
    return(FALSE)
  }

  # Unit part should be of amount kind
  unit_part <- unit_def$units[[1]]
  if (!(tolower(unit_part$kind) %in% amount_kinds)) {
    return(FALSE)
  }

  # Unit part should have exponent 1
  if (unit_part$exponent != 1) {
    return(FALSE)
  }

  # All checks passed
  return(TRUE)
}

#' Check if a unitDefinition is a volume unit (litre)
#'
#' @noRd
#' @keywords internal
is_volume_unit <- function(unit_def) {
  # Recognized volume units
  volume_kinds <- c("litre")

  # Volume unit definition should only have one part
  if (length(unit_def$units) != 1) {
    return(FALSE)
  }

  # Unit part should be of volume kind
  unit_part <- unit_def$units[[1]]
  if (!(tolower(unit_part$kind) %in% volume_kinds)) {
    return(FALSE)
  }

  # Unit part should have exponent 1
  if (unit_part$exponent != 1) {
    return(FALSE)
  }

  # All checks passed
  return(TRUE)
}

#' Check if a unitDefinition is a time unit (second, minute, hour)
#'
#' @noRd
#' @keywords internal
is_time_unit <- function(unit_def) {
  # Recognized time units
  time_kinds <- c("second")

  # Time unit definition should only have one part
  if (length(unit_def$units) != 1) {
    return(FALSE)
  }

  # Unit part should be of kind second
  unit_part <- unit_def$units[[1]]
  if (!(tolower(unit_part$kind) %in% time_kinds)) {
    return(FALSE)
  }

  # Unit part should have exponent 1
  if (unit_part$exponent != 1) {
    return(FALSE)
  }

  # All checks passed
  return(TRUE)
}

#'
#' @noRd
#' @keywords internal
get_amount_conversion_factor <- function(unit_def, to_unit = "ug") {
  # Supported target units and their value in grams
  to_unit <- tolower(to_unit)
  to_unit_to_g <- c(
    kg = 1e3,
    g  = 1,
    mg = 1e-3,
    ug = 1e-6,
    ng = 1e-9
  )

  if (!to_unit %in% names(to_unit_to_g)) {
    stop(sprintf("Unsupported target unit '%s'", to_unit))
  }

  # Amount unit definition should only have one part
  if (length(unit_def$units) != 1) {
    stop(sprintf("Provided unit is not an amount unit."))
  }

  # Unit part should be of amount kind
  amount_kinds <- c("mole", "gram")
  unit_part <- unit_def$units[[1]]
  if (!(tolower(unit_part$kind) %in% amount_kinds)) {
    stop(sprintf("Cannot convert kind '%s' to grams", unit_part$kind))
  }

  # Unit part should have exponent 1
  if (unit_part$exponent != 1) {
    stop(sprintf("Cannot convert provided exponent '%s' to grams.", unit_part$exponent))
  }

  # Conversion factor of unit def to grams
  scale_factor <- 10^(ifelse(is.na(unit_part$scale), 0, unit_part$scale))
  mult_factor <- ifelse(is.na(unit_part$multiplier), 1, unit_part$multiplier)
  unit_to_g <- scale_factor * mult_factor

  # Conversion factor gram to target
  g_to_target <- as.numeric(1 / to_unit_to_g[to_unit])

  # Convert from grams to desired unit
  unit_to_target <- unit_to_g * g_to_target

  return(unit_to_target)
}

#'
#' @noRd
#' @keywords internal
get_volume_conversion_factor <- function(unit_def, to_unit = "L") {
  # Supported target units and their value in litres
  to_unit <- tolower(to_unit)
  to_unit_to_L <- c(
    l  = 1,
    ml = 1e-3
  )

  if (!to_unit %in% names(to_unit_to_L)) {
    stop(sprintf("Unsupported target unit '%s'", to_unit))
  }

  # Litre unit definition should only have one part
  if (length(unit_def$units) != 1) {
    stop(sprintf("Provided unit is not a volume unit."))
  }

  # Unit part should be of litre kind
  unit_part <- unit_def$units[[1]]
  if (tolower(unit_part$kind) != 'litre') {
    stop(sprintf("Provided unit is not a volume unit."))
  }

  # Unit part should have exponent 1
  if (unit_part$exponent != 1) {
    stop(sprintf("Cannot convert provided exponent '%s' to litres", unit_part$exponent))
  }

  # Conversion factor of unit def to litres
  scale_factor <- 10^(ifelse(is.na(unit_part$scale), 0, unit_part$scale))
  mult_factor <- ifelse(is.na(unit_part$multiplier), 1, unit_part$multiplier)
  unit_to_L <- scale_factor * mult_factor

  # Conversion factor litres to target
  L_to_target <- as.numeric(1 / to_unit_to_L[to_unit])

  # Convert from litres to desired unit
  unit_to_target <- unit_to_L * L_to_target

  return(unit_to_target)
}

#'
#' @noRd
#' @keywords internal
get_time_conversion_factor <- function(unit_def, to_unit = "s") {
  # Supported target units and their value in seconds
  to_unit <- tolower(to_unit)
  to_unit_to_sec <- c(
    s = 1,
    h  = 3600,
    d = 86400
  )

  if (!to_unit %in% names(to_unit_to_sec)) {
    stop(sprintf("Unsupported target unit '%s'", to_unit))
  }

  # Amount unit definition should only have one part
  if (length(unit_def$units) != 1) {
    stop(sprintf("Provided unit is not a time unit."))
  }

  # Unit part should be of second kind
  unit_part <- unit_def$units[[1]]
  if (tolower(unit_part$kind) != 'second') {
    stop(sprintf("Provided unit is not a time unit."))
  }

  # Unit part should have exponent 1
  if (unit_part$exponent != 1) {
    stop(sprintf("Provided unit is not a time unit."))
  }

  # Conversion factor of unit def to seconds
  scale_factor <- 10^(ifelse(is.na(unit_part$scale), 0, unit_part$scale))
  mult_factor <- ifelse(is.na(unit_part$multiplier), 1, unit_part$multiplier)
  unit_to_sec <- scale_factor * mult_factor

  # Conversion factor seconds to target
  sec_to_target <- 1 / as.numeric(to_unit_to_sec[to_unit])

  # Convert from unit_to_sec to desired unit
  unit_to_target <- unit_to_sec * sec_to_target

  return(unit_to_target)
}
