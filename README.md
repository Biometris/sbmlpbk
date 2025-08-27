# The `sbmlpbk` package

An R package for importing Physiologically Based Kinetic (PBK) models encoded in the Systems Biology Markup Language (SBML) format, enabling  simulation using the `deSolve` package. This package is still under development.

## Installation

Install from GitHub (using the [remotes](https://github.com/r-lib/remotes) package):

``` r
remotes::install_github("Biometris/sbmlpbk", ref = "main", dependencies = TRUE)
```
## Quickstart

### Loading SBML files

To load an SBML model from a file, simply use the `load_sbml` function. The following example loads the file `simple_oral.sbml` provided with the package:

``` r
# Load package
library(sbmlpbk)

# Get filename
file_simple_oral <- system.file("extdata/", "simple_oral.sbml", package = "sbmlpbk")

# Load model
model <- load_sbml(file_simple_oral)

# Get model summary
summary(model)
```

### Running simulations

The code below shows how to run simulations on the model loaded above using the `ode` function of `deSolve`.

``` r
# Load package deSolve
library(deSolve)

# Load model functions
load_functions(model)

# Load params
params_csv <- system.file("extdata/", "simple_oral_params.csv", package = "sbmlpbk")
params <- load_params(model, file = params_csv, param_instance = "simple_PARAM")

# Set input species
input_species <- "AGut"

# Set input events (single unit bolus at time 1)
eventdat <- data.frame(var = c(input_species), time = c(1), value = c(1), method = c("add"))

# Set initial states
initial_states <- setNames(rep(0, length(model$species)), names(model$species))

# Set simulation times
times <- seq(0, 40, 1)

# Load deSolve model function
func <- create_desolve_func(model)

# Simulate
out <- ode(
  y = initial_states,
  times = times,
  func = func,
  parms = params,
  events = list(data = eventdat)
)

# Plot results
plot(out)
```

### Helper functions for unit alignment and simulation

`sbmlpbk` offers a number of functions to make it easier to run simulations with `deSolve` on loaded models for which time units have been specified. These functions automatically align e.g. time resolution and substance amount units of the model with the desired units for dosing and simulation. 

The code below how the functions `create_desolve_times` and `create_desolve_events` are used to generate the `deSolve` timings and events, for a simulation of 10 days with a dosing pattern of a repeated bolus dose with amount 10, starting at day 4, repeating every day until time day 10.

``` r
# Create deSolve timings; 10 days with hourly evaluation resolution
times <- create_desolve_times(
  model,
  duration = 10,
  step = 1/24,
  unit = 'd' # time unit day
)

# Create and set deSolve dosing events for the model
eventdat <- create_desolve_events(
  model,
  dosing_events = list(
    list(
      target = "AGut",
      dose_type = "repeated_bolus",
      time = 4,
      amount = 10,
      interval = 1,
      until = 10
    )
  ),
  time_unit = 'd', # events time unit was in days
  amount_unit = 'ug'
)

# Set initial states
initial_states <- setNames(rep(0, length(model$species)), names(model$species))

# Load deSolve model function
func <- create_desolve_func(model)

# Simulate
out <- ode(
  y = initial_states,
  times = times,
  func = func,
  parms = model$params,
  events = list(data = eventdat)
)

# Set up plotting layout
n_cols <- 3
n_rows <- ceiling(length(model$species) / n_cols)
par(mfrow = c(n_rows, n_cols), mar = c(4, 4, 2, 1))

# Loop through species and plot
for (species in names(model$species)) {
  plot(out[,1], out[,species],
       type = "l",
       main = species,
       xlab = paste("time [", summary(model)$time_unit, "]", sep=""),
       ylab = paste(species, " [", summary(model)$amount_unit, "]", sep=""))
}
```

## Detailed example

For a more details, see the vignette.

``` r
vignette('sbmlpbk', package='sbmlpbk')
```
