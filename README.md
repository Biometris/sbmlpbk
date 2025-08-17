# The `sbmlpbk` package

An R package for importing Physiologically Based Kinetic (PBK) models encoded in the Systems Biology Markup Language (SBML) format, enabling  simulation using the `deSolve` package. This package is still under development.

## Installation

Install from GitHub (using the [remotes](https://github.com/r-lib/remotes) package):

``` r
remotes::install_github("Biometris/sbmlpbk", ref = "main", dependencies = TRUE)
```
## Quickstart

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

The code below shows how to run simulations using the `ode` function of `deSolve`.

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

## Detailed example

For a more details, see the vignette.

``` r
vignette('sbmlpbk', package='sbmlpbk')
```
