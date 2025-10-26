## --- Test load_sbml ---

## Test invalid file path
expect_error(load_sbml(sbml_file = 1))

## Test loading a valid SBML file
file_simple_oral <- system.file("extdata/", "simple_oral.sbml", package = "sbmlpbk")
expect_silent(load_sbml(sbml_file = file_simple_oral))

## --- Test get_dependency_order ---

## Test simple linear dependency
deps <- list(
  x = c("a", "b"),
  y = c("x"),
  z = c("y")
)
order <- sbmlpbk:::get_dependency_order(deps)
expect_equal(order, c(1, 2, 3), info = "Linear dependencies should be in declared order")

# Test multiple independent rules
deps <- list(
  x = c("a", "b"),
  y = c("c", "d")
)
order <- sbmlpbk:::get_dependency_order(deps)
expect_true(all(order %in% c(1, 2)), info = "Independent rules can be in any order")

# Test dependency chain with external parameters
deps <- list(
  A = "k1",
  B = "A",
  C = c("B", "k2")
)
order <- sbmlpbk:::get_dependency_order(deps)
expect_equal(order, c(1, 2, 3), info = "Should order A -> B -> C")

# Test detect circular dependency
deps <- list(
  A = "B",
  B = "A"
)
expect_error(sbmlpbk:::get_dependency_order(deps), info = "Should throw error for circular dependency")

# Test empty input
deps <- list()
order <- sbmlpbk:::get_dependency_order(deps)
expect_equal(order, integer(0), info = "Empty list should return empty integer vector")
