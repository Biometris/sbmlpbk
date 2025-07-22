file_simple_oral <- system.file("extdata/", "simple_oral.sbml", package = "sbmlpbk")

expect_error(load_sbml(sbml_file = 1))
expect_silent(load_sbml(sbml_file = file_simple_oral))
