#' @noRd
#' @keywords internal
parse_function_definition <- function(fn_id, fn_node, ns) {
  # Find <lambda> inside <math>
  lambda_node <- xml2::xml_find_first(fn_node, ".//mathml:lambda", ns)
  if (is.null(lambda_node)) {
    stop(paste("No <lambda> found in functionDefinition:", fn_id))
  }

  children <- xml2::xml_children(lambda_node)

  # Extract argument names from <bvar>
  bvar_nodes <- children[xml2::xml_name(children) == "bvar"]
  args <- sapply(bvar_nodes, function(bvar) {
    ci_node <- xml2::xml_find_first(bvar, ".//mathml:ci", ns)
    if (is.null(ci_node)) stop("Missing <ci> in <bvar>")
    xml2::xml_text(ci_node)
  })

  # The last child is the function body (e.g. <apply>...</apply>)
  body_node <- children[[length(children)]]

  # Convert MathML expression to R using your helper
  body_expr <- mathml_to_r(body_node)


  # Build the R function definition as a string
  fn_code <- paste0(
    fn_id, " <- function(", paste(args, collapse = ", "), ") { ", body_expr, " }"
  )

  return(fn_code)
}

#' Extracts kinetic law math expression from mathml xml node.
#'
#' @param xml_node XML node.
#' @param xml_node XML namespaces.
#'
#' @noRd
#' @keywords internal
get_kinetic_law_expression <- function(xml_node, ns) {
  # Extract kinetic law
  kinetic_law_node <- xml2::xml_find_first(xml_node, ".//sbml:kineticLaw", ns)
  if (!is.null(kinetic_law_node)) {
    math_node <- xml2::xml_find_first(kinetic_law_node, ".//mathml:math", ns)
    if (!is.null(math_node)) {
      mathml_to_r(xml2::xml_children(math_node))
    } else {
      stop(paste("Missing math node in kinetc law XML node"))
    }
  } else {
    stop(paste("Missing kinetc law XML node"))
  }
}

#' Load SBML model
#'
#' @param sbml_file File path of the SBML file.
#'
#' @returns An object of class \code{sbmlModel}.
#'
#' @examples
#' file <- system.file("extdata/", "simple_oral.sbml", package = "sbmlpbk")
#' model <- load_sbml(file)
#'
#' @export
load_sbml <- function(sbml_file) {
  if (missing(sbml_file)
      || !is.character(sbml_file)
      || length(sbml_file) > 1
      || file.access(sbml_file, mode = 4) == -1
      || !(tools::file_ext(sbml_file) %in% c("xml", "sbml", "txt"))
  ) {
    stop("sbml_file should be a character string pointing to a readable SBML file (.xml or .sbml).\n")
  }

  sbml_xml <- xml2::read_xml(sbml_file)

  # Check that root namespace is a valid SBML Level 3 namespace
  valid_sbml_namespaces <- c(
    "http://www.sbml.org/sbml/level3/version1/core",
    "http://www.sbml.org/sbml/level3/version2/core"
  )

  # Get all namespace declarations from the root node
  ns_map <- xml2::xml_ns(sbml_xml)

  # Extract the default namespace (no prefix), if any
  root_ns <- ns_map[[1]]

  if (!(root_ns %in% valid_sbml_namespaces)) {
    stop(
      sprintf(
        "Unsupported SBML namespace: '%s'. Expected Level 3 Version 1 or 2.",
        root_ns
      )
    )
  }

  # Define known namespaces for SBML and MathML
  ns <- c(
    sbml = root_ns,
    mathml = "http://www.w3.org/1998/Math/MathML"
  )

  # Get model time unit
  time_unit <- xml2::xml_attr(sbml_xml, "timeUnits")

  # Get model substance amount unit
  amount_unit <- xml2::xml_attr(sbml_xml, "substanceUnits")

  # Extract compartments and compartment sizes
  compartment_nodes <- xml2::xml_find_all(sbml_xml, ".//sbml:listOfCompartments/sbml:compartment", ns)
  compartment_ids <- xml2::xml_attr(compartment_nodes, "id")
  compartment_sizes <- as.numeric(xml2::xml_attr(compartment_nodes, "size"))

  # Extract species and initial conditions
  species_nodes <- xml2::xml_find_all(sbml_xml, ".//sbml:listOfSpecies/sbml:species", ns)
  species_ids <- xml2::xml_attr(species_nodes, "id")

  # Extract parameters
  param_nodes <- xml2::xml_find_all(sbml_xml, ".//sbml:listOfParameters/sbml:parameter", ns)
  param_ids <- xml2::xml_attr(param_nodes, "id")
  param_vals <- as.numeric(xml2::xml_attr(param_nodes, "value"))
  parameters <- stats::setNames(param_vals, param_ids)

  # Extract species and initial conditions
  functions_list <- xml2::xml_find_all(sbml_xml, ".//sbml:listOfFunctionDefinitions/sbml:functionDefinition", ns)
  function_defs <- stats::setNames(
    # Convert each rule to R syntax and store in a named list
    lapply(functions_list, function(fct) {
      fct_id <- xml2::xml_attr(fct, "id")
      # Find the <math> node
      math_node <- xml2::xml_find_first(fct, ".//mathml:math", ns)
      # Convert MathML to R
      expression <- parse_function_definition(fct_id, math_node, ns)
    }),
    sapply(functions_list, function(fct) xml2::xml_attr(fct, "id"))
  )

  # Check for initial assignments
  initial_assignment_nodes <- xml2::xml_find_all(sbml_xml, ".//sbml:listOfInitialAssigments/sbml:initialAssignment", ns)
  if (length(initial_assignment_nodes) > 0) {
    stop("SBML model contains initial assigmnents, which are currently not supported")
  }

  # Extract assignment rules
  rule_nodes <- xml2::xml_find_all(sbml_xml, ".//sbml:listOfRules/sbml:assignmentRule", ns)
  rules <- stats::setNames(
    # Convert each rule to R syntax and store in a named list
    lapply(rule_nodes, function(rule) {
      variable <- xml2::xml_attr(rule, "variable")
      # Find the <math> node
      math_node <- xml2::xml_find_first(rule, ".//mathml:math", ns)
      # Convert MathML to R
      expression <- mathml_to_r(xml2::xml_children(math_node)[[1]])
      paste0(variable, " <- ", expression)
    }),
    sapply(rule_nodes, function(rule) xml2::xml_attr(rule, "variable"))
  )

  # Check for initial assignments
  constraint_nodes <- xml2::xml_find_all(sbml_xml, ".//sbml:listOfConstraints/sbml:constraint", ns)
  if (length(constraint_nodes) > 0) {
    stop("SBML model contains constraints, which are currently not supported")
  }

  # Extract reactions and kinetic laws
  reaction_nodes <- xml2::xml_find_all(sbml_xml, ".//sbml:listOfReactions/sbml:reaction", ns)
  reaction_eqns <- sapply(reaction_nodes, function(rxn) {
    law <- xml2::xml_find_first(rxn, ".//sbml:kineticLaw/mathml:math", ns)
    # remove whitespace
    gsub("\\s+", "", xml2::xml_text(law))
  })

  # Extract odes
  reactions_list <- xml2::xml_find_all(sbml_xml, ".//sbml:listOfReactions/sbml:reaction", ns)
  reactions_data <- sapply(reactions_list, function(reaction) {
    # Extract the reaction ID
    reaction_id <- xml2::xml_attr(reaction, "id")

    # Extract reactants
    reactant_id <- xml2::xml_attr(xml2::xml_find_all(reaction, ".//sbml:listOfReactants/sbml:speciesReference", ns), "species")

    # Extract products
    product_id <- xml2::xml_attr(xml2::xml_find_all(reaction, ".//sbml:listOfProducts/sbml:speciesReference", ns), "species")

    # Extract kinetic law math expression
    kinetic_law_expression <- get_kinetic_law_expression(reaction, ns)

    # Return list for this reaction
    list(
      id = reaction_id,
      reactant = reactant_id,
      product = product_id,
      kinetic_law = kinetic_law_expression
    )
  }, simplify = FALSE)

  # Check for initial assignments
  event_nodes <- xml2::xml_find_all(sbml_xml, ".//sbml:listOfEvents/sbml:event", ns)
  if (length(constraint_nodes) > 0) {
    warning("SBML model contains events, which are ignored")
  }

  # Build ODEs for each species
  odes <- stats::setNames(vector("list", length(species_ids)), species_ids)
  for (reaction in reactions_data) {
    reactants <- reaction$reactant
    products <- reaction$product
    rate <- reaction$kinetic_law

    # Add rate to product species (production)
    for (prod in products) {
      odes[[prod]] <- if (is.null(odes[[prod]])) {
        paste0("d", prod, " <- ", rate)
      } else {
        paste(odes[[prod]], "+", rate)
      }
    }

    # Subtract rate from reactant species (consumption)
    for (reac in reactants) {
      odes[[reac]] <- if (is.null(odes[[reac]])) {
        paste0("d", reac, " <- -", rate)
      } else {
        paste(odes[[reac]], "-", rate)
      }
    }
  }

  # Define model function
  model_func <- function(time, state, parameters) {
    with(as.list(c(state, parameters)), {
      # Evaluate assignment rules and assign to local variables
      if (length(rules) > 0) {
        for (eqn in rules) {
          eval(parse(text = eqn), envir = environment())  # evaluate in local environment
        }
      }
      derivs <- lapply(odes, function(eqn) eval(parse(text = eqn)))
      names(derivs) <- names(state)
      return(list(derivs))
    })
  }

  # Combine parameters with compartment sizes
  parameters <- c(
    stats::setNames(parameters, param_ids),
    stats::setNames(compartment_sizes, compartment_ids)
  )
  param_ids <- c(param_ids, compartment_ids)

  sbmlModel <- structure(
    list(
      compartment_ids = compartment_ids,
      species_ids = species_ids,
      param_ids = param_ids,
      function_defs = function_defs,
      reactions = reactions_data,
      rules = rules,
      odes = odes,
      default_params = parameters,
      func = model_func,
      time_unit = time_unit,
      amount_unit = amount_unit
    ),
    class = "sbmlModel"
  )

  return(sbmlModel)
}
