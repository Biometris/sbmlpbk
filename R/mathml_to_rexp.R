#' @noRd
#' @keywords internal
mathml_to_r <- function(math_node, simplify = TRUE) {
  ns <- c(mathml = "http://www.w3.org/1998/Math/MathML")
  node_type <- xml2::xml_name(math_node)

  if (node_type == "cn") {
    # Constant number
    r_expr <- trimws(xml2::xml_text(math_node))
  } else if (node_type == "ci") {
    # Variable identifier
    r_expr <- trimws(xml2::xml_text(math_node))
  } else if (node_type == "piecewise") {
    # Piecewise function
    pieces <- xml2::xml_children(math_node)

    r_expr <- NULL
    for (piece in pieces) {
      piece_type <- xml2::xml_name(piece)

      if (piece_type == "piece") {
        children <- xml2::xml_children(piece)
        if (length(children) != 2) {
          stop("<piece> must have exactly 2 children: value and condition.")
        }

        value_expr <- mathml_to_r(children[[1]], FALSE)
        condition_expr <- mathml_to_r(children[[2]], FALSE)

        # Nest condition using ifelse
        if (is.null(r_expr)) {
          r_expr <- paste0("ifelse(", condition_expr, ", ", value_expr)
        } else {
          r_expr <- paste0(r_expr, ", ifelse(", condition_expr, ", ", value_expr)
        }
      }

      if (piece_type == "otherwise") {
        other_expr <- mathml_to_r(xml2::xml_children(piece)[[1]], FALSE)
        r_expr <- paste0(r_expr, ", ", other_expr, strrep(")", length(xml2::xml_find_all(math_node, ".//mathml:piece", ns))))
      }
    }

    # If no <otherwise> clause, default to NA
    if (!any(xml2::xml_name(xml2::xml_children(math_node)) == "otherwise")) {
      r_expr <- paste0(r_expr, ", NA", strrep(")", length(xml2::xml_find_all(math_node, ".//mathml:piece", ns))))
    }
  } else if (node_type == "apply") {
    # Operator/function application
    children <- xml2::xml_children(math_node)
    if (length(children) < 2) {
      stop("Malformed <apply> node: less than 2 children.")
    }

    op_node <- children[[1]]
    op_type <- xml2::xml_name(op_node)

    # Handle <ci> as function name (user-defined or variable-based)
    if (op_type == "ci") {
      func_name <- xml2::xml_text(op_node)
      args <- children[-1]
      r_args <- sapply(args, function(x) { mathml_to_r(x, FALSE) })
      r_expr <- paste0(func_name, "(", paste(r_args, collapse = ", "), ")")
    } else {
      # Handle standard MathML operators like <plus/>, <times/>, etc.
      operator <- op_type
      args <- children[-1]
      r_args <- sapply(args, function(x) { mathml_to_r(x, FALSE) })

      op_map <- list(
        plus = "+",
        minus = "-",
        times = "*",
        divide = "/",
        power = "^",
        eq = "==",
        neq = "!=",
        lt = "<",
        leq = "<=",
        gt = ">",
        geq = ">="
      )

      if (operator %in% names(op_map)) {
        # Binary operators
        r_expr <- paste("(", paste0(r_args, collapse = op_map[[operator]], sep=""), ")", sep="")
      } else if (operator == "minus" && length(r_args) == 1) {
        # Unary minus
        r_expr <- paste0("-(", r_args[1], ")")
      } else if (operator %in% c("exp", "log", "sin", "cos", "tan", "sqrt", "min", "max")) {
        # Built-in function (non-<ci>)
        r_expr <- paste0(operator, "(", paste(r_args, collapse = ", "), ")")
      } else {
        stop(paste("Unsupported MathML operator:", operator))
      }
    }
  } else {
    stop(paste("Unsupported MathML element:", node_type))
  }

  if (simplify) {
    r_expr <- Deriv::Simplify(r_expr)
  }

  return(r_expr)
}
