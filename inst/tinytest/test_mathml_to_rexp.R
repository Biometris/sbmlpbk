# Load dependencies
library(xml2)

create_test_node <- function(xml_str) {
  doc <- xml2::read_xml(
    paste(
      '<math xmlns="http://www.w3.org/1998/Math/MathML">',
      xml_str,
      '</math>',
      sep = ""
    )
  )
  node <- xml2::xml_children(doc)[[1]]
}

# Test parsing of ln
mathml_str <- '<apply><ln/><ci>x</ci></apply>'
node <- create_test_node(mathml_str)
r_expr <- sbmlpbk:::mathml_to_r(node)
expect_equal(r_expr, "log(x)")

# Test parsing of log
mathml_str <- '<apply><log/><ci>x</ci></apply>'
node <- create_test_node(mathml_str)
r_expr <- sbmlpbk:::mathml_to_r(node)
expect_equal(r_expr, "log10(x)")

# Test parsing of log
mathml_str <- '<apply><log/><logbase><cn>2</cn></logbase><ci>x</ci></apply>'
node <- create_test_node(mathml_str)
r_expr <- sbmlpbk:::mathml_to_r(node)
expect_equal(r_expr, "log(x, base = 2)")

# Test parsing of sqrt(x)
mathml_str <- '<apply><sqrt/><ci>x</ci></apply>'
node <- create_test_node(mathml_str)
r_expr <- sbmlpbk:::mathml_to_r(node)
expect_equal(r_expr, "sqrt(x)")

# Test parsing of sin(x)
mathml_str <- '<apply><sin/><ci>x</ci></apply>'
node <- create_test_node(mathml_str)
r_expr <- sbmlpbk:::mathml_to_r(node)
expect_equal(r_expr, "sin(x)")

# Test parsing of cos(x)
mathml_str <- '<apply><cos/><ci>x</ci></apply>'
node <- create_test_node(mathml_str)
r_expr <- sbmlpbk:::mathml_to_r(node)
expect_equal(r_expr, "cos(x)")

# Test parsing of simple arithmetic: x + y
mathml_str <- '<apply><plus/><ci>x</ci><ci>y</ci></apply>'
node <- create_test_node(mathml_str)
r_expr <- sbmlpbk:::mathml_to_r(node)
expect_equal(r_expr, "x + y")

# Test parsing of unary minus: -x
mathml_str <- '<apply><minus/><ci>x</ci></apply>'
node <- create_test_node(mathml_str)
r_expr <- sbmlpbk:::mathml_to_r(node)
expect_equal(r_expr, "-x")

# Test parsing of exp(x) -> e^x
mathml_str <- '<apply><exp/><ci>x</ci></apply>'
node <- create_test_node(mathml_str)
r_expr <- sbmlpbk:::mathml_to_r(node)
expect_equal(r_expr, "exp(x)")

# Test parsing of power: x^y
mathml_str <- '<apply><power/><ci>x</ci><ci>y</ci></apply>'
node <- create_test_node(mathml_str)
r_expr <- sbmlpbk:::mathml_to_r(node)
expect_equal(r_expr, "x^y")

# Test parsing of min(x, y)
mathml_str <- '<apply><min/><ci>x</ci><ci>y</ci></apply>'
node <- create_test_node(mathml_str)
r_expr <- sbmlpbk:::mathml_to_r(node)
expect_equal(r_expr, "min(x, y)")

# Test parsing of max(x, y)
mathml_str <- '<apply><max/><ci>x</ci><ci>y</ci></apply>'
node <- create_test_node(mathml_str)
r_expr <- sbmlpbk:::mathml_to_r(node)
expect_equal(r_expr, "max(x, y)")

# Test piecewise function
mathml_str <- '
  <piecewise>
    <piece>
      <cn>1</cn>
      <apply><lt/><ci>x</ci><cn>0</cn></apply>
    </piece>
    <piece>
      <cn>2</cn>
      <apply><eq/><ci>x</ci><cn>0</cn></apply>
    </piece>
    <otherwise>
      <cn>3</cn>
    </otherwise>
  </piecewise>
'
piecewise_node <- create_test_node(mathml_str)
r_expr <- sbmlpbk:::mathml_to_r(piecewise_node)
expected_expr <- "ifelse(x < 0, 1, ifelse(x == 0, 2, 3))"
expect_equal(r_expr, expected_expr)
