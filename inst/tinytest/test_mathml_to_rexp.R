# Load dependencies
library(xml2)

# Define the piecewise MathML as a string
mathml_str <- '
<math xmlns="http://www.w3.org/1998/Math/MathML">
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
</math>
'

# Parse XML and extract the <piecewise> node
doc <- read_xml(mathml_str)
piecewise_node <- xml2::xml_find_first(doc, ".//d1:piecewise")

# Call the function under test
r_expr <- sbmlpbk:::mathml_to_r(piecewise_node)

# Expected result
expected_expr <- "ifelse(x < 0, 1, ifelse(x == 0, 2, 3))"

# Assert equality
expect_equal(r_expr, expected_expr)
