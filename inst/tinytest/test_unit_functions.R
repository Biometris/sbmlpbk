# Named list of unit definitions
unit_definitions <- list(
  mol = list(id = "mol", units = list(list(kind="mole", exponent=1, scale=0, multiplier=1))),
  g = list(id = "g", units = list(list(kind="gram", exponent=1, scale=0, multiplier=1))),
  mg = list(id = "mg", units = list(list(kind="gram", exponent=1, scale=-3, multiplier=1))),
  ug = list(id = "ug", units = list(list(kind="gram", exponent=1, scale=-6, multiplier=1))),
  l = list(id = "l", units = list(list(kind="litre", exponent=1, scale=0, multiplier=1))),
  ml = list(id = "ml", units = list(list(kind="litre", exponent=1, scale=-3, multiplier=1))),
  m3 = list(id = "m3", units = list(list(kind="meter", exponent=3, scale=0, multiplier=1))),
  s = list(id = "s", units = list(list(kind="second", exponent=1, scale=0, multiplier=1))),
  min = list(id = "min", units = list(list(kind="second", exponent=1, scale=0, multiplier=60))),
  h = list(id = "h", units = list(list(kind="second", exponent=1, scale=0, multiplier=3600))),
  d = list(id = "d", units = list(list(kind="second", exponent=1, scale=0, multiplier=86400))),
  per_d = list(id = "per_d", units = list(list(kind="second", exponent=-1, scale=0, multiplier=86400))),
  mol_per_s = list(
    id = "mol_per_s",
    units = list(
      list(kind="mole", exponent=1, scale=0, multiplier=1),
      list(kind="second", exponent=-1, scale=0, multiplier=1)
    )
  ),
  ug_per_kg = list(
    id = "ug_per_kg",
    units = list(
      list(kind="gram", exponent=1, scale=-6, multiplier=1),
      list(kind="gram", exponent=-1, scale=3, multiplier=1)
    )
  )
)

# Tests create unit string
expect_equal(sbmlpbk:::create_unit_string(unit_definitions$mol), 'mol')
expect_equal(sbmlpbk:::create_unit_string(unit_definitions$mol_per_s), 'mol/s')
expect_equal(sbmlpbk:::create_unit_string(unit_definitions$ug_per_kg), 'ug/kg')
expect_equal(sbmlpbk:::create_unit_string(unit_definitions$h), 'h')
expect_equal(sbmlpbk:::create_unit_string(unit_definitions$d), 'd')

# Tests for amount
expect_true(sbmlpbk:::is_amount_unit(unit_definitions$mol))
expect_true(sbmlpbk:::is_amount_unit(unit_definitions$g))
expect_false(sbmlpbk:::is_amount_unit(unit_definitions$mol_per_s))
expect_false(sbmlpbk:::is_amount_unit(unit_definitions$ug_per_kg))

# Tests for volume
expect_true(sbmlpbk:::is_volume_unit(unit_definitions$l))
expect_false(sbmlpbk:::is_volume_unit(unit_definitions$m3))
expect_false(sbmlpbk:::is_volume_unit(unit_definitions$mol))

# Tests for time
expect_true(sbmlpbk:::is_time_unit(unit_definitions$s))
expect_false(sbmlpbk:::is_time_unit(unit_definitions$mol))
expect_false(sbmlpbk:::is_time_unit(unit_definitions$per_day))

# Tests for amount conversion factor
expect_equal(sbmlpbk:::get_amount_conversion_factor(unit_definitions$g), 1e6)
expect_equal(sbmlpbk:::get_amount_conversion_factor(unit_definitions$mg), 1e3)
expect_equal(sbmlpbk:::get_amount_conversion_factor(unit_definitions$g, "mg"), 1e3)
expect_equal(sbmlpbk:::get_amount_conversion_factor(unit_definitions$ug, "ng"), 1e3)
expect_error(sbmlpbk:::get_amount_conversion_factor(unit_definitions$d))

# Tests for time conversion factor
expect_equal(sbmlpbk:::get_time_conversion_factor(unit_definitions$s), 1)
expect_equal(sbmlpbk:::get_time_conversion_factor(unit_definitions$min), 60)
expect_equal(sbmlpbk:::get_time_conversion_factor(unit_definitions$h), 3600)
expect_equal(sbmlpbk:::get_time_conversion_factor(unit_definitions$d), 86400)
expect_equal(sbmlpbk:::get_time_conversion_factor(unit_definitions$d, 'd'), 1)
expect_equal(sbmlpbk:::get_time_conversion_factor(unit_definitions$d, 'h'), 24)
expect_error(sbmlpbk:::get_time_conversion_factor(unit_definitions$mg))
expect_error(sbmlpbk:::get_time_conversion_factor(unit_definitions$per_d))

# Tests for volume conversion factor
expect_equal(sbmlpbk:::get_volume_conversion_factor(unit_definitions$l), 1)
expect_equal(sbmlpbk:::get_volume_conversion_factor(unit_definitions$ml), 1e-3)
expect_equal(sbmlpbk:::get_volume_conversion_factor(unit_definitions$l, 'mL'), 1e3)
expect_error(sbmlpbk:::get_volume_conversion_factor(unit_definitions$mg))
