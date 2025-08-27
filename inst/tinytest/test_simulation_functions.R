fake_model_h_ug <- list(
  unit_defs = list(
    ug = list(id = "ug", units = list(list(kind="gram", exponent=1, scale=-6, multiplier=1))),
    h = list(id = "h", units = list(list(kind="second", exponent=1, scale=0, multiplier=3600)))
  ),
  model_units = list(
    time = 'h',
    amount = 'ug'
  )
)

fake_model_d_mg <- list(
  unit_defs = list(
    mg = list(id = "mg", units = list(list(kind="gram", exponent=1, scale=-3, multiplier=1))),
    d = list(id = "d", units = list(list(kind="second", exponent=1, scale=0, multiplier=86400)))
  ),
  model_units = list(
    time = 'd',
    amount = 'mg'
  )
)

## --- Test create_desolve_times ---

times <- create_desolve_times(fake_model_h_ug, duration=10, step=1, unit='d')
expect_equal(length(times), 11)
expect_equal(times[1], 0)
expect_equal(times[11], 240)

times <- create_desolve_times(fake_model_h_ug, duration=10, step=2, unit='d')
expect_equal(length(times), 6)
expect_equal(times[1], 0)
expect_equal(times[6], 240)

times <- create_desolve_times(fake_model_h_ug, duration=10, step=.5, unit='d')
expect_equal(length(times), 21)
expect_equal(times[1], 0)
expect_equal(times[2], 12)
expect_equal(times[21], 240)

times <- create_desolve_times(fake_model_h_ug, duration=10, step=1, unit='h')
expect_equal(length(times), 11)
expect_equal(times[1], 0)
expect_equal(times[11], 10)

times <- create_desolve_times(fake_model_d_mg, duration=10, step=1, unit='d')
expect_equal(length(times), 11)
expect_equal(times[1], 0)
expect_equal(times[11], 10)

## --- Test create_desolve_events ---

eventdat <- create_desolve_events(
  model = fake_model_h_ug,
  dosing_events = list(
    list(
      target = "AGut",
      dose_type = "single_bolus",
      time = 3,
      amount = 20
    )
  ),
  time_unit = 'd',
  amount_unit = 'ug'
)
expect_equal(nrow(eventdat), 1)
expect_equal(eventdat[,'var'], "AGut")
expect_equal(eventdat[,'time'], 3)
expect_equal(eventdat[,'value'], 20)
expect_equal(eventdat[,'method'], "add")

eventdat <- create_desolve_events(
  model = fake_model_h_ug,
  dosing_events = list(
    list(
      target = "AGut",
      dose_type = "repeated_bolus",
      time = 1,
      amount = 20,
      until = 10,
      interval = 2
    )
  ),
  time_unit = 'd',
  amount_unit = 'ug'
)
expect_equal(nrow(eventdat), 5)

## Test no events
eventdat <- create_desolve_events(
  model = fake_model_h_ug,
  dosing_events = list(),
  time_unit = 'd',
  amount_unit = 'ug'
)
expect_equal(length(eventdat), 0)
