
test_that("si_extremes_works", {

  #Use the extent sample data:
  data("extent_data")

  #sea ice extent should be positive for both the max and the min
  #this should still return a warning message because the extent data contains no data values of -9999
  expect_true(si_extremes(extent_data)$max > 0)
  expect_true(si_extremes(extent_data)$min > 0)

  #sea ice extent maximum should be larger than the minimum
  expect_true(si_extremes(sept_extent)$max > si_extremes(sept_extent)$min)


})





