
test_that("sle_works" , {

  #Create some test data with a series of ice volumes and percents:
  ice_data <- data.frame(volume = c(500,200,600, 350, 465), #assign 5 volumes
                         percent_above = c(0.5,0.2,0.6,0.8,1.2)) #assign 5 percents with one above 100


  #The 'above' input should be a percent, for 1.2 it should return an error:
  expect_equal(sle(ice_vol = ice_data$volume[2], above = ice_data$percent_above[5]),
               "Above parameter should be a percent in decimal format between 0 and 1")

  #The 'ice_vol' input can only accept a single value, if two or more values are inputted it should return an error:
  expect_equal(sle(ice_vol = ice_data$volume[1:2], above = ice_data$percent_above[2]),
               "Provide a single numeric value for ice_vol, not a vector")

  #For a volume of 500 with 60% above water, the sle rounded to 2 digits should be 0.76
  expect_equal(round(sle(ice_vol = ice_data$volume[1], above = ice_data$percent_above[3]),2), 0.76)

})
