predict_revenue <- function(old_data, new_data) {
  
  # The project examples fit an lm model inside this function,
  # predict on new_data, and return the predictions. This function
  # follows that same setup.
  
  # Make categorical variables match between old_data and new_data.
  old_data$location_type <- factor(old_data$location_type)
  old_data$cuisine_type <- factor(old_data$cuisine_type)
  old_data$delivery <- factor(old_data$delivery)
  
  new_data$location_type <- factor(new_data$location_type,
                                   levels = levels(old_data$location_type))
  new_data$cuisine_type <- factor(new_data$cuisine_type,
                                  levels = levels(old_data$cuisine_type))
  new_data$delivery <- factor(new_data$delivery,
                              levels = levels(old_data$delivery))
  
  # Create the transformed variables inside the function.
  # This is important because the same variables must exist in new_data.
  old_data$walk_ins_2 <- old_data$average_daily_walk_ins^2
  new_data$walk_ins_2 <- new_data$average_daily_walk_ins^2
  
  old_data$avg_check_2 <- old_data$avg_check^2
  new_data$avg_check_2 <- new_data$avg_check^2
  
  # Final lm model. This uses only variables from the given dataset,
  # plus polynomial and interaction terms made from those variables.
  fit <- lm(
    monthly_revenue ~ location_type + cuisine_type + delivery +
      average_daily_walk_ins + walk_ins_2 + seating_capacity +
      avg_check + avg_check_2 + competitors_nearby + years_open +
      weekly_reservations + website_visits + food_cost_percent +
      average_daily_walk_ins:location_type +
      avg_check:cuisine_type +
      avg_check:location_type +
      weekly_reservations:delivery +
      website_visits:delivery +
      avg_check:food_cost_percent,
    data = old_data
  )
  
  preds <- predict(fit, newdata = new_data)
  
  return(as.numeric(preds))
}
