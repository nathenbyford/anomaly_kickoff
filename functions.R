## Nate byford
## Functions for anomaly detection


lm_inf_detection <- function(data) {
  ## Using a linear model of time and values we are able to identify anomalous values
  ## from the influence points. This model takes into account if the given point has
  ## influence from on or all of the measures Cook's distance, Covariance ratio*, dffit*,
  ## df betas, and/or hat values.
  
  train_anomaly <- pull(data, anomaly)
  
  lm_mod <- lm(value ~ timestamp, data)
  
  inf_point <- as.data.frame(influence.measures(lm_mod)$is.inf)
  
  anom_ind <- inf_point |> 
    mutate(n = 1:nrow(inf_point)) |> 
    pivot_longer(-n) |> 
    group_by(n) |> 
    reframe(n_inf = sum(value)) |> 
    filter(n_inf > 0) |> 
    pull(n)
  
  anom_points <- left_join(
    data[, c(1, 2, 4)], 
    tibble(
      timestamp = apple$timestamp[anom_ind], 
      anomaly_mod = TRUE
      ), 
    by = join_by(timestamp)
  )|> 
    mutate(anomaly_mod = if_else(is.na(anomaly_mod), FALSE, anomaly_mod))
  
  mod_anom <- anom_points |> 
    pull(anomaly_mod)
  
  list(misclassification = mean(mod_anom == train_anomaly),
       conf_matrix = table(mod_anom, train_anomaly),
       data = anom_points)
}


stl_detection <- function(data) {
  ## Using seasonal-trend decomposition procedure based on LOESS or STL, we can 
  ## identify anomalies. This is done with help from the timetk package.
  
  train_anomaly <- pull(data, anomaly)
  
  mod_anomalies <- data |> 
    anomalize(timestamp, value) |> 
    mutate(anomaly_model = anomaly == "Yes") |>
    select(timestamp, anomaly_model)
  
  anomaly_results <- data |> 
    select(timestamp, value, anomaly) |> 
    left_join(mod_anomalies, by = join_by(timestamp))
  
  model_anomaly <- pull(mod_anomalies, anomaly_model)
  
  list(misclassification = mean(model_anomaly == train_anomaly),
       conf_matrix = table(model_anomaly, train_anomaly),
       data = anomaly_results)
}
