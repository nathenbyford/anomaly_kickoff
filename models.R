## Project models

library("tidyverse"); theme_set(theme_minimal())
library("timetk")

data <- read_csv("data/combined_data.csv")

glimpse(data)

apple <- data |> filter(company == names[1])

names <- as.character(unique(data$company))

lm_results <- map_dfr(names,
  ~tibble(
    company = .,
    lm = lm_inf_detection(filter(data, company == .))$misclassification,
    )
  )


stl_results <- map_dfr(
  names,
  ~tibble(
    company = .,
    stl = stl_detection(filter(data, company == .))$misclassification
  )
)

left_join(lm_results, stl_results) |> 
  knitr::kable(digits = 4)
