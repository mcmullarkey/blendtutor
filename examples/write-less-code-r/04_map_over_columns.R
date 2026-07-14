library(purrr)
library(dplyr)

survey_data <- data.frame(
  respondent_id = 1:5,
  stress_1 = c(3, 4, 5, 2, 1),
  stress_2 = c(2, 3, 4, 5, 1),
  stress_3 = c(1, 2, 3, 4, 5),
  stress_4 = c(5, 4, 3, 2, 1),
  stress_5 = c(4, 3, 2, 1, 5),
  stress_6 = c(1, 2, 3, 4, 5)
)
stress_cols <- paste0("stress_", 1:6)
survey_data <- survey_data |>
  mutate(across(all_of(stress_cols), .fns = list(rev = ~ 6 - .x), .names = "{.col}_rev"))
