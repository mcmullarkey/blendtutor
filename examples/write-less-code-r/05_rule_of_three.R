survey_data <- data.frame(
  respondent_id = 1:5,
  stress_1 = c(3, 4, 5, 2, 1),
  stress_2 = c(2, 3, 4, 5, 1),
  stress_3 = c(1, 2, 3, 4, 5),
  stress_4 = c(5, 4, 3, 2, 1),
  stress_5 = c(4, 3, 2, 1, 5),
  stress_6 = c(1, 2, 3, 4, 5)
)
should_abstract <- function(count) count >= 3
