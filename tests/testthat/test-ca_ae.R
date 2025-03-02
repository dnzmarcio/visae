id <- rep(1:50, each = 2)
group <- c(rep("A", 50), rep("B", 50))
ae_grade <- sample(paste0("G", 1:5), size = 100, replace = TRUE)
ae_domain <- sample(c("D1", "D2"), size = 100, replace = TRUE)
ae_term <- sample(c("AE1", "AE2", "AE3", "AE4"), size = 100, replace = TRUE)
cycle <- rep(c("C1", "C2"), each = 2)
df <- tibble(id = id,
             group = group,
             ae_grade = ae_grade,
             ae_domain = ae_domain,
             ae_term = ae_term)

test_that("ca calculates inertia", {
  result <- visae::ca_ae(data = df,
                         id = id,
                         group = group,
                         ae = ae_grade,
                         label = "AE",
                         contr_indicator = TRUE,
                         mass_indicator = TRUE,
                         contr_threshold = 0.01,
                         mass_threshold = 0.01)
  expect_type(result$total_inertia, "double")  # Checks if the result is a ggplot object
})

