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



test_that("shiny_grade returns a ggplot object", {
  result <- visae:::shiny_grade(data = df,
                                selected_cycle = "C1",
                                contr_indicator = TRUE,
                                mass_indicator = TRUE,
                                contr_threshold = 0.01,
                                mass_threshold = 0.01)
  expect_s3_class(result$asymmetric_plot, "ggplot")  # Checks if the result is a ggplot object
})

test_that("shiny_domain returns a ggplot object", {
  result <- visae:::shiny_domain(data = df,
                                 selected_cycle = "C1",
                                 selected_grade = paste0("G", 1:5),
                                 contr_indicator = TRUE,
                                 mass_indicator = TRUE,
                                 contr_threshold = 0.01,
                                 mass_threshold = 0.01)
  expect_s3_class(result$asymmetric_plot, "ggplot")  # Checks if the result is a ggplot object
})

test_that("shiny_domain_grade returns a ggplot object", {
  result <- visae:::shiny_domain_grade(data = df,
                                       selected_cycle = "C1",
                                       contr_indicator = TRUE,
                                       mass_indicator = TRUE,
                                       contr_threshold = 0.01,
                                       mass_threshold = 0.01)
  expect_s3_class(result$asymmetric_plot, "ggplot")  # Checks if the result is a ggplot object
})

test_that("shiny_term returns a ggplot object", {
  result <- visae:::shiny_term(data = df,
                               selected_cycle = "C1",
                               selected_grade = paste0("G", 1:5),
                               selected_domain = paste0("D", 1:2),
                               contr_indicator = TRUE,
                               mass_indicator = TRUE,
                               contr_threshold = 0.01,
                               mass_threshold = 0.01)
  expect_s3_class(result$asymmetric_plot, "ggplot")  # Checks if the result is a ggplot object
})

test_that("shiny_term_grade returns a ggplot object", {
  result <- visae:::shiny_term_grade(data = df,
                       selected_cycle = "C1",
                       selected_domain = paste0("D", 1:2),
                       contr_indicator = TRUE,
                       mass_indicator = TRUE,
                       contr_threshold = 0.01,
                       mass_threshold = 0.01)
  expect_s3_class(result$asymmetric_plot, "ggplot")  # Checks if the result is a ggplot object
})
