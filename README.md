# visae
Implementation of Shiny apps to visualize adverse events.

- Correspondence Analysis
```{}
library(magrittr)
library(dplyr)
group <- c(rep("A", 50), rep("B", 50))
ae_grade <- sample(1:5, size = 100, replace = TRUE)
ae_domain <- sample(c("C", "D"), size = 100, replace = TRUE)
ae_term <- sample(c("E", "F", "G", "H"), size = 100, replace = TRUE)
dt <- tibble(trt = group, ae_g = ae_grade, ae_d = ae_domain, ae_t = ae_term)

dt %>% run_ca(., group = trt,
             ae_grade = ae_g,
             ae_domain = ae_d,
             ae_term = ae_t)
```

