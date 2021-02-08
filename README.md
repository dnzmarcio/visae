# visae
Implementation of 'shiny' apps to visualize adverse events based on the Common Terminology Criteria for Adverse Events (CTCAE) 

- Stacked correspondence Analysis as described in Diniz et. al (2021) <arXiv:2101.03454>:
```{}
library(visae)
library(magrittr)
library(dplyr)

patient_id <- 1:100
group <- c(rep("A", 50), rep("B", 50))
ae_grade <- sample(1:5, size = 100, replace = TRUE)
ae_domain <- sample(c("C", "D"), size = 100, replace = TRUE)
ae_term <- sample(c("E", "F", "G", "H"), size = 100, replace = TRUE)
dt <- tibble(patient_id = patient_id, trt = group,
            ae_g = ae_grade, ae_d = ae_domain, ae_t = ae_term)
dt %>% run_ca(., group = trt,
             id = patient_id,
             ae_grade = ae_g,
             ae_domain = ae_d,
             ae_term = ae_t)
```

