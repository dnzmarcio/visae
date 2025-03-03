shiny_grade <- function(data, selected_cycle,
                        contr_indicator, mass_indicator,
                        contr_threshold, mass_threshold){

  if ("ae_cycle" %in% colnames(data))
    data <- data |> filter(ae_cycle %in% selected_cycle)

  out <- visae::ca_ae(data, id = id,
                      group = group,
                      ae_class = ae_grade,
                      label = "Grade",
                      contr_indicator = contr_indicator,
                      mass_indicator = mass_indicator,
                      contr_threshold = contr_threshold,
                      mass_threshold = mass_threshold)
  return(out)
}

shiny_domain <- function(data, selected_cycle, selected_grade,
                         contr_indicator, mass_indicator,
                         contr_threshold, mass_threshold){

  if ("ae_cycle" %in% colnames(data))
    data <- data |> filter(ae_cycle %in% selected_cycle)
  if ("ae_grade" %in% colnames(data))
    data <- data |> filter(ae_grade %in% c(selected_grade, NA))

  out <- visae::ca_ae(data, id = id,
                      group = group,
                      ae_class = ae_domain,
                      label = "Domain",
                      contr_indicator = contr_indicator,
                      mass_indicator = mass_indicator,
                      contr_threshold = contr_threshold,
                      mass_threshold = mass_threshold)
  return(out)
}

shiny_domain_grade <- function(data, selected_cycle,
                               contr_indicator, mass_indicator,
                               contr_threshold, mass_threshold){

  if ("ae_cycle" %in% colnames(data))
    data <- data |> filter(ae_cycle %in% selected_cycle)

  data <- data |>
    mutate(ae_domain_grade = ifelse(!is.na(ae_domain) &
                                      !is.na(ae_grade),
                                    paste0(ae_domain, ": ", ae_grade),
                                    NA))

  out <- visae::ca_ae(data, id = id,
                      group = group,
                      ae_class = ae_domain_grade,
                      label = "Domain:Grade",
                      contr_indicator = contr_indicator,
                      mass_indicator = mass_indicator,
                      contr_threshold = contr_threshold,
                      mass_threshold = mass_threshold)
  return(out)
}

shiny_term <- function(data, selected_cycle, selected_domain, selected_grade,
                       contr_indicator, mass_indicator,
                       contr_threshold, mass_threshold){

  if ("ae_cycle" %in% colnames(data))
    data <- data |> filter(ae_cycle %in% selected_cycle)
  if ("ae_domain" %in% colnames(data))
    data <- data |> filter(ae_domain %in% c(selected_domain, NA))
  if ("ae_grade" %in% colnames(data))
    data <- data |> filter(ae_grade %in% c(selected_grade, NA))

  out <- visae::ca_ae(data, id = id,
                      group = group,
                      ae_class = ae_term,
                      label = "Term",
                      contr_indicator = contr_indicator,
                      mass_indicator = mass_indicator,
                      contr_threshold = contr_threshold,
                      mass_threshold = mass_threshold)
  return(out)
}

shiny_term_grade <- function(data, selected_cycle, selected_domain,
                             contr_indicator, mass_indicator,
                             contr_threshold, mass_threshold){

  if ("ae_cycle" %in% colnames(data))
    data <- data |> filter(ae_cycle %in% selected_cycle)
  if ("ae_domain" %in% colnames(data))
    data <- data |> filter(ae_domain %in% c(selected_domain, NA))

  data <- data |>
    mutate(ae_term_grade = ifelse(!is.na(ae_term) &
                                    !is.na(ae_grade),
                                  paste0(ae_term, ": ", ae_grade),
                                  NA))

  out <- visae::ca_ae(data, id = id,
                      group = group,
                      ae_class = ae_term_grade,
                      label = "Term:Grade",
                      contr_indicator = contr_indicator,
                      mass_indicator = mass_indicator,
                      contr_threshold = contr_threshold,
                      mass_threshold = mass_threshold)
  return(out)
}
