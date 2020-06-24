#'@import magrittr
#'@import ggplot2
#'@import dplyr
#'@importFrom rlang .data enquos
#'@importFrom tidyr pivot_wider
#'@importFrom ca ca
#'@importFrom stats addmargins
#'@importFrom ggrepel geom_text_repel
ca_ae <- function(data, group, ae, label = "AE",
                  contr_indicator = TRUE, mass_indicator = TRUE,
                  contr_threshold = NULL) {

  temp <- enquos(group = group,
                 ae = ae,
                 .ignore_empty = "all")
  aux <- data %>% select(!!!temp)

  tab <- with(aux, table(ae, group))
  res.ca <- ca(tab)

  tab_rel <- round(prop.table(tab, 2), 3) %>% as_tibble() %>%
   pivot_wider(names_from = .data$group, values_from = .data$n) %>%
    mutate(Average = round(res.ca$rowmass, 3)) %>%
    rename(AE = .data$ae)

  if (is.null(contr_threshold))
    contr_threshold <- 1/nrow(tab)

  tab_abs <- addmargins(tab) %>% as_tibble() %>%
    pivot_wider(names_from = .data$group, values_from = .data$n) %>%
    mutate(ae = ifelse(.data$ae == "Sum", "Total", .data$ae)) %>%
    rename(AE = .data$ae, Total = .data$Sum)

  inertia <- res.ca$sv
  total_inertia <- sum(inertia)
  explained_var <- 100*inertia/total_inertia

  if (ncol(tab_abs) == 4){

    principal.coordinates.col <-
      tibble(dim_1 = res.ca$colcoord*sqrt(inertia[1])) %>%
      mutate(labels = rownames(res.ca$colcoord),
             type = "col", contr = 1, mass = 1)

    aux <- res.ca$rowcoord*sqrt(res.ca$rowmass)
    standard.coordinates.row <-
      tibble(dim_1 = aux) %>%
      mutate(labels = rownames(res.ca$rowcoord),
             type = "row",
             contr = aux^2,
             mass = res.ca$rowmass/max(res.ca$rowmass)) %>%
      filter(.data$contr > contr_threshold) %>%
      mutate(contr = .data$contr/max(.data$contr))


    dp <- bind_rows(principal.coordinates.col,
                           standard.coordinates.row)

    if (mass_indicator & contr_indicator){
      asymmetric_plot <- ggplot(dp, aes(x = .data$dim_1, y = NA,
                                        color = .data$type,
                                        alpha = .data$contr,
                                        size = .data$mass))
    } else if (mass_indicator & !contr_indicator){
      asymmetric_plot <- ggplot(dp, aes(x = .data$dim_1,
                                        y = NA,
                                        color = .data$type,
                                        size = .data$mass))
    } else if (!mass_indicator & contr_indicator) {
      asymmetric_plot <- ggplot(dp, aes(x = .data$dim_1,
                                        y = NA,
                                        color = .data$type,
                                        alpha = .data$contr))
    } else {
      asymmetric_plot <- ggplot(dp, aes(x = .data$dim_1,
                                        y = NA,
                                        color = .data$type))
    }

    asymmetric_plot <- asymmetric_plot +
      geom_vline(xintercept = 0, linetype = 2) +
      geom_point() +
      geom_text_repel(aes(label = .data$labels)) +
      scale_colour_manual(values = c("red", "blue")) +
      labs(x = paste0("Dim 1 ", "(", round(explained_var[1], 2), "%)")) +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(size = 20)) +
      scale_x_continuous(limits = c(-1, 1))

    # dp <- bind_rows(principal.coordinates.col,
    #                 principal.coordinates.row)
    #
    # symetric_plot <- ggplot(dp, aes(x = dim_1, y = NA,
    #                                 color = type))  +
    #   geom_vline(xintercept = 0, linetype = 2) +
    #   geom_point() +
    #   geom_text_repel(aes(label = labels)) +
    #   scale_colour_manual(values = c("red", "blue")) +
    #   labs(x = paste0("Dim 1 ", "(", round(res.ca$eig[1, 2], 2), "%)")) +
    #   theme_minimal() +
    #   theme(
    #     legend.position = "none",
    #     axis.text.y = element_blank(),
    #     axis.ticks.y = element_blank(),
    #     axis.title.y = element_blank(),
    #     text = element_text(size = 12))
    #
    # aux <- as_tibble(res.ca$row$contrib) %>%
    #   mutate(ae = as.factor(names(res.ca$row$contrib))) %>% clean_names()
    #
    # dp <- aux %>%
    #   mutate(ae = factor(ae, levels = levels(aux$ae)[order(aux$dim_1)]))
    #
    # contr <- list()
    #
    # contr$dim1 <- ggplot(dp, aes(y = dim_1, x = ae)) +
    #   theme_minimal() +
    #   geom_col(fill = "steelblue") +
    #   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    #   labs(y = "Contributions (%)", x = label,
    #        title = "Dimension 1") +
    #   geom_hline(yintercept = 100/nrow(tab), linetype = 2) + coord_flip()

  } else {

    principal.coordinates.col <-
      as_tibble(res.ca$colcoord*sqrt(inertia[1])) %>%
      mutate(labels = rownames(res.ca$colcoord),
             type = "col",
             contr = 1,
             mass = 1)

    aux <- res.ca$rowcoord*sqrt(res.ca$rowmass)
    standard.coordinates.row <-
      as_tibble(aux) %>%
      mutate(labels = rownames(res.ca$rowcoord),
             type = "row",
             contr = pmax(aux[, 1]^2, aux[, 2]^2),
             mass = res.ca$rowmass/max(res.ca$rowmass)) %>%
      filter(.data$contr > contr_threshold)  %>%
      mutate(contr = .data$contr/max(.data$contr))

    dp <- bind_rows(principal.coordinates.col,
                    standard.coordinates.row)
    colnames(dp) <- paste0("dim_", 1:ncol(dp))

    if (mass_indicator & contr_indicator){
      asymmetric_plot <- ggplot(dp, aes(x = .data$dim_1,
                                        y = .data$dim_2,
                                        color = .data$type,
                                        alpha = .data$contr,
                                        size = .data$mass))
    } else if (mass_indicator & !contr_indicator){
      asymmetric_plot <- ggplot(dp, aes(x = .data$dim_1, y = .data$dim_2,
                                        color = .data$type,
                                        size = .data$mass))
    } else if (!mass_indicator & contr_indicator){
      asymmetric_plot <- ggplot(dp, aes(x = .data$dim_1,
                                        y = .data$dim_2,
                                        color = .data$type,
                                        alpha = .data$contr))
    } else {
      asymmetric_plot <- ggplot(dp, aes(x = .data$dim_1,
                                        y = .data$dim_2,
                                        color = .data$type))
    }

    asymmetric_plot <- asymmetric_plot +
      geom_hline(yintercept = 0, linetype = 2) +
      geom_vline(xintercept = 0, linetype = 2) +
      geom_point() +
      geom_text_repel(aes(label = .data$labels)) +
      scale_colour_manual(values = c("red", "blue")) +
      labs(x = paste0("Dim 1 ", "(", round(explained_var[1], 2), "%)"),
           y = paste0("Dim 2 ", "(", round(explained_var[2], 2), "%)")) +
      theme_minimal() +
      theme(legend.position = "none",
            text = element_text(size = 20))+
      scale_x_continuous(limits = c(-1, 1)) +
      scale_y_continuous(limits = c(-1, 1))


    # dp <- bind_rows(principal.coordinates.col,
    #                 principal.coordinates.row) %>% clean_names()
    #
    # symetric_plot <- ggplot(dp, aes(x = dim_1, y = dim_2,
    #                                 color = type))  +
    #   geom_hline(yintercept = 0, linetype = 2) +
    #   geom_vline(xintercept = 0, linetype = 2) +
    #   geom_point() +
    #   geom_text_repel(aes(label = labels)) +
    #   scale_colour_manual(values = c("red", "blue")) +
    #   labs(x = paste0("Dim 1 ", "(", round(res.ca$eig[1, 2], 2), "%)"),
    #        y = paste0("Dim 2 ", "(", round(res.ca$eig[2, 2], 2), "%)")) +
    #   theme_minimal() +
    #   theme(legend.position = "none",
    #         text = element_text(size = 12))
    #
    # aux <- as_tibble(res.ca$row$contrib) %>%
    #   mutate(ae = as.factor(rownames(res.ca$row$contrib))) %>% clean_names()
    #
    # dp <- aux %>%
    #   mutate(ae = factor(ae, levels = levels(aux$ae)[order(aux$dim_1)]))
    #
    # contr <- list()
    #
    # contr$dim1 <- ggplot(dp, aes(y = dim_1, x = ae)) +
    #   theme_minimal() +
    #   geom_col(fill = "steelblue") +
    #   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    #   labs(y = "Contributions (%)", x = label,
    #        title = "Dimension 1") +
    #   geom_hline(yintercept = 100/nrow(tab), linetype = 2) + coord_flip()
    #
    # dp <- aux %>%
    #   mutate(ae = factor(ae, levels = levels(aux$ae)[order(aux$dim_2)]))
    #
    # contr$dim2 <- ggplot(dp, aes(y = dim_2, x = ae)) +
    #   theme_minimal() +
    #   geom_col(fill = "steelblue") +
    #   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    #   labs(y = "Contributions (%)", x = label,
    #        title = "Dimension 2") +
    #   geom_hline(yintercept = 100/nrow(tab), linetype = 2) + coord_flip()
  }

  out <- list(tab_abs = tab_abs, tab_rel = tab_rel,
              total_inertia = total_inertia,
              inertia = inertia,
              asymmetric_plot = asymmetric_plot)

  return(out)
}

#'@import dplyr
#'@import magrittr
shiny_grade <- function(data, selected_cycle,
                        contr_indicator, mass_indicator,
                        contr_threshold){

  if ("ae_cycle" %in% colnames(data))
    data <- data %>% filter(.data$ae_cycle %in% selected_cycle)

  data <- data %>%
    mutate(ae_grade = paste0("G", .data$ae_grade))

  out <- ca_ae(data, group = .data$group, ae = .data$ae_grade,
               contr_indicator = contr_indicator,
               mass_indicator = mass_indicator,
               contr_threshold = contr_threshold)
  return(out)
}

#'@import dplyr
#'@import magrittr
shiny_domain <- function(data, selected_cycle, selected_grade,
                         contr_indicator, mass_indicator,
                         contr_threshold){

  if ("ae_cycle" %in% colnames(data))
    data <- data %>% filter(.data$ae_cycle %in% selected_cycle)
  if ("ae_grade" %in% colnames(data))
    data <- data %>% filter(.data$ae_grade %in% selected_grade)

  out <- ca_ae(data, group = .data$group, ae = .data$ae_domain,
               contr_indicator = contr_indicator,
               mass_indicator = mass_indicator,
               contr_threshold = contr_threshold)
  return(out)
}

#'@import dplyr
#'@import magrittr
shiny_domain_grade <- function(data, selected_cycle,
                               contr_indicator, mass_indicator,
                               contr_threshold){

  if ("ae_cycle" %in% colnames(data))
    data <- data %>% filter(.data$ae_cycle %in% selected_cycle)

  data <- data %>%
    mutate(ae_domain_grade =
             paste0(.data$ae_domain, ": G", .data$ae_grade))

  out <- ca_ae(data, group = .data$group, ae = .data$ae_domain_grade,
               contr_indicator = contr_indicator,
               mass_indicator = mass_indicator,
               contr_threshold = contr_threshold)
  return(out)
}

#'@import dplyr
#'@import magrittr
shiny_term <- function(data, selected_cycle, selected_domain, selected_grade,
                       contr_indicator, mass_indicator,
                       contr_threshold){

  if ("ae_cycle" %in% colnames(data))
    data <- data %>% filter(.data$ae_cycle %in% selected_cycle)
  if ("ae_domain" %in% colnames(data))
    data <- data %>% filter(.data$ae_domain %in% selected_domain)
  if ("ae_grade" %in% colnames(data))
    data <- data %>% filter(.data$ae_grade %in% selected_grade)

  out <- ca_ae(data, group = .data$group, ae = .data$ae_term,
               contr_indicator = contr_indicator,
               mass_indicator = mass_indicator,
               contr_threshold = contr_threshold)
  return(out)
}

#'@import dplyr
#'@import magrittr
shiny_term_grade <- function(data, selected_cycle, selected_domain,
                             contr_indicator, mass_indicator,
                             contr_threshold){

  if ("ae_cycle" %in% colnames(data))
    data <- data %>% filter(.data$ae_cycle %in% selected_cycle)
  if ("ae_domain" %in% colnames(data))
    data <- data %>% filter(.data$ae_domain %in% selected_domain)

  data <- data %>%
    mutate(ae_term_grade =
             paste0(.data$ae_term, ": G", .data$ae_grade))

  out <- ca_ae(data, group = .data$group, ae = .data$ae_term_grade,
               contr_indicator = contr_indicator,
               mass_indicator = mass_indicator,
               contr_threshold = contr_threshold)
  return(out)
}

