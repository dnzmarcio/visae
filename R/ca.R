#'Correspondence Analysis of Adverse Events
#'@param data data.frame or tibble object.
#'@param group unquoted expression indicating the
#'variable name in \code{data} that corresponds to the group variable.
#'@param ae_class unquoted expression indicating the
#'variable name in \code{data} that corresponds to AE class.
#'@param label character value indicating the
#'column name of AE class in resulting tables.
#'@param contr_indicator logical value indicating the
#'use of color intensity to represent the maximum contribution of each \code{ae_class}.
#'@param mass_indicator logical value indicating the
#'use of dot size to represent the overall relative frequency of each \code{ae_class}.
#'@param contr_threshold numerical value between 0 an 1 filtering
#'\code{ae_class} with contribution greater than \code{contr_threshold}.
#'@param mass_threshold numerical value between 0 an 1 filtering
#'\code{ae_class} with mass greater than \code{mass_threshold}.
#'
#'@return a list of
#'\item{tab_abs}{a tibble showing absolute frequency of \code{ae_class} by \code{group};}
#'\item{tab_rel}{a tibble showing percent of \code{ae_class} by \code{group};}
#'\item{total_inertia}{a numerical value indicating the total inertia;}
#'\item{tab_inertia}{a tibble showing inertia broken down by dimension and the percent relative to the total inertia;}
#'\item{asymmetric_plot}{a contribution biplot.}
#'
#'@references Levine RA, Sampson E, Lee TC. Journal of Computational and Graphical Statistics. Wiley Interdisciplinary Reviews: Computational Statistics. 2014 Jul;6(4):233-9.
#'
#'@examples
#'library(magrittr)
#'library(dplyr)
#'group <- c(rep("A", 50), rep("B", 50))
#'ae_grade <- sample(1:5, size = 100, replace = TRUE)
#'ae_domain <- sample(c("C", "D"), size = 100, replace = TRUE)
#'ae_term <- sample(c("E", "F", "G", "H"), size = 100, replace = TRUE)
#'dt <- tibble(trt = group,
#'             ae_g = ae_grade, ae_d = ae_domain, ae_t = ae_term)
#'dt %>% ca_ae(., group = trt, ae = ae_g, label = "AE",
#'             contr_indicator = TRUE, mass_indicator = TRUE,
#'             contr_threshold = 0.01, mass_threshold = 0.01)
#'
#'@import magrittr
#'@import ggplot2
#'@import dplyr
#'@importFrom rlang .data enquos :=
#'@importFrom tidyr pivot_wider
#'@importFrom ca ca
#'@importFrom stats addmargins
#'@importFrom ggrepel geom_text_repel
#'@export
ca_ae <- function(data, group, ae_class, label = "AE",
                  contr_indicator = TRUE, mass_indicator = TRUE,
                  contr_threshold = NULL, mass_threshold = NULL) {

  temp <- enquos(group = group,
                 ae = ae_class,
                 .ignore_empty = "all")
  aux <- data %>% select(!!!temp)

  tab <- with(aux, table(ae, group))
  res.ca <- ca(tab)

  tab_rel <- round(100*prop.table(tab, 2), 3) %>% as_tibble() %>%
   pivot_wider(names_from = .data$group, values_from = .data$n) %>%
    mutate(Average = round(100*res.ca$rowmass, 3)) %>%
    rename(!!label := .data$ae)

  if (is.null(contr_threshold))
    contr_threshold <- 1/nrow(tab)
  if (is.null(mass_threshold))
    mass_threshold <- 1/nrow(tab)

  tab_abs <- addmargins(tab) %>% as_tibble() %>%
    pivot_wider(names_from = .data$group, values_from = .data$n) %>%
    mutate(ae = ifelse(.data$ae == "Sum", "Total", .data$ae)) %>%
    rename(!!label := .data$ae, Total = .data$Sum)

  inertia <- res.ca$sv^2
  total_inertia <- sum(inertia)
  explained_var <- 100*inertia/total_inertia
  tab_inertia = tibble(Dimension = 1:length(inertia),
                       Inertia = inertia,
                       'Explained Variance' = explained_var)

  if (ncol(tab_abs) == 4){

    principal.coordinates.col <-
      tibble(dim_1 = res.ca$colcoord*res.ca$sv) %>%
      mutate(labels = rownames(res.ca$colcoord),
             type = "col", contr = 1, mass = 1)

    aux <- res.ca$rowcoord*sqrt(res.ca$rowmass)
    standard.coordinates.row <-
      tibble(dim_1 = aux) %>%
      mutate(labels = rownames(res.ca$rowcoord),
             type = "row",
             contr = aux^2,
             mass = res.ca$rowmass) %>%
      filter(.data$contr > contr_threshold & .data$mass > mass_threshold) %>%
      mutate(contr = .data$contr/max(.data$contr),
             mass = .data$mass/max(.data$mass))


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
    aux <- res.ca$colcoord*res.ca$sv
    colnames(aux) <- paste0("dim_", 1:ncol(aux))
    principal.coordinates.col <-
      as_tibble(aux) %>%
      mutate(labels = rownames(res.ca$colcoord),
             type = "col",
             contr = 1,
             mass = 1)

    aux <- res.ca$rowcoord*sqrt(res.ca$rowmass)
    colnames(aux) <- paste0("dim_", 1:ncol(aux))
    standard.coordinates.row <-
      as_tibble(aux) %>%
      mutate(labels = rownames(res.ca$rowcoord),
             type = "row",
             contr = pmax(aux[, 1]^2, aux[, 2]^2),
             mass = res.ca$rowmass) %>%
      filter(.data$contr > contr_threshold & .data$mass > mass_threshold)  %>%
      mutate(contr = .data$contr/max(.data$contr),
             mass = .data$mass/max(.data$mass))

    dp <- bind_rows(principal.coordinates.col,
                    standard.coordinates.row)


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
              tab_inertia = tab_inertia,
              asymmetric_plot = asymmetric_plot)

  return(out)
}

