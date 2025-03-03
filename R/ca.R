#'Correspondence Analysis of Adverse Events
#'@param data data.frame or tibble object.
#'@param id unquoted expression indicating the
#'variable name in \code{data} that corresponds to the id variable.
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
#'library(dplyr)
#'
#'id <- rep(1:50, each = 2)
#'group <- c(rep("A", 50), rep("B", 50))
#'ae_grade <- sample(1:5, size = 100, replace = TRUE)
#'ae_domain <- sample(c("D", "E"), size = 100, replace = TRUE)
#'ae_term <- sample(c("F", "G", "H", "I"), size = 100, replace = TRUE)
#'df <- tibble(id = id, trt = group,
#'             ae_g = ae_grade, ae_d = ae_domain, ae_t = ae_term)
#'test <- df |> ca_ae(id = id,
#'                    group = trt,
#'                    ae = ae_g,
#'                    label = "AE",
#'                    contr_indicator = TRUE,
#'                    mass_indicator = TRUE,
#'                    contr_threshold = 0.01,
#'                    mass_threshold = 0.01)
#'
#'@import ggplot2
#'@import dplyr
#'@importFrom rlang .data enquos :=
#'@importFrom tidyr pivot_wider separate
#'@importFrom ca ca
#'@importFrom stats addmargins
#'@importFrom ggrepel geom_label_repel
#'
#'@export
ca_ae <- function(data, id, group, ae_class, label = "AE",
                  contr_indicator = TRUE, mass_indicator = TRUE,
                  contr_threshold = NULL, mass_threshold = NULL) {

  temp <- enquos(group = group,
                 ae = ae_class, id = id,
                 .ignore_empty = "all")

  aux <- data |> select(!!!temp) |>
    na.exclude() |>
    distinct(id, .data$ae, .keep_all = TRUE)
  total <- data |> select(!!!temp) |>
    distinct(id, .keep_all = TRUE) |>
    count(group)
  tab <- table(aux$ae, aux$group)
  p <- t(t(tab)/as.numeric(total$n))
  q <- 1 - p
  rownames(q) <- paste0(rownames(q), "_C")
  tab.ca <- rbind(p, q)

  res.ca <- ca(tab.ca)

  names(dimnames(p)) <- c("ae", "group")
  average <- round(100*rowMeans(p), 3)
  tab_rel <- round(100*p, 3) |> as_tibble() |>
   pivot_wider(names_from = .data$group, values_from = .data$n) |>
   mutate(Average = average)

  if (is.null(contr_threshold))
    contr_threshold <- 1/nrow(tab)
  if (is.null(mass_threshold))
    mass_threshold <- 1/nrow(tab)

  expected_threshold <- 1/nrow(tab)

  names(dimnames(tab)) <- c("ae", "group")
  tab_abs <- tab |> as_tibble() |>
    pivot_wider(names_from = .data$group, values_from = .data$n)

  inertia <- res.ca$sv^2
  total_inertia <- sum(inertia)
  explained_var <- 100*inertia/total_inertia
  tab_inertia = tibble(Dimension = 1:length(inertia),
                       Inertia = inertia,
                       'Explained Variance' = explained_var)

  if (ncol(tab_abs) < 4){

    aux <- res.ca$rowcoord*sqrt(res.ca$rowmass)

    contr <- round(100*(res.ca$rowcoord*sqrt(res.ca$rowmass))^2, 2)
    tab_contr <- as_tibble(contr, rownames = "labels") |>
      separate(labels, into = c("ae", "delete"),
               sep = "_", fill = "right") |>
      group_by(.data$ae) |>
      summarise(across(starts_with("Dim"), sum, .names = "{col}"),
                .groups = "drop_last")
    colnames(tab_contr)[-1] <- paste0("Dim ", 1:ncol(aux))

    standard.coordinates.row <-
      as_tibble(aux, rownames = "labels") |>
      separate(labels, into = c("labels", "delete"),
               sep = "_", fill = "right") |>
      filter(is.na(.data$delete)) |>
      select(-all_of("delete")) |>
      mutate(type = "row",
             contr = tab_contr[[2]]/100,
             mass = average/100) |>
      filter(.data$contr > contr_threshold & .data$mass > mass_threshold)
    colnames(standard.coordinates.row)[2] <- "dim_1"

    group_mass <- ifelse(is.finite(min(standard.coordinates.row$mass, na.rm = TRUE)) &
                           is.finite(max(standard.coordinates.row$mass, na.rm = TRUE)),
                         (min(standard.coordinates.row$mass, na.rm = TRUE) +
                            max(standard.coordinates.row$mass, na.rm = TRUE))/2,
                         ifelse(is.finite(max(standard.coordinates.row$mass, na.rm = TRUE)),
                                0.5*max(standard.coordinates.row$mass, na.rm = TRUE),
                                ifelse(is.finite(min(standard.coordinates.row$mass, na.rm = TRUE)),
                                       1.5*min(standard.coordinates.row$mass, na.rm = TRUE), 0.5)))

    principal.coordinates.col <-
      tibble(dim_1 = as.numeric(res.ca$colcoord*res.ca$sv)) |> #
      mutate(labels = rownames(res.ca$colcoord),
             type = "col", contr = 1, mass = group_mass)

    selected_classes <- as.character(standard.coordinates.row$labels)

    if (nrow(standard.coordinates.row) > 0)
      standard.coordinates.row <- standard.coordinates.row |>
      mutate(contr = .data$contr/max(.data$contr))


    dp <- bind_rows(principal.coordinates.col, standard.coordinates.row)

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
      geom_label_repel(aes(label = .data$labels),
                       xlim = c(-Inf, Inf), ylim = c(-Inf, Inf),
                       min.segment.length = 0) +
      scale_colour_manual(values = c("red", "blue")) +
      labs(x = paste0("Dim 1 ", "(", round(explained_var[1], 2), "%)")) +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(size = 20)) +
      scale_size_continuous(range = c(3, 6)) +
      scale_alpha_continuous(range = c(0.3, 1))

    temp <- round(100*(res.ca$rowcoord*sqrt(res.ca$rowmass))^2, 2)
    tab_contr <- as_tibble(temp, rownames = "ae") |>
      separate(.data$ae, into = c("ae", "delete"),
               sep = "_", fill = "right") |>
      group_by(.data$ae) |>
      summarize(across(starts_with("Dim"), sum, .names = "{col}"))
    colnames(tab_contr)[-1] <- "Dim 1"

  } else {

    aux <- res.ca$rowcoord*sqrt(res.ca$rowmass)
    colnames(aux) <- paste0("dim_", 1:ncol(aux))

    contr <- round(100*(res.ca$rowcoord*sqrt(res.ca$rowmass))^2, 2)
    tab_contr <- as_tibble(contr, rownames = "labels") |>
      separate(labels, into = c("ae", "delete"),
               sep = "_", fill = "right") |>
      group_by(.data$ae) |>
      summarise(across(starts_with("Dim"), sum, .names = "{col}"),
                .groups = "drop_last")
    colnames(tab_contr)[-1] <- paste0("Dim ", 1:ncol(aux))

    standard.coordinates.row <-
      as_tibble(aux, rownames = "labels") |>
      separate(labels, into = c("labels", "delete"),
               sep = "_", fill = "right") |>
      filter(is.na(.data$delete)) |>
      select(-all_of("delete")) |>
      mutate(type = "row",
             contr = pmax(tab_contr[[2]]/100, tab_contr[[3]]/100),
             mass = average/100) |>
      filter(.data$contr > contr_threshold & .data$mass > mass_threshold)
    selected_classes <- as.character(standard.coordinates.row$labels)

    group_mass <- ifelse(is.finite(min(standard.coordinates.row$mass, na.rm = TRUE)) &
      is.finite(max(standard.coordinates.row$mass, na.rm = TRUE)),
      (min(standard.coordinates.row$mass, na.rm = TRUE) +
        max(standard.coordinates.row$mass, na.rm = TRUE))/2,
      ifelse(is.finite(max(standard.coordinates.row$mass, na.rm = TRUE)),
             0.5*max(standard.coordinates.row$mass, na.rm = TRUE),
             ifelse(is.finite(min(standard.coordinates.row$mass, na.rm = TRUE)),
                    1.5*min(standard.coordinates.row$mass, na.rm = TRUE), 0.5)))

    aux <- res.ca$colcoord%*%diag(res.ca$sv)
    colnames(aux) <- paste0("dim_", 1:ncol(aux))
    principal.coordinates.col <-
      as_tibble(aux) |>
      mutate(labels = rownames(res.ca$colcoord),
             type = "col",
             contr = 1, mass = group_mass)

    if (nrow(standard.coordinates.row) > 0)
      standard.coordinates.row <- standard.coordinates.row |>
      mutate(contr = .data$contr/max(.data$contr))

    dp <- bind_rows(principal.coordinates.col, standard.coordinates.row)


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
      geom_label_repel(aes(label = .data$labels),
                       xlim = c(-Inf, Inf), ylim = c(-Inf, Inf),
                       min.segment.length = 0) +
      scale_colour_manual(values = c("red", "blue")) +
      labs(x = paste0("Dim 1 ", "(", round(explained_var[1], 2), "%)"),
           y = paste0("Dim 2 ", "(", round(explained_var[2], 2), "%)")) +
      theme_minimal() +
      theme(legend.position = "none",
            text = element_text(size = 20))+
      scale_size_continuous(range = c(3, 6)) +
      scale_alpha_continuous(range = c(0.3, 1))
  }

  tab_rel <- tab_rel |>
    filter(.data$ae %in% selected_classes) |>
    rename(!!label := .data$ae) |>
    mutate(across(where(is.numeric), ~ format(.x, digits = 2, nsmall = 2)))
  colnames(tab_rel)[-c(1, ncol(tab_rel))] <-
    paste0(colnames(tab_rel)[-c(1, ncol(tab_rel))], "<br> (n = ", total$n, ")")
  tab_contr  <- tab_contr |> filter(.data$ae %in% selected_classes) |>
    rename(!!label := .data$ae)

  out <- list(tab_abs = tab_abs, tab_rel = tab_rel,
              total_inertia = total_inertia,
              tab_inertia = tab_inertia,
              tab_contr = tab_contr,
              asymmetric_plot = asymmetric_plot)

  return(out)
}

