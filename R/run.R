#'Shiny App for Correspondence Analysis of Adverse Events
#'
#'@param data data.frame or tibble object.
#'@param group unquoted expression indicating the
#'variable name in \code{data} that corresponds to the group variable.
#'@param ae_grade unquoted expression indicating the
#'variable name in \code{data} that corresponds to AE grade class.
#'@param ae_domain unquoted expression indicating the
#'variable name in \code{data} that corresponds to AE domain class.
#'@param ae_term unquoted expression indicating the
#'variable name in \code{data} that corresponds to AE term class.
#'@param ae_cycle unquoted expression indicating the
#'variable name in \code{data} that corresponds to AE cycle.
#'
#'@return an interactive web application to perform correspondence analysis
#'for adverse event data.
#'
#'
#'@examples
#'\dontrun{
#'library(magrittr)
#'library(dplyr)
#'group <- c(rep("A", 50), rep("B", 50))
#'ae_grade <- sample(1:5, size = 100, replace = TRUE)
#'ae_domain <- sample(c("C", "D"), size = 100, replace = TRUE)
#'ae_term <- sample(c("E", "F", "G", "H"), size = 100, replace = TRUE)
#'dt <- tibble(trt = group,
#'             ae_g = ae_grade, ae_d = ae_domain, ae_t = ae_term)
#'dt %>% run_ca(., group = trt,
#'              ae_grade = ae_g,
#'              ae_domain = ae_d,
#'              ae_term = ae_t)
#'              }
#'
#'@import shiny
#'@import magrittr
#'@import dplyr
#'@importFrom shinyjs js useShinyjs extendShinyjs
#'@importFrom DT renderDataTable dataTableOutput
#'@importFrom rlang enquos enquo quo_is_null
#'@importFrom stats na.exclude
#'@export
run_ca <- function(data,
                   group,
                   ae_grade = NULL,
                   ae_domain = NULL,
                   ae_term = NULL,
                   ae_cycle = NULL) {

  group <- enquo(group)
  ae_grade <- enquo(ae_grade)
  ae_domain <- enquo(ae_domain)
  ae_term <- enquo(ae_term)
  ae_cycle <- enquo(ae_cycle)

  if (quo_is_null(ae_grade) & quo_is_null(ae_domain) & quo_is_null(ae_term))
    stop("There is no toxicity data available.
         Please input either ae_grade, ae_domain or ae_term.")

  aux <- enquos(group = group,
                ae_grade = ae_grade,
                ae_domain = ae_domain,
                ae_term = ae_term,
                ae_cycle = ae_cycle,
                .ignore_empty = "all")

  cond <- lapply(aux, function(x) !quo_is_null(x))
  aux <- aux[unlist(cond)]

  data <- data %>% select(!!!aux)


  #https://stackoverflow.com/questions/49470474/saving-r-shiny-app-as-a-function-with-arguments-passed-to-the-shiny-app
  shinyOptions(data = data)
  source(system.file("ca_shiny.R", package = "visae"))$value
}

