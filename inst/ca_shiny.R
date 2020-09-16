appDir <- getwd()
data <- getShinyOption("data")

#https://stackoverflow.com/questions/44319664/r-shiny-condition-a-tab-in-the-navbar-based-on-previous-tabs-condition
jscode <- "
      shinyjs.disabletabD =function(name){
      $('ul li:has(a[data-value= \"Domain\"])').addClass('disabled');
      $('ul li:has(a[data-value= \"DomainGrade\"])').addClass('disabled');
      }

      shinyjs.enabletabD = function(name){
      $('ul li:has(a[data-value= \"Domain\"])').removeClass('disabled');
      $('ul li:has(a[data-value= \"DomainGrade\"])').removeClass('disabled');
      }

      shinyjs.disabletabT =function(name){
      $('ul li:has(a[data-value= \"TermGrade\"])').addClass('disabled');
      $('ul li:has(a[data-value= \"Term\"])').addClass('disabled');
      }

      shinyjs.enabletabT = function(name){
      $('ul li:has(a[data-value= \"TermGrade\"])').removeClass('disabled');
      $('ul li:has(a[data-value= \"Term\"])').removeClass('disabled');
      }
      "

ui = function(data){

  if("ae_cycle" %in% colnames(data)){
    cycle_options <- as.numeric(levels(as.factor(data$ae_cycle)))
  } else {
    cycle_options <- NULL
  }

  if("ae_grade" %in% colnames(data)){
    grade_options <- as.numeric(levels(as.factor(data$ae_grade)))
  } else {
    grade_options <- NULL
  }

  if("ae_domain" %in% colnames(data)){
    domain_options <- levels(as.factor(data$ae_domain))
  } else {
    domain_options <- NULL
  }

  if("ae_domain" %in% colnames(data) & "ae_grade" %in% colnames(data)){
    domain_grade_options <- levels(as.factor(paste0(data$ae_domain, ":", data$ae_grade)))
  } else {
    domain_grade_options <- NULL
  }

  if("ae_term" %in% colnames(data)){
    term_options <- levels(as.factor(data$ae_term))
  } else {
    term_options <- NULL
  }

  if("ae_term" %in% colnames(data) & "ae_grade" %in% colnames(data)){
    term_grade_options <- levels(as.factor(paste0(data$ae_term, ":", data$ae_grade)))
  } else {
    term_grade_options <- NULL
  }

  fluidPage(
    navbarPage("AE Report",
               tabPanel("Grade",
                        value = "Grade",
                        sidebarLayout(
                          sidebarPanel(h4("Trial"),
                                       selectInput(
                                         inputId = 'selected_cycle_grade',
                                         label = h6('Selected Cycles'),
                                         multiple = TRUE,
                                         selectize = TRUE,
                                         choices = cycle_options,
                                         selected = cycle_options
                                       ),
                                       h4("Plot"),
                                       numericInput(
                                         inputId = 'contr_threshold_grade',
                                         label = h6('Contribution Threshold'),
                                         min = 0,
                                         max = 100,
                                         step = round(100/length(grade_options), 2),
                                         value = round(100/length(grade_options), 2)
                                       ),
                                       numericInput(
                                         inputId = 'mass_threshold_grade',
                                         label = h6('Mass Threshold'),
                                         min = 0,
                                         max = 100,
                                         step = round(100/length(grade_options), 2),
                                         value = round(100/length(grade_options), 2)
                                       ),
                                       checkboxInput(
                                         inputId = 'contr_grade',
                                         label = h6('Contribution Color Intensity'),
                                         value = TRUE
                                       ),
                                       checkboxInput(
                                         inputId = 'mass_grade',
                                         label = h6('Mass Dot Size'),
                                         value = TRUE
                                       )
                          ),
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Table", DT::dataTableOutput("ae_grade_table")),
                              tabPanel("Contribution Plot", plotOutput("ae_grade_contr_plot",
                                                                       height = "600px",
                                                                       width = "600px")),
                              tabPanel("Asymmetric Plot", plotOutput("ae_grade_biplot",
                                                                     height = "600px",
                                                                     width = "600px"))
                            )
                          )
                        )
               ),
               tabPanel("Domain",
                        value = "Domain",
                        sidebarLayout(
                          sidebarPanel(h4("Trial"),
                                       selectInput(
                                         inputId = 'selected_cycle_domain',
                                         label = h6('Selected Cycles'),
                                         multiple = TRUE,
                                         selectize = TRUE,
                                         choices = cycle_options,
                                         selected = cycle_options
                                       ),
                                       selectInput(
                                         inputId = 'selected_grade_domain',
                                         label = h6('Selected Grades'),
                                         multiple = TRUE,
                                         selectize = TRUE,
                                         choices = grade_options,
                                         selected = grade_options
                                       ),
                                       h4("Plot"),
                                       numericInput(
                                         inputId = 'contr_threshold_domain',
                                         label = h6('Contribution Threshold'),
                                         min = 0,
                                         max = 100,
                                         step = round(100/length(domain_options), 2),
                                         value = round(100/length(domain_options), 2)
                                       ),
                                       numericInput(
                                         inputId = 'mass_threshold_domain',
                                         label = h6('Mass Threshold'),
                                         min = 0,
                                         max = 100,
                                         step = round(100/length(domain_options), 2),
                                         value = round(100/length(domain_options), 2)
                                       ),
                                       checkboxInput(
                                         inputId = 'contr_domain',
                                         label = h6('Contribution Color Intensity'),
                                         value = TRUE
                                       ),
                                       checkboxInput(
                                         inputId = 'mass_domain',
                                         label = h6('Mass Dot Size'),
                                         value = TRUE
                                       )
                          ),
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Table", DT::dataTableOutput("ae_domain_table")),
                              tabPanel("Contribution Plot", plotOutput("ae_domain_contr_plot",
                                                                       height = "600px",
                                                                       width = "600px")),
                              tabPanel("Asymmetric Plot", plotOutput("ae_domain_biplot",
                                                                     height = "600px",
                                                                     width = "600px"))
                            )
                          )
                        )
               ),
               tabPanel("Domain and Grade",
                        value = "DomainGrade",
                        sidebarLayout(
                          sidebarPanel(h4("Trial"),
                                       selectInput(
                                         inputId = 'selected_cycle_domain_grade',
                                         label = h6('Selected Cycles'),
                                         multiple = TRUE,
                                         selectize = TRUE,
                                         choices = cycle_options,
                                         selected = cycle_options
                                       ),
                                       h4("Plot"),
                                       numericInput(
                                         inputId = 'contr_threshold_domain_grade',
                                         label = h6('Contribution Threshold'),
                                         min = 0,
                                         max = 100,
                                         step = round(100/length(domain_grade_options), 2),
                                         value = round(100/length(domain_grade_options), 2)
                                       ),
                                       numericInput(
                                         inputId = 'mass_threshold_domain_grade',
                                         label = h6('Mass Threshold'),
                                         min = 0,
                                         max = 100,
                                         step = round(100/length(domain_grade_options), 2),
                                         value = round(100/length(domain_grade_options), 2)
                                       ),
                                       checkboxInput(
                                         inputId = 'contr_domain_grade',
                                         label = h6('Contribution Color Intensity'),
                                         value = TRUE
                                       ),
                                       checkboxInput(
                                         inputId = 'mass_domain_grade',
                                         label = h6('Mass Dot Size'),
                                         value = TRUE
                                       )
                          ),
                          mainPanel(tabsetPanel(
                            tabPanel("Table", DT::dataTableOutput("ae_domain_grade_table")),
                            tabPanel("Contribution Plot", plotOutput("ae_domain_grade_contr_plot",
                                                                     height = "600px",
                                                                     width = "600px")),
                            tabPanel("Asymmetric Plot", plotOutput("ae_domain_grade_biplot",
                                                                   height = "600px",
                                                                   width = "600px"))
                          )
                          )
                        )
               ),
               tabPanel("Term",
                        value = "Term",
                        sidebarLayout(
                          sidebarPanel(h4("Trial"),
                                       selectInput(
                                         inputId = 'selected_cycle_term',
                                         label = h6('Selected Cycles'),
                                         multiple = TRUE,
                                         selectize = TRUE,
                                         choices = cycle_options,
                                         selected = cycle_options
                                       ),
                                       selectInput(
                                         inputId = 'selected_domain_term',
                                         label = h6('Selected Domains'),
                                         multiple = TRUE,
                                         selectize = TRUE,
                                         choices = domain_options,
                                         selected = domain_options
                                       ),
                                       selectInput(
                                         inputId = 'selected_grade_term',
                                         label = h6('Selected Grades'),
                                         multiple = TRUE,
                                         selectize = TRUE,
                                         choices = grade_options,
                                         selected = grade_options
                                       ),
                                       h4("Plot"),
                                       numericInput(
                                         inputId = 'contr_threshold_term',
                                         label = h6('Contribution Threshold'),
                                         min = 0,
                                         max = 100,
                                         step = round(100/length(term_options), 2),
                                         value = round(100/length(term_options), 2)
                                       ),
                                       numericInput(
                                         inputId = 'mass_threshold_term',
                                         label = h6('Mass Threshold'),
                                         min = 0,
                                         max = 100,
                                         step = round(100/length(term_options), 2),
                                         value = round(100/length(term_options), 2)
                                       ),
                                       checkboxInput(
                                         inputId = 'contr_term',
                                         label = h6('Contribution Color Intensity'),
                                         value = TRUE
                                       ),
                                       checkboxInput(
                                         inputId = 'mass_term',
                                         label = h6('Mass Dot Size'),
                                         value = TRUE
                                       )
                          ),
                          mainPanel(tabsetPanel(
                            tabPanel("Table", DT::dataTableOutput("ae_term_table")),
                            tabPanel("Asymmetric Plot", plotOutput("ae_term_biplot"))
                          )
                          )
                        )
               ),
               tabPanel("Term and Grade",
                        value = "TermGrade",
                        sidebarLayout(
                          sidebarPanel(h4("Trial"),
                                       selectInput(
                                         inputId = 'selected_cycle_term_grade',
                                         label = h6('Selected Cycles'),
                                         multiple = TRUE,
                                         selectize = TRUE,
                                         choices = cycle_options,
                                         selected = cycle_options
                                       ),
                                       selectInput(
                                         inputId = 'selected_domain_term_grade',
                                         label = h6('Selected Domains'),
                                         multiple = TRUE,
                                         selectize = TRUE,
                                         choices = domain_options,
                                         selected = domain_options
                                       ),
                                       h4("Plot"),
                                       numericInput(
                                         inputId = 'contr_threshold_term_grade',
                                         label = h6('Contribution Threshold'),
                                         min = 0,
                                         max = 100,
                                         step = round(100/length(term_grade_options), 2),
                                         value = round(100/length(term_grade_options), 2)
                                       ),
                                       numericInput(
                                         inputId = 'mass_threshold_term_grade',
                                         label = h6('Mass Threshold'),
                                         min = 0,
                                         max = 100,
                                         step = round(100/length(term_grade_options), 2),
                                         value = round(100/length(term_grade_options), 2)
                                       ),
                                       checkboxInput(
                                         inputId = 'contr_term_grade',
                                         label = h6('Contribution Color Intensity'),
                                         value = TRUE
                                       ),
                                       checkboxInput(
                                         inputId = 'mass_term_grade',
                                         label = h6('Mass Dot Size'),
                                         value = TRUE
                                       )
                          ),
                          mainPanel(tabsetPanel(
                            tabPanel("Table", DT::dataTableOutput("ae_term_grade_table")),
                            tabPanel("Contribution Plot", plotOutput("ae_term_grade_contr_plot",
                                                                     height = "600px",
                                                                     width = "600px")),
                            tabPanel("Asymmetric Plot", plotOutput("ae_term_grade_biplot",
                                                                   height = "600px",
                                                                   width = "600px"))
                          )
                          )
                        )
               ),
               tabPanel("About",
                        mainPanel(tabsetPanel(
                          tabPanel("Authors",
                                   column(width = 8, offset = 1,
                                          p(h4("Contact")),
                                          p("Marcio Augusto Diniz", "<marcio.diniz@cshs.org>"),
                                          p("Michael Luu", "<michael.luu@cshs.org>"),
                                          p("Gillian Gresham", "<gillian.gresham@cshs.org>"),
                                          p("Andre Rogatko", "<andre.rogatko@cshs.org>"),
                                          p(strong("Cedars Sinai Medical Center")),
                                          p(strong("Samuel Oschin Comprehensive Cancer Institute,
                                              Biostatistics Core"))
                                   ),
                                   column(width = 8, offset = 1,
                                          p(h4("Funding")),
                                          p(a("Moonshot Initiative",
                                              href="https://www.cancer.gov/research/key-initiatives/moonshot-cancer-initiative"),
                                            " - Grant Number: 1U01CA232859-01")

                                   ),
                                   tabPanel("Correspondence Analysis", )
                          )
                        )
                        )
               ),

               shinyjs::useShinyjs(),
               shinyjs::extendShinyjs(text = jscode)
    )
  )
}

server = function(input, output, session) {

  shiny_grade <- function(data, selected_cycle,
                          contr_indicator, mass_indicator,
                          contr_threshold, mass_threshold){

    if ("ae_cycle" %in% colnames(data))
      data <- data %>% filter(.data$ae_cycle %in% selected_cycle)

    data <- data %>% na.exclude() %>%
      mutate(ae_grade = paste0("G", .data$ae_grade))

    out <- visae::ca_ae(data, group = .data$group,
                        ae_class = .data$ae_grade,
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
      data <- data %>% filter(.data$ae_cycle %in% selected_cycle)
    if ("ae_grade" %in% colnames(data))
      data <- data %>% filter(.data$ae_grade %in% selected_grade)

    out <- visae::ca_ae(data, group = .data$group,
                        ae_class = .data$ae_domain,
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
      data <- data %>% filter(.data$ae_cycle %in% selected_cycle)

    data <- data %>% na.exclude() %>%
      mutate(ae_domain_grade =
               paste0(.data$ae_domain, ": G", .data$ae_grade))

    out <- visae::ca_ae(data, group = .data$group,
                        ae_class = .data$ae_domain_grade,
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
      data <- data %>% filter(.data$ae_cycle %in% selected_cycle)
    if ("ae_domain" %in% colnames(data))
      data <- data %>% filter(.data$ae_domain %in% selected_domain)
    if ("ae_grade" %in% colnames(data))
      data <- data %>% filter(.data$ae_grade %in% selected_grade)

    out <- visae::ca_ae(data, group = .data$group,
                        ae_class = .data$ae_term,
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
      data <- data %>% filter(.data$ae_cycle %in% selected_cycle)
    if ("ae_domain" %in% colnames(data))
      data <- data %>% filter(.data$ae_domain %in% selected_domain)

    data <- data %>% na.exclude() %>%
      mutate(ae_term_grade =
               paste0(.data$ae_term, ": G", .data$ae_grade))

    out <- visae::ca_ae(data, group = .data$group,
                        ae_class = .data$ae_term_grade,
                        label = "Term:Grade",
                        contr_indicator = contr_indicator,
                        mass_indicator = mass_indicator,
                        contr_threshold = contr_threshold,
                        mass_threshold = mass_threshold)
    return(out)
  }

  if('ae_grade' %in% colnames(data)){

    output$ae_grade_table <- DT::renderDataTable(
      shiny_grade(data,
                  selected_cycle = input$selected_cycle_grade,
                  contr_indicator = input$contr_grade,
                  mass_indicator = input$mass_grade,
                  contr_threshold = input$contr_threshold_grade/100,
                  mass_threshold = input$mass_threshold_grade/100)$tab_rel,
      rownames = FALSE
    )

    plotInput_grade <- reactive({
      gp <- shiny_grade(data,
                        selected_cycle = input$selected_cycle_grade,
                        contr_indicator = input$contr_grade,
                        mass_indicator = input$mass_grade,
                        contr_threshold = input$contr_threshold_grade/100,
                        mass_threshold = input$mass_threshold_grade/100)$asymmetric_plot
    })

    output$ae_grade_biplot  <- renderPlot({print(plotInput_grade())})


    output$ae_grade_contr_plot <- renderPlot(
      shiny_grade(data,
                  selected_cycle = input$selected_cycle_grade,
                  contr_indicator = input$contr_grade,
                  mass_indicator = input$mass_grade,
                  contr_threshold = input$contr_threshold_grade/100,
                  mass_threshold = input$mass_threshold_grade/100)$contr_plot
    )

    output$downloadplot_grade <- downloadHandler(
      filename = function() { paste('ca_grade.pdf', sep='') },
      content = function(file) {
        ggsave(file, plot = plotInput_grade(), device = "pdf",
                height = 7, width = 7)
      }
    )
  }

  if('ae_domain' %in% colnames(data)){
    shinyjs::js$enabletabD("Domain")

    output$ae_domain_table <- DT::renderDataTable(
      shiny_domain(data,
                   selected_cycle = input$selected_cycle_domain,
                   selected_grade = input$selected_grade_domain,
                   contr_indicator = input$contr_domain,
                   mass_indicator = input$mass_domain,
                   contr_threshold = input$contr_threshold_domain/100,
                   mass_threshold = input$mass_threshold_domain/100)$tab_rel
      )

    plotInput_domain <- reactive({
      gp <- shiny_domain(data,
                         selected_cycle = input$selected_cycle_domain,
                         selected_grade = input$selected_grade_domain,
                         contr_indicator = input$contr_domain,
                         mass_indicator = input$mass_domain,
                         contr_threshold = input$contr_threshold_domain/100,
                         mass_threshold = input$mass_threshold_domain/100)$asymmetric_plot
    })

    output$ae_domain_biplot <- renderPlot({print(plotInput_domain())})

    output$ae_domain_contr_plot <- renderPlot(
      shiny_domain(data,
                   selected_cycle = input$selected_cycle_domain,
                   selected_grade = input$selected_grade_domain,
                   contr_indicator = input$contr_domain,
                   mass_indicator = input$mass_domain,
                   contr_threshold = input$contr_threshold_domain/100,
                   mass_threshold = input$mass_threshold_domain/100)$contr_plot
    )

    output$downloadplot_domain <- downloadHandler(
      filename = function() { paste('ca_domain.pdf', sep='') },
      content = function(file) {
        ggsave(file, plot = plotInput_domain(), device = "pdf",
                height = 7, width = 7)
      }
    )

  } else {
    shinyjs::js$disabletabD("Domain")
  }

  if('ae_domain' %in% colnames(data) & 'ae_grade' %in% colnames(data)){
    shinyjs::js$enabletabD("DomainGrade")

    output$ae_domain_grade_table <- DT::renderDataTable(
      shiny_domain_grade(data,
                         selected_cycle =
                           input$selected_cycle_domain_grade,
                         contr_indicator = input$contr_domain_grade,
                         mass_indicator = input$mass_domain_grade,
                         contr_threshold = input$contr_threshold_domain_grade/100,
                         mass_threshold = input$mass_threshold_domain_grade/100)$tab_rel
      )

    plotInput_domain_grade <- reactive({
      gp <- shiny_domain_grade(data,
                         selected_cycle =
                           input$selected_cycle_domain_grade,
                         contr_indicator = input$contr_domain_grade,
                         mass_indicator = input$mass_domain_grade,
                         contr_threshold = input$contr_threshold_domain_grade/100,
                         mass_threshold = input$mass_threshold_domain_grade/100)$asymmetric_plot

    })

    output$ae_domain_grade_biplot <- renderPlot({print(plotInput_domain_grade())})

    output$ae_domain_grade_contr_plot <- renderPlot(
      shiny_domain_grade(data,
                         selected_cycle =
                           input$selected_cycle_domain_grade,
                         contr_indicator = input$contr_domain_grade,
                         mass_indicator = input$mass_domain_grade,
                         contr_threshold = input$contr_threshold_domain_grade/100,
                         mass_threshold = input$mass_threshold_domain_grade/100)$contr_plot
    )

    output$downloadplot_domain_grade <- downloadHandler(
      filename = function() { paste('ca_domain_grade.pdf', sep='') },
      content = function(file) {
        ggsave(file, plot = plotInput_domain_grade(), device = "pdf",
                height = 7, width = 7)
      }
    )

  } else {
    shinyjs::js$disabletabD("DomainGrade")
  }

  if('ae_term' %in% colnames(data)){#If true enable, else disable
    shinyjs::js$enabletabT("Term")

    output$ae_term_table <- DT::renderDataTable(
      shiny_term(data,
                 selected_cycle = input$selected_cycle_term,
                 selected_domain = input$selected_domain_term,
                 selected_grade = input$selected_grade_term,
                 contr_indicator = input$contr_term,
                 mass_indicator = input$mass_term,
                 contr_threshold = input$contr_threshold_term/100,
                 mass_threshold = input$mass_threshold_term/100)$tab_rel
    )

    plotInput_term <- reactive({
      gp <- shiny_term(data,
                       selected_cycle = input$selected_cycle_term,
                       selected_domain = input$selected_domain_term,
                       selected_grade = input$selected_grade_term,
                       contr_indicator = input$contr_term,
                       mass_indicator = input$mass_term,
                       contr_threshold = input$contr_threshold_term/100,
                       mass_threshold = input$mass_threshold_term/100)$asymmetric_plot
    })

    output$ae_term_biplot  <- renderPlot({print(plotInput_term())})

    output$ae_term_contr_plot <- renderPlot(
      shiny_term(data,
                 selected_cycle = input$selected_cycle_term,
                 selected_domain = input$selected_domain_term,
                 selected_grade = input$selected_grade_term,
                 contr_indicator = input$contr_term,
                 mass_indicator = input$mass_term,
                 contr_threshold = input$contr_threshold_term/100,
                 mass_threshold = input$mass_threshold_term/100)$contr_plot
    )

    output$downloadplot_term <- downloadHandler(
      filename = function() { paste('ca_term.pdf', sep='') },
      content = function(file) {
        ggsave(file, plot = plotInput_term(), device = "pdf",
                height = 7, width = 7)
      }
    )

  } else {
    shinyjs::js$disabletabT("Term")
  }

  if('ae_term' %in% colnames(data) & 'ae_grade' %in% colnames(data)){#If true enable, else disable
    shinyjs::js$enabletabT("TermGrade")

    output$ae_term_grade_table <- DT::renderDataTable(
      shiny_term_grade(data,
                       selected_cycle = input$selected_cycle_term_grade,
                       selected_domain = input$selected_domain_term_grade,
                       contr_indicator = input$contr_term_grade,
                       mass_indicator = input$mass_term_grade,
                       contr_threshold = input$contr_threshold_term_grade/100,
                       mass_threshold = input$mass_threshold_term_grade/100)$tab_rel
    )

    plotInput_term_grade <- reactive({gp <- shiny_term_grade(data,
                                                  selected_cycle = input$selected_cycle_term_grade,
                                                  selected_domain = input$selected_domain_term_grade,
                                                  contr_indicator = input$contr_term_grade,
                                                  mass_indicator = input$mass_term_grade,
                                                  contr_threshold = input$contr_threshold_term_grade/100,
                                                  mass_threshold = input$mass_threshold_term_grade/100)$asymmetric_plot
    })

    output$ae_term_grade_biplot <- renderPlot({print(plotInput_term_grade())})

    output$ae_term_grade_contr_plot <- renderPlot(
      shiny_term_grade(data,
                       selected_cycle = input$selected_cycle_term_grade,
                       selected_domain = input$selected_domain_term_grade,
                       contr_indicator = input$contr_term_grade,
                       mass_indicator = input$mass_term_grade,
                       contr_threshold = input$contr_threshold_term_grade/100,
                       mass_threshold = input$mass_threshold_term_grade/100)$contr_plot
    )

    output$downloadplot_term_grade <- downloadHandler(
      filename = function() { paste('ca_term_grade.pdf', sep='') },
      content = function(file) {
        ggsave(file, plot = plotInput_term_grade(), device = "pdf",
                height = 7, width = 7)
      }
    )

  } else {
    shinyjs::js$disabletabT("TermGrade")
  }
}

shinyApp(ui = ui(data), server = server)


