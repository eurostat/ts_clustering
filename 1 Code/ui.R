ui <- fluidPage(
  
  # App title ----
  titlePanel(h1("BRIEF ANALYSIS OF ASYLUMS", 
               style="text-align: center;",
               id = "titlepanel"), 
             windowTitle = "Brief Analysis of Asylums"),
  tags$style(HTML("#titlepanel{background-color: #5d91ff;font-weight: bold;}")),
  setBackgroundColor(
    color = "#E0FFFF",
    gradient = c("linear", "radial"),
    direction = c("bottom", "top", "right", "left"),
    shinydashboard = FALSE
  ),

  
  br(),
  ##############################################################################
  #       MAIN PANEL---
  ##############################################################################
    mainPanel(
      width = 12,
      align = "center",
      
      # first note 
      br(),
      
      fluidRow(
        column(12,
               style = "background-color:#E6EEFF;",
               p(syrtext1, style="text-align: justify;")
        )
      ),
      
      br(),
      
      # BARPLOTS UI ------------------------------------------------------------
      fluidRow(
        column(10,
               style = "background-color:#E6EEFF;",
               tabsetPanel(type = "tabs",
                           tabPanel("RR - First istance", 
                                    plotlyOutput(outputId = "bar1", height = 600),
                                    sliderInput(inputId = "threshold1",
                                                label = "Number of decisions threshold",
                                                min = 1,
                                                max = 3,
                                                value= c(1,3)),
                                    htmlOutput("rrfirst", style="text-align: justify;font-weight: bold;"),
                                    p("In the bar plot we represent the recognition rate (RR) defined
                                      as the number of positive decisions over the total decisions
                                      on asylum. The ratio in this case refers to first istance decision of a specific destination country and
                                      it is calculated for each provenience country of the asylum seekers.", style="text-align: justify;"),
                                    p("The user can select the year, the country, the number of decisions threshold for
                                      a provenience country to appear, the sex and the age of the applicants.", style="text-align: justify;"), 
                                    p("Data comes from eurostat database migr_asydcfsta.", style="text-align: justify;")
                           ),
                           tabPanel("RR - Definitive decision", 
                                    plotlyOutput(outputId = "bar2", height = 600),
                                    sliderInput(inputId = "threshold2",
                                                label = "Number of decisions threshold",
                                                min = 1,
                                                max = 3,
                                                value= c(1,3)),
                                    htmlOutput("rrdef", style="text-align: justify;font-weight: bold;"),
                                    p("In the bar plot we represent the recognition rate (RR) defined
                                      as the number of positive decisions over the total decisions
                                      on asylum. The ratio in this case refers to definitive decision of a specific destination country and
                                      it is calculated for each provenience country of the asylum seekers.", style="text-align: justify;"),
                                    p("The user can select the year, the country, the number of decisions threshold for
                                      a provenience country to appear, the sex and the age of the applicants.", style="text-align: justify;"), 
                                    p("Data comes from eurostat database migr_asydcfina.", style="text-align: justify;")
                           )
               )
        ),
        column(2,
               style = "background-color:#E6EEFF;",
               sliderInput(inputId = "time1",
                           label = "Time frame",
                           min = 2008,
                           max = 2020,
                           value = c(2008, 2020),
                           sep = ""),
               selectInput(inputId = "geo1",
                           label = "Select country",
                           choices = unique(asylum$geo)[order(unique(asylum$geo))],
                           selected = "Belgium"),
               selectInput(inputId = "sex1",
                           label = "Sex",
                           choices = unique(asylum$sex),
                           selected = "T"),
               selectInput(inputId = "age1",
                           label = "Age",
                           choices = unique(asylum$age),
                           selected = "TOTAL"))
      ),
      
      br(),
      
      fluidRow(
        column(12,
               style = "background-color:#E6EEFF;",
               p(syrtext2, style="text-align: justify;")
        )
      ),
      
      br(),
      
      # GEO PIECHARTS ----------------------------------------------------------
      fluidRow(
        column(10,
               style = "background-color:#E6EEFF;",
               tabsetPanel(type = "tabs",
                           tabPanel("Applications", 
                                    plotlyOutput(outputId = "applpie"),
                                    sliderInput(inputId = "threshold3",
                                                label = "Number of applications threshold",
                                                min = 1,
                                                max = 3,
                                                value= c(1,3)),
                                    htmlOutput("appltext", style="text-align: justify;font-weight: bold;"),
                                    p("In this pie chart we represent the number of applications a country recieved 
                                      in a determinate time window with informations about the birth country of applicants.", style="text-align: justify;"),
                                    p("The user can select the time window, the country, the number of applications threshold for
                                      a provenience country to appear or be grouped in 'Other', the sex and the age of the applicants.", style="text-align: justify;"), 
                                    p("Data comes from eurostat database migr_asyappctza.", style="text-align: justify;")),
                           
                           tabPanel("Approval by first istance",
                                    plotlyOutput(outputId = "firstpie"),
                                    sliderInput(inputId = "threshold4",
                                                label = "Number of decisions threshold",
                                                min = 1,
                                                max = 3,
                                                value= c(1,3)),
                                    htmlOutput("firsttext", style="text-align: justify;font-weight: bold;"),
                                    p("In this pie chart we represent the number of positive decisions by first istance on asylum a country granted 
                                      in a determinate time window with informations about the birth country of applicants.", style="text-align: justify;"),
                                    p("The user can select the time window, the country, the number of applications threshold for
                                      a provenience country to appear or be grouped in 'Other', the sex and the age of the applicants.", style="text-align: justify;"),
                                    p("Data comes from eurostat database migr_asydcfsta.", style="text-align: justify;")),
                           tabPanel("Approval by definitive decision",
                                    plotlyOutput(outputId = "defpie"),
                                    sliderInput(inputId = "threshold5",
                                                label = "Number of decisions threshold",
                                                min = 1,
                                                max = 3,
                                                value= c(1,3)),
                                    htmlOutput("deftext", style="text-align: justify;font-weight: bold;"),
                                    p("In this pie chart we represent the number of positive decisions by definitive decision on asylum a country granted 
                                      in a determinate time window with informations about the birth country of applicants.", style="text-align: justify;"),
                                    p("The user can select the time window, the country, the number of applications threshold for
                                      a provenience country to appear or be grouped in 'Other', the sex and the age of the applicants.", style="text-align: justify;"),
                                    p("Data comes from eurostat database migr_asydcfina.", style="text-align: justify;"))
               )
        ),
        column(2, 
               style = "background-color:#E6EEFF;",
               sliderInput(inputId = "time2",
                           label = "Time window you want to consider",
                           min = 2008,
                           max = 2020,
                           value = c(2008,2020),
                           sep = ""),
               selectInput(inputId = "geo2",
                           label = "Select country",
                           choices = unique(asylum$geo)[order(unique(asylum$geo))],
                           selected = "Belgium"),
               selectInput(inputId = "sex2",
                           label = "Sex",
                           choices = unique(asylum$sex),
                           selected = "T"),
               selectInput(inputId = "age2",
                           label = "Age",
                           choices = unique(asylum$age),
                           selected = "TOTAL")
        )
      ),
      
      br(),
      
      fluidRow(
        column(12,
               style = "background-color:#E6EEFF;",
               p(syrtext3, style="text-align: justify;")
        )
      ),
      
      br(),
      
      # CITIZEN PIECHARTS-------------------------------------------------------
      fluidRow(
        column(10,
               style = "background-color:#E6EEFF;",
               tabsetPanel(type = "tabs",
                           tabPanel("Applications", 
                                    plotlyOutput(outputId = "applpie_cit"),
                                    sliderInput(inputId = "threshold6",
                                                label = "Number of applications threshold",
                                                min = 1,
                                                max = 3,
                                                value= c(1,3)),
                                    htmlOutput("appltext_cit", style="text-align: justify;font-weight: bold;"),
                                    p("In this pie chart we represent the number of asylum applicants from a certain country 
                                      in a determinate time window with informations about the countries to which they sent the application.", style="text-align: justify;"),
                                    p("The user can select the time window, the country, the number of applications threshold for
                                      a destination country to appear or be grouped in 'Other', the sex and the age of the applicants.", style="text-align: justify;"),
                                    p("Data comes from eurostat database migr_asyappctza.", style="text-align: justify;")),
                           
                           tabPanel("Approval by first istance",
                                    plotlyOutput(outputId = "firstpie_cit"),
                                    sliderInput(inputId = "threshold7",
                                                label = "Number of decisions threshold",
                                                min = 1,
                                                max = 3,
                                                value= c(1,3)),
                                    htmlOutput("firsttext_cit", style="text-align: justify;font-weight: bold;"),
                                    p("In this pie chart we represent the number of positive decisions by first istance on asylum applicants from 
                                    a certain coyntry recieved in a determinate time window with informations about the destination countries.", style="text-align: justify;"),
                                    p("The user can select the time window, the country, the number of applications threshold for
                                      a destination country to appear or be grouped in 'Other', the sex and the age of the applicants.", style="text-align: justify;"),
                                    p("Data comes from eurostat database migr_asydcfsta.", style="text-align: justify;")),
                           tabPanel("Approval by definitive decision",
                                    plotlyOutput(outputId = "defpie_cit"),
                                    sliderInput(inputId = "threshold8",
                                                label = "Number of decisions threshold",
                                                min = 1,
                                                max = 3,
                                                value= c(1,3)),
                                    htmlOutput("deftext_cit", style="text-align: justify;font-weight: bold;"),
                                    p("In this pie chart we represent the number of positive decisions by definitive decision on asylum applicants from 
                                    a certain coyntry recieved in a determinate time window with informations about the destination countries.", style="text-align: justify;"),
                                    p("The user can select the time window, the country, the number of applications threshold for
                                      a destination country to appear or be grouped in 'Other', the sex and the age of the applicants.", style="text-align: justify;"),
                                    p("Data comes from eurostat database migr_asydcfina.", style="text-align: justify;"))
               )
        ),
        column(2, 
               style = "background-color:#E6EEFF;",
               sliderInput(inputId = "time3",
                           label = "Time window you want to consider",
                           min = 2008,
                           max = 2020,
                           value = c(2008,2020),
                           sep = ""),
               selectInput(inputId = "ctz",
                           label = "Select country",
                           choices = unique(asylum$citizen)[order(unique(asylum$citizen))],
                           selected = "Syria"),
               selectInput(inputId = "sex3",
                           label = "Sex",
                           choices = unique(asylum$sex),
                           selected = "T"),
               selectInput(inputId = "age3",
                           label = "Age",
                           choices = unique(asylum$age),
                           selected = "TOTAL")
        )
      ),
      br(),
      
      fluidRow(
        column(12,
               style = "background-color:#E6EEFF;",
               p(syrtext4, style="text-align: justify;")
        )
      ),
      
      br(),
      br(),
      br(),
      br(),
      br()
    )
)
    


