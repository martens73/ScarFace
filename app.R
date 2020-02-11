library(shiny)
library(shinydashboard)
library(shinyFiles)
library(seacarb)
library(readr)
library(dplyr)

##### UI #####
ui <- dashboardPage(
    # dashboardHeader(title = img(src="logo.png", height = 25)),
    dashboardHeader(title = "ScarFace"),
    dashboardSidebar(
        sidebarMenu(id = "tabs",
            menuItem("About", tabName = "about", icon = icon("info")),
            menuItem("Bjerrum plot", tabName = "bjerrum", icon = icon("chart-area")),
            menuItem("Working Directory", tabname = "wd", icon = icon("cog"),
                     menuSubItem("Directory to store data", tabName = "wd_sub"),
                     shinyDirButton("dir", "Choose", "Upload", icon=icon("folder-plus")),
                     verbatimTextOutput("dir", placeholder = TRUE)),
            menuItem("Carbonate chemistry", tabName = "carb", icon = icon("flask"),
                     menuSubItem("manual input", tabName = "man", icon = icon("hand-pointer")),
                     menuSubItem("batch input", tabName = "batch", icon = icon("folder-open"))),
            menuItem("Error propagation", tabName = "error", icon = icon("arrow-circle-right")),
            menuItem("Citation", tabName = "cite", icon = icon("pen-alt"))
        )
    ),
    dashboardBody(
        tabItems(
            # Tab content 'About'
            tabItem(tabName = "about",
                    br(),
                    br(),
                    br(),
                    # div(img(src = "logo.png", width = 200), style="text-align: center;"),
                    p("ScarFace", align = "center", style="color: #7da2d1; font-size: 48px"),
                    p(em("version 1.0.0"), align="center"),
                    p("This application is designed to calculate the carbonate system chemistry of seawater based on the 'seacarb' package using a graphical user interface", align="center"),
                    p("Its name stands for ",tags$b("s"),"ea",tags$b("car"),"b calculations with R Shiny user inter",tags$b("face"),".",align="center"),
                    p("'ScarFace' was written in 'R' and embedded in an interactive web app using the 'Shiny' package. Shiny combines the computational power of R with the interactivity of the modern web.", align="center"),
                    p("Shiny uses reactive elements, i.e. the user interacts via the 'ui' (user interface) with the actual R code running in the background called 'server'. Whenever the user make input changes, this will automatically affect all dependent elements (e.g. plots or data tables).", align="center"),
                    p("Even though the code can be accessed and modified, there is no need for the user to dig into R programming. So 'ScarFace' is extremely user-friendly and makes the usage of 'seacarb' work like a charm.", align="center"),
                    p("The Shiny package as a whole is distributed under GPL-3 (GNU GENERAL PUBLIC LICENSE version 3). R as a package is licensed under GPL-2 and GPL-3.", align="center"),
                    br(),
                    br(),
                    p("Authors: Markus Raitzsch and Jean-Pierre Gattuso", align="center"),
                    p("February 2020", align="center")
            ),
            
            # Tab content 'Bjerrum'
            tabItem(tabName = "bjerrum",
                    fluidRow(
                        box(width = 3, height = 650, solidHeader = TRUE, title="Define parameters using the slider or directly", collapsible=T,
                            uiOutput("T"),
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 25%",
                                    numericInput("T", value=25, label=""))),
                            br(),
                            uiOutput("S"),
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 25%",
                                    numericInput("S", value=35, label=""))),
                            br(),
                            uiOutput("P"),
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 25%",
                                    numericInput("P", value=0, label="")))
                        ),
                        box(width = 6, height = 650, solidHeader = TRUE, title="Bjerrum plot", collapsible=T,
                            plotOutput(outputId = "plot1", width = "100%", height = "600px")
                        ),
                        box(width = 3, height = 650, solidHeader = TRUE, title="Dissociation constants", collapsible=T,
                            textOutput(outputId = "pK1"),
                            br(),
                            textOutput(outputId = "pK2"),
                            br(),
                            textOutput(outputId = "info")
                        ))),
            
            # Tab content 'Manual'
            tabItem(tabName = "man",
                    fluidRow(
                        box(width = 3, height = 500, solidHeader = TRUE, title="Carbonate system parameters [INPUT]", collapsible=T,
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 90%",
                                    selectInput("pair", "Define pair of known carbonate system variables",
                                                c("pH and CO2" = "1",
                                                  "CO2 and HCO3" = "2",
                                                  "CO2 and CO3" = "3",
                                                  "CO2 and ALK" = "4",
                                                  "CO2 and DIC" = "5",
                                                  "pH and HCO3" = "6",
                                                  "pH and CO3" = "7",
                                                  "pH and ALK" = "8",
                                                  "pH and DIC" = "9",
                                                  "HCO3 and CO3" = "10",
                                                  "HCO3 and ALK" = "11",
                                                  "HCO3 and DIC" = "12",
                                                  "CO3 and ALK" = "13",
                                                  "CO3 and DIC" = "14",
                                                  "ALK and DIC" = "15",
                                                  "pCO2 and pH" = "21",
                                                  "pCO2 and HCO3" = "22",
                                                  "pCO2 and CO3" = "23",
                                                  "pCO2 and ALK" = "24",
                                                  "pCO2 and DIC" = "25")))),
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.5%",
                                    numericInput("first", value="", label="First variable")),
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.5%",
                                    numericInput("second", value="", label="Second variable"))),
                            hr(),
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 90%",
                                    numericInput("tempc", value="25", step=0.1, label="Temperature (°C)")),
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 90%",
                                    numericInput("salc", min=0, step=0.1, value="35", label="Salinity (psu scale)")),
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 90%",
                                    numericInput("presc", min=0, value="0", label="Pressure (bar) | P=0 at surface")))
                            ),
                        box(width = 3, height = 500, solidHeader = TRUE, title="Additional choices [optional]", collapsible=T,
                            p("Leave silicate and phosphate concentrations at zero if unknown"),
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.5%",
                                    numericInput("sil", min=0, value="0", label="Silicate (µmol/kg)")),
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.5%",
                                    numericInput("pho", min=0, value="0", label="Phosphate (µmol/kg)"))),
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 90%",
                            selectInput("k1k2", "Constants for K1 and K2",
                                        c("Auto" = "x",
                                          "Lueker et al. (2000)" = "l",
                                          "Millero et al. (2006)" = "m06",
                                          "Millero (2010)" = "m10",
                                          "Waters et al. (2014)" = "w14",
                                          "Roy et al. (1993)" = "r")))),
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 90%",
                                    selectInput("kf", "Stability constant of hydrogen fluoride",
                                                c("Auto" = "x",
                                                  "Perez and Fraga (1987)" = "pf",
                                                  "Dickson and Riley (1979 in Dickson and Goyet, 1994)" = "dg")))),
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 90%",
                                    selectInput("b", "Total Boron concentration",
                                                c("Lee et al. (2010) [default]" = "l10",
                                                  "Uppstrom (1974)" = "u74")))),
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 90%",
                                    selectInput("pHscale", "pH scale",
                                                c("Total scale [default]" = "T",
                                                  "Free scale" = "F",
                                                  "Seawater scale" = "SWS"))))
                            )),
                    fluidRow(
                        box(width = 12, solidHeader = TRUE, title="Carbonate system parameters [OUTPUT]", collapsible=T,
                            div(style = 'overflow-x: auto', tableOutput("hot")),
                            hr(),
                            actionButton('collect', label = 'Collect current data')
                            )),
                    fluidRow(
                        box(width = 12, solidHeader = TRUE, title="Collected output data", collapsible=T,
                            div(style = 'overflow-x: auto', tableOutput('CollectedData')),
                            hr(),
                            shinyUI(bootstrapPage(
                                shinySaveButton('coll','Save collected data as csv table','Save as "filename"_coll', filetype=list(csv='csv'), icon=icon("save"))
                            )),
                            actionButton('delete', label = 'Delete collected data',
                                         style="color: #000; background-color: #F0BD18; border-color: #E0E0E0"))
                        )),
            
            # Tab content 'Batch'
            tabItem(tabName = "batch",
                    fluidRow(
                        box(width = 12, solidHeader = TRUE, title="Table with input data", collapsible=T,
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 25%",
                                    fileInput("batch", "Choose CSV file to upload",
                                              multiple = FALSE,
                                              accept = c("text/csv",
                                                         "text/comma-separated-values,text/plain",
                                                         ".csv"))
                                    ),
                            div(style="float: left; display:inline-block; margin-left: 15px; width: 15%",
                                selectInput("sep", "Choose separator type",
                                        c("Comma-separated" = ",",
                                          "Semicolon-separated" = ";",
                                          "Tab-separated" = "\tab")))),
                            div(style = 'overflow-x: auto', tableOutput(outputId = 'table1'))
                        )),
                    fluidRow(
                        box(width = 3, height = 500, solidheader = TRUE, title = "Carbonate system parameters [INPUT]", collapsible = T,
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 90%",
                                    selectInput("pairb", "Define pair of known carbonate system variables",
                                                c("pH and CO2" = "1",
                                                  "CO2 and HCO3" = "2",
                                                  "CO2 and CO3" = "3",
                                                  "CO2 and ALK" = "4",
                                                  "CO2 and DIC" = "5",
                                                  "pH and HCO3" = "6",
                                                  "pH and CO3" = "7",
                                                  "pH and ALK" = "8",
                                                  "pH and DIC" = "9",
                                                  "HCO3 and CO3" = "10",
                                                  "HCO3 and ALK" = "11",
                                                  "HCO3 and DIC" = "12",
                                                  "CO3 and ALK" = "13",
                                                  "CO3 and DIC" = "14",
                                                  "ALK and DIC" = "15",
                                                  "pCO2 and pH" = "21",
                                                  "pCO2 and HCO3" = "22",
                                                  "pCO2 and CO3" = "23",
                                                  "pCO2 and ALK" = "24",
                                                  "pCO2 and DIC" = "25")))),
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.5%",
                                    selectInput("firstb", "Select Variable 1", choices = NULL)),
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.5%",
                                    selectInput("secondb", "Select Variable 2", choices = NULL))),
                            hr(),
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 90%",
                                    selectInput("tempb", "Select Temperature", choices = NULL)),
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 90%",
                                    selectInput("salb", "Select Salinity", choices = NULL))),
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.5%",
                                    selectInput("presb", "Select Pressure or Depth", choices = NULL)),
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.5%",
                                    radioButtons("PorD", "What is given?", choices = c("Pressure" = 1, "Depth" = 9.9548), inline = TRUE)))
                            ),
                        box(width = 3, height = 500, solidHeader = TRUE, title="Additional choices [optional]", collapsible=T,
                            p("Leave silicate and phosphate concentrations at zero if unknown"),
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.5%",
                                    numericInput("silb", min=0, value="0", label="Silicate (µmol/kg)")),
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.5%",
                                    numericInput("phob", min=0, value="0", label="Phosphate (µmol/kg)"))),
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 90%",
                                    selectInput("k1k2b", "Constants for K1 and K2",
                                                c("Auto" = "x",
                                                  "Lueker et al. (2000)" = "l",
                                                  "Millero et al. (2006)" = "m06",
                                                  "Millero (2010)" = "m10",
                                                  "Waters et al. (2014)" = "w14",
                                                  "Roy et al. (1993)" = "r")))),
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 90%",
                                    selectInput("kfb", "Stability constant of hydrogen fluoride",
                                                c("Auto" = "x",
                                                  "Perez and Fraga (1987)" = "pf",
                                                  "Dickson and Riley (1979 in Dickson and Goyet, 1994)" = "dg")))),
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 90%",
                                    selectInput("bb", "Total Boron concentration",
                                                c("Lee et al. (2010) [default]" = "l10",
                                                  "Uppstrom (1974)" = "u74")))),
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 90%",
                                    selectInput("pHscaleb", "pH scale",
                                                c("Total scale [default]" = "T",
                                                  "Free scale" = "F",
                                                  "Seawater scale" = "SWS"))))
                            ),
                        box(width = 3, height = 500, solidheader = TRUE, title = "Include additional columns [optional]", collapsible = T,
                            checkboxGroupInput("incol",
                                               "Select columns from source table to be included in output table"))
                        ),
                    fluidRow(
                        box(width = 12, solidHeader = TRUE, title="Carbonate system parameters [OUTPUT]", collapsible=T,
                            div(style = 'overflow-x: auto', tableOutput('table2')),
                            hr(),
                            shinyUI(bootstrapPage(
                                shinySaveButton('batch_end','Save batch data as csv table','Save as "filename"_calc', filetype=list(csv='csv'), icon=icon("save"))
                                )),
                            actionButton('gotoerr', label = 'OR continue here for error propagation',
                                         style="color: #000; background-color: #F0BD18; border-color: #E0E0E0")
                            ))
                    ),
            
            # Tab content 'Error propagation'
            tabItem(tabName = "error",
                    fluidRow(
                        box(width = 3, height = 650, solidHeader = TRUE, title="Define uncertainties of input parameters", collapsible=T,
                            p("either from the data source or by entering manually", align="left"),
                            p("NOTE: 'Manual' will override 'Data Source'", align="left"),
                            hr(),
                            p(tags$b("Variable 1 uncertainty:"), align="left"),
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.5%",
                                    selectInput("firste", "Data source", choices = NULL)),
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.5%",
                                    numericInput("firstem", min=0, value="", label="Manual"))),
                            p(tags$b("Variable 2 uncertainty:"), align="left"),
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.5%",
                                    selectInput("seconde", "Data source", choices = NULL)),
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.5%",
                                    numericInput("secondm", min=0, value="", label="Manual"))),
                            p(tags$b("Temperature uncertainty:"), align="left"),
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.5%",
                                    selectInput("tempe", "Data source", choices = NULL)),
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.5%",
                                    numericInput("tempm", min=0, step=0.1, value="", label="Manual"))),
                            p(tags$b("Salinity uncertainty:"), align="left"),
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.5%",
                                    selectInput("sale", "Data source", choices = NULL)),
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.5%",
                                    numericInput("salm", min=0, step=0.1, value="", label="Manual"))),
                        ),
                        box(width = 3, height = 650, solidHeader = TRUE, title="Define additional uncertainties [optional]", collapsible=T,
                            p("Leave silicate and phosphate uncertainties at zero if unknown"),
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.5%",
                                    numericInput("Pte", min=0, value="0", label="Phosphate uncertainty (µmol/kg)")),
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.5%",
                                    numericInput("Site", min=0, value="0", label="Silicate uncertainty (µmol/kg)"))),
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 90%",
                                    selectInput("pKe", "Uncertainties in equilibrium constants",
                                                choices = c(
                                                    "Default [see SeaCarb package for details]" = "c(0.002, 0.0075, 0.015, 0.01, 0.01, 0.02, 0.02)",
                                                    "Neglect errors of equilibrium constants" = "0")))),
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 90%",
                                    numericInput("Bte", min=0, value="0.02", step=0.01, label="Total Boron uncertainty [default 0.02 = 2% error]"))),
                            hr(),
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 90%",
                                    selectInput("meth", "Error propagation method",
                                                choices = c(
                                                    "Gaussian [default]" = "ga",
                                                    "Method of Moments" = "mo",
                                                    "Monte Carlo" = "mc")))),
                            p("The following inputs are only applied if 'Method of Moments' or 'Monte Carlo' are selected.
                              Correlation coefficient is the R-squared between the pair of chosen CS parameters, and repetitions is the number of Monte Carlo simulations"),
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.5%",
                                    numericInput("corr", step=0.1, min=-1, max=1, value="0.0", label="Correlation coefficient")),
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.5%",
                                    numericInput("reps", step=1000, min=1, value="10000", label="Monte Carlo repetitions"))),
                        )),
                    fluidRow(
                        box(width = 12, solidHeader = TRUE, title="Standard uncertainties of carbonate system parameters [OUTPUT]", collapsible=T,
                            div(style = 'overflow-x: auto', tableOutput('table3')),
                            hr(),
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px",
                                    checkboxInput(inputId="comb", label = "Should input data be included?", value = FALSE, width = NULL)),
                                div(style="float: left; display:inline-block; margin-left: 15px",
                                    shinyUI(bootstrapPage(
                                        shinySaveButton('batch_err','Save uncertainties as csv table','Save as "filename"_err', filetype=list(csv='csv'), icon=icon("save"))))))
                            ))
                    ),
            
            # Tab content 'Citation'
            tabItem(tabName = "cite",
                    br(),
                    br(),
                    br(),
                    # div(img(src = "logo.png", width = 200), style="text-align: center;"),
                    p("ScarFace", align = "center", style="color: #7da2d1; font-size: 48px"),
                    p(em("version 1.0"), align="center"),
                    p("When you use ScarFace for your published research, please cite it as follows:", align="center"),
                      br(),
                      p("Raitzsch, M. and Gattuso, J.-P. (2020): ScarFace - seacarb calculations with a Shiny user interface. Available from: https://github.com/martens73/ScarFace.", align="center"),
                      br(),
                      p("but also please give due credit to the creators of the package 'seacarb':", align="center"),
                      br(),
                      p("Gattuso J.-P., Epitalon J.-M., Lavigne H. & Orr J., 2019. seacarb: seawater carbonate chemistry. R package version 3.2.12. http://CRAN.R-project.org/package=seacarb.", align="center")
            )
        )
    )
)

##### Server #####
server <- function(input, output, session) {
    #### Bjerrum plot ####
    # Render plot #
    output$plot1 <- renderPlot({
        bjerrum(K1=K1(T=25,S=35,P=0), K2=K2(T=25,S=35,P=0), K3=NULL, phmin=2, phmax=12, by=0.1, conc=1,
                type="l", col="black", ylab="Relative concentration", lwd=2)
        bjerrum(K1=K1(T=input$temp,S=input$sal,P=input$pres), K2=K2(T=input$temp,S=input$sal,P=input$pres), K3=NULL, phmin=2, phmax=12, by=0.1, conc=1,
                type="l", col="red", add=TRUE)
        legend("right",lty=1:3,legend=c(expression(CO[2]),expression(HCO[3]^"-"),
                                        expression(CO[3]^"2-")))
        legend("left",lty=1, col="black", legend="T=25, S=35, P=0\" <br>npK1=5.847, pK2=8.966")
        })
    
    # Slider and numeric inputs #
    output$T <- renderUI({
        sliderInput('temp', 'Temperature (°C)',
                    min = 0, max = 60, value=input$T, step=0.5)
        })
    output$S <- renderUI({
        sliderInput('sal', 'Salinity (psu scale)',
                    min = 0, max = 50, value=input$S, step=0.5)
        })
    output$P <- renderUI({
        sliderInput('pres', 'Pressure (bar) | P=0 at surface',
                    min = 0, max = 1000, value=input$P, step=1)
        })
    
    # Text output of calculated K1 and K2 #
    output$pK1 <- renderText({
        K1 <- K1(S=input$sal, T=input$temp, P=input$pres, k1k2="x", pHscale="T", warn="y")
        K1[1]
        pK1 <- -log10(K1)
        pK1 <- format(pK1[1], digits = 4)
        paste("First dissociation constant pK1 =", pK1)
        })
    output$pK2 <- renderText({
        K2 <- K2(S=input$sal, T=input$temp, P=input$pres, k1k2="x", pHscale="T", warn="y")
        K2[1]
        pK2 <- -log10(K2)
        pK2 <- format(pK2[1], digits = 4)
        paste("Second dissociation constant pK2 =", pK2)
        })
    
    #### Set working directory ####
    shinyDirChoose(
        input,
        'dir',
        # roots = c(home = '~'),
        roots = c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()()),
        filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
    )
    
    global <- reactiveValues(datapath = getwd())
    
    dir <- reactive(input$dir)
    
    wd <- output$dir <- renderText({
        global$datapath
    })
    
    observeEvent(ignoreNULL = TRUE,
                 eventExpr = {
                     input$dir
                 },
                 handlerExpr = {
                     if (!"path" %in% names(dir()))
                         return()
                     home <- normalizePath("~")
                     global$datapath <-
                         file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
                 })
    
    #### Carbonate system parameters (manual) ####
    # Carbonate system calculation based on parameter inputs #
    carb_man <- reactive({
        if (input$pair == "2" || input$pair == "3" || input$pair == "4" || input$pair == "5" || input$pair == "10" || input$pair == "11" || input$pair == "12" || input$pair == "13" || input$pair == "14" || input$pair == "15") {
            carb(flag=as.numeric(input$pair), var1=input$first/1000000, var2=input$second/1000000, S=input$salc, T=input$tempc, Patm=1, P=input$presc, Pt=input$pho/1000000, Sit=input$sil/1000000,
                 k1k2=input$k1k2, kf=input$kf, ks="d", pHscale=input$pHscale, b=input$b, gas="potential",
                 warn="y", eos="eos80", long=1.e20, lat=1.e20)
        } else if (input$pair == "21") {
            carb(flag=as.numeric(input$pair), var1=input$first, var2=input$second, S=input$salc, T=input$tempc, Patm=1, P=input$presc, Pt=input$pho/1000000, Sit=input$sil/1000000,
                 k1k2=input$k1k2, kf=input$kf, ks="d", input$pHscale, b=input$b, gas="potential",
                 warn="y", eos="eos80", long=1.e20, lat=1.e20)
        } else {
            carb(flag=as.numeric(input$pair), var1=input$first, var2=input$second/1000000, S=input$salc, T=input$tempc, Patm=1, P=input$presc, Pt=input$pho/1000000, Sit=input$sil/1000000,
                 k1k2=input$k1k2, kf=input$kf, ks="d", input$pHscale, b=input$b, gas="potential",
                 warn="y", eos="eos80", long=1.e20, lat=1.e20)
            }
        })
    
    # Render the output table #
    output$hot <- renderTable({
        carb_man() %>%
            mutate(HCO3 = HCO3*1000000) %>%
            mutate(CO3 = CO3*1000000) %>%
            mutate(CO2 = CO2*1000000) %>%
            mutate(DIC = DIC*1000000) %>%
            mutate(ALK = ALK*1000000)
        }, align='c', digits=2)
    
    # Collect current data and render in extra table #    
        values <- reactiveValues(df = data.frame())

        newEntry <- observe({
            if(input$collect > 0) {
                values$df <- isolate(rbind(values$df, carb_man())) %>%
                    mutate(flag = ifelse(row_number()==nrow(.), isolate(input$pair), flag)) %>%
                    mutate(CO2 = ifelse(row_number()==nrow(.), CO2*1000000, CO2)) %>%
                    mutate(HCO3 = ifelse(row_number()==nrow(.), HCO3*1000000, HCO3)) %>%
                    mutate(CO3 = ifelse(row_number()==nrow(.), CO3*1000000, CO3)) %>%
                    mutate(DIC = ifelse(row_number()==nrow(.), DIC*1000000, DIC)) %>%
                    mutate(ALK = ifelse(row_number()==nrow(.), ALK*1000000, ALK))
                }
            })

    output$CollectedData <- renderTable({values$df}, align='c', digits=2)
    
    delete <- observe({
        if(input$delete > 0) {
            values$df <- NULL
            }
        })
    
    # Save data as csv #
    observe({
        volumes <- c('Working Directory'=wd())
        shinyFileSave(input,'coll', roots=volumes)
        fileinfo <- parseSavePath(volumes, input$coll)
        if (nrow(fileinfo) > 0) {
            write_csv(values$df, fileinfo$datapath, append = FALSE)
            }
        })
    
    #### Carbonate system parameters (batch) ####
    # Upload data as csv #
    tbl1 <- reactive({
        inFile <- input$batch
        req(inFile)
        tbl1 <- read_delim(inFile$datapath, delim = input$sep, col_names = TRUE, col_types = NULL, na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, progress = show_progress(), skip_empty_rows = TRUE)
        })

    # Render uploaded table #
    output$table1 <- renderTable({
        tbl1()
        }, align='c', digits=2)
    
    # Read column names and render for dropdown menu #
    observeEvent(tbl1(), {
        updateSelectInput(session, "firstb", choices = names(tbl1()))
        updateSelectInput(session, "secondb", choices = names(tbl1()))
        updateSelectInput(session, "tempb", choices = names(tbl1()))
        updateSelectInput(session, "salb", choices = names(tbl1()))
        updateSelectInput(session, "presb", choices = names(tbl1()))
    })
    
    # Read column names and create checkbox list #
    observe({
        req(input$batch)
        dnames <- names(tbl1())
        inc_options <- list()
        inc_options[dnames] <- dnames
        updateCheckboxGroupInput(session, "incol",
                                 label = "Select original columns to be included in output table",
                                 choices = inc_options,
                                 selected = "")
        })
    
    # Carbonate system calculation based on parameter inputs #
    carb_batch <- reactive({
        if (input$pairb == "2" || input$pairb == "3" || input$pairb == "4" || input$pairb == "5" || input$pairb == "10" || input$pairb == "11" || input$pairb == "12" || input$pairb == "13" || input$pairb == "14" || input$pairb == "15") {
            carb(flag=as.numeric(input$pairb), var1=tbl1()[[input$firstb]]/1000000, var2=tbl1()[[input$secondb]]/1000000, S=tbl1()[[input$salb]], T=tbl1()[[input$tempb]], Patm=1, P=tbl1()[[input$presb]]/as.numeric(input$PorD), Pt=input$phob/1000000, Sit=input$silb/1000000,
                 k1k2=input$k1k2b, kf=input$kfb, ks="d", pHscale=input$pHscaleb, b=input$bb, gas="potential",
                 warn="y", eos="eos80", long=1.e20, lat=1.e20)
        } else if (input$pairb == "21") {
            carb(flag=as.numeric(input$pairb), tbl1()[[input$firstb]], var2=tbl1()[[input$secondb]], S=tbl1()[[input$salb]], T=tbl1()[[input$tempb]], Patm=1, P=tbl1()[[input$presb]]/as.numeric(input$PorD), Pt=input$phob/1000000, Sit=input$silb/1000000,
                 k1k2=input$k1k2b, kf=input$kfb, ks="d", input$pHscaleb, b=input$bb, gas="potential",
                 warn="y", eos="eos80", long=1.e20, lat=1.e20)
        } else {
            carb(flag=as.numeric(input$pairb), var1=tbl1()[[input$firstb]], var2=tbl1()[[input$secondb]]/1000000, S=tbl1()[[input$salb]], T=tbl1()[[input$tempb]], Patm=1, P=tbl1()[[input$presb]]/as.numeric(input$PorD), Pt=input$phob/1000000, Sit=input$silb/1000000,
                 k1k2=input$k1k2b, kf=input$kfb, ks="d", input$pHscaleb, b=input$bb, gas="potential",
                 warn="y", eos="eos80", long=1.e20, lat=1.e20)
            }
        })
    
    # Render output table and (if needed) combine with selected columns from uploaded table #
    dat <- reactive({
        if(is.null(input$batch))
            return()
        if (is.null(input$incol))
            return()
        tbl1()[, input$incol]
        })
    
    tbl_final <- reactive({
        carb_batch() %>%
            mutate(HCO3 = HCO3*1000000) %>%
            mutate(CO3 = CO3*1000000) %>%
            mutate(CO2 = CO2*1000000) %>%
            mutate(DIC = DIC*1000000) %>%
            mutate(ALK = ALK*1000000) %>%
            bind_cols(dat()) %>%
            select(input$incol, everything())
        })
    
    
    output$table2 <- renderTable({
        tbl_final()
        }, align='c', digits=2)
    
    # Save data as csv #
    observe({
        volumes <- c('Working Directory'=wd())
        shinyFileSave(input,'batch_end', roots=volumes)
        fileinfo <- parseSavePath(volumes, input$batch_end)
        if (nrow(fileinfo) > 0) {
            write_csv(isolate(tbl_final()), fileinfo$datapath, append = FALSE)
            }
        })
    
    #### Error propagation ####
    # On click switch to other tab #
    observeEvent(input$gotoerr, {
        newtab <- switch(input$tabs, "batch" = "error")
        updateTabItems(session, "tabs", newtab)
        })
    
    # Select errors from uploaded table or enter manually #
    observeEvent(tbl1(), {
        updateSelectInput(session, "firste", choices = names(tbl1()))
        })
    
    observeEvent(tbl1(), {
        updateSelectInput(session, "seconde", choices = names(tbl1()))
        })
    
    observeEvent(tbl1(), {
        updateSelectInput(session, "sale", choices = names(tbl1()))
        })
    
    observeEvent(tbl1(), {
        updateSelectInput(session, "tempe", choices = names(tbl1()))
        })
    
    evar1 <- reactive({
        req(input$batch)
        if (isTruthy(input$firstem))
            return(input$firstem)
        tbl1()[[input$firste]]
        })
    
    evar2 <- reactive({
        req(input$batch)
        if (isTruthy(input$secondm))
            return(input$secondm)
        tbl1()[[input$seconde]]
        })
    
    eS <- reactive({
        req(input$batch)
        if (isTruthy(input$salm))
            return(input$salm)
        tbl1()[[input$sale]]
        })
    
    eT <- reactive({
        req(input$batch)
        if (isTruthy(input$tempm))
            return(input$tempm)
        tbl1()[[input$tempe]]
        })
    
    epK <- reactive ({
        req(input$batch)
        if (input$pKe == "c(0.002, 0.0075, 0.015, 0.01, 0.01, 0.02, 0.02)")
            return(c(0.002, 0.0075, 0.015, 0.01, 0.01, 0.02, 0.02))
        return(0)
        })
    
    # Perform error propagation based on parameter inputs #
    carb_err <- reactive ({
        if (input$pairb == "2" || input$pairb == "3" || input$pairb == "4" || input$pairb == "5" || input$pairb == "10" || input$pairb == "11" || input$pairb == "12" || input$pairb == "13" || input$pairb == "14" || input$pairb == "15") {
            errors(flag=as.numeric(input$pairb), var1=tbl1()[[input$firstb]]/1000000, var2=tbl1()[[input$secondb]]/1000000, S=tbl1()[[input$salb]], T=tbl1()[[input$tempb]], Patm=1, P=tbl1()[[input$presb]]/as.numeric(input$PorD), Pt=input$phob/1000000, Sit=input$silb/1000000,
                   evar1=evar1()/1000000, evar2=evar2()/1000000, eS=eS(), eT=eT(), ePt=input$Pte/1000000, eSit=input$Site/1000000,
                   epK=epK(),
                   eBt=input$Bte, method = input$meth, r=input$corr, runs=input$reps,
                   k1k2='x', kf='x', ks="d", pHscale="T", b="u74", gas="potential",
                   warn="y", eos = "eos80", long = 1e+20, lat = 1e+20)
        } else if (input$pairb == "21") {
            errors(flag=as.numeric(input$pairb), var1=tbl1()[[input$firstb]], var2=tbl1()[[input$secondb]], S=tbl1()[[input$salb]], T=tbl1()[[input$tempb]], Patm=1, P=tbl1()[[input$presb]]/as.numeric(input$PorD), Pt=input$phob/1000000, Sit=input$silb/1000000,
                   evar1=evar1(), evar2=evar2(), eS=eS(), eT=eT(), ePt=input$Pte/1000000, eSit=input$Site/1000000,
                   epK=epK(),
                   eBt=input$Bte, method = input$meth, r=input$corr, runs=input$reps,
                   k1k2='x', kf='x', ks="d", pHscale="T", b="u74", gas="potential",
                   warn="y", eos = "eos80", long = 1e+20, lat = 1e+20)
        } else {
            errors(flag=as.numeric(input$pairb), var1=tbl1()[[input$firstb]], var2=tbl1()[[input$secondb]]/1000000, S=tbl1()[[input$salb]], T=tbl1()[[input$tempb]], Patm=1, P=tbl1()[[input$presb]]/as.numeric(input$PorD), Pt=input$phob/1000000, Sit=input$silb/1000000,
                   evar1=evar1(), evar2=evar2()/1000000, eS=eS(), eT=eT(), ePt=input$Pte/1000000, eSit=input$Site/1000000,
                   epK=epK(),
                   eBt=input$Bte, method = input$meth, r=input$corr, runs=input$reps,
                   k1k2='x', kf='x', ks="d", pHscale="T", b="u74", gas="potential",
                   warn="y", eos = "eos80", long = 1e+20, lat = 1e+20)
            }
        })
    
    # Render table with calculated errors #
    tbl_err <- reactive({
        carb_err() %>%
            mutate_at(vars(one_of("HCO3")), function(x) x*1000000) %>%
            mutate_at(vars(one_of("CO3")), function(x) x*1000000) %>%
            mutate_at(vars(one_of("CO2")), function(x) x*1000000) %>%
            mutate_at(vars(one_of("DIC")), function(x) x*1000000) %>%
            mutate_at(vars(one_of("ALK")), function(x) x*1000000) %>%
            rename_all(., ~ paste0(.,'_err'))
        })
    
    # On checked box include output table from 'Batch' #
    tbl5 <- reactive({
        if (isTruthy(input$comb))
            tbl_final() %>%
            bind_cols(tbl_err())
        else (tbl_err())
        })
    
    # Render output table #
    output$table3 <- renderTable({
        tbl5()
        }, align='c', digits=2)
    
    # Save data as csv #
    observe({
        volumes <- c('Working Directory'=wd())
        shinyFileSave(input,'batch_err', roots=volumes)
        fileinfo <- parseSavePath(volumes, input$batch_err)
        if (nrow(fileinfo) > 0) {
            write_csv(isolate(tbl5()), fileinfo$datapath, append = FALSE)
            }
        })
}

# Run the application #
shinyApp(ui = ui, server = server)
