library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(seacarb)
library(ggplot2)
library(plotly)
library(readr)
library(dplyr)
library(DT)

##### UI #####
ui <- dashboardPage(
    dashboardHeader(title = "ScarFace"),
    dashboardSidebar(
        sidebarMenu(id = "tabs",
            menuItem("About", tabName = "about", icon = icon("info")),
            menuItem("Bjerrum plot", tabName = "bjerrum", icon = icon("chart-area")),
            menuItem("Carbonate chemistry", tabName = "carb", icon = icon("flask"),
                     menuSubItem("manual input", tabName = "man", icon = icon("hand-pointer")),
                     menuSubItem("batch input", tabName = "batch", icon = icon("folder-open"))),
            menuItem("Error propagation", tabName = "error", icon = icon("arrow-circle-right")),
            menuItem("Citation", tabName = "cite", icon = icon("pen-alt"))
        )
    ),
    dashboardBody(
        setShadow(class = "box"),
        tabItems(
            # Tab content 'About'
            tabItem(tabName = "about",
                    br(),
                    br(),
                    br(),
                    p("ScarFace", align = "center", style="color: #7da2d1; font-size: 48px"),
                    p(em("web version 1.3.0"), align="center"),
                    p("This application is designed to calculate the carbonate system chemistry of seawater based on the 'seacarb' package using a graphical user interface", align="center"),
                    p("Its name stands for ",tags$b("s"),"ea",tags$b("car"),"b calculations with R Shiny user inter",tags$b("face"),".",align="center"),
                    p("'ScarFace' was written in 'R' and embedded in an interactive web app using the 'Shiny' package. Shiny combines the computational power of R with the interactivity of the modern web.", align="center"),
                    p("Shiny uses reactive elements, i.e. the user interacts via the 'ui' (user interface) with the actual R code running in the background called 'server'. Whenever the user make input changes, this will automatically affect all dependent elements (e.g. plots or data tables).", align="center"),
                    p("Even though the code can be accessed and modified, there is no need for the user to dig into R programming. So 'ScarFace' is extremely user-friendly and makes the usage of 'seacarb' work like a charm.", align="center"),
                    br(),
                    br(),
                    p("Authors: Markus Raitzsch and Jean-Pierre Gattuso", align="center"),
                    p("February 2020", align="center")
            ),
            
            # Tab content 'Bjerrum'
            tabItem(tabName = "bjerrum",
                    fluidRow(
                        box(width = 3, status = "primary", solidHeader = F, title="Set physical parameters", collapsible=T,
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 90%",
                                    sliderInput('temp', 'Temperature (°C)',
                                                min = 0, max = 60, value=25, step=0.1)),
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 45%",
                                    numericInput("T", value=25, min=-10, step=0.1, label=""))),
                            br(),
                            uiOutput("S"),
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 90%",
                                    sliderInput('sal', 'Salinity (psu scale)',
                                                min = 0, max = 50, value=35, step=0.1)),
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 45%",
                                    numericInput("S", value=35, min=0, step=0.1, label=""))),
                            br(),
                            uiOutput("P"),
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 90%",
                                    sliderInput('pres', 'Pressure (bar) | P=0 at surface',
                                                min = 0, max = 1200, value=0, step=1)),
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 45%",
                                    numericInput("P", value=0, min=0, step=1, label=""))),
                            hr(),
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 90%",
                                    p(tags$b("Dissociation constants [OUTPUT]")),
                                    textOutput(outputId = "pK1"),
                                    br(),
                                    textOutput(outputId = "pK2"),
                                    br(),
                                    textOutput(outputId = "info")
                                    ))
                            ),
                        box(width = 9, status = "primary", solidHeader = F, title="Bjerrum plot", collapsible=T,
                            p("The grey lines are reference carbon speciations at T = 25 °C, S = 35 (psu), and P = 0 bar", align="left"),
                            plotlyOutput(outputId = "plot1", width = "100%", height = "600px")
                        )
                        )),
            
            # Tab content 'Manual'
            tabItem(tabName = "man",
                    fluidRow(
                        box(width = 3, status = "primary", solidHeader = F, title="Carbonate system parameters [INPUT]", collapsible=T,
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 90%",
                                textInput(inputId="ID", "Sample name [optional]", value="NA", placeholder = T))),
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
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.2%",
                                    numericInput("first", value="", min=0, label="First variable")),
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.2%",
                                    numericInput("second", value="", min=0, label="Second variable"))),
                            hr(),
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 90%",
                                    numericInput("tempc", value="25", min=-10, step=0.1, label="Temperature (°C)")),
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 90%",
                                    numericInput("salc", min=0, step=0.1, value="35", label="Salinity (psu scale)")),
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 90%",
                                    numericInput("presc", min=0, value="0", label="Pressure (bar) | P=0 at surface")))
                            ),
                        box(width = 3, status = "primary", solidHeader = F, title="Additional choices [optional]", collapsible=T,
                            p("Leave silicate and phosphate concentrations at zero if unknown"),
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.2%",
                                    numericInput("sil", min=0, value="0", label="Silicate (µmol/kg)")),
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.2%",
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
                                    selectInput("gasm", "INPUT pCO2",
                                                c("refers to 1 atm P and potential T [default]" = "potential",
                                                  "refers to in situ P and in situ T" = "insitu",
                                                  "refers to 1 atm P and in situ T" = "standard")))),
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
                                                  "Seawater scale" = "SWS",
                                                  "NBS scale" = "NBS"))))
                            ),
                            box(width = 3, status = "primary", solidHeader = F, title=tags$div(HTML('<i class="fa fa-info" style = "color:#0072B2;"></i> &nbsp; Flags')), collapsible=T, collapsed = T,
                                p("1 = pH and CO2", br(),
                                  "2 = CO2 and HCO3", br(),
                                  "3 = CO2 and CO3", br(),
                                  "4 = CO2 and ALK", br(),
                                  "5 = CO2 and DIC", br(),
                                  "6 = pH and HCO3", br(),
                                  "7 = pH and CO3", br(),
                                  "8 = pH and ALK", br(),
                                  "9 = pH and DIC", br(),
                                  "10 = HCO3 and CO3", br(),
                                  "11 = HCO3 and ALK", br(),
                                  "12 = HCO3 and DIC", br(),
                                  "13 = CO3 and ALK", br(),
                                  "14 = CO3 and DIC", br(),
                                  "15 = ALK and DIC", br(),
                                  "21 = pCO2 and pH", br(),
                                  "22 = pCO2 and HCO3", br(),
                                  "23 = pCO2 and CO3", br(),
                                  "24 = pCO2 and ALK", br(),
                                  "25 = pCO2 and DIC"))
                            ),
                    fluidRow(
                        box(width = 12, status = "primary", solidHeader = F, title="Carbonate system parameters [OUTPUT]", collapsible=T,
                            div(style = 'overflow-x: auto', tableOutput("hot")),
                            hr(),
                            actionButton('collect', label = tags$div(HTML('<i class="fa fa-plus-circle" style = "color:black;"></i> &nbsp; Collect current data')))
                            )),
                    fluidRow(
                        box(width = 12, status = "primary", solidHeader = F, title="Collected output data", collapsible=T,
                            div(style = 'overflow-x: auto', DTOutput('CollectedData')),
                            hr(),
                            shinyUI(bootstrapPage(
                                downloadButton("coll", "Save collected data as csv table")
                            )),
                            actionButton('delete', label = tags$div(HTML('<i class="fa fa-trash" style = "color:black;"></i> &nbsp; Delete selected rows')),
                                         style="color: #000; background-color: #F0BD18; border-color: #E0E0E0"))
                        )),
            
            # Tab content 'Batch'
            tabItem(tabName = "batch",
                    fluidRow(
                        box(width = 12, status = "primary", solidHeader = F, title="Table with input data", collapsible=T,
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 25%",
                                    fileInput("batch", "Choose CSV file to upload",
                                              multiple = FALSE,
                                              accept = c(".csv"))
                                    ),
                            div(style="float: left; display:inline-block; margin-left: 15px; width: 15%",
                                selectInput("sep", "Choose separator type",
                                        c("Comma-separated" = ",",
                                          "Semicolon-separated" = ";",
                                          "Tab-separated" = "\tab")))),
                            div(style = 'overflow-x: auto', tableOutput(outputId = 'table1'))
                        )),
                    fluidRow(
                        box(width = 3, status = "primary", solidHeader = F, title = "Carbonate system parameters [INPUT]", collapsible = T,
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
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.2%",
                                    selectInput("firstb", "Select Variable 1", choices = NULL)),
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.2%",
                                    selectInput("secondb", "Select Variable 2", choices = NULL))),
                            hr(),
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.2%",
                                    selectInput("tempb", "Select Temperature", choices = NULL)),
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.2%",
                                    numericInput("tempbm", "or enter generic value", value = "", min = 0, max = 50))),
                                div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.2%",
                                    selectInput("salb", "Select Salinity", choices = NULL)),
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.2%",
                                    numericInput("salbm", "or enter generic value", value = "", min = 0, max = 50))),
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.2%",
                                    selectInput("presb", "Select Pressure or Depth", choices = NULL)),
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 20%",
                                    numericInput("presbm", "or generic", value = "", min = 0, max = 12000)),
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 20%",
                                    radioButtons("PorD", "What is given?", choices = c("Pressure" = 1, "Depth" = 9.9548), inline = F)))
                            ),
                        box(width = 3, status = "primary", solidHeader = F, title="Additional choices [optional]", collapsible=T,
                            p("Leave silicate and phosphate concentrations at zero if unknown"),
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.2%",
                                    numericInput("silb", min=0, value="0", label="Silicate (µmol/kg)")),
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.2%",
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
                                    selectInput("gasb", "INPUT pCO2",
                                                c("refers to 1 atm P and potential T [default]" = "potential",
                                                  "refers to in situ P and in situ T" = "insitu",
                                                  "refers to 1 atm P and in situ T" = "standard")))),
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
                                                  "Seawater scale" = "SWS",
                                                  "NBS scale" = "NBS"))))
                            ),
                        box(width = 3, status = "primary", solidHeader = F, title = "Include additional columns [optional]", collapsible = T,
                            pickerInput("incol", "Select columns from source table to be included in output table",
                                        choices = "", multiple = T, options = list('actions-box' = T))),
                        box(width = 3, status = "primary", solidHeader = F, title=tags$div(HTML('<i class="fa fa-info" style = "color:#0072B2;"></i> &nbsp; Flags')), collapsible=T, collapsed = T,
                            p("1 = pH and CO2", br(),
                              "2 = CO2 and HCO3", br(),
                              "3 = CO2 and CO3", br(),
                              "4 = CO2 and ALK", br(),
                              "5 = CO2 and DIC", br(),
                              "6 = pH and HCO3", br(),
                              "7 = pH and CO3", br(),
                              "8 = pH and ALK", br(),
                              "9 = pH and DIC", br(),
                              "10 = HCO3 and CO3", br(),
                              "11 = HCO3 and ALK", br(),
                              "12 = HCO3 and DIC", br(),
                              "13 = CO3 and ALK", br(),
                              "14 = CO3 and DIC", br(),
                              "15 = ALK and DIC", br(),
                              "21 = pCO2 and pH", br(),
                              "22 = pCO2 and HCO3", br(),
                              "23 = pCO2 and CO3", br(),
                              "24 = pCO2 and ALK", br(),
                              "25 = pCO2 and DIC"))
                        ),
                    fluidRow(
                        box(width = 12, status = "primary", solidHeader = F, title="Carbonate system parameters [OUTPUT]", collapsible=T,
                            div(style = 'overflow-x: auto', tableOutput('table2')),
                            hr(),
                            shinyUI(bootstrapPage(
                                downloadButton("batch_end", "Save results as csv file")
                                )),
                            actionButton('gotoerr', label = 'OR continue here for error propagation',
                                         style="color: #000; background-color: #F0BD18; border-color: #E0E0E0")
                            ))
                    ),
            
            # Tab content 'Error propagation'
            tabItem(tabName = "error",
                    fluidRow(
                        box(width = 3, status = "primary", solidHeader = F, title="Define uncertainties of input parameters", collapsible=T,
                            p("either from the data source or by entering manually", align="left"),
                            p("NOTE: 'Manual' will override 'Data Source'", align="left"),
                            hr(),
                            htmlOutput("var1e_text"),
                            br(),
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.2%",
                                    selectInput("firste", "Data source", choices = NULL)),
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.2%",
                                    numericInput("firstem", min=0, value="", label="or enter generic value"))),
                            htmlOutput("var2e_text"),
                            br(),
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.2%",
                                    selectInput("seconde", "Data source", choices = NULL)),
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.2%",
                                    numericInput("secondm", min=0, value="", label="or enter generic value"))),
                            p(tags$b("Temperature uncertainty:"), align="left"),
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.2%",
                                    selectInput("tempe", "Data source", choices = NULL)),
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.2%",
                                    numericInput("tempm", min=0, step=0.1, value="", label="or enter generic value"))),
                            p(tags$b("Salinity uncertainty:"), align="left"),
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.2%",
                                    selectInput("sale", "Data source", choices = NULL)),
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.2%",
                                    numericInput("salm", min=0, step=0.1, value="", label="or enter generic value"))),
                        ),
                        box(width = 3, status = "primary", solidHeader = F, title="Additional choices [optional]", collapsible=T,
                            p("Leave silicate and phosphate uncertainties at zero if unknown"),
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.2%",
                                    numericInput("Pte", min=0, value="0", label="Phosphate uncertainty (µmol/kg)")),
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.2%",
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
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.2%",
                                    numericInput("corr", step=0.1, min=-1, max=1, value="0.0", label="Correlation coefficient")),
                                div(style="float: left; display:inline-block; margin-left: 15px; width: 43.2%",
                                    numericInput("reps", step=1000, min=1, value="10000", label="Monte Carlo repetitions"))),
                        )),
                    fluidRow(
                        box(width = 12, status = "primary", solidHeader = F, title="Propagated uncertainties of carbonate system parameters [OUTPUT]", collapsible=T,
                            div(style = 'overflow-x: auto', tableOutput('table3')),
                            hr(),
                            div(class='row',
                                div(style="float: left; display:inline-block; margin-left: 15px",
                                    checkboxInput(inputId="comb", label = "Should input data be included?", value = FALSE, width = NULL)),
                                div(style="float: left; display:inline-block; margin-left: 15px",
                                    shinyUI(bootstrapPage(
                                        downloadButton("batch_err", "Save uncertainties as csv table")
                                        ))))
                            ))
                    ),
            
            # Tab content 'Citation'
            tabItem(tabName = "cite",
                    br(),
                    br(),
                    br(),
                    p("ScarFace", align = "center", style="color: #7da2d1; font-size: 48px"),
                    p(em("web version 1.3.0"), align="center"),
                    p("When you use 'ScarFace' for your published research, please cite the following two references:", align="center"),
                      br(),
                      p("Raitzsch, M. and Gattuso, J.-P., 2020. ScarFace - seacarb calculations with R Shiny user interface. https://doi.org/10.5281/zenodo.3662139.", align="center"),
                      p("Gattuso J.-P., Epitalon J.-M., Lavigne H. and Orr J., 2019. seacarb: seawater carbonate chemistry. R package version 3.2.12. http://CRAN.R-project.org/package=seacarb.", align="center")
            )
        )
    )
)

##### Server #####
server <- function(input, output, session) {
    #### Bjerrum plot ####
    # Dynamic Slider and numeric inputs #
    observeEvent(input$T, {
        updateSliderInput(session, "temp", value = input$T)
    })
    observeEvent(input$temp, {
        updateNumericInput(session, "T", value = input$temp)
    })
    observeEvent(input$S, {
        updateSliderInput(session, "sal", value = input$S)
    })
    observeEvent(input$sal, {
        updateNumericInput(session, "S", value = input$sal)
    })
    observeEvent(input$P, {
        updateSliderInput(session, "pres", value = input$P)
    })
    observeEvent(input$pres, {
        updateNumericInput(session, "P", value = input$pres)
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
    
    # Render plot
    output$plot1 <- renderPlotly({
        
        pH <- seq(2, 12, 0.1)
        pH <- as.data.frame(pH)
        
        K10 <- as.numeric(K1(S=35, T=25, P=0, pHscale="T"))
        K20 <- as.numeric(K2(S=35, T=25, P=0, pHscale="T"))
        res0 <- speciation(K1=K10, K2=K20, K3=NULL, pH, conc=1)
        res0 <- as.data.frame(res0)
        df0 <- bind_cols(pH, res0) %>%
            rename(c(CO2 = pH1, HCO3 = pH.1, CO3 = pH.2))
        
        K1 <- as.numeric(K1(S=input$sal, T=input$temp, P=input$pres, pHscale="T"))
        K2 <- as.numeric(K2(S=input$sal, T=input$temp, P=input$pres, pHscale="T"))
        res <- speciation(K1=K1, K2=K2, K3=NULL, pH, conc=1)
        res <- as.data.frame(res)
        df <- bind_cols(pH, res) %>%
            rename(c(CO2 = pH1, HCO3 = pH.1, CO3 = pH.2))
        
        ggplot(df0) +
            geom_line(aes(pH, CO2), linetype = "solid", color = "grey", size = 0.5) +
            geom_line(aes(pH, HCO3), linetype = "dashed", color = "grey", size = 0.5) +
            geom_line(aes(pH, CO3), linetype = "dotted", color = "grey", size = 0.5) +
            theme(axis.line = element_line(size=0.3),
                  axis.text = element_text(size=8, colour="black"),
                  axis.ticks.length = unit(0.15,"cm"),
                  axis.ticks = element_line(colour = "black", size = 0.3),
                  axis.title = element_text(size=8),
                  panel.grid = element_blank(),
                  panel.background = element_blank()) +
            scale_y_continuous(name=bquote("Relative concentration"), breaks=seq(0, 1, 0.2), sec.axis=dup_axis(label=NULL, name=NULL)) +
            scale_x_continuous(name=bquote("pH(tot)"), breaks=seq(2, 12, 0.5), expand = c(0,0), sec.axis=dup_axis(label=NULL, name=NULL)) +
            geom_line(df, mapping = aes(pH, CO2), linetype = "solid", color = "darkred", size = 0.5) +
            geom_line(df, mapping = aes(pH, HCO3), linetype = "dashed", color = "darkred", size = 0.5) +
            geom_line(df, mapping = aes(pH, CO3), linetype = "dotted", color = "darkred", size = 0.5)
    })
    
    #### Carbonate system parameters (manual) ####
    # Some code for dynamic labels #
    observeEvent(input$pair, {
        x <- if (input$pair==1 || input$pair==6 || input$pair==7 || input$pair==8 || input$pair==9){
            "pH"}
        else if (input$pair==2 || input$pair==3 || input$pair==4 || input$pair==5){
            "CO2 (µmol/kg)"}
        else if (input$pair==10 || input$pair==11 || input$pair==12){
            "HCO3 (µmol/kg)"}
        else if (input$pair==13 || input$pair==14){
            "CO3 (µmol/kg)"}
        else if (input$pair==15){
            "ALK (µmol/kg)"}
        else
            "pCO2 (µatm)"
        
        y <- if (input$pair==21){
            "pH"}
        else if (input$pair==1){
            "CO2 (µmol/kg)"}
        else if (input$pair==3 || input$pair==7 || input$pair==10 || input$pair==23){
            "CO3 (µmol/kg)"}
        else if (input$pair==2 || input$pair==6 || input$pair==22){
            "HCO3 (µmol/kg)"}
        else if (input$pair==4 || input$pair==8 || input$pair==11 || input$pair==13 || input$pair==24){
            "ALK (µmol/kg)"}
        else
            "DIC (µmol/kg)"

        updateNumericInput(session, "first",
                           label = paste(x))
        updateNumericInput(session, "second",
                           label = paste(y))
    })
    
    # Carbonate system calculation based on parameter inputs #
    carb_man <- reactive({
        if (input$pair == "2" || input$pair == "3" || input$pair == "4" || input$pair == "5" || input$pair == "10" || input$pair == "11" || input$pair == "12" || input$pair == "13" || input$pair == "14" || input$pair == "15") {
            if (!input$pHscale == "NBS") {
            carb(flag=as.numeric(input$pair), var1=input$first/1000000, var2=input$second/1000000, S=input$salc, T=input$tempc, Patm=1, P=input$presc, Pt=input$pho/1000000, Sit=input$sil/1000000,
                 k1k2=input$k1k2, kf="x", ks="d", pHscale=input$pHscale, b=input$b, gas=input$gasm,
                 warn="y", eos="eos80", long=1.e20, lat=1.e20) %>%
                    mutate(Sample = input$ID) %>%
                    select(Sample, everything())
            } else {
                carb(flag=as.numeric(input$pair), var1=input$first/1000000, var2=input$second/1000000, S=input$salc, T=input$tempc, Patm=1, P=input$presc, Pt=input$pho/1000000, Sit=input$sil/1000000,
                     k1k2=input$k1k2, kf="x", ks="d", pHscale="SWS", b=input$b, gas=input$gasm,
                     warn="y", eos="eos80", long=1.e20, lat=1.e20) %>%
                    mutate(Sample = input$ID) %>%
                    select(Sample, everything())
            }
        } else if (input$pair == "22" || input$pair == "23" || input$pair == "24" || input$pair == "25") {
            if (!input$pHscale == "NBS") {
                carb(flag=as.numeric(input$pair), var1=input$first, var2=input$second/1000000, S=input$salc, T=input$tempc, Patm=1, P=input$presc, Pt=input$pho/1000000, Sit=input$sil/1000000,
                     k1k2=input$k1k2, kf="x", ks="d", pHscale=input$pHscale, b=input$b, gas=input$gasm,
                     warn="y", eos="eos80", long=1.e20, lat=1.e20) %>%
                    mutate(Sample = input$ID) %>%
                    select(Sample, everything())
            } else {
                carb(flag=as.numeric(input$pair), var1=input$first, var2=input$second/1000000, S=input$salc, T=input$tempc, Patm=1, P=input$presc, Pt=input$pho/1000000, Sit=input$sil/1000000,
                     k1k2=input$k1k2, kf="x", ks="d", pHscale="SWS", b=input$b, gas=input$gasm,
                     warn="y", eos="eos80", long=1.e20, lat=1.e20) %>%
                    mutate(Sample = input$ID) %>%
                    select(Sample, everything())
            }
        } else if (input$pair == "21") {
            if (!input$pHscale == "NBS") {
                carb(flag=as.numeric(input$pair), var1=input$first, var2=input$second, S=input$salc, T=input$tempc, Patm=1, P=input$presc, Pt=input$pho/1000000, Sit=input$sil/1000000,
                     k1k2=input$k1k2, kf="x", ks="d", pHscale=input$pHscale, b=input$b, gas=input$gasm,
                     warn="y", eos="eos80", long=1.e20, lat=1.e20) %>%
                    mutate(Sample = input$ID) %>%
                    select(Sample, everything())
                } else {
                    carb(flag=as.numeric(input$pair), var1=input$first, var2=input$second+log10(1.2948 - 2.036e-3*(input$tempc+273.15) + (4.607e-4 - 1.475e-6*(input$tempc+273.15))*input$salc^2), S=input$salc, T=input$tempc, Patm=1, P=input$presc, Pt=input$pho/1000000, Sit=input$sil/1000000,
                         k1k2=input$k1k2, kf="x", ks="d", pHscale="SWS", b=input$b, gas=input$gasm,
                         warn="y", eos="eos80", long=1.e20, lat=1.e20) %>%
                        mutate(Sample = input$ID) %>%
                        select(Sample, everything())
                }
        } else {
            if (input$pHscale == "NBS") {
            carb(flag=as.numeric(input$pair), var1=input$first+log10(1.2948 - 2.036e-3*(input$tempc+273.15) + (4.607e-4 - 1.475e-6*(input$tempc+273.15))*input$salc^2), var2=input$second/1000000, S=input$salc, T=input$tempc, Patm=1, P=input$presc, Pt=input$pho/1000000, Sit=input$sil/1000000,
                 k1k2=input$k1k2, kf="x", ks="d", pHscale="SWS", b=input$b, gas=input$gasm,
                 warn="y", eos="eos80", long=1.e20, lat=1.e20) %>%
                    mutate(Sample = input$ID) %>%
                    select(Sample, everything())
                } else {
                    carb(flag=as.numeric(input$pair), var1=input$first, var2=input$second/1000000, S=input$salc, T=input$tempc, Patm=1, P=input$presc, Pt=input$pho/1000000, Sit=input$sil/1000000,
                         k1k2=input$k1k2, kf="x", ks="d", pHscale=input$pHscale, b=input$b, gas=input$gasm,
                         warn="y", eos="eos80", long=1.e20, lat=1.e20) %>%
                        mutate(Sample = input$ID) %>%
                        select(Sample, everything())
                }
            }
        })
    
    # Render the output table #
    output$hot <- renderTable({
        if (input$pHscale == "NBS") {
        carb_man() %>%
            mutate(HCO3 = HCO3*1000000) %>%
            mutate(CO3 = CO3*1000000) %>%
            mutate(CO2 = CO2*1000000) %>%
            mutate(DIC = DIC*1000000) %>%
            mutate(ALK = ALK*1000000) %>%
            mutate(pH = pH-log10(1.2948 - 2.036e-3*(input$tempc+273.15) + (4.607e-4 - 1.475e-6*(input$tempc+273.15))*input$salc^2))
        } else {
            carb_man() %>%
                mutate(HCO3 = HCO3*1000000) %>%
                mutate(CO3 = CO3*1000000) %>%
                mutate(CO2 = CO2*1000000) %>%
                mutate(DIC = DIC*1000000) %>%
                mutate(ALK = ALK*1000000)
        }
        }, align='c', digits=2)
    
    # Collect current data and render in extra table #    
        values <- reactiveValues(df = data.frame())

        newEntry <- observeEvent(input$collect,{
                if (input$pHscale == "NBS") {
                values$df <- isolate(rbind(values$df, carb_man())) %>%
                    mutate(flag = ifelse(row_number()==nrow(.), isolate(input$pair), flag)) %>%
                    mutate(CO2 = ifelse(row_number()==nrow(.), CO2*1000000, CO2)) %>%
                    mutate(HCO3 = ifelse(row_number()==nrow(.), HCO3*1000000, HCO3)) %>%
                    mutate(CO3 = ifelse(row_number()==nrow(.), CO3*1000000, CO3)) %>%
                    mutate(DIC = ifelse(row_number()==nrow(.), DIC*1000000, DIC)) %>%
                    mutate(ALK = ifelse(row_number()==nrow(.), ALK*1000000, ALK)) %>%
                    mutate(pH = ifelse(row_number()==nrow(.), pH-log10(1.2948 - 2.036e-3*(input$tempc+273.15) + (4.607e-4 - 1.475e-6*(input$tempc+273.15))*input$salc^2), pH))
                } else {
                    values$df <- isolate(rbind(values$df, carb_man())) %>%
                        mutate(flag = ifelse(row_number()==nrow(.), isolate(input$pair), flag)) %>%
                        mutate(CO2 = ifelse(row_number()==nrow(.), CO2*1000000, CO2)) %>%
                        mutate(HCO3 = ifelse(row_number()==nrow(.), HCO3*1000000, HCO3)) %>%
                        mutate(CO3 = ifelse(row_number()==nrow(.), CO3*1000000, CO3)) %>%
                        mutate(DIC = ifelse(row_number()==nrow(.), DIC*1000000, DIC)) %>%
                        mutate(ALK = ifelse(row_number()==nrow(.), ALK*1000000, ALK))
                }
            })
    
    output$CollectedData <- renderDT({
        req(input$collect)
        datatable(values$df,
                  options = list(pageLength = 10, autoWidth = TRUE, bFilter=0), rownames= FALSE) %>%
            formatRound(c(3:20), 2) %>% 
            formatStyle(columns = c(1:20), 'text-align' = 'center')
    })
    
    # Delete selected rows
    observeEvent(input$delete,{
        if (!is.null(input$CollectedData_rows_selected)) {
            values$df <- values$df[-as.numeric(input$CollectedData_rows_selected),]
        }
    })
    
    # Save data as csv
    output$coll <- downloadHandler(
        filename = "Results_collected_data.csv",
        content = function(file) {
            write.csv(isolate(values$df), file, row.names = FALSE)
        }
    )
    
    #### Carbonate system parameters (batch) ####
    # Upload data as csv #
     tbl1 <- reactive({
         req(input$batch)
         tbl1 <- read_delim(input$batch$datapath, delim = input$sep, col_names = TRUE, col_types = NULL, na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, progress = show_progress(), skip_empty_rows = TRUE)
     })

    # Render uploaded table #
    output$table1 <- renderTable({
        tbl1()
        }, align='c', digits=2)
    
    # Read column names and render for drop-down menu #
    observeEvent(tbl1(), {
        updateSelectInput(session, "firstb", choices = names(tbl1()))
        updateSelectInput(session, "secondb", choices = names(tbl1()))
        updateSelectInput(session, "tempb", choices = names(tbl1()))
        updateSelectInput(session, "salb", choices = names(tbl1()))
        updateSelectInput(session, "presb", choices = names(tbl1()))
    })
    
    # Read column names and create check-box list #
    observe({
        req(input$batch)
        dnames <- names(tbl1())
        inc_options <- list()
        inc_options[dnames] <- dnames
        updatePickerInput(session, "incol",
                                 label = "Select original columns to be included in output table",
                                 choices = inc_options,
                                 selected = "")
        })
    
    # Some code for dynamic labels for input variables #
    observeEvent(input$pairb, {
        x <- if (input$pairb==1 || input$pairb==6 || input$pairb==7 || input$pairb==8 || input$pairb==9){
            "pH"}
        else if (input$pairb==2 || input$pairb==3 || input$pairb==4 || input$pairb==5){
            "CO2 (µmol/kg)"}
        else if (input$pairb==10 || input$pairb==11 || input$pairb==12){
            "HCO3 (µmol/kg)"}
        else if (input$pairb==13 || input$pairb==14){
            "CO3 (µmol/kg)"}
        else if (input$pairb==15){
            "ALK (µmol/kg)"}
        else
            "pCO2 (µatm)"
        
        y <- if (input$pairb==21){
            "pH"}
        else if (input$pairb==1){
            "CO2 (µmol/kg)"}
        else if (input$pairb==3 || input$pairb==7 || input$pairb==10 || input$pairb==23){
            "CO3 (µmol/kg)"}
        else if (input$pairb==2 || input$pairb==6 || input$pairb==22){
            "HCO3 (µmol/kg)"}
        else if (input$pairb==4 || input$pairb==8 || input$pairb==11 || input$pairb==13 || input$pairb==24){
            "ALK (µmol/kg)"}
        else
            "DIC (µmol/kg)"
        
        updateNumericInput(session, "firstb",
                           label = paste("Select ", x))
        updateNumericInput(session, "secondb",
                           label = paste("Select ", y))
    })
    
    # Define T, S, and P in case they are not available from the source table #
    tx <- reactive({
        req(input$batch)
        if (isTruthy(input$tempbm)) {
            as.numeric(input$tempbm)
        } else {
            tbl1()[[input$tempb]]
        }
    })
    
    sx <- reactive({
        req(input$batch)
        if (isTruthy(input$salbm)) {
            as.numeric(input$salbm)
        } else {
            tbl1()[[input$salb]]
        }
    })
    
    px <- reactive({
        req(input$batch)
        if (isTruthy(input$presbm)) {
            as.numeric(input$presbm)/as.numeric(input$PorD)
        } else {
            tbl1()[[input$presb]]/as.numeric(input$PorD)
        }
    })
    
    # Carbonate system calculation based on parameter inputs #
    carb_batch <- reactive({
        if (input$pairb == "2" || input$pairb == "3" || input$pairb == "4" || input$pairb == "5" || input$pairb == "10" || input$pairb == "11" || input$pairb == "12" || input$pairb == "13" || input$pairb == "14" || input$pairb == "15") {
            if (!input$pHscaleb == "NBS") {
            carb(flag=as.numeric(input$pairb), var1=tbl1()[[input$firstb]]/1000000, var2=tbl1()[[input$secondb]]/1000000, S=sx(), T=tx(), Patm=1, P=px(), Pt=input$phob/1000000, Sit=input$silb/1000000,
                 k1k2=input$k1k2b, kf="x", ks="d", pHscale=input$pHscaleb, b=input$bb, gas=input$gasb,
                 warn="y", eos="eos80", long=1.e20, lat=1.e20)
            } else {
                carb(flag=as.numeric(input$pairb), var1=tbl1()[[input$firstb]]/1000000, var2=tbl1()[[input$secondb]]/1000000, S=sx(), T=tx(), Patm=1, P=px(), Pt=input$phob/1000000, Sit=input$silb/1000000,
                     k1k2=input$k1k2b, kf="x", ks="d", pHscale="SWS", b=input$bb, gas=input$gasb,
                     warn="y", eos="eos80", long=1.e20, lat=1.e20)
            }
        } else if (input$pairb == "22" || input$pairb == "23" || input$pairb == "24" || input$pairb == "25") {
            if (!input$pHscaleb == "NBS") {
                carb(flag=as.numeric(input$pairb), tbl1()[[input$firstb]], var2=tbl1()[[input$secondb]]/1000000, S=sx(), T=tx(), Patm=1, P=px(), Pt=input$phob/1000000, Sit=input$silb/1000000,
                     k1k2=input$k1k2b, kf="x", ks="d", pHscale=input$pHscaleb, b=input$bb, gas=input$gasb,
                     warn="y", eos="eos80", long=1.e20, lat=1.e20)
            } else {
                carb(flag=as.numeric(input$pairb), tbl1()[[input$firstb]], var2=tbl1()[[input$secondb]]/1000000, S=sx(), T=tx(), Patm=1, P=px(), Pt=input$phob/1000000, Sit=input$silb/1000000,
                     k1k2=input$k1k2b, kf="x", ks="d", pHscale="SWS", b=input$bb, gas=input$gasb,
                     warn="y", eos="eos80", long=1.e20, lat=1.e20)
            }
        } else if (input$pairb == "21") {
            if (!input$pHscaleb == "NBS") {
            carb(flag=as.numeric(input$pairb), tbl1()[[input$firstb]], var2=tbl1()[[input$secondb]], S=sx(), T=tx(), Patm=1, P=px(), Pt=input$phob/1000000, Sit=input$silb/1000000,
                 k1k2=input$k1k2b, kf="x", ks="d", pHscale=input$pHscaleb, b=input$bb, gas=input$gasb,
                 warn="y", eos="eos80", long=1.e20, lat=1.e20)
            } else {
                carb(flag=as.numeric(input$pairb), tbl1()[[input$firstb]], var2=tbl1()[[input$secondb]]+log10(1.2948 - 2.036e-3*(tx()+273.15) + (4.607e-4 - 1.475e-6*(tx()+273.15))*sx()^2), S=sx(), T=tx(), Patm=1, P=px(), Pt=input$phob/1000000, Sit=input$silb/1000000,
                     k1k2=input$k1k2b, kf="x", ks="d", pHscale="SWS", b=input$bb, gas=input$gasb,
                     warn="y", eos="eos80", long=1.e20, lat=1.e20)
            }
        } else {
            if (input$pHscaleb == "NBS") {
            carb(flag=as.numeric(input$pairb), var1=tbl1()[[input$firstb]]+log10(1.2948 - 2.036e-3*(tx()+273.15) + (4.607e-4 - 1.475e-6*(tx()+273.15))*tbl1()[[input$salb]]^2), var2=tbl1()[[input$secondb]]/1000000, S=sx(), T=tx(), Patm=1, P=px(), Pt=input$phob/1000000, Sit=input$silb/1000000,
                 k1k2=input$k1k2b, kf="x", ks="d", pHscale="SWS", b=input$bb, gas=input$gasb,
                 warn="y", eos="eos80", long=1.e20, lat=1.e20)
            } else {
                carb(flag=as.numeric(input$pairb), var1=tbl1()[[input$firstb]], var2=tbl1()[[input$secondb]]/1000000, S=sx(), T=tx(), Patm=1, P=px(), Pt=input$phob/1000000, Sit=input$silb/1000000,
                     k1k2=input$k1k2b, kf="x", ks="d", pHscale=input$pHscaleb, b=input$bb, gas=input$gasb,
                     warn="y", eos="eos80", long=1.e20, lat=1.e20)
                }
            }
        })
    
    # Render output table and (if needed) combine with selected columns from uploaded table #
    dat <- reactive({
        req(input$batch)
        if (is.null(input$incol))
            return()
        tbl1()[, input$incol]
        })
    
    tbl_final <- reactive({
        if (input$pHscaleb == "NBS") {
        carb_batch() %>%
            mutate(HCO3 = HCO3*1000000) %>%
            mutate(CO3 = CO3*1000000) %>%
            mutate(CO2 = CO2*1000000) %>%
            mutate(DIC = DIC*1000000) %>%
            mutate(ALK = ALK*1000000) %>%
            mutate(pH = pH-+log10(1.2948 - 2.036e-3*(tbl1()[[input$tempb]]+273.15) + (4.607e-4 - 1.475e-6*(tbl1()[[input$tempb]]+273.15))*tbl1()[[input$salb]]^2)) %>%
            bind_cols(dat()) %>%
            select(input$incol, everything())
        } else {
            carb_batch() %>%
                mutate(HCO3 = HCO3*1000000) %>%
                mutate(CO3 = CO3*1000000) %>%
                mutate(CO2 = CO2*1000000) %>%
                mutate(DIC = DIC*1000000) %>%
                mutate(ALK = ALK*1000000) %>%
                bind_cols(dat()) %>%
                select(input$incol, everything())
        }
        })
    
    output$table2 <- renderTable({
        req(input$batch)
        tbl_final()
        }, align='c', digits=2)
    
    # Save data as csv
    output$batch_end <- downloadHandler(
        filename = function() {
            paste("Results", input$batch, sep = "_")
        },
        content = function(file) {
            write.csv(isolate(tbl_final()), file, row.names = FALSE)
        }
    )
    
    #### Error propagation ####
    # On click switch to other tab #
    observeEvent(input$gotoerr, {
        newtab <- switch(input$tabs, "batch" = "error")
        updateTabItems(session, "tabs", newtab)
        })
    
    # Dynamically change labels when input parameters are changed #
    x <- reactive(
        if (input$pairb==1 || input$pairb==6 || input$pairb==7 || input$pairb==8 || input$pairb==9){
            "pH"}
        else if (input$pairb==2 || input$pairb==3 || input$pairb==4 || input$pairb==5){
            "CO2 (µmol/kg)"}
        else if (input$pairb==10 || input$pairb==11 || input$pairb==12){
            "HCO3 (µmol/kg)"}
        else if (input$pairb==13 || input$pairb==14){
            "CO3 (µmol/kg)"}
        else if (input$pairb==15){
            "ALK (µmol/kg)"}
        else
            "pCO2 (µatm)"
    )
    
    output$var1e_text <- renderText({paste("<b>", x(), " uncertainty</b>")})
    
    y <- reactive(
        if (input$pairb==21){
            "pH"}
        else if (input$pairb==1){
            "CO2 (µmol/kg)"}
        else if (input$pairb==3 || input$pairb==7 || input$pairb==10 || input$pairb==23){
            "CO3 (µmol/kg)"}
        else if (input$pairb==2 || input$pairb==6 || input$pairb==22){
            "HCO3 (µmol/kg)"}
        else if (input$pairb==4 || input$pairb==8 || input$pairb==11 || input$pairb==13 || input$pairb==24){
            "ALK (µmol/kg)"}
        else
            "DIC (µmol/kg)"
    )
    
    output$var2e_text <- renderText({paste("<b>", y(), " uncertainty</b>")})

    
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
                   k1k2='x', kf='x', ks="d", pHscale="T", b="l10", gas="potential",
                   warn="y", eos = "eos80", long = 1e+20, lat = 1e+20)
        } else if (input$pairb == "21") {
            errors(flag=as.numeric(input$pairb), var1=tbl1()[[input$firstb]], var2=tbl1()[[input$secondb]], S=tbl1()[[input$salb]], T=tbl1()[[input$tempb]], Patm=1, P=tbl1()[[input$presb]]/as.numeric(input$PorD), Pt=input$phob/1000000, Sit=input$silb/1000000,
                   evar1=evar1(), evar2=evar2(), eS=eS(), eT=eT(), ePt=input$Pte/1000000, eSit=input$Site/1000000,
                   epK=epK(),
                   eBt=input$Bte, method = input$meth, r=input$corr, runs=input$reps,
                   k1k2='x', kf='x', ks="d", pHscale="T", b="l10", gas="potential",
                   warn="y", eos = "eos80", long = 1e+20, lat = 1e+20)
        } else {
            errors(flag=as.numeric(input$pairb), var1=tbl1()[[input$firstb]], var2=tbl1()[[input$secondb]]/1000000, S=tbl1()[[input$salb]], T=tbl1()[[input$tempb]], Patm=1, P=tbl1()[[input$presb]]/as.numeric(input$PorD), Pt=input$phob/1000000, Sit=input$silb/1000000,
                   evar1=evar1(), evar2=evar2()/1000000, eS=eS(), eT=eT(), ePt=input$Pte/1000000, eSit=input$Site/1000000,
                   epK=epK(),
                   eBt=input$Bte, method = input$meth, r=input$corr, runs=input$reps,
                   k1k2='x', kf='x', ks="d", pHscale="T", b="l10", gas="potential",
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
        req(input$batch)
        tbl5()
        }, align='c', digits=2)
    
    # Save data as csv
    output$batch_err <- downloadHandler(
        filename = function() {
            paste("Results_err", input$batch, sep = "_")
        },
        content = function(file) {
            write.csv(isolate(tbl5()), file, row.names = FALSE)
        }
    )
}

# Run the application #
shinyApp(ui = ui, server = server)
