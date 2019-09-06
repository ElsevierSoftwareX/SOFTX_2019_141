library(shiny)
library(leaflet)
library(plotly)
library(visNetwork)
library(networkD3)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)


# Choices for drop-downs
vars <- c(
  'Collision Vehicle 1' = 'VEHICLE.TYPE.CODE.1',
  'Collision Vehicle 2' = 'VEHICLE.TYPE.CODE.2',
  "Injuries" = "NUMBER.OF.PERSONS.INJURED",
  "Deaths" = "NUMBER.OF.PERSONS.KILLED"
)

vars2 <- c(
  "Injuries" = "NUMBER.OF.PERSONS.INJURED",
  "Deaths" = "NUMBER.OF.PERSONS.KILLED"
)

vars3 <- c(
  "All Vehicles" = "",
  "Ambulance" = "AMBULANCE",
  "Bicycle" = "BICYCLE",
  "Bus" = "BUS",
  "Fire Truck" = "FIRE TRUCK",
  "Large Commercial Vehicle(6 or more tires)" = "LARGE COM VEH(6 OR MORE TIRES)",
  "Motorcycle" = "MOTORCYCLE",
  "Passenger" = "PASSENGER VEHICLE",
  "Pick-up Truck" = "PICK-UP TRUCK",
  "Small Commercial Vehicle(4 tires)" = "SMALL COM VEH(4 TIRES)",
  "Taxi" = "TAXI"
)

vars4 <- c("All boroughs"="",
           'Manhattan'='MANHATTAN',
           'Brooklyn'='BROOKLYN',
           'Queens'='QUEENS','Bronx'='BRONX')


shinyUI(dashboardPage(
  dashboardHeader(title = "DM-MCDA",
                  tags$li(class = "dropdown", tags$a(icon("github"), href = "https://github.com/",title = "See the code on github"))
  ),
  
  dashboardSidebar(sidebarMenu(
    menuItem("Data Sources", tabName = "ds", icon = icon("database")),
    menuItem("ARM", tabName = "arm", icon = icon("random")),
    menuItem("MCDA", tabName = "mcda", icon = icon("users")),
    menuItem("MAP", tabName = "map", icon = icon("map")),
    menuItem("TMS", tabName = "tms", icon = icon("line-chart")),
    menuItem("About", tabName = "about", icon = icon("info-circle"))
  )),
  
  dashboardBody(
    # ensure links to sidebar menus
    tags$script(HTML("
        var openTab = function(tabName){
          $('a', $('.sidebar')).each(function() {
            if(this.getAttribute('data-value') == tabName) {
              this.click()
            };
          });
        }
      ")),
    #to use mini sidebar
    useShinyjs(),
    # Include styling for the app
    includeCSS("www/app.css"),

    
    tabItems(
      # Meta data section
      tabItem(tabName = "ds",
              # Boxes need to be put in a row (or column)
              fluidRow(
                column(width = 12,
                       tabBox(
                         side = "left",width = 12,
                         tabPanel("Meta Data Structure",
                                  box(width = NULL, title = "Dataset:", status = "primary", DT::dataTableOutput("dataset_ds"),collapsible = TRUE),
                                  box(width = NULL, title = "Decision Matrix:", status = "primary", DT::dataTableOutput("descion_matrix_ds"),collapsible = TRUE)
                         )
                       )
                )
              )
      ),
      
      # First tab content
      tabItem(tabName = "arm",
              # Boxes need to be put in a row (or column)
              fluidRow(
                column(width = 3,
                       box(width = NULL, status = "info",
                         fileInput("file", "File")
                         ),
                       box(width = NULL, status = "info", collapsible = T, title = "Parameters",
                           conditionalPanel(
                             condition = "input.samp=='Sample'",
                             numericInput("nrule", 'Number of Rules', 5)
                           ),
                           
                         conditionalPanel(
                           condition = "input.mytab=='graph'",
                           radioButtons('graphType', label='Graph Type', choices=c('itemsets','items'), inline=T)
                         ),
                         
                         conditionalPanel(
                           condition = "input.lhsv=='Subset'", 
                           uiOutput("choose_lhs")
                         ),
                         
                         conditionalPanel(
                           condition = "input.rhsv=='Subset'", 
                           uiOutput("choose_rhs")
                         ),
                         
                         conditionalPanel(
                           condition = "input.mytab=='grouped'",
                           sliderInput('k', label='Choose # of rule clusters', min=1, max=150, step=1, value=15)
                         ),
                         
                         conditionalPanel(
                           condition = "input.mytab %in%' c('grouped', 'graph', 'table', 'datatable', 'scatter', 'paracoord', 'matrix', 'itemFreq')", 
                           radioButtons('samp', label='Sample', choices=c('All Rules', 'Sample'), inline=T),
                           uiOutput("choose_columns"),
                           sliderInput("supp", "Support:", min = 0, max = 1, value = 0.1 , step = 1/10000),
                           sliderInput("conf", "Confidence:", min = 0, max = 1, value = 0.5 , step = 1/10000),
                           selectInput('sort', label='Sorting Criteria:', choices = c('lift', 'confidence', 'support')),
                           numericInput("minL", "Min. items per set:", 2),
                           numericInput("maxL", "Max. items per set::", 3),
                           downloadButton('downloadData', 'Download Rules as CSV')
                           #radioButtons('lhsv', label='LHS variables', choices=c('All', 'Subset')), br(),
                           #radioButtons('rhsv', label='RHS variables', choices=c('All', 'Subset')), br()
                         )
                       )
              ),
              column(width = 9,
                     tabBox(
                       side = "left",width = 12,
                       tabPanel('Summary', value='table', verbatimTextOutput("statistics")),
                       tabPanel('ScatterPlot', plotlyOutput("scatterPlot")),
                       tabPanel('FrequentItemset', value='itemFreq', plotOutput("itemFreqPlot", width='100%', height='100%')),
                       tabPanel('Grouped', value='grouped', plotOutput("groupedPlot", width='100%', height='100%')),
                       tabPanel('RulesGraph', visNetworkOutput("graphPlot", width='100%', height='800px')),
                       tabPanel('Parallel Coordinates', value='paracoord', plotOutput("paracoordPlot", width='100%', height='100%')),
                       tabPanel('Rules Table', value='datatable', DT::dataTableOutput("rulesDataTable"))
                     )
              )
                )),
      
      tabItem(tabName = "mcda",
              fluidRow(
                column(width = 3,
                       box(width = NULL, status = "info",
                           fileInput("datafile", "File input:")
                       ),
                       box(width = NULL, status = "info",
                           selectInput("txt", "Method:", choices = c('Electre Tri', 
                                                                     'Electre', 'Promethee'), selected = "Electre Tri")
                       ),
                      box(width = NULL, status = "info",
                           sliderInput("slider", "Slider input:", 1, 100, 30),
                           actionButton("action2", "Compute!", class = "btn-primary")
                       )
              ),
              column(width = 9,
                     tabBox(side = "left",width = 12,
                           tabPanel("Decision Matrix",dataTableOutput("filetable_DM1")),
                           tabPanel("Partial Concordance", dataTableOutput("partialConcordance_al_pr_gj")),
                           tabPanel("Global Concordance",dataTableOutput("globalconcordance")),
                           #tabPanel("PartialC_pr_al_gj", dataTableOutput("partialConcordance_pr_al_gj")),
                           tabPanel("Partial Discordance",dataTableOutput("partialDiscordance_al_pr_gj")),
                           #tabPanel("PartialD_pr_al_gj",dataTableOutput("partialDiscordance_pr_al_gj")),
                           
                           tabPanel("Credibility",dataTableOutput("credibility")),
                           tabPanel("Relations",dataTableOutput("relations")),
                           tabPanel("Assignments",dataTableOutput("assignment"))
                     )
                     )
              )
            ),
                         
      tabItem(tabName = "map",
              tabPanel("Interactive Map",
                       div(class="outer",
                           tags$head(
                             # Include our custom CSS
                             includeCSS("styles.css"),
                             includeScript("gomap.js")
                           ),
                           
                           leafletOutput("map", width="100%", height="100%"),
                           
                           # Shiny versions prior to 0.11 should use class="modal" instead.
                           absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                         draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                         width = 330, height = "auto",
                                         
                                         h2("Vehicle Collisions"),
                                         
                                         selectInput("color", "Color", vars),
                                         selectInput("size", "Size", vars2, selected = "NUMBER.OF.PERSONS.INJURED"),
                                         checkboxInput("cluster", "Add Cluster"),
                                         helpText("Cluster numbers show total accidents for each area", 
                                                  "(applies to all vehicles only)"),
                                         radioButtons("vehicle", "Show Just One Vehicle", vars3, selected = '')
                           ),
                           
                           tags$div(id="cite",
                                    'Data from: ', tags$em('Data Details of Vehicle Collisions'), '  | CNPAC Data. 
                                         Details of Vehicle Collisions in Marrakech City provided by the 
                                         Ministry of Equipment, Transport and Logistics of Morocco.'
                           )
                       )
              )
              
              ),
      
      tabItem(tabName = "tms",
              fluidRow(
                column(width = 3,
                       box(width = NULL, status = "info",
                             numericInput("months", label = "Months to Predict", 
                                          value = 12, min = 12, max = 144, step = 12)
                       ),
                       box(width = NULL, status = "info",
                             selectInput("interval", label = "Prediction Interval",
                                         choices = c("0.80", "0.90", "0.95", "0.99"),
                                         selected = "0.95")
                       ),
                       box(width = NULL, status = "info",
                             checkboxInput("showgrid", label = "Show Grid", value = TRUE),
                             actionButton("action2", "Search", class = "btn-primary")
                           )
                       ),
                column(width = 9,
                       tabBox(width = NULL,
                             tabPanel("Predicting The Injury", dygraphOutput("dygraph1")),
                             tabPanel("Predicting The Deaths", dygraphOutput("dygraph2"))
                       )
                ))
      ),
      
      tabItem(tabName = "about",
              fluidRow(
                column(width = 3,
                       box(width = NULL, status = "info",
                           h4("Data model for ARM"),
                           p("For general use of this app, you need to prepare your data with according to your needs, 
                            the data model look like this csv file"),
                           br(),
                           downloadButton("dataset_model", label = "Dataset model"),
                           br(),
                           br(),
                           
                           h4("Data model for MCDA"),
                           p("For general use of this app, you need to prepare your data according to your needs, 
                            the data model look like this csv file"),
                           br(),
                           downloadButton("decision_matrix_model", label = "Decision matrix model"),
                           br(),
                           br()
                       )
                ),
                column(width = 9,
                       box(width = NULL, status = "info",
                           h4("Abstract"),
                           p("Today’s ultra-connected world is generating massive volumes of data stored in database and cloud 
                             environment especially in logistics and transportation, these large data need to be analyzed in order 
                             to extract useful knowledge, and present it as a valid element for logistics managers for further use 
                             such as road safety, shipping delays and shipping optimization. The potential of data mining algorithms
                             is largely untapped, this paper shows large-scale techniques such as associations rule analysis and time
                             series to improve road safety by identifying hot-spots in advance and giving chance to drivers to avoid 
                             the dangers. Indeed, we proposed a framework based on association rules technique as a preliminary task 
                             to analyze and extract relationships between variables related to road accident, and then use multiple
                             criteria analysis to select relevant associations rules. The developed system is flexible and allows 
                             intuitive creation and execution of different algorithms for an extensive range of road traffic topics. 
                             DM-MCDA can be expanded with new topics on demand, rendering knowledge extraction more robust and provide 
                             meaningful information that could help in developing suitable policies for decision makers. "),
                           br(),
                           h4("Data Source"),
                           p("Description: ","Data Details of road accident in Morocco provided by CNPAC: National Committee 
                           for the Prevention of Traffic Accidents (Morocco)."),
                           p("Source: ",a("Data Details of rainfall | CNPAC",href="http://www.equipement.gov.ma")),

                           br(),
                           h4("Authors Information"),
                           p("¹Addi Ait-Mlouk, ²Tarik Agouti"),
                           p("¹Department of computing Science, Umeå University, Umeå, Sweden "),
                           p("²Cadi Ayyad University, Faculty of science semlalia, Marrakech, Morocco "),
                           br(),
                           h4("Aknowledgement"),
                           p("The authors would like to thank Jihane Mounji Manji for her support"),
                           h4("Maintainer"),
                           p("Email: aitmlouk@gmail.com")
                       )
                )
               ))
    ))
))