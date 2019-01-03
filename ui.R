
if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny,shinyWidgets,shinydashboard,DT,ggplot2,
ggthemes,leaflet,plotly,data.table,sf,mapview,mapedit,RColorBrewer,
rintrojs,utf8,stringr,bit64,plyr,dplyr,sp,crosstalk, htmltools, leaflet.extras)


#######Tabs############



listofparams=c("CADMIUM","COPPER","LEAD","MERCURY","NICKEL","SILVER","ZINC")

params <- paste(listofparams, collapse = "|")

#listofsites=as.vector(unique(metals$Basin))

diss=c("Total","Dissolved")


########  


sidebar <- dashboardSidebar(
  width = 325,
  sidebarMenu(
    fileInput("file1", "Choose CSV File",
              multiple = TRUE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    
    conditionalPanel(condition = "output.csv_import_ready",
    dateRangeInput(
      "dates",
      label = "Select Date Range",
      start = as.Date("2001-01-01"),
      end = Sys.Date(),
      min = as.Date("2001-01-01"),
      max = Sys.Date(),
      format = "yyyy-mm-dd",
      startview = "year",
      weekstart = 0,
      language = "en",
      separator = " to "
    ),
    #pickerInput("basin","Select a Basin", "", options = list(`actions-box` = TRUE),multiple = T),
    uiOutput("ui_basins"),
    selectInput("paramschoice","Filter by Metal",selected="Copper",listofparams,multiple=TRUE),
    sliderInput("samples",h3("Number of Samples"),min(1), max(100),value=c(1:20),step = 1)
    ),
    
    menuItem("Filtered Table", tabName = "metaltable", icon = icon("table"),badgeLabel = "Reactive", badgeColor = "black"),
    menuItem("Exceedances Table", tabName = "nosampletable", icon = icon("table")),
    menuItem("Map", icon = icon("map"), tabName = "widgets",badgeLabel = "Reactive", badgeColor = "black"),
    menuItem("Plots2", icon = icon("plot"), tabName = "plots2"),
    menuItem("Plots", icon = icon("plot"), tabName = "plots")
             #badgeLabel = "new", badgeColor = "green")
  )
)

body <- dashboardBody(
tabItems(
  #leafletOutput("mapplot"),
  #mapview:::plainViewOutput("test"),
  
    tabItem(tabName = "metaltable",
            uiOutput("downloadbutton"),
            br(),
            dataTableOutput("standards")
            
           
    ),
    tabItem(tabName = "nosampletable",
            h2("Exceedances by Date and Site"),
            dataTableOutput("exceed")
            #h2("Number of Sample Days"),
            #dataTableOutput("numsamples"),
            #h2("Adjusted Hardness"),
            #dataTableOutput("hardness")
    ),
    tabItem(tabName = "widgets",
            numericInput("exceedances","Minimum No.of Exceedances",value =1,min=1, max=22),
            leafletOutput("mymap",width = 995,height=575)
                    
    ),
    tabItem(tabName = "plots2",
            
            
            plotlyOutput("scatter2"),
            dataTableOutput("plotlyscatter2")
            #verbatimTextOutput("brush"),
            #  
            
    ),
    tabItem(tabName = "plots",
            #numericInput("exceedances","Minimum No.of Exceedances",value =1,min=1, max=22),
            #leafletOutput("mymap",width = 995,height=575)
            
            #div(style="display: inline-block;vertical-align:top; width: 150px;", uiOutput("ui_sites")),
            #div(style="display: inline-block;vertical-align:top; width: 150px;", uiOutput("ui_streams")),
          
            
            radioButtons("col","Switch Plot", choices = c("scatter", "bar"), selected = "scatter"),
            
    conditionalPanel(
            condition = "input.col == 'scatter'",  plotlyOutput("plot1")),
            conditionalPanel(
            condition = "input.col == 'bar'",  plotlyOutput("plot2"))
                     
    )
    
    ))
    


# Put them together into a dashboardPage
dashboardPage(
  dashboardHeader(title = "Metals App v1.0"),
  sidebar,
  body
)
