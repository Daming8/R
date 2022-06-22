library(shiny)
library(shiny.semantic)
library(semantic.dashboard)
library(plotly)
library(DT)
library(leaflet)
library(ggplot2)

data = read.csv("accident-dataV2.csv")

ui <- dashboardPage(
  dashboardHeader(dropdownMenuOutput("dropdown"),
                  dropdownMenu(type = "notifications",
                               taskItem("Project progress...", 50.777, color = "red")),
                  dropdownMenu(
                               notificationItem("This is an important notification!", color = "red"))),
  dashboardSidebar(side = "left",
                   sidebarMenu(
                     menuItem(tabName = "plot_tab", text = "Main Dashboard", icon = icon("home")),
                     menuItem(tabName = "table_tab", text = "Dataframe", icon = icon("browser")))),
  dashboardBody(
    tabItems(
      tabItem(tabName = "plot_tab",
              fluidRow(
                box(title = "Filters and Slicers", color = "orange", width = 4,
                    selectInput(inputId =  "accident severity", choices = unique(data$accident_severity),
                                label = "Accident Severity", selected = "Fatal"),
                    selectInput(inputId =  "weather", choices = unique(data$weather_conditions),
                                label = "Weather", selected = "Select"),
                ),
                box(color = "orange", width = 4,
                    selectInput(inputId =  "road type", choices = unique(data$road_type),
                                label = "Road Type", selected = ""),
                    selectInput(inputId =  "surface condition", choices = unique(data$road_surface_conditions),
                                label = "Surface Condition", selected = "")
                ),
                box(color = "orange", width = 4,
                    selectInput(inputId =  "light condition", choices = unique(data$light_conditions),
                                label = "Light Condition", selected = ""),
                    selectInput(inputId =  "hazards", choices = unique(data$carriageway_hazards),
                                label = "Hazards", selected = "Select")
                ),
                box(color = "orange", width = 4,
                    selectInput(inputId =  "urban", choices = unique(data$urban_or_rural_area),
                                label = "Urban or Rural", selected = "Fatal"),
                    selectInput(inputId =  "weather", choices = unique(df$weather_conditions),
                                label = "Weather", selected = "Select")
                )
              ),
              fluidRow(
                box(title = "Map plot", color = "purple", width = 11,
                    leafletOutput("mymap")),
                
                tabBox(title = "Sample box", color = "blue", width = 5,
                       collapsible = FALSE,
                       tabs = list(
                         list(menu = "First Tab", content = "Some text..."),
                         list(menu = "Second Tab", content = plotlyOutput("plot1"))
                       ))),
              
              ),
      tabItem(tabName = "table_tab",
              fluidRow(
                valueBox("Fatal", nrow(data[data$accident_severity=="Fatal",]),  color = "red", width = 6, size = "big"),
                valueBox("Serious", nrow(data[data$accident_severity=="Serious",]),  color = "yellow", width = 5, size = "big"),
                valueBox("Slight", nrow(data[data$accident_severity=="Slight",]), color = "green", width = 5, size = "big")
              ),
              fluidRow(
                box(title = "Data Frame", color = "blue", ribbon = FALSE,
                    title_side = "top left", width = 16,
                    tags$div(
                      dataTableOutput("table1")
                      , style = paste0("color:", semantic_palette[["blue"]], ";"))
                ))))
  ), theme = "slate"
)

server <- function(input, output) {

  output$plot1 <- renderPlotly(plot_ly(data, x = ~ data[ , data$day_of_week],
                                             y = ~ nrow(data),
                                             type = "bar")
  )
  output$mtcars_plot2 <- renderPlotly(plot_ly(mtcars, x = ~ mtcars[ , input$variable1],
                                              y = ~ mtcars[ , input$variable2],
                                              type = "scatter", mode = "markers"))
  
  output$table1 <- renderDataTable(data, options = list(dom = 't'))
  
  output$dropdown <- renderDropdownMenu({
    dropdownMenu(messageItem("User", "Test message", color = "teal", style = "min-width: 200px"),
                 messageItem("Users", "Test message", color = "teal", icon = "users"),
                 messageItem("See this", "Another test", icon = "warning", color = "red"))
  })
  
  dt <- reactive({
    
    data[data$accident_severity == as.character(input$accident_severity) &
           data$day_of_week == as.character(input$day_of_week) &
           data$weather_conditions == as.character(input$weather_conditions) &
           data$speed_limit == as.numeric(input$speed_limit),
    ]
  })
  
  output$mymap <- renderLeaflet({
    dat <- dt()[,c(4,5)]
    leaflet() %>% setView(lng = -1.1893, lat = 52.35, zoom = 6) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addCircleMarkers(lng = dat$longitude, lat = dat$latitude, radius = 4) 
  })
}

shinyApp(ui, server)