
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Anonimyzed/randomized data with the structure 
# of a mobility data frame with information by lines
# and routes. 

load(file="test_dashboard_data1.Rda")

df<- aggregate(list("CASH_TICKETS" = tdf$BOLETO,
                    "PREPAID" = tdf$PREPAGO,
                    "TRANSFERS" = tdf$TRANSFER,
                    "KMS" = as.numeric(tdf$KMS)),
               by = list("DATE" = as.Date(tdf$HORA_SALIDA),
                         "ROUTE" = tdf$RUTA,
                         "LINE" = tdf$LINEA),
               sum, na.rm = TRUE)


library(shiny)
library(plotly)
library(ggplot2)

ui <- fluidPage(

  titlePanel("Mobility dashboards"),

  sidebarLayout(

    sidebarPanel(
      radioButtons("shown_data", "Shown data:",
                   c("Total" = "total",
                     "per KM" = "km")),
      br(),
      checkboxGroupInput("type_mobility", 
                         h3("Mobility type"), 
                         choices = list("Cash tickets" = "CASH_TICKETS", 
                                        "Prepaid" = "PREPAID", 
                                        "Transfers" = "TRANSFERS"),
                         inline = TRUE),
      br(),
      dateRangeInput("dates", h3("Date range")),
      br(),
      checkboxGroupInput("lines", 
                         h3("Lines"), 
                         choices = list("GEN" = "GEN", 
                                        "NORTE" = "NORTE", 
                                        "STAR" = "STAR",
                                        "TORO" = "TORO",
                                        "VX" = "VX")),
      br(),
      downloadButton('down_csv','Download table')
    ),
    

    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Daily mobility",plotlyOutput("plot_mob"))
                  ,tabPanel("Table",tableOutput("table"))
      )
      
    )
  )
)

server <- function(input, output) {
  
  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression
  
  
  db <- reactive({
    db<-df[,c("DATE","ROUTE","LINE","KMS",input$type_mobility)]
    if(length(input$type_mobility)==1){
      db$MOBILITY <- db[,input$type_mobility]
    }else{
      db$MOBILITY <- rowSums(db[,input$type_mobility])
    }
    db<-db[db$DATE >= input$dates[1] & db$DATE <= input$dates[2],
           c("DATE","ROUTE","LINE","MOBILITY","KMS")]
    db$DATE <- as.character(as.Date(db$DATE,format = "%Y%m%d"))
    db<-db[db$LINE %in% input$lines,]
    if(input$shown_data == "km"){
      db$MOBILITY <- db$MOBILITY / db$KMS
    }
    db$KMS<-NULL
    db
  })
  
  plot_mob<-reactive({
    db() %>%
      plot_ly(x=~DATE, y=~MOBILITY, group=~ROUTE,
              type="scatter",color=~ROUTE, mode="lines+markers") %>%
      config(
        modeBarButtonsToRemove = list(
          "zoom2d",
          "pan2d",
          "zoomIn2d",
          "zoomOut2d",
          "autoScale2d",
          "resetScale2d",
          "hoverClosestCartesian",
          "hoverCompareCartesian",
          "sendDataToCloud",
          "toggleHover",
          "resetViews",
          "toggleSpikelines",
          "resetViewMapbox"
        ),
        displaylogo = FALSE)
  })
  
  
  # Generate an HTML table view of the data ----
  output$table <- renderTable({
    xtable::xtable(db())
  })
  
  output$plot_mob <-renderPlotly({
    plot_mob()
  })
  
  output$down_csv <- downloadHandler(
    filename = "data.csv"
    ,
    content = function(file) {
      write.csv(db(), file)
    }
  )

  
}

# Create Shiny app ----
shinyApp(ui, server)