# Loading required libraries
library(shiny)
library(tidyverse)
library(readr)
library(dplyr)
library(plotly)
library(DT)
# library(rsconnect)

# load data set
sat <- read.csv("SAT Report 2015-2016.csv")
naRemoved <- sat %>% 
  filter(!is.na(sat$cname),!is.na(as.numeric(sat$AvgScrRead)),
         !is.na(as.numeric(sat$AvgScrMath)), !is.na(as.numeric(sat$AvgScrWrit)))
county <- unique(naRemoved$cname)

data = read.csv("SAT Report 2015-2016.csv",sep=",")
data = data[data$index!=0,]
data[,7:14] <- lapply(data[,7:14],as.numeric)

ui <- fluidPage(
  titlePanel("California SAT Scores Evaluation"),
  tabsetPanel(
    # First tab of the app
    tabPanel("Main Page",
             p(strong("General Info:"), style = "font-size:20px;"),
             p(em("The California Department of Education colleced this from the College Board and have been 
               aggregated to generate school, district, county and state-level reports about 
               students' SAT scores.")),
             uiOutput("tab")
             
    ),
    
    tabPanel("Number of SAT takers",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "level", label = "Select level:", 
                             choices = c("County", "District"))
               ),
               mainPanel(
                 plotlyOutput(outputId = "plot2"),
                 textOutput("text2"),
                 tags$head(tags$style("#text2{font-size: 20px;}"))
               )
             )      
    ),
  
    tabPanel("High Scorers",
             sidebarLayout(
               sidebarPanel(radioButtons("var2","choose district or county:",
                                         c("dname" ,"cname"))),
               mainPanel(
                 #show the result in each panel
                 plotlyOutput("plot3"),
                 textOutput("text3"),
                 tags$head(tags$style("#text3{font-size: 20px;}"))
               )
             )
    )
    
  )
)

server <- function(input, output) {

  # filter data based on user input
  filtered_data <- reactive({
    if (input$level == "County") {
      naRemoved %>%
        group_by(cname) %>%
        summarize(Total = (NumTstTakr))
    } else {
      naRemoved %>%
        group_by(dname) %>%
        summarize(Total = (NumTstTakr))
    }
  })
  # Create plotly bar chart
  output$plot2 <- renderPlotly({
    plot_ly(data = filtered_data(), x = ~Total, y = ~as.character(filtered_data()[[1]]),
            type = "bar", orientation = "v") %>%
      layout(xaxis = list(title = "Total Test Takers"),
             yaxis = list(title = input$level))
  })
  output$text2 <- renderPrint({
    if (input$level == "County") {
      cat("This graph shows the total number of test takers for each county in California.")
    } else {
      cat("This graph shows the total number of test takers for each district in California.")
    }
  })
  

  output$plot3 <- renderPlotly({
      data1 <- data%>%select(input$var2,PctGE1500,NumGE1500)
      colnames(data1)[1] <- "var"
      ggplotly(ggplot(data=data1,aes(y=NumGE1500, x=PctGE1500)) +
                 geom_point(alpha=0.5))
    
  })
  output$text3 <- renderPrint({
    if (input$var2 == "dname") {
      cat("This plot shows the number of people in each district that scored 1500.")
    } else {
      cat("This plot shows the number of people in each county that scored 1500.")
    }
  })
  

}

shinyApp(ui = ui, server = server)