#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load libraries
library(shiny)
library(ggplot2)
library(DT) # Interactive tables

# Define the Holocaust Victims Dataset
holocaust_victims_data <- data.frame(
  Nationality = c("Jews", "Poles", "Other groups", "Roma (Gypsies)", "Soviet POWs"),
  NumberOfDeportees = c(1100000, 140000, 25000, 23000, 15000),
  PercentTotalDeportees = c(85, 10.8, 1.9, 1.6, 1.2),
  NumberOfVictims = c(1000000, 70000, 12000, 21000, 14000),
  PercentMurdered = c(90, 46, 48, 91.3, 93),
  PercentAllVictims = c(91, 5.8, 1, 1.7, 1.3)
)

# User Interface
ui <- fluidPage(
  titlePanel("Holocaust Victims at Auschwitz"),
  sidebarLayout(
    sidebarPanel(
      selectInput("nationality", "Select Nationality:",
                  choices = c("Jews", "Poles", "Other groups", "Roma (Gypsies)", "Soviet POWs"))
    ),
    mainPanel(
      plotOutput("nationalityBarPlot"),
      br(),
      plotOutput("categoryPlot"),
      br(),
      DTOutput("dataOverviewTable")
    )
  )
)

# Server Logic
server <- function(input, output) {
  
  # Bar Plot for Number of Deportees and Victims
  output$nationalityBarPlot <- renderPlot({
    ggplot(holocaust_victims_data, aes(x = Nationality)) +
      geom_bar(aes(y = NumberOfDeportees, fill = "Deportees"), stat = "identity", color = "blue") +
      geom_bar(aes(y = NumberOfVictims, fill = "Victims"), stat = "identity", color = "red") +
      ylab("Count") +
      ggtitle("Deportees & Victims by Nationality") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_manual(values = c("Deportees" = "blue", "Victims" = "red"),
                        guide = guide_legend(title = "Category"))
  })
  
  # Polar Plot based on Selected Category
  output$categoryPlot <- renderPlot({
    selected_data <- holocaust_victims_data[holocaust_victims_data$Nationality == input$nationality, ]
    
    ggplot(selected_data, aes(x = "", fill = Nationality)) +
      geom_bar(aes(y = NumberOfDeportees), stat = "identity", fill = "blue", width = 0.5) +
      geom_bar(aes(y = NumberOfVictims), stat = "identity", fill = "red", width = 0.5) +
      coord_polar(theta = "y") +
      labs(title = paste("Victims (Red) & Deportees (Blue):", input$nationality),
           fill = "Category") +
      scale_fill_manual(values = c("blue", "red")) +
      theme_minimal() +
      theme(axis.text.x = element_blank(),
            legend.position = "bottom")
  })
  
  # Data Table Overview
  output$dataOverviewTable <- renderDT({
    datatable(holocaust_victims_data[holocaust_victims_data$Nationality == input$nationality, ],
              options = list(lengthChange = FALSE, searching = FALSE))
  })
  
}

# Run the Shiny App
shinyApp(ui = ui, server = server)
