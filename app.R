library(shiny)
library(tidyverse)
library(caret)
library(ggplot2)
library(rpart.plot)
library(randomForest)
library(DT)

# Load the data
iris_data <- read.csv("flower.csv")

# Convert Species to factor
iris_data$Species <- as.factor(iris_data$Species)

# Split data
set.seed(123)
train_index <- createDataPartition(iris_data$Species, p = 0.8, list = FALSE)
train_data <- iris_data[train_index, ]
test_data <- iris_data[-train_index, ]

# Train Models
model_dt <- train(Species ~ ., data = train_data, method = "rpart")
model_rf <- train(Species ~ ., data = train_data, method = "rf")

# Predictions
predictions_dt <- predict(model_dt, test_data)
predictions_rf <- predict(model_rf, test_data)

# Confusion Matrices
conf_matrix_dt <- confusionMatrix(predictions_dt, test_data$Species)
conf_matrix_rf <- confusionMatrix(predictions_rf, test_data$Species)

# UI
ui <- navbarPage("Iris Flower Classification",
                 
                 tabPanel("Data View",
                          fluidPage(style = "background-color: #E0F7FA;",
                                    titlePanel("Iris Dataset Preview"),
                                    DTOutput("data_table")
                          )
                 ),
                 
                 tabPanel("EDA",
                          fluidPage(style = "background-color: #FFF3E0;",
                                    titlePanel("Exploratory Data Analysis"),
                                    plotOutput("boxplot"),
                                    plotOutput("barplot"),
                                    plotOutput("piechart")
                          )
                 ),
                 
                 tabPanel("Machine Learning",
                          fluidPage(style = "background-color: #E8F5E9;",
                                    titlePanel("ML Models"),
                                    selectInput("model_choice", "Choose ML Model:",
                                                choices = c("Decision Tree", "Random Forest")),
                                    verbatimTextOutput("model_summary"),
                                    plotOutput("tree_plot")
                          )
                 ),
                 
                 tabPanel("Model Evaluation",
                          fluidPage(style = "background-color: #F3E5F5;",
                                    titlePanel("Model Accuracy Evaluation"),
                                    verbatimTextOutput("conf_matrix_output")
                          )
                 )
)

# Server
server <- function(input, output) {
  
  output$data_table <- renderDT({
    iris_data
  })
  
  # EDA
  output$boxplot <- renderPlot({
    ggplot(iris_data, aes(x = Species, y = SepalLengthCm, fill = Species)) +
      geom_boxplot() +
      labs(title = "Boxplot of Sepal Length by Species", y = "Sepal Length (cm)") +
      theme_minimal()
  })
  
  output$barplot <- renderPlot({
    iris_data %>%
      count(Species) %>%
      ggplot(aes(x = Species, y = n, fill = Species)) +
      geom_bar(stat = "identity") +
      labs(title = "Count of Each Species", y = "Count") +
      theme_minimal()
  })
  
  output$piechart <- renderPlot({
    species_count <- iris_data %>%
      count(Species) %>%
      mutate(percent = round(100 * n / sum(n), 1))
    
    ggplot(species_count, aes(x = "", y = percent, fill = Species)) +
      geom_col(width = 1) +
      coord_polar("y") +
      geom_text(aes(label = paste0(percent, "%")), 
                position = position_stack(vjust = 0.5)) +
      labs(title = "Species Distribution (Pie Chart)") +
      theme_void()
  })
  
  # ML Model Output
  output$model_summary <- renderPrint({
    if (input$model_choice == "Decision Tree") {
      model_dt
    } else {
      model_rf
    }
  })
  
  output$tree_plot <- renderPlot({
    if (input$model_choice == "Decision Tree") {
      rpart.plot(model_dt$finalModel, main = "Decision Tree")
    } else {
      varImpPlot(model_rf$finalModel, main = "Random Forest Variable Importance")
    }
  })
  
  # Confusion Matrix Output
  output$conf_matrix_output <- renderPrint({
    if (input$model_choice == "Decision Tree") {
      conf_matrix_dt
    } else {
      conf_matrix_rf
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
