library(shiny)
library(shinydashboard)
library(ggplot2) 
library(readr) 
library(dplyr) 
library(DT)
library(corrplot)
library(randomForest)
library(class)
library(patchwork)
library(caret)
library(pROC)
library(PRROC)
library(rsample)
library(tidyr)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Liver Disease Diagnosis Analysis"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("sidebar"),
      width = 3 
    ),
    mainPanel(
      tabsetPanel(id = "mainPanel",
                  tabPanel("Data Overview", value = "overview", DTOutput("dataView")),
                  tabPanel("EDA", value = "eda",
                           uiOutput("edaPlotChoices"), 
                           plotOutput("edaPlot")),
                  tabPanel("Model Performance", value = "model_perf", plotOutput("rocPlot")),
                  tabPanel("Prediction", value = "predict", verbatimTextOutput("predictionOutput")),

                  selected = "overview"
      ),
      width = 9
    )
  ),
  tags$style(HTML("
    .shiny-output-error { 
      color: #000; 
      background-color: #f2f2f2; 
    }
    .shiny-output-error:before { 
      content: 'Error: '; 
    }
  ")) 
)


df <- read.csv("./Data/Indian Liver Patient Dataset (ILPD).csv")
colnames(df) <- c("age", "gender", "tb", "db", "alkphos" ,"sgpt", "sgot" ,"tp" ,"alb" ,"agratio" ,"selector" )
liver_data = df

for(col in names(df)) {
  if(class(df[[col]]) %in% c("integer", "numeric")) {
    df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
  } else if(class(df[[col]]) == "factor") {
    mode <- names(sort(table(df[[col]]), decreasing = TRUE))[1]
    df[[col]][is.na(df[[col]])] <- mode
  }
}
df$selector <- df$selector - 1
df$gender <- as.factor(df$gender)
df$selector <- as.factor(df$selector)
set.seed(123)
trainIndex <- createDataPartition(df$selector, p = 0.7, list = FALSE, times = 1)
train_df <- df[trainIndex, ]
test_df <- df[-trainIndex, ]
data = df
data$ratio_tb_db <- data$tb / data$db
data$diff_tb_db <- data$tb - data$db

data$interaction_sgpt_sgot <- data$sgpt * data$sgot

data$alkphos_sq <- data$alkphos^2
data$sgpt_sq <- data$sgpt^2

data$norm_alkphos <- (data$alkphos - min(data$alkphos)) / (max(data$alkphos) - min(data$alkphos))
data$std_sgpt <- (data$sgpt - mean(data$sgpt)) / sd(data$sgpt)

data$log_sgpt <- log(data$sgpt + 1)

data$age_group <- cut(data$age, breaks=c(0, 20, 40, 60, 80, 100), labels=c("0-20", "21-40", "41-60", "61-80", "81-100"))
data$age_group <- as.factor(data$age_group)
data = data[, c("selector", "ratio_tb_db", "diff_tb_db", "interaction_sgpt_sgot", "alkphos_sq", "sgpt_sq", "norm_alkphos", "std_sgpt", "log_sgpt")]
trainIndex <- createDataPartition(data$selector, p = 0.7, list = FALSE, times = 1)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

tuning_grid <- expand.grid(
  mtry = c(2, round(sqrt(ncol(train_data)-1)), round(ncol(train_data)/3))
)

train_control <- trainControl(
  method = "cv",         
  number = 10,            
  search = "grid"         
)

model_rf <- train(
  selector ~ .,
  data = train_data,
  method = "rf",              
  trControl = train_control,
  tuneGrid = tuning_grid,
  metric = "Accuracy"
)
prob_predictions <- predict(model_rf, newdata = test_data, type = "prob")
binary_predictions <- ifelse(prob_predictions[, 2] > 0.5, 1, 0)
roc_curverf <- roc(response = as.numeric(test_data$selector)-1, predictor = as.numeric(binary_predictions))

preProcValues <- preProcess(data[, -which(names(data) == "selector")], method = c("center", "scale"))
data_scaled <- predict(preProcValues, data)
data_scaled$selector <- data$selector

trainIndex <- createDataPartition(data_scaled$selector, p = 0.7, list = FALSE, times = 1)
train_df_scaled <- data_scaled[trainIndex, ]
test_df_scaled <- data_scaled[-trainIndex, ]
knn_fit <- train(selector ~ ., data = train_df_scaled, method = "knn",
                 trControl = trainControl(method = "cv", number = 10),
                 preProcess = "scale",
                 tuneLength = 10)  
knn_predictions <- predict(knn_fit, newdata = test_df_scaled)

roc_curveknn <- roc(response = as.numeric(test_df_scaled$selector)-1, predictor = as.numeric(knn_predictions)-1)



server <- function(input, output, session) {
  output$sidebar <- renderUI({
    if (input$mainPanel == "predict") {
      tagList(
        numericInput("age", "Age", 30),
        selectInput("gender", "Gender", choices = c("Male", "Female")),
        numericInput("tb", "Total Bilirubin", 0.7),
        numericInput("db", "Direct Bilirubin", 0.1),
        numericInput("alkphos", "Alkaline Phosphatase", 85),
        numericInput("sgpt", "SGPT", 35),
        numericInput("sgot", "SGOT", 35),
        numericInput("tp", "Total Protein", 6.8),
        numericInput("alb", "Albumin", 3.5),
        numericInput("agratio", "Albumin/Globulin Ratio", 1),
        actionButton("predictButton", "Make Prediction", class = "btn-primary")
      )
    } else if (input$mainPanel == "model_perf") {
      tagList(
        selectInput("modelType", "Select Model Type", choices = c("All Models", "Random Forest", "KNN")),
        actionButton("updateModel", "Update Model Performance", class = "btn-success")
      )
    } else if (input$mainPanel == "eda") {
      tagList(
        selectInput("analysisType", "Choose Analysis Type", choices = c("Univariate", "Multivariate"))
      )
    } else if (input$mainPanel == "overview") {
      tagList(
        h3("Data Overview Information"),
        p("In this project, I will investigate a dataset comprising various liver function test results to extract critical insights into liver disease diagnosis. By employing exploratory data analysis (EDA) and basic machine learning (ML) modeling techniques")
      )
    }
  })
  
  output$dataView <- renderDT({
    datatable(df)
  })
  
  output$edaPlotChoices <- renderUI({
    if (input$analysisType == "Univariate") {
      selectInput("edaPlotType", "Select EDA Plot:",
                  choices = c("Age Distribution", "Gender Distribution", "Total Bilirubin Distribution", "Density Plots"))
    } else if (input$analysisType == "Multivariate") {
      selectInput("edaPlotType", "Select EDA Plot:",
                  choices = c("Age vs Total Bilirubin", "Albumin and Globulin Ratio by Gender", "Gender Distribution by Disease Presence", "Age Distribution by Disease Presence", "Correlation Plot")) 
    }
  })
  output$edaPlot <- renderPlot({
    if (input$mainPanel == "eda" && input$analysisType == "Univariate") {
      if (input$edaPlotType == "Age Distribution") {
        ggplot(data = liver_data, aes(x = age)) +
          geom_histogram(binwidth = 5, fill = "#1999f1", color = "black") +
          labs(title = "Distribution of Age", x = "Age", y = "Frequency") +
          theme_minimal()
      } else if (input$edaPlotType == "Gender Distribution") {
        ggplot(data = liver_data, aes(x = gender)) +
          geom_bar(fill = "#1199b1", color = "black") +
          labs(title = "Gender Distribution", x = "Gender", y = "Count") +
          theme_minimal()
      } else if (input$edaPlotType == "Total Bilirubin Distribution") {
        ggplot(data = liver_data, aes(x = tb)) +
          geom_histogram(binwidth = 0.5, fill = "orange", color = "black") +
          labs(title = "Distribution of Total Bilirubin", x = "Total Bilirubin (mg/dL)", y = "Frequency") +
          theme_minimal()
      } else if (input$edaPlotType == "Density Plots") {
        selected_columns <- c("age", "tb", "db", "alkphos", "sgpt", "sgot", "tp", "alb")
        df_selected <- liver_data[selected_columns]
        df_long <- tidyr::pivot_longer(df_selected,
                                       cols = everything(),
                                       names_to = "variable", 
                                       values_to = "value")
        ggplot(df_long, aes(x = value, fill = variable)) +
          geom_density(alpha = 0.6) +
          scale_fill_viridis_d() +
          labs(x = "Value", y = "Density", fill = "Variable") +
          facet_wrap(~variable, scales = "free") +
          theme_minimal() +
          ggtitle("Density Plots for Numeric Variables")
      }
    }
    else if (input$mainPanel == "eda" && input$analysisType == "Multivariate") {
      if (input$edaPlotType == "Age vs Total Bilirubin") {
        ggplot(data = liver_data, aes(x = age, y = tb, color = factor(selector))) +
          geom_point(alpha = 0.6) +
          labs(title = "Age vs Total Bilirubin by Disease Presence", x = "Age", y = "Total Bilirubin (mg/dL)", color = "Disease Presence") +
          theme_minimal()
        
      } else if (input$edaPlotType == "Albumin and Globulin Ratio by Gender") {
        ggplot(data = liver_data, aes(x = gender, y = agratio, fill = gender)) +
          geom_boxplot() +
          labs(title = "Albumin and Globulin Ratio by Gender", x = "Gender", y = "A/G Ratio") +
          theme_minimal()
      } else if (input$edaPlotType == "Correlation Plot") {
        corrplot(cor(df[c("age", "tb", "db", "alkphos", "sgpt", "sgot", "tp", "alb")]), 
                 method="color",col=colorRampPalette(c("#df1911", "#df5d00", "#F5E3B3","#D9fdd9","#C8F3B3" ))(100),type="upper",tl.srt=90,tl.col="black")
      } else if (input$edaPlotType == "Age Distribution by Disease Presence") {
        age_disease_plot <- ggplot(data = liver_data, aes(x = age, fill = factor(selector))) +
          geom_histogram(position = "identity", alpha = 0.6, bins = 30) +
          labs(title = "Age Distribution by Disease Presence",
               x = "Age",
               y = "Count",
               fill = "Disease Presence") +
          theme_minimal()
        print(age_disease_plot)
      } else if (input$edaPlotType == "Gender Distribution by Disease Presence") {
        gender_disease_plot <- ggplot(data = liver_data, aes(x = factor(gender), fill = factor(selector))) +
          geom_bar(position = "dodge") +
          scale_fill_manual(values = c("1" = "red", "2" = "blue")) +
          labs(title = "Gender Distribution by Disease Presence",
               x = "Gender",
               y = "Count",
               fill = "Disease Presence") +
          theme_minimal()
        print(gender_disease_plot)
      }
    }
  })
  
  # Model performance output
  roc_curve_rf <- reactive({
    roc_curverf
  })
  
  roc_curve_knn <- reactive({
    roc_curveknn
  })
  
  filteredROCData <- reactive({
    if (input$modelType == "Random Forest") {
      data.frame(FPR = roc_curverf$specificities, TPR = roc_curverf$sensitivities, Model = "Random Forest")
    } else if (input$modelType == "KNN") {
      data.frame(FPR = roc_curveknn$specificities, TPR = roc_curveknn$sensitivities, Model = "KNN")
    } else {
      rbind(
        data.frame(FPR = roc_curverf$specificities, TPR = roc_curverf$sensitivities, Model = "Random Forest"),
        data.frame(FPR = roc_curveknn$specificities, TPR = roc_curveknn$sensitivities, Model = "KNN")
      )
    }
  })
  
  output$rocPlot <- renderPlot({
    roc_df <- filteredROCData()
    ggplot(roc_df, aes(x = 1-FPR, y = TPR, color = Model)) +
      geom_line() +
      geom_abline(linetype = "dashed", color = "gray") +  
      labs(title = "ROC Curve Comparison", x = "False Positive Rate", y = "True Positive Rate") +
      scale_color_manual(values = c("red", "blue")) +  
      theme_minimal() +
      coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))  
  })
  
  # Prediction output
  preprocess_input <- function(data) {
    data$ratio_tb_db <- data$tb / data$db
    data$diff_tb_db <- data$tb - data$db
    
    data$interaction_sgpt_sgot <- data$sgpt * data$sgot
    
    data$alkphos_sq <- data$alkphos^2
    data$sgpt_sq <- data$sgpt^2
    
    data$norm_alkphos <- data$alkphos
    data$std_sgpt <- data$sgpt
    
    data$log_sgpt <- log(data$sgpt + 1)
    
    data$age_group <- cut(data$age, breaks=c(0, 20, 40, 60, 80, 100), labels=c("0-20", "21-40", "41-60", "61-80", "81-100"))
    data$age_group <- as.factor(data$age_group)
    
    return(data)
  }
    observeEvent(input$predictButton, {
      user_data <- data.frame(
        age = as.numeric(input$age),
        gender = as.factor(input$gender),
        tb = as.numeric(input$tb),
        db = as.numeric(input$db),
        alkphos = as.numeric(input$alkphos),
        sgpt = as.numeric(input$sgpt),
        sgot = as.numeric(input$sgot),
        tp = as.numeric(input$tp),
        alb = as.numeric(input$alb),
        agratio = as.numeric(input$agratio),
        stringsAsFactors = FALSE
      )
      user_data <- preprocess_input(user_data)
      user_data = user_data[, c( "ratio_tb_db", "diff_tb_db", "interaction_sgpt_sgot", "alkphos_sq", "sgpt_sq", "norm_alkphos", "std_sgpt", "log_sgpt")]
      
      
      if (any(is.na(user_data))) {
        output$predictionOutput <- renderText("Please complete all input fields.")
      } else {
        prediction <- predict(model_rf, newdata = user_data, type = "prob")  # or "class"
        output$predictionOutput <- renderText(paste("Predicted Probability of having a Liver Disease: ", prediction[][1]))
        print(prediction)
      }
    })
  
}


shinyApp(ui = ui, server = server)




