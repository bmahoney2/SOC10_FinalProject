
# Welcome to the interactive quiz! We based each question on the most important feature variables from the lasso model. Using the Shiny
# package in R, we created a prediction model based on real-time responses. The user simply fills out each of the questions, and the model
# computes their predicted probability of diabetes based on those responses. We considered anything below 20% as low risk, 20-50% moderate
# risk, and anything above 50% as high risk. Most notably, we intend this quiz to be used as an exploratory tool, not a tool for personal
# diagnosis. All conclusions drawn from this quiz should be used at the discretion of a medical professional. 

library(shiny)
library(tidymodels)
library(dplyr)
library(glmnet)
glmnet::glmnet

# Load the trained lasso model
model <- readRDS("lasso_model.rds")

# make the interface with the following questions 
ui <- fluidPage(
  
# Title
  titlePanel("Diabetes Risk Calculator"),
  
# Create a side panel with all of the questions
  sidebarLayout(
    sidebarPanel(
      h4("Answer the following questions:"),
      
# Add all respective questions

# All binary (yes/no) questions first:
      radioButtons("HighBP", "Have you been told you have high blood pressure?",
                   choices = c("No" = 0, "Yes" = 1), selected = 0),

      radioButtons("HvyAlcoholConsump", "Are you a heavy alcohol drinker?",
             choices = c("No" = 0, "Yes" = 1), selected = 0),

      radioButtons("HeartDiseaseorAttack", "Have you ever had a heart disease or heart attack?",
             choices = c("No" = 0, "Yes" = 1), selected = 0),
    
# radioButtons("CholCheck", "Have you had your cholesterol checked in the last 5 years?",
# choices = c("No" = 0, "Yes" = 1), selected = 1),
      
      radioButtons("HighChol", "Have you ever been told you have high cholesterol?",
             choices = c("No" = 0, "Yes" = 1), selected = 0),
     
# Manual question:
      numericInput("BMI", "What is your BMI?", value = 25, min = 10, max = 100),


# Likerd scale questions: 
      sliderInput("GenHlth", "On a scale of 1-5, how would you rate your general health?",
                  min = 1, max = 5, value = 3, step = 1),
      helpText("1 = Excellent, 2 = Very Good, 3 = Good, 4 = Fair, 5 = Poor"),
      
      sliderInput("Age", "What is your age group?",
                  min = 1, max = 13, value = 5, step = 1),
      helpText("1=18-24, 2=25-29, 3=30-34, 4=35-39, 5=40-44, 6=45-49, 7=50-54, 8=55-59, 9=60-64, 10=65-69, 11=70-74, 12=75-79, 13=80+"),
  
      sliderInput("Income", "What is your annual household income?",
            min = 1, max = 8, value = 5, step = 1),
      helpText("1 = <$10k, 4 = $25-35k, 8 = $75k+"),
      
# Prompt the user to submit responses
    actionButton("submit", "Calculate My Risk")
    ),
    
# Display probability of diabetes in real-time
    mainPanel(
      h2("Estimated Diabetes Risk"),
      uiOutput("riskOutput")
    )
  )
)

# Assign values to all 21 features
# Only use the features from the lasso (impute the rest as zero)
server <- function(input, output, session) {
  session$allowReconnect(TRUE)  # add this line
  
  observeEvent(input$submit, {
    tryCatch({
      new_data <- tibble(
        HighBP = as.numeric(input$HighBP),
        HighChol = as.numeric(input$HighChol),
        CholCheck = 0,
        BMI = as.numeric(input$BMI),
        Smoker = 0,
        Stroke = 0,
        HeartDiseaseorAttack = as.numeric(input$HeartDiseaseorAttack),
        PhysActivity = 1,
        Fruits = 1,
        Veggies = 1,
        HvyAlcoholConsump = as.numeric(input$HvyAlcoholConsump),
        AnyHealthcare = 1,
        NoDocbcCost = 0,
        GenHlth = as.numeric(input$GenHlth),
        MentHlth = 0,
        PhysHlth = 0,
        DiffWalk = 0,
        Sex = 0,
        Age = as.numeric(input$Age),
        Education = 5,
        Income = as.numeric(input$Income)
      )
      
      # make a real time prediction using these results
      pred <- predict(model, new_data = new_data, type = "prob")
      prob <- pred$.pred_1
      
      # convert to a percent
      pct <- round(prob * 100, 1)
      
      # Label diabetes risk as low, moderate, and high risk
      if (pct < 20) {
        color <- "green"
        band <- "Low Risk"
      } else if (pct < 50) {
        color <- "orange"
        band <- "Moderate Risk"
      } else {
        color <- "red"
        band <- "Elevated Risk"
      }
      
      # Output the results 
      output$riskOutput <- renderUI({
        tagList(
          h1(paste0(pct, "%"), 
             # assign green, orange, or red for each prediction
             style = paste0("color:", color, "; font-size:72px;")),
          
          h3(band, style = paste0("color:", color)),
          p(paste0("Estimated diabetes risk: ", pct, "%")),
          p("**DISCLAIMER:** this tool is NOT to be used as a proxy for making medical diagnoses. All results should be interpreted
          at the discresion of a medical professional.")
        )
      })
    })
  })
}

# connect to the server and load app!
shinyApp(ui = ui, server = server)