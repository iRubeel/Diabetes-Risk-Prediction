#1
# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(patchwork)
library(caret)
library(pROC)


# Read the data from the Excel file
data <- read_excel("diabetes.xlsx")

# Get a summary of the dataset
summary(data)

# Check for missing values
sum(is.na(data))

# View the first few rows of the dataset
head(data)



#2
library(mice)
# Replace zeros with NA for specific columns
cols_to_replace <- c("Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI")
data[cols_to_replace] <- lapply(data[cols_to_replace], function(x) replace(x, x == 0, NA))

# Impute missing values using mice
imputed_data <- mice(data, m=5, method='pmm', seed=500)

# Complete the data by filling in the NAs with the imputed values
completed_data <- complete(imputed_data)

# Convert 'Outcome' from 'Y' and 'N' to 1 and 0
completed_data$Outcome <- ifelse(completed_data$Outcome == "Y", 1, 0)


#3

library(ggplot2)
library(patchwork)

# Assuming 'completed_data' is the dataset after imputation and conversion of the 'Outcome' variable.
completed_data$Outcome <- as.factor(completed_data$Outcome)

# A list to store all the plots
plot_list <- list()

for (feature in features) {
  p <- ggplot(completed_data, aes_string(x=feature, fill="Outcome")) +
    geom_density(alpha=0.7) +
    labs(title=paste("Density Plot of", feature), x=feature) +
    theme_minimal() +
    scale_fill_discrete(name="Outcome") +
    theme(legend.position="bottom")
  
  # Add the plot to the list
  plot_list[[feature]] <- p
}

# Combine all the plots. The `wrap_plots` function can be used to specify the number of columns or rows you want.
combined_plot <- wrap_plots(plot_list, ncol = 3)

# Print the combined plot
print(combined_plot)



features <- c("Age", "BloodPressure", "BMI", "Glucose", "Insulin", "Pregnancies", "SkinThickness", "DiabetesPedigreeFunction")

# A list to store all the plots
plot_list <- list()

# Loop through features to create histograms
for (feature in features) {
  p <- ggplot(completed_data, aes_string(x=feature)) +
    geom_histogram(bins=30, fill='blue', alpha=0.7) +
    labs(title=paste("Histogram of", feature), x=feature) +
    theme_minimal()
  
  # Add the plot to the list
  plot_list[[feature]] <- p
}

# Combine all the plots. Adjust 'ncol' to fit your number of features or desired layout
combined_histograms <- wrap_plots(plot_list, ncol = 3)

# Print the combined histograms
print(combined_histograms)


# Load the necessary library for correlation matrix heat map
library(corrplot)

# Correlation Matrix Heat Map
# Ensure that 'completed_data' only includes numeric columns for the correlation matrix
numeric_data <- completed_data[sapply(completed_data, is.numeric)]
cor_matrix <- cor(numeric_data, use="complete.obs") # 'use="complete.obs"' to handle any remaining NAs
corrplot(cor_matrix, method = "color", tl.cex = 0.8, title="Correlation Matrix", type="upper")

# Count of patients with and without Diabetes
p <- ggplot(completed_data, aes(x=Outcome)) +
  geom_bar(fill=c("blue", "red")) +
  labs(x="Outcome", title="Count of patients with and without Diabetes") +
  theme_minimal()
print(p) # Print the bar plot in RStudio's Plots pane






#4
library(caret)

# Set seed for reproducibility
set.seed(123)

# Split the data into training and testing sets (75:25)
splitIndex <- createDataPartition(completed_data$Outcome, p = 0.75, list = FALSE)
trainingData <- completed_data[splitIndex, ]
testingData <- completed_data[-splitIndex, ]

# Check the number of cases in each set
nrow(trainingData)
nrow(testingData)




#5
# Build a linear model with multiple predictors
linear_model <- lm(Glucose ~ BMI + BloodPressure + Age + Outcome, data = trainingData)

# Summarize the linear model
summary(linear_model)

# Plotting the linear model
ggplot(trainingData, aes(x=Outcome, y=Glucose)) +
  geom_point() +
  geom_smooth(method="lm", col="blue") +
  labs(title="Linear Model of Glucose by Outcome", x="Outcome", y="Glucose")





#6
# Build the logistic regression model using the specified variables
logistic_model <- glm(Outcome ~ BMI + Glucose + SkinThickness + BloodPressure + Insulin + DiabetesPedigreeFunction + Age, 
                      data=trainingData, family="binomial")

# Summarize the logistic regression model
summary(logistic_model)

# Predict on testing set
predictions <- predict(logistic_model, newdata=testingData, type="response")

# Convert probabilities to binary outcome
predicted_class <- ifelse(predictions > 0.5, 1, 0)

# Confusion Matrix to get accuracy, sensitivity, etc.
conf_matrix <- table(Predicted=predicted_class, Actual=testingData$Outcome)

# Calculate metrics
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
sensitivity <- conf_matrix[2,2] / sum(conf_matrix[2,])
specificity <- conf_matrix[1,1] / sum(conf_matrix[1,])

# Print the metrics
print(paste("Accuracy:", accuracy))
print(paste("Sensitivity:", sensitivity))
print(paste("Specificity:", specificity))

# ROC Curve
library(pROC)
roc_curve <- roc(response=testingData$Outcome, predictor=predictions)
plot(roc_curve)
auc(roc_curve)

#7
library(caret)
library(pROC)

# Step 1: Feature Selection using Recursive Feature Elimination (RFE)
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
rfe_model <- rfe(trainingData[, -ncol(trainingData)], trainingData$Outcome, sizes=c(1:5), rfeControl=control)

# View the results of RFE
print(rfe_model)
selected_vars <- predictors(rfe_model)
print(selected_vars)

# Step 2: Check for imbalance in the outcome variable
table(trainingData$Outcome)


# Step 4: Regularization and Hyperparameter tuning using train function from caret
train_control <- trainControl(method="repeatedcv", number=10, repeats=3, classProbs=TRUE, summaryFunction=twoClassSummary)
grid <- expand.grid(.lambda=seq(0,0.1,by=0.01), .alpha=c(0,0.5,1))

# Ensure that the factor levels of 'Outcome' are valid R variable names
trainingData$Outcome <- as.factor(trainingData$Outcome)
levels(trainingData$Outcome) <- make.names(levels(trainingData$Outcome))

# Do the same for the testing data since you will be predicting on this dataset
testingData$Outcome <- as.factor(testingData$Outcome)
levels(testingData$Outcome) <- make.names(levels(testingData$Outcome))

# Now retry training the model
set.seed(123)
logistic_model <- train(Outcome ~ ., data=trainingData[, c(selected_vars, "Outcome")], method="glmnet",
                        trControl=train_control, tuneGrid=grid, metric="ROC")


# Step 5: Evaluate the final model using pROC functions
final_predictions <- predict(logistic_model, newdata=testingData, type="prob")[,2]

# Create an ROC curve object using the roc function from pROC
roc_curve <- roc(response = testingData$Outcome, predictor = final_predictions)

# Calculate AUC
final_auc <- auc(roc_curve)
print(paste("AUC for final model:", final_auc))

# Plot ROC Curve for the final model
plot(roc_curve, main="ROC Curve for Final Model")
abline(a=0, b=1, lty=2, col="red") # Add a diagonal reference line

# Save your logistic model to an RDS file
saveRDS(logistic_model, "logistic_model.rds")

# You can then load your model like this:
# loaded_model <- readRDS("logistic_model.rds")

var_imp <- varImp(logistic_model, scale = FALSE)
print(var_imp)




#9
library(shiny)
library(caret)
library(pROC)

# Load your final logistic model
# Make sure to save your logistic_model as an RDS file using saveRDS(logistic_model, "logistic_model.rds")
# and upload it to your Shiny Server or load from local directory if running locally.
logistic_model <- readRDS("logistic_model.rds")

# Define UI
ui <- fluidPage(
  titlePanel("Diabetes Risk Calculator"),
  sidebarLayout(
    sidebarPanel(
      actionButton("start", "Start", class = "btn-primary"),
      conditionalPanel(
        condition = "input.start > 0",
        numericInput("glucose", "Glucose", value = NA),
        numericInput("bloodPressure", "Blood Pressure", value = NA),
        numericInput("skinThickness", "Skin Thickness", value = NA),
        numericInput("insulin", "Insulin", value = NA),
        numericInput("bmi", "BMI", value = NA),
        numericInput("diabetesPedigreeFunction", "Diabetes Pedigree Function", value = NA),
        numericInput("age", "Age", value = NA),
        actionButton("calculate", "Calculate", class = "btn-success"),
        br(),
        tags$a(href = "https://www.ageuk.org.uk/information-advice/health-wellbeing/conditions-illnesses/diabetes/#:~:text=About%209%20in%2010%20people,ever%20had%20high%20blood%20pressure", "More Information on Diabates", target = "_blank")
      )
    ),
    mainPanel(
      tags$img(src = "image.jpg", height = "400px", width = "500px"),
      verbatimTextOutput("riskText"),
      uiOutput("riskIndicator")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  observeEvent(input$start, {
    # Code to reset the output text and fields when Start is pressed again
    output$riskText <- renderText({ "" })
  })
  
  output$riskIndicator <- renderUI({
    req(input$calculate)
    risk_prediction <- predict(logistic_model, newdata = input_data(), type = "prob")[,2]
    
    if (is.na(risk_prediction)) {
      return(NULL)
    }
    
    if (risk_prediction > 0.5) {
      tags$div(style="color:red", "High risk")
    } else {
      tags$div(style="color:green", "Low risk")
    }
  })
  
  input_data <- reactive({
    data.frame(
      BMI = input$bmi,
      Glucose = input$glucose,
      SkinThickness = input$skinThickness,
      BloodPressure = input$bloodPressure,
      Insulin = input$insulin,
      DiabetesPedigreeFunction = input$diabetesPedigreeFunction,
      Age = input$age
    )
  })
  
  observeEvent(input$calculate, {
    # This is where you take input values and pass them to the model
    inputData <- input_data()
    
    # Make sure that the inputData types match the model's expected input types
    # For example, if the model expects a factor for some columns, make sure to convert them
    
    # Predict the risk using the logistic model
    # Note: Make sure that the names and order of the columns match the model's training data
    risk_prediction <- predict(logistic_model, newdata = inputData, type = "prob")[,2]
    
    # Output the risk to the user
    output$riskText <- renderText({
      if (is.na(risk_prediction)) {
        "Please fill in all fields."
      } else if (risk_prediction > 0.5) {
        paste("High risk of diabetes. Predicted probability: ", round(risk_prediction, 4))
      } else {
        paste("Low risk of diabetes. Predicted probability: ", round(risk_prediction, 4))
      }
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)

