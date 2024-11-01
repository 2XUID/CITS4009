
set.seed(24121014)
library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(caret)
library(ROCR)
library(e1071)
library(pROC)
library(rpart)
library(lime)
library(cluster)
library(purrr)
library(shiny)


dataset <- read.csv("StudentPerformanceFactors.csv", na.strings = c("", "NA"))

missing_data <- colSums(is.na(dataset))
table(dataset$Teacher_Quality, useNA = "ifany")
table(dataset$Distance_from_Home, useNA = "ifany")
table(dataset$Parental_Education_Level, useNA = "ifany")

table(dataset$Family_Income, dataset$Parental_Education_Level, useNA = "ifany")

dataset <- dataset %>%
  group_by(Family_Income) %>%
  mutate(Parental_Education_Level = ifelse(is.na(Parental_Education_Level), 
                                           sample(Parental_Education_Level[!is.na(Parental_Education_Level)], 
                                                  sum(is.na(Parental_Education_Level)), replace = TRUE), 
                                           Parental_Education_Level))

# For Teacher_Quality
dataset$Teacher_Quality[is.na(dataset$Teacher_Quality)] <- mode(dataset$Teacher_Quality)

#For Distance_from_Home
dataset$Distance_from_Home[is.na(dataset$Distance_from_Home)] <- mode(dataset$Distance_from_Home)

# Check for missing values
sum(is.na(dataset$Parental_Education_Level))
sum(is.na(dataset$Teacher_Quality))
sum(is.na(dataset$Distance_from_Home))


# Visualize outliers for numeric columns
numeric_columns <- c("Hours_Studied", "Attendance", "Sleep_Hours", 
                     "Previous_Scores", "Tutoring_Sessions", "Physical_Activity")

par(mfrow = c(2, 3),  # The layout is 2 rows and 4 columns
    mar = c(4, 4, 2, 1),  # Adjust margins
    cex.main = 1.5,  # Increase font size
    cex.lab = 1.2)




numeric_columns <- c("Hours_Studied", "Tutoring_Sessions")

remove_outliers <- function(data, cols) {
  for (col in cols) {

    Q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
    Q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    

    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    
    # Filter outliers that exceed the lower and upper limits
    data <- data %>%
      filter(!!sym(col) >= lower_bound & !!sym(col) <= upper_bound)
  }
  return(data)
}

dataset_cleaned <- remove_outliers(dataset, numeric_columns)

# Check whether the size of the cleaned data set has been reduced
nrow(dataset_cleaned)



correlation_matrix <- cor(dataset_cleaned[, sapply(dataset_cleaned, is.numeric)])
ggcorrplot(correlation_matrix, lab = TRUE)

dataset_classification <- dataset_cleaned

# Convert Exam_Score to a binary categorical variable (threshold 67), Pass = 1 Fail =- 0
dataset_classification$Pass_Fail <- ifelse(dataset_classification$Exam_Score >= 67, 1, 0)

# Delete the Exam_Score column because we have generated binary categorical variables
dataset_classification <- dataset_classification %>% select(-Exam_Score)

# Check the distribution of categories
table(dataset_classification$Pass_Fail)


dataset_classification <- dataset_classification %>%
  select(-Previous_Scores)

dataset_classification <- dataset_classification %>%
  select(-Gender)


dataset_classification_dummy <- dataset_classification

target <- dataset_classification$Pass_Fail

# one-hot encoding of character variables, excluding target variables
dataset_classification_dummy <- dataset_classification_dummy %>%
  select(-Pass_Fail) %>%
  mutate_at(vars(Parental_Involvement, Access_to_Resources, Extracurricular_Activities, 
                 Motivation_Level, Internet_Access, Family_Income, Teacher_Quality, 
                 School_Type, Peer_Influence, Learning_Disabilities, 
                 Parental_Education_Level, Distance_from_Home), 
            funs(as.factor(.)))

# one-hot code the remaining features using dummyVars
dummies <- dummyVars(~., data = dataset_classification)
dataset_classification_dummy$Pass_Fail <- target

dataset_classification_dummy <- data.frame(predict(dummies, newdata = dataset_classification_dummy))

# Calculate the correlation matrix of the numerical variables
Vars <- setdiff(colnames(dataset_classification_dummy), "Pass_Fail")
correlation_matrix <- cor(dataset_classification_dummy$Pass_Fail, dataset_classification_dummy[, Vars], use = 'pairwise.complete.obs')
sorted_corr <- correlation_matrix[, order(abs(correlation_matrix), decreasing = TRUE)]

# Remove highly correlated features (assuming a correlation coefficient threshold of 0.8)
high_corr_vars <- names(sorted_corr[abs(sorted_corr) > 0.8])
dataset_classification_dummy <- dataset_classification_dummy %>%
  select(-all_of(high_corr_vars))


trainIndex <- createDataPartition(dataset_classification_dummy$Pass_Fail, p = 0.8, 
                                  list = FALSE, 
                                  times = 1)

train_data <- dataset_classification_dummy[trainIndex,]

test_data <- dataset_classification_dummy[-trainIndex,]

dim(train_data)
dim(test_data)


### 2.1 Classification

### 2.1.1 Single Variable Classification


# Get all dummy variable column names (excluding Pass_Fail column)
dummyVars <- setdiff(colnames(dataset_classification_dummy), "Pass_Fail")

# Define the AUC evaluation function
calcAUC <- function(predcol, outcol) {
  perf <- performance(prediction(as.numeric(predcol), outcol == 1), "auc")
  as.numeric(perf@y.values)
}

# Make predictions for each single variable and calculate the AUC
for (v in dummyVars) {

    train_pred <- train_data[[v]]
  test_pred <- test_data[[v]]
  
# Calculate the AUC of the training set and the test set
  aucTrain <- calcAUC(train_pred, train_data$Pass_Fail)
  if (aucTrain >= 0.53) {
    aucTest <- calcAUC(test_pred, test_data$Pass_Fail)
    print(sprintf("%s: trainAUC: %4.3f; testAUC: %4.3f", v, aucTrain, aucTest))
  }
}



selected_trainvars <- train_data[, c('Hours_Studied','Attendance','Parental_InvolvementHigh','Access_to_ResourcesHigh','Tutoring_Sessions', 'Peer_InfluencePositive', 'Parental_Education_LevelPostgraduate', 'Distance_from_HomeNear','Pass_Fail')]

selected_testvars <- test_data[, c('Hours_Studied','Attendance','Parental_InvolvementHigh','Access_to_ResourcesHigh','Tutoring_Sessions', 'Peer_InfluencePositive', 'Parental_Education_LevelPostgraduate', 'Distance_from_HomeNear','Pass_Fail')]



train_data$Pass_Fail <- as.factor(train_data$Pass_Fail)
test_data$Pass_Fail <- as.factor(test_data$Pass_Fail)

tree_model <- rpart(Pass_Fail ~ ., data=train_data, method="class")

# Prediction for the test data
tree_pred_class <- predict(tree_model, newdata=test_data, type="class")

# Evaluate the performance (confusion matrix)
conf_matrix <- confusionMatrix(tree_pred_class, test_data$Pass_Fail)
print(conf_matrix)


nb_model <- naiveBayes(Pass_Fail ~ ., data=train_data)

# Prediction for the test data
nb_pred_class <- predict(nb_model, newdata=test_data)

# Evaluate the performance (confusion matrix)
conf_matrix_nb <- confusionMatrix(nb_pred_class, test_data$Pass_Fail)
print(conf_matrix_nb)


# Decision Tree Prediction probabilities (use type="prob" for predicted probabilities)
tree_pred_prob <- predict(tree_model, newdata=test_data, type="prob")

# Naive Bayes Prediction probabilities
nb_pred_prob <- predict(nb_model, newdata=test_data, type="raw")

# Plot ROC curve for Decision Tree
roc_tree <- roc(test_data$Pass_Fail, tree_pred_prob[,2])


# Plot ROC curve for Naive Bayes
roc_nb <- roc(test_data$Pass_Fail, nb_pred_prob[,2])

auc(roc_nb)
auc(roc_tree)



selected_trainvars$Pass_Fail <- as.factor(selected_trainvars$Pass_Fail)
selected_testvars$Pass_Fail <- as.factor(selected_testvars$Pass_Fail)

tree_model <- rpart(Pass_Fail ~ ., data=selected_trainvars, method="class")

# Prediction for the test data
tree_pred_class <- predict(tree_model, newdata=selected_testvars, type="class")

# Evaluate the performance (confusion matrix)
conf_matrix <- confusionMatrix(tree_pred_class, selected_testvars$Pass_Fail)
print(conf_matrix)


nb_model <- naiveBayes(Pass_Fail ~ ., data=selected_trainvars)

# Prediction for the test data
nb_pred_class <- predict(nb_model, newdata=selected_testvars)

# Evaluate the performance (confusion matrix)
conf_matrix_nb <- confusionMatrix(nb_pred_class, selected_testvars$Pass_Fail)
print(conf_matrix_nb)

# Decision Tree Prediction probabilities (use type="prob" for predicted probabilities)
tree_pred_prob <- predict(tree_model, newdata=selected_testvars, type="prob")

# Naive Bayes Prediction probabilities
nb_pred_prob <- predict(nb_model, newdata=selected_testvars, type="raw")

# Plot ROC curve for Decision Tree
roc_tree <- roc(selected_testvars$Pass_Fail, tree_pred_prob[,2])


# Plot ROC curve for Naive Bayes
roc_nb <- roc(selected_testvars$Pass_Fail, nb_pred_prob[,2])

auc(roc_nb)
auc(roc_tree)



# Define a custom function for the Decision Tree model
assign("model_type.lime_model_rpart", function(object, ...) {
  return("classification")
}, envir = .GlobalEnv)

lime_model_rpart <- function(x) {
  class(x) <- c("lime_model_rpart", class(x))
  x
}
# Define a custom predict function for Naive Bayes model
predict_proba <- function(model, newdata) {
  predict(model, newdata, type = "raw")
}

# Define model type and prediction functions for Naive Bayes
assign("model_type.nb", function(x, ...) {
  return(x$type)
}, envir = .GlobalEnv)

assign("predict_model.nb", function(x, newdata, ...) {
  predict_result <- predict(x$model, newdata = newdata, type = "raw")
  return(data.frame(predict_result))
}, envir = .GlobalEnv)

nb_model_proba <- list(model = nb_model, predict = predict_proba, type = "classification")
class(nb_model_proba) <- 'nb'


# Prepare the explainer
explainer_tree <- lime(selected_trainvars, lime_model_rpart(tree_model))
explainer_nb <- lime(selected_trainvars, nb_model_proba)

# Generate explanations
explanation_tree <- explain(selected_testvars[1:3, ], explainer_tree, n_labels = 1, n_features = 3)
explanation_nb <- explain(selected_testvars[1:3, ], explainer_nb, n_labels = 1, n_features = 3)


print(explanation_tree)
print(explanation_nb)


plot_features(explanation_tree)
plot_features(explanation_nb)

selected_features <- dataset_cleaned%>%
  select(Hours_Studied, Attendance, Tutoring_Sessions, Exam_Score)

# Standardized numerical features
numeric_features <- selected_features[, c("Hours_Studied", "Attendance", "Tutoring_Sessions","Exam_Score")]

scaled_data <- scale(numeric_features)


pca_model <- prcomp(scaled_data, center = TRUE, scale. = TRUE)

summary(pca_model)

# The first few principal components are selected, so that the cumulative variance interpretation rate reaches 90%
pca_data <- data.frame(pca_model$x[, 1:which(cumsum(pca_model$sdev^2 / sum(pca_model$sdev^2)) >= 0.9)[1]])

wss <- function(data, k) {
  kmeans(data, k, nstart = 10)$tot.withinss
}

# Try different K values and calculate WSS
k_values <- 1:10
wss_values <- map_dbl(k_values, wss, data = pca_data)




silhouette_scores <- numeric(length(k_values))

# Do k-means clustering for each K value and calculate the Silhouette Score
for (i in 1:length(k_values)) {
  k <- k_values[i]
  
  kmeans_model <- kmeans(pca_data, centers = k, nstart = 25)
  
  dist_matrix <- dist(pca_data)
  
  silhouette_result <- silhouette(kmeans_model$cluster, dist_matrix)
  
  if (length(dim(silhouette_result)) == 2) {
    silhouette_scores[i] <- mean(silhouette_result[, 3])
  } else {
    silhouette_scores[i] <- NA
  }
}

# Draw the Silhouette Score for each K value


k = 2
kmeans_result <- kmeans(pca_data, centers = k, nstart = 10)

# Visualize the clustering results
pca_data$cluster <- as.factor(kmeans_result$cluster)



# Combine raw data with clustering results
clustered_data <- cbind(dataset_cleaned , cluster = kmeans_result$cluster)

# The average value of each feature is calculated according to the clustering results
cluster_summary <- clustered_data %>%
  group_by(cluster) %>%
  summarise(across(c(Hours_Studied, Attendance, Tutoring_Sessions), mean, na.rm = TRUE))

# Print the average of each cluster
print(cluster_summary)






# Shiny APP

# Define UI
ui <- fluidPage(
  titlePanel("Student Performance Analysis"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.tabselected == 1",
        selectInput("single_var", "Select Variable for Single Variable Model:", choices = NULL),
        actionButton("run_single_var", "Run Single Variable Model")
      ),
      conditionalPanel(
        condition = "input.tabselected == 2",
        checkboxGroupInput("selected_features", "Select Features for Classification Models:", choices = NULL, selected = NULL),
        actionButton("run_classification", "Run Classification Models")
      ),
      conditionalPanel(
        condition = "input.tabselected == 3",
        numericInput("num_clusters", "Select Number of Clusters:", value = 2, min = 2, max = 10),
        actionButton("run_clustering", "Run Clustering")
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Single Variable Model", value = 1,
                 verbatimTextOutput("single_var_auc"),
                 plotOutput("single_var_plot")
        ),
        tabPanel("Classification Models", value = 2,
                 verbatimTextOutput("class_model_summary"),
                 plotOutput("roc_plot")
        ),
        tabPanel("Clustering Results", value = 3,
                 plotOutput("cluster_plot"),
                 verbatimTextOutput("cluster_summary")
        ),
        id = "tabselected"
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Load data
  dataset <- read.csv("StudentPerformanceFactors.csv", na.strings = c("", "NA"))
  
  # Define Mode function
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  # Handle missing data
  dataset <- dataset %>%
    group_by(Family_Income) %>%
    mutate(Parental_Education_Level = ifelse(is.na(Parental_Education_Level), 
                                             sample(Parental_Education_Level[!is.na(Parental_Education_Level)], 
                                                    sum(is.na(Parental_Education_Level)), replace = TRUE), 
                                             Parental_Education_Level)) %>%
    ungroup()
  
  dataset$Teacher_Quality[is.na(dataset$Teacher_Quality)] <- Mode(dataset$Teacher_Quality)
  dataset$Distance_from_Home[is.na(dataset$Distance_from_Home)] <- Mode(dataset$Distance_from_Home)
  
  # Remove outliers
  numeric_columns <- c("Hours_Studied", "Tutoring_Sessions")
  
  remove_outliers <- function(data, cols) {
    for (col in cols) {
      Q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
      Q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      data <- data %>%
        filter(!!sym(col) >= lower_bound & !!sym(col) <= upper_bound)
    }
    return(data)
  }
  
  dataset_cleaned <- remove_outliers(dataset, numeric_columns)
  
  # Prepare classification dataset
  dataset_classification <- dataset_cleaned
  
  # Convert Exam_Score to binary Pass_Fail
  dataset_classification$Pass_Fail <- ifelse(dataset_classification$Exam_Score >= 67, 1, 0)
  dataset_classification <- dataset_classification %>% select(-Exam_Score)
  
  # Remove unnecessary features
  dataset_classification <- dataset_classification %>% select(-Previous_Scores, -Gender)
  
  # One-hot encoding
  factor_vars <- c('Parental_Involvement', 'Access_to_Resources', 'Extracurricular_Activities', 
                   'Motivation_Level', 'Internet_Access', 'Family_Income', 'Teacher_Quality', 
                   'School_Type', 'Peer_Influence', 'Learning_Disabilities', 
                   'Parental_Education_Level', 'Distance_from_Home')
  
  dataset_classification_dummy <- dataset_classification
  
  dataset_classification_dummy[factor_vars] <- lapply(dataset_classification_dummy[factor_vars], as.factor)
  
  dummies <- dummyVars(~., data = dataset_classification_dummy)
  dataset_classification_dummy <- data.frame(predict(dummies, newdata = dataset_classification_dummy))
  
  # Split into train and test
  trainIndex <- createDataPartition(dataset_classification_dummy$Pass_Fail, p = 0.8, list = FALSE)
  train_data <- dataset_classification_dummy[trainIndex, ]
  test_data <- dataset_classification_dummy[-trainIndex, ]
  
  # Prepare variables for the single variable model
  dummyVarsList <- setdiff(colnames(dataset_classification_dummy), "Pass_Fail")
  
  # Update the choices for single variable model
  updateSelectInput(session, "single_var", choices = dummyVarsList)
  
  # Update the choices for classification features
  updateCheckboxGroupInput(session, "selected_features", choices = dummyVarsList, selected = dummyVarsList)
  
  ### Single Variable Model
  
  observeEvent(input$run_single_var, {
    req(input$single_var)
    v <- input$single_var
    
    # Get predictions for the single variable
    train_pred <- train_data[[v]]
    test_pred <- test_data[[v]]
    
    # Calculate AUC
    calcAUC <- function(predcol, outcol) {
      perf <- performance(prediction(as.numeric(predcol), outcol == 1), "auc")
      as.numeric(perf@y.values)
    }
    
    aucTrain <- calcAUC(train_pred, train_data$Pass_Fail)
    aucTest <- calcAUC(test_pred, test_data$Pass_Fail)
    
    auc_text <- sprintf("%s: trainAUC: %4.3f; testAUC: %4.3f", v, aucTrain, aucTest)
    
    output$single_var_auc <- renderText({
      auc_text
    })
    
    # Plotting ROC curve with AUC and Random Guess Line
    output$single_var_plot <- renderPlot({
      pred <- prediction(as.numeric(test_pred), test_data$Pass_Fail)
      perf <- performance(pred, "tpr", "fpr")
      
      # Plot ROC curve
      plot(perf, col = "blue", lwd = 2, main = paste("ROC Curve for", v))
      abline(a = 0, b = 1, lty = 2, col = "red")  # Random Guess Line
      
      # Add legend
      legend("bottomright", legend = c("ROC Curve", "Random Guess"), col = c("blue", "red"), lwd = 2, lty = c(1, 2))
    })
  })
  
  
  ### Classification Models
  # Define the best features
  best_features <- c('Hours_Studied', 'Attendance', 'Parental_Involvement.High',
                     'Access_to_Resources.High', 'Tutoring_Sessions', 'Peer_Influence.Positive', 'Distance_from_Hom.Near')
  
  # Update the choices for classification features and set default selected features
  updateCheckboxGroupInput(session, "selected_features",
                           choices = dummyVarsList,
                           selected = best_features)
  
  # Automatically run classification models when the tab is selected or features are changed
  observeEvent({
    input$tabselected
    input$selected_features
  }, {
    req(input$selected_features)
    features <- input$selected_features
    
    # Build models using selected features
    formula <- as.formula(paste("Pass_Fail ~", paste(features, collapse = "+")))
    
    # Decision Tree
    train_data$Pass_Fail <- as.factor(train_data$Pass_Fail)
    test_data$Pass_Fail <- as.factor(test_data$Pass_Fail)
    
    tree_model <- rpart(formula, data = train_data, method = "class")
    tree_pred_class <- predict(tree_model, newdata = test_data, type = "class")
    conf_matrix_tree <- confusionMatrix(tree_pred_class, test_data$Pass_Fail)
    
    # Naive Bayes
    nb_model <- naiveBayes(formula, data = train_data)
    nb_pred_class <- predict(nb_model, newdata = test_data)
    conf_matrix_nb <- confusionMatrix(nb_pred_class, test_data$Pass_Fail)
    
    # ROC Curves
    tree_pred_prob <- predict(tree_model, newdata = test_data, type = "prob")
    nb_pred_prob <- predict(nb_model, newdata = test_data, type = "raw")
    
    roc_tree <- roc(as.numeric(test_data$Pass_Fail), tree_pred_prob[, 2])
    roc_nb <- roc(as.numeric(test_data$Pass_Fail), nb_pred_prob[, 2])
    
    auc_tree <- auc(roc_tree)
    auc_nb <- auc(roc_nb)
    
    # Output summaries
    output$class_model_summary <- renderPrint({
      cat("Decision Tree Confusion Matrix:\n")
      print(conf_matrix_tree)
      cat("\nDecision Tree AUC:", round(auc_tree, 3), "\n\n")
      cat("Naive Bayes Confusion Matrix:\n")
      print(conf_matrix_nb)
      cat("\nNaive Bayes AUC:", round(auc_nb, 3), "\n")
    })
    
    # Plot ROC curves
    output$roc_plot <- renderPlot({
      plot(roc_tree, col = "blue", lwd = 2, main = "ROC Curves")
      plot(roc_nb, col = "red", lwd = 2, add = TRUE)
      legend("bottomright", legend = c("Decision Tree", "Naive Bayes", "Random Guess"), 
             col = c("blue", "red"), lwd = 2, lty = c(1, 1, 2))
    })
  })
  
  ### Clustering Results
  
  observeEvent(input$run_clustering, {
    k <- input$num_clusters
    
    # Select features for clustering
    selected_features <- dataset_cleaned %>%
      select(Hours_Studied, Attendance, Tutoring_Sessions, Exam_Score)
    
    # Standardize features
    scaled_data <- scale(selected_features)
    
    # PCA
    pca_model <- prcomp(scaled_data, center = TRUE, scale. = TRUE)
    pca_data <- data.frame(pca_model$x[, 1:2])  # Using first 2 PCs
    colnames(pca_data) <- c("PC1", "PC2")
    
    # K-means clustering
    
    kmeans_result <- kmeans(pca_data, centers = k, nstart = 10)
    pca_data$cluster <- as.factor(kmeans_result$cluster)
    
    # Output cluster plot
    output$cluster_plot <- renderPlot({
      ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +
        geom_point(size = 3) +
        labs(title = paste("K-means Clustering (K=", k, ") on PCA-reduced Data", sep = ""), 
             x = "Principal Component 1", y = "Principal Component 2") +
        theme_minimal()
    })
    
    # Output cluster summaries
    clustered_data <- cbind(dataset_cleaned, cluster = kmeans_result$cluster)
    cluster_summary <- clustered_data %>%
      group_by(cluster) %>%
      summarise(across(c(Hours_Studied, Attendance, Tutoring_Sessions), mean, na.rm = TRUE))
    
    output$cluster_summary <- renderPrint({
      print(cluster_summary)
    })
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)


