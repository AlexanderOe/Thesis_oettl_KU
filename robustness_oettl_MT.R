library(dplyr)
library(gbm)
library(caret)
library(reshape2)
library(viridis)
library(smotefamily)
library(randomForest)


# Robustness checks -----------------------------------------------
# The robustness checks contain two sections. First, a gradient boosting
# machine is estimated. It is similarly presented to the random forests,
# therefore, the confusion matrix plot function is defined at the beginning.
# Second, the random forest estimations are recalculated with a balanced panel.
# Hence, the forest is only trained on 2018 observations that are also in 2019
# and only has to predict 2019 observations that are also in 2018. 



# Individualize the working directories to run the script!
# figure_folder indicates folder to save developed figures in this script
# data_folder has to contain the datasets developed in final_data_oettl_MT.R
figure_folder <- "~/UCPH/Master_Thesis/Thesis_work/R_Thesis/Figures"
data_folder <- "~/UCPH/Master_Thesis/Thesis_work/R_Thesis/Data"




################################
# Boosted tree models          #
################################

# Defining function for confusion matrix as output having multiple estimations
confusion_plot <- function(confusion_matrices){
  # Set names
  estimations_value <- c("Pollution", "Food prod climate", "Global warming", 
                         "Food shortage", "No concernes", 
                         "Rainforest", "Growing population",
                         "Aggregated concern")
  
  # Develop data from confusion matrices 
  confusion_matrix <- c()
  for (i in c(1:4,6,7)){
    confusion_matrices[[i]]$table <- t(confusion_matrices[[i]]$table)
    total_value <- rowSums(confusion_matrices[[i]]$table[,1:2])
    confusion_matrices[[i]]$table[,1] <- confusion_matrices[[i]]$table[,1]/total_value
    confusion_matrices[[i]]$table[,2] <- confusion_matrices[[i]]$table[,2]/total_value
    long <- melt(confusion_matrices[[i]]$table, id.vars = c("0", "1"),stringsAsFactors = FALSE)
    long <- cbind(long, c(rep(estimations_value[i],4)))
    confusion_matrix <- rbind(confusion_matrix, long)
  }
  
  # Rename for plot
  confusion_matrix[,1:2][confusion_matrix[,1:2] == "0"] <- "not concerned" 
  confusion_matrix[,1:2][confusion_matrix[,1:2] == "1"] <- "concerned" 
  
  # Adjust sorting to conventional matrix layout
  confusion_matrix$Reference <- factor(confusion_matrix$Reference, 
                                       levels = unique(confusion_matrix$Reference))
  
  # plot
  confusion_plot <- ggplot(confusion_matrix, 
                           aes(x = Prediction, y = Reference, 
                               fill = value)) +
    facet_wrap(~ confusion_matrix[,4]) +
    scale_fill_viridis(option = "viridis", limits = c(0,1)) + 
    geom_tile() + theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab("Predicted") + ylab("Actual")  +
    geom_text(aes(label = sprintf("%0.2f", value)), vjust = 1) +
    guides(fill = guide_colourbar("Relative \nPredictions:"))
  
  return(confusion_plot)
}


# Predicting with highest aggregation-------------------------------------
setwd(data_folder)
data <- read.csv("data_2018_value.csv", check.names=FALSE)
data_2019 <- read.csv("data_2019_value.csv", check.names=FALSE)


# SMOTE over-sampling as possible revenue for future research
# Using smote to artificially oversample for adjustment to imbalance data
# with k-nearest neighbors

#genData = SMOTE(data,data[,36],K=10)
#genData <- genData$data
#data <- genData_2

# Estimating the random forest with stratification
gbm_highagg <- list()
for (i in c(1:7)){
  set.seed(118)
  
  gbm_highagg[[i]] <-  gbm(data[,(i+35)]~., data=data[,45:52], 
                           distribution = 'bernoulli', shrinkage=0.01,
                           n.minobsinnode = 10, interaction.depth = 25,
                           n.trees=1000, verbose=F, class.stratify.cv=T)
  
}


# confusion plot with predictions in 2019
predictions <- list()
confusion_matrices <- list()
for (i in 1:7){
  prediction <- ifelse(predict(gbm_highagg[[i]], newdata = data_2019[,45:52])>0.5,1,0)
  predictions[[i]] <- as.factor(prediction)
  confusion_matrices[[i]] <- confusionMatrix(predictions[[i]], as.factor(data_2019[,i+35]), 
                                             mode = "everything")
}

confusion_matrices_plot <- confusion_plot(confusion_matrices)
confusion_matrices_plot
setwd(figure_folder)
ggsave("confmatrix_gbm_highagg.pdf", confusion_matrices_plot, height = 6, width = 9)

rm(list=setdiff(ls(), c("data_folder","figure_folder","confusion_plot")))


# Predicting with middle aggregation-------------------------------------
setwd(data_folder)
data <- read.csv("data_2018_org.csv", check.names=FALSE)
data_2019 <- read.csv("data_2019_org.csv", check.names=FALSE)


# setting up same width for prediction filter products which are either
# not in 2018 or not in 2019
different_products <- setdiff(colnames(data), colnames(data_2019))
data <- data %>% select_if(!(colnames(data) %in% different_products))
data_2019 <- data_2019 %>% select_if(colnames(data_2019) %in% (colnames(data)))

# Estimating the random forest with stratification
gbm_midagg <- list()
for (i in c(1:7)){
  set.seed(118)
  
  gbm_midagg[[i]] <-  gbm(data[,(i+141)]~., data=data[,2:141], 
                           distribution = 'bernoulli', shrinkage=0.01,
                           n.minobsinnode = 10, interaction.depth = 25,
                           n.trees=1000, verbose=F, class.stratify.cv=T)
  
}

# confusion plot with predictions in 2019
predictions <- list()
confusion_matrices <- list()
for (i in 1:7){
  prediction <- ifelse(predict(gbm_midagg[[i]], newdata = data_2019[,2:141])>0.5,1,0)
  predictions[[i]] <- as.factor(prediction)
  confusion_matrices[[i]] <- confusionMatrix(predictions[[i]], as.factor(data_2019[,i+141]), 
                                             mode = "everything")
}

confusion_matrices_plot <- confusion_plot(confusion_matrices)
confusion_matrices_plot
setwd(figure_folder)
ggsave("confmatrix_gbm_midagg.pdf", confusion_matrices_plot, height = 6, width = 9)

rm(list=setdiff(ls(), c("data_folder","figure_folder","confusion_plot")))


# Predicting with low aggregation-------------------------------------
setwd(data_folder)
data <- read.csv("data_2018_dtype_org.csv", check.names=FALSE)
data_2019 <- read.csv("data_2019_dtype_org.csv", check.names=FALSE)

# setting up same width for prediction filter products which are either
# not in 2018 or not in 2019
different_products <- setdiff(colnames(data), colnames(data_2019))
data <- data %>% select_if(!(colnames(data) %in% different_products))
data_2019 <- data_2019 %>% select_if(colnames(data_2019) %in% (colnames(data)))

# Estimating the random forest with stratification
gbm_lowagg <- list()
for (i in c(1:7)){
  set.seed(118)
  
  gbm_lowagg[[i]] <-  gbm(data[,(i+1366)]~., data=data[,2:1366], 
                           distribution = 'bernoulli', shrinkage=0.01,
                           n.minobsinnode = 10, interaction.depth = 20,
                           n.trees=1000, verbose=T, class.stratify.cv=T)
  
}

# confusion plot with predictions in 2019
predictions <- list()
confusion_matrices <- list()
for (i in 1:7){
  prediction <- ifelse(predict(gbm_lowagg[[i]], newdata = data_2019[,2:1366])>0.5,1,0)
  predictions[[i]] <- as.factor(prediction)
  confusion_matrices[[i]] <- confusionMatrix(predictions[[i]], as.factor(data_2019[,i+1366]), 
                                             mode = "everything")
}

confusion_matrices_plot <- confusion_plot(confusion_matrices)
confusion_matrices_plot
setwd(figure_folder)
ggsave("confmatrix_gbm_lowagg.pdf", confusion_matrices_plot, height = 6, width = 9)

rm(list=setdiff(ls(), c("data_folder","figure_folder","confusion_plot")))


################################
# Predict with balanced panel  #
################################

# Predicting with highest aggregation-------------------------------------
setwd(data_folder)
data <- read.csv("data_2018_value.csv", check.names=FALSE)
data_2019 <- read.csv("data_2019_value.csv", check.names=FALSE)

# making dataset balanced 
data_2019 <- subset(data_2019, hhnr %in% data$hhnr)
data <- subset(data, hhnr %in% data_2019$hhnr)

# Adjusted to imbalanced data
# Running algorithm to find best Mtrys 
best_mtry <- c()
for (i in 1:7){
  set.seed(118)
  
  mtry <- tuneRF(data[,c(45:52)],as.factor(data[,(i+35)]),
                 stepFactor = 1.5,
                 plot = F,
                 strata = data[,(i+35)],
                 sampsize = c(rep(min(table(data[,(i+35)])),
                                  length(unique(data[,(i+35)])))))
  
  best_mtry[i] <- mtry[mtry[,2] == min(mtry[,2]), 1]
}

# Estimating the random forest with stratification
rf_strat_highagg <- list()
for (i in c(1:7)){
  set.seed(118)
  
  rf_strat_highagg[[i]] <- randomForest(as.factor(data[,(i+35)])~.,
                                        ntree=200, mtry=best_mtry[i],
                                        confusion=T, importance = T,
                                        strata = data[,(i+35)],
                                        sampsize = c(rep(min(table(data[,(i+35)])),
                                                         length(unique(data[,(i+35)])))),
                                        data = data[,45:52])
}

# confusion plot with predictions in 2019
predictions <- list()
confusion_matrices <- list()
for (i in 1:7){
  predictions[[i]] <- predict(rf_strat_highagg[[i]], newdata = data_2019[,45:52])
  confusion_matrices[[i]] <- confusionMatrix(predictions[[i]], as.factor(data_2019[,i+35]), 
                                             mode = "everything")
  print(confusion_matrices[[i]])
}

confusion_matrices_plot <- confusion_plot(confusion_matrices)
confusion_matrices_plot
setwd(figure_folder)
ggsave("confmatrix_brf_highagg_balancedPanel.pdf", 
       confusion_matrices_plot, height = 6, width = 9)

rm(list=setdiff(ls(), c("data_folder","figure_folder","confusion_plot")))

# Predicting with middle aggregation-------------------------------------
setwd(data_folder)
data <- read.csv("data_2018_org.csv", check.names=FALSE)
data_2019 <- read.csv("data_2019_org.csv", check.names=FALSE)

# setting up same width for prediction filter products which are either
# not in 2018 or not in 2019
different_products <- setdiff(colnames(data), colnames(data_2019))
data <- data %>% select_if(!(colnames(data) %in% different_products))
data_2019 <- data_2019 %>% select_if(colnames(data_2019) %in% (colnames(data)))

# making dataset balanced 
data_2019 <- subset(data_2019, hhnr %in% data$hhnr)
data <- subset(data, hhnr %in% data_2019$hhnr)

# Adjusted to imbalanced data
# Running algorithm to find best Mtrys
best_mtry <- c()
for (i in 1:7){
  set.seed(118)
  
  mtry <- tuneRF(data[,c(2:141)],as.factor(data[,(i+141)]),
                 stepFactor = 1.5, 
                 plot = F,
                 strata = data[,(i+141)],
                 sampsize = c(rep(min(table(data[,(i+141)])),
                                  length(unique(data[,(i+141)])))))
  best_mtry[i] <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
}

rf_strat_midagg <- list()
for (i in c(1:7)){
  set.seed(118)
  
  rf_strat_midagg[[i]] <- randomForest(as.factor(data[,(i+141)])~.,
                                       ntree=200, mtry=best_mtry[i],
                                       confusion=T, importance = T,
                                       strata = data[,(i+141)],
                                       sampsize = c(rep(min(table(data[,(i+141)])),
                                                        length(unique(data[,(i+141)])))),
                                       data = data[,2:141])
}

# confusion plot with predictions in 2019
predictions <- list()
confusion_matrices <- list()
for (i in 1:7){
  predictions[[i]] <- predict(rf_strat_midagg[[i]], newdata = data_2019[,2:141])
  confusion_matrices[[i]] <- confusionMatrix(predictions[[i]], as.factor(data_2019[,i+141]), 
                                             mode = "everything")
  print(confusion_matrices[[i]])
}

confusion_matrices_plot <- confusion_plot(confusion_matrices)
confusion_matrices_plot
setwd(figure_folder)
ggsave("confmatrix_brf_midagg_balancedPanel.pdf", 
       confusion_matrices_plot, height = 6, width = 9)

rm(list=setdiff(ls(), c("data_folder","figure_folder","confusion_plot")))

# Predicting with low aggregation-------------------------------------
setwd(data_folder)
data <- read.csv("data_2018_dtype_org.csv", check.names=FALSE)
data_2019 <- read.csv("data_2019_dtype_org.csv", check.names=FALSE)

# setting up same width for prediction filter products which are either
# not in 2018 or not in 2019
different_products <- setdiff(colnames(data), colnames(data_2019))
data <- data %>% select_if(!(colnames(data) %in% different_products))
data_2019 <- data_2019 %>% select_if(colnames(data_2019) %in% (colnames(data)))

# making dataset balanced 
data_2019 <- subset(data_2019, hhnr %in% data$hhnr)
data <- subset(data, hhnr %in% data_2019$hhnr)

# Adjusted to imbalanced data
# Running algorithm to find best Mtrys
best_mtry <- c()
for (i in 1:7){
  set.seed(118)
  
  mtry <- tuneRF(data[,c(2:1366)],as.factor(data[,(i+1366)]),
                 stepFactor = 1.5, 
                 plot = F,
                 strata = data[,(i+1366)],
                 sampsize = c(rep(min(table(data[,(i+1366)])),
                                  length(unique(data[,(i+1366)])))))
  best_mtry[i] <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
}

rf_strat_lowagg <- list()
for (i in c(1:7)){
  set.seed(118)
  
  rf_strat_lowagg[[i]] <- randomForest(as.factor(data[,(i+1366)])~.,
                                       ntree=200, mtry=best_mtry[i],
                                       confusion=T, importance = T,
                                       strata = data[,(i+1366)],
                                       sampsize = c(rep(min(table(data[,(i+1366)])),
                                                        length(unique(data[,(i+1366)])))),
                                       data = data[,2:1366])
}

# predicting 2019
predictions <- list()
confusion_matrices <- list()
for (i in 1:7){
  predictions[[i]] <- predict(rf_strat_lowagg[[i]], newdata = data_2019[,2:1366])
  confusion_matrices[[i]] <- confusionMatrix(predictions[[i]], as.factor(data_2019[,i+1366]), 
                                             mode = "everything")
}

confusion_matrices_plot <- confusion_plot(confusion_matrices)
confusion_matrices_plot
setwd(figure_folder)
ggsave("confmatrix_brf_lowagg_balancedPanel.pdf", 
       confusion_matrices_plot, height = 6, width = 9)
