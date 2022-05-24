library(dplyr)
library(tidyverse)
library(ggplot2)
library(stringr)
library(MASS)
library(randomForest)
library(caret)
library(viridis)
library(reshape2)
library(stargazer)


# Predictive Modeling -----------------------------------------------
# This script is structured as the thesis with three subsections in the
# analysis. First, the relation of food purchases and environmental concerns
# is investigated using inference statistical methods. Second, the machine
# learning algorithm - random forest classification - is applied for 
# predictive modeling from 2018 to 2019. There are three different 
# feature specifications (aggregations) to predict six different environmental
# concerns. Third, the variable importance of the estimated random forests are
# examined. As a preamble, a descriptive table is developed for the highest 
# aggregation level. 

# Note: Depending on the computational power this script runs approx. 20 min

# Individualize the working directories to run the script!
# figure_folder indicates folder to save developed figures in this script
# data_folder has to contain the datasets developed in final_data_oettl_MT.R
figure_folder <- "~/UCPH/Master_Thesis/Thesis_work/R_Thesis/Figures"
data_folder <- "~/UCPH/Master_Thesis/Thesis_work/R_Thesis/Data"


#######################################################################
# Preamble: Descriptive tables                                        #
#######################################################################

# 2018 data
setwd(data_folder)
data_2018 <- read.csv("data_2018_value.csv", check.names=FALSE)
stargazer(data_2018[,c(43,37,39,38,42,36,41,50,47,48,52,46,45,51,49)], 
          title="Descriptive Statistics for 2018 observations", 
          summary.stat = c("n", "mean", "median", 
                           "sd" , "max" ,"min"), digits=3,
          covariate.labels = c("Aggregated concern", "Food prod climate", 
                               "Food shortage", "Global warming", "Growing Population",
                              "Pollution", "Rainforest", 
                              "Alcohol", "Dairy egg", "Fruit", "Grain based",
                              "Meat", "Organic", "Seafood","Vegetables"))

# 2019 data
data_2019 <- read.csv("data_2019_value.csv", check.names=FALSE)
stargazer(data_2019[,c(43,37,39,38,42,36,41,50,47,48,52,46,45,51,49)], 
          title="Descriptive Statistics for 2019 observations", 
          summary.stat = c("n", "mean", "median", 
                           "sd" , "max" ,"min"), digits=3,
          covariate.labels = c("Aggregated concern", "Food prod climate", 
                               "Food shortage", "Global warming", "Growing Population",
                               "Pollution", "Rainforest", 
                               "Alcohol", "Dairy egg", "Fruit", "Grain based",
                               "Meat", "Organic", "Seafood","Vegetables"))
rm(data_2018,data_2019)


#######################################################################
# Chapter: Relation between environmental concerns and food purchases #
#######################################################################

# Logit and negative binomial regression analysis to investigate the relation
# This section introduces the estimation as well as figure development for
# 112 separate regressions and the corresponding estimation of the McFadden's
# pseudo R-square 

setwd(data_folder)
full_data_value <- read.csv("data_2019_value.csv", check.names=FALSE)
full_data_volume <- read.csv("data_2019_volume.csv", check.names=FALSE)

#--------------------------------------------------------------------
# Value estimation of logit and negative binomial
df_value <- expand.grid(dv = colnames(full_data_value[,c(36:39,41:43)]),
                        pv = colnames(full_data_value[,45:52]),
                        r = NA, datatype = "Value share",
                        stringsAsFactors = FALSE)


for (i in 1:nrow(df_value)) {
  # Negative binomial for agg concern
  if (df_value[i,1]=="environmental_concern"){
    glm_model <- glm.nb(formula(paste0(df_value$dv[i], 
                                       " ~ ", 
                                       df_value$pv[i])),
                        data = full_data_value)
    
    glm_model_0 <- glm.nb(formula(paste0(df_value$dv[i], 
                                         " ~ ", 
                                         1)),
                          data = full_data_value)
    
    # Likelihood ratio test to see negative binomaial better than poisson
    glm_model_pois <- glm(formula(paste0(df_value$dv[i], 
                                            " ~ ", 
                                            df_value$pv[i])),
                             data = full_data_value, family = "poisson")
    print(pchisq(2 * (logLik(glm_model) - logLik(glm_model_pois)), 
                 df = 1, lower.tail = FALSE))
    
    # Estimating McFadden's R square
    df_value$r[i] <- (1-(logLik(glm_model)/logLik(glm_model_0)))
  } else{
    # Estimating binary logit regressions
    glm_model <- glm(formula(paste0(df_value$dv[i], 
                                    " ~ ", 
                                    df_value$pv[i])),
                     data = full_data_value, family = binomial (link="logit"))
    
    glm_model_0 <- glm(formula(paste0(df_value$dv[i], 
                                      " ~ ", 
                                      1)),
                       data = full_data_value, family = binomial (link="logit"))
    
    # Estimating McFadden's R square
    df_value$r[i] <- (1-(logLik(glm_model)/logLik(glm_model_0)))
  }
}

#--------------------------------------------------------------------
# Volume estimation of logit and negative binomial (for comments see above)
df_volume <- expand.grid(dv = colnames(full_data_volume[,c(36:39,41:43)]),
                         pv = colnames(full_data_volume[,45:52]),
                         r = NA, datatype = "Volume share",
                         stringsAsFactors = FALSE)


for (i in 1:nrow(df_volume)) {
  if (df_volume[i,1]=="environmental_concern"){
    glm_model <- glm.nb(formula(paste0(df_volume$dv[i], 
                                       " ~ ", 
                                       df_volume$pv[i])),
                        data = full_data_volume)
    
    glm_model_0 <- glm.nb(formula(paste0(df_volume$dv[i], 
                                         " ~ ", 
                                         1)),
                          data = full_data_volume)
    
    glm_model_pois <- glm(formula(paste0(df_volume$dv[i], 
                                         " ~ ", 
                                         df_volume$pv[i])),
                          data = full_data_value, family = "poisson")
    print(pchisq(2 * (logLik(glm_model) - logLik(glm_model_pois)), 
                 df = 1, lower.tail = FALSE))
    
    df_volume$r[i] <- (1-(logLik(glm_model)/logLik(glm_model_0)))
  } else{
    glm_model <- glm(formula(paste0(df_volume$dv[i], 
                                    " ~ ", 
                                    df_volume$pv[i])),
                     data = full_data_volume, family = binomial (link="logit"))
    
    glm_model_0 <- glm(formula(paste0(df_volume$dv[i], 
                                      " ~ ", 
                                      1)),
                       data = full_data_volume, family = binomial (link="logit"))
    
    df_volume$r[i] <- (1-(logLik(glm_model)/logLik(glm_model_0)))
    
  }
}

#-------------------------------------------------------------------
# plot development 
results_df <- rbind(df_value,df_volume)

# Adjust variable names 
results_df$dv <- gsub("concerned_", "",results_df$dv)
results_df$dv <- gsub("environmental", "aggregated", results_df$dv)
results_df$dv <- gsub("_", " ",results_df$dv)
results_df$dv <- str_replace(results_df$dv, "^\\w{1}", toupper)

results_df$pv <- gsub("_", " ",results_df$pv)
results_df$pv <- str_replace(results_df$pv, "^\\w{1}", toupper)

# make dv an ordered factor
results_df$dv <- factor(results_df$dv, levels = c(sort(unique(results_df$dv), decreasing = T)))

# Figure 
relation_figure <- ggplot(results_df, aes(x = dv, y = r, group = pv, colour = pv,
                                   shape = pv))  +
  facet_wrap(~ datatype, scales = "free_y") +
  geom_point() + 
  ylab(expression(R^{2})) +
  coord_flip() +
  scale_shape_manual(values=1:10) +
  guides(colour = guide_legend(title = "Product group"),
         shape = guide_legend(title = "Product group")) +
  xlab("Environmental concerns") +
  theme_bw()

relation_figure

setwd(figure_folder)
ggsave("relation_figure.pdf", relation_figure, height = 6, width = 9)

# see distribution of environmental concerns -> McFaddens can be artificially high
for(i in 1:8){
  print(table(full_data_value[,35+i]))
}


rm(list=setdiff(ls(), c("data_folder","figure_folder")))



################################
# Chapter: Predictive Accuracy #
################################

# This section is structured as follows: First a function for the confusion 
# matrix figure is defined. Then high, middle, and low aggregation levels are
# used to estimate random forests, once with adjustment to imbalanced data 
# and once without

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

# Label for plots to check 
estimations_value <- c("Pollution", "Food prod climate", "Global warming", 
                       "Food shortage", "No concernes", 
                       "Rainforest", "Growing population",
                       "Aggregated concern")

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
  
  print(rf_strat_highagg[[i]])
}

# Visualization of ntree
for (i in 1:7){
  plot(rf_strat_highagg[[i]], main = estimations_value[i])
  legend("topright", colnames(rf_strat_highagg[[i]]$err.rate),col=1:(length(unique(data[,(i+35)]))+1),
         cex=0.8,fill=1:(length(unique(data[,(i+35)]))+1))
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
ggsave("confmatrix_strat_highagg.pdf", confusion_matrices_plot, height = 6, width = 9)

# Without adjustment to imbalanced data
# Running algorithm to find best Mtrys 
best_mtry <- c()
for (i in 1:7){
  set.seed(118)
  
  mtry <- tuneRF(data[,c(45:52)],as.factor(data[,(i+35)]),
                 stepFactor = 1.5,
                 plot = F)
  
  best_mtry[i] <- mtry[mtry[,2] == min(mtry[,2]), 1]
}


# Estimating the base random forest without stratification
rf_highagg <- list()
for (i in c(1:7)){
  set.seed(118)
  
  rf_highagg[[i]] <- randomForest(as.factor(data[,(i+35)])~.,
                                        ntree=200, mtry=best_mtry[i],
                                        confusion=T, importance = T,
                                        data = data[,45:52])
}

# Visualization of ntree
for (i in 1:7){
  plot(rf_highagg[[i]], main = estimations_value[i])
  legend("topright", colnames(rf_highagg[[i]]$err.rate),col=1:(length(unique(data[,(i+35)]))+1),
         cex=0.8,fill=1:(length(unique(data[,(i+35)]))+1))
}

# confusion plot with predictions in 2019
predictions <- list()
confusion_matrices <- list()
for (i in 1:7){
  predictions[[i]] <- predict(rf_highagg[[i]], newdata = data_2019[,45:52])
  confusion_matrices[[i]] <- confusionMatrix(predictions[[i]], as.factor(data_2019[,i+35]), 
                                             mode = "everything")
  print(confusion_matrices[[i]])
}

confusion_matrices_plot <- confusion_plot(confusion_matrices)
confusion_matrices_plot
ggsave("confmatrix_highagg.pdf", confusion_matrices_plot, height = 6, width = 9)

rm(confusion_matrices,confusion_matrices_plot,data,data_2019,mtry,predictions,best_mtry)


# Predicting with middle aggregation-------------------------------------
setwd(data_folder)
data <- read.csv("data_2018_org.csv", check.names=FALSE)
data_2019 <- read.csv("data_2019_org.csv", check.names=FALSE)

# setting up same width for prediction filter products which are either
# not in 2018 or not in 2019
different_products <- setdiff(colnames(data), colnames(data_2019))
data <- data %>% select_if(!(colnames(data) %in% different_products))
data_2019 <- data_2019 %>% select_if(colnames(data_2019) %in% (colnames(data)))

# Descriptive table with all products
products <- colnames(data_2019)[2:141]
products <- gsub("0", "non-organic",products)
products <- gsub("1", "organic",products)
products <- gsub("[1-9][0-9]?", "",products)
products <- gsub("_", " ",products)

# Split in two tables for latex
products_table_1 <- cbind(products[1:35], products[36:70])
products_table_2 <- cbind(products[71:105], products[106:140])

setwd(figure_folder)
write.table(products_table_1, "products_140_1.csv",sep=",",row.names=F, col.names = F)
write.table(products_table_2, "products_140_2.csv",sep=",",row.names=F, col.names = F)
rm(products, products_table_1,products_table_2)

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
  
  print(rf_strat_midagg[[i]])
}

# Visual inspection for ntrees 
for (i in 1:7){
  plot(rf_strat_midagg[[i]], main = estimations_value[i])
  legend("topright", colnames(rf_strat_midagg[[i]]$err.rate),col=1:(length(unique(data[,(i+141)]))+1),
         cex=0.8,fill=1:(length(unique(data[,(i+141)]))+1))
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
ggsave("confmatrix_strat_midagg.pdf", confusion_matrices_plot, height = 6, width = 9)

# Running algorithm without adjustment to imbalanced data
# Running algorithm to find best Mtrys
best_mtry <- c()
for (i in 1:7){
  set.seed(118)
  
  mtry <- tuneRF(data[,c(2:141)],as.factor(data[,(i+141)]),
                 stepFactor = 1.5,
                 plot = F)
  
  best_mtry[i] <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
}

rf_midagg <- list()
for (i in c(1:7)){
  set.seed(118)
  
  rf_midagg[[i]] <- randomForest(as.factor(data[,(i+141)])~.,
                                       ntree=200, mtry=best_mtry[i],
                                       confusion=T, importance = T,
                                       data = data[,2:141])
}

# Visual inspection for ntrees 
for (i in 1:7){
  plot(rf_midagg[[i]], main = estimations_value[i])
  legend("topright", colnames(rf_midagg[[i]]$err.rate),col=1:(length(unique(data[,(i+141)]))+1),
         cex=0.8,fill=1:(length(unique(data[,(i+141)]))+1))
}

# confusion plot with predictions in 2019
predictions <- list()
confusion_matrices <- list()
for (i in 1:7){
  predictions[[i]] <- predict(rf_midagg[[i]], newdata = data_2019[,2:141])
  confusion_matrices[[i]] <- confusionMatrix(predictions[[i]], as.factor(data_2019[,i+141]), 
                                             mode = "everything")
  print(confusion_matrices[[i]])
}

confusion_matrices_plot <- confusion_plot(confusion_matrices)
confusion_matrices_plot
ggsave("confmatrix_midagg.pdf", confusion_matrices_plot, height = 6, width = 9)

rm(data, data_2019, mtry, best_mtry, confusion_matrices, confusion_matrices_plot,
   predictions, different_products)


# Predicting with low aggregation-------------------------------------
setwd(data_folder)
data <- read.csv("data_2018_dtype_org.csv", check.names=FALSE)
data_2019 <- read.csv("data_2019_dtype_org.csv", check.names=FALSE)

# setting up same width for prediction filter products which are either
# not in 2018 or not in 2019
different_products <- setdiff(colnames(data), colnames(data_2019))
data <- data %>% select_if(!(colnames(data) %in% different_products))
data_2019 <- data_2019 %>% select_if(colnames(data_2019) %in% (colnames(data)))

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
  
  print(rf_strat_lowagg[[i]])
}

# Visual inspection for ntrees 
for (i in 1:7){
  plot(rf_strat_lowagg[[i]], main = estimations_value[i])
  legend("topright", colnames(rf_strat_lowagg[[i]]$err.rate),col=1:(length(unique(data[,(i+1366)]))+1),
         cex=0.8,fill=1:(length(unique(data[,(i+1366)]))+1))
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
ggsave("confmatrix_strat_lowagg.pdf", confusion_matrices_plot, height = 6, width = 9)

# Running algorithm without adjustment to imbalanced data
# Running algorithm to find best Mtrys
best_mtry <- c()
for (i in 1:7){
  set.seed(118)
  
  mtry <- tuneRF(data[,c(2:1366)],as.factor(data[,(i+1366)]),
                 stepFactor = 1.5,
                 plot = F)
  
  best_mtry[i] <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
}

rf_lowagg <- list()
for (i in c(1:7)){
  set.seed(118)
  
  rf_lowagg[[i]] <- randomForest(as.factor(data[,(i+1366)])~.,
                                       ntree=200, mtry=best_mtry[i],
                                       confusion=T, importance = T,
                                       data = data[,2:1366])
  
  print(rf_lowagg[[i]])
}

# Visual inspection for ntrees 
for (i in 1:7){
  plot(rf_lowagg[[i]], main = estimations_value[i])
  legend("topright", colnames(rf_lowagg[[i]]$err.rate),col=1:(length(unique(data[,(i+1366)]))+1),
         cex=0.8,fill=1:(length(unique(data[,(i+1366)]))+1))
}

# predicting 2019
predictions <- list()
confusion_matrices <- list()
for (i in 1:7){
  predictions[[i]] <- predict(rf_lowagg[[i]], newdata = data_2019[,2:1366])
  confusion_matrices[[i]] <- confusionMatrix(predictions[[i]], as.factor(data_2019[,i+1366]), 
                                             mode = "everything")
}

confusion_matrices_plot <- confusion_plot(confusion_matrices)
confusion_matrices_plot
ggsave("confmatrix_lowagg.pdf", confusion_matrices_plot, height = 6, width = 9)

rm(data, data_2019, confusion_matrices,confusion_matrices_plot, predictions,
   mtry, best_mtry, i, different_products, confusion_plot)



################################
# Chapter: Variable importance #
################################

# High aggregation-----------------------------------------
setwd(data_folder)
data <- read.csv("data_2018_value.csv", check.names=FALSE)

# Develop data frame from results
varimp_highagg <- data.frame()
for (i in c(1:4,6,7)){
  varimp_highagg <- rbind(varimp_highagg, importance(rf_strat_highagg[[i]])[,3:4])
}

varimp_highagg$dv <- rep(colnames(data[,c(36:39,41,42)]), 
                         each = 8)
varimp_highagg$pv <- gsub('[0-9]+', '', rownames(varimp_highagg))

# Adjust variable names 
varimp_highagg$dv <- gsub("concerned_", "",varimp_highagg$dv)
varimp_highagg$dv <- gsub("environmental", "aggregated", varimp_highagg$dv)
varimp_highagg$dv <- gsub("_", " ",varimp_highagg$dv)
varimp_highagg$dv <- str_replace(varimp_highagg$dv, "^\\w{1}", toupper)

varimp_highagg$pv <- gsub("_", " ",varimp_highagg$pv)
varimp_highagg$pv <- str_replace(varimp_highagg$pv, "^\\w{1}", toupper)

colnames(varimp_highagg)[2] <- "MeanDecreaseImpurity"

# make dv an ordered factor
varimp_highagg$dv <- factor(varimp_highagg$dv, 
                            levels = c(sort(unique(varimp_highagg$dv), decreasing = T)))

# Transpose data to long format for ggplot
varimp_highagg <- melt(varimp_highagg, id.vars = c("dv","pv"))

# Develop Figure
imp_figure <- ggplot(varimp_highagg, aes(x = dv, y = value,
                                    group = pv, colour = pv,
                                    shape = pv)) +
  facet_wrap(~ variable, scales = "free") +
  geom_point() + 
  coord_flip() +
  scale_shape_manual(values=1:10) +
  guides(colour = guide_legend(title = "Product group"),
         shape = guide_legend(title = "Product group")) +
  xlab("Environmental concerns") +
  ylab("") +
  theme_bw()

imp_figure
setwd(figure_folder)
ggsave("imp_figure.pdf", imp_figure, height = 6, width = 9)

rm(data, imp_figure,varimp_highagg)

# Middle aggregation-----------------------------------------
# Develop data frame from results
varimp_middleagg <- data.frame()
for (i in c(1:4,6,7)){
  var_imp <- as.data.frame(importance(rf_strat_midagg[[i]])[,])
  MeanDecreaseAccuracy <- rownames(var_imp[var_imp[, 3] == max(var_imp[, 3]), ])
  MeanDecreaseImpurity <- rownames(var_imp[var_imp[, 4] == max(var_imp[, 4]), ])
  varimp_middleagg <- rbind(varimp_middleagg, 
                            cbind(MeanDecreaseAccuracy, MeanDecreaseImpurity))
}

# Renaming
rownames(varimp_middleagg) <- estimations_value[c(1:4,6,7)]
varimp_middleagg$MeanDecreaseAccuracy <- gsub("_", " ",varimp_middleagg$MeanDecreaseAccuracy)
varimp_middleagg$MeanDecreaseImpurity <- gsub("_", " ",varimp_middleagg$MeanDecreaseImpurity)
varimp_middleagg <- varimp_middleagg %>% arrange(rownames(varimp_middleagg))
varimp_middleagg


# Low aggregation-----------------------------------------
# Develop data frame from results
varimp_lowagg <- data.frame()
for (i in c(1:4,6,7)){
  var_imp <- as.data.frame(importance(rf_strat_lowagg[[i]])[,])
  MeanDecreaseAccuracy <- rownames(var_imp[var_imp[, 3] == max(var_imp[, 3]), ])
  MeanDecreaseImpurity <- rownames(var_imp[var_imp[, 4] == max(var_imp[, 4]), ])
  varimp_lowagg <- rbind(varimp_lowagg, 
                         cbind(MeanDecreaseAccuracy, MeanDecreaseImpurity))
}

# Renaming
rownames(varimp_lowagg) <- estimations_value[c(1:4,6,7)]
varimp_lowagg$MeanDecreaseAccuracy <- gsub("_", " ",varimp_lowagg$MeanDecreaseAccuracy)
varimp_lowagg$MeanDecreaseImpurity <- gsub("_", " ",varimp_lowagg$MeanDecreaseImpurity)
varimp_lowagg <- varimp_lowagg %>% arrange(rownames(varimp_lowagg))
varimp_lowagg



write.csv(varimp_middleagg, "varimp_midagg.csv",row.names=T)
write.csv(varimp_lowagg, "varimp_lowagg.csv",row.names=T)

