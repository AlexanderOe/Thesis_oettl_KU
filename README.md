# Thesis_oettl_KU
This repository contains supplementary material from the Master's Thesis written by Alexander Öttl.
It is structured in three sections that contain the preparation of the data, the estimation of the analysis, and the robustness checks. 

## Data development - data_oettl_MT.R
For the thesis, 8 datasets are developed - divided into 2018 and 2019 observations
as well as 3 levels of aggregation. Starting from highest to lowest level
of aggregation. Furthermore, the highest level of aggregation is divided 
into volume and value observations.


## Predictive Modeling - estimations_oettl_MT.R
This script is structured as the thesis with three subsections in the
analysis. First, the relation of food purchases and environmental concerns
is investigated using inference statistical methods. Second, the machine
learning algorithm - random forest classification - is applied for 
predictive modeling from 2018 to 2019. There are three different 
feature specifications (aggregations) to predict six different environmental
concerns. Third, the variable importance of the estimated random forests are
examined. As a preamble, a descriptive table is developed for the highest 
aggregation level. 

Note: Depending on the computational power this script runs approx. 20 min


## Robustness checks - robustness_oettl_MT.R
The robustness checks contain two sections. First, a gradient boosting
machine is estimated. It is similarly presented to the random forests,
therefore, the confusion matrix plot function is defined at the beginning.
Second, the random forest estimations are recalculated with a balanced panel.
Hence, the forest is only trained on 2018 observations that are also in 2019
and only has to predict 2019 observations that are also in 2018. 

Note: Depending on the computational power this script runs approx. 35 min
