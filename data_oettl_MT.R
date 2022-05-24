library(dplyr)
library(reshape2)
library(stringr)


# Data development ------------------------------------------------------
# Subsequently, 8 datasets are developed. Divided into 2018 and 2019 observations
# as well as 3 levels of aggregation. Starting from highest to lowest level
# of aggregation. Furthermore, the highest level of aggregation is divided 
# into volume and value observations.

# Individualize the working directories to run the script!
# data_folder indicates location to save newly developed datasets
# base_data_folder has to contain "purchases_agg_no_climate_1819.csv"
# and "purchase1820.csv"
data_folder <- "~/UCPH/Master_Thesis/Thesis_work/R_Thesis/Data"
base_data_folder <- "~/UCPH/Master_Thesis/Master_data/Final_data"



#############################
# HIGHEST AGGREGATION LEVEL #
#############################
# 2018-------------------------------------------------------------------
setwd(base_data_folder)
data <- read.csv("purchases_agg_no_climate_1819.csv", check.names=FALSE)

# get background data to merge again later
data_2018 <- data %>% filter(year==2018)
background_data <- data_2018[,c(1,11:17)]
background_data <- distinct(background_data, hhnr, .keep_all = TRUE)

# The highest aggregation is also divided into volume and value 
# VOLUME-----------------------------------------------------------------
# estimate yearly volumes
data_2018_volume <- data_2018 %>% group_by(hhnr, group) %>% summarise(across(volume, list(sum)))
data_2018_volume <- dcast(data_2018_volume,  hhnr ~ group, value.var = "volume_1")
data_2018_volume[is.na(data_2018_volume)] <- 0

# Faster merge instead of left join for computational speed (careful with sorting!)
data_2018_volume <- cbind(data_2018_volume, background_data[,2:8])

# drop data for missing survey
data_2018_volume <- na.omit(data_2018_volume)

# Aggregate predictors 
data_2018_volume$environmental_concern <- rowSums(data_2018_volume[,c(36:39,41,42)])
data_2018_volume$non_organic <- rowSums(data_2018_volume[,seq(2, 34, 2)])
data_2018_volume$organic <- rowSums(data_2018_volume[,seq(3, 35, 2)])
data_2018_volume$meat <- rowSums(data_2018_volume[,22:23])
data_2018_volume$dairy_egg <- rowSums(data_2018_volume[,c(10,11,24,25)])
data_2018_volume$fruit <- rowSums(data_2018_volume[,15:16])
data_2018_volume$vegetables <- rowSums(data_2018_volume[,c(28,29,32,33)])
data_2018_volume$alcohol <- rowSums(data_2018_volume[,2:3])
data_2018_volume$seafood <- rowSums(data_2018_volume[,12:13])
data_2018_volume$grain_based <- rowSums(data_2018_volume[,18:19])

# Estimate shares of yearly consumption 
data_2018_volume$total_value <- rowSums(data_2018_volume[,2:35])

for (i in c(2:35,44:52)){
  data_2018_volume[,i] <- data_2018_volume[,i]/data_2018_volume[,53]
}

# Check and change alphanumeric values in colname to avoid problems with RF function
colnames(data_2018_volume) <- gsub("[^[:alnum:]]", "_",colnames(data_2018_volume))

# save data 
setwd(data_folder)
write.csv(data_2018_volume, "data_2018_volume.csv",row.names=FALSE)

# VALUE------------------------------------------------------------------- 
# estimate yearly values 
data_2018_value <- data_2018 %>% group_by(hhnr, group) %>% summarise(across(value, list(sum)))
data_2018_value <- dcast(data_2018_value,  hhnr ~ group, value.var = "value_1")
data_2018_value[is.na(data_2018_value)] <- 0

# Faster merge instead of left join for computational speed (careful with sorting!)
data_2018_value <- cbind(data_2018_value, background_data[,2:8])

# drop data for missing survey
data_2018_value <- na.omit(data_2018_value)

# Aggregate predictors 
data_2018_value$environmental_concern <- rowSums(data_2018_value[,c(36:39,41,42)])
data_2018_value$non_organic <- rowSums(data_2018_value[,seq(2, 34, 2)])
data_2018_value$organic <- rowSums(data_2018_value[,seq(3, 35, 2)])
data_2018_value$meat <- rowSums(data_2018_value[,22:23])
data_2018_value$dairy_egg <- rowSums(data_2018_value[,c(10,11,24,25)])
data_2018_value$fruit <- rowSums(data_2018_value[,15:16])
data_2018_value$vegetables <- rowSums(data_2018_value[,c(28,29,32,33)])
data_2018_value$alcohol <- rowSums(data_2018_value[,2:3])
data_2018_value$seafood <- rowSums(data_2018_value[,12:13])
data_2018_value$grain_based <- rowSums(data_2018_value[,18:19])

# Estimate shares of yearly consumption 
data_2018_value$total_value <- rowSums(data_2018_value[,2:35])

for (i in c(2:35,44:52)){
  data_2018_value[,i] <- data_2018_value[,i]/data_2018_value[,53]
}

# Check and change alphanumeric values in colname to avoid problems with RF function
colnames(data_2018_value) <- gsub("[^[:alnum:]]", "_",colnames(data_2018_value))

# save data 
write.csv(data_2018_value, "data_2018_value.csv",row.names=FALSE)

rm(data_2018_value,data_2018_volume,data_2018,background_data)

# 2019-------------------------------------------------------------------
# get background data to merge again later (2019 obs with missing groups filtered)
data_2019 <- data %>% filter(year==2019)
data_2019 <- data_2019 %>% filter(group!="")
background_data <- data_2019[,c(1,11:17)]
background_data <- distinct(background_data, hhnr, .keep_all = TRUE)

# The highest aggregation is also divided into volume and value 
# VOLUME-----------------------------------------------------------------
# estimate yearly volumes
data_2019_volume <- data_2019 %>% group_by(hhnr, group) %>% summarise(across(volume, list(sum)))
data_2019_volume <- dcast(data_2019_volume,  hhnr ~ group, value.var = "volume_1")
data_2019_volume[is.na(data_2019_volume)] <- 0

# Faster merge instead of left join for computational speed (careful with sorting!)
data_2019_volume <- cbind(data_2019_volume, background_data[,2:8])

# drop data for missing survey
data_2019_volume <- na.omit(data_2019_volume)

# Aggregate predictors 
data_2019_volume$environmental_concern <- rowSums(data_2019_volume[,c(36:39,41,42)])
data_2019_volume$non_organic <- rowSums(data_2019_volume[,seq(2, 34, 2)])
data_2019_volume$organic <- rowSums(data_2019_volume[,seq(3, 35, 2)])
data_2019_volume$meat <- rowSums(data_2019_volume[,22:23])
data_2019_volume$dairy_egg <- rowSums(data_2019_volume[,c(10,11,24,25)])
data_2019_volume$fruit <- rowSums(data_2019_volume[,15:16])
data_2019_volume$vegetables <- rowSums(data_2019_volume[,c(28,29,32,33)])
data_2019_volume$alcohol <- rowSums(data_2019_volume[,2:3])
data_2019_volume$seafood <- rowSums(data_2019_volume[,12:13])
data_2019_volume$grain_based <- rowSums(data_2019_volume[,18:19])

# Estimate shares of yearly consumption 
data_2019_volume$total_value <- rowSums(data_2019_volume[,2:35])

for (i in c(2:35,44:52)){
  data_2019_volume[,i] <- data_2019_volume[,i]/data_2019_volume[,53]
}

# Check and change alphanumeric values in colname to avoid problems with RF function
colnames(data_2019_volume) <- gsub("[^[:alnum:]]", "_",colnames(data_2019_volume))

# save data 
write.csv(data_2019_volume, "data_2019_volume.csv",row.names=FALSE)

# VALUE------------------------------------------------------------------- 
# estimate yearly values 
data_2019_value <- data_2019 %>% group_by(hhnr, group) %>% summarise(across(value, list(sum)))
data_2019_value <- dcast(data_2019_value,  hhnr ~ group, value.var = "value_1")
data_2019_value[is.na(data_2019_value)] <- 0

# Faster merge instead of left join for computational speed (careful with sorting!)
data_2019_value <- cbind(data_2019_value, background_data[,2:8])

# drop data for missing survey
data_2019_value <- na.omit(data_2019_value)

# Aggregate predictors 
data_2019_value$environmental_concern <- rowSums(data_2019_value[,c(36:39,41,42)])
data_2019_value$non_organic <- rowSums(data_2019_value[,seq(2, 34, 2)])
data_2019_value$organic <- rowSums(data_2019_value[,seq(3, 35, 2)])
data_2019_value$meat <- rowSums(data_2019_value[,22:23])
data_2019_value$dairy_egg <- rowSums(data_2019_value[,c(10,11,24,25)])
data_2019_value$fruit <- rowSums(data_2019_value[,15:16])
data_2019_value$vegetables <- rowSums(data_2019_value[,c(28,29,32,33)])
data_2019_value$alcohol <- rowSums(data_2019_value[,2:3])
data_2019_value$seafood <- rowSums(data_2019_value[,12:13])
data_2019_value$grain_based <- rowSums(data_2019_value[,18:19])

# Estimate shares of yearly consumption 
data_2019_value$total_value <- rowSums(data_2019_value[,2:35])

for (i in c(2:35,44:52)){
  data_2019_value[,i] <- data_2019_value[,i]/data_2019_value[,53]
}

# Check and change alphanumeric values in colname to avoid problems with RF function
colnames(data_2019_value) <- gsub("[^[:alnum:]]", "_",colnames(data_2019_value))

# save data 
write.csv(data_2019_value, "data_2019_value.csv",row.names=FALSE)

rm(data, background_data, data_2019, data_2019_value, data_2019_volume, i)



#############################
# MIDDLE AGGREGATION LEVEL #
#############################
# Getting yearly base for organic 
# Loaded data will be reduced subsequently for more detailed subdivision
setwd(base_data_folder)
data <- read.csv("purchase1820.csv", check.names=FALSE)

# 2018-------------------------------------------------------------------
data_2018 <- data %>% filter(year==2018)
background_data <- data_2018[,c(1,28:34)]
background_data <- distinct(background_data, hhnr, .keep_all = TRUE)

# Product identification by connecting only organic 
# AVOID na clash by replacing with none
data_2018[,10:23][is.na(data_2018[,10:23])] <- "none"
data_2018$product_ID <- str_c(data_2018[,24],"_", data_2018[,23])

# Summarize over year per HH 
data_sum <- data_2018 %>% group_by(hhnr, product_ID) %>% summarise(across(value, list(sum)))

# transposing with relevant variables only value 
data_2018_org <- dcast(data_sum, hhnr ~ product_ID, value.var = "value_1")

# prepare for estimation and merge environmental concern
data_2018_org[is.na(data_2018_org)] <- 0
data_2018_org <- cbind(data_2018_org, background_data[,2:8])

# drop data for missing survey
data_2018_org <- na.omit(data_2018_org)

# Estimate aggregated environmental concern 
data_2018_org$environmental_concern <- rowSums(data_2018_org[,c(163:166,168,169)])

# Estimate shares 
data_2018_org$total_value <- rowSums(data_2018_org[,2:162])

for (i in 2:162){
  data_2018_org[,i] <- data_2018_org[,i]/data_2018_org[,171]
}

# Check and change alphanumeric values in colname to avoid problems with RF function
colnames(data_2018_org) <- gsub("[^[:alnum:]]", "_",colnames(data_2018_org))

# Final data contains products subdivided in their organic structure 
# observed on a yearly bases as value share of total yearly consumption
setwd(data_folder)
write.csv(data_2018_org, "data_2018_org.csv",row.names=FALSE)

rm(data_2018_org, background_data, data_2018, data_sum)

# 2019-------------------------------------------------------------------
# get background data to merge again later (2019 obs with missing groups filtered)
data_2019 <- data %>% filter(year==2019)
data_2019 <- data_2019 %>% filter(group!="")
background_data <- data_2019[,c(1,28:34)]
background_data <- distinct(background_data, hhnr, .keep_all = TRUE)

# Product identification by connecting only organic 
# AVOID na clash by replacing with none
data_2019[,10:23][is.na(data_2019[,10:23])] <- "none"
data_2019$product_ID <- str_c(data_2019[,24],"_", data_2019[,23])

# Summarize over year per HH 
data_sum <- data_2019 %>% group_by(hhnr, product_ID) %>% summarise(across(value, list(sum)))

# transposing with relevant variables only value 
data_2019_org <- dcast(data_sum, hhnr ~ product_ID, value.var = "value_1")

# prepare for estimation and merge environmental concern
data_2019_org[is.na(data_2019_org)] <- 0
data_2019_org <- cbind(data_2019_org, background_data[,2:8])

# drop data for missing survey
data_2019_org <- na.omit(data_2019_org)

# Estimate aggregated environmental concern 
data_2019_org$environmental_concern <- rowSums(data_2019_org[,c(169:172,174,175)])

# Estimate shares 
data_2019_org$total_value <- rowSums(data_2019_org[,2:168])

for (i in 2:168){
  data_2019_org[,i] <- data_2019_org[,i]/data_2019_org[,177]
}

# Check and change alphanumeric values in colname to avoid problems with RF function
colnames(data_2019_org) <- gsub("[^[:alnum:]]", "_",colnames(data_2019_org))

# Final data contains products subdivided in their organic structure 
# observed on a yearly bases as value share of total yearly consumption
write.csv(data_2019_org, "data_2019_org.csv",row.names=FALSE)

rm(data_2019_org, background_data, data_2019, data_sum)



#############################
# LOWEST AGGREGATION LEVEL #
#############################
# Getting yearly base for dtype and organic 
# 2018-------------------------------------------------------------------
# get background data to merge again later
data_2018 <- data %>% filter(year==2018)
background_data <- data_2018[,c(1,28:34)]
background_data <- distinct(background_data, hhnr, .keep_all = TRUE)

# Product identification by connecting only dtype and organic 
# AVOID na clash by replacing with none
data_2018[,10:23][is.na(data_2018[,10:23])] <- "none"
data_2018$product_ID <- str_c(data_2018[,24],"_", data_2018[,10])
data_2018$product_ID <- str_c(data_2018[,35],"_", data_2018[,23])

# Summarize over year per HH 
data_sum <- data_2018 %>% group_by(hhnr, product_ID) %>% summarise(across(value, list(sum)))

# transposing with relevant variables only value 
data_2018_dtype_org <- dcast(data_sum, hhnr ~ product_ID, value.var = "value_1")

# prepare for estimation and merge environmental concern
data_2018_dtype_org[is.na(data_2018_dtype_org)] <- 0
data_2018_dtype_org <- cbind(data_2018_dtype_org, background_data[,2:8])

# drop data for missing survey
data_2018_dtype_org <- na.omit(data_2018_dtype_org)

# Estimate aggregated environmental concern 
data_2018_dtype_org$environmental_concern <- rowSums(data_2018_dtype_org[,c(1506:1509,1511,1512)])

# Estimate shares 
data_2018_dtype_org$total_value <- rowSums(data_2018_dtype_org[,2:1505])

for (i in 2:1505){
  data_2018_dtype_org[,i] <- data_2018_dtype_org[,i]/data_2018_dtype_org[,1514]
}

# Check and change alphanumeric values in colname to avoid problems with RF function
colnames(data_2018_dtype_org) <- gsub("[^[:alnum:]]", "_",colnames(data_2018_dtype_org))

# Final data contains products subdivided in dtype, organic
# observed on a yearly bases as value share of total yearly consumption
write.csv(data_2018_dtype_org, "data_2018_dtype_org.csv",row.names=FALSE)

rm(background_data,data_2018,data_2018_dtype_org,data_sum,i)

# 2019-------------------------------------------------------------------
# get background data to merge again later (2019 obs with missing groups filtered)
data_2019 <- data %>% filter(year==2019)
data_2019 <- data_2019 %>% filter(group!="")
background_data <- data_2019[,c(1,28:34)]
background_data <- distinct(background_data, hhnr, .keep_all = TRUE)

# Product identification by connecting only dtype and organic 
# AVOID na clash by replacing with none
data_2019[,10:23][is.na(data_2019[,10:23])] <- "none"
data_2019$product_ID <- str_c(data_2019[,24],"_", data_2019[,10])
data_2019$product_ID <- str_c(data_2019[,35],"_", data_2019[,23])

# Summarize over year per HH 
data_sum <- data_2019 %>% group_by(hhnr, product_ID) %>% summarise(across(value, list(sum)))

# transposing with relevant variables only value 
data_2019_dtype_org <- dcast(data_sum, hhnr ~ product_ID, value.var = "value_1")

# prepare for estimation and merge environmental concern
data_2019_dtype_org[is.na(data_2019_dtype_org)] <- 0
data_2019_dtype_org <- cbind(data_2019_dtype_org, background_data[,2:8])

# drop data for missing survey
data_2019_dtype_org <- na.omit(data_2019_dtype_org)

# Estimate aggregated environmental concern 
data_2019_dtype_org$environmental_concern <- rowSums(data_2019_dtype_org[,c(1532:1535,1537,1538)])

# Estimate shares 
data_2019_dtype_org$total_value <- rowSums(data_2019_dtype_org[,2:1531])

for (i in 2:1531){
  data_2019_dtype_org[,i] <- data_2019_dtype_org[,i]/data_2019_dtype_org[,1540]
}

# Check and change alphanumeric values in colname to avoid problems with RF function
colnames(data_2019_dtype_org) <- gsub("[^[:alnum:]]", "_",colnames(data_2019_dtype_org))

# Final data contains products subdivided in dtype, organic
# observed on a yearly bases as value share of total yearly consumption
write.csv(data_2019_dtype_org, "data_2019_dtype_org.csv",row.names=FALSE)


rm(list=ls())
