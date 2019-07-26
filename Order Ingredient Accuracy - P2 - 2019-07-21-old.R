library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(readxl)
library(ggQC)
library(sm)
library(skimr)
library(qwraps2)
library(lattice)
library(reshape2)
library(knitr)


##Background
#This data set is from powder blending system. The plant produces truck loads (orders) of finished product, each with its own recipe typically. The order specifies the recipe to use and weight desired. The order must be blended in smaller batches due to blender capacity limits. The finished product is assessed by the accuracy of each ingredient's composition of the total order compared to the recipe target percent. 

##Notebook Purpose 
#To create a reliable measure of performance and clean the data for use in analysis.

##Data Import
#Data for each order is logged in a SQL database consisting of 3 tables. 
#Import the data and take a look at the fields. 
#Note that data has been imported into excel and modified to be used for demonstration purposes to maintain confidentiality of the data set.

fileP2 <- './P2 - SQL Database Query2.xlsx'
sheetnameP2a <- 'SQL Batch_Additions'
sheetnameP2b <- 'SQL Orders'

BatchP2 <- read_excel(fileP2,sheet = sheetnameP2a,col_names = TRUE)
OrdersP2 <- read_excel(fileP2,sheet = sheetnameP2b,col_names = TRUE)

head(BatchP2,100)
head(OrdersP2)


#Combine data from all tables to one dataframe. 
#Each customer has their own recipe identified by a unique Engineering Number. 
#Every order made follows a recipe and has a unique Lot Number. 


BatchOrdersP2 <- merge(x = BatchP2, y = OrdersP2, by = "LOT_NUM", all.x = TRUE)

#convert material names to IDs
matfile <- './Material ID.xlsx'
sheetnameID <- 'use table'
MatNametoID <- read_excel(matfile,sheet = sheetnameID,col_names = TRUE)
MatIDP2 <- merge(x = BatchOrdersP2, y = MatNametoID, by = "ACTUAL_PROD", all.x = TRUE)

DataP2 <-  MatIDP2

#Convert data to unidentifiable
lotmod <- 0
DataP2$LOT_NUM <- DataP2$LOT_NUM + lotmod


head(DataP2,30)


##Data Cleaning

#It is known that occasionally the actual weight for an ingredient for a batch will not get logged due to data collection errors. In such cases, the entire row in the Batch Additions table will be missing for that batch ingredient addition. When this happens, the total weight for that ingredient with the missing value will be erroroneous. Sometimes when this happens the ingredient weight gets logged as the next ingreidnet so there may be mulitple errors in weights for that order. Let's remove these orders from the data set completely. 

#Let's also remove data with na, any duplicate lines in case they arose, and data before 1/1/19 as we only started collecting data then, so anything else would be erroneous. 

#in order to identify orders that have missing data, we assume that the # of ingredients would not be the same for each batch of the load. 
missingdataorders <- DataP2 %>% group_by(LOT_NUM,BATCH_NUM) %>% summarize(ningreds=n_distinct(ACTUAL_PROD)) %>% group_by(LOT_NUM) %>% summarize(n_diffningreds=n_distinct(ningreds))
#head(missingdataordersP1)

#create a data frame for orders that don't have missing data
nomissingdata <- missingdataorders[missingdataorders$n_diffningreds == 1,]


#remove data taht may cause issues
iDataP2Filtered <- DataP2 %>%  distinct(LOT_NUM,ACTUAL_PROD, BATCH_NUM, .keep_all = TRUE) %>%
                        filter(LOT_NUM %in% nomissingdata$LOT_NUM)%>% 
                       #date was input wrong year on some. let's just omit this data for plotting vs time 
                        filter(END_TIME > "2019-01-01") %>% 
                        na.omit()

#determine how many addition rows were removed from this process
iP2OrigFilteredPoints <- nrow(DataP2)
iP2FilteredPoints <- nrow(iDataP2Filtered)

print(iP2OrigFilteredPoints)
print(iP2FilteredPoints)
print(paste('Data reduced to ', iP2FilteredPoints/iP2OrigFilteredPoints*100,"%"))



#Estimated that 1 batch/2 days might have issues with missing data. This seems reasonable that ~10% of loads had to be removed. Issue expecte to have been fixed 7/2/19.

#in order to identify orders that have errors in the weights, we assume that the target range should not be more than 100 lbs   
errororders <- DataP2 %>% group_by(LOT_NUM, MaterialID) %>% summarize(TarMax = max(TARGET), TarMin=min(TARGET), TarRange = TarMax-TarMin,ActMax = max(ACTUAL), ActMin=min(ACTUAL), ActRange = ActMax-ActMin )
#head(missingdataordersP1)

#check that an ingredient in this batch has a larger range to determine what cut off to remove
#head(errororders[errororders$LOT_NUM==4021386,])

#create a data frame for orders that  have errors
errorsdata <- errororders[errororders$TarRange > 100 | errororders$ActRange > 100,]

#remove data that may cause issues
iiDataP2Filtered <- iDataP2Filtered %>%  
                        filter(!LOT_NUM %in% 
                                 errorsdata$LOT_NUM)

#identify any batches with MaterialID =0 
materialID0 <- DataP2 %>% group_by(LOT_NUM, MaterialID) %>% summarize(MatIDMin = min(MaterialID))

#create a data frame for orders that  have a Material with ID = 0 and remove these orders
orderserrorsMaterial <- materialID0[materialID0$MatIDMin ==0,]
iiiDataP2Filtered <- iiDataP2Filtered %>%  
                        filter(!LOT_NUM %in% 
                                 orderserrorsMaterial$LOT_NUM)

#identify any batches with only 1 batch - indicates it was probably aborted or something did not go as expected - we don't need to include these in our analysis 
batches <- DataP2 %>% group_by(LOT_NUM) %>% summarize(TotalBatches = max(BATCH_NUM))

#create a data frame for orders that  have a Material with ID = 0 and remove these orders
only1batch <- batches[batches$TotalBatches ==1,]
iiiiDataP2Filtered <- iiiDataP2Filtered %>%  
                        filter(!LOT_NUM %in% 
                                 only1batch$LOT_NUM)
                       
#determine how many addition rows were removed from this process
iiP2OrigPoints <- nrow(DataP2)
iiP2FilteredPoints <- nrow(iiiiDataP2Filtered)

print(iiP2OrigPoints)
print(iiP2FilteredPoints)
print(paste('Data reduced to ', iiP2FilteredPoints/iiP2OrigPoints*100,"%"))

DataP2Filtered <- iiiiDataP2Filtered


#Let's now summarize data by ingredient for each order. Total ingredient target weights by order need to be calculated. There are a variety of ways to do this so let's test multiple ways. 

iByingredP2 <- DataP2Filtered %>%
  group_by(LOT_NUM, MaterialID) %>%
  summarize(EndTime = max(END_TIME),
            IngredWeight = sum(ACTUAL, na.rm = TRUE))


head(iByingredP2,30)



#Let's add total actual weight to each load to calulate the difference vs target as a percent of the total load since this is how our spec is written.
#Any loads that don't have a batch 1 will not have a target calculated. 



#Determine actual total order wt and add value to data frame
byorderP2 <- DataP2Filtered %>% group_by(LOT_NUM) %>% summarise(ActTotalOrderWt = sum(ACTUAL),   OrderWt=max(TRGT_WGHT))
iiByingredP2 <- merge(x = iByingredP2, y =byorderP2, by = c("LOT_NUM"), all.x = TRUE)

#Determine first batch target total weight and add value to data frame
firstbatchP2 <- DataP2Filtered %>% group_by(LOT_NUM) %>% filter(BATCH_NUM==1) %>% summarise( FirstBatchTarWt=sum(TARGET))
iiiByingredP2 <- merge(x = iiByingredP2, y =firstbatchP2, by = c("LOT_NUM"), all.x = TRUE)

#Determine first batch target weight for each ingredient and add value to data frame
firstbatchingredP2 <- DataP2Filtered %>% group_by(LOT_NUM, MaterialID) %>%  arrange(BATCH_NUM)%>% summarize(FirstBatchIngredWeight = first(TARGET))
iiiiByingredP2 <- merge(x = iiiByingredP2, y =firstbatchingredP2, by = c("LOT_NUM", "MaterialID"), all.x = TRUE)

#Set all iterations equal to the dataframe for future use

ByingredP2 <- iiiiByingredP2

#Determine % ingredient based on first batch for target and actual
ByingredP2$TarPercbyFirst <- (ByingredP2$FirstBatchIngredWeight/ByingredP2$FirstBatchTarWt)*100
ByingredP2$ActPerc <- ByingredP2$IngredWeight/ByingredP2$ActTotalOrderWt*100

#Let's calculate the % error in ingredient % for the order
ByingredP2$OrderDiff_inPerc <- ByingredP2$ActPerc -ByingredP2$TarPercbyFirst 

