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

##Background
#This data set is from powder blending system. The plant produces truck loads (orders) of finished product, each with its own recipe typically. The order specifies the recipe to use and weight desired. The order must be blended in smaller batches due to blender capacity limits. The finished product is assessed by the accuracy of each ingredient's composition of the total order compared to the recipe target percent. 

##Notebook Purpose 
#To create a reliable measure of performance and clean the data for use in analysis.

##Data Import
#Data for each order is logged in a Microsoft Access database consisting of multiple tables. Useful data has been queried in SQL and exported to a an Excel file. 
#Import the data and take a look at the fields. 
#Note that data has been imported into excel and modified to be used for demonstration purposes to maintain confidentiality of the data set.


fileP1 <- './P1 - logger tables.xlsx'
sheetnameP1 <- 'export data'

DataP1 <- read_excel(fileP1,sheet = sheetnameP1,col_names = TRUE)
head(DataP1)

#Each customer has their own recipe identified by a unique Engineering Number. 
#Every order made follows a recipe and has a unique Lot Number. 
#Recipes are not exported with this data.

#Dates are broken down by component so let's combine into one date. We will be using End Date instead of Start Date as a reference point for the batch, so let's not worry about Start Date. 
DataP1$END_TIME <- ISOdate(DataP1$EndYY,DataP1$EndMM,DataP1$EndDD,DataP1$EndHH,DataP1$EndNN,DataP1$EndSS)

#Let's standardize on column names so we can use the same code. 
colnames(DataP1)[colnames(DataP1)=='Batch'] <- c("BATCH_NUM")
colnames(DataP1)[colnames(DataP1)=='Target'] <- c("TARGET")
colnames(DataP1)[colnames(DataP1)=='Lot'] <- c("LOT_NUM")
head(DataP1)


#Let's combine some ingredients with the same name

DataP1$MaterialID[DataP1$MaterialID==112086] <- 115102
DataP1$MaterialID[DataP1$MaterialID==112186] <- 115102
DataP1$MaterialID[DataP1$MaterialID==112210] <- 112221
DataP1$MaterialID[DataP1$MaterialID==112283] <- 112281
DataP1$MaterialID[DataP1$MaterialID==112292] <- 112281

##Data Cleaning

#For this system, system is currently set to stop if data is not transferred, so it is very unlikely that a row will be missing due to data logging error but there could be incomplete orders that were aborted before all ingredients were added.  Let's go ahead and remove any orders that do not have an equal number of additions logged for each batch to eliminate such cases regardless. 

#With the communication errors experienced on this system, duplicate rows are most likely.
#Let's also remove data with na, any duplicate lines in case they arose, and data before 1/1/18 as we only started exported data after that point, so anything else would be erroneous. 

#in order to identify orders that have missing data, we assume that the # of ingredients would not be the same for each batch of the load.
missingdataordersP1 <- DataP1 %>% group_by(LOT_NUM,BATCH_NUM) %>% summarize(ningreds=n_distinct(MaterialID)) %>% group_by(LOT_NUM) %>% summarize(n_diffningreds=n_distinct(ningreds))
#head(missingdataordersP1)

#create a data frame for orders that don't have missing data because # ingreds for each batch is the same
nomissingdataP1 <- missingdataordersP1[missingdataordersP1$n_diffningreds == 1,]
#head(nomissingdataP1)

#remove data taht may cause issues
DataP1Filtered <- DataP1 %>%  distinct(LOT_NUM,MaterialID, BATCH_NUM, .keep_all = TRUE) %>%
                        filter(LOT_NUM %in% nomissingdataP1$LOT_NUM)%>%
                        filter(END_TIME > "2018-01-01") %>% 
                        na.omit()

#determine how many addition rows were removed from this process
P1OrigPoints <- nrow(DataP1)
P1FilteredPoints <- nrow(DataP1Filtered)

print(P1OrigPoints)
print(P1FilteredPoints)
print(paste('Data reduced to ', P1FilteredPoints/P1OrigPoints*100,"%"))

#This seems reasonable that <1% of data was removed, most probably due to duplicate rows. 


#Let's now summarize data by ingredient for each order. Total ingredient target weights by order need to be calculated. There are a variety of ways to do this so let's test multiple ways. 

iByingredP1 <- DataP1Filtered %>%
  group_by(LOT_NUM, MaterialID) %>%
  summarize(EndTime = max(END_TIME),
            IngredWeight = sum(tblBlendBatchAdd.Weight, na.rm = TRUE))
      
#Let's add total actual weight to each load to calulate the difference vs target as a percent of the total load since this is how our spec is written TBD


#Determine actual total order wt and add value to data frame
byorderP1 <- DataP1Filtered %>% group_by(LOT_NUM) %>% summarise(ActTotalOrderWt = sum(tblBlendBatchAdd.Weight),   OrderWt=max(tblBlendOrder.Weight))
iiByingredP1 <- merge(x = iByingredP1, y =byorderP1, by = c("LOT_NUM"), all.x = TRUE)

#Determine first batch target total weight and add value to data frame
firstbatchP1 <- DataP1Filtered %>% group_by(LOT_NUM) %>% filter(BATCH_NUM==1) %>% summarise( FirstBatchTarWt=sum(TARGET))
iiiByingredP1 <- merge(x = iiByingredP1, y =firstbatchP1, by = c("LOT_NUM"), all.x = TRUE)

#Determine first batch target weight for each ingredient and add value to data frame
firstbatchingredP1 <- DataP1Filtered %>% group_by(LOT_NUM, MaterialID) %>%  arrange(BATCH_NUM)%>% summarize(FirstBatchIngredWeight = first(TARGET))
iiiiByingredP1 <- merge(x = iiiByingredP1, y =firstbatchingredP1, by = c("LOT_NUM", "MaterialID"), all.x = TRUE)

#Set all iterations equal to the dataframe for future use

ByingredP1 <- iiiiByingredP1

#Determine % ingredient based on first batch for target and actual
ByingredP1$TarPercbyFirst <- (ByingredP1$FirstBatchIngredWeight/ByingredP1$FirstBatchTarWt)*100
ByingredP1$ActPerc <- ByingredP1$IngredWeight/ByingredP1$ActTotalOrderWt*100

#Let's calculate the % error in ingredient % for the order
ByingredP1$OrderDiff_inPerc <- ByingredP1$ActPerc -ByingredP1$TarPercbyFirst 
