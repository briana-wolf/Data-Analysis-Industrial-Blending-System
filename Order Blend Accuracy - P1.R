
##Import Data 

fileP1 <- './P1 - logger tables.xlsx'
sheetnameP1 <- 'export data'

DataP1 <- read_excel(fileP1,sheet = sheetnameP1,col_names = TRUE)

##Simplify with P2 Names

#Dates are broken down by component so let's combine into one date. We will be using End Date instead of Start Date as a reference point for the batch, so let's not worry about Start Date. 
DataP1$END_TIME <- ISOdate(DataP1$EndYY,DataP1$EndMM,DataP1$EndDD,DataP1$EndHH,DataP1$EndNN,DataP1$EndSS)

#Let's standardize on column names so we can use the same code. 
colnames(DataP1)[colnames(DataP1)=='Batch'] <- c("BATCH_NUM")
colnames(DataP1)[colnames(DataP1)=='Target'] <- c("TARGET")
colnames(DataP1)[colnames(DataP1)=='Lot'] <- c("LOT_NUM")

#Let's combine some ingredients with the same name
DataP1$MaterialID[DataP1$MaterialID==112086] <- 115102
DataP1$MaterialID[DataP1$MaterialID==112186] <- 115102
DataP1$MaterialID[DataP1$MaterialID==112210] <- 112221
DataP1$MaterialID[DataP1$MaterialID==112283] <- 112281
DataP1$MaterialID[DataP1$MaterialID==112292] <- 112281


##Data Cleaning


#~~~~Missing Data
#For this system, system is currently set to stop if data is not transferred, so it is very unlikely that a row will be missing due to data logging error but there could be incomplete orders that were aborted before all ingredients were added.  Let's go ahead and remove any orders that do not have an equal number of additions logged for each batch to eliminate such cases regardless. 


#to identify orders that have missing data, assume that the # of ingredients would not be the same for each batch of the load.
missingdataordersP1 <- DataP1 %>% group_by(LOT_NUM,BATCH_NUM) %>% summarize(ningreds=n_distinct(MaterialID)) %>% group_by(LOT_NUM) %>% summarize(n_diffningreds=n_distinct(ningreds))

#create a data frame for orders that don't have missing data because # ingreds for each batch is the same 
nomissingdataP1 <- missingdataordersP1[missingdataordersP1$n_diffningreds == 1,]

#~~~~~~~~~~~~~~~~~~~~~

#Remove duplicate rows - most likely error with the communication errors experienced on this system,
#Remove data with na
#Remove data before 1/1/19 to study the same time period as P2 

#Remove data for all such orders with errors from data frame
DataP1Filtered <- DataP1 %>%  distinct(LOT_NUM,MaterialID, BATCH_NUM, .keep_all = TRUE) %>%
                        filter(LOT_NUM %in% nomissingdataP1$LOT_NUM)%>%
                        filter(END_TIME > "2018-01-01") %>% 
                        na.omit()


##Summarize Cleaning
#determine how many addition rows were removed from this process
P1OrigPoints <- nrow(DataP1)
P1FilteredPoints <- nrow(DataP1Filtered)

print(P1OrigPoints)
print(P1FilteredPoints)
print(paste('Data reduced to ', P1FilteredPoints/P1OrigPoints*100,"%"))

#This seems reasonable that <1% of data was removed, most probably due to duplicate rows. 

##Summarize data by each ingredient for each order. 
#Need to compare actual ingredient % of order and target ingredient % of order for each ingredient in each order 

#~~~~Calc actual ingredient weights of order
#Calc actual ingredient weight for each order
iByingredP1 <- DataP1Filtered %>%
  group_by(LOT_NUM, MaterialID) %>%
  summarize(EndTime = max(END_TIME),
            IngredWeight = sum(tblBlendBatchAdd.Weight, na.rm = TRUE))
      
#Calc actual total order wt and add value to data frame
byorderP1 <- DataP1Filtered %>% group_by(LOT_NUM) %>% summarise(ActTotalOrderWt = sum(tblBlendBatchAdd.Weight),   OrderWt=max(tblBlendOrder.Weight))
iiByingredP1 <- merge(x = iByingredP1, y =byorderP1, by = c("LOT_NUM"), all.x = TRUE)

#~~~~Calc target ingredient weights of order
#Calc first batch target total weight and add value to data frame
#Any loads that don't have a batch 1 will not have a target calculated
firstbatchP1 <- DataP1Filtered %>% group_by(LOT_NUM) %>% filter(BATCH_NUM==1) %>% summarise( FirstBatchTarWt=sum(TARGET))
iiiByingredP1 <- merge(x = iiByingredP1, y =firstbatchP1, by = c("LOT_NUM"), all.x = TRUE)

#Calc first batch target weight for each ingredient and add value to data frame
firstbatchingredP1 <- DataP1Filtered %>% group_by(LOT_NUM, MaterialID) %>%  arrange(BATCH_NUM)%>% summarize(FirstBatchIngredWeight = first(TARGET))
iiiiByingredP1 <- merge(x = iiiByingredP1, y =firstbatchingredP1, by = c("LOT_NUM", "MaterialID"), all.x = TRUE)

#Set all iterations equal to the dataframe for future use
ByingredP1 <- iiiiByingredP1

#~~~~Calc % ingredient based on first batch for target and actual
ByingredP1$TarPercbyFirst <- (ByingredP1$FirstBatchIngredWeight/ByingredP1$FirstBatchTarWt)*100
ByingredP1$ActPerc <- ByingredP1$IngredWeight/ByingredP1$ActTotalOrderWt*100

#~~~~Calc difference in actual ingredient % to target ingredient % for the order 
ByingredP1$OrderDiff_inPerc <- ByingredP1$ActPerc -ByingredP1$TarPercbyFirst 
