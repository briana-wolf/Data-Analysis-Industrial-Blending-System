##Import Data

fileP2 <- './P2 - SQL Database Query3.xlsx'
sheetnameP2a <- 'SQL Batch_Additions'
sheetnameP2b <- 'SQL Orders'

BatchP2 <- read_excel(fileP2,sheet = sheetnameP2a,col_names = TRUE)
OrdersP2 <- read_excel(fileP2,sheet = sheetnameP2b,col_names = TRUE)

#Combine data from all tables to one dataframe. 
BatchOrdersP2 <- merge(x = BatchP2, y = OrdersP2, by = "LOT_NUM", all.x = TRUE)
DataP2 <-  BatchOrdersP2

##Data Cleaning



#~~~~Missing Data
#It is known that occasionally the actual weight for an ingredient for a batch will not get logged due to data collection errors. In such cases, the entire row in the Batch Additions table will be missing for that batch ingredient addition. When this happens, the total weight for that ingredient with the missing value will be erroroneous. Sometimes when this happens the ingredient weight gets logged as the next ingreidnet so there may be mulitple errors in weights for that order. Let's remove these orders from the data set completely. 

#To identify orders that have missing data, assume that the # of ingredients would not be the same for each batch of the load. 
missingdataorders <- DataP2 %>% group_by(LOT_NUM,BATCH_NUM) %>% summarize(ningreds=n_distinct(MaterialID)) %>% group_by(LOT_NUM) %>% summarize(n_diffningreds=n_distinct(ningreds))

#create a data frame for orders that don't have missing data
nomissingdata <- missingdataorders[missingdataorders$n_diffningreds == 1,]

#remove these orders from data set
iDataP2Filtered <- DataP2 %>%  distinct(LOT_NUM,MaterialID, BATCH_NUM, .keep_all = TRUE) %>%
                        filter(LOT_NUM %in% nomissingdata$LOT_NUM)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``


#~~~~Wrong Values Errors

#to identify orders that have errors in the weights, we assume that the target range should not be more than 100 lbs   
errororders <- DataP2 %>% group_by(LOT_NUM, MaterialID) %>% summarize(TarMax = max(TARGET), TarMin=min(TARGET), TarRange = TarMax-TarMin,ActMax = max(ACTUAL), ActMin=min(ACTUAL), ActRange = ActMax-ActMin )
#head(missingdataordersP1)

#create a data frame for orders that  have errors
errorsdata <- errororders[errororders$TarRange > 100 | errororders$ActRange > 100,]

#remove data that may cause issues
iiDataP2Filtered <- iDataP2Filtered %>%  
                        filter(!LOT_NUM %in% 
                                 errorsdata$LOT_NUM)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`

#~~~~MatID Errors
#identify any batches with MaterialID =0 
materialID0 <- DataP2 %>% group_by(LOT_NUM, MaterialID) %>% summarize(MatIDMin = min(MaterialID))

#create a data frame for orders that  have a Material with ID = 0 and remove these orders
orderserrorsMaterial <- materialID0[materialID0$MatIDMin ==0,]
iiiDataP2Filtered <- iiDataP2Filtered %>%  
                        filter(!LOT_NUM %in% 
                                 orderserrorsMaterial$LOT_NUM)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`

#~~~~Single Batch Errors
#identify any batches with only 1 batch - indicates it was probably aborted or something did not go as expected - we don't need to include these in our analysis 
batches <- DataP2 %>% group_by(LOT_NUM) %>% summarize(TotalBatches = max(BATCH_NUM))

#create a data frame for orders that  have a Material with ID = 0 and remove these orders
only1batch <- batches[batches$TotalBatches ==1,]
iiiiDataP2Filtered <- iiiDataP2Filtered %>%  
                        filter(!LOT_NUM %in% 
                                 only1batch$LOT_NUM)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`

#~~~~Misc Errors
#Remove data with na, 
#Remove any duplicate lines in case they arose, 
#Remove data before 1/1/19. This is when data collection began so anything prior would be erroroneous. 

#date was input wrong year on some. let's just omit this data for plotting vs time 
iiiiiDataP2Filtered <- iiiiDataP2Filtered %>% filter(END_TIME > "2019-01-01") %>% 
  na.omit()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``
 
##Summarize Cleaning
#determine how many addition rows were removed from this process
iiP2OrigPoints <- nrow(DataP2)
iiP2FilteredPoints <- nrow(iiiiiDataP2Filtered)

print(iiP2OrigPoints)
print(iiP2FilteredPoints)
print(paste('Data reduced to ', iiP2FilteredPoints/iiP2OrigPoints*100,"%"))

DataP2Filtered <- iiiiiDataP2Filtered

##Summarize data by each ingredient for each order. 
#Need to compare actual ingredient % of order and target ingredient % of order for each ingredient in each order 

#~~~~Calc actual ingredient weights of order
#Calc actual ingredient weight for each order
iByingredP2 <- DataP2Filtered %>%
  group_by(LOT_NUM, MaterialID) %>%
  summarize(EndTime = max(END_TIME),
            IngredWeight = sum(ACTUAL, na.rm = TRUE))

#Calc actual total order wt and add value to data frame
byorderP2 <- DataP2Filtered %>% group_by(LOT_NUM) %>% summarise(ActTotalOrderWt = sum(ACTUAL),   OrderWt=max(TRGT_WGHT))
iiByingredP2 <- merge(x = iByingredP2, y =byorderP2, by = c("LOT_NUM"), all.x = TRUE)

#~~~~Calc target ingredient weights of order
#Calc first batch target total weight and add value to data frame
#Any loads that don't have a batch 1 will not have a target calculated. 
firstbatchP2 <- DataP2Filtered %>% group_by(LOT_NUM) %>% filter(BATCH_NUM==1) %>% summarise( FirstBatchTarWt=sum(TARGET))
iiiByingredP2 <- merge(x = iiByingredP2, y =firstbatchP2, by = c("LOT_NUM"), all.x = TRUE)

#Calc first batch target weight for each ingredient and add value to data frame
firstbatchingredP2 <- DataP2Filtered %>% group_by(LOT_NUM, MaterialID) %>%  arrange(BATCH_NUM)%>% summarize(FirstBatchIngredWeight = first(TARGET))
iiiiByingredP2 <- merge(x = iiiByingredP2, y =firstbatchingredP2, by = c("LOT_NUM", "MaterialID"), all.x = TRUE)

#Set all iterations equal to the dataframe for future use
ByingredP2 <- iiiiByingredP2

#~~~~Calc % ingredient based on first batch for target and actual
ByingredP2$TarPercbyFirst <- (ByingredP2$FirstBatchIngredWeight/ByingredP2$FirstBatchTarWt)*100
ByingredP2$ActPerc <- ByingredP2$IngredWeight/ByingredP2$ActTotalOrderWt*100

#~~~~Calc difference in actual ingredient % to target ingredient % for the order 
ByingredP2$OrderDiff_inPerc <- ByingredP2$ActPerc -ByingredP2$TarPercbyFirst 

