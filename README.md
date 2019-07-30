# Data Analysis - Industrial Blending Systems Performance
## Description
This data analysis was completed to assess the performance of two blending plants, specifically, to assess how Plant 2 (P2) performs compared to Plant 1 (P1). 

Each plant has a powder blending system that produces finished product blends per a custom recipe at a specified order weight. Since order size is much greater than blender capacity, the order is made in a sequence of batches. The finished product quality is assessed by the accuracy of each ingredient's composition of the total order compared to the recipe target percent. 

Note that the data used here is dummy data modified to be used for demonstration purposes to maintain confidentiality of the data set.

## Documents
Plant 1
Data for Plant 1 is located in the P1 - Logger Tables.xlsx which would have been an export from a Microsoft Access database.
The Order Blend Accuracy - P1.R script executes the source data cleaning. 

Plant 2
Data for Plant 2 is located in P2- SQL Database Query3.xlsx which would have been an exported query of two SQL tables. 
The Order Blend Accuracy - P2.R script executes the source data cleaning. 
The P2 - Analysis of Errors in Data.Rmd looks at the progress that has been made to reduce data collection errors to improve the data set accuracy. 

Comparison
Analysis was completed using R-Studio in the notebook Order Blend Accuracy - Plant Comparison.Rmd. 

Various graphs are saved as pdfs from the notebooks as well for more detailed analysis and review. 

