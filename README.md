# Neural_Computation_2024 

 The code can be applied to measurement samples collected from one feature at the time. 

# Inputs:

There are two essential datasets used as inputs for the main.R code. The file name and column names should be named exactly the way instructed here. 

A. The Excel file named "Pairs.xlsx" contains the IDs (names) of the two members of the paired observations along with their values.

.xlsx file name: "Pairs.xlsx" with four columns as follows: "G1_ID"    "G1_Value" "G2_ID"    "G2_Value"

B. The Excel file named "Complete_Set.xlsx" contains the complete sample of the observations for the feature.

.xlsx file name: Complete_Set.xlsx with two columns: "Feature_ID"    "Feature_Value"

Place both Excel files in the same directory as the main file.

# Outputs:

1. SISC_Output.csv. Output saved as a CSV file containing the precision level, entropy, and Eta (efficiency ratio).

2. A .CSV file named "Bins.csv" that contains K number of bins starting and ending values. Example bin: [a,b] where "a" is the start value and "b" is the end value for that bin.  
Each row in the CSV file is a bin. (Total of K rows in the .CSV file for the K bins (states).)

3. A .txt file named "List_Bins_Content.txt" that contains those values of the feature that fall within each of the bins (states) range. 




# By Mohammad Samavat, Email: msamavat@ucsd.edu
For questions, please contact Mohammad Samavat. 
