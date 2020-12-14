# Midterm_Project_NF


## About the project

Repository for the midterm project

The combine_clean_data.r file takes in a 10 years of cdc data, diagnosis codes, and medical code; and outputs a binary dataframe that contains information on if a particular person has a specific diagnosis or takes a particular opioid.

## Functions in combine_clean_data.r 

 - combine_columns(df, list) : The function takes in a df and a column to grab the selected columns from each dataframe
 - whitespace2NA(column) :The function takes in a column and changes the white space to NA
 - dash_to_nothing(column) : The function takes in a column and gets rid of the dashes
 - bicols(data, list): The function takes in a data frame and a list of codes to return binary column containing if the code was in the list

## Code Inputs

Requires the following inputs: 

 - The combine_clean_data.r file takes in 16 data files from the 'data' folder
    - CDC 10 Years of Data: patient data from 2006 to 2015
    - BI_DATA_sample.Rda: Sample file to test what the final data should look like
    - new_names.Rda: A file containing a list of drug ids and diagnosis
    - ICD9_codes.RDA: A list of ICD9 codes
    - RFV_codes.RDA: A list of RFV codes
    - testdf.Rda: A file containing four Data Frames with different class types, used to test the bicols() function
    - OpioidCodesOnly.csv: A list of opioid codes
    - Parameter_OP_codes.csv: User inputed list of opioid codes
    
## Proceess

## Code Outputs

The code outputs clean data as bidata_clean.Rda and newDF.rda

## To run the Code

 1. Open the R Project file: Midterm_Project_NF.Rproj. 
 2. Make sure the directory is set to the project folder.
 3. Run the code.
 
