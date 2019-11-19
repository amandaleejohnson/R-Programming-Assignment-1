# Part 1
# 
# Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) 
# across a specified list of monitors. 

# The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. 
# 
# Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate matter data from the directory 
# specified in the 'directory' argument and returns:
# the mean of the pollutant across all of the monitors, ignoring any missing values coded as NA. 
# # 
# The function that you write should be able to match this output. Please save your code to a file named pollutantmean.R.
# https://d18ky98rnyall9.cloudfront.net/_3b0da118473bfa0845efddcbe29cc336_pollutantmean-demo.html?Expires=1556409600&Signature=gZ2Y76elJa2I046s5UmWZhrrv5ky~jn1dkgU8zdxSaqond9AGuH1BX8W6pQ8oDC0ymLZ1kn0eJmQ~5mZf4AJKRgMVi3BNiyt6pvN3afLya0ZeQvIajvH6TOtZzJbF0xEx11X8DGpywCeAKTmvDZ82fhmCJ1g5MEHdrctVc6Vg-I_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A

pollutantmean=function(directory, pollutant, id=1:332) {
        ##directory = character vector of length 1 indicating the location of the CSV files
        
        ##pollutant = character vector of length 1 indicating the name of the pollutant
        ##for which we will calculate the mean; either "sulfate" or "nitrate".
        ##id = integer vector indicating the monitor ID numbers to be used
        ##Return the mean of the pollutant across all monitors list in the "id" vector
        joined_data = data.frame()
        for (i in id){
                new_i = ifelse(nchar(i) == 1, paste0("00",i),
                               ifelse(nchar(i) == 2, paste0("0",i), i))
                full_directory = paste0("C:/Users/ajohns34/Box/Data Science Specialization/", directory, "/")
                file_name = paste0(full_directory, new_i, ".csv")
                temp = read.csv(file = file_name)
                joined_data=rbind(joined_data, temp)
        } 
        
        return(mean(joined_data[[pollutant]], na.rm=TRUE))
        
}

