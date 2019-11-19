##Homework 2 - Week 2
setwd("C:/Users/ajohns34/Box/Data Science Specialization")

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


pollutantmean("specdata", "sulfate", 1:10)


##Try to do this outside of a function:
        setwd("C:/Users/ajohns34/Box/Data Science Specialization/specdata")

        d023 <- read.csv(file = "023.csv", header = TRUE)

        mean(d023$nitrate, na.rm=TRUE)

        #This matches the last example in the above link to sample output!
        
        
#Try building this out, still outside of a function        
pollutant = "nitrate"
id = c(1, 5, 9, 11, 2, 7)
joined_data = data.frame()
for (i in id){
        new_i = ifelse(nchar(i) == 1, paste0("00",i),
                       ifelse(nchar(i) == 2, paste0("0",i), i))
        file_dir = "C:/Users/ajohns34/Box/Data Science Specialization/specdata/"
        file_name = paste0(file_dir, new_i, ".csv")
        temp = read.csv(file = file_name)
        joined_data=rbind(joined_data, temp)
} 

print(mean(joined_data[[pollutant]], na.rm=TRUE))


#Part 2

#Write a function that reads a directory full of files and reports the number of completely observed cases in each data file. 
#The function should return a data frame where the first column is the name of the file 
#and the second column is the number of complete cases. 

complete = function(directory, id=1:332) {
        ##directory = character vector of length 1 indicating the location of CSV files
        ##id = integer vector indicating the monitor id numbers to be used
        ##returns a data frame of the form:
                ##id nobs
                ##1  117
                ##2  1041
                ##...
                ##where id is the monitor id number and obs is the number of complete cases
        new_data=data.frame(matrix(ncol=2, nrow=0)) #Start out with an empty data frame
        cols=c("id", "nobs")
        colnames(new_data) = cols
        for (i in id){
                ##Same as part 1:
                new_i = ifelse(nchar(i) == 1, paste0("00",i),
                               ifelse(nchar(i) == 2, paste0("0",i), i))
                file_dir = "C:/Users/ajohns34/Box/Data Science Specialization/specdata/"
                file_name = paste0(file_dir, new_i, ".csv")
                temp = read.csv(file = file_name)
                ##
                
                ##New stuff: 
                good=complete.cases(temp)
                nobs=sum(good)
                to_insert=c(i, nobs)
                new_data=rbind(new_data, to_insert)
        } 
        
        return(new_data)        
}

complete("specdata", c(2, 4, 8, 10, 12))


##Try to do this outside of a function:
        setwd("C:/Users/ajohns34/Box/Data Science Specialization/specdata")

        d3 <- read.csv(file = "003.csv", header = TRUE)
        
        #T/F which rows are complete?
        #good will give you a dataframe of "TRUES" and "FALSES" that identify which rows are complete
        good=complete.cases(d3)
        #Since TRUE=1 and FALSE=0, we can get a count of the number of trues:
        nobs=sum(good)

        #Now we want a new data frame that has two variables: id and nobs
        new_data=data.frame(matrix(ncol=2, nrow=0)) #Start out with an empty data frame
        cols=c("id", "nobs")
        colnames(new_data) = cols
        
        #Join the output from id=3 and nobs into the dataset:
        to_insert=c(3, nobs)
        new_data=rbind(new_data, to_insert)
        

#Try building this out, still outside of a function      
        id = c(2, 4, 8, 10, 12)
        new_data=data.frame(matrix(ncol=2, nrow=0)) #Start out with an empty data frame
        cols=c("id", "nobs")
        colnames(new_data) = cols
        for (i in id){
                ##Same as part 1:
                new_i = ifelse(nchar(i) == 1, paste0("00",i),
                               ifelse(nchar(i) == 2, paste0("0",i), i))
                file_dir = "C:/Users/ajohns34/Box/Data Science Specialization/specdata/"
                file_name = paste0(file_dir, new_i, ".csv")
                temp = read.csv(file = file_name)
                ##
                
                ##New stuff: 
                good=complete.cases(temp)
                nobs=sum(good)
                to_insert=c(i, nobs)
                
                #Instead of rbind, let's use a different strategy to retain the column names created in new_data
                #at the beginning:
                #OLD - new_data=rbind(new_data, to_insert) 
                new_data[nrow(new_data) + 1,] = to_insert
                #This is telling R - create a new row at the bottom of the dataframe and put in "to_insert"
        } 
        
#PART 3
        
#Write a function that takes a directory of data files and a threshold for complete cases and 
#calculates the correlation between sulfate and nitrate for monitor locations 
#where the number of completely observed cases (on all variables) is greater than the threshold. 
#The function should return a vector of correlations for the monitors that meet the threshold requirement. 
#If no monitors meet the threshold requirement, then the function should return a numeric vector of length 0. 
#A prototype of this function follows

corr = function(directory, threshold = 0) {
        ##directory = character vector of length 1 indicating the location of the csv files
        ##threshold = numeric vector of length 1 indicating the number of completely observed observations
        ##(on all variables) required to compute the correlation between nitrate and sulfate;
        ##default is 0!!
        goodcors=c()
        for (i in 1:332) {
                #Pull in the csv file coresponding to the id name
                new_i = ifelse(nchar(i) == 1, paste0("00",i),
                               ifelse(nchar(i) == 2, paste0("0",i), i))
                full_directory = paste0("C:/Users/ajohns34/Box/Data Science Specialization/", directory, "/")
                file_name = paste0(full_directory, new_i, ".csv")
                temp = read.csv(file = file_name)
                
                good=complete.cases(temp)
                nobs=sum(good)
                if (nobs >= threshold) {
                        correlation=cor(temp$sulfate, temp$nitrate, use="complete.obs")
                        goodcors=c(goodcors, correlation)
                }
                
        }        
        return(goodcors)
        ##Return a numeric vector of correlations 
}


##Try to do this outside of a function:
        setwd("C:/Users/ajohns34/Box/Data Science Specialization/specdata")
        
        d3 <- read.csv(file = "003.csv", header = TRUE)
        
        cor(d3$sulfate, d3$nitrate, use="complete.obs")

        ##This works, but doesn't establish a threshold for complete cases.
        ##What if we use the previous program to assess the number of complete cases for each data frame?
        good=complete.cases(d3)
        nobs=sum(good)
        nobs                
                #This says that data frame d3 has n=243 complete cases

        ##What if we make a new data frame that has the number of complete cases for every single csv file?
        id=1:332
        new_data=data.frame(matrix(ncol=2, nrow=0)) #Start out with an empty data frame
        cols=c("id", "nobs")
        colnames(new_data) = cols
        for (i in id){
                ##Same as part 1:
                new_i = ifelse(nchar(i) == 1, paste0("00",i),
                               ifelse(nchar(i) == 2, paste0("0",i), i))
                file_dir = "C:/Users/ajohns34/Box/Data Science Specialization/specdata/"
                file_name = paste0(file_dir, new_i, ".csv")
                temp = read.csv(file = file_name)
                ##
                
                ##New stuff: 
                good=complete.cases(temp)
                nobs=sum(good)
                to_insert=c(i, nobs)
                
                #Instead of rbind, let's use a different strategy to retain the column names created in new_data
                #at the beginning:
                #OLD - new_data=rbind(new_data, to_insert) 
                new_data[nrow(new_data) + 1,] = to_insert
                #This is telling R - create a new row at the bottom of the dataframe and put in "to_insert"
        } 
        
        #THIS WORKS!!!
        #THis is a new data frame with each csv and the number of complete cases
        #Now, we want to take a subset of this data frame if nobs is greater than a certain threshold
        goodcors=c()
        for (i in 1:332) {
                #Pull in the csv file coresponding to the id name
                new_i = ifelse(nchar(i) == 1, paste0("00",i),
                                ifelse(nchar(i) == 2, paste0("0",i), i))
                full_directory = paste0("C:/Users/ajohns34/Box/Data Science Specialization/", directory, "/")
                file_name = paste0(full_directory, new_i, ".csv")
                temp = read.csv(file = file_name)

                good=complete.cases(temp)
                nobs=sum(good)
                if (nobs >= 150) {
                        correlation=cor(temp$sulfate, temp$nitrate, use="complete.obs")
                        goodcors=c(goodcors, correlation)
                }
                
        }        
        return(goodcors)
















