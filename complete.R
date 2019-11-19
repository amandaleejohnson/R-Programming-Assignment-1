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
                #Instead of rbind, let's use a different strategy to retain the column names created in new_data
                #at the beginning:
                #OLD - new_data=rbind(new_data, to_insert) 
                new_data[nrow(new_data) + 1,] = to_insert
                #This is telling R - create a new row at the bottom of the dataframe and put in "to_insert"
        } 
        
        return(new_data)        
}
