
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
                if (nobs >= threshold & nobs != 0) {
                        correlation=cor(temp$sulfate, temp$nitrate, use="complete.obs")
                        goodcors=c(goodcors, correlation)
                }
                
        }        
        return(goodcors)
        ##Return a numeric vector of correlations 
}

