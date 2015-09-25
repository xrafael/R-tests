pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!me
  data = matrix(nrow = 0, ncol = 4)
  
  for (i in id)
  {
    if (i < 10)
      f = paste0("00",i)
    else
    {
      if (i< 100)
          f = paste0("0",i)
      else
        f = i
    }
    filename = paste0(directory, "/", f, ".csv")
    print("hola", filename)
    d <- cbind(read.table(filename, sep = ","))
    data <- rbind(data,d)
  }
  
  data
#   
#   # Clean data
#   good <- complete.cases(data)
#   clean <- data[good, ]
#   
#   #Compute mean of appropiated column
#   if (pollutant == "sulfate")
#     result <- mean(c(clean[2:dim(clean)[1],2]))
#   else
#     result <- mean(c(clean[2:dim(clean)[1],3]))
#   
#   result
}