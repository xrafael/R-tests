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
  data = matrix(nrow=0, ncol = 4)
  filenames <- list.files(directory, pattern="*.csv")
  fs <- paste(directory, filenames, sep="/")
  for (f in fs){
    d <- read.table(f, sep = ",")
    data <- rbind(data, d)
  }
  
  # Clean data
  good <- complete.cases(data)
  clean <- data[good, ]

  # select ids
  good <- clean[,4] == id
  selected <- clean[good,]
  
  #Compute mean of appropiated column
  if (pollutant == "sulfate")
    result <- mean(c(selected[,2]))
  else
    result <- mean(c(selected[,3]))
  
  result
}