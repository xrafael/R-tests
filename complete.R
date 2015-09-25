complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  filenames <- sprintf("%03d.csv", id) 
  filenames <- paste(directory, filenames, sep="/") 
  ldf <- lapply(filenames, "read.csv") 
  df=ldply(ldf) 
  
  dat = complete.cases(df)
  data<-df[dat == TRUE, 4]

  result <- count(data)
  res <- rename(result, c("x"="id", "freq"="nobs"))
  res

}