corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  filenames <- list.files(directory, pattern="*.csv", full.names=TRUE)
  ldf <- lapply(filenames, read.csv)
  df=ldply(ldf) 

  dat = complete.cases(df)
  data<-df[dat == TRUE, ]
  
  result <- count(data[,4])
  res <- rename(result, c("x"="id", "freq"="nobs"))

  good <- res[(res$nobs)>threshold,]$id
  if (length(good) == 0) {
    res <- 0
  }
  else {
    res <- c()
    for (g in good)
    {
      dataMon = data[data$ID == g,]
      res <- c(res, cor(dataMon[,2],dataMon[,3]))
    }
  }
  res
}