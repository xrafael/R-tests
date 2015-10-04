# Write a function called

rankhospital <- function(state, outcome, num){

  data <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", 
                   colClasses = "character") 
  data<-na.omit(data)
  
  #Validate params
  if (!state %in% data[,7])
    stop("invalid state")
  
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia"))
    stop("invalid outcome")
  
  if (class(num) != "numeric" && (!num %in% c("best","worst")))
    stop("invalid num")
  
  # Get data from outcome
  if (outcome == "heart attack"){
    data <- data[data[,7] == state, c(2,11)]}
  else 
  if(outcome == "pneumonia")
    data <- data[data[,7] == state,c(2,23)]
  else
  if (outcome == "heart failure"){
    outcome
    data <- data[data[,7] == state,c(2,17)]
  }
  
  data <- data[data[,2] != "Not Available",]  
  dsorted<-data[do.call("order", data[c(2,1)]),]
  
  if (class(num) == "numeric")
    if(dim(dsorted)[1] > num)
      dsorted[num,1]
    else
      NA
  else
    if(num == "best")
      dsorted[1,1]
    else
      dsorted[dim(dsorted)[1],1]
}
