
best <- function(state, outcome){
  data <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", 
                      colClasses = "character") 
  data <- na.omit(data)
  
  if (state %in% data[,7])
  {
    if (outcome == "heart attack"){
      data <- data[data[,7] == state, c(2,11)]}
    else 
      {
        if (outcome == "heart failure"){
          outcome
          data <- data[data[,7] == state,c(2,17)]
        }
        else 
          if(outcome == "pneumonia")
            data <- data[data[,7] == state,c(2,23)]
          else
            stop("invalid outcome")
      }
    data <- data[data[,2] != "Not Available",]
    dsorted<-data[do.call("order", data[c(2,1)]),]
    dsorted[1,1]
 }
  else
    stop("invalid state")
}