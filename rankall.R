rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", 
                   colClasses = "character") 
  data<-na.omit(data)
  
  ## Check that state and outcome are valid
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia"))
    stop("invalid outcome")
  
  if (class(num) != "numeric" && (!num %in% c("best","worst")))
    stop("invalid num")
  
  ## For each state, find the hospital of the given rank
  states<-c()
  for (state in unique(data[,7])) {
    
    if (outcome == "heart attack")
      datastate <- data[data[,7] == state, c(2,11)]
    else 
      if(outcome == "pneumonia")
        datastate <- data[data[,7] == state,c(2,23)]
      else
        if (outcome == "heart failure")
          datastate <- data[data[,7] == state,c(2,17)]
  
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    datastate <- datastate[datastate[,2] != "Not Available",]  
    dsorted<-datastate[order(as.numeric(datastate[[2]]),datastate[[1]],decreasing=FALSE,na.last=NA),]
    if (class(num) == "numeric")
      if(dim(dsorted)[1] > num)
        hosp<-dsorted[num,1]
      else
        hosp<- NA
    else
      if(num == "best")
        hosp<- dsorted[1,1]
      else
        hosp<- dsorted[dim(dsorted)[1],1]

    states<-rbind(states,c(hosp,state))
  }
  data.frame(hospital=states[1:dim(states)[1],1], state=states[1:dim(states)[1],2])
}