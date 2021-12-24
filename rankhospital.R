########################### Ranking hospitals by outcome in a state ###########################
###############################################################################################

# base R approach
rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv")
  colnames(data)[c(11, 17, 23)] <- c("heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  if(!state %in% data$State) stop("invalid state")
  else if(!outcome %in% colnames(data)[c(11, 17, 23)]) stop("invalid outcome")
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  else {
    myorder <- data[data$State == state, c(2, 11, 17, 23)]
    myorder <- myorder[order(as.numeric(myorder[[outcome]]), myorder[[1]], na.last = NA), ]
    
    if(num == "best") myorder[1, 1]
    else if(num == "worst") myorder[nrow(myorder), 1]
    else myorder[num, 1]
  }
}