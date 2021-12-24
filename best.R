########################### Finding the best hospital in a state by condition ###########################
#########################################################################################################

# with base R
best <- function(state, outcome){
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv")
  colnames(data)[c(11, 17, 23)] <- c("heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  if(!state %in% data$State) stop("invalid state")
  else if(!outcome %in% colnames(data)[c(11, 17, 23)]) stop("invalid outcome")
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  else {
    myorder <- data[data$State == state, c(2, 11, 17, 23)]
    myorder <- myorder[order(as.numeric(myorder[[outcome]]), myorder[[1]]), ]
    myorder[1, 1]
  }
}
