########################### Ranking best hospitals per state by condition ###########################
#####################################################################################################

rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv")
  colnames(data)[c(2, 7, 11, 17, 23)] <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  ## Check that outcome is valid
  if(!outcome %in% colnames(data)[c(11, 17, 23)]) stop("invalid outcome")
  
  ## For each state, find the hospital of the given rank
  else {
    states <- unique(data$state)
    rv <- data.frame(matrix(NA, 0, 2))
    colnames(rv) <- c("hospital", "states")
    
    for(i in states){
      myorder <- data[data$state == i, c(2, 7, 11, 17, 23)]
      myorder <- myorder[order(as.numeric(myorder[[outcome]]), myorder[[1]], na.last = NA), ]
      
      if(num == "best") {myorder <- data.frame(hospital = myorder[1, 1], state = i)}
      else if(num == "worst") {myorder <- data.frame(hospital = myorder[nrow(myorder), 1], state = i)}
      else {myorder <- data.frame(hospital = myorder[num, 1], state = i)}
      rv <- rbind(rv, myorder)
    }
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    rv <- rv[order(rv$state), ]
  }
}