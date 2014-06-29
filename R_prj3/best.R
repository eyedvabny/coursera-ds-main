## Find the best hospital in the state
##
## Input: two-character state name, outcome name
## Output: Name of the hospital with lowest 30-day mortality rate
best <- function(state,outcome){
  
  # Allowed outcomes (pseudo-dictionary)
  allowed_outcomes <- list(11,17,23)
  names(allowed_outcomes) <- c("heart attack","heart failure","pneumonia")
  
  # Allowed states are built into R with state.abb
  
  # Check if provided state and outcome are permitted
  if (!(state %in% state.abb)){
    stop("invalid state")
  }else if (!(outcome %in% names(allowed_outcomes))){
    stop("invalid outcome")
  }
  
  # Read in the outcome data
  outcome_data <- read.csv("data/outcome-of-care-measures.csv", colClasses='character')
  
  # Select out hospitals from the correct state and with correct outcome
  matched_hospitals <- subset(outcome_data,State==state,select=c(2,allowed_outcomes[[outcome]]))
  matched_hospitals[,2] <- as.numeric(matched_hospitals[,2])
  matched_hospitals <- na.omit(matched_hospitals)
  
  # Find the indices of the best hospitals
  best_hosp_ind <- which(matched_hospitals[,2] == min(matched_hospitals[,2]))
  
  # If there is only one matching hospital, return that
  if(length(best_hosp_ind)==1){
    return(matched_hospitals[best_hosp_ind,1])
  }
  
  # Otherwise sort alphabetically
  else{
    sorted_matched_hospitals <- sort(matched_hospitals[best_hosp_ind,1])
    return(sorted_matched_hospitals[1])
  }
}