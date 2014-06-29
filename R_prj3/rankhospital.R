## Return the hospital name with desired ranking by 30-day mortality
##
## Input: two-letter state abbreviation, outcome code, ranking number (or 'best'/'worst')
## Output: character vector with name of the hospital or NA

rankhospital <- function(state, outcome, num = "best") {

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
  
  # Sort the dataframe by death rate and alphabetically
  matched_hospitals <- matched_hospitals[order(matched_hospitals[,2],matched_hospitals[,1]),]
  
  if(num=='best'){
    return(matched_hospitals[1,1])
  }else if(num=='worst'){
    return(matched_hospitals[nrow(matched_hospitals),1])
  }else{
    return(matched_hospitals[num,1])
  }
}