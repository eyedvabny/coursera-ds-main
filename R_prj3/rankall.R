rankall <- function(outcome, num = "best") {

  # Allowed outcomes (pseudo-dictionary)
  allowed_outcomes <- list(11,17,23)
  names(allowed_outcomes) <- c("heart attack","heart failure","pneumonia")
  
  # Check if provided state and outcome are permitted
  if (!(outcome %in% names(allowed_outcomes))){
    stop("invalid outcome")
  }
  
  # Read in the outcome data
  outcome_data <- read.csv("data/outcome-of-care-measures.csv",
                           colClasses='character')
  
  # Clean the columns and remove NA
  hospitals <- outcome_data[,c(2,7,allowed_outcomes[[outcome]])]
  hospitals[,3] <- as.numeric(hospitals[,3])
  hospitals <- na.omit(hospitals)
  
  # Order by rank, then name
  hospitals <- hospitals[order(hospitals[,3],hospitals[,1]),]
  
  # Re-order the state abbr to be alphabetical
  states <- c(state.abb,"DC","GU","MP","PR","VI")
  states <- states[order(states)]
  
  # Function to generate a list of ranked hospitals in each state
  get_elem<-function(state,num,hosp){
    
    # Extract the element for each state
    filt_hosp <- hosp[hosp[,2]==state,]
    
    # Pick the right index
    if(num=='best'){
      ind <- 1
    }else if(num == 'worst'){
      ind <- nrow(filt_hosp)
    }else{
      ind <- num
    }
    
    filt_hosp[ind,1]
  }
  
  # Get the list of hospital names for each state that match out criteria
  hospital_names<- lapply(states,get_elem,num,hospitals)
  
  hospitals <- data.frame(cbind("hospital"=hospital_names,"state"=states))
  
  # Return a data fram matching
  return(hospitals)
}