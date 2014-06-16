complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases  
  
  # Load up the CSV datasets
  mons <- dir(directory,pattern="*.csv",full.name=TRUE)
  mons_data <- lapply(mons, read.csv)
  mons_data_comb <- rbindlist(mons_data)
  
  # Pick the IDs we want and full column
  mons_data_ref <- subset(mons_data_comb,ID%in%id)
  
  # Filter out all the incompletes
  mons_complete <- mons_data_ref[complete.cases(mons_data_ref)]
  
  # Count the number of unique elements
  # (Use a custom function for lapply)
  count_ent <- function(id_val,lst){
    sum(lst$ID==id_val)    
  }
  nobs <- lapply(id,count_ent,mons_complete)
  
  # Return the bound data as a data frame
  as.data.frame(cbind(id,nobs))
}