corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  # Load up the CSV datasets
  mons <- dir(directory,pattern="*.csv",full.name=TRUE)
  mons_data <- lapply(mons, read.csv)
  mons_data_comb <- rbindlist(mons_data)
  
  # Filter out all the incompletes
  mons_complete <- mons_data_comb[complete.cases(mons_data_comb)]
  
  # Find out the unique IDs
  id<-unique(mons_complete$ID)
  
  # Count the number of unique elements
  # (Use a custom function for lapply)
  count_ent <- function(id_val,lst){
    sum(lst$ID==id_val)    
  }
  nobs <- lapply(id,count_ent,mons_complete)
  
  # Filter monitors with nobs below the threshold
  id_ref = id[nobs>=threshold]
  
  # Only do the correlation if there's data to correlate
  if (length(id_ref) == 0){
    return(id_ref) 
  }else{
    # Run correlation for each remaining monitor
    corr_pollutants <- function(id_val,mon_data){
      dat <- subset(mon_data,ID==id_val,select=c(sulfate,nitrate))
      cor(dat[[1]],dat[[2]])
    }
    unlist(lapply(id_ref,corr_pollutants,mons_complete))
  }
}