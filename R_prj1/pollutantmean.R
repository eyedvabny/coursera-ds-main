pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  # Load up the CSV datasets
  mons <- dir(directory,pattern="*.csv",full.name=TRUE)
  mons_data <- lapply(mons, read.csv)
  mons_data_comb <- rbindlist(mons_data)
  
  # Pick the IDs we want and the right pollutant column
  mons_data_ref <- subset(mons_data_comb,ID%in%id,select=pollutant)
  
  # Return the mean, exclusing the NA
  mean(mons_data_ref[[1]],na.rm=TRUE)
}