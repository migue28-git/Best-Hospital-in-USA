rankall <- function(outcome,num = "best")
{
  ##Read Data
  directory <- list.files(path = "Data",full.names = TRUE, pattern = ".csv")
  outcome_data <- read.csv(directory[2],stringsAsFactors = FALSE,
                           na.strings = "Not Available")
  outcome_v <- c("heart attack","heart failure", "pneumonia")
  
  ##Making a compact and useful data frame
  useful_data <- outcome_data[,c(7,2,11,17,23)]
  
  ##Check that state and coutcome are valid
  if(!(outcome %in% outcome_v))
  {
    stop("invalid outcome") 
  }
  
  ##Which hospital is the best
  if(outcome == outcome_v[1])
  {
    filtered_data <- useful_data[!is.na(useful_data[,3]), c(1,2,3)]
    ordered_data <-  filtered_data[order(filtered_data[,1],filtered_data[,3],filtered_data[,2]),] 
    if(num == "best" || num == 1) 
    { 
      state_data <- sapply(split(ordered_data,ordered_data[,1]), function(data) data[1,2])         
    }
    else if(num == "worst")
    {
      state_data <- sapply(split(ordered_data,ordered_data[,1]), function(data) data[which.max(data),2])         
    }
    else
    {
      state_data <- sapply(split(ordered_data,ordered_data[,1]), function(data) data[num,2])         
    }
    return(data.frame(hospital = state_data,state = names(state_data)))
  }
  
  else if(outcome == outcome_v[2])
  {
    filtered_data <- useful_data[!is.na(useful_data[,4]), c(1,2,4)]
    ordered_data <-  filtered_data[order(filtered_data[,1],filtered_data[,3],filtered_data[,2]),] 
    if(num == "best" || num == 1) 
    { 
      state_data <- sapply(split(ordered_data,ordered_data[,1]), function(data) data[1,2])         
    }
    else if(num == "worst")
    {
      state_data <- sapply(split(ordered_data,ordered_data[,1]), function(data) data[which.max(data),2])         
    }
    else
    {
      state_data <- sapply(split(ordered_data,ordered_data[,1]), function(data) data[num,2])         
    }
    return(data.frame(hospital = state_data,state = names(state_data)))
  }
  else if(outcome == outcome_v[3])
  {
    filtered_data <- useful_data[!is.na(useful_data[,5]), c(1,2,5)]
    ordered_data <-  filtered_data[order(filtered_data[,1],filtered_data[,3],filtered_data[,2]),] 
    if(num == "best" || num == 1) 
    { 
      state_data <- sapply(split(ordered_data,ordered_data[,1]), function(data) data[1,2])         
    }
    else if(num == "worst")
    {
      state_data <- sapply(split(ordered_data,ordered_data[,1]), function(data) data[which.max(data[,3]),2]) 
    }
    else
    {
      state_data <- sapply(split(ordered_data,ordered_data[,1]), function(data) data[num,2])         
    }
    return(data.frame(hospital = state_data,state = names(state_data)))
  }
  
}