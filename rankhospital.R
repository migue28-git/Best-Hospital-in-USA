rankhospital <- function(state,outcome,num = "best" )
{
  ##Read Data
  directory <- list.files(path = "Data",full.names = TRUE,pattern = ".csv")
  outcome_data <- read.csv(directory[2],stringsAsFactors = FALSE,na.strings = "Not Available")
  outcome_v <- c("heart attack","heart failure", "pneumonia")
  
  ##Making a compact and useful data frame
  useful_data <- outcome_data[outcome_data$State == state,c(7,2,11,17,23)]
  
  ##Check that state and coutcome are valid
  if(!(state %in% outcome_data$State))
  {
    stop("invalid state") 
  }
  else if(!(outcome %in% outcome_v))
  {
    stop("invalid outcome") 
  }
  
  ##Which hospital is the best
  if(outcome == outcome_v[1])
  {
    ordered_data <- useful_data[order(as.numeric(useful_data[,3]),useful_data[,2]),]
    final_data <- ordered_data[!is.na(ordered_data[,3]),]
     if(num == "best")
     {
       return(final_data[1,2])
     }
     else if(num == "worst")
     {
       return(final_data[length(final_data[,3]),2])
     }
     else
     {
       return(final_data[num,2])
     }
  }
  else if(outcome == outcome_v[2])
  {
    ordered_data <- useful_data[order(as.numeric(useful_data[,4]),useful_data[,2]),]
    final_data <- ordered_data[!is.na(ordered_data[,4]),]
    if(num == "best")
    {
      return(final_data[1,2])
    }
    else if(num == "worst")
    {
      return(final_data[length(final_data[,4]),2])
    }
    else
    {
      return(final_data[num,2])
    }
  }
  else if(outcome == outcome_v[3])
  {
    ordered_data <- useful_data[order(as.numeric(useful_data[,5]),useful_data[,2]),]
    final_data <- ordered_data[!is.na(ordered_data[,5]),]
    if(num == "best")
    {
      return(final_data[1,2])
    }
    else if(num == "worst")
    {
      return(final_data[length(final_data[,5]),2])
    }
    else
    {
      return(final_data[num,2])
    }
  }
}