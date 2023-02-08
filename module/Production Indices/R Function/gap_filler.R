#' The purpose of this function is to fill the gap in the production data.
#' 
#' The function require the following parameters :
#' 
#' @param data_with_gaps : a dataframe or datatable containing the
#'                         time series of the production on each rows.
#'        
#' @param column_to_skip : number of column to skip starting from the left of the 
#'                         dataframe, contains the keys of the time series. Default to 0.
#'        
#' 
#' The logics to fill the gaps are the following : 
#' 
#' - if the production data for the three year before the gap are available, perform
#'   the three year mean and fill it into the gap.
#'   
#' - if the data of the three year before isn't available, it will take the most
#'   recent available production data and fill the gap with it until some value
#'   is found.
#'   
#' - if no data is available from the begginnig of the time series it will return NA.
#'



gap_filler = function(data_with_gaps, column_to_skip = 0) {
  
  
  data = data_with_gaps[  ,-c(1:column_to_skip)] #remove first two column (and consider only first row for now)
  data_skipped_column = data_with_gaps[ , c(1:column_to_skip)]
  
  #remove gaps for each row
  
  patcher <- function(data) {
    
    data <- as.vector(data)   
    na.pos = which(is.na(data))  
    na.pos = na.pos[! na.pos %in% 1:3]
    
    for (i in na.pos) { 
      
      #check if the past three year data is available
      
      na.pos.media = c((i-3),(i-2),(i-1))
      na.value.media = data[na.pos.media]
      check_na <-  any(is.na(na.value.media))
      check_estimated <- any(is.character(na.value.media ))
      
      if (check_na == F & check_estimated == F) {
        
        three_year_mean = round( mean(na.value.media) ,digits = 0) #calculate the mean of last three year
        three_year_mean = as.character(three_year_mean)
        data[i] = three_year_mean
        
      } else {
        
        if (check_na == F & check_estimated == T ) { 
          
          
          character_data <- which( sapply(na.value.media , class) == "character" )
          if (3 %in% character_data) { data[i] = na.value.media[3] }
          else { data[i] = na.value.media[max( character_data) ] } 
        }
        
        else { 
          
          count.na = sum(is.na( na.value.media)) # check if all the value are absent
          na.value.pos = which(  !is.na( na.value.media))
          
          if (check_na == T & count.na == 3) { data[i] = NA }
          else{  data[i] =  na.value.media[max(na.value.pos)]  } # if a value is present in the three obervation before the NA the function will replicate this value to fill the gap
          
        }
      }
      
    } #end_for
    
    return(data)
    #rm(list = c(  ) )
    
  } #end patcher
  
  data_patched = apply(data, 1, patcher)
  data_patched =  t(data_patched) 
  
  colnames(data_patched) = colnames(data)
  
  
  data_patched = cbind( data_skipped_column, data_patched  )
  
  return(data_patched)
  
}
