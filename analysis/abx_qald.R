#### QALD calculations

library(pracma)

## Edited from Anton: https://github.com/ntncmch/influenzanet/blob/f7c4329206eed09a12a7b6883444cb905262da44/sandbox/flusurvey.R#L2266

compute_QALD_loss<-function(df_data,baseline,
                            symptom_start, symptom_end, 
                            symptom_end_at_baseline=FALSE, 
                            timeline_only = FALSE){

  # Stop if person_id not unique in df_data? why? each person only one bout? 
  stopifnot(length(unique(df_data$person_id))==1,
            length(unique(df_data$n_bout))==1,
            !is.na(baseline) & baseline>=0,
            !is.na(symptom_start),
            !is.na(symptom_end), symptom_start<=symptom_end, symptom_start<=df_data$report_date[1])
  
  
  #use report dates with available health score		
  y <- !is.na(df_data$health_score)
  x <- df_data$report_date[y]
  
  #add symptom start date with baseline health_score
  if(timeline_only){
    #add baseline just the day before as some people report on symptom onset
    x <- c(symptom_start - 1, x)
  }else{
    x <- c(symptom_start, x)
  }
  
  y <- c(baseline, y)
  
  last_report_date <- max(df_data$report_date)
  if(symptom_end > last_report_date){
    #case where n_bout finish with still_ill==TRUE
    #add symptom end date with baseline health_score
    x <- c(x,symptom_end)
    y <- c(y,baseline)
  }else{
    #replace last report date by symptom end date 
    x[length(x)] <- symptom_end
    #with baseline health_score at recovery date? (only if symptom_end<last_report)
    if(symptom_end_at_baseline && (symptom_end < last_report_date)){
      y[length(y)] <- baseline		
    }
    
  }
  
  
  if(timeline_only){
    x <- as.numeric(x-symptom_start)
    y[y>baseline] <- baseline
    return(data.frame(time_SSS=x,health_score=y))
  }
  
  x<-as.numeric(x)		
  y <- baseline - y
  y[y < 0] <- 0
  y<-y/100
  ans <- trapz(x, y)
  
  return(ans)	
}