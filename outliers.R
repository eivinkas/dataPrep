


outliers <- function(df,
                           cor_threshold          = 0.3,   # Consider changing for dcor (nonlinear correlation)
                           cor_p_threshold        = 0.001, # Required significance level
                           three_median_threshold = 6,     # maximum for numerosities 1-3
                           outlier_threshold      = 4,     # factor of mean per numerosity (psychophysical function)
                           screen                 = FALSE) # Plot for screening purposes 
                           { 
  
  # Number of data points (rows) per participant, threshold: 50% must be included
  n_pts <- df %>%
    group_by(id) %>%
    summarise(num_rows = n()) %>%
    summarise(median_rows = median(num_rows))
  
  
  # Summary statistics
  ds <- summaryStat(df)
  ds <- na.omit(ds)
  
  # Identify persons who satisfy inclusion criteria
  in_id <- ds$id[ds$three_median_y < three_median_threshold & ds$cor_p < cor_p_threshold & ds$cor > cor_threshold]
  
  # Remove excluded persons
  df_in  <- df[ df$id %in% in_id, ]   # included participants
  
  df_out <- df[!df$id %in% in_id, ] # excluded participants
  
  # Calculate psychophysical function (response curve) for each participant
  df_in <- df_in %>%
    group_by(id) %>%
    arrange(x) %>%
    mutate(pfy = pf(x,y,newx=x,stat="median",lambda=0,freeZ=0)) %>%   # median for outlier detection  
    mutate(res = y-pfy) # residuals
  
  # Remove outliers
  df_in <- df_in[df_in$y < outlier_threshold * df_in$pfy & df_in$y > df_in$pfy/outlier_threshold,]
  
  # Outliers    
  df_in_outpts <- anti_join(df,df_in, by = c("id","x","y"))  # Extract data that is not included
  df_in_outpts <- df_in_outpts[df_in_outpts$id %in% in_id, ] # Remove the already excluded participants
  
  # Re-calculate the psychophysical function and residuals if used later
  df_in <- df_in %>%
    group_by(id) %>%
    arrange(x) %>%
    mutate(pfy = pf(x,y,newx=x,stat="median",lambda=0,freeZ=0)) %>%   
    mutate(res = y-pfy)
  
  #  Output statistics
  print(paste('N persons: ',length(unique(df$id))))
  print(paste('N excluded persons: ',length(unique(df$id))-length(unique(df_in$id))))
  print(paste('Persons exclusion rate: ', ( length(unique(df$id))-length(unique(df_in$id))) / length(unique(df$id))    ))
  print('------------------')  
  print(paste('N included data points: ',nrow(df_in)))
  print(paste('N excluded data points: ',nrow(df_in_outpts)))
  print(paste('Data point exclusion rate: ',nrow(df_in_outpts) / (nrow(df_in_outpts) +nrow(df_in)  ) ))
  
  
  if(screen == TRUE){ # Should change to plotting one by one when ~100 participants
    
    if(nrow(df_out)>10){          
      plot1 <- ggplot(df_out, aes(x=x,y=y))+geom_abline(intercept=0,slope=1,linetype="dashed",linewidth=1)+geom_point()+facet_wrap(~id,scales="free")  
      print(plot1)
    }
    
    plot2 <- ggplot(df_in, aes(x=x,y=y))+geom_abline(intercept=0,slope=1,linetype="dashed",linewidth=1)+geom_point()+geom_line(aes(x=x,y=pfy),color="red",linewidth=1)+geom_point(data=df_in_outpts,aes(x=x,y=y),color="red",size=3)+facet_wrap(~id,scales="free")  
    
    print(plot2)
    
    print(ds)
  }
  
  return(list(df_in,df_out,df_in_outpts))
}
