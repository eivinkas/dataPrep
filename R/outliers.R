library(cobs)
source("C:/Users/Trygve/OneDrive - NTNU/Documents/2 NUMEROSITY PERCEPTION/R analysis/ManuscriptAnalysis/summaryStat.R")

outliers <- function(df,
                           cor_threshold          = 0.3,   # Consider changing for dcor (nonlinear correlation)
                           cor_p_threshold        = 0.001, # Required significance level
                           three_median_threshold = 6,     # maximum for numerosities 1-3
                           outlier_threshold      = 3,     # factor of mean per numerosity (psychophysical function)
                           screen                 = FALSE) # Plot for screening purposes 
                           { 
  
  # Number of data points (rows) per participant, threshold: 50% must be included
  n_pts <- df %>%
    group_by(id) %>%
    summarise(num_rows = n()) %>%
    summarise(median_rows = median(num_rows))
  
  # Remove data points that are labelled as "error" and responses = 0
  print(sum(df$s==9))
  rdf <- df[df$s != 9 & df$y > 0,]
  
    # Summary statistics
  ds <- summaryStat(rdf)
  ds <- na.omit(ds)
  
  # Identify persons who satisfy inclusion criteria
  in_id <- ds$id[ds$three_median_y < three_median_threshold & ds$cor_p < cor_p_threshold & ds$cor > cor_threshold]
  
  # Remove excluded persons
  df_in  <- df[ df$id %in% in_id, ]   # included participants
  
  df_out <- df[!df$id %in% in_id, ] # excluded participants
  
  # Remove 0 responses
  df_in <- df_in[df_in$y  > 0,]
  df_in <- df_in[df_in$s != 9,]
  
  
  # Calculate psychophysical function (response curve) for each participant
  
  
  df_new <- df_in %>%
    group_by(id) %>%
    arrange(x) %>%
      mutate(pfy = pf(x,y,newx=x,stat="median",lambda=0,freeZ=0)) %>%   # median for outlier detection  
      mutate(pfy10 = pf(x,y,newx=x,stat="median",freeZ=0,tau=0.1)) %>%
      mutate(pfy90 = pf(x,y,newx=x,stat="median",freeZ=0,tau=0.9)) %>%
      mutate(iqr = pfy90-pfy10,mad = mad(y/pfy)) %>%
      mutate(res = y-pfy)  %>% 
    
      mutate(rtf = predict(cobs(x,rt,nknots = 6),x)[,2]) %>%
      mutate(rtf25 = predict(cobs(x,rt,tau=.25,nknots = 6),x)[,2]) %>%
      mutate(rtf75 = predict(cobs(x,rt,tau=.75,nknots = 6),x)[,2]) %>%
    
      mutate(inlier_rel = y < outlier_threshold*pfy & y > pfy/outlier_threshold ) %>% # index for outliers
      mutate(inlier_quart = y < 1.5*pfy90 & y > pfy10/2 ) %>%  # index for outliers
      mutate(rtinlier_quart = rt < 3*rtf75 & rt > rtf25/3 )   # index for outliers
  
  # Remove outliers
    df_in <- df_new[df_new$inlier_quart & df_new$rtinlier_quart,] #, < outlier_threshold * df_in$pfy & df_in$y > df_in$pfy/outlier_threshold,]
  #df_in <- df_new[df_new$inlier_rel & df_new$rtinlier_quart,] # The quartiole-based outliers are tighter but more unstable!
  
  # Outliers    
  df_in_outpts <- anti_join(df,df_in, by = c("id","x","y"))  # Extract data that is not included
  df_in_outpts <- df_in_outpts[df_in_outpts$id %in% in_id, ] # Remove the already excluded participants
  
  # Re-calculate the psychophysical function and residuals if used later
  df_in <- df_in %>%
    group_by(id) %>%
    arrange(x) %>%
    mutate(pfy = pf(x,y,newx=x,stat="median",lambda=0,freeZ=0)) %>%   
    mutate(res = y-pfy) %>%
    mutate(rtf = predict(cobs(x,rt,nknots = 6),x)[,2])
  
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
    
    plotrt <- ggplot(df_in, aes(x=x,y=rt))+geom_point()+
      geom_line(aes(x=x,y=rtf),color="purple",linewidth=2)+
      geom_line(aes(x=x,y=rtf25/3),color="orange",linewidth=1)+
      geom_line(aes(x=x,y=rtf75*3),color="orange",linewidth=1)+
      
      geom_point(data=df_in_outpts,aes(x=x,y=rt),color="red",size=3)+
      
      facet_wrap(~id,scales="free")  
    
    
    plot2 <- ggplot(df_in, aes(x=x,y=y))+geom_abline(intercept=0,slope=1,linetype="dashed",linewidth=1)+geom_point()+
      geom_line(aes(x=x,y=pfy),color="purple",linewidth=1)+
      #geom_line(aes(x=x,y=pfy/outlier_threshold),color="orange",linewidth=1)+
      #geom_line(aes(x=x,y=pfy*outlier_threshold),color="orange",linewidth=1)+
      
      geom_line(aes(x=x,y=0.5*pfy10),color="orange",linewidth=1,linetype="dashed")+
      geom_line(aes(x=x,y=1.5*pfy90),color="orange",linewidth=1,linetype="dashed")+
      
      
      geom_point(data=df_in_outpts,aes(x=x,y=y),color="red",size=3)+
      #    geom_point(data=df_new[df_new$inlier_rel==FALSE,],aes(x=x,y=y),color="cyan",size=1)+
      facet_wrap(~id,scales="free")  
    
    print(plotrt)
    print(plot2)
    
    print(ds)
  }
  
  return(list(df_in,df_out,df_in_outpts))
}
