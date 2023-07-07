library(energy)

summaryStat <- function(df){
  ds <- df %>%
    group_by(id) %>%
    summarise(
      
      median_y = median(y),
      iqr_y = IQR(y),
      mad_y = mad(y),
      mean_y = mean(y),
      sd_y = sd(y),
      
      
      
      dcor = energy::dcor(dist(x),dist(y)),   #distance correlation
      dcor_p = energy::dcor.test(dist(x),dist(y),R=999)$p.value, # p value of significance test
      cor = ifelse(sd(y) < 1,NA,cor(x,y,method="spearman")),        # linear correlation
      cor_p = ifelse(sd(y)<1,NA,cor.test(x,y)$p.value),
      
      three_median_y = median(y[x<4]),
      three_iqr_y  = IQR(y[x<4]),
      
      low_median_y = median(y[x < 0.5 * max(x)]),
      low_iqr_y = IQR(y[x < 0.5 * max(x)]),
      low_mean_y = mean(y[x < 0.5 * max(x)]),
      low_sd_y = sd(y[x < 0.5 * max(x)]),
      
      hi_median_y = median(y[x > 0.5 * max(x)]),
      hi_iqr_y = IQR(y[x > 0.5 * max(x)]),
      hi_mean_y = mean(y[x > 0.5 * max(x)]),
      hi_sd_y = sd(y[x > 0.5 * max(x)]), 
      
      bias = median(y-x),
      bias_lo = median(y[x>10 & x <21]-x[x>10 & x <21]),
      bias_hi = median(y[x>30]-x[x>30]),
      bias_mean = mean(y-x),
      bias_prop = mean((y-x)>0)-mean((y-x)<0) #proportion of overestimates minus proportion of underestimates
      
      
    ) %>%
    ungroup()
  
  
  return(ds)
}
