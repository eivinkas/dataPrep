
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
      cor = ifelse(sd(y) < 1,NA,cor(x,y,method="pearson")),        # linear correlation
      cor_p = ifelse(sd(y)<1,NA,cor.test(x,y)$p.value),
      
      five_median_y = median(y[x<6]),
      five_iqr_y  = IQR(y[x<6]),
      
      low_median_y = median(y[x < 0.5 * max(x)]),
      low_iqr_y = IQR(y[x < 0.5 * max(x)]),
      low_mean_y = mean(y[x < 0.5 * max(x)]),
      low_sd_y = sd(y[x < 0.5 * max(x)]),
      
      hi_median_y = median(y[x > 0.5 * max(x)]),
      hi_iqr_y = IQR(y[x > 0.5 * max(x)]),
      hi_mean_y = mean(y[x > 0.5 * max(x)]),
      hi_sd_y = sd(y[x > 0.5 * max(x)])
    ) %>%
    ungroup()
  
  
  return(ds)
}

