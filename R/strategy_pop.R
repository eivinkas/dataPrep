strategy_pop <- function(df,plot=TRUE) {

  con<-rbind(c(1,min(df$x),0), #f(min(x))>=0 
             c(-1,max(df$x),1)) #f(max(x))<=1 
             
  
proportion_df <- df %>% ungroup() %>% 
  group_by(x) %>%
  mutate(total_rows = n()) %>%
  group_by(x, s) %>%
  summarize(proportion = n() / first(total_rows)) %>% 
  ungroup()%>%
  complete(x = unique(df$x), s = 1:8, fill = list(count = 0, proportion = 0))

proportion_df <- proportion_df %>% 
  group_by(s) %>%
  mutate(curve = predict(cobs(x,proportion,pointwise = con,lambda=1),1:max(x))[,2])
  
  if (plot==TRUE){

  # Plot proportions
  colors <- brewer.pal(8, "Set3")  # nlevels(factor(proportion_df$s))
  g<-ggplot(proportion_df, aes(x=x,y=proportion,group=as.factor(s),color=as.factor(s)))+geom_point(lwd=2)+
    geom_line(aes(x=x,y=curve))+
    scale_fill_manual(values=colors)
  print(g)
  }

  return(proportion_df)
}