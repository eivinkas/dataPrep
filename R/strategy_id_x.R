library(tidyverse)
strategy_id_x <- function(df,plot=TRUE){
  strategy_matrix <- matrix(NA, nrow = length(U), ncol = max(df$x))
  
  c <- df %>%
    group_by(id,x) %>%
    count(s) 
  
  # Strategy matrix
  new_matrix <- c %>%
    group_by(id, x) %>%
    slice_max(n, with_ties = FALSE) %>%
    select(id, x, s) %>%
    pivot_wider(names_from = x, values_from = s)
  
  # Convert the resulting matrix to a numeric matrix
  new_matrix <- as.matrix(new_matrix[, -1])
  
  matrix_data<-new_matrix
  
  max_x <- apply(matrix_data, 1, function(row) max(which(row %in% c(1, 2,3))))
  
  # Sort the rows based on the maximum 'x' value in decreasing order
  sorted_matrix <- matrix_data[order(-max_x), ]
  
  da <- as.data.frame(sorted_matrix, stringsAsFactors = FALSE)
  da$id <- as.numeric(rownames(da))
  
  
  # Reshape the dataframe into long format
  strategy_matrix <- tidyr::gather(da, x, value, -id)
  strategy_matrix$x <- as.numeric(strategy_matrix$x)
  
  if (plot==TRUE) {
  # Define a color palette
  
  colors <- brewer.pal(nlevels(factor(strategy_matrix$value)), "Set3")
  
  # Plot the matrix
  ggplot(strategy_matrix, aes(x = x, y = id, fill = factor(value))) +
    geom_tile() +
    scale_fill_manual(values = colors) +
    labs(x = "X", y = "ID", fill = "Value") +
    theme_bw()
  }
  
  
  return(strategy_matrix)
}
