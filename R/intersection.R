intersection <- function(data,
                         x,
                         group,
                         first_strat,
                         second_strat,
                         min_x = 3,
                         plot = FALSE,
                         title = "",
                         output = "point") {

  library(dplyr)
  library(ggplot2)

  gr <- c(first_strat, second_strat)
  colnames(data)[colnames(data) == x] <- "x"
  colnames(data)[colnames(data) == group] <- "group"
  unique_x <- unique(data$x)
  df1 <- data.frame(x = NA, p = NA, group = NA)

  for (i in seq_along(unique_x)) {
    newdat <- data %>%
      filter(x == unique_x[i])
    len1 <- length(newdat[, 1])
    df1 <- rbind(
      df1,
      c(
        unique_x[i],
        sum(newdat$group == gr[1]) / len1,
        gr[1]
      )
    )

    df1 <- rbind(
      df1,
      c(
        unique_x[i],
        sum(newdat$group == gr[2]) / len1,
        gr[2]
      )
    )
  }

  df1 = df1[-1,]
  df1$x = as.numeric(df1$x)
  df1$p = as.numeric(df1$p)

  # Data for group1 only
  g1 = filter(df1, group == gr[1])
  g1$x = as.numeric(g1$x)
  g1$p = as.numeric(g1$p)
  g1 = g1 %>%
    arrange(x)

  # Find last x over 50%
  lower = g1 %>%
    filter(p > 0.5) %>%
    tail(n = 1)

  # Find first x under 50%
  higher = g1 %>%
    filter(x>lower$x) %>%
    filter(p < 0.5) %>%
    head(n = 1)

  # Find intersection
  slope <- (higher$p - lower$p) / (higher$x - lower$x)
  intercept <- lower$p - slope * lower$x
  interPoint <- (0.5 - intercept) / slope

  # Add NA
  if (length(interPoint) == 0) interPoint = -1
  if (interPoint < min_x) interPoint = NA

  # Plot
  if (plot) {
    p1 = ggplot(data = df1, aes(x = x, y = p, group = group, color = group)) +
      scale_x_continuous() +
      geom_line() +
      ggtitle(title) +
      xlab("") +
      labs(color = "") +
      theme_classic() +
      theme(
        legend.position = "none",
        axis.text = element_blank()
      )

    if (!is.na(interPoint)) {
      p1 = p1 +
        geom_vline(xintercept = interPoint)
    }

    plot(p1)
  }

  if (output == "point") return(interPoint)
  if (output == "plot") return(p1)
}
