month_summary <- 
  acc %>% 
  group_by(issue_d) %>% 
  summarise(n_issue_d = n())

plot( month_summary,
     main="OSSERVAZIONI NEL TEMPO",
     ylab="osservazioni",
     xlab="tempo",
     type="l",
     col="blue")
###############################################################################

month_summary_current <- 
  acc_with_current %>% 
  group_by(issue_d) %>% 
  summarise(n_issue_d = n())

plot( month_summary_current,
      main="OSSERVAZIONI NEL TEMPO",
      ylab="osservazioni",
      xlab="tempo",
      type="l",
      col="red")


##################

month_summary <- acc_with_current %>% group_by(issue_d) %>% summarise(n=n())
plotly::plot_ly(month_summary, x = ~issue_d) %>% 
  plotly::add_trace(y = ~n, mode = "lines+markers", name = "Prestiti", opacity = 1) %>%
  plotly::layout(title = "Crescita di LendingClub",  xaxis = list(title = 'Data'),
                 yaxis = list(title = 'Numero prestiti finanziati'))
