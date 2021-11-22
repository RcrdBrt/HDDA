month_summary <- 
  acc %>% 
  group_by(issue_d) %>% 
  summarise(n_issue_d = n())
month_summary


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
month_summary

plot( month_summary_current,
      main="OSSERVAZIONI NEL TEMPO",
      ylab="osservazioni",
      xlab="tempo",
      type="l",
      col="red")