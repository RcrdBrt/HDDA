###############################################################################

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

month_summary_current <- 
  acc_with_current %>% 
  group_by(issue_d) %>% 
  summarise(n = n())

plot( month_summary_current,
      main="OSSERVAZIONI NEL TEMPO",
      ylab="osservazioni",
      xlab="tempo",
      type="l",
      col="red")


###############################################################################

acc_full <- fread("./archive/accepted_2007_to_2018Q4.csv", sep = ",", header = T)

month_summary_full <- 
  acc_full %>% 
  mutate(issue_d = lubridate::my(issue_d)) %>%
  group_by(issue_d) %>% 
  summarise(n_full= n())

month_summary_with_current <- 
  acc_with_current %>% 
  group_by(issue_d) %>% 
  summarise(n_current= n())

month_summary <- 
  acc %>% 
  group_by(issue_d) %>% 
  summarise(n = n())

month_summary_full <- 
  month_summary_full %>%
  left_join(month_summary_with_current) %>%
  left_join(month_summary) 

plotly::plot_ly(month_summary_full, x = ~issue_d) %>% 
  plotly::add_trace(y = ~n_full, mode = "lines+markers", name = "All", opacity = 0.2) %>%
  plotly::add_trace(y = ~n_current, mode = "lines+markers", name = "No Missing Values", opacity = 0.5) %>%
  plotly::add_trace(y = ~n, mode = "lines+markers", name = "No Missing Values and No Current", opacity = 1) %>%
  plotly::layout(title = "Crescita di LendingClub",  xaxis = list(title = 'Data'),
                 yaxis = list(title = 'Numero prestiti finanziati'))
