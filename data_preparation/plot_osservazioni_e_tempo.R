month_summary_with_current <- 
  acc_with_current %>% 
  group_by(issue_d) %>% 
  summarise(n_current= n())

month_summary <- 
  acc %>% 
  group_by(issue_d) %>% 
  summarise(n = n())

month_summary_full <- 
  acc_full %>% 
  mutate(issue_d = lubridate::my(issue_d)) %>%
  group_by(issue_d) %>% 
  summarise(n_full= n()) %>%
  left_join(month_summary_with_current) %>%
  left_join(month_summary) 

plotly::plot_ly(month_summary_full, x = ~issue_d) %>% 
  plotly::add_trace(y = ~n_full, mode = "lines+markers", name = "All", opacity = 0.2) %>%
  plotly::add_trace(y = ~n_current, mode = "lines+markers", name = "No Missing Values", opacity = 0.5) %>%
  plotly::add_trace(y = ~n, mode = "lines+markers", name = "No Missing Values and No Current", opacity = 1) %>%
  plotly::layout(title = "Crescita di LendingClub",  xaxis = list(title = 'Data'),
                 yaxis = list(title = 'Numero prestiti finanziati'),
                 legend = list(x=0.1,y=0.9))

rm(acc_full)
