library(ggplot2)

x <-
  acc %>% 
  group_by(y) %>% 
  summarise(n_y = n())

class(x)

piepercent<- x %>% 
  mutate(n_perc= round( n_y/nrow(acc)* 100,2))


ggplot(piepercent, aes(x = "", y = n_perc, fill = y)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = n_perc), position = position_stack(vjust = 0.5),color = "white")+
   guides(fill=guide_legend(title="Default vs Fully Paid"))+
  theme_void()