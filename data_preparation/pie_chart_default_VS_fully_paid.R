piepercent <- acc %>% 
  group_by(y) %>% 
  summarise(n_y = n()) %>% 
  mutate(n_perc= round( n_y/nrow(acc)* 100,2))

ggplot(piepercent, aes(x = "", y = n_perc, fill = y)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = n_perc), position = position_stack(vjust = 0.5),color = "white")+
  scale_fill_manual(values = c("darkred", "darkgreen"), labels=c("Default", "Fully Paid")) +
  guides(fill=guide_legend(title="Outcome"))+
  theme_void()
