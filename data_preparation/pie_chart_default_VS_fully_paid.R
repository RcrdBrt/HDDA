piechart <- function(d, group, legend_title = "Outcome"){
  piepercent <- d %>% 
    group_by((!!sym(group))) %>% 
    summarise(n_group = n()) %>% 
    mutate(n_perc= round( n_group/nrow(d)* 100,2))
  
  ggplot(piepercent, aes(x = "", y = n_perc, fill = !!sym(group))) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0) +
    geom_text(aes(label = n_perc), position = position_stack(vjust = 0.5),color = "white")+
    scale_fill_brewer("Blues") +
    guides(fill=guide_legend(title=legend_title))+
    theme_void()
}
