#Topic over time Time plot 



plot_topic_time <- function(estimate.effect.tt, topic_number){
  
  p_jahr <- estimate.topic.time %>% filter(topic == topic_number) %>%
    ggplot(aes(x = value, y = proportion, color = label,
               group = label, fill = label)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2)  +
    theme_light() + labs(x = 'day', y = 'Topic Proportion')  +
    theme(legend.position = "bottom") +
    ggtitle(label = paste0("Diffusion of Topics")) +
    theme(plot.title = element_text(hjust = 0.5))
  
  p_jahr
  
  return(p_jahr)
  
}