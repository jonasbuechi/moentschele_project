library(tidyverse)

data <- read_csv("data/labels_old_camera.csv")

# Data wrangling
data_p <- data %>% 
  rename(img = ...1) %>% 
  filter(!is.na(wind_direction)) %>% 
  mutate(wind_direction = factor(wind_direction,
                                 levels = c("r", "l", "d", "u", "n", "0"),
                                 labels = c("right", "left", "down", "up", "no wind", "not clear")),
         wind_force = factor(wind_force,
                             levels = c("n", "w", "m", "s", "0"),
                             labels = c("no wind", "weak", "medium", "strong", "not clear"),
                             ordered = TRUE))


# Barplot of winddirections
ggplot(data = data_p)+
  geom_bar(stat = "count",
           mapping = aes(x = wind_direction,
                         fill = wind_direction))+
  geom_text(stat = "count",
            mapping = aes(x = wind_direction,
                          label = after_stat(count)),
            vjust=-1)+
  #viridis::scale_fill_viridis(discrete = T, option = "C") +
  scale_fill_manual(values=c('#66c2a5','#fc8d62','#8da0cb','#e78ac3','#a6d854','#ffd92f'))+
  
  labs(title = "Frequency of wind direction in labeled images",
       subtitle = "October 1 - October 27, 2022 \nMöntschelealp Bern",
       caption = paste("Total N =", as.character(nrow(data_p))))+
  xlab("Wind direction")+
  ylab("Image count")+
  
  theme_minimal()+
  theme(plot.title = element_text(size=16, face= "bold"), axis.title = element_text(size= 12),
        legend.position = "none",
        panel.grid.major.x = element_blank())
ggsave("wind_direction.png",height = 16.25, width = 24.75, units = "cm", dpi = "retina")


# Barplot of windforce
ggplot(data = data_p)+
  geom_bar(mapping = aes(x = wind_force,
                         fill = wind_force))+
  geom_text(stat = "count",
            mapping = aes(x = wind_force,
                          label = after_stat(count)),
            vjust=-1)+
  
  #viridis::scale_fill_viridis(discrete = T, option = "C") +
  scale_fill_manual(values=c('#f6eff7','#bdc9e1','#67a9cf','#02818a', '#ffd92f'))+
  
  labs(title = "Frequency of wind force in labeled images",
       subtitle = "October 1 - October 27, 2022 \nMöntschelealp Bern",
       caption = paste("Total N =", as.character(nrow(data_p))))+
  xlab("Wind force")+
  ylab("Image count")+
  
  theme_minimal()+
  theme(plot.title = element_text(size=16, face= "bold"), axis.title = element_text(size= 12),
        legend.position = "none",
        panel.grid.major.x = element_blank())
ggsave("wind_force.png",height = 16.25, width = 24.75, units = "cm", dpi = "retina")


ggplot(data = data_p)+
  geom_jitter(aes(wind_direction, wind_force))







data_p_r <- data %>% 
  rename(img = ...1) %>% 
  mutate(wind_direction = na_if(wind_direction, "0"),
         wind_force = na_if(wind_force, "0")) %>% 
  
  mutate(wind_direction = factor(wind_direction,
                                 #levels = c("r", "l", "d", "u"),
                                 #labels = c("right", "left", "down", "up")),
                                 levels = c("l", "u", "r", "d"),
                                 labels = c("left", "up", "right", "down")),
         wind_force = factor(wind_force,
                             #levels = c("w", "m", "s"),
                             levels = c("s", "m", "w"),
                             labels = c("strong", "medium", "weak"))) %>% 
  filter(!is.na(wind_direction), !is.na(wind_direction))







ggplot(data = data_p_r, aes(x = wind_direction, fill = wind_force))+
  geom_bar(stat="count",width=1,colour="black",size=0.1, alpha= 0.5)+
  coord_polar(theta = "x", start = 3.93)+
  theme_minimal()

ggplot(data = data_p_r, aes(x = wind_direction, fill = wind_direction))+
  geom_bar(stat="count",width=1,colour="black",size=0.1, alpha= 0.5)+
  coord_polar(theta = "x", start = 3.93)+
  facet_wrap(~wind_force)+
  theme_minimal()+
  theme(legend.position = "none")

# ggplot(data.frame(Rose), aes(x=Rose, fill=Rose))+
#   geom_bar(stat="count",width=1,colour="black",size=0.1, alpha= 0.5)+
#   coord_polar(theta = "x", start=6.0729, direction = 1)+
#   scale_color_discrete ()+
#   ggtitle("Frequency Wind Rose", subtitle= "March 1 - May 16, 2017 \n Saint Thomas University, Miami, FL") + 
#   xlab("Wind Direction Recorded Every Five Minutes")+
#   ylab("")+ guides(fill=guide_legend(title="Direction")) + 
#   theme(plot.title = element_text(size=18, face= "bold", color = "red"), axis.title.x = element_text(size= 7), axis.text.y=element_blank(), axis.ticks.y = element_blank())


