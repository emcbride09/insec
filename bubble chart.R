require(tidyverse)
library(extrafont)
require(devtools)
devtools::install_github("wesanderson","karthik")
require(wesanderson)


loadfonts(device = "win")

health_ds <- read.csv('C:/Users/EMCB-PC/Desktop/rdir/Projects/HDX/Insecurity Insight/shcc-healthcare-dataset-2017-v-may-2018.csv')

h_assault_df <- health_ds %>% select(X, Total.health.worker.killed:Total.health.worker.arrested)

h_assault_df <- h_assault_df %>% rename(killed = Total.health.worker.killed, 
                                        injured = Total.health.worker.injured,
                                        assaulted = Total.health.worker.assaulted, 
                                        sv = Sexual.violence.against.health.worker,
                                        kidnapped = Total.health.worker.kidnapped, 
                                        arrested = Total.health.worker.arrested,
                                        country = X)

h_assault_df <- h_assault_df %>% filter(country != '#country+name')

h_assault_df_long <- h_assault_df %>% gather(killed:arrested, key = 'incident', value = 'freq')

write.csv(h_assault_df_long, "health_personnel_attack.csv")

h_assault_df_long$freq <- as.numeric(h_assault_df_long$freq)

h_assault_df_long <- h_assault_df_long %>% filter(country != '#country +name',
                                                  country != 'Nigerÿ')
       
#--------------------
#----Black plot
#--------------------
black_point <- h_assault_df_long %>% ggplot(aes(country, freq, colour = incident)) +
  labs(title = "Attacks Affecting Health Workers", subtitle = "2017", x = "Country", y = "Frequency") +
  geom_point(aes(size = freq), alpha = 0.8) +
  scale_color_brewer(palette = "Spectral", name = "Incident",
                     labels = c("Arrested", "Assaulted", "Injured", "Kidnapped", "Killed",
                                "Sexual Violence")) +
  scale_size_continuous(range = c(1,50)) +
  theme(plot.title = element_text(family = 'Klinic Slab', 
                                  face = 'bold', 
                                  size = 30, 
                                  vjust = 0,
                                  hjust = 0,
                                  colour = 'white'),
        plot.subtitle = element_text(family = 'Klinic Slab',
                                     colour = 'white',
                                     size = 20,
                                     vjust = 0),
        axis.text.x = element_text(angle = 45, hjust = 1, family = 'Klinic Slab', size = 12, color = 'white'),
        axis.text.y = element_text(family = 'Klinic Slab', size = 15, color = 'white'),
        axis.title.x = element_text(vjust = 10, size = 14, family = 'Arial', face = 'bold', colour = 'white'),
        axis.title.y = element_text(size = 14, family = 'Arial', face = 'bold', color = 'white'),
        panel.grid = element_blank(),
        panel.background = element_rect(fill= "black"),
        plot.background = element_rect(fill = "black"),
        legend.text = element_text(colour = 'white'),
        legend.title = element_text(colour = 'white'),
        legend.background = element_rect(fill = NA, color = 'white'),
        legend.position = c(.1,.68),
        legend.key = element_rect(fill = NA, size = 7, color = NA)) + 
  guides(size = FALSE, color = guide_legend(override.aes = list(size = 5))) +
  scale_y_continuous(limits = c(0,70))
  
black_point
        
ggplotly(last_plot())
      
ggsave("attacks_HW.png", plot = last_plot(), width = 500, height = 220, units = "mm", dpi = 400)

        
#      ggsave(filename, plot = last_plot(), device = NULL, path = NULL,
#             scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"),
#             dpi = 300, limitsize = TRUE, ...)


#----------------
#------White plot
#----------------

white_point <- h_assault_df_long %>% ggplot(aes(country, freq, colour = incident)) +
  labs(title = "Attacks Affecting Health Workers", subtitle = "2017", x = "Country", y = "Frequency") +
  geom_point(aes(size = freq), alpha = 0.8) +
  scale_color_brewer(palette = "Dark2", name = "Incident",
                     labels = c("Arrested", "Assaulted", "Injured", "Kidnapped", "Killed",
                                "Sexual Violence")) +
  scale_size_continuous(range = c(1,50)) +
  theme(plot.title = element_text(family = 'Klinic Slab', 
                                  face = 'bold', 
                                  size = 30, 
                                  hjust = 0,
                                  colour = 'black'),
        plot.subtitle = element_text(family = 'Klinic Slab',
                                     colour = 'black',
                                     size = 20,
                                     vjust = -50,
                                     hjust = 0),
        axis.text.x = element_text(angle = 45, hjust = 1, family = 'Klinic Slab', size = 12, color = 'black'),
        axis.text.y = element_text(family = 'Klinic Slab', size = 15, color = 'black'),
        axis.title.x = element_text(vjust = 10, size = 14, family = 'Arial', face = 'bold', colour = 'black'),
        axis.title.y = element_text(size = 14, family = 'Arial', face = 'bold', color = 'black'),
        panel.grid = element_blank(),
        panel.background = element_rect(fill= "white"),
        plot.background = element_rect(fill = "white"),
        legend.text = element_text(colour = 'black'),
        legend.title = element_text(colour = 'black'),
        legend.background = element_rect(fill = NA, color = 'black'),
        legend.position = c(.1,.68),
        legend.key = element_rect(fill = NA, size = 7, color = NA)) + 
  guides(size = FALSE, color = guide_legend(override.aes = list(size = 5))) +
  scale_y_continuous(limits = c(0,70))



white_point

ggsave("attacks_HW_white.png", plot = last_plot(), width = 500, height = 220, units = "mm", dpi = 400)
