#Code for visualisations
library(dplyr)
library(ggplot2)
library(tidyr)
data <- read.csv('3_clusters_M1_v2.csv')

data_1 <- data %>% 
  group_by(cluster_final) %>% 
  summarise(R = mean(Recency), F = mean(Frequency), M = mean(Monetary_value))


data_2 <-  data_1 %>% pivot_longer(
  cols = R:M,
  names_to = 'Value',
  values_to = "mean"
)

data_2$cluster_final <- as.character(data_2$cluster_final)
ggplot(data_2,aes(x=cluster_final,y=mean, color = Value, fill = Value)) +
  geom_col(alpha=0.85, width = 0.5,show.legend = FALSE) +
  coord_flip()+
  facet_wrap(~Value)+
  scale_y_continuous(breaks=seq(0, 600, 100))+
  theme_classic()+
  theme(
    axis.title = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    plot.title = element_text(size = 11),
  #  plot.margin = margin(3, 4, 2, 2),
 #   axis.text.x = element_text(angle = 45),
 panel.grid.major.x = element_blank(),
 axis.text.y = element_text(size = 15),
 strip.text.x = element_text(size = 15)
  #  panel.grid.major.x = element_line(size = 0.2, linetype = 'solid',colour = "lightgrey")
 )


M1 = c(23.9,19.2,23.9,23.10,33)
M_baseline = c(16,9,9.4,10.4,22.4)
l3 = c('I1','I2','I3','I4','I5')

df <- data.frame(l3,M1,M_baseline)


df_2 <-  df %>% pivot_longer(
  cols = M1:M_baseline,
  names_to = 'Model',
  values_to = "Accuracy"
)



ggplot(df_2, (aes(x= l3, y=Accuracy, fill=Model))) + 
  geom_col(position="dodge", alpha=0.85, width = 0.7) +
scale_y_continuous(
  breaks = c(0, 5, 10, 15, 20, 25, 30, 35),
  expand = c(0, 0),
   name = "Accuracy") +
  xlab(label = "Iteration Number") +
  scale_fill_manual(values = c("#fc8d62","#66c2a5" ), name = NULL) +
  
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(size = 0.4, linetype = 'solid', colour = "white"),
    panel.ontop = TRUE,
    legend.position= c(0.75, 0.9), legend.direction="horizontal") 

  