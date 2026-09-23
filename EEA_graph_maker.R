library(tidyverse)
library(readxl)
EEA_EBD_result <- read_excel("data/ETCHE/EEA_EBD_result.xlsx")


EEA_EBD_result %>% 
  ggplot(aes(y=value,x=0,
             fill=outcome))+
  geom_col()+
  facet_grid(cols=vars(source),
             rows=vars(metric))+
  labs(y="attributable burden [DALY/ year]",
       fill="Outcome",
       title = "Estimated number of DALYs due to road, rail and aircraft\nin areas covered under the END, EEA-32 (excluding Türkiye)")+
  scale_y_continuous(labels = scales::label_comma())+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
ggsave("plots/EEA_burden_of_noise.png",
       width = 8.5,
       height = 5)
