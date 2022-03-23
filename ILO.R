#importing per_capita GDP file 
Per_Capita_SDP_State <- read_excel("Per_Capita_SDP_State.xlsx") %>% rename(state=State)
Per_Capita_SDP_State$state[Per_Capita_SDP_State$state=="Odisha"]<-"Orissa"
Per_Capita_SDP_State$state[Per_Capita_SDP_State$state=="Jammu and Kashmir"]<-"Jammu & Kashmir"

#Filtering Rural Wages
rural_wage<-income_data %>% filter(area=="(0) rural 0") %>%
  as_survey(weights = c(weight)) %>% 
  group_by(state) %>% 
  summarise(rural_wd=survey_mean(w_days),
            rural_wage=round(survey_mean(sal_per_day),2)) %>% 
  mutate(state=fct_reorder(state,rural_wage))

#Filtering Urban Wages
urban_wage<-income_data %>% filter(area=="(1) urban 1") %>%
  as_survey(weights = c(weight)) %>% 
  group_by(state) %>% 
  summarise(urban_wd=survey_mean(w_days),
            urban_wage=round(survey_mean(sal_per_day),2)) %>% 
  mutate(state=fct_reorder(state,urban_wage))

#Combining all 3 files with the existing file
pd_dat<-merge(pd_dat,rural_wage)
pd_dat<-merge(pd_dat,urban_wage)
pd_dat<-merge(pd_dat,Per_Capita_SDP_State)

#Plotting Urban and Rural Wages
ruralwage_urbanwage<-pd_dat %>% ggplot(aes(x=urban_wage,y=rural_wage,label=state)) + 
  geom_point() +
  geom_smooth(method = "lm", se=FALSE, color="blue", formula = y~x) +
  stat_cor(method = "pearson", aes(label = ..r.label..)) + xlab("Urban Wages") + 
  geom_label_repel(aes(fontface = 'bold'),box.padding = unit(0.25, 'lines'),point.padding = unit(0.5, 'lines'),max.overlaps = 30) +
  ylab("Rural Wages") + theme_light()

ruralwage_urbanwage

wage_percapitaSDP<-pd_dat %>% ggplot(aes(x=per_capita,y=wage,label=state)) + 
  geom_point() +
  geom_smooth(method = "lm", se=FALSE, color="blue", formula = y~x) +
  stat_cor(method = "pearson", aes(label = ..r.label..)) + xlab("Per Capita SDP") + 
  geom_label_repel(aes(fontface = 'bold'),box.padding = unit(0.25, 'lines'),point.padding = unit(0.5, 'lines'),max.overlaps = 30) +
  ylab("Wages") + theme_light()
wage_percapitaSDP










