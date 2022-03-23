#Data Prep
income_data_casual<-main_data %>% 
  #cleaning the state column
  separate(col=STATEID,into=c("No","State","No."),sep=c(5,-3)) %>%
  
  #selecting the columns we need
  select(state=State,duration=WS9,salary=WS10,nature=WS13,b3=WS12,
         weight=FWT,total=WSEARN,w_days=WS7,employer=WS14,edu_level=ED6,lit=ED2,
         area=URBAN2011,job=WS4,industry=WS5) %>% 
  drop_na() %>%
  
  #taking out fixed payment jobs
  filter(duration %in% c("(2) Per Month 2","(1) Per Day 1")) %>% 
  
  #converting all salary in daily basis and adding bonus
  mutate(sal_per_day=round(ifelse(duration=="(2) Per Month 2",
                                  salary/30+b3/365,salary+b3/365)))
#Average Salary in each nature of job 
income_data_casual %>% 
  as_survey(weights = c(weight)) %>% 
  group_by(nature) %>% 
  summarise(wage=round(survey_mean(sal_per_day),2)) %>% 
  separate(col=nature,into=c("n","nature","n."),sep=c(4,-1)) %>% 
  ggplot(aes(x=fct_reorder(nature,wage),y=wage,fill=nature)) +
  geom_bar(stat="identity") + 
  theme_bw() +
  theme(legend.position="none") +
  xlab("Nature of Job") +
  ylab("Wage") + 
  geom_text(aes(label=wage),vjust=-0.1)

#People in Casual Labour
prop_casual_labour=income_data_casual %>% as_survey(weights = c(weight)) %>% group_by(state,nature) %>% 
  summarize(n = survey_total()) %>% mutate(casual_labour=round(n*100/sum(n),2)) %>% 
  filter(nature=="(1) Casual daily 1") %>% select(state,casual_labour)

pd_dat<-merge(pd_dat,prop_casual_labour)

pd_dat %>% ggplot(aes(x=casual_labour,y=wage,label=state)) + 
  geom_point() +
  geom_smooth(method = "lm", se=FALSE, color="blue", formula = y~x) +
  stat_cor(method = "pearson", aes(label = ..r.label..)) + xlab("Percentage of People Engaged in Casual Labour") + 
  geom_label_repel(aes(fontface = 'bold'),box.padding = unit(0.25, 'lines'),point.padding = unit(0.5, 'lines'),max.overlaps = 30) +
  ylab("Wages") + theme_bw()

#Proposed Model 
library(stargazer)
regression_model<-lm(wage~per_capita+casual_labour,pd_dat)
stargazer(regression_model,type="text")





