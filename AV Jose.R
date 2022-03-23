ruralwage_percapitaSDP<-pd_dat %>% ggplot(aes(x=per_capita,y=rural_wage,label=state)) + 
  geom_point() +
  geom_smooth(method = "lm", se=FALSE, color="blue", formula = y~x) +
  stat_cor(method = "pearson", aes(label = ..r.label..)) + xlab("Per Capita SDP") + 
  geom_label_repel(aes(fontface = 'bold'),box.padding = unit(0.25, 'lines'),point.padding = unit(0.5, 'lines'),max.overlaps = 30) +
  ylab("Wages") + theme_light()
ruralwage_percapitaSDP


agri_state<-read_excel("Agriculture_State.xlsx") %>% rename(state=State)
agri_state$state[agri_state$state=="Odisha"]<-"Orissa"
agri_state$state[agri_state$state=="Jammu and Kashmir"]<-"Jammu & Kashmir"

pd_dat<-merge(pd_dat,agri_state)

people_in_agri<-income_data %>% as_survey(weights = c(weight)) %>% group_by(state,industry) %>% 
  summarize(n = survey_total()) %>% 
  arrange(-n) %>% filter(industry=="(00) Agriculture 0") %>% select(state,n_in_agri=n) 

pd_dat<-merge(pd_dat,people_in_agri)

peopleagri_sdp<-pd_dat %>% ggplot(aes(x=agri/n_in_agri,y=rural_wage,label=state)) + 
  geom_point() +
  geom_smooth(method = "lm", se=FALSE, color="blue", formula = y~x) +
  stat_cor(method = "pearson", label.x=15,aes(label = ..r.label..)) + 
  xlab("Agri Contribution in GDP per person") + 
  geom_label_repel(aes(fontface = 'bold'),box.padding = unit(0.25, 'lines'),point.padding = unit(0.5, 'lines'),max.overlaps = 30) +
  ylab("Rural Wages") + theme_light()

peopleagri_sdp

#Part 2 
#Prepping the Data 
income_data_agri<-main_data %>% 
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
                                  salary/30+b3/365,salary+b3/365))) %>% 
  #Filtering only Casual Daily Workers who are engaged in Agriculture
  filter(nature=="(1) Casual daily 1",industry=="(00) Agriculture 0") 

#Average wage and working days for each state
wdata_agri<-income_data_agri %>% 
  as_survey(weights = c(weight)) %>% 
  group_by(state) %>% 
  summarise(wd=survey_mean(w_days),
            agri_wage=round(survey_mean(sal_per_day),2)) %>% 
  mutate(state=fct_reorder(state,agri_wage))

pd_dat1<-merge(gdp_dat,wdata_agri)


#Filtering Rural Wages
rural_wage<-income_data_agri %>% filter(area=="(0) rural 0") %>%
  as_survey(weights = c(weight)) %>% 
  group_by(state) %>% 
  summarise(rural_wd=survey_mean(w_days),
            rural_wage=round(survey_mean(sal_per_day),2)) %>% 
  mutate(state=fct_reorder(state,rural_wage))

#Combining all files with the existing file
pd_dat1<-merge(pd_dat1,rural_wage)
pd_dat1<-merge(pd_dat1,Per_Capita_SDP_State)
pd_dat1<-merge(pd_dat1,agri_state)
pd_dat1<-merge(pd_dat1,people_in_agri) %>% mutate(ppw=(agri*100000)/n_in_agri)

pd_dat1 %>% ggplot(aes(x=ppw,y=rural_wage,label=state)) + 
  geom_point() +
  geom_smooth(method = "lm", se=FALSE, color="blue", formula = y~x) +
  stat_cor(method = "pearson", aes(label = ..r.label..)) + xlab("Product Per Worker in Agriculture") + 
  geom_label_repel(aes(fontface = 'bold'),box.padding = unit(0.25, 'lines'),point.padding = unit(0.5, 'lines'),max.overlaps = 30) +
  ylab("Rural Agricultural Wages") + theme_light()

#Part 3 
prop_in_agri_state<-income_data %>% as_survey(weights = c(weight)) %>% group_by(state,industry) %>% 
  summarize(n = survey_total()) %>% mutate(prop_in_agri=round(n*100/sum(n),2)) %>% 
  arrange(-prop_in_agri) %>% filter(industry=="(00) Agriculture 0") %>% 
  select(state,prop_in_agri) 

prop_in_agri_state %>% ggplot(aes(x=prop_in_agri,y=fct_reorder(state,prop_in_agri),fill=state)) +
  geom_bar(stat="identity") + 
  xlab("Proportion in Agriculture") +
  ylab("States") +
  theme(legend.position="none") +
  ggtitle("People in Agriculture") +
  scale_x_continuous(limits = c(0, 100),expand=c(0,0)) + 
  geom_text(aes(label=prop_in_agri),hjust=-0.1)

