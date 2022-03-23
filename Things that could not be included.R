##Part 1
#Average wages for literates and illiterates
income_data %>% as_survey(weights = c(weight)) %>% 
  group_by(lit) %>% 
  summarise(wage=round(survey_mean(sal_per_day),2)) %>% 
  separate(col=lit,sep=c(4,-2),into=c("n","literate","no.")) %>% select(literate,wage) %>% 
  ggplot(aes(x=literate,y=wage,fill=literate)) + geom_bar(stat="identity",width=0.6) +
  theme_bw() + 
  xlab("Literacy") +
  ylab("Wages") +
  theme(legend.position="none") +
  geom_text(aes(label=wage),vjust=-0.1)

#Proportion of Literates in each state
lit_state<-income_data %>% as_survey(weights = c(weight)) %>% group_by(state,lit) %>% 
  summarize(n = survey_total()) %>% 
  mutate(n_prop=n*100/sum(n)) %>% filter(lit=="(1) Yes 1") %>% 
  select(state,n_prop) 

#Combining with the previous data
pd_dat<-merge(pd_dat,lit_state)

print(paste("Correlation Coefficient between wages and percentage of literacy in each state is",round(cor(pd_dat$n_prop,pd_dat$wage),2)))

##Part 2
#Filtering Data at each education Level 
edu_dat<-income_data %>% as_survey(weights = c(weight)) %>% 
  group_by(edu_level) %>% 
  summarise(wage=round(survey_mean(sal_per_day),2)) %>% select(edu_level,wage)

#cleaning the data
edu_dat$edu_level=as.character(edu_dat$edu_level)

edu_dat[14,1]="(13) 1 year post-secondary 13"         
edu_dat[15,1]="(14) 2 year post-secondary 14" 

#finally presenting it 
abc<-edu_dat %>% separate(col=edu_level,into=c("n","edu_level","no"),sep=c(5,-2)) %>% 
  select(`Education Level`=edu_level,Wage=wage)

datatable(abc)

#people who are fifth and tenthpass in each state
fifth<-as.character(levels(income_data$edu_level))[6:17]
tenth<-as.character(levels(income_data$edu_level))[11:17]
  
income_data<-income_data %>% 
  mutate(tenthp=ifelse(edu_level %in% tenth,1,0),fifthp=ifelse(edu_level %in% fifth,1,0))
passdat<-income_data %>% as_survey(weights = c(weight)) %>% 
  group_by(state) %>% summarise(tenthpass=round(survey_mean(tenthp),2)*100,
                                   fifthpass=round(survey_mean(fifthp),2)*100) %>% 
  select(state,tenthpass,fifthpass)
pd_dat<-merge(pd_dat,passdat)
cor(pd_dat$wage,pd_dat$fifthpass)
cor(pd_dat$wage,pd_dat$tenthpass)





