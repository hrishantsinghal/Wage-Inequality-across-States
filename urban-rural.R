library(tidyverse)
library(srvyr)
library(readxl)
library(ggrepel)
library(ggpubr)
library(ggrepel)
# Loading the Data
setwd("D://Income - Casual Labour")
load("36151-0001-Data.rda")
main_data<-da36151.0001
main_data$FWT

#Taking out useful columns and finding wages per day 
income_data<-main_data %>% 
  separate(col=STATEID,into=c("No","State","No."),sep=c(5,-3)) %>%
  select(state=State,duration=WS9,salary=WS10,nature=WS13,b1=WS11MEALSRS,
         b2=WS11HOUSERS,b3=WS12,
         weight=FWT,total=WSEARN,w_days=WS7,employer=WS14,WS11,ED6,ED2,
         MGYEAR5,URBAN2011,WS4,WS5) %>% 
  drop_na() %>% 
  filter(duration %in% c("(2) Per Month 2","(1) Per Day 1")) %>% 
  mutate(sal_per_day=ifelse(duration=="(2) Per Month 2",
                            salary/30+b3/365,salary+b3/365))

#Filtering Columns we need
wdata<- income_data %>% 
  filter(nature=="(1) Casual daily 1") 

#Finding people who are 5th and 10th pass in each state
fifthpass<-as.character(levels(wdata$ED6))[6:17]
tenthpass<-as.character(levels(wdata$ED6))[11:17]
pd_dat<- wdata %>%
  mutate(total_wage_by_total_work=round((total/w_days),2)) %>% 
  mutate(tenth=ifelse(ED6 %in% tenthpass,1,0),fifth=ifelse(ED6 %in% fifthpass,1,0))

pd_dat1<-pd_dat %>% 
  as_survey(weights = c(weight)) %>% 
  group_by(state) %>% 
  summarise(wd=survey_mean(w_days),
            wage=round(survey_mean(sal_per_day),2),
            tenth=round(survey_mean(tenth),2)*100,
            fifth=round(survey_mean(fifth),2)*100) %>% 
  mutate(state=fct_reorder(state,wage))

#Grouping by Education
pd_dat %>% as_survey(weights = c(weight)) %>% 
  group_by(ED6) %>% 
  summarise(wage=round(survey_mean(sal_per_day),2))

#Grouping by Literacy
pd_dat %>% as_survey(weights = c(weight)) %>% group_by(ED2) %>% 
  summarize(n = survey_total()) %>% 
  mutate(n_prop=n*100/sum(n)) %>% 
  select(ED2,n_prop)


#Finding People who are in agriculture in each state
people_in_agri<-pd_dat %>% as_survey(weights = c(weight)) %>% group_by(state,WS5) %>% 
  summarize(n = survey_total()) %>% 
  arrange(-n) %>% filter(WS5=="(00) Agriculture 0") %>% select(state,n_in_agri=n) 

pd_dat1<-merge(pd_dat1,people_in_agri,by="state")

#Finding Wages at each education Level
pd_dat %>% as_survey(weights = c(weight)) %>% group_by(ED6) %>% 
  summarize(n = survey_mean(sal_per_day))

#No. of People in each state in different professions
prop_in_agri_dat<-pd_dat %>% as_survey(weights = c(weight)) %>% group_by(state,WS5) %>% 
  summarize(n = survey_total()) %>% mutate(prop_in_agri=n*100/sum(n)) %>% 
  arrange(-prop_in_agri) %>% filter(WS5=="(00) Agriculture 0") %>% 
  select(State="state",prop_in_agri) 


#Importing GDP-Per-Cap and GDP data
Per_Capita_SDP_State <- read_excel("Per_Capita_SDP_State.xlsx")
sdp<-read_excel("sdp.xlsx")
agri_state<-read_excel("Agriculture_State.xlsx")
sdp<-merge(sdp,Per_Capita_SDP_State,by="State")
sdp<-merge(sdp,agri_state,by="State")
sdp<-merge(sdp,prop_in_agri_dat,by="State")
gdp_dat<-sdp %>%
  filter(SDP>=7271982)


#Some cleaning in the data
gdp_dat$State[gdp_dat$State=="Odisha"]<-"Orissa"
gdp_dat$State[gdp_dat$State=="Jammu and Kashmir"]<-"Jammu & Kashmir"

names(gdp_dat)[names(gdp_dat)=="State"]="state"

#Merging wages and the GDP data
wage_gdp<-merge(pd_dat1,gdp_dat,by="state",all=TRUE)
wage_gdp <- wage_gdp %>% drop_na() %>% merge(people_in_agri,by="state") %>% 
  mutate(per_capita_agri=agri*100000/n_in_agri)



#Inlcuding Literacy and PSU Data
literacy<-income_data %>% as_survey(weights = c(weight)) %>% 
  group_by(state,ED2) %>% 
  summarize(n = survey_total()) %>% mutate(lr=n*100/sum(n)) %>% 
  filter(ED2=="(1) Yes 1") %>% arrange(-lr) %>% select(state,lr)

psu_e<-income_data %>% as_survey(weights = c(weight)) %>% 
  group_by(state,employer) %>% 
  summarize(n = survey_total()) %>% mutate(psu=n*100/sum(n)) %>% 
  filter(employer=="(1) Govt/PSU 1") %>% arrange(-psu) %>% select(state,psu)

#Combining wages,gdp to lieracy and data
wage_gdp_literacy<-wage_gdp %>% inner_join(literacy,by="state")
wage_gdp_literacy<-wage_gdp_literacy %>% inner_join(psu_e,by="state")

#Grouping Urban and Rural Wages
urban_wage<-pd_dat %>% filter(URBAN2011=="(1) urban 1") %>% 
  as_survey(weights = c(weight)) %>% 
  group_by(state) %>% 
  summarise(urban_wd=survey_mean(w_days),
            urban_wage=round(survey_mean(sal_per_day),2)) %>% 
  mutate(state=fct_reorder(state,urban_wage))

rural_wage<-pd_dat %>% filter(URBAN2011=="(0) rural 0") %>% 
  as_survey(weights = c(weight)) %>% 
  group_by(state) %>% 
  summarise(rural_wd=survey_mean(w_days),
            rural_wage=round(survey_mean(sal_per_day),2)) %>% 
  mutate(state=fct_reorder(state,rural_wage))

#Merging Urban and Rural Wages
urban_rural<-merge(urban_wage,rural_wage,by="state") %>% 
  select(state,urban_wd,urban_wage,rural_wage,rural_wd)

#Looking at proportion of People employed in Casual Labour in each State
prop_casual_labour=income_data %>% as_survey(weights = c(weight)) %>% group_by(state,nature) %>% 
  summarize(n = survey_total()) %>% mutate(casual_labour=n*100/sum(n)) %>% 
  filter(nature=="(1) Casual daily 1") %>% select(state,casual_labour)

wage_gdp_literacy<-wage_gdp_literacy %>% inner_join(prop_casual_labour,by="state")

#Combining above data with urban and rural wages
wage_gdp_urban<-wage_gdp_literacy %>% filter(state!="Kerala") %>% 
  inner_join(urban_rural,by="state") 


#Plotting correlation of urban-rural, wages-casual labour, per_capita_gdp-wages
urban_rural_wage<-wage_gdp_urban %>% 
  ggplot(aes(x=rural_wage,y=urban_wage,label=state)) + 
  geom_point() +
  geom_label_repel() + geom_smooth(method='lm', formula= y~x) +
  stat_cor(label.y = 35)

casual_labour_wage<-wage_gdp_urban %>% ggplot(aes(x=casual_labour,y=wage,label=state)) + 
  geom_point() +
  geom_label_repel() + 
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y~x) +
  stat_cor(label.y = 35) 

per_capita_wage<-wage_gdp_urban %>% ggplot(aes(x=per_capita,y=wage,label=state)) + 
  geom_point() +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y~x) +
  stat_cor(label.y = 35) + geom_label()

literacy_wage<-wage_gdp_urban %>% ggplot(aes(x=lr,y=wage,label=state)) + 
  geom_point() +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y~x) +
  stat_cor(label.y = 35) + xlab("Literacy") + geom_label()

psu_wage<-wage_gdp_urban %>% ggplot(aes(x=psu,y=wage,label=state)) + 
  geom_point() +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y~x) +
  stat_cor(label.y = 35) + xlab("people employed in PSUs") + geom_label_repel(max.overlaps = 30)

agri_wage<-wage_gdp_urban %>% ggplot(aes(x=per_capita_agri,y=rural_wage,label=state)) + 
  geom_point() +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y~x) +
  stat_cor(label.y = 35) + xlab("per_capita_agri") + geom_label_repel(max.overlaps = 30)


urban_rural_wage
per_capita_wage
literacy_wage
casual_labour_wage
psu_wage
agri_wage

regression_model<-lm(wage~psu+per_capita,wage_gdp_urban)
summary(regression_model)


cor(wage_gdp_urban$lr,wage_gdp_urban$per_capita_net)
cor(wage_gdp_urban$lr,wage_gdp_urban$psu)

wage_gdp_urban %>% ggplot(aes(x=wage,y=fct_reorder(state,wage),fill=state)) +
  geom_bar(stat="identity") + 
  xlab("Average Salary") +
  ylab("States") +
  theme(legend.position="none") +
  ggtitle("Average Salary Per Person in a Day in Casual Labour") +
  scale_x_continuous(limits = c(0, 400),expand=c(0,0)) + 
  geom_text(aes(label=wage),hjust=-0.1)

colinearity_test<-data.frame(w=wage_gdp_urban$wage,
                       lr=wage_gdp_urban$lr,
                       cl=wage_gdp_urban$casual_labour,
                       per=wage_gdp_urban$per_capita,
                       psu=wage_gdp_urban$psu)
cor(colinearity_test)









