library(tidyverse)
library(srvyr)
library(readxl)
library(ggrepel)
library(ggpubr)

# Loading the Data
setwd("D://Income - Casual Labour")
load("36151-0001-Data.rda")
main_data<-da36151.0001

#Prepping the Data 
income_data<-main_data %>% 
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
  #Filtering only Casual Daily Workers
  filter(nature=="(1) Casual daily 1") 

#Average wage and working days for each state
wdata<-income_data %>% 
  as_survey(weights = c(weight)) %>% 
  group_by(state) %>% 
  summarise(wd=survey_mean(w_days),
            wage=round(survey_mean(sal_per_day),2)) %>% 
  mutate(state=fct_reorder(state,wage))

#importing SDP file and keeping only top 22 states (excluding Kerala) and doing some data cleaning before combining 
sdp<-read_excel("sdp.xlsx")
gdp_dat<-sdp %>% top_n(22) %>% filter(State!="Kerala") %>% select(state=State,SDP)
gdp_dat$state[gdp_dat$state=="Odisha"]<-"Orissa"
gdp_dat$state[gdp_dat$state=="Jammu and Kashmir"]<-"Jammu & Kashmir"

pd_dat<-merge(gdp_dat,wdata)
#Telangana is a part of gdp_dat but not income_data since IHDS did not have Telangana as a separate state during survey

#Plotting the Graph of average wage in each state
pd_dat %>% ggplot(aes(x=wage,y=fct_reorder(state,wage),fill=state)) +
  geom_bar(stat="identity") + 
  xlab("Average Salary") +
  ylab("States") +
  theme(legend.position="none") +
  ggtitle("Average Salary Per Person in a Day in Casual Labour") +
  scale_x_continuous(limits = c(0, 400),expand=c(0,0)) + 
  geom_text(aes(label=wage),hjust=-0.1)









