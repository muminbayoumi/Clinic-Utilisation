library(tidyverse)
library(magrittr)
library(here)
library(magrittr)
library(readxl)
library(RColorBrewer)
library(lubridate)
library(forcats)
library(gridExtra)
setwd(here::here())

CU <- read_excel("6045-1920 - Clinic Slots Utilisation - Pilgrim Hospital and Peripheral S....xlsx",
sheet = "Data", skip = 11)

CU %<>% filter(Division=='Surgery') 
colnames(CU) <- str_replace_all(colnames(CU),pattern = ' ',replacement ='.') 

#Surgical Clinics
CU %<>% filter(Session.Clinic.Code=='SD-ATESD'|
                  Session.Clinic.Code=='SD-MMCSD'|
                  Session.Clinic.Code=='JH-ZAIO4'|
                  Session.Clinic.Code=='JH-MIRO2'|
                  Session.Clinic.Code=='PH-RTH11'|
                  Session.Clinic.Code=='PH-MIRPF'|
                  Session.Clinic.Code=='PH-ATEPF'|
                  Session.Clinic.Code=='PH-MOB21'|
                  Session.Clinic.Code=='PH-MMC35'|
                  Session.Clinic.Code=='PH-ATE35'|
                  Session.Clinic.Code=='PH-MIR41'|
                  Session.Clinic.Code=='PH-RTH45'|
                  Session.Clinic.Code=='PH-MOB40'|
                  Session.Clinic.Code=='PH-ZAI50'|
                  Session.Clinic.Code=='PH-GOR52')

#Two Man Clinics

CU$TMC <-  'No'
CU$TMC[CU$Session.Clinic.Code=='PH-RTH11'|CU$Session.Clinic.Code=='PH-ATE35'|
            CU$Session.Clinic.Code=='PH-MIR41'|
            CU$Session.Clinic.Code=='PH-ZAI50'] <-  "Yes"
CU$TMC %<>%  as_factor()

# Peripheral VS Pilgrim 
CU$PvP <- 'Pilgrim'
CU$PvP[CU$Clinic.Hospital!='Pilgrim Hospital Boston'] <- 'Peripheral'
CU %>% group_by(PvP) %>% summarise(count=n()) %>%   ggplot(aes(PvP,y=count))+ geom_bar(stat='identity') + geom_label(aes(label=count))
CU %>% group_by(PvP,Clinic.Hospital,DNAs) %>% summarise(count=n()) %>%  ggplot(aes(DNAs,y=count)) + geom_bar(stat = 'identity')+
       facet_wrap(~Clinic.Hospital) + geom_label(aes(label=count))




#Clean.Up 
CU$Clinic.Hospital %<>% as_factor()
CU$Specialty %<>% as_factor()  
CU$Session.Start.Date %<>% date()
CU %<>%  mutate(M=month(Session.Start.Date,label = T))
CU$M %<>% forcats::fct_relevel('Jan',after=12)
CU$Utilisation %<>% as.numeric() 
CU$PvP %<>% as_factor()
CU %<>%  mutate(DNARate= DNAs/Booked.Slots)
CU %<>%  mutate(True.U= (Booked.Slots-DNAs)/Total.Slots)
CU %<>%  mutate(BR=Booked.Slots/Total.Slots)
CU %<>% filter(Booked.Slots!=0)



CU %>% ggplot(aes(BR))+geom_histogram(aes(y=(..count..)/sum(..count..)),bins=4)+ scale_y_continuous(labels=scales::percent)
CU %>% ggplot(aes(BR))+geom_histogram(aes(y=(..count..)/sum(..count..)),bins=4)+ scale_y_continuous(labels=scales::percent) +facet_wrap(~PvP)
CU %>% ggplot(aes(BR,fill=PvP))+geom_density(alpha=0.5)
CU %>% ggplot(aes(DNARate,fill=PvP))+geom_density(alpha=0.5)
CU %>%  ggplot(aes(Utilisation,fill=PvP))+geom_histogram(alpha=0.5)
CU %>%  ggplot(aes(True.U,fill=PvP))+geom_histogram(alpha=0.5)+facet_wrap(~PvP)
CU %>%  ggplot(aes(Utilisation,fill=PvP))+geom_density(alpha=0.5)
CU %>%  ggplot(aes(True.U,fill=PvP))+geom_density(alpha=0.5)
CU %>%  ggplot(aes(Utilisation,fill=TMC))+geom_histogram(alpha=.5)
CU %>%  ggplot(aes(True.U,fill=TMC))+geom_histogram(alpha=.5)
CU %>%  ggplot(aes(Utilisation,fill=TMC))+geom_density(alpha=.5)
CU %>%  ggplot(aes(DNAs,fill=PvP))+geom_bar() + facet_wrap(~PvP)
CU %>%  ggplot(aes(DNAs,fill=Clinic.Hospital))+geom_bar(position = 'dodge') + facet_wrap(~PvP)
CU %>%  ggplot(aes(x=Clinic.Hospital,DNAs))+geom_boxplot() + facet_wrap(~PvP)

#Pilgrim Vs Periphry 
CUsumPvP <-  CU %>% group_by(PvP) %>% summarise(P25=paste0(round(quantile(Utilisation)[[2]],2)*100,'%'),P75=paste0(round(quantile(Utilisation)[[4]],2)*100,'%'))
CUsumPvPTU <-  CU %>% group_by(PvP) %>% summarise(P25=paste0(round(quantile(True.U)[[2]],2)*100,'%'),P75=paste0(round(quantile(True.U)[[4]],2)*100,'%'))
CU %>%  ggplot(aes(Utilisation,x=PvP))+geom_boxplot(alpha=.5) + annotation_custom(tableGrob(CUsumPvP), xmin=1.4, xmax=1.5, ymin=0.55, ymax=0.6)
CU %>%  ggplot(aes(True.U,x=PvP))+geom_boxplot(alpha=.5) + annotation_custom(tableGrob(CUsumPvPTU), xmin=1.4, xmax=1.5, ymin=0.4, ymax=0.5)

CUsumPvPDNA <-  CU %>% group_by(PvP) %>% summarise(P25=paste0(round(quantile(Utilisation)[[2]],2)*100,'%'),P75=paste0(round(quantile(Utilisation)[[4]],2)*100,'%'))

CU %>%  ggplot(aes(Utilisation,x=PvP))+geom_boxplot(alpha=.5) + annotation_custom(tableGrob(CUsumPvP), xmin=1.4, xmax=1.5, ymin=0.55, ymax=0.7)




#Hosps
CUsumH <-  CU %>% group_by(Clinic.Hospital) %>% summarise(P25=paste0(round(quantile(Utilisation)[[2]],2)*100,'%'),P75=paste0(round(quantile(Utilisation)[[4]],2)*100,'%'))
CUsumHTU <-  CU %>% group_by(Clinic.Hospital) %>% summarise(P25=paste0(round(quantile(True.U)[[2]],2)*100,'%'),P75=paste0(round(quantile(True.U)[[4]],2)*100,'%'))
CU %>%  ggplot(aes(Utilisation,x=Clinic.Hospital))+geom_boxplot(alpha=.5) + annotation_custom(tableGrob(CUsumH), xmin=1, xmax=2.5, ymin=.55, ymax=0.6)
CU %>%  ggplot(aes(True.U,x=Clinic.Hospital))+geom_boxplot(alpha=.5) + annotation_custom(tableGrob(CUsumHTU), xmin=2, xmax=2.5, ymin=.4, ymax=0.6)


CUsumHDNA <-  CU %>% group_by(Clinic.Hospital) %>% summarise(P25=paste0(round(quantile(DNARate)[[2]],2)*100,'%'),P75=paste0(round(quantile(DNARate)[[4]],2)*100,'%'))
CU %>%  ggplot(aes(DNARate,x=Clinic.Hospital))+geom_boxplot(alpha=.5) + annotation_custom(tableGrob(CUsumHDNA), xmin=1, xmax=2.5, ymin=0, ymax=0.5)


CU %>% ggplot(aes(DNAs,fill=Clinic.Hospital))+geom_bar(position = 'dodge')
CU %>% ggplot(aes(DNARate,y=Clinic.Hospital))+geom_jitter(height = 0.1)+coord_flip() 


CUsumClin <-  CU %>% group_by(Session.Clinic.Code) %>% summarise(P25=paste0(round(quantile(True.U)[[2]],2)),P75=paste0(round(quantile(True.U)[[4]],2)))
CUsumClin <-  CU %>% group_by(Session.Clinic.Code) %>% summarise(P25=paste0(round(quantile(Utilisation)[[2]],2)*100,'%'),P75=paste0(round(quantile(Utilisation)[[4]],2)*100,'%'))
CU %>%  ggplot(aes(Utilisation,x=Session.Clinic.Code))+geom_boxplot(alpha=.5) + annotation_custom(tableGrob(CUsumClin,theme=ttheme_minimal(base_size = 6)), xmin=1, xmax=2.5, ymin=0, ymax=0.5)
CUsumClin %>% reshape2::melt(value.name ='True.U',id='Session.Clinic.Code') %>% ggplot(aes(Session.Clinic.Code,as.numeric(True.U),fill=fct_infreq(variable)))+geom_bar(stat = 'identity',position = 'dodge') +coord_flip() 



#Colo Vs Gen Surg
CUsumH <-  CU %>% group_by(Clinic.Hospital) %>% 
  summarise(P25=paste0(round(quantile(Utilisation)[[2]],2)*100,'%'),
            P75=paste0(round(quantile(Utilisation)[[4]],2)*100,'%'))
CU %>%  ggplot(aes(Utilisation,x=Clinic.Hospital))+
  geom_boxplot(alpha=.5) + 
  annotation_custom(tableGrob(CUsumH), xmin=1, xmax=2.5, ymin=0, ymax=0.5)
CU[CU$Utilisation==0,"Session.Clinic.Code"]


CU$FB <- 'Yes'
CU$FB[which(CU$Total.Slots != CU$Booked.Slots)] <- 'No'

CU %>% ggplot(aes(as_factor(Total.Slots),fill=FB))+geom_bar()
CU$DN <- 'No'
CU$DN[which(CU$DNAs>=1)] <- 'Yes'
CU %>% filter(FB=='No') %>% ggplot(aes(Available.Slots,fill=DN))+geom_bar()+facet_wrap(~M)
