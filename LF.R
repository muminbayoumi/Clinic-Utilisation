library(tidyverse)
library(magrittr)
library(here)
library(magrittr)
library(readxl)
library(RColorBrewer)
library(lubridate)
library(forcats)

setwd(here::here())


CU <- read_excel("6045-1920 - Clinic Slots Utilisation - Pilgrim Hospital and Peripheral S....xlsx", 
                 sheet = "Data", col_types = c("text", 
                                               "text", "text", "text", "date", "text", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric"), skip = 11)
 
table(CU$Division) 

CU %<>% filter(Division=='Surgery')
table(CU$Specialty)
CU1 <- CU[str_detect(CU$Specialty,'Colorectal')|str_detect(CU$Specialty,'Gen Surg')|str_detect(CU$Specialty,'GS Endo')|str_detect(CU$Specialty,'General Surgery'),]
CU2 <-  CU1[CU1$Specialty!='100 - General Surgery Breast Clinic',]
CU2 %>%  ggplot(aes(x=Specialty))+ geom_bar()+coord_flip()

colnames(CU2) <- str_replace_all(colnames(CU2),pattern = ' ',replacement ='.') 

table(CU2$Clinic.Hospital)
CU2$P <- CU2$Clinic.Hospital
CU2$P[CU2$P !='Pilgrim Hospital Boston'] <- 'Peripheral'
CU2 %<>% filter(Clinic.Hospital!='Spire Nottingham (ULHT)') 
CU2 %<>% filter(Clinic.Hospital!='Louth County Hospital') 

CU2 %<>% filter(Session.Clinic.Code=='SD-ATESD'|
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

max(CU2$Session.Start.Date)
min(CU2$Session.Start.Date)
CU2 %<>% mutate(M=month(Session.Start.Date))

CU2 %>% group_by(Clinic.Hospital,Session.Clinic.Code,M) %>% summarise(Median=median(Utilisation))%>% 
  filter(Clinic.Hospital=='Pilgrim Hospital Boston') %>%
  ggplot(aes(x=Session.Clinic.Code,y=Median,fill=forcats::fct_inorder(as.factor(M))))+
  geom_bar(stat = 'identity',position = position_dodge())+
  coord_flip()
CU2 %>% group_by(Clinic.Hospital,M) %>% summarise(Median=median(Utilisation))%>% 
  ggplot(aes(x=Clinic.Hospital,y=Median,fill=forcats::fct_inorder(fct_recode(as.factor(M),Jan20='1',Aug='8',Sep='9',Oct='10',Nov='11',Dec='12'))))+
  geom_bar(stat = 'identity',position = position_dodge())+
  coord_flip()
CU2 %>% group_by(P,M) %>% summarise(Median=median(Utilisation))%>% 
  ggplot(aes(x=P,y=Median,fill=forcats::fct_inorder(fct_recode(as.factor(M),Jan20='1',Aug='8',Sep='9',Oct='10',Nov='11',Dec='12'))))+
  geom_bar(stat = 'identity',position = position_dodge())+
  coord_flip()
## BarPLOT Colorectal
CU2 %>%filter(Session.Clinic.Code=='SD-ATESD'|
                Session.Clinic.Code=='JH-ZAIO4'|
                Session.Clinic.Code=='JH-MIRO2'|
                Session.Clinic.Code=='PH-RTH11'|
                Session.Clinic.Code=='PH-MIRPF'|
                Session.Clinic.Code=='PH-ATEPF'|
                Session.Clinic.Code=='PH-ATE35'|
                Session.Clinic.Code=='PH-MIR41'|
                Session.Clinic.Code=='PH-RTH45'|
                Session.Clinic.Code=='PH-ZAI50') %>%  group_by(P,M) %>% summarise(Median=median(Utilisation)) %>% mutate(Pct=paste0(Median*100,'%'))%>% 
  ggplot(aes(x=P,y=Median,fill=forcats::fct_inorder(fct_recode(as.factor(M),Jan20='1',Aug='8',Sep='9',Oct='10',Nov='11',Dec='12'))))+
  geom_bar(stat = 'identity',position = position_dodge())+ geom_label(aes(x=P,y=Median,label=Pct),position = position_dodge(width = 1))+coord_flip() +labs(title = 'Utilisation Colorectal Clinics by Month',fill= 'Month',y='Median Utilisation Per Month') + theme_minimal() +scale_fill_brewer(palette =8)

CU2 %>%filter(Session.Clinic.Code=='SD-ATESD'|
                Session.Clinic.Code=='JH-ZAIO4'|
                Session.Clinic.Code=='JH-MIRO2'|
                Session.Clinic.Code=='PH-RTH11'|
                Session.Clinic.Code=='PH-MIRPF'|
                Session.Clinic.Code=='PH-ATEPF'|
                Session.Clinic.Code=='PH-ATE35'|
                Session.Clinic.Code=='PH-MIR41'|
                Session.Clinic.Code=='PH-RTH45'|
                Session.Clinic.Code=='PH-ZAI50') %>%  ggplot(aes(x=Month,y=Utilisation))+geom_boxplot(col='black') +
  stat_summary(geom="text", 
               fun.y=quantile,
               aes(label=paste0(round(..y..,2)*100,'%')),col='Red',position =position_nudge(x=0.5,), size=4) + facet_wrap(~P) + scale_y_continuous(limits = c(0.45,1))

  


##PLOT 2 Man Clinics Barplot & Boxplot
  CU2 %>%filter(  Session.Clinic.Code=='PH-RTH11'|
                Session.Clinic.Code=='PH-ATE35'|
                Session.Clinic.Code=='PH-MIR41'|
                Session.Clinic.Code=='PH-ZAI50') %>%  group_by(P,M) %>% summarise(Median=median(Utilisation))%>% mutate(Pct=paste0(Median*100,'%'))%>%
  ggplot(aes(x=P,y=Median,fill=forcats::fct_inorder(fct_recode(as.factor(M),Jan20='1',Aug='8',Sep='9',Oct='10',Nov='11',Dec='12'))))+
  geom_bar(stat = 'identity',position = position_dodge())+
  coord_flip() + geom_label(aes(x=P,y=Median,label=Pct),position = position_dodge(width = 1))+coord_flip() +labs(title = 'Utilisation of 2 Man Clinics by Month',fill= 'Month',y='Median Utilisation Per Month') + theme_minimal() +scale_fill_brewer(palette =8)

 CU2 %>%filter(  Session.Clinic.Code=='PH-RTH11'|
                    Session.Clinic.Code=='PH-ATE35'|
                    Session.Clinic.Code=='PH-MIR41'|
                    Session.Clinic.Code=='PH-ZAI50') %>%  ggplot(aes(x=P,y=Utilisation,col=Month))+geom_boxplot() +stat_summary(geom="text", fun.y=quantile,
                                                                                                                                aes(label=sprintf("%1.1f", ..y..),col=Month),
                                                                                                                                position=position_dodge2(width=.8,preserve = 'total'), size=5)
 