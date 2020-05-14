#__________________________________________________3- Bulk visuals- Unido data____________________________________________________________________
                            #1- Sectoral evolution of all variables
#_______________________________________________________________________________________________________________________________________________________________

rm(list=ls()); 
library(tidyverse); library(plotly); library(trelliscopejs); library(ggrepel); library(ggthemes)

unido0<-readRDS("Outputs/dta/unido02d.RDS") %>% filter(value!=0)
unido<-readRDS("Outputs/dta/unido2d.RDS") %>% filter(value!=0)


#1-Unido- sectoral evol:----
areas<-read.csv2("Outputs/dta/areas.csv")

#few sectors(unido0)

d0<-unido0 %>% arrange(year) %>%filter(variable%in%c("emp","capitallc" ,"valc" ,"emp_share", "valc_share",
                                                     "nbest", "nbfirm","wagelc", "wagelc_share", "ptylc", "wageus", "labour_share",
                                                     "ptylc_share"), isic!="sum")


d0 %>% ggplot(aes(year, value, color=isic, label=isic, group=isic))+geom_line(size=0.9)+
  #geom_text_repel(data=subset(d0, year==2010),nudge_x = 0.1,nudge_y = 0, segment.color = NA)+
  #guides(color=FALSE)+
  theme_wsj()+
  facet_trelliscope(~area+variable, scales="free", as_plotly = T, width = 1000, path="Outputs/unido", name="sect.evol")

#all sectors (unido)

d<-unido %>% arrange(year) %>%filter(variable%in%c("emp","capitallc" ,"valc" ,"emp_share", "valc_share",
                                                     "nbest", "nbfirm","wagelc", "wagelc_share", "ptylc", "wageus", "labour_share",
                                                     "ptylc_share"), isic!="sum")
d%>%   
  ggplot(aes(year, value, color=isic, label=isic, group=isic))+geom_line(size=0.9)+
  #geom_text_repel(data=subset(d, year==2010),nudge_x = 0.1,nudge_y = 0, segment.color = NA)+guides(color=FALSE)+
  theme_wsj()+
  facet_trelliscope(~area+variable,scales="free", as_plotly = T, width = 1000, path="Outputs/unido", name="sect.evol detailed")



#2-Morocco_alone----


d0<-filter(unido0, area=="MAR") %>% arrange(year) %>%filter(variable%in%c("emp","capitallc" ,"valc" ,"emp_share", "valc_share",
                                                     "nbest", "nbfirm","wagelc", "wagelc_share", "ptylc", "wageus", "labour_share",
                                                     "ptylc_share"), isic!="sum")
d0%>%   
  ggplot(aes(year, value))+geom_line(size=0.9)+theme_wsj()+
  facet_trelliscope(~area+variable+isic,scales="free", as_plotly = T, width = 1000, path="Outputs/unido", name="..mar")


d<-filter(unido, area=="MAR") %>% arrange(year) %>%filter(variable%in%c("emp","capitallc" ,"valc" ,"emp_share", "valc_share",
                                                     "nbest", "nbfirm","wagelc", "wagelc_share", "ptylc", "wageus", "labour_share",
                                                     "ptylc_share"), isic!="sum")
d%>%   
  ggplot(aes(year, value))+geom_line(size=0.9)+theme_wsj()+
  facet_trelliscope(~area+variable+isic,scales="free", as_plotly = T, width = 1000, path="Outputs/unido", name="..mar detail")




#_______________________________________________________________END________________





unidow<-spread(unido, variable, value)



#productivity vs wages
plot(y=log(unidow$wagelc), x=log(unidow$ptylc))






morocco<-unido %>% filter(area %in% c("Morocco"))
  
coverage<-morocco %>%
  group_by(area, variable, isic) %>% arrange(year) %>% 
  summarize(range=paste0(min(year),"_",max(year))) %>% spread(variable, range) #%>% tab_df()

coverage

moroccow<-morocco %>% spread(variable, value)
plot(log(moroccow$emp_share), log(moroccow$wagelc_share))
l<-lm(log(moroccow$emp_share)~ log(moroccow$wagelc_share))
abline(l)

str(unido)


