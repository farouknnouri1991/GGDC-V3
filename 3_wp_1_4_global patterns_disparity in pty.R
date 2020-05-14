rm(list=ls())
library(plotly)
ggplot2::theme_set(ggplot2::theme_bw()) 
unido<-readRDS("Outputs/dta/unido2d.RDS") #%>% filter(area%in%dta$Area)

#_______________________________________________________________________________________________________________________________________________________________




#1-Productivity disparity among sectors and countries----
#disparity in sector productivity within country (comparing intra manuf with disparity between sectors)
ptymanuf<-subset (unido, area=="MAR"& year==2010& variable=="ptyus_share") %>% arrange(value) %>% mutate(sector="manufacturing")
ptysectors<-subset (morocco, Year==2010& Variable=="ptyq_share") %>% select(-Regioncode, -dec) %>% arrange(Value) %>% mutate(sector="economy")
names(ptymanuf)<-names(ptysectors)
rbind(ptymanuf, ptysectors) %>% stargazer(type="text", summary = F)

#disparity in sectors productivity between countries

s1<-subset (unido, variable=="ptyus" ) %>% group_by(isic, year) %>% mutate(pty_share=value/max(value))

s01<-subset(s1, pty_share==1) %>% select(-variable, -pty_share) %>% rename(maxarea=area, maxvalue=value)

s1 %>% filter(year==2009, area%in%c("MAR", "CHN"))

s1<-merge(s1, s01)

dta<-readRDS("Outputs/dta/dta.RDS")
s2<-subset (dta, Variable=="ptyq" & DB=="GGDC" & PPP=="PPP"& !Area%in%c("BRA", "ZMB")
              ) %>% group_by(Sector, Year) %>% mutate(pty_share=Value/max(Value, na.rm = T)) %>% select(-PPP, -DB)
s02<-subset(s2, pty_share==1) %>% select(-Variable, -pty_share) %>% rename(maxarea=Area, maxvalue=Value) %>% select(-Regioncode)
s2<-merge(s2, s02) %>% select(-Regioncode)
names(s2)<-names(s1)
rbind(s2, s1) %>% filter(area=="MAR", year==2009)

      #variance in sctors against RODRIK, MANUFACTURING IS EXIBITING HIGH PTY VARIATION
      s2<-filter(dta, Variable=="ptyq" , DB=="GGDC" , PPP=="PPP", !Area%in%c("BRA", "ZMB"), Year<2011) %>% 
        group_by(Sector, Year) %>% summarize(cv=100*sd(log(Value))/mean(log(Value)),
                                       q=quantile(Value, 0.9)/quantile(Value, 0.1)
                                       ) %>% ungroup()
      s2$Sector<-as.factor(s2$Sector)
      p<-s2 %>% ggplot(aes(Year, cv, shape=as.factor(Sector), group=Sector))+geom_line()+geom_point(size=2)+
        scale_shape_manual(values=1:nlevels(s2$Sector))
      p
      ggplotly(p)
      
      
  
#check variability in manuf using undo (WITH RODRIK!!)
s2<-unido %>% filter(isic=="sum", variable=="ptyus")%>% 
  group_by(isic, year) %>% summarize(cv=100*sd(log(value))/mean(log(value)),
                                 q=quantile(value, 0.9)/quantile(value, 0.1), n=n_distinct(area) 
                                 ) %>% ungroup()
s2$isic<-as.factor(s2$isic)

(p<-s2 %>% ggplot(aes(year, cv))+geom_line()+geom_point(size=2))#Wha is happening aroud 1999
(p<-s2 %>% ggplot(aes(year, q))+geom_line()+geom_point(size=2))#Wha is happening aroud 1999
(p<-s2 %>% ggplot(aes(year, n))+geom_line()+geom_point(size=2))#its not about sampls


#Go deeper

s2<-unido %>% filter(variable=="ptyus")%>% 
  group_by(isic, year) %>% summarize(cv=100*sd(log(value))/mean(log(value)),
                                 q=quantile(value, 0.9)/quantile(value, 0.1), n=n_distinct(area) 
                                 ) %>% ungroup()
s2$isic<-as.factor(s2$isic)

p<-s2 %>% ggplot(aes(year, cv))+geom_line()+geom_point(size=1)+facet_wrap(~isic, scales="free")
ggplotly(p)
#WAAAAAAAAAAAAW what is happening 
p<-s2 %>% ggplot(aes(year, q))+geom_line()+geom_point(size=1)+facet_wrap(~isic,  scales="free")
ggplotly(p)

