#___________________________________________________5- The case of Morocco: determinants?_______________________________________________________11/mars/2020
      #II-Structural change and education
      #1-Manuf/fire and eduction: emp_sh, ptyq_share, vaq_share
      #2-Ptyq(not share and education)
      #3-Employment correlations between sectors

#_______________________________________________________________________________________________________________________________________________________________
rm(list=ls())
library(trelliscopejs); library(tidyverse); library(plotly); library(stargazer); library(ggrepel)
source("GGDC_2_functions.R")
ggplot2::theme_set(ggplot2::theme_bw()) 

#source("GGDC_4_clusters.R")

#_______________________________________________________________________________________________________________________________________________________________


dta<-readRDS("Outputs/dta/dta.RDS") %>% filter(DB=="GGDC", PPP=="LC")  ; sectors<-attributes(dta)$sectors; 
gdp<-readRDS("Outputs/dta/gdp.RDS") %>% filter(Variable=="gdp_pc_ppp", Area%in%dta$Area)
unido<-readRDS("Outputs/dta/unido02d.RDS") %>% filter(area%in%dta$Area)
sectors<-attributes(dta)$sectors

#II-Structural change and education-----
#1-Manuf/fire and eduction: emp_sh, ptyq_share, vaq_share----
library(haven)
codes<-read.csv2(("Inputs/codes.csv")) %>% select(-X)

education<-read_dta("Inputs/4-Education/LeeLee_v1.dta") %>% filter(sex=="MF") %>% 
  select(country, year, lpc, lsc, lhc, tyr) %>% rename(Area=country, Year=year) %>% 
  merge(codes, by.x="Area", by.y="Area") %>% mutate(Area=Area_code) %>% select(-Area_code)

#compare how much is demading to increase emp_sh in manuf in comparison with other sectors (or through plots if it is necessary to have or no education)
d<-dta %>% filter(PPP=="LC", Variable%in%c("EMP_share", "VAQ_share", "ptyq_share"))
d<-merge(d, education ) %>% gather(var1, val1,9:12 ) %>% mutate(mar=(Area=="MAR"))


d<-d%>% filter(Sector=="FIRE")
p<-d %>%   ggplot(aes(val1, Value))+geom_point(alpha=1/2)+
  geom_point(data=filter(d, Area=="MAR", Sector=="FIRE"), color="Red")+
  geom_point(data=filter(d, Area=="THA", Sector=="FIRE"), color="Blue")+theme_bw()+
  geom_smooth(method = "lm",formula =  y ~ poly(1/x, 4)+poly(x, 2))+
  facet_wrap(~Variable+var1, scales="free")
p
ggplotly(p)

d<-d%>% filter(Sector=="MAN")
p<-d %>%   ggplot(aes(val1, Value))+geom_point(alpha=1/2)+
  geom_point(data=filter(d, Area=="MAR", Sector=="MAN"), color="Red")+
  geom_point(data=filter(d, Area=="THA", Sector=="MAN"), color="Blue")+theme_bw()+
  geom_smooth(method = "lm",formula =  y ~ poly(1/x, 4)+poly(x, 2))+
  facet_wrap(~Variable+var1, scales="free")
p
ggplotly(p)
#no need for teriary or sec educt, primary is sufficient total yers is not that important

#

dd<-d %>% select(-mar) %>%  spread( var1, val1) #%>% spread(Variable, Value)

#2-Ptyq(not share and education)----
dta<-readRDS("Outputs/dta/dta.RDS") %>% filter(DB=="GGDC", !Area%in%c("BRA", "ZMB")) 

d<-dta %>% filter(PPP=="PPP", Variable%in%c("ptyq")) %>% 
  merge( education ) %>% gather(var1, val1,9:12 ) #%>%  filter(var1=="lc")
p<-d %>%
  ggplot(aes(val1, log(Value)))+geom_point(alpha=1/2)+theme_bw()+
  #geom_point(data=filter(d, Area=="MAR"), color="Red")+
  #geom_point(data=filter(d, Area=="THA"), color="Blue")+theme_bw()+
  #geom_smooth(method = "lm",formula =  y ~ poly(1/x, 4)+poly(x, 2))+
  facet_trelliscope(~Sector+var1, scales="free")
p

ggplotly()

#a correlation analysis

corr<-d %>% group_by(var1, Variable, Sector) %>%nest() 

c1<-corr%>%  
  summarize(pval=map(.x = data,~cor.test(.$Value, .$val1)$p.value)<0.05) %>% unnest(pval)
c1 %>% spread(Sector, pval) %>% stargazer(summary = F, type="text")

c2<-corr%>%  
  summarize(corr=map(.x = data,~cor.test(.$Value, .$val1)$estimate %>% round(2)) ) %>% unnest(corr)
c2 %>% spread(Sector, corr) %>% stargazer(summary = F, type="text")

corr<-merge(c1, c2) 
corr%>% filter(pval==T) %>% spread(Sector, corr)





#3-Employment correlations between sectors---- 
#does some sectors goes together in employment?
d<-dta %>% filter(Variable=="EMP_share", DB=="GGDC", PPP=="LC", Sector!="SUM") %>% spread(Sector, Value)
GGally::ggpairs(d[7:16])

#you d better do it in changes(try not emp its just trends)

change<-dta %>% filter(Variable=="EMP_share", DB=="GGDC", PPP=="LC", Sector!="SUM", Regioncode!="EUR")  %>% 
  mutate(dec=Year%/%2) %>% 
  group_by(Area, Sector, Variable, dec)%>%arrange(Year) %>% 
  summarize(change=dplyr::last(Value)-dplyr::first(Value)) %>% ungroup() %>% filter(change<0.02)

d<-change %>% spread(Sector, change) #%>% filter(AGR<0.1)
GGally::ggpairs(log(1+(d[4:13])), 
                lower = list(
                  continuous = GGally::wrap("smooth", alpha = 0.3, size=0.1)))

change %>% 
  
  
#cor.tests

d1<-cbind(d, d) #

d1<-d1[-c(14:16)]

d1<-d1 %>% gather(Sector1, change1, 14:23) %>% gather(Sector2, change2, 4:13)

corr<-d1 %>% group_by(Sector1, Sector2) %>%nest() 

c1<-corr%>%  
  summarize(pval=map(.x = data,~cor.test(.$change1, .$change2)$p.value)<0.01) %>% unnest(pval)
c1 %>% spread(Sector1, pval) %>% stargazer(summary = F, type="text")

c2<-corr%>%  
  summarize(corr=map(.x = data,~cor.test(.$change1, .$change2)$estimate %>% round(2)) ) %>% unnest(corr)
c2 %>% spread(Sector1, corr) %>% stargazer(summary = F, type="text")

corr<-merge(c1, c2) 
corr%>% filter(pval==T) %>% spread(Sector2, corr)



#
