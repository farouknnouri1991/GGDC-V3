#___________________________________________________5- The case of Morocco: disparity and frontier_______________________________________________________11/mars/2020
rm(list=ls())
library(trelliscopejs); library(tidyverse); library(plotly); library(stargazer); library(ggrepel); library(RColorBrewer)
source("GGDC_2_functions.R", echo=T)
ggplot2::theme_set(ggplot2::theme_bw());mycolors<-colorRampPalette(brewer.pal(8, "Dark2")) (10)

#_______________________________________________________________________________________________________________________________________________________________


dta<-readRDS("Outputs/dta/dta.RDS") %>% filter(DB=="GGDC", PPP=="LC")  ; sectors<-attributes(dta)$sectors; 
#gdp<-readRDS("Outputs/dta/gdp.RDS") %>% filter(Variable=="gdp_pc_ppp", Area%in%dta$Area)
unido<-readRDS("Outputs/dta/unido2d.RDS") #%>% filter(area%in%dta$Area)
unido_dec<-unido %>% group_by(area, decade=year%/%10, variable, isic) %>% summarize(value=mean(value, na.rm=T))



#A-Section A: Morocco alone__________________________________________----
dta<-dta%>% mutate(dec=Year%/%10)

morocco<-filter(dta, Area=="MAR", DB=="GGDC",PPP=="LC", Sector!="SUM") %>% select(-PPP, -DB)

#Morocco, urbanization, agri and argi_ptyq
subset(pop, Area=="MAR") %>% ggplot(aes(x=Year, y=Value, color=Variable))+geom_line()

p<-filter(morocco, Variable%in%c("EMP","ptyq"), Sector=="AGR") %>% ggplot(aes(x=Year, y=Value))+
  geom_line()+facet_wrap(~Variable, scales="free")
ggplotly(p)



#Morocco and the frontier __----
dta<-readRDS("Outputs/dta/dta.RDS") %>% filter(DB=="GGDC")

s1<-filter(dta, Variable=="ptyq" , DB=="GGDC" , PPP=="PPP", !Area%in%c("BRA", "ZMB"), Year%in%c(1990,2010))
p1<-s1 %>% ggplot(aes(Value, fill=Area=="MAR"), alpha=1/3)+geom_histogram()+
  facet_wrap(~Sector+Year, scale="free", ncol=4)

ggplotly(p1)



#Morocco and ptys and wages 
d<-filter(unido_dec, variable%in%c("wageus", "ptyus"))
d$area<-as.character(d$area)
areas<-c("TUN", "MAR", "TUR", "CHN" )
d1<-filter(d, area%in%areas) %>% mutate(r=area)
d<-merge(d, d1, all.x=T)
d$r[is.na(d$r)]<-"oth"

#d<-d %>% mutate(r=ifelse(area%in%areas,areas[which(area==areas)], "others")) #%>%
#  group_by(decade, variable, isic) %>% mutate(n=n()) %>% filter( n>20 ) %>%  ungroup
#there is no continuum of productivity. for each time, each sector there is a bimodal-or more productivity distributions 

 
p1<-d %>% ggplot(aes(value, fill=area=="MAR", group=area))+geom_histogram()+scale_fill_manual(values = c("grey", "black"))+
  facet_trelliscope(~isic+decade+variable, scales="free",
                    as_plotly = T,  path="Outputs/Morocco-unido-frontier", name="ptyus-wages")
p1

p1<-d %>% ggplot(aes(reorder(area, value), value, fill=r, group=area))+geom_bar(stat="identity")+scale_fill_brewer(palette = "Dark2")+
  #geom_point(aes(color=region))+
  facet_trelliscope(~isic+decade+variable, scales="free", as_plotly = T, path="Outputs/Morocco-unido-frontier", name="ptyus-wages-order")
p1

#Does low wages imply high shre of global va?
d<-unido_dec %>%filter(variable%in%c("vaus", "wageus")) %>% spread(variable, value) #
d<-d%>%group_by(decade, isic) %>% 
  mutate( va_share=vaus/sum(vaus, na.rm=T), relative_wage=wageus/median(wageus, na.rm=T))
d%>%filter(isic=="textiles") %>% filter(va_share<0.5) %>% ggplot(aes(log(wageus), log(va_share)))+geom_point()+geom_smooth(method = "lm")


#Pty/on wages no calculate the distance to frotier and then ratio 
d<-d %>% spread(variable, value) %>% mutate(wage_to_pty=wageus/ptyus )
p<-d %>% filter(area=="MAR") %>% ggplot(aes(as.integer(decade), wage_to_pty, color=isic))+geom_line()
ggplotly(p)


#relatively to the median wage/ptyus

d<-filter(unido_dec, variable%in%c("wageus", "ptyus"))
d<-d %>%group_by(decade, isic, variable) %>% arrange((value)) %>% 
  summarize(r=ifelse("MAR"%in%area, value[which(area=="MAR")]/median(value), 0)) %>% 
  filter(r!=0)

p<-d %>%ggplot(aes(decade,r, linetype=variable ))+geom_line() +facet_wrap(~isic, scales="free")
ggplotly(p)

