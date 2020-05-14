#___________________________________________________5- The case of Morocco:1-GGDC_______________________________________________________11/mars/2020
                            
#_______________________________________________________________________________________________________________________________________________________________
rm(list=ls())
library(trelliscopejs); library(tidyverse); library(plotly); library(stargazer); library(ggrepel); library(RColorBrewer)
source("1_support_2_functions.R", echo=T)
ggplot2::theme_set(ggplot2::theme_bw());mycolors<-colorRampPalette(brewer.pal(8, "Dark2")) (10)

#_______________________________________________________________________________________________________________________________________________________________


dta<-readRDS("Outputs/dta/dta.RDS") %>% filter(DB=="GGDC", PPP=="LC")  ; sectors<-attributes(dta)$sectors; 
unido<-readRDS("Outputs/dta/unido2d.RDS") #%>% filter(area%in%dta$Area)
unido_dec<-unido %>% group_by(area, decade=year%/%10, variable, isic) %>% summarize(value=mean(value, na.rm=T))
morocco<-filter(dta, Area=="MAR", DB=="GGDC",PPP=="LC", Sector!="SUM") %>% select(-PPP, -DB)



#1-Structural change in Morocco -----


#1-1- The whole picture: va, emp and ptyq
#1-1-1- evolution of VAQ and Emp
d<-dta %>% filter(Area=="MAR", Variable%in% c("EMP_share", "VAQ_share"), Sector!="SUM")
bars<-d %>%  ggplot(aes(x=Year, y=Value, fill=Sector, label=Area) )+
  geom_bar(stat="identity", position="stack")+
  scale_fill_brewer(palette="Set3")+theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  facet_wrap("Variable", nrow=2)
ggplotly(bars)

    p<-morocco %>% filter(Variable %in% c("VAQ_share","EMP_share"), Sector!="OTH") %>%  
      ggplot(aes(x=Year, y=100*Value, linetype= Variable))+
      theme_bw()+guides()+
      geom_line()+facet_wrap(~Sector)
    
    ggplotly(p)

    #reark the two plots above say diffrent things, one is about comparing contries shares between, 
    #and the second is giving more attention to the sectors themselves 
    
#1-1-2- Evolution of productivities
p<-morocco %>% filter(Variable %in% c("ptyq"), Sector!="OTH") %>%  
  ggplot(aes(x=Year, y=Value, linetype= Variable))+
  theme_bw()+guides()+
  geom_line()+facet_wrap(~Sector, scales="free")

ggplotly(p)

#changes in the wholee period
      change2<-dta_op_f(dta, group = group_vars, first = 1960, last=2009) %>% filter(operation=="change")
      change2 %>% filter(Area=="MAR", Variable=="VAQ_share") %>% arrange(desc(Value))
      
      #Asia is the leading in idutrialisation
      change_sum<-change%>% group_by(Regioncode, Sector, Variable) %>% summarize(change=mean(change, na.rm = T))
      
      change_sum %>% filter(Variable=="VAQ_share", Sector=="MAN")
      
      change %>% filter(Variable=="ptyq", Sector=="MAN")
      
      


#Measuring the magnitude of structural change

#in the whole period  
change<-dta %>% arrange(Year) %>% 
  group_by(Area, Sector, Variable) %>% 
  summarize(change=dplyr::last(Value)-dplyr::first(Value)) %>% ungroup() #%>% filter(Year>1960)

total_change<-change %>% group_by(Area, Variable) %>% 
  summarise(total_change=sum(abs(change))/2 ) %>% ungroup() %>% filter(Variable %in% c("VAQ_share", "EMP_share"))


comp<-filter(total_change, Area%in% c('MAR', 'CHN', 'THA', "ZAF", 'TUN', 'BRA', 'CHL', "EGY"))%>% filter(Variable=="VAQ_share") 
p<-total_change %>% filter(Variable=="VAQ_share") %>% ggplot( )+
  geom_bar(aes(x=reorder(Area, total_change), y=total_change),   
           alpha=0.1 , stat = "identity"
           )+
   geom_bar( aes(x=reorder(Area, total_change), y=total_change,fill=Area),
             stat = "identity", data=comp) 
ggplotly(p)

#China has displaced 48% of its workforce (morocco 38%, kor55%) and 72% of its value added(Morocco 24% 56%)
    
    #annual changes
    change<-dta %>% mutate(r= Year%/%2) %>% 
      group_by(Area, Sector, Variable,r)%>%arrange(r) %>% 
      summarize(change=dplyr::last(Value)-dplyr::first(Value)) %>% ungroup() #%>% filter(Year>1960)
    
    speed<-change %>% group_by(r, Area, Variable) %>% 
      summarise(speed=sum(abs(change))/2 ) %>% filter(Variable=="EMP_share") %>% ungroup()
      
    speed2<-speed %>% group_by( Area, Variable) %>% 
      summarise(speed=sum(abs(speed))) %>%
      ungroup() #%>% spread(dec, speed)
    
    comp<-filter(speed, Area%in% c('MAR', 'CHN', 'THA', "ZAF", 'TUN', 'BRA', 'CHL', "EGY"))
    p<-speed %>% ggplot(aes(x=r*2, y=speed,  group=Area)  )+geom_line(alpha=0.1)+
      geom_line( aes(color=Area), comp) 
    ggplotly(p)

#the  role of agri----
dta<-readRDS("Outputs/dta/dta.RDS") %>% filter(DB=="GGDC", PPP=="LC")  

d<-dta %>% filter(Variable%in%c("ptyq", "EMP_share"), Sector=="AGR", PPP=="LC") %>% spread(Variable, Value)%>% na.omit()
p<-d%>% arrange(Year)  %>% 
  ggplot(aes(ptyq, EMP_share, group=Area, text=Year))+geom_path()+facet_wrap(~Area, scales="free")+
  geom_point(data = filter(d, Year==2010), color="Red")
ggplotly(p, plotly_arg=list(tools="text") )

a<-smooth.spline(d[c('EMP_share', 'ptyq')])
d %>% group_by(Area) %>% mutate(map(p= smooth.spline(.data[c('EMP_share', 'ptyq')])))

#the very high variance in productivity growth
var_ag<-d %>% group_by(Area, Year%/%2) %>% mutate(growth=(last(ptyq)-first(ptyq))/first(ptyq)) %>% 
  group_by(Area) %>% summarize(var=var(growth)/abs(mean(growth)), negative=sum(growth<0)) %>% 
  arrange(-negative) %>% print.data.frame()
#morocco variance 0.8; CHN 0.0 , KOR 0.05, EGY 0.07
#morocco periods of negative growth: Morocco the highest 28, kor 6, egypt 8, Malayia 6, CHN 16


#Structural evolution the whole picture----
library(RColorBrewer)

#getPalette = colorRampPalette(brewer.pal(8, "Set2"))



#2-The composition fallacy -----
#see cluster analysis==>morocco is with mex bra cri egy twn 
#however, morocco is poorer than all of them 

gdp<-spread(gdp, Variable, Value)
gdp %>% filter(Area%in% c("MAR", "CRI", "EGY", "BRA", "TWN", "MUS", "MYS", "ZAF")) %>%
  ggplot(aes(Year, gdp_pc_ppp, color=Area ))+geom_line(size=1)+scale_color_manual(values=mycolors)



      
cat("Must see in trellis, and in clusters")


#3-Premature deindutrialization----

dtaw<-dta %>% filter(PPP=="LC") %>% spread(Variable,Value)%>% filter(Sector=="MAN")
    
dtaw<-gdp %>% merge(dtaw, all.y=T) %>% group_by(Area) %>% mutate(h=first(gdp_pc_ppp),hh=first(VAQ_share) ) %>% ungroup
p<-dtaw %>%  filter(PPP=="LC") %>%  
  arrange(Year) %>% #filter(region=="Europe") %>% 
  ggplot()+geom_path(aes(gdp_pc_ppp ,VAQ_share,color=Regioncode, group=Area ),alpha=1/2, size=0.7)#+
  #geom_text_repel(data=subset(dtaw, Year==max(Year)))
  #geom_smooth(aes(gdp_pc_ppp ,VAQ_share,  color=region), se=F, size=1/3)
ggplotly(p)

p<-dtaw  %>% 
  ggplot(aes(Year ,VAQ_share,color=Area))+##geom_line( )+
  geom_smooth( method='lm', formula = y~poly(x,2), se=F)+
  scale_fill_brewer(palette="Set3")+facet_wrap(~Regioncode, scales="free")


ggplotly(p)
dta %>% group_by(Regioncode, summarize)


#emp_sh_time

#vaq_sh emp_sh: detect a change in iemp_sh trends for levels of vaq_share
p<-dtaw  %>% #filter(region=="Europe") %>% 
  ggplot()+geom_path(aes(EMP_share ,VAQ_share,color=Regioncode, group=Area ),alpha=1/2, size=0.7)+
  geom_text(aes(x=h,y=hh, label=Area), size=3, alpha=1/3)#+
  #geom_smooth(aes(gdp_pc_ppp ,VAQ_share,  color=region), se=F, size=1/3)
ggplotly(p)

#4-Relative/abs Productiviti(es) and absorpion capacity----
#ptyq_share of manufacturing evolution 

d<-dta %>% filter(Variable=="ptyq_share", PPP=="LC", DB=="GGDC", Sector=="MAN")
d%>% ggplot(aes(x=Year, y=Value, group=Area, label=Area))+geom_line(size=0.8) +
  geom_text_repel(data=subset(d, Year==2000), force=3)+guides(color=F)+
  facet_wrap(~Area, scales="free")+scale_fill_manual(values=mycolors)

#ptyq_share of fire evolution

d<-dta %>% filter(Variable=="ptyq_share", PPP=="LC", DB=="GGDC", Sector=="FIRE")
p<-d%>% ggplot(aes(x=Year, y=Value, group=Area, label=Area ))+
  geom_line(size=0.8, aes( color=(Area=="MAR"))) +scale_color_brewer(palette = "Dark2")
ggplotly(p)#we are only surpassed by SSA Tanzaania, senegal, malawi... 

p<-d%>% ggplot(aes(x=Year, y=Value, group=Area, label=Area ))+
  geom_line(size=0.8) +facet_wrap(~Area, scales="free")
ggplotly(p)

#ptyq of fire evolution
dta<-readRDS("Outputs/dta/dta.RDS") %>% filter(DB=="GGDC", !Area%in%c("ZMB", "BRA"))

d<-dta %>% filter(Variable=="ptyq", PPP=="PPP", DB=="GGDC", Sector=="FIRE")
p<-d%>% ggplot(aes(x=Year, y=Value, group=Area, label=Area ))+
  geom_line(size=0.8, aes( alpha=(Area=="MAR"))) +scale_color_brewer(palette = "Dark2")
ggplotly(p)#we are only surpassed by SSA Tanzaania, senegal, malawi... 

d<-dta %>% filter(Variable=="ptyq", PPP=="LC", DB=="GGDC", Sector=="FIRE")
p<-d%>% ggplot(aes(x=Year, y=Value, group=Area, label=Area ))+
  geom_line(size=0.8) +facet_wrap(~Area, scales="free")
ggplotly(p)

d<-decomp_f(data=dta, first=1990, last=2010, group_vars) %>% 
  filter(Variable=="ptyq_change", Sector%in%c("FIRE", "MAN"), PPP=="PPP") %>% spread(Sector, Value)

p<-dta%>% filter(Variable=="ptyq", Sector%in%c("FIRE", "MAN"), PPP=="PPP") %>% spread(Sector, Value) %>%
  arrange(Year) %>% 
  ggplot(aes(x=FIRE, y=MAN, shape=Regioncode))+
  geom_point(size=3, alpha=0.5)

ggplotly(p)

#The most productive sectors around the world: is what is happening in morocco normal?
dta<-dta%>% mutate(dec=Year%/%10)
d<-dta %>% filter(Variable=="ptyq_share", PPP=="LC") %>% 
  group_by(Regioncode, dec, Sector) %>% summarize(Value=mean(Value))
 
p<-d %>% ggplot(aes(x=dec, y=Value, color=Sector))+geom_line(size=0.2)+scale_color_manual(values=mycolors)+
  facet_wrap(~Regioncode, scales="free")

ggplotly(p)


      #Is morocco abberant-ptyq_share (no need, see graphs)
      dta$Regioncode[dta$Area=="MAR"]<-"MAR"
      d<-dta %>% filter(Variable=="ptyq_share", PPP=="LC") %>% 
        group_by(Regioncode, dec, Sector) %>% summarize(Value=mean(Value))
       
      d%>% filter(Sector%in%c("MAN")) %>% mutate(Value=round(Value, 2)) %>% spread(dec, Value) %>% 
       #filter(Sector=="MAN") %>% 
        stargazer(summary = F, type = "text")




#can these sectors absorb pop (or see cluster bars): Yes FIRE can
d<-dta %>% filter(Variable=="EMP_share", PPP=="LC") %>%  mutate(dec=Year%/%10) %>% 
  group_by(Regioncode, dec, Sector) %>% summarize(Value=mean(Value))
 
d%>% filter(Sector%in%c("FIRE", "PU", "MIN")) %>% group_by(Regioncode,  dec) %>% summarize(Value=sum(Value)) %>% 
  mutate(Value=round(Value, 2)) %>% spread(dec, Value) %>% 
  stargazer(summary = F, type = "text")
#since public utilities,mining and transport sector cannot absorbmuch of employment as we can see in the majority of contries


