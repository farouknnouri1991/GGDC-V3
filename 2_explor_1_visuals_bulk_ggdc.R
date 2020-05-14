#__________________________________________________BULK VISUALS: sectoral evolution____________________________________________________________________
                            #1- Sectoral evolution of all variables-GGDC-EASD only
#_______________________________________________________________________________________________________________________________________________________________
rm(list=ls()); 

source( file="GGDC_2_functions.R") 
library(trelliscopejs); library(plotly);  library(tidyverse)#; library(ggthemes);library(GGally)
group_vars<-c("DB","Area","Regioncode", "PPP", "Sector", "Variable")

#_______________________________________________________________________________________________________________________________________________________________


dta<-readRDS("Outputs/dta/dta.RDS") %>% filter(Sector!="SUM") %>% filter(DB=="GGDC", Area!="ZMB")
gdp<-readRDS("Outputs/dta/gdp.RDS") %>% spread(Variable, Value)
attributes(dta)
dtaw<-dta %>% spread(Variable,Value)
unido<- readRDS("Outputs/dta/unido02d.RDS") %>% na.omit() %>% filter(area%in%dta$Area); 


#use this
#1-Sectoral evolutions----

#1-1- Sectorial Evolution-All indicators, by country----

#Time (2D)----
#Trelli 1
dta %>% arrange(Area, Year) %>%  
ggplot() +geom_path(aes(x=Year, y=Value, color=Sector))+theme_bw()+scale_color_brewer(palette = "Dark2")+
facet_trelliscope(~Area+PPP+DB+Variable, as_plotly = T,width=1000, 
                  name="sectors by time (all indicators)", path="Outputs/Sectorial_Evolution")
dta %>% group_by(decade=Year%/%10,Sector, Variable) %>% summarize(value=mean(Value))


#sectoral correlations

    #In search for causality
    d<-dta %>% filter(Variable=="EMP_share", DB=="GGDC", PPP=="LC")%>% filter(Area=="CHN") 
    v<-d %>% group_by(Sector) %>% summarize(v=var(Value)) %>% spread(Sector, v) %>% as.matrix()%>% rep(each=10) %>% 
      matrix(nrow=10, byrow = T)
    d<-spread(d, Sector, Value) [7: 16]
    cc<-cov(d, use="complete.obs")  #%>% ggplotly()

    caus<-cc/v# all this says nothing (its cov on var ratio it has a meaning only if there is a theoretical foundation, keep it in mind)
    cor(d)
    
#sectors pair correlations
d<-dta %>% filter(Variable=="VAQ", DB=="GGDC", PPP=="LC") %>% spread(Sector, Value)
ggpairs(d[7: 16]) #%>% ggplotly()

d<-dta %>% filter(Variable=="VAQ_share", DB=="GGDC", PPP=="LC") %>% spread(Sector, Value)
ggpairs(d[7: 16]) #%>% ggplotly()


d<-dta %>% filter(Variable=="EMP", DB=="GGDC", PPP=="LC") %>% spread(Sector, Value)
ggpairs(d[7: 16]) #%>% ggplotly()


d<-dta %>% filter(Variable=="EMP_share", DB=="GGDC", PPP=="LC") %>% spread(Sector, Value)
ggpairs(d[7: 16]) #%>% ggplotly()

d<-dta %>% filter(Variable=="ptyq", DB=="GGDC", PPP=="LC") %>% spread(Sector, Value)
ggpairs(d[7: 16]) #%>% ggplotly()

#Paths(3D)----

#Employment Share vs Productivityq 

d<-dta %>% filter(Variable%in% c("EMP_share","ptyq", "ptyq_share"),Sector!="SUM") %>% spread(Variable, Value) %>%
  na.omit() %>%  arrange(Year) %>% 
  group_by(Area,PPP,DB, Sector) %>% mutate(x=last(EMP_share), y=last(ptyq), z=last(ptyq_share))# %>% 

#trelli2  
d %>% ggplot()+geom_path(aes(x=EMP_share, y=ptyq, color=Sector, text=Year))+geom_point(aes(x=x, y=y), size=1)+
  
  theme_bw()+scale_color_brewer(palette = "Set3")+scale_x_log10()+scale_y_log10()+
  
  facet_trelliscope(~Area+PPP+DB, as_plotly = T,width=1000,plotly_args = list(toolkit=text), 
                    name="ptyq_vs_EMP_share", path="Outputs/Sectorial_Evolution")


#Employmenst Share vs Productivityq Share:
#trelli3
d %>% 
  
  ggplot()+geom_path(aes(x=EMP_share, y=ptyq, color=Sector, text=Year))+geom_point(x=last(EMP_share, y=last(ptyq)))+
  
  theme_bw()+scale_color_brewer(palette = "Set3")+
  
  facet_trelliscope(~Area+PPP+DB, as_plotly = T,width=1000,plotly_args = list(toolkit=text), 
                    name="ptyq_share_vs_EMP_share", path="Outputs/Sectorial_Evolution")




#dta %>% filter(Variable%in%c("VAQ_share"), Sector%in%c("MAN", "AGR", "FIRE"), Area %in%c("MAR", "CHN", "KOR", "TWN")) %>% 
 # ggplot(aes(Year, Value, color=Sector, linetype=Area ))+geom_line()+theme_economist() +facet_wrap(~Area)






