#___________________________________________________BULK tables_________________________________________________________

                            #1- Sectoral evolution of all variables
#_______________________________________________________________________________________________________________________________________________________________
rm(list=ls()); 

source( file="GGDC_2_functions.R") 
library(trelliscopejs); library(plotly);  library(tidyverse)#; library(ggthemes);library(GGally)
group_vars<-c("DB","Area","Regioncode", "PPP", "Sector", "Variable")


#_______________________________________________________________________________________________________________________________________________________________


dta<-readRDS("Outputs/dta/dta.RDS") %>% filter(Sector!="SUM") %>% filter(DB=="GGDC", Area!="ZMB")
#gdp<-readRDS("Outputs/dta/gdp.RDS") %>% spread(Variable, Value)
#attributes(dta)
#dtaw<-dta %>% spread(Variable,Value)
unido<- readRDS("Outputs/dta/unido02d.RDS") %>% na.omit() %>% filter(area%in%dta$Area); 


#use this
#1-Sectoral evolutions----

#1-1- Sectorial Evolution-All indicators, by country----

#Time (2D)----
#Trelli 1
tab_evol<-dta %>% group_by(Area,PPP,DB,decade=Year%/%10,Sector, Variable) %>% summarize(value=mean(Value))




#2- Sectoral and total within and btwn contributions-----
c<-c("within_ptyq","between_ptyq", "ptyq_change", "D_ptyq")

#2-1- Contribution of within and between components to total economy:----

decomp_tot_dec<-decomp_n_f(y0=1960, yn=2013, by=10)%>% filter(variable%in%c, Sector=="SUM" ) %>% spread(variable, value) %>% 
  mutate_at(vars("within_ptyq","between_ptyq"), ~./D_ptyq) %>% gather(variable, value, 7:9)




#________________________________________________________________________________________________________________________________________

#2-2- By sectors-----
  
#The data____________________________________

decomp_sect_dec<-decomp_sect_dec %>% filter(Area=="MAR", PPP=="LC") %>% 
  filter (Sector%in%c("SUM"), variable%in% c("between_ptyq","within_ptyq"))
decomp_sect_dec



## 2 year decomp


decomp_sect_2<- decomp_n_f(1960, 2013, by=2)%>% filter(Area=="MAR", PPP=="LC") %>% filter (Sector%in%c("SUM")) 

p<-decomp_sect_2 %>% 
  ggplot(aes(x=decade))+geom_line(aes(y=value , linetype=variable))

ggplotly(p)  


#__________________________________________________________________________________________________________________________________

#2-3-Changes in emp_share and in pty_shares (1990-2010)----

dat<-dta_op_f(dta, first=1990, last=2010, group_vars) %>%
  filter( Variable %in% c("EMP_share","ptyq_share"), PPP=="PPP",operation=="change" ) %>%  spread(Variable, Value) 
  

df<-dta %>% filter( Variable%in% c("EMP_share", "ptyq_share"), Area=="MAR", PPP=="LC") #%>% ggplot()
df %>%filter(Sector!="SUM") %>%  spread(Variable, Value) %>%  
ggplot()+geom_point(aes(ptyq_share, EMP_share, color=Sector))+scale_y_log10()+scale_x_log10()



#LAM
decomp_f(first=2000, last=2009, group_vars, data=dta)  %>% 
  filter(Regioncode=="LAM",PPP=="PPP", Sector=="SUM",
              Variable %in% c("within_ptyq","between_ptyq","ptyq_change")) %>%
spread(Variable, Value) %>% select(-Sector, -Regioncode)# %>% tab_df()



#3-Unido- sectoral evol:----
areas<-read.csv2("Outputs/dta/areas.csv")
unido<- readRDS("Outputs/dta/unido02d.RDS") %>% na.omit() %>% filter(area%in%areas$Area); 
unido %>% arrange(year) %>%
  filter(variable%in%c("emp","capitallc" ,"valc" ,"emp_share", "valc_share", 
                       "wagelc", "wagelc_share", "ptylc", "ptylc_share"), isic!="sum") %>%   
  ggplot(aes(year, value, color=isic, label=isic, group=isic))+geom_line(size=0.9)+
  geom_text_repel(data=subset(c, year==2010),
                             nudge_x = 0.1,nudge_y = 0, segment.color = NA)+guides(color=FALSE)+theme_wsj()+

  facet_trelliscope(~area+variable, as_plotly = T, width = 1000, path="Outputs/unido", name="sect.evol")







