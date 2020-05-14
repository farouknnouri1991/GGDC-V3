#__________________________________________________BULK VISUALS____________________________________________________________________
                                       #2- WITHIN -BETWEEN DECOMPOSITION
#_______________________________________________________________________________________________________________________________________________________________

rm(list=ls()); 
source( file="GGDC_2_functions.R") 
library(trelliscopejs); library(plotly);  library(tidyverse); library(RColorBrewer)#; library(ggthemes);library(GGally)
group_vars<-c("DB","Area","Regioncode", "PPP", "Sector", "Variable"); c<-c("within_ptyq","between_ptyq", "D_ptyq")
#_______________________________________________________________________________________________________________________________________________________________

dta<-readRDS("Outputs/dta/dta.RDS") %>% filter(Sector!="SUM") %>% filter(DB=="GGDC", Area!="ZMB")

mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(10)


#2- Sectoral and total within and btwn contributions-----

#2-1- Contribution of within and between components to total economy:----

decomp_tot_dec<-decomp_n_f(y0=1960, yn=2011, by=2)%>% filter(variable%in%c, Sector=="SUM" ) #%>% 
  #spread(variable, value) %>% 
  #mutate_at(vars("within_ptyq","between_ptyq"), ~./D_ptyq) %>% gather(variable, value, 7:9)



#trelli4
decomp_tot_dec %>% filter(variable!="D_ptyq", Area!="ZMB") %>%
  ggplot() + geom_line(aes(y=value,x=decade, fill=variable), stat="identity")+
  scale_fill_brewer(palette="Dark2")+theme_bw()+
  facet_trelliscope(~Area+PPP+DB, scales="free", width=1000, as_plotly=T,name="totoal economy", path="Outputs/decomposition")


decomp_tot<-decomp_f(first=1990, last =2011, data=dta, group_vars)%>% filter(Variable%in%c, Sector=="SUM" ) #%>% 

decomp_tot %>% filter( !Area%in%c("ZMB", "VEN")) %>%
  ggplot() + geom_bar(aes(y=Value,x=reorder(Area,Value), fill=Area=="MAR"), stat="identity")+facet_trelliscope(~Variable+PPP, as_plotly = T, scales="free")+scale_fill_manual(values=c("grey", "black"))


#________________________________________________________________________________________________________________________________________

#2-2- By sectors-----
  
decomp_sect_dec<-decomp_n_f(y0=1960, yn=2012, by=10) %>% spread(variable, value) %>% filter(Sector!="SUM")



#trelli5
f<-decomp_sect_dec %>% gather(Variable,Value,within_ptyq,between_ptyq)
 

f %>% ggplot()+theme_bw()+ scale_fill_manual(values = mycolors)+
  geom_bar(aes(y=Value,x=decade,  fill=Sector), stat="identity", position = "dodge")+guides(alpha=F)+
  
  facet_trelliscope(~Area+PPP+DB+Variable, scales="free", width=1000, as_plotly=T,name="sectors-absol contrib", path="Outputs/decomposition")


#Appendix----
#Appendix- to within and betrween contribution: Changes in labour shares-----

#trelli6
decomp_sect_dec %>% 
  ggplot()+theme_bw() + geom_bar(aes(y=EMP_share_change,x=decade,fill=Sector), stat="identity", position = "dodge")+
  scale_fill_manual(values = mycolors)+
  facet_trelliscope(~Area+PPP+DB, scales="free", width=1000, as_plotly=T,name="emp_share changes", path="Outputs/decomposition")
  







#_____________________________________END___________________________________________________________________________________________
