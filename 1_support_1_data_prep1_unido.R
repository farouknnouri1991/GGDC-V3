#______________________________________________________________1.2data prep UNIDO_____________________________________________________


#________________________________________________________________________________________________________________________________________
library(tidyverse); library(foreign)

rm(list=ls())

unido<-read.dta("Inputs/2-Unido/INDSTAT2- ISIC Rev.3- All countries- All years.dta")
attributes(unido)
#unido<-to_labelled(unido)


#1- Tydying data----
  vars_unido<-data.frame(variable=names(unido),label=attributes(unido)$var.labels,stringsAsFactors = FALSE)

#selecting variables
  names(unido)
  var1<-c(1:4); var2<- c(seq(5, 13, 2), 39); var3lc<- c(15, 18,27,36 ); var3us<-var3lc+1
  
  vars<-vars_unido[c(var2,var3lc, var3us),"variable"]
  
  unido<-unido[,c(names(unido)[var1], vars) ]%>% gather(variable,value, 5:18) %>% filter(!is.na(value)) 


#Uniqueness in contry, year, Isic, variable (remove non unique data-Netherlands)
  #unido %>% group_by(Country, Year, Isic, variable) %>% mutate(n=n()) %>% filter(n!=1)%>% ungroup() %>% select(Country)  %>% unique()
  
  unido<-unido %>%  group_by(Country, Year, Isic, variable) %>% mutate(n=n()) %>% filter(n==1)%>% ungroup() 
  unido<-unido %>% select(-n)
  
      #unido %>%group_by(Country, Year, Isic, variable) %>% mutate(n=n()) %>% filter(n!=1) %>% nrow
  
  rm( attrib, var1, var2, var3lc, var3us,vars)#keep vars_unido


#renaming variables

  vars_unido<-filter(vars_unido, variable%in% unique(unido$variable))
  
  vars_unido<-data.frame(short=c("nbest", "nbfirm", "nbppl", "emp", "empf", "wageslc", "wagesus", "outputlc", "outputus",
    "valc","vaus", "capitallc", "cpitalus", "indexip"),vars_unido)#good
  
  unido<-merge(unido, vars_unido) %>% select(-variable, -label) %>% rename(variable=short)
  names(unido)<-c("area", "year", "isic", "isiccomb", "value","variable")

  
#change isic names
  unido$isic<-as.character(unido$isic); unido$area<-as.character(unido$area)
  short<-data.frame(isic=sort(unique(unido$isic)),
                   short=c("sum","metal.basic","chemicals_","fossil","elec_mach","fab.metal", "food_","furniture", 
                           "leather_", "machin.eq","med.op.inst","motor.vehicles",
                           "nonmetal.min","office.info.machin","trans.equip.oth", "paper_", "print","radio.tel.eq","recycle","plastic", 
                           "textiles","tabacco","wearing", "wood"))
  unido<-merge(unido,short) %>% select(-isic)#%>%rename(short=isic) 
  unido<-unido %>% rename(isic=short)
  unido<-select(unido, -isiccomb)
  
  
#Adding groupings
  
  short<- read.csv2("Outputs/dta/Isic_groups_final.csv") %>% select(-long)

  unido0<-unido %>% merge(short) %>% select(-isic) %>% rename(isic=group) 
  
  unido0<-unido0 %>% group_by( area, year, isic,  variable) %>% summarize(value=sum(value))
  
  

  
#2- Calculating pty and shares----  
#calculating productivities, average wages, etc. 
 
  
  unido<-unido %>%spread(variable, value) %>%
    mutate(wagelc=wageslc/emp, wageus=wagesus/emp, labour_share=wageslc/valc,
           ptylc=valc/emp, ptyus=vaus/emp, 
           ptylc_out=outputlc/emp, ptyus_out=outputus/emp)   
  #%>% dont bring the line of gather it will be false
  
  unido<-unido %>%  gather(variable, value, 4:ncol(unido))%>%  filter(!is.na(value)) 
##
  unido0<-unido0 %>%spread(variable, value) %>%
    mutate(wagelc=wageslc/emp, wageus=wagesus/emp, labour_share=wageslc/valc,
           ptylc=valc/emp, ptyus=vaus/emp, 
           ptylc_out=outputlc/emp, ptyus_out=outputus/emp)   

  unido0<-unido0 %>%  gather(variable, value, 4:ncol(unido0))%>%  filter(!is.na(value)) 
  

#-calculating shares in (emp, va, pty)
  
  sectors<-unique(unido$isic[unido$isic!="sum"]) %>% as.character(); id<-c("area", "year", "variable")
  
  unido_shares<-unido  %>% spread(isic, value)%>% select(id, c(sectors, "sum")) %>% #il faut remettre total à la fin pour ne pas le modifier avant de tout diviser par lui
      mutate_at(vars(4:27), ~./sum) %>% 
      gather(isic, value, 4:27) %>% mutate(variable=paste0(variable,"_share")) %>% 
      filter(!is.na(value), !is.nan(value)) 
  #control reussi: que la somme des parts =1
  
  unido<-rbind(unido,unido_shares)#use shares for decomposition of growth equations 1 & 2 in Rodrik framework
##
  sectors<-unique(unido0$isic[unido0$isic!="sum"]) %>% as.character(); id<-c("area", "year", "variable")
  
  unido0_shares<-unido0  %>% spread(isic, value)%>% select(id, c(sectors, "sum")) %>% #il faut remettre total à la fin pour ne pas le modifier avant de tout diviser par lui
      mutate_at(vars(4:11), ~./sum) %>% 
      gather(isic, value, 4:11) %>% mutate(variable=paste0(variable,"_share")) %>% 
      filter(!is.na(value), !is.nan(value)) 
  #control reussi: que la somme des parts =1
  
  unido0<-rbind(unido0,unido0_shares)#use shares for decomposi
  
#SAVE: but keep only countries in GGDC
  codes<-read.csv2("Inputs/codes_fao.csv") %>% select(Short.name, ISO3)
  unido<-merge(codes, unido, by.x="Short.name", by.y="area") %>% select(-Short.name) %>% rename(area=ISO3) #only few losses
  unido0<-merge(codes, unido0, by.x="Short.name", by.y="area") %>% select(-Short.name) %>% rename(area=ISO3) #only few losses
   #unido2<-merge(codes, unido2, by.x="Official.name", by.y="area", all.y=T)
  
  
  attributes(unido)$isic.names<-short
  attributes(unido)$vars.names<-vars_unido
  unido %>% saveRDS("Outputs/dta/unido2d.RDS")

 attributes(unido0)$isic.names<-short
  attributes(unido0)$vars.names<-vars_unido
  unido0 %>% saveRDS("Outputs/dta/unido02d.RDS")

#_____________________________________________________________THE END_________________________________________________________________________________________________________

