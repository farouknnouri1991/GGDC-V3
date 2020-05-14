#_______________________________________6-Morocco in its clusters___________________________________________ 

#_______________________________________________________________________________________________________________________________________________________________
rm(list=ls())
library(trelliscopejs); library(tidyverse); library(plotly); library(stargazer); library(ggrepel)
source("GGDC_2_functions.R")
ggplot2::theme_set(ggplot2::theme_bw()) 

source("GGDC_4_clusters.R")
#_______________________________________________________________________________________________________________________________________________________________


dta<-readRDS("Outputs/dta/dta.RDS") %>% filter(DB=="GGDC", PPP=="LC")  ; sectors<-attributes(dta)$sectors; 
gdp<-readRDS("Outputs/dta/gdp.RDS") %>% filter(Variable=="gdp_pc_ppp", Area%in%dta$Area)
pop<-read.csv2("Inputs/wp1/dta.agri_wp1.csv", stringsAsFactors = F) %>% filter(Variable%in%c("population", "urbans", "rurals")) %>% mutate(Value=as.numeric(Value)) %>% na.omit()
#pop<-pop %>% spread(Variable, Value)
unido<-readRDS("Outputs/dta/unido02d.RDS") %>% filter(area%in%dta$Area)
sectors<-attributes(dta)$sectors

#Morocco in its clusters_____________________________________________________________________________________________________________----

#clusters are not stable: each time they can give you one clusterig which one to choose
    for(i in 1:4){
    source("GGDC_4_clusters.R")
    print(clusters%>% filter(Area=="MAR", decade==199, cl.var=="EMP_share") %>% select(-Area) %>% merge( clusters, all.x=T))
    }
    
    for(i in 1:4){
    source("GGDC_4_clusters.R")
    print(clusters%>% filter(Area=="MAR", decade==199, cl.var=="VA_share") %>% select(-Area) %>% merge( clusters, all.x=T))
    }


    #d<-clusters %>% group_by(cl.var, decade, cl) %>% summarize(z=n()/34*100) %>% spread(decade, z)


#___________________Application: 
                    #Sectoral composition by group for each custering variable__________
                    agg<-merge(agg, clusters_long)
                    
                    emp<-filter(agg,  Variable=="EMP_share", !is.na(cl), cl.var=="EMP_share" )
                    va<-filter(agg, Variable=="VA_share", !is.na(cl), cl.var=="EMP_share" )
                    
                    
                    #Structural evolution
                      #EMP structure for EMP_clusters
                      bars<-emp %>%ggplot(aes(x=paste(Area, dec), y=Value, fill=Sector, label=Area) )+
                        geom_bar(stat="identity", position="stack")+
                      scale_fill_brewer(palette="Dark2")+
                        theme(axis.text.x=element_text(angle = -90, hjust = 0))+#scale_y_log10()+
                        trelliscopejs::facet_trelliscope(~cl, scales="free", width=1000) 
                      #bars
                      #ggplotly(bars)
                    
                      #VA structure for emp_clusters
                      bars<-va %>%ggplot(aes(x=paste(Area, dec), y=Value, fill=Sector, label=Area) )+
                        geom_bar(stat="identity", position="stack")+
                      scale_fill_brewer(palette="Dark2")+
                        theme(axis.text.x=element_text(angle = -90, hjust = 0))+#scale_y_log10()+
                        trelliscopejs::facet_trelliscope(~cl, scales="free", width=1000) 
                      #bars
                      #ggplotly(bars)
                      
                      rm(va, emp, bars)
                      
#_________
                      
#_______________Application
                #
                #Maps of clusters
                library(sf); library(rworldmap)
                world<- getMap(resolution = "li")
                #transform to simple feature
                world<-st_as_sf(world)
                world$ISO3<-as.character(world$ISO3)
                world<-world %>% dplyr::select(ISO3, GEO3major, GEO3, LON, LAT)%>%  inner_join(clusters_dec, by=c(ISO3="Area"))
                  
                p<-world %>% ggplot()+geom_sf(aes(fill=as.factor(cl)))+theme_bw()+facet_wrap(~decade+cl.var, ncol=2)

                rm(world, p)


#1- list comparables
  #morocco clusters
  mor.cl<-filter(clusters, Area=="MAR") %>% select(-Area)
  
  #keep all countries that matches with the (cl.var, decade, cluster) trio: 
  #those belonging to the moroccan cluster, based on clustering variabe and in a given decade
  
  mor.comp<-merge(mor.cl, clusters, all.x=T)
  #mor.comp<-morocco.cluster %>%group_by(cl.var,decade, cl) %>%  nest()

#2- Studying morocco in/since the 90's
  m90<-filter(mor.comp, decade==199, cl.var=="gdp_pcp") %>% select(Area)
  gdp90<-filter(gdp, Area%in% m90$Area)  
  p<-ggplot(gdp90)+geom_line(aes(x=Year, y=Value, color=Area))
  ggplotly(p)#(1)
  
  gdp90 %>% dta_op_f(group="Area", 1990, 2015) %>% filter(operation=="growth") %>% arrange(desc(Value))#(2)
  
  #from (1) and (2), the countries comparable to morocco structurally and that has done better in terms of growth are
  #China groing 7 times, India 2 times, Thailand and Indonesia 1.3 times, while morocco 0.8 times
  
  #go to decomposition...to understand the how of growth and 
  #Why some have done better(see also institutional arrangeents in those countries)
  
#studying The world since the 90 
  cl<-clusters %>% filter(decade==199, cl.var=="gdp_pcp")
  gdp90<-merge(cl, gdp, all.x=T)  
  
  p<-ggplot(gdp90)+geom_line(aes(x=Year, y=Value, color=Area))+facet_wrap(~cl, scales="free")
  p
  ggplotly(p)#(1)
  