#___________________________________________________Structural clusters________________________________________________________11/mars/2020----
                            #1- Long run clusters of countries based on sectoral share of labour, value added, etc...
                            #2- Short run (decade) clusters based on the sectoral shares of the same variables
                            #3- Studying the clusters stemming from different criteria employment share, va shares, pty, etc...
                            #4- Maybe we have too many criteria, should we focus on the most informative ones (ACP). (theory and history gives us reasons to focus on mafacturing)
                            #5- 
 

#_______________________________________________________________________________________________________________________________________________________________

rm(list=ls()); 
dta<-readRDS("Outputs/dta/dta.RDS"); gdp<-readRDS("Outputs/dta/gdp.RDS")%>% filter(Variable=="gdp_pc_ppp", Area%in%dta$Area)
#library(GGally); 
library(plotly); library(tidyverse)#DONT WORRY PTY SHARES, as other shares ARE IN %
library(trelliscopejs); library(ggrepel)

#_______________________________________________________________________________________________________________________________________________________________

dta<-filter(dta, PPP=="LC"& DB=="GGDC" & !Sector%in%c("SUM", "OTH"), Year>=1970)%>% mutate(dec=Year%/%10)

#1- Clusters_______________________________________________________________________________________________________________________----

#1- Clustering for long periods ----

a<-c("EMP_share", "VA_share", "ptyq")

agg<- dta%>% filter(Variable%in%a)  %>% group_by(Area, dec,Variable,Sector) %>% summarize(Value=mean(Value)) 


agg_w<-agg %>% mutate(z=paste0( Sector, dec)) %>% ungroup() %>% select(-dec, -Sector) %>% spread(z, Value) %>% na.omit#Turning all information into colums

cl7010<-as.list(a)

for (i in 1:length(cl7010)){
d<-agg_w %>% filter(Variable==a[i])
km7010<-kmeans(d[-c(1,2)], 4);     
cl7010[[i]]<-data.frame(d[c(1,2)], cl=km7010$cluster); rm(km7010)
}

clusters_long<-cl7010 %>% reduce(rbind) %>% rename(cl.var=Variable);                       rm(agg_w, a, d, i, cl7010)
clusters_long$decade<-7010


#2- Clustering for short periods: each decade----

a<-c("EMP_share", "VA_share", "ptyq_share")
agg<- dta%>% filter(Variable%in%a)  %>% group_by(Area, dec,Variable,Sector) %>% summarize(Value=mean(Value)) 

decade<-197:201

#______
cl2<-as.list(a)
  
for (j in 1:length(a)) {
  
  Data<-agg %>% filter(Variable==a[j])
  
  cl<-as.list(decade)
  for (i in 1:length(cl)) {
  d<-filter(Data, dec==decade[i]) %>%spread(Sector, Value) %>%  na.omit()
  
  km<-kmeans(d[, -c(1:3)],4)
  
  clusters<-km$cluster
  #centers<-data.frame(cl=1:4, km$centers) %>% gather(Variable, Value, 2:10) #%>% mutate(Variable=str_trunc(Variable, 3, "left"))
  
  cl_dec<-data.frame(d[1], cl=clusters) #%>% merge(centers)
  
  cl_dec$decade<-decade[i]
  
  cl[[i]]<-cl_dec
  }
  
  cl<-cl %>% reduce(rbind) %>% mutate(cl.var=a[j])
  cl2[[j]]<-cl
  
}

clusters_dec<-cl2 %>% reduce(rbind)       ;    rm(cl2, cl, cl_dec, d, Data, km, a, decade, clusters, i, j)
#_______________________________________________________________________________________________________________




#3- GDP clusters------

gdp_agg<-gdp %>%mutate(decade=Year%/%10) %>%  group_by(Area, Variable, decade) %>% summarize(Value=mean(Value)) %>% ungroup
decade<-197:201 %>% as.numeric
cl<-as.list(decade)

 for (i in 1:length(cl)) {
  d<-filter(gdp_agg, decade==decade[i]) 
  
  km<-kmeans(d[, -c(1:3)],4)
  
  clusters<-km$cluster
  #centers<-data.frame(cl=1:4, km$centers) %>% gather(Variable, Value, 2:10) #%>% mutate(Variable=str_trunc(Variable, 3, "left"))
  
  cl_dec<-data.frame(d[1], cl=clusters) #%>% merge(centers)
  
  cl_dec$decade<-decade[i]
  
  cl[[i]]<-cl_dec
  }
  
  clusters_dec_gdp<-cl %>% reduce(rbind) %>% mutate(cl.var="gdp_pcp") 
rm(d, gdp, gdp_agg, cl, cl_dec, km)  
#SAVE in one DB
clusters<-rbind(clusters_dec, clusters_long, clusters_dec_gdp) %>% select(cl.var,decade, cl, Area); rm(clusters_dec, clusters_long, clusters_dec_gdp)

#_______________________________________________THE END_________________________________________________________________________________----


#Appendix-Clustering study: Calculer withiss/totss for1:20 culsters for each variable----
a<-c("EMP_share", "VA_share", "ptyq")
agg_w<-agg %>% mutate(z=paste0( Sector, dec)) %>% ungroup() %>% select(-dec, -Sector) %>% spread(z, Value) %>% na.omit#Turning all information into colums

withinss<-as.list(a); A<-matrix(1:20, ncol=1)

for (i in 1:length(withinss)){
d<-agg_w %>% filter(Variable==a[i])

B<-apply(A, FUN= function(x){k<-kmeans(d[-c(1,2)], x); sum(k$withinss)/k$totss }, MARGIN = 1);
B<-data.frame(withinss=B); B$Variable<-a[i]
withinss[[i]]<-B; names(withinss)[i]<-a[i]
}
plot(A, withinss$EMP_share$withinss)
plot(A, withinss$VA_share$withinss)
plot(A, withinss$ptyq$withinss)
#while it is easy to distinguish countries based on emp it not easy to do it iin va, while countries can have closer structures in employment, they may have distinct productivities and hence diff va structures
#yo can proof it by ploting bars on va for the emp clusters and show how much they differ




rm(A, B, d, withinss,a,i, agg_w)


#_____________________________________________________The End2______________________________________________________________________

#Sector correlations in emp_share
emp70<-dta %>% filter(Variable=="EMP_share"& Year==1970  ) %>% spread(Sector, Value)
pairs(emp70[7:15]); rm(emp70)
