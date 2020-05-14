

#testing clustering function
emp<-filter(dta, PPP=="LC"& DB=="GGDC" & !Sector%in%c("SUM", "OTH"), Variable=="EMP_share", Year==1970)
emp_w<-emp  %>% spread(Sector, Value) %>% na.omit
clemp70<-data.frame(emp_w[1], cl70=kmeans(emp_w[, 7:15],4)$cluster) %>% arrange(cl70, Area)



d<-filter(dta, PPP=="LC"& DB=="GGDC" & !Sector%in%c("SUM", "OTH"), Year==1970) 
clemp702<-kmeans_f(d,"EMP_share", n=4)
clemp702<-clemp702$DATA$Data_w[1:2] %>% arrange(cluster, Area)

z<-merge(clemp70, clemp702)



#test 2

emp<-filter(dta, PPP=="LC"& DB=="GGDC" & !Sector%in%c("SUM", "OTH"), Variable=="EMP_share") %>% 
  mutate(decade=Year%/%10, z= paste0( Sector, decade)) %>% filter(decade==197) %>% group_by(Area, z) %>% 
  summarize(Value=mean(Value)) %>% spread(z, Value) %>% na.omit

clemp70<-data.frame(emp[1], cl70=kmeans(emp[, paste0(sectors, "197")],4)$cluster) #%>% arrange(Area)



d<-filter(dta, PPP=="LC"& DB=="GGDC" & !Sector%in%c("SUM", "OTH")                              ) %>% 
  mutate(decade=Year%/%10)                         %>% group_by(Area,decade,Variable, Sector) %>% 
  summarize(Value=mean(Value)) 

clemp702<-kmeans_f(Data=d,n=4, VARIABLE = "EMP_share" )

clemp702<-clemp702$DATA$Data_w %>% filter(decade==197) %>% select(Area, cluster)

z<-merge(clemp70, clemp702)

#SUCCESS