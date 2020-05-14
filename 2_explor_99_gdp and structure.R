library(tidyverse)
#how good is information on the structure can tell us about the 


#GDP and composition and composition

dta<-readRDS("Outputs/dta/dta.RDS")  %>% filter(DB=="GGDC", Area!="ZMB", PPP=="PPP") %>% filter(Year>1999)
#%>% filter(Year%in%c(2000, 2005, 2009))

shares<-dta %>% filter( Variable%in%c("EMP_share", "VAQ_share", "ptyq_share", "ptyq", ""), Sector!="SUM") %>% spread(Sector, Value)%>%na.omit()

gdp<-dta %>% filter( Variable%in%c("EMP", "VAQ"), Sector=="SUM") %>%
  spread(Variable, Value) %>% 
  mutate(gdp=VAQ/EMP)%>% merge(shares)%>% na.omit()

  
model <-gdp~AGR+CON+FIRE+GOV+MAN+MIN+PU+TRA+WRT+OTH 

a<-gdp %>% group_by(Year, Variable) %>% nest() %>%  mutate(r2=map( .x=data,~  summary(lm(model, .x) )$r.squared)) %>% select(-data)

a$r2<-as.numeric(a$r2)

ggplot(a)+geom_point(aes(Year, r2, color=Variable))



reg1<-lm(model, filter(gdp, Variable=="EMP_share")) #%>% summary()
reg2<-lm(model, filter(gdp, Variable=="VAQ_share")) #%>% summary()
reg3<-lm(model, filter(gdp, Variable=="ptyq_share")) #%>% summary()
reg4<-lm(model, filter(gdp, Variable=="ptyq")) #%>% summary()

stargazer::stargazer(reg1, reg2,reg3,reg4, type="text")
anova(reg2)

##and growth

gdp<-dta %>% filter(Sector=="SUM", Variable=="VAQ", Year%in%c(2000, 2010))%>% arrange(Year) %>% group_by(Area) %>% 
  summarize(gdp=(last(Value)-first(Value)/first(Value)))%>% 
  merge(shares %>% filter(Year==2000))%>% na.omit()


a<-gdp %>% group_by(Year, Variable) %>% nest() %>%  mutate(r2=map( .x=data,~  summary(lm(model, .x) )$r.squared)) %>% select(-data)

a$r2<-as.numeric(a$r2)

ggplot(a)+geom_point(aes(Year, r2, color=Variable))



reg1<-lm(model, filter(gdp, Variable=="EMP_share")) #%>% summary()
reg2<-lm(model, filter(gdp, Variable=="VAQ_share")) #%>% summary()
reg3<-lm(model, filter(gdp, Variable=="ptyq_share")) #%>% summary()
reg4<-lm(model, filter(gdp, Variable=="ptyq")) #%>% summary()

stargazer::stargazer(reg1, reg2,reg3,reg4, type="text")
anova(reg2)













