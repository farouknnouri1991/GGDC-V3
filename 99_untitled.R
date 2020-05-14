library(ggrepel); library(ggthemes)

c<-unido %>% arrange(year) %>%filter(area%in%c("MAR", "CHN", "THA")) %>% 
  filter(variable%in%c("emp_share"), isic!="sum") #%>%   
c %>%   ggplot(aes(year, value, color=isic, label=isic, group=isic))+geom_line(size=0.9)+
  theme_bw()+geom_text_repel(data=subset(c, year==2010),
                             nudge_x = 0.1,nudge_y = 0, segment.color = NA)+guides(color=FALSE)+theme_wsj()+facet_wrap(~area)

#+
  
#facet_trelliscope(~area+variable, as_plotly = T, width = 1000, path="Outputs/unido", name="sect.evol")