#read data
library(ggthemes)
library(tidyverse)
pacman::p_load(readxl)

xx <- lapply(c(2, 4, 6), function(x){
  aa <- read_xlsx('C:\\Users\\user\\Desktop\\資料科學程式設計\\CRM.xlsx', sheet = x)
  return(aa)
    })
onshore<- data.frame(xx[1])
offshore<- read.table("C:\\Users\\user\\Desktop\\資料科學程式設計\\CRM.csv", header=T,sep=",")
omnibus<- data.frame(xx[3])


##age and assessmentrisk
onyear <- data.frame(as.numeric(str_extract(onshore$birthdate, '\\d{4}'))) %>%
  mutate(onage=2019-onyear[,c(1)]) 
assessmentrisk <- cbind(onshore$assessmentrisk, onyear) #%>% drop_na(.)

ggplot(data=assessmentrisk, aes(x=onshore$assessmentrisk, y=onage, colour = onshore$assessmentrisk))+ 
    geom_point(stat="identity")+
    expand_limits(y=c(0, 100))+
    labs(x = " age", y = "assessmentrisk", size=16)+
    theme(legend.position="none")+
    scale_y_continuous(expand = c(0, 0))

##onshore$age and offshore$age
offyear <- data.frame(as.numeric(str_extract(offshore$birthdate,'\\d{4}')))%>%
  mutate(offage=2019-.[,c(1)])
AGE<-cbind.data.frame(on=assessmentrisk$onage,off=offyear$offage) %>%
  mutate(onrange=trunc(on/10), offrange=trunc(off/10)) %>%
  gather(key=name, value=values,onrange,offrange)%>%
  mutate(range=case_when(values==0~"0~10",
         values==1~"10~20",
         values==2~"20~30",
         values==3~"30~40",
         values==4~"40~50",
         values==5~"50~60",
         values==6~"60~70",
         values==7~"70~80",
         values==8~"80~90",
         values==9~"90~100",
         values==11~"110~120"))

ggplot(data=AGE, aes(x=name, y=values, fill = range))+ 
  geom_bar(alpha = 0.7,stat="identity",position="fill",width = .6)+
  labs(x = " ", y = "age percentage", size=16)+
  #scale_fill_economist()+
  #theme(legend.position="none")+
  scale_y_continuous(expand = c(0, 0))

#area
onaddress <- data.frame(str_extract(onshore$censusaddress, '[\u4e00-\u9fa5]{2}'))
offaddress <- data.frame(str_extract(offshore$censusaddress, '[\u4e00-\u9fa5]{2}'))
address<-cbind.data.frame(onshore=onaddress[,c(1)],offshore=offaddress[,c(1)]) %>%
  gather(key=type, value=area,onshore,offshore) %>% drop_na()

ggplot(data=address, aes(x=area, fill = type))+ 
  geom_bar(alpha=0.7, stat="count",position="stack",width = .9)+
  labs(x = " ", y = "amount of people", size=16)+
  #scale_fill_economist()+
  theme(axis.text.x=element_text(angle=90))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_tableau('Classic Purple-Gray 6')

# gender/ webmember,assessmentrisk
gen <- onshore[,c("webmember", "assessmentrisk", "gendercode")] %>% drop_na()

ggplot(data=gen, aes(x=gendercode, fill = factor(assessmentrisk)))+ 
  geom_bar(alpha=0.7, stat="count",position="stack",width = .6)+
  labs(x = " ", y = "amount of people", size=16)+
  scale_x_discrete(labels = c("female", "male", "neutral"))+
  scale_fill_discrete(name = "assessmentrisk") 
  #scale_fill_tableau('Winter')
  
  

