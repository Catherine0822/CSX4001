library(data.table)
library(tidyverse)

ecoupon <- read.csv("C:\\Users\\user\\Desktop\\data_science\\91APP\\ECoupon.csv",
  header=T,sep=",",fileEncoding ="UTF-8-BOM")
orders <- read.csv("C:\\Users\\user\\Desktop\\data_science\\91APP\\Orders.csv",
  header=T,sep=",",fileEncoding ="UTF-8-BOM") %>%
  mutate(OrderGroupCode=str_trim(OrderGroupCode, side = "both"))
members <- read.csv("C:\\Users\\user\\Desktop\\data_science\\91APP\\Member.csv",
  header=T,sep=",",fileEncoding ="UTF-8-BOM")
promotion <- read.csv("C:\\Users\\user\\Desktop\\data_science\\91APP\\PromotionOrders.csv",
  header=T,sep=",",fileEncoding ="UTF-8-BOM")

sorders <- head(orders, 10000)
smembers <- sample_n(members, 1000)
spromotion <- sample_n(promotion, 1000)



## SalesOrderTotalPayment & PayProfileTypeDef
## SalesOrderSlaveDateTime & SalesOrderTotalPayment
orders <- sorders %>%
  mutate(weekday = as.POSIXct(SalesOrderSlaveDateTime)) %>%
  mutate(weekday = weekdays(weekday)) %>%
  mutate(hr = strftime(SalesOrderSlaveDateTime,format="%H" )) %>%
  mutate(yearmonth= strftime(SalesOrderSlaveDateTime,format="%Y-%m-%d"))
 

dta1 <- sorders[,c(4,5,6,9,12,13,17,18,29,30)] 

##plot g1
g1 <- dta1 %>%
  group_by(hr,TrackSourceTypeDef) %>%
  subset(IsMajor != TRUE) %>%
  summarise(meanpay = mean(SalesOrderSlaveTotalPayment, na.rm = TRUE),
            sumquantity = sum(Quantity, na.rm = TRUE),
            sumpay = sum(SalesOrderSlaveTotalPayment, na.rm = TRUE))
ggplot(g1,aes(x=hr, y=sumpay,group = TrackSourceTypeDef))+ 
    geom_line(aes(color=TrackSourceTypeDef))+
    geom_point(aes(color=TrackSourceTypeDef))+
    scale_color_manual(values=c("steelblue3", "violetred3"))+
    labs(x = "hour", y = "mean payment (NT)", size=16)+
    theme(axis.text.x=element_text(angle=60),legend.position="top")

## plot g2
g2 <- dta1 %>%
  group_by(weekday,TrackSourceTypeDef) %>%
  subset(IsMajor != TRUE) %>%
  summarise(meanpay = mean(SalesOrderSlaveTotalPayment, na.rm = TRUE),
            sumquantity = sum(Quantity, na.rm = TRUE),
            sumpay = sum(SalesOrderSlaveTotalPayment, na.rm = TRUE))
ggplot(g2,aes(x=weekday, y=sumpay,group=TrackSourceTypeDef))+ 
    geom_line(aes(color=TrackSourceTypeDef))+
    geom_point(aes(color=TrackSourceTypeDef))+
    scale_x_discrete(limits=c("星期一","星期二","星期三","星期四","星期五","星期六","星期日"))+
    scale_color_manual(values=c("steelblue3", "violetred3"))+
    labs(x = "hour", y = "mean payment (NT)", size=16)+
    theme(axis.text.x=element_text(angle=60),legend.position="top")

dta2 <- orders[,-c(8,19,21,22,23,24,25,26,27,28)] 

b <- dta2[,c(2,3)] %>%
  group_by(OrderGroupCode,MemberId) %>%
  summarise(n =n()) 
filter_g3 <- count(b[,c(2)],MemberId ) %>%
  subset(n!=1)

g3 <- dta2 %>%
  group_by(MemberId,OrderGroupCode,yearmonth) %>%
  summarise(totalpay = sum(SalesOrderSlaveTotalPayment),
            totalquan = sum(Quantity)) %>%
  filter(MemberId %in% filter_g3$MemberId) %>%
  mutate(yearmonth = as.POSIXct(yearmonth))
g4 <- g3 %>%
  group_by(MemberId) %>% 
  arrange(yearmonth) %>%
  mutate(diff = difftime(lead(yearmonth),yearmonth,units = "days"))
  #mutate(diff = yearmonth - lead(yearmonth)) 
a <- subset(g4, diff !=0| NA ) %>% mutate(diff= as.numeric(diff))
colMeans(a[,6])




g4 <- g3[,c(1,2,3)] %>%
  #mutate(MemberId = as.factor(MemberId)) %>%
  spread( key= MemberId, value = yearmonth)


g3$yearmonth[3]-g3$yearmonth[4]

