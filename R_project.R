# 2014-2017 부산 여행객객
library(readxl)
x <- read_excel('HongKong.xlsx')
str(x)
x <- as.data.frame(x)
x$year <- as.numeric(x$year)
table(is.na(x$year))
install.packages('tidyverse')
library(tidyverse)
install.packages('lubridate')
library(lubridate)
library(dplyr)
S <- read_excel('SGP visitors.xlsx')
S <- rename(S, 'arrival'='Arrival Month Year', 'visitor'='Visitor Arrivals')
?rename
S <- mutate(year(S$arrival))
S$visitor <- as.numeric(S$visitor)
str(S)
sum(S$visitor)
tot <- S %>% group_by(year=year(S$arrival)) %>% summarise(tot_visitor=sum(visitor))
str(tot)
ggplot(tot, aes(year,tot_visitor)) + geom_line(col='red')
S %>% group_by(year) %>% 
?mutate
S <- mutate(S, year=year(S$arrival))
BV <- read.csv('BusanVisitor.csv')
BVG <- BV %>% group_by(year=year(Year_Month)) %>% summarise(tot=sum(Total_Visitor),tot_dom=sum(Domestic_Visitor),tot_for=sum(Foreign_Visitor))
BVG$growth <- for(a in 9){
  (BVG$tot[a]-BVG$tot[a-1])/BVG$tot[a-1]*100
}
for(a in 9){
  (BVG$tot[a]-BVG$tot[a-1])/BVG$tot[a-1]*100
}
for(a in 9){(BVG$tot[a]-BVG$tot[a-1])/BVG$tot[a-1]*100}
str(BVG)  
BVG <- as.data.frame(BVG)
(BVG$tot[a]-BVG$tot[a-1])/BVG$tot[a-1]*100
for(a in c(1,2,3,4,5,6,7,8,9)){
  a+1
}
mutate(BVG, growth=for(a in c(1:9)){
  (BVG$tot[a]-BVG$tot[a-1])/BVG$tot[a-1]*100
})
BVG <- read.csv('BusanVisitorYear.csv')
ggplot(BVG,aes(Year,Increase_Rate_Tot))+geom_line()

names(BVG_early)

#install.packages('lubridate')
library(lubridate)

BVG_early <- BV[year(BV$Year_Month) < 2018,]

ggplot(BVG_early,aes(Year_Month,Total_Visitor,col='총 여행객')) + geom_line(size=2) + geom_line(aes(Year_Month,Domestic_Visitor, col='국내여행객'),size=2) + geom_line(aes(Year_Month,Foreign_Visitor, col='해외여행객'),size=2) + scale_x_continuous(breaks = seq(2014,2017,1)) + theme(axis.title = element_blank(), legend.title = element_blank())

shapiro.test(BVG_early$Total_Visitor)

t.test(BVG_early$Total_Visitor, mu = BVG_early$Total_Visitor[BVG$Year==2014],alternative = 'greater')

t.test(BVG_early$Total_Visitor, mu = BVG_early$Total_Visitor[BVG$Year==2014],alternative = 'less')

t.test(BVG_early$Total_Visitor, mu = BVG_early$Total_Visitor[BVG$Year==2014],alternative = 'two.sided')


# 2018년 이후 부산 여행객
str(BVG_later)
names(BVG_later)
BVG_later$Year_Month <- as_datetime(BVG_later$Year_Month)

ggplot(BVG_later,aes(Year_Month,Total_Visitor,col='총 여행객')) + geom_line(size=2) + geom_line(aes(Year_Month,Domestic_Visitor, col='국내여행객'),size=2) + geom_line(aes(Year_Month,Foreign_Visitor, col='해외여행객'),size=3) + theme(axis.title = element_blank())

ggplot(BVG_later,aes(Total_Visitor,col='총 여행객')) + geom_histogram() + geom_histogram(aes(Domestic_Visitor, col='국내여행객')) + geom_histogram(aes(Foreign_Visitor, col='해외여행객')) + theme(axis.title = element_blank())

boxplot(BVG$Total_Visitor[BVG$Year>=2018])
boxplot(BVG$Total_Visitor[BVG$Year<2018])


BVG_early <- BVG[BVG$Year<2018,]
shapiro.test(BVG_early$Total_Visitor)
t.test(BVG_early$Total_Visitor, mu = BVG_early$Total_Visitor[BVG$Year==2014],alternative = 'greater')

BVG_later <- BV[year(BV$Year_Month)>2017,]
BVG_later <- BVG_later[year(BVG_later$Year_Month)<2020,]
shapiro.test(BVG_later$Total_Visitor)
library(lubridate)

t.test(BVG_later$Total_Visitor, mu = BVG_later$Total_Visitor[year(BVG_later$Year_Month)==2018 & month(BVG_later$Year_Month)==1],alternative = 'greater')
# 귀무가설: 2018~2019년까지 부산관광객 수는 오르지 않았다.
# 대립가설: 2018~2019년까지 부산관광객 수는 성장했다.
year(BVG_later$Year_Month)==2018
?read_excel

# 주요관광지
main <- read_excel('main_tour_points.xlsx')
names(main)
str(main)
main$시점 <- as.numeric(main$시점)

ggplot(main, aes(시점,유료관광지,col='유료관광지')) + geom_line(size=6) + geom_line(aes(시점, 무료관광지, col='무료관광지'),size=6) + coord_cartesian(ylim = c(0,max(main$무료관광지)+500000)) + scale_x_continuous(breaks = seq(2014,2020,1))

?geom_line
shapiro.test(main$유료관광지)
shapiro.test(main$무료관광지)
t.test(main$유료관광지[main$시점<2020], mu=main$유료관광지[main$시점==2014], alternative = 'greater')
t.test(main$유료관광지[main$시점<2020], mu=main$유료관광지[main$시점==2014], alternative = 'two.sided')
t.test(main$유료관광지[main$시점<2020], mu=main$유료관광지[main$시점==2014], alternative = 'less')
t.test(main$무료관광지[main$시점<2020], mu=main$무료관광지[main$시점==2014], alternative = 'greater')
t.test(main$무료관광지[main$시점<2020], mu=main$무료관광지[main$시점==2014], alternative = 'less')
t.test(main$무료관광지[main$시점<2020], mu=main$무료관광지[main$시점==2014], alternative = 'two.sided')
?ggplot

# 부산 관광사업체
BTB <- read_excel('BusanTourBusiness.xlsx')
str(BTB)
View(BTB)
BTB$type1 <- as.factor(BTB$type1)
BTB$type2 <- as.factor(BTB$type2)

BTB %>% group_by(year,type1) %>% summarise(sum=sum(tot)) %>% ggplot(aes(year, sum),fill=type1) + geom_col() %>% ggplot(aes(year, sum),fill=type1) + geom_col()

View(BTBS)

BTBS <- BTB %>% group_by(year, type1) %>% summarise(sum=sum(tot))
BTBS %>% ggplot(aes(year,sum,fill=type1)) + geom_col(position = 'dodge') + theme(axis.title = element_blank(), legend.title = element_blank())

#주요관광지
str(BV)
BV$Year_Month <- as.Date(BV$Year_Month)
BVG_after <- BV[year(BV$Year_Month)>=2021,]
BVG_after <- BVG_after[!(year(BVG_after$Year_Month)==2021 & month(BVG_after$Year_Month)<=10),]
BVG_after[!(year(BVG_after$Year_Month)==2021 & month(BVG_after$Year_Month)<=10),]
library(tidyverse)
library(lubridate)
class(ymd(20190101))
class(BV$Year_Month)
as.date
?datetime
names(BVG_after)

ggplot(BVG_after, aes(Year_Month,Total_Visitor,col='전체 여행객')) + geom_line() + geom_line(aes(Year_Month, Domestic_Visitor, col='국내 여행객')) + geom_line(aes(Year_Month, Foreign_Visitor, col='해외 여행객')) + theme(axis.title = element_blank(), legend.title = element_blank())

# 부산 관광 소비액
BS <- read.csv('BusanSpend.csv',header = TRUE, fileEncoding = "CP949", encoding = "UTF-8")
BS$Type <- as.factor(BS$Type)
BS$Year_Month <- as_datetime(BS$Year_Month)
str(BS)
BST <- BS[BS$Type=='총소비',]
BST <- BST[year(BST$Year_Month)>=2020,]

ggplot(BST, aes(Year_Month, Spend, col='총소비')) + geom_line(size=2) + theme(axis.title = element_blank(), legend.title = element_blank())

shapiro.test(BST$Spend)

t.test(BST$Spend, mu=BST$Spend[year(BST$Year_Month)==2020 & month(BST$Year_Month)==1],alternative='less')
?which

# 해운대구 불법 주정차
BP <- read.csv('Busan_Parking.csv',header = TRUE, fileEncoding = "CP949", encoding = "UTF-8")
library(readxl)
BP <- read_excel('H_parking.xlsx')
str(BP)
BP$year <- as_datetime(BP$year)
ggplot(BP,aes(year, tot, col='총 누계')) + geom_line()
boxplot(BP$tot)

BVN <- BV %>% filter(year(BV$Year_Month)<=2021 & year(BV$Year_Month)>=2018)
BVN <- read_excel('H_Visitor.xlsx')
BPBP <- left_join(BVN, BP, by=c('Year_Month'='year'))

ggplot(BPBP,aes(Year_Month, tot, col='총 누계')) + geom_line(size=3) + geom_line(aes(Year_Month, Total_Visitor, col = '총 여행객'),size=3)


BVN <- read_excel('H_Visitor.xlsx')
BPBP <- left_join(BVN, BP, by=c('Year_Month'='year'))
shapiro.test(BPBP$Total_Visitor)
shapiro.test(BPBP$tot)

cor(BPBP$Total_Visitor,BPBP$tot)
cor(BPBP$Total_Visitor,BPBP$p_tot)
cor(BPBP$tot,BPBP$p_tot)
result <- lm(tot ~ Total_Visitor, BPBP)
summary(result)

library(readxl)
BDT <- read_excel('Daytour_Transport.xlsx')
BDT$transport <- as.factor(BDT$transport)
levels(BDT$transport)
BDT$transport<-factor(BDT$transport, levels=c("기타", "고속/시외/시내버스", "지하철", "전세/관광버스", "철도", "차량대여/렌트", "택시", "항공기", "자전거", "선박/해상교통", "자가용"))
BDT$transport<-factor(BDT$transport, levels=c("자가용", "고속/시외/시내버스", "지하철", "전세/관광버스", "철도", "차량대여/렌트", "택시", "항공기", "자전거", "선박/해상교통", "기타"))
ggplot(BDT, aes(year, pct, fill=transport)) + geom_col()+theme(axis.title = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.title = element_blank())
install.packages('waffle')
library(waffle)
waffle(c('만족'=76,'불만족'=24),flip=T,size=2,colors=c('green','red'))
waffle(c('만족'=74,'불만족'=26),flip=T,size=2,colors=c('green','red'))
waffle(c('만족'=76,'불만족'=24),flip=T,size=2,colors=c('green','red'))
waffle(c('만족'=75,'불만족'=25),flip=T,size=2,colors=c('green','red'))
waffle(c('혼잡'=73,'!혼잡'=27),flip=T,size=2,colors=c('red','green'))
waffle(c('혼잡'=74,'!혼잡'=26),flip=T,size=2,colors=c('red','green'))

?waffle
?layout
?element_text
waffle(c(''))


install.packages("remotes")
install.packages("Ecdat")
install.packages("tidyverse")
install.packages("gganimate")

??Ecdat
?gganimate
library(Ecdat)
library(tidyverse)
library(gganimate)
library(remotes)
remotes::install_github("R-CoderDotCom/ggcats@main")
library(ggcats)
library(ggplot2)
grid <- expand.grid(1:5, 3:1)
# Cats for each line
HPV$cat <- rep(NA, 96)
HPV$cat[which(HPV$type == "park")] <- "nyancat"
HPV$cat[which(HPV$type == "visit")] <- rep(c("pop_close", "pop"), 48)

# Animation
dat

ggplot(HPV, aes(x = year, y = pop, group = type, color = type)) +
  geom_line(size = 2) +
  geom_cat(aes(cat = cat), size = 5) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  transition_reveal(year)

ggplot(df) +
  geom_cat(aes(x, y, cat = image), size = 5) +
  geom_text(aes(x, y - 0.5, label = image), size = 2.5) +
  xlim(c(0.25, 5.5)) + 
  ylim(c(0.25, 3.5)) 

#--------------------------------------------------------------------
# Data frame

dat <-
  incomeInequality %>%
  select(Year, P99, median) %>%
  rename(income_median = median,
         income_99percent = P99) %>%
  pivot_longer(cols = starts_with("income"),
               names_to = "income",
               names_prefix = "income_")

# Cats for each line
BPBP$cat <- rep(NA, 48)
dat$cat[which(dat$income == "median")] <- "nyancat"
dat$cat[which(dat$income == "99percent")] <- rep(c("pop_close", "pop"), 33)

# Animation
dat
ggplot(dat, aes(x = Year, y = value, group = income, color = income)) +
  geom_line(size = 2) +
  ggtitle("ggcats, a core package of the memeverse") +
  geom_cat(aes(cat = cat), size = 5) +
  xlab("Cats") +
  ylab("Cats") +
  theme(legend.position = "none",
        plot.title = element_text(size = 20),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  transition_reveal(Year)




#----------------------------------------------------------------
HPV <- read_excel('HPV.xlsx')
str(HPV)
library(readxl)


dat <-
  incomeInequality %>%
  select(Year, P99, median) %>%
  rename(income_median = median,
         income_99percent = P99) %>%
  pivot_longer(cols = starts_with("income"),
               names_to = "income",
               names_prefix = "income_")

# Cats for each line
HPV$cat <- rep(NA, 96)
HPV$cat[which(HPV$type == "불법 주정차 건수")] <- "nyancat"
HPV$cat[which(HPV$type == "여행객 수")] <- rep(c("pop_close", "pop"), 48)

# Animation
dat
ggplot(HPV, aes(x = year, y = pop, group = type, color = type)) +
  geom_line(size = 2) +
  geom_cat(aes(cat = cat), size = 5) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  transition_reveal(year)
library(dplyr)
library(ggplot2)

ggplot(HPV, aes(x = year, y = pop, group = type, color = type)) +
  geom_line(size = 2) + theme(axis.title = element_blank(), legend.title = element_blank(), )

ggplot(HPV, aes(x = year, y = pop, group = type, color = type)) + geom_line(size=2) + theme(axis.title = element_blank(), legend.title = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())

HPV %>% filter(type == '')
?geom_bar
names(BV)
