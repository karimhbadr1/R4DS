#Chapter 13: Dates and Times with lubridate


# Prerequisites -----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(nycflights13)


# Create Date/Times -------------------------------------------------------

today()
now()


# From Strings ------------------------------------------------------------

ymd("2017-01-31")
mdy("januAry 31st, 2017")
dmy("31-JAN-2017")
ymd(20170131)
mdy(01312017)
ymd_hms("2017-01-31 20:11:48")
mdy_hm("01/31/2017 08:01")
ymd(20210328,tz="GMT")


# From Individual Components ----------------------------------------------

flights %>% 
  select(year,month,day,hour,minute)

View(flights %>% 
  select(year,month,day,hour,minute) %>% 
  mutate(departure=make_datetime(year,month,day,hour,minute)))

make_datetime_100<-function(year,month,day,time){
  make_datetime(year,month,day,time%/%100,time%%100)
}

flights_dt<-flights %>% 
  filter(!is.na(dep_time),!is.na(arr_time)) %>% 
  mutate(
    dep_time=make_datetime_100(year,month,day,dep_time),
    arr_time=make_datetime_100(year,month,day,arr_time),
    sched_dep_time=make_datetime_100(year,month,day,sched_dep_time),
    sched_arr_time=make_datetime_100(year,month,day,sched_arr_time)
  ) %>% 
  select(origin,dest,ends_with("delay"),ends_with("time"))

flights_dt %>% 
  ggplot(aes(dep_time)) + 
  geom_freqpoly(binwidth=86400)

flights_dt %>% 
filter(dep_time < ymd(20130102)) %>% 
  ggplot(aes(dep_time)) + 
  geom_freqpoly(binwidth = 600)


flights_dt %>% 
  filter(dep_time < ymd(20130102)) %>% 
  count()

# View(flights %>% 
#   filter(!is.na(dep_time),!is.na(arr_time)) %>% 
#   mutate(
#     dep_time=make_datetime_100(year,month,day,dep_time),
#     arr_time=make_datetime_100(year,month,day,arr_time),
#     sched_dep_time=make_datetime_100(year,month,day,sched_dep_time),
#     sched_arr_time=make_datetime_100(year,month,day,sched_arr_time)
#   )%>% 
#   filter(dep_time < ymd(20130102)))
# 
# %>% 
#   group_by(hour) %>% 
#   count()


# From Other Types --------------------------------------------------------

as_datetime(today())
today()
now()
as_datetime(now())
as_date(now())

as_datetime(60 * 60 * 10)

as_date(365 * 10 + 2)

ymd(c("2010-10-10", "bananas"))
ymd(c("2010-10-10", "2011-10-11"))
?today()
today(tzone="NZDT")
OlsonNames()
today(tzone="Zulu")
now(tzone="Asia/Beirut")

d1<-mdy("January 1, 2010")
d1

d2<-ymd("2015-Mar-07")
d2

d3 <-dmy("06-Jun-2017") 
d3

d4 <- c("August 19 (2015)", "July 1 (2015)")
mdy(d4)

d5 <- "12/30/14"
mdy(d5)


# Date-Time Components ----------------------------------------------------


# Getting Components ------------------------------------------------------

datetime<-ymd_hms("2016-07-08 12:34:56")

year(datetime)
month(datetime)
day(datetime)
mday(datetime)
yday(datetime)
wday(datetime)

month(datetime,label=TRUE)
month(datetime,label=TRUE,abbr=FALSE)

wday(datetime,label=TRUE)
wday(datetime,label=TRUE,abbr=FALSE)

flights_dt %>% 
  mutate(wday=wday(dep_time,label=TRUE,abbr=FALSE)) %>% 
  ggplot(aes(x=wday))+
  geom_bar()

flights_dt %>% 
  mutate(minute=minute(dep_time)) %>% 
  group_by(minute) %>% 
  summarise(
    avg_delay=mean(dep_delay,na.rm=TRUE)
  ) %>% 
  ggplot(aes(minute,avg_delay))+
  geom_line()

sched_dep<-flights_dt %>% 
  mutate(minute=minute(sched_dep_time)) %>% 
  group_by(minute) %>% 
  summarise(
    avg_delay=mean(dep_delay,na.rm=TRUE),
    n=n()
  )

ggplot(sched_dep,aes(minute,avg_delay))+
  geom_line()

ggplot(sched_dep, aes(minute, n)) +
  geom_line()


# Rounding ----------------------------------------------------------------

floor_date(datetime,"week")
floor_date(datetime,"month")

flights_dt %>% 
  count(week = floor_date(dep_time, "week")) %>% 
  ggplot(aes(week, n)) +
  geom_line()

View(flights_dt %>% 
  count(week=floor_date(dep_time,"week")))


d1<-today()
round_date(d1,"week")
floor_date(d1,"week")
ceiling_date(d1,"week")
d2<-"2021-04-01"
d2<-ymd(d2)
round_date(d2,"month")


# Setting Components ------------------------------------------------------

datetime<-ymd_hms("2016-07-08 12:34:56")
datetime
year(datetime)<-2020
month(datetime)<-01
hour(datetime)<-hour(datetime)+1

update(datetime,year=2020,month=2,mday=2,hour=2)

ymd("2021-02-01") %>% 
  update(mday=30)

ymd("2021-02-01") %>% 
  update(hour=400)

flights_dt %>% 
  mutate(dep_hour=update(dep_time,yday=1)) %>% 
  ggplot(aes(dep_hour))+
  geom_freqpoly(binwidth=300)

flights_dt %>% 
  filter(!is.na(dep_time)) %>% 
  mutate(dep_hour=update(dep_time,yday=1)) %>% 
  mutate(month=factor(month(dep_time))) %>%
  ggplot(aes(dep_hour,n,colour=month))+
  geom_freqpoly(aes(y=..density..),binwidth=60*60)
 

flights_dt %>% 
  mutate(dep_time_=sched_dep_time+dep_delay*60) %>% 
  filter(dep_time_!=dep_time) %>% 
  select(dep_time_,dep_time,sched_dep_time,dep_delay)


flights_dt %>% 
  mutate(
    flight_duration=as.numeric(arr_time-dep_time),
    air_time_mins=air_time,
    diff=flight_duration-air_time_mins
  ) %>% 
  select(origin,dest,flight_duration,air_time_mins,diff) %>%
  filter(air_time_mins!=diff) %>% 
  count(origin)

flights_dt %>% 
  mutate(sched_dep_hour=hour(sched_dep_time)) %>% 
  group_by(sched_dep_hour) %>% 
  summarise(dep_delay=mean(dep_delay)) %>% 
  ggplot(aes(x=sched_dep_hour,y=dep_delay))+
  geom_line()+
  geom_point()+
  geom_smooth()

flights_dt_1<-flights_dt %>% 
  mutate(sched_dep_day=wday(sched_dep_time,label=TRUE)) %>% 
  group_by(sched_dep_day) %>% 
  summarise(dep_delay=mean(dep_delay,na.rm=TRUE),arr_delay=mean(arr_delay,na.rm=TRUE))

flights_dt_1 %>% 
  arrange(arr_delay)

flights_dt_1 %>% 
  arrange(dep_delay)

ggplot(flights_dt_1,aes(x=sched_dep_day,y=dep_delay))+
  geom_histogram(stat="identity")


ggplot(diamonds,aes(x=carat))+
  geom_density()

ggplot(diamonds,aes(x=carat%%1*100))+
  geom_histogram(binwidth=1)

ggplot(flights_dt, aes(x = minute(sched_dep_time))) +
  geom_histogram(binwidth = 1)

flights_A<-flights_dt %>% 
  mutate(minute=minute(dep_time),
         early=dep_delay<0) %>% 
  group_by(minute) %>% 
  summarise(early=mean(early,na.rm=TRUE))

flights_A %>% ggplot(aes(minute,early))+
  geom_line()


# Time Spans --------------------------------------------------------------


# Durations ---------------------------------------------------------------


k_age<-today()-ymd(19970403)
as.duration(k_age)
dseconds(15)
dminutes(10)
dhours(c(12,24))
ddays(0:5)
dweeks(3)
dmonths(1)
dyears(1)

2*dyears(1)

dyears(1)+dweeks(12)+dhours(15)

tomorrow<-today()+ddays(1)
last_year<-today()-dyears(1)

one_pm<-ymd_hms("2016-03-12 13:00:00",tz="America/New_York")
one_pm
one_pm+ddays(1)

# Periods -----------------------------------------------------------------

one_pm+days(1)

seconds(15)

minutes(10)

hours(c(12,24))

days(7)

months(1:6)

weeks(3)

years(1)

10*(months(6)+days(1))

10*(dmonths(6)+ddays(1))

days(50)+hours(25)+minutes(2)

ymd("2016-01-01")+dyears(1)

ymd("2016-01-01")+years(1)

one_pm+ddays(1)
one_pm+days(1)

flights_dt %>% 
  filter(arr_time<dep_time)

flights_dt<-flights_dt %>% 
  mutate(
    overnight=arr_time<dep_time,
    arr_time=arr_time+days(overnight*1),
    sched_arr_time=sched_arr_time+days(overnight*1)
  )

flights_dt %>% 
  filter(arr_time<dep_time)


# Intervals ---------------------------------------------------------------

dyears(1)/ddays(365)
years(1)/days(1)

next_year<-today()+years(1)
next_year

(today()%--% next_year)/ddays(1)

(today()%--% next_year)%/%days(1)

ymd("2015-01-01") + months(0:11)

floor_date(today(),"year")+months(0:11)

my_age<-function(birthday){
  (dmy(birthday) %--% today())%/%years(1)
}

ymd(20160229)+years(1)

(today() %--% (today() + years(1))) / months(1)

(today() %--% (today() + years(1)))

Sys.timezone()
length(OlsonNames())
head(OlsonNames())

(x1 <- ymd_hms("2015-06-01 12:00:00", tz = "America/New_York"))

(x2 <- ymd_hms("2015-06-01 18:00:00", tz = "Europe/Copenhagen"))

(x3 <- ymd_hms("2015-06-02 04:00:00", tz = "Pacific/Auckland"))

x1-x2
x1-x3
x2-x3

x4 <- c(x1, x2, x3)
x4

x4a <- with_tz(x4, tzone = "Australia/Lord_Howe")
x4a

x4-x4a

x4b <- force_tz(x4, tzone = "Australia/Lord_Howe")
x4b

x4b-x4a
x4b-x4
