#第十八章作业
rm(list=ls())
library(tidyverse)
library(nycflights13)

#18.1 planes数据合并
data(planes)
data(flights)
f18 <- left_join(flights,planes,by="tailnum")

#18.2 使用管道算子，把多个数据和称道flights中
data("weather")
data(airlines)
data(airports)
f19 <- flights %>% left_join(weather) %>% left_join(airlines) %>% left_join(planes) %>% left_join(airports,by=c("origin"="faa"))
                                                                                                  
                                                                                                  