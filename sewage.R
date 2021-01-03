#script to do cost benefit analysis on sewage treatment options: secondary, tertiary or enhanced.
rm(list=ls())#clean out environment
set.seed(123)#this keeps the randomly generated data the same each time you source the file. 
library(tidyverse)# for data manipulation
discount_rate <- .05 # you can perform sensitivity analysis by altering the annual discount rate.
max_horizon <- 50 #the maximum time horizon we are considering. 
truncated_horizon <- 25 #you can perform sensitivity analysis by altering the time horizon.
#make tibble with 4 variables: treatment plant, year, thing (cost/benefit), dollar amount.
plant <- c(rep("secondary", 2*max_horizon), 
           rep("tertiary", 2*max_horizon), 
           rep("enhanced", 2*max_horizon))
year <- rep(0:(max_horizon-1), 6)
thing <- rep(c(rep("cost", max_horizon), rep("benefit", max_horizon)), 3)
dollar_amount <- round(100*runif(6*max_horizon))#randomly generated amounts
mydf <- tibble(plant=plant, year=year, thing=thing, dollar_amount=dollar_amount)

mydf <- mydf%>%#use dataframe mydf THEN
  filter(year < truncated_horizon)%>%# just use some of the data THEN
  mutate(present_value = dollar_amount/((1+discount_rate)^year))#calculate present value of dollar amount.

results <- mydf%>%#use dataframe mydf THEN
  group_by(plant, thing)%>%#keep separate the plants and costs vs. benefits THEN
  summarise(total = sum(present_value))%>%#sum the present_values for each plant type for each thing THEN
  pivot_wider(names_from = thing, values_from = total, names_prefix = "sum_pv_")%>%# make data tidy THEN
  mutate(net_present_value = sum_pv_benefit - sum_pv_cost)%>%#calculate the net present value THEN
  arrange(desc(net_present_value))#arrange in order of attractiveness: best at top.

print(results)
cat (paste0("Given the costs, benefits, discount rate and time horizon, \n ",results$plant[[1]]," treatment yields the greatest net present value: ",round(results$net_present_value[[1]],1)))
 