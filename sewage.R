#script to do cost benefit analysis on sewage treatment options: secondary, tertiary or enhanced.
rm(list=ls())#clean out environment
library(tidyverse)# for data manipulation
discount_rate <- .05 # you can perform sensitivity analysis by altering the annual discount rate.
max_horizon <- 50 #the maximum time horizon we are considering. 
truncated_horizon <- 20 #you can perform sensitivity analysis by altering the time horizon.
#make tibble with 4 variables: treatment plant, year, thing (cost/benefit), dollar amount.
plant <- c(rep("secondary", 2*max_horizon), 
           rep("tertiary", 2*max_horizon), 
           rep("enhanced", 2*max_horizon))%>%
  factor(ordered=TRUE, levels=c("secondary","tertiary","enhanced"))
year <- rep(1:(max_horizon), 6)
thing <- rep(c(rep("cost", max_horizon), rep("benefit", max_horizon)), 3)
mydf <- tibble(plant=plant, year=year, thing=thing)%>%#create dataframe THEN
  # create artificial data where cost is mostly upfront, where as benefits increase slowly over time, and
  # and the costs and benefits are greatest for enhanced, lowest for secondary, with tertiary intermediate. 
  mutate(dollar_amount=case_when(plant == "secondary" & thing == "cost" ~ 39/year,
                                 plant == "secondary" & thing == "benefit" ~ log(30000*year),
                                 plant == "tertiary" & thing == "cost" ~ 40/year,
                                 plant == "tertiary" & thing == "benefit" ~ log(40000*year),
                                 plant == "enhanced" & thing == "cost" ~ 41/year,
                                 plant == "enhanced" & thing == "benefit" ~ log(50000*year)))%>%
  filter(year < truncated_horizon)%>%# just use some of the data THEN
  mutate(present_value = dollar_amount/((1+discount_rate)^year))#calculate present value.

plot1 <- mydf%>%#graphical depiction costs and benefits for the three options
  ggplot(aes(year, dollar_amount, colour=thing))+
  geom_line()+
  facet_grid(~plant)
print(plot1)
#not obvious by looking which will be best.


results <- mydf%>%#use dataframe mydf THEN
  group_by(plant, thing)%>%#keep separate the plants and costs vs. benefits THEN
  summarise(total = sum(present_value))%>%#sum the present_values for each plant type for each thing THEN
  pivot_wider(names_from = thing, values_from = total, names_prefix = "sum_pv_")%>%# make data tidy THEN
  mutate(net_present_value = sum_pv_benefit - sum_pv_cost)%>%#calculate the net present value THEN
  arrange(desc(net_present_value))#arrange in order of attractiveness: best at top.

print(results)
cat (paste0("Given the costs, benefits, discount rate and time horizon, \n ",results$plant[[1]]," treatment yields the greatest net present value: ",round(results$net_present_value[[1]],1)))
 