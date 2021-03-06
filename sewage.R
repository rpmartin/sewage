#script to do cost benefit analysis on sewage treatment options: secondary, tertiary or enhanced.
rm(list=ls())#clean out environment
library(tidyverse)# for data manipulation
discount_rate <- .05 # you can perform sensitivity analysis by changing the annual discount rate.
year <- 1:20
plant <-  c("secondary","tertiary","enhanced")
thing <- c("cost","benefit")
mydf <- crossing(year,plant,thing)%>%#create df with all combinations THEN
# create artificial data for dollar amounts: cost mostly upfront, benefits increase over time &
# both costs and benefits greatest for enhanced, lowest for secondary, tertiary intermediate. 
  mutate(dollar_amount=case_when(plant == "secondary" & thing == "cost" ~ 39/year,
                                 plant == "secondary" & thing == "benefit" ~ log(30000*year),
                                 plant == "tertiary" & thing == "cost" ~ 40/year,
                                 plant == "tertiary" & thing == "benefit" ~ log(40000*year),
                                 plant == "enhanced" & thing == "cost" ~ 41/year,
                                 plant == "enhanced" & thing == "benefit" ~ log(50000*year)))%>%#THEN
  mutate(present_value = dollar_amount/((1+discount_rate)^year))#calculate present value.

plot1 <- mydf%>%#graphical depiction costs and benefits for the three options
  ggplot(aes(year, dollar_amount, colour=thing))+
  geom_line()+
  facet_grid(~fct_reorder(plant,dollar_amount,sum))#over-ride default alphabetic layout.
print(plot1)
#pretty subtle differences!

results <- mydf%>%#use dataframe mydf THEN
  group_by(plant, thing)%>%#keep separate the plants and costs vs. benefits THEN
  summarise(total = sum(present_value))%>%#sum the present_values for each plant type for each thing THEN
  pivot_wider(names_from = thing, values_from = total, names_prefix = "sum_pv_")%>%# make data tidy THEN
  mutate(net_present_value = sum_pv_benefit - sum_pv_cost)%>%#calculate the net present value THEN
  arrange(desc(net_present_value))#arrange in order of attractiveness: best at top.

print(results)
cat (paste0("Given the costs, benefits, discount rate and time horizon, \n ",results$plant[[1]]," treatment yields the greatest net present value: ",round(results$net_present_value[[1]],1)))
 