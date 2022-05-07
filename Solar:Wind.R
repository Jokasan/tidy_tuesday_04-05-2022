================================================================================
===                                                                          ===
===                      Tidy Tuesday- 04-05-2022                            ===
================================================================================
  
# Load in data sets:

capacity <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/capacity.csv')
wind <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/wind.csv')
solar <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/solar.csv')
average_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/average_cost.csv')

library(tidyverse)

# some data manipulations:
average_cost %>% pivot_longer(cols = gas_mwh:wind_mwh,names_to = "type",values_to = "cost")-> tt

tt %>% mutate(flag = if_else(type == 'solar_mwh', 'top', false = 'bottom')) -> tt

tt %>% ggplot(aes(year,cost,color=type))+
  geom_line(aes(linetype=flag), size=1.5,alpha =0.8)+
  scale_x_continuous(breaks = seq(2008, 2022, by = 2))+
  scale_y_continuous(labels = scales::dollar_format(), breaks = seq(0,175,by=25))+
  tidyquant::theme_tq()+
  expand_limits(y=0)->gg

  gg+
  guides(linetype="none")+
  scale_color_viridis_d()+
  scale_linetype_manual(values = c("dashed","solid"))+
  labs(
    x="",
    y="Cost",
    title = "Energy costs over the years",
    subtitle = "Solar has experienced the most drastic decrease in costs",
    caption = "Data from Tidy Tuesday: 04-05-2022")+
    tidyquant::theme_tq()+
    tidyquant::scale_color_tq()+
    theme(legend.title = element_blank()) -> plot1

library(patchwork)

solar %>% 
  ggplot(aes(solar_mwh))+
  geom_histogram(bins = 10)+
  tidyquant::theme_tq()

solar %>% 
  ggplot(
    aes(solar_mwh)
  )+
  geom_histogram(color = 'white',
                 fill ='midnightblue')+
  geom_rug()+
  labs(
    title = "Distribution of price for solar",
    x = "",
    y= "Count"
  ) +
  scale_x_continuous(labels = scales::dollar_format(suffix = "/MWh")) +
  # scale_y_continuous(expand = c(0,0), limits = c(0,50))+
  tidyquant::theme_tq() -> plot2

solar %>% 
  ggplot(aes(solar_capacity,solar_mwh, color=solar_capacity))+
  geom_jitter()+
  geom_smooth(color='midnightblue')+
  tidyquant::theme_tq()+
  theme(legend.position = "none")+
  scale_color_continuous(name = 'Wind Capacity') +
  scale_y_continuous(labels = scales::dollar_format(suffix = "/MWh"))+
  labs(
    x="Solar Capacity",
    y="Solar Projected Price",
    title = "Relationship between solar\nprojected price and capacity" ) -> plot3
# 
# plot1 %>% 
#  plotly::ggplotly() -> plot1
# 
# plot2 %>% 
#   plotly::ggplotly() -> plot2
# 
# plot3 %>% 
#   plotly::ggplotly() -> plot3

((plot2/plot3|plot1))
