#Delay flight vs Total flight

Time_2019<-Airline_Delay_Cause._2019 %>%
  select(year,month, arr_flights, arr_del15,carrier_name) %>% drop_na() %>% 
  group_by(month) %>%
  filter(carrier_name == as.character("Delta Air Lines Inc.") |carrier_name == as.character("Southwest Airlines Co."))%>% 
  ggplot( aes(x=arr_flights, y=arr_del15, colour = factor(carrier_name) ))+ 
  geom_point(size= .3) +theme(legend.position='bottom')+labs(y = "Total number of delayed flights", x = "Total number of arriving flights",fill ="Airline")+
  scale_y_continuous()  + scale_x_continuous(trans = "log10") +geom_smooth()
                                                                    

Time_2020<-Airline_Delay_Cause_2020 %>%
  select(year,month, arr_flights, arr_del15,carrier_name) %>% drop_na() %>% 
  group_by(month) %>%
  filter(carrier_name == as.character("Delta Air Lines Inc.") |carrier_name == as.character("Southwest Airlines Co."))%>% 
  ggplot( aes(x=arr_flights, y=arr_del15, colour = factor(carrier_name) ))+ 
  geom_point(size= .3) +theme(legend.position='bottom')+labs(y = "Total number of delayed flights", x = "Total number of arriving flights",fill ="Airline")+
  scale_y_continuous()  + scale_x_continuous(trans = "log10") +geom_smooth()

grid.arrange(Time_2019,Time_2020,ncol=2)






#Distribution of the delay flight and the the delay reason


b1 <-Airline_Delay_Cause._2019 %>% 
  mutate(del_pct = arr_del15/arr_flights) %>%
  ggplot(aes(x =carrier_name, y=arr_del15)) + 
  geom_boxplot(aes(colour = carrier_name), na.rm = TRUE) + coord_flip() + 
  theme(legend.position='none') + xlab("Arline Carrier") +ylab("Total number of delayed flights in a day(2019)")


b2 <-Airline_Delay_Cause_2020 %>% 
  mutate(del_pct = arr_del15/arr_flights) %>%
  ggplot(aes(x =carrier_name, y=arr_del15)) + 
  geom_boxplot(aes(colour = carrier_name), na.rm = TRUE) + coord_flip() + 
  theme(legend.position='none') + xlab("Arline Carrier") +ylab("Total number of delayed flights in a day(2020)")

grid.arrange(b1,b2,ncol=2)


#Correlation between the delay flight and the airport 

p1<-Airline_Delay_Cause._2019 %>% select(carrier_name, airport, arr_del15, arr_flights) %>% drop_na() %>% 
  filter(airport == as.character("RDU") |airport == as.character("JAX")|airport == as.character("BNA")|airport == as.character("CLE")|airport == as.character("IND"))%>% 
  group_by(airport, carrier_name) %>% dplyr::summarize_all(funs(sum)) %>% 
  mutate(del_pct = arr_del15/arr_flights) %>% 
  ggplot(aes(x=factor(airport), y= factor(carrier_name), fill=del_pct),na.rm=TRUE) + geom_tile() + 
  theme(axis.text.x=element_text(angle=45)) + 
  scale_fill_gradient(low = "white", high = "red")+labs(y = "Airline", x = "2019 Airport",fill ="Percentage of delayed aircraft in a day ")+theme(legend.position = "bottom")   


p2<-Airline_Delay_Cause_2020 %>% select(carrier_name, airport, arr_del15, arr_flights) %>% drop_na() %>% 
  filter(airport == as.character("RDU") |airport == as.character("JAX")|airport == as.character("BNA")|airport == as.character("CLE")|airport == as.character("IND"))%>% 
  group_by(airport, carrier_name) %>% dplyr::summarize_all(funs(sum)) %>% 
  mutate(del_pct = arr_del15/arr_flights) %>% 
  ggplot(aes(x=factor(airport), y= factor(carrier_name), fill=del_pct),na.rm=TRUE) + geom_tile() + 
  theme(axis.text.x=element_text(angle=45)) + 
  scale_fill_gradient(low = "white", high = "orange")+labs(y = "Airline", x = "2020  Airport",fill ="Percentage of delayed aircraft in a day  ") +theme(legend.position = "bottom") 
grid.arrange(p1,p2,ncol=2) 
