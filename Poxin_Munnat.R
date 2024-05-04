billionaires <- read.csv('C:\\Users\\Admin\\Desktop\\Semester 2\\Programming for DS\\R\\Poxin_Munnat\\df_ready.csv')
head(billionaires)

library(ggplot2)
library(dplyr)


#1 year/month/day connection with wealth
billionaires_byyear <- billionaires %>%
  group_by(birth_year) %>%
  summarize(count_year=n())

ggplot(billionaires_byyear, aes(x=birth_year, y=count_year)) +
  geom_point(shape=15, color='magenta3') + #s that all lines go to right
  
  labs(x='Number of billionaires born', y='Year of birth')+
  ggtitle('Number of billionaires born during 1920-2004.')



max(billionaires_byyear$birth_year)

billionaires$birth_month <-   factor(billionaires$birth_month, levels=c(1:12))

billionaires_bymonth <- billionaires %>%
  group_by(birth_month) %>%
  summarize(count_month=n())

ggplot(billionaires_bymonth, aes(x=birth_month, y=count_month)) +
  geom_bar(stat='identity', fill='red3', width=.4) + #s that all lines go to right
  labs(x='Number of billionaires born', y='Month of birth')+
  ggtitle('Number of billionaires born depending on month.')+
  scale_x_discrete(labels=c('1'='January', '2'='February', '3'='March', '4'='April', '5'='May', '6'='June', '7'='July', '8'='August', '9'='September', '10'='October', '11'='November', '12'='December'))+
  theme(axis.text.x=element_text(angle=90))




billionaires_byday <- billionaires %>%
  group_by(birth_day) %>%
  summarize(count_day=n())

ggplot(billionaires_byday, aes(x=birth_day, y=count_day)) +
  geom_segment(aes(x=birth_day, xend=birth_day, y=0,yend=count_day), color='darkblue')+
  geom_point(shape=19, size=3, color='darkblue') + #s that all lines go to right
  labs(y='Number of billionaires born', x='Birth day')+
  ggtitle('Number of billionaires depending on the day')



#2 wealth dependence on gender
billionaires_gender <- billionaires %>%
  group_by(gender) %>%
  summarize(count=n()) %>%
  mutate(percentage = count/sum(count)*100)

ggplot(data=billionaires_gender, aes(x=gender, y=count , fill=gender)) +
  geom_bar(stat='identity')+
  scale_fill_manual(values=c('deeppink', 'blue3'))+
  labs(x = 'Gender', y = 'Count')+
  ggtitle('The difference in the number of male and female billionaires.')+
  scale_x_discrete(labels = c('F' = 'Female', 'M' = 'Male'))+
  geom_text(aes(label=paste0(round(percentage), '%')), vjust=-0.5, size=3, fontface='bold')




#3. wealth dependence on continent of residence
billionaires_bycountinent <- billionaires%>%
  group_by(continent)%>%
  summarize(count_countries = n())






ggplot(billionaires_bycountinent, aes(x=continent, y=count_countries, fill=continent))+
  geom_bar(stat='identity')+
  ylim(-1000, 1062)+
  coord_polar(start=0)+
  theme_minimal() +
  scale_fill_manual(values = c('darkslategrey', 'darkslategray4', 'darkturquoise','darkslategray3', 'darkslategray2', 'darkslategray1'))+

  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-2,4), "cm")
  )+
  ggtitle('Distribution by continents.')+
  geom_text(data=billionaires_bycountinent, aes(x=continent, y=count_countries, label=continent, hjust=-0.2), color="black", fontface="bold",alpha=0.6, size=2.5, angle= c(60, 0, 300, 240, 180, 120), inherit.aes = FALSE ) 



#4.First/last Letter of name with wealth, we can make it as in #2

+
initial_finder <- function(x){
  substr(x, 1, 1)
}

billionaires <- billionaires %>%
  mutate(initial = initial_finder(full_name))

billionaires_byname <- billionaires %>%
  group_by(initial) %>%
  summarise(count_byinitial = n()) %>%
  mutate(percentage_initial = count_byinitial/sum(count_byinitial)*100)

ggplot(billionaires_byname, aes(x=initial, y=count_byinitial, fill=initial))+
  geom_bar(stat='identity')+
  ggtitle('How likely are you to become a billionaire?')+
  labs(x='The first letter of your name.', y='How many people are billionaires.')+
  geom_text(aes(label=paste0(round(percentage_initial), '%')), vjust=-0.5, size=3, fontface='bold')





