billionaires <- read.csv('C:\\Users\\Admin\\Desktop\\Semester 2\\Programming for DS\\R\\Poxin_Munnat\\df_ready.csv')
head(billionaires)

library(ggplot2)
library(dplyr)



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


billionaires_bycountinent <- billionaires%>%
  group_by(continent)%>%
  summarize(count_countries = n())

label_billionaires <- billionaires_bycountinent

# calculate the ANGLE of the labels
number_of_bar <- nrow(label_billionaires)
angle <-  90 - 360 * (label_billionaires$count_countries-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
label_billionaires$hjust<-ifelse( angle < -90, 1, 0)

# flip angle BY to make them readable
label_billionaires$angle<-ifelse(angle < -90, angle+180, angle)



ggplot(label_billionaires, aes(x=continent, y=count_countries))+
  geom_bar(stat='identity', fill=alpha('blue', 0.3))+
  ylim(-1000, 1062)+
  coord_polar(start=0)+
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-2,4), "cm")
  )+
  ggtitle('Distribution by continents.')+
  geom_text(data=label_billionaires, aes(x=continent, y=count_countries, label=continent, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= angle, inherit.aes = FALSE ) 

```
```{r}
```

***4.***
  First/last Letter of name with wealth 
we can make it as in #2

```{r}
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





