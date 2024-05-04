library(ggplot2)
library(wordcloud)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(RColorBrewer)
library(gt)
library(gtExtras)
billionaires <- read.csv("billionaires.csv")

wealth_gender <- ggplot(data = billionaires, aes(x = gender, y = wealth, 
                                                 fill = gender)) +
  geom_bar(stat = "identity") +
  labs(x = "Gender", y = "Wealth", title = "Wealth by Gender") +
  scale_fill_manual(values = c("pink", "skyblue"))+
  scale_y_continuous(labels = scales::comma)

wealth_gender


#wordcloud by industry
indus <- billionaires$industry
fre <- table(billionaires$industry)
words <- names(fre)

print(paste("Out of", length(fre), "industries:"))
minindex <- which.min(fre)
print(paste("Insdustry with least amount of billionares:", names(fre)[minindex],
            ", with frequency:", fre[minindex]))
print("And")

maxindex <- which.max(fre)
print(paste("Insdustry with most amount of billionares:", names(fre)[maxindex],
            ", with frequency:", fre[maxindex]))
fre

print("Most frequent industries of billionaires")

par(bg="black")
wordcloud(words, fre, scale = c(2, 1), min.freq = 40, random.order = TRUE, 
          rot.per = 0.5, colors = rainbow(length(words), 0.7))



#industry bar plot based on gender
indplot <- ggplot(data = billionaires, aes(y = industry, fill = gender)) +
  geom_bar(position = "dodge", width = 0.7) + 
  labs(y = "Frequency", title = "Industry Distribution by Gender") + 
  scale_fill_manual(values = c("pink", "skyblue"), name = "Gender", 
                    labels = c("Female", "Male")) +  
  theme_minimal()
indplot


#wordcloud by sources
sources <- billionaires$source
fresource <- table(billionaires$source)
wordsource <- names(fresource)
print(paste("Out of", length(fresource), "sources:"))
minindexsource <- which.min(fresource)
print(paste("Source with least amount of billionares:", 
            names(fresource)[minindexsource], ", with frequency:", 
            fresource[minindexsource]))
print("And")

maxindexsource <- which.max(fresource)
print(paste("Source with most amount of billionares:", 
            names(fresource)[maxindexsource], ", with frequency:", 
            fresource[maxindexsource]))
fresource

print("Most frequent sources of billionaires")

par(bg="black")
wordcloud(wordsource, fresource, scale = c(2, 1), min.freq = 17, 
          random.order = TRUE, rot.per = 0.7, 
          color = rainbow(length(wordsource), 0.7))




# Define breaks for color bins
rang <- seq(min(billionaires$birth_year), max(billionaires$birth_year), by = 20)

# Create color palette
col <- colorBin("RdPu", domain = billionaires$birth_year, bins = rang)

# Create leaflet map
wherebillion <- leaflet(billionaires) %>%
  addTiles() %>%
  setView( lng = 2.34, lat = 48.85, zoom = 3 ) %>% 
  addCircleMarkers( lng = ~country_long, lat = ~country_lat, fillOpacity = 0.6,
    color = ~col(birth_year), radius = 4, stroke = TRUE,
    label = ~paste("Industry:", industry)
  ) %>%
  addLegend( pal = col, values = ~birth_year, opacity = 0.9, 
             title = "Birth Year", position = "bottomright"
  )

wherebillion



#nasa space bg1
#or morning map - "addProviderTiles("Esri.WorldImagery") %>%"
nightwherebillion <- leaflet(billionaires) %>%
  addTiles() %>%
  setView( lng = 2.34, lat = 48.85, zoom = 3 ) %>% 
  addProviderTiles("NASAGIBS.ViirsEarthAtNight2012")%>%
  addCircleMarkers( lng = ~country_long, lat = ~country_lat, fillOpacity = 0.5,
                    color = ~col(birth_year), radius = 4, stroke = TRUE,
                    label = ~paste("Country:", citizenship)
  ) %>%
  addLegend( pal = col, values = ~birth_year, opacity = 0.9, 
             title = "Birth Year", position = "bottomleft"
  )
nightwherebillion


#13) in which industry are there more females then males


femm <-billionaires %>%
  group_by(industry) %>%
  summarise(females = sum(gender == "F"), males = sum(gender == "M"))%>%
  arrange(desc(females))
femm%>%
  gt()
#unfortunately, there is no industry with more female billionaires than males, here՛s the visualization

femm2 <- ggplot(billionaires, aes(y = industry, fill = gender)) +
  geom_histogram(stat = "count", position = "dodge")+
  scale_fill_brewer(palette="Pastel1")
femm2

#11)life expectancy and wealth by countries
lifeexp <- ggplot(billionaires, aes(y = life_expectancy, x = wealth, color = gender)) +
  geom_boxplot(aes(fill = gender), notch = TRUE) +
  labs(title = "Life Expectancy vs. Wealth by Gender", 
       x = "Wealth (Billions)", y = "Life Expectancy (Years)") +  
  theme_light() + 
  scale_color_manual(values = c("pink", "skyblue"))+
  scale_fill_brewer(palette="Pastel1")
lifeexp

#The data shows that life expectency for female billionaires is higher than for males


#12) tertiary education, citizenship, wealth
edu <- ggplot(data = billionaires, aes(x = g_tertiary_ed_enroll, y = wealth, color = industry))+
  geom_point()+facet_wrap(.~industry)
edu

edu2 <- billionaires %>%
  group_by(industry)%>%
  summarise(wealth_correlation = cor(g_tertiary_ed_enroll, wealth))
edu2 %>%
  gt()%>%
  gt_theme_dot_matrix()%>%
  tab_header(title = "Correlation: Wealth vs. Teritiery eduaction")%>%
  gt_color_rows(wealth_correlation, domain = c(0, 1), palette = "Greens")

                