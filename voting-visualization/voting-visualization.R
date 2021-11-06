library(methods)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(GGally)
library(colorspace)
library(plotly)

vote <- read.csv(file = 'VotingData.csv', header = TRUE)

vote$Year <- as.numeric(vote$Year)
vote$TotalPercent <- as.numeric(gsub(",","",as.character(vote$TotalPercent)))
vote$X18to24Percent <- as.numeric(gsub(",","",as.character(vote$X18to24Percent)))
vote$X25to44Percent <- as.numeric(gsub(",","",as.character(vote$X25to44Percent)))
vote$X45to64Percent <- as.numeric(gsub(",","",as.character(vote$X45to64Percent)))
vote$X65plusPercent <- as.numeric(gsub(",","",as.character(vote$X65plusPercent)))

presidentialVotes <- vote[-c(1,3,5,7,9,11,13,15,17,19,21,23,25,27),]
midtermVotes <- vote[-c(2,4,6,8,10,12,14,16,18,20,22,24,26,28),]

colnames(presidentialVotes)[3] <- "All Ages"
colnames(presidentialVotes)[5] <- "18-24"
colnames(presidentialVotes)[7] <- "25-44"
colnames(presidentialVotes)[9] <- "45-64"
colnames(presidentialVotes)[11] <- "65+"

presidentialVotes <- as_tibble(presidentialVotes)

presidentialVotes <- presidentialVotes %>%
  pivot_longer(c(`18-24`, `25-44`, `45-64`, `65+`),
               names_to = "Population",
               values_to = "Percentage")

parties <- data.frame(start = c(1964, 1968, 1974, 1976, 1980, 1988, 1992, 2000, 2008),
  end = c(1968, 1974, 1976, 1980, 1988, 1992, 2000, 2008, 2016),
  Party = c("Democrat", "Republican", "Republican", "Democrat",
            "Republican", "Republican", "Democrat", "Republican", "Democrat"),
  name = c("Johnson", "Nixon", "Ford", "Carter", "Reagan", "Bush", "Clinton",
           "Bush", "Obama"),
  stringsAsFactors = FALSE)

parties <- parties %>%
  mutate(medianYear = start + floor((end-start)/2))


ggplot(presidentialVotes, aes(x = Year, y = Percentage)) +
  scale_fill_manual(values = c("Republican" = "red", "Democrat" = "blue")) +
  geom_rect(data = parties, aes(NULL,NULL, xmin = start, xmax = end, fill = Party),
            ymin = -Inf, ymax = Inf, alpha = 0.1, size = 0.5, color = "white") +
  scale_color_discrete_sequential(palette = "Inferno")+
  geom_line(aes(color = Population), size = 1) +
  scale_x_continuous(name = "Presidential Election Year", breaks =seq(1964,2016,4),
                     limits = c(1964, 2016)) +
  scale_y_continuous(name = "Voted(%)",
                     breaks = seq(0,100,5)) +
  labs(title = "No Country for Young Men? The Aging Voter Base, 1964-2016",
       subtitle = "Source: United States Census Bureau, Historical Reported 
       Voting Rates Table A-1")

ggsave("draft2.png", width = 8, height = 4)


#POST REVISIONS

p <- ggplot(presidentialVotes, aes(x = Year, y = Percentage)) +
  scale_fill_manual(values = c("Republican" = "red", "Democrat" = "blue")) +
  geom_rect(data = parties, aes(NULL,NULL, xmin = start, xmax = end, fill = Party),
            ymin = -Inf, ymax = Inf, alpha = 0.1, size = 0.5, color = "white") +
  scale_color_manual(values = c("18-24" = "red3", "25-44" = "gray69",
                                "45-64" = "gray25", "65+" = "black"))+
  geom_line(aes(color = Population), size = 1) +
  scale_x_continuous(name = "Presidential Election Year", breaks =seq(1964,2016,4),
                     limits = c(1964, 2016)) +
  scale_y_continuous(name = "Voted(%)",
                     breaks = seq(0,100,5), limits = c(27.5,80)) +
  labs(title = "No Country for Young Men? Decreasing Youth Turnout in Presidential 
       Elections, 1964-2016",
       subtitle = "Source: United States Census Bureau, Historical Reported Voting Rates Table A-1") +
  geom_text(data = parties, aes(x = medianYear,  y = 27.5, label = name),
            size = 3) +
  theme_minimal()
p
ggsave("final.png", width = 8, height = 4)

plotVotes <- ggplotly(p)

plotVotes
