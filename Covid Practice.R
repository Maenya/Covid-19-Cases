library(tidyverse)


#loading the data
covid <- read.csv("covid.csv")

colnames(covid)
glimpse(covid)

#a. checking for missing data
# confirming that there is no missing data
colSums(is.na(covid))

# confirmed that there was no missing data
missing_percentage <- colSums(is.na(covid)) / nrow(covid) * 100
print(missing_percentage)


#Selecting the columns we will focus on
covid_select <- covid %>%
  select("Country_Region", "positive", "total_tested")
view(covid_select)


#Putting all the information from all teh different countries together
covid_sum <- covid_select  %>% 
  group_by(Country_Region) %>% 
  summarise(tested = sum(total_tested, na.rm = TRUE), 
            positive = sum(positive, na.rm = TRUE)) %>%
            arrange(desc(positive)) 

view(covid_sum)

#Which countries have high covid cases against teh tested ?
covid_sum %>%
  arrange(desc(positive),10)

#Creating a visual for the same
ggplot(covid_sum %>% head(10), aes(x = reorder(Country_Region, positive), y = positive)) +
  geom_col(fill = "blue") +  # Use a blue color for the bars
  coord_flip() +  # Flip the axes for better readability
  labs(title = "Top 10 Countries with Highest COVID-19 Cases",
       x = "Country",
       y = "Number of Positive Cases") +
  theme_minimal()

