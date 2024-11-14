library(readr)
library(ggplot2)
library(dplyr)
library(scales)


path_to_cases <- "cases.tsv"
cases<- read_tsv(path_to_cases)

cases$agegroup <- factor(cases$agegroup, 
            levels = c("0-4", "5-9", "10-14", "15-19", "20-24", 
            "25-29", "30-34", "35-39", "40-44", "45-49", 
            "50-54", "55-59", "60-64", "65-69", "70-74", 
            "75-79", "80-84", "85-89"))

ggplot(cases, aes(x = agegroup, y = n, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number of Colon Cancer Cases by Age Group and Sex",
       x = "Age Group",
       y = "Number of Cases") +
  theme_minimal()+
  scale_y_continuous(labels = label_comma())+
  theme(plot.title = element_text(hjust = 0.5))



total_cases_by_year_sex<- cases %>%
  group_by(year, sex) %>%
  summarise(total_cases = sum(n, na.rm = TRUE))

ggplot(subset(total_cases_by_year_sex, sex == "Female"), aes(x = year, y = total_cases)) +
  geom_line() +
  geom_point() +
  labs(title = "Number of Colon Cancer Cases for Females by Year",
       x = "Year",
       y = "Total Number of Cases") +
  theme_minimal()+
  scale_y_continuous(labels = label_comma())+
  theme(plot.title = element_text(hjust = 0.5))


ggplot(subset(total_cases_by_year_sex, sex == "Male"), aes(x = year, y = total_cases)) +
  geom_line() +
  geom_point() +
  labs(title = "Number of Colon Cancer Cases for Males by Year",
       x = "Year",
       y = "Total Number of Cases") +
  theme_minimal()+
  scale_y_continuous(labels = label_comma())+
  theme(plot.title = element_text(hjust = 0.5))

path_to_population <- "population.tsv"
population<- read_tsv("path_to_population")

population$agegroup <- factor(population$agegroup, 
                         levels = c("0-4", "5-9", "10-14", "15-19", "20-24", 
                                    "25-29", "30-34", "35-39", "40-44", "45-49", 
                                    "50-54", "55-59", "60-64", "65-69", "70-74", 
                                    "75-79", "80-84", "85-89"))


ggplot(population, aes(x = agegroup, y = n_pop, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") + 
  facet_wrap(~ year) + 
  labs(title = "Population Size by Age Group and Year",
       x = "Age Group",
       y = "Population Size") +
  theme_minimal() + 
  scale_y_continuous(labels = label_comma())+
  theme(axis.text.x = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))


merged_data <- left_join(cases, population, by = c("agegroup", "year", "sex"))

summary_data <- merged_data %>%
  group_by(year, sex) %>%
  summarise(
    total_cases = sum(n, na.rm = TRUE), 
    total_population = sum(n_pop, na.rm = TRUE) 
  )
head(summary_data)

merged_data <- merged_data %>%
  mutate(incidence_rate = n/ n_pop)

head(merged_data)

summary_data <- summary_data %>%
  mutate(incidence_rate = total_cases / total_population)

head(summary_data)



