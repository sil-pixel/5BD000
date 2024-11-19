library(readr)
library(ggplot2)
library(dplyr)
library(scales)
library(splines)

# Q1

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

# Q2

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

# Q3

path_to_population <- "population.tsv"
population<- read_tsv(path_to_population)

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

# Q4

merged_data <- left_join(cases, population, by = c("agegroup", "year", "sex"))

summary_data <- merged_data %>%
  group_by(year, sex) %>%
  summarise(
    total_cases = sum(n, na.rm = TRUE), 
    total_population = sum(n_pop, na.rm = TRUE) 
  )
head(summary_data)

# Q5

merged_data <- merged_data %>%
  mutate(incidence_rate = n/ n_pop)

head(merged_data)

summary_data <- summary_data %>%
  mutate(incidence_rate = total_cases / total_population)

head(summary_data)

# Q6

ggplot(summary_data, aes(x = year, y = incidence_rate, color = sex)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Incidence Rate of Colon Cancer Over Calendar Year",
       x = "Year",
       y = "Incidence Rate") +
  theme_minimal()

ggplot(merged_data, aes(x = year, y = incidence_rate, color = agegroup)) +
  geom_point(cex = 0.7) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Incidence Rate of Colon Cancer Over Calendar Year by age group and sex",
       x = "Year",
       y = "Incidence Rate") +
  theme_minimal() +
  facet_wrap(~ sex) 

# Q7

poisson_model <- glm(total_cases ~ year + sex + offset(log(total_population)), 
                     data = summary_data,
                     family = poisson)

summary(poisson_model)

# Q8

predict_data <- data.frame(year = c(1970, 2020),
                           sex = rep(c("Male", "Female"), each = 2),
                           total_population = 100000)  

predict_data$predicted_cases <- predict(poisson_model,
                                        newdata = predict_data,
                                        type = "response")

predict_data$incidence_rate <- (predict_data$predicted_cases / predict_data$total_population) 
print(predict_data)

# Q9

poisson_model_age <- glm(n ~ year + sex + agegroup + offset(log(n_pop)),
                         data = merged_data,
                         family = poisson)

summary(poisson_model_age)

predict_data_age <- data.frame(year = c(1970, 2020),
                               agegroup = '70-74',
                               sex = rep(c("Male", "Female"), each = 2),
                               n_pop = 100000)  

predict_data_age$predicted_cases <- predict(poisson_model_age,
                                        newdata = predict_data_age,
                                        type = "response")

predict_data_age$incidence_rate <- (predict_data_age$predicted_cases / predict_data_age$n_pop) 
print(predict_data_age)

#Q10

# Convert agegroup to midpoints
age_midpoints <- c(2.5, 7.5, 12.5, 17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5, 
                   52.5, 57.5, 62.5, 67.5, 72.5, 77.5, 82.5, 87.5)
names(age_midpoints) <- levels(cases$agegroup)

merged_data <- merged_data %>%
  mutate(age_mid = age_midpoints[as.character(agegroup)])

# Fit Poisson model with splines
spline_model <- glm(n ~ ns(year, df = 4) * sex + ns(age_mid, df = 4),
                    data = merged_data,
                    offset = log(n_pop),
                    family = poisson)

summary(spline_model)

# Data for prediction
predict_data <- expand.grid(
  year = seq(min(merged_data$year), max(merged_data$year), by = 1),
  age_mid = c(52, 72, 87),  # Specific ages for prediction
  sex = c("Male", "Female"),
  n_pop = 100000  # Assume a constant population for prediction
)

predict_data$predicted_cases <- predict(spline_model, newdata = predict_data, type = "response")
predict_data$incidence_rate <- predict_data$predicted_cases / predict_data$n_pop

# Extract observed data for age groups 50-54, 70-74, 85-89
observed_data <- merged_data %>%
  filter(agegroup %in% c("50-54", "70-74", "85-89")) %>%
  group_by(year, sex, agegroup) %>%
  summarise(incidence_rate = sum(n) / sum(n_pop), .groups = 'drop')

# Match midpoints for comparison
observed_data <- observed_data %>%
  mutate(age_mid = age_midpoints[as.character(agegroup)])



# Plot modeled vs. observed incidence rates
ggplot() +
  geom_line(data = predict_data, aes(x = year, y = incidence_rate, color = factor(age_mid), linetype = sex), size = 1) +
  geom_point(data = observed_data, aes(x = year, y = incidence_rate, shape = sex, color = factor(age_mid)), size = 2) +
  labs(
    title = "Modeled vs Observed Incidence Rates Across Calendar Time",
    x = "Year",
    y = "Incidence Rate",
    color = "Age",
    linetype = "Sex",
    shape = "Sex"
  ) +
  theme_minimal()

#Q11
# Load necessary libraries
library(dplyr)
library(ggplot2)

# Load the data
cases <- read.delim("cases.tsv")
population <- read.delim("population.tsv")
head(cases)
head(population)

# Summarize the age distribution in 2022
standard_population <- population %>%
  filter(year == 2022) %>%
  group_by(sex, agegroup) %>%
  summarise(Population_2022 = sum(n_pop), .groups = "drop")

head(standard_population)

# Merge cases and population data
merged_data <- cases %>%
  left_join(population, by = c("year", "agegroup", "sex"))

# Calculate crude incidence rates
merged_data <- merged_data %>%
  mutate(Crude_Incidence_Rate = n / n_pop)

standardized_rates <- merged_data %>%
  filter(!is.na(Crude_Incidence_Rate)) %>%
  left_join(standard_population, by = c("sex", "agegroup")) %>%
  group_by(year, sex) %>%
  summarise(
    Standardized_Rate = sum(Crude_Incidence_Rate * Population_2022) / sum(Population_2022),
    .groups = "drop"
  )

head(standardized_rates)

# Calculate non-age-standardized (crude) rates per year and sex
crude_rates <- merged_data %>%
  group_by(year, sex) %>%
  summarise(
    Crude_Rate = sum(n) / sum(n_pop),
    .groups = "drop"
  )

# Combine standardized and crude rates for plotting

comparison_data <- crude_rates %>%
  rename(Rate = Crude_Rate) %>%
  mutate(Type = "Non-Age-Standardized") %>%
  bind_rows(
    standardized_rates %>%
      rename(Rate = Standardized_Rate) %>%
      mutate(Type = "Age-Standardized")
  )

# Plot standardized vs non-standardized rates
ggplot(comparison_data, aes(x = year, y = Rate, color = Type, linetype = sex)) +
  geom_line() +
  labs(
    title = "Comparison of Age-Standardized vs Non-Age-Standardized Incidence Rates",
    x = "Year",
    y = "Incidence Rate",
    color = "Rate Type"
  ) +
  theme_minimal()


