# Title: "Lesson 3, Workflow and Statistics"
# Author: "Brooks Steppe"
# Data: "2025-06-05"

# Importing Proper Packages
library('arrow') # Reads Parquets
library('tidyverse') # Manipulates data and allows visualization

# Creating a dataset
base_data <- read_parquet(file = "merged_data.parquet", stringsAsFactors = TRUE)

# Create a box plot
boxplot(formula = upward_mobility_rate_2010 ~ STATE_NAME_2010SVI, data = base_data)

# Eliminating the 06
base_data <- base_data[!(base_data$STATE_NAME_2010SVI %in% "06"), ]

# And now the box plot
boxplot(formula = upward_mobility_rate_2010 ~ STATE_NAME_2010SVI, data = base_data)

# And another
boxplot(formula = M_TOTPOP_2010SVI ~ STATE_NAME_2010SVI, data = base_data)

# Creating California and Arizona data sets
ca_data <- subset(base_data, STATE_ABBR_2010SVI == "CA")
az_data <- subset(base_data, STATE_ABBR_2010SVI == "AZ")

# Creating CA and AZ mean while creating the value
print(UMeanCA2010 <- mean(ca_data$upward_mobility_rate_2010))
print(UMeanAZ2010 <- mean(az_data$upward_mobility_rate_2010))

#Creating a data frame with the two means for each state. State and UMean are the names, c means column
UMeans2010 <- data.frame(State = c("CA", "AZ"), UMean2010 = c(UMeanCA2010, UMeanAZ2010))

# Created a data table of tracts grouped by State Abbreviation, base data then grouo
stated <- base_data %>% group_by(STATE_ABBR_2010SVI)
StateUMeans2010 <- stated %>%
  summarise(UMean = mean(upward_mobility_rate_2010, na.rm = TRUE))

# Then get rid of null values
StateUMeans2010 <- StateUMeans2010 %>%
  filter(!is.na(STATE_ABBR_2010SVI))

# Creating upward stats
UpwardStats <- base_data %>%
  group_by(STATE_ABBR_2010SVI, COUNTY_2010SVI) %>%
  summarise(UMean = mean(upward_mobility_rate_2010, na.rm = TRUE),
            UStEr = sd(upward_mobility_rate_2010, na.rm = TRUE)/sqrt(n()))

# And get rid of non abbreviations
UpwardStats <- UpwardStats %>% filter(!is.na(STATE_ABBR_2010SVI))

# Error plots for gg plot
ggplot(data = UpwardStats, mapping = aes(x = STATE_ABBR_2010SVI, y = UMean)) + geom_point()

# Adding error bars and adjusting their thickness via width
ggplot(data = UpwardStats, mapping = aes(x = STATE_ABBR_2010SVI, y = UMean)) + geom_point() + geom_errorbar(mapping = aes(ymin = UMean - UStEr, ymax= UMean + UStEr, width = .3))

# Creating a new stats page
UStatesFix <- base_data %>% 
  group_by(STATE_ABBR_2010SVI) %>%
  summarize(UMean = mean(upward_mobility_rate_2010),
            UStEr = sd(upward_mobility_rate_2010)/sqrt(n()))

# Getting rid of non-applicables
UStatesFix <- UStatesFix %>% filter(!is.na(STATE_ABBR_2010SVI))

# Replot
ggplot(data = UStatesFix, mapping = aes(x = STATE_ABBR_2010SVI, y = UMean)) + geom_point() +  geom_errorbar(mapping = aes(ymin = UMean - UStEr, ymax = UMean - UStEr), width = 0.30)

# Copy base data
base <- base_data

# Filtering to only include columns named "upward"
base %>%
  dplyr:: select(!contains("_"), starts_with ("upward"))

# Make a unique id column and filter
base <- base %>% 
  dplyr:: group_by(STATE_FIPS_2010SVI, COUNTY_2010SVI, CENSUSAREA_2010SVI) %>%
  dplyr:: mutate(unique_id = row_number(), .before = contains("_"))
base <- base %>% filter(!is.na(STATE_ABBR_2010SVI))


# Summarizing upward mobility
BaseStatsUp <- base %>%
  dplyr:: group_by(STATE_ABBR_2010SVI) %>%
  dplyr:: summarize(across(starts_with("upward"),
                           list(~mean(.x, na.rm = TRUE), 
                                ~sd(.x, na.rm = TRUE))))

# Renaming Columns
BaseStatsUp <- base %>% 
  dplyr::group_by(STATE_ABBR_2010SVI) %>% 
  dplyr::summarize(across(starts_with("upward"), 
                          list(mean = ~mean(.x, na.rm = TRUE), 
                               sd = ~sd(.x, na.rm = TRUE)),
                          .names = "{gsub('_', '', col)}_{fn}")) 
BaseStatsUp <- BaseStatsUp %>% filter(!is.na(STATE_ABBR_2010SVI))

# Creating a model group whocj can plot population and upward mobility
UModels <- base %>%
  group_by(STATE_ABBR_2010SVI) %>% summarise(model = list(lm(upward_mobility_rate_2010 ~ POP2010)))

# Nesting
base <- nest_by(stated)

# Starting step 4 so we create a new copy of the data also filter now
merged <- base_data
merged <- merged %>% filter(!is.na(STATE_ABBR_2010SVI))

# Create state data
az <- base_data[base_data$STATE_NAME_2010SVI == "Arizona", ]
ca <- base_data[base_data$STATE_NAME_2010SVI == "California", ]

# Check rows and columns
dim(merged)

# This syntax lets you see specific rows (l) or collumns (r) 
merged[3,]
merged[,3]

# Check the number of specific rows or columns for data
print(nrow(az))
print(nrow(ca))

# T tests!!
t.test(x = az$upward_mobility_rate_2010, y = ca$upward_mobility_rate_2010)

# Compare multiple groups to make a mountain l~r is y = x
MobilityRateAZAov <- aov(formula = upward_mobility_rate_2010 ~ COUNTY_2010SVI, data = az)

# This means mobility rate explained by county
summary(object = MobilityRateAZAov)

# Sinking to help with big files
sink(file = "output/az_mobility_anova.txt")
summary(object = MobilityRateAZAov)
sink()

# Linear Regressions!!

# Plotting
plot(x = base_data$upward_mobility_rate_2010, y = base_data$M_TOTPOP_2010SVI)

# Making it a log
merged$logpop <- log10(merged$M_TOTPOP_2010SVI)
plot(x = merged$upward_mobility_rate_2010, y = merged$logpop, xlab = "Upward Mobility", ylab = "log10(Population)")

# Running a linear model
MobilityVPlot <- lm(upward_mobility_rate_2010 ~ logpop, data = merged)
summary(MobilityVPlot)

# New binary categories
merged$az <- ifelse(merged$STATE_NAME_2010SVI == "Arizona", 1, 0)
merged$ca <- ifelse(merged$STATE_NAME_2010SVI == "California", 1, 0)

# Adding az as a predictor
mobility_v_pop_state <- lm(formula = upward_mobility_rate_2010 ~ logpop + az, data = merged)
summary(mobility_v_pop_state)

# Sinking and saving
sink(file = "output/mobility-pop-state-regression.txt")
summary(MobilityVPlot)
sink()

# Making a new box plot of data I want to explore::::

# Making new filtered data
l1 <- base_data %>% filter(!is.na(STATE_ABBR_2010SVI)) %>% filter(E_MOBILE_2010SVI != -999)

# Creating a box plot of 
boxplot(formula = E_MOBILE_2010SVI ~ STATE_NAME_2010SVI, data = l1)

# Using anova to summarize
az_emobile_aov <- aov(formula = upward_mobility_rate_2010 ~ E_MOBILE_2010SVI, data = az)
summary(object = az_emobile_aov)

# No significant impact of emobile on svi
# For ca...
ca_emobile_aov <- aov(formula = E_MOBILE_2010SVI ~ COUNTY_2010SVI, data = ca)
summary(object = ca_emobile_aov)

# There is significant influnce of social vulnerability by county over E_Mobile. This is evidenced by the F value of 24.

# A new regression 
mobility_v_pop_state <- lm(formula = upward_mobility_rate_2020 ~  COUNTY_2010SVI + E_MOBILE_2010SVI + TOTPOP_2010SVI, data = l1)
summary(mobility_v_pop_state)