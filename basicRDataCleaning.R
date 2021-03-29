# Anthony Stachowski
# Data Management
# R Assignment #2
# 2020-02-27


##### Aff_2012 Data Wrangling to CSV #####

rm(list = ls())

library(tidyverse)
library(stringr)
library(lubridate)
library(knitr)

#Read in dataset and assign a name to allow future adaptations
aff <- read_csv('https://itao-datasets.s3.amazonaws.com/aff_2012.csv')

head(aff)

#Make a copy of original dataset to have base table

aff_original <- aff

#Remove first row of the dataset as contains description information

aff <- aff[-c(1),]
head(aff)

#Remove rows that contain summary information using column = NAICS.display-label where rows = "Total for all sectors"
#These rows combine business information for all firms operating in the specific location and therefore can be removed.

aff_naics <- aff %>%
  filter(`NAICS.display-label` != "Total for all sectors")

#After the NAICS codes, the data is sorted by gender
#We are only concerned with firms that are primarily owned by males, females, or equally

table(aff_naics$`SEX.display-label`)

#Therefore, we will filter data to only include these values
#The values are: Male-owned, Female-owned, Equally male-/female-owned.
#The remaining values are summary information or the classification is not possible as publicly held.

aff_gender <- aff_naics %>%
  filter(`SEX.display-label` == "Equally male-/female-owned" |
           `SEX.display-label` == "Female-owned" |
           `SEX.display-label` == "Male-owned")

head(aff_gender)

#Remove columns that are not necessary, but I will retain some columns that are not ultimately needed.
#These columns may be useful as I clean the data to ensure that I am interpreting it correctly.

aff_columns <- aff_gender %>%
  select(`GEO.display-label`,
         `NAICS.display-label`,
         `SEX.display-label`,
         `EMP`,
         `PAYANN`,
         `FIRMALL`,
         `FIRMPDEMP`,
         `FIRMNOPD`)

head(aff_columns)

#Separate GEO.dispaly-label into separate columns, one for county and one for state
#Also, rename some of the columns to names easier to work with.

aff_rename <- aff_columns %>%
  separate(`GEO.display-label`,
           c('county', 'state'),
           sep = ", ") %>%
  rename('sector' = `NAICS.display-label`,
         'gender' = `SEX.display-label`)

summary(aff_rename)

#Convert columns from character and adjust EMP and PAYANN columns.
#PAYANN needs to be adjsuted so that "S" values are converted to NA and then viable numbers are multiplied by 1000.
#EMP column needs to have characters replaced with the midpoint, rounded up to nearest whole number, of the range.
#   Number values in EMP are to be retained.

emp_table <- c('10', '60', '175', '375', '750', '1750', '3750', '7500', '17500', '37500', '75000')
names(emp_table) <- c("a", "b", "c", "e", "f", "g", "h", "i", "j", "k", "l")
emp_code <- c("a", "b", "c", "e", "f", "g", "h", "i", "j", "k", "l")

aff_adjusted <- aff_rename %>%
  mutate(county = as.factor(county),
         state = as.factor(state),
         sector = as.factor(sector),
         gender = as.factor(gender),
         EMP = if_else(EMP %in% emp_code, emp_table[EMP], EMP),
         PAYANN = if_else(PAYANN == "S", typeof(NA_character_), PAYANN),
         FIRMALL = as.numeric(FIRMALL),
         FIRMPDEMP = as.numeric(FIRMPDEMP),
         FIRMNOPD = as.numeric(FIRMNOPD)) %>%
  mutate(EMP = as.numeric(EMP),
         PAYANN = as.numeric(PAYANN)*1000)

summary(aff_adjusted)

#Examine EMP column to look at the cases where the value is 0

aff_partial <- aff_adjusted %>%
  filter(EMP == 0)

table(aff_partial$FIRMPDEMP)
table(aff_partial$PAYANN)

#The majority of the rows where the employee count is 0 also have no annual pay.
#As this will not provide useful information in regard to average salaries, 
#I will remove the rows where the annual pay is 0, but retain NA values.
#I am now at the stage where I can eliminate the firm columns (FIRMALL, FIRMPDEMP, FIRMNOPD)

aff_focus <- aff_adjusted %>%
  filter(PAYANN != 0 | is.na(PAYANN)) %>%
  select(county,
         state,
         sector,
         gender,
         EMP,
         PAYANN)

summary(aff_focus)

#Create column where the average salary paid by firms is shown.
#This is done by taking PAYANN / EMP and rounding to the nearest dollar.
#NA's will be retained.

aff_long <- aff_focus %>%
  mutate(salary = round(PAYANN / EMP, 0))

summary(aff_long)

#There appears to be an issue with some of these calculations as max salary value is showing as infinity.
#Examine these records to see where this is occurring.

aff_inf <- aff_long %>%
  filter(salary == Inf)

view(aff_inf)

#The Inf is occurring for salary where the EMP column is showing 0 (4 records).
#It is possible that these entries could be in error.
#However, I will eliminate these rows so that the statistics in summary will be valid.

aff_long_1 <- aff_long %>%
  filter(salary != Inf | is.na(salary))

summary(aff_long_1)

#I will now remove the PAYANN and EMP columns.
#I will also pivot_wider to have a salary column for male-owned, female-owned, and equally-owned.

aff_wide <- aff_long_1 %>%
  select(-PAYANN,
         -EMP) %>%
  pivot_wider(names_from = gender,
              values_from = salary)

summary(aff_wide)

#Rename the new columns as male, female, and equal.
#Remove rows where all salary values are NA.
#Order the columns by: state, county, sector.

aff_clean <- aff_wide %>%
  filter(!is.na(`Equally male-/female-owned`) |
           !is.na(`Male-owned`) |
           !is.na(`Female-owned`)) %>%
  rename(equal = `Equally male-/female-owned`,
         male = `Male-owned`,
         female = `Female-owned`) %>%
  arrange(state,
          county,
          sector) %>%
  select(county,
         state,
         sector,
         equal,
         male,
         female)

#Check tibble aff_clean

head(aff_clean)
summary(aff_clean)
dim(aff_clean)

#Write into CSV file replacing the NA with blanks.

write_csv(aff_clean,
          'Stachowski_2.csv',
          na = "",
          col_names = TRUE)


