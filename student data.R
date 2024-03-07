library(stringr)
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(skimr)
library(kableExtra)
library(lubridate)

# Importing all the files from excel to R

df_student <- read_excel('~/Downloads/Student.xlsx')
df_registration <- read_excel('~/Downloads/Registration.xlsx')
df_course <- read_excel('~/Downloads/Course.xlsx')

# Joining the registration table to the course table by using left join 

df_course_registration <- left_join(df_course, df_registration, by = c('Instance ID'))

# Joining the student table with the table we created joining the course and registration table using left join

df_student_course_registration <- left_join(df_course_registration, df_student, by = c('Student ID'))

skimr::skim(df_student_course_registration)
# number of majors

df_student_course_registration %>%
  group_by(Title) %>%
  summarize(count = n())

# ggplot for number of majors (TITLE)

ggplot(df_student_course_registration, aes(x = Title)) + 
  geom_bar( fill = "pink") +
  labs(title = "Count of majors", x = "Majors", y = "Count") +
  theme(axis.text = element_text(angle = 45, hjust = 1))

# total cost per major 

df_student_course_registration %>%
  group_by(Title) %>%
  summarise(`Total Cost` = sum(`Total Cost`))

# changing column names
colnames(df_student_course_registration)[colnames(df_student_course_registration) == "Student ID"] <- "Student_ID"
colnames(df_student_course_registration)[colnames(df_student_course_registration) == "Birth Date"] <- "Birth_Date"


# Birth_date
df_student_course_registration %>%
  group_by(`Birth_Date`, `Student_ID`) %>%
  summarize(count_by_birth_date = n())

# separate the birth date in to date and year 

df_birth_year <- df_student_course_registration %>%
  mutate(Birth_Year = year(Birth_Date)) %>%
  group_by(Birth_Year) %>%
  summarise(count = n())

ggplot(df_birth_year, aes(x = Birth_Year, y = count)) +
  geom_bar(stat = "identity", width = 0.9, fill = "pink", color = "blue") +
  labs(title = "Number of Students by Birth Year",
       x = "Birth Year",
       y = "Number of Students")


#total cost

df_student_course_registration %>%
  group_by(Title, `Payment Plan`) %>%
  summarise(`Total Cost` = sum(`Total Cost`))
ggplot(df_student_course_registration, aes(x = Title, y= `Total Cost`, fill = `Payment Plan`)) + 
  geom_bar(positions = "dodge", stat = "identity") +
  labs(title = "Total cost of majors", x = "Majors", y = "total cost") +
  theme(axis.text = element_text(angle = 55, vjust = 0.5, hjust = 1))  

#balance due
 
df_student_course_registration %>%
  group_by(Title, `Payment Plan`) %>%
  summarise(`Balance Due` = sum (`Balance Due`))

ggplot(df_student_course_registration, aes(x = Title, y= `Balance Due`, fill = `Payment Plan`)) + 
  geom_bar(positions = "stack", stat = "identity") +
  labs(title = "balance due graph", x = "Majors", y = "balance due") +
  theme(axis.text = element_text(angle = 55, vjust = 0.5, hjust = 1))  

#student_ID and city comparison
df2 <- df_student_course_registration %>%
  group_by(Title,`Hours Per Week`)%>%
  summarise(`Hours Per Week` = sum (`Balance Due`))

ggplot(df2, aes(x = Title, y = `Hours Per Week`)) +
  geom_bar(  stat = "identity", width = 0.9, fill = "dark green") +
  labs(title = "Hours for majors",
       x = "Majors",
       y = "Sum of Hours")

