# Install necessary packages-libraries
install.packages("ggplot2")
library(ggplot2)

install.packages("psych")
library(psych)

install.packages("car")
library(car)

install.packages("tidyverse")
library(tidyverse)

# Change working directory
getwd()
setwd("C:/Users/admin/Desktop/Students R Project")

# Import dataset
data = read.csv("responses.csv")
data = subset(data,select = -c(Timestamp)) # Remove first column (Timestamp)

################################################################################
################################################################################
########################## Pre-processing ######################################
################################################################################
################################################################################

# Convert greek to english
english_colnames = c(
  "Gender", "Age", "Marital_Status", "Education_Level", "Employment_Status",
  "Years_of_Professional_Experience", "Leadership_Position",
  "If_I_have_a_work_problem_I_know_whom_to_contact",
  "I_have_good_relationship_with_colleagues", "I_feel_empty_at_the_end_of_school_day",
  "Lack_of_financial_resources_for_effective_task_execution",
  "I_feel_angry_with_someone_at_work",
  "I_behave_impersonally_towards_some_students",
  "I_utilize_my_skills_at_work", "Lately_I_argue_with_friends",
  "Recently_I_react_badly_to_things_that_did_not_bother_me_before",
  "Lately_I_make_mistakes_or_am_careless_at_work",
  "It_is_tiring_to_work_with_students_all_day", "Support_from_colleagues_exists",
  "Support_from_professional_bodies_exists",
  "Training_and_innovative_teaching_methods_are_part_of_my_work",
  "I_feel_comfortable_teaching_subjects_outside_my_specialty",
  "I_feel_trapped_in_workplace_cliques", "I_feel_my_teaching_is_effective",
  "My_students_do_not_understand_me", "I_struggle_to_remain_interested_in_teaching",
  "There_is_good_organization_from_management", "Support_from_parents_exists",
  "There_is_an_increased_number_of_students_per_class", "My_job_causes_me_stress",
  "Lately_I_get_angry_with_my_students", "I_can_understand_how_my_students_feel",
  "I_am_satisfied_with_my_chosen_profession", "I_feel_isolated_in_my_work_environment",
  "Work_thoughts_continue_after_school_day_ends",
  "I_have_good_relationship_with_my_students'_parents",
  "I_am_satisfied_with_the_classroom_psychological_climate",
  "I_have_more_workload_than_I_can_handle",
  "I_effectively_handle_students'_problems",
  "Limited_opportunities_for_professional_growth",
  "Working_with_students_causes_me_tension",
  "I_am_satisfied_with_my_salary", "Lately_I_feel_anxious",
  "I_have_help_with_important_things_I_need_to_do", "I_am_bored_with_my_job",
  "There_is_at_least_one_person_I_feel_comfortable_talking_to_about_work_problems")

colnames(data) = english_colnames

# Convert gender values to english
data$Gender = ifelse(data$Gender == "Άνδρας","Male","Female")

# Convert marital status values to english
data$Marital_Status = ifelse(data$Marital_Status == "Άγαμος/η","Single",
                             ifelse(data$Marital_Status == "Έγγαμος/η","Married",
                                    ifelse(data$Marital_Status == "Διαζευμένος/η","Divorced","Widow/er")))

# Convert education level values to english
data$Education_Level = ifelse(data$Education_Level == "Πτυχιούχος ΑΕΙ","University Degree Holder",
                              ifelse(data$Education_Level == "Κάτοχος Μεταπτυχιακού τίτλου","Master's Degree Holder",
                                     ifelse(data$Education_Level == "Κάτοχος Διδακτορικού τίτλου","PhD Holder","Technological Institute Degree Holder")))

# Convert employement status values to english
data$Employment_Status = ifelse(data$Employment_Status == "Μόνιμος","Permanent Position","Substitute teacher/Temporary teacher")

# Convert leadership values to english
data$Leadership_Position = ifelse(data$Leadership_Position == "Ναι", "Yes","No")

################################################################################
################################################################################
######################### Descriptive Statistics ###############################
################################################################################
################################################################################

# Barplot Number of Male/Female
ggplot(data = data,mapping = aes(x=Gender))+
  geom_bar(fill = c("blue","red"))+ggtitle(label = "Number of Males and Females")+
  xlab(label = "Gender")

# Count the number of male and females in the dataset
data %>% group_by(Gender) %>% summarise(n = n())

# Age Histogram
group_number = round(1+3.32*log10(120)) # 8 groups
group_width = diff(range(data$Age))/group_number # 5.25 width
breaks_age = seq(from = floor(min(data$Age)),to = ceiling(max(data$Age)),by = group_width)
hist(data$Age,breaks = breaks_age,probability = "True", main = "Age Distribution", xlab = "Age")
lines(density(data$Age),lwd = 3, col= "blue")
curve(dnorm(x,mean = mean(data$Age),sd = sd(data$Age)), add = TRUE, lwd = 3, col = "red")
legend("topright", legend = c("Normal Curve", "Density Curve"), col = c("red", "blue"), lwd = 2, bty = "n")

# summary age
summary(data$Age)

# Barplot Marriage situation
ggplot(data = data, mapping = aes(x = Marital_Status))+
  geom_bar(fill = "blue")+
  ggtitle(label = "Marital Status")+xlab(label = "")

data %>% group_by(Marital_Status) %>% summarise(n = n())

# Barplot Education
ggplot(data = data, mapping = aes(x = Education_Level))+
  geom_bar(fill = "blue")+
  ggtitle(label = "Education Level")+xlab(label = "")

# Barplot Job situation
ggplot(data = data, mapping = aes(x = Employment_Status))+
  geom_bar(fill = "blue")+
  ggtitle(label = "Employment Status")+xlab(label = "")

# Histogram Job Experience 
group_number_exp = round(1+3.32*log10(120)) # 8 groups
group_width_exp = diff(range(data$Χρόνια.Συναφούς.Επαγγελματικής.Εμπειρίας))/group_number_exp # 4.75
breaks_exp = seq(from = floor(min(data$Χρόνια.Συναφούς.Επαγγελματικής.Εμπειρίας)),to = ceiling(max(data$Χρόνια.Συναφούς.Επαγγελματικής.Εμπειρίας)),by = group_width_exp)
hist(data$Χρόνια.Συναφούς.Επαγγελματικής.Εμπειρίας,breaks = breaks_exp, main = "Χρόνια Συναφούς Επαγγελματικής Εμπειρίας", xlab = "", ylab = "Συχνότητα",col = "blue")

# Bar Plot Leadership
ggplot(data = data, mapping = aes(x = Leadership_Position))+
  geom_bar(fill = "blue")+
  ggtitle(label = "Leadership Position")+xlab(label = "")


