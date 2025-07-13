# Install necessary packages-libraries
install.packages("ggplot2")
library(ggplot2)

install.packages("psych")
library(psych)

install.packages("car")
library(car)

install.packages("tidyverse")
library(tidyverse)

install.packages("corrplot")
library(corrplot)

# Change working directory
#getwd()
#setwd("C:/Users/admin/factor_analysis")

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
  "I_am_satisfied_with_the_classroom_psychological_state",
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

# Inverted likert scale responses for some variables 

# "I_effectively_handle_students'_problems" -> "I_dont_effectively_handle_students_problems"
# "I_can_understand_how_my_students_feel" -> "I_cant_understand_how_my_students_feel"
# "I_feel_my_teaching_is_effective" -> "I_feel_my_teaching_is_not_effective"
# "I_am_satisfied_with_the_classroom_psychological_climate" -> 
# "I_am_not_satisfied_with_the_classrooms_psychological_state"

# "I_am_satisfied_with_my_chosen_profession" -> "I_am_not_satisfied_with_my_chosen_profession"
# "I_am_satisfied_with_my_salary" -> "I_am_not_satisfied_with_my_salary"
# "I_have_good_relationship_with_my_students'_parents" -> "I_have_not_good_relationship_with_my_students'_parents"
# "There_is_good_organization_from_management" -> "There_not_is_good_organization_from_management"
# "I_have_good_relationship_with_colleagues" -> "I_have_not_good_relationship_with_colleagues"
# "Training_and_innovative_teaching_methods_are_part_of_my_work" ->
# "Training_and_innovative_teaching_methods_are_not_part_of_my_work"
# "I_utilize_my_skills_at_work" -> "I_dont_utilize_my_skills_at_work"

# "Support_from_colleagues_exists" -> "Support_from_colleagues_does_not_exist"
# "Support_from_professional_bodies_exist" -> "Support_from_professional_bodies_does_not_exist"
# "Support_from_parents_exists" -> "Support_from_parents_does_not_exist"
# "There_is_at_least_one_person_I_feel_comfortable_talking_to_about_work_problems" ->
# "There_is_no_one_I_feel_comfortable_talking_to_about_work_problems"
# "I_have_help_with_important_things_I_need_to_do" -> "I_dont_have_help_with_important_things_I_need_to_do"
invert_likert = function(x){
  return(6-x)
}

data$`I_effectively_handle_students'_problems`= invert_likert(data$`I_effectively_handle_students'_problems`)
data$I_can_understand_how_my_students_feel= invert_likert(data$I_can_understand_how_my_students_feel)
data$I_feel_my_teaching_is_effective= invert_likert(data$I_feel_my_teaching_is_effective)
data$I_am_satisfied_with_the_classroom_psychological_state= invert_likert(data$I_am_satisfied_with_the_classroom_psychological_state)
data$I_am_satisfied_with_my_chosen_profession= invert_likert(data$I_am_satisfied_with_my_chosen_profession)
data$I_am_satisfied_with_my_salary= invert_likert(data$I_am_satisfied_with_my_salary)
data$`I_have_good_relationship_with_my_students'_parents`= invert_likert(data$`I_have_good_relationship_with_my_students'_parents`)
data$There_is_good_organization_from_management= invert_likert(data$There_is_good_organization_from_management)
data$I_have_good_relationship_with_colleagues = invert_likert(data$I_have_good_relationship_with_colleagues)
data$Training_and_innovative_teaching_methods_are_part_of_my_work = invert_likert(data$Training_and_innovative_teaching_methods_are_part_of_my_work)
data$I_utilize_my_skills_at_work = invert_likert(data$I_utilize_my_skills_at_work)
data$Support_from_colleagues_exists = invert_likert(data$Support_from_colleagues_exists)
data$Support_from_professional_bodies_exists = invert_likert(data$Support_from_professional_bodies_exists)
data$Support_from_parents_exists = invert_likert(data$Support_from_parents_exists)
data$There_is_at_least_one_person_I_feel_comfortable_talking_to_about_work_problems = invert_likert(data$There_is_at_least_one_person_I_feel_comfortable_talking_to_about_work_problems)
data$I_have_help_with_important_things_I_need_to_do = invert_likert(data$I_have_help_with_important_things_I_need_to_do)
data$I_feel_comfortable_teaching_subjects_outside_my_specialty = invert_likert(data$I_feel_comfortable_teaching_subjects_outside_my_specialty)

inverted_english_colnames = c(
  "Gender", "Age", "Marital_Status", "Education_Level", "Employment_Status",
  "Years_of_Professional_Experience", "Leadership_Position",
  "If_I_have_a_work_problem_I_know_whom_to_contact",
  "I_hdont_have_good_relationship_with_colleagues", "I_feel_empty_at_the_end_of_school_day",
  "Lack_of_financial_resources_for_effective_task_execution",
  "I_feel_angry_with_someone_at_work",
  "I_behave_impersonally_towards_some_students",
  "I_dont_utilize_my_skills_at_work", "Lately_I_argue_with_friends",
  "Recently_I_react_badly_to_things_that_did_not_bother_me_before",
  "Lately_I_make_mistakes_or_am_careless_at_work",
  "It_is_tiring_to_work_with_students_all_day", "Support_from_colleagues_does_not_exist",
  "Support_from_professional_bodies_does_not_exist",
  "Training_and_innovative_teaching_methods_are__not_part_of_my_work",
  "I_dont_feel_comfortable_teaching_subjects_outside_my_specialty",
  "I_feel_trapped_in_workplace_cliques", "I_feel_my_teaching_is_not_effective",
  "My_students_do_not_understand_me", "I_struggle_to_remain_interested_in_teaching",
  "There_is_not_good_organization_from_management", "Support_from_parents_does_not_exist",
  "There_is_an_increased_number_of_students_per_class", "My_job_causes_me_stress",
  "Lately_I_get_angry_with_my_students", "I_cant_understand_how_my_students_feel",
  "I_am_not_satisfied_with_my_chosen_profession", "I_feel_isolated_in_my_work_environment",
  "Work_thoughts_continue_after_school_day_ends",
  "I_dont_have_good_relationship_with_my_students_parents",
  "I_am_not_satisfied_with_the_classroom_psychological_state",
  "I_have_more_workload_than_I_can_handle",
  "I_dont_effectively_handle_students'_problems",
  "Limited_opportunities_for_professional_growth",
  "Working_with_students_causes_me_tension",
  "I_am_not_satisfied_with_my_salary", "Lately_I_feel_anxious",
  "I_dont_have_help_with_important_things_I_need_to_do", "I_am_bored_with_my_job",
  "There_is_no_one_I_feel_comfortable_talking_to_about_work_problems")

colnames(data) = inverted_english_colnames

# Questionaire dataset
questions = colnames(data)[8:ncol(data)]

# Drop columns that measure what can be a solution 
to_remove = c(questions[13],questions[21],questions[12],questions[39],questions[1],
              questions[37])
data = data[, -c(20, 28, 19, 46, 8, 44)]

# Drop columns that measure stress
stress_to_remove = c(inverted_english_colnames[10],inverted_english_colnames[12],inverted_english_colnames[13]
                     ,inverted_english_colnames[15],inverted_english_colnames[16],inverted_english_colnames[17],
                     inverted_english_colnames[24],inverted_english_colnames[25],inverted_english_colnames[26],
                     inverted_english_colnames[32],inverted_english_colnames[35],inverted_english_colnames[43],
                     inverted_english_colnames[45],inverted_english_colnames[46],inverted_english_colnames[39])

data = data[ , !(names(data) %in% inverted_english_colnames[c(10,12,13,15,16,17,24,25,26,32,35,43,45,46,39)])]

# Change column names 
# colnames(data)[8:ncol(data)] = paste0("V",1:(ncol(data) - 7))

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
group_width_exp = diff(range(data$Years_of_Professional_Experience))/group_number_exp # 4.75
breaks_exp = seq(from = floor(min(data$Years_of_Professional_Experience)),to = ceiling(max(data$Years_of_Professional_Experience)),by = group_width_exp)
hist(data$Years_of_Professional_Experience,breaks = breaks_exp, main = "Years of professional experience", xlab = "", ylab = "ΣFrequency",col = "blue")

# Bar Plot Leadership
ggplot(data = data, mapping = aes(x = Leadership_Position))+
  geom_bar(fill = "blue")+
  ggtitle(label = "Leadership Position")+xlab(label = "")

# Split data to demographics and analysis
demographics_data = data[,1:7]
analysis_data = data[,8:ncol(data)]

################################################################################
################################################################################
############################## Factor Analysis #################################
################################################################################
################################################################################

############################# Check Assumptions ################################
# Correlations 
correlations = round(cor(analysis_data),2)
correl_plot = corrplot(correlations)

factor_assumptions = function(analysis_data){
  
  # determinant lower than 0.0001
  correlations = round(cor(analysis_data),2)
  determinant = 0.00001 < det(correlations)
  
  # Bartlett's Test of Sphericity 
  bartlett_result = cortest.bartlett(analysis_data)
  
  # KMO 
  kmo_result = KMO(analysis_data)
  
  return(list(
    Determinant = determinant,
    BartlettTest = bartlett_result,
    KMO = kmo_result
  ))
}

factor_assumptions(analysis_data) 

# Remove I_dont_feel_comfortable_teaching_subjects_outside_my_specialty MSA = 0.36
analysis_data = analysis_data[, !(names(analysis_data) %in% "I_dont_feel_comfortable_teaching_subjects_outside_my_specialty")]
# Check assumptions again
factor_assumptions(analysis_data)

############################# Number of Factors ################################
# Scree plot
scree(analysis_data,pc = FALSE) 

# parallel analysis
fa.parallel(analysis_data) # 3 

# VSS
vss(analysis_data) # 2 factors 

############################ Model Estimate ####################################
model1 = fa(analysis_data, nfactors = 3, rotate = "varimax", fm = "pa")
print(model1, cut = 0.4, sort = TRUE)

################################################################################
################################################################################
################################################################################
################################################################################

# drop There_is_an_increased_number_of_students_per_class, com = 0.096 no factor loading and re run
analysis_data = analysis_data[, !(names(analysis_data) %in% "There_is_an_increased_number_of_students_per_class")]
# Check assumptions 
factor_assumptions(analysis_data)
# number of factors 
scree(analysis_data,pc = FALSE) # 3
fa.parallel(analysis_data) # 3
vss(analysis_data)
# estimate 
model2 = fa(analysis_data, nfactors = 3, rotate = "varimax", fm = "pa")
print(model2, cut = 0.4, sort = TRUE)

# drop Limited_opportunities_for_professional_growth , com = 0.24 no factor loading and re run
analysis_data = analysis_data[, !(names(analysis_data) %in% "Limited_opportunities_for_professional_growth")]
# Check assumptions 
factor_assumptions(analysis_data)
# number of factors 
scree(analysis_data,pc = FALSE) # 3
fa.parallel(analysis_data) # 3
vss(analysis_data)
# estimate 
model3 = fa(analysis_data, nfactors = 3, rotate = "varimax", fm = "pa")
print(model3, cut = 0.4, sort = TRUE)

# drop It_is_tiring_to_work_with_students_all_day , com = 0.24 no factor loading and re run
analysis_data = analysis_data[, !(names(analysis_data) %in% "It_is_tiring_to_work_with_students_all_day")]
# Check assumptions 
factor_assumptions(analysis_data)
# number of factors 
scree(analysis_data,pc = FALSE) # 3
fa.parallel(analysis_data) # 3
vss(analysis_data)
# estimate 
model4 = fa(analysis_data, nfactors = 3, rotate = "varimax", fm = "pa")
print(model4, cut = 0.4, sort = TRUE)

# factors 
fa1 = c("My_job_causes_me_stress","I_have_more_workload_than_I_can_handle","Working_with_students_causes_me_tension",
        "Lately_I_get_angry_with_my_students","I_feel_trapped_in_workplace_cliques","I_feel_isolated_in_my_work_environment")
fa2 = c("I_dont_have_good_relationship_with_my_students_parents","I_am_not_satisfied_with_the_classroom_psychological_state",
        "I_hdont_have_good_relationship_with_colleagues","I_am_not_satisfied_with_my_chosen_profession",
        "I_dont_utilize_my_skills_at_work","There_is_not_good_organization_from_management",
        "Training_and_innovative_teaching_methods_are__not_part_of_my_work")
fa3 = c("Lack_of_financial_resources_for_effective_task_execution","I_am_not_satisfied_with_my_salary")

# reliability analysis
psych::alpha(data[,fa1])
psych::alpha(data[,fa2])
psych::alpha(data[,fa3])
