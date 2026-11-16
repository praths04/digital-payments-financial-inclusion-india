#uploaded files
file21 <- read.csv("21-Dataset.csv")

#Renaming the following columns 
#Q1_2 as State_Code 
#Q2_25 as mobile_banking_app_on_phone 
#Q2_31 as use_digital_payment_methods
#Q2_34 as could_use_digital_payment
#Q2_35 as factors_stopping_use 
#Q2_36 as discontinued_digital_payment_methods 
#Q2_37 as reason_for_discontinuing 
#Q12_1 as Age
#Q12_2 as Gender 
#Q12_3 as Martial Status 
#Q12_4 as Education 
#Q12_5 as Occupation  
#AllIndiaHHDWeight as Weights 
#Q12_12 as social_group 
#Q12_13 as religion
#Q2_17 as access_to_internet
#Q2_21a as bac_jandhan
#Q2_21b as bac_savings
#Q2_21c as bac_current 
#Q2_21d as bac_huf
#Q2_22_1 as bat_psu 
#Q2_22_2 as bat_pvt 
#Q2_22_3 as bat_coop
#Q4_1 as household_standard_of_living 
#Q5_1 as annual_household_income_range 

library(dplyr)

file21 <- file21 %>%
  rename(
    State_Code = Q1_2,
    mobile_banking_app_on_phone = Q2_25,
    use_digital_payment_methods = Q2_31,
    could_use_digital_payment = Q2_34,
    factors_stopping_use = Q2_35,
    discontinued_digital_payment_methods = Q2_36,
    reason_for_discontinuing = Q2_37,
    Age = Q12_1,
    Gender = Q12_2,
    Martial_Status = Q12_3,
    Education = Q12_4,
    Occupation = Q12_5,
    Weights = AllIndiaHHDWeight,
    social_group = Q12_12,
    religion = Q12_13,
    access_to_internet = Q2_17,
    bac_jandhan = Q2_21a,
    bac_savings = Q2_21b,
    bac_current = Q2_21c,
    bac_huf = Q2_21d, 
    bat_psu = Q2_22_1,
    bat_pvt = Q2_22_2,
    bat_coop = Q2_22_3,
    household_standard_of_living = Q4_1,
    annual_household_income_range = Q5_1,
  )

#Renaming the following columns 
#Q2_33_1 as dp_banking_cards 
#Q2_33_2 as dp_ussd
#Q2_33_3 as dp_aeps 
#Q2_33_4 as dp_upi 
#Q2_33_5 as dp_mobile_wallets
#Q2_33_6 as dp_internet_banking 
#Q2_33_7 as dp_mobile_banking 

file21 <- file21 %>%
  rename(
    dp_banking_card = Q2_33_1,
    dp_ussd = Q2_33_2,
    dp_aeps = Q2_33_3,
    dp_upi = Q2_33_4,
    dp_mobile_wallets = Q2_33_5,
    dp_internet_banking = Q2_33_6,
    dp_mobile_banking = Q2_33_7
  )

#making a seperate table just for the above columns
new_file21 <- file21 %>%
  select( State_Code, mobile_banking_app_on_phone, use_digital_payment_methods, could_use_digital_payment, factors_stopping_use, discontinued_digital_payment_methods, reason_for_discontinuing, Age, Gender, Martial_Status, Education, Occupation, dp_banking_card, dp_ussd, dp_aeps, dp_upi, dp_mobile_wallets, dp_internet_banking, dp_mobile_banking, Weights, social_group, religion, access_to_internet, bac_jandhan, bac_savings,  bac_current, bac_huf, bat_psu, bat_pvt, bat_coop, household_standard_of_living, annual_household_income_range)
nrow(new_file21)

#cleaning the above dataset 
#checking for duplicates in the dataset 
sum(duplicated(new_file21))
#removing duplicates in the dataset
new_file21 <- new_file21[!duplicated(new_file21), ]
nrow(new_file21)
#checking for null values in the dataset 
sum(is.na(new_file21))
#checking null values column by column 
colSums(is.na(new_file21))
#for the use_digital_payment_methods column, there are a total of 2458 null values, will only be removing those 
new_file21 <- new_file21[!is.na(new_file21$use_digital_payment_methods), ]
colSums(is.na(new_file21))
nrow(new_file21)

#plotting individual graphs 
library(ggplot2)

#PLOT 1:  for checking how many of the respondents use digital payment methods, 1:Yes & 2:No 
head(new_file21$use_digital_payment_methods)

#treating them with weights 
#filtering by each value 
#for option 1
data_F_21_1 <- new_file21%>%
  filter(use_digital_payment_methods == 1)
weights_F_21_1 <- data_F_21_1$use_digital_payment_methods*data_F_21_1$Weights
percentage_F_21_1 <- sum(weights_F_21_1) / sum(new_file21$Weights) * 100
#for option 2
data_F_21_2 <- new_file21%>%
  filter(use_digital_payment_methods == 2)
weights_F_21_2 <- data_F_21_2$use_digital_payment_methods*data_F_21_2$Weights
weights_F_21_2_i <- weights_F_21_2 / 2
percentage_F_21_2 <- sum(weights_F_21_2_i) / sum(new_file21$Weights) * 100

#creating a common data frame to make it easier for plotting 
percentages_F_21 <- data.frame(
  Dataset_F_21 = c("1", "2"),
  Percentage_2_Change = c(percentage_F_21_1, percentage_F_21_2)
)

ggplot(percentages_F_21, aes(x = Dataset_F_21, y = Percentage_2_Change)) +
  geom_bar(stat = "identity") +
  ylab("Percentage") +
  xlab("Use digital payment methods") + 
  theme_minimal()


#PLOT 2: If use digital payment methods, which of the listed digital payment methods do you use? 
#need to filter for people who said yes in the previous question 
subset_data <- new_file21[new_file21$use_digital_payment_methods == 1, ]

#addressing Q2.33_1:  dp_banking_cards 
subset_data$dp_banking_card
#replacing null values with 0
subset_data$dp_banking_card[is.na(subset_data$dp_banking_card)] <- 0
subset_data$dp_banking_card
#multiplying with weights 
weights_21 <- subset_data$Weights
digital_payment_methods_21 <- subset_data$dp_banking_card
weighted_mean_21 <- sum(digital_payment_methods_21*weights_21)/sum(weights_21)
wm_21_DP_percentage <- weighted_mean_21*100

#addressing Q2.33_2
subset_data$dp_ussd
#replacing null values with 0
subset_data$dp_ussd[is.na(subset_data$dp_ussd)] <- 0
subset_data$dp_ussd
#multiplying with weights 
weights_21 <- subset_data$Weights
digital_payment_methods_2_21 <- subset_data$dp_ussd
weighted_mean_2_21 <- sum(digital_payment_methods_2_21*weights_21)/sum(weights_21)
wm_2_21_DP_percentage <- weighted_mean_2_21*100

#addressing Q2.33_3
subset_data$dp_aeps 
#replacing null values with 0
subset_data$dp_aeps[is.na(subset_data$dp_aeps)] <- 0
subset_data$dp_aeps 
#multiplying with weights 
weights_21 <- subset_data$Weights
digital_payment_methods_3_21 <- subset_data$dp_aeps 
weighted_mean_3_21 <- sum(digital_payment_methods_3_21*weights_21)/sum(weights_21)
wm_3_21_DP_percentage <- weighted_mean_3_21*100


#addressing Q2.33_4
subset_data$dp_upi  
#replacing null values with 0
subset_data$dp_upi  [is.na(subset_data$dp_upi)] <- 0
subset_data$dp_upi  
#multiplying with weights 
weights_21 <- subset_data$Weights
digital_payment_methods_4_21 <- subset_data$dp_upi  
weighted_mean_4_21 <- sum(digital_payment_methods_4_21*weights_21)/sum(weights_21)
wm_4_21_DP_percentage <- weighted_mean_4_21*100


#addressing Q2.33_5
subset_data$dp_mobile_wallets
#replacing null values with 0
subset_data$dp_mobile_wallets[is.na(subset_data$dp_mobile_wallets)] <- 0
subset_data$dp_mobile_wallets
#multiplying with weights 
weights_21 <- subset_data$Weights
digital_payment_methods_5_21 <- subset_data$dp_mobile_wallets
weighted_mean_5_21 <- sum(digital_payment_methods_5_21*weights_21)/sum(weights_21)
wm_5_21_DP_percentage <- weighted_mean_5_21*100

#addressing Q2.33_6
subset_data$dp_internet_banking 
#replacing null values with 0
subset_data$dp_internet_banking[is.na(subset_data$dp_internet_banking)] <- 0
subset_data$dp_internet_banking 
#multiplying with weights 
weights_21 <- subset_data$Weights
digital_payment_methods_6_21 <- subset_data$dp_internet_banking 
weighted_mean_6_21 <- sum(digital_payment_methods_6_21*weights_21)/sum(weights_21)
wm_6_21_DP_percentage <- weighted_mean_6_21*100

#addressing Q2.33_7
subset_data$dp_mobile_banking 
#replacing null values with 0
subset_data$dp_mobile_banking[is.na(subset_data$dp_mobile_banking)] <- 0
subset_data$dp_mobile_banking 
#multiplying with weights 
weights_21 <- subset_data$Weights
digital_payment_methods_7_21 <- subset_data$dp_mobile_banking 
weighted_mean_7_21 <- sum(digital_payment_methods_7_21*weights_21)/sum(weights_21)
wm_7_21_DP_percentage <- weighted_mean_7_21*100

#plotting the above values
library(ggplot2)

#creating a common dataframe to make it easier for plotting 
percentages <- data.frame(
  Dataset = c("Banking Cards", "USSD", "AePS", "UPI", "M-wallets", "I-banking", "M-banking" ),
  Percentage = c(wm_21_DP_percentage, wm_2_21_DP_percentage, wm_3_21_DP_percentage, wm_4_21_DP_percentage, wm_5_21_DP_percentage, wm_6_21_DP_percentage, wm_7_21_DP_percentage)
)

#building the overview plot 
ggplot(percentages, aes(x = Dataset, y = Percentage)) +
  geom_bar(stat = "identity") +
  ylab("Percentage-different digital payments methods used") +
  xlab("Digital Payments Methods") +
  theme_minimal()


#PLOT 3: If not using any of the digital payments, do you think sometimes that you too could use it? 1: Yes and 2: No 
data_subset <- new_file21[new_file21$use_digital_payment_methods == 2, ]
data_subset$could_use_digital_payment

#treating them with weights 
#filtering by each value 
#for option 1
DAata_F_21_1 <- data_subset%>%
  filter( could_use_digital_payment == 1)
WEeights_F_21_1 <- DAata_F_21_1$could_use_digital_payment*DAata_F_21_1$Weights
PEercentage_F_21_1 <- sum(WEeights_F_21_1) / sum(data_subset$Weights) * 100
#for option 2
DAata_F_21_2 <- data_subset%>%
  filter(use_digital_payment_methods == 2)
WEeights_F_21_2 <- DAata_F_21_2$could_use_digital_payment*DAata_F_21_2$Weights
WEeights_F_21_2_i <- WEeights_F_21_2 / 2
PEercentage_F_21_2 <- sum(WEeights_F_21_2_i) / sum(data_subset$Weights) * 100

#creating a common data frame to make it easier for plotting 
PEercentages_F_21 <- data.frame(
  DDataset_F_21 = c("1", "2"),
  PPercentage_2_Change = c(PEercentage_F_21_1, PEercentage_F_21_2)
)

ggplot(PEercentages_F_21, aes(x = DDataset_F_21, y = PPercentage_2_Change)) +
  geom_bar(stat = "identity") +
  ylab("Percentage") +
  xlab("Sometimes could use digtal payments") + 
  theme_minimal()

#PLOT 4: Which of the major factors is stopping you to not have the facility?
data_subset <- new_file21[new_file21$use_digital_payment_methods == 2, ]
nrow(data_subset$factors_stopping_use)
#replacing values = 999 with 7 
data_subset$factors_stopping_use[data_subset$factors_stopping_use != 0 & data_subset$factors_stopping_use != 1 & data_subset$factors_stopping_use != 2 & data_subset$factors_stopping_use != 3 & data_subset$factors_stopping_use != 4 & data_subset$factors_stopping_use != 5 & data_subset$factors_stopping_use != 6 ] <- 7
data_subset$factors_stopping_use
num_unique_values <- length(unique(data_subset$factors_stopping_use))
cat("Number of unique values in the column:", num_unique_values, "\n")

#for option 1
dataA_F_21_1 <- data_subset%>%
  filter(factors_stopping_use == 1)
weightsS_F_21_1 <- dataA_F_21_1$factors_stopping_use*dataA_F_21_1$Weights
percentageE_F_21_1 <- sum(weightsS_F_21_1) / sum(data_subset$Weights) * 100

#for option 2
dataA_F_21_2 <- data_subset%>%
  filter(factors_stopping_use == 2)
weightsS_F_21_2 <- dataA_F_21_2$factors_stopping_use*dataA_F_21_2$DWeights
#dividing by 2 
weightsS_F_21_2_i <- weightsS_F_21_2 / 2
#calculating the percentage
percentageE_F_21_2 <- sum(weightsS_F_21_2_i) / sum(data_subset$Weights) * 100

#for option 3
dataA_F_21_3 <- data_subset%>%
  filter(factors_stopping_use == 3)
weightsS_F_21_3 <- dataA_F_21_3$factors_stopping_use*dataA_F_21_3$Weights
#dividing by 3
weightsS_F_21_3_i <- weightsS_F_21_3 / 3
#calculating the percentage
percentageE_F_21_3 <- sum(weightsS_F_21_3_i) / sum(data_subset$Weights) * 100

#for option 4
dataA_F_21_4 <- data_subset%>%
  filter( factors_stopping_use == 4)
weightsS_F_21_4 <- dataA_F_21_4$Q2_35*dataA_F_21_4$Weights
#dividing by 4
weightsS_F_21_4_i <- weightsS_F_21_4 / 4
#calculating the percentage
percentageE_F_21_4 <- sum(weightsS_F_21_4_i) / sum(data_subset$Weights) * 100

#for option 5
dataA_F_21_5 <- data_subset%>%
  filter( factors_stopping_use == 5)
weightsS_F_21_5 <- dataA_F_21_5$factors_stopping_use*dataA_F_21_5$Weights
#dividing by 5 
weightsS_F_21_5_i <- weightsS_F_21_5 / 5
#calculating the percentage
percentageE_F_21_5 <- sum(weightsS_F_21_5_i) / sum(data_subset$Weights) * 100

#for option 6
dataA_F_21_6 <- data_subset%>%
  filter(factors_stopping_use == 6)
weightsS_F_21_6 <- dataA_F_21_6$factors_stopping_use*dataA_F_21_6$Weights
#dividing by 6
weightsS_F_21_6_i <- weightsS_F_21_6 / 6
#calculating the percentage
percentageE_F_21_6 <- sum(weightsS_F_21_6_i) / sum(data_subset$Weights) * 100

#for option 7
dataA_F_21_7 <- data_subset%>%
  filter(factors_stopping_use == 7)
weightsS_F_21_7 <- dataA_F_21_7$dataA_F_21_7$Weights
#dividing by 7 
weightsS_F_21_7_i <- weightsS_F_21_7 / 7
#calculating the percentage
percentageE_F_21_7 <- sum(weightsS_F_21_7_i) / sum(data_subset$Weights) * 100

#plotting the values 

library(ggplot2)

#creating a common data frame to make it easier for plotting 
PERcentages_F_21 <- data.frame(
  Dataset_F_21 = c("Don't know how to get the facility", "Nobody to teach me how to use", "Afraid I will do the wrong thing and lose money", "safety concerns", "bank said not possible", "family forbids", "Others"),
  Percentage_2_Change = c(percentageE_F_21_1, percentageE_F_21_2, percentageE_F_21_3, percentageE_F_21_4, percentageE_F_21_5, percentageE_F_21_6, percentageE_F_21_7)
)

ggplot(PERcentages_F_21, aes(x = Dataset_F_21, y = Percentage_2_Change)) +
  geom_bar(stat = "identity") +
  ylab("Percentage-major factors") +
  xlab("factors") + 
  theme_minimal()

#PLOT 5: Have you used digital payment methods before and discontinued? 1: Yes and 2: No 

#treating them with weights 
#filtering by each value 
#for option 1
DATA_F_21_1 <- data_subset%>%
  filter(discontinued_digital_payment_methods  == 1)
WEIGHTS_F_21_1 <- DATA_F_21_1$discontinued_digital_payment_methods *DATA_F_21_1$Weights
PERCENTAGE_F_21_1 <- sum(WEIGHTS_F_21_1) / sum(data_subset$Weights) * 100
#for option 2
DATA_F_21_2 <- data_subset%>%
  filter(discontinued_digital_payment_methods  == 2)
WEIGHTS_F_21_2 <- DATA_F_21_2$discontinued_digital_payment_methods *DATA_F_21_2$Weights
WEIGHTS_F_21_2_i <- WEIGHTS_F_21_2 / 2
PERCENTAGE_F_21_2 <- sum(WEIGHTS_F_21_2_i) / sum(data_subset$Weights) * 100

#creating a common data frame to make it easier for plotting 
perCENtages_F_21 <- data.frame(
  Dataset_F_21 = c("YES", "NO"),
  Percentage_2_Change = c(PERCENTAGE_F_21_1, PERCENTAGE_F_21_2)
)

ggplot(percentages_F_21, aes(x = Dataset_F_21, y = Percentage_2_Change)) +
  geom_bar(stat = "identity") +
  ylab("Percentage") +
  xlab("Used and then discontinued digital payment methods") + 
  theme_minimal()

#PLOT 6: Why did you stop using digital payment methods?

data_subset$reason_for_discontinuing
unique_values <- unique(data_subset$reason_for_discontinuing)
nrow(data_subset$reason_for_discontinuing)

#for option 1
dataAA_F_21_1 <- data_subset%>%
  filter(reason_for_discontinuing == 1)
weightsSS_F_21_1 <- dataAA_F_21_1$reason_for_discontinuing*dataAA_F_21_1$Weights
percentageEE_F_21_1 <- sum(weightsSS_F_21_1) / sum(data_subset$Weights) * 100

#for option 2
dataAA_F_21_2 <- data_subset%>%
  filter(reason_for_discontinuing == 2)
weightsSS_F_21_2 <- dataAA_F_21_2$reason_for_discontinuing*dataAA_F_21_2$DWeights
#dividing by 2 
weightsSS_F_21_2_i <- weightsSS_F_21_2 / 2
#calculating the percentage
percentageEE_F_21_2 <- sum(weightsSS_F_21_2_i) / sum(data_subset$Weights) * 100

#for option 3
dataAA_F_21_3 <- data_subset%>%
  filter(reason_for_discontinuing == 3)
weightsSS_F_21_3 <- dataAA_F_21_3$reason_for_discontinuing*dataAA_F_21_3$Weights
#dividing by 3
weightsSS_F_21_3_i <- weightsSS_F_21_3 / 3
#calculating the percentage
percentageEE_F_21_3 <- sum(weightsSS_F_21_3_i) / sum(data_subset$Weights) * 100

#for option 4
dataAA_F_21_4 <- data_subset%>%
  filter( reason_for_discontinuing == 4)
weightsSS_F_21_4 <- dataAA_F_21_4$Q2_35*dataAA_F_21_4$Weights
#dividing by 4
weightsSS_F_21_4_i <- weightsSS_F_21_4 / 4
#calculating the percentage
percentageEE_F_21_4 <- sum(weightsSS_F_21_4_i) / sum(data_subset$Weights) * 100

#for option 5
dataAA_F_21_5 <- data_subset%>%
  filter( reason_for_discontinuing == 5)
weightsSS_F_21_5 <- dataAA_F_21_5$reason_for_discontinuing*dataAA_F_21_5$Weights
#dividing by 5 
weightsSS_F_21_5_i <- weightsSS_F_21_5 / 5
#calculating the percentage
percentageEE_F_21_5 <- sum(weightsSS_F_21_5_i) / sum(data_subset$Weights) * 100

#for option 6
dataAA_F_21_6 <- data_subset%>%
  filter(reason_for_discontinuing == 6)
weightsSS_F_21_6 <- dataAA_F_21_6$reason_for_discontinuing*dataAA_F_21_6$Weights
#dividing by 6
weightsSS_F_21_6_i <- weightsSS_F_21_6 / 6
#calculating the percentage
percentageEE_F_21_6 <- sum(weightsSS_F_21_6_i) / sum(data_subset$Weights) * 100

#for option 7
dataAA_F_21_7 <- data_subset%>%
  filter( reason_for_discontinuing == 7)
weightsSS_F_21_7 <- dataAA_F_21_7$dataAA_F_21_7$Weights
#dividing by 7 
weightsSS_F_21_7_i <- weightsSS_F_21_7 / 7
#calculating the percentage
percentageEE_F_21_7 <- sum(weightsSS_F_21_7_i) / sum(data_subset$Weights) * 100

#for option 8
dataAA_F_21_8 <- data_subset%>%
  filter( reason_for_discontinuing == 8)
weightsSS_F_21_8 <- dataAA_F_21_7$dataAA_F_21_8$Weights
#dividing by 8 
weightsSS_F_21_8_i <- weightsSS_F_21_8 / 8
#calculating the percentage
percentageEE_F_21_8 <- sum(weightsSS_F_21_8_i) / sum(data_subset$Weights) * 100

#plotting the values 

library(ggplot2)

#creating a common data frame to make it easier for plotting 
PERCentages_F_21 <- data.frame(
  Dataset_F_21 = c("Difficulty in usage", "Lack of internet access", "Instances: cheated", "Instances: overspending", "Lack of merchant acceptance", "no percieved benefits", "extra cost", "don't need it"),
  Percentage_2_Change = c(percentageEE_F_21_1, percentageEE_F_21_2, percentageEE_F_21_3, percentageEE_F_21_4, percentageEE_F_21_5, percentageEE_F_21_6, percentageEE_F_21_7, percentageEE_F_21_8)
)

ggplot(PERcentages_F_21, aes(x = Dataset_F_21, y = Percentage_2_Change)) +
  geom_bar(stat = "identity") +
  ylab("Percentage") +
  xlab("why did you stop using digital payment methods") + 
  theme_minimal()

#preparing the dataset for regression 

#combining certain options in every column, to make the regression more understandable 
#checking the values of every column for that 
#dependent y variable 
new_file21$use_digital_payment_methods
#replacing 2 with 0 in this case 
new_file21$use_digital_payment_methods[new_file21$use_digital_payment_methods == 2] <- 0
new_file21$use_digital_payment_methods
new_file21$use_digital_payment_methods <- factor(new_file21$use_digital_payment_methods)
class(new_file21$use_digital_payment_methods)

#independent variables
#independent variable 1: Gender
new_file21$Gender
new_file21$Gender[new_file21$Gender == 1] <- 0
new_file21$Gender[new_file21$Gender == 2] <- 1
new_file21$Gender
new_file21$Gender <- factor(new_file21$Gender)
class(new_file21$Gender)

#independent variable 2: Martial Status 
new_file21$Martial_Status
new_file21$Martial_Status[new_file21$Martial_Status == 2] <- 0
#combining variables 3,4, 999 as 2
new_file21$Martial_Status[new_file21$Martial_Status == 3] <- 2
new_file21$Martial_Status[new_file21$Martial_Status == 4] <- 2
new_file21$Martial_Status[new_file21$Martial_Status == 999] <- 2
new_file21$Martial_Status
new_file21$Martial_Status <- factor(new_file21$Martial_Status)
class(new_file21$Martial_Status)

#independent variable 3: Education  
new_file21$Education
#combining 1 and 999 (illiterate and others) 
new_file21$Education[new_file21$Education == 999] <- 0
new_file21$Education[new_file21$Education == 1] <- 0

#combining 2, 3 and 4
new_file21$Education[new_file21$Education == 3] <- 1
new_file21$Education[new_file21$Education == 2] <- 1
#combining 5, 6, 7 and 8
new_file21$Education[new_file21$Education == 4] <- 2
new_file21$Education[new_file21$Education == 5] <- 2
new_file21$Education[new_file21$Education == 6] <- 2
new_file21$Education[new_file21$Education == 7] <- 2
new_file21$Education[new_file21$Education == 8] <- 2
new_file21$Education
new_file21$Education <- factor(new_file21$Education)
class(new_file21$Education)

#independent variable 4: Occupation
new_file21$Occupation
#changing value of regular salried to 0 
new_file21$Occupation[new_file21$Occupation == 2] <- 0
new_file21$Occupation
new_file21$Occupation <- factor(new_file21$Occupation)
class(new_file21$Occupation)

#independent variable 5: Social Group 
new_file21$social_group
#changing value of others to 0 
new_file21$social_group[new_file21$social_group == 4] <- 0
new_file21$social_group <- factor(new_file21$social_group)
class(new_file21$social_group)

#independent variable 6: Religion 
new_file21$religion
#changing value of others to 0 
new_file21$religion[new_file21$religion == 1] <- 0
new_file21$religion <- factor(new_file21$religion)
class(new_file21$religion)

#independent variable 7: jandhan account 
new_file21$bac_jandhan
#changing value of no to 0 
new_file21$bac_jandhan[new_file21$bac_jandhan == 2] <- 0
new_file21$bac_jandhan <- factor(new_file21$bac_jandhan)
class(new_file21$bac_jandhan)

#independent variable 8: savings account 
new_file21$bac_savings
#changing value of no to 0 
new_file21$bac_savings[new_file21$bac_savings == 2] <- 0
new_file21$bac_savings <- factor(new_file21$bac_savings)
class(new_file21$bac_savings)

#independent variable 9: current account 
new_file21$bac_current
#changing value of no to 0 
new_file21$bac_current[new_file21$bac_current == 2] <- 0
new_file21$bac_current <- factor(new_file21$bac_current)
class(new_file21$bac_current)

#independent variable 10: huf 
new_file21$bac_huf
#changing value of no to 0 
new_file21$bac_huf[new_file21$bac_huf == 2] <- 0
new_file21$bac_huf <- factor(new_file21$bac_huf)
class(new_file21$bac_huf)

#independent variable 11: pvt 
new_file21$bat_pvt
new_file21$bat_pvt[is.na(new_file21$bat_pvt)] <- 0
new_file21$bat_pvt
new_file21$bat_pvt <- factor(new_file21$bat_pvt)
class(new_file21$bat_pvt)

#independent variable 12: coop 
new_file21$bat_coop
new_file21$bat_coop[is.na(new_file21$bat_coop)] <- 0
new_file21$bat_coop
new_file21$bat_coop <- factor(new_file21$bat_coop)
class(new_file21$bat_coop)

#independent variable 13: psu 
class(new_file21$bat_psu)
new_file21$bat_psu <- as.numeric(as.character(new_file21$bat_psu))
class(new_file21$bat_psu)
new_file21$bat_psu[is.na(new_file21$bat_psu)] <- 0
new_file21$bat_psu
new_file21$bat_psu <- factor(new_file21$bat_psu)
class(new_file21$bat_psu)

#independent variable 14: annual household income range 
new_file21$annual_household_income_range 
#changing 11 to 0
new_file21$annual_household_income_range [new_file21$annual_household_income_range  == 11] <- 0
new_file21$annual_household_income_range [new_file21$annual_household_income_range  == 2] <- 1
new_file21$annual_household_income_range [new_file21$annual_household_income_range  == 3] <- 1
new_file21$annual_household_income_range [new_file21$annual_household_income_range  == 4] <- 1
new_file21$annual_household_income_range [new_file21$annual_household_income_range  == 5] <- 1
new_file21$annual_household_income_range [new_file21$annual_household_income_range  == 6] <- 2
new_file21$annual_household_income_range [new_file21$annual_household_income_range  == 7] <- 2
new_file21$annual_household_income_range [new_file21$annual_household_income_range  == 8] <- 2
new_file21$annual_household_income_range [new_file21$annual_household_income_range  == 9] <- 3
new_file21$annual_household_income_range [new_file21$annual_household_income_range  == 10] <- 3
new_file21$annual_household_income_range 
new_file21$annual_household_income_range  <- factor(new_file21$annual_household_income_range)
class(new_file21$annual_household_income_range)

#independent variable: age^2
new_file21$Age_squared <- new_file21$Age^2
new_file21$Age_squared


#converting everything into dummy variables 
#creating dummy variables 
install.packages("fastDummies")
#loading the installed package 
library(fastDummies)

#creating dummy variables, removing the first value (for multicollinearity) 
file21_dummies <- dummy_cols(
  new_file21, 
  select_columns = c("Gender", "Martial_Status", "Education", "Occupation", "social_group", "religion", "annual_household_income_range" ),
  remove_first_dummy = TRUE
) 
file21_dummies

#model 1: demographics 
model_1_demographics <- glm(use_digital_payment_methods ~ Age + Age_squared + Gender_1 + Education_1 + Education_2 + Occupation_1 + Occupation_3 + Occupation_4 + Occupation_5 + Occupation_6 + social_group_1 + social_group_2 + social_group_3 + religion_2 + religion_3 + religion_4 + religion_5 + religion_6 + religion_999  + annual_household_income_range_1 + annual_household_income_range_2 + annual_household_income_range_3,      
                            data = file21_dummies,
                            family = binomial(link = "logit"))

summary(model_1_demographics)

new_file21$use_digital_payment_methods

