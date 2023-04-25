library(ggplot2)


# In this code we will use our ENEMDU data set to create cohorts of 17 - 18 - 19 years 

#And analyse the effect of the measure with a diff and diff with the clusters used before

#We have to create a data set with cohorts first per year at a national level and after at a province level

#We will study employment characteristics (informality rate, employment rate) and inscription to the university


cohort_nat <- Enemdu %>%
  group_by(period, cohort_17_19) %>%
  summarise_at(vars(employment, informal, university),
               mean)


plot(cohort_nat$period, cohort_nat$university)

plot(cohort_nat$period, cohort_nat$informal)

plot(cohort_nat$period, cohort_nat$employment)

#Now we will create per cohort and per province and per year

cohort_prov <- Enemdu %>%
  group_by(period, province, cohort_17_19) %>%
  summarise_at(vars(employment, informal, university),
               mean)

cohort_prov$informal <- cohort_prov$informal*100
cohort_prov$employment <- cohort_prov$employment*100
cohort_prov$university <- cohort_prov$university*100

#Dummies for control and treatment 

#We have to put in the dataset to which cluster each province belong

# Specify the encoding with the locale() function

my_locale <- locale(encoding = "latin1")

treatment_group <- read_csv("~/PHD/DB MINEDUC/Clean_MINEDUC/treatment_group.csv", local = my_locale)

cohort_prov <- cohort_prov[cohort_prov$province != "ZONAS NO DELIMITADAS" ,]
cohort_prov <- cohort_prov[cohort_prov$province != "GALAPAGOS" ,]


cohort_prov<- merge(cohort_prov,treatment_group, by = "province")

#We see with colours if we can see differences between the treatment groups


# Create scatter plot
ggplot(cohort_prov, aes(x = period, y = university, color = as.factor(treat_group))) +
  geom_point() +
  labs(x = "Year", y = "Education", color = "Treatment") +
  scale_color_manual(values = c("black", "red"))


# Create scatter plot
ggplot(cohort_prov, aes(x = period, y = informal, color = as.factor(treat_group))) +
  geom_point() +
  labs(x = "Year", y = "Informal", color = "Treatment") +
  scale_color_manual(values = c("black", "red"))

# Create scatter plot
ggplot(cohort_prov, aes(x = period, y = informal, color = as.factor(cohort_17_19))) +
  geom_point() +
  labs(x = "Year", y = "Informal", color = "Treatment") +
  scale_color_manual(values = c("black", "red"))


#We evaluate this difference with a simple ols regression


dif_reg <- lm(university ~ treat_group, data = cohort_prov)

summary(dif_reg)

#We see that there is a significant difference between the two treated groups.
#The population between 17 and 19 years of treated provinces is 1.7  percentage points higher than the treated provinces

summary(cohort_nat$university)


#This is really important when we know that the national average the last 10 years is 5.1% 

dif_reg2 <- lm(informal ~ treat_group, data = cohort_prov)

summary(dif_reg2)

#There is no statistical difference between the two groups for informal employment

dif_reg3 <- lm(employment ~ treat_group, data = cohort_prov)

summary(dif_reg3)

#Same for employment 

#Let's create a dummy equal to one after the treatment (2012)

cohort_prov$treat_year <- rep(0, length(cohort_prov$province))
cohort_prov$treat_year[cohort_prov$period>2011]=1

#First we will realize the did with including percentages of people that are and that are not in the cohort

did_reg1 <- lm(university ~ treat_year*treat_group, data = cohort_prov)

summary(did_reg1)

#We find that the effect on the percentage of people doing the first year of university is not significant

did_reg2 <- lm(informal ~ treat_year*treat_group, data = cohort_prov)

summary(did_reg2)

#We find that the effect of the policy on the percentage working on informal employment is positive and significant. 
#The policy has increased 2.9 percentage points of the informal employment.

did_reg3 <- lm(employment ~ treat_year*treat_group, data = cohort_prov)

summary(did_reg3)

#The effect on en employment is not significant


#The effect measured here could have a selection bias because people in the cohort have different characteristics than people that are not


#Therefore we add as a control variable the dummy of being in the cohort

didc_reg1 <- lm(university ~ treat_year*treat_group + cohort_17_19, data = cohort_prov)

summary(didc_reg1)

#We get that the effect of the policy remains non significant on being in the first year of university

didc_reg2 <- lm(informal ~ treat_year*treat_group + cohort_17_19, data = cohort_prov)

summary(didc_reg2)

#We have the same results for informal employment but is more significant

didc_reg3 <- lm(employment ~ treat_year*treat_group + cohort_17_19, data = cohort_prov)

summary(didc_reg3)

#Now we have that the policy increases the employment of 2 percentage points


#Let's see if when we study only the dependent variables of the cohort we have the same results : 

cohort_prov2<- cohort_prov[cohort_prov$cohort_17_19==1,]

didcc_reg1 <- lm(university ~ treat_year*treat_group, data = cohort_prov2)

summary(didcc_reg1)

#We get that the effect of the policy remains non significant on being in the first year of university

didcc_reg2 <- lm(informal ~ treat_year*treat_group, data = cohort_prov2)

summary(didcc_reg2)

#We have now that the effect if higher but now it is only significant at a 5% level

didcc_reg3 <- lm(employment ~ treat_year*treat_group, data = cohort_prov2)

summary(didcc_reg3)

#Now we have again that the policy has not an effect on the percentage of employment


############################ Computation with all the cohorts #####################

cohort_prov3 <- Enemdu %>%
  group_by(period, province, cohort) %>%
  summarise_at(vars(employment, informal, university),
               mean)


cohort_prov3$informal <- cohort_prov3$informal*100
cohort_prov3$employment <- cohort_prov3$employment*100
cohort_prov3$university <- cohort_prov3$university*100

#We have to put in the dataset to which cluster each province belong

# Specify the encoding with the locale() function

my_locale <- locale(encoding = "latin1")

treatment_group <- read_csv("~/PHD/DB MINEDUC/Clean_MINEDUC/treatment_group.csv", local = my_locale)

cohort_prov3 <- cohort_prov3[cohort_prov3$province != "ZONAS NO DELIMITADAS" ,]
cohort_prov3 <- cohort_prov3[cohort_prov3$province != "GALAPAGOS" ,]


cohort_prov3<- merge(cohort_prov3,treatment_group, by = "province")

#Let's create a dummy equal to one after the treatment (2012)

cohort_prov3$treat_year <- rep(0, length(cohort_prov3$province))
cohort_prov3$treat_year[cohort_prov3$period>2011]=1

#We add as a control variable a dummy of not being in the cohort of interest 17-19

cohort_prov3$c_17_19 <- rep(1,length(cohort_prov3$province))
cohort_prov3$c_17_19[cohort_prov3$cohort == "17-19"]=0

#Now we realize the did with all the cohorts but controlling with a dummy for all cohorts that are not 17-19

did_allc_reg1 <- lm(university ~ treat_year*treat_group + c_17_19, data = cohort_prov3)

summary(did_allc_reg1)

#We get that the effect of the policy remains non significant on being in the first year of university

did_allc_reg2 <- lm(informal ~ treat_year*treat_group + c_17_19, data = cohort_prov3)

summary(did_allc_reg2)

#We have the same results for informal employment but is more significant

did_allc_reg3 <- lm(employment ~ treat_year*treat_group + c_17_19, data = cohort_prov3)

summary(did_allc_reg3)


#Now will add some control variables to control the characteristics of each cohort such as the income mean 
#and mechanisms such as the migration percentage and the percentage of students working and studying

#Migration

table(Enemdu$student_employment)

controls <- Enemdu %>%
  group_by(period, province, cohort) %>%
  summarise_at(vars(employment, informal, university, h_tot_labour_income_pc, migrant, student_employment),
               mean)

controls <- controls[controls$province != "ZONAS NO DELIMITADAS" ,]
controls <- controls[controls$province != "GALAPAGOS" ,]

cohort_prov4<- merge(treatment_group,controls, by = c("province"))

#We add as a control variable a dummy of not being in the cohort of interest 17-19

cohort_prov4$c_17_19 <- rep(1,length(cohort_prov4$province))
cohort_prov4$c_17_19[cohort_prov4$cohort == "17-19"]=0

#Let's create a dummy equal to one after the treatment (2012)

cohort_prov4$treat_year <- rep(0, length(cohort_prov4$province))
cohort_prov4$treat_year[cohort_prov4$period>2011]=1

#When we add the income as a control variable

did_mec_con1 <- lm(informal ~ treat_group*treat_year + h_tot_labour_income_pc + c_17_19 , data = cohort_prov4 )

summary(did_mec_con1)

#Now we evaluate the mechanism : migrating due to studies

did_mec_con2 <- lm(informal ~ treat_group*treat_year + migrant + h_tot_labour_income_pc + c_17_19 , data = cohort_prov4 )

summary(did_mec_con2)

#Now we evaluate the mechanism : the students are now working and work in informal employments

did_mec_con3 <- lm(informal ~ treat_group*treat_year + student_employment + h_tot_labour_income_pc + c_17_19 , data = cohort_prov4 )

summary(did_mec_con3)

#THe interaction between the two mechanisms 

did_mec_con4 <- lm(informal ~ treat_group*treat_year + student_employment*migrant + h_tot_labour_income_pc + c_17_19 , data = cohort_prov4 )

summary(did_mec_con4)

#How the policy have affected the fact that students work and study 

did_mec_con5 <- lm(student_employment ~ treat_group*treat_year + migrant + h_tot_labour_income_pc + c_17_19 , data = cohort_prov4 )

summary(did_mec_con5)


############################# Data presentation ################################

library(stargazer)

stargazer(cohort_prov2)

stargazer(cohort_prov3)

stargazer(cohort_prov4)
