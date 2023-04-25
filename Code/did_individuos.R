#Now we will focuss on doing the DiD with individuals in the cohort of treatment 

individuos <- Enemdu[Enemdu$cohort == "17-19",]

individuos <- individuos[individuos$province != "GALAPAGOS",]
individuos <- individuos[individuos$province != "ZONAS NO DELIMITADAS",]

individuos <- merge(individuos, treatment_group, by = "province")

individuos$policy <- rep(0, length(individuos$period))
individuos$policy[individuos$period > 2011]=1

table(individuos$university)
table(individuos$assist_c)

individuos$university <- individuos$university * individuos$university

table(individuos$university)

# First we do the did without controlling for personal characteristics


reg_did_ind1 <- lm(university ~ policy*treat_group, data = individuos)

summary(reg_did_ind1)

#We find that the exam has not a significant effect on the likelihood of being subscribed in the university

reg_did_ind2 <- lm(informal ~ policy*treat_group, data = individuos)

summary(reg_did_ind2)

#We find that the exam has a positive and significant effect on the likelihood of being an informal employee

reg_did_ind3 <- lm(employment ~ policy*treat_group, data = individuos)

summary(reg_did_ind3)

#Indeed we find that the exam has a positive and significant effect on the likelihood of being employed 

################################ Control Variables #############################

reg_did_ind_c1 <- lm(university ~ policy*treat_group + age + sex + indigenous + afro + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = individuos)

summary(reg_did_ind_c1)

#We find that the exam has not a significant effect on the likelihood of being subscribed in the university

reg_did_ind_c2 <- lm(informal ~ policy*treat_group + age + sex + indigenous + afro + formal_per_house + h_tot_labour_income_pc + nhousehold.y +  as.factor(period) + as.factor(province), data = individuos)

summary(reg_did_ind_c2)

#We find that the exam has a positive and significant effect on the likelihood of being an informal employee

reg_did_ind_c3 <- lm(employment ~ policy*treat_group + age + sex + indigenous + afro + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = individuos)

summary(reg_did_ind_c3)

#Indeed we find that the exam has a positive and significant effect on the likelihood of being employed 

############################## Mechanisms #####################################

#In order to check the mechanisms we will add first the effect of being a migrant

migrant_did <- lm(informal ~ policy*treat_group + migrant  + age + sex + indigenous + afro + formal_per_house + h_tot_labour_income_pc + nhousehold.y  + as.factor(period) + as.factor(province), data = individuos)

summary(migrant_did)

# We see that when we include being a migrant the effect of the policy in the likelihood of being informal is lower 
# We can say that the exam has not reduced the likelihood of being in the university but has increased the migration of students

#Now we check the effect of being student and work at the same time 

student_employment_did <- lm(informal ~ policy*treat_group + student_employment  + age + sex + indigenous + afro + formal_per_house + h_tot_labour_income_pc + nhousehold.y  + as.factor(period) + as.factor(province)  , data = individuos)

summary(student_employment_did)

#We see that the effect of the policy on the likelihood of being informal decrease significantly when we add a the student_employment variable

# As last check of the mechanisms we will verify if the policy has an effect on being a migrant and working and studying

migrant_did2 <- lm(migrant ~ policy*treat_group  + age + sex + indigenous + afro + formal_per_house+ h_tot_labour_income_pc + nhousehold.y  + as.factor(period) + as.factor(province), data = individuos)

summary(migrant_did2)

#Now we check the effect of being student and work at the same time 

student_employment_did2 <- lm(student_employment ~ policy*treat_group + age + sex + indigenous + afro + formal_per_house + h_tot_labour_income_pc + nhousehold.y  + as.factor(period) + as.factor(province), data = individuos)

summary(student_employment_did2)

#We conclude that the exam  increased the likelihood of being an student and work at the same time.

student_employment_did3 <- lm(student_employment ~ policy*treat_group + migrant + age + sex + indigenous + afro + formal_per_house + h_tot_labour_income_pc + nhousehold.y  + as.factor(period) + as.factor(province), data = individuos)

summary(student_employment_did3)

#It increased the probability of students to migrate which increased the probability of working informally while they are studying 

######################## Robustness checks ###################################

library(AER)
library(stargazer)

bptest(reg_did_ind1)
bptest(reg_did_ind2)
bptest(reg_did_ind3)

bptest(reg_did_ind_c1)
bptest(reg_did_ind_c2)
bptest(reg_did_ind_c3)

coeftest(reg_did_ind1, vcov=vcovHC)
coeftest(reg_did_ind2, vcov=vcovHC)
coeftest(reg_did_ind3, vcov=vcovHC)

stargazer(coeftest(reg_did_ind_c1, vcov=vcovHC),coeftest(reg_did_ind_c2, vcov=vcovHC),coeftest(reg_did_ind_c3, vcov=vcovHC))
coeftest(reg_did_ind_c2, vcov=vcovHC)
coeftest(reg_did_ind_c3, vcov=vcovHC)

coeftest(migrant_did, vcov=vcovHC)
coeftest(migrant_did2, vcov=vcovHC)

stargazer(coeftest(reg_did_ind_c2, vcov=vcovHC),coeftest(migrant_did, vcov=vcovHC),coeftest(student_employment_did, vcov=vcovHC))

coeftest(student_employment_did, vcov=vcovHC)
coeftest(student_employment_did2, vcov=vcovHC)
coeftest(student_employment_did3, vcov=vcovHC)

#We got the same results when we compute the coefficients robust against heteroscedasticity 

###################### Description of the data ################################


library(xtable)


statistics_years1 <- Enemdu %>%
  group_by(period) %>%
  summarise_at(vars(employment, informal, university),
               mean)


latex_statistics_year <- xtable(statistics_years1, caption = "Data description per year", label = "tab:datayears")

print(latex_statistics_year, include.rownames = FALSE)

statistics_years2 <- Enemdu %>%
  group_by(period) %>%
  summarise_at(vars(age, sex, indigenous, afro , formal_per_house, h_tot_labour_income_pc,nhousehold.y),
               mean)


latex_statistics_year2 <- xtable(statistics_years2, caption = "Data description per year", label = "tab:datayears")

print(latex_statistics_year2, include.rownames = FALSE)



###################### Effect on the policy in quality of universities ########

table(Enemdu$cohort)

#Now we will focus on the cohort of 23 to 25 year. And Evaluate if there is an effect how many people have finished university

individuos2 <- Enemdu[Enemdu$cohort == "23-25",]

individuos2 <- individuos2[individuos2$province != "GALAPAGOS",]
individuos2 <- individuos2[individuos2$province != "ZONAS NO DELIMITADAS",]

individuos2 <- merge(individuos2, treatment_group, by = "province")

#Now we are going to evaluate the effect since 2012+ 5 year, the time that the student that entered with the senescyt should finished the university

individuos2$policy <- rep(0, length(individuos2$period))
individuos2$policy[individuos2$period > 2016]=1

table(individuos2$education_y)

#People that have more that 16 years of studies should have finished the university

individuos2$finished_u <- rep(0,length(individuos2$education_y))
individuos2$finished_u[individuos2$education_y>12]=1

table(individuos2$finished_u)

#Now we evaluate the effect of the policy with a DID


univ_quality <- lm(finished_u ~ policy*treat_group + age + sex + indigenous + afro + formal_per_house + h_tot_labour_income_pc + nhousehold.y , data = individuos2)

summary(univ_quality)


sum(individuos$highschool)/nrow(individuos$highschool)

sum(individuos$migrant)/68731


nrow(individuos$highschool)










