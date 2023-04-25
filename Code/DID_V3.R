#Now we will focuss on doing the DiD. The treatment group will be people of 17-19 years
#The control group will be people of 20 - 22 years because they do not pass the exam



individuos1722 <- Enemdu[Enemdu$cohort == "17-19" | Enemdu$cohort == "20-22" ,]
individuos1722 <- individuos1722[individuos1722$education > 6,]

individuos1722$migrant_student <- individuos1722$migrant * individuos1722$university

individuos1722 <- individuos1722[individuos1722$province != "GALAPAGOS",]
individuos1722 <- individuos1722[individuos1722$province != "ZONAS NO DELIMITADAS",]

#We create two data sets one for the did from 2009 to 2015
#Another to the parallel trends assumption from 2007 to 2015

individuos17222 <- individuos1722[individuos1722$period < 2016,]

individuos1722 <- individuos1722[individuos1722$period > 2008 & individuos1722$period < 2017,]

individuos1722$treat_group <- rep(0, length(individuos1722$employment))
individuos1722$treat_group[individuos1722$cohort_17_19 == 1] = 1


individuos1722$policy <- rep(0, length(individuos1722$period))
individuos1722$policy[individuos1722$period > 2011]=1


############################# Descriptive Statistics ##########################

desc_stat <- individuos1722 %>%
  group_by(period) %>%
  summarise_at(vars(employment, university, migrant),
               mean)

empleados <- individuos1722[individuos1722$employment == 1,]

desc_stat_inf <- empleados %>%
  group_by(period) %>%
  summarise_at(vars(informal),
               mean)

universitarios <- individuos1722[individuos1722$university==1,]

migrantes <- universitarios %>%
  group_by(period) %>%
  summarise_at(vars(migrant),
               mean)

desc_stat <- merge(desc_stat, desc_stat_inf, by = "period")

desc_stat$migrant_student <- migrantes$migrant


attach(desc_stat)

jpeg("Senecyt_stats.jpg")

par(mfrow = c(2,2))

plot(period, employment, type = "l")
plot(period, informal, type = "l")
plot(period, university, type = "l")
plot(period, migrant_student, type = "l")

dev.off()


#################################  DID  #########################################

library(stargazer)

# First we do the did without controlling for personal characteristics


reg_did_ind1 <- lm(university ~ policy*treat_group, data = individuos1722)

stargazer(reg_did_ind1, type = "text")

#We find that the exam has not a significant effect on the likelihood of being subscribed in the university

reg_did_ind2 <- lm(informal ~ policy*treat_group, data = individuos1722)

stargazer(reg_did_ind2, type = "text")

#We find that the exam has a positive and significant effect on the likelihood of being an informal employee

reg_did_ind3 <- lm(employment ~ policy*treat_group, data = individuos1722)

stargazer(reg_did_ind3, type = "text")

#Indeed we find that the exam has a positive and significant effect on the likelihood of being employed 

################################ Control Variables #############################

reg2_did_ind_c1 <- lm(university ~ policy*treat_group + age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = individuos1722)

stargazer(reg2_did_ind_c1, type = "text")

#We find that the exam has not a significant effect on the likelihood of being subscribed in the university

reg2_did_ind_c2 <- lm(informal ~ policy*treat_group + age + sex + indigenous + afro + rural + formal_per_house + h_tot_labour_income_pc + nhousehold.y +  as.factor(period) + as.factor(province), data = individuos1722)

stargazer(reg2_did_ind_c2, type = "text")

#We find that the exam has a positive and significant effect on the likelihood of being an informal employee

reg2_did_ind_c3 <- lm(employment ~ policy*treat_group + age + sex + indigenous + afro + rural + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = individuos1722)

stargazer(reg2_did_ind_c3, type = "text")

#Indeed we find that the exam has a positive and significant effect on the likelihood of being employed 

############################## Mechanisms #####################################

#In order to check the mechanisms we will add first the effect of being a migrant

migrant_did <- lm(informal ~ policy*treat_group + migrant  + age + sex + indigenous + afro + rural + formal_per_house + h_tot_labour_income_pc + nhousehold.y  + as.factor(period) + as.factor(province), data = individuos1722)

stargazer(migrant_did, type = "text")

# We see that when we include being a migrant the effect of the policy in the likelihood of being informal is lower 
# We can say that the exam has not reduced the likelihood of being in the university but has increased the migration of students


migrant_did2 <- lm(informal ~ policy*treat_group + migrant_student   + age + sex + indigenous + afro + rural + formal_per_house+ h_tot_labour_income_pc + nhousehold.y  + as.factor(period) + as.factor(province), data = individuos1722)

stargazer(migrant_did2, type = "text")

migrant_did3 <- lm(migrant ~ policy*treat_group + age + sex + indigenous + afro + rural + formal_per_house+ h_tot_labour_income_pc + nhousehold.y  + as.factor(period) + as.factor(province), data = individuos1722)

stargazer(migrant_did3, type = "text")


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

stargazer(coeftest(reg_did_ind_c1, vcov=vcovHC),coeftest(reg_did_ind_c2, vcov=vcovHC),coeftest(reg_did_ind_c3, vcov=vcovHC), type = "text")
coeftest(reg_did_ind_c2, vcov=vcovHC)
coeftest(reg_did_ind_c3, vcov=vcovHC)

coeftest(migrant_did, vcov=vcovHC)
coeftest(migrant_did2, vcov=vcovHC)

stargazer(coeftest(reg_did_ind_c2, vcov=vcovHC),coeftest(migrant_did, vcov=vcovHC),coeftest(migrant_did2, vcov=vcovHC), type = "text")

stargazer(coeftest(migrant_did3, vcov=vcovHC),type = "text")

#We got the same results when we compute the coefficients robust against heteroscedasticity 

############################## Sub population models ##########################

#Now we estimate our model for many subpopulations : women\men, rural\urban, and finalyy we will do a quantile regression

#Women

mujeres <- individuos1722[individuos1722$sex == 1,]

did_mujeres1 <- lm(university ~ policy*treat_group + age +  indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = mujeres)

did_mujeres2 <- lm(informal ~ policy*treat_group + age + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = mujeres)

did_mujeres3 <- lm(employment ~ policy*treat_group + age +  indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = mujeres)


stargazer(coeftest(did_mujeres1, vcov=vcovHC),coeftest(did_mujeres2, vcov=vcovHC),coeftest(did_mujeres3, vcov=vcovHC), type = "text")


#men


hombres <- individuos1722[individuos1722$sex == 0,]

did_hombres1 <- lm(university ~ policy*treat_group + age +  indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = hombres)

did_hombres2 <- lm(informal ~ policy*treat_group + age + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = hombres)

did_hombres3 <- lm(employment ~ policy*treat_group + age +  indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = hombres)


stargazer(coeftest(did_hombres1, vcov=vcovHC),coeftest(did_hombres2, vcov=vcovHC),coeftest(did_hombres3, vcov=vcovHC), type = "text")


#rural/urban

rural <- individuos1722[individuos1722$rural == 1,]

did_rural1 <- lm(university ~ policy*treat_group + age +  indigenous + afro + sex  + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = rural)

did_rural2 <- lm(informal ~ policy*treat_group + age + indigenous + afro + sex  + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = rural)

did_rural3 <- lm(employment ~ policy*treat_group + age +  indigenous + sex + afro  + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = rural)


stargazer(coeftest(did_rural1, vcov=vcovHC),coeftest(did_rural2, vcov=vcovHC),coeftest(did_rural3, vcov=vcovHC), type = "text")

urban <- individuos1722[individuos1722$rural == 0,]

did_urban1 <- lm(university ~ policy*treat_group + age +  indigenous + afro + sex  + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = urban)

did_urban2 <- lm(informal ~ policy*treat_group + age + indigenous + afro + sex  + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = urban)

did_urban3 <- lm(employment ~ policy*treat_group + age +  indigenous + sex + sex + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = urban)


stargazer(coeftest(did_urban1, vcov=vcovHC),coeftest(did_urban2, vcov=vcovHC),coeftest(did_urban3, vcov=vcovHC), type = "text")


#quantile regession

# Load the quantreg package
library(quantreg)

qreg <- rq(informal ~  policy*treat_group + age + sex +  h_tot_labour_income_pc  , data = individuos1722, tau = 0.3)

summary(qreg)


#################### Parallel Trend Assumption ###############################

before <- individuos17222[individuos17222$period < 2012,]
after <- individuos17222[individuos17222$period > 2011,]

#PTA for university

after_did1 <- lm(university ~ treat_group + age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = after)

stargazer(after_did1, type = "text")

before_did1 <- lm(university ~ treat_group + age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = before)

stargazer(before_did2, type = "text")

stargazer(coeftest(before_did1, vcov=vcovHC),coeftest(after_did1, vcov=vcovHC), type = "text")

#For university the PTA is not meet

after_did2 <- lm(informal ~ treat_group + age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = after)

stargazer(after_did2, type = "text")

before_did2 <- lm(informal ~ treat_group + age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = before)

stargazer(before_did2, type = "text")

stargazer(coeftest(before_did2, vcov=vcovHC),coeftest(after_did2, vcov=vcovHC), type = "text")


# For informal employment the PTA is meet 

after_did3 <- lm(employment ~ treat_group + age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = after)

stargazer(after_did3, type = "text")

before_did3 <- lm(employment ~ treat_group + age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = before)

stargazer(before_did3, type = "text")

stargazer(coeftest(before_did3, vcov=vcovHC),coeftest(after_did3, vcov=vcovHC), type = "text")