library(feather)
library(readr)
library(dplyr)
library(ineq)
library(stargazer)
library(AER)


treatment_group <- read_csv("~/PHD/DB MINEDUC/Clean_MINEDUC/treatment_group.csv")
Enemdu <- read_feather("Enemdu.feather")

#Now we will focuss on doing the DiD with individuals in the cohort of treatment 
#We will keep people that have completed high school between 17 - 19 years

individuos <- Enemdu[Enemdu$cohort == "17-19",]
individuos <- individuos[individuos$education > 6,]

individuos$migrant_student <- individuos$migrant * individuos$university

individuos <- individuos[individuos$province != "GALAPAGOS",]
individuos <- individuos[individuos$province != "ZONAS NO DELIMITADAS",]

remove(Enemdu)

#We create two data sets one for the did from 2009 to 2015
#Another to the parallel trends assumption from 2007 to 2015

individuos2 <- individuos[individuos$period < 2017,]

individuos <- individuos[individuos$period > 2008 & individuos$period < 2017,]

individuos <- merge(individuos, treatment_group, by = "province")
individuos2 <- merge(individuos2, treatment_group, by = "province")

individuos$policy <- rep(0, length(individuos$period))
individuos$policy[individuos$period > 2011]=1

individuos2$policy <- rep(0, length(individuos2$period))
individuos2$policy[individuos2$period > 2011]=1

individuos$lformal_per_house <- ifelse(individuos$formal_per_house < 1, 0 ,log(individuos$formal_per_house))

individuos$lh_tot_labour_income_pc <- ifelse(individuos$h_tot_labour_income_pc < 1, 0 ,log(individuos$h_tot_labour_income_pc))

individuos$lnhousehold.y <- ifelse(individuos$nhousehold.y < 1, 0 ,log(individuos$nhousehold.y))


remove(Enemdu)

############################# Descriptive Statistics ##########################

desc_stat <- individuos %>%
  group_by(period) %>%
  summarise_at(vars(employment, university, migrant),
               mean)

empleados <- individuos[individuos$employment == 1,]

desc_stat_inf <- empleados %>%
  group_by(period) %>%
  summarise_at(vars(informal),
               mean)

universitarios <- individuos[individuos$university==1,]

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


remove(desc_stat)
remove(empleados)
remove(migrantes)
remove(desc_stat_inf)
remove(universitarios)

#################################  DID  #########################################

library(stargazer)

# First we do the did without controlling for personal characteristics


reg_did_ind1 <- lm(university ~ policy*treat_group, data = individuos)

stargazer(reg_did_ind1, type = "text")

#We find that the exam has not a significant effect on the likelihood of being subscribed in the university

reg_did_ind2 <- lm(informal ~ policy*treat_group, data = individuos)

stargazer(reg_did_ind2, type = "text")

#We find that the exam has a positive and significant effect on the likelihood of being an informal employee

reg_did_ind3 <- lm(employment ~ policy*treat_group, data = individuos)

stargazer(reg_did_ind3, type = "text")

#Indeed we find that the exam has a positive and significant effect on the likelihood of being employed 

################################ Control Variables #############################

reg_did_ind_c1 <- lm(university ~ policy*treat_group + age + sex + indigenous + afro + rural + lformal_per_house  + lh_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = individuos)

stargazer(reg_did_ind_c1, type = "text")

#We find that the exam has not a significant effect on the likelihood of being subscribed in the university

reg_did_ind_c2 <- lm(informal ~ policy*treat_group + age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc + nhousehold.y+   as.factor(period) + as.factor(province), data = individuos)

stargazer(reg_did_ind_c2, type = "text")

#We find that the exam has a positive and significant effect on the likelihood of being an informal employee

reg_did_ind_c3 <- lm(employment ~ policy*treat_group + age + sex + indigenous + afro + rural + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = individuos)

stargazer(reg_did_ind_c3, type = "text")

#Indeed we find that the exam has a positive and significant effect on the likelihood of being employed 

############################## Mechanisms #####################################

#In order to check the mechanisms we will add first the effect of being a migrant

migrant_did <- lm(informal ~ policy*treat_group + migrant  + age + sex + indigenous + afro + rural + formal_per_house + h_tot_labour_income_pc + nhousehold.y  + as.factor(period) + as.factor(province), data = individuos)

stargazer(reg_did_ind_c2, migrant_did)

# We see that when we include being a migrant the effect of the policy in the likelihood of being informal is lower 
# We can say that the exam has not reduced the likelihood of being in the university but has increased the migration of students


migrant_did2 <- lm(informal ~ policy*treat_group + migrant + age + sex + indigenous + afro + rural + formal_per_house+ h_tot_labour_income_pc + nhousehold.y  + as.factor(period) + as.factor(province), data = individuos)

stargazer(migrant_did2, type = "text")

migrant_did3 <- lm(migrant ~ policy*treat_group + age + sex + indigenous + afro + rural + formal_per_house+ h_tot_labour_income_pc + nhousehold.y  + as.factor(period) + as.factor(province), data = individuos)

stargazer(migrant_did3, type = "text")

migrant_did4 <- lm(migrant_student ~ policy*treat_group + age + sex + indigenous + afro + rural + formal_per_house+ h_tot_labour_income_pc + nhousehold.y  + as.factor(period) + as.factor(province), data = individuos)

stargazer(migrant_did4, type = "text")


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
stargazer(coeftest(reg_did_ind_c1, vcov=vcovHC),coeftest(reg_did_ind_c2, vcov=vcovHC),coeftest(reg_did_ind_c3, vcov=vcovHC))
coeftest(reg_did_ind_c2, vcov=vcovHC)
coeftest(reg_did_ind_c3, vcov=vcovHC)

coeftest(migrant_did, vcov=vcovHC)
coeftest(migrant_did2, vcov=vcovHC)

stargazer(coeftest(reg_did_ind_c2, vcov=vcovHC),coeftest(migrant_did, vcov=vcovHC), type = "text")

stargazer(coeftest(migrant_did3, vcov=vcovHC))

#We got the same results when we compute the coefficients robust against heteroscedasticity 

############################## Sub population models ##########################

#Now we estimate our model for many subpopulations : women\men, rural\urban, and finalyy we will do a quantile regression

#Women

mujeres <- individuos[individuos$sex == 1,]

did_mujeres1 <- lm(university ~ policy*treat_group + age +  indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = mujeres)

did_mujeres2 <- lm(informal ~ policy*treat_group + age + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = mujeres)

did_mujeres3 <- lm(employment ~ policy*treat_group + age +  indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = mujeres)


stargazer(coeftest(did_mujeres1, vcov=vcovHC),coeftest(did_mujeres2, vcov=vcovHC),coeftest(did_mujeres3, vcov=vcovHC), type = "text")


#men


hombres <- individuos[individuos$sex == 0,]

did_hombres1 <- lm(university ~ policy*treat_group + age +  indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = hombres)

did_hombres2 <- lm(informal ~ policy*treat_group + age + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = hombres)

did_hombres3 <- lm(employment ~ policy*treat_group + age +  indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = hombres)


stargazer(coeftest(did_hombres1, vcov=vcovHC),coeftest(did_hombres2, vcov=vcovHC),coeftest(did_hombres3, vcov=vcovHC), type = "text")


#rural/urban

rural <- individuos[individuos$rural == 1,]

did_rural1 <- lm(university ~ policy*treat_group + age +  indigenous + afro + sex  + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = rural)

did_rural2 <- lm(informal ~ policy*treat_group + age + indigenous + afro + sex  + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = rural)

did_rural3 <- lm(employment ~ policy*treat_group + age +  indigenous + sex + afro  + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = rural)


stargazer(coeftest(did_rural1, vcov=vcovHC),coeftest(did_rural2, vcov=vcovHC),coeftest(did_rural3, vcov=vcovHC), type = "text")

urban <- individuos[individuos$rural == 0,]

did_urban1 <- lm(university ~ policy*treat_group + age +  indigenous + afro + sex  + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = urban)

did_urban2 <- lm(informal ~ policy*treat_group + age + indigenous + afro + sex  + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = urban)

did_urban3 <- lm(employment ~ policy*treat_group + age +  indigenous + sex + sex + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = urban)


stargazer(coeftest(did_urban1, vcov=vcovHC),coeftest(did_urban2, vcov=vcovHC),coeftest(did_urban3, vcov=vcovHC), type = "text")

#Indígenas 

indigenas <- individuos[individuos$indigenous == 1,]

did_indigenas1 <- lm(university ~ policy*treat_group + age + rural + afro + sex  + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = indigenas)

did_indigenas2 <- lm(informal ~ policy*treat_group + age  + afro +rural + sex  + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = indigenas)

did_indigenas3 <- lm(employment ~ policy*treat_group + age + afro + rural + sex + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = indigenas)


stargazer(coeftest(did_indigenas1, vcov=vcovHC),coeftest(did_indigenas2, vcov=vcovHC),coeftest(did_indigenas3, vcov=vcovHC), type = "text")

#No indigenas 

noindigenas <- individuos[individuos$indigenous == 0,]

did_noindigenas1 <- lm(university ~ policy*treat_group + age + rural + afro + sex  + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = noindigenas)

did_noindigenas2 <- lm(informal ~ policy*treat_group + age  + afro +rural + sex  + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = noindigenas)

did_noindigenas3 <- lm(employment ~ policy*treat_group + age + afro + rural + sex + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = noindigenas)


stargazer(coeftest(did_noindigenas1, vcov=vcovHC),coeftest(did_noindigenas2, vcov=vcovHC),coeftest(did_noindigenas3, vcov=vcovHC), type = "text")


#########################Regrssion on different quintiles ######################

#Q1

table(individuos$quintil)

Q1 <- individuos[individuos$quintil == 1 ,]

summary(Q1$h_tot_labour_income_pc)

did_q11 <- lm(university ~ policy*treat_group + age +  indigenous + afro + sex + rural + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = Q1)

did_q12 <- lm(informal ~ policy*treat_group + age + indigenous + afro + sex + rural + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = Q1)

did_q13 <- lm(employment ~ policy*treat_group + age +  indigenous + sex + sex + rural + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = Q1)


stargazer(coeftest(did_q11, vcov=vcovHC),coeftest(did_q12, vcov=vcovHC),coeftest(did_q13, vcov=vcovHC), type = "text")

stargazer(coeftest(did_q11, vcov=vcovHC),coeftest(did_q12, vcov=vcovHC),coeftest(did_q13, vcov=vcovHC), type = "text")

table(Q2$policy*Q2$treat_group)

#Q2

Q2 <- individuos[individuos$quintil == 2 ,]

summary(Q2$h_tot_labour_income_pc)

did_q21 <- lm(university ~ policy*treat_group + age +  indigenous + afro + sex + rural + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = Q2)

did_q22 <- lm(informal ~ policy*treat_group + age + indigenous + afro + sex + rural + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = Q2)

did_q23 <- lm(employment ~ policy*treat_group + age +  indigenous + sex + sex + rural + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = Q2)


stargazer(coeftest(did_q21, vcov=vcovHC),coeftest(did_q22, vcov=vcovHC),coeftest(did_q23, vcov=vcovHC), type = "text")

table(Q2$policy*Q2$treat_group)


#Q3

Q3 <- individuos[individuos$quintil == 3 ,]

summary(Q3$h_tot_labour_income_pc)


did_q31 <- lm(university ~ policy*treat_group + age +  indigenous + afro + sex + rural + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = Q3)

did_q32 <- lm(informal ~ policy*treat_group + age + indigenous + afro + sex + rural + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = Q3)

did_q33 <- lm(employment ~ policy*treat_group + age +  indigenous + sex + sex + rural + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = Q3)


stargazer(coeftest(did_q31, vcov=vcovHC),coeftest(did_q32, vcov=vcovHC),coeftest(did_q33, vcov=vcovHC), type = "text")

table(Q3$policy*Q3$treat_group)

#Q4

Q4 <- individuos[individuos$quintil == 4 ,]

summary(Q4$h_tot_labour_income_pc)

did_Q41 <- lm(university ~ policy*treat_group + age +  indigenous + afro + sex + rural + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = Q4)

did_Q42 <- lm(informal ~ policy*treat_group + age + indigenous + afro + sex + rural + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = Q4)

did_Q43 <- lm(employment ~ policy*treat_group + age +  indigenous + sex + sex + rural + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = Q4)


stargazer(coeftest(did_Q41, vcov=vcovHC),coeftest(did_Q42, vcov=vcovHC),coeftest(did_Q43, vcov=vcovHC), type = "text")

table(Q4$policy*Q4$treat_group)

#Q5

Q5 <- individuos[individuos$quintil == 5 ,]

summary(Q5$h_tot_labour_income_pc)

did_Q51 <- lm(university ~ policy*treat_group + age +  indigenous + afro + sex + rural + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = Q5)

did_Q52 <- lm(informal ~ policy*treat_group + age + indigenous + afro + sex + rural + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = Q5)

did_Q53 <- lm(employment ~ policy*treat_group + age +  indigenous + sex + sex + rural + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = Q5)


stargazer(coeftest(did_Q51, vcov=vcovHC),coeftest(did_Q52, vcov=vcovHC),coeftest(did_Q53, vcov=vcovHC), type = "text")

table(Q5$policy*Q5$treat_group)

####################### Dummy per quintil ########################

#Q1

individuos$Q1 <- rep(0,length(individuos$province))
individuos$Q1[individuos$quintil == 1]=1

table(individuos$Q1)

did_q11 <- lm(university ~ policy*treat_group + age +  indigenous + afro + sex + rural + formal_per_house + nhousehold.y + as.factor(period) + as.factor(province)+ Q1, data = individuos)

did_q12 <- lm(informal ~ policy*treat_group + age + indigenous + afro + sex + rural + formal_per_house  + nhousehold.y + as.factor(period) + as.factor(province)+ Q1, data = individuos)

did_q13 <- lm(employment ~ policy*treat_group + age +  indigenous + sex + sex + rural + formal_per_house  + nhousehold.y + as.factor(period) + as.factor(province)+ Q1, data = individuos)


stargazer(coeftest(did_q11, vcov=vcovHC),coeftest(did_q12, vcov=vcovHC),coeftest(did_q13, vcov=vcovHC), type = "text")

#Q2

individuos$Q2 <- rep(0,length(individuos$province))
individuos$Q2[individuos$quintil == 2]=1

table(individuos$Q2)

did_q21 <- lm(university ~ policy*treat_group + age +  indigenous + afro + sex + rural + formal_per_house + nhousehold.y + as.factor(period) + as.factor(province)+ Q2, data = individuos)

did_q22 <- lm(informal ~ policy*treat_group + age + indigenous + afro + sex + rural + formal_per_house  + nhousehold.y + as.factor(period) + as.factor(province)+ Q2, data = individuos)

did_q23 <- lm(employment ~ policy*treat_group + age +  indigenous + sex + sex + rural + formal_per_house  + nhousehold.y + as.factor(period) + as.factor(province)+ Q2, data = individuos)


stargazer(coeftest(did_q21, vcov=vcovHC),coeftest(did_q22, vcov=vcovHC),coeftest(did_q23, vcov=vcovHC), type = "text")

#Q3

individuos$Q3 <- rep(0,length(individuos$province))
individuos$Q3[individuos$quintil == 3]=1

table(individuos$Q3)

did_q31 <- lm(university ~ policy*treat_group + age +  indigenous + afro + sex + rural + formal_per_house + nhousehold.y + as.factor(period) + as.factor(province)+ Q3, data = individuos)

did_q32 <- lm(informal ~ policy*treat_group + age + indigenous + afro + sex + rural + formal_per_house  + nhousehold.y + as.factor(period) + as.factor(province)+ Q3, data = individuos)

did_q33 <- lm(employment ~ policy*treat_group + age +  indigenous + sex + sex + rural + formal_per_house  + nhousehold.y + as.factor(period) + as.factor(province)+ Q3, data = individuos)


stargazer(coeftest(did_q31, vcov=vcovHC),coeftest(did_q32, vcov=vcovHC),coeftest(did_q33, vcov=vcovHC), type = "text")

#Q4

individuos$Q4 <- rep(0,length(individuos$province))
individuos$Q4[individuos$quintil == 4]=1

table(individuos$Q4)

did_q41 <- lm(university ~ policy*treat_group + age +  indigenous + afro + sex + rural + formal_per_house + nhousehold.y + as.factor(period) + as.factor(province)+ Q4, data = individuos)

did_q42 <- lm(informal ~ policy*treat_group + age + indigenous + afro + sex + rural + formal_per_house  + nhousehold.y + as.factor(period) + as.factor(province)+ Q4, data = individuos)

did_q43 <- lm(employment ~ policy*treat_group + age +  indigenous + sex + sex + rural + formal_per_house  + nhousehold.y + as.factor(period) + as.factor(province)+ Q4, data = individuos)


stargazer(coeftest(did_q41, vcov=vcovHC),coeftest(did_q42, vcov=vcovHC),coeftest(did_q43, vcov=vcovHC), type = "text")

#Q5

individuos$Q5 <- rep(0,length(individuos$province))
individuos$Q5[individuos$quintil == 5]=1

table(individuos$Q5)

did_q51 <- lm(university ~ policy*treat_group + age +  indigenous + afro + sex + rural + formal_per_house + nhousehold.y + as.factor(period) + as.factor(province)+ Q5, data = individuos)

did_q52 <- lm(informal ~ policy*treat_group + age + indigenous + afro + sex + rural + formal_per_house  + nhousehold.y + as.factor(period) + as.factor(province)+ Q5, data = individuos)

did_q53 <- lm(employment ~ policy*treat_group + age +  indigenous + sex + sex + rural + formal_per_house  + nhousehold.y + as.factor(period) + as.factor(province)+ Q5, data = individuos)


stargazer(coeftest(did_q51, vcov=vcovHC),coeftest(did_q52, vcov=vcovHC),coeftest(did_q53, vcov=vcovHC), type = "text")

#################### Parallel Trend Assumption  ###############################

PTA <- individuos2[individuos2$period < 2012,]

table(PTA$period)


PTA$policy <- rep(0, length(PTA$policy))
PTA$policy[PTA$period>2009]=1
table(PTA$policy)

PTA_reg1 <- lm(university ~ policy*treat_group + age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = PTA)

PTA_reg2 <- lm(informal ~ policy*treat_group + age + sex + indigenous + afro + rural + formal_per_house + h_tot_labour_income_pc + nhousehold.y +  as.factor(period) + as.factor(province), data = PTA)

PTA_reg3 <- lm(employment ~ policy*treat_group + age + sex + indigenous + afro + rural + formal_per_house + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = PTA)

PTA_reg4 <- lm(migrant ~ policy*treat_group + age + sex + indigenous + afro + rural + formal_per_house+ h_tot_labour_income_pc + nhousehold.y  + as.factor(period) + as.factor(province), data = PTA)


stargazer(coeftest(PTA_reg1, vcov=vcovHC),coeftest(PTA_reg2, vcov=vcovHC),coeftest(PTA_reg3, vcov=vcovHC),coeftest(PTA_reg4, vcov=vcovHC))

# The Parallel Trend Assumption is respected in the four models

names(individuos)

###############################################################################

#Let's change the treatment group for those families where there in the family no one went to univerisity 

#Dummy equal to one if there is a person with univserisity in the family

univ_family <- Enemdu %>%
  group_by(id_hogar) %>%
  summarise_at(vars(university),
               sum) %>%
  ungroup()

univ_family$univ_family <- rep(0, length(univ_family$id_hogar))
univ_family$univ_family[univ_family$university >= 1] =1

names(univ_family) <- c("id_hogar", "univ_family2", "univ_family")

Enemdu2 <- merge(Enemdu,univ_family, by = "id_hogar")

table(Enemdu2$univ_family)

#Now we keep only the individuals we want to evaluate

#We will keep people that have completed high school between 17 - 19 years

individuos3 <- Enemdu2[Enemdu2$cohort == "17-19",]
individuos3 <- individuos3[individuos3$education > 6,]

individuos3$migrant_student <- individuos3$migrant * individuos3$university

individuos3 <- individuos3[individuos3$province != "GALAPAGOS",]
individuos3 <- individuos3[individuos3$province != "ZONAS NO DELIMITADAS",]

#We create two data sets one for the did from 2009 to 2015
#Another to the parallel trends assumption from 2007 to 2015

individuos4 <- individuos3[individuos$period < 2017,]

individuos3 <- individuos3[individuos$period > 2008 & individuos$period < 2017,]

individuos3$treat <- rep(1, length(individuos3$univ_family))
individuos3$treat <- individuos3$treat - individuos3$univ_family

individuos4$treat <- rep(1, length(individuos4$univ_family))
individuos4$treat <- individuos4$treat - individuos4$univ_family


individuos3 <- merge(individuos3, treatment_group, by = "province")
individuos4 <- merge(individuos4, treatment_group, by = "province")

individuos3$policy <- rep(0, length(individuos3$period))
individuos3$policy[individuos3$period > 2011]=1

individuos4$policy <- rep(0, length(individuos4$period))
individuos4$policy[individuos4$period > 2008]=1

individuos3$lformal_per_house <- ifelse(individuos3$formal_per_house < 1, 0 ,log(individuos3$formal_per_house))

individuos3$lh_tot_labour_income_pc <- ifelse(individuos3$h_tot_labour_income_pc < 1, 0 ,log(individuos3$h_tot_labour_income_pc))

individuos3$lnhousehold.y <- ifelse(individuos3$nhousehold.y < 1, 0 ,log(individuos3$nhousehold.y))

individuos4$lformal_per_house <- ifelse(individuos4$formal_per_house < 1, 0 ,log(individuos4$formal_per_house))

individuos4$lh_tot_labour_income_pc <- ifelse(individuos4$h_tot_labour_income_pc < 1, 0 ,log(individuos4$h_tot_labour_income_pc))

individuos4$lnhousehold.y <- ifelse(individuos4$nhousehold.y < 1, 0 ,log(individuos4$nhousehold.y))

table(individuos3$treat)

# DID

fam_univ_reg1 <- lm(university ~ policy*treat + age + sex + indigenous + afro + rural + lh_tot_labour_income_pc + lnhousehold.y + as.factor(period) + as.factor(province), data = individuos3)


stargazer(fam_univ_reg1 , type = "text")


fam_univ_reg2 <- lm(informal ~ policy*treat + age + sex + indigenous + afro + rural  + h_tot_labour_income_pc + nhousehold.y+   as.factor(period) + as.factor(province), data = individuos3)

stargazer(fam_univ_reg2, type = "text")


fam_univ_reg3 <- lm(employment ~ policy*treat+ age + sex + indigenous + afro + rural  + h_tot_labour_income_pc + nhousehold.y + as.factor(period) + as.factor(province), data = individuos3)

stargazer(fam_univ_reg3, type = "text")

stargazer(coeftest(fam_univ_reg1, vcov=vcovHC),coeftest(fam_univ_reg2, vcov=vcovHC),coeftest(fam_univ_reg3, vcov=vcovHC), type = "text")


#################### Parallel Trend Assumption  ###############################

PTA2 <- individuos4[individuos4$period < 2012,]

table(PTA2$period)


PTA2$policy <- rep(0, length(PTA2$policy))
PTA2$policy[PTA2$period>2009]=1
table(PTA2$policy)

PTA2_reg1 <- lm(university ~ policy*treat_group*treat + age + sex + indigenous + afro + rural  + lformal_per_house + lh_tot_labour_income_pc + lnhousehold.y + as.factor(period) + as.factor(province), data = PTA2)

PTA2_reg2 <- lm(informal ~ policy*treat_group*treat + age + sex + indigenous + afro + rural + lformal_per_house + lh_tot_labour_income_pc + lnhousehold.y +  as.factor(period) + as.factor(province), data = PTA2)

PTA2_reg3 <- lm(employment ~ policy*treat_group*treat + age + sex + indigenous + afro + rural + lformal_per_house + lh_tot_labour_income_pc + lnhousehold.y + as.factor(period) + as.factor(province), data = PTA2)

PTA2_reg4 <- lm(migrant ~ policy*treat_group*treat + age + sex + indigenous + afro + rural + lformal_per_house+ lh_tot_labour_income_pc + lnhousehold.y  + as.factor(period) + as.factor(province), data = PTA2)


stargazer(coeftest(PTA2_reg1, vcov=vcovHC),coeftest(PTA2_reg2, vcov=vcovHC),coeftest(PTA2_reg3, vcov=vcovHC),coeftest(PTA2_reg4, vcov=vcovHC), type = "text")

