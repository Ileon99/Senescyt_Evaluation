library(feather)
library(readr)
library(dplyr)
library(ineq)
library(stargazer)
library(AER)
library(stringi)


treatment_group <- read_feather("C:/Users/ignac/OneDrive/Documentos/PHD/Senescyt_Evaluation/Data/treatment_univ.feather")
Enemdu <- read_feather("C:/Users/ignac/OneDrive/Documentos/PHD/Senescyt_Evaluation/Data/Enemdu.feather")
prov_univ <- read_feather("C:/Users/ignac/OneDrive/Documentos/PHD/Senescyt_Evaluation/Data/prov_univ.feather")


#Now we will focuss on doing the DiD with individuals in the cohort of treatment 
#We will keep people that have completed high school between 17 - 19 years

#Adding as a control the number of universities 


treatment_group <- treatment_group[, c("province","treat_group")]

treatment_group$treat_group <- ifelse(treatment_group$treat_group == 1, 0, 1)

individuos <- Enemdu[Enemdu$age > 16 & Enemdu$age < 22,]
individuos <- individuos[individuos$education > 6,]

individuos$migrant_student <- individuos$migrant * individuos$university

individuos <- individuos[individuos$province != "GALAPAGOS",]
individuos <- individuos[individuos$province != "ZONAS NO DELIMITADAS",]

#We create a data set of 2012 when people of 17-18 were affected by the exam and 19-20 that were not affected by the law 

individuos <- individuos[ individuos$period > 2011 & individuos$period < 2014,]

individuos <- merge(individuos, treatment_group, by = "province")

individuos$policy <- rep(0, length(individuos$period))
individuos$policy[individuos$period == 2012 & individuos$age < 19
                  | individuos$period == 2013 & individuos$age < 20]=1


individuos$lformal_per_house <- ifelse(individuos$formal_per_house < 1, 0 ,log(individuos$formal_per_house))

individuos$lh_tot_labour_income_pc <- ifelse(individuos$h_tot_labour_income_pc < 1, 0 ,log(individuos$h_tot_labour_income_pc))

individuos$lnhousehold.y <- ifelse(individuos$nhousehold.y < 1, 0 ,log(individuos$nhousehold.y))


remove(Enemdu)
remove(treatment_group)

table(individuos$treat_group, individuos$province)

################################ Control Variables (Provincias) #############################

reg_did_ind_c1 <- lm(university ~ policy * treat_group +
                       age + sex + indigenous + afro + rural + formal_per_house  + h_tot_labour_income_pc +
                       h_little_kids + h_kid + h_teen + h_adult + nhousehold.y + educ_house + 
                       as.factor(period) + as.factor(province),
                     data = individuos)

stargazer(reg_did_ind_c1, type = "text")

#We find that the exam has not a significant effect on the likelihood of being subscribed in the university

reg_did_ind_c2 <- lm(informal ~ policy*treat_group +
                       age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                       h_little_kids + h_kid + h_teen + h_adult + nhousehold.y + educ_house +
                       as.factor(period) + as.factor(province),
                     data = individuos)

stargazer(reg_did_ind_c2, type = "text")

#We find that the exam has a positive and significant effect on the likelihood of being an informal employee

reg_did_ind_c3 <- lm(employment ~ policy*treat_group +
                       age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                       h_little_kids + h_kid + h_teen + h_adult + nhousehold.y + educ_house +
                       as.factor(period) + as.factor(province),
                     data = individuos)

stargazer(reg_did_ind_c3, type = "text")


reg_did_ind_c4 <- lm(migrant ~ policy*treat_group +
                       age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                       h_little_kids + h_kid + h_teen + h_adult + nhousehold.y + educ_house +
                       as.factor(period) + as.factor(province),
                     data = individuos)

stargazer(reg_did_ind_c4, type = "text")


stargazer(coeftest(reg_did_ind_c1, vcov=vcovHC),coeftest(reg_did_ind_c3, vcov=vcovHC), coeftest(reg_did_ind_c2, vcov=vcovHC), coeftest(reg_did_ind_c4, vcov=vcovHC), type = "text")

stargazer(coeftest(reg_did_ind_c1, vcov=vcovHC),coeftest(reg_did_ind_c3, vcov=vcovHC), coeftest(reg_did_ind_c2, vcov=vcovHC), coeftest(reg_did_ind_c4, vcov=vcovHC))

cluster_se1 <- vcovHC(reg_did_ind_c1, cluster = individuos3$id_hogar)

cluster_se2 <- vcovHC(reg_did_ind_c2, cluster = individuos3$id_hogar)

cluster_se3 <- vcovHC(reg_did_ind_c3, cluster = individuos3$id_hogar)

cluster_se4 <- vcovHC(reg_did_ind_c4, cluster = individuos3$id_hogar)


stargazer(coeftest(reg_did_ind_c1, vcov=cluster_se1),coeftest(reg_did_ind_c3, vcov=cluster_se3),coeftest(reg_did_ind_c2, vcov=cluster_se2), coeftest(reg_did_ind_c4, vcov=cluster_se4), type = "text")

remove(cluster_se1)
remove(cluster_se2)
remove(cluster_se3)
remove(cluster_se4)

remove(reg_did_ind_c1)
remove(reg_did_ind_c2)
remove(reg_did_ind_c3)
remove(reg_did_ind_c4)

############################## Sub population models (Provincias) ##########################

#Now we estimate our model for many subpopulations : women\men, rural\urban, and finalyy we will do a quantile regression

#Women

mujeres <- individuos[individuos$sex == 1,]

did_mujeres1 <- lm(university ~ policy*treat_group +
                     age + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province), data = mujeres)

did_mujeres2 <- lm(informal ~ policy*treat_group +
                     age + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province), data = mujeres)

did_mujeres3 <- lm(employment ~ policy*treat_group +
                     age + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province), data = mujeres)

did_mujeres4 <- lm(migrant ~ policy*treat_group +
                     age + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province),
                   data = mujeres)


stargazer(coeftest(did_mujeres1, vcov=vcovHC) ,coeftest(did_mujeres3, vcov=vcovHC), coeftest(did_mujeres2, vcov=vcovHC), coeftest(did_mujeres4, vcov=vcovHC), type = "text")

cluster_se1 <- vcovHC(did_mujeres1, cluster = mujeres$id_hogar)

cluster_se2 <- vcovHC(did_mujeres2, cluster = mujeres$id_hogar)

cluster_se3 <- vcovHC(did_mujeres3, cluster = mujeres$id_hogar)

cluster_se4 <- vcovHC(did_mujeres4, cluster = mujeres$id_hogar)


stargazer(coeftest(did_mujeres1, vcov=cluster_se1),coeftest(did_mujeres3, vcov=cluster_se3),coeftest(did_mujeres2, vcov=cluster_se2), coeftest(did_mujeres4, vcov=cluster_se4), type = "text")

remove(cluster_se1)
remove(cluster_se2)
remove(cluster_se3)
remove(cluster_se4)


remove(mujeres)
remove(did_mujeres1)
remove(did_mujeres2)
remove(did_mujeres3)
remove(did_mujeres4)

#men


hombres <- individuos[individuos$sex == 0,]

did_hombres1 <- lm(university ~ policy*treat_group +
                     age + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province), data = hombres)

did_hombres2 <- lm(informal ~ policy*treat_group +
                     age  + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province), data = hombres)

did_hombres3 <- lm(employment ~ policy*treat_group +
                     age  + indigenous + afro + rural  + lformal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province), data = hombres)

did_hombres4 <- lm(migrant ~ policy*treat_group +
                     age + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province),
                   data = hombres)


stargazer(coeftest(did_hombres1, vcov=vcovHC),coeftest(did_hombres3, vcov=vcovHC),coeftest(did_hombres2, vcov=vcovHC), coeftest(did_hombres4, vcov=vcovHC) , type = "text")


cluster_se1 <- vcovHC(did_hombres1, cluster = hombres$id_hogar)

cluster_se2 <- vcovHC(did_hombres2, cluster = hombres$id_hogar)

cluster_se3 <- vcovHC(did_hombres3, cluster = hombres$id_hogar)

cluster_se4 <- vcovHC(did_hombres4, cluster = hombres$id_hogar)


stargazer(coeftest(did_hombres1, vcov=cluster_se1),coeftest(did_hombres3, vcov=cluster_se3),coeftest(did_hombres2, vcov=cluster_se2), coeftest(did_hombres4, vcov=cluster_se4), type = "text")

remove(cluster_se1)
remove(cluster_se2)
remove(cluster_se3)
remove(cluster_se4)

remove(hombres)
remove(did_hombres1)
remove(did_hombres2)
remove(did_hombres3)
remove(did_hombres4)

#rural/urban

rural <- individuos[individuos$rural == 1,]

did_rural1 <- lm(university ~ policy*treat_group +
                   age + sex + indigenous + afro  + formal_per_house + h_tot_labour_income_pc +
                   h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                   as.factor(period) + as.factor(province), data = rural)

did_rural2 <- lm(informal ~ policy*treat_group +
                   age + sex + indigenous + afro  + formal_per_house + h_tot_labour_income_pc +
                   h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                   as.factor(period) + as.factor(province), data = rural)

did_rural3 <- lm(employment ~ policy*treat_group +
                   age + sex + indigenous + afro + formal_per_house + h_tot_labour_income_pc +
                   h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                   as.factor(period) + as.factor(province), data = rural)

did_rural4 <- lm(migrant ~ policy*treat_group +
                   age + sex + indigenous + afro  + formal_per_house + h_tot_labour_income_pc +
                   h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                   as.factor(period) + as.factor(province),
                 data = rural)


stargazer(coeftest(did_rural1, vcov=vcovHC),coeftest(did_rural3, vcov=vcovHC),coeftest(did_rural2, vcov=vcovHC), coeftest(did_rural4, vcov=vcovHC), type = "text")

cluster_se1 <- vcovHC(did_rural1, cluster = rural$id_hogar)

cluster_se2 <- vcovHC(did_rural2, cluster = rural$id_hogar)

cluster_se3 <- vcovHC(did_rural3, cluster = rural$id_hogar)

cluster_se4 <- vcovHC(did_rural4, cluster = rural$id_hogar)


stargazer(coeftest(did_rural1, vcov=cluster_se1),coeftest(did_rural3, vcov=cluster_se3),coeftest(did_rural2, vcov=cluster_se2), coeftest(did_rural4, vcov=cluster_se4), type = "text")

remove(cluster_se1)
remove(cluster_se2)
remove(cluster_se3)
remove(cluster_se4)

remove(rural)
remove(did_rural1)
remove(did_rural2)
remove(did_rural3)
remove(did_rural4)


urban <- individuos[individuos$rural == 0,]

did_urban1 <- lm(university ~ policy*treat_group +
                   age + sex + indigenous + afro  + formal_per_house + h_tot_labour_income_pc +
                   h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                   as.factor(period) + as.factor(province), data = urban)

did_urban2 <- lm(informal ~ policy*treat_group +
                   age + sex + indigenous + afro + formal_per_house + h_tot_labour_income_pc +
                   h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                   as.factor(period) + as.factor(province), data = urban)

did_urban3 <- lm(employment ~ policy*treat_group +
                   age + sex + indigenous + afro  + formal_per_house + h_tot_labour_income_pc +
                   h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                   as.factor(period) + as.factor(province), data = urban)

did_urban4 <- lm(migrant ~ policy*treat_group +
                   age + sex + indigenous + afro   + formal_per_house + h_tot_labour_income_pc +
                   h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                   as.factor(period) + as.factor(province),
                 data = urban)

stargazer(coeftest(did_urban1, vcov=vcovHC),coeftest(did_urban3, vcov=vcovHC),coeftest(did_urban2, vcov=vcovHC),coeftest(did_urban4, vcov=vcovHC), type = "text")

cluster_se1 <- vcovHC(did_urban1, cluster = urban$id_hogar)

cluster_se2 <- vcovHC(did_urban2, cluster = urban$id_hogar)

cluster_se3 <- vcovHC(did_urban3, cluster = urban$id_hogar)

cluster_se4 <- vcovHC(did_urban4, cluster = urban$id_hogar)


stargazer(coeftest(did_urban1, vcov=cluster_se1),coeftest(did_urban3, vcov=cluster_se3),coeftest(did_urban2, vcov=cluster_se2), coeftest(did_urban4, vcov=cluster_se4), type = "text")

remove(cluster_se1)
remove(cluster_se2)
remove(cluster_se3)
remove(cluster_se4)

remove(urban)
remove(did_urban1)
remove(did_urban2)
remove(did_urban3)
remove(did_urban4)

#Indígenas 

indigenas <- individuos[individuos$indigenous == 1,]

did_indigenas1 <- lm(university ~ policy*treat_group +
                       age + sex + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                       h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                       as.factor(period) + as.factor(province), data = indigenas)

did_indigenas2 <- lm(informal ~ policy*treat_group +
                       age + sex  + afro + rural  + lformal_per_house + h_tot_labour_income_pc +
                       h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                       as.factor(period) + as.factor(province), data = indigenas)

did_indigenas3 <- lm(employment ~ policy*treat_group +
                       age + sex +  afro + rural  + lformal_per_house + h_tot_labour_income_pc +
                       h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                       as.factor(period) + as.factor(province), data = indigenas)

did_indigenas4 <- lm(migrant ~ policy*treat_group +
                       age + sex +  afro + rural  + formal_per_house + h_tot_labour_income_pc +
                       h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                       as.factor(period) + as.factor(province),
                     data = indigenas)


stargazer(coeftest(did_indigenas1, vcov=vcovHC),coeftest(did_indigenas3, vcov=vcovHC),coeftest(did_indigenas2, vcov=vcovHC), coeftest(did_indigenas4, vcov=vcovHC), type = "text")

cluster_se1 <- vcovHC(did_indigenas1, cluster = indigenas$id_hogar)

cluster_se2 <- vcovHC(did_indigenas2, cluster = indigenas$id_hogar)

cluster_se3 <- vcovHC(did_indigenas3, cluster = indigenas$id_hogar)

cluster_se4 <- vcovHC(did_indigenas4, cluster = indigenas$id_hogar)


stargazer(coeftest(did_indigenas1, vcov=cluster_se1),coeftest(did_indigenas3, vcov=cluster_se3),coeftest(did_indigenas2, vcov=cluster_se2), coeftest(did_indigenas4, vcov=cluster_se4), type = "text")

remove(cluster_se1)
remove(cluster_se2)
remove(cluster_se3)
remove(cluster_se4)


remove(indigenas)
remove(did_indigenas1)
remove(did_indigenas2)
remove(did_indigenas3)
remove(did_indigenas4)


######################### Regrssion on different quintiles (Provincias) ######################


#Q1

Q1 <- individuos[individuos$quintil == 1 ,]

summary(Q1$h_tot_labour_income_pc)

did_q11 <- lm(university ~ policy*treat_group +
                age + sex + indigenous + afro + rural  + formal_per_house  +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                as.factor(period) + as.factor(province), data = Q1)

did_q12 <- lm(informal ~ policy*treat_group +
                age + sex + indigenous + afro + rural  + formal_per_house +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                as.factor(period) + as.factor(province), data = Q1)

did_q13 <- lm(employment ~ policy*treat_group +
                age + sex + indigenous + afro + rural  + formal_per_house +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                as.factor(period) + as.factor(province), data = Q1)

did_q14 <- lm(migrant ~ policy*treat_group +
                age + sex +  afro + rural  + formal_per_house + h_tot_labour_income_pc +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                as.factor(period) + as.factor(province),
              data = Q1)


stargazer(coeftest(did_q11, vcov=vcovHC),coeftest(did_q13, vcov=vcovHC),coeftest(did_q12, vcov=vcovHC), coeftest(did_q14, vcov=vcovHC), type = "text")

cluster_se_q11 <- vcovHC(did_q11, cluster = Q1$id_hogar)

cluster_se_q12 <- vcovHC(did_q12, cluster = Q1$id_hogar)

cluster_se_q13 <- vcovHC(did_q13, cluster = Q1$id_hogar)

cluster_se_q14 <- vcovHC(did_q14, cluster = Q1$id_hogar)


stargazer(coeftest(did_q11, vcov=cluster_se_q11),coeftest(did_q13, vcov=cluster_se_q13),coeftest(did_q12, vcov=cluster_se_q12), coeftest(did_q14, vcov=cluster_se_q14), type = "text")




#Q2

Q2 <- individuos[individuos$quintil == 2 ,]

summary(Q2$h_tot_labour_income_pc)

did_q21 <- lm(university ~ policy*treat_group +
                age + sex + indigenous + afro + rural  + formal_per_house  +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                as.factor(period) + as.factor(province), data = Q2)

did_q22 <- lm(informal ~ policy*treat_group +
                age + sex + indigenous + afro + rural  + formal_per_house +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                as.factor(period) + as.factor(province), data = Q2)

did_q23 <- lm(employment ~ policy*treat_group +
                age + sex + indigenous + afro + rural  + formal_per_house  +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                as.factor(period) + as.factor(province), data = Q2)

did_q24 <- lm(migrant ~ policy*treat_group +
                age + sex +  afro + rural  + formal_per_house +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                as.factor(period) + as.factor(province),
              data = Q2)


stargazer(coeftest(did_q21, vcov=vcovHC),coeftest(did_q23, vcov=vcovHC),coeftest(did_q22, vcov=vcovHC),coeftest(did_q24, vcov=vcovHC),  type = "text")

cluster_se_q21 <- vcovHC(did_q21, cluster = Q2$id_hogar)

cluster_se_q22 <- vcovHC(did_q22, cluster = Q2$id_hogar)

cluster_se_q23 <- vcovHC(did_q23, cluster = Q2$id_hogar)

cluster_se_q24 <- vcovHC(did_q24, cluster = Q2$id_hogar)


stargazer(coeftest(did_q21, vcov=cluster_se_q21),coeftest(did_q23, vcov=cluster_se_q23),coeftest(did_q22, vcov=cluster_se_q22), coeftest(did_q24, vcov=cluster_se_q24), type = "text")


#Q3

Q3 <- individuos[individuos$quintil == 3 ,]

summary(Q3$h_tot_labour_income_pc)


did_q31 <- lm(university ~ policy*treat_group +
                age + sex + indigenous + afro + rural  + formal_per_house  +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                as.factor(period) + as.factor(province), data = Q3)

did_q32 <- lm(informal ~ policy*treat_group +
                age + sex + indigenous + afro + rural  + formal_per_house +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                as.factor(period) + as.factor(province), data = Q3)

did_q33 <- lm(employment ~ policy*treat_group +
                age + sex + indigenous + afro + rural  + formal_per_house +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                as.factor(period) + as.factor(province), data = Q3)

did_q34 <- lm(migrant ~ policy*treat_group +
                age + sex +  afro + rural  + formal_per_house  +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                as.factor(period) + as.factor(province),
              data = Q3)


stargazer(coeftest(did_q31, vcov=vcovHC),coeftest(did_q33, vcov=vcovHC),coeftest(did_q32, vcov=vcovHC), coeftest(did_q34, vcov=vcovHC), type = "text")

cluster_se_q31 <- vcovHC(did_q31, cluster = Q3$id_hogar)

cluster_se_q32 <- vcovHC(did_q32, cluster = Q3$id_hogar)

cluster_se_q33 <- vcovHC(did_q33, cluster = Q3$id_hogar)

cluster_se_q34 <- vcovHC(did_q34, cluster = Q3$id_hogar)


stargazer(coeftest(did_q31, vcov=cluster_se_q31),coeftest(did_q33, vcov=cluster_se_q33),coeftest(did_q32, vcov=cluster_se_q32), coeftest(did_q34, vcov=cluster_se_q34), type = "text")


#Q4

Q4 <- individuos[individuos$quintil == 4 ,]

summary(Q4$h_tot_labour_income_pc)

did_Q41 <- lm(university ~ policy*treat_group +
                age + sex + indigenous + afro + rural  + formal_per_house +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                as.factor(period) + as.factor(province), data = Q4)

did_Q42 <- lm(informal ~ policy*treat_group +
                age + sex + indigenous + afro + rural  + formal_per_house +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                as.factor(period) + as.factor(province), data = Q4)

did_Q43 <- lm(employment ~ policy*treat_group +
                age + sex + indigenous + afro + rural  + formal_per_house +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                as.factor(period) + as.factor(province), data = Q4)

did_Q44 <- lm(migrant ~ policy*treat_group +
                age + sex +  afro + rural  + formal_per_house + h_tot_labour_income_pc +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                as.factor(period) + as.factor(province),
              data = Q4)


stargazer(coeftest(did_Q41, vcov=vcovHC),coeftest(did_Q43, vcov=vcovHC),coeftest(did_Q42, vcov=vcovHC), coeftest(did_Q44, vcov=vcovHC), type = "text")

cluster_se_Q41 <- vcovHC(did_Q41, cluster = Q4$id_hogar)

cluster_se_Q42 <- vcovHC(did_Q42, cluster = Q4$id_hogar)

cluster_se_Q43 <- vcovHC(did_Q43, cluster = Q4$id_hogar)

cluster_se_Q44 <- vcovHC(did_Q44, cluster = Q4$id_hogar)


stargazer(coeftest(did_Q41, vcov=cluster_se_Q41),coeftest(did_Q43, vcov=cluster_se_Q43),coeftest(did_Q42, vcov=cluster_se_Q42), coeftest(did_Q44, vcov=cluster_se_Q44), type = "text")


#Q5

Q5 <- individuos[individuos$quintil == 5 ,]

summary(Q5$h_tot_labour_income_pc)

did_Q51 <- lm(university ~ policy*treat_group +
                age + sex + indigenous + afro + rural  + formal_per_house +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                as.factor(period) + as.factor(province), data = Q5)

did_Q52 <- lm(informal ~ policy*treat_group +
                age + sex + indigenous + afro + rural  + formal_per_house +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                as.factor(period) + as.factor(province), data = Q5)

did_Q53 <- lm(employment ~ policy*treat_group +
                age + sex + indigenous + afro + rural  + formal_per_house +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                as.factor(period) + as.factor(province), data = Q5)

did_Q54 <- lm(migrant ~ policy*treat_group +
                age + sex +  afro + rural  + formal_per_house + h_tot_labour_income_pc +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                as.factor(period) + as.factor(province),
              data = Q5)

stargazer(coeftest(did_Q51, vcov=vcovHC),coeftest(did_Q53, vcov=vcovHC),coeftest(did_Q52, vcov=vcovHC),coeftest(did_Q54, vcov=vcovHC), type = "text")

cluster_se_Q51 <- vcovHC(did_Q51, cluster = Q5$id_hogar)

cluster_se_Q52 <- vcovHC(did_Q52, cluster = Q5$id_hogar)

cluster_se_Q53 <- vcovHC(did_Q53, cluster = Q5$id_hogar)

cluster_se_Q54 <- vcovHC(did_Q54, cluster = Q5$id_hogar)


stargazer(coeftest(did_Q51, vcov=cluster_se_Q51),coeftest(did_Q53, vcov=cluster_se_Q53),coeftest(did_Q52, vcov=cluster_se_Q52), coeftest(did_Q54, vcov=cluster_se_Q54), type = "text")

remove(cluster_se_q11)
remove(cluster_se_q12)
remove(cluster_se_q13)
remove(cluster_se_q14)
remove(cluster_se_q21)
remove(cluster_se_q22)
remove(cluster_se_q23)
remove(cluster_se_q24)
remove(cluster_se_q31)
remove(cluster_se_q32)
remove(cluster_se_q33)
remove(cluster_se_q34)
remove(cluster_se_Q41)
remove(cluster_se_Q42)
remove(cluster_se_Q43)
remove(cluster_se_Q44)
remove(cluster_se_Q51)
remove(cluster_se_Q52)
remove(cluster_se_Q53)
remove(cluster_se_Q54)



remove(Q1)
remove(Q2)
remove(Q3)
remove(Q4)
remove(Q5)
remove(did_Q53)
remove(did_Q52)
remove(did_Q51)
remove(did_Q43)
remove(did_Q42)
remove(did_Q41)
remove(did_q33)
remove(did_q23)
remove(did_q32)
remove(did_q31)
remove(did_q22)
remove(did_q21)
remove(did_q11)
remove(did_q12)
remove(did_q13)
remove(did_Q54)
remove(did_Q44)
remove(did_q34)
remove(did_q24)
remove(did_q14)

#################### Parallel Trend Assumption (Provincias)  ###############################

treatment_group <- read_feather("C:/Users/ignac/OneDrive/Documentos/PHD/Senescyt_Evaluation/Data/treatment_univ.feather")
Enemdu <- read_feather("C:/Users/ignac/OneDrive/Documentos/PHD/Senescyt_Evaluation/Data/Enemdu2.feather")
prov_univ <- read_feather("C:/Users/ignac/OneDrive/Documentos/PHD/Senescyt_Evaluation/Data/prov_univ.feather")

treatment_group <- treatment_group[, c("province","treat_group")]

treatment_group$treat_group <- ifelse(treatment_group$treat_group == 1, 0, 1)

individuos <- Enemdu[Enemdu$age > 16 & Enemdu$age < 22,]
individuos <- individuos[individuos$education > 6,]

individuos$migrant_student <- individuos$migrant * individuos$university

individuos <- individuos[individuos$province != "GALAPAGOS",]
individuos <- individuos[individuos$province != "ZONAS NO DELIMITADAS",]

#We create a data set of 2010 and 2011 to study the PTA

individuos <- individuos[ individuos$period > 2009 & individuos$period < 2012,]

individuos <- merge(individuos, treatment_group, by = "province")

individuos$policy <- rep(0, length(individuos$period))
individuos$policy[individuos$period == 2010 & individuos$age < 19
                  | individuos$period == 2011 & individuos$age < 20]=1


individuos$lformal_per_house <- ifelse(individuos$formal_per_house < 1, 0 ,log(individuos$formal_per_house))

individuos$lh_tot_labour_income_pc <- ifelse(individuos$h_tot_labour_income_pc < 1, 0 ,log(individuos$h_tot_labour_income_pc))

individuos$lnhousehold.y <- ifelse(individuos$nhousehold.y < 1, 0 ,log(individuos$nhousehold.y))

PTA <- individuos

table(PTA$period)


PTA_reg1 <- lm(university ~ policy*treat_group +
                 age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                 h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                 as.factor(period) + as.factor(province), data = PTA)

PTA_reg2 <- lm(informal ~ policy*treat_group +
                 age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                 h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                 as.factor(period) + as.factor(province), data = PTA)

PTA_reg3 <- lm(employment ~ policy*treat_group +
                 age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                 h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                 as.factor(period) + as.factor(province), data = PTA)

PTA_reg4 <- lm(migrant ~ policy*treat_group +
                 age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                 h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                 as.factor(period) + as.factor(province), data = PTA)


stargazer(coeftest(PTA_reg1, vcov=vcovHC),coeftest(PTA_reg3, vcov=vcovHC),coeftest(PTA_reg2, vcov=vcovHC),coeftest(PTA_reg4, vcov=vcovHC), type = "text")

# The Parallel Trend Assumption is respected in the four models

remove(Enemdu)
remove(individuos)
remove(treatment_group)
remove(prov_univ)
remove(PTA)
remove(PTA_reg1)
remove(PTA_reg2)
remove(PTA_reg3)
remove(PTA_reg4)


########################## Quality of the University ##########################################

treatment_group <- read_feather("C:/Users/ignac/OneDrive/Documentos/PHD/Senescyt_Evaluation/Data/treatment_univ.feather")
Enemdu <- read_feather("C:/Users/ignac/OneDrive/Documentos/PHD/Senescyt_Evaluation/Data/Enemdu.feather")
prov_univ <- read_feather("C:/Users/ignac/OneDrive/Documentos/PHD/Senescyt_Evaluation/Data/prov_univ.feather")


#Now we will focuss on doing the DiD with individuals in the cohort of treatment 
#We will keep people that have completed high school between 17 - 19 years

#Adding as a control the number of universities 


treatment_group <- treatment_group[, c("province","treat_group")]

treatment_group$treat_group <- ifelse(treatment_group$treat_group == 1, 0, 1)

individuos <- Enemdu[Enemdu$age > 20 & Enemdu$age < 26,]
individuos <- individuos[individuos$education > 6,]

individuos$migrant_student <- individuos$migrant * individuos$university

individuos <- individuos[individuos$province != "GALAPAGOS",]
individuos <- individuos[individuos$province != "ZONAS NO DELIMITADAS",]

#We create a data set of 2012 when people of 17-18 were affected by the exam and 19-20 that were not affected by the law 

individuos <- individuos[ individuos$period > 2015 & individuos$period < 2017,]

individuos <- merge(individuos, treatment_group, by = "province")

individuos$policy <- rep(0, length(individuos$period))
individuos$policy[individuos$period == 2016 & individuos$age < 23
                  | individuos$period == 2017 & individuos$age < 24]=1




individuos$lformal_per_house <- ifelse(individuos$formal_per_house < 1, 0 ,log(individuos$formal_per_house))

individuos$lh_tot_labour_income_pc <- ifelse(individuos$h_tot_labour_income_pc < 1, 0 ,log(individuos$h_tot_labour_income_pc))

individuos$lnhousehold.y <- ifelse(individuos$nhousehold.y < 1, 0 ,log(individuos$nhousehold.y))


remove(Enemdu)
remove(treatment_group)

table(individuos$treat_group, individuos$province)

table(individuos$education_y)

individuos$finished_uni <- rep(0, length(individuos$province))
individuos$finished_uni <- ifelse(individuos$education_y > 17, 1,0)

table(individuos$policy)

table(individuos$university)

################################ Control Variables (Provincias) #############################

reg_did_ind_c1 <- lm(finished_uni ~ policy * treat_group +
                       age + sex + indigenous + afro + rural + formal_per_house  + h_tot_labour_income_pc +
                       h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                       as.factor(province),
                     data = individuos)

stargazer(reg_did_ind_c1, type = "text")

#We find that the exam has not a significant effect on the likelihood of being subscribed in the university

reg_did_ind_c2 <- lm(informal ~ policy*treat_group +
                       age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                       h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                        as.factor(province),
                     data = individuos)

stargazer(reg_did_ind_c2, type = "text")

#We find that the exam has a positive and significant effect on the likelihood of being an informal employee

reg_did_ind_c3 <- lm(employment ~ policy*treat_group +
                       age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                       h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                        as.factor(province),
                     data = individuos)

stargazer(reg_did_ind_c3, type = "text")


reg_did_ind_c4 <- lm(migrant ~ policy*treat_group +
                       age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                       h_little_kids + h_kid + h_teen + h_adult + nhousehold.y 
                      + as.factor(province),
                     data = individuos)

stargazer(reg_did_ind_c4, type = "text")


stargazer(coeftest(reg_did_ind_c1, vcov=vcovHC),coeftest(reg_did_ind_c3, vcov=vcovHC), coeftest(reg_did_ind_c2, vcov=vcovHC), coeftest(reg_did_ind_c4, vcov=vcovHC), type = "text")

stargazer(coeftest(reg_did_ind_c1, vcov=vcovHC),coeftest(reg_did_ind_c3, vcov=vcovHC), coeftest(reg_did_ind_c2, vcov=vcovHC), coeftest(reg_did_ind_c4, vcov=vcovHC))

cluster_se1 <- vcovHC(reg_did_ind_c1, cluster = individuos3$id_hogar)

cluster_se2 <- vcovHC(reg_did_ind_c2, cluster = individuos3$id_hogar)

cluster_se3 <- vcovHC(reg_did_ind_c3, cluster = individuos3$id_hogar)

cluster_se4 <- vcovHC(reg_did_ind_c4, cluster = individuos3$id_hogar)


stargazer(coeftest(reg_did_ind_c1, vcov=cluster_se1),coeftest(reg_did_ind_c3, vcov=cluster_se3),coeftest(reg_did_ind_c2, vcov=cluster_se2), coeftest(reg_did_ind_c4, vcov=cluster_se4), type = "text")

remove(cluster_se1)
remove(cluster_se2)
remove(cluster_se3)
remove(cluster_se4)

remove(reg_did_ind_c1)
remove(reg_did_ind_c2)
remove(reg_did_ind_c3)
remove(reg_did_ind_c4)

############################## Sub population models (Provincias) ##########################

#Now we estimate our model for many subpopulations : women\men, rural\urban, and finalyy we will do a quantile regression

#Women

mujeres <- individuos[individuos$sex == 1,]

did_mujeres1 <- lm(finished_uni ~ policy*treat_group +
                     age + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                      as.factor(province), data = mujeres)

did_mujeres2 <- lm(informal ~ policy*treat_group +
                     age + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                      as.factor(province), data = mujeres)

did_mujeres3 <- lm(employment ~ policy*treat_group +
                     age + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(province), data = mujeres)

did_mujeres4 <- lm(migrant ~ policy*treat_group +
                     age + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                      as.factor(province),
                   data = mujeres)


stargazer(coeftest(did_mujeres1, vcov=vcovHC) ,coeftest(did_mujeres3, vcov=vcovHC), coeftest(did_mujeres2, vcov=vcovHC), coeftest(did_mujeres4, vcov=vcovHC), type = "text")

cluster_se1 <- vcovHC(did_mujeres1, cluster = mujeres$id_hogar)

cluster_se2 <- vcovHC(did_mujeres2, cluster = mujeres$id_hogar)

cluster_se3 <- vcovHC(did_mujeres3, cluster = mujeres$id_hogar)

cluster_se4 <- vcovHC(did_mujeres4, cluster = mujeres$id_hogar)


stargazer(coeftest(did_mujeres1, vcov=cluster_se1),coeftest(did_mujeres3, vcov=cluster_se3),coeftest(did_mujeres2, vcov=cluster_se2), coeftest(did_mujeres4, vcov=cluster_se4), type = "text")

remove(cluster_se1)
remove(cluster_se2)
remove(cluster_se3)
remove(cluster_se4)


remove(mujeres)
remove(did_mujeres1)
remove(did_mujeres2)
remove(did_mujeres3)
remove(did_mujeres4)

#men


hombres <- individuos[individuos$sex == 0,]

did_hombres1 <- lm(finished_uni ~ policy*treat_group +
                     age + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y  + as.factor(province), data = hombres)

did_hombres2 <- lm(informal ~ policy*treat_group +
                     age  + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(province), data = hombres)

did_hombres3 <- lm(employment ~ policy*treat_group +
                     age  + indigenous + afro + rural  + lformal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                      as.factor(province), data = hombres)

did_hombres4 <- lm(migrant ~ policy*treat_group +
                     age + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                      as.factor(province),
                   data = hombres)


stargazer(coeftest(did_hombres1, vcov=vcovHC),coeftest(did_hombres3, vcov=vcovHC),coeftest(did_hombres2, vcov=vcovHC), coeftest(did_hombres4, vcov=vcovHC) , type = "text")


cluster_se1 <- vcovHC(did_hombres1, cluster = hombres$id_hogar)

cluster_se2 <- vcovHC(did_hombres2, cluster = hombres$id_hogar)

cluster_se3 <- vcovHC(did_hombres3, cluster = hombres$id_hogar)

cluster_se4 <- vcovHC(did_hombres4, cluster = hombres$id_hogar)


stargazer(coeftest(did_hombres1, vcov=cluster_se1),coeftest(did_hombres3, vcov=cluster_se3),coeftest(did_hombres2, vcov=cluster_se2), coeftest(did_hombres4, vcov=cluster_se4), type = "text")

remove(cluster_se1)
remove(cluster_se2)
remove(cluster_se3)
remove(cluster_se4)

remove(hombres)
remove(did_hombres1)
remove(did_hombres2)
remove(did_hombres3)
remove(did_hombres4)

#rural/urban

rural <- individuos[individuos$rural == 1,]

did_rural1 <- lm(finished_uni ~ policy*treat_group +
                   age + sex + indigenous + afro  + formal_per_house + h_tot_labour_income_pc +
                   h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                    as.factor(province), data = rural)

did_rural2 <- lm(informal ~ policy*treat_group +
                   age + sex + indigenous + afro  + formal_per_house + h_tot_labour_income_pc +
                   h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                    as.factor(province), data = rural)

did_rural3 <- lm(employment ~ policy*treat_group +
                   age + sex + indigenous + afro + formal_per_house + h_tot_labour_income_pc +
                   h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                    as.factor(province), data = rural)

did_rural4 <- lm(migrant ~ policy*treat_group +
                   age + sex + indigenous + afro  + formal_per_house + h_tot_labour_income_pc +
                   h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                   as.factor(province),
                 data = rural)


stargazer(coeftest(did_rural1, vcov=vcovHC),coeftest(did_rural3, vcov=vcovHC),coeftest(did_rural2, vcov=vcovHC), coeftest(did_rural4, vcov=vcovHC), type = "text")

cluster_se1 <- vcovHC(did_rural1, cluster = rural$id_hogar)

cluster_se2 <- vcovHC(did_rural2, cluster = rural$id_hogar)

cluster_se3 <- vcovHC(did_rural3, cluster = rural$id_hogar)

cluster_se4 <- vcovHC(did_rural4, cluster = rural$id_hogar)


stargazer(coeftest(did_rural1, vcov=cluster_se1),coeftest(did_rural3, vcov=cluster_se3),coeftest(did_rural2, vcov=cluster_se2), coeftest(did_rural4, vcov=cluster_se4), type = "text")

remove(cluster_se1)
remove(cluster_se2)
remove(cluster_se3)
remove(cluster_se4)

remove(rural)
remove(did_rural1)
remove(did_rural2)
remove(did_rural3)
remove(did_rural4)


urban <- individuos[individuos$rural == 0,]

did_urban1 <- lm(finished_uni ~ policy*treat_group +
                   age + sex + indigenous + afro  + formal_per_house + h_tot_labour_income_pc +
                   h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                    as.factor(province), data = urban)

did_urban2 <- lm(informal ~ policy*treat_group +
                   age + sex + indigenous + afro + formal_per_house + h_tot_labour_income_pc +
                   h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                    as.factor(province), data = urban)

did_urban3 <- lm(employment ~ policy*treat_group +
                   age + sex + indigenous + afro  + formal_per_house + h_tot_labour_income_pc +
                   h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                    as.factor(province), data = urban)

did_urban4 <- lm(migrant ~ policy*treat_group +
                   age + sex + indigenous + afro   + formal_per_house + h_tot_labour_income_pc +
                   h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                    as.factor(province),
                 data = urban)

stargazer(coeftest(did_urban1, vcov=vcovHC),coeftest(did_urban3, vcov=vcovHC),coeftest(did_urban2, vcov=vcovHC),coeftest(did_urban4, vcov=vcovHC), type = "text")

cluster_se1 <- vcovHC(did_urban1, cluster = urban$id_hogar)

cluster_se2 <- vcovHC(did_urban2, cluster = urban$id_hogar)

cluster_se3 <- vcovHC(did_urban3, cluster = urban$id_hogar)

cluster_se4 <- vcovHC(did_urban4, cluster = urban$id_hogar)


stargazer(coeftest(did_urban1, vcov=cluster_se1),coeftest(did_urban3, vcov=cluster_se3),coeftest(did_urban2, vcov=cluster_se2), coeftest(did_urban4, vcov=cluster_se4), type = "text")

remove(cluster_se1)
remove(cluster_se2)
remove(cluster_se3)
remove(cluster_se4)

remove(urban)
remove(did_urban1)
remove(did_urban2)
remove(did_urban3)
remove(did_urban4)



######################### Regrssion on different quintiles (Provincias) ######################


#Q1

Q1 <- individuos[individuos$quintil == 1 ,]

summary(Q1$h_tot_labour_income_pc)

did_q11 <- lm(finished_uni ~ policy*treat_group +
                age + sex + indigenous + afro + rural  + formal_per_house  +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                 as.factor(province), data = Q1)

did_q12 <- lm(informal ~ policy*treat_group +
                age + sex + indigenous + afro + rural  + formal_per_house +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                 as.factor(province), data = Q1)

did_q13 <- lm(employment ~ policy*treat_group +
                age + sex + indigenous + afro + rural  + formal_per_house +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                 as.factor(province), data = Q1)

did_q14 <- lm(migrant ~ policy*treat_group +
                age + sex +  afro + rural  + formal_per_house + h_tot_labour_income_pc +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                 as.factor(province),
              data = Q1)


stargazer(coeftest(did_q11, vcov=vcovHC),coeftest(did_q13, vcov=vcovHC),coeftest(did_q12, vcov=vcovHC), coeftest(did_q14, vcov=vcovHC), type = "text")

cluster_se_q11 <- vcovHC(did_q11, cluster = Q1$id_hogar)

cluster_se_q12 <- vcovHC(did_q12, cluster = Q1$id_hogar)

cluster_se_q13 <- vcovHC(did_q13, cluster = Q1$id_hogar)

cluster_se_q14 <- vcovHC(did_q14, cluster = Q1$id_hogar)


stargazer(coeftest(did_q11, vcov=cluster_se_q11),coeftest(did_q13, vcov=cluster_se_q13),coeftest(did_q12, vcov=cluster_se_q12), coeftest(did_q14, vcov=cluster_se_q14), type = "text")




#Q2

Q2 <- individuos[individuos$quintil == 2 ,]

summary(Q2$h_tot_labour_income_pc)

did_q21 <- lm(finished_uni~ policy*treat_group +
                age + sex + indigenous + afro + rural  + formal_per_house  +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                as.factor(province), data = Q2)

did_q22 <- lm(informal ~ policy*treat_group +
                age + sex + indigenous + afro + rural  + formal_per_house +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                 as.factor(province), data = Q2)

did_q23 <- lm(employment ~ policy*treat_group +
                age + sex + indigenous + afro + rural  + formal_per_house  +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                 as.factor(province), data = Q2)

did_q24 <- lm(migrant ~ policy*treat_group +
                age + sex +  afro + rural  + formal_per_house +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                 as.factor(province),
              data = Q2)


stargazer(coeftest(did_q21, vcov=vcovHC),coeftest(did_q23, vcov=vcovHC),coeftest(did_q22, vcov=vcovHC),coeftest(did_q24, vcov=vcovHC),  type = "text")

cluster_se_q21 <- vcovHC(did_q21, cluster = Q2$id_hogar)

cluster_se_q22 <- vcovHC(did_q22, cluster = Q2$id_hogar)

cluster_se_q23 <- vcovHC(did_q23, cluster = Q2$id_hogar)

cluster_se_q24 <- vcovHC(did_q24, cluster = Q2$id_hogar)


stargazer(coeftest(did_q21, vcov=cluster_se_q21),coeftest(did_q23, vcov=cluster_se_q23),coeftest(did_q22, vcov=cluster_se_q22), coeftest(did_q24, vcov=cluster_se_q24), type = "text")


#Q3

Q3 <- individuos[individuos$quintil == 3 ,]

summary(Q3$h_tot_labour_income_pc)


did_q31 <- lm(finished_uni ~ policy*treat_group +
                age + sex + indigenous + afro + rural  + formal_per_house  +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                 as.factor(province), data = Q3)

did_q32 <- lm(informal ~ policy*treat_group +
                age + sex + indigenous + afro + rural  + formal_per_house +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                 as.factor(province), data = Q3)

did_q33 <- lm(employment ~ policy*treat_group +
                age + sex + indigenous + afro + rural  + formal_per_house +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                 as.factor(province), data = Q3)

did_q34 <- lm(migrant ~ policy*treat_group +
                age + sex +  afro + rural  + formal_per_house  +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                as.factor(province),
              data = Q3)


stargazer(coeftest(did_q31, vcov=vcovHC),coeftest(did_q33, vcov=vcovHC),coeftest(did_q32, vcov=vcovHC), coeftest(did_q34, vcov=vcovHC), type = "text")

cluster_se_q31 <- vcovHC(did_q31, cluster = Q3$id_hogar)

cluster_se_q32 <- vcovHC(did_q32, cluster = Q3$id_hogar)

cluster_se_q33 <- vcovHC(did_q33, cluster = Q3$id_hogar)

cluster_se_q34 <- vcovHC(did_q34, cluster = Q3$id_hogar)


stargazer(coeftest(did_q31, vcov=cluster_se_q31),coeftest(did_q33, vcov=cluster_se_q33),coeftest(did_q32, vcov=cluster_se_q32), coeftest(did_q34, vcov=cluster_se_q34), type = "text")


#Q4

Q4 <- individuos[individuos$quintil == 4 ,]

summary(Q4$h_tot_labour_income_pc)

did_Q41 <- lm(finished_uni ~ policy*treat_group +
                age + sex + indigenous + afro + rural  + formal_per_house +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                as.factor(province), data = Q4)

did_Q42 <- lm(informal ~ policy*treat_group +
                age + sex + indigenous + afro + rural  + formal_per_house +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                as.factor(province), data = Q4)

did_Q43 <- lm(employment ~ policy*treat_group +
                age + sex + indigenous + afro + rural  + formal_per_house +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                 as.factor(province), data = Q4)

did_Q44 <- lm(migrant ~ policy*treat_group +
                age + sex +  afro + rural  + formal_per_house + h_tot_labour_income_pc +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                 as.factor(province),
              data = Q4)


stargazer(coeftest(did_Q41, vcov=vcovHC),coeftest(did_Q43, vcov=vcovHC),coeftest(did_Q42, vcov=vcovHC), coeftest(did_Q44, vcov=vcovHC), type = "text")

cluster_se_Q41 <- vcovHC(did_Q41, cluster = Q4$id_hogar)

cluster_se_Q42 <- vcovHC(did_Q42, cluster = Q4$id_hogar)

cluster_se_Q43 <- vcovHC(did_Q43, cluster = Q4$id_hogar)

cluster_se_Q44 <- vcovHC(did_Q44, cluster = Q4$id_hogar)


stargazer(coeftest(did_Q41, vcov=cluster_se_Q41),coeftest(did_Q43, vcov=cluster_se_Q43),coeftest(did_Q42, vcov=cluster_se_Q42), coeftest(did_Q44, vcov=cluster_se_Q44), type = "text")


#Q5

Q5 <- individuos[individuos$quintil == 5 ,]

summary(Q5$h_tot_labour_income_pc)

did_Q51 <- lm(finished_uni ~ policy*treat_group +
                age + sex + indigenous + afro + rural  + formal_per_house +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                as.factor(province), data = Q5)

did_Q52 <- lm(informal ~ policy*treat_group +
                age + sex + indigenous + afro + rural  + formal_per_house +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                 as.factor(province), data = Q5)

did_Q53 <- lm(employment ~ policy*treat_group +
                age + sex + indigenous + afro + rural  + formal_per_house +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                 as.factor(province), data = Q5)

did_Q54 <- lm(migrant ~ policy*treat_group +
                age + sex +  afro + rural  + formal_per_house + h_tot_labour_income_pc +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                 as.factor(province),
              data = Q5)

stargazer(coeftest(did_Q51, vcov=vcovHC),coeftest(did_Q53, vcov=vcovHC),coeftest(did_Q52, vcov=vcovHC),coeftest(did_Q54, vcov=vcovHC), type = "text")

cluster_se_Q51 <- vcovHC(did_Q51, cluster = Q5$id_hogar)

cluster_se_Q52 <- vcovHC(did_Q52, cluster = Q5$id_hogar)

cluster_se_Q53 <- vcovHC(did_Q53, cluster = Q5$id_hogar)

cluster_se_Q54 <- vcovHC(did_Q54, cluster = Q5$id_hogar)


stargazer(coeftest(did_Q51, vcov=cluster_se_Q51),coeftest(did_Q53, vcov=cluster_se_Q53),coeftest(did_Q52, vcov=cluster_se_Q52), coeftest(did_Q54, vcov=cluster_se_Q54), type = "text")

remove(cluster_se_q11)
remove(cluster_se_q12)
remove(cluster_se_q13)
remove(cluster_se_q14)
remove(cluster_se_q21)
remove(cluster_se_q22)
remove(cluster_se_q23)
remove(cluster_se_q24)
remove(cluster_se_q31)
remove(cluster_se_q32)
remove(cluster_se_q33)
remove(cluster_se_q34)
remove(cluster_se_Q41)
remove(cluster_se_Q42)
remove(cluster_se_Q43)
remove(cluster_se_Q44)
remove(cluster_se_Q51)
remove(cluster_se_Q52)
remove(cluster_se_Q53)
remove(cluster_se_Q54)



remove(Q1)
remove(Q2)
remove(Q3)
remove(Q4)
remove(Q5)
remove(did_Q53)
remove(did_Q52)
remove(did_Q51)
remove(did_Q43)
remove(did_Q42)
remove(did_Q41)
remove(did_q33)
remove(did_q23)
remove(did_q32)
remove(did_q31)
remove(did_q22)
remove(did_q21)
remove(did_q11)
remove(did_q12)
remove(did_q13)
remove(did_Q54)
remove(did_Q44)
remove(did_q34)
remove(did_q24)
remove(did_q14)

#################### Parallel Trend Assumption (Provincias)  ###############################

treatment_group <- read_feather("C:/Users/ignac/OneDrive/Documentos/PHD/Senescyt_Evaluation/Data/treatment_univ.feather")
Enemdu <- read_feather("C:/Users/ignac/OneDrive/Documentos/PHD/Senescyt_Evaluation/Data/Enemdu2.feather")
prov_univ <- read_feather("C:/Users/ignac/OneDrive/Documentos/PHD/Senescyt_Evaluation/Data/prov_univ.feather")

treatment_group <- treatment_group[, c("province","treat_group")]

treatment_group$treat_group <- ifelse(treatment_group$treat_group == 1, 0, 1)

individuos <- Enemdu[Enemdu$age > 20 & Enemdu$age < 26,]
individuos <- individuos[individuos$education > 6,]

individuos$migrant_student <- individuos$migrant * individuos$university

individuos <- individuos[individuos$province != "GALAPAGOS",]
individuos <- individuos[individuos$province != "ZONAS NO DELIMITADAS",]

#We create a data set of 2012 when people of 17-18 were affected by the exam and 19-20 that were not affected by the law 

individuos <- individuos[ individuos$period > 2012 & individuos$period < 2015,]

individuos <- merge(individuos, treatment_group, by = "province")

individuos$policy <- rep(0, length(individuos$period))
individuos$policy[individuos$period == 2013 & individuos$age < 23
                  | individuos$period == 2014 & individuos$age < 24]=1




individuos$lformal_per_house <- ifelse(individuos$formal_per_house < 1, 0 ,log(individuos$formal_per_house))

individuos$lh_tot_labour_income_pc <- ifelse(individuos$h_tot_labour_income_pc < 1, 0 ,log(individuos$h_tot_labour_income_pc))

individuos$lnhousehold.y <- ifelse(individuos$nhousehold.y < 1, 0 ,log(individuos$nhousehold.y))


remove(Enemdu)
remove(treatment_group)

table(individuos$treat_group, individuos$province)

table(individuos$education_y)

individuos$finished_uni <- rep(0, length(individuos$province))
individuos$finished_uni <- ifelse(individuos$education_y > 17, 1,0)

table(individuos$policy)

table(individuos$university)


PTA <- individuos

table(PTA$period)


PTA_reg1 <- lm(finished_uni ~ policy*treat_group +
                 age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                 h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                 as.factor(period) + as.factor(province), data = PTA)

PTA_reg2 <- lm(informal ~ policy*treat_group +
                 age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                 h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                 as.factor(period) + as.factor(province), data = PTA)

PTA_reg3 <- lm(employment ~ policy*treat_group +
                 age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                 h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                 as.factor(period) + as.factor(province), data = PTA)

PTA_reg4 <- lm(migrant ~ policy*treat_group +
                 age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                 h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                 as.factor(period) + as.factor(province), data = PTA)


stargazer(coeftest(PTA_reg1, vcov=vcovHC),coeftest(PTA_reg3, vcov=vcovHC),coeftest(PTA_reg2, vcov=vcovHC),coeftest(PTA_reg4, vcov=vcovHC), type = "text")

# The Parallel Trend Assumption is respected in the four models

remove(Enemdu)
remove(individuos)
remove(treatment_group)
remove(prov_univ)
remove(PTA)
remove(PTA_reg1)
remove(PTA_reg2)
remove(PTA_reg3)
remove(PTA_reg4)

#################### DDM #############################

#First we will match people in the bad education quality provinces with people in the good education quality provinces 

match_obj <- matchit(policy ~ age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                       h_little_kids + h_kid + h_teen + h_adult + nhousehold.y + educ_house,
                     data = individuos, method = "nearest", distance ="glm",
                     ratio = 1,
                     replace = FALSE)



summary(match_obj)

matched_data <- match.data(match_obj)

matched_data <- matched_data[,-c(111,112,113)]

match_obj1 <- matchit(treat_group ~ age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                       h_little_kids + h_kid + h_teen + h_adult + nhousehold.y + educ_house,
                     data = matched_data, method = "nearest", distance ="glm",
                     ratio = 1,
                     replace = FALSE)

summary(match_obj1)

matched_data1 <- match.data(match_obj1)


ddm1 <- lm(university ~ treat_group*policy +
             as.factor(period) + as.factor(province),
           data = matched_data1, weights = weights)

coeftest(ddm1, vcov=vcovHC)

ddm2 <- lm(employment ~ treat_group*policy +
             as.factor(period) + as.factor(province),
           data = matched_data1, weights = weights)

coeftest(ddm2, vcov=vcovHC)

ddm3 <- lm(informal ~ treat_group*policy +
             as.factor(period) + as.factor(province),
           data = matched_data1, weights = weights)

coeftest(ddm3, vcov=vcovHC)

ddm4 <- lm(migrant ~ treat_group*policy +
             as.factor(period) + as.factor(province),
           data = matched_data1, weights = weights)

coeftest(ddm4, vcov=vcovHC)

stargazer(coeftest(ddm1, vcov=vcovHC), coeftest(ddm2, vcov=vcovHC),coeftest(ddm3, vcov=vcovHC),coeftest(ddm4, vcov=vcovHC), type = "text")

remove(ddm1, ddm2, ddm3,ddm4, individuos, match_obj, match_obj1,matched_data, matched_data, prov_univ)
