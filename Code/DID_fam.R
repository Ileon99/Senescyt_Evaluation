################### Treatment group less educated families ####################

#Let's change the treatment group for those families where there in the family no one went to univerisity

Enemdu <- read_feather("C:/Users/ignac/OneDrive/Documentos/PHD/Senescyt_Evaluation/Data/Enemdu.feather")

#Dummy equal to one if there is a person with univserisity in the family

univ_family <- Enemdu %>%
  group_by(id_hogar) %>%
  summarise_at(vars(university),
               sum) %>%
  ungroup()

univ_family$univ_family <- rep(0, length(univ_family$id_hogar))
univ_family$univ_family[univ_family$university >= 1]=1

names(univ_family) <- c("id_hogar", "univ_family2", "univ_family")

Enemdu <- merge(Enemdu,univ_family, by = "id_hogar")

table(Enemdu$univ_family)

table(Enemdu$university)

Enemdu$univ_family2 <- ifelse(Enemdu$university == 1, Enemdu$univ_family2 - 1, Enemdu$univ_family2)


Enemdu$univ_family<- rep(0, length(Enemdu$univ_family))
Enemdu$univ_family[Enemdu$univ_family2 > 0]= 1

table(Enemdu$univ_family)


#Now we keep only the individuals we want to evaluate

#We will keep people that have completed high school between 17 - 19 years

individuos3 <- Enemdu[Enemdu$cohort == "17-19",]
individuos3 <- individuos3[individuos3$education > 6,]

individuos3$migrant_student <- individuos3$migrant * individuos3$university

individuos3 <- individuos3[individuos3$province != "GALAPAGOS",]
individuos3 <- individuos3[individuos3$province != "ZONAS NO DELIMITADAS",]

#We create two data sets one for the did from 2009 to 2015
#Another to the parallel trends assumption from 2007 to 2015

individuos4 <- individuos3[individuos3$period < 2017,]

individuos3 <- individuos3[individuos3$period > 2008 & individuos3$period < 2017,]

individuos3$treat <- rep(1, length(individuos3$univ_family))
individuos3$treat <- individuos3$treat - individuos3$univ_family

individuos4$treat <- rep(1, length(individuos4$univ_family))
individuos4$treat <- individuos4$treat - individuos4$univ_family

individuos3$policy <- rep(0, length(individuos3$period))
individuos3$policy[individuos3$period > 2011]=1

individuos4$policy <- rep(0, length(individuos4$period))
individuos4$policy[individuos4$period > 2009]=1

individuos3$lformal_per_house <- ifelse(individuos3$formal_per_house < 1, 0 ,log(individuos3$formal_per_house))

individuos3$lh_tot_labour_income_pc <- ifelse(individuos3$h_tot_labour_income_pc < 1, 0 ,log(individuos3$h_tot_labour_income_pc))

individuos3$lnhousehold.y <- ifelse(individuos3$nhousehold.y < 1, 0 ,log(individuos3$nhousehold.y))

individuos4$lformal_per_house <- ifelse(individuos4$formal_per_house < 1, 0 ,log(individuos4$formal_per_house))

individuos4$lh_tot_labour_income_pc <- ifelse(individuos4$h_tot_labour_income_pc < 1, 0 ,log(individuos4$h_tot_labour_income_pc))

individuos4$lnhousehold.y <- ifelse(individuos4$nhousehold.y < 1, 0 ,log(individuos4$nhousehold.y))

table(individuos3$treat)


remove(Enemdu)
remove(univ_family)
remove(treatment_group)



# DID

fam_univ_reg1 <- lm(university ~ policy*treat + 
                      age + sex + indigenous + afro + rural + formal_per_house + h_tot_labour_income_pc + 
                      h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                      as.factor(period) + as.factor(province),
                    data = individuos3)


stargazer(fam_univ_reg1 , type = "text")


fam_univ_reg2 <- lm(informal ~ policy*treat
                    + age + sex + indigenous + afro + rural + formal_per_house + h_tot_labour_income_pc + 
                      h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                      as.factor(period) + as.factor(province), data = individuos3)

stargazer(fam_univ_reg2, type = "text")


fam_univ_reg3 <- lm(employment ~ policy*treat +
                      age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                      h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                      as.factor(period) + as.factor(province), data = individuos3)

fam_univ_reg4 <- lm(migrant ~ policy*treat +
                      age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                      h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                      as.factor(period) + as.factor(province), data = individuos3)

stargazer(fam_univ_reg3, type = "text")

stargazer(coeftest(fam_univ_reg1, vcov=vcovHC),coeftest(fam_univ_reg2, vcov=vcovHC),coeftest(fam_univ_reg3, vcov=vcovHC), coeftest(fam_univ_reg4, vcov=vcovHC), type = "text")

cluster_se1 <- vcovHC(fam_univ_reg1, cluster = individuos3$id_hogar)

cluster_se2 <- vcovHC(fam_univ_reg2, cluster = individuos3$id_hogar)

cluster_se3 <- vcovHC(fam_univ_reg3, cluster = individuos3$id_hogar)

cluster_se4 <- vcovHC(fam_univ_reg4, cluster = individuos3$id_hogar)


stargazer(coeftest(fam_univ_reg1, vcov=cluster_se1),coeftest(fam_univ_reg2, vcov=cluster_se2),coeftest(fam_univ_reg3, vcov=cluster_se3), coeftest(fam_univ_reg4, vcov=cluster_se4), type = "text")

remove(cluster_se1)
remove(cluster_se2)
remove(cluster_se3)
remove(cluster_se4)
remove(fam_univ_reg1)
remove(fam_univ_reg2)
remove(fam_univ_reg3)
remove(fam_univ_reg4)


############################## Sub population models (Familias) ##########################

#Now we estimate our model for many subpopulations : women\men, rural\urban, and finalyy we will do a quantile regression

#Women

mujeres <- individuos3[individuos3$sex == 1,]

did_mujeres1 <- lm(university ~ policy*treat +
                     age + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province), data = mujeres)

did_mujeres2 <- lm(informal ~ policy*treat +
                     age + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province), data = mujeres)

did_mujeres3 <- lm(employment ~ policy*treat +
                     age + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province), data = mujeres)

did_mujeres4 <- lm(migrant ~ policy*treat +
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


hombres <- individuos3[individuos3$sex == 0,]

did_hombres1 <- lm(university ~ policy*treat +
                     age + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province), data = hombres)

did_hombres2 <- lm(informal ~ policy*treat +
                     age  + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province), data = hombres)

did_hombres3 <- lm(employment ~ policy*treat +
                     age  + indigenous + afro + rural  + lformal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province), data = hombres)

did_hombres4 <- lm(migrant ~ policy*treat +
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

rural <- individuos3[individuos3$rural == 1,]

did_rural1 <- lm(university ~ policy*treat +
                   age + sex + indigenous + afro  + formal_per_house + h_tot_labour_income_pc +
                   h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                   as.factor(period) + as.factor(province), data = rural)

did_rural2 <- lm(informal ~ policy*treat +
                   age + sex + indigenous + afro  + formal_per_house + h_tot_labour_income_pc +
                   h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                   as.factor(period) + as.factor(province), data = rural)

did_rural3 <- lm(employment ~ policy*treat +
                   age + sex + indigenous + afro + formal_per_house + h_tot_labour_income_pc +
                   h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                   as.factor(period) + as.factor(province), data = rural)

did_rural4 <- lm(migrant ~ policy*treat +
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


urban <- individuos3[individuos3$rural == 0,]

did_urban1 <- lm(university ~ policy*treat +
                   age + sex + indigenous + afro  + formal_per_house + h_tot_labour_income_pc +
                   h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                   as.factor(period) + as.factor(province), data = urban)

did_urban2 <- lm(informal ~ policy*treat +
                   age + sex + indigenous + afro + formal_per_house + h_tot_labour_income_pc +
                   h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                   as.factor(period) + as.factor(province), data = urban)

did_urban3 <- lm(employment ~ policy*treat +
                   age + sex + indigenous + afro  + formal_per_house + h_tot_labour_income_pc +
                   h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                   as.factor(period) + as.factor(province), data = urban)

did_urban4 <- lm(migrant ~ policy*treat +
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

indigenas <- individuos3[individuos3$indigenous == 1,]

did_indigenas1 <- lm(university ~ policy*treat +
                       age + sex + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                       h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                       as.factor(period) + as.factor(province), data = indigenas)

did_indigenas2 <- lm(informal ~ policy*treat +
                       age + sex  + afro + rural  + lformal_per_house + h_tot_labour_income_pc +
                       h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                       as.factor(period) + as.factor(province), data = indigenas)

did_indigenas3 <- lm(employment ~ policy*treat +
                       age + sex +  afro + rural  + lformal_per_house + h_tot_labour_income_pc +
                       h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                       as.factor(period) + as.factor(province), data = indigenas)

did_indigenas4 <- lm(migrant ~ policy*treat +
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


######################### Regrssion on different quintiles (Familias) ######################


#Q1

Q1 <- individuos3[individuos3$quintil == 1 ,]

summary(Q1$h_tot_labour_income_pc)

did_q11 <- lm(university ~ policy*treat +
                age + sex + indigenous + afro + rural  + formal_per_house  +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                as.factor(period) + as.factor(province), data = Q1)

did_q12 <- lm(informal ~ policy*treat +
                age + sex + indigenous + afro + rural  + formal_per_house +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                as.factor(period) + as.factor(province), data = Q1)

did_q13 <- lm(employment ~ policy*treat +
                age + sex + indigenous + afro + rural  + formal_per_house +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                as.factor(period) + as.factor(province), data = Q1)

did_q14 <- lm(migrant ~ policy*treat +
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

Q2 <- individuos3[individuos3$quintil == 2 ,]

summary(Q2$h_tot_labour_income_pc)

did_q21 <- lm(university ~ policy*treat +
                age + sex + indigenous + afro + rural  + formal_per_house  +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                as.factor(period) + as.factor(province), data = Q2)

did_q22 <- lm(informal ~ policy*treat +
                age + sex + indigenous + afro + rural  + formal_per_house +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                as.factor(period) + as.factor(province), data = Q2)

did_q23 <- lm(employment ~ policy*treat +
                age + sex + indigenous + afro + rural  + formal_per_house  +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                as.factor(period) + as.factor(province), data = Q2)

did_q24 <- lm(migrant ~ policy*treat +
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

Q3 <- individuos3[individuos3$quintil == 3 ,]

summary(Q3$h_tot_labour_income_pc)


did_q31 <- lm(university ~ policy*treat +
                age + sex + indigenous + afro + rural  + formal_per_house  +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                as.factor(period) + as.factor(province), data = Q3)

did_q32 <- lm(informal ~ policy*treat +
                age + sex + indigenous + afro + rural  + formal_per_house +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                as.factor(period) + as.factor(province), data = Q3)

did_q33 <- lm(employment ~ policy*treat +
                age + sex + indigenous + afro + rural  + formal_per_house +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                as.factor(period) + as.factor(province), data = Q3)

did_q34 <- lm(migrant ~ policy*treat +
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

Q4 <- individuos3[individuos3$quintil == 4 ,]

summary(Q4$h_tot_labour_income_pc)

did_Q41 <- lm(university ~ policy*treat +
                age + sex + indigenous + afro + rural  + formal_per_house +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                as.factor(period) + as.factor(province), data = Q4)

did_Q42 <- lm(informal ~ policy*treat +
                age + sex + indigenous + afro + rural  + formal_per_house +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                as.factor(period) + as.factor(province), data = Q4)

did_Q43 <- lm(employment ~ policy*treat +
                age + sex + indigenous + afro + rural  + formal_per_house +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                as.factor(period) + as.factor(province), data = Q4)

did_Q44 <- lm(migrant ~ policy*treat +
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

Q5 <- individuos3[individuos3$quintil == 5 ,]

summary(Q5$h_tot_labour_income_pc)

did_Q51 <- lm(university ~ policy*treat +
                age + sex + indigenous + afro + rural  + formal_per_house +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                as.factor(period) + as.factor(province), data = Q5)

did_Q52 <- lm(informal ~ policy*treat +
                age + sex + indigenous + afro + rural  + formal_per_house +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                as.factor(period) + as.factor(province), data = Q5)

did_Q53 <- lm(employment ~ policy*treat +
                age + sex + indigenous + afro + rural  + formal_per_house +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                as.factor(period) + as.factor(province), data = Q5)

did_Q54 <- lm(migrant ~ policy*treat +
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

#################### Parallel Trend Assumption (Familias)  ###############################

PTA2 <- individuos4[individuos4$period < 2012,]

table(PTA2$period)


PTA2$policy <- rep(0, length(PTA2$policy))
PTA2$policy[PTA2$period>2008]=1
table(PTA2$policy*PTA2$treat)

PTA2_reg1 <- lm(university ~ policy*treat +
                  age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                  h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                  as.factor(period) + as.factor(province), data = PTA2)

PTA2_reg2 <- lm(informal ~ policy*treat +
                  age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                  h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                  as.factor(period) + as.factor(province), data = PTA2)

PTA2_reg3 <- lm(employment ~ policy*treat + 
                  age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                  h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                  as.factor(period) + as.factor(province), data = PTA2)

PTA2_reg4 <- lm(migrant ~ policy*treat +
                  age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                  h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                  as.factor(period) + as.factor(province), data = PTA2)


stargazer(coeftest(PTA2_reg1, vcov=vcovHC),coeftest(PTA2_reg2, vcov=vcovHC),coeftest(PTA2_reg3, vcov=vcovHC),coeftest(PTA2_reg4, vcov=vcovHC), type = "text")


remove(PTA2)
remove(PTA2_reg1)
remove(PTA2_reg2)
remove(PTA2_reg3)
remove(PTA2_reg4)

write_feather(individuos, "individuos_did.feather")
write_feather(individuos2, "individuos_PTA.feather")
write_feather(individuos3, "individuos_fam.feather")
write_feather(individuos4, "individuos_fam_PTA.feather")


remove(individuos, individuos2, individuos3, individuos4)