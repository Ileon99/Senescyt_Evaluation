################### Treatment group cohort that passed the exam ####################

#We will take 2013 and compare two cohorts that passed the exam 
#The second variation is geographically

treatment_group <- read_feather("C:/Users/ignac/OneDrive/Documentos/PHD/Senescyt_Evaluation/Data/treat_check.feather")
Enemdu <- read_feather("C:/Users/ignac/OneDrive/Documentos/PHD/Senescyt_Evaluation/Data/Enemdu2.feather")

#Now we will focuss on doing the DiD with individuals in the cohort of treatment 
#We will keep people that have completed high school between 17 - 19 years

treatment_group <- treatment_group[, c("province","treatment_group")]

names(treatment_group) <- c("province","treat_group")

table(Enemdu$cohort)

individuos <- Enemdu[Enemdu$cohort == "17-19" & Enemdu$period == 2013 | Enemdu$cohort == "20-22"& Enemdu$period == 2013,]
individuos <- individuos[individuos$education > 6,]

individuos$migrant_student <- individuos$migrant * individuos$university

individuos <- individuos[individuos$province != "GALAPAGOS",]
individuos <- individuos[individuos$province != "ZONAS NO DELIMITADAS",]

individuos <- merge(individuos, treatment_group, by = "province")

individuos$province_var <- individuos$treat_group

individuos$treat_group <- rep(0, length(individuos$province))
individuos$treat_group[individuos$cohort == "17-19"]=1


individuos$lformal_per_house <- ifelse(individuos$formal_per_house < 1, 0 ,log(individuos$formal_per_house))

individuos$lh_tot_labour_income_pc <- ifelse(individuos$h_tot_labour_income_pc < 1, 0 ,log(individuos$h_tot_labour_income_pc))

individuos$lnhousehold.y <- ifelse(individuos$nhousehold.y < 1, 0 ,log(individuos$nhousehold.y))

remove(Enemdu)
remove(treatment_group)



# DID

coh_reg1 <- lm(university ~ treat_group*province_var + 
                      age + sex + indigenous + afro + rural + formal_per_house + h_tot_labour_income_pc + 
                      h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +  as.factor(province),
                    data = individuos)


stargazer(coh_reg1 , type = "text")


coh_reg2 <- lm(informal ~ treat_group*province_var
                    + age + sex + indigenous + afro + rural + formal_per_house + h_tot_labour_income_pc + 
                      h_little_kids + h_kid + h_teen + h_adult + nhousehold.y + as.factor(province), data = individuos)

stargazer(coh_reg2, type = "text")


coh_reg3 <- lm(employment ~ treat_group*province_var +
                      age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                      h_little_kids + h_kid + h_teen + h_adult + nhousehold.y + as.factor(province), data = individuos)

coh_reg4 <- lm(migrant_student ~ treat_group*province_var +
                      age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                      h_little_kids + h_kid + h_teen + h_adult + nhousehold.y + as.factor(province), data = individuos)

stargazer(coh_reg3, type = "text")

stargazer(coeftest(coh_reg1, vcov=vcovHC),coeftest(coh_reg3, vcov=vcovHC),coeftest(coh_reg2, vcov=vcovHC), coeftest(coh_reg4, vcov=vcovHC), type = "text")

cluster_se1 <- vcovHC(coh_reg1, cluster = individuos3$id_hogar)

cluster_se2 <- vcovHC(coh_reg2, cluster = individuos3$id_hogar)

cluster_se3 <- vcovHC(coh_reg3, cluster = individuos3$id_hogar)

cluster_se4 <- vcovHC(coh_reg4, cluster = individuos3$id_hogar)


stargazer(coeftest(coh_reg1, vcov=cluster_se1),coeftest(coh_reg3, vcov=cluster_se3),coeftest(coh_reg2, vcov=cluster_se2), coeftest(coh_reg4, vcov=cluster_se4), type = "text")

remove(cluster_se1)
remove(cluster_se2)
remove(cluster_se3)
remove(cluster_se4)
remove(coh_reg1)
remove(coh_reg2)
remove(coh_reg3)
remove(coh_reg4)


############################### PTA ###########################################

#For the parallel trend assumption we are going to do the same regression but in 2010 

#We will take 2013 and compare two cohorts that passed the exam 
#The second variation is geographically

treatment_group <- read_feather("C:/Users/ignac/OneDrive/Documentos/PHD/Senescyt_Evaluation/Data/treat_check.feather")
Enemdu <- read_feather("C:/Users/ignac/OneDrive/Documentos/PHD/Senescyt_Evaluation/Data/Enemdu2.feather")

#Now we will focuss on doing the DiD with individuals in the cohort of treatment 
#We will keep people that have completed high school between 17 - 19 years

treatment_group <- treatment_group[, c("province","treatment_group")]

names(treatment_group) <- c("province","treat_group")

table(Enemdu$cohort)

individuos <- Enemdu[Enemdu$cohort == "17-19" & Enemdu$period == 2008 | Enemdu$cohort == "20-22"& Enemdu$period == 2008,]
individuos <- individuos[individuos$education > 6,]

individuos$migrant_student <- individuos$migrant * individuos$university

individuos <- individuos[individuos$province != "GALAPAGOS",]
individuos <- individuos[individuos$province != "ZONAS NO DELIMITADAS",]

individuos <- merge(individuos, treatment_group, by = "province")

individuos$province_var <- individuos$treat_group

individuos$treat_group <- rep(0, length(individuos$province))
individuos$treat_group[individuos$cohort == "20-22"]=1


individuos$lformal_per_house <- ifelse(individuos$formal_per_house < 1, 0 ,log(individuos$formal_per_house))

individuos$lh_tot_labour_income_pc <- ifelse(individuos$h_tot_labour_income_pc < 1, 0 ,log(individuos$h_tot_labour_income_pc))

individuos$lnhousehold.y <- ifelse(individuos$nhousehold.y < 1, 0 ,log(individuos$nhousehold.y))

remove(Enemdu)
remove(treatment_group)



# DID

coh_reg1 <- lm(university ~ treat_group*province_var + 
                 age + sex + indigenous + afro + rural + formal_per_house + h_tot_labour_income_pc + 
                 h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +  as.factor(province),
               data = individuos)


stargazer(coh_reg1 , type = "text")


coh_reg2 <- lm(informal ~ treat_group*province_var
               + age + sex + indigenous + afro + rural + formal_per_house + h_tot_labour_income_pc + 
                 h_little_kids + h_kid + h_teen + h_adult + nhousehold.y + as.factor(province), data = individuos)

stargazer(coh_reg2, type = "text")


coh_reg3 <- lm(employment ~ treat_group*province_var +
                 age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                 h_little_kids + h_kid + h_teen + h_adult + nhousehold.y + as.factor(province), data = individuos)

coh_reg4 <- lm(migrant ~ treat_group*province_var +
                 age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                 h_little_kids + h_kid + h_teen + h_adult + nhousehold.y + as.factor(province), data = individuos)

stargazer(coh_reg3, type = "text")

stargazer(coeftest(coh_reg1, vcov=vcovHC),coeftest(coh_reg3, vcov=vcovHC),coeftest(coh_reg2, vcov=vcovHC), coeftest(coh_reg4, vcov=vcovHC), type = "text")

cluster_se1 <- vcovHC(coh_reg1, cluster = individuos3$id_hogar)

cluster_se2 <- vcovHC(coh_reg2, cluster = individuos3$id_hogar)

cluster_se3 <- vcovHC(coh_reg3, cluster = individuos3$id_hogar)

cluster_se4 <- vcovHC(coh_reg4, cluster = individuos3$id_hogar)


stargazer(coeftest(coh_reg1, vcov=cluster_se1),coeftest(coh_reg3, vcov=cluster_se3),coeftest(coh_reg2, vcov=cluster_se2), coeftest(coh_reg4, vcov=cluster_se4), type = "text")

remove(cluster_se1)
remove(cluster_se2)
remove(cluster_se3)
remove(cluster_se4)
remove(coh_reg1)
remove(coh_reg2)
remove(coh_reg3)
remove(coh_reg4)
