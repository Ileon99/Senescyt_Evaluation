# Robustness Checks 

library(feather)
library(readr)
library(dplyr)
library(ineq)
library(stargazer)
library(AER)

############# Robustness of the province control group ###################

individuos <- read_feather("C:/Users/ignac/OneDrive/Documentos/PHD/Senescyt_Evaluation/Data/individuos_did.feather")
treatment_group <- read_feather("C:/Users/ignac/OneDrive/Documentos/PHD/Senescyt_Evaluation/Data/treat_check.feather")

# This is what we conclude from the clustering robustness checks : 

#We picked two dimensions for the PCA even if the kaiser method suggest to pick 4 dimensions. 
#However when we did the clustering for 3 and four dimensions we get the same results : 

table(treatment_group$PCA_2dim, treatment_group$PCA_3dim)
table(treatment_group$PCA_2dim, treatment_group$PCA_4dim)

#Therefore, we do not need to check for this problem in our diff-in-diff
#However, we see that the k-means without PCA has a different control group

table(treatment_group$PCA_2dim, treatment_group$kmeans )

#El Oro is not in the treatment group in this k-means so we test what happens when we leave El Oro from the control group

########################### K-means without PCA ###############################

#Let's put El Oro in the Treatment group 

robustness <- merge(individuos, treatment_group, by = "province")
remove(individuos)

sinElOro <- robustness

sinElOro$treat_group <- sinElOro$kmeans

did_sinElOro1 <- lm(university ~ policy*treat_group +
                     age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province), data = sinElOro)

did_sinElOro2 <- lm(informal ~ policy*treat_group +
                     age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province), data = sinElOro)

did_sinElOro3 <- lm(employment ~ policy*treat_group +
                     age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province), data = sinElOro)

did_sinElOro4 <- lm(migrant ~ policy*treat_group +
                     age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province),
                   data = sinElOro)


stargazer(coeftest(did_sinElOro1, vcov=vcovHC) ,coeftest(did_sinElOro3, vcov=vcovHC), coeftest(did_sinElOro2, vcov=vcovHC), coeftest(did_sinElOro4, vcov=vcovHC), type = "text")

cluster_se1 <- vcovHC(did_sinElOro1, cluster = sinElOro$id_hogar)

cluster_se2 <- vcovHC(did_sinElOro2, cluster = sinElOro$id_hogar)

cluster_se3 <- vcovHC(did_sinElOro3, cluster = sinElOro$id_hogar)

cluster_se4 <- vcovHC(did_sinElOro4, cluster = sinElOro$id_hogar)


stargazer(coeftest(did_sinElOro1, vcov=cluster_se1),coeftest(did_sinElOro3, vcov=cluster_se3),coeftest(did_sinElOro2, vcov=cluster_se2), coeftest(did_sinElOro4, vcov=cluster_se4), type = "text")

remove(cluster_se1)
remove(cluster_se2)
remove(cluster_se3)
remove(cluster_se4)


remove(sinElOro)
remove(did_sinElOro1)
remove(did_sinElOro2)
remove(did_sinElOro3)
remove(did_sinElOro4)

#We do not get anymore significant on employment results because the treat group is too little 
#However we still have significant results on migration

table(sinElOro$treat_group)
table(sinElOro$treat_group,sinElOro$policy)

#For next robustness checks we should add provinces in the control group and not leave them 

########################## Temporal checks of the PCA #########################

#When we analyze the temporal variance of our control group we get that : 

#There are some provinces that are in the control group in 2010 and are not in 2011

#Azuay : 2012-2013
#Chimborazo : is in 2010 but not in any other
#Loja : 2012-2013
#Tungurahua : 2012-2013

#Picincha is always in the control group and Santa elena is in the control group only on 2012-2013

#When we see what is happening in the clustering in 2012 and 2013, we see that 
#Pichincha and Santa elena are the only provinces in the control group, and there
#is another cluster where there are all the other control provinces, in 2012 with Guayas
#in 2013 with Napo

#There are three other provinces that appears in the control group in some years : 

#Carchi : 2011-2015

#Guayas: 2011 - 2015 - 2018 

#Imbabura : 2015 - 2016 - 2017

#Napo : 2011 - 2018

#Santa Elena: 2012-2013

# We should add one by one each province to the control group in order to check for robustness


##When we add Carchi to the control group : 

conCarchi <- robustness

table(conCarchi$treat_group)

conCarchi$treat_group[conCarchi$province== "CARCHI"]=0

table(conCarchi$treat_group)

did_conCarchi1 <- lm(university ~ policy*treat_group +
                     age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province), data = conCarchi)

did_conCarchi2 <- lm(informal ~ policy*treat_group +
                     age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province), data = conCarchi)

did_conCarchi3 <- lm(employment ~ policy*treat_group +
                     age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province), data = conCarchi)

did_conCarchi4 <- lm(migrant ~ policy*treat_group +
                     age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province),
                   data = conCarchi)


stargazer(coeftest(did_conCarchi1, vcov=vcovHC) ,coeftest(did_conCarchi3, vcov=vcovHC), coeftest(did_conCarchi2, vcov=vcovHC), coeftest(did_conCarchi4, vcov=vcovHC), type = "text")

cluster_se1 <- vcovHC(did_conCarchi1, cluster = conCarchi$id_hogar)

cluster_se2 <- vcovHC(did_conCarchi2, cluster = conCarchi$id_hogar)

cluster_se3 <- vcovHC(did_conCarchi3, cluster = conCarchi$id_hogar)

cluster_se4 <- vcovHC(did_conCarchi4, cluster = conCarchi$id_hogar)


stargazer(coeftest(did_conCarchi1, vcov=cluster_se1),coeftest(did_conCarchi3, vcov=cluster_se3),coeftest(did_conCarchi2, vcov=cluster_se2), coeftest(did_conCarchi4, vcov=cluster_se4), type = "text")

#When we add Carchi to the control group the only effect of the policy that remains significant is on informal

remove(cluster_se1)
remove(cluster_se2)
remove(cluster_se3)
remove(cluster_se4)


remove(conCarchi)
remove(did_conCarchi1)
remove(did_conCarchi2)
remove(did_conCarchi3)
remove(did_conCarchi4) 


## When we add Guayas to the control group : 

conGuayas <- robustness

table(conGuayas$treat_group)

conGuayas$treat_group[conGuayas$province== "GUAYAS"]=0

table(conGuayas$treat_group)


did_conGuayas1 <- lm(university ~ policy*treat_group +
                     age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province), data = conGuayas)

did_conGuayas2 <- lm(informal ~ policy*treat_group +
                     age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province), data = conGuayas)

did_conGuayas3 <- lm(employment ~ policy*treat_group +
                     age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province), data = conGuayas)

did_conGuayas4 <- lm(migrant ~ policy*treat_group +
                     age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province),
                   data = conGuayas)


stargazer(coeftest(did_conGuayas1, vcov=vcovHC) ,coeftest(did_conGuayas3, vcov=vcovHC), coeftest(did_conGuayas2, vcov=vcovHC), coeftest(did_conGuayas4, vcov=vcovHC), type = "text")

cluster_se1 <- vcovHC(did_conGuayas1, cluster = conGuayas$id_hogar)

cluster_se2 <- vcovHC(did_conGuayas2, cluster = conGuayas$id_hogar)

cluster_se3 <- vcovHC(did_conGuayas3, cluster = conGuayas$id_hogar)

cluster_se4 <- vcovHC(did_conGuayas4, cluster = conGuayas$id_hogar)


stargazer(coeftest(did_conGuayas1, vcov=cluster_se1),coeftest(did_conGuayas3, vcov=cluster_se3),coeftest(did_conGuayas2, vcov=cluster_se2), coeftest(did_conGuayas4, vcov=cluster_se4), type = "text")

#When we add Guayas to the control group we have a less significant effect on the informal employment but we still have the same type of effects

remove(cluster_se1)
remove(cluster_se2)
remove(cluster_se3)
remove(cluster_se4)


remove(conGuayas)
remove(did_conGuayas1)
remove(did_conGuayas2)
remove(did_conGuayas3)
remove(did_conGuayas4)

## When we add Imbabura to the control group : 

conImbabura <- robustness

table(conImbabura$treat_group)

conImbabura$treat_group[conImbabura$province== "IMBABURA"]=0

table(conImbabura$treat_group)

did_conImbabura1 <- lm(university ~ policy*treat_group +
                     age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province), data = conImbabura)

did_conImbabura2 <- lm(informal ~ policy*treat_group +
                     age + sex +indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province), data = conImbabura)

did_conImbabura3 <- lm(employment ~ policy*treat_group +
                     age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province), data = conImbabura)

did_conImbabura4 <- lm(migrant ~ policy*treat_group +
                     age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province),
                   data = conImbabura)


stargazer(coeftest(did_conImbabura1, vcov=vcovHC) ,coeftest(did_conImbabura3, vcov=vcovHC), coeftest(did_conImbabura2, vcov=vcovHC), coeftest(did_conImbabura4, vcov=vcovHC), type = "text")

cluster_se1 <- vcovHC(did_conImbabura1, cluster = conImbabura$id_hogar)

cluster_se2 <- vcovHC(did_conImbabura2, cluster = conImbabura$id_hogar)

cluster_se3 <- vcovHC(did_conImbabura3, cluster = conImbabura$id_hogar)

cluster_se4 <- vcovHC(did_conImbabura4, cluster = conImbabura$id_hogar)


stargazer(coeftest(did_conImbabura1, vcov=cluster_se1),coeftest(did_conImbabura3, vcov=cluster_se3),coeftest(did_conImbabura2, vcov=cluster_se2), coeftest(did_conImbabura4, vcov=cluster_se4), type = "text")

#When we add Imbabura to the control group we got the same results that in the base regression

remove(cluster_se1)
remove(cluster_se2)
remove(cluster_se3)
remove(cluster_se4)


remove(conImbabura)
remove(did_conImbabura1)
remove(did_conImbabura2)
remove(did_conImbabura3)
remove(did_conImbabura4)

## When we add Napo to the control group : 

conNapo <- robustness

table(conNapo$treat_group)

conNapo$treat_group[conNapo$province== "NAPO"]=0

table(conNapo$treat_group)

did_conNapo1 <- lm(university ~ policy*treat_group +
                     age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province), data = conNapo)

did_conNapo2 <- lm(informal ~ policy*treat_group +
                     age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province), data = conNapo)

did_conNapo3 <- lm(employment ~ policy*treat_group +
                     age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province), data = conNapo)

did_conNapo4 <- lm(migrant ~ policy*treat_group +
                     age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province),
                   data = conNapo)


stargazer(coeftest(did_conNapo1, vcov=vcovHC) ,coeftest(did_conNapo3, vcov=vcovHC), coeftest(did_conNapo2, vcov=vcovHC), coeftest(did_conNapo4, vcov=vcovHC), type = "text")

cluster_se1 <- vcovHC(did_conNapo1, cluster = conNapo$id_hogar)

cluster_se2 <- vcovHC(did_conNapo2, cluster = conNapo$id_hogar)

cluster_se3 <- vcovHC(did_conNapo3, cluster = conNapo$id_hogar)

cluster_se4 <- vcovHC(did_conNapo4, cluster = conNapo$id_hogar)


stargazer(coeftest(did_conNapo1, vcov=cluster_se1),coeftest(did_conNapo3, vcov=cluster_se3),coeftest(did_conNapo2, vcov=cluster_se2), coeftest(did_conNapo4, vcov=cluster_se4), type = "text")

#We have the same results when we add Napo

remove(cluster_se1)
remove(cluster_se2)
remove(cluster_se3)
remove(cluster_se4)


remove(conNapo)
remove(did_conNapo1)
remove(did_conNapo2)
remove(did_conNapo3)
remove(did_conNapo4)

## When we add Santa Elena to the control group : 

conSantaElena <- robustness

table(conSantaElena$treat_group)

conSantaElena$treat_group[conSantaElena$province== "SANTA ELENA"]=0

table(conSantaElena$treat_group)


did_conSantaElena1 <- lm(university ~ policy*treat_group +
                     age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province), data = conSantaElena)

did_conSantaElena2 <- lm(informal ~ policy*treat_group +
                     age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province), data = conSantaElena)

did_conSantaElena3 <- lm(employment ~ policy*treat_group +
                     age + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province), data = conSantaElena)

did_conSantaElena4 <- lm(migrant ~ policy*treat_group +
                     age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province),
                   data = conSantaElena)


stargazer(coeftest(did_conSantaElena1, vcov=vcovHC) ,coeftest(did_conSantaElena3, vcov=vcovHC), coeftest(did_conSantaElena2, vcov=vcovHC), coeftest(did_conSantaElena4, vcov=vcovHC), type = "text")

cluster_se1 <- vcovHC(did_conSantaElena1, cluster = conSantaElena$id_hogar)

cluster_se2 <- vcovHC(did_conSantaElena2, cluster = conSantaElena$id_hogar)

cluster_se3 <- vcovHC(did_conSantaElena3, cluster = conSantaElena$id_hogar)

cluster_se4 <- vcovHC(did_conSantaElena4, cluster = conSantaElena$id_hogar)


stargazer(coeftest(did_conSantaElena1, vcov=cluster_se1),coeftest(did_conSantaElena3, vcov=cluster_se3),coeftest(did_conSantaElena2, vcov=cluster_se2), coeftest(did_conSantaElena4, vcov=cluster_se4), type = "text")

#Quite same results when we add Santa Elena, only we have the effect on employment that is less significant

remove(cluster_se1)
remove(cluster_se2)
remove(cluster_se3)
remove(cluster_se4)


remove(conSantaElena)
remove(did_conSantaElena1)
remove(did_conSantaElena2)
remove(did_conSantaElena3)
remove(did_conSantaElena4)

## When we add these 4 provinces to the control group : 

robustness2 <- robustness

table(robustness2$treat_group)

robustness2$treat_group[robustness2$province== "SANTA ELENA" | robustness2$province== "CARCHI" | robustness2$province== "GUAYAS" | robustness2$province== "NAPO" | robustness2$province== "IMBABURA"]=0

table(robustness2$treat_group)

did_robustness21 <- lm(university ~ policy*treat_group +
                     age + sex +  indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province), data = robustness2)

did_robustness22 <- lm(informal ~ policy*treat_group +
                     age + sex +  indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province), data = robustness2)

did_robustness23 <- lm(employment ~ policy*treat_group +
                     age + sex +  indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province), data = robustness2)

did_robustness24 <- lm(migrant ~ policy*treat_group +
                     age + sex +  indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province),
                   data = robustness2)


stargazer(coeftest(did_robustness21, vcov=vcovHC) ,coeftest(did_robustness23, vcov=vcovHC), coeftest(did_robustness22, vcov=vcovHC), coeftest(did_robustness24, vcov=vcovHC), type = "text")

cluster_se1 <- vcovHC(did_robustness21, cluster = robustness2$id_hogar)

cluster_se2 <- vcovHC(did_robustness22, cluster = robustness2$id_hogar)

cluster_se3 <- vcovHC(did_robustness23, cluster = robustness2$id_hogar)

cluster_se4 <- vcovHC(did_robustness24, cluster = robustness2$id_hogar)


stargazer(coeftest(did_robustness21, vcov=cluster_se1),coeftest(did_robustness23, vcov=cluster_se3),coeftest(did_robustness22, vcov=cluster_se2), coeftest(did_robustness24, vcov=cluster_se4), type = "text")

#We have the same kind of results

remove(cluster_se1)
remove(cluster_se2)
remove(cluster_se3)
remove(cluster_se4)


remove(robustness2)
remove(did_robustness21)
remove(did_robustness22)
remove(did_robustness23)
remove(did_robustness24)

remove(robustness, treatment_group)

############### Robustness checks with means of each variable ###############

#We are going to create control groups from the mean of each variable 


individuos <- read_feather("C:/Users/ignac/OneDrive/Documentos/PHD/Senescyt_Evaluation/Data/individuos_did.feather")
educ_indices2010 <- read_feather("C:/Users/ignac/OneDrive/Documentos/PHD/Senescyt_Evaluation/Data/educ_indices2010.feather")

treatment_robust <- educ_indices2010[educ_indices2010$province_c != 90 & educ_indices2010$province_c != 20,]

remove(educ_indices2010)

#Basic Education Enrollment rate 


treatment_robust$beer <- ifelse(treatment_robust$beer > mean(treatment_robust$beer), 0, 1)

beer <- treatment_robust[, c("province","beer")]

names(beer) <- c("province", "treat_educ")

beer <- merge(individuos, beer, by = "province")

table(beer$policy, beer$treat_educ)

did_beer1 <- lm(university ~ policy*treat_educ +
                     age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province), data = beer)

did_beer2 <- lm(informal ~ policy*treat_educ +
                     age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province), data = beer)

did_beer3 <- lm(employment ~ policy*treat_educ +
                     age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province), data = beer)

did_beer4 <- lm(migrant ~ policy*treat_educ +
                     age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province),
                   data = beer)


stargazer(coeftest(did_beer1, vcov=vcovHC) ,coeftest(did_beer3, vcov=vcovHC), coeftest(did_beer2, vcov=vcovHC), coeftest(did_beer4, vcov=vcovHC), type = "text")

cluster_se1 <- vcovHC(did_beer1, cluster = beer$id_hogar)

cluster_se2 <- vcovHC(did_beer2, cluster = beer$id_hogar)

cluster_se3 <- vcovHC(did_beer3, cluster = beer$id_hogar)

cluster_se4 <- vcovHC(did_beer4, cluster = beer$id_hogar)


stargazer(coeftest(did_beer1, vcov=cluster_se1),coeftest(did_beer3, vcov=cluster_se3),coeftest(did_beer2, vcov=cluster_se2), coeftest(did_beer4, vcov=cluster_se4), type = "text")

#Nice we do not have the same results because now most of the provinces that were in the control group are treated

remove(cluster_se1)
remove(cluster_se2)
remove(cluster_se3)
remove(cluster_se4)

remove(beer)
remove(did_beer1)
remove(did_beer2)
remove(did_beer3)
remove(did_beer4)

#High School enrollment rate

treatment_robust$hser <- ifelse(treatment_robust$hser > mean(treatment_robust$hser), 0, 1)

hser <- treatment_robust[, c("province","hser")]

names(hser) <- c("province", "treat_educ")

hser <- merge(individuos, hser, by = "province")

table(hser$policy, hser$treat_educ)


did_hser1 <- lm(university ~ policy*treat_educ +
                     age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province), data = hser)

did_hser2 <- lm(informal ~ policy*treat_educ +
                     age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province), data = hser)

did_hser3 <- lm(employment ~ policy*treat_educ +
                     age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province), data = hser)

did_hser4 <- lm(migrant ~ policy*treat_educ +
                     age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                     h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                     as.factor(period) + as.factor(province),
                   data = hser)


stargazer(coeftest(did_hser1, vcov=vcovHC) ,coeftest(did_hser3, vcov=vcovHC), coeftest(did_hser2, vcov=vcovHC), coeftest(did_hser4, vcov=vcovHC), type = "text")

cluster_se1 <- vcovHC(did_hser1, cluster = hser$id_hogar)

cluster_se2 <- vcovHC(did_hser2, cluster = hser$id_hogar)

cluster_se3 <- vcovHC(did_hser3, cluster = hser$id_hogar)

cluster_se4 <- vcovHC(did_hser4, cluster = hser$id_hogar)


stargazer(coeftest(did_hser1, vcov=cluster_se1),coeftest(did_hser3, vcov=cluster_se3),coeftest(did_hser2, vcov=cluster_se2), coeftest(did_hser4, vcov=cluster_se4), type = "text")

#When we make the control group with the mean of hser we got same kind of results but more significant
#2 explanations : 

#The same provinces that are in the control in the k-means are in this control 

#There are more individuals in the control group 

remove(cluster_se1)
remove(cluster_se2)
remove(cluster_se3)
remove(cluster_se4)


remove(hser)
remove(did_hser1)
remove(did_hser2)
remove(did_hser3)
remove(did_hser4)


#Basic Education Enrollment rate in the rural areas

treatment_robust$beer_rur <- ifelse(treatment_robust$beer_rur > mean(treatment_robust$beer_rur), 0, 1)

beer_rur <- treatment_robust[, c("province","beer_rur")]

names(beer_rur) <- c("province", "treat_educ")

beer_rur <- merge(individuos, beer_rur, by = "province")

table(beer_rur$policy, beer_rur$treat_educ)


did_beer_rur1 <- lm(university ~ policy*treat_educ +
                  age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                  h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                  as.factor(period) + as.factor(province), data = beer_rur)

did_beer_rur2 <- lm(informal ~ policy*treat_educ +
                  age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                  h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                  as.factor(period) + as.factor(province), data = beer_rur)

did_beer_rur3 <- lm(employment ~ policy*treat_educ +
                  age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                  h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                  as.factor(period) + as.factor(province), data = beer_rur)

did_beer_rur4 <- lm(migrant ~ policy*treat_educ +
                  age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                  h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                  as.factor(period) + as.factor(province),
                data = beer_rur)


stargazer(coeftest(did_beer_rur1, vcov=vcovHC) ,coeftest(did_beer_rur3, vcov=vcovHC), coeftest(did_beer_rur2, vcov=vcovHC), coeftest(did_beer_rur4, vcov=vcovHC), type = "text")

cluster_se1 <- vcovHC(did_beer_rur1, cluster = beer_rur$id_hogar)

cluster_se2 <- vcovHC(did_beer_rur2, cluster = beer_rur$id_hogar)

cluster_se3 <- vcovHC(did_beer_rur3, cluster = beer_rur$id_hogar)

cluster_se4 <- vcovHC(did_beer_rur4, cluster = beer_rur$id_hogar)


stargazer(coeftest(did_beer_rur1, vcov=cluster_se1),coeftest(did_beer_rur3, vcov=cluster_se3),coeftest(did_beer_rur2, vcov=cluster_se2), coeftest(did_beer_rur4, vcov=cluster_se4), type = "text")


#For the beer in the rural areas we got the same provinces that we had in the control group before more some others
#We get that for the employment and informal the results remain significant
#However for migrant it is no more significant this is for the fact that more rural provinces are counted now
#This migration go usually from rural to urban areas


remove(cluster_se1)
remove(cluster_se2)
remove(cluster_se3)
remove(cluster_se4)


remove(beer_rur)
remove(did_beer_rur1)
remove(did_beer_rur2)
remove(did_beer_rur3)
remove(did_beer_rur4)

#Basic Education Enrollment rate in the urban areas

treatment_robust$beer_urb <- ifelse(treatment_robust$beer_urb > mean(treatment_robust$beer_urb), 0, 1)

beer_urb <- treatment_robust[, c("province","beer_urb")]

names(beer_urb) <- c("province", "treat_educ")

beer_urb <- merge(individuos, beer_urb, by = "province")

table(beer_urb$policy, beer_urb$treat_educ)


did_beer_urb1 <- lm(university ~ policy*treat_educ +
                      age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                      h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                      as.factor(period) + as.factor(province), data = beer_urb)

did_beer_urb2 <- lm(informal ~ policy*treat_educ +
                      age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                      h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                      as.factor(period) + as.factor(province), data = beer_urb)

did_beer_urb3 <- lm(employment ~ policy*treat_educ +
                      age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                      h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                      as.factor(period) + as.factor(province), data = beer_urb)

did_beer_urb4 <- lm(migrant ~ policy*treat_educ +
                      age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                      h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                      as.factor(period) + as.factor(province),
                    data = beer_urb)


stargazer(coeftest(did_beer_urb1, vcov=vcovHC) ,coeftest(did_beer_urb3, vcov=vcovHC), coeftest(did_beer_urb2, vcov=vcovHC), coeftest(did_beer_urb4, vcov=vcovHC), type = "text")

cluster_se1 <- vcovHC(did_beer_urb1, cluster = beer_urb$id_hogar)

cluster_se2 <- vcovHC(did_beer_urb2, cluster = beer_urb$id_hogar)

cluster_se3 <- vcovHC(did_beer_urb3, cluster = beer_urb$id_hogar)

cluster_se4 <- vcovHC(did_beer_urb4, cluster = beer_urb$id_hogar)


stargazer(coeftest(did_beer_urb1, vcov=cluster_se1),coeftest(did_beer_urb3, vcov=cluster_se3),coeftest(did_beer_urb2, vcov=cluster_se2), coeftest(did_beer_urb4, vcov=cluster_se4), type = "text")

#Here we have the opposite effect that is what we are looking for : 
#the provinces that were in the control are in the treatment and vice-versa
#Now we have a significant effect on university, which is interesting 
#In provinces where education quality (for the PCA-K-means) is higher the inscription on universities increased

remove(cluster_se1)
remove(cluster_se2)
remove(cluster_se3)
remove(cluster_se4)


remove(beer_urb)
remove(did_beer_urb1)
remove(did_beer_urb2)
remove(did_beer_urb3)
remove(did_beer_urb4)

#High School Enrollment rate in the urban areas

treatment_robust$hser_urb <- ifelse(treatment_robust$hser_urb > mean(treatment_robust$hser_urb), 0, 1)

hser_urb <- treatment_robust[, c("province","hser_urb")]

names(hser_urb) <- c("province", "treat_educ")

hser_urb <- merge(individuos, hser_urb, by = "province")

table(hser_urb$policy, hser_urb$treat_educ)


did_hser_urb1 <- lm(university ~ policy*treat_educ +
                      age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                      h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                      as.factor(period) + as.factor(province), data = hser_urb)

did_hser_urb2 <- lm(informal ~ policy*treat_educ +
                      age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                      h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                      as.factor(period) + as.factor(province), data = hser_urb)

did_hser_urb3 <- lm(employment ~ policy*treat_educ +
                      age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                      h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                      as.factor(period) + as.factor(province), data = hser_urb)

did_hser_urb4 <- lm(migrant ~ policy*treat_educ +
                      age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                      h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                      as.factor(period) + as.factor(province),
                    data = hser_urb)


stargazer(coeftest(did_hser_urb1, vcov=vcovHC) ,coeftest(did_hser_urb3, vcov=vcovHC), coeftest(did_hser_urb2, vcov=vcovHC), coeftest(did_hser_urb4, vcov=vcovHC), type = "text")

cluster_se1 <- vcovHC(did_hser_urb1, cluster = hser_urb$id_hogar)

cluster_se2 <- vcovHC(did_hser_urb2, cluster = hser_urb$id_hogar)

cluster_se3 <- vcovHC(did_hser_urb3, cluster = hser_urb$id_hogar)

cluster_se4 <- vcovHC(did_hser_urb4, cluster = hser_urb$id_hogar)


stargazer(coeftest(did_hser_urb1, vcov=cluster_se1),coeftest(did_hser_urb3, vcov=cluster_se3),coeftest(did_hser_urb2, vcov=cluster_se2), coeftest(did_hser_urb4, vcov=cluster_se4), type = "text")


#Not significant : same explanation than for beer_urb

remove(cluster_se1)
remove(cluster_se2)
remove(cluster_se3)
remove(cluster_se4)


remove(hser_urb)
remove(did_hser_urb1)
remove(did_hser_urb2)
remove(did_hser_urb3)
remove(did_hser_urb4)


#High School Enrollment rate in the rural areas

treatment_robust$hser_rur <- ifelse(treatment_robust$hser_rur > mean(treatment_robust$hser_rur), 0, 1)

hser_rur <- treatment_robust[, c("province","hser_rur")]

names(hser_rur) <- c("province", "treat_educ")

hser_rur <- merge(individuos, hser_rur, by = "province")

table(hser_rur$policy, hser_rur$treat_educ)


did_hser_rur1 <- lm(university ~ policy*treat_educ +
                      age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                      h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                      as.factor(period) + as.factor(province), data = hser_rur)

did_hser_rur2 <- lm(informal ~ policy*treat_educ +
                      age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                      h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                      as.factor(period) + as.factor(province), data = hser_rur)

did_hser_rur3 <- lm(employment ~ policy*treat_educ +
                      age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                      h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                      as.factor(period) + as.factor(province), data = hser_rur)

did_hser_rur4 <- lm(migrant ~ policy*treat_educ +
                      age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                      h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                      as.factor(period) + as.factor(province),
                    data = hser_rur)


stargazer(coeftest(did_hser_rur1, vcov=vcovHC) ,coeftest(did_hser_rur3, vcov=vcovHC), coeftest(did_hser_rur2, vcov=vcovHC), coeftest(did_hser_rur4, vcov=vcovHC), type = "text")

cluster_se1 <- vcovHC(did_hser_rur1, cluster = hser_rur$id_hogar)

cluster_se2 <- vcovHC(did_hser_rur2, cluster = hser_rur$id_hogar)

cluster_se3 <- vcovHC(did_hser_rur3, cluster = hser_rur$id_hogar)

cluster_se4 <- vcovHC(did_hser_rur4, cluster = hser_rur$id_hogar)


stargazer(coeftest(did_hser_rur1, vcov=cluster_se1),coeftest(did_hser_rur3, vcov=cluster_se3),coeftest(did_hser_rur2, vcov=cluster_se2), coeftest(did_hser_rur4, vcov=cluster_se4), type = "text")

#We got significant results for employment informal and migrant

remove(cluster_se1)
remove(cluster_se2)
remove(cluster_se3)
remove(cluster_se4)


remove(hser_rur)
remove(did_hser_rur1)
remove(did_hser_rur2)
remove(did_hser_rur3)
remove(did_hser_rur4)

#Education Years 

treatment_robust$educ_years <- ifelse(treatment_robust$educ_years > mean(treatment_robust$educ_years), 0, 1)

educ_years <- treatment_robust[, c("province","educ_years")]

names(educ_years) <- c("province", "treat_educ")

educ_years <- merge(individuos, educ_years, by = "province")

table(educ_years$policy, educ_years$treat_educ)


did_educ_years1 <- lm(university ~ policy*treat_educ +
                      age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                      h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                      as.factor(period) + as.factor(province), data = educ_years)

did_educ_years2 <- lm(informal ~ policy*treat_educ +
                      age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                      h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                      as.factor(period) + as.factor(province), data = educ_years)

did_educ_years3 <- lm(employment ~ policy*treat_educ +
                      age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                      h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                      as.factor(period) + as.factor(province), data = educ_years)

did_educ_years4 <- lm(migrant ~ policy*treat_educ +
                      age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                      h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                      as.factor(period) + as.factor(province),
                    data = educ_years)


stargazer(coeftest(did_educ_years1, vcov=vcovHC) ,coeftest(did_educ_years3, vcov=vcovHC), coeftest(did_educ_years2, vcov=vcovHC), coeftest(did_educ_years4, vcov=vcovHC), type = "text")

cluster_se1 <- vcovHC(did_educ_years1, cluster = educ_years$id_hogar)

cluster_se2 <- vcovHC(did_educ_years2, cluster = educ_years$id_hogar)

cluster_se3 <- vcovHC(did_educ_years3, cluster = educ_years$id_hogar)

cluster_se4 <- vcovHC(did_educ_years4, cluster = educ_years$id_hogar)


stargazer(coeftest(did_educ_years1, vcov=cluster_se1),coeftest(did_educ_years3, vcov=cluster_se3),coeftest(did_educ_years2, vcov=cluster_se2), coeftest(did_educ_years4, vcov=cluster_se4), type = "text")

#For this variable we have only significant effect for employment: this could be due to the fact that now there are few observations in the treated 

remove(cluster_se1)
remove(cluster_se2)
remove(cluster_se3)
remove(cluster_se4)


remove(educ_years)
remove(did_educ_years1)
remove(did_educ_years2)
remove(did_educ_years3)
remove(did_educ_years4)

#Assistance rate

treatment_robust$ar <- ifelse(treatment_robust$ar > mean(treatment_robust$ar), 0, 1)

ar <- treatment_robust[, c("province","ar")]

names(ar) <- c("province", "treat_educ")

ar <- merge(individuos, ar, by = "province")

table(ar$policy, ar$treat_educ)


did_ar1 <- lm(university ~ policy*treat_educ +
                        age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                        h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                        as.factor(period) + as.factor(province), data = ar)

did_ar2 <- lm(informal ~ policy*treat_educ +
                        age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                        h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                        as.factor(period) + as.factor(province), data = ar)

did_ar3 <- lm(employment ~ policy*treat_educ +
                        age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                        h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                        as.factor(period) + as.factor(province), data = ar)

did_ar4 <- lm(migrant ~ policy*treat_educ +
                        age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                        h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                        as.factor(period) + as.factor(province),
                      data = ar)


stargazer(coeftest(did_ar1, vcov=vcovHC) ,coeftest(did_ar3, vcov=vcovHC), coeftest(did_ar2, vcov=vcovHC), coeftest(did_ar4, vcov=vcovHC), type = "text")

cluster_se1 <- vcovHC(did_ar1, cluster = ar$id_hogar)

cluster_se2 <- vcovHC(did_ar2, cluster = ar$id_hogar)

cluster_se3 <- vcovHC(did_ar3, cluster = ar$id_hogar)

cluster_se4 <- vcovHC(did_ar4, cluster = ar$id_hogar)


stargazer(coeftest(did_ar1, vcov=cluster_se1),coeftest(did_ar3, vcov=cluster_se3),coeftest(did_ar2, vcov=cluster_se2), coeftest(did_ar4, vcov=cluster_se4), type = "text")

#For this variable we have only significant effect for employment: this could be due to the fact that now there are few observations in the treated 

remove(cluster_se1)
remove(cluster_se2)
remove(cluster_se3)
remove(cluster_se4)


remove(ar)
remove(did_ar1)
remove(did_ar2)
remove(did_ar3)
remove(did_ar4)


#Population per IE 

treatment_robust$pop_per_IE <- ifelse(treatment_robust$pop_per_IE > mean(treatment_robust$pop_per_IE), 0, 1)

pop_per_IE <- treatment_robust[, c("province","pop_per_IE")]

names(pop_per_IE) <- c("province", "treat_educ")

pop_per_IE <- merge(individuos, pop_per_IE, by = "province")

table(pop_per_IE$policy, pop_per_IE$treat_educ)


did_pop_per_IE1 <- lm(university ~ policy*treat_educ +
                age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                as.factor(period) + as.factor(province), data = pop_per_IE)

did_pop_per_IE2 <- lm(informal ~ policy*treat_educ +
                age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                as.factor(period) + as.factor(province), data = pop_per_IE)

did_pop_per_IE3 <- lm(employment ~ policy*treat_educ +
                age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                as.factor(period) + as.factor(province), data = pop_per_IE)

did_pop_per_IE4 <- lm(migrant ~ policy*treat_educ +
                age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                as.factor(period) + as.factor(province),
              data = pop_per_IE)


stargazer(coeftest(did_pop_per_IE1, vcov=vcovHC) ,coeftest(did_pop_per_IE3, vcov=vcovHC), coeftest(did_pop_per_IE2, vcov=vcovHC), coeftest(did_pop_per_IE4, vcov=vcovHC), type = "text")

cluster_se1 <- vcovHC(did_pop_per_IE1, cluster = pop_per_IE$id_hogar)

cluster_se2 <- vcovHC(did_pop_per_IE2, cluster = pop_per_IE$id_hogar)

cluster_se3 <- vcovHC(did_pop_per_IE3, cluster = pop_per_IE$id_hogar)

cluster_se4 <- vcovHC(did_pop_per_IE4, cluster = pop_per_IE$id_hogar)


stargazer(coeftest(did_pop_per_IE1, vcov=cluster_se1),coeftest(did_pop_per_IE3, vcov=cluster_se3),coeftest(did_pop_per_IE2, vcov=cluster_se2), coeftest(did_pop_per_IE4, vcov=cluster_se4), type = "text")

#For this variable we have only significant effect for employment: control provinces are in the treated

remove(cluster_se1)
remove(cluster_se2)
remove(cluster_se3)
remove(cluster_se4)


remove(pop_per_IE)
remove(did_pop_per_IE1)
remove(did_pop_per_IE2)
remove(did_pop_per_IE3)
remove(did_pop_per_IE4)

#Student per teacher

treatment_robust$spt <- ifelse(treatment_robust$spt > mean(treatment_robust$spt), 0, 1)

spt <- treatment_robust[, c("province","spt")]

names(spt) <- c("province", "treat_educ")

spt <- merge(individuos, spt, by = "province")

table(spt$policy, spt$treat_educ)


did_spt1 <- lm(university ~ policy*treat_educ +
                        age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                        h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                        as.factor(period) + as.factor(province), data = spt)

did_spt2 <- lm(informal ~ policy*treat_educ +
                        age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                        h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                        as.factor(period) + as.factor(province), data = spt)

did_spt3 <- lm(employment ~ policy*treat_educ +
                        age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                        h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                        as.factor(period) + as.factor(province), data = spt)

did_spt4 <- lm(migrant ~ policy*treat_educ +
                        age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                        h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                        as.factor(period) + as.factor(province),
                      data = spt)


stargazer(coeftest(did_spt1, vcov=vcovHC) ,coeftest(did_spt3, vcov=vcovHC), coeftest(did_spt2, vcov=vcovHC), coeftest(did_spt4, vcov=vcovHC), type = "text")

cluster_se1 <- vcovHC(did_spt1, cluster = spt$id_hogar)

cluster_se2 <- vcovHC(did_spt2, cluster = spt$id_hogar)

cluster_se3 <- vcovHC(did_spt3, cluster = spt$id_hogar)

cluster_se4 <- vcovHC(did_spt4, cluster = spt$id_hogar)


stargazer(coeftest(did_spt1, vcov=cluster_se1),coeftest(did_spt3, vcov=cluster_se3),coeftest(did_spt2, vcov=cluster_se2), coeftest(did_spt4, vcov=cluster_se4), type = "text")

#Not significant effects : prvinces that were in he control are now treated

remove(cluster_se1)
remove(cluster_se2)
remove(cluster_se3)
remove(cluster_se4)


remove(spt)
remove(did_spt1)
remove(did_spt2)
remove(did_spt3)
remove(did_spt4)
