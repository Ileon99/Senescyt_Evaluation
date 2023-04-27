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

