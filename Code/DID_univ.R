############## Treatment group Número de universidades ########################

Enemdu <- read_feather("C:/Users/ignac/OneDrive/Documentos/PHD/Senescyt_Evaluation/Data/Enemdu.feather")

universidades <- read_csv2("Data/universidades.csv")

#We eliminate from the data set 4 universities created after 2010

#UNIVERSIDAD NACIONAL DE EDUCACIÓN
#UNIVERSIDAD REGIONAL AMAZÓNICA IKIAM
#UNIVERSIDAD DE LAS ARTES (UARTES)
#UNIVERSIAD YACHAY TECH

universidades <- universidades[universidades$`NOMBRE IES` != "UNIVERSIDAD NACIONAL DE EDUCACIÓN",]
universidades <- universidades[universidades$`NOMBRE IES` != "UNIVERSIDAD REGIONAL AMAZÓNICA IKIAM",]
universidades <- universidades[universidades$`NOMBRE IES` != "UNIVERSIDAD DE LAS ARTES (UARTES)",]
universidades <- universidades[universidades$`NOMBRE IES` != "UNIVERSIAD YACHAY TECH",]

table(universidades$`ESTRUCTURA INSTITUCIONAL`, universidades$`TIPO FINANCIAMIENTO`)

universidades$publica <- rep(0, length(universidades$`CÓDIGO IES`))
universidades$publica[universidades$`TIPO FINANCIAMIENTO` == "PÚBLICA" & universidades$`ESTRUCTURA INSTITUCIONAL` == "SEDE"
                      | universidades$`TIPO FINANCIAMIENTO` == "PÚBLICA" & universidades$`ESTRUCTURA INSTITUCIONAL` == "SEDE MATRIZ" ]=1

prov_univ <- universidades %>%
  group_by(PROVINCIA) %>%
  summarise_at(vars(publica),
               sum) %>%
  ungroup()


ciudad_univ <- universidades %>%
  group_by(CANTÓN) %>%
  summarise_at(vars(publica),
               sum) %>%
  ungroup()

table(Enemdu$indigenous, Enemdu$province)



#We leave NA because in the whole database it represents universidad particular de loja that is a private university

prov_univ <- na.omit(prov_univ)

names(prov_univ) <- c("province", "publica")

prov_univ <- prov_univ[, province = stri_trans_general(str = province, 
                                                       id = "Latin-ASCII")]

prov_univ$province <- gsub("í", "I", prov_univ$province, ignore.case = TRUE)

prov_univ$province <- gsub("á", "A", prov_univ$province, ignore.case = TRUE)

write_feather(prov_univ,"prov_univ.feather")

Enemdu <- merge(Enemdu, prov_univ, by = "province")

remove(prov_univ)

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

table(individuos3$treat, individuos3$policy)


remove(Enemdu, universidades)




# DID

univ_reg1 <- lm(university ~ policy*treat + 
                  age + sex + indigenous + afro + rural + formal_per_house + h_tot_labour_income_pc + 
                  h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                  as.factor(period) + as.factor(province),
                data = individuos3)


stargazer(univ_reg1 , type = "text")


univ_reg2 <- lm(informal ~ policy*treat
                + age + sex + indigenous + afro + rural + formal_per_house + h_tot_labour_income_pc + 
                  h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                  as.factor(period) + as.factor(province), data = individuos3)

stargazer(univ_reg2, type = "text")


univ_reg3 <- lm(employment ~ policy*treat +
                  age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                  h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                  as.factor(period) + as.factor(province), data = individuos3)

univ_reg4 <- lm(migrant ~ policy*treat +
                  age + sex + indigenous + afro + rural  + formal_per_house + h_tot_labour_income_pc +
                  h_little_kids + h_kid + h_teen + h_adult + nhousehold.y +
                  as.factor(period) + as.factor(province), data = individuos3)

stargazer(fam_univ_reg3, type = "text")

stargazer(coeftest(univ_reg1, vcov=vcovHC),coeftest(univ_reg2, vcov=vcovHC),coeftest(univ_reg3, vcov=vcovHC), coeftest(univ_reg4, vcov=vcovHC), type = "text")

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

