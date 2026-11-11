#### Upload all the packages ####

source("code/upload_packages.R")

#### Upload all the functions ####

source("code/helpers.R")

#### France ####

##### Female #####

france_asfr_fem = read.csv("data/france/fertility/france_female_asfr_hfd.txt", sep="")
phi_matrix = france_asfr_fem %>%
  dplyr::select(year=Year,Age,Fx=ASFR) %>%
  mutate(age=as.numeric(Age)) %>%
  filter(!(is.na(age)),age %in% seq(15,49,1),year %in% seq(2000,2020,1)) %>%
  dplyr::select(-Age) %>%
  mutate(age = cut(
    age,
    breaks = seq(15, 50, by = 5),
    right = FALSE,
    labels = c(
      "15-19", "20-24", "25-29", "30-34",
      "35-39", "40-44", "45-49"
    )
  )
  ) %>%
  group_by(year,age) %>%
  summarise(Fx=mean(Fx)) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(TFR=5*sum(Fx),
         phi=5*Fx/TFR) %>%
  dplyr::select(-TFR,-Fx) %>%
  ungroup() %>%
  pivot_wider(names_from="year",values_from="phi") %>%
  dplyr::select(-age) %>%
  as.matrix()



matrix_prop = matrix(NA,nrow(phi_matrix),ncol(phi_matrix))
for(i in 1:nrow(phi_matrix)){
  for(j in 1:ncol(phi_matrix)){
    matrix_prop[i,j] = log(phi_matrix[i,j]/phi_matrix[1,j])
  }
}

mean_columns = apply(matrix_prop, 1, mean)
matrix_prop_demeaned = matrix_prop - mean_columns
svd_results = svd(matrix_prop_demeaned)
X_france_female = matrix(NA,2,7)
X_france_female[1,] = round(svd_results$u[,1],6)
X_france_female[2,] = round(svd_results$u[,2],6)
m_france_female = rowMeans(matrix_prop)

nat_tfr_france_female = read.csv("data/france/fertility/france_female_tfr_hfd.txt", sep="")

nat_tfr_france_female = nat_tfr_france_female %>%
  select(year=Year,tfr=TFR) %>%
  filter(year %in% seq(2000,2020,1))



##### Male #####

france_asfr_male = read.csv("data/france/fertility/france_male_asfr_hfc.txt")


phi_matrix = france_asfr_male %>%
  select(year=Year1,age=Age,Fx=ASFR) %>%
  filter(!(is.na(age)),age %in% seq(15,59,1),year %in% seq(2000,2020,1)) %>%
  mutate(age = cut(
    age,
    breaks = seq(15, 60, by = 5),
    right = FALSE,
    labels = c(
      "15-19", "20-24", "25-29", "30-34",
      "35-39", "40-44", "45-49","50-54","55-59"
    )
  )
  ) %>%
  group_by(year,age) %>%
  summarise(Fx=mean(Fx)) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(TFR=5*sum(Fx),
         phi=5*Fx/TFR) %>%
  dplyr::select(-TFR,-Fx) %>%
  ungroup() %>%
  pivot_wider(names_from="year",values_from="phi") %>%
  dplyr::select(-age) %>%
  as.matrix()

matrix_prop = matrix(NA,nrow(phi_matrix),ncol(phi_matrix))
for(i in 1:nrow(phi_matrix)){
  for(j in 1:ncol(phi_matrix)){
    matrix_prop[i,j] = log(phi_matrix[i,j]/phi_matrix[1,j])
  }
}

mean_columns = apply(matrix_prop, 1, mean)
matrix_prop_demeaned = matrix_prop - mean_columns
svd_results = svd(matrix_prop_demeaned)
X_france_male = matrix(NA,2,9)
X_france_male[1,] = round(svd_results$u[,1],6)
X_france_male[2,] = round(svd_results$u[,2],6)
m_france_male = rowMeans(matrix_prop)

nat_tfr_france_male = read.csv("data/france/fertility/france_tfr_male_hfc.txt")

nat_tfr_france_male = nat_tfr_france_male %>%
  select(year=Year1,tfr=TFR) %>%
  filter(year %in% seq(2000,2013,1))


reg_pred_data = nat_tfr_france_male %>%
  rename(tfr_m=tfr) %>%
  left_join(nat_tfr_france_female)

model_pred = lm(tfr_m~tfr+I(year-2000),data=reg_pred_data)

new_data_pred = nat_tfr_france_female[nat_tfr_france_female$year>2013,]
tfr_m_new = predict(model_pred,newdata = new_data_pred)


nat_tfr_france_male_final = nat_tfr_france_male %>%
  add_row(year=2014:2020,tfr=tfr_m_new)

save(nat_tfr_france_male_final,
     nat_tfr_france_female,
     m_france_female,
     m_france_male,
     X_france_female,
     X_france_male,
     file=paste0(wd,"Data/France/Fertility/france_fert_input.RData"))




#### Spain ####

##### Female #####

spain_asfr_fem = read.csv("data/spain/fertility/spain_asfr_female_hfd.txt", sep="")
phi_matrix = spain_asfr_fem %>%
  dplyr::select(year=Year,Age,Fx=ASFR) %>%
  mutate(age=as.numeric(Age)) %>%
  filter(!(is.na(age)),age %in% seq(15,49,1),year %in% seq(2000,2020,1)) %>%
  dplyr::select(-Age) %>%
  mutate(age = cut(
    age,
    breaks = seq(15, 50, by = 5),
    right = FALSE,
    labels = c(
      "15-19", "20-24", "25-29", "30-34",
      "35-39", "40-44", "45-49"
    )
  )
  ) %>%
  group_by(year,age) %>%
  summarise(Fx=mean(Fx)) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(TFR=5*sum(Fx),
         phi=5*Fx/TFR) %>%
  dplyr::select(-TFR,-Fx) %>%
  ungroup() %>%
  pivot_wider(names_from="year",values_from="phi") %>%
  dplyr::select(-age) %>%
  as.matrix()



matrix_prop = matrix(NA,nrow(phi_matrix),ncol(phi_matrix))
for(i in 1:nrow(phi_matrix)){
  for(j in 1:ncol(phi_matrix)){
    matrix_prop[i,j] = log(phi_matrix[i,j]/phi_matrix[1,j])
  }
}

mean_columns = apply(matrix_prop, 1, mean)
matrix_prop_demeaned = matrix_prop - mean_columns
svd_results = svd(matrix_prop_demeaned)
X_spain_female = matrix(NA,2,7)
X_spain_female[1,] = round(svd_results$u[,1],6)
X_spain_female[2,] = round(svd_results$u[,2],6)
m_spain_female = rowMeans(matrix_prop)

nat_tfr_spain_female = read.csv("data/spain/fertility/spain_tfr_female_hfd.txt", sep="")

nat_tfr_spain_female = nat_tfr_spain_female %>%
  select(year=Year,tfr=TFR) %>%
  filter(year %in% seq(2000,2020,1))



##### Male #####

spain_asfr_male = read.csv("data/spain/fertility/spain_asfr_male_hfc.txt")


phi_matrix = spain_asfr_male %>%
  select(year=Year1,age=Age,Fx=ASFR) %>%
  filter(!(is.na(age)),age %in% seq(15,59,1),year %in% seq(2000,2020,1)) %>%
  mutate(age = cut(
    age,
    breaks = seq(15, 60, by = 5),
    right = FALSE,
    labels = c(
      "15-19", "20-24", "25-29", "30-34",
      "35-39", "40-44", "45-49","50-54","55-59"
    )
  )
  ) %>%
  group_by(year,age) %>%
  summarise(Fx=mean(Fx)) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(TFR=5*sum(Fx),
         phi=5*Fx/TFR) %>%
  dplyr::select(-TFR,-Fx) %>%
  ungroup() %>%
  pivot_wider(names_from="year",values_from="phi") %>%
  dplyr::select(-age) %>%
  as.matrix()

matrix_prop = matrix(NA,nrow(phi_matrix),ncol(phi_matrix))
for(i in 1:nrow(phi_matrix)){
  for(j in 1:ncol(phi_matrix)){
    matrix_prop[i,j] = log(phi_matrix[i,j]/phi_matrix[1,j])
  }
}

mean_columns = apply(matrix_prop, 1, mean)
matrix_prop_demeaned = matrix_prop - mean_columns
svd_results = svd(matrix_prop_demeaned)
X_spain_male = matrix(NA,2,9)
X_spain_male[1,] = round(svd_results$u[,1],6)
X_spain_male[2,] = round(svd_results$u[,2],6)
m_spain_male = rowMeans(matrix_prop)

nat_tfr_spain_male = read.csv("data/spain/fertility/spain_tfr_male_hfc.txt")

nat_tfr_spain_male = nat_tfr_spain_male %>%
  select(year=Year1,tfr=TFR) %>%
  filter(year %in% seq(2000,2014,1))


reg_pred_data = nat_tfr_spain_male %>%
  rename(tfr_m=tfr) %>%
  left_join(nat_tfr_spain_female)

model_pred = lm(tfr_m~tfr+I(year-2000),data=reg_pred_data)

new_data_pred = nat_tfr_spain_female[nat_tfr_spain_female$year>2014,]
tfr_m_new = predict(model_pred,newdata = new_data_pred)


nat_tfr_spain_male_final = nat_tfr_spain_male %>%
  add_row(year=2015:2020,tfr=tfr_m_new)

save(nat_tfr_spain_male_final,
     nat_tfr_spain_female,
     m_spain_female,
     m_spain_male,
     X_spain_female,
     X_spain_male,
     file=paste0(wd,"Data/Spain/Fertility/spain_fert_input.RData"))



#### Bangladesh ####

##### Female #####

load("data/bangladesh/fertility/wpp_2024.RData")

phi_matrix = WPP2024_Fertility_by_Age5 %>%
  dplyr::select(year=Time,region=Location,age=AgeGrpStart,Fx=ASFR) %>%
  mutate(Fx=Fx/1e3) %>%
  filter(region == "Bangladesh",age %in% seq(15,45,5),year>=2000,year<=2020) %>%
  #group_by(year,age) %>%
  #summarise(Fx=mean(Fx)) %>%
  group_by(year,region) %>%
  mutate(TFR=5*sum(Fx),
         phi=5*Fx/TFR) %>%
  dplyr::select(-TFR,-Fx) %>%
  mutate(comb=paste0(region,year)) %>%
  ungroup() %>%
  dplyr::select(-year,-region) %>%
  pivot_wider(names_from="comb",values_from="phi") %>%
  dplyr::select(-age) %>%
  as.matrix()


matrix_prop = matrix(NA,nrow(phi_matrix),ncol(phi_matrix))
for(i in 1:nrow(phi_matrix)){
  for(j in 1:ncol(phi_matrix)){
    matrix_prop[i,j] = log(phi_matrix[i,j]/phi_matrix[1,j])
  }
}

mean_columns = apply(matrix_prop, 1, mean)
matrix_prop_demeaned = matrix_prop - mean_columns
svd_results = svd(matrix_prop_demeaned)
X_bangladesh_female = matrix(NA,2,7)
X_bangladesh_female[1,] = round(svd_results$u[,1],6)
X_bangladesh_female[2,] = round(svd_results$u[,2],6)
m_bangladesh_female = rowMeans(matrix_prop)



tfr_bangladesh_female = WPP2024_Fertility_by_Age5 %>%
  dplyr::select(year=Time,region=Location,age=AgeGrpStart,Fx=ASFR) %>%
  mutate(Fx=Fx/1e3) %>%
  filter(region == "Bangladesh",age %in% seq(15,45,5),year>=2000,year<=2020) %>%
  #group_by(year,age) %>%
  #summarise(Fx=mean(Fx)) %>%
  group_by(year) %>%
  summarise(tfr=5*sum(Fx)) 


tfr_bangladesh_female = WPP2024_Fertility_by_Age5 %>%
  dplyr::select(year=Time,region=Location,age=AgeGrpStart,Fx=ASFR) %>%
  mutate(Fx=Fx/1e3) %>%
  filter(region == "Bangladesh",age %in% seq(15,45,5),year>=2000,year<=2024) %>%
  #group_by(year,age) %>%
  #summarise(Fx=mean(Fx)) %>%
  group_by(year) %>%
  summarise(tfr=5*sum(Fx)) 

##### Male #####

bangladesh_asfr_male_hfd <- read_excel("data/bangladesh/fertility/bangladesh_asfr_male_hfd.xlsx")



phi_matrix = bangladesh_asfr_male_hfd %>%
  dplyr::select(year=Period,age=`Age group`,Fx=Value) %>%
  mutate(Fx=Fx/1e3) %>%
  filter(age!="75-79") %>%
  #group_by(year,age) %>%
  #summarise(Fx=mean(Fx)) %>%
  group_by(year) %>%
  mutate(TFR=5*sum(Fx),
         phi=5*Fx/TFR) %>%
  dplyr::select(-TFR,-Fx) %>%
  ungroup() %>%
  pivot_wider(names_from="year",values_from="phi") %>%
  dplyr::select(-age) %>%
  as.matrix()

phi_matrix[phi_matrix==0] = 0.0001



matrix_prop = matrix(NA,nrow(phi_matrix),ncol(phi_matrix))
for(i in 1:nrow(phi_matrix)){
  for(j in 1:ncol(phi_matrix)){
    matrix_prop[i,j] = log(phi_matrix[i,j]/phi_matrix[1,j])
  }
}

mean_columns = apply(matrix_prop, 1, mean)
matrix_prop_demeaned = matrix_prop - mean_columns
svd_results = svd(matrix_prop_demeaned)
X_bangladesh_male = matrix(NA,2,12)
X_bangladesh_male[1,] = round(svd_results$u[,1],6)
X_bangladesh_male[2,] = round(svd_results$u[,2],6)
m_bangladesh_male = rowMeans(matrix_prop)


tfr_bangladesh_male <- read_excel("data/bangladesh/fertility/bangladesh_tfr_male_mfd.xlsx") 
bangladesh_male_single_tfr <- tfr_bangladesh_male %>%
  mutate(
    year_start = as.numeric(substr(Period, 1, 4)),
    year_end   = as.numeric(substr(Period, 6, 9))-1
  ) %>%
  rowwise() %>%
  mutate(year = list(seq(year_start, year_end))) %>%
  tidyr::unnest(year) %>%
  ungroup() %>%
  select(
    year,
    tfr=Value
  ) %>%
  add_row(year=2020,tfr=2.3)


save(bangladesh_male_single_tfr,
     tfr_bangladesh_female,
     m_bangladesh_female,
     m_bangladesh_male,
     X_bangladesh_female,
     X_bangladesh_male,
     file="data/bangladesh/fertility/bangladesh_fert_input.RData")





#### Pakistan ####

##### Female #####

load("data/pakistan/fertility/wpp_2024.RData")

phi_matrix = WPP2024_Fertility_by_Age5 %>%
  dplyr::select(year=Time,region=Location,age=AgeGrpStart,Fx=ASFR) %>%
  mutate(Fx=Fx/1e3) %>%
  filter(region == "Pakistan",age %in% seq(15,45,5),year>=2000,year<=2020) %>%
  #group_by(year,age) %>%
  #summarise(Fx=mean(Fx)) %>%
  group_by(year,region) %>%
  mutate(TFR=5*sum(Fx),
         phi=5*Fx/TFR) %>%
  dplyr::select(-TFR,-Fx) %>%
  mutate(comb=paste0(region,year)) %>%
  ungroup() %>%
  dplyr::select(-year,-region) %>%
  pivot_wider(names_from="comb",values_from="phi") %>%
  dplyr::select(-age) %>%
  as.matrix()



matrix_prop = matrix(NA,nrow(phi_matrix),ncol(phi_matrix))
for(i in 1:nrow(phi_matrix)){
  for(j in 1:ncol(phi_matrix)){
    matrix_prop[i,j] = log(phi_matrix[i,j]/phi_matrix[1,j])
  }
}

mean_columns = apply(matrix_prop, 1, mean)
matrix_prop_demeaned = matrix_prop - mean_columns
svd_results = svd(matrix_prop_demeaned)
X_pakistan_female = matrix(NA,2,7)
X_pakistan_female[1,] = round(svd_results$u[,1],6)
X_pakistan_female[2,] = round(svd_results$u[,2],6)
m_pakistan_female = rowMeans(matrix_prop)



tfr_pakistan_female = WPP2024_Fertility_by_Age5 %>%
  dplyr::select(year=Time,region=Location,age=AgeGrpStart,Fx=ASFR) %>%
  mutate(Fx=Fx/1e3) %>%
  filter(region == "Pakistan",age %in% seq(15,45,5),year>=2000,year<=2020) %>%
  #group_by(year,age) %>%
  #summarise(Fx=mean(Fx)) %>%
  group_by(year) %>%
  summarise(tfr=5*sum(Fx)) 

y = WPP2024_Fertility_by_Age5 %>%
  dplyr::select(year=Time,region=Location,age=AgeGrpStart,Fx=ASFR) %>%
  mutate(Fx=Fx/1e3) %>%
  filter(region == "Pakistan",age %in% seq(15,45,5),year>=1990,year<=2024) %>%
  #group_by(year,age) %>%
  #summarise(Fx=mean(Fx)) %>%
  group_by(year) %>%
  summarise(tfr=5*sum(Fx))

##### Male #####

pakistan_asfr_male_hfd <- read_excel("data/pakistan/fertility/pakistan_asfr_male_mfd.xlsx")



phi_matrix = pakistan_asfr_male_hfd %>%
  dplyr::select(year=Period,age=`Age group`,Fx=Value) %>%
  mutate(Fx=Fx/1e3) %>%
  filter(age!="75-79") %>%
  #group_by(year,age) %>%
  #summarise(Fx=mean(Fx)) %>%
  group_by(year) %>%
  mutate(TFR=5*sum(Fx),
         phi=5*Fx/TFR) %>%
  dplyr::select(-TFR,-Fx) %>%
  ungroup() %>%
  pivot_wider(names_from="year",values_from="phi") %>%
  dplyr::select(-age) %>%
  as.matrix()

phi_matrix[phi_matrix==0] = 0.0001



matrix_prop = matrix(NA,nrow(phi_matrix),ncol(phi_matrix))
for(i in 1:nrow(phi_matrix)){
  for(j in 1:ncol(phi_matrix)){
    matrix_prop[i,j] = log(phi_matrix[i,j]/phi_matrix[1,j])
  }
}

mean_columns = apply(matrix_prop, 1, mean)
matrix_prop_demeaned = matrix_prop - mean_columns
svd_results = svd(matrix_prop_demeaned)
X_pakistan_male = matrix(NA,2,12)
X_pakistan_male[1,] = round(svd_results$u[,1],6)
X_pakistan_male[2,] = round(svd_results$u[,2],6)
m_pakistan_male = rowMeans(matrix_prop)


tfr_pakistan_male <- read_excel("data/pakistan/fertility/pakistan_tfr_male_mfd.xlsx") 
pakistan_male_single_tfr <- tfr_pakistan_male %>%
  mutate(
    year_start = as.numeric(substr(Period, 1, 4)),
    year_end   = as.numeric(substr(Period, 6, 9))-1
  ) %>%
  rowwise() %>%
  mutate(year = list(seq(year_start, year_end))) %>%
  tidyr::unnest(year) %>%
  ungroup() %>%
  select(
    year,
    tfr=Value
  ) %>%
  add_row(year=2020,tfr=4)


save(pakistan_male_single_tfr,
     tfr_pakistan_female,
     m_pakistan_female,
     m_pakistan_male,
     X_pakistan_female,
     X_pakistan_male,
     file="data/pakistan/fertility/pakistan_fert_input.RData"))


