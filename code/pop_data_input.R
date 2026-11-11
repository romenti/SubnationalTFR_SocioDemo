rm(list=ls())


source("code/upload_packages.R")



#### France ####

wd <- "data/france/pop/"

file <- paste0(wd,"estim-pop-dep-sexe-aq-1975-2026.xlsx")



years <- 2000:2020

age_groups <- c(
  "0-4","5-9","10-14","15-19","20-24","25-29",
  "30-34","35-39","40-44","45-49","50-54",
  "55-59","60-64","65-69","70-74","75-79",
  "80-84","85-89","90-94","95+","Total"
)

read_year <- function(y){
  
  x <- read_excel(
    file,
    sheet = as.character(y),
    skip = 5,
    col_names = FALSE
  )
  
  names(x) <- c(
    "prov","prov_name",
    paste0("Both_", age_groups),
    paste0("Male_", age_groups),
    paste0("Female_", age_groups)
  )
  
  x %>%
    filter(!is.na(prov)) %>%
    mutate(year = y) %>%
    pivot_longer(
      -c(prov, prov_name, year),
      names_to = c("sex","age_group"),
      names_sep = "_",
      values_to = "pop"
    )
}

population_france <- map_dfr(years, read_year)



population_france_final <- population_france[!(population_france$prov %in% c("971",
                                                                      "972",
                                                                      "France métropolitaine",
                                                                      "France métropolitaine et DOM",
                                                                      "973",
                                                                      "974",
                                                                      "976",
                                                                      "DOM",
                                                                      "Source : Insee - Estimations de population")),]


children_france <- population_france_final %>%
  filter(sex=="Both",age_group=="0-4") %>%
  select(prov,prov_name,year,C=pop) %>%
  arrange(prov,year)


women_france <- population_france_final %>%
  filter(sex=="Female",age_group %in% c("15-19","20-24","25-29",
                                         "30-34","35-39","40-44","45-49")) %>%
  select(prov,prov_name,year,age_group,W=pop) %>%
  arrange(prov,year) %>%
  pivot_wider(names_from = "age_group",values_from = "W") %>%
  ungroup()


names(women_france)[4:10] = paste0('W',seq(15,45,5))



men_france <- population_france_final %>%
  filter(sex=="Male",age_group %in% c("15-19","20-24","25-29",
                                        "30-34","35-39","40-44","45-49",
                                      "50-54","55-59")) %>%
  select(prov,prov_name,year,age_group,W=pop) %>%
  arrange(prov,year) %>%
  pivot_wider(names_from = "age_group",values_from = "W") %>%
  ungroup()


names(men_france)[4:12] = paste0('M',seq(15,55,5))

save(children_france,men_france,women_france,file = paste0(wd,"pop_data_input_france.RData"))




#### Spain ####


wd <- "data/spain/pop/"

spain_population_long <- read_csv(paste0(wd,"spain_population_province_age_sex_2000_2020.csv"))




age_levels <- c(
  "0", "1-4", "5-9", "10-14", "15-19", "20-24",
  "25-29", "30-34", "35-39", "40-44", "45-49",
  "50-54", "55-59", "60-64", "65-69", "70-74",
  "75-79", "80-84", "85+"
)

spain_population_long <- spain_population_long %>%
  mutate(
    age = as.integer(age),
    
    age_group = case_when(
      age == 0                         ~ "0",
      between(age, 1, 4)               ~ "1-4",
      age >= 5 & age <= 84             ~ paste0(
        5L * (age %/% 5L),
        "-",
        5L * (age %/% 5L) + 4L
      ),
      age >= 85                        ~ "85+",
      TRUE                             ~ NA_character_
    )
  ) %>%
  filter(!is.na(age_group)) %>%
  group_by(
    province_code,
    province_name,
    year,
    sex,
    age_group
  ) %>%
  summarise(
    population = if (all(is.na(population))) {
      NA_real_
    } else {
      sum(population, na.rm = TRUE)
    },
    .groups = "drop"
  ) %>%
  mutate(
    age_group = factor(
      age_group,
      levels = age_levels,
      ordered = TRUE
    )
  ) %>%
  arrange(
    province_code,
    year,
    sex,
    age_group
  )


children_spain = spain_population_long %>%
  filter(age_group %in% c("0","1-4")) %>%
  ungroup() %>%
  group_by(province_name,province_code,year) %>%
  summarise(C=sum(population)) %>%
  arrange(province_code)


women_spain = spain_population_long %>%
  filter(age_group %in% c("15-19", "20-24",
                          "25-29", "30-34", 
                          "35-39", "40-44", 
                          "45-49"),
         sex=="Female") %>%
  select(-sex) %>%
  pivot_wider(names_from = "age_group",values_from = "population")

names(women_spain)[4:10] = paste0('W',seq(15,45,5))



men_spain = spain_population_long %>%
  filter(age_group %in% c("15-19", "20-24",
                          "25-29", "30-34", 
                          "35-39", "40-44", 
                          "45-49","50-54","55-59"),
         sex=="Male") %>%
  select(-sex) %>%
  pivot_wider(names_from = "age_group",values_from = "population")

names(men_spain)[4:12] = paste0('M',seq(15,55,5))


save(children_spain,women_spain,men_spain,file = paste0(wd,"pop_data_input_spain.RData"))



#### Bangladesh ####

wd <- "data/bangladesh/pop/"


bangladesh_adm2 <- read_excel(
  paste0(wd,"bangladesh.xlsx"),
  sheet = "2000 - 2040",
  skip = 3
) %>%
  # Select second-order administrative units
  filter(ADM_LEVEL == 2) %>%
  
  # Keep identifiers and male/female population columns
  select(
    ADM1_NAME,
    ADM2_NAME,
    GEO_MATCH,
    matches("^[MF](TOTL|[0-9]{4}|80PL)_[0-9]{4}$")
  ) %>%
  
  # Convert population columns to long format
  pivot_longer(
    cols = matches("^[MF](TOTL|[0-9]{4}|80PL)_[0-9]{4}$"),
    names_to = c("sex", "age", "year"),
    names_pattern = "^([MF])(TOTL|[0-9]{4}|80PL)_([0-9]{4})$",
    values_to = "population"
  ) %>%
  
  # Retain age-specific counts rather than totals
  filter(age != "TOTL") %>%
  
  mutate(
    sex = recode(
      sex,
      "M" = "Male",
      "F" = "Female"
    ),
    age = case_when(
      age == "80PL" ~ "80+",
      TRUE ~ paste0(
        as.integer(str_sub(age, 1, 2)),
        "-",
        as.integer(str_sub(age, 3, 4))
      )
    ),
    year = as.integer(year),
    population = as.numeric(population)
  ) %>%
  
  # Restrict the period, if required
  filter(year >= 2000, year <= 2020) %>%
  arrange(ADM1_NAME, ADM2_NAME, year, sex, age)


children_bangladesh = bangladesh_adm2 %>%
  filter(age %in% c("0-4")) %>%
  group_by(ADM1_NAME, ADM2_NAME,year) %>%
  summarise(C=sum(population))

women_bangladesh = bangladesh_adm2 %>%
  mutate(age = as.numeric(sub("^([0-9]+).*", "\\1", age))) %>%
  filter(sex=="Female",
         age %in% seq(15,45,5)) %>%
  select(adm1=ADM1_NAME,
         adm2=ADM2_NAME,
         year,
         age,
         population) %>%
  pivot_wider(names_from = "age",values_from = "population")

names(women_bangladesh)[4:10] = paste0('W',seq(15,45,5))


men_bangladesh = bangladesh_adm2 %>%
  mutate(age = as.numeric(sub("^([0-9]+).*", "\\1", age))) %>%
  filter(sex=="Male",
         age %in% seq(15,70,5)) %>%
  select(adm1=ADM1_NAME,
         adm2=ADM2_NAME,
         year,
         age,
         population) %>%
  pivot_wider(names_from = "age",values_from = "population")

names(men_bangladesh)[4:15] = paste0('M',seq(15,70,5))


save(children_bangladesh,women_bangladesh,men_bangladesh,
     file=paste0(wd,"bangladesh_pop_data.RData"))


#### Pakistan ####

wd <- "data/pakistan/"



pakistan_adm2 <- read_excel(
  paste0(wd,"pop/pakistan.xlsx"),
  sheet = "2000 - 2040",
  skip = 3
) %>%
  # Select second-order administrative units
  filter(ADM_LEVEL == 3) %>%
  
  # Keep identifiers and male/female population columns
  select(
    ADM1_NAME,
    ADM2_NAME,
    ADM3_NAME,
    GEO_MATCH,
    matches("^[MF](TOTL|[0-9]{4}|80PL)_[0-9]{4}$")
  ) %>%
  
  # Convert population columns to long format
  pivot_longer(
    cols = matches("^[MF](TOTL|[0-9]{4}|80PL)_[0-9]{4}$"),
    names_to = c("sex", "age", "year"),
    names_pattern = "^([MF])(TOTL|[0-9]{4}|80PL)_([0-9]{4})$",
    values_to = "population"
  ) %>%
  
  # Retain age-specific counts rather than totals
  filter(age != "TOTL") %>%
  
  mutate(
    sex = recode(
      sex,
      "M" = "Male",
      "F" = "Female"
    ),
    age = case_when(
      age == "80PL" ~ "80+",
      TRUE ~ paste0(
        as.integer(str_sub(age, 1, 2)),
        "-",
        as.integer(str_sub(age, 3, 4))
      )
    ),
    year = as.integer(year),
    population = as.numeric(population)
  ) %>%
  
  # Restrict the period, if required
  filter(year >= 2000, year <= 2020) %>%
  arrange(ADM1_NAME, ADM2_NAME,ADM3_NAME, year, sex, age)

children_pakistan = pakistan_adm2 %>%
  filter(age %in% c("0-4")) %>%
  select(adm1=ADM1_NAME, adm2=ADM2_NAME,adm3=ADM3_NAME,year,population) %>%
  mutate(adm3 = case_when(
    adm3 == "FRONTIER REGION BANNU DISTRICT"                ~ "BANNU DISTRICT",
    adm3 == "FRONTIER REGION DERA ISMAIL KHAN DISTRICT"     ~ "DERA ISMAIL KHAN DISTRICT",
    adm3 == "FRONTIER REGION KOHAT DISTRICT"                ~ "KOHAT DISTRICT",
    adm3 == "FRONTIER REGION LAKKI MARWAT DISTRICT"         ~ "LAKKI MARWAT DISTRICT",
    adm3 == "FRONTIER REGION PESHAWAR DISTRICT"             ~ "PESHAWAR DISTRICT",
    adm3 == "FRONTIER REGION TANK DISTRICT"                 ~ "TANK DISTRICT",
    adm3 %in% c("SKARDU DISTRICT", "RONDU DISTRICT")                                    ~ "BALTISTAN DISTRICT",
    adm3 %in% c("CHITRAL LOWER DISTRICT", "CHITRAL UPPER DISTRICT")                      ~ "CHITRAL DISTRICT",
    adm3 %in% c("KOHISTAN LOWER DISTRICT", "KOHISTAN UPPER DISTRICT", "KOLAI PALAS KOHISTAN DISTRICT") ~ "KOHISTAN DISTRICT",
    TRUE ~ adm3
  )) %>%
  group_by(adm1,adm2,adm3,year) %>%
  summarise(C=sum(population),.groups = "drop")

children_pakistan_sex = pakistan_adm2 %>%
  filter(age %in% c("0-4")) %>%
  select(adm1=ADM1_NAME, adm2=ADM2_NAME,adm3=ADM3_NAME,year,population,sex) %>%
  mutate(adm3 = case_when(
    adm3 == "FRONTIER REGION BANNU DISTRICT"                ~ "BANNU DISTRICT",
    adm3 == "FRONTIER REGION DERA ISMAIL KHAN DISTRICT"     ~ "DERA ISMAIL KHAN DISTRICT",
    adm3 == "FRONTIER REGION KOHAT DISTRICT"                ~ "KOHAT DISTRICT",
    adm3 == "FRONTIER REGION LAKKI MARWAT DISTRICT"         ~ "LAKKI MARWAT DISTRICT",
    adm3 == "FRONTIER REGION PESHAWAR DISTRICT"             ~ "PESHAWAR DISTRICT",
    adm3 == "FRONTIER REGION TANK DISTRICT"                 ~ "TANK DISTRICT",
    adm3 %in% c("SKARDU DISTRICT", "RONDU DISTRICT")                                    ~ "BALTISTAN DISTRICT",
    adm3 %in% c("CHITRAL LOWER DISTRICT", "CHITRAL UPPER DISTRICT")                      ~ "CHITRAL DISTRICT",
    adm3 %in% c("KOHISTAN LOWER DISTRICT", "KOHISTAN UPPER DISTRICT", "KOLAI PALAS KOHISTAN DISTRICT") ~ "KOHISTAN DISTRICT",
    TRUE ~ adm3
  )) %>%
  group_by(adm1,adm2,adm3,year,sex) %>%
  summarise(C=sum(population),.groups = "drop") %>%
  pivot_wider(names_from = "sex",values_from = "C") 


women_pakistan = pakistan_adm2 %>%
  mutate(age = as.numeric(sub("^([0-9]+).*", "\\1", age))) %>%
  filter(sex == "Female",
         age %in% seq(15, 45, 5)) %>%
  select(adm1 = ADM1_NAME,
         adm2 = ADM2_NAME,
         adm3 = ADM3_NAME,
         year,
         age,
         population) %>%
  mutate(adm3 = case_when(
    adm3 == "FRONTIER REGION BANNU DISTRICT"                ~ "BANNU DISTRICT",
    adm3 == "FRONTIER REGION DERA ISMAIL KHAN DISTRICT"     ~ "DERA ISMAIL KHAN DISTRICT",
    adm3 == "FRONTIER REGION KOHAT DISTRICT"                ~ "KOHAT DISTRICT",
    adm3 == "FRONTIER REGION LAKKI MARWAT DISTRICT"         ~ "LAKKI MARWAT DISTRICT",
    adm3 == "FRONTIER REGION PESHAWAR DISTRICT"             ~ "PESHAWAR DISTRICT",
    adm3 == "FRONTIER REGION TANK DISTRICT"                 ~ "TANK DISTRICT",
    adm3 %in% c("SKARDU DISTRICT", "RONDU DISTRICT")                                    ~ "BALTISTAN DISTRICT",
    adm3 %in% c("CHITRAL LOWER DISTRICT", "CHITRAL UPPER DISTRICT")                      ~ "CHITRAL DISTRICT",
    adm3 %in% c("KOHISTAN LOWER DISTRICT", "KOHISTAN UPPER DISTRICT", "KOLAI PALAS KOHISTAN DISTRICT") ~ "KOHISTAN DISTRICT",
    TRUE ~ adm3
  )) %>%
  group_by(adm1,adm2, adm3, year, age) %>%
  summarise(population = sum(population), .groups = "drop") %>%
  pivot_wider(names_from = "age", values_from = "population")
names(women_pakistan)[5:11] = paste0('W',seq(15,45,5))


# recode the FR districts into their parent adm3 names
women_pakistan <- pakistan_adm2 %>%
  mutate(age = as.numeric(sub("^([0-9]+).*", "\\1", age))) %>%
  filter(sex == "Female",
         age %in% seq(15, 45, 5)) %>%
  select(adm1 = ADM1_NAME,
         adm2 = ADM2_NAME,
         adm3 = ADM3_NAME,
         year,
         age,
         population) %>%
  mutate(adm3 = case_when(
    adm3 == "FRONTIER REGION BANNU DISTRICT"            ~ "BANNU DISTRICT",
    adm3 == "FRONTIER REGION DERA ISMAIL KHAN DISTRICT" ~ "DERA ISMAIL KHAN DISTRICT",
    adm3 == "FRONTIER REGION KOHAT DISTRICT"             ~ "KOHAT DISTRICT",
    adm3 == "FRONTIER REGION LAKKI MARWAT DISTRICT"      ~ "LAKKI MARWAT DISTRICT",
    adm3 == "FRONTIER REGION PESHAWAR DISTRICT"          ~ "PESHAWAR DISTRICT",
    adm3 == "FRONTIER REGION TANK DISTRICT"              ~ "TANK DISTRICT",
    TRUE ~ adm3
  ))



# lookup: the adm1/adm2 that each (new) adm3 value is associated with elsewhere
adm_lookup <- women_pakistan %>%
  distinct(adm1, adm2, adm3)

# every adm3 should map to exactly one adm1/adm2 pair -- check before trusting the join
adm_lookup %>% count(adm3) %>% filter(n > 1)

# overwrite adm1/adm2 with the value tied to the (possibly-recoded) adm3
women_pakistan <- women_pakistan %>%
  select(-adm1, -adm2) %>%
  left_join(adm_lookup, by = "adm3") %>%
  relocate(adm1, adm2, adm3) %>%
  group_by(adm1, adm2, adm3, year, age) %>%
  summarise(population = sum(population), .groups = "drop") %>%
  pivot_wider(names_from = "age", values_from = "population")

adm_lookup <- women_pakistan %>%
  distinct(adm1, adm2, adm3)

# every adm3 should map to exactly one adm1/adm2 pair -- check before trusting the join
women_pakistan %>% count(adm3) %>% filter(n > 1)

women_pakistan <- women_pakistan %>%
  select(-adm1, -adm2) %>%
  left_join(adm_lookup, by = "adm3") %>%
  relocate(adm1, adm2, adm3) %>%
  group_by(adm1, adm2, adm3, year, age) %>%
  summarise(population = sum(population), .groups = "drop") %>%
  pivot_wider(names_from = "age", values_from = "population")

names(women_pakistan)[5:11] = paste0('W',seq(15,45,5))

men_pakistan = pakistan_adm2 %>%
  mutate(age = as.numeric(sub("^([0-9]+).*", "\\1", age))) %>%
  filter(sex=="Male",
         age %in% seq(15,70,5)) %>%
  select(adm1=ADM1_NAME,
         adm2=ADM2_NAME,
         adm3=ADM3_NAME,
         year,
         age,
         population) %>%
  mutate(adm3 = case_when(
    adm3 == "FRONTIER REGION BANNU DISTRICT"                ~ "BANNU DISTRICT",
    adm3 == "FRONTIER REGION DERA ISMAIL KHAN DISTRICT"     ~ "DERA ISMAIL KHAN DISTRICT",
    adm3 == "FRONTIER REGION KOHAT DISTRICT"                ~ "KOHAT DISTRICT",
    adm3 == "FRONTIER REGION LAKKI MARWAT DISTRICT"         ~ "LAKKI MARWAT DISTRICT",
    adm3 == "FRONTIER REGION PESHAWAR DISTRICT"             ~ "PESHAWAR DISTRICT",
    adm3 == "FRONTIER REGION TANK DISTRICT"                 ~ "TANK DISTRICT",
    adm3 %in% c("SKARDU DISTRICT", "RONDU DISTRICT")                                    ~ "BALTISTAN DISTRICT",
    adm3 %in% c("CHITRAL LOWER DISTRICT", "CHITRAL UPPER DISTRICT")                      ~ "CHITRAL DISTRICT",
    adm3 %in% c("KOHISTAN LOWER DISTRICT", "KOHISTAN UPPER DISTRICT", "KOLAI PALAS KOHISTAN DISTRICT") ~ "KOHISTAN DISTRICT",
    TRUE ~ adm3
  )) %>%
  group_by(adm1,adm2, adm3, year, age) %>%
  summarise(population = sum(population), .groups = "drop") %>%
  pivot_wider(names_from = "age", values_from = "population")

names(men_pakistan)[5:16] = paste0('M',seq(15,70,5))

save(children_pakistan,women_pakistan,men_pakistan,
     file=paste0(wd,"Pop/pakistan_pop_data.RData"))



# Crosswalk to a common, older district geography

children_pakistan_harmonized <- children_pakistan %>%
  mutate(
    adm2_standard = standardize_pk_adm2(adm3),
    region = case_when(
      adm1 == "BALOCHISTAN" ~ "Balochistan",
      
      adm1 %in% c(
        "KHYBER PAKHTUNKHWA",
        "FEDERALLY ADMINISTERED TRIBAL AREAS",
        "GILGIT-BALTISTAN"
      ) ~ "Khyber Pakhtunkhwa",
      
      adm1 %in% c(
        "PUNJAB",
        "ISLAMABAD",
        "AZAD KASHMIR"
      ) ~ "Punjab",
      
      adm1 == "SINDH" ~ "Sindh",
      
      TRUE ~ NA_character_
    )
  )


women_pakistan_harmonized <- women_pakistan %>%
  mutate(
    adm2_standard = standardize_pk_adm2(adm3),
    region = case_when(
      adm1 == "BALOCHISTAN" ~ "Balochistan",
      
      adm1 %in% c(
        "KHYBER PAKHTUNKHWA",
        "FEDERALLY ADMINISTERED TRIBAL AREAS",
        "GILGIT-BALTISTAN"
      ) ~ "Khyber Pakhtunkhwa",
      
      adm1 %in% c(
        "PUNJAB",
        "ISLAMABAD",
        "AZAD KASHMIR"
      ) ~ "Punjab",
      
      adm1 == "SINDH" ~ "Sindh",
      
      TRUE ~ NA_character_
    )
  )



men_pakistan_harmonized <- men_pakistan %>%
  mutate(
    adm2_standard = standardize_pk_adm2(adm3),
    region = case_when(
      adm1 == "BALOCHISTAN" ~ "Balochistan",
      
      adm1 %in% c(
        "KHYBER PAKHTUNKHWA",
        "FEDERALLY ADMINISTERED TRIBAL AREAS",
        "GILGIT-BALTISTAN"
      ) ~ "Khyber Pakhtunkhwa",
      
      adm1 %in% c(
        "PUNJAB",
        "ISLAMABAD",
        "AZAD KASHMIR"
      ) ~ "Punjab",
      
      adm1 == "SINDH" ~ "Sindh",
      
      TRUE ~ NA_character_
    )
  )


save(men_pakistan_harmonized,
     women_pakistan_harmonized,
     children_pakistan_harmonized,file="data/pakistan/pop/pakistan_pop_data.RData")






