



#### Mortality Functions ####


derive_lx_values <- function(nmx, ages){
  a0 <- 0.07 + 1.7*nmx[1] 
  a1 <- 1.5
  a_inf <- 1/nmx[length(nmx)]
  nax <- c(a0, a1, ((ages - lag(ages))/2)[4:(length(ages))], a_inf) 
  ns <- c(1,4, (ages - lag(ages))[4:(length(ages))], Inf) #interval length
  nqx <- ns*nmx / (1 + (ns - nax)*nmx)
  nqx[length(nqx)] <- 1 #last age prob dying =1
  l0 <- 1
  lx <- c(l0, rep(0, length(nqx)-1))
  for (i in 2:length(nqx)){
    lx[i]<- lx[i-1]*(1-nqx[i-1])
  }
  
  return(lx)
}

ages = unique(age_crosswalk$age)

derive_lx_values <- function(nmx, ages){
  nmx = mx_draws[1,]
  a0 <- 0.07 + 1.7*nmx[1] 
  a1 <- 1.5
  a_inf <- 1/nmx[length(nmx)]
  nax <- c(a0, a1, ((ages - lag(ages))/2)[4:(length(ages))], a_inf) 
  ns <- c(1,4, (ages - lag(ages))[4:(length(ages))], Inf) #interval length
  nqx <- ns*nmx / (1 + (ns - nax)*nmx)
  nqx[length(nqx)] <- 1 #last age prob dying =1
  l0 <- 1
  lx <- c(l0, rep(0, length(nqx)-1))
  for (i in 2:length(nqx)){
    lx[i]<- lx[i-1]*(1-nqx[i-1])
  }
  
  return(lx)
}



derive_nLx_values <- function (nmx, ages){
  
  lx <- derive_lx_values(nmx, ages)
  a0 <- 0.07 + 1.7*nmx[1] 
  a1 <- 1.5
  a_inf <- 1/nmx[length(nmx)]
  nax <- c(a0, a1, ((ages - lag(ages))/2)[4:(length(ages))], a_inf) 
  ns <- c(1,4, (ages - lag(ages))[4:(length(ages))], Inf) #interval length
  
  # 
  ndx <- rep(NA, length(lx))
  for(i in 1:(length(ndx)-1)){
    ndx[i] <- lx[i] - lx[i+1]
  }
  ndx[length(ndx)] <- lx[length(ndx)]
  
  nLx <- rep(NA, length(nmx))
  for(i in 1:(length(nLx)-1)){
    nLx[i] <- ns[i]*lx[i+1] + nax[i]*ndx[i]
  }
  
  nLx[length(nLx)] <-  nax[length(nax)]*ndx[length(ndx)]
  
  return(nLx)
}




extract_spain_population <- function(
    input_file,
    sheet = "tabla-0",
    years_keep = 2000:2020,
    include_both_sexes = FALSE
) {
  
  # The workbook has sex labels in row 7 and years in row 8.
  header <- read_excel(
    path = input_file,
    sheet = sheet,
    range = "A7:BX8",
    col_names = FALSE
  )
  
  sex_header <- as.character(unlist(header[1, -1], use.names = FALSE))
  year_header <- as.character(unlist(header[2, -1], use.names = FALSE))
  
  # Merged Excel cells leave sex blank after the first column of each block.
  # Fill the sex labels forward across the corresponding year columns.
  sex_header <- na_if(sex_header, "")
  sex_header <- tibble(sex = sex_header) %>%
    fill(sex, .direction = "down") %>%
    pull(sex)
  
  if (anyNA(sex_header) || anyNA(year_header)) {
    stop("Could not reconstruct the sex/year column headers.")
  }
  
  value_names <- paste(sex_header, year_header, sep = "__")
  
  # Read the data below the two header rows.
  # trim_ws = FALSE is essential because leading spaces encode the hierarchy:
  # 0 spaces = province, 4 spaces = age, 8 spaces = nationality category.
  raw <- read_excel(
    path = input_file,
    sheet = sheet,
    skip = 8,
    col_names = FALSE,
    col_types = c("text", rep("numeric", length(value_names))),
    trim_ws = FALSE
  )
  
  if (ncol(raw) != length(value_names) + 1L) {
    stop(
      "Unexpected number of columns: found ", ncol(raw),
      ", expected ", length(value_names) + 1L, "."
    )
  }
  
  names(raw) <- c("label", value_names)
  
  keep_sexes <- if (include_both_sexes) {
    c("Both", "Male", "Female")
  } else {
    c("Male", "Female")
  }
  
  population_long <- raw %>%
    mutate(
      label = coalesce(label, ""),
      indentation = str_length(str_extract(label, "^\\s*")),
      label_clean = str_trim(label),
      
      # Province rows have no indentation and begin with a two-digit code.
      province_raw = if_else(
        indentation == 0L & str_detect(label_clean, "^[0-9]{2}\\s+"),
        label_clean,
        NA_character_
      ),
      
      # Age rows have four leading spaces.
      age_raw = if_else(
        indentation == 4L,
        label_clean,
        NA_character_
      ),
      
      # Nationality rows have eight leading spaces.
      nationality = if_else(
        indentation == 8L,
        label_clean,
        NA_character_
      )
    ) %>%
    fill(province_raw, age_raw, .direction = "down") %>%
    
    # Keep the total population across nationality categories, exclude
    # the national total and the all-ages rows.
    filter(
      !is.na(province_raw),
      nationality == "TOTAL",
      age_raw != "TOTAL AGES"
    ) %>%
    select(province_raw, age_raw, all_of(value_names)) %>%
    pivot_longer(
      cols = all_of(value_names),
      names_to = c("sex", "year"),
      names_sep = "__",
      values_to = "population"
    ) %>%
    mutate(
      province_code = str_extract(province_raw, "^[0-9]{2}"),
      province_name = str_remove(province_raw, "^[0-9]{2}\\s+"),
      year = as.integer(year),
      sex = recode(
        sex,
        "Both sexes" = "Both",
        "Males" = "Male",
        "Females" = "Female"
      ),
      # 0 through 99 are single ages; 100 represents the open group 100+.
      age = as.integer(str_extract(age_raw, "[0-9]+")),
      age_label = if_else(age == 100L, "100+", as.character(age))
    ) %>%
    filter(
      year %in% years_keep,
      sex %in% keep_sexes
    ) %>%
    select(
      province_code,
      province_name,
      year,
      sex,
      age,
      age_label,
      population
    ) %>%
    arrange(province_code, year, sex, age)
  
  # Basic integrity checks.
  duplicate_rows <- population_long %>%
    count(province_code, year, sex, age, name = "n") %>%
    filter(n > 1L)
  
  if (nrow(duplicate_rows) > 0L) {
    stop("The extracted data contain duplicate province-year-sex-age rows.")
  }
  
  if (n_distinct(population_long$province_code) != 52L) {
    warning(
      "Expected 52 province-level units (including Ceuta and Melilla), but found ",
      n_distinct(population_long$province_code), "."
    )
  }
  
  if (anyNA(population_long$population)) {
    warning("Some population counts are missing.")
  }
  
  population_long
}

# -----------------------------------------------------------------------------
# Run the extraction
# -----------------------------------------------------------------------------

input_file <- paste0(wd,"Pop/03003.xlsx")  # Change this path if necessary.
output_file <- paste0(wd,"spain_population_province_age_sex_2000_2020.csv")

spain_population_long <- extract_spain_population(
  input_file = input_file,
  years_keep = 2000:2020,
  include_both_sexes = FALSE
)

# Save a comma-separated file without row names.
write_csv(spain_population_long, output_file)

# Useful checks
print(dplyr::glimpse(spain_population_long))
print(range(spain_population_long$year))
print(sort(unique(spain_population_long$age)))
print(
  spain_population_long %>%
    distinct(province_code, province_name) %>%
    arrange(province_code)
)





# Compute R^2 functions
compute_r2 <- function(predicted, observed) {
  ss_res <- sum((observed - predicted)^2)
  ss_tot <- sum((observed - mean(observed))^2)
  r2 <- 1 - ss_res / ss_tot
  return(r2)
}

compute_r <- function(predicted, observed) {
  r <- cor(predicted,observed)
  return(r)
}


## compute rmse
compute_rmse <- function(predicted, observed) {
  mse <- sum((observed - predicted)^2) / length(observed)
  rmse <- sqrt(mse)
  return(rmse)
}

## compute mape
compute_mape <- function(predicted, observed) {
  mape <- 100 * mean(abs((observed - predicted) / observed))
  return(mape)
}


## compute mare
compute_mare <- function(predicted, observed) {
  mare <- mean(abs((observed - predicted) / observed))
  return(mare)
}

## compute mae
compute_mae <- function(predicted, observed) {
  mae <- mean((observed - predicted))
  return(mae)
}


#### Run chains using MCMC ####

run_MCMC_allcode <- function(data, code, consts,init,init_name,niter=niter,nburnin=250000) {
  library(nimble)
  
  dynModel = nimbleModel(code = code,
                         data = data, 
                         constants = consts, 
                         inits = init)
  
  
  cat("============\n[compiling]\n============\n")
  CdynModel = compileNimble(dynModel,  showCompilerOutput = TRUE)
  cat("============\n[building]\n============\n")
  dynMCMC = buildMCMC(dynModel,  monitors = init_name, print=T)
  cat("=========\n[merging]\n============\n")
  CdynMCMC = compileNimble(dynMCMC, project = dynModel, showCompilerOutput = T)
  
  
  samples = runMCMC(CdynMCMC, 
                    niter = niter,
                    nburnin = nburnin,
                    thin = 50)
  
  
  
  return(samples)
}

##### US Male Data for Model Input ###

data_input_men_us = function(data,
                             statefip,
                             usa_geo,
                             m,
                             X,
                             m_new,
                             X_new,
                             years=1982:2024){
  
  adj_matrix = adj_matrix_constr(usa_geo,fips=statefip) 
  W = as.matrix(adj_matrix)
  W.scale = -W
  diag(W.scale) = abs(apply(W.scale, 1, sum))
  Q = INLA::inla.scale.model(W.scale, constr=list(A=matrix(1, nrow=1, ncol=nrow(W.scale)), e=0))
  scale = exp((1/nrow(W.scale))*sum(log(1/diag(Q))))
  data = filter(data,year %in% years,state %in% statefip) 
  spatial_object = as.carAdjacency(adj_matrix)
  N = nrow(data)
  Ti = length(unique(data$year))
  S = length(unique(data$state))
  n = length(unique(data$fips))
  
  Q_time <- build_RW1(Ti)      
  Q_ST <- kronecker(Q_time, W)
  
  nb_ST   <- mat2listw(Q_ST != 0, style = "B")$neighbours
  # weights = actual values from Q_ST (off-diagonal, negated)
  num_ST  <- sapply(nb_ST, length)
  adj_ST  <- unlist(nb_ST)
  weights_ST <- sapply(seq_along(nb_ST), function(i){
    j_list <- nb_ST[[i]]
    as.numeric(-Q_ST[i, j_list])   # off-diagonal entries (positive)
  })
  weights_ST <- unlist(weights_ST)
  L_ST <- length(adj_ST)
  
  state_i = data %>%
    ungroup() %>%
    dplyr::select(state) %>%
    pull()
  
  year_i = data %>%
    ungroup() %>%
    dplyr::select(year) %>%
    pull()
  
  county_i = data %>%
    ungroup() %>%
    dplyr::select(fips) %>%
    pull()
  
  
  state_i = as.integer(factor(state_i,levels=unique(data$state)))
  year_i = as.integer(factor(year_i,levels=unique(data$year)))
  county_i = as.integer(factor(county_i,levels=unique(data$fips)))
  
  
  years = unique(data$year)
  time = (years-mean(years))/sd(years)
  
  
  Wt = matrix(0,nrow=Ti,ncol=Ti)
  for(i in 1:(Ti-1)) Wt[i,i+1] <- 1
  for(i in 1:(Ti-1)) Wt[i+1,i] <- 1
  Wnb <- mat2listw(Wt)
  Wnb <- nb2WB(nb = Wnb$neighbours)
  
  # Indicator matrices — pass these as data
  Z_space <- matrix(0, nrow = n, ncol = N)   # n × N
  Z_time  <- matrix(0, nrow = Ti, ncol = N)  # Ti × N
  
  for(i in 1:N){
    Z_space[county_i[i], i] <- 1 / sum(county_i == county_i[i])
    Z_time[year_i[i],   i] <- 1 / sum(year_i   == year_i[i])
  }
  
  
  consts = list(N = nrow(data),
                Ti = length(unique(data$year)),
                S  = length(unique(data$state)),
                n  = length(spatial_object$num),
                L = length(spatial_object$weights),
                state_i = state_i,
                year_i = year_i,
                county_i = county_i,
                adj = spatial_object$adj, 
                num = spatial_object$num,
                weights= spatial_object$weights,
                scale=scale,
                time=time,
                L_time = length(Wnb$adj),
                adj_time = Wnb$adj,
                num_time = Wnb$num,
                weights_time = Wnb$weights, 
                lambda=4.60,
                L_ST = L_ST,
                weights_ST=weights_ST,
                adj_ST=adj_ST,
                num_ST=num_ST)
  
  nimble_input = list(
    C = as.integer(data$C),
    M = as.matrix(data[, paste0('M',seq(15,55,5))]),
    Lx_star = as.matrix(data[, paste0('L',seq(0,55,5))]),
    VAR_Lx_star = (sqrt(as.matrix(data[, paste0('VAR_L',seq(0,55,5))]))),
    TFR_nat = data$TFR_nat,
    m = m,
    X = X,
    Z_time=Z_time,
    Z_space=Z_space)
  
  
  nimble_input_new = list(
    C = as.integer(data$C),
    M = as.matrix(data[, paste0('M',seq(15,55,5))]),
    Lx_star = as.matrix(data[, paste0('L',seq(0,55,5))]),
    VAR_Lx_star = (sqrt(as.matrix(data[, paste0('VAR_L',seq(0,55,5))]))),
    TFR_nat = data$TFR_nat,
    m = m_new,
    X = X_new,
    Z_time=Z_time,
    Z_space=Z_space)
  
  HMD_usa_male_mort <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/PHD STAT/PhD Dissertation/Male Fertility/Replication_Package/HMD_usa_male_mort.txt", sep="")
  
  HMD_usa_male_mort <- HMD_usa_male_mort %>%
    mutate(Lx=Lx/1e5,
           Lx=ifelse(Age=="0",Lx[Age=="0"]+Lx[Age=="1-4"],Lx)) %>%
    filter(Age!="1-4") %>%
    mutate(Age=as.numeric(sub("-.*", "", Age))) %>%
    filter(Age %in% seq(0,55,5)) %>%
    dplyr::select(year=Year,Age,Lx) %>%
    pivot_wider(names_from = "Age",values_from = "Lx")
  
  names(HMD_usa_male_mort)[2:ncol(HMD_usa_male_mort)] = paste0('Lnat_',seq(0,55,5))
  
  data = data %>%
    left_join(HMD_usa_male_mort)
  
  nimble_input_new = list(
    C = as.integer(data$C),
    M = as.matrix(data[, paste0('M',seq(15,55,5))]),
    Lx_star = as.matrix(data[, paste0('L',seq(0,55,5))]),
    VAR_Lx_star = (sqrt(as.matrix(data[, paste0('VAR_L',seq(0,55,5))]))),
    TFR_nat = data$TFR_nat,
    m = m_new,
    X = X_new,
    Z_time=Z_time,
    Z_space=Z_space)
  
  nimble_input_nat_mort = list(
    C = as.integer(data$C),
    M = as.matrix(data[, paste0('M',seq(15,55,5))]),
    Lx_star = as.matrix(data[, paste0('Lnat_',seq(0,55,5))]),
    VAR_Lx_star = matrix(0.001,nrow=nrow(data),12),
    TFR_nat = data$TFR_nat,
    m = m_new,
    X = X_new,
    Z_time=Z_time,
    Z_space=Z_space)
  
  init = list(beta = matrix(runif(N*2, min=-.10, max=.10) , N ,2),
              TFR  = pmax(.10, rnorm(N, 2, sd=.50)),
              Lx = matrix(runif(N*12,4.5,5),N,12),
              us = rep(0,n),
              s = rep(0,n),
              b = rep(0,n),
              nu = rep(0,N),
              delta = rep(0,Ti),
              sigma_delta=1,
              alpha = rep(0,n),
              sigma_alpha=1,
              rho=0.5,
              sigma_nu = 0.025,
              log_r = rep(2,n),
              sigma_log_r = 1,
              mu_log_r = 1,
              sigma_b=1)
  
  
  return(list(nimble_input=nimble_input,nimble_input_new=nimble_input_new,nimble_input_nat_mort=nimble_input_nat_mort,consts=consts,init=init))
  
  
  
}



##### US Female Data for Model Input ###

data_input_women_us = function(data,
                               statefip,
                               usa_geo,
                               m,
                               X,
                               m_new,
                               X_new,
                               years=1982:2019){
  
  adj_matrix = adj_matrix_constr(usa_geo,fips=statefip) 
  W = as.matrix(adj_matrix)
  W.scale = -W
  diag(W.scale) = abs(apply(W.scale, 1, sum))
  Q = INLA::inla.scale.model(W.scale, constr=list(A=matrix(1, nrow=1, ncol=nrow(W.scale)), e=0))
  scale = exp((1/nrow(W.scale))*sum(log(1/diag(Q))))
  data = filter(data,year %in% years,state %in% statefip) 
  spatial_object = as.carAdjacency(adj_matrix)
  N = nrow(data)
  Ti = length(unique(data$year))
  S = length(unique(data$state))
  n = length(unique(data$fips))
  
  
  Q_time <- build_RW1(Ti)      
  Q_ST <- kronecker(Q_time, W)
  
  nb_ST   <- mat2listw(Q_ST != 0, style = "B")$neighbours
  # weights = actual values from Q_ST (off-diagonal, negated)
  num_ST  <- sapply(nb_ST, length)
  adj_ST  <- unlist(nb_ST)
  weights_ST <- sapply(seq_along(nb_ST), function(i){
    j_list <- nb_ST[[i]]
    as.numeric(-Q_ST[i, j_list])   # off-diagonal entries (positive)
  })
  weights_ST <- unlist(weights_ST)
  L_ST <- length(adj_ST)
  
  state_i = data %>%
    ungroup() %>%
    dplyr::select(state) %>%
    pull()
  
  year_i = data %>%
    ungroup() %>%
    dplyr::select(year) %>%
    pull()
  
  county_i = data %>%
    ungroup() %>%
    dplyr::select(fips) %>%
    pull()
  
  
  state_i = as.integer(factor(state_i,levels=unique(data$state)))
  year_i = as.integer(factor(year_i,levels=unique(data$year)))
  county_i = as.integer(factor(county_i,levels=unique(data$fips)))
  
  
  years = unique(data$year)
  time = (years-mean(years))/sd(years)
  
  Wt = matrix(0,nrow=Ti,ncol=Ti)
  for(i in 1:(Ti-1)) Wt[i,i+1] <- 1
  for(i in 1:(Ti-1)) Wt[i+1,i] <- 1
  Wnb <- mat2listw(Wt)
  Wnb <- nb2WB(nb = Wnb$neighbours)
  
  # Indicator matrices — pass these as data
  Z_space <- matrix(0, nrow = n, ncol = N)   # n × N
  Z_time  <- matrix(0, nrow = Ti, ncol = N)  # Ti × N
  
  for(i in 1:N){
    Z_space[county_i[i], i] <- 1 / sum(county_i == county_i[i])
    Z_time[year_i[i],   i] <- 1 / sum(year_i   == year_i[i])
  }
  
  
  
  consts = list(N = nrow(data),
                Ti = length(unique(data$year)),
                S  = length(unique(data$state)),
                n  = length(spatial_object$num),
                L = length(spatial_object$weights),
                state_i = state_i,
                year_i = year_i,
                county_i = county_i,
                adj = spatial_object$adj, 
                num = spatial_object$num,
                weights= spatial_object$weights,
                scale=scale,
                time=time,
                L_time = length(Wnb$adj),
                adj_time = Wnb$adj,
                num_time = Wnb$num,
                weights_time = Wnb$weights, 
                lambda=4.60,
                L_ST = L_ST,
                weights_ST=weights_ST,
                adj_ST=adj_ST,
                num_ST=num_ST)
  
  
  nimble_input = list(
    C = as.integer(data$C),
    W = as.matrix(data[, paste0('W',seq(15,45,5))]),
    Lx_star = as.matrix(data[, paste0('L',seq(0,45,5))]),
    VAR_Lx_star = (sqrt(as.matrix(data[, paste0('VAR_L',seq(0,45,5))]))),
    TFR_nat = data$TFR_nat,
    m = m,
    X = X)
  
  
  
  
  nimble_input_new = list(
    C = as.integer(data$C),
    W = as.matrix(data[, paste0('W',seq(15,45,5))]),
    Lx_star = as.matrix(data[, paste0('L',seq(0,45,5))]),
    VAR_Lx_star = (sqrt(as.matrix(data[, paste0('VAR_L',seq(0,45,5))]))),
    TFR_nat = data$TFR_nat,
    m = m_new,
    X = X_new)
  
  
  
  HMD_usa_male_mort <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/PHD STAT/PhD Dissertation/Male Fertility/Replication_Package/HMD_usa_male_mort.txt", sep="")
  
  HMD_usa_male_mort <- HMD_usa_male_mort %>%
    mutate(Lx=Lx/1e5,
           Lx=ifelse(Age=="0",Lx[Age=="0"]+Lx[Age=="1-4"],Lx)) %>%
    filter(Age!="1-4") %>%
    mutate(Age=as.numeric(sub("-.*", "", Age))) %>%
    filter(Age %in% seq(0,55,5)) %>%
    dplyr::select(year=Year,Age,Lx) %>%
    pivot_wider(names_from = "Age",values_from = "Lx")
  
  names(HMD_usa_male_mort)[2:ncol(HMD_usa_male_mort)] = paste0('Lnat_',seq(0,55,5))
  
  data = data %>%
    left_join(HMD_usa_male_mort)
  
  
  
  nimble_input_nat_mort = list(
    C = as.integer(data$C),
    W = as.matrix(data[, paste0('W',seq(15,45,5))]),
    Lx_star = as.matrix(data[, paste0('L',seq(0,45,5))]),
    VAR_Lx_star = matrix(0.01,nrow=nrow(data),ncol=10),
    TFR_nat = data$TFR_nat,
    m = m_new,
    X = X_new)
  
  
  
  
  init = list(beta = matrix(runif(N*2, min=-.10, max=.10) , N ,2),
              TFR  = pmax(.10, rnorm(N, 2, sd=.50)),
              Lx = matrix(runif(N*10,4.5,5),N,10),
              us = rep(0,n),
              s = rep(0,n),
              b = rep(0,n),
              nu = rep(0,N),
              delta = rep(0,Ti),
              sigma_delta=1,
              alpha = rep(0,n),
              sigma_alpha=1,
              rho=0.5,
              sigma_nu = 0.025,
              log_r = rep(2,n),
              sigma_log_r = 1,
              mu_log_r = 1,
              sigma_b=1)
  
  
  
  return(list(nimble_input=nimble_input,
              nimble_input_new=nimble_input_new,
              nimble_input_nat_mort=nimble_input_nat_mort,
              consts=consts,
              init=init))
  
  
  
}




#### Nigeria Male Data for Model Input ###

data_input_men_nigeria = function(data,
                                  nga_geo,
                                  m,
                                  X,
                                  m_new,
                                  X_new,
                                  years=2000:2020){
  
  
  nb_nigeria <- poly2nb(nga_geo, queen = FALSE)
  adj_matrix_nigeria <- nb2mat(nb_nigeria, style = "B", zero.policy = TRUE)
  colnames(adj_matrix_nigeria) = unique(data$region)
  rownames(adj_matrix_nigeria) = unique(data$region)
  spatial_object_nigeria = as.carAdjacency(adj_matrix_nigeria)
  
  
  data = filter(data,year %in% years) 
  
  year_i = data %>%
    ungroup() %>%
    #arrange(year) %>%
    dplyr::select(year) %>%
    pull()
  
  county_i = data %>%
    ungroup() %>%
    arrange(region) %>%
    dplyr::select(region) %>%
    pull()
  
  year_i = as.integer(factor(year_i,levels=unique(data$year)))
  county_i = as.integer(factor(county_i,levels=unique(data$region)))
  
  W = as.matrix(adj_matrix_nigeria)
  W <- ifelse(W > 0, 1, 0)
  diag(W) <- 0
  
  
  N = nrow(data)
  Ti = length(unique(data$year))
  n = length(unique(data$region))
  time = (years-mean(years))/sd(years)
  
  Q_time <- build_RW1(Ti)      
  Q_ST <- kronecker(Q_time, W)
  
  nb_ST   <- mat2listw(Q_ST != 0, style = "B")$neighbours
  # weights = actual values from Q_ST (off-diagonal, negated)
  num_ST  <- sapply(nb_ST, length)
  adj_ST  <- unlist(nb_ST)
  weights_ST <- sapply(seq_along(nb_ST), function(i){
    j_list <- nb_ST[[i]]
    as.numeric(-Q_ST[i, j_list])   # off-diagonal entries (positive)
  })
  weights_ST <- unlist(weights_ST)
  L_ST <- length(adj_ST)
  
  
  W.scale <- -W
  diag(W.scale) <- abs(apply(W.scale, 1, sum))
  Q = INLA::inla.scale.model(W.scale, constr=list(A=matrix(1, nrow=1, ncol=nrow(W.scale)), e=0))
  scale = exp((1/nrow(W.scale))*sum(log(1/diag(Q))))
  
  Wt = matrix(0,nrow=Ti,ncol=Ti)
  for(i in 1:(Ti-1)) Wt[i,i+1] <- 1
  for(i in 1:(Ti-1)) Wt[i+1,i] <- 1
  Wnb <- mat2listw(Wt)
  Wnb <- nb2WB(nb = Wnb$neighbours)
  
  consts = list(N = nrow(data),
                Ti = length(unique(data$year)),
                n  = length(spatial_object_nigeria$num),
                L = length(spatial_object_nigeria$weights),
                year_i = year_i,
                county_i = county_i,
                adj = spatial_object_nigeria$adj, 
                num = spatial_object_nigeria$num,
                weights= spatial_object_nigeria$weights,
                scale=scale,
                time=time,
                L_time = length(Wnb$adj),
                adj_time = Wnb$adj,
                num_time = Wnb$num,
                weights_time = Wnb$weights, 
                lambda = 4.60,
                L_ST = L_ST,
                weights_ST=weights_ST,
                adj_ST=adj_ST,
                num_ST=num_ST)
  
  nimble_input = list(
    C = as.integer(data$C),
    M = as.matrix(data[, paste0('M',seq(15,70,5))]),
    Lx_star = as.matrix(data[, paste0('L',seq(0,70,5))]),
    VAR_Lx_star = as.matrix(data[, paste0('se_L',seq(0,70,5))]),
    TFR_nat = data$TFR,
    m = m,
    X = X)
  
  load("un_life_tables.RData")
  
  nigeria_nat_mort = un_life_tables_male %>%
    filter(iso3=="NGA",year %in% years)
  
  
  data = data %>%
    left_join(nigeria_nat_mort,by="year")
  
  nimble_input_nat_mort = list(
    C = as.integer(data$C),
    M = as.matrix(data[, paste0('M',seq(15,70,5))]),
    Lx_star = as.matrix(data[, paste0('Lnat_',seq(0,70,5))]),
    VAR_Lx_star = matrix(0.001,nrow=nrow(data),15),
    TFR_nat = data$TFR,
    m = m,
    X = X)
  
  
  nimble_input_new_ref = list(
    C = as.integer(data$C),
    M = as.matrix(data[, paste0('M',seq(15,70,5))]),
    Lx_star = as.matrix(data[, paste0('L',seq(0,70,5))]),
    VAR_Lx_star = as.matrix(data[, paste0('se_L',seq(0,70,5))]),
    TFR_nat = data$TFR,
    m = m_new,
    X = X_new)
  
  
  init = list(beta = matrix(runif(N*2, min=-.10, max=.10) , N ,2),
              TFR  = pmax(.10, rnorm(N, 2, sd=.50)),
              Lx = matrix(runif(N*15,4.5,5),N,15),
              us = rep(0,n),
              s = rep(0,n),
              b = rep(0,n),
              nu = rep(0,N),
              delta = rep(0,Ti),
              sigma_delta=1,
              alpha = rep(0,n),
              sigma_alpha=1,
              rho=0.5,
              sigma_nu = 0.025,
              log_r = rep(2,n),
              sigma_log_r = 1,
              mu_log_r = 1,
              sigma_b=1)
  
  
  return(list(nimble_input=nimble_input,
              nimble_input_new_ref=nimble_input_new_ref,
              nimble_input_nat_mort=nimble_input_nat_mort,
              consts=consts,
              init=init))
  
  
  
}



#### Nigeria Female Data for Model Input ###

data_input_women_nigeria = function(data,
                                    nga_geo,
                                    m,
                                    X,
                                    m_new,
                                    X_new,
                                    years=2000:2020){
  
  
  nb_nigeria <- poly2nb(nga_geo, queen = FALSE)
  adj_matrix_nigeria <- nb2mat(nb_nigeria, style = "B", zero.policy = TRUE)
  colnames(adj_matrix_nigeria) = unique(data$region)
  rownames(adj_matrix_nigeria) = unique(data$region)
  spatial_object_nigeria = as.carAdjacency(adj_matrix_nigeria)
  
  
  
  year_i = data %>%
    ungroup() %>%
    #arrange(year) %>%
    dplyr::select(year) %>%
    pull()
  
  county_i = data %>%
    ungroup() %>%
    arrange(region) %>%
    dplyr::select(region) %>%
    pull()
  
  year_i = as.integer(factor(year_i,levels=unique(data$year)))
  county_i = as.integer(factor(county_i,levels=unique(data$region)))
  
  W = as.matrix(adj_matrix_nigeria)
  W <- ifelse(W > 0, 1, 0)
  diag(W) <- 0
  
  data = filter(data,year %in% years) 
  N = nrow(data)
  Ti = length(unique(data$year))
  n = length(unique(data$region))
  time = (years-mean(years))/sd(years)
  
  Q_time <- build_RW1(Ti)      
  Q_ST <- kronecker(Q_time, W)
  
  nb_ST   <- mat2listw(Q_ST != 0, style = "B")$neighbours
  # weights = actual values from Q_ST (off-diagonal, negated)
  num_ST  <- sapply(nb_ST, length)
  adj_ST  <- unlist(nb_ST)
  weights_ST <- sapply(seq_along(nb_ST), function(i){
    j_list <- nb_ST[[i]]
    as.numeric(-Q_ST[i, j_list])   # off-diagonal entries (positive)
  })
  weights_ST <- unlist(weights_ST)
  L_ST <- length(adj_ST)
  Wt = matrix(0,nrow=Ti,ncol=Ti)
  for(i in 1:(Ti-1)) Wt[i,i+1] <- 1
  for(i in 1:(Ti-1)) Wt[i+1,i] <- 1
  Wnb <- mat2listw(Wt)
  Wnb <- nb2WB(nb = Wnb$neighbours)
  
  
  W.scale <- -W
  diag(W.scale) <- abs(apply(W.scale, 1, sum))
  Q = INLA::inla.scale.model(W.scale, constr=list(A=matrix(1, nrow=1, ncol=nrow(W.scale)), e=0))
  scale = exp((1/nrow(W.scale))*sum(log(1/diag(Q))))
  
  consts = list(N = nrow(data),
                Ti = length(unique(data$year)),
                n  = length(spatial_object_nigeria$num),
                L = length(spatial_object_nigeria$weights),
                year_i = year_i,
                county_i = county_i,
                adj = spatial_object_nigeria$adj, 
                num = spatial_object_nigeria$num,
                weights= spatial_object_nigeria$weights,
                scale=scale,
                time=time,
                L_time = length(Wnb$adj),
                adj_time = Wnb$adj,
                num_time = Wnb$num,
                weights_time = Wnb$weights, 
                lambda=4.60,
                L_ST = L_ST,
                weights_ST=weights_ST,
                adj_ST=adj_ST,
                num_ST=num_ST)
  
  nimble_input = list(
    C = as.integer(data$C),
    W = as.matrix(data[, paste0('W',seq(15,45,5))]),
    Lx_star = as.matrix(data[, paste0('L',seq(0,45,5))]),
    VAR_Lx_star = as.matrix(data[, paste0('se_L',seq(0,45,5))]),
    TFR_nat = data$TFR_female,
    m = m,
    X = X)
  
  
  nimble_input_new_ref = list(
    C = as.integer(data$C),
    W = as.matrix(data[, paste0('W',seq(15,45,5))]),
    Lx_star = as.matrix(data[, paste0('L',seq(0,45,5))]),
    VAR_Lx_star = as.matrix(data[, paste0('se_L',seq(0,45,5))]),
    TFR_nat = data$TFR_female,
    m = m_new,
    X = X_new)
  
  load("un_life_tables.RData")
  
  nigeria_nat_mort = un_life_tables_female %>%
    filter(iso3=="NGA",year %in% years)
  
  
  data = data %>%
    left_join(nigeria_nat_mort,by="year")
  
  nimble_input_nat_mort = list(
    C = as.integer(data$C),
    W = as.matrix(data[, paste0('W',seq(15,45,5))]),
    Lx_star = as.matrix(data[, paste0('Lnat_',seq(0,45,5))]),
    VAR_Lx_star = matrix(0.001,nrow=nrow(data),10),
    TFR_nat = data$TFR_female,
    m = m,
    X = X)
  
  
  
  init = list(beta = matrix(runif(N*2, min=-.10, max=.10) , N ,2),
              TFR  = pmax(.10, rnorm(N, 2, sd=.50)),
              Lx = matrix(runif(N*10,4.5,5),N,10),
              us = rep(0,n),
              s = rep(0,n),
              b = rep(0,n),
              nu = rep(0,N),
              delta = rep(0,Ti),
              sigma_delta=1,
              alpha = rep(0,n),
              sigma_alpha=1,
              rho=0.5,
              sigma_nu = 0.025,
              log_r = rep(2,n),
              sigma_log_r = 1,
              mu_log_r = 1,
              sigma_b=1)
  
  
  return(list(nimble_input=nimble_input,
              nimble_input_new_ref=nimble_input_new_ref,
              nimble_input_nat_mort=nimble_input_nat_mort,
              consts=consts,
              init=init))
  
  
  
}



#### Mexico Male Data for Model Input ####

data_input_men_mexico = function(data,mex_geo,m,
                                 X,m_new,X_new,years=1990:2024){
  
  
  
  data <- filter(data,year %in% years) %>%
    arrange(state,year)
  
  
  nb_mexico <- poly2nb(mex_geo, queen = FALSE)
  
  adj_matrix_mexico <- nb2mat(nb_mexico, style = "B", zero.policy = TRUE)
  colnames(adj_matrix_mexico) = unique(data$state)
  rownames(adj_matrix_mexico) = unique(data$state)
  spatial_object_mexico = as.carAdjacency(adj_matrix_mexico)
  
  
  
  year_i = data %>%
    ungroup() %>%
    #arrange(year) %>%
    dplyr::select(year) %>%
    pull()
  
  county_i = data %>%
    ungroup() %>%
    arrange(state) %>%
    dplyr::select(state) %>%
    pull()
  
  
  year_i = as.integer(factor(year_i,levels=unique(data$year)))
  county_i = as.integer(factor(county_i,levels=unique(data$state)))
  
  W = as.matrix(adj_matrix_mexico)
  W <- ifelse(W > 0, 1, 0)
  diag(W) <- 0
  
  N = nrow(data)
  Ti = length(unique(data$year))
  n = length(unique(data$state))
  time = (years-mean(years))/sd(years)
  
  Q_time <- build_RW1(Ti)      
  Q_ST <- kronecker(Q_time, W)
  
  nb_ST   <- mat2listw(Q_ST != 0, style = "B")$neighbours
  # weights = actual values from Q_ST (off-diagonal, negated)
  num_ST  <- sapply(nb_ST, length)
  adj_ST  <- unlist(nb_ST)
  weights_ST <- sapply(seq_along(nb_ST), function(i){
    j_list <- nb_ST[[i]]
    as.numeric(-Q_ST[i, j_list])   # off-diagonal entries (positive)
  })
  weights_ST <- unlist(weights_ST)
  L_ST <- length(adj_ST)
  
  
  W.scale <- -W
  diag(W.scale) <- abs(apply(W.scale, 1, sum))
  Q = INLA::inla.scale.model(W.scale, constr=list(A=matrix(1, nrow=1, ncol=nrow(W.scale)), e=0))
  scale = exp((1/nrow(W.scale))*sum(log(1/diag(Q))))
  
  # Precompute outside NIMBLE (linear chain graph)
  Wt = matrix(0,nrow=Ti,ncol=Ti)
  for(i in 1:(Ti-1)) Wt[i,i+1] <- 1
  for(i in 1:(Ti-1)) Wt[i+1,i] <- 1
  Wnb <- mat2listw(Wt)
  Wnb <- nb2WB(nb = Wnb$neighbours)
  consts = list(N = nrow(data),
                Ti = length(unique(data$year)),
                n  = length(spatial_object_mexico$num),
                L = length(spatial_object_mexico$weights),
                year_i = year_i,
                county_i = county_i,
                adj = spatial_object_mexico$adj, 
                num = spatial_object_mexico$num,
                weights= spatial_object_mexico$weights,
                scale=scale,
                time=time,
                L_time = length(Wnb$adj),
                adj_time = Wnb$adj,
                num_time = Wnb$num,
                weights_time = Wnb$weights, 
                lambda=4.60,
                L_ST = L_ST,
                weights_ST=weights_ST,
                adj_ST=adj_ST,
                num_ST=num_ST)
  
  nimble_input = list(
    C = as.integer(data$C),
    M = as.matrix(data[, paste0('M',seq(15,55,5))]),
    Lx_star = as.matrix(data[, paste0('L',seq(0,55,5))]),
    VAR_Lx_star = as.matrix(data[, paste0('se_L',seq(0,55,5))]),
    TFR_nat = data$TFR,
    m = m,
    X = X)
  
  
  nimble_input_new_ref = list(
    C = as.integer(data$C),
    M = as.matrix(data[, paste0('M',seq(15,55,5))]),
    Lx_star = as.matrix(data[, paste0('L',seq(0,55,5))]),
    VAR_Lx_star = as.matrix(data[, paste0('se_L',seq(0,55,5))]),
    TFR_nat = data$TFR,
    m = m_new,
    X = X_new)
  
  
  load("un_life_tables.RData")
  
  mexico_nat_mort = un_life_tables_male %>%
    filter(iso3=="MEX",year %in% years)
  
  
  
  data = data %>%
    left_join(mexico_nat_mort,by="year")
  
  
  nimble_input_nat_mort = list(
    C = as.integer(data$C),
    M = as.matrix(data[, paste0('M',seq(15,55,5))]),
    Lx_star = as.matrix(data[, paste0('Lnat_',seq(0,55,5))]),
    VAR_Lx_star = matrix(0.001,nrow=nrow(data),ncol=12),
    TFR_nat = data$TFR,
    m = m,
    X = X)
  
  
  
  init = list(beta = matrix(runif(N*2, min=-.10, max=.10) , N ,2),
              TFR  = pmax(.10, rnorm(N, 2.5, sd=.50)),
              Lx = matrix(runif(N*12,4.8,5),N,12),
              us = rep(0.01,n),
              s = rep(0.01,n),
              b = rep(0.01,n),
              nu = rep(0.01,N),
              delta = rep(0.01,Ti),
              sigma_delta=1,
              alpha = rep(0.01,n),
              sigma_alpha=1,
              rho=0.5,
              sigma_nu = 0.025,
              log_r = rep(2,n),
              sigma_log_r = 1,
              mu_log_r = 1,
              sigma_b=1)
  
  
  return(list(nimble_input=nimble_input,
              nimble_input_new_ref=nimble_input_new_ref,
              nimble_input_nat_mort=nimble_input_nat_mort,
              consts=consts,
              init=init))
  
  
  
}


#### Mexico Female Data for Model Input ####

data_input_women_mexico = function(data,mex_geo,m,
                                   X,m_new,X_new,years=1990:2024){
  
  #data <- CW_mexico
  #years <- 1990:2024
  #mex_geo <- mexico_geo
  data <- filter(data,year %in% years) %>%
    arrange(state,year)
  
  nb_mexico <- poly2nb(mex_geo, queen = FALSE)
  
  adj_matrix_mexico <- nb2mat(nb_mexico, style = "B", zero.policy = TRUE)
  colnames(adj_matrix_mexico) = unique(data$state)
  rownames(adj_matrix_mexico) = unique(data$state)
  spatial_object_mexico = as.carAdjacency(adj_matrix_mexico)
  
  
  
  year_i = data %>%
    ungroup() %>%
    #arrange(year) %>%
    dplyr::select(year) %>%
    pull()
  
  county_i = data %>%
    ungroup() %>%
    arrange(state) %>%
    dplyr::select(state) %>%
    pull()
  
  
  year_i = as.integer(factor(year_i,levels=unique(data$year)))
  county_i = as.integer(factor(county_i,levels=unique(data$state)))
  
  W = as.matrix(adj_matrix_mexico)
  W <- ifelse(W > 0, 1, 0)
  diag(W) <- 0
  build_RW1 <- function(Ti) {
    Q <- matrix(0, nrow = Ti, ncol = Ti)
    
    diag(Q) <- c(1, rep(2, Ti - 2), 1)
    
    for (i in 1:(Ti - 1)) {
      Q[i, i + 1] <- -1
      Q[i + 1, i] <- -1
    }
    
    return(Q)
  }
  
  
  
  N = nrow(data)
  Ti = length(unique(data$year))
  n = length(unique(data$state))
  time = (years-mean(years))/sd(years)
  
  Q_time <- build_RW1(Ti)
  
  Q_ST <- kronecker(Q_time, W)
  dim(Q_ST)
  nb_ST   <- mat2listw(Q_ST != 0, style = "B")$neighbours
  # weights = actual values from Q_ST (off-diagonal, negated)
  num_ST  <- sapply(nb_ST, length)
  adj_ST  <- unlist(nb_ST)
  weights_ST <- sapply(seq_along(nb_ST), function(i){
    j_list <- nb_ST[[i]]
    as.numeric(-Q_ST[i, j_list])   # off-diagonal entries (positive)
  })
  weights_ST <- unlist(weights_ST)
  L_ST <- length(adj_ST)
  
  
  W.scale <- -W
  diag(W.scale) <- abs(apply(W.scale, 1, sum))
  Q = INLA::inla.scale.model(W.scale, constr=list(A=matrix(1, nrow=1, ncol=nrow(W.scale)), e=0))
  scale = exp((1/nrow(W.scale))*sum(log(1/diag(Q))))
  
  Wt = matrix(0,nrow=Ti,ncol=Ti)
  for(i in 1:(Ti-1)) Wt[i,i+1] <- 1
  for(i in 1:(Ti-1)) Wt[i+1,i] <- 1
  Wnb <- mat2listw(Wt)
  Wnb <- nb2WB(nb = Wnb$neighbours)
  consts = list(N = nrow(data),
                Ti = length(unique(data$year)),
                n  = length(spatial_object_mexico$num),
                L = length(spatial_object_mexico$weights),
                year_i = year_i,
                county_i = county_i,
                adj = spatial_object_mexico$adj, 
                num = spatial_object_mexico$num,
                weights= spatial_object_mexico$weights,
                scale=scale,
                time=time,
                L_time = length(Wnb$adj),
                adj_time = Wnb$adj,
                num_time = Wnb$num,
                weights_time = Wnb$weights, 
                lambda=4.60,
                L_ST = L_ST,
                weights_ST=weights_ST,
                adj_ST=adj_ST,
                num_ST=num_ST)
  
  
  
  
  
  nimble_input = list(
    C = as.integer(data$C),
    W = as.matrix(data[, paste0('W',seq(15,45,5))]),
    Lx_star = as.matrix(data[, paste0('L',seq(0,45,5))]),
    VAR_Lx_star = as.matrix(data[, paste0('se_L',seq(0,45,5))]),
    TFR_nat = data$TFR,
    m = m,
    X = X)
  
  
  nimble_input_new_ref = list(
    C = as.integer(data$C),
    W = as.matrix(data[, paste0('W',seq(15,45,5))]),
    Lx_star = as.matrix(data[, paste0('L',seq(0,45,5))]),
    VAR_Lx_star = as.matrix(data[, paste0('se_L',seq(0,45,5))]),
    TFR_nat = data$TFR,
    m = m_new,
    X = X_new)
  
  
  load("un_life_tables.RData")
  
  mexico_nat_mort = un_life_tables_female %>%
    filter(iso3=="MEX",year %in% years)
  
  data = data %>%
    left_join(mexico_nat_mort)
  
  
  nimble_input_nat_mort = list(
    C = as.integer(data$C),
    W = as.matrix(data[, paste0('W',seq(15,45,5))]),
    Lx_star = as.matrix(data[, paste0('Lnat_',seq(0,45,5))]),
    VAR_Lx_star = matrix(0.001,nrow=nrow(data),ncol=10),
    TFR_nat = data$TFR,
    m = m,
    X = X)
  
  init = list(beta = matrix(runif(N*2, min=-.10, max=.10) , N ,2),
              TFR  = pmax(.10, rnorm(N, 2, sd=.50)),
              Lx = matrix(runif(N*10,4.5,5),N,10),
              us = rep(0,n),
              s = rep(0,n),
              b = rep(0,n),
              nu = rep(0,N),
              delta = rep(0,Ti),
              sigma_delta=1,
              alpha = rep(0,n),
              sigma_alpha=1,
              rho=0.5,
              sigma_nu = 0.025,
              log_r = rep(2,n),
              sigma_log_r = 1,
              mu_log_r = 1,
              sigma_b=1)
  
  
  return(list(nimble_input=nimble_input,
              nimble_input_new_ref=nimble_input_new_ref,
              nimble_input_nat_mort=nimble_input_nat_mort,
              consts=consts,
              init=init))
  
  
  
}


#### Global Spatio-temporal Moran's I ####

global_moran_I_spatio_temporal_usa = function(output,data,usa_geo,fips="06"){
  
  
  
  data = data[data$state==fips,]
  output = results_by_state_male[[fips]] 
  post_mat <- do.call(rbind, lapply(output, function(s) as.matrix(s)))
  post_mat <- post_mat[1:5000,]
  
  data_draw = numeric()
  library(doFuture, quietly = TRUE)
  N = nrow(data)
  n_draws = nrow(post_mat)
  col_res   <- function(i) sprintf("res[%d]", i)
  W = adj_matrix_constr(long_usa,fips=fips)
  
  plan(multisession, workers = 4)
  options(future.globals.maxSize = 2 * 1024^3) 
  ## Toy example of using foreach+future
  out <- foreach(i = seq_len(N),.combine = 'rbind') %dofuture% {
    data.frame(year=data$year[i],
               fips=data$fips[i],
               draw=1:n_draws,
               est=post_mat[,col_res(i)])
  }
  
  #post_mat <- do.call(rbind, lapply(output, function(s) as.matrix(s)))
  
  
  
  
  N = nrow(out[out$draw==1,])
  Ti = length(unique(out$year))
  n = length(unique(out$fips))
  n_draws = length(unique(out$draw))
  
  T_adj <- bandSparse(Ti, Ti, c(-1, 1),           # sparse tridiagonal
                      diagonals = list(rep(1, Ti-1), rep(1, Ti-1)))
  
  W_sp  <- as(W,"sparseMatrix")
  I_n   <- Diagonal(n)
  I_Ti  <- Diagonal(Ti)
  
  # Full ST weight: (T_adj ⊗ W) + (T_adj ⊗ I_n)
  V_full <- kronecker(T_adj, W_sp) + kronecker(T_adj, I_n)
  
  # Row-standardise once → listw
  V_listw <- mat2listw(as.matrix(V_full), style = "W", zero.policy = TRUE)
  
  library(future.apply)
  
  res_list <- future_lapply(1:n_draws, function(m) {
    # 1. Filter for the specific posterior draw
    # Ensure data_sim is ordered identically to how V_listw was built (Time then Space)
    dm <- out[out$draw == m, ]
    
    # 2. Run the test
    # zero.policy = TRUE is vital if any regions have no neighbors
    res <- moran.test(
      dm$est, 
      V_listw, 
      zero.policy = TRUE, 
      alternative = "two.sided"
    )
    
    # 3. Return as a clean data frame row
    data.frame(
      draw = m,
      I = as.numeric(res$estimate[1]),
      expectation = as.numeric(res$estimate[2]),
      variance = as.numeric(res$estimate[3]),
      p_value = res$p.value
    )
  }, future.seed = TRUE)
  
  
  moran_results <- bind_rows(res_list)
  
  summary <- moran_results %>%
    summarise(
      I_mean = mean(I, na.rm = TRUE),
      I_median = median(I, na.rm = TRUE),
      I_q025 = quantile(I, 0.025, na.rm = TRUE),
      I_q975 = quantile(I, 0.975, na.rm = TRUE),
      .groups = "drop"
    )
  
  
  return(list(moran_results=moran_results,summary=summary))
  
}

global_moran_I_spatio_temporal_nga = function(output,data,nga_geo){
  output = nigeria_female_model 
  post_mat <- do.call(rbind, lapply(output, function(s) as.matrix(s)))
  col_res   <- function(i) sprintf("res[%d]", i)
  n_draws = nrow(post_mat)
  data = CW_nigeria
  res_result = numeric()
  N = nrow(data)
  library(data.table)
  
  N <- nrow(data)
  
  dt <- as.data.table(post_mat[, sapply(seq_len(N), col_res), drop = FALSE])
  dt[, draw := .I]
  
  res_long <- melt(
    dt,
    id.vars = "draw",
    variable.name = "param",
    value.name = "est"
  )
  
  res_long = as.data.frame(res_long)
  
  res_long$obs = as.integer(sub(".*\\[(\\d+)\\].*", "\\1", res_long$param))
  
  
  meta <- data.table(
    obs = seq_len(N),
    year = data$year,
    region = data$region
  )
  
  res_result <- res_long %>%
    left_join(meta,by=c("obs"))
  nga_geo = nigeria
  nb_nigeria <- poly2nb(nga_geo, queen = FALSE)
  adj_matrix_nigeria <- nb2mat(nb_nigeria, style = "B", zero.policy = TRUE)
  colnames(adj_matrix_nigeria) = unique(data$region)
  rownames(adj_matrix_nigeria) = unique(data$region)
  N = nrow(data)
  Ti = length(unique(data$year))
  n = length(unique(data$region))
  n_draws = nrow(post_mat)
  T_adj <- matrix(0, Ti, Ti)
  for(i in 1:(Ti-1)) T_adj[i, i+1] <- T_adj[i+1, i] <- 1
  W = adj_matrix_nigeria
  # 3. The Global Spatio-Temporal Adjacency Matrix
  # Option A: Only Spatial neighbors within the same year (Blocked Diagonal)
  V_spatial_only <- kronecker(T_adj, W)
  
  V_full <- kronecker(T_adj, W) + kronecker(T_adj, diag(n))
  # 4. Convert to listw for Moran's I
  V_listw <- mat2listw(as.matrix(V_full), style = "W")
  library(future.apply)
  
  options(future.globals.maxSize = 2 * 1024^3) 
  
  res_list <- future_lapply(1:n_draws, function(m) {
    # 1. Filter for the specific posterior draw
    # Ensure data_sim is ordered identically to how V_listw was built (Time then Space)
    
    dm <- res_result[res_result$draw == m, ]
    
    # 2. Run the test
    # zero.policy = TRUE is vital if any regions have no neighbors
    out <- moran.test(
      dm$est, 
      V_listw, 
      zero.policy = TRUE, 
      alternative = "two.sided"
    )
    
    # 3. Return as a clean data frame row
    data.frame(
      draw = m,
      I = as.numeric(out$estimate[1]),
      expectation = as.numeric(out$estimate[2]),
      variance = as.numeric(out$estimate[3]),
      p_value = out$p.value
    )
  }, future.seed = TRUE)
  
  
  moran_results <- bind_rows(res_list)
  
  summary <- moran_results %>%
    summarise(
      I_mean = mean(I, na.rm = TRUE),
      I_median = median(I, na.rm = TRUE),
      I_q025 = quantile(I, 0.025, na.rm = TRUE),
      I_q975 = quantile(I, 0.975, na.rm = TRUE),
      .groups = "drop"
    )
  
  
  return(list(moran_results=moran_results,summary=summary))
  
}



global_moran_I_spatio_temporal_mex <- function(post_mat, data, mex_geo) {
  
  
  # ── 1. Posterior matrix ───────────────────────────────────────────────────
  #post_mat <- do.call(rbind, lapply(output, as.matrix))
  n_draws  <- nrow(post_mat)
  N        <- nrow(data)
  Ti       <- length(unique(data$year))
  n        <- length(unique(data$state))
  
  # ── 2. Extract residual columns ONCE as a plain numeric matrix ────────────
  res_cols  <- sprintf("res[%d]", seq_len(N))
  
  
  res_mat   <- post_mat[, res_cols, drop = FALSE]   # n_draws × N matrix
  # rows = draws, cols = observations (ordered 1..N)
  
  # ── 3. Build spatio-temporal weight matrix ONCE ───────────────────────────
  nb_mex         <- poly2nb(mex_geo, queen = FALSE)
  W              <- nb2mat(nb_mex, style = "B", zero.policy = TRUE)
  rownames(W)    <- colnames(W) <- unique(data$state)
  
  T_adj          <- bandSparse(Ti, Ti, c(-1, 1),           # sparse tridiagonal
                               diagonals = list(rep(1, Ti-1), rep(1, Ti-1)))
  
  W_sp  <- as(W,        "sparseMatrix")
  I_n   <- Diagonal(n)
  I_Ti  <- Diagonal(Ti)
  
  # Full ST weight: (T_adj ⊗ W) + (T_adj ⊗ I_n)
  V_full <- kronecker(T_adj, W_sp) + kronecker(T_adj, I_n)
  
  # Row-standardise once → listw
  V_listw <- mat2listw(as.matrix(V_full), style = "W", zero.policy = TRUE)
  
  # Pre-extract the listw internals used by moran.test to avoid repeated work
  listw_weights <- V_listw$weights          # list of neighbour weight vectors
  listw_nb      <- V_listw$neighbours       # nb object
  
  # ── 4. Pre-compute Moran scaling constants (n, S0, S1, S2) ONCE ──────────
  #    moran.test re-derives these every call — we'll replicate the formula
  n_st  <- N * Ti                           # total ST observations
  S0    <- sum(unlist(listw_weights))
  # These are fixed across draws; pass them explicitly to avoid recomputation
  
  # ── 5. Parallel loop over draws ───────────────────────────────────────────
  library(future.apply)
  options(future.globals.maxSize = 2 * 1024^4)
  
  # Export only what each worker needs (avoid sending the whole environment)
  res_list <- future_lapply(
    seq_len(n_draws),
    function(m) {
      x   <- as.numeric(res_mat[m, ])          # already ordered 1..N*Ti
      out <- moran.test(x, V_listw,
                        zero.policy = TRUE,
                        alternative = "two.sided")
      c(draw        = m,
        I           = as.numeric(out$estimate[1]),
        expectation = as.numeric(out$estimate[2]),
        variance    = as.numeric(out$estimate[3]),
        p_value     = out$p.value)
    },
    future.seed   = TRUE,
    future.packages = c("spdep")
  )
  
  # ── 6. Collapse results with vapply (faster than bind_rows) ───────────────
  moran_results <- as.data.frame(
    t(vapply(res_list, identity, numeric(5)))
  )
  names(moran_results) <- c("draw", "I", "expectation", "variance", "p_value")
  
  # ── 7. Summary ────────────────────────────────────────────────────────────
  summary_out <- data.frame(
    I_mean   = mean(  moran_results$I, na.rm = TRUE),
    I_median = median(moran_results$I, na.rm = TRUE),
    I_q025   = quantile(moran_results$I, 0.025, na.rm = TRUE),
    I_q975   = quantile(moran_results$I, 0.975, na.rm = TRUE)
  )
  
  list(moran_results = moran_results, summary = summary_out)
}


#### Run code in parallel ####

run_MCMC_allcode <- function(data, code, consts,
                             init,init_name,
                             niter=niter,
                             nburnin=50000) {
  library(nimble)
  
  dynModel = nimbleModel(code = code,
                         data = data, 
                         constants = consts, 
                         inits = init)
  
  
  cat("============\n[compiling]\n============\n")
  CdynModel = compileNimble(dynModel,  showCompilerOutput = TRUE)
  cat("============\n[building]\n============\n")
  dynMCMC = buildMCMC(dynModel,  monitors = init_name, print=T)
  cat("=========\n[merging]\n============\n")
  CdynMCMC = compileNimble(dynMCMC, project = dynModel, showCompilerOutput = T)
  
  
  samples = runMCMC(CdynMCMC, 
                    niter = niter,
                    nburnin = nburnin,
                    thin = 50)
  
  
  
  return(samples)
}





build_RW1 <- function(Ti) {
  Q <- matrix(0, nrow = Ti, ncol = Ti)
  
  diag(Q) <- c(1, rep(2, Ti - 2), 1)
  
  for (i in 1:(Ti - 1)) {
    Q[i, i + 1] <- -1
    Q[i + 1, i] <- -1
  }
  
  return(Q)
}



