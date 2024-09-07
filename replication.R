### This script (partially) replicates the analysis from T. Strohsal, E. Wolf:
### "Data revisions to German national accounts: are initial releases good nowcasts?"

### The main aim is to investigate the bias, variance and predictability of final revisions. 
### A related question, which we only touch at the end, is whether simple filtering strategies can help 
### improve the initial release (hence the properties of revisions). 

### Finally, we extend the analysis to the Swiss natinal accounts (data available from SECO).


### Import libraries ###

library(dplyr)
library(tsbox)
library(readxl)
library(rsdmx)
library(lubridate)
library(zoo)
library(ggplot2)
library(car)
library(sandwich)
library(dlm)
library(data.table)
library(tidyverse)
library(xtable)


### Set folder paths ### 

DATA_PATH <- paste0(getwd(), "/data/")
RESULT_PATH <- paste0(getwd(), "/result/")


### Define functions ### 

#' load xml file to df
#'
#' @param dataPath path to data
#' @param cols cols to be selected
#' @param freq frequency of obsdate
#'
#' @return dataframe with revisions, longformat
#' @export
#'
#' @examples
loadDataSDMXToDf <- function(dataPath, cols, freq) {
  # load data
  obj_sdmx <- readSDMX(dataPath, isURL = FALSE)
  # create df
  df_raw <- as.data.frame(obj_sdmx, labels=TRUE) %>%
    # keep necessary columns
    select(all_of(cols)) %>%
    filter(BBK_RTD_REL_DAY != 40) %>%
    # create lubridate date obj
    mutate(REL_Date = dmy(
      paste(BBK_RTD_REL_DAY, BBK_RTD_REL_MONTH, BBK_RTD_REL_YEAR, sep = "-"))
    )
  return(df_raw)
}

#' Preprocess time series and compute growth rates
#'
#' @param dataPath 
#' @param cols 
#' @param freq 
#'
#' @return
#' @export
#'
#' @examples
loadPreprocessPipeline <- function(dataPath, cols, freq) {
  
  df_pp <- loadDataSDMXToDf(dataPath, cols) %>%
    mutate(obsTime = yq(obsTime)) %>%
    arrange(REL_Date, obsTime) %>%
    # if no group by, change of release date leads to enormous growth rate
    group_by(REL_Date) %>%
    mutate(diff = ((obsValue - lag(obsValue, 1)) / lag(obsValue, 1))*100) %>%
    filter(!is.na(diff)) %>%
    ungroup() %>%
    select(-c(BBK_RTD_REL_DAY, BBK_RTD_REL_MONTH, BBK_RTD_REL_YEAR))
  
  return(df_pp)
}
  

#' Get first release series ("realtime")
#'
#' @param df dataframe with revisions, longformat
#' @param col_rel_date name of column with release dates (date format)
#' @param col_val_date name of column with observation dates (date format)
#'
#' @return filtered df with every first release for obs col_val_date
#' @export
#'
#' @examples
get_realtime_series <- function(df, col_rel_date, col_val_date) {
  df_adj <- df %>%
    group_by({{ col_val_date }}) %>%
    filter({{ col_rel_date }} == min({{ col_rel_date }})) %>%
    ungroup()
  return(df_adj)
}

#' Get revisions
#'
#' @param df 
#' @param col_rel_date 
#' @param col_val_date 
#' @param i time after initial release. i=0: initial release, i=1: first revision...
#'
#' @return
#' @export
#'
#' @examples
get_revision <- function(df, col_rel_date, col_val_date, i) {
  df_adj <- df %>%
    group_by({{ col_val_date }}) %>%
    arrange({{ col_rel_date }}) %>%
    slice(i) %>%
    ungroup()
  return(df_adj)
}

#' Get final revision series for DBB data.
#'
#' @param df dataframe with revisions, longformat
#' @param col_rel_date name of column with release dates (date format)
#' @param col_val_date name of column with observation dates (date format)
#' @param p_month number of months to be added to release until "final" revision 
#' 
#' @return filtered df only with first release + p_month
#' @export
#'
#' @examples
get_final_revision_series <- function(df, col_rel_date, col_val_date) {
  df_adj <- df %>%
    group_by({{ col_val_date }}) %>%
    # collect first release date
    mutate(REL_Date_First = min({{ col_rel_date }})) %>% 
    # select final release for quarterly data
    filter( year({{ col_rel_date }}) == year(REL_Date_First) + 4 & 
              (month({{ col_rel_date }}) == 8 | month({{ col_rel_date }}) == 9)) %>%
    ungroup()
  
  df_adj <- df_adj %>%
    group_by({{ col_val_date }}) %>%
    arrange({{ col_rel_date }}) %>%
    slice(1)
  
  return(df_adj)
} 

#' Get final revision series for SECO data (only difference is +3 instead of +4. Could just be implemented by
#' adding a parameter to previous function)
#'
#' @param df dataframe with revisions, longformat
#' @param col_rel_date name of column with release dates (date format)
#' @param col_val_date name of column with observation dates (date format)
#' @param p_month number of months to be added to release until "final" revision 
#' 
#' @return filtered df only with first release + p_month
#' @export
#'
#' @examples
get_final_revision_series_seco <- function(df, col_rel_date, col_val_date) {
  df_adj <- df %>%
    group_by({{ col_val_date }}) %>%
    # collect first release date
    mutate(REL_Date_First = min({{ col_rel_date }})) %>% 
    # select final release for quarterly data
    filter( year({{ col_rel_date }}) == year(REL_Date_First) + 3 & 
              (month({{ col_rel_date }}) == 8 | month({{ col_rel_date }}) == 9)) %>%
    ungroup()
  
  df_adj <- df_adj %>%
    group_by({{ col_val_date }}) %>%
    arrange({{ col_rel_date }}) %>%
    slice(1)
  
  return(df_adj)
} 

#' Get final revision for production (different schedule of final revisions from GDP)
#'
#' @param df 
#' @param col_rel_date 
#' @param col_val_date 
#'
#' @return
#' @export
#'
#' @examples
get_final_revision_series_prod <- function(df, col_rel_date, col_val_date) {
  df_adj <- df %>%
    group_by({{ col_val_date }}) %>%
    # collect first release date
    mutate(REL_Date_First = min({{ col_rel_date }})) %>% 
    # select final release for quarterly data
    filter( year({{ col_rel_date }}) == year(REL_Date_First) + 1 & month({{ col_rel_date }}) == 5)  %>%
    ungroup()
  
  return(df_adj)
} 


#' Get latest vintage (alternative def of final revision for robustness checks)
#'
#' @param df 
#' @param col_rel_date 
#' @param col_val_date 
#'
#' @return
#' @export
#'
#' @examples
get_latest_vintage_series <- function(df, col_rel_date, col_val_date) {
  df_adj <- df %>%
    group_by({{ col_val_date }}) %>%
    filter({{ col_rel_date }} == max({{ col_rel_date }})) %>%
    ungroup()
  return(df_adj)
}



#' Calculate summary statistics
#' 
#' requires input to have initial and revision col
#' 
#' @param df_i 
#'
#' @return dataframe with 1 row and summary stats
#' @export
#'
#' @examples
create_summary_stats <- function(df_i) {
  noise <- (sd(df_i[["revision"]])**2)/(sd(df_i[["initial"]])**2)
  print("noise")
  print(sd(df_i[["revision"]])**2)
  print("signal")
  print(sd(df_i[["initial"]])**2)
  print("-----")
  
  corr <- cor(df_i[["initial"]], df_i[["revision"]])
  summary_df <- data.frame(
    NoObs = length(df_i[["revision"]]),
    Mean = mean(df_i[["revision"]]),
    Min = min(df_i[["revision"]]),
    Max = max(df_i[["revision"]]),
    SD = sd(df_i[["revision"]]),
    Noise = noise,
    Correlation = corr
  )
  return(summary_df)
}


#' Perform MincerZarnowitz test (find smallest i such that i-th release is efficient, i.e. 
#' the null "final release = i-th release" is not rejected at given significance level)
#'
#' @param df_full 
#' @param df_final 
#' @param significance 
#'
#' @return
#' @export
#'
#' @examples
MincerZarnowitz <- function(df_full, df_final, significance) {
  df_final <- df_final %>%
    rename(time="obsTime", value="final") %>% 
    select(time, value) %>%
    ts_long() %>%
    mutate(id = 'final')
  
  e = 0 
  p_value = 0
  all_releases <- data.frame(id = character(0), time = numeric(0), value = numeric(0))
  
  while (p_value <= significance) {
    
    e <- e+1
    
    revision <- get_revision(df_full, REL_Date, obsTime, e) %>%
      rename(time="obsTime", value="diff") %>% 
      select(time, value) %>%
      filter(time >= ymd("1995-04-01") & time <= ymd("2017-12-31")) %>%
      ts_long() %>%
      mutate(id = paste0("release_", e))
    
    all_releases <- ts_bind(all_releases, revision)
    
    df_all <- ts_bind(all_releases, df_final) %>%
      ts_wide()
    
    model_formula <- as.formula(paste("final ~ release_", e, sep = ""))
    model <- lm(model_formula, data = df_all)
    hac_se <- vcovHAC(model)
    print(summary(model))
    test <- linearHypothesis(model, c("(Intercept) = 0", paste0("release_", e, " = 1")), vcov = hac_se)
    print(test)
    p_value <- test[2, 'Pr(>F)']
  }
  df_output <- list('e' = e-1, 'coef' = model$coefficients, 'p_value' = p_value, 'releases' = df_all)
  return(df_output)
}


#' MLE of state space model - Kalman filter
#'
#' @param x 
#' @param y
#' @param m0
#' @param C0
#' @param initial_params
#'
#' @return series of squared prediction errors
#' @export
#'
#' @examples
KalmanNowcastPrecision <- function(x, y, m0, C0, initial_params) {
  # TODO: mention imprecision in paper (how mo and c0 were estimtaed)
  fn <- function(param) {
    dlm(FF = 1, V = exp(param[1]), GG = param[2], W= exp(param[3]), m0 = m0, C0 = C0)
  }
  # Initial parameter values 
  err <- numeric(length(y))
  for (i in 1:length(y)) {
    y_i <- y[1:i]
    fit <- dlmMLE(y_i, initial_params, build = fn)
    # Check convergence and log-likelihood
    print(paste("Convergence: ", fit$convergence))
    print(paste("Log-likelihood: ", fit$value))
    # Update state estimates
    x_hat <- dlmFilter(y_i, fn(fit$par))$m
    x_i_hat <- x_hat[length(x_hat)]
    # Update error calculation
    err[i] <- (x[i] - x_i_hat)^2
  }
  return(err)
}

### Load data -----------------------------------------------------------------

# load info on data
df_info <- read.csv(paste0(DATA_PATH, "info.csv"))
keys <- as.character(df_info$keys)
names(keys) <- df_info$indicators
print(keys)

# each of the indicators has these columns
COLS <- c("BBK_RTD_REL_YEAR",
          "BBK_RTD_REL_MONTH",  
          "BBK_RTD_REL_DAY",    
          "obsTime", 
          "obsValue")

# load gdp
df_gdp_chain <- loadPreprocessPipeline(paste0(DATA_PATH, keys["gdp_chain"], ".xml"), COLS, "P3M")
df_gdp_constant <- loadPreprocessPipeline(paste0(DATA_PATH, keys["gdp_constant"], ".xml"), COLS, "P3M")
df_gdp <- rbind(df_gdp_constant, df_gdp_chain)

# load private consumption
df_privcon_chain <- loadPreprocessPipeline(paste0(DATA_PATH, keys["private consumption_chain"], ".xml"), COLS, "P3M")
df_privcon_constant <- loadPreprocessPipeline(paste0(DATA_PATH, keys["private consumption_constant"], ".xml"), COLS, "P3M")
df_privcon <- rbind(df_privcon_constant, df_privcon_chain)

# load public consumption
df_pubcon_chain <- loadPreprocessPipeline(paste0(DATA_PATH, keys["public consumption_chain"], ".xml"), COLS, "P3M")
df_pubcon_constant <- loadPreprocessPipeline(paste0(DATA_PATH, keys["public consumption_constant"], ".xml"), COLS, "P3M")
df_pubcon <- rbind(df_pubcon_constant, df_pubcon_chain)

# load investment
df_inv_chain <- loadPreprocessPipeline(paste0(DATA_PATH, keys["investment_chain"], ".xml"), COLS, "P3M")
df_inv_constant <- loadPreprocessPipeline(paste0(DATA_PATH, keys["investment_constant"], ".xml"), COLS, "P3M")
df_inv <- rbind(df_inv_chain, df_inv_constant)

# load exports
df_exp_chain <- loadPreprocessPipeline(paste0(DATA_PATH, keys["exports_chain"], ".xml"), COLS, "P3M")
df_exp_constant <- loadPreprocessPipeline(paste0(DATA_PATH, keys["exports_constant"], ".xml"), COLS, "P3M")
df_exp <- rbind(df_exp_chain, df_exp_constant)

# load production
df_prod <- loadDataSDMXToDf(paste0(DATA_PATH, keys["production_constant"], ".xml"), COLS) %>%
  mutate(obsTime = ymd(paste0(obsTime, "-01"))) %>%
  arrange(REL_Date, obsTime) %>%
  # # if no group by, change of release date leads to enormous growth rate
  group_by(REL_Date) %>%
  mutate(diff = ((obsValue - lag(obsValue, 1)) / lag(obsValue, 1))*100) %>%
  filter(!is.na(diff)) %>%
  ungroup() %>%
  select(-c(BBK_RTD_REL_DAY, BBK_RTD_REL_MONTH, BBK_RTD_REL_YEAR))

# load SWISS gdp
df_gdp_seco <- read_xlsx(paste0(DATA_PATH, "realtime_database_gdp_seco.xlsx"),
                         sheet = "gdp",
                         col_names = FALSE) %>%
  # drop first 8 rows
  slice(10:n())

#colnames(df_gdp_seco) <-
col_names_temp = as.character(df_gdp_seco[1, ])
col_names_temp[1] <- "obsTime"
colnames(df_gdp_seco) <- col_names_temp

# slice first + second row, pivot longer
df_gdp_seco_final <- df_gdp_seco %>%
  slice(3:n()) %>%
  pivot_longer(cols = -obsTime, names_to = "REL_Date", values_to = "obsValue") %>%
  mutate(obsTime = as.Date(as.numeric(obsTime), origin = "1899-12-30")) %>%
  mutate(REL_Date = as.Date(as.numeric(REL_Date), origin = "1899-12-30")) %>%
  arrange(REL_Date)

df_gdp_seco_na_cleaned <- df_gdp_seco_final %>%
  group_by(REL_Date) %>%
  mutate(max_non_na_date = max(obsTime[!is.na(obsValue)])) %>%
  filter(obsTime <= max_non_na_date) %>%
  ungroup() %>%
  arrange(REL_Date) %>%
  select(-max_non_na_date) %>%
  filter(obsTime >= ymd("1995-01-01")) %>%
  mutate(obsValue = as.numeric(obsValue))

# todo_ rename again to df_gdp_seco
df_gdp_seco <- df_gdp_seco_na_cleaned %>%
  # copied from date prep from previously
  arrange(REL_Date, obsTime) %>%
  # if no group by, change of release date leads to enormous growth rate
  group_by(REL_Date) %>%
  mutate(diff = ((obsValue - lag(obsValue, 1)) / lag(obsValue, 1))*100) %>%
  filter(!is.na(diff)) %>%
  ungroup()


### plotting german gdp data-------------------------------------------------------

# plot 0 (replication of figure 1)
df_gdp_obs20050401 <- df_gdp %>% filter(obsTime == ymd("2005-04-01"))
df_gdp_obs20050401 %>%
  ggplot() + 
  geom_line(aes(x = REL_Date, y = diff))  +
  ggtitle("QoQ 2005Q2 GDP growth (for every release)")

df_gdp_obs20050401 <- df_gdp_seco %>% filter(obsTime == ymd("2005-04-01"))
df_gdp_obs20050401 %>%
  ggplot() + 
  geom_line(aes(x = REL_Date, y = diff))  +
  ggtitle("QoQ 2005Q2 GDP growth (for every release)")


# plot 1 takeaways:
# - there have probably been 4 benchmark revisions
# - from the legend can see how starting in 2020, more than 4 releases a year 
df_gdp_chain %>%
  ggplot() + 
  geom_line(aes(x = obsTime, y = obsValue, color = as.factor(REL_Date)))  +
  labs(color = "Release Dates") + 
  ggtitle("GDP in levels reveal benchmark revisions)")

# plot 2 takeaways:
df_gdp %>% 
  ggplot() + 
  geom_line(aes(x = obsTime, y = diff, color = as.factor(REL_Date)))  +
  labs(color = "Release Dates") + 
  ggtitle("GDP growth rate (for every release)")

# snapshot around covid:
df_gdp %>% 
  filter(obsTime < ymd("2022-12-31") & obsTime > ymd("2019-12-31")) %>%
  ggplot() + 
  geom_line(aes(x = obsTime, y = diff, color = as.factor(REL_Date)))  +
  labs(color = "Release Dates") + 
  ggtitle("GDP growth rate during Covid")


### Preparing realtime and revision series -------------------------------------

df_gdp_realtime <- get_realtime_series(df_gdp, REL_Date, obsTime) 
df_privcon_realtime <- get_realtime_series(df_privcon, REL_Date, obsTime) 
df_pubcon_realtime <- get_realtime_series(df_pubcon, REL_Date, obsTime) 
df_inv_realtime <- get_realtime_series(df_inv, REL_Date, obsTime)
df_exp_realtime <- get_realtime_series(df_exp, REL_Date, obsTime)
df_prod_realtime <- get_realtime_series(df_prod, REL_Date, obsTime)
df_gdp_seco_realtime <- get_realtime_series(df_gdp_seco, REL_Date, obsTime)


# see function description for definition of final revision for this call
df_gdp_finrev <- get_final_revision_series(df_gdp, REL_Date, obsTime)
df_privcon_finrev <- get_final_revision_series(df_privcon, REL_Date, obsTime)
df_pubcon_finrev <- get_final_revision_series(df_pubcon, REL_Date, obsTime)
df_inv_finrev <- get_final_revision_series(df_inv, REL_Date, obsTime)
df_exp_finrev <- get_final_revision_series(df_exp, REL_Date, obsTime)
df_prod_finrev <- get_final_revision_series_prod(df_prod, REL_Date, obsTime)
df_gdp_seco_finrev <- get_final_revision_series_seco(df_gdp_seco, REL_Date, obsTime)


# see function description for definition of "final 2" revision for this call
# df_gdp_finrev <- get_latest_vintage_series(df_gdp, REL_Date, obsTime)
# df_privcon_finrev <- get_latest_vintage_series(df_privcon, REL_Date, obsTime)
# df_pubcon_finrev <- get_latest_vintage_series(df_pubcon, REL_Date, obsTime)
# df_inv_finrev <- get_latest_vintage_series(df_inv, REL_Date, obsTime)
# df_exp_finrev <- get_latest_vintage_series(df_exp, REL_Date, obsTime)
# df_prod_finrev <- get_latest_vintage_series(df_prod, REL_Date, obsTime)
# df_gdp_seco_finrev <- get_latest_vintage_series(df_gdp_seco, REL_Date, obsTime)

# plot 3 - 6:
# - how to explain the strange spread decrease around 2014? benchmark revision?
# - does it even make sense to look at the level? or only look at growth?
df_merged <- merge(df_gdp_realtime, df_gdp_finrev, by = "obsTime") %>%
  mutate(levelSpread = obsValue.y - obsValue.x) %>%
  mutate(revision = diff.y - diff.x) # final - initial

df_merged  %>% 
  filter(obsTime >= ymd("1995-04-01") & obsTime <= ymd("2014-01-01")) %>%
  ggplot() + 
  geom_line(aes(x = obsTime, y = diff.x), color = "red")  +
  geom_line(aes(x = obsTime, y = diff.y), color = "blue")  +
  ggtitle("Realtime vs. final Release (growth rate)") 

df_merged %>% 
  ggplot() + 
  geom_line(aes(x = obsTime, y = revision))  +
  ggtitle("Revision (final vs initial spread")

# create data frames to feed to the Mincer-Zarnowitz regression function
scenario <- c("1995-04-01", "2014-01-01") # scenario from paper
# scenario <-c("1995-04-01", "2019-12-01") # scenario with max data

gdp_growth <- df_merged %>%
  select(c(obsTime, diff.x, diff.y, revision)) %>%
  rename(initial ="diff.x", final = "diff.y") %>%
  filter(obsTime >= ymd(scenario[1]) & obsTime < ymd(scenario[2])) 

privcon_growth <- merge(df_privcon_realtime, df_privcon_finrev, by = "obsTime") %>%
  mutate(revision = diff.y - diff.x) %>% # final - initial  
  select(c(obsTime, diff.x, diff.y, revision)) %>%
  rename(initial ="diff.x", final = "diff.y") %>%
  filter(obsTime >= ymd(scenario[1]) & obsTime < ymd(scenario[2])) 

pubcon_growth <- merge(df_pubcon_realtime, df_pubcon_finrev, by = "obsTime") %>%
  mutate(revision = diff.y - diff.x) %>% # final - initial  
  select(c(obsTime, diff.x, diff.y, revision)) %>%
  rename(initial ="diff.x", final = "diff.y") %>%
  filter(obsTime >= ymd(scenario[1]) & obsTime < ymd(scenario[2])) 

inv_growth <- merge(df_inv_realtime, df_inv_finrev, by = "obsTime") %>%
  mutate(revision = diff.y - diff.x) %>% # final - initial  
  select(c(obsTime, diff.x, diff.y, revision)) %>%
  rename(initial ="diff.x", final = "diff.y") %>%
  filter(obsTime >= ymd(scenario[1]) & obsTime < ymd(scenario[2])) 

exp_growth <- merge(df_exp_realtime, df_exp_finrev, by = "obsTime") %>%
  mutate(revision = diff.y - diff.x) %>% # final - initial  
  select(c(obsTime, diff.x, diff.y, revision)) %>%
  rename(initial ="diff.x", final = "diff.y") %>%
  filter(obsTime >= ymd(scenario[1]) & obsTime < ymd(scenario[2])) 

# production is different
scenario_prod <- c("1995-02-01", "2017-01-01")
# scenario_prod <- c("1995-02-01", "2022-12-01")
prod_growth <- merge(df_prod_realtime, df_prod_finrev, by = "obsTime") %>%
  mutate(revision = diff.y - diff.x) %>% # final - initial  
  select(c(obsTime, diff.x, diff.y, revision)) %>%
  rename(initial ="diff.x", final = "diff.y") %>%
  filter(obsTime >= ymd(scenario_prod[1]) & obsTime < ymd(scenario_prod[2])) # try to recreate sample in paper

# swiss gdp is also different
gdp_seco_growth <- merge(df_gdp_seco_realtime, df_gdp_seco_finrev, by = "obsTime") %>%
  mutate(revision = diff.y - diff.x) %>% # final - initial  
  select(c(obsTime, diff.x, diff.y, revision)) %>%
  rename(initial ="diff.x", final = "diff.y") %>%
  filter(obsTime >= ymd(scenario[1]) & obsTime < ymd(scenario[2])) 


# TABLE 2 Replication: Compute summary stats and properties of revision measure
res_gdp <- create_summary_stats(gdp_growth)
res_privcon <- create_summary_stats(privcon_growth)
res_pubcon <- create_summary_stats(pubcon_growth)
res_inv <- create_summary_stats(inv_growth)
res_exp <- create_summary_stats(exp_growth)
res_prod <- create_summary_stats(prod_growth)
res_gdp_seco <- create_summary_stats(gdp_seco_growth)

table2_replica <- rbind(res_gdp, res_privcon, res_pubcon, res_inv, res_exp, res_prod, res_gdp_seco)
indicators <- c("Real GDP", "Private consumption", "Public consumption", "Investment", "Exports", "Production", "Real GDP Seco")
table2_replica <- table2_replica %>%
  mutate(Variable = indicators) %>%
  select(c(Variable, everything())) %>%
  mutate_at(vars(-1,-2), list(~ round(., 2)))
  
# TABLE 3 Replication: Optimality tests through Mincer_zarnowitz regressions
MZ_gdp <- MincerZarnowitz(df_gdp, gdp_growth, 0.05)
MZ_privcon <- MincerZarnowitz(df_privcon, privcon_growth, 0.05)
MZ_pubcon <- MincerZarnowitz(df_pubcon, pubcon_growth, 0.05)
MZ_inv <- MincerZarnowitz(df_inv, inv_growth, 0.05)
MZ_exp <- MincerZarnowitz(df_exp, exp_growth, 0.05)
MZ_prod <- MincerZarnowitz(df_prod, prod_growth, 0.05)
MZ_gdp_seco <- MincerZarnowitz(df_gdp_seco, gdp_seco_growth, 0.05)

table3_replica <- data.frame(
  indicator = indicators,
  "No. of revisions" = c(MZ_gdp$e[[1]], MZ_privcon$e[[1]], MZ_pubcon$e[[1]], MZ_inv$e[[1]], MZ_exp$e[[1]], MZ_prod$e[[1]], MZ_gdp_seco$e[[1]]),
  "Unbiasedness alpha" = c(MZ_gdp$coef[[1]], MZ_privcon$coef[[1]], MZ_pubcon$coef[[1]], MZ_inv$coef[[1]], MZ_exp$coef[[1]], MZ_prod$coef[[1]],  MZ_gdp_seco$coef[[1]]),
  "Efficiency beta" = c(MZ_gdp$coef[[2]], MZ_privcon$coef[[2]], MZ_pubcon$coef[[2]], MZ_inv$coef[[2]], MZ_exp$coef[[2]], MZ_prod$coef[[2]], MZ_gdp_seco$coef[[2]]),
  "P_value" = c(MZ_gdp$p_value[[1]], MZ_privcon$p_value[[1]], MZ_pubcon$p_value[[1]], MZ_inv$p_value[[1]], MZ_exp$p_value[[1]], MZ_prod$p_value[[1]], MZ_gdp_seco$p_value[[1]])
) 

table3_replica$Unbiasedness.alpha <- round(table3_replica$Unbiasedness.alpha, 2)
table3_replica$Efficiency.beta <- round(table3_replica$Efficiency.beta, 2)
table3_replica$P_value <- round(table3_replica$P_value, 2)
xtable(table3_replica, caption = "Optimality test of releases: Mincer-Zarnowitz regressions", label = "tab:MZ")

# TABLE 4 Replication: nowcasting exercise with Kalman Filtering 

# testing for public consumption
y1 <- pubcon_growth %>%
  filter(obsTime > as.Date("2005-10-01")) %>%
  pull(initial)
x1 <- pubcon_growth %>%
  filter(obsTime > as.Date("2005-10-01")) %>%
  pull(final)
err_pubcon <- KalmanNowcastPrecision(x1, y1, 0, 10, c(-2, 1, -2))
prec_score <- mean(err_pubcon)/mean((x1-y1)**2)
print(prec_score)
ggplot() + 
  geom_point(aes(x = c(1:length(err_pubcon)), y = err_pubcon)) +
  geom_point(aes(x = c(1:length(err_pubcon)), y = (x1-y1)**2), color = 'red') +
  ggtitle("Naive (red) vs. Kalman (black) nowcast")


# testing for private consumption
y2 <- privcon_growth %>%
  filter(obsTime > as.Date("2005-10-01")) %>%
  pull(initial)
x2 <- privcon_growth %>%
  filter(obsTime > as.Date("2005-10-01")) %>%
  pull(final)
err_privcon <- KalmanNowcastPrecision(x2, y2, 0.2, 0.5, c(-2, 1, -2))
prec_score2 <- mean(err_privcon)/mean((x2-y2)**2)
print(prec_score2)
ggplot() + 
  geom_point(aes(x = c(1:length(err_privcon)), y = err_privcon)) +
  geom_point(aes(x = c(1:length(err_privcon)), y = (x2-y2)**2), color = 'red') +
  ggtitle("Naive (red) vs. Kalman (black) nowcast")

# testing for production
y3 <- prod_growth %>%
  filter(obsTime > as.Date("1999-12-31") & obsTime < as.Date("2017-01-01") ) %>%
  pull(initial)
x3 <- prod_growth %>%
  filter(obsTime > as.Date("1999-12-31") & obsTime < as.Date("2017-01-01") ) %>%
  pull(final)
err_prod <- KalmanNowcastPrecision(x3, y3, 0, 10, c(-2, 1, -2))
prec_score3 <- mean(err_prod)/mean((x3-y3)**2)
print(prec_score3)
ggplot() + 
  geom_point(aes(x = c(1:length(err_prod)), y = err_prod)) +
  geom_point(aes(x = c(1:length(err_prod)), y = (x3-y3)**2), color = 'red') +
  ggtitle("Naive (red) vs. Kalman (black) nowcast")
