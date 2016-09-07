library(ggthemes); library(stringr);library(reshape2);library(rstan); library(dplyr); library(ggplot2); library(Quandl);

options(mc.cores = parallel::detectCores())

#Clear Workspace
rm(list = ls())

#Rtools incompatibility fix
"3.3" = list(
  version_min = "3.2.0",
  version_max = "3.2.99",
  path = c("bin", "gcc-4.6.3/bin")
)

Quandl.api_key("ZZZZZZZZZ")

#real gdp millions chained dollars - seasonally adj
GDP <- Quandl("AUSBS/5206002_EXPENDITURE_VOLUME_MEASURES_A2304402X") %>% 
  mutate(Date = as.Date(Date)) %>% 
  arrange(Date) %>% 
  mutate(LGDP = 100*log(Value)) %>%
  mutate(Yg = (LGDP-lag(LGDP, 1))) %>%
  dplyr::select(Date, LGDP, Yg)

#Consumer Price Index, excl. exports - qtly
CPI <- Quandl("AUSBS/640101_A2325846C") %>% 
  mutate(Date = as.Date(Date)) %>% 
  arrange(Date) %>% 
  mutate(PIE = 100*log(Value/lag(Value, 1))) %>%
  mutate(PIE = replace(PIE, Date=="2000-09-30", 0.8)) %>% #Remove GST effect
  dplyr::select(Date, PIE)

#unemployment rate - monthly seasonally adj
UNR <- Quandl("AUSBS/6202001_A84423050A") %>%
  mutate(Date = as.Date(Date)) %>% 
  arrange(Date) %>% 
  mutate(UNR_chg = (Value-lag(Value, 12))) %>%
  mutate(UNR = (Value+lag(Value,1)+lag(Value,2))/3) %>%
  dplyr::select(Date, UNR)
  
  # Join series together at Quarterly Frequency
  full_data <- list(GDP,CPI,UNR) %>%
    Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by="Date"), .)  
  
  # Join series together at Quarterly Frequency
  full_data_short <- full_data[76:227,]
  
  # Standardize columns (without -999s) and replace -999s
  full_data_tmp <- full_data[,-1]
  full_data_tmp <- scale(full_data_tmp)[,1:ncol(full_data_tmp)]
  full_data_tmp[is.na(full_data_tmp)] <- -999
  
  # Now run the model
  mvf_model <- stan(file = "~/MVFStan/MVF_Stan.stan", 
                    data = list(T = nrow(full_data_short),
                                P = ncol(full_data_short[,-1]),
                                Y = full_data_tmp,
                                growth_ss=0.9, 
                                unr_ss=5.25),
                                chains = 4)
  
  summarised_state <- as.data.frame(mvf_model) %>% 
    select(contains("YGAP")) %>%
    melt() %>% 
    group_by(variable) %>% 
    summarise(median = median(value),
              lower = quantile(value, 0.025),
              upper = quantile(value, 0.975))
