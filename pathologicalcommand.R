library(tidyverse)
library(healthiar)


healthiar::attribute_health(
  approach_risk = "absolute_risk",
  bhd_central = 0.00163,
  #prop_pop_exp = 1200/246263,
  pop_exp = 1200,
  exp_central = 57,
  cutoff_central = 45,
  erf_eq_central = "exp(log(1.055)/10*(c-45))",
  geo_id_micro = "01002000",
  geo_id_macro = "01",
  duration_central = 1) %>% 
  .$health_detailed %>% 
  .$results_raw %>% 
  select(erf_eq,impact)

resEEA_ar %>% 
  group_by(outcome,source,metric,datenquelle) %>% 
  summarize(allimpact=sum(impact))

bla<-healthiar::attribute_health(
  approach_risk = "relative_risk",
  bhd_central = 0.00163,
  prop_pop_exp = 1200/246263,
  exp_central = 57,
  #cutoff_central = 45,
  erf_eq_central = "exp(log(1.055)/10*(c-45))",#(c-45)*1.073",#"ifelse(c>45,(c-45)*1.073,0)",
  geo_id_micro = "01002000",
  geo_id_macro = "01",
  duration_central = 1) %>% 
  .$health_detailed %>% 
  .$results_raw
bla

exp(log(1.055)/10*(65-45))

bla<-dat_exp_ERF %>% 
  filter(outcome=="Behavioural problems",
         source=="rail",
         metric=="lden")%>%
  filter(kartierungsumfang=="all",
         datenquelle=="EEA")%>% 
  first() %>% 
  {
    healthiar::attribute_health(
      approach_risk = "relative_risk",
      bhd_central = first(.$bhd),
      prop_pop_exp = .$exponierte/.$bevoelkerung,
      exp_central = .$l_zentral,
      cutoff_central = first(.$threshold),
      erf_eq_central = first(.$ERF),
      geo_id_micro = .$gemeinde_kennziffer,
      geo_id_macro = .$bundesland_code,
      duration_central = 1,
      info = select(., source, metric, outcome, datenquelle, kartierungsumfang)
    )
  } %>% 
  .$health_detailed %>% 
  .$results_raw


exdat_noise

blubb<-exdat_noise |>
  (\(df) {
    healthiar::attribute_health(
      approach_risk = df$risk_estimate_type,
      exp_central = df$exposure_mean,
      pop_exp = df$exposed,
      erf_eq_central = df$erf
    )$health_detailed$results_raw
  })()
