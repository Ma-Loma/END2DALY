allExpo_w |>
  filter(quelle == "Straße") |>
  filter(kartierungsumfang == "all") |>
  filter(datenquelle == "EEA") |>
  summarise(
   # n = n(),
    exponierte = sum(exponierte),
    #.by=) %>%
    .by = c(#gemeinde_bezeichnung,
            metrik,
            l_untergrenze)
  )|>
  pivot_wider(names_from = l_untergrenze,
              values_from = exponierte)|>
  write_tsv("test.csv")


datBundesland_major %>% 
  summarise(n=n(),
            #.by=gemeinde_bezeichnung) %>%
            .by=gemeinde_kennziffer)


bla<-datBundesland_major|>
  filter(gemeinde_bezeichnung=="Erzhausen")|>
  filter(source=="road")
bla %>% 
  summarise(n=n(),
            .by=c(source,outcome,risk_type))

datBundesland_major|>
  filter(gemeinde_kennziffer=="06431001") |>
  summarise(n=n(),
          .by=c(source,outcome))

dat_exp_ERF  %>% 
  filter(gemeinde_bezeichnung=="Frankfurt am Main, Stadt") %>% 
  filter(metric=="lden") %>% 
  filter(source=="road") %>% 
  summarise(n=n(),expon=sum(exponierte),
             .by=c(metric,source,kartierungsumfang,datenquelle,outcome))



data %>% 
  filter(country == "Germany") %>% 
  filter(name_stadt_gemeinde=="all") %>% 
  summarise(sum_exposed=sum(exponierte),
            .by = c(metric, noise_source,agglomeration, mapping_extend, data_source,name_stadt_gemeinde))


datBundesland |>
  filter(risk_type == "absolute_risk") |>
  filter(
    noise_source == "air",
    metric == "lden",
    outcome == "High noise annoyance",
    data_source == "Bundesland",
    mapping_extend == "major sources",
    is.na(agglomeration)
  )


ger_data |> 
  filter(is.na(gemeinde_kennziffer)) |> 
  select(name_stadt_gemeinde) |> 
  unique()
  
  #  summarise(n=n(),
            .by = c(metric,
                    agglomeration,
                    mapping_extend,
                    noise_source))
