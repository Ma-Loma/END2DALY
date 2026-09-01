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


haind <- function (Lden) {
  1 - pnorm((72 - (-126.52 + Lden * 2.49)) / sqrt(2054.43))
}


data.frame(Lden=seq(40,75,1)) |> 
  mutate(HA=haind(Lden)) |> 
  ggplot(aes(x=Lden,y=HA))+
  geom_point()+geom_line()

hsdind <- function(lnight) {
  1-pnorm((72-(-90.70+(lnight)*(1.80)))/sqrt(1789+272))  
}
data.frame(lnight=seq(40,75,1)) |> 
  mutate(HSD=hsdind(lnight)) |> 
  ggplot(aes(x=lnight,y=HSD))+
  geom_point()+geom_line()
