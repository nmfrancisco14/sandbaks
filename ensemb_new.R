ensemble_sua2030<- 
  ensemb_forecast2030 |>
  filter(year<=2030) |> 
  mutate(area = production/yield) |> 
  rename(
    Production = production,
    Area_Harvested = area,
    Yield = yield
  ) |> 
  mutate(
    eco = "alleco",
    sem = "annual",
    location = clean_location(location)
  ) |> 
  left_join(Pop_1625c |> 
              select(-type) |> 
              mutate(year = as.numeric(year))
  ) |>
  left_join(foodcon2023) |> 
  bind_rows(  ensemb_forecast2030 |>
                filter(year<=2030) |> 
                filter(loc2=="vis") |> 
                group_by(year) |> 
                summarise(production = sum(production,na.rm = T),
                          yield = mean(yield,na.rm=T)
                ) |> 
                mutate(area = production/yield,
                       location = "Visayas",
                       eco = "alleco",
                       sem = "annual") |> 
                rename(
                  Production = production,
                  Area_Harvested = area,
                  Yield = yield
                ) |> 
                left_join(Pop_1625c |> 
                            filter(location %in% clean_location(visayas_prov)) |> 
                            group_by(year) |> 
                            summarise(population = sum(population,na.rm = T)) |> 
                            mutate(location="Visayas",
                                   year = as.numeric(year))
                ) |> 
                left_join(foodcon2023 |> 
                            filter(location %in% clean_location(visayas_prov)) |> 
                            summarise(foodcon = sum(foodcon,na.rm = T)) |> 
                            mutate(location="Visayas")
                          ) |> 
                mutate(loc2="vis")
  ) |> 
  mutate(location = if_else(location == "philippines", "Philippines",location),
         year = factor(year),
         aProduction = Production,
         Production_rice = aProduction*0.63,
         Seeds =(Area_Harvested*49.05)/1000, #seed use per ha
         Processing = Production_rice*.04, #processing constant
         Feeds_waste= Production_rice*.065, # feeds and waste constant
         Food = case_when(year==2024 & location =="Philippines"~Production_rice+2027180+4797762.599-2155924.7- Seeds-Processing-Feeds_waste,
                          .default =(population *122.7)/1000),
         Food_req = Food/365, #constant days in a year
         buffer = Food_req*60 ,#60 days or 2 months buffer
         totEndingStock = if_else(year==2024 & location =="Philippines",2155924.7,buffer)) |> 
  group_by(type,location) %>% 
  mutate(
    begStock = case_when(
      year==2024 & location =="Philippines"~2027180,
      year==2024~(foodcon/365)*60,
      year==first(year)~(foodcon/365)*60,
      .default = lag(totEndingStock,1)
    )) %>% 
  rowwise() %>% 
  mutate(
    #beg inventory 2023 pop and 2023 annual per cap
    `Imports` = if_else(year==2024 & location =="Philippines",4797762.599,
                        sum(c(Food,Seeds,Processing,Feeds_waste,totEndingStock),na.rm=TRUE) -Production_rice-begStock
    ),
    `SUPPLY` = NA_real_ ,
    `UTILIZATION` = NA_real_,
    SuffRatio = (Production_rice/(sum(c(Food,Seeds,Processing,Feeds_waste,totEndingStock),
                                      na.rm=TRUE)-begStock))*100
  ) %>% 
  select(-foodcon)


saveRDS(ensemble_sua2030,"ensemble_sua2030.rds")
