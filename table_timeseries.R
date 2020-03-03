ts_func <- function(my_countries, my_year){
  IISS <- readRDS(file = paste0(here::here(), '/data/IISS.rds'))

  for(i in 1:length(my_countries)){
     my_sub <- IISS %>%
            dplyr::filter (country %in% my_countries[i] & year == my_year) %>%
            dplyr::select (year, equipment_type, unit_count) %>%
            dplyr::mutate(equipment_type = ifelse(is.na(equipment_type), "Unknown", equipment_type),
                          equipment_type = tolower(equipment_type),
                          equipment_type = tools::toTitleCase(equipment_type)) %>%
            dplyr::group_by(year, equipment_type) %>%
            dplyr::summarise(unit_count = sum(as.numeric(unit_count))) %>%
            tidyr::spread(year, unit_count, fill = 0) %>%
            tibble::column_to_rownames('equipment_type')

     print(my_sub)

     print(stargazer::stargazer(my_sub, summary = F, rownames = T, colnames = T,
          title = paste("Distribution of Military Capabilities:",tools::toTitleCase(my_countries[i]), min(my_year),"to",max(my_year)),
          notes = "Data taken from the International Institute for Strategic Studies (IISS) Military Balance",
          out = paste0(here::here(),'/paper/figures/capability_tables/',paste(my_countries[i],"_",min(my_year),"_",max(my_year),"capabilities.tex"))))
      }
      }








