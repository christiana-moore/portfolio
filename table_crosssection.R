cs_func <- function(my_countries, my_years){
    IISS <- readRDS(file = paste0(here::here(), '/data/IISS.rds'))

    for(i in 1:length(my_years)){
      my_sub <- IISS %>%
          dplyr::filter (year %in% my_years[i] & country == my_countries) %>%
          dplyr::select (country, equipment_type, unit_count) %>%
          dplyr::mutate(equipment_type = tolower(equipment_type),
                      equipment_type = tools::toTitleCase(equipment_type),
                      country = tools::toTitleCase(country),
                      equipment_type = ifelse(is.na(equipment_type), "Unknown", equipment_type)) %>%
          dplyr::group_by(country, equipment_type) %>%
          dplyr::summarise(unit_count = sum(as.numeric(unit_count))) %>%
          tidyr::spread(equipment_type, unit_count, fill = 0) %>%
          tibble::column_to_rownames('country')

      print(my_sub)

      print(stargazer::stargazer(my_sub, summary = F, rownames = T, colnames = T,
            title = paste("Distribution of Military Capabilities:", (my_years[i])),
            notes = "Data taken from the International Institute for Strategic Studies (IISS) Military Balance",
            out = paste0(here::here(),'/paper/figures/capability_tables/',paste0(my_years[i],"capabilities.tex"))))

        }
        }
cs_func(my_countries, my_years)


