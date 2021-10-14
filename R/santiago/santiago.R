#' ---
#' title: "Preprocessing of Santiago's travel dataset. Most of it comes from Lambed's code"
#' author: "Lambed and Daniel"
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: true
#' ---

#' Note: in order to create the markdown properly from this file, Rstudio options need to be changed. Tools > Global Options > R Markdown > Evaluate chunks in directory > Current. By default this option is set to *Document*, but here you need to change it to *Current* so everything works as expected.
#' 
#' 
#' # **Understanding phase**
#+ warning=FALSE, message=FALSE, echo=FALSE
# Loading libraries
#library(haven) 
library(kableExtra)
library(readxl)
library(tidyverse)
#library(nnet) # To use which.is.max function


#+ warning=FALSE, message=FALSE, echo=FALSE
# Cleaning workspace
rm(list = ls());gc()

# Printing options
options(scipen = 50)

#' This file is based on the script "travel_survey.R". It has the same code but
#' I added some comments.
#' 
#' 
#' ## Documentation 
#' Documentation is located in ".../Chile/Santiago/Trips/Reports/". I downloaded
#' these files from v drive and can be downloaded from http://www.sectra.gob.cl/encuestas_movilidad/encuestas_movilidad.htm
#'
#' From now on: 
#+ warning=FALSE, message=FALSE, echo=FALSE
data.frame(
  Reference = c("File1", "File2", "File3"),
  Description = c("Technical report and final results Vol. I",
                  "Technical report and final results Vol. II",
                  "Report with only final results"
                  ),
  Title = c("Encuesta Origen Destino Santiago 2021 Informe Final Volumen I",
            "Encuesta Origen Destino Santiago 2021 Informe Final Volumen II",
            "Informe ejecutivo Origen Destino de Viajes 2012 "
            ),
  File = c("Actualizacion_recolecc_STU_Santiago_IX Etapa_EOD Stgo 2012_Inf_Final vol I.pdf",
           "Actualizacion_recolecc_STU_Santiago_IX Etapa_EOD Stgo 2012_Inf_Final vol 2.pdf",
           "Actualizacion_recolecc_STU_Santiago_IX Etapa_EOD Stgo 2012_Inf_Ejec.pdf"
           )
) %>% kbl() %>% kable_classic()

#' ## Definition of a trip 
#' 1. *Trip:* All trips without any restriction (**File1** page 83)
#' 
#' 2. *Collection:* The surveys consists of two steps: 1) Collect household and
#'  people information. Here a specific day is assigned to record the trips 
#'  made. 2) After having assigned a day, people record trips made in that day
#'  (**File1** page 83 and Figura 42, page 89).
#'
#' ## Replicate main results from raw datasets
#' Loading standardize_modes function:
#+ warning=FALSE, message=FALSE
#' **Note: Before running this script, make sure this function is up to date**
standardize_modes <- function(trip, mode){
  # Read lookup table
  smodes <- read_csv('Data/Standardization/standardized_modes.csv')
  # Separate rows 
  smodes <- smodes %>% separate_rows(original, sep = ';')
  
  smodes <- smodes %>% 
    mutate(across(where(is.character), str_trim))
  
  if (length(mode) == 1) {
    if (mode == 'stage')
      trip$stage_mode <- smodes$exhaustive_list[match(trip$stage_mode, smodes$original)]
    else
      trip$trip_mode <- smodes$exhaustive_list[match(trip$trip_mode, smodes$original)]
  }else if (length(mode) == 2) {
    if (all(mode %in% c('stage', 'trip'))) {
      trip$trip_mode <- smodes$exhaustive_list[match(trip$trip_mode, smodes$original)]
      trip$stage_mode <- smodes$exhaustive_list[match(trip$stage_mode, smodes$original)]
    }
  }
  
  return(trip)
  
}

#' ### Importing datasets
#' In the access database there's a data dictionary.
#' I ran everything local because it is faster, but if someone wants to run this
#' script, then only the path needs to be changed.
#' I exported these excel files from the access database, because I couldn't 
#' read them directly from the database
# V-Drive folder
#path <- "V:/Studies/MOVED/HealthImpact/Data/Country/Chile/Travel Surveys/Santiago/"
# Local folder
path <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2021/Cambridge/Data/Chile/Santiago/Trips/"

#+ warning=FALSE, message=FALSE, cache=TRUE
# Households (hh)
hh <- read_excel(paste0(path, "Hogar.xlsx")) 

#+ warning=FALSE, message=FALSE, cache=TRUE
# People
people <- read_excel(paste0(path, "Persona.xlsx"))
#' It looks like the age of each person is in a different dataset
people_age <- read_excel(paste0(path, "EdadPersonas.xlsx"))
people <- people %>% inner_join(people_age, by = "Persona")

# Vehicles
#vehicles <- read_excel(paste0(path, "Vehiculo.xlsx"))

#+ warning=FALSE, message=FALSE, cache=TRUE
# Trips
trips <- read_excel(paste0(path, "Viaje.xlsx"))

#+ warning=FALSE, message=FALSE, cache=TRUE
# Stages: there's information about walk_to_pt stages
stages <- read_excel(paste0(path, "Etapa.xlsx"))


#' ### Number households and people
#' The first thing to do is verify that the number of hh and people is 
#' the same to what is mentioned in page 55 of **File2** (Tabla 17).
sum(hh$Factor)
sum(people$Factor) 
#' Results are the same.
#' 
#' ### Number of people by education
#' Compare this with what is mentioned in page 57 (Tabla 19) of **File2**. 
people %>% group_by(Estudios) %>% summarise(Total = sum(Factor))
#' Results are the same.
#' 
#' ### Number of hh and people by macrozone (sector)
#' Compare this with what is mentioned in page 60 (Tabla 22) of **File2**. 
data.frame(household = hh %>% 
             group_by(Sector) %>% summarise(Total = sum(Factor)),
           n_people =  people %>% 
             left_join(hh[, c("Hogar", "Sector")], by = "Hogar") %>% 
             group_by(Sector)  %>% summarise(Total = sum(Factor))
)
#' Results are the same
#' 
#' ### Mode share
#' Compare this with what is mentioned in page 76 (Tabla 34) of **File2**. 
#' The sampling weights in this survey are completely different from other
#' Chilean cities. In this case, there are weights for each kind of trip (e.g. 
#' weekday in regular season, weekend in regular season and same in summer).
#' Besides, the trips weights need to be multiplied by the weight at person
#' level. 
#sum(trips$FactorLaboralNormal, na.rm = T)
#sum(trips$FactorDomingoNormal, na.rm = T)
#sum(trips$FactorSabadoNormal, na.rm = T)
#sum(trips$FactorLaboralEstival, na.rm = T)
#sum(trips$FactorFindesemanaEstival, na.rm = T)
trips2 <- trips %>% left_join(people, by = c("Hogar", "Persona")) %>% 
  mutate(
    FactorLaboralNormal_new = FactorLaboralNormal * Factor_LaboralNormal,
    FactorSabadoNormal_new = FactorSabadoNormal * Factor_SábadoNormal,
    FactorDomingoNormal_new = FactorDomingoNormal * Factor_DomingoNormal,
    FactorLaboralEstival_new = FactorLaboralEstival * Factor_LaboralEstival,
    FactorFindesemanaEstival_new = FactorFindesemanaEstival *
      Factor_FindesemanaEstival)
sum(trips2$FactorLaboralNormal_new, na.rm = T) # Same total as in Table 34
sum(trips2$FactorSabadoNormal_new, na.rm = T) # Same total as in Table 94 (Pag 175 of File2)
sum(trips2$FactorDomingoNormal_new, na.rm = T) # Same total as in Table 109 (Pag 190  of File2)
sum(trips2$FactorLaboralEstival_new, na.rm = T) # Same total as in Table 124 (Pag 205 of File2)
sum(trips2$FactorFindesemanaEstival_new, na.rm = T) # Not published

# Now computing mode share
trips3 <- trips2 %>% # Join ViajesDifusion because it isn't in trips dataset
  left_join(read_excel(paste0(path, "ViajesDifusion.xlsx")), by = "Viaje")
trips3 %>% 
  group_by(ModoDifusion) %>% 
  summarise(Total = sum(FactorLaboralNormal_new, na.rm = T))
#' Results are the same. I need to filter only trips made in regular season
#' 
#' # **Preprocessing phase**
#' ## Filtering people from Santiago city
#' Since the survey was conducted in 45 comunas (page 15 of **File1**) and 
#' injuries information is at city level then I should filter only households
#' located within the city. To identify comunas that don't belong to Santiago
#' I took as reference "Tabla 2" page 15 of **File 1**.
people_v2 <- people %>% 
  left_join(hh[, c("Hogar", "Comuna")], by = "Hogar") %>% 
  filter(!Comuna %in% c("COLINA", "LAMPA", "SAN BERNARDO", "CALERA DE TANGO",
                        "BUIN", "PUENTE ALTO", "PIRQUE", "MELIPILLA",
                        "TALAGANTE", "EL MONTE", "ISLA DE MAIPO", 
                        "PADRE HURTADO", "PEÑAFLOR")) 

#' I just verify that there are no duplicates in people dataset
length(unique(people_v2$Persona)) == nrow(people_v2)

#' ## Classification and translation of trip modes
#' In the trip dataset there's already a classification of trip modes. It is
#' important to note that there are different variables that define trip mode, 
#' but none of them seems to be useful for the package, because it combines
#' modes such as motorcycle and metro in a single category. For this reason, I 
#' decided to use the classification used in stages dataset.To create the 
#' following table I used the information I found in the access database
#' (table Modo) and then defined a hierarchy to be applied. Finally, I
#' translated it. This is the result:
main_mode <- read_csv("Data/Standardization/Modes_by_city.csv") %>% 
  filter(City == "Santiago")
main_mode[,-c(1:2)] %>% kbl() %>% kable_classic()

#' Now with respect to trip purpose, there are two different classifications.
#' I decided to use *PropositoAgregado* because it has the categories we need.
#' This is the result:
purpose <- read_csv("Data/Standardization/Purpose_by_city.csv") %>% 
  filter(City == "Santiago")
purpose[,-c(1:2)] %>% kbl() %>% kable_classic()

#' The first two columns have been taken from the dataset and data dictionary,
#' and the third column is the translation and classification of these motives.
#' 
#' ## Information at stage or trip level?
#' There is information at stage level although it seems that is not enough 
#' because of the duration of each stage. The information available is about
#' minutes walking before taking other mode, but there's no
#' information about the time spent in each stage. 
#' 
#' **Since the proportion of trips with more than one stage is relatively**
#' **small, we could compute the difference between the trip duration and the**
#' **time spent walking to the PT, and then split this duration equally in the**
#' **remaining modes. In this way, we used all information provided for trips**
#' **with only one stage and have a rough estimate of duration for trips with**
#' **more than one stage.**
#' 
#' First I verify there are no duplicates in trips and stage datasets. It is
#' important to note that in this survey the IDs (participant, trip and stage)
#' are different to other surveys, because they already come as a concatenation
#' of IDs. 
trips <- trips %>% 
  mutate(trip_id_paste = Viaje)
length(unique(trips$trip_id_paste)) == nrow(trips) # OK

stages <- stages %>% mutate(stage_id_paste = Etapa)
length(unique(stages$stage_id_paste)) == nrow(stages) # OK

#' 15% of trips have more than 1 stage
n_stages <- stages %>% count(Hogar, Persona, Viaje)
table(n_stages$n, useNA = "always")
table(n_stages$n, useNA = "always") / nrow(n_stages)

#' ## Row for each trip, translate trip_mode and create duration, sex and age
#' Trip dataset already has a row for each trip, so I have to create the 
#' variables I need. 
#' 
#' To create trip_mode I have to extract first the modes used from variable
#' *MediosUsados*. This commented code allows me to do that, and it is used
#' when defining trips_v2.
#' 
#' Also it is important to note that I will only consider trips made in regular 
#' season and filter out trips made in summer. Therefore, I create a dummy
#' variable called "regularseason" to identify these trips.
# asdf <- map(trips$MediosUsados, function(x){
#   modes <- trimws(unlist(str_split(x, ";")))
#   hierarchy <- unlist(map(modes, function(y){
#     return(main_mode$Hierarchy[match(y, main_mode$Code)])
#   }))
#   minimum = min(hierarchy)
#   return(main_mode$ITHIM[match(minimum, main_mode$Hierarchy)])
# })

trips_v2 <- trips %>% 
  mutate(trip_id = trip_id_paste,
         trip_duration = TiempoViaje,
         #' To create trip_mode I have to extract first the modes used from
         #' variable *ModosUsados*. Map function is used to apply the function
         #' to each element of ModosUsados. 
         trip_mode = unlist(map(trips$MediosUsados, function(x){
           # trimws and str_split are used to extract each mode from ModosUsado
           modes <- trimws(unlist(str_split(x, ";")))
           # hierarchy defines the hierarchy of each mode
           hierarchy <- unlist(map(modes, function(y){
             return(main_mode$Hierarchy[match(y, main_mode$Code)])
           }))
           # Minimum defines the main mode
           minimum = min(hierarchy)
           return(main_mode$ITHIM[match(minimum, main_mode$Hierarchy)])
         })),
         # As I said before, I used PropositoEstraus because it has a
         # classification we can use
         trip_purpose = purpose$ITHIM[match(PropositoAgregado, purpose$Code)],
         regularseason = ifelse(!is.na(trips$FactorLaboralNormal) | 
                                  !is.na(trips$FactorSabadoNormal) | 
                                  !is.na(trips$FactorDomingoNormal), 1, 0))

#' Check purpose and trip mode
table(trips_v2$MediosUsados, trips_v2$trip_mode, useNA = "always")
table(trips_v2$PropositoAgregado, trips_v2$trip_purpose, useNA = "always")

#' Check that the number of trips in regular of season is ok
sum(!is.na(trips_v2$FactorLaboralNormal)) + 
  sum(!is.na(trips_v2$FactorSabadoNormal)) + 
  sum(!is.na(trips_v2$FactorDomingoNormal)) == sum(trips_v2$regularseason)

#' Now, each stage needs to be in a single row. The original stage dataset has
#' walking stages in the same row as the regular stages, so I need to create
#' one for each walking stage.
#' 
#' As I said before, most of the trips have only stage (without counting 
#' walking stages), and the processing is different when there's only one, than 
#' when there are more than one. So I have to merge trip information and then
#' create one dataset for trips with only one stage, and other dataset
#' where trips have two or more stages.
#' 
#' Before I do that, I need to translate stage mode and merge the information
#' about the number of stages per trip. Then I have to make sure that the total
#' walking duration is less that the total duration of the trip
names(stages)
sort(unique(stages$Modo))
stages_v2 <- stages %>% 
  mutate(household_id = Hogar,
         participant_id = Persona,
         trip_id = Viaje,
         stage_id = Etapa,
         trip_id_paste = Viaje,
         stage_mode = main_mode$ITHIM[match(Modo, main_mode$Code)]
  ) %>% 
  # Merge number of stages per trip
  left_join(n_stages, by = c("Hogar", "Persona", "Viaje")) %>% 
  # Merge trip duration information
  left_join(trips_v2[,c("trip_id_paste", "trip_duration", "trip_mode",
                        "trip_purpose", "regularseason")], by = "trip_id_paste")

#' In this survey, when trip_mode is "other", it means indeed that is other mode
#' such as "informal modes". For this reason, there's no need to replace "other"
#' modes as it is done in other Chilean surveys. I however rename stages dataset
#' so it is consistent with other scripts.
stages_v3 <- stages_v2
#table(stages_v3$trip_mode, stages_v3$stage_mode, useNA = "always")

#' Now I'm going to compute stage duration. The processing is different in trips
#' with only one main stage (i.e. without counting walking stages) and with more
#' than one. In the first case, I will subtract the walking duration from the 
#' trip duration and assign this value to the stage. In the second case, I will
#' subtract the walking duration from the trip duration and then this value will
#' be split equally in the number of stages so every stage will have the same 
#' duration. This is solution is proposed under the assumption that other stages
#' are not as important as walking stages, so the estimation of duration is not 
#' sensible for the analysis.
#'
#' **Trips with one stage:**
sum(is.na(stages_v3$MinutosAntes)) # Surprisingly there are NAs
# sum(is.na(stages_v3$MinutosCaminadosDespues)) # This variable doesn't exist

# Filling NAs with zeros
stages_v3 <- stages_v3 %>% 
  mutate(MinutosAntes = ifelse(is.na(MinutosAntes), 0, MinutosAntes))

stages_v3_1 <- stages_v3 %>% filter(n == 1) %>% 
  # Compute walking duration
  mutate(walking_duration = MinutosAntes,# + MinutosCaminadosDespues,
         # Create a variable to see which trips need adjustment because the 
         # walking duration is equal to or larger than trip duration. Important
         # to note that this applies to all trips but walking trips.
         need_adjustment = ifelse(trip_mode != "walk" &
                                    walking_duration >=  trip_duration, 1, 0),
         # Compute stage duration by subtracting walking stages
         stage_duration = ifelse(trip_mode != "walk",
                                 ifelse(need_adjustment == 0,
                                        trip_duration - walking_duration,
                                        trip_duration),
                                 trip_duration # Walking stages
         )
  )

#' In 1841 (2%) trips the walking duration is the same or larger than the trip
#' duration. Since this proportion is small, then I will assume that these
#' trips didn't have the walking component.
table(stages_v3_1$need_adjustment)
table(stages_v3_1$need_adjustment, useNA = "always") / nrow(stages_v3_1)

#' *Now, I have to create rows for the walking stages.*
#' 
#' In trips when the adjustment is needed or in walking trips, there will be
#' a single stage, meaning, no walking component. As a consequence, I leave this
#' trips as they are (won't add rows)
#table(stages_v3_1$trip_mode, stages_v3_1$stage_mode)
#names(stages_v3_1)
stages_v3_1_adjust <- stages_v3_1 %>% 
  filter(need_adjustment == 1 | trip_mode == "walk") %>% 
  mutate(stage_id_paste = as.character(stage_id_paste)) %>% 
  dplyr::select(household_id, participant_id, trip_id, trip_mode, trip_duration,
                trip_purpose, stage_id, stage_mode, stage_duration,
                stage_id_paste, trip_id_paste, regularseason)

#' In trips when NO adjustment is needed, there will be multiple stages because
#' of the walking stages. As a consequence, each trip will have at least two
#' rows: one for the main stage and one for the walking stage.
#names(stages_v3_1)
stages_v3_1_noadjust <- stages_v3_1 %>% 
  filter(need_adjustment == 0 & trip_mode != "walk") %>% 
  # I use pivot_longer to put walking stages in a new row
  pivot_longer(c("MinutosAntes", "stage_duration"), names_to = "variable",
               values_to = "stage_duration") %>% 
  # Assign "walk_to_pt" to walking stages
  mutate(stage_mode = ifelse(variable %in% c("MinutosAntes"), 
                             "walk_to_pt", stage_mode)) %>%
  # Filter out walking stages without duration
  filter(!is.na(stage_duration) & stage_duration > 0) %>% 
  # Reassign stage_id
  # from https://stackoverflow.com/questions/54581440/r-count-consecutive-occurrences-of-values-in-a-single-column-and-by-group
  group_by(household_id, participant_id, trip_id, 
           grp = with(rle(stage_id_paste), 
                      rep(seq_along(lengths), lengths))) %>%
  mutate(stage_id = seq_along(grp)) %>% 
  ungroup() %>% 
  # Recreate stage_id
  mutate(stage_id_paste = paste(household_id, participant_id, trip_id, 
                                stage_id, sep = "-")) %>% 
  dplyr::select(household_id, participant_id, trip_id, trip_mode, trip_duration,
                trip_purpose, stage_id, stage_mode, stage_duration,
                stage_id_paste, trip_id_paste, regularseason)

#length(unique(stages_v3_1_noadjust$stage_id_paste)) == nrow(stages_v3_1_noadjust) #OK
#sum(is.na(stages_v3_1_noadjust$stage_duration))

#' Append both datasets into one with trips with only one stage.
#' 
#' In this dataset *stage_id_paste* let me know the original stage_id so I can
#' trace back the original trip if I wanted to.
# names(stages_v3_1_adjust)
# names(stages_v3_1_noadjust)
stages_v3_1_ready <- stages_v3_1_adjust %>% bind_rows(stages_v3_1_noadjust)

# Same number of trips before and after creating walking stages
# length(unique(stages_v3_1$trip_id_paste)) ==
#   length(unique(stages_v3_1_ready$trip_id_paste)) # Not OK 
# sum(is.na(trips_v2$trip_duration))
#' In this survey, the number of trips is not the same because there are trips
#' without duration. This must be a mistake from the consultancy company.
#' 
#' **Trips with two or more stages:**
#' 
#' In this dataset *stage_id_paste* and *trip_id_paste* let me know the original
#' stage_id and trip_id so I can trace back the original trip if I wanted to.
stages_v3_2 <- stages_v3 %>% filter(n > 1) %>% 
  # I compute the walking duration at the beginning and end of a stage
  group_by(household_id, participant_id, trip_id) %>% 
  mutate(MinutosAntes_sum = sum(MinutosAntes, na.rm = T),
         # I compute total walking duration
         walking_duration = MinutosAntes_sum,
         # The remaining trip duration is split equally in the other modes
         stage_duration = (trip_duration - walking_duration) / n) %>% 
  ungroup() %>% 
  # I use pivot_longer to put walking stages in a new row
  pivot_longer(c("MinutosAntes", "stage_duration"), names_to = "variable",
               values_to = "stage_duration") %>% 
  # Assign "walk_to_pt" to walking stages
  mutate(stage_mode = ifelse(variable %in% c("MinutosAntes"), 
                             "walk_to_pt", stage_mode)) %>% 
  # Filter out walking stages without duration
  filter(!is.na(stage_duration) & stage_duration > 0) %>% 
  # Reassign stage_id
  # from https://stackoverflow.com/questions/54581440/r-count-consecutive-occurrences-of-values-in-a-single-column-and-by-group
  # group_by(id_hogar, id_persona, 
  #          grp = with(rle(id_viaje), rep(seq_along(lengths), lengths))) %>%
  group_by(trip_id_paste) %>%
  mutate(stage_id = seq_len(dplyr::n()),
         # Recreate stage_id_paste
         stage_id_paste = paste(household_id, participant_id, trip_id, 
                                stage_id, sep = "-")) %>% 
  ungroup() %>% 
  dplyr::select(household_id, participant_id, trip_id, trip_mode, trip_duration,
                trip_purpose, stage_id, stage_mode, stage_duration,
                stage_id_paste, trip_id_paste, regularseason)

#' **Append all stages:**
#' 
#' I merge all stages again to create a unique dataset and filter out trips that
#' are not in the regular season (i.e. trips in summer season)
stages_ready <- stages_v3_1_ready %>% 
  bind_rows(stages_v3_2) %>% 
  filter(regularseason == 1) %>% 
  arrange(household_id, participant_id, trip_id, stage_id) 

#' The number of trips is the same and there are more stages as a consequence of
#' creating the walking stages. 
# Number of trips before processing
length(unique(stages_v3$trip_id_paste)) 
# Number of trips after processing
length(unique(stages_v3_1_ready$trip_id_paste)) + 
  length(unique(stages_v3_2$trip_id_paste)) 
length(unique(stages_ready$trip_id_paste)) # Filter out summer trips

# Number of stages before processing
length(unique(stages_v3$stage_id_paste))
# Number of stages after processing
length(unique(stages_v3_1_ready$stage_id_paste)) + 
  length(unique(stages_v3_2$stage_id_paste)) 
length(unique(stages_ready$stage_id_paste)) # Filter out summer trips 

#' ## Create variables for quick report
#' I need to create some variables to run the report that Lambed developed in 
#' the function *quality_check*.
#' 
#' Since there are trips collected in weekends (as in mexico) I have to make 
#' sure that the IDs of these trips are different to avoid increasing the daily
#' number of trips per person. Now, given the fact that people was assigned 
#' a day before collecting the trips it's not possible that a same person has
#' trips in weekdays or in weekends, so there's no need to change any ID. This
#' is not the case in Mexico, that's why there I had to create new IDs.
report <- people_v2 %>%  # People from Santiago only
  left_join(stages_ready, 
            by = c("Hogar" = "household_id", 
                   "Persona" = "participant_id")) %>% 
  mutate(cluster_id = 1,
         household_id = Hogar,
         participant_id = Persona,
         age = Edad,
         sex = ifelse(Sexo == 1, "Male", "Female"),
         participant_wt = Factor,
         meta_data = NA) %>% 
  dplyr::select(cluster_id, household_id, participant_id, sex, age,
                participant_wt,
                trip_id, trip_mode, trip_duration, trip_purpose,
                stage_id, stage_mode, stage_duration, 
                stage_id_paste, trip_id_paste, meta_data) 

report$meta_data <- NA
report$meta_data[1] <- 7164400
report$meta_data[2] <- 23929 
report$meta_data[3] <- "Travel Survey"
report$meta_data[4] <- 2012
report$meta_data[5] <- "1 day"
report$meta_data[6] <- "Yes" #Stage level data available
report$meta_data[7] <- "All purpose"#Overall trip purpose
report$meta_data[8] <- "Yes" # Short walks to PT
report$meta_data[9] <- "Yes (unreliable)" # Distance available
report$meta_data[10] <- "N/A" # missing modes

#' I verify that every trip has the sex and age of the person who did it. Since
#' the sum of NAs is zero, then I can conclude that every trip has the
#' information.
sum(is.na(report$sex))
sum(is.na(report$age))
sum(is.na(report$trip_id))
sum(is.na(report$trip_duration))
sum(is.na(report$trip_mode))
sum(is.na(report$stage_id))
table(people$Viajes)
sum(table(people$NoViaja))

#' The number of NAs is not the same as it is reported in variables "Viajes" and
#' #NoViaja, but I think this is because the quality of the dataset is not good
#' at all (there were some consistency problems).
#' 
#' # **Exporting phase**
#' Export dataset to make the report
write_csv(report, 'Data/Report/santiago/arica_santiago.csv')

#' ## **Processing for ITHIM**
#' ### Standardize trip modes
#' There's already a function that standardize these modes so the package can
#' use these trips. I made sure to translate trip modes so that the function
#' works perfectly (take a look at the *original* variable of *smodes* dataframe
#' in this function).

trips_export <- standardize_modes(report, mode = c('trip', 'stage'))
table(report$trip_mode)
table(trips_export$trip_mode)

#' *standardize_modes* function converts walk to pedestrian, bicycle to cycle,
#' van to car, metro and train to rail.
#' 
#' ### Creating again IDs
trips_export <- trips_export %>% mutate(
  participant_id = as.integer(as.factor(paste(cluster_id, household_id,
                                              participant_id, sep = "_"))),
  trip_id = ifelse(is.na(trip_mode), NA,
                   as.integer(as.factor(paste(cluster_id, household_id,
                                        participant_id, trip_id, sep = "_")))))

#sapply(trips_export, function(x) sum(is.na(x)))

#' ### Variables to export
#' Now I filter the columns I need
trips_export <- trips_export %>% 
  dplyr::select(participant_id, age, sex, trip_id, trip_mode, trip_duration,
                stage_id, stage_mode, stage_duration)

#' ### Export dataset
write_csv(trips_export, 'Data/ITHIM/santiago/trips_santiago.csv')