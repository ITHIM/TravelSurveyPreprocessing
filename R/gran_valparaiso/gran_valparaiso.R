#' ---
#' title: "Preprocessing of Greater Valparaiso's travel dataset"
#' author: "Daniel"
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
library(kableExtra)
library(readxl)
library(tidyverse)


#+ warning=FALSE, message=FALSE, echo=FALSE
# Cleaning workspace
rm(list = ls());gc()

# Printing options
options(scipen = 50)

#' ## Documentation
#' These files are available in the v-drive in the path "V:/Studies/MOVED/HealthImpact/Data/Country/Chile/Travel Surveys/GranValparaiso/". Locally, this documentation is located in ".../Chile/GranValparaiso/Trips/Reports".
#' These files were found in: http://www.sectra.gob.cl/encuestas_movilidad/encuestas_movilidad.htm
#'
#' From now on: 
#+ warning=FALSE, message=FALSE, echo=FALSE
data.frame(
  Reference = c("File1", "File2"),
  Description = c("Final report Vol. 1",
                  "Final report Vol. 2"),
  Title = c("ACTUALIZACIÓN DIAGNÓSTICO DEL S.T.U. DEL GRAN VALPARAÍSO, ETAPA I - VOLUMEN 1",
            "ACTUALIZACIÓN DIAGNÓSTICO DEL S.T.U. DEL GRAN VALPARAÍSO, ETAPA I - VOLUMEN 2"),
  File = c("Informe final GV_Volumen 1.pdf",
           "Informe final GV_Volumen 2.pdf")
) %>% kbl() %>% kable_classic()

#' ## Definition of a trip (pendiente)
#' 1. *Trip:* All trips without any restriction (**File1** page 13-109)
#' 
#' 2. *Collection:* Some people were asked about trips made in a day during the
#' week, other about trips made on Saturdays and other about trips made on
#' Sundays.(Page 13-111 of **File1**)
#' 
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
#' In page 7-37 of **File1** there's a data dictionary.
#' I ran everything local because it is faster, but if someone wants to run this
#' script, then only the path needs to be changed.
#' I exported these excel files from the access database, because I couldn't 
#' read them directly from the database
# V-Drive folder
#path <- "V:/Studies/MOVED/HealthImpact/Data/Country/Chile/Travel Surveys/GranValparaiso/"
# Local folder
#' ### Importing datasets
path <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2021/Cambridge/Data/Chile/GranValparaiso/Trips/"

#+ warning=FALSE, message=FALSE, cache=TRUE
# Households (hh)
hh <- read_excel(paste0(path, "Hogar.xlsx")) 

#+ warning=FALSE, message=FALSE, cache=TRUE
# People
people <- read_excel(paste0(path, "Persona.xlsx"))

#+ warning=FALSE, message=FALSE, cache=TRUE
# Trips
trips <- read_excel(paste0(path, "Viaje.xlsx"))

#+ warning=FALSE, message=FALSE, cache=TRUE
# Stages: there's information about walk_to_pt stages
stages <- read_excel(paste0(path, "Etapa.xlsx"))


#' ### Number households and people
#' The first thing to do is verify that the number of hh and people is 
#' the same to what is mentioned in page 17-11 (Cuadro 17.8) of **File2**.
sum(hh$Factor)
sum(people$Factor)
#' Results are the same.
#' 
#' ### Number of people by education
#' Compare this with what is mentioned in page 17-12 (Cuadro 17.9) of **File1** 
names(people)
people %>% 
  group_by(Estudios) %>% summarise(Total = sum(Factor))
#' Results are the same.
#' 
#' ### Number of hh and people by macrozone
#' Compare this with what is mentioned in page 17-15 (Cuadro 17.12) of 
#' **File1**. 
data.frame(household = hh %>% 
             group_by(Macrozona) %>% summarise(Total = sum(Factor)),
           n_people =  people %>% 
             left_join(hh[, c("Hogar", "Macrozona")], by = "Hogar") %>% 
             group_by(Macrozona)  %>% summarise(Total = sum(Factor))
)

#' ### Number of hh by macrozona
#' Compare this with what is mentioned in page 17-15 (Cuadro 17.12) of **File2**.
data.frame(household = hh %>%
             group_by(Macrozona) %>% summarise(Total = round(sum(Factor), 0)),
           n_people =  people %>%
             left_join(hh[, c("Hogar", "Macrozona")], by = "Hogar") %>%
             group_by(Macrozona)  %>% summarise(Total = round(sum(Factor), 0))
)
#' Results are the same.
#' 
#' ### Mode share
#' Compare this with what is mentioned in page 17-44 (Cuadro 17.26) of 
#' **File1**.
sum(trips$FactorLaboral, na.rm = T)
trips %>% group_by(ModoViaje) %>% 
  summarise(viaja = sum(FactorLaboral, na.rm = T))

#' Results are the same. There's no need to filter any trip here.
#' 
#' # **Preprocessing phase**
#' ## Filtering people from Greater Valparaiso
#' Since the survey was conducted in greater valparaiso (page 2-1 of **File1**)
#' and there's information in this same coverage about injuries, then I won't
#' filter any trip. 
#' 
#' Note: According to page 2-1 of **File1** the survey covered comunas
#' Valparaiso, Viña del Mar, Concon, Quilpe and Villa Alemana.
#' 
#' I just verify that there are no duplicates in people dataset
people <- people %>% 
  mutate(participant_id_paste = paste(Hogar, Persona, sep = "-"))
length(unique(people$participant_id_paste)) == nrow(people)

#' ## Classification and translation of trip modes
#' In the trip dataset there's already a classification of trip modes. It is
#' important to note that there are different variables that define trip mode, 
#' but none of them seems to be useful for the package, because it combines
#' modes such as motorcycle and metro in a single category. For this reason, I 
#' decided to use the classification used in stages dataset.To create the 
#' following table I used the information I found in the access database
#' (table ModoEtapa) and then defined a hierarchy to be applied. Finally, I
#' translated it. This is the result:
main_mode <- read_csv("Data/Standardization/Modes_by_city.csv") %>% 
  filter(City == "GranValparaiso")
main_mode[,-c(1:2)] %>% kbl() %>% kable_classic()

#' Now with respect to trip purpose, there are two different classifications.
#' I decided to use *PropositoEstraus* because it has the categories we need.
#' This is the result:
purpose <- read_csv("Data/Standardization/Purpose_by_city.csv") %>% 
  filter(City == "GranValparaiso")
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
#' First I verify there are no duplicates in trips and stage datasets
trips <- trips %>% 
  mutate(trip_id_paste = paste(Hogar, Persona, Viaje, sep = "-"))
length(unique(trips$trip_id_paste)) == nrow(trips) # OK

stages <- stages %>% 
  mutate(stage_id_paste = paste(Hogar, Persona, Viaje, 
                                Etapa, sep = "-"))
length(unique(stages$stage_id_paste)) == nrow(stages) # OK

#'Less than 5% of trips have more than 1 stage
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
# asdf <- map(trips$ModosUsados, function(x){
#   modes <- unlist(str_split(x, ";"))
#   hierarchy <- unlist(map(modes, function(y){
#     return(main_mode$Hierarchy[match(y, main_mode$Code)])
#   }))
#   minimum = min(hierarchy)
#   return(main_mode$ITHIM[match(minimum, main_mode$Hierarchy)])
# })

trips_v2 <- trips %>% 
  mutate(trip_id = trip_id_paste,
         trip_duration = (as.numeric(format(TiempoViaje, "%H")) * 60) +
           (as.numeric(format(TiempoViaje, "%M"))),
         #' To create trip_mode I have to extract first the modes used from
         #' variable *ModosUsados*. Map function is used to apply the function
         #' to each element of ModosUsados. 
         trip_mode = unlist(map(trips$ModosUsados, function(x){
           # str_split is used to extract each mode from ModosUsados
           modes <- unlist(str_split(x, ";"))
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
         trip_purpose = purpose$ITHIM[match(PropositoEstraus, purpose$Code)])

#' Check purpose and trip mode
table(trips_v2$ModosUsados, trips_v2$trip_mode, useNA = "always")
table(trips_v2$PropositoEstraus, trips_v2$trip_purpose, useNA = "always")

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
sort(unique(stages$ModoEtapa))
stages_v2 <- stages %>% 
  mutate(household_id = Hogar,
         participant_id = Persona,
         trip_id = Viaje,
         stage_id = Etapa,
         trip_id_paste = paste(Hogar, Persona, Viaje, sep = "-"),
         stage_mode = main_mode$ITHIM[match(ModoEtapa, main_mode$Code)]
  ) %>% 
  # Merge number of stages per trip
  left_join(n_stages, by = c("Hogar", "Persona", "Viaje")) %>% 
  # Merge trip duration information
  left_join(trips_v2[,c("trip_id_paste", "trip_duration", "trip_mode",
                        "trip_purpose")], by = "trip_id_paste")

#' In this survey, when trip_mode is "other", it means indeed that is other mode
#' such as "informal modes" or "elevator". For this reason, there's no need to
#' replace "other" modes as it is done in other Chilean surveys. I however
#' rename stages dataset so it is consistent with other scripts.
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
sum(is.na(stages_v3$MinutosAntes))
sum(is.na(stages_v3$MinutosDespues))
stages_v3_1 <- stages_v3 %>% filter(n == 1) %>% 
  # Compute walking duration
  mutate(walking_duration = MinutosAntes + MinutosDespues,
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

#' Only in 217 trips the walking duration is the same or larger than the trip
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
  dplyr::select(household_id, participant_id, trip_id, trip_mode, trip_duration,
                trip_purpose, stage_id, stage_mode, stage_duration,
                stage_id_paste, trip_id_paste)

#' In trips when NO adjustment is needed, there will be multiple stages because
#' of the walking stages. As a consequence, each trip will have at least two
#' rows: one for the main stage and one for the walking stage.
#names(stages_v3_1)
stages_v3_1_noadjust <- stages_v3_1 %>% 
  filter(need_adjustment == 0 & trip_mode != "walk") %>% 
  # I use pivot_longer to put walking stages in a new row
  pivot_longer(c("MinutosAntes", "stage_duration",
                 "MinutosDespues"), names_to = "variable",
               values_to = "stage_duration") %>% 
  # Assign "walk_to_pt" to walking stages
  mutate(stage_mode = ifelse(variable %in% c("MinutosAntes",
                                             "MinutosDespues"), 
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
                stage_id_paste, trip_id_paste)

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
#   length(unique(stages_v3_1_ready$trip_id_paste)) ## ok

#' **Trips with two or more stages:**
#' 
#' In this dataset *stage_id_paste* and *trip_id_paste* let me know the original
#' stage_id and trip_id so I can trace back the original trip if I wanted to.
stages_v3_2 <- stages_v3 %>% filter(n > 1) %>% 
  mutate(
    # To avoid double counting the end walking component of each stage, 
    # I set it up to zero. This happens because walking minutes are asked before
    # and after the stage, so when there are two stages or more, the after-stage
    # walking duration is the same as the before-stage walking duration of the
    # next stage.
    MinutosDespues_ok = ifelse(stage_id != n, 0, MinutosDespues)) %>% 
  # I compute the walking duration at the beginning and end of a stage
  group_by(household_id, participant_id, trip_id) %>% 
  mutate(MinutosAntes_sum = sum(MinutosAntes, na.rm = T),
         MinutosDespues_sum = sum(MinutosDespues_ok, na.rm = T),
         # I compute total walking duration
         walking_duration = MinutosAntes_sum + MinutosDespues_sum,
         # The remaining trip duration is split equally in the other modes
         stage_duration = (trip_duration - walking_duration) / n) %>% 
  ungroup() %>% 
  # I use pivot_longer to put walking stages in a new row
  pivot_longer(c("MinutosAntes", "stage_duration",
                 "MinutosDespues_ok"), names_to = "variable",
               values_to = "stage_duration") %>% 
  # Assign "walk_to_pt" to walking stages
  mutate(stage_mode = ifelse(variable %in% c("MinutosAntes",
                                             "MinutosDespues_ok"), 
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
                stage_id_paste, trip_id_paste)

#' **Append all stages:**
#' 
#' I merge all stages again to create a unique dataset
stages_ready <- stages_v3_1_ready %>% 
  bind_rows(stages_v3_2) %>% 
  arrange(household_id, participant_id, trip_id, stage_id) 

#' The number of trips is the same and there are more stages as a consequence of
#' creating the walking stages. 
# Number of trips before processing
length(unique(stages_v3$trip_id_paste)) 
# Number of trips after processing
length(unique(stages_v3_1_ready$trip_id_paste)) + 
  length(unique(stages_v3_2$trip_id_paste)) 
length(unique(stages_ready$trip_id_paste)) 

# Number of stages before processing
length(unique(stages_v3$stage_id_paste))
# Number of stages after processing
length(unique(stages_v3_1_ready$stage_id_paste)) + 
  length(unique(stages_v3_2$stage_id_paste)) 
length(unique(stages_ready$stage_id_paste)) 

#' ## Create variables for quick report
#' I need to create some variables to run the report that Lambed developed in 
#' the function *quality_check*.
report <- people %>% 
  left_join(stages_ready, 
            by = c("Hogar" = "household_id", 
                   "Persona" = "participant_id")) %>% 
  mutate(cluster_id = 1,
         household_id = Hogar,
         participant_id = Persona,
         age = 2014 - AnoNac, # Survey took place in 2014
         sex = ifelse(Sexo == 1, "Male", "Female"),
         participant_wt = Factor,
         meta_data = NA) %>% 
  dplyr::select(cluster_id, household_id, participant_id, sex, age,
                participant_wt,
                trip_id, trip_mode, trip_duration, trip_purpose,
                stage_id, stage_mode, stage_duration, 
                stage_id_paste, trip_id_paste, meta_data) 

report$meta_data[1] <- 979127 # Population in 2012
report$meta_data[2] <- 999999 
report$meta_data[3] <- "Travel Survey"
report$meta_data[4] <- 2014
report$meta_data[5] <- "1 day"
report$meta_data[6] <- "Yes" #Stage level data available
report$meta_data[7] <- "All purpose"#Overall trip purpose
report$meta_data[8] <- "Yes" # Short walks to PT
report$meta_data[9] <- "No" # Distance available
report$meta_data[10] <- "" # missing modes 


#' I verify that every trip has the sex and age of the person who did it. Since
#' the sum of NAs is zero, then I can conclude that every trip has the
#' information.
sum(is.na(report$sex))
sum(is.na(report$age))
sum(is.na(report$trip_id))
sum(is.na(report$trip_duration))
sum(is.na(report$trip_mode))
sum(is.na(report$stage_id))
sum(table(people$NoViaja))

#' There are some trips without duration from the beginning, this is why there 
#' are more NAs here. This corresponds to variable "NoViaja" which indicates, 
#' people without trips
#' 
#' # **Exporting phase**
#' Export dataset to make the report
write_csv(report, 'Data/Report/gran_valparaiso/gran_valparaiso_trips.csv')

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
#' van to car, metro to rail.
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
write_csv(trips_export, 'Data/ITHIM/gran_valparaiso/trips_gran_valparaiso.csv')