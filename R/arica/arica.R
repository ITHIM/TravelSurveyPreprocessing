#' ---
#' title: "Preprocessing of Arica's travel dataset"
#' author: "Daniel"
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: true
#' ---

#' Note: in order to create the markdown properly from this file, Rstudio options need to be changed. Tools > Global Options > R Markdown > Evaluate chunks in directory > Current. By default this option is set to *Document*, but here I need to change it to *Current* so everything works as expected.
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
#' These files are available in the v-drive in the path "V:/Studies/MOVED/HealthImpact/Data/Country/Chile/Travel Surveys/Arica/". Locally, this documentation is located in ".../Chile/Arica/Trips/Reports".
#' These files were found in: http://www.sectra.gob.cl/encuestas_movilidad/encuestas_movilidad.htm
#'
#' From now on: 
#+ warning=FALSE, message=FALSE, echo=FALSE
data.frame(
  Reference = c("File1", ""),
  Description = c("Technical report and final results",
                  ""),
  Title = c("ACTUALIZACIÓN DIAGNÓSTICO DEL S.T.U. DE LA CIUDAD DE ARICA",
            ""),
  File = c("Actualizacion_diag_STU_Arica_Inf_Final.pdf",
           "")
) %>% kbl() %>% kable_classic()

#' ## Definition of a trip 
#' 1. *Trip:* All trips without any restriction (**File1** page 7-20)
#' 
#' 2. *Collection:* The surveys consists of two steps: 1) Collect household and
#'  people information. Here a specific day is assigned to record the trips 
#'  made. 2) After having assigned a day, people record trips made in that day
#'  (**File1** page 7-21).
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
#' In page 7-41 of **File1** there's a data dictionary.
#' I ran everything local because it is faster, but if someone wants to run this
#' script, then only the path needs to be changed.
#' I exported these excel files from the access database, because I couldn't 
#' read them directly from the database
# V-Drive folder
#path <- "V:/Studies/MOVED/HealthImpact/Data/Country/Chile/Travel Surveys/Arica/"
# Local folder
path <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Chile/Arica/Trips/"

#+ warning=FALSE, message=FALSE, cache=TRUE
# Households (hh)
hh <- read_excel(paste0(path, "Hogar.xlsx")) 

#+ warning=FALSE, message=FALSE, cache=TRUE
# People
people <- read_excel(paste0(path, "Persona.xlsx"))
#' It looks like the age of each person is in a different dataset
people_age <- read_excel(paste0(path, "Edad de personas.xlsx"))
people <- people %>% inner_join(people_age, by = c("IDFolio", "IDPersona"))

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
#' the same to what is mentioned in page 7-56 **File1**.
sum(hh$Factor) # Same as in Cuadro 7.12
sum(people$Factor) # Same as in Cuadro 7.14
#' Results are the same.
#' 
#' ### Number of people by education
#' Compare this with what is mentioned in page 7-57 (Tabla 7.14) of **File1**. 
people %>% 
  group_by(IDEstudios) %>% summarise(Total = sum(Factor))

#'Results are not the same, but they are the same when compared to the database.
#' In table "005-Tabla5" (adding columns in excel). Maybe this is a mistake in 
#' the document.
#' 
#' ### Number of hh and people by sector
#' Compare this with what is mentioned in page 7-59 (Cuadro 7.17) of **File1**. 
data.frame(household = hh %>% 
             group_by(IDMacrozona) %>% summarise(Total = sum(Factor)),
           n_people =  people %>% 
             left_join(hh[, c("IDFolio", "IDMacrozona")], by = "IDFolio") %>% 
             group_by(IDMacrozona)  %>% summarise(Total = sum(Factor))
)

#' Results of households are the same but not for people. Although they are the
#' same when compared to the database, in table "006-Tabla6a".
#' 
#' ### Mode share
#' Compare this with what is mentioned in page 7-76 (Tabla 7.35) of **File1**. 
sum(trips[trips$valida == 1, "Factor"])
trips %>% filter(valida == 1) %>% 
  group_by(IDModo) %>% summarise(n_trips = sum(Factor, na.rm = T))

#' Results are the same. Important here is that is important to filter only
#' valid trips, which can be identified by the column "valida == 1".
#' 
#' # **Preprocessing phase**
#' ## Filtering people from Arica only
#' Since the survey was conducted in only Arica (page 2-1 of **File1**)
#' and there's information in this same coverage about injuries, then I won't
#' filter any trip. 
#' 
#' Note: According to page 2-1 of **File1** the survey cover only comuna Arica
#' 
#' I just verify that there are no duplicates in people dataset
people <- people %>% 
  mutate(participant_id_paste = paste(IDFolio, IDPersona, sep = "-"))
length(unique(people$participant_id_paste)) == nrow(people)

#' ## Classification and translation of trip modes
#' In the trip dataset there's already a classification of trip modes. To create
#' the following table I used the information I found in the data dictionary in
#' the access database (table Modo_Desagregado) and then translated them.
#' This is the result:
main_mode <- read_csv("Data/Standardization/Modes_by_city.csv") %>% 
  filter(City == "Arica")
main_mode[,-c(1:2,6)] %>% kbl() %>% kable_classic()

#' The stage table has modes coded in a different way. The meaning of each code 
#' is presented in table ModoEtapa, in the access database. Here I just
#' translated them:
mode_stage <- 
  data.frame(Code = 1:6,
             ModoEtapa = c("A pie", 
                           "Auto, Moto u Otro (Bicicleta, Camión, etc.)",
                           "Micro, Bus o Taxibus",
                           "Taxi Colectivo",
                           "Taxi Básico o Radiotaxi",
                           "Tren o Metrotren"),
             ITHIM = c("walk", "car", "bus", "taxi", "taxi", "metro"),
             # I give priority to public transport and organize by size
             # This is useful to classify "other" trip_mode, because in this 
             # suvey "other" means a combination of modes rather than "other"
             # mode
             Hierarchy = c(5,4,2,3,3,1))
mode_stage %>% kbl() %>% kable_classic()

#' Now with respect to trip purpose, there are two different classifications.
#' I decided to use *PropositoEstraus* because it has the categories we need.
#' This is the result:
purpose <- read_csv("Data/Standardization/Purpose_by_city.csv") %>% 
  filter(City == "Arica")
purpose[,-c(1:2)] %>% kbl() %>% kable_classic()

#' The first two columns have been taken from the dataset and data dictionary,
#' and the third column is the translation and classification of these motives. 
#' 
#' ## Information at stage or trip level?
#' There is information at stage level although it seems that is not enough 
#' because of the duration of each stage. The information available is about
#' minutes walking before and after taking other mode, but there's no
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
  mutate(trip_id_paste = paste(IDFolio, IDPersona, IDviaje, sep = "-"))
length(unique(trips$trip_id_paste)) == nrow(trips) # OK

stages <- stages %>% 
  mutate(stage_id_paste = paste(IDFolio, IDPersona, IDViaje, 
                                IDEtapa, sep = "-"))
length(unique(stages$stage_id_paste)) == nrow(stages) # OK

#'Less than 1% of trips have more than 1 stage
n_stages <- stages %>% count(IDFolio, IDPersona, IDViaje)
table(n_stages$n, useNA = "always")
table(n_stages$n, useNA = "always") / nrow(n_stages)

#' ## Row for each trip, translate trip_mode and create duration, sex and age
#' Trip dataset already has a row for each trip, so I have to create the 
#' variables I need. 
trips_v2 <- trips %>% 
  mutate(trip_id = trip_id_paste,
         trip_duration = (as.numeric(format(TiempoViaje, "%H")) * 60) +
           (as.numeric(format(TiempoViaje, "%M"))),
         trip_mode = main_mode$ITHIM[match(IDModo, main_mode$Code)],
         # As I said before, I used PropositoEstraus because it has a
         # classification we can use
         trip_purpose = purpose$ITHIM[match(PropositoEstraus, purpose$Code)])

#' Check purpose and trip mode
table(trips_v2$IDModo, trips_v2$trip_mode, useNA = "always")
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
unique(stages$IDModoEtapa)
stages_v2 <- stages %>% 
  mutate(household_id = IDFolio,
         participant_id = IDPersona,
         trip_id = IDViaje,
         stage_id = IDEtapa,
         trip_id_paste = paste(IDFolio, IDPersona, IDViaje, sep = "-"),
         stage_mode = mode_stage$ITHIM[match(IDModoEtapa, mode_stage$Code)],
         hierarchy = mode_stage$Hierarchy[match(IDModoEtapa, mode_stage$Code)]
  ) %>% 
  # Merge number of stages per trip
  left_join(n_stages, by = c("IDFolio", "IDPersona", "IDViaje")) %>% 
  # Merge trip duration information
  left_join(trips_v2[,c("trip_id_paste", "trip_duration", "trip_mode",
                        "trip_purpose", "valida")], by = "trip_id_paste")

#' In this survey, when trip_mode is "other" means that it is a combination of
#' any mode, so by definition is not "other" mode. For this reason I replace the
#' trip mode by using the hierarchy of the stages. In this hierarchy, the lower
#' the number the more priority. For example, a trip that has two stages, taxi
#' and car, will be replaced from "other" to taxi, because taxi has more
#' priority.
#' Note: if in other surveys "other" mode indeed means "other", then this step 
#' is not needed.
#table(stages_v2$trip_mode, stages_v2$stage_mode, useNA = "always")
stages_v2_other <- stages_v2 %>% 
  filter(trip_mode == "other") %>% 
  group_by(trip_id_paste) %>% 
  # Change trip_mode by taking the mode with the minimum value in hierarchy
  mutate(trip_mode = mode_stage$ITHIM[
    match(min(hierarchy), mode_stage$Hierarchy)])

#' Merge trips with "other" mode with the rest of trips
stages_v3 <- stages_v2 %>% filter(trip_mode != "other") %>% 
  bind_rows(stages_v2_other)

#' Note: It is important to note that stage_mode is aggregated in the stage
#' dataset but it's not in the trip dataset. Modes such as bicycle would be lost
#' if I don't correct this. For this reason, when working with single stage
#' trips, I will use trip_mode as stage_mode (see trip 5029006-2-3 as example).
#' When working with more than 2 stages trips, I will leave them as they are.
#' 
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
sum(is.na(stages_v3$MinutosCaminadosAntes))
sum(is.na(stages_v3$MinutosCaminadosDespues))
stages_v3_1 <- stages_v3 %>% filter(n == 1) %>% 
  # Compute walking duration
  mutate(stage_mode = trip_mode, # Correcting stage_mode according to note above
         walking_duration = MinutosCaminadosAntes + MinutosCaminadosDespues,
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

#' Only in 15 trips the walking duration is the same or larger than the trip
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
                trip_purpose, valida, stage_id, stage_mode, stage_duration,
                stage_id_paste, trip_id_paste)

#' In trips when NO adjustment is needed, there will be multiple stages because
#' of the walking stages. As a consequence, each trip will have at least two
#' rows: one for the main stage and one for the walking stage.
#names(stages_v3_1)
stages_v3_1_noadjust <- stages_v3_1 %>% 
  filter(need_adjustment == 0 & trip_mode != "walk") %>% 
  # I use pivot_longer to put walking stages in a new row
  pivot_longer(c("MinutosCaminadosAntes", "stage_duration",
                 "MinutosCaminadosDespues"), names_to = "variable",
               values_to = "stage_duration") %>% 
  # Assign "walk_to_pt" to walking stages
  mutate(stage_mode = ifelse(variable %in% c("MinutosCaminadosAntes",
                                             "MinutosCaminadosDespues"), 
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
                trip_purpose, valida, stage_id, stage_mode, stage_duration,
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
    MinutosCaminadosDespues_ok = ifelse(stage_id != n, 
                                        0, MinutosCaminadosDespues)) %>% 
  # I compute the walking duration at the beginning and end of a stage
  group_by(household_id, participant_id, trip_id) %>% 
  mutate(MinutosCaminadosAntes_sum = sum(MinutosCaminadosAntes, na.rm = T),
         MinutosCaminadosDespues_sum = sum(MinutosCaminadosDespues_ok, 
                                           na.rm = T),
         # I compute total walking duration
         walking_duration = MinutosCaminadosAntes_sum +
           MinutosCaminadosDespues_sum,
         # The remaining trip duration is split equally in the other modes
         stage_duration = (trip_duration - walking_duration) / n) %>% 
  ungroup() %>% 
  # I use pivot_longer to put walking stages in a new row
  pivot_longer(c("MinutosCaminadosAntes", "stage_duration",
                 "MinutosCaminadosDespues_ok"), names_to = "variable",
               values_to = "stage_duration") %>% 
  # Assign "walk_to_pt" to walking stages
  mutate(stage_mode = ifelse(variable %in% c("MinutosCaminadosAntes",
                                             "MinutosCaminadosDespues_ok"), 
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
                trip_purpose, valida, stage_id, stage_mode, stage_duration,
                stage_id_paste, trip_id_paste)

#' **Append all stages:**
#' 
#' I merge all stages again to create a unique dataset and filter out trips that
#' are not valid
stages_ready <- stages_v3_1_ready %>% 
  bind_rows(stages_v3_2) %>% 
  filter(valida == 1) %>% 
  arrange(household_id, participant_id, trip_id, stage_id) 

#' The number of trips is the same and there are more stages as a consequence of
#' creating the walking stages. Important to note that in stages_ready invalid
#' trips were removed.
# Number of trips before processing
length(unique(stages_v3$trip_id_paste)) 
# Number of trips after processing
length(unique(stages_v3_1_ready$trip_id_paste)) + 
  length(unique(stages_v3_2$trip_id_paste)) 
length(unique(stages_ready$trip_id_paste)) # Invalid trips are removed

# Number of stages before processing
length(unique(stages_v3$stage_id_paste))
# Number of stages after processing
length(unique(stages_v3_1_ready$stage_id_paste)) + 
  length(unique(stages_v3_2$stage_id_paste)) 
length(unique(stages_ready$stage_id_paste)) # Invalid trips are removed

#' ## Create variables for quick report
#' I need to create some variables to run the report that Lambed developed in 
#' the function *quality_check*.
report <- people %>% 
  left_join(stages_ready, 
            by = c("IDFolio" = "household_id", 
                   "IDPersona" = "participant_id")) %>% 
  mutate(cluster_id = 1,
         household_id = IDFolio,
         participant_id = IDPersona,
         age = Edad,
         sex = ifelse(IDSexo == 1, "Male", "Female"),
         participant_wt = Factor,
         meta_data = NA) %>% 
  dplyr::select(cluster_id, household_id, participant_id, sex, age,
                participant_wt,
                trip_id, trip_mode, trip_duration, trip_purpose,
                stage_id, stage_mode, stage_duration, 
                stage_id_paste, trip_id_paste, meta_data) 

report$meta_data[1] <- 221364 # Population in 2017
report$meta_data[2] <- 999999 
report$meta_data[3] <- "Travel Survey"
report$meta_data[4] <- 2010
report$meta_data[5] <- "1 day"
report$meta_data[6] <- "Yes (no duration)" #Stage level data available
report$meta_data[7] <- "All purpose"#Overall trip purpose
report$meta_data[8] <- "Yes" # Short walks to PT
report$meta_data[9] <- "Yes" # Short walks to PT
report$meta_data[10] <- "train, motorcycle" # missing modes 

#' I verify that every trip has the sex and age of the person who did it. Since
#' the sum of NAs is zero, then I can conclude that every trip has the
#' information.
sum(is.na(report$sex))
sum(is.na(report$age))
sum(is.na(report$trip_id))
sum(is.na(report$trip_duration))
sum(is.na(report$trip_mode))
sum(is.na(report$stage_id))
table(people$NumeroViajes)

#' # **Exporting phase**
#' Export dataset to make the report
write_csv(report, 'Data/Report/arica/arica_trips.csv')

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
#' van to car.
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
write_csv(trips_export, 'Data/ITHIM/arica/trips_arica.csv')