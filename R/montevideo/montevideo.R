#' ---
#' title: "Preprocessing of Montevideo's travel dataset"
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
#' These files are available in the v-drive in the path "V:/Studies/MOVED/HealthImpact/Data/Country/Uruguay/Montevideo/TravelSurvey/". Locally, this
#' documentation is located in ".../Uruguay/Montevideo/Trips/".
#' These files were sent by the municipality after a meeting with them (in the
#' project of the WB).
#'
#' From now on: 
#+ warning=FALSE, message=FALSE, echo=FALSE
data.frame(
  Reference = c("File1"),
  Description = c("Technical report and final results"),
  Title = c("ENCUESTA DE MOVILIDAD DEL ÁREA METROPOLITANA DE MONTEVIDEO - PRINCIPALES RESULTADOS E INDICADORES"),
  File = c("EncuestadeMovilidadMVD-documentocompleto-final.pdf")
) %>% kbl() %>% kable_classic()

#' ## Definition of a trip 
#' 1. *Trip:* Movement from one part to another made by one person with a
#' specific reason/motive, A definite hour of start and end. Meaning, All trips
#' without any restriction (Pag 14 of **File1**).
#' 
#' 2. *Collection:* Trips collected in this survey correspond to those made the 
#' day of reference, i.e., the day before the survey (between 4am of the day before the survey and 4am of the day of the survey (Pag 14 of **File1**).
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
#' Each file has a data dictionary as a txt file.
#' I ran everything local because it is faster, but if someone wants to run this
#' script, then only the path needs to be changed.
#' Before importing datasets I had to replace "," for "." in decimal values that
#' correspond to sampling weights. These files have the suffix "_DG"
# V-Drive folder
#path <- "V:/Studies/MOVED/HealthImpact/Data/Country/Uruguay/Montevideo/TravelSurvey/EOD2016/"
# Local folder
path <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2021/Cambridge/Data/Uruguay/Montevideo/Trips/EOD2016/"

#+ warning=FALSE, message=FALSE, cache=TRUE
# Households (hh)
hh <- read_delim(paste0(path, "Base Hogar Habitos_DG.csv"), delim = ";") 

#+ warning=FALSE, message=FALSE, cache=TRUE
# People
people <- read_delim(paste0(path, "Base Personas_DG.csv"), delim = ";") 

#+ warning=FALSE, message=FALSE, cache=TRUE
# Trips
trips <- read_delim(paste0(path, "Base Viajes_DG.csv"), delim = ";") 

#+ warning=FALSE, message=FALSE, cache=TRUE
# Stages: there's information about walk_to_pt stages
stages <- read_delim(paste0(path, "Base Etapas_DG.csv"), delim = ";") 


#' ### Number households and people
#' The first thing to do is verify that the number of hh and people is 
#' the same to what is mentioned in page 15 (Tabla 3) of **File1**.
nrow(hh);sum(hh$wcal0) # ok
nrow(people);sum(people$wcal0) # ok
nrow(trips);sum(trips$wcal0) # not the same, 9 trips less here
nrow(stages);sum(stages$wcal0) # not the same, 11 stages less here
#' Results are the same for households and people but not for trips and stages.
#' 
#' ### Number households and people in Montevideo
#' Compare this with what is mentioned in page 15 (Tabla 4) of **File1**. 
#' 
#' To get the results for Montevideo strata that starts with "Montevideo" and
#' "Zona" must be considered (see Tabla 1 of **File1**). In this way household
#' and people results are the 
#' same. Trips is close but not the same (can be explained by decimal points)
hh %>%
  group_by(EstratoGeografico) %>% summarise(Total = sum(wcal0))
# 56388+35672+120613+120331+79052+20342+6801+17387+7842+11473+7157 #OK
people %>%
  group_by(EstratoGeografico) %>% summarise(Total = sum(wcal0))
# 135419+114970+297287+298189+217846+72330+18024+58155+20076+38613+21310 #OK
trips %>%
  group_by(EstratoGeografico) %>% summarise(Total = sum(wcal0))
# 392482+260388+734409+781691+452313+150602+47890+159821+51745+69977+48214 #OK

#' 
#' ### Mode share
#' Compare this with what is mentioned in page 21 (Tabla 10) of 
#' **File1**. 
#' ModoPrincipal is (taken from data dictionary file):
#' 0	A pie hasta 10 cuadras
#' 1	A pie
#' 2	Bicicleta
#' 3	Otro, animal
#' 4	Remise
#' 5	Otro Uber
#' 6	Taxi
#' 7	Auto pasajero
#' 8	Auto conductor
#' 9	Moto pasajero
#' 10	Moto conductor
#' 11	Bus escolar
#' 12	Bus de la empresa
#' 13	Bus
#' 14	Ferrocarril
#' 15	Otros, sin especificar
trips %>% mutate(modoprincipal2 = case_when(
  modoprincipal %in% c(7,8) ~ "Auto",
  modoprincipal %in% c(9,10) ~ "Moto",
  modoprincipal %in% c(13) ~ "Bus",
  modoprincipal %in% c(0) ~ "A pie corto",
  modoprincipal %in% c(1) ~ "A pie",
  modoprincipal %in% c(2) ~ "Bicicleta",
  modoprincipal %in% c(4,5,6) ~ "Taxi,remise",
  modoprincipal %in% c(11,12) ~ "Bus escolar o empresa",
  TRUE ~ "Otros")) %>% 
  group_by(modoprincipal2) %>% summarise(Total = sum(wcal0)) %>% 
  mutate(Prop = Total / sum(Total) * 100)

#' I can't compare directly, I tried this classification but is not quite right.
#' The results are pretty similar though.
#' 
#' # **Preprocessing phase**
#' ## Filtering people from Montevideo only
#' Since the survey was conducted only the Metropolitan Area of Montevideo
#' (page 7 of **File1**) and there information about injuries is only from 
#' Montevideo city, then I have to filter some trips
#sort(unique(people$EstratoGeografico))
people_v2 <- people %>% 
  filter(EstratoGeografico %in% c("Montevideo Alto", "Montevideo Bajo",
                                  "Montevideo Medio", "Montevideo Medio Alto",
                                  "Montevideo Medio Bajo", 
                                  "Zona I - Casavalle            Bajo",
                                  "Zona I - Casavalle            NoBajo",
                                  "Zona II - Cuenca Noreste      Bajo",
                                  "Zona II - Cuenca Noreste      NoBajo",
                                  "Zona III - Cuenca Cerro Norte Bajo",
                                  "Zona III - Cuenca Cerro Norte NoBajo"))

#' I just verify that there are no duplicates in people dataset
people_v2 <- people_v2 %>% 
  mutate(participant_id_paste = paste(NFORM, nnper, sep = "-"))
length(unique(people_v2$participant_id_paste)) == nrow(people_v2)

#' ## Classification and translation of trip modes
#' In the trip dataset there's already a classification of trip modes. To create
#' the following table I used the information I found in the data dictionary in
#' the .txt file (Dicc Base Viajes.txt) and then translated them.
#' This is the result:
main_mode <- read_csv("Data/Standardization/Modes_by_city.csv") %>% 
  filter(City == "Montevideo")
main_mode[,-c(1:2,6)] %>% kbl() %>% kable_classic()

#' Now with respect to trip purpose, I did the same as with mode. From the data
#' dictionary variable "proposito" I translated them.
#' This is the result:
purpose <- read_csv("Data/Standardization/Purpose_by_city.csv") %>% 
  filter(City == "Montevideo")
purpose[,-c(1:2)] %>% kbl() %>% kable_classic()

#' The first two columns have been taken from the data dictionary, and the third
#' column is the translation and classification of these motives. 
#' 
#' #' ## Information at stage or trip level?
#' There is information at stage level although it seems that is not enough 
#' because of the duration of each stage. The information available is about the
#' number of blocks walked before taking other mode, but there's no information
#' about minutes walked or the time spent in each stage.
#' 
#' **For this reason I conclude that even though there's information at stage**
#' **level, it's not enough to get the duration for each stage. Therefore,**
#' **I will continue working at trip level, using trip modes defined in the**
#' **dataset.**
#' 
#' But first I verify there are no duplicates in trips dataset
trips <- trips %>% 
  mutate(trip_id_paste = paste(NFORM, nnper, nvj, sep = "-"))
length(unique(trips$trip_id_paste)) == nrow(trips) # There are duplicates

#' ## Row for each trip, translate trip_mode and create duration, sex and age
#' Trip dataset already has a row for each trip, so I have to create the 
#' variables I need. 
trips_v2 <- trips %>% 
  mutate(trip_id = trip_id_paste,
         trip_duration = tiempoviaje,
         trip_mode = main_mode$ITHIM[match(modoprincipal, main_mode$Code)],
         trip_purpose = purpose$ITHIM[match(proposito, purpose$Code)],
         participant_id_paste = paste(NFORM, nnper, sep = "-"))

#' Check purpose and trip mode
table(trips_v2$modoprincipal, trips_v2$trip_mode, useNA = "always")
table(trips_v2$proposito, trips_v2$trip_purpose, useNA = "always")

#' ## Create variables for quick report
#' I need to create some variables to run the report that Lambed developed in 
#' the function *quality_check*.
report <- people_v2 %>% 
  left_join(trips_v2, by = c("NFORM", "nnper")) %>% 
  mutate(cluster_id = 1,
         household_id = NFORM,
         participant_id = nnper,
         age = EDAD,
         sex = ifelse(SEXO == 1, "Male", "Female"),
         participant_wt = wcal0.x,
         meta_data = NA) %>% 
  select(cluster_id, household_id, participant_id, sex, age, participant_wt,
         trip_id, trip_mode, trip_duration, trip_purpose, meta_data,
         trip_id_paste) 

report$meta_data[1] <- 1380432 # Population in 2016
report$meta_data[2] <- 999999
report$meta_data[3] <- "Travel Survey"
report$meta_data[4] <- 2016
report$meta_data[5] <- "1 day"
report$meta_data[6] <- "Yes (no duration)" #Stage level data available
report$meta_data[7] <- "All purpose"#Overall trip purpose
report$meta_data[8] <- "Yes" # Short walks to PT
report$meta_data[9] <- "Yes (not here)" # Short walks to PT
report$meta_data[10] <- "" # missing modes 

#' I verify that every trip has the sex and age of the person who did it. Since
#' the sum of NAs is zero, then I can conclude that every trip has the
#' information.
sum(is.na(report$sex))
sum(is.na(report$age))
sum(is.na(report$trip_id))
sum(is.na(report$trip_duration))
sum(is.na(report$trip_mode))

#' # **Exporting phase**
#' Export dataset to make the report
write_csv(report, 'Data/Report/montevideo/montevideo_trips.csv')

#' ## **Processing for ITHIM**
#' ### Standardize trip modes
#' There's already a function that standardize these modes so the package can
#' use these trips. I made sure to translate trip modes so that the function
#' works perfectly (take a look at the *original* variable of *smodes* dataframe
#' in this function).

trips_export <- standardize_modes(report, mode = c('trip'))
table(report$trip_mode)
table(trips_export$trip_mode)

#' *standardize_modes* function converts walk to pedestrian, bicycle to cycle,
#' van to car
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
  dplyr::select(participant_id, age, sex, trip_id, trip_mode, trip_duration)

#' ### Export dataset
write_csv(trips_export, 'Data/ITHIM/montevideo/trips_montevideo.csv')