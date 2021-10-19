#' ---
#' title: "Preprocessing of Medellin's travel dataset"
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
#' These files are available in the v-drive in the path "V:/Studies/MOVED/HealthImpact/Data/Country/Colombia/Medellin/Travel/EOD2017/Reports/". Locally, this documentation is located in ".../Colombia/Medellin/Trips/Reports/". 
#' These files
#' were found in https://datosabiertos.metropol.gov.co/ but also sent to me from
#' the project of WWF. I looked for information with more detail and the only
#' thing that is published is this dashboard: https://www.metropol.gov.co/observatorio/Paginas/encuestaorigendestino.aspx
#'
#' From now on: 
#+ warning=FALSE, message=FALSE, echo=FALSE
data.frame(
  Reference = c("File1", "File2"),
  Description = c("Short Report with final results",
                  "Theoretical and methodological framework"),
  Title = c("Encuesta de Movilidad 2017 Origen-Destino",
            "Encuesta de Movilidad Origen y Destino 2017 Marco teorico y metodologico"),
  File = c("PresentacionEOD2017.pdf",
           "Encuesta Movilidad_Valle de Aburrá_ 2017.pdf")
) %>% kbl() %>% kable_classic()

#' ## Definition of a trip
#' 1. *Trip:* All trips with the only restriction that walking trips are
#' collected if they're longer than 2 blocks (**File1** page 7).
#' 
#' 2. *Collection:* Trips collected in this survey correspond to those made the 
#' day of reference, i.e., the day and night before the survey (last 24 hours) 
#' (page 25, Modulo III from **File2**).
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
#' In file "DataDictionary.xlsx" there's a data dictionary. This data dictionary
#' was taken from https://datosabiertos.metropol.gov.co/dataset/encuesta-origen-destino-2017-datos-por-viajes
#' 
#' I ran everything local because it is faster, but if someone wants to run this
#' script, then only the path needs to be changed.
#' Information about households, people and trips are in the same file but in 
#' different sheets.
# V-Drive folder
#path <- "V:/Studies/MOVED/HealthImpact/Data/Country/Colombia/Medellin/Travel/EOD2017/"
# Local folder
path <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2021/Cambridge/Data/Colombia/Medellin/Trips/"

#+ warning=FALSE, message=FALSE, cache=TRUE
# Households (hh)
hh <- read_excel(paste0(path, "EOD_2017_DatosViajes.xlsx"), 
                 sheet = "DATOS HOGARES", guess_max = 100000) 
# People
people <- read_excel(paste0(path, "EOD_2017_DatosViajes.xlsx"),
                     sheet = "DATOS MORADORES", guess_max = 100000)
# Trips
trips <- read_excel(paste0(path, "EOD_2017_DatosViajes.xlsx"), 
                    sheet = "DATOS VIAJES", guess_max = 100000)
trips <- trips[rowSums(is.na(trips)) != ncol(trips), ] # Removing empty rows

#' ### Number households and people
#' The first thing to do is verify that the number of households and Macrozones 
#' in the dataset are the same to what is mentioned in page 5 of **File1**.
nrow(hh)
length(unique(hh$NOM_MACROZONA))
#' These values are the same as reported. However I could not replicate any 
#' other result because these datasets don't have sampling weights, and every 
#' number in **File1** was estimated using them.
#' 
#' ### Number of hh in trip dataset
#' First I have to make sure that there are no duplicates in the hh dataset.
length(unique(hh$ID_HOGAR)) == nrow(hh)

#' Since this is ok, then I have to see whether the number of hh in the trip 
#' dataset is the same that in the hh dataset
length(unique(trips$ID_HOGAR)) == nrow(hh)
hh$ID_HOGAR[!hh$ID_HOGAR %in% c(trips$ID_HOGAR)]

#' These households don't have trips recorded. Here's a couple of cases 
#' (randomly chosen)
trips[trips$ID_HOGAR == "3001827",]
trips[trips$ID_HOGAR == "305914",]

#' Since there are hh without trips, I want to see whether there are trips 
#' without hh information. The following output suggest that it's not the case.
trips$ID_HOGAR[!trips$ID_HOGAR %in% c(hh$ID_HOGAR)]
#' 
#' ### Number of people in trip dataset
#' Check first that there are no duplicates in people dataset.
people <- people %>% 
  mutate(participant_id_paste = paste(ID_HOGAR, ORDEN_MORADOR, sep = "-"))
length(unique(people$participant_id_paste)) == nrow(people) 
#' Since this is ok, then I have to see whether the number of people in the trip
#' dataset is the same that in the people dataset
trips <- trips %>% 
  mutate(participant_id_paste = paste(ID_HOGAR, ID_MORADOR, sep = "-"))

length(unique(trips$participant_id_paste)) == nrow(people)

head(people$participant_id_paste[!people$participant_id_paste %in% c(trips$participant_id_paste)], 100)

#' These people don't have trips recorded, and it must be because of
#' restrictions in the questionnaire. Here's a couple of cases (randomly
#' chosen)
trips[trips$participant_id_paste == "10804-1106",]
trips[trips$participant_id_paste == "616-50",]

#' Since there are people without trips, I want to see whether there are trips 
#' without people information. The following output suggest there is one trip
#' without personal information, and when looking for it in the people dataset
#' it doesn't appear. Therefore, these trips are going to be removed (later).
trips$participant_id_paste[!trips$participant_id_paste %in% c(people$participant_id_paste)]

people[people$ID_HOGAR == "401022" & people$ORDEN_MORADOR == "158",]
trips[trips$participant_id_paste == "401022-158",]

#' # **Preprocessing phase**
#' ## Filtering people from Medellin only
#' Since the survey was conducted in 10 municipalities and we are only
#' interested in Medellin, then these trips are the only ones used. I could've
#'  used the information of other municipalities, if there had been data about
#'  injuries in these locations, but unfortunately we only have injuries in
#'  Medellin.
people_v2 <- people %>% 
  inner_join(hh[,c("ID_HOGAR", "NOM_MUNICIPIO")], 
             by = "ID_HOGAR") %>% 
  filter(NOM_MUNICIPIO == "Medellin")

#' I verify that there are no duplicates in people dataset
length(unique(people_v2$participant_id_paste)) == nrow(people_v2)

#' ## Classification and translation of trip modes
#' Nowhere in the documentation says something about the process to define the
#' trip main mode of transport. I asked directly to people from Medellin and 
#' they told me there was hierarchy (like in Bogota) but they didn't send it to
#' me. As a consequence I decided to create this classification and its
#' hierarchy based on the information I have from Bogota. 
#' This is the result:
main_mode <- read_csv("Data/Standardization/Modes_by_city.csv") %>% 
  filter(City == "Medellin")
main_mode[,-c(1:3)] %>% kbl() %>% kable_classic()

#' Now with respect to trip purpose (column MOTIVO_VIAJE), I only had to
#' translate them. This is the result:
#table(trips$DESC_MOTIVO_VIAJE, trips$MOTIVO_VIAJE)
purpose <- read_csv("Data/Standardization/Purpose_by_city.csv") %>% 
  filter(City == "Medellin")
purpose[,-c(1:2)] %>% kbl() %>% kable_classic()

#' The first two columns have been taken from the dataset, and the third column
#' is the translation and classification of these motives. 
#' 
#' ## Information at stage or trip level?
#' There is information at stage level although it seems that is not enough 
#' because of the duration of each stage. The information available is about the
#' number of blocks walked before taking other mode, but there's no information
#' about minutes walked or the time spent in each stage.
#' 
#' **For this reason I conclude that even though there's information at stage**
#' **level, it's not enough to get the duration for each stage. Therefore,**
#' **I will continue working at trip level, and to get the trip mode I will**
#' **define the mode with highest priority.**
#' 
#' But first I verify there are no duplicates in trips dataset
trips <- trips %>% 
  mutate(trip_id_paste = paste(ID_HOGAR, ID_MORADOR, SEC_VIAJE, sep = "-"))
length(unique(trips$trip_id_paste)) == nrow(trips) # There are duplicates

#' In the following output I can see that there's only one duplicate and it has the same information, So I'm removing it.
which(duplicated(trips$trip_id_paste))
View(trips[which(duplicated(trips$trip_id_paste)),])
View(trips[trips$trip_id_paste == "1330375-1128-2612",])

#' Almost all the information is the same so I will remove this trip
trips <- trips[-which(duplicated(trips$trip_id_paste)),]
length(unique(trips$trip_id_paste)) == nrow(trips) # OK

#' To understand how the information was collected at stage level, I selected a
#' person at random and tried to understand what modes person took to get to its
#' destination. The person selected has ID_HOGAR = 309974, ID_MORADOR = 2 and
#' SEC_VIAJE = 6.
#' 
trips %>% filter(ID_HOGAR == "309974", ID_MORADOR == "2", 
                 SEC_VIAJE == "6") %>% 
  select(ID_HOGAR, ID_MORADOR, SEC_VIAJE, HORA_O, HORA_D, DESC_MODO_TTE_E1,
         DESC_MODO_TTE_E2, DESC_MODO_TTE_E3, DESC_MODO_TTE_E4, DEC_MODO_TTE_E5,
         DESC_MODO_TTE_E6, DESC_MODO_TTE_E7) %>% 
  mutate(HORA_O = format(HORA_O, format = "%H:%M"),
         HORA_D = format(HORA_D, format = "%H:%M")) %>%
  kbl() %>% kable_classic()

#' From this table I can see that the person started her trip at 17:00, and 
#' walked a block to get the metro. Then she took a bus (ruta integrada) to 
#' finally walk a block to get to her destination at 18:00.
#' 
#' Here it's clear that she walked before and after the public transport, and 
#' even though it says that she walked a block, we cannot know how much time she
#' spent walking. We could somehow estimate the time walked by using only
#' walking trips but for now I will use this dataset at trip level.
#' 
#' ## Row for each trip, translate trip_mode and create duration, sex and age
#' Trip dataset already has a row for each trip, so I have to create the 
#' variables I need. 
trips_v2 <- trips %>% 
  mutate(trip_id = trip_id_paste,
         trip_duration = as.numeric(difftime(HORA_D, HORA_O, units = "mins")),
         # Replace modes by its hierarchy
         mode_e1 = main_mode$Hierarchy[
           match(DESC_MODO_TTE_E1, main_mode$RawMode)],
         mode_e2 = main_mode$Hierarchy[
           match(DESC_MODO_TTE_E2, main_mode$RawMode)],
         mode_e3 = main_mode$Hierarchy[
           match(DESC_MODO_TTE_E3, main_mode$RawMode)],
         mode_e4 = main_mode$Hierarchy[
           match(DESC_MODO_TTE_E4, main_mode$RawMode)],
         mode_e5 = main_mode$Hierarchy[
           match(DEC_MODO_TTE_E5, main_mode$RawMode)],
         mode_e6 = main_mode$Hierarchy[
           match(DESC_MODO_TTE_E6, main_mode$RawMode)],
         mode_e7 = main_mode$Hierarchy[
           match(DESC_MODO_TTE_E7, main_mode$RawMode)],
         trip_purpose = purpose$ITHIM[
           match(as.numeric(MOTIVO_VIAJE), purpose$Code)]) %>%
  # Now compute the main mode by looking at the hierarchy
  rowwise() %>% mutate(
    main_modes = min(mode_e1, mode_e2, mode_e3, mode_e4, mode_e5, mode_e6,
                     mode_e7, na.rm = T),
    trip_mode = main_mode$ITHIM[
      match(main_modes, main_mode$Hierarchy)]
  )

#' Check purpose and trip mode
table(trips_v2$DESC_MODO_TTE_E1, trips_v2$trip_mode, useNA = "always")
table(trips_v2$MOTIVO_VIAJE, trips_v2$trip_purpose, useNA = "always")

#' **ToDo: Try to implement time duration following the protocol from Bogota's**
#' **documentation, because it takes into account differences between days.**
#' **This file is "Caracterización de la movilidad – Encuesta de Movilidad de Bogotá 2019", page 202, paragraph 6.44**
#' 
#' 
#' ## Create variables for quick report
#' I need to create some variables to run the report that Lambed developed in 
#' the function *quality_check*.
report <- people_v2 %>% 
  left_join(trips_v2, by = c("ID_HOGAR", "ORDEN_MORADOR" = "ID_MORADOR")) %>% 
  mutate(cluster_id = 1,
         household_id = ID_HOGAR,
         participant_id = ORDEN_MORADOR,
         age = EDAD,
         sex = ifelse(GENERO == "1", "Male", "Female"),
         participant_wt = 1,
         meta_data = NA) %>% 
  select(cluster_id, household_id, participant_id, sex, age, participant_wt,
         trip_id, trip_mode, trip_duration, trip_purpose, meta_data) 

report$meta_data[1] <- 2293601
report$meta_data[2] <- "..." 
report$meta_data[3] <- "Travel Survey"
report$meta_data[4] <- 2017
report$meta_data[5] <- "1 day"
report$meta_data[6] <- "Yes, but no stage duration" #Stage level data available
report$meta_data[7] <- "All purpose"#Overall trip purpose
report$meta_data[8] <- "Yes, but not here (yet)" # Short walks to PT
report$meta_data[9] <- "No" # Distance available
report$meta_data[10] <- "..." # missing modes#' 

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
write_csv(report, 'Data/Report/medellin/medellin_trips.csv')

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
#' van to car, rickshaw to auto_rickshaw, metro to rail
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
write_csv(trips_export, 'Data/ITHIM/medellin/trips_medellin.csv')