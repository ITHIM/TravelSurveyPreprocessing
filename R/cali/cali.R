#' ---
#' title: "Preprocessing of Cali's travel dataset"
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
#' These files are available in the v-drive in the path "V:/Studies/MOVED/HealthImpact/Data/Country/Colombia/Cali/Trips/EODH2015/". Locally, this documentation is located in ".../Colombia/Cali/Trips/Documentos".
#'
#' From now on: 
#+ warning=FALSE, message=FALSE, echo=FALSE
data.frame(
  Reference = c("File1", "File2"),
  Description = c("Technical report and final results",
                  "Report with only final results"),
  Title = c("Producto 4, Indicadores Encuesta de Movilidad",
            "Encuesta de movilidad Cali 2015"),
  File = c("160216_Producto 4_Indicadores EODH.pdf",
           "20151127_Publicacion_EODH.pdf")
) %>% kbl() %>% kable_classic()

#' ## Definition of a trip
#' 1. *Trip:* All trips that are longer than 3 minutes made by people of 5 years
#' old or older (**File1**, page 102, paragraph 4.8).
#' 
#' Definition of trip in page 146 of **File1**: *Movement from one part to another made by one person with a specific reason/motive and a duration greater than 3 minutes. Or a movement from one part to another with reason/motive work or study of any duration.*
#' 
#' 2. *Collection:* Trips collected in this survey correspond to those made the 
#' day of reference, i.e., the day before the survey (**File1**, page 102,
#' paragraph 4.8).
#' 
#' **Results presented are for trips made in a single day.**
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
#' In page 98 of **File1** there's a data dictionary.
#' I ran everything local because it is faster, but if someone wants to run this
#' script, then only the path needs to be changed.
# V-Drive folder
#path <- "V:/Studies/MOVED/HealthImpact/Data/Country/Colombia/Cali/Trips/EODH2015/Base de datos EODH 2015/2. Módulos/"
# Local folder
path <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2021/Cambridge/Data/Colombia/Cali/Trips/Base de datos EODH 2015/2. Módulos/"

#+ warning=FALSE, message=FALSE, cache=TRUE
# Households (hh)
hh <- read_excel(paste0(path, "MOD_A_ID_HOGAR_.xlsx"), guess_max = 100000) 
#' These warnings are from a variable that we're not going to use, so it 
#' doesn't matter

#+ warning=FALSE, message=FALSE, cache=TRUE
#' **People:** When trying to read people file I saw that in the original file
#' the first row is the label of each variable and the second row is the meaning
#' of that label. I had to remove this second row and delete some images that
#' were inside the file to read it easily here in R. This file is now called:
#' *MOD_B_PERSONAS_Modified.xlsx*
#' 
#' *Note: When reading person there are some warnings related to people without*
#' *age*
people <- read_excel(paste0(path, "MOD_B_PERSONAS_Modified.xlsx"), 
                     guess_max = 100000)

# Vehicles
#vehicles <- read_excel(paste0(path, "MOD_C_VEHICULOS.xlsx"))

#+ warning=FALSE, message=FALSE, cache=TRUE
#' **Trips:** According to the documentation, the dataset with imputed trips is
#' the one that we should use (see paragraph 4.9-4.13, pag 102, **File1**).
#' Before importing this dataset, in excel I added a new column called "EMPTY"
#' because values in the dataset didn't correspond to the label of the variable.
#' By creating this empty column everything makes more sense now. This file is
#' now called: *MOD_D_VIAJES_IMP_Modified.xlsx*
trips <- read_excel(paste0(path, "MOD_D_VIAJES_IMP_Modified.xlsx"),
                    guess_max = 100000)

#' ### Number of rows
#' The first thing to do is verify that the number of rows of each dataset is 
#' the same to what is mentioned in chapter 4 of **File1**. In this chapter
#' there is the data dictionary with the meaning of every variable as well as
#' the number of rows in each dataset.
nrow(hh) # Same as in paragraph 4.2
nrow(people) # Same as in paragraph 4.4
nrow(trips) # Same as in paragraph 4.12
#' In all of them is the same as reported, then we can continue.
#' 
#' ### Number of surveys per socio-economical level (strata)
#' As before, I'll verify that the number of surveys per socio-economical strata
#' is the same to what is mentioned in page 112 (Figura 5.4) of **File1**.
hh %>% group_by(ESTRATO) %>% summarise(n()) %>% 
  kbl() %>% kable_classic(full_width = F)
#' Results are the same.
#' 
#' ### Number of households per municipality (with weights)
#' To check that the number of hh per municipality is the same to what is
#' mentioned in page 116 (Figura 5.9) of **File1**. This number is estimated
#' using sampling weights.
hh %>% group_by(MUNICIPIO) %>% summarise(suma = sum(F_EXP)) %>% 
  arrange(desc(suma)) %>% 
  kbl() %>% kable_classic(full_width = F)
#' In all of them is the same as reported, then we can continue.
#' 
#' ### Average number of people per household
#' To check that the number of people per hh is the same to what is mentioned in
#' page 119 (Figura 5.12) of **File1**. This number is estimated using sampling
#' weights.
people_per_hh <- people %>% group_by(ORDEN) %>% summarise(total_people = n())

hh %>% inner_join(people_per_hh, by = "ORDEN") %>% 
  rowwise() %>% 
  mutate(n_people = total_people*F_EXP) %>% 
  group_by(MUNICIPIO) %>% 
  summarise(sum(n_people)/sum(F_EXP)) %>% 
  kbl() %>% kable_classic(full_width = F)
#' In all of them is the same as reported, then we can continue.
#' 
#' ### Number of people by sex
#' To check that the number of people is the same to what is mentioned in page
#' 126 (Figura 5.22) of **File1**. This number is estimated using sampling
#' weights
people %>% group_by(P_04_B) %>% summarise(suma = sum(F_EXP)) %>% 
  kbl() %>% kable_classic(full_width = F)
#' The number of men is exactly the same, whereas the number of women is not by
#' just one person. This difference can be explained as rounding error.
#'
#' ### Number of people per municipality
#' To check that the number of people per municipality is the same to what is
#' mentioned in page 123 (Figura 5.19) of **File1**. This number is estimated
#' using sampling weights.
t_people <- people %>% 
  left_join(hh[, c("ORDEN", "MUNICIPIO")], by = "ORDEN") %>% 
  group_by(MUNICIPIO) %>% summarise(total = sum(F_EXP)) %>% 
  mutate(prop = total / sum(total)*100) # Ok Pag 123, Figure 5.19
t_people
#' There's a difference in the proportion of Jamundi and Yumbo. Although this
#' doesn't really matter because we need information for Cali only.
#' 
#' ### Mode share
#' Compare this with what is mentioned in page 169 (Figura 5.77) of **File1**
sum(trips$F_EXP) # There's one more trip (paragraph 5.73)
trips %>% group_by(P_E1_14_D) %>% summarise(total = sum(F_EXP)) %>%
  mutate(prop = total / sum(total)*100) %>%
  arrange(desc(prop))
#' Result for walking trips is the same, however the trip mode needs to be 
#' categorized.
#' 
#' # **Preprocessing phase**
#' ## Filtering people from Cali only
#' Since the survey was conducted in 5 municipalities and we are only interested
#' in Cali, then these people are the only ones used. I could've used the
#' information of other municipalities, if there had been data about injuries in
#' these locations, but unfortunately we only have injuries in Cali.
people_v2 <- people %>% 
  inner_join(hh[,c("ORDEN", "MUNICIPIO")], 
             by = "ORDEN") %>% 
  filter(MUNICIPIO == "CALI")

#' I verify that there are no duplicates in people dataset
people_v2 <- people_v2 %>% 
  mutate(participant_id_paste = paste(ORDEN, ID_PER, sep = "-"))
length(unique(people_v2$participant_id_paste)) == nrow(people_v2)

#' ## Classification and translation of trip modes
#' Nowhere in the documentation says something about the process to define the
#' trip main mode of transport. It looks like that in the imputed dataset the
#' information about the main mode is already cleaned in variable P_MP_14_D
#' because there's no info
#' about other stages. So I decided to create this table based on the 
#' information I have (in the access database) and translated it (similar to
#' what I did with Medellin). This is the result:
main_mode <- read_csv("Data/Standardization/Modes_by_city.csv") %>% 
  filter(City == "Cali")
main_mode[,-c(1:2,6)] %>% kbl() %>% kable_classic()

#' *Note: In the dataset these modes are coded, so I had to open the Access*
#' *database to get the meaning of each code.*
#' 
#' Now with respect to trip purpose, I only had to translate them (P_13_D). 
#' This is the result:
purpose <- read_csv("Data/Standardization/Purpose_by_city.csv") %>% 
  filter(City == "Cali")
purpose[,-c(1:2)] %>% kbl() %>% kable_classic()

#' The first two columns have been taken from the dataset and access database,
#' and the third column is the translation and classification of these motives. 
#' 
#' ## Information at stage or trip level?
#' There is information at stage level although it seems that is not enough 
#' because of the duration of each stage. It looks like the information
#' available is about minutes walked before each stage, but there's no
#' information about the time spent in each stage. However, the dataset that
#' was used to create official statistics has some imputations and this dataset
#' does not have the information at stage level.
#' 
#' **For this reason I conclude that even though there's information at stage**
#' **level, The imputed dataset does not have it. Therefore,**
#' **I will continue working at trip level**
#' 
#' But first I verify there are no duplicates in trips dataset
trips <- trips %>% 
  mutate(trip_id_paste = paste(ORDEN, ID_PER, NO_VIAJE, sep = "-"))
length(unique(trips$trip_id_paste)) == nrow(trips) # OK

#' To understand how the information was collected at stage level, I selected a
#' person at random and followed the questionnaire (see
#' **150508_EODH_MODULO_D_V06.pdf**) while checking
#' the information in the datasets. The person selected has ORDEN = "04414"
#' and ID_PER = "01" and NO_VIAJE = "01"
trips %>% filter(ORDEN == "04414" & ID_PER == "01" & 
                   NO_VIAJE == "01") %>% 
  select(ORDEN, ID_PER, P_09_HR_D, P_09_MN_D, P_MP_14_D, P_E1_14_D, P_E2_14_D, P_E3_14_D, P_E4_14_D, P_E5_14_D, P_E6_14_D, P_E7_14_D, P_27_HR_D,
         P_27_MN_D, T_VIAJE) %>% kbl() %>% kable_classic()

#' From this table I can see that the person started her trip at 6:50, and 
#' her main mode of transport was walking (P_MP_14_D == 30). It looks like she
#' didn't use any other mode because columns related to stages are filled with
#' NAs. She finally ended her trip at 7:00. The duration of this trip was 10 
#' min.
#' 
#' When comparing this dataset with what is mentioned in the data dictionary of
#' **File1** (page 104), I can see that some variables are missing like walking
#' duration from origin to the first stage and from last stage to the 
#' destination. And also information about each stage, because it looks like in
#' the imputed dataset the remaining modes are missing. If this is case, there
#' is still no information about stage duration. 
#' 
#' ## Row for each trip, translate trip_mode and create duration, sex and age
#' Trip dataset already has a row for each trip, so I have to create the 
#' variables I need. 
trips_v2 <- trips %>% 
  mutate(trip_id = NO_VIAJE,
         trip_duration = T_VIAJE,
         trip_mode = main_mode$ITHIM[
           match(as.numeric(P_E1_14_D), main_mode$Code)],
         trip_purpose = purpose$ITHIM[
           match(as.numeric(P_13_D), purpose$Code)],
         trip_id_paste = paste(ORDEN, ID_PER, NO_VIAJE, sep = "-"))

#' Check purpose and trip mode
table(trips_v2$P_E1_14_D, trips_v2$trip_mode, useNA = "always")
table(trips_v2$P_13_D, trips_v2$trip_purpose, useNA = "always")

#' It is important to mention that in this dataset I decided to use the main 
#' mode that appears in *P_E1_14_D* and not in *P_MP_14_D*, because the later 
#' have missing values. 
#' 
#' **ToDo: Try to implement time duration following the protocol from Bogota's**
#' **documentation, because it takes into account differences between days.**
#' **This file is "Caracterización de la movilidad – Encuesta de Movilidad de Bogotá 2019", page 202, paragraph 6.44**
#' 
#' 
#' ## Create variables for quick report
#' I need to create some variables to run the report that Lambed developed in 
#' the function *quality_check*.
report <- people_v2 %>% 
  left_join(trips_v2, by = c("ORDEN", "ID_PER")) %>% 
  mutate(cluster_id = 1,
         household_id = ORDEN,
         participant_id = ID_PER,
         age = as.numeric(P_05_B),
         sex = ifelse(P_04_B == "HOMBRE", "Male", "Female"),
         participant_wt = F_EXP.x,
         meta_data = NA) %>% 
  select(cluster_id, household_id, participant_id, sex, age, participant_wt,
         trip_id, trip_mode, trip_duration, trip_purpose, meta_data) 

#' I verify that every trip has the sex and age of the person who did it. Since
#' the sum of NAs is zero, then I can conclude that every trip has the
#' information.
sum(is.na(report$sex))
sum(is.na(report$age))
sum(is.na(report$trip_id))
sum(is.na(report$trip_duration))
sum(is.na(report$trip_mode))

#' There are some trips without duration. This is related to some trips that
#' don't have values in T_VIAJE.
#' 
#' Since there are some trips without duration, I removed them for now. But I 
#' think it's better to compute duration using Bogota's protocol.
#' **I leave this part in stand by, while I figure something out**
#trips_cali_v2 <- trips_cali_v2 %>% drop_na(trip_duration)

#' # **Exporting phase**
#' Export dataset to make the report
write_csv(report, 'Data/Report/cali/cali_trips.csv')

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
write_csv(trips_export, 'Data/ITHIM/cali/trips_cali.csv')
