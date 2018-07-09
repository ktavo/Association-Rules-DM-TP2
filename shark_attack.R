#Shark Attacks

rm(list=ls())
setwd("E:/UBA/Data Mining/TP2")

library(plyr)

sharksData <- read.csv(file="Shark_Attack_Data.csv", header=TRUE, sep=",")
glimpse(sharksData)

#Variables seleccionadas para el estudio
selectedVariables = c("Case.Number", "Date", "Year", "Type", "Country", "Area", "Activity",
                  "Sex", "Age", "Injury", "Fatal..Y.N.", "Time", "Species")
sharksData <- sharksData[selectedVariables]


#Transformación Variable Case.Number
sharksData$Case.Number <- gsub(".R", "", sharksData$Case.Number)
sharksData$Case.Number <- gsub(".a", "", sharksData$Case.Number)
sharksData$Case.Number <- gsub(".b", "", sharksData$Case.Number)
sharksData$Case.Number <- gsub(".c", "", sharksData$Case.Number)
sharksData$Case.Number <- gsub(".d", "", sharksData$Case.Number)

#Transformación Variable Date
sharksData$Date <- gsub("Reported ", "", sharksData$Date)



############################################################################
#Transformación Variable sex
levels(sharksData$Sex)[levels(sharksData$Sex) == "M "] <- "M"
levels(sharksData$Sex)[levels(sharksData$Sex) == ""] <- "Unknown"
levels(sharksData$Sex)[levels(sharksData$Sex) == "."] <- "Unknown"
levels(sharksData$Sex)[levels(sharksData$Sex) == "N"] <- "Unknown"
levels(sharksData$Sex)[levels(sharksData$Sex) == "lli"] <- "Unknown"
levels(sharksData$Sex)


#Transformación Fatal..Y.N
levels(sharksData$Fatal..Y.N.)[levels(sharksData$Fatal..Y.N.) == ""] <- "Unknown"
levels(sharksData$Fatal..Y.N.)[levels(sharksData$Fatal..Y.N.) == " N"] <- "N"
levels(sharksData$Fatal..Y.N.)[levels(sharksData$Fatal..Y.N.) == "F"] <- "N"
levels(sharksData$Fatal..Y.N.)[levels(sharksData$Fatal..Y.N.) == "N "] <- "N"
levels(sharksData$Fatal..Y.N.)[levels(sharksData$Fatal..Y.N.) == "UNKNOWN"] <- "Unknown"
levels(sharksData$Fatal..Y.N.)



#Transformación Age
#[Baby{0-4}, Kid{5-13}, Teen{14-18}, Young{19-30} , Adult{31-49}, Elderly{50-100}]
levels(sharksData$Age)[levels(sharksData$Age) == ""] <- "Unknown"
levels(sharksData$Age)[levels(sharksData$Age) == " "] <- "Unknown"
levels(sharksData$Age)[levels(sharksData$Age) == "  "] <- "Unknown"
levels(sharksData$Age)[5] <- "Unknown"
levels(sharksData$Age)[levels(sharksData$Age) == " 28"] <- "28"
levels(sharksData$Age)[levels(sharksData$Age) == " 30"] <- "30"
levels(sharksData$Age)[levels(sharksData$Age) == "20 "] <- "20"
levels(sharksData$Age)[levels(sharksData$Age) == "20?"] <- "20"
levels(sharksData$Age)[levels(sharksData$Age) == " 43"] <- "43"
levels(sharksData$Age)[levels(sharksData$Age) == "74 "] <- "74"

levels(sharksData$Age)[levels(sharksData$Age) == "\"middle-age\""] <- "Adult"
levels(sharksData$Age)[levels(sharksData$Age) == "\"young\""] <- "Young"
levels(sharksData$Age)[levels(sharksData$Age) == "young"] <- "Young"
levels(sharksData$Age)[levels(sharksData$Age) == "(adult)"] <- "Adult"
levels(sharksData$Age)[levels(sharksData$Age) == "?    &   14"] <- "Teen"
levels(sharksData$Age)[levels(sharksData$Age) == "? & 19"] <- "Teen"
levels(sharksData$Age)[levels(sharksData$Age) == "18 months"] <- "Baby"
levels(sharksData$Age)[levels(sharksData$Age) == "9 months"] <- "Baby"
levels(sharksData$Age)[levels(sharksData$Age) == "2 to 3 months"] <- "Baby"
levels(sharksData$Age)[levels(sharksData$Age) == "2½"] <- "2"
levels(sharksData$Age)[levels(sharksData$Age) == "6½"] <- "6"
levels(sharksData$Age)[levels(sharksData$Age) == "Both 11"] <- "11"
levels(sharksData$Age)[levels(sharksData$Age) == ">50"] <- "50"
levels(sharksData$Age)[levels(sharksData$Age) == "MAKE LINE GREEN"] <- "Unknown"
levels(sharksData$Age)[levels(sharksData$Age) == "N/A"] <- "Unknown"
levels(sharksData$Age)[levels(sharksData$Age) == "Ca. 33"] <- "33"
levels(sharksData$Age)[levels(sharksData$Age) == "teen"] <- "Teen"
levels(sharksData$Age)[levels(sharksData$Age) == "Teens"] <- "Teen"
levels(sharksData$Age)[levels(sharksData$Age) == "F"] <- "Unknown"
levels(sharksData$Age)[levels(sharksData$Age) == "M"] <- "Unknown"
levels(sharksData$Age)[levels(sharksData$Age) == "adult"] <- "Adult"
levels(sharksData$Age)[levels(sharksData$Age) == "A.M."] <- "Unknown"
levels(sharksData$Age)[levels(sharksData$Age) == "X"] <- "Unknown"

levels(sharksData$Age)[levels(sharksData$Age) == "20s"] <- "25"
levels(sharksData$Age)[levels(sharksData$Age) == "30s"] <- "35"
levels(sharksData$Age)[levels(sharksData$Age) == "40s"] <- "45"
levels(sharksData$Age)[levels(sharksData$Age) == "50s"] <- "55"
levels(sharksData$Age)[levels(sharksData$Age) == "60's"] <- "65"
levels(sharksData$Age)[levels(sharksData$Age) == "mid-20s"] <- "25"
levels(sharksData$Age)[levels(sharksData$Age) == "mid-30s"] <- "35"

levels(sharksData$Age)[levels(sharksData$Age) == "8 or 10"] <- "9"
levels(sharksData$Age)[levels(sharksData$Age) == "9 or 10"] <- "9"
levels(sharksData$Age)[levels(sharksData$Age) == "10 or 12"] <- "11"
levels(sharksData$Age)[levels(sharksData$Age) == "12 or 13"] <- "12"
levels(sharksData$Age)[levels(sharksData$Age) == "13 or 14"] <- "13"
levels(sharksData$Age)[levels(sharksData$Age) == "13 or 18"] <- "15"
levels(sharksData$Age)[levels(sharksData$Age) == "18 or 20"] <- "19"
levels(sharksData$Age)[levels(sharksData$Age) == "21 or 26"] <- "23"
levels(sharksData$Age)[levels(sharksData$Age) == "25 or 28"] <- "26"
levels(sharksData$Age)[levels(sharksData$Age) == "30 or 36"] <- "33"
levels(sharksData$Age)[levels(sharksData$Age) == "31 or 33"] <- "32"
levels(sharksData$Age)[levels(sharksData$Age) == "33 or 37"] <- "35"
levels(sharksData$Age)[levels(sharksData$Age) == "7 or 8"] <- "7"

levels(sharksData$Age)[levels(sharksData$Age) == "18 to 22"] <- "20"
levels(sharksData$Age)[levels(sharksData$Age) == "16 to 18"] <- "17"
levels(sharksData$Age)[levels(sharksData$Age) == "25 to 35"] <- "30"

levels(sharksData$Age)[levels(sharksData$Age) == "17 & 16"] <- "17"
levels(sharksData$Age)[levels(sharksData$Age) == "17 & 35"] <- "17"
levels(sharksData$Age)[levels(sharksData$Age) == "23 & 20"] <- "22"
levels(sharksData$Age)[levels(sharksData$Age) == "23 & 26"] <- "25"
levels(sharksData$Age)[levels(sharksData$Age) == "28 & 26"] <- "27"
levels(sharksData$Age)[levels(sharksData$Age) == "30 & 32"] <- "31"
levels(sharksData$Age)[levels(sharksData$Age) == "32 & 30"] <- "31"
levels(sharksData$Age)[levels(sharksData$Age) == "33 & 26"] <- "30"
levels(sharksData$Age)[levels(sharksData$Age) == "33 & 37"] <- "35"
levels(sharksData$Age)[levels(sharksData$Age) == "34 & 19"] <- "27"
levels(sharksData$Age)[levels(sharksData$Age) == "36 & 23"] <- "36"
levels(sharksData$Age)[levels(sharksData$Age) == "36 & 26"] <- "32"
levels(sharksData$Age)[levels(sharksData$Age) == "46 & 34"] <- "40"
levels(sharksData$Age)[levels(sharksData$Age) == "50 & 30"] <- "40"
levels(sharksData$Age)[levels(sharksData$Age) == "9 & 12"] <- "10"
levels(sharksData$Age)[levels(sharksData$Age) == "21 & ?"] <- "21"
levels(sharksData$Age)[levels(sharksData$Age) == "28, 23 & 30"] <- "27"
levels(sharksData$Age)[levels(sharksData$Age) == "21, 34,24 & 35"] <- "29"
levels(sharksData$Age)[levels(sharksData$Age) == "37, 67, 35, 27,  ? & 27"] <- "39"
levels(sharksData$Age)[levels(sharksData$Age) == "7      &    31"] <- "7"

#[Baby{0-4}, Kid{5-13}, Teen{14-18}, Young{19-30} , Adult{31-49}, Elderly{50-100}]
levels(sharksData$Age)[as.numeric(levels(sharksData$Age)) < 5 & 
                         !is.na(as.numeric(levels(sharksData$Age)))] <- "Baby"
levels(sharksData$Age)[as.numeric(levels(sharksData$Age)) > 4 & 
                         as.numeric(levels(sharksData$Age)) < 14 &
                         !is.na(as.numeric(levels(sharksData$Age)))] <- "Kid"
levels(sharksData$Age)[as.numeric(levels(sharksData$Age)) > 13 & 
                         as.numeric(levels(sharksData$Age)) < 19 &
                         !is.na(as.numeric(levels(sharksData$Age)))] <- "Teen"
levels(sharksData$Age)[as.numeric(levels(sharksData$Age)) > 18 & 
                         as.numeric(levels(sharksData$Age)) < 31 &
                         !is.na(as.numeric(levels(sharksData$Age)))] <- "Young"
levels(sharksData$Age)[as.numeric(levels(sharksData$Age)) > 30 & 
                         as.numeric(levels(sharksData$Age)) < 50 &
                         !is.na(as.numeric(levels(sharksData$Age)))] <- "Adult"
levels(sharksData$Age)[as.numeric(levels(sharksData$Age)) > 49 &
                         !is.na(as.numeric(levels(sharksData$Age)))] <- "Elderly"
levels(sharksData$Age)




####VAriabbles a transformar
##Añadir transformaciones pasadas al documento
#Case.number + date -> month
#Maybe Country -> Per continent?
#Area -> Demasiada cardinalidad
#Actividad -> demasiada cardinalidad
#Age -> Agrupar en rangos de edad [Baby{0-4}, Kid{5-13}, Teen{14-18}, Young{19-30} , Adult{31-49}, Elderly{50-100}]
# Baby 0-4, Kid
#Time -> Agrupar en rangos [Madrugada, Mañana, Medio dia, tarde, noche]
#Species -> Text para entcontar especies de tiburones






