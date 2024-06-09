# Bachelorarbeit Maja Ziemer, Datenaufbereitung Ergebnisse von BirdNET

#### 1. GRUNDLAGEN ####
library(readr)
library(stringr)
library(dplyr)
library(strex)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(gridExtra)

# Vogelarten
NR <- "Nycticorax nycticorax, Nachtreiher"
WR <- "Rallus aquaticus, Wasserralle"
DRS <- "Acrocephalus arundinaceus, Drosselrohrsänger"
ZD <- "Ixobrychus minutus, Zwergdommel"
OF <- "Lithobates catesbeianus, American Bullfrog"

# Zeitraume 2023-04-18 bis 2023-10- (jew. 00 Uhr)
Start <- as.POSIXct("2023-04-25000000+0200", tz= "utc", 
                    format = "%Y-%m-%d%H%M%OS%z")
Ende <- as.POSIXct("2023-10-01000000+0200", tz= "utc", 
                   format = "%Y-%m-%d%H%M%OS%z")
Kartenwechsel <- as.POSIXct("2023-08-12120000+0200", tz= "utc", 
                                     format = "%Y-%m-%d%H%M%OS%z")

#### 2. ZUSAMMENFUEGEN DER DATEN ####
# Daten einlesen
detections = list.files("01_raw_data/") # 16053 Eintraege

# Meta-Daten extrahieren und zu Rekordername und Datum in Dataframe fuegen
detections = as.data.frame(detections) %>%
  rename("path" = 1) %>%
  mutate(file_name = basename(path),
         recorder= strex::str_before_first(file_name,"_"),
         date = strex::str_after_last(file_name, pattern = "_") %>%
           strex::str_before_first(".B") %>% str_remove("T") %>%
           as.POSIXct(date,tz= "utc", format = "%Y-%m-%d%H%M%OS%z"))

# Korrektur Datum "date" um +2 Stunden
detections$date <- detections$date + 7200

# Spalte "Rek" hinzufügen, Bedingung: dazugehoerendem Rekorder 
detections <- detections %>% 
  mutate(Rek = ifelse(recorder == "000000005d0f3177", 1,
               ifelse(recorder == "00000000537ecd88", 2, 
               ifelse(recorder == "00000000f252eb0a", 3,
               ifelse(recorder == "00000000b523b4d4", 4, 
               NA)))))

# datections nur für den Zeitraum betrachten, in dem die Rekorder im Gebiet waren
detections <- detections %>%  filter(date >= Start) # 15854 (alte anzahl an detetctions: 16053)

# Dataframes fuer det_total erstellen
det_total <- data.frame()


# Alle txt-Dateien aus Ordner 01_raw_data einlesen und aneinanderhaengen in 
# einen Datag´frame
for (i in 1:length(detections$path)) {
  a <- readr::read_delim(paste0("01_raw_data/", detections$path[i]),
                                 delim = "\t", escape_double = FALSE, 
                                 col_names =  c("start_time", "end_time", 
                                                "spec", "confidence"),
                                 col_types = c("d", "d", "c", "d"),
                                 trim_ws = TRUE) %>%
    mutate(file_name = detections$file_name[i])
  det_total <- bind_rows(det_total, a)
}                                         # 419555 Eintraege

# Metadaten anhängen an det_total
det_total <- left_join(det_total, detections, by="file_name")
names(det_total)[names(det_total) == "date"] <- "date_time"

# doppelte Spalten herausnehmen (det_tatal$path)
det_total <- det_total %>% select(-path)

# richtge Uhrzeit der Detaktion als Spalte date_time_real einfuegen
det_total$date_time_real <- as.POSIXct(det_total$date_time)
det_total$date_time_real <- det_total$date_time_real + det_total$start_time


#### 2.1 Metadaten ####
# Anzahl an files pro Rekorder im Zeitraum
#detections_1 <- detections %>%  filter(Rek == 1) # 3486
#detections_2 <- detections %>%  filter(Rek == 2) # 4495
#detections_3 <- detections %>%  filter(Rek == 3) # 3109
#detections_4 <- detections %>%  filter(Rek == 4) # 4761  
#rm(detections_1, detections_2, detections_3, detections_4)
# Summe = 15851 (es haben wohl 3 files keine eindeutige Kennung)


#### 3. KLANGATTRAPPE  ####
#### 3.1 KA Aufbereitung ####

# Zeitraume in denen Klangattrappe abgespielt wurde als csv-Datei einlesen
ka <- read.csv(file = "F:/BA/BirdNET/Arbeitsordner/DATENVERARBEITUNG/
               03_klangattrappe/Zeiten_Klangattrappe.csv", sep = ";")

# Artnamen korrigieren
ka$spec[which(ka$spec == "ZD")] <- "Ixobrychus minutus, Zwergdommel"
ka$spec[which(ka$spec == "WR")] <- "Rallus aquaticus, Wasserralle"

# Spaltenueberschriften benennen
colnames(ka) <- c("Rek", "date", "time_start", "time_end", "spec")

# Zusammenfuehren der Spalten date und date_start bzw. date_end
ka$date_time_start <- paste(ka$date, ka$time_start) 
ka$date_time_end <- paste(ka$date, ka$time_end)

# Loeschen der urspruenglichen Spalten
ka <- ka[, -c(2,3,4)]

#Datum in richtiges Format
ka$date_time_start <- gsub("\\.", "-", ka$date_time_start)
ka$date_time_end <- gsub("\\.", "-", ka$date_time_end)

# richtiges Zeitformat einstellen
ka$date_time_start <- as.POSIXct(ka$date_time_start, tz = "utc", 
                                 format = "%d-%m-%Y %H:%M:%S")
ka$date_time_end <- as.POSIXct(ka$date_time_end, tz = "utc", 
                               format = "%d-%m-%Y %H:%M:%S")


#### 3.2 KA ausschliessen ####
# An det_total eine neue Spalte anhaengen um Klangattrappen auszuschliessen
det_total$KA <- NaN

# Identifizieren von Zeilen in det_total, in denen die Klangattrappe abgespielt 
# wurde, spec und Rek muessen ebenfalls uebereinstimmen
# 0: Detektion nicht im Zeitraum der Abgespielten KA, 
# 1: Detektion im Zeitraum der abgespielten KA

det_total <- det_total %>%
  rowwise() %>%
  mutate(KA = ifelse(
    any(
      1 <= sum(date_time_real %within% interval(ka$date_time_start, ka$date_time_end)) &
        ka$Rek == Rek &
        ka$spec == spec),
    1, 0)) 
# 324 als KA = 1 markiert

#### 4. Ochsenfrosch ####
#### 4.1 data OF einlesen ####
detections_o = list.files("06_ochsenfrosch/")

# Meta-Daten extrahieren und zu Rekordername und Datum in Dataframe fuegen
detections_o = as.data.frame(detections_o) %>%
  rename("path" = 1) %>%
  mutate(file_name = basename(path),
         recorder= strex::str_before_first(file_name,"_"),
         date = strex::str_after_last(file_name, pattern = "_") %>%
           strex::str_before_first(".B") %>% str_remove("T") %>%
           as.POSIXct(date,tz= "utc", format = "%Y-%m-%d%H%M%OS%z")) 

# Korrektur Datum "date" um +2 Stunden
detections_o$date <- detections_o$date + 7200

# Spalte "Rek" hinzufügen, Bedingung: dazugehoerender Rekorder 
detections_o <- detections_o %>% 
  mutate(Rek = ifelse(recorder == "000000005d0f3177", 1,
               ifelse(recorder == "00000000537ecd88", 2, 
               ifelse(recorder == "00000000f252eb0a", 3,
               ifelse(recorder == "00000000b523b4d4", 4, 
               NA)))))

# datections nur für den Zeitraum betrachten, in dem die Rekorder im Gebiet waren
detections_o <- detections_o %>%  filter(date >= Start) # 15910 detections

# Dataframes fuer det_total erstellen
det_ochsenfrosch <- data.frame()

# Alle txt-Dateien aus Ordner 06_raw_data einlesen und aneinanderhaengen in 
# einen Dataframe
for (i in 1:length(detections_o$path)) {
  a <- readr::read_delim(paste0("06_ochsenfrosch/", detections_o$path[i]),
                         delim = "\t", escape_double = FALSE, 
                         col_names =  c("start_time", "end_time", 
                                        "spec", "confidence"),
                         col_types = c("d", "d", "c", "d"),
                         trim_ws = TRUE) %>% mutate(file_name = detections_o$file_name[i])
  det_ochsenfrosch <- bind_rows(det_ochsenfrosch, a)
}

# Metadaten anhaengen an det_total
det_ochsenfrosch <- left_join(det_ochsenfrosch, detections_o, by="file_name")
names(det_ochsenfrosch)[names(det_ochsenfrosch) == "date"] <- "date_time"

# Dataframe mit Ochsenfrosch erstellen
OF <- det_ochsenfrosch %>%
  filter(confidence >= 0.5, spec == OF) # 299 Eintraege

# richtiges Format Zeitzone
OF$date_time_real <- OF$date_time + OF$start_time


#### 5. FILTER Detektionen Gesamtzeitraum ####
# Filter DRS Gesamtzeitraum
F_01_DRS <- det_total %>% filter(confidence >= 0.5, 
            between(date_time, Start, Ende), spec == DRS, Rek == 1) # 0
F_02_DRS <- det_total %>% filter(confidence >= 0.5, 
            between(date_time, Start, Ende), spec == DRS, Rek == 2) # 204
F_03_DRS <- det_total %>% filter(confidence >= 0.5, 
            between(date_time, Start, Ende), spec == DRS, Rek == 3) # 42
F_04_DRS <- det_total %>% filter(confidence >= 0.5, 
            between(date_time, Start, Ende), spec == DRS, Rek == 4) # 33
rm(F_01_DRS, F_02_DRS, F_03_DRS, F_04_DRS)

# Filter NR Gesamtzeitraum
F_01_NR <- det_total %>% filter(confidence >= 0.5, 
           between(date_time, Start, Ende), spec == NR, Rek == 1) # 180
F_02_NR <- det_total %>% filter(confidence >= 0.5, 
           between(date_time, Start, Ende), spec == NR, Rek == 2) # 292
F_03_NR <- det_total %>% filter(confidence >= 0.5, 
           between(date_time, Start, Ende), spec == NR, Rek == 3) # 236
F_04_NR <- det_total %>% filter(confidence >= 0.5, 
           between(date_time, Start, Ende), spec == NR, Rek == 4) # 177
rm(F_01_NR, F_02_NR, F_03_NR, F_04_NR)

# Filter WR Gesamtzeitraum (inkl. KA/KA ausgeschlossen)
F_01_WR <- det_total %>% filter(confidence >= 0.5, 
           between(date_time, Start, Ende), spec == WR, Rek == 1) # 4/4
F_02_WR <- det_total %>% filter(confidence >= 0.5, 
           between(date_time, Start, Ende), spec == WR, Rek == 2) # 33/3
F_03_WR <- det_total %>% filter(confidence >= 0.5, 
           between(date_time, Start, Ende), spec == WR, Rek == 3) # 682/682
F_04_WR <- det_total %>% filter(confidence >= 0.5, 
           between(date_time, Start, Ende), spec == WR, Rek == 4) # 119/119
rm(F_01_WR, F_02_WR, F_03_WR, F_04_WR)

# Filter ZD  Gesamtzeitraum (inkl. KA)
F_01_ZD <- det_total %>% filter(confidence >= 0.5, 
           between(date_time, Start, Ende), spec == ZD, Rek == 1) # 2/2
F_02_ZD <- det_total %>% filter(confidence >= 0.5, 
           between(date_time, Start, Ende), spec == ZD, Rek == 2) # 7768/7474
F_03_ZD <- det_total %>% filter(confidence >= 0.5, 
           between(date_time, Start, Ende), spec == ZD, Rek == 3) # 38/38
F_04_ZD <- det_total %>% filter(confidence >= 0.5, 
           between(date_time, Start, Ende), spec == ZD, Rek == 4) # 52
rm(F_01_ZD, F_02_ZD, F_03_ZD, F_04_ZD)
 

#### 6. Plausibilisierung/Validierung ####
#### 6.1 Listen Plausibilisierung ####
# Plausibilisierung der Arten, Intervallbreite von 0.1 B: >= 0.5 bis < 0.6
# bei mehr als 40 Detektionen: randomisiert 40 Detektionen herausgesuchen und in 
# Dataframe speichern

#### 6.1.1 DRS-Liste ####
# DRS_01 (0 Detektionen)

# DRS Rekorder 02
F_02_DRS_0.5 <- det_total %>% filter(confidence >= 0.5, confidence < 0.6, 
                                     between(date_time, Start, Ende), 
                                     spec == DRS, Rek == 2) # 92
 # mehr als 40 Detektionen: 40 random entries (re) auswaehlen
re_F_02_DRS_0.5 = sample_n(F_02_DRS_0.5, 40)

F_02_DRS_0.6 <- det_total %>% filter(confidence >= 0.6, confidence < 0.7, 
                                     between(date_time, Start, Ende), 
                                     spec == DRS, Rek == 2) # 51
# mehr als 40 Detektionen: 40 random entries (re) auswaehlen
re_F_02_DRS_0.6 = sample_n(F_02_DRS_0.6, 40)

F_02_DRS_0.7 <- det_total %>% filter(confidence >= 0.7, confidence < 0.8, 
                                     between(date_time, Start, Ende), 
                                     spec == DRS, Rek == 2) # 19

F_02_DRS_0.8 <- det_total %>% filter(confidence >= 0.8, confidence < 0.9, 
                                     between(date_time, Start, Ende), 
                                     spec == DRS, Rek == 2) # 22

F_02_DRS_0.9 <- det_total %>% filter(confidence >= 0.9, confidence <=1, 
                                     between(date_time, Start, Ende), 
                                     spec == DRS, Rek == 2) # 20

# ein Dataframe mit DRS_02 erstellen, mit jew. max. 40 Detektionen
DRS_02 <- bind_rows(re_F_02_DRS_0.5, re_F_02_DRS_0.6, F_02_DRS_0.7, 
                    F_02_DRS_0.8, F_02_DRS_0.9) # 141 Eintraege

# als csv-Datei "DRS_02.csv" abspeichern
write.csv(DRS_02, file = "D:/BA/BirdNET/Arbeitsordner/DATENVERARBEITUNG/
          05_plausi_arten/DRS_02.csv", na = "NA", row.names = FALSE)

rm(F_02_DRS_0.5, F_02_DRS_0.6, F_02_DRS_0.7, F_02_DRS_0.8, F_02_DRS_0.9, 
   re_F_02_DRS_0.5, re_F_02_DRS_0.6, DRS_02)

# DRS Rekorder 03
F_03_DRS_0.5 <- det_total %>% filter(confidence >= 0.5, confidence < 0.6, 
                                     between(date_time, Start, Ende), 
                                     spec == DRS, Rek == 3) # 18

F_03_DRS_0.6 <- det_total %>% filter(confidence >= 0.6, confidence < 0.7, 
                                     between(date_time, Start, Ende), 
                                     spec == DRS, Rek == 3) # 15

F_03_DRS_0.7 <- det_total %>% filter(confidence >= 0.7, confidence < 0.8, 
                                     between(date_time, Start, Ende), 
                                     spec == DRS, Rek == 3) # 5

F_03_DRS_0.8 <- det_total %>% filter(confidence >= 0.8, confidence < 0.9, 
                                     between(date_time, Start, Ende), 
                                     spec == DRS, Rek == 3) # 1

F_03_DRS_0.9 <- det_total %>% filter(confidence >= 0.9, confidence <=1, 
                                     between(date_time, Start, Ende), 
                                     spec == DRS, Rek == 3) # 3

# ein Dataframe mit DRS_03 erstellen, mit jew. max. 40 Detektionen
DRS_03 <- bind_rows(F_03_DRS_0.5, F_03_DRS_0.6, F_03_DRS_0.7, 
                    F_03_DRS_0.8, F_03_DRS_0.9) # 42 Eintraege

# als csv-Datei "DRS_03.csv" abspeichern
write.csv(DRS_03, file = "D:/BA/BirdNET/Arbeitsordner/DATENVERARBEITUNG/
          05_plausi_arten/DRS_03.csv", na = "NA", row.names = FALSE)
rm(F_03_DRS_0.5, F_03_DRS_0.6, F_03_DRS_0.7, F_03_DRS_0.8, F_03_DRS_0.9, DRS_03)

# DRS Rekorder 04
F_04_DRS_0.5 <- det_total %>% filter(confidence >= 0.5, confidence < 0.6, 
                                     between(date_time, Start, Ende), 
                                     spec == DRS, Rek == 4) # 22

F_04_DRS_0.6 <- det_total %>% filter(confidence >= 0.6, confidence < 0.7, 
                                     between(date_time, Start, Ende), 
                                     spec == DRS, Rek == 4) # 6

F_04_DRS_0.7 <- det_total %>% filter(confidence >= 0.7, confidence < 0.8, 
                                     between(date_time, Start, Ende), 
                                     spec == DRS, Rek == 4) # 4

F_04_DRS_0.8 <- det_total %>% filter(confidence >= 0.8, confidence < 0.9, 
                                     between(date_time, Start, Ende), 
                                     spec == DRS, Rek == 4) # 0

F_04_DRS_0.9 <- det_total %>% filter(confidence >= 0.9, confidence <=1, 
                                     between(date_time, Start, Ende), 
                                     spec == DRS, Rek == 4) # 1

# ein Dataframe mit DRS_03 erstellen, mit jew. max. 40 Detektionen
DRS_04 <- bind_rows(F_04_DRS_0.5, F_04_DRS_0.6, F_04_DRS_0.7, 
                    F_04_DRS_0.8, F_04_DRS_0.9) # 33 Eintraege

# als csv-Datei "DRS_04.csv" abspeichern
write.csv(DRS_04, file = "D:/BA/BirdNET/Arbeitsordner/DATENVERARBEITUNG/
          05_plausi_arten/DRS_04.csv", na = "NA", row.names = FALSE)
rm(F_04_DRS_0.5, F_04_DRS_0.6, F_04_DRS_0.7, F_04_DRS_0.8, F_04_DRS_0.9, DRS_04)


#### 6.1.2 WR-Liste #### 
# WR Rekorder 01
F_01_WR_0.5 <- det_total %>% filter(confidence >= 0.5, confidence < 0.6, 
                                     between(date_time, Start, Ende), 
                                     spec == WR, Rek == 1, KA == 0) # 2

F_01_WR_0.6 <- det_total %>% filter(confidence >= 0.6, confidence < 0.7, 
                                     between(date_time, Start, Ende), 
                                     spec == WR, Rek == 1, KA == 0) # 1

F_01_WR_0.7 <- det_total %>% filter(confidence >= 0.7, confidence < 0.8, 
                                     between(date_time, Start, Ende), 
                                     spec == WR, Rek == 1, KA == 0) # 0

F_01_WR_0.8 <- det_total %>% filter(confidence >= 0.8, confidence < 0.9, 
                                     between(date_time, Start, Ende), 
                                     spec == WR, Rek == 1, KA == 0) # 0

F_01_WR_0.9 <- det_total %>% filter(confidence >= 0.9, confidence <=1, 
                                     between(date_time, Start, Ende), 
                                     spec == WR, Rek == 1, KA == 0) # 1

# ein Dataframe mit WR_01 erstellen, mit jew. max. 40 Detektionen
WR_01 <- bind_rows(F_01_WR_0.5, F_01_WR_0.6, F_01_WR_0.7, 
                    F_01_WR_0.8, F_01_WR_0.9) # 4 Eintraege

# als csv-Datei "WR_01.csv" abspeichern
write.csv(WR_01, file = "D:/BA/BirdNET/Arbeitsordner/DATENVERARBEITUNG/
          05_plausi_arten/WR_01.csv", na = "NA", row.names = FALSE)
rm(F_01_WR_0.5, F_01_WR_0.6, F_01_WR_0.7, F_01_WR_0.8, F_01_WR_0.9, WR_01)

# WR Rekorder 02 (ohne Ausschluss KA/ mit Ausschluss KA)
# nur an diesem Rekorder WR-Klangattrappe erkannt
F_02_WR_0.5 <- det_total %>% filter(confidence >= 0.5, confidence < 0.6, 
                                         between(date_time, Start, Ende), 
                                         spec == WR, Rek == 2, KA == 0) # 6/4

F_02_WR_0.6 <- det_total %>% filter(confidence >= 0.6, confidence < 0.7, 
                                    between(date_time, Start, Ende), 
                                    spec == WR, Rek == 2, KA == 0) # 5/3

F_02_WR_0.7 <- det_total %>% filter(confidence >= 0.7, confidence < 0.8, 
                                    between(date_time, Start, Ende), 
                                    spec == WR, Rek == 2, KA == 0) # 5/3

F_02_WR_0.8 <- det_total %>% filter(confidence >= 0.8, confidence < 0.9, 
                                    between(date_time, Start, Ende), 
                                    spec == WR, Rek == 2, KA == 0) # 9/6

F_02_WR_0.9 <- det_total %>% filter(confidence >= 0.9, confidence <=1, 
                                    between(date_time, Start, Ende), 
                                    spec == WR, Rek == 2, KA == 0) # 8/3

# ein Dataframe mit WR_02 erstellen, mit jew. max. 40 Detektionen
WR_02 <- bind_rows(F_02_WR_0.5, F_02_WR_0.6, F_02_WR_0.7, 
                   F_02_WR_0.8, F_02_WR_0.9) # 19 Eintraege

# als csv-Datei "WR_02.csv" abspeichern
write.csv(WR_02, file = "D:/BA/BirdNET/Arbeitsordner/DATENVERARBEITUNG/
          05_plausi_arten/WR_02.csv", na = "NA", row.names = FALSE)
rm(F_02_WR_0.5, F_02_WR_0.6, F_02_WR_0.7, F_02_WR_0.8, F_02_WR_0.9, WR_02)

# WR Rekorder 03
F_03_WR_0.5 <- det_total %>% filter(confidence >= 0.5, confidence < 0.6, 
                                         between(date_time, Start, Ende), 
                                         spec == WR, Rek == 3, KA == 0) # 126
# mehr als 40 Detektionen: 40 random entries (re) auswaehlen
re_F_03_WR_0.5 <- sample_n(F_03_WR_0.5, 40, replace = TRUE)

F_03_WR_0.6 <- det_total %>% filter(confidence >= 0.6, confidence < 0.7, 
                                    between(date_time, Start, Ende), 
                                    spec == WR, Rek == 3, KA == 0) # 133
# mehr als 40 Detektionen: 40 random entries (re) auswaehlen
re_F_03_WR_0.6 = sample_n(F_03_WR_0.6, 40)

F_03_WR_0.7 <- det_total %>% filter(confidence >= 0.7, confidence < 0.8, 
                                    between(date_time, Start, Ende), 
                                    spec == WR, Rek == 3, KA == 0) # 104
# mehr als 40 Detektionen: 40 random entries (re) auswaehlen
re_F_03_WR_0.7 = sample_n(F_03_WR_0.7, 40)

F_03_WR_0.8 <- det_total %>% filter(confidence >= 0.8, confidence < 0.9, 
                                    between(date_time, Start, Ende), 
                                    spec == WR, Rek == 3, KA == 0) # 137
# mehr als 40 Detektionen: 40 random entries (re) auswaehlen
re_F_03_WR_0.8 = sample_n(F_03_WR_0.8, 40)

F_03_WR_0.9 <- det_total %>% filter(confidence >= 0.9, confidence <=1, 
                                    between(date_time, Start, Ende), 
                                    spec == WR, Rek == 3, KA == 0) # 182
# mehr als 40 Detektionen: 40 random entries (re) auswaehlen
re_F_03_WR_0.9 = sample_n(F_03_WR_0.9, 40)

# ein Dataframe mit WR_03 erstellen, mit jew. max. 40 Detektionen
WR_03 <- bind_rows(re_F_03_WR_0.5, re_F_03_WR_0.6, re_F_03_WR_0.7, 
                   re_F_03_WR_0.8, re_F_03_WR_0.9) # 200 Eintraege

# als csv-Datei "WR_03.csv" abspeichern
write.csv(WR_03, file = "D:/BA/BirdNET/Arbeitsordner/DATENVERARBEITUNG/
          05_plausi_arten/WR_03.csv", na = "NA", row.names = FALSE)
rm(F_03_WR_0.5, F_03_WR_0.6, F_03_WR_0.7, F_03_WR_0.8, F_03_WR_0.9,re_F_03_WR_0.5, 
   re_F_03_WR_0.6, re_F_03_WR_0.7, re_F_03_WR_0.8, re_F_03_WR_0.9, WR_03)

# WR Rekorder 04
F_04_WR_0.5 <- det_total %>% filter(confidence >= 0.5, confidence < 0.6, 
                                         between(date_time, Start, Ende), 
                                         spec == WR, Rek == 4, KA == 0) # 25

F_04_WR_0.6 <- det_total %>% filter(confidence >= 0.6, confidence < 0.7, 
                                    between(date_time, Start, Ende), 
                                    spec == WR, Rek == 4, KA == 0) # 10

F_04_WR_0.7 <- det_total %>% filter(confidence >= 0.7, confidence < 0.8, 
                                    between(date_time, Start, Ende), 
                                    spec == WR, Rek == 4, KA == 0) # 16

F_04_WR_0.8 <- det_total %>% filter(confidence >= 0.8, confidence < 0.9, 
                                    between(date_time, Start, Ende), 
                                    spec == WR, Rek == 4, KA == 0) # 14

F_04_WR_0.9 <- det_total %>% filter(confidence >= 0.9, confidence <=1, 
                                    between(date_time, Start, Ende), 
                                    spec == WR, Rek == 4, KA == 0) # 54
# mehr als 40 Detektionen: 40 random entries (re) auswaehlen
re_F_04_WR_0.9 = sample_n(F_04_WR_0.9, 40)

# ein Dataframe mit WR_04 erstellen, mit jew. max. 40 Detektionen
WR_04 <- bind_rows(F_04_WR_0.5, F_04_WR_0.6, F_04_WR_0.7, 
                   F_04_WR_0.8, re_F_04_WR_0.9) # 105 Eintraege

# als csv-Datei "DRS_04.csv" abspeichern
write.csv(WR_04, file = "D:/BA/BirdNET/Arbeitsordner/DATENVERARBEITUNG/
          05_plausi_arten/WR_04.csv", na = "NA", row.names = FALSE)
rm(F_04_WR_0.5, F_04_WR_0.6, F_04_WR_0.7, F_04_WR_0.8, F_04_WR_0.9, 
   re_F_04_WR_0.9, WR_04)


#### 6.1.3 NR-Liste #### 
# NR Rekorder 01
F_01_NR_0.5 <- det_total %>% filter(confidence >= 0.5, confidence < 0.6, 
                                         between(date_time, Start, Ende), 
                                         spec == NR, Rek == 1) # 64
# mehr als 40 Detektionen: 40 random entries (re) auswaehlen
re_F_01_NR_0.5 = sample_n(F_01_NR_0.5, 40)

F_01_NR_0.6 <- det_total %>% filter(confidence >= 0.6, confidence < 0.7, 
                                    between(date_time, Start, Ende), 
                                    spec == NR, Rek == 1) # 45
# mehr als 40 Detektionen: 40 random entries (re) auswaehlen
re_F_01_NR_0.6 = sample_n(F_01_NR_0.6, 40)

F_01_NR_0.7 <- det_total %>% filter(confidence >= 0.7, confidence < 0.8, 
                                    between(date_time, Start, Ende), 
                                    spec == NR, Rek == 1) # 32

F_01_NR_0.8 <- det_total %>% filter(confidence >= 0.8, confidence < 0.9, 
                                    between(date_time, Start, Ende), 
                                    spec == NR, Rek == 1) # 24

F_01_NR_0.9 <- det_total %>% filter(confidence >= 0.9, confidence <=1, 
                                    between(date_time, Start, Ende), 
                                    spec == NR, Rek == 1) # 14

# Dataframe mit NR_01 erstellen, mit jew. max. 40 Detektionen
NR_01 <- bind_rows(re_F_01_NR_0.5, re_F_01_NR_0.6, F_01_NR_0.7, 
                   F_01_NR_0.8, F_01_NR_0.9) # 150 Eintraege

# als csv-Datei "NR_01.csv" abspeichern
write.csv(NR_01, file = "D:/BA/BirdNET/Arbeitsordner/DATENVERARBEITUNG/
          05_plausi_arten/NR_01.csv", na = "NA", row.names = FALSE)
rm(F_01_NR_0.5, F_01_NR_0.6, F_01_NR_0.7, F_01_NR_0.8, F_01_NR_0.9, 
   re_F_01_NR_0.5, re_F_01_NR_0.6, NR_01)

# NR Rekorder 02
F_02_NR_0.5 <- det_total %>% filter(confidence >= 0.5, confidence < 0.6, 
                                    between(date_time, Start, Ende), 
                                    spec == NR, Rek == 2) # 58
# mehr als 40 Detektionen: 40 random entries (re) auswaehlen
re_F_02_NR_0.5 = sample_n(F_02_NR_0.5, 40)

F_02_NR_0.6 <- det_total %>% filter(confidence >= 0.6, confidence < 0.7, 
                                    between(date_time, Start, Ende), 
                                    spec == NR, Rek == 2) # 57
# mehr als 40 Detektionen: 40 random entries (re) auswaehlen
re_F_02_NR_0.6 = sample_n(F_02_NR_0.6, 40)

F_02_NR_0.7 <- det_total %>% filter(confidence >= 0.7, confidence < 0.8, 
                                    between(date_time, Start, Ende), 
                                    spec == NR, Rek == 2) # 41
# mehr als 40 Detektionen: 40 random entries (re) auswaehlen
re_F_02_NR_0.7 = sample_n(F_02_NR_0.7, 40)

F_02_NR_0.8 <- det_total %>% filter(confidence >= 0.8, confidence < 0.9, 
                                    between(date_time, Start, Ende), 
                                    spec == NR, Rek == 2) # 48
# mehr als 40 Detektionen: 40 random entries (re) auswaehlen
re_F_02_NR_0.8 = sample_n(F_02_NR_0.8, 40)

F_02_NR_0.9 <- det_total %>% filter(confidence >= 0.9, confidence <=1, 
                                    between(date_time, Start, Ende), 
                                    spec == NR, Rek == 2) # 88
# mehr als 40 Detektionen: 40 random entries (re) auswaehlen
re_F_02_NR_0.9 = sample_n(F_02_NR_0.9, 40)

# Dataframe mit NR_02 erstellen, mit jew. max. 40 Detektionen
NR_02 <- bind_rows(re_F_02_NR_0.5, re_F_02_NR_0.6, re_F_02_NR_0.7, 
                   re_F_02_NR_0.8, re_F_02_NR_0.9) # 200 Eintraege

# als csv-Datei "NR_02.csv" abspeichern
write.csv(NR_02, file = "D:/BA/BirdNET/Arbeitsordner/DATENVERARBEITUNG/
          05_plausi_arten/NR_02.csv", na = "NA", row.names = FALSE)
rm(F_02_NR_0.5, F_02_NR_0.6, F_02_NR_0.7, F_02_NR_0.8, F_02_NR_0.9, re_F_02_NR_0.5, 
   re_F_02_NR_0.6, re_F_02_NR_0.7, re_F_02_NR_0.8, re_F_02_NR_0.9, NR_02)

# NR Rekorder 03
F_03_NR_0.5 <- det_total %>% filter(confidence >= 0.5, confidence < 0.6, 
                                    between(date_time, Start, Ende), 
                                    spec == NR, Rek == 3) # 73
# mehr als 40 Detektionen: 40 random entries (re) auswaehlen
re_F_03_NR_0.5 = sample_n(F_03_NR_0.5, 40)

F_03_NR_0.6 <- det_total %>% filter(confidence >= 0.6, confidence < 0.7, 
                                    between(date_time, Start, Ende), 
                                    spec == NR, Rek == 3) # 51
# mehr als 40 Detektionen: 40 random entries (re) auswaehlen
re_F_03_NR_0.6 = sample_n(F_03_NR_0.6, 40)

F_03_NR_0.7 <- det_total %>% filter(confidence >= 0.7, confidence < 0.8, 
                                    between(date_time, Start, Ende), 
                                    spec == NR, Rek == 3) # 43
# mehr als 40 Detektionen: 40 random entries (re) auswaehlen
re_F_03_NR_0.7 = sample_n(F_03_NR_0.7, 40)

F_03_NR_0.8 <- det_total %>% filter(confidence >= 0.8, confidence < 0.9, 
                                    between(date_time, Start, Ende), 
                                    spec == NR, Rek == 3) # 36

F_03_NR_0.9 <- det_total %>% filter(confidence >= 0.9, confidence <=1, 
                                    between(date_time, Start, Ende), 
                                    spec == NR, Rek == 3) # 33

# Dataframe mit NR_03 erstellen, mit jew. max. 40 Detektionen
NR_03 <- bind_rows(re_F_03_NR_0.5, re_F_03_NR_0.6, re_F_03_NR_0.7, 
                   F_03_NR_0.8, F_03_NR_0.9) # 189 Eintraege

# als csv-Datei "NR_03.csv" abspeichern
write.csv(NR_03, file = "D:/BA/BirdNET/Arbeitsordner/DATENVERARBEITUNG/
          05_plausi_arten/NR_03.csv", na = "NA", row.names = FALSE)

rm(F_03_NR_0.5, F_03_NR_0.6, F_03_NR_0.7, F_03_NR_0.8, F_03_NR_0.9, 
   re_F_03_NR_0.5, re_F_03_NR_0.6, re_F_03_NR_0.7, NR_03)

# NR Rekorder 04
F_04_NR_0.5 <- det_total %>% filter(confidence >= 0.5, confidence < 0.6, 
                                    between(date_time, Start, Ende), 
                                    spec == NR, Rek == 4) # 65
# mehr als 40 Detektionen: 40 random entries (re) auswaehlen
re_F_04_NR_0.5 = sample_n(F_04_NR_0.5, 40)

F_04_NR_0.6 <- det_total %>% filter(confidence >= 0.6, confidence < 0.7, 
                                    between(date_time, Start, Ende), 
                                    spec == NR, Rek == 4) # 41
# mehr als 40 Detektionen: 40 random entries (re) auswaehlen
re_F_04_NR_0.6 = sample_n(F_04_NR_0.6, 40)

F_04_NR_0.7 <- det_total %>% filter(confidence >= 0.7, confidence < 0.8, 
                                    between(date_time, Start, Ende), 
                                    spec == NR, Rek == 4) # 22

F_04_NR_0.8 <- det_total %>% filter(confidence >= 0.8, confidence < 0.9, 
                                    between(date_time, Start, Ende), 
                                    spec == NR, Rek == 4) # 25

F_04_NR_0.9 <- det_total %>% filter(confidence >= 0.9, confidence <=1, 
                                    between(date_time, Start, Ende), 
                                    spec == NR, Rek == 4) # 24

# Dataframe mit NR_04 erstellen, mit jew. max. 40 Detektionen
NR_04 <- bind_rows(re_F_04_NR_0.5, re_F_04_NR_0.6, F_04_NR_0.7, 
                   F_04_NR_0.8, F_04_NR_0.9) # 151 Eintraege

# als csv-Datei "NR_04.csv" abspeichern
write.csv(NR_04, file = "D:/BA/BirdNET/Arbeitsordner/DATENVERARBEITUNG/
          05_plausi_arten/NR_04.csv", na = "NA", row.names = FALSE)
rm(F_04_NR_0.5, F_04_NR_0.6, F_04_NR_0.7, F_04_NR_0.8, F_04_NR_0.9, 
   re_F_04_NR_0.5, re_F_04_NR_0.6, NR_04)


#### 6.1.4 ZD-Liste #### 
# ZD Rekorder 01
F_01_ZD_0.5 <- det_total %>% filter(confidence >= 0.5, confidence < 0.6, 
                                         between(date_time, Start, Ende), 
                                         spec == ZD, Rek == 1, KA == 0) # 1

F_01_ZD_0.6 <- det_total %>% filter(confidence >= 0.6, confidence < 0.7, 
                                         between(date_time, Start, Ende), 
                                         spec == ZD, Rek == 1, KA == 0) # 0

F_01_ZD_0.7 <- det_total %>% filter(confidence >= 0.7, confidence < 0.8, 
                                         between(date_time, Start, Ende), 
                                         spec == ZD, Rek == 1, KA == 0) # 1

F_01_ZD_0.8 <- det_total %>% filter(confidence >= 0.8, confidence < 0.9, 
                                         between(date_time, Start, Ende), 
                                         spec == ZD, Rek == 1, KA == 0) # 0

F_01_ZD_0.9 <- det_total %>% filter(confidence >= 0.9, confidence <=1, 
                                         between(date_time, Start, Ende), 
                                         spec == ZD, Rek == 1, KA == 0) # 0

# ein Dataframe mit ZD_01 erstellen, mit jew. max. 40 Detektionen
ZD_01 <- bind_rows(F_01_ZD_0.5, F_01_ZD_0.6, F_01_ZD_0.7, 
                   F_01_ZD_0.8, F_01_ZD_0.9) # 2 Eintraege

# als csv-Datei "ZD_01.csv" abspeichern
write.csv(ZD_01, file = "D:/BA/BirdNET/Arbeitsordner/DATENVERARBEITUNG/
          05_plausi_arten/ZD_01.csv", na = "NA", row.names = FALSE)
rm(F_01_ZD_0.5, F_01_ZD_0.6, F_01_ZD_0.7, F_01_ZD_0.8, F_01_ZD_0.9, ZD_01)

# ZD Rekorder 02
F_02_ZD_0.5 <- det_total %>% filter(confidence >= 0.5, confidence < 0.6, 
                                         between(date_time, Start, Ende), 
                                         spec == ZD, Rek == 2, KA == 0) # 962/948
# mehr als 40 Detektionen: 40 random entries (re) auswaehlen
re_F_02_ZD_0.5 = sample_n(F_02_ZD_0.5, 40)

F_02_ZD_0.6 <- det_total %>% filter(confidence >= 0.6, confidence < 0.7, 
                                         between(date_time, Start, Ende), 
                                         spec == ZD, Rek == 2, KA == 0) # 953/937
# mehr als 40 Detektionen: 40 random entries (re) auswaehlen
re_F_02_ZD_0.6 = sample_n(F_02_ZD_0.6, 40)

F_02_ZD_0.7 <- det_total %>% filter(confidence >= 0.7, confidence < 0.8, 
                                         between(date_time, Start, Ende), 
                                         spec == ZD, Rek == 2, KA == 0) # 1029/1006
# mehr als 40 Detektionen: 40 random entries (re) auswaehlen
re_F_02_ZD_0.7 = sample_n(F_02_ZD_0.7, 40)

F_02_ZD_0.8 <- det_total %>% filter(confidence >= 0.8, confidence < 0.9, 
                                         between(date_time, Start, Ende), 
                                         spec == ZD, Rek == 2, KA == 0) # 1352/1319
# mehr als 40 Detektionen: 40 random entries (re) auswaehlen
re_F_02_ZD_0.8 = sample_n(F_02_ZD_0.8, 40)

F_02_ZD_0.9 <- det_total %>% filter(confidence >= 0.9, confidence <=1, 
                                         between(date_time, Start, Ende), 
                                         spec == ZD, Rek == 2, KA == 0) # 3472/3264
# mehr als 40 Detektionen: 40 random entries (re) auswaehlen
re_F_02_ZD_0.9 = sample_n(F_02_ZD_0.9, 40)

# ein Dataframe mit ZD_02 erstellen, mit jew. max. 40 Detektionen
ZD_02 <- bind_rows(re_F_02_ZD_0.5, re_F_02_ZD_0.6, re_F_02_ZD_0.7, 
                   re_F_02_ZD_0.8, re_F_02_ZD_0.9) # 200 Eintraege

# als csv-Datei "ZD_02.csv" abspeichern
write.csv(ZD_02, file = "D:/BA/BirdNET/Arbeitsordner/DATENVERARBEITUNG/
          05_plausi_arten/ZD_02.csv", na = "NA", row.names = FALSE)
rm(F_02_ZD_0.5, F_02_ZD_0.6, F_02_ZD_0.7, F_02_ZD_0.8, F_02_ZD_0.9, re_F_02_ZD_0.5,
   re_F_02_ZD_0.6, re_F_02_ZD_0.7, re_F_02_ZD_0.8, re_F_02_ZD_0.9, ZD_02)

# ZD Rekorder 03
F_03_ZD_0.5 <- det_total %>% filter(confidence >= 0.5, confidence < 0.6, 
                                         between(date_time, Start, Ende), 
                                         spec == ZD, Rek == 3, KA == 0) # 22

F_03_ZD_0.6 <- det_total %>% filter(confidence >= 0.6, confidence < 0.7, 
                                         between(date_time, Start, Ende), 
                                         spec == ZD, Rek == 3, KA == 0) # 7

F_03_ZD_0.7 <- det_total %>% filter(confidence >= 0.7, confidence < 0.8, 
                                         between(date_time, Start, Ende), 
                                         spec == ZD, Rek == 3, KA == 0) # 5

F_03_ZD_0.8 <- det_total %>% filter(confidence >= 0.8, confidence < 0.9, 
                                         between(date_time, Start, Ende), 
                                         spec == ZD, Rek == 3, KA == 0) # 3

F_03_ZD_0.9 <- det_total %>% filter(confidence >= 0.9, confidence <=1, 
                                         between(date_time, Start, Ende), 
                                         spec == ZD, Rek == 3, KA == 0) # 1

# ein Dataframe mit ZD_03 erstellen, mit jew. max. 40 Detektionen
ZD_03 <- bind_rows(F_03_ZD_0.5, F_03_ZD_0.6, F_03_ZD_0.7, 
                   F_03_ZD_0.8, F_03_ZD_0.9) # 38 Eintraege

# als csv-Datei "ZD_03.csv" abspeichern
write.csv(ZD_03, file = "D:/BA/BirdNET/Arbeitsordner/DATENVERARBEITUNG/
          05_plausi_arten/ZD_03.csv", na = "NA", row.names = FALSE)
rm(F_03_ZD_0.5, F_03_ZD_0.6, F_03_ZD_0.7, F_03_ZD_0.8, F_03_ZD_0.9, ZD_03)

# ZD Rekorder 04
F_04_ZD_0.5 <- det_total %>% filter(confidence >= 0.5, confidence < 0.6, 
                                         between(date_time, Start, Ende), 
                                         spec == ZD, Rek == 4, KA == 0) # 20

F_04_ZD_0.6 <- det_total %>% filter(confidence >= 0.6, confidence < 0.7, 
                                         between(date_time, Start, Ende), 
                                         spec == ZD, Rek == 4, KA == 0) # 11

F_04_ZD_0.7 <- det_total %>% filter(confidence >= 0.7, confidence < 0.8, 
                                         between(date_time, Start, Ende), 
                                         spec == ZD, Rek == 4, KA == 0) # 9

F_04_ZD_0.8 <- det_total %>% filter(confidence >= 0.8, confidence < 0.9, 
                                         between(date_time, Start, Ende), 
                                         spec == ZD, Rek == 4, KA == 0) # 4

F_04_ZD_0.9 <- det_total %>% filter(confidence >= 0.9, confidence <=1, 
                                         between(date_time, Start, Ende), 
                                         spec == ZD, Rek == 4, KA == 0) # 8

# ein Dataframe mit ZD_04 erstellen, mit jew. max. 40 Detektionen
ZD_04 <- bind_rows(F_04_ZD_0.5, F_04_ZD_0.6, F_04_ZD_0.7, F_04_ZD_0.8, 
                   F_04_ZD_0.9) # 52 Eintraege

# als csv-Datei "ZD_02.csv" abspeichern
write.csv(ZD_04, file = "D:/BA/BirdNET/Arbeitsordner/DATENVERARBEITUNG/
          05_plausi_arten/ZD_04.csv", na = "NA", row.names = FALSE)
rm(F_04_ZD_0.5, F_04_ZD_0.6, F_04_ZD_0.7, F_04_ZD_0.8, F_04_ZD_0.9, ZD_04)


#### 6.1.5 OF-Liste ####
# OF Rekorder 02
F_02_OF_0.5 <- OF %>% filter(confidence >= 0.5, confidence < 0.6, 
                             between(date_time, Start, Ende), 
                             Rek == 2) # 46
# mehr als 40 Detektionen: 40 random entries (re) auswaehlen
re_F_02_OF_0.5 = sample_n(F_02_OF_0.5, 40)

F_02_OF_0.6 <- OF %>% filter(confidence >= 0.6, confidence < 0.7, 
                             between(date_time, Start, Ende), 
                             Rek == 2) # 41
# mehr als 40 Detektionen: 40 random entries (re) auswaehlen
re_F_02_OF_0.6 = sample_n(F_02_OF_0.6, 40)

F_02_OF_0.7 <- OF %>% filter(confidence >= 0.7, confidence < 0.8, 
                             between(date_time, Start, Ende), 
                             Rek == 2) # 47

# mehr als 40 Detektionen: 40 random entries (re) auswaehlen
re_F_02_OF_0.7 = sample_n(F_02_OF_0.7, 40)

F_02_OF_0.8 <- OF %>% filter(confidence >= 0.8, confidence < 0.9, 
                             between(date_time, Start, Ende), 
                             Rek == 2) # 52
# mehr als 40 Detektionen: 40 random entries (re) auswaehlen
re_F_02_OF_0.8 = sample_n(F_02_OF_0.8, 40)

F_02_OF_0.9 <- OF %>% filter(confidence >= 0.9, confidence <=1, 
                             between(date_time, Start, Ende), 
                             Rek == 2) # 85
# mehr als 40 Detektionen: 40 random entries (re) auswaehlen
re_F_02_OF_0.9 = sample_n(F_02_OF_0.9, 40)

# Dataframe mit OF_02 erstellen, mit jew. max. 40 Detektionen
OF_02 <- bind_rows(re_F_02_OF_0.5, re_F_02_OF_0.6, re_F_02_OF_0.7, 
                   re_F_02_OF_0.8, re_F_02_OF_0.9) # 150 Eintraege

# als csv-Datei "OF_02.csv" abspeichern
write.csv(OF_02, file = "D:/BA/BirdNET/Arbeitsordner/DATENVERARBEITUNG/
          05_plausi_arten/OF_02.csv", na = "NA", row.names = FALSE)

rm(F_02_OF_0.5, F_02_OF_0.6, F_02_OF_0.7, F_02_OF_0.8, F_02_OF_0.9, re_F_02_OF_0.5, 
   re_F_02_OF_0.6, re_F_02_OF_0.7, re_F_02_OF_0.8, re_F_02_OF_0.9, OF_02)


#### 6.2 ARTEN PLAUSIBILISIERT####
#### 6.2.1 data_plausi vorbereiten ####
# Daten einlesen
# csv-Datei einlesen der plausibilisierten FIles
data_plausi <- read.csv(file = "F:/BA/BirdNET/Arbeitsordner/DATENVERARBEITUNG/
                        05_plausi_arten/data_plausibilisiert.csv", 
                        header = TRUE, sep = ";")

# Datum richtiges Format
data_plausi$date_time <- gsub("\\.", "-", data_plausi$date_time)   
data_plausi$date_time <- as.POSIXct(data_plausi$date_time, tz = "utc", 
                                    format = "%d-%m-%Y %H:%M")


#### 7. PLOTS Schwellenwert ####
#### 7.1 glm Model - Schwellenwert ####
#### 7.1.1  Model DRS ####
data_DRS <- data_plausi %>% 
  filter(spec == "Acrocephalus arundinaceus, Drosselrohrsänger")
# confidence
model_DRS <- glm(outcome ~ confidence, family = "binomial", data = data_DRS)
p <- 0.9
threshold_DRS <- (log(p/(1-p)) - model_DRS$coefficients[1]) / model_DRS$coefficients[2] 
# Wert: 0.955 (0.9)
threshold_label_DRS = "Schwellenwert (p = 0.9, c = 0.955)"

# Plots DRS Schwellen
pDRS <- ggplot(model_DRS, aes(x = confidence, y = outcome)) +
  geom_point(shape = 16, size = 3, alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), 
              linetype = "solid", linewidth = 2, color = "#377eb8", fill = NA) +
  geom_vline(aes(xintercept = threshold_DRS, color = threshold_label_DRS), 
             linewidth = 2) +
  labs(title = expression(paste("a) Drosselrohrsänger 
                                (", italic("A. arundinaceus"), ")")),
       y = 'Wahrscheinlichkeit p \nBirdNET-Vorhersage richtig',
       x = 'Konfidenz-Wert c') +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2), limits = c(0, 1)) +
  scale_color_manual(name = "Legende", values = setNames("#e41a1c", 
                                                         threshold_label_DRS)) +
  theme_minimal() +
  theme(legend.position = "bottom")
pDRS


#### 7.1.2  Model NR ####
data_NR <- data_plausi %>% filter(spec == "Nycticorax nycticorax, Nachtreiher")
model_NR <- glm(outcome ~ confidence, family = "binomial", data = data_NR)
p <- 0.6
threshold_NR <- (log(p/(1-p)) - model_NR$coefficients[1]) / model_NR$coefficients[2] 
# Wert: 1.23 (0.9), 0.977(0.6)
threshold_label_NR = "Schwellenwert (p = 0,6 , c = 0,977)"

# Plots NR Schwellen
pNR <- ggplot(model_NR, aes(x = confidence, y = outcome)) +
  geom_point(shape = 16, size = 3, alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), 
              linetype = "solid", linewidth = 2, color = "#377eb8", fill = NA) +
  # geom_vline(aes(xintercept = threshold_NR, color = threshold_label_NR), 
  # linewidth = 2) + #auskommentiert, da kein Schwellenwert bei p = 0.9
  labs(title = expression(paste("a) Nachtreiher (", italic("N. nycticorax"), ")")),
       y = 'Wahrscheinlichkeit p \nBirdNET-Vorhersage richtig',
       x = 'Konfidenz-Wert c') +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2), limits = c(0, 1)) +
  scale_color_manual(name = "Legende", values = setNames("#e41a1c", 
                                                         threshold_label_NR)) +
  theme_minimal() +
  theme(legend.position = "bottom")
pNR


#### 7.1.3  Model WR ####
data_WR <- data_plausi %>% filter(spec == "Rallus aquaticus, Wasserralle")
model_WR <- glm(outcome ~ confidence, family = "binomial", data = data_WR)
p <- 0.9
threshold_WR <- (log(p/(1-p)) - model_WR$coefficients[1]) / model_WR$coefficients[2] 
# Wert: 0.77 (0.9)
threshold_label_WR = "Schwellenwert (p = 0.9, c = 0.77)"

pWR <- ggplot(model_WR, aes(x = confidence, y = outcome)) +
  geom_point(shape = 16, size = 3, alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), 
              linetype = "solid", linewidth = 2, color = "#377eb8", fill = NA) +
  geom_vline(aes(xintercept = threshold_NR, color = threshold_label_WR), 
             linewidth = 2) +
  labs(title = expression(paste("b) Wasserralle (", italic("R. aquaticus"), ")")),
       y = 'Wahrscheinlichkeit p \nBirdNET-Vorhersage richtig',
       x = 'Konfidenz-Wert c') +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2), limits = c(0, 1)) +
  scale_color_manual(name = "Legende", values = setNames("#e41a1c", 
                                                         threshold_label_WR)) +
  theme_minimal() +
  theme(legend.position = "bottom")
pWR


#### 7.1.4  Model ZD ####
data_ZD <- data_plausi %>% filter(spec == "Ixobrychus minutus, Zwergdommel")
model_ZD <- glm(outcome ~ confidence, family = "binomial", data = data_ZD)
p <- 0.9
threshold_ZD <- (log(p/(1-p)) - model_ZD$coefficients[1]) / model_ZD$coefficients[2] 
# Wert: 0.913 (0.9), 0.879 (0.88)
threshold_label_ZD = "Schwellenwert (p = 0.9, c = 0.913)"

pZD <- ggplot(model_ZD, aes(x = confidence, y = outcome)) +
  geom_point(shape = 16, size = 3, alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), 
              linetype = "solid", linewidth = 2, color = "#377eb8", fill = NA) +
  geom_vline(aes(xintercept = threshold_ZD, color = threshold_label_ZD), 
             linewidth = 2) +
  labs(title = expression(paste("c) Zwergdommel (", italic("I. minutus"), ")")),
       y = 'Wahrscheinlichkeit p \nBirdNET-Vorhersage richtig',
       x = 'Konfidenz-Wert c') +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2), limits = c(0, 1)) +
  scale_color_manual(name = "Legende", values = setNames("#e41a1c", 
                                                         threshold_label_ZD)) +
  theme_minimal() +
  theme(legend.position = "bottom")
pZD


#### 7.1.5  Model OF ####
data_OF <- data_plausi %>% 
  filter(spec == "Lithobates catesbeianus, American Bullfrog")
#confidence
model_OF <- glm(outcome ~ confidence, family = "binomial", data = data_OF)
p <- 0.9
threshold_OF <- (log(p/(1-p)) - model_OF$coefficients[1]) / model_OF$coefficients[2] 
# Wert: -0.317 (0.9), 0.822 (0.95)
threshold_label_OF = "Schwellenwert (p = 0,9, c = -0,317)"

pOF <- ggplot(model_OF, aes(x = confidence, y = outcome)) +
  geom_point(shape = 16, size = 3, alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), 
              linetype = "solid", linewidth = 2, color = "#377eb8", fill = NA) +
  # geom_vline(aes(xintercept = threshold_OF, color = threshold_label_OF), 
  # linewidth = 2) + # auskommentiert, da kein Schwellenwert bei 0.9
  labs(title = expression(paste("b) Ochsenfrosch (", italic("L. catesbeianus"), ")")),
       y = 'Wahrscheinlichkeit p \nBirdNET-Vorhersage richtig',
       x = 'Konfidenz-Wert c') +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2), limits = c(0, 1)) +
  scale_color_manual(name = "Legende", values = setNames("#e41a1c", 
                                                         threshold_label_OF)) +
  theme_minimal() +
  theme(legend.position = "bottom")
pOF


# aufrauemen #
rm(threshold_label_DRS, threshold_label_NR, threshold_label_WR, 
   threshold_label_ZD, threshold_label_OF)
rm(threshold_DRS, threshold_NR, threshold_WR, threshold_ZD, threshold_OF)
rm(model, model_DRS, model_NR, model_WR, model_ZD, model_OF)
rm(pDRS, pNR, pWR, pZD, pOF)


#### 7.2. Hochrechnung mit Schwellenwerten ####
# Dataframe data_spec um temporär filter zu überprüfen
det_total_zd <- det_total %>% filter(spec == ZD, KA == 0)
det_total_wr <- det_total %>% filter(spec == WR, KA == 0)
det_total_drs <- det_total %>% filter(spec == DRS)
det_total_nr <- det_total %>% filter(spec == NR)
det_total_of <- OF %>% filter(spec == OF)
data_spec<- bind_rows(det_total_wr, det_total_zd, det_total_drs, det_total_nr, OF)
rm(det_total_wr, det_total_zd, det_total_drs, det_total_nr)

# p = 0.9
nDRS <- data_spec %>% filter (spec == DRS, confidence >= 0.955, Rek == 4)
# nach Rekordern: 01: 0 , 02: 5 , 03: 0, 04: 1, gesamt: 6

nWR<- data_spec %>% filter (spec == WR, confidence >= 0.77, KA == 0, Rek == 4) 
# nach Rekordern: 01: 1, 02: 0, 03: 357, 04: 78, gesamt: 436

nZD<- data_spec %>% filter (spec == ZD, confidence >= 0.913, KA == 0, Rek == 4) 
# nach Rekordern: 01: 0, 02: 3051, 03: 1, 04: 6, gesamt: 3596

# für OF wurden wegen glm Model alle Detektionen verwendet
rm(nDRS, nWR, nZD)

#### 7.3 True Positive Plots ####
#### 7.3.1 data true pos####
# Anzahlen der True Positives in Plausibilisierungsliste 

# DRS
pd1 <- data_plausi %>% filter (spec == DRS, confidence >= 0.5, confidence < 0.6, 
                               outcome == 0) # 7,  73
pd2 <- data_plausi %>% filter (spec == DRS, confidence >= 0.6, confidence < 0.7, 
                               outcome == 0) # 19, 42
pd3 <- data_plausi %>% filter (spec == DRS, confidence >= 0.7, confidence < 0.8, 
                               outcome == 0) # 13, 15
pd4 <- data_plausi %>% filter (spec == DRS, confidence >= 0.8, confidence < 0.9, 
                               outcome == 0) # 14, 9
pd5 <- data_plausi %>% filter (spec == DRS, confidence >= 0.9, confidence < 1.1, 
                               outcome == 0) # 22, 2

# NR
pd1 <- data_plausi %>% filter (spec == NR, confidence >= 0.5, confidence < 0.6, 
                               outcome == 0) # 14, 146
pd2 <- data_plausi %>% filter (spec == NR, confidence >= 0.6, confidence < 0.7, 
                               outcome == 0) # 25, 135
pd3 <- data_plausi %>% filter (spec == NR, confidence >= 0.7, confidence < 0.8, 
                               outcome == 0) # 26, 108
pd4 <- data_plausi %>% filter (spec == NR, confidence >= 0.8, confidence < 0.9, 
                               outcome == 0) # 46, 79
pd5 <- data_plausi %>% filter (spec == NR, confidence >= 0.9, confidence < 1.1, 
                               outcome == 0) # 63, 48

# WR
pd1 <- data_plausi %>% filter (spec == WR, confidence >= 0.5, confidence < 0.6, 
                               outcome == 0) # 44, 25
pd2 <- data_plausi %>% filter (spec == WR, confidence >= 0.6, confidence < 0.7, 
                               outcome == 0) # 41, 10
pd3 <- data_plausi %>% filter (spec == WR, confidence >= 0.7, confidence < 0.8, 
                               outcome == 0) # 46, 11
pd4 <- data_plausi %>% filter (spec == WR, confidence >= 0.8, confidence < 0.9, 
                               outcome == 0) # 52, 2
pd5 <- data_plausi %>% filter (spec == WR, confidence >= 0.9, confidence < 1.1, 
                               outcome == 0) # 80, 1

# ZD
pd1 <- data_plausi %>% filter (spec == ZD, confidence >= 0.5, confidence < 0.6, 
                               outcome == 1) # 41,  42
pd2 <- data_plausi %>% filter (spec == ZD, confidence >= 0.6, confidence < 0.7, 
                               outcome == 1) # 40, 18
pd3 <- data_plausi %>% filter (spec == ZD, confidence >= 0.7, confidence < 0.8, 
                               outcome == 1) # 40, 15
pd4 <- data_plausi %>% filter (spec == ZD, confidence >= 0.8, confidence < 0.9, 
                               outcome == 1) # 41, 6
pd5 <- data_plausi %>% filter (spec == ZD, confidence >= 0.9, confidence < 1.1, 
                               outcome == 1) # 45, 4

pd1 <- data_plausi %>% filter (spec == OF, confidence >= 0.5, confidence < 0.6, 
                               outcome == 0) # 46, 3
pd2 <- data_plausi %>% filter (spec == OF, confidence >= 0.6, confidence < 0.7, 
                               outcome == 0) # 46, 2
pd3 <- data_plausi %>% filter (spec == OF, confidence >= 0.7, confidence < 0.8, 
                               outcome == 0) # 40, 3
pd4 <- data_plausi %>% filter (spec == OF, confidence >= 0.8, confidence < 0.9, 
                               outcome == 0) # 41, 3
pd5 <- data_plausi %>% filter (spec == OF, confidence >= 0.9, confidence < 1.1, 
                               outcome == 0) # 43, 1
rm(pd1, pd2, pd3, pd4, pd5)

# Daten einlesen - Prozente
# Verteilung von richtig- und falsch-positiven Detektionen
truepos = read.csv(
  file = "F:/BA/BirdNET/Arbeitsordner/DATENVERARBEITUNG/07_true_positive/
  truepos.csv",  
  sep = ";", header = TRUE, dec = ",")

# Gesamtzahlen der Validierten Detektionen der Arten                  
nDRS = 216
nNR = 690
nWR = 312
nZD = 292
nOF = 228

# X-Achse umbenennen: 
truepos$Interval <- c("≥ 0.5", "≥ 0.6", "≥ 0.7", "≥ 0.8", "≥ 0.9")

#### Plots richtig-/ falschpositiv ####
# DRS
pDRS <- ggplot(truepos, aes(x = Interval)) +
  geom_bar(aes(y = DRS_p_pos + DRS_p_neg, fill = "richtig"), stat = "identity") +
  geom_bar(aes(y = DRS_p_pos, fill = "falsch"), stat = "identity") +
  scale_fill_manual(values = c("richtig" = "#e41a1c", "falsch" = "#377eb8"), 
                    labels = c("richtig", "falsch"),
                    guide = guide_legend(title = paste("n = ", nDRS))) +
  labs(x = "Konfidenz-Interval ", y = "Verteilung Detektionen richtig / falsch", 
title = 
  expression(paste("a) Drosselrohrsänger (", italic("A. arundinaceus"), ")"))) +
  theme_minimal() +
  theme(legend.position = "bottom")
pDRS

# NR
pNR <- ggplot(truepos, aes(x = Interval)) +
  geom_bar(aes(y = NR_p_pos + NR_p_neg, fill = "richtig"), stat = "identity") +
  geom_bar(aes(y = NR_p_pos, fill = "falsch"), stat = "identity") +
  scale_fill_manual(values = c("richtig" = "#e41a1c", "falsch" = "#377eb8"), 
                    labels = c("richtig", "falsch"),
                    guide = guide_legend(title = paste("n = ", nNR))) +
  labs(x = "Konfidenz-Interval ", y = "Verteilung Detektionen richtig / falsch", 
title = expression(paste("b) Nachtreiher (", italic("N. nycticorax"), ")"))) +
  theme_minimal() +
  theme(legend.position = "bottom")
pNR

# WR
pWR <- ggplot(truepos, aes(x = Interval)) +
  geom_bar(aes(y = WR_p_pos + WR_p_neg, fill = "richtig"), stat = "identity") +
  geom_bar(aes(y = WR_p_pos, fill = "falsch"), stat = "identity") +
  scale_fill_manual(values = c("richtig" = "#e41a1c", "falsch" = "#377eb8"), 
                    labels = c("richtig", "falsch"),
                    guide = guide_legend(title = paste("n = ", nWR))) +
  labs(x = "Konfidenz-Interval ", y = "Verteilung Detektionen richtig / falsch", 
       title = expression(paste("c) Wasserralle (", italic("R. aquaticus"), ")"))) +
  theme_minimal() +
  theme(legend.position = "bottom")
pWR

# ZD
pZD <- ggplot(truepos, aes(x = Interval)) +
  geom_bar(aes(y = ZD_p_pos + ZD_p_neg, fill = "richtig"), stat = "identity") +
  geom_bar(aes(y = ZD_p_pos, fill = "falsch"), stat = "identity") +
  scale_fill_manual(values = c("richtig" = "#e41a1c", "falsch" = "#377eb8"), 
                    labels = c("richtig", "falsch"),
                    guide = guide_legend(title = paste("n = ", nZD))) +
  labs(x = "Konfidenz-Interval ", y = "Verteilung Detektionen richtig / falsch", 
       title = expression(paste("d) Zwergdommel (", italic("I. minutus"), ")"))) +
  theme_minimal() +
  theme(legend.position = "bottom")
pZD

# OF
pOF <- ggplot(truepos, aes(x = Interval)) +
  geom_bar(aes(y = OF_p_pos + OF_p_neg, fill = "richtig"), stat = "identity") +
  geom_bar(aes(y = OF_p_pos, fill = "falsch"), stat = "identity") +
  scale_fill_manual(values = c("richtig" = "#e41a1c", "falsch" = "#377eb8"), 
                    labels = c("richtig", "falsch"),
                    guide = guide_legend(title = paste("n = ", nOF))) +
  labs(x = "Konfidenz-Interval ", y = "Verteilung Detektionen richtig / falsch", 
       title = 
         expression(paste("e) Ochsenfrosch (", italic("L. catesbeianus"), ")"))) +
  theme_minimal() +
  theme(legend.position = "bottom")
pOF

rm(pDRS, pNR, pWR, pZD, pOF)


#### 7.4 Plots Verteilung det ####
#### 7.4.1data fuer plot Verteilung ####
vert <- data.frame (
  interval = c("≥ 0.5", "≥ 0.6", "≥ 0.7", "≥ 0.8", "≥ 0.9"), 
  DRS = c(132,	72,	28,	23,	24),
  NR = c(260,	154,	138,	133,	159),
  WR = c(157,	147,	123,	157,	240),
  ZD = c(991,	955,	1021,	1326,	3273),
  OF = c(55,	49,	50,	56,	89))

ngesDRS = 279
ngesNR = 844
ngesWR = 824
ngesZD = 7566
ngesOF = 299

#### Plots der Arten Verteilung ####
# DRS
pDRS <- ggplot(vert, aes(x = interval, y = DRS)) +
  geom_bar(stat = 'identity', fill = '#377eb8', width = 0.45) +  
  theme_minimal() +
  annotate("text", x = max(vert$interval), y = max(vert$DRS), 
           label = paste("n =", ngesDRS), 
           hjust = 0.45, vjust = -0.2, size = 4) +
  scale_y_continuous(breaks = seq(0, max(vert$DRS), by = 20)) +
  labs(title = expression(paste
      ('a) Drosselrohrsänger (', italic('A. arundinaceus'), ')')),
       x = 'Konfidenz-Intervall', y = 'Anzahl')
pDRS

# NR
pNR <- ggplot(vert, aes(x = interval, y = NR)) +
  geom_bar(stat = 'identity', fill = '#377eb8', width = 0.45) +  
  theme_minimal() +
  annotate("text", x = max(vert$interval), y = max(vert$NR), 
           label = paste("n =", ngesNR), 
           hjust = 0.45, vjust = -0.2, size = 4) +
  scale_y_continuous(breaks = seq(0, max(vert$NR), by = 20)) +
  labs(title = expression(paste
      ('b) Nachtreiher (', italic('N. nycticorax'), ')')),
       x = 'Konfidenz-Intervall', y = 'Anzahl')
pNR

# WR
pWR <- ggplot(vert, aes(x = interval, y = WR)) +
  geom_bar(stat = 'identity', fill = '#377eb8', width = 0.45) +  
  theme_minimal() +
  annotate("text", x = max(vert$interval), y = max(vert$WR), 
           label = paste("n =", ngesWR), 
           hjust = 0.45, vjust = -0.2, size = 4) +
  scale_y_continuous(breaks = seq(0, max(vert$WR), by = 20)) +
  labs(title = expression(paste
      ('c) Wasserralle (', italic('R. aquaticus'), ')')),
       x = 'Konfidenz-Intervall', y = 'Anzahl')
pWR

pZD <- ggplot(vert, aes(x = interval, y = ZD)) +
  geom_bar(stat = 'identity', fill = '#377eb8', width = 0.45) +  
  theme_minimal() +
  annotate("text", x = max(vert$interval), y = max(vert$ZD), 
           label = paste("n =", ngesZD), 
           hjust = 0.45, vjust = -0.2, size = 4) +
  scale_y_continuous(breaks = seq(0, max(vert$ZD), by = 200)) +
  labs(title = expression(paste
      ('d) Zwergdommel (', italic('I. minutus'), ')')),
       x = 'Konfidenz-Intervall', y = 'Anzahl')
pZD

# OF
pOF <- ggplot(vert, aes(x = interval, y = OF)) +
  geom_bar(stat = 'identity', fill = '#377eb8', width = 0.45) +  
  theme_minimal() +
  annotate("text", x = max(vert$interval), y = max(vert$OF), 
           label = paste("n =", ngesOF), 
           hjust = 0.45, vjust = -0.2, size = 4) +
  scale_y_continuous(breaks = seq(0, max(vert$OF), by = 20)) +
  labs(title = expression(paste
      ('e) Ochsenfrosch (', italic('L. catesbeianus'), ')')),
       x = 'Konfidenz-Intervall', y = 'Anzahl')
pOF


#### 8. Plots Phaenologie/Aktivitaetsverteilung ####
#### 8.1 Daten vorbeiten ####
data_plausi <- data_plausi %>% mutate(Uhrzeit = as_hms(date_time))
data_plausi$date_time_real <- data_plausi$date_time + data_plausi$start_time
data_plausi <- data_plausi %>% mutate(Uhrzeit_real = as_hms(date_time_real))

#SA/SU Funktion
calcsunrise = function(date, latitude, longitude, tz){
  sun.pos = suncalc::getSunlightTimes(as.Date(date), lat = latitude, lon
                                      = longitude, tz=tz)
  sun.pos$sunrise.s = lubridate::hour(sun.pos$sunrise)*3600 +
    lubridate::minute((sun.pos$sunrise))*60 +
    lubridate::second((sun.pos$sunrise))
  sun.pos$sunset.s = lubridate::hour(sun.pos$sunset)*3600 +
    lubridate::minute((sun.pos$sunset))*60 +
    lubridate::second((sun.pos$sunset))
  sun.pos$dawn.s = lubridate::hour(sun.pos$dawn)*3600 +
    lubridate::minute((sun.pos$dawn))*60 + lubridate::second((sun.pos$dawn))
  sun.pos$dusk.s = lubridate::hour(sun.pos$dusk)*3600 +
    lubridate::minute((sun.pos$dusk))*60 + lubridate::second((sun.pos$dusk))
  return(sun.pos)
}

library(suncalc)
sun_pos<-calcsunrise(date = seq(min(as.POSIXct(as.Date(data_plausi$date_time))),
                                max(as.POSIXct(as.Date(data_plausi$date_time))),
                                24*3600),
                             latitude = as.numeric(49.1),
                             longitude =as.numeric(8.35),
                             tz = paste0("UTC",
                                         -as.numeric(1))) %>%
  dplyr::transmute(date, sunrise, sunset, dawn,
                   dusk, sunrise.s, sunset.s, dusk.s, dawn.s) %>%
  tidyr::pivot_longer(cols = c("dawn.s",
                               "sunrise.s", "sunset.s", "dusk.s"),  
                      names_to = "type") %>%
  dplyr::mutate(sun.o.dawn = dplyr::recode(type,
                                           "sunrise.s" = "sun", 
                                           "sunset.s" = "sun", 
                                           "dawn.s" = "duswn", 
                                           "dusk.s" =
                                           "duswn")) %>%
  dplyr::select(date, type, value, sun.o.dawn)

# Zeit korrigieren 
#sun_pos$value <- sun_pos$value +3600
#data_plausi_DRS$Uhrzeit_real <- data_plausi_DRS$Uhrzeit_real +3600
#data_plausi_NR$Uhrzeit_real <- data_plausi_NR$Uhrzeit_real +3600
#data_plausi_WR$Uhrzeit_real <- data_plausi_WR$Uhrzeit_real -3600 # nichts geaendert
#data_plausi_NR$Uhrzeit_real <- data_plausi_NR$Uhrzeit_real +3600 # nichts geaendert
# data_plausi_OF$Uhrzeit_real <- data_plausi_OF$Uhrzeit_real +3600 # 3x 

# graue Blöcke hinzufuegen (für detections & detections_o)
detections$date_end <- detections$date + 600
detections <- detections %>% mutate(
  Uhrzeit_start = as.numeric(seconds(as_hms(date))))
detections <- detections %>% mutate(
  Uhrzeit_end = as.numeric(seconds(as_hms(date_end))))
detections$Uhrzeit_end[detections$Uhrzeit_end < 6000] = 86400
detections = detections %>%
  filter(Uhrzeit_start>7200)

detections_o$date_end <- detections_o$date + 600
detections_o <- detections_o %>% mutate(
  Uhrzeit_start = as.numeric(seconds(as_hms(date))))
detections_o <- detections_o %>% mutate(
  Uhrzeit_end = as.numeric(seconds(as_hms(date_end))))
detections_o$Uhrzeit_end[detections_o$Uhrzeit_end < 6000] = 86400
detections_o = detections_o %>%
  filter(Uhrzeit_start>7200)


#### 8. Phaenologie Plots  ####
#### 8.1 data plausi vorbereiten ####
data_plausi_DRS <- data_plausi %>% filter(spec == DRS, outcome == 1) # 75
data_plausi_NR <- data_plausi %>% filter(spec == NR, outcome == 1)   # 174
data_plausi_WR <- data_plausi %>% filter(spec == WR, outcome == 1)   # 263
data_plausi_ZD <- data_plausi %>% filter(spec == ZD, outcome == 1)   # 207
data_plausi_OF <- data_plausi %>% filter(spec == OF, outcome == 1)   # 216

# Labels für n= Plausibilisierte Daten
np_DRS = 75
np_NR = 174
np_WR = 263
np_ZD = 207
np_OF = 216


#### 8.2.1 DRS Plausi ####
pptp_DRS1 <- ggplot() +
  geom_rect(data = detections, 
            aes(ymin = Uhrzeit_start, ymax = Uhrzeit_end, xmin = date - 30000, 
                xmax = date + 30000, fill = factor("Aufnahmezeitraum", 
                      levels = c("Aufnahmezeitraum", "Drs", "duswn", "sun"))), 
            alpha = 4) +
  geom_line(data = sun_pos, 
            aes(x = as.POSIXct(date), y = value, 
                col = factor(sun.o.dawn,   levels = c("sun", "duswn", "Drs",
                                                      "Aufnahmezeitraum")), 
                group = type), 
            alpha = 0.7, 
            size = 1.5) +
  geom_point(data = data_plausi_DRS, 
             aes(x = date_time, y = Uhrzeit_real, color = factor("Drs", 
                 levels = c("Aufnahmezeitraum", "Drs", "duswn", "sun"))), 
             size = 1.5) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 25*3600),
                     breaks = c(seq(0, 24*3600, 7200)),
                     labels = c("00:00", "02:00", "04:00", "06:00", "08:00", 
                                "10:00", "12:00", "14:00", "16:00", "18:00", 
                                "20:00", "22:00", "24:00")) +
  labs(x = "Datum", y = "Uhrzeit", fill = "Legende") +  
  scale_color_manual(values = c("sun" = "orange", "duswn" = "#377eb8", 
                                "Drs" = "#e41a1c"), 
                     labels = c("sun" = "SA / SU", "duswn" = "Dämmerung", 
                                "Drs" = "Drs")) +
  scale_fill_manual(values = c("Aufnahmezeitraum" = "grey"), 
                    labels = c("Aufnahmezeitraum" = "Aufnahmezeitraum")) +
  theme_minimal() +  
  theme(panel.grid.major = element_line(color = "darkgrey", linetype = "dashed"), 
        legend.position = "bottom", legend.title = element_blank(),
        legend.box = "horizontal") +
  annotate("text", x = max(data_plausi_DRS$date_time), 
           y = max(data_plausi_DRS$Uhrzeit_real), 
           label = paste("n =", np_DRS), hjust = -17.65, vjust = -19, size = 4) +
  ggtitle(expression("Drosselrohrsänger (" * italic("A. arundinaceus") * ")"))
pptp_DRS1

#### 8.2.2 NR Plausi ####
pptp_NR1 <- ggplot() +
  geom_rect(data = detections, 
            aes(ymin = Uhrzeit_start, ymax = Uhrzeit_end, xmin = date - 30000, 
                xmax = date + 30000, fill = factor("Aufnahmezeitraum")), 
            alpha = 4) +
  geom_line(data = sun_pos, 
            aes(x = as.POSIXct(date), y = value, 
                col = factor(sun.o.dawn, levels = c("sun", "duswn", "Nr")), 
                group = type), 
            alpha = 0.7, 
            size = 1.5) +
  geom_point(data = data_plausi_NR, 
             aes(x = date_time, y = Uhrzeit_real, color = factor("Nr", 
             levels = c("sun", "duswn", "Nr"))), 
             size = 1.5) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 25*3600),
                     breaks = c(seq(0, 24*3600, 7200)),
                     labels = c("00:00", "02:00", "04:00", "06:00", "08:00", 
                                "10:00", "12:00", "14:00", "16:00", "18:00", 
                                "20:00", "22:00", "24:00")) +
  labs(x = "Datum", y = "Uhrzeit", fill = "Legende") +  
  scale_color_manual(values = c("sun" = "orange", "duswn" = "#377eb8", 
                                "Nr" = "#e41a1c"), 
                     labels = c("sun" = "SA / SU", "duswn" = "Dämmerung", 
                                "Nr" = "Nr"),
                     breaks = c("sun", "duswn", "Nr")) +
  scale_fill_manual(values = c("Aufnahmezeitraum" = "grey"), 
                    labels = c("Aufnahmezeitraum" = "Aufnahmezeitraum"),
                    breaks = c("Aufnahmezeitraum")) +
  guides(fill = guide_legend(order = 2), color = guide_legend(order = 1)) +
  theme_minimal() +  
  theme(panel.grid.major = element_line(color = "darkgrey", linetype = "dashed"), 
        legend.position = "bottom", legend.title = element_blank(),
        legend.box = "horizontal") +
  annotate("text", x = max(data_plausi_NR$date_time), 
           y = max(data_plausi_NR$Uhrzeit_real), 
           label = paste("n =", np_NR), hjust = 1, vjust = -0.15, size = 4) +
  ggtitle(expression("Nachtreiher (" * italic("N. nycticorax") * ")"))
pptp_NR1

#### 8.2.3 WR Plausi####
pptp_WR1 <- ggplot() +
  geom_rect(data = detections, 
            aes(ymin = Uhrzeit_start, ymax = Uhrzeit_end, xmin = date - 30000, 
                xmax = date + 30000, fill = factor("Aufnahmezeitraum")), 
            alpha = 4) +
  geom_line(data = sun_pos, 
            aes(x = as.POSIXct(date), y = value, 
                col = factor(sun.o.dawn, levels = c("sun", "duswn", "Wr")), 
                group = type), 
            alpha = 0.7, 
            size = 1.5) +
  geom_point(data = data_plausi_WR, 
             aes(x = date_time, y = Uhrzeit_real, color = factor("Wr", 
                 levels = c("sun", "duswn", "Wr"))), 
             size = 1.5) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 25*3600),
                     breaks = c(seq(0, 24*3600, 7200)),
                     labels = c("00:00", "02:00", "04:00", "06:00", "08:00", 
                                "10:00", "12:00", "14:00", "16:00", "18:00", 
                                "20:00", "22:00", "24:00")) +
  labs(x = "Datum", y = "Uhrzeit", fill = "Legende") +  
  scale_color_manual(values = c("sun" = "orange", "duswn" = "#377eb8", 
                                "Wr" = "#e41a1c"), 
                     labels = c("sun" = "SA / SU", "duswn" = "Dämmerung", 
                                "Wr" = "Wr"),
                     breaks = c("sun", "duswn", "Wr")) +
  scale_fill_manual(values = c("Aufnahmezeitraum" = "grey"), 
                    labels = c("Aufnahmezeitraum" = "Aufnahmezeitraum"),
                    breaks = c("Aufnahmezeitraum")) +
  guides(fill = guide_legend(order = 2), color = guide_legend(order = 1)) +
  theme_minimal() +  
  theme(panel.grid.major = element_line(color = "darkgrey", linetype = "dashed"), 
        legend.position = "bottom", legend.title = element_blank(),
        legend.box = "horizontal") +
  annotate("text", x = max(data_plausi_WR$date_time), 
           y = max(data_plausi_WR$Uhrzeit_real), 
           label = paste("n =", np_WR), hjust = -0.2, vjust = 0.6, size = 4) +
  ggtitle(expression("Wasserralle (" * italic("R. aquaticus") * ")"))
pptp_WR1


#### 8.2.4 ZD Plausi####
pptp_ZD1 <- ggplot() +
  geom_rect(data = detections, 
            aes(ymin = Uhrzeit_start, ymax = Uhrzeit_end, xmin = date - 30000, 
                xmax = date + 30000, fill = factor("Aufnahmezeitraum")), 
            alpha = 4) +
  geom_line(data = sun_pos, 
            aes(x = as.POSIXct(date), y = value, 
                col = factor(sun.o.dawn, levels = c("sun", "duswn", "Zd")), 
                group = type), 
            alpha = 0.9, 
            size = 1.5) +
  geom_point(data = data_plausi_ZD, 
             aes(x = date_time, y = Uhrzeit_real, color = factor("Zd", 
                 levels = c("sun", "duswn", "Zd"))), 
             size = 1.5) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 25*3600),
                     breaks = c(seq(0, 24*3600, 7200)),
                     labels = c("00:00", "02:00", "04:00", "06:00", "08:00", 
                                "10:00", "12:00", "14:00", "16:00", "18:00", 
                                "20:00", "22:00", "24:00")) +
  labs(x = "Datum", y = "Uhrzeit", fill = "Legende") +  
  scale_color_manual(values = c("sun" = "orange", "duswn" = "#377eb8", 
                                "Zd" = "#e41a1c"), 
                     labels = c("sun" = "SA / SU", "duswn" = "Dämmerung", 
                                "Zd" = "Zd"),
                     breaks = c("sun", "duswn", "Zd")) +
  scale_fill_manual(values = c("Aufnahmezeitraum" = "grey"), 
                    labels = c("Aufnahmezeitraum" = "Aufnahmezeitraum"),
                    breaks = c("Aufnahmezeitraum")) +
  guides(fill = guide_legend(order = 2), color = guide_legend(order = 1)) +
  theme_minimal() +  
  theme(panel.grid.major = element_line(color = "darkgrey", linetype = "dashed"), 
        legend.position = "bottom", legend.title = element_blank(),
        legend.box = "horizontal") +
  annotate("text", x = max(data_plausi_ZD$date_time), 
           y = max(data_plausi_ZD$Uhrzeit_real), 
           label = paste("n =", np_ZD), hjust = -9.65, vjust = 1, size = 4) +
  ggtitle(expression("Zwergdommel (" * italic("I. minutus") * ")"))
pptp_ZD1


#### 8.2.5 OF Plausi ####
pptp_OF1 <- ggplot() +
  geom_rect(data = detections, 
            aes(ymin = Uhrzeit_start, ymax = Uhrzeit_end, xmin = date - 30000, 
                xmax = date + 30000, fill = factor("Aufnahmezeitraum")), 
            alpha = 4) +
  geom_line(data = sun_pos, 
            aes(x = as.POSIXct(date), y = value, 
                col = factor(sun.o.dawn, levels = c("sun", "duswn", "Of")), 
                group = type), 
            alpha = 0.7, 
            size = 1.5) +
  geom_point(data = data_plausi_OF, 
             aes(x = date_time, y = Uhrzeit_real, color = factor("Of", 
                 levels = c("sun", "duswn", "Of"))), 
             size = 1.5) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 25*3600),
                     breaks = c(seq(0, 24*3600, 7200)),
                     labels = c("00:00", "02:00", "04:00", "06:00", "08:00", 
                                "10:00", "12:00", "14:00", "16:00", "18:00", 
                                "20:00", "22:00", "24:00")) +
  labs(x = "Datum", y = "Uhrzeit", fill = "Legende") +  
  scale_color_manual(values = c("sun" = "orange", "duswn" = "#377eb8", 
                                "Of" = "#e41a1c"), 
                     labels = c("sun" = "SA / SU", "duswn" = "Dämmerung", 
                                "Of" = "Of"),
                     breaks = c("sun", "duswn", "Of")) +
  scale_fill_manual(values = c("Aufnahmezeitraum" = "grey"), 
                    labels = c("Aufnahmezeitraum" = "Aufnahmezeitraum"),
                    breaks = c("Aufnahmezeitraum")) +
  guides(fill = guide_legend(order = 2), color = guide_legend(order = 1)) +
  theme_minimal() +  
  theme(panel.grid.major = element_line(color = "darkgrey", linetype = "dashed"), 
        legend.position = "bottom", legend.title = element_blank(),
        legend.box = "horizontal") +
  annotate("text", x = max(data_plausi_OF$date_time), 
           y = max(data_plausi_OF$Uhrzeit_real), 
           label = paste("n =", np_OF), hjust = -1.45, vjust = 1, size = 4) +
  ggtitle(expression("Ochsenfrosch (" * italic("L. catesbeianus") * ")"))
pptp_OF1

#### 8.3 Rekorderweise Plausi ####
#### 8.3.1 DRS Rek Plausi ####
data_plausi_DRS_2 <- data_plausi_DRS %>% filter(spec == DRS, outcome == 1, Rek == 2) # 62
data_plausi_DRS_3 <- data_plausi_DRS %>% filter(spec == DRS, outcome == 1, Rek == 3) # 13

# DRS plausi Rek 2
pptp_DRS_R2 <- ggplot() +
  geom_rect(data = detections, 
            aes(ymin = Uhrzeit_start, ymax = Uhrzeit_end, xmin = date - 30000, 
                xmax = date + 30000, fill = factor("Aufnahmezeitraum", 
                                                   levels = c("Aufnahmezeitraum", "Drs", "duswn", "sun"))), 
            alpha = 4) +
  geom_line(data = sun_pos, 
            aes(x = as.POSIXct(date), y = value, 
                col = factor(sun.o.dawn,   levels = c("sun", "duswn", "Drs",
                                                      "Aufnahmezeitraum")), group = type), 
            alpha = 0.7, 
            size = 1.5) +
  geom_point(data = data_plausi_DRS_2, 
             aes(x = date_time, y = Uhrzeit_real, color = factor("Drs", levels = c("Aufnahmezeitraum", "Drs", "duswn", "sun"))), 
             size = 1) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 25*3600),
                     breaks = c(seq(0, 24*3600, 7200)),
                     labels = c("00:00", "02:00", "04:00", "06:00", "08:00", 
                                "10:00", "12:00", "14:00", "16:00", "18:00", 
                                "20:00", "22:00", "24:00")) +
  labs(x = "Datum", y = "Uhrzeit", fill = "Legende") +  
  scale_color_manual(values = c("sun" = "orange", "duswn" = "#377eb8", 
                                "Drs" = "#e41a1c"), 
                     labels = c("sun" = "SA / SU", "duswn" = "Dämmerung", 
                                "Drs" = "Drs")) +
  scale_fill_manual(values = c("Aufnahmezeitraum" = "grey"), 
                    labels = c("Aufnahmezeitraum" = "Aufnahmezeitraum")) +
  theme_minimal() +  
  theme(panel.grid.major = element_line(color = "darkgrey", linetype = "dashed"), 
        legend.position = "bottom", legend.title = element_blank(),
        legend.box = "horizontal") +
  annotate("text", x = max(data_plausi_DRS_2$date_time), 
           y = max(data_plausi_DRS_2$Uhrzeit_real), 
           label = paste("n = 62"), hjust = -13.6, vjust = -18.1, size = 4) +
  ggtitle(expression("Drosselrohrsänger (" * italic("A. arundinaceus") * ") - Rekorder 2"))
pptp_DRS_R2


# DRS plausi Rek 3
pptp_DRS_R3 <- ggplot() +
  geom_rect(data = detections, 
            aes(ymin = Uhrzeit_start, ymax = Uhrzeit_end, xmin = date - 30000, 
                xmax = date + 30000, fill = factor("Aufnahmezeitraum", 
                                                   levels = c("Aufnahmezeitraum", "Drs", "duswn", "sun"))), 
            alpha = 4) +
  geom_line(data = sun_pos, 
            aes(x = as.POSIXct(date), y = value, 
                col = factor(sun.o.dawn,   levels = c("sun", "duswn", "Drs",
                                                      "Aufnahmezeitraum")), group = type), 
            alpha = 0.7, 
            size = 1.5) +
  geom_point(data = data_plausi_DRS_3, 
             aes(x = date_time, y = Uhrzeit_real, color = factor("Drs", levels = c("Aufnahmezeitraum", "Drs", "duswn", "sun"))), 
             size = 1) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 25*3600),
                     breaks = c(seq(0, 24*3600, 7200)),
                     labels = c("00:00", "02:00", "04:00", "06:00", "08:00", 
                                "10:00", "12:00", "14:00", "16:00", "18:00", 
                                "20:00", "22:00", "24:00")) +
  labs(x = "Datum", y = "Uhrzeit", fill = "Legende") +  
  scale_color_manual(values = c("sun" = "orange", "duswn" = "#377eb8", 
                                "Drs" = "#e41a1c"), 
                     labels = c("sun" = "SA / SU", "duswn" = "Dämmerung", 
                                "Drs" = "Drs")) +
  scale_fill_manual(values = c("Aufnahmezeitraum" = "grey"), 
                    labels = c("Aufnahmezeitraum" = "Aufnahmezeitraum")) +
  theme_minimal() +  
  theme(panel.grid.major = element_line(color = "darkgrey", linetype = "dashed"), 
        legend.position = "bottom", legend.title = element_blank(),
        legend.box = "horizontal") +
  annotate("text", x = max(data_plausi_DRS_3$date_time), 
           y = max(data_plausi_DRS_3$Uhrzeit_real), 
           label = paste("n = 13"), hjust = -14, vjust = -18.0, size = 4) +
  ggtitle(expression("Drosselrohrsänger (" * italic("A. arundinaceus") * ") - Rekorder 3"))
pptp_DRS_R3

#### 8.3.2 NR Rek Plausi ####
data_plausi_NR_1 <- data_plausi_NR %>% filter(spec == NR, outcome == 1, Rek == 1) # 7
data_plausi_NR_2 <- data_plausi_NR %>% filter(spec == NR, outcome == 1, Rek == 2) # 112
data_plausi_NR_3 <- data_plausi_NR %>% filter(spec == NR, outcome == 1, Rek == 3) # 32
data_plausi_NR_4 <- data_plausi_NR %>% filter(spec == NR, outcome == 1, Rek == 4) # 23

# Rek 1 NR phaeno
pptp_NR_R1 <- ggplot() +
  geom_rect(data = detections, 
            aes(ymin = Uhrzeit_start, ymax = Uhrzeit_end, xmin = date - 30000, 
                xmax = date + 30000, fill = factor("Aufnahmezeitraum")), 
            alpha = 4) +
  geom_line(data = sun_pos, 
            aes(x = as.POSIXct(date), y = value, 
                col = factor(sun.o.dawn, levels = c("sun", "duswn", "Nr")), group = type), 
            alpha = 0.7, 
            size = 1.5) +
  geom_point(data = data_plausi_NR_1, 
             aes(x = date_time, y = Uhrzeit_real, color = factor("Nr", levels = c("sun", "duswn", "Nr"))), 
             size = 1.5) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 25*3600),
                     breaks = c(seq(0, 24*3600, 7200)),
                     labels = c("00:00", "02:00", "04:00", "06:00", "08:00", 
                                "10:00", "12:00", "14:00", "16:00", "18:00", 
                                "20:00", "22:00", "24:00")) +
  labs(x = "Datum", y = "Uhrzeit", fill = "Legende") +  
  scale_color_manual(values = c("sun" = "orange", "duswn" = "#377eb8", 
                                "Nr" = "#e41a1c"), 
                     labels = c("sun" = "SA / SU", "duswn" = "Dämmerung", 
                                "Nr" = "Nr"),
                     breaks = c("sun", "duswn", "Nr")) +
  scale_fill_manual(values = c("Aufnahmezeitraum" = "grey"), 
                    labels = c("Aufnahmezeitraum" = "Aufnahmezeitraum"),
                    breaks = c("Aufnahmezeitraum")) +
  guides(fill = guide_legend(order = 2), color = guide_legend(order = 1)) +
  theme_minimal() +  
  theme(panel.grid.major = element_line(color = "darkgrey", linetype = "dashed"), 
        legend.position = "bottom", legend.title = element_blank(),
        legend.box = "horizontal") +
  annotate("text", x = max(data_plausi_NR_1$date_time), 
           y = max(data_plausi_NR_1$Uhrzeit_real), 
           label = paste("n = 7"), hjust = 1, vjust = -0.85, size = 4) +
  ggtitle(expression("a) Nachtreiher (" * italic("N. nycticorax") * ") - Rekorder 1"))
pptp_NR_R1

pptp_NR_R2 <- ggplot() +
  geom_rect(data = detections, 
            aes(ymin = Uhrzeit_start, ymax = Uhrzeit_end, xmin = date - 30000, 
                xmax = date + 30000, fill = factor("Aufnahmezeitraum")), 
            alpha = 4) +
  geom_line(data = sun_pos, 
            aes(x = as.POSIXct(date), y = value, 
                col = factor(sun.o.dawn, levels = c("sun", "duswn", "Nr")), group = type), 
            alpha = 0.7, 
            size = 1.5) +
  geom_point(data = data_plausi_NR_2, 
             aes(x = date_time, y = Uhrzeit_real, color = factor("Nr", levels = c("sun", "duswn", "Nr"))), 
             size = 1.5) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 25*3600),
                     breaks = c(seq(0, 24*3600, 7200)),
                     labels = c("00:00", "02:00", "04:00", "06:00", "08:00", 
                                "10:00", "12:00", "14:00", "16:00", "18:00", 
                                "20:00", "22:00", "24:00")) +
  labs(x = "Datum", y = "Uhrzeit", fill = "Legende") +  
  scale_color_manual(values = c("sun" = "orange", "duswn" = "#377eb8", 
                                "Nr" = "#e41a1c"), 
                     labels = c("sun" = "SA / SU", "duswn" = "Dämmerung", 
                                "Nr" = "Nr"),
                     breaks = c("sun", "duswn", "Nr")) +
  scale_fill_manual(values = c("Aufnahmezeitraum" = "grey"), 
                    labels = c("Aufnahmezeitraum" = "Aufnahmezeitraum"),
                    breaks = c("Aufnahmezeitraum")) +
  guides(fill = guide_legend(order = 2), color = guide_legend(order = 1)) +
  theme_minimal() +  
  theme(panel.grid.major = element_line(color = "darkgrey", linetype = "dashed"), 
        legend.position = "bottom", legend.title = element_blank(),
        legend.box = "horizontal") +
  annotate("text", x = max(data_plausi_NR_2$date_time), 
           y = max(data_plausi_NR_2$Uhrzeit_real), 
           label = paste("n = 112"), hjust = -2.4, vjust = -0.8, size = 4) +
  ggtitle(expression("b) Nachtreiher (" * italic("N. nycticorax") * ") - Rekorder 2"))
pptp_NR_R2

pptp_NR_R3 <- ggplot() +
  geom_rect(data = detections, 
            aes(ymin = Uhrzeit_start, ymax = Uhrzeit_end, xmin = date - 30000, 
                xmax = date + 30000, fill = factor("Aufnahmezeitraum")), 
            alpha = 4) +
  geom_line(data = sun_pos, 
            aes(x = as.POSIXct(date), y = value, 
                col = factor(sun.o.dawn, levels = c("sun", "duswn", "Nr")), group = type), 
            alpha = 0.7, 
            size = 1.5) +
  geom_point(data = data_plausi_NR_3, 
             aes(x = date_time, y = Uhrzeit_real, color = factor("Nr", levels = c("sun", "duswn", "Nr"))), 
             size = 1.5) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 25*3600),
                     breaks = c(seq(0, 24*3600, 7200)),
                     labels = c("00:00", "02:00", "04:00", "06:00", "08:00", 
                                "10:00", "12:00", "14:00", "16:00", "18:00", 
                                "20:00", "22:00", "24:00")) +
  labs(x = "Datum", y = "Uhrzeit", fill = "Legende") +  
  scale_color_manual(values = c("sun" = "orange", "duswn" = "#377eb8", 
                                "Nr" = "#e41a1c"), 
                     labels = c("sun" = "SA / SU", "duswn" = "Dämmerung", 
                                "Nr" = "Nr"),
                     breaks = c("sun", "duswn", "Nr")) +
  scale_fill_manual(values = c("Aufnahmezeitraum" = "grey"), 
                    labels = c("Aufnahmezeitraum" = "Aufnahmezeitraum"),
                    breaks = c("Aufnahmezeitraum")) +
  guides(fill = guide_legend(order = 2), color = guide_legend(order = 1)) +
  theme_minimal() +  
  theme(panel.grid.major = element_line(color = "darkgrey", linetype = "dashed"), 
        legend.position = "bottom", legend.title = element_blank(),
        legend.box = "horizontal") +
  annotate("text", x = max(data_plausi_NR_3$date_time), 
           y = max(data_plausi_NR_3$Uhrzeit_real), 
           label = paste("n = 32"), hjust = -7.35, vjust = -0.2, size = 4) +
  ggtitle(expression("c) Nachtreiher (" * italic("N. nycticorax") * ") - Rekorder 3"))
pptp_NR_R3

pptp_NR_R4 <- ggplot() +
  geom_rect(data = detections, 
            aes(ymin = Uhrzeit_start, ymax = Uhrzeit_end, xmin = date - 30000, 
                xmax = date + 30000, fill = factor("Aufnahmezeitraum")), 
            alpha = 4) +
  geom_line(data = sun_pos, 
            aes(x = as.POSIXct(date), y = value, 
                col = factor(sun.o.dawn, levels = c("sun", "duswn", "Nr")), group = type), 
            alpha = 0.7, 
            size = 1.5) +
  geom_point(data = data_plausi_NR_4, 
             aes(x = date_time, y = Uhrzeit_real, color = factor("Nr", levels = c("sun", "duswn", "Nr"))), 
             size = 1.5) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 25*3600),
                     breaks = c(seq(0, 24*3600, 7200)),
                     labels = c("00:00", "02:00", "04:00", "06:00", "08:00", 
                                "10:00", "12:00", "14:00", "16:00", "18:00", 
                                "20:00", "22:00", "24:00")) +
  labs(x = "Datum", y = "Uhrzeit", fill = "Legende") +  
  scale_color_manual(values = c("sun" = "orange", "duswn" = "#377eb8", 
                                "Nr" = "#e41a1c"), 
                     labels = c("sun" = "SA / SU", "duswn" = "Dämmerung", 
                                "Nr" = "Nr"),
                     breaks = c("sun", "duswn", "Nr")) +
  scale_fill_manual(values = c("Aufnahmezeitraum" = "grey"), 
                    labels = c("Aufnahmezeitraum" = "Aufnahmezeitraum"),
                    breaks = c("Aufnahmezeitraum")) +
  guides(fill = guide_legend(order = 2), color = guide_legend(order = 1)) +
  theme_minimal() +  
  theme(panel.grid.major = element_line(color = "darkgrey", linetype = "dashed"), 
        legend.position = "bottom", legend.title = element_blank(),
        legend.box = "horizontal") +
  annotate("text", x = max(data_plausi_NR_4$date_time), 
           y = max(data_plausi_NR_4$Uhrzeit_real), 
           label = paste("n = 23"), hjust = -4.7, vjust = -1, size = 4) +
  ggtitle(expression("d) Nachtreiher (" * italic("N. nycticorax") * ") - Rekorder 4"))
pptp_NR_R4

#### 8.3.3 WR Rek Plausi ####
# für Rek 1 und 2 sind nur 1 bzw. 2 Detektionen vorhanden, es wird kein extra-Plot erstellt
data_plausi_WR_3 <- data_plausi_WR %>% filter(spec == WR, outcome == 1, Rek == 3) # 188
data_plausi_WR_4 <- data_plausi_WR %>% filter(spec == WR, outcome == 1, Rek == 4) # 72

pptp_WR_R3 <- ggplot() +
  geom_rect(data = detections, 
            aes(ymin = Uhrzeit_start, ymax = Uhrzeit_end, xmin = date - 30000, 
                xmax = date + 30000, fill = factor("Aufnahmezeitraum")), 
            alpha = 4) +
  geom_line(data = sun_pos, 
            aes(x = as.POSIXct(date), y = value, 
                col = factor(sun.o.dawn, levels = c("sun", "duswn", "Wr")), group = type), 
            alpha = 0.7, 
            size = 1.5) +
  geom_point(data = data_plausi_WR_3, 
             aes(x = date_time, y = Uhrzeit_real, color = factor("Wr", levels = c("sun", "duswn", "Wr"))), 
             size = 1.5) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 25*3600),
                     breaks = c(seq(0, 24*3600, 7200)),
                     labels = c("00:00", "02:00", "04:00", "06:00", "08:00", 
                                "10:00", "12:00", "14:00", "16:00", "18:00", 
                                "20:00", "22:00", "24:00")) +
  labs(x = "Datum", y = "Uhrzeit", fill = "Legende") +  
  scale_color_manual(values = c("sun" = "orange", "duswn" = "#377eb8", 
                                "Wr" = "#e41a1c"), 
                     labels = c("sun" = "SA / SU", "duswn" = "Dämmerung", 
                                "Wr" = "Wr"),
                     breaks = c("sun", "duswn", "Wr")) +
  scale_fill_manual(values = c("Aufnahmezeitraum" = "grey"), 
                    labels = c("Aufnahmezeitraum" = "Aufnahmezeitraum"),
                    breaks = c("Aufnahmezeitraum")) +
  guides(fill = guide_legend(order = 2), color = guide_legend(order = 1)) +
  theme_minimal() +  
  theme(panel.grid.major = element_line(color = "darkgrey", linetype = "dashed"), 
        legend.position = "bottom", legend.title = element_blank(),
        legend.box = "horizontal") +
  annotate("text", x = max(data_plausi_WR_3$date_time), 
           y = max(data_plausi_WR_3$Uhrzeit_real), 
           label = paste("n = 188"), hjust = -3.45, vjust = -1.5, size = 4) +
  ggtitle(expression("Wasserralle (" * italic("R. aquaticus") * ") - Rekorder 3"))
pptp_WR_R3


pptp_WR_R4 <- ggplot() +
  geom_rect(data = detections, 
            aes(ymin = Uhrzeit_start, ymax = Uhrzeit_end, xmin = date - 30000, 
                xmax = date + 30000, fill = factor("Aufnahmezeitraum")), 
            alpha = 4) +
  geom_line(data = sun_pos, 
            aes(x = as.POSIXct(date), y = value, 
                col = factor(sun.o.dawn, levels = c("sun", "duswn", "Wr")), group = type), 
            alpha = 0.7, 
            size = 1.5) +
  geom_point(data = data_plausi_WR_4, 
             aes(x = date_time, y = Uhrzeit_real, color = factor("Wr", levels = c("sun", "duswn", "Wr"))), 
             size = 1.5) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 25*3600),
                     breaks = c(seq(0, 24*3600, 7200)),
                     labels = c("00:00", "02:00", "04:00", "06:00", "08:00", 
                                "10:00", "12:00", "14:00", "16:00", "18:00", 
                                "20:00", "22:00", "24:00")) +
  labs(x = "Datum", y = "Uhrzeit", fill = "Legende") +  
  scale_color_manual(values = c("sun" = "orange", "duswn" = "#377eb8", 
                                "Wr" = "#e41a1c"), 
                     labels = c("sun" = "SA / SU", "duswn" = "Dämmerung", 
                                "Wr" = "Wr"),
                     breaks = c("sun", "duswn", "Wr")) +
  scale_fill_manual(values = c("Aufnahmezeitraum" = "grey"), 
                    labels = c("Aufnahmezeitraum" = "Aufnahmezeitraum"),
                    breaks = c("Aufnahmezeitraum")) +
  guides(fill = guide_legend(order = 2), color = guide_legend(order = 1)) +
  theme_minimal() +  
  theme(panel.grid.major = element_line(color = "darkgrey", linetype = "dashed"), 
        legend.position = "bottom", legend.title = element_blank(),
        legend.box = "horizontal") +
  annotate("text", x = max(data_plausi_WR_4$date_time), 
           y = max(data_plausi_WR_4$Uhrzeit_real), 
           label = paste("n = 72"), hjust = -0.45, vjust = 0.5, size = 4) +
  ggtitle(expression("b) Wasserralle (" * italic("R. aquaticus") * ") - Rekorder 4"))
pptp_WR_R4


#### 8.3.4 ZD Rek Plausi ####
data_plausi_ZD_2 <- data_plausi_ZD %>% filter(spec == ZD, outcome == 1, Rek == 2) # 198

pptp_ZD_R2 <- ggplot() +
  geom_rect(data = detections, 
            aes(ymin = Uhrzeit_start, ymax = Uhrzeit_end, xmin = date - 30000, 
                xmax = date + 30000, fill = factor("Aufnahmezeitraum")), 
            alpha = 4) +
  geom_line(data = sun_pos, 
            aes(x = as.POSIXct(date), y = value, 
                col = factor(sun.o.dawn, levels = c("sun", "duswn", "Zd")), group = type), 
            alpha = 0.9, 
            size = 1.5) +
  geom_point(data = data_plausi_ZD_2, 
             aes(x = date_time, y = Uhrzeit_real, color = factor("Zd", levels = c("sun", "duswn", "Zd"))), 
             size = 1.5) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 25*3600),
                     breaks = c(seq(0, 24*3600, 7200)),
                     labels = c("00:00", "02:00", "04:00", "06:00", "08:00", 
                                "10:00", "12:00", "14:00", "16:00", "18:00", 
                                "20:00", "22:00", "24:00")) +
  labs(x = "Datum", y = "Uhrzeit", fill = "Legende") +  
  scale_color_manual(values = c("sun" = "orange", "duswn" = "#377eb8", 
                                "Zd" = "#e41a1c"), 
                     labels = c("sun" = "SA / SU", "duswn" = "Dämmerung", 
                                "Zd" = "Zd"),
                     breaks = c("sun", "duswn", "Zd")) +
  scale_fill_manual(values = c("Aufnahmezeitraum" = "grey"), 
                    labels = c("Aufnahmezeitraum" = "Aufnahmezeitraum"),
                    breaks = c("Aufnahmezeitraum")) +
  guides(fill = guide_legend(order = 2), color = guide_legend(order = 1)) +
  theme_minimal() +  
  theme(panel.grid.major = element_line(color = "darkgrey", linetype = "dashed"), 
        legend.position = "bottom", legend.title = element_blank(),
        legend.box = "horizontal") +
  annotate("text", x = max(data_plausi_ZD_2$date_time), 
           y = max(data_plausi_ZD_2$Uhrzeit_real), 
           label = paste("n = 198"), hjust = -12.3, vjust = 1, size = 4) +
  ggtitle(expression("a) Zwergdommel (" * italic("I. minutus") * ") - Rekorder 2"))
pptp_ZD_R2

#### 8.3.5 OF Rek Plausi####
data_plausi_OF_2 <- data_plausi_OF %>% filter(spec == OF, outcome == 1, Rek == 2) # 198

pptp_OF_R2 <- ggplot() +
  geom_rect(data = detections, 
            aes(ymin = Uhrzeit_start, ymax = Uhrzeit_end, xmin = date - 30000, 
                xmax = date + 30000, fill = factor("Aufnahmezeitraum")), 
            alpha = 4) +
  geom_line(data = sun_pos, 
            aes(x = as.POSIXct(date), y = value, 
                col = factor(sun.o.dawn, levels = c("sun", "duswn", "Of")), group = type), 
            alpha = 0.7, 
            size = 1.5) +
  geom_point(data = data_plausi_OF_2, 
             aes(x = date_time, y = Uhrzeit_real, color = factor("Of", levels = c("sun", "duswn", "Of"))), 
             size = 1.5) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 25*3600),
                     breaks = c(seq(0, 24*3600, 7200)),
                     labels = c("00:00", "02:00", "04:00", "06:00", "08:00", 
                                "10:00", "12:00", "14:00", "16:00", "18:00", 
                                "20:00", "22:00", "24:00")) +
  labs(x = "Datum", y = "Uhrzeit", fill = "Legende") +  
  scale_color_manual(values = c("sun" = "orange", "duswn" = "#377eb8", 
                                "Of" = "#e41a1c"), 
                     labels = c("sun" = "SA / SU", "duswn" = "Dämmerung", 
                                "Of" = "Of"),
                     breaks = c("sun", "duswn", "Of")) +
  scale_fill_manual(values = c("Aufnahmezeitraum" = "grey"), 
                    labels = c("Aufnahmezeitraum" = "Aufnahmezeitraum"),
                    breaks = c("Aufnahmezeitraum")) +
  guides(fill = guide_legend(order = 2), color = guide_legend(order = 1)) +
  theme_minimal() +  
  theme(panel.grid.major = element_line(color = "darkgrey", linetype = "dashed"), 
        legend.position = "bottom", legend.title = element_blank(),
        legend.box = "horizontal") +
  annotate("text", x = max(data_plausi_OF_2$date_time), 
           y = max(data_plausi_OF_2$Uhrzeit_real), 
           label = paste("n = 198"), hjust = -1.45, vjust = 1, size = 4) +
  ggtitle(expression("a) Ochsenfrosch (" * italic("L. catesbeianus") * ") - Rekorder 2"))
pptp_OF_R2


#### 9 Phaenologie hochgerechnet ####
#### data hochrechnung vorbereiten ####
# sun_pos wird aus andererm Teil verwendet

# Uhrzeit erstellen und Uhrzeit_real 
det_total <- det_total %>% mutate(Uhrzeit = as_hms(date_time))
det_total$date_time_real <- det_total$date_time + det_total$start_time
det_total <- det_total %>% mutate(Uhrzeit_real = as_hms(date_time_real))

det_ochsenfrosch <- det_ochsenfrosch %>% mutate(Uhrzeit = as_hms(date_time))
det_ochsenfrosch$date_time_real <- det_ochsenfrosch$date_time + det_ochsenfrosch$start_time
det_ochsenfrosch<- det_ochsenfrosch %>% mutate(Uhrzeit_real = as_hms(date_time_real))

# data für Arten- Dataframes erstellen
data_det_DRS <- det_total %>% filter(spec == DRS, confidence >= threshold_DRS) # 6
data_det_NR<- det_total %>% filter(spec == NR, confidence >= threshold_NR) # 46
data_det_WR <- det_total %>% filter(spec == WR, confidence >= threshold_WR, KA == 0) # 436 
data_det_ZD <- det_total %>% filter(spec == ZD, confidence >= threshold_ZD, KA == 0) # 3059

data_det_OF <- det_ochsenfrosch %>% filter(spec == OF, confidence >= 0.5) # 299
  

#### 9.1 DRS plot phäno det_total ####
#### 9.2.1 DRS hoch ####
ppdt_DRS1 <- ggplot() +
  geom_rect(data = detections, 
            aes(ymin = Uhrzeit_start, ymax = Uhrzeit_end, xmin = date - 30000, 
                xmax = date + 30000, fill = factor("Aufnahmezeitraum", 
                                                   levels = c("Aufnahmezeitraum", "Drs", "duswn", "sun"))), 
            alpha = 4) +
  geom_line(data = sun_pos, 
            aes(x = as.POSIXct(date), y = value, 
                col = factor(sun.o.dawn,   levels = c("sun", "duswn", "Drs",
                                                      "Aufnahmezeitraum")), group = type), 
            alpha = 0.7, 
            size = 1.5) +
  geom_point(data = data_det_DRS, 
             aes(x = date_time, y = Uhrzeit_real, color = factor("Drs", levels = c("Aufnahmezeitraum", "Drs", "duswn", "sun"))), 
             size = 1.5) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 25*3600),
                     breaks = c(seq(0, 24*3600, 7200)),
                     labels = c("00:00", "02:00", "04:00", "06:00", "08:00", 
                                "10:00", "12:00", "14:00", "16:00", "18:00", 
                                "20:00", "22:00", "24:00")) +
  labs(x = "Datum", y = "Uhrzeit", fill = "Legende") +  
  scale_color_manual(values = c("sun" = "orange", "duswn" = "#377eb8", 
                                "Drs" = "#e41a1c"), 
                     labels = c("sun" = "SA / SU", "duswn" = "Dämmerung", 
                                "Drs" = "Drs")) +
  scale_fill_manual(values = c("Aufnahmezeitraum" = "grey"), 
                    labels = c("Aufnahmezeitraum" = "Aufnahmezeitraum")) +
  theme_minimal() +  
  theme(panel.grid.major = element_line(color = "darkgrey", linetype = "dashed"), 
        legend.position = "bottom", legend.title = element_blank(),
        legend.box = "horizontal") +
  annotate("text", x = max(data_det_DRS$date_time), 
           y = max(data_det_DRS$Uhrzeit_real), 
           label = paste("n = 6"), hjust = -16.75, vjust = -3.1, size = 4) +
  ggtitle(expression("Drosselrohrsänger (" * italic("A. arundinaceus") * ") - Hochrechnung - alle Rekorder"))
ppdt_DRS1

#### 9.2.2 WR hoch ####
ppdt_WR1 <- ggplot() +
  geom_rect(data = detections, 
            aes(ymin = Uhrzeit_start, ymax = Uhrzeit_end, xmin = date - 30000, 
                xmax = date + 30000, fill = factor("Aufnahmezeitraum")), 
            alpha = 4) +
  geom_line(data = sun_pos, 
            aes(x = as.POSIXct(date), y = value, 
                col = factor(sun.o.dawn, levels = c("sun", "duswn", "Wr")), group = type), 
            alpha = 0.7, 
            size = 1.5) +
  geom_point(data = data_det_WR, 
             aes(x = date_time, y = Uhrzeit_real, color = factor("Wr", levels = c("sun", "duswn", "Wr"))), 
             size = 1.5) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 25*3600),
                     breaks = c(seq(0, 24*3600, 7200)),
                     labels = c("00:00", "02:00", "04:00", "06:00", "08:00", 
                                "10:00", "12:00", "14:00", "16:00", "18:00", 
                                "20:00", "22:00", "24:00")) +
  labs(x = "Datum", y = "Uhrzeit", fill = "Legende") +  
  scale_color_manual(values = c("sun" = "orange", "duswn" = "#377eb8", 
                                "Wr" = "#e41a1c"), 
                     labels = c("sun" = "SA / SU", "duswn" = "Dämmerung", 
                                "Wr" = "Wr"),
                     breaks = c("sun", "duswn", "Wr")) +
  scale_fill_manual(values = c("Aufnahmezeitraum" = "grey"), 
                    labels = c("Aufnahmezeitraum" = "Aufnahmezeitraum"),
                    breaks = c("Aufnahmezeitraum")) +
  guides(fill = guide_legend(order = 2), color = guide_legend(order = 1)) +
  theme_minimal() +  
  theme(panel.grid.major = element_line(color = "darkgrey", linetype = "dashed"), 
        legend.position = "bottom", legend.title = element_blank(),
        legend.box = "horizontal") +
  annotate("text", x = max(data_det_WR$date_time), 
           y = max(data_det_WR$Uhrzeit_real), 
           label = paste("n = 436"), hjust = 0.45, vjust = 0.85, size = 4) +
  ggtitle(expression("c) Wasserralle (" * italic("R. aquaticus") * ") - Hochrechnung - alle Rekorder"))
ppdt_WR1

#### 9.2.3 ZD hoch ####
ppdt_ZD1 <- ggplot() +
  geom_rect(data = detections, 
            aes(ymin = Uhrzeit_start, ymax = Uhrzeit_end, xmin = date - 30000, 
                xmax = date + 30000, fill = factor("Aufnahmezeitraum")), 
            alpha = 4) +
  geom_line(data = sun_pos, 
            aes(x = as.POSIXct(date), y = value, 
                col = factor(sun.o.dawn, levels = c("sun", "duswn", "Zd")), group = type), 
            alpha = 0.9, 
            size = 1.5) +
  geom_point(data = data_det_ZD, 
             aes(x = date_time, y = Uhrzeit_real, color = factor("Zd", levels = c("sun", "duswn", "Zd"))), 
             size = 1.5) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 25*3600),
                     breaks = c(seq(0, 24*3600, 7200)),
                     labels = c("00:00", "02:00", "04:00", "06:00", "08:00", 
                                "10:00", "12:00", "14:00", "16:00", "18:00", 
                                "20:00", "22:00", "24:00")) +
  labs(x = "Datum", y = "Uhrzeit", fill = "Legende") +  
  scale_color_manual(values = c("sun" = "orange", "duswn" = "#377eb8", 
                                "Zd" = "#e41a1c"), 
                     labels = c("sun" = "SA / SU", "duswn" = "Dämmerung", 
                                "Zd" = "Zd"),
                     breaks = c("sun", "duswn", "Zd")) +
  scale_fill_manual(values = c("Aufnahmezeitraum" = "grey"), 
                    labels = c("Aufnahmezeitraum" = "Aufnahmezeitraum"),
                    breaks = c("Aufnahmezeitraum")) +
  guides(fill = guide_legend(order = 2), color = guide_legend(order = 1)) +
  theme_minimal() +  
  theme(panel.grid.major = element_line(color = "darkgrey", linetype = "dashed"), 
        legend.position = "bottom", legend.title = element_blank(),
        legend.box = "horizontal") +
  annotate("text", x = max(data_det_ZD$date_time), 
           y = max(data_det_ZD$Uhrzeit_real), 
           label = paste("n = 3059"), hjust = -1.8, vjust = 1, size = 4) +
  ggtitle(expression("b) Zwergdommel (" * italic("I. minutus") * ") - Hochrechnung - alle Rekorder"))
ppdt_ZD1


#### 9.2.4 OF hoch ####
ppdt_OF1 <- ggplot() +
  geom_rect(data = detections, 
            aes(ymin = Uhrzeit_start, ymax = Uhrzeit_end, xmin = date - 30000, 
                xmax = date + 30000, fill = factor("Aufnahmezeitraum")), 
            alpha = 4) +
  geom_line(data = sun_pos, 
            aes(x = as.POSIXct(date), y = value, 
                col = factor(sun.o.dawn, levels = c("sun", "duswn", "Of")), group = type), 
            alpha = 0.7, 
            size = 1.5) +
  geom_point(data = data_det_OF, 
             aes(x = date_time, y = Uhrzeit_real, color = factor("Of", levels = c("sun", "duswn", "Of"))), 
             size = 1.5) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 25*3600),
                     breaks = c(seq(0, 24*3600, 7200)),
                     labels = c("00:00", "02:00", "04:00", "06:00", "08:00", 
                                "10:00", "12:00", "14:00", "16:00", "18:00", 
                                "20:00", "22:00", "24:00")) +
  labs(x = "Datum", y = "Uhrzeit", fill = "Legende") +  
  scale_color_manual(values = c("sun" = "orange", "duswn" = "#377eb8", 
                                "Of" = "#e41a1c"), 
                     labels = c("sun" = "SA / SU", "duswn" = "Dämmerung", 
                                "Of" = "Of"),
                     breaks = c("sun", "duswn", "Of")) +
  scale_fill_manual(values = c("Aufnahmezeitraum" = "grey"), 
                    labels = c("Aufnahmezeitraum" = "Aufnahmezeitraum"),
                    breaks = c("Aufnahmezeitraum")) +
  guides(fill = guide_legend(order = 2), color = guide_legend(order = 1)) +
  theme_minimal() +  
  theme(panel.grid.major = element_line(color = "darkgrey", linetype = "dashed"), 
        legend.position = "bottom", legend.title = element_blank(),
        legend.box = "horizontal") +
  annotate("text", x = max(data_det_OF$date_time), 
           y = max(data_det_OF$Uhrzeit_real), 
           label = paste("n = 299"), hjust = -1.4, vjust = 1, size = 4) +
  ggtitle(expression("b) Ochsenfrosch (" * italic("L. catesbeianus") * ") - Hochrechnung - alle Rekorder"))
ppdt_OF1

#### 9.3 Rekorderweise hochrechnung ####
# DRS2, DRS3, ZD2, OF2, WR3   # DRS2 macht kein Sinn, aufteilung ist 5:1
#### 9.3.1 WR Rek hoch ####
data_det_WR_3 <- data_det_WR %>% filter(spec == WR, Rek == 3) # 357
data_det_WR_4 <- data_det_WR %>% filter(spec == WR, Rek == 4) # 78

ppdt_WR_R3 <- ggplot() +
  geom_rect(data = detections, 
            aes(ymin = Uhrzeit_start, ymax = Uhrzeit_end, xmin = date - 30000, 
                xmax = date + 30000, fill = factor("Aufnahmezeitraum")), 
            alpha = 4) +
  geom_line(data = sun_pos, 
            aes(x = as.POSIXct(date), y = value, 
                col = factor(sun.o.dawn, levels = c("sun", "duswn", "Wr")), group = type), 
            alpha = 0.7, 
            size = 1.5) +
  geom_point(data = data_det_WR_3, 
             aes(x = date_time, y = Uhrzeit_real, color = factor("Wr", levels = c("sun", "duswn", "Wr"))), 
             size = 1.5) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 25*3600),
                     breaks = c(seq(0, 24*3600, 7200)),
                     labels = c("00:00", "02:00", "04:00", "06:00", "08:00", 
                                "10:00", "12:00", "14:00", "16:00", "18:00", 
                                "20:00", "22:00", "24:00")) +
  labs(x = "Datum", y = "Uhrzeit", fill = "Legende") +  
  scale_color_manual(values = c("sun" = "orange", "duswn" = "#377eb8", 
                                "Wr" = "#e41a1c"), 
                     labels = c("sun" = "SA / SU", "duswn" = "Dämmerung", 
                                "Wr" = "Wr"),
                     breaks = c("sun", "duswn", "Wr")) +
  scale_fill_manual(values = c("Aufnahmezeitraum" = "grey"), 
                    labels = c("Aufnahmezeitraum" = "Aufnahmezeitraum"),
                    breaks = c("Aufnahmezeitraum")) +
  guides(fill = guide_legend(order = 2), color = guide_legend(order = 1)) +
  theme_minimal() +  
  theme(panel.grid.major = element_line(color = "darkgrey", linetype = "dashed"), 
        legend.position = "bottom", legend.title = element_blank(),
        legend.box = "horizontal") +
  annotate("text", x = max(data_det_WR_3$date_time), 
           y = max(data_det_WR_3$Uhrzeit_real), 
           label = paste("n = 357"), hjust = -3.45, vjust = 1, size = 4) +
  ggtitle(expression("a) Wasserralle (" * italic("R. aquaticus") * ") - Hochrechnung - Rekorder 3"))
ppdt_WR_R3


ppdt_WR_R4 <- ggplot() +
  geom_rect(data = detections, 
            aes(ymin = Uhrzeit_start, ymax = Uhrzeit_end, xmin = date - 30000, 
                xmax = date + 30000, fill = factor("Aufnahmezeitraum")), 
            alpha = 4) +
  geom_line(data = sun_pos, 
            aes(x = as.POSIXct(date), y = value, 
                col = factor(sun.o.dawn, levels = c("sun", "duswn", "Wr")), group = type), 
            alpha = 0.7, 
            size = 1.5) +
  geom_point(data = data_det_WR_4, 
             aes(x = date_time, y = Uhrzeit_real, color = factor("Wr", levels = c("sun", "duswn", "Wr"))), 
             size = 1.5) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 25*3600),
                     breaks = c(seq(0, 24*3600, 7200)),
                     labels = c("00:00", "02:00", "04:00", "06:00", "08:00", 
                                "10:00", "12:00", "14:00", "16:00", "18:00", 
                                "20:00", "22:00", "24:00")) +
  labs(x = "Datum", y = "Uhrzeit", fill = "Legende") +  
  scale_color_manual(values = c("sun" = "orange", "duswn" = "#377eb8", 
                                "Wr" = "#e41a1c"), 
                     labels = c("sun" = "SA / SU", "duswn" = "Dämmerung", 
                                "Wr" = "Wr"),
                     breaks = c("sun", "duswn", "Wr")) +
  scale_fill_manual(values = c("Aufnahmezeitraum" = "grey"), 
                    labels = c("Aufnahmezeitraum" = "Aufnahmezeitraum"),
                    breaks = c("Aufnahmezeitraum")) +
  guides(fill = guide_legend(order = 2), color = guide_legend(order = 1)) +
  theme_minimal() +  
  theme(panel.grid.major = element_line(color = "darkgrey", linetype = "dashed"), 
        legend.position = "bottom", legend.title = element_blank(),
        legend.box = "horizontal") +
  annotate("text", x = max(data_det_WR_4$date_time), 
           y = max(data_det_WR_4$Uhrzeit_real), 
           label = paste("n = 78"), hjust = -0.45, vjust = 0.5, size = 4) +
  ggtitle(expression("b) Wasserralle (" * italic("R. aquaticus") * ") - Hochrechnung - Rekorder 4"))
ppdt_WR_R4

#### 9.3.2 ZD Rek hoch ####
data_det_ZD_2 <- data_det_ZD %>% filter(spec == ZD, Rek == 2) # 3052

ppdt_ZD_R2 <- ggplot() +
  geom_rect(data = detections, 
            aes(ymin = Uhrzeit_start, ymax = Uhrzeit_end, xmin = date - 30000, 
                xmax = date + 30000, fill = factor("Aufnahmezeitraum")), 
            alpha = 4) +
  geom_line(data = sun_pos, 
            aes(x = as.POSIXct(date), y = value, 
                col = factor(sun.o.dawn, levels = c("sun", "duswn", "Zd")), group = type), 
            alpha = 0.9, 
            size = 1.5) +
  geom_point(data = data_det_ZD_2, 
             aes(x = date_time, y = Uhrzeit_real, color = factor("Zd", levels = c("sun", "duswn", "Zd"))), 
             size = 1.5) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 25*3600),
                     breaks = c(seq(0, 24*3600, 7200)),
                     labels = c("00:00", "02:00", "04:00", "06:00", "08:00", 
                                "10:00", "12:00", "14:00", "16:00", "18:00", 
                                "20:00", "22:00", "24:00")) +
  labs(x = "Datum", y = "Uhrzeit", fill = "Legende") +  
  scale_color_manual(values = c("sun" = "orange", "duswn" = "#377eb8", 
                                "Zd" = "#e41a1c"), 
                     labels = c("sun" = "SA / SU", "duswn" = "Dämmerung", 
                                "Zd" = "Zd"),
                     breaks = c("sun", "duswn", "Zd")) +
  scale_fill_manual(values = c("Aufnahmezeitraum" = "grey"), 
                    labels = c("Aufnahmezeitraum" = "Aufnahmezeitraum"),
                    breaks = c("Aufnahmezeitraum")) +
  guides(fill = guide_legend(order = 2), color = guide_legend(order = 1)) +
  theme_minimal() +  
  theme(panel.grid.major = element_line(color = "darkgrey", linetype = "dashed"), 
        legend.position = "bottom", legend.title = element_blank(),
        legend.box = "horizontal") +
  annotate("text", x = max(data_det_ZD_2$date_time), 
           y = max(data_det_ZD_2$Uhrzeit_real), 
           label = paste("n = 3052"), hjust = -10.4, vjust = 0.9, size = 4) +
  ggtitle(expression("a) Zwergdommel (" * italic("I. minutus") * ") - Hochrechnung - Rekorder 2"))
ppdt_ZD_R2

#### 9.3.3 OF Rek hich ####
data_det_OF_2 <- data_det_OF %>% filter(spec == OF, Rek == 2) # 271

ppdt_OF_R2 <- ggplot() +
  geom_rect(data = detections, 
            aes(ymin = Uhrzeit_start, ymax = Uhrzeit_end, xmin = date - 30000, 
                xmax = date + 30000, fill = factor("Aufnahmezeitraum")), 
            alpha = 4) +
  geom_line(data = sun_pos, 
            aes(x = as.POSIXct(date), y = value, 
                col = factor(sun.o.dawn, levels = c("sun", "duswn", "Of")), group = type), 
            alpha = 0.7, 
            size = 1.5) +
  geom_point(data = data_det_OF_2, 
             aes(x = date_time, y = Uhrzeit_real, color = factor("Of", levels = c("sun", "duswn", "Of"))), 
             size = 1.5) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 25*3600),
                     breaks = c(seq(0, 24*3600, 7200)),
                     labels = c("00:00", "02:00", "04:00", "06:00", "08:00", 
                                "10:00", "12:00", "14:00", "16:00", "18:00", 
                                "20:00", "22:00", "24:00")) +
  labs(x = "Datum", y = "Uhrzeit", fill = "Legende") +  
  scale_color_manual(values = c("sun" = "orange", "duswn" = "#377eb8", 
                                "Of" = "#e41a1c"), 
                     labels = c("sun" = "SA / SU", "duswn" = "Dämmerung", 
                                "Of" = "Of"),
                     breaks = c("sun", "duswn", "Of")) +
  scale_fill_manual(values = c("Aufnahmezeitraum" = "grey"), 
                    labels = c("Aufnahmezeitraum" = "Aufnahmezeitraum"),
                    breaks = c("Aufnahmezeitraum")) +
  guides(fill = guide_legend(order = 2), color = guide_legend(order = 1)) +
  theme_minimal() +  
  theme(panel.grid.major = element_line(color = "darkgrey", linetype = "dashed"), 
        legend.position = "bottom", legend.title = element_blank(),
        legend.box = "horizontal") +
  annotate("text", x = max(data_det_OF_2$date_time), 
           y = max(data_det_OF_2$Uhrzeit_real), 
           label = paste("n = 271"), hjust = -1.4, vjust = 1, size = 4) +
  ggtitle(expression("a) Ochsenfrosch (" * italic("L. catesbeianus") * ") - Hochrechnung - Rekorder 2"))
ppdt_OF_R2


#### 10. Artenlisten ####
# Filter nach Arten - Artenliste erstellen im Gesamtzeitraum
#### 10.1 Filter Artenzahl ####
# Filtern nach Artenzahl, Konfidenz >= 0.5, nach Rekordern getrennt
anzahl_ges <- det_total %>%
  filter(confidence >= 0.5, between(date_time, Start, Ende)) %>%
  summarise(anzahl_verschiedene_einträge = n_distinct(spec)) # 240 Arten

anzahl_01 <- det_total %>%
  filter(confidence >= 0.5, between(date_time, Start, Ende), Rek == 1) %>%
  summarise(anzahl_verschiedene_einträge = n_distinct(spec)) # 126 Arten

anzahl_02 <- det_total %>%
  filter(confidence >= 0.5, between(date_time, Start, Ende), Rek == 2) %>%
  summarise(anzahl_verschiedene_einträge = n_distinct(spec)) # 171 Arten

anzahl_03 <- det_total %>%
  filter(confidence >= 0.5, between(date_time, Start, Ende), Rek == 3) %>%
  summarise(anzahl_verschiedene_einträge = n_distinct(spec)) # 170 Arten

anzahl_04 <- det_total %>%
  filter(confidence >= 0.5, between(date_time, Start, Ende), Rek == 4) %>%
  summarise(anzahl_verschiedene_einträge = n_distinct(spec)) # 195 Arten

#### 10.2 Artenlsiten erstellen ####
# alle Arten die mindestens ein mal detektiert wurden, Konfidenz <= 0.5. 
# Gesamtartenliste und Artenlisten je Rekorder
arten_gesamt <- unique(det_total$spec)
artenliste_gesamt <- data.frame(spec = arten_gesamt)

arten_01 <- unique(det_total$spec[det_total$Rek == 1])
artenliste_01 <- data.frame(spec = arten_01)

arten_02 <- unique(det_total$spec[det_total$Rek == 2])
artenliste_02 <- data.frame(spec = arten_02)

arten_03 <- unique(det_total$spec[det_total$Rek == 3])
artenliste_03 <- data.frame(spec = arten_03)

arten_04 <- unique(det_total$spec[det_total$Rek == 4])
artenliste_04 <- data.frame(spec = arten_04)

# Rekorder 01 - Dataframe nach Rekorder sortieren
det_total_gefiltert_01 <- det_total[det_total$Rek == 1, ]
#  gefilterte Dataframe nach spec & conf sortieren, absteigende Reihenfolge
det_total_gefiltert_01 <- det_total_gefiltert_01[order
        (det_total_gefiltert_01$spec, -det_total_gefiltert_01$confidence), ] 
# nach spec sortieren und ersten drei Zeilen für jede Art auswaehlen
plausi_arten_01 <- det_total_gefiltert_01[ave(seq(nrow(det_total_gefiltert_01)), det_total_gefiltert_01$spec, FUN = seq_along) <= 3, ]
# Dataframe abspeichern
write.csv(plausi_arten_01, file = "04_filter/plausi_arten_01_new.csv", 
          na = "NA", row.names = FALSE)

# Rekorder 02, Vorgehen analog wie fuer Rekorder 01
det_total_gefiltert_02 <- det_total[det_total$Rek == 2, ]
det_total_gefiltert_02 <- det_total_gefiltert_02[order
        (det_total_gefiltert_02$spec, -det_total_gefiltert_02$confidence), ]
plausi_arten_02 <- det_total_gefiltert_02[ave(seq(nrow(det_total_gefiltert_02)), det_total_gefiltert_02$spec, FUN = seq_along) <= 3, ] # 457 Eintraege fuer 171 Arten
write.csv(plausi_arten_02, file = "D:/BA/BirdNET/Arbeitsordner/DATENVERARBEITUNG/03_filter/plausi_arten_02.csv", na = "NA", row.names = FALSE)

# Rekorder 03, Vorgehen analog wie fuer Rekorder 01
det_total_gefiltert_03 <- det_total[det_total$Rek == 3, ]
det_total_gefiltert_03 <- det_total_gefiltert_03[order(det_total_gefiltert_03$spec, -det_total_gefiltert_03$confidence), ]
plausi_arten_03 <- det_total_gefiltert_03[ave(seq(nrow(det_total_gefiltert_03)), det_total_gefiltert_03$spec, FUN = seq_along) <= 3, ] # 446 Eintraege fuer 170 Arten
write.csv(plausi_arten_03, file = "D:/BA/BirdNET/Arbeitsordner/DATENVERARBEITUNG/03_filter/plausi_arten_03.csv", na = "NA", row.names = FALSE)

# Rekorder 04, Vorgehen analog wie fuer Rekorder 01
det_total_gefiltert_04 <- det_total[det_total$Rek == 4, ]
det_total_gefiltert_04 <- det_total_gefiltert_04[order(det_total_gefiltert_04$spec, -det_total_gefiltert_04$confidence), ]
plausi_arten_04 <- det_total_gefiltert_04[ave(seq(nrow(det_total_gefiltert_04)), det_total_gefiltert_04$spec, FUN = seq_along) <= 3, ] # 500 Eintraege fuer 195 Arten
write.csv(plausi_arten_04, file = "D:/BA/BirdNET/Arbeitsordner/DATENVERARBEITUNG/03_filter/plausi_arten_04.csv", na = "NA", row.names = FALSE)


#### 11. Ochsenfrosch ####
# Filter OF nach Rekordern
OF_01 <- OF %>%  filter(Rek == 1) # 0
OF_02 <- OF %>%  filter(Rek == 2) # 271
OF_03 <- OF %>%  filter(Rek == 3) # 19
OF_04 <- OF %>%  filter(Rek == 4) # 9
rm(OF_01, OF_02, OF_03, OF_04)

#### 11.1 Plausi Liste OF ####

# 11.1 OF Rekorder 03
# 11.2 Detektionen
write.csv(OF_03, file = "D:/BA/BirdNET/Arbeitsordner/DATENVERARBEITUNG/05_plausi_arten/OF_03.csv", na = "NA", row.names = FALSE)

# OF Rekorder 04
# 4 Detektionen
write.csv(OF_04, file = "D:/BA/BirdNET/Arbeitsordner/DATENVERARBEITUNG/05_plausi_arten/OF_04.csv", na = "NA", row.names = FALSE)

# Gesamtdatensatz der zu plausibilisiernden Detektionen OF
OF_total <- bind_rows(OF_02, OF_03, OF_04)
write.csv(OF_total, file = "D:/BA/BirdNET/Arbeitsordner/DATENVERARBEITUNG/05_plausi_arten/OF_total.csv", na = "NA", row.names = FALSE)

