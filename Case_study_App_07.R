packages <- c("tidyverse", "data.table")

for(i in 1:length(packages)){
  if(!require(packages[i], character.only = TRUE)){
    install.packages(packages[i])
  }
  library(packages[i], character.only = TRUE)
}
ordner_namen <- c("Einzelteil", "Fahrzeug", "Geodaten", "Komponente", "Logistikverzug", "Zulassungen")
ordner <- dir(path = c("data"), full.names = TRUE)
names(ordner) <- ordner_namen

staedte <- c("Koeln", "Dortmund", "Bielefeld", "Bonn", "Oberhausen", "Bochum", "Muenster", "Aachen")

hilfs_vector <- c("/Bestandteile", "/Fahrzeuge", "/Komponente", "Werke")

list_files_of_folder<- function(folder) {
  dir(path = folder, full.names = TRUE)
}

read_data_from_file_to_tibble <- function(file, encoding = "unknown", my_func = function(my_var) {my_var}) {
  fread_data <- fread(file, data.table = FALSE, encoding = encoding)
  as_tibble(fread_data) %>%
    my_func()
}

read_data_from_file_to_tibble_for_rel_tabelle <- function(file, encoding = "unknown", my_func = function(my_var, file_name) {
  counter <- length(file_name)
  my_var
}) {
  fread_data <- fread(file, data.table = FALSE, encoding = encoding)
  as_tibble(fread_data) %>%
    my_func(file)
}

# importing data from Fahrzeug folder

fahrzeuge_dateien <- list_files_of_folder(ordner["Fahrzeug"])

#fahrzeuge_dateien_indexes <- grepl(pattern = paste("^", paste(ordner["Fahrzeug"], hilfs_vector[1])), x = fahrzeuge_paths)
bestandteile_fahrzeuge_paths <- fahrzeuge_dateien[1:4]

#fahrzeuge_paths <- list_files_with_pattern(ordner["Fahrzeug"], pattern = paste("^", paste(ordner["Fahrzeug"], hilfs_vector[2])))
bestandteile_fahrzeuge_paths_indexes <- 1:4
names(bestandteile_fahrzeuge_paths_indexes) <- bestandteile_fahrzeuge_paths

bestandteile_fahrzeuge <- lapply(bestandteile_fahrzeuge_paths, read_data_from_file_to_tibble_for_rel_tabelle, my_func = function(my_var, file_name){
  tibble_column_names <- names(my_var)
  first_column_name <- tibble_column_names[1]
  last_column_name <- tibble_column_names[length(tibble_column_names)]
  id_fahrzeuge_column_name <- "ID_F"
  z <- bestandteile_fahrzeuge_paths_indexes[file_name]
  if (z < 4){
    #print(paste("z : ", z))
    my_var <- my_var %>%
      slice(-1) %>%
      select(- !!first_column_name) %>%
      pivot_longer(! (!!last_column_name), names_to = NULL, values_to = "ID_K") %>%
      rename(!!id_fahrzeuge_column_name := !!last_column_name)
  }else {
    #print(paste("z : ", z))
    my_var <- my_var %>%
      select(- !!first_column_name) %>%
      pivot_longer(! (!!last_column_name), names_to = NULL, values_to = "ID_K") %>%
      rename(!!id_fahrzeuge_column_name := !!last_column_name)
  }
  
  #print(head(my_var))
  my_var
  
})

fahrzeuge_relation_tabelle <- bind_rows(bestandteile_fahrzeuge)%>%
  distinct()

#bestandteile_fahrzeuge_big_df <- bind_rows(bestandteile_fahrzeuge)

fahrzeuge_paths <- fahrzeuge_dateien[5:length(fahrzeuge_dateien)]
fahrzeuge_paths_indexes <- 1:5
names(fahrzeuge_paths_indexes) <- fahrzeuge_paths

fahrzeuge <- lapply(fahrzeuge_paths, read_data_from_file_to_tibble_for_rel_tabelle, my_func = function(my_var, file_name){
  
  z <- fahrzeuge_paths_indexes[file_name] # Hilf jede einzige Datei spezifisch zu handeln
  if (z == 1){
    #print(paste("z : ", z))
    my_var <- my_var %>% 
      mutate(fehlerhaft_year = year(Fehlerhaft_Datum), Fehlerhaft_Fahrleistung = as.double(Fehlerhaft_Fahrleistung)) %>%
      filter(fehlerhaft_year >= 2014, fehlerhaft_year <= 2016) %>%
      select(-V1, -X1, -Herstellernummer, -Werksnummer, -Fehlerhaft_Fahrleistung, -Produktionsdatum)
  }else if(z == 2) {
    #print(paste("z : ", z))
    my_var <- my_var %>% 
      mutate(fehlerhaft_year = year(Fehlerhaft_Datum), Fehlerhaft = as.integer(0)) %>%
      filter(fehlerhaft_year >= 2014, fehlerhaft_year <= 2016) %>%
      select(-V1, -X, -X1, -Herstellernummer, -Werksnummer, -Fehlerhaft_Fahrleistung, -days, -fuel, -engine)
  }else if(z == 3){
    #print(paste("z : ", z))
    my_var <- my_var %>% 
      mutate(fehlerhaft_year = year(Fehlerhaft_Datum), Fehlerhaft_Fahrleistung = as.double(Fehlerhaft_Fahrleistung)) %>%
      filter(fehlerhaft_year >= 2014, fehlerhaft_year <= 2016) %>%
      select(-V1, -X1, -Herstellernummer, -Werksnummer, -Fehlerhaft_Fahrleistung, -Produktionsdatum)
  }else if(z == 4){
    #print(paste("z : ", z))
    my_var <- my_var %>% 
      mutate(fehlerhaft_year = year(Fehlerhaft_Datum), Fehlerhaft_Fahrleistung = as.double(Fehlerhaft_Fahrleistung)) %>%
      filter(fehlerhaft_year >= 2014, fehlerhaft_year <= 2016) %>%
      select(-V1, -X1, -Herstellernummer, -Werksnummer, -Fehlerhaft_Fahrleistung, -Produktionsdatum_Origin_01011970, -origin)
  }else if(z == 5){
    #print(paste("z : ", z))
    my_var <- my_var %>% 
      mutate(fehlerhaft_year = year(Fehlerhaft_Datum), Fehlerhaft_Fahrleistung = as.double(Fehlerhaft_Fahrleistung)) %>%
      filter(fehlerhaft_year >= 2014, fehlerhaft_year <= 2016) %>%
      select(-V1, -X1, -Herstellernummer, -Werksnummer, -Fehlerhaft_Fahrleistung, -Produktionsdatum_Origin_01011970, -origin)
  }
  
  #print(head(my_var))
  my_var
  
})

fahrzeuge_big_df <- bind_rows(fahrzeuge) %>%
  distinct(ID_Fahrzeug, .keep_all =  TRUE) # For no duplicate in the table

# importing data from Geodaten folder

read_data_from_geofile_to_tibble <- function(file, encoding = "utf-8", my_func = function(my_var, file_name) {
  counter <- length(file_name)
  my_var
}) {
  fread_data <- fread(file, sep = ";", data.table = FALSE, encoding = encoding)
  as_tibble(fread_data) %>%
    my_func(file)
}

geodaten_dateien <- list_files_of_folder(ordner["Geodaten"])
geodaten_dateien_paths <- geodaten_dateien[4:5]
# geodaten <- lapply(geodaten_dateien_paths, read_data_from_file_to_tibble, encoding = "Latin-1", my_func = function(my_var){
#   if(is.integer(my_var$Werk)){
#     my_var <- my_var %>%
#       replace_na(list(Werk = 0)) %>%
#       filter(Werk %in% toupper(staedte))
#   }
#   print(glimpse(my_var))
#   my_var
# })

geo_column_names <- c("PLZ", "ORT", "Werk", "Breitengrad", "Laengengrad")
geo_cols_to_drop <- c("V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15", "V16", "V17", "V18")
gemeinde_column_names <- c("PLZ", "Gemeinde", "Laengengrad", "Breitengrad")
#geo_data1 <- as_tibble(fread(geodaten_dateien_paths[1], sep = ";", data.table = FALSE, encoding = "UTF-8", col.names = geo_column_names))
#geo_data2 <- as_tibble(fread(geodaten_dateien_paths[2], sep = ";", data.table = FALSE, drop = geo_cols_to_drop, col.names = geo_column_names))
geo_data3 <- as_tibble(fread(geodaten_dateien[1], sep = ";", data.table = FALSE, encoding = "UTF-8", drop = c("V1", "X"), col.names = gemeinde_column_names))

#Aufbereitung der Geodaten
# geo_data1_filtered <- geo_data1 %>%
#   filter(!is.na(Werk)) %>%
#   filter(ORT %in% toupper(staedte))%>%
#   select(-Werk) %>%
#   rename(Gemeinde = ORT)
# geo_data2_filtered <- geo_data2 %>%
#   filter(!is.na(Werk)) %>%
#   filter(ORT %in% toupper(staedte)) %>%
#   select(-Werk)%>%
#   rename(Gemeinde = ORT)
geo_data3_filtered <- geo_data3 %>%
  filter(!is.na(Gemeinde)) %>%
  filter(Gemeinde %in% toupper(staedte))
#geo_data_filtered <- list(geo_data1_filtered, geo_data2_filtered, geo_data3_filtered)
geo_data_filtered <- list(geo_data3_filtered)

geodaten_big_df <- bind_rows(geo_data_filtered)

# importing Zulassung Data
zulassung_dateien <- list_files_of_folder(ordner["Zulassungen"])

zulassungen <- read_csv2(zulassung_dateien, col_select =c("IDNummer","Gemeinden"))

# Aufbereitung der Geodaten

#die Zulassungen nach Gemeinden filtern
zulassungen_filtered <- zulassungen %>%
  mutate(Gemeinden = trimws(toupper(Gemeinden))) %>%  # Leerzeichen entfernen & alles groß schreiben
  filter(Gemeinden %in% c("KOELN", "DORTMUND", "BIELEFELD", "BONN", "OBERHAUSEN", "BOCHUM", "MUENSTER", "AACHEN"))

zulassung_with_breitengrad_and_laengengrad <- zulassungen_filtered %>%
  inner_join(geo_data3, by = c("Gemeinden" = "Gemeinde"))

# Verknüpft die Zulassungsdaten mit den Fahrzeugdaten basierend auf der Fahrzeug-ID.
# inner_join fahrzeuge_big_df with zulassung_with_breitengrad_and_laengengrad
fahrzeuge_geodaten_df <- zulassung_with_breitengrad_and_laengengrad %>%
  inner_join(fahrzeuge_big_df, by = c("IDNummer" = "ID_Fahrzeug")) %>%
  rename(ID_F = IDNummer)

# Verknüpft die Fahrzeuge mit den Fahrzeugsrelationstabelle basierend auf der ID_F.
fahrzeuge_with_rel_tab_df <- fahrzeuge_geodaten_df %>%
  inner_join(fahrzeuge_relation_tabelle, by = "ID_F")


einzelteil_dateien_paths <- list_files_of_folder(ordner["Einzelteil"])
einzelteil_csv_dateien_paths <- einzelteil_dateien_paths[grep(pattern = "\\.csv$", x = einzelteil_dateien_paths)] # .csv dateien
einzelteil_txt_dateien_paths <- einzelteil_dateien_paths[grep(pattern = "\\.txt$", x = einzelteil_dateien_paths)] # .txt dateien

einzelteil_csv_dateien_paths_indexes <- 1:length(einzelteil_csv_dateien_paths)
names(einzelteil_csv_dateien_paths_indexes) <- einzelteil_csv_dateien_paths

csv_einzelteile <- lapply(einzelteil_csv_dateien_paths, read_data_from_file_to_tibble_for_rel_tabelle, my_func = function(my_var, file_name){
  z <- einzelteil_csv_dateien_paths_indexes[file_name]
  
  if (z %in% c(1, 3, 4, 5, 7, 8, 11, 12, 13, 15, 16, 19, 20, 22)){
    #print(paste("z : ", z))
    my_var <- my_var %>%
      select(all_of(c(3, 6, 7))) %>%
      rename("ID_T" = colnames(my_var)[3])
  } else {
    #print(paste("z : ", z))
    my_var <- my_var %>%
      select(all_of(c(3, 7, 8))) %>%
      rename("ID_T" = colnames(my_var)[3])%>%
      rename("Fehlerhaft" = colnames(my_var)[7])%>%
      rename("Fehlerhaft_Datum" = colnames(my_var)[8])
  }
  #print(paste("z : ", z))
  #print(head(my_var))
  my_var #%>%
})

csv_einzelteile_big_df <- bind_rows(csv_einzelteile)  

txt <- readLines(einzelteil_dateien_paths[1])
txt <- gsub(pattern = " | | ", replacement = ",", x = txt, fixed = TRUE)
txt <- gsub(pattern = " ", replacement = "\n", x = txt, fixed = TRUE)
part_T01 <- fread(text = txt, sep = ",", data.table = FALSE)


txt <- readLines(einzelteil_txt_dateien_paths[2])
txt <- gsub(pattern = "\t", replacement = "\n", x = txt, fixed = TRUE)
txt <- gsub(pattern = "  ", replacement = ",", x = txt, fixed = TRUE)
part_T02 <- fread(text = txt, sep = ",", data.table = FALSE)


txt <- readLines(einzelteil_dateien_paths[3])
txt <- gsub("|",",",txt, fixed = TRUE)
txt <- gsub("\v","\n",txt, fixed = TRUE)
part_T03 <- fread(text = txt, sep = ",", data.table = FALSE)
head(as_tibble(part_T03))

# korrekt
txt <- readLines(einzelteil_txt_dateien_paths[length(einzelteil_txt_dateien_paths) - 12])
txt <- gsub("\"\"", "\"\n\"",txt, fixed = TRUE)
txt <- gsub("\t", ",",txt, fixed = TRUE)
part_T07 <- fread(text = txt, sep = ",", data.table = FALSE)

txt <- readLines(einzelteil_dateien_paths[9])
txt <- gsub("\\\\", ';', txt)  # Ersetzt "\" durch ";" oder ein anderes Trennzeichen
txt <- gsub("\v", "\n", txt)
part_T09 <- fread(text = txt, sep = ";", data.table = FALSE, na.strings = "NA")

# korrekt
txt <- readLines(einzelteil_txt_dateien_paths[length(einzelteil_txt_dateien_paths) - 10])
txt <- gsub("\f", "\n", txt)  # Form Feed (``) durch echte Zeilenumbrüche ersetzen
part_T11 <- fread(text = txt, sep = "\t", quote = "\"", data.table = FALSE, na.strings = "NA")

# korrekt
txt <- readLines(einzelteil_txt_dateien_paths[length(einzelteil_txt_dateien_paths) - 9])
txt <- sub("\"\t\"", "\"\n\"", txt, fixed = TRUE)
txt <- gsub("NA \"", "NA\n\"",txt, fixed = TRUE)
txt <- gsub("NA\t\"", "NA\n\"",txt, fixed = TRUE)
txt <- gsub(" | | ", ",",txt, fixed = TRUE)
part_T16 <- fread(text = txt, sep = ",", data.table = FALSE)

# korrekt
txt <- readLines(einzelteil_txt_dateien_paths[length(einzelteil_txt_dateien_paths) - 8])
txt <- gsub("\" \"", "\"\n\"", txt, fixed = TRUE)
txt <- gsub(" | | ", ",",txt, fixed = TRUE)
part_T20 <- fread(text = txt, sep = ",", data.table = FALSE)

# korrekt
txt <- readLines(einzelteil_txt_dateien_paths[length(einzelteil_txt_dateien_paths) - 7])
txt <- sub("\"\"", "\"\n\"", txt, fixed = TRUE)
txt <- gsub("NA\"", "NA\n\"",txt, fixed = TRUE)
txt <- gsub("\t", ",",txt, fixed = TRUE)
part_T22 <- fread(text = txt, sep = ",", data.table = FALSE)

txt <- readLines(einzelteil_txt_dateien_paths[length(einzelteil_txt_dateien_paths) - 6])
txt <- gsub("  ", ";", txt)
txt <- gsub("\f", "\n", txt)
part_T24 <- fread(text = txt, sep = ";", data.table = FALSE)


txt <- readLines(einzelteil_txt_dateien_paths[length(einzelteil_txt_dateien_paths) - 5])
# Ersetze " | | " durch Kommas
txt <- gsub(" \\| \\| ", ",", txt)
txt <- gsub("\a", "\n", txt)
part_T27 <- fread(text = txt, sep = ",", data.table = FALSE)


# korrekt
txt <- readLines(einzelteil_txt_dateien_paths[length(einzelteil_txt_dateien_paths) - 4])
txt <- gsub("  ",",",txt, fixed = TRUE)
txt <- gsub("","\n",txt, fixed = TRUE)
part_T31 <- fread(text = txt, sep = ",", data.table = FALSE)

# korrekt
txt <- readLines(einzelteil_txt_dateien_paths[length(einzelteil_txt_dateien_paths) - 3])
txt <- gsub(pattern = "\"\"", replacement = "\"\n\"", x = txt, fixed = TRUE)
txt <- gsub(pattern = " | | ", replacement = ",", x = txt, fixed = TRUE)
part_T34 <- fread(text = txt, sep = ",", data.table = FALSE)


# korrekt
txt <- readLines(einzelteil_txt_dateien_paths[length(einzelteil_txt_dateien_paths) - 2])
txt <- sub("\"X1\"", "\"V1\"\\\"X1\"", txt, fixed = TRUE)
txt <- sub(pattern = "\"\"", replacement = "\"\n\"", x = txt, fixed = TRUE)
txt <- gsub("NA\"", "NA\n\"",txt, fixed = TRUE)
txt <- gsub("\\", ",",txt, fixed = TRUE)
part_T35 <- fread(text = txt, sep = ",", data.table = FALSE)


#korrekt
txt <- readLines(einzelteil_txt_dateien_paths[length(einzelteil_txt_dateien_paths) - 1])
txt1 <- gsub(pattern = "  ", replacement = ",", x = txt, fixed = TRUE)
txt2 <- gsub(pattern = " ", replacement = "\n", x = txt1, fixed = TRUE)
part_T36 <- fread(text = txt2, sep = ",", data.table = FALSE)


txt <- readLines(einzelteil_txt_dateien_paths[length(einzelteil_txt_dateien_paths)])
txt <- gsub("\\",",",txt, fixed = TRUE)
txt <- gsub("\a","\n",txt, fixed = TRUE)
part_T39 <- fread(text = txt, sep = ",", data.table = FALSE)


txt_einzelteile_list <- list(part_T01, part_T02, part_T03, part_T07, part_T09, part_T11, part_T16,
                             part_T20, part_T22, part_T24, part_T27, part_T31, part_T34, part_T35, part_T36, part_T39)
names(txt_einzelteile_list) <- einzelteil_txt_dateien_paths

# Aufbereiten der .txt Einzelteile Dateien

convert_txt_einzelteile_to_tibble <- function(file, my_func = function(my_var, file_name){ 
  counter <- filename
  my_var
}){
  my_df <- txt_einzelteile_list[[file]]
  as_tibble(my_df) %>%
    my_func(file)
}

txt_einzelteile_paths_index <- 1:length(einzelteil_txt_dateien_paths)
names(txt_einzelteile_paths_index) <- einzelteil_txt_dateien_paths

txt_einzelteile <- lapply(einzelteil_txt_dateien_paths, convert_txt_einzelteile_to_tibble, my_func = function(my_var, file_name){
  
  z <- txt_einzelteile_paths_index[file_name]
  
  if (z %in% c(3, 4, 6, 8, 11, 12, 13, 15)){
    #print(paste("z : ", z))
    my_var <- my_var %>%
      select(all_of(c(3, 6, 7))) %>%
      rename("ID_T" = colnames(my_var)[3])
  } else {
    #print(paste("z : ", z))
    my_var <- my_var %>%
      select(all_of(c(3, 7, 8))) %>%
      rename("ID_T" = colnames(my_var)[3])%>%
      rename("Fehlerhaft" = colnames(my_var)[7])%>%
      rename("Fehlerhaft_Datum" = colnames(my_var)[8])
  }
  #print(paste("z : ", z))
  #print(head(my_var))
  my_var
  
})

txt_einzelteile_big_df <- bind_rows(txt_einzelteile)

# final table of einzelteile
einzelteile_big_df <- bind_rows(list(csv_einzelteile_big_df, txt_einzelteile_big_df)) %>%
  filter(!is.na(ID_T))

# Komponente Dateien

komponenten_dateien_paths <- list_files_of_folder(ordner["Komponente"])

komponenten_csv_dateien_paths <- komponenten_dateien_paths[grep(pattern = "\\.csv$", x = komponenten_dateien_paths)] # alle .csv dateien

csv_bestandteile_komponenten_paths <- komponenten_csv_dateien_paths[grep(pattern = "Bestandteile_Komponente", x = komponenten_csv_dateien_paths)] # .csv Bestandteile_Kompoenente dateien

csv_komponenten_paths <- komponenten_csv_dateien_paths[grep(pattern = "/Komponente/Komponente", x = komponenten_csv_dateien_paths)] # .csv Komponenten dateien

txt_komponenten_paths <- komponenten_dateien_paths[grep(pattern = "\\.txt$", x = komponenten_dateien_paths)] # .txt Komponenten dateien

csv_bestandteile_komponenten_paths_indexes <- 1:16
names(csv_bestandteile_komponenten_paths_indexes) <- csv_bestandteile_komponenten_paths

#z <- 1 # Hilfsindex zum Handeln der Datei
csv_bestandteile_komponenten <- lapply(csv_bestandteile_komponenten_paths, read_data_from_file_to_tibble_for_rel_tabelle, my_func = function(my_var, file_name){
  tibble_column_names <- names(my_var)
  first_column_name <- tibble_column_names[1]
  last_column_name <- tibble_column_names[length(tibble_column_names)]
  last_coumn_index <- length(tibble_column_names)
  id_komponente_column_name <- "ID_K"
  z <- csv_bestandteile_komponenten_paths_indexes[file_name]
  #print(paste("first Spalte: ", first_column_name))
  #print(paste("last Spalte: ", last_column_name))
  if (z < 7){
    #print(paste("z : ", z))
    my_var <- my_var %>%
      slice(-1) %>%
      select(- !!first_column_name) %>%
      pivot_longer(! (!!last_column_name), names_to = NULL, values_to = "ID_T") %>%
      rename(!!id_komponente_column_name := !!last_column_name)
  } else if( z == 7){
    second_column_name <- tibble_column_names[2]
    cols_to_delete <- c(first_column_name, second_column_name)
    #print(paste("z : ", z))
    my_var <- my_var %>%
      select(- !!first_column_name) %>%
      select(-X) %>%
      pivot_longer(! (!!last_column_name), names_to = NULL, values_to = "ID_T") %>%
      rename(!!id_komponente_column_name := !!last_column_name)
  }else {
    #print(paste("z : ", z))
    my_var <- my_var %>%
      select(- !!first_column_name) %>%
      pivot_longer(! (!!last_column_name), names_to = NULL, values_to = "ID_T") %>%
      rename(!!id_komponente_column_name := !!last_column_name)
  }
  
  #print(head(my_var))
  my_var #%>%
  
})

komponenten_relation_tabelle <- bind_rows(csv_bestandteile_komponenten)%>%
  distinct() # For no duplicate in the table

csv_komponenten_paths_indexes <- 1:10
names(csv_komponenten_paths_indexes) <- csv_komponenten_paths

csv_komponenten <- lapply(csv_komponenten_paths, read_data_from_file_to_tibble_for_rel_tabelle, my_func = function(my_var, file_name){
  
  z <- csv_komponenten_paths_indexes[file_name]
  
  if (z %in% c(1, 2, 4, 7, 10)){
    #print(paste("z : ", z))
    my_var <- my_var %>%
      select(all_of(c(3, 6))) %>%
      rename("ID_K" = colnames(my_var)[3])
  } else {
    #print(paste("z : ", z))
    my_var <- my_var %>%
      select(all_of(c(3, 7))) %>%
      rename("ID_K" = colnames(my_var)[3])%>%
      rename("Fehlerhaft" = colnames(my_var)[7])
  }
  
  #print(head(my_var))
  my_var #%>%
  
})

csv_komponenten_big_df <- bind_rows(csv_komponenten)

# Einlesen von .txt Kom

txt <- readLines(komponenten_dateien_paths[20])
txt <- gsub("\\", ",", txt, fixed = TRUE)
txt <- gsub("\t","\n",txt, fixed = TRUE)
Komponente_K1DI2 <- fread(text = txt, sep = ",", data.table = FALSE)

txt <- readLines(komponenten_dateien_paths[21])
txt <- gsub("II", ",", txt, fixed = TRUE)
txt <- gsub("\v", "\n", txt, fixed = TRUE)
Komponente_K1LE1 <- fread(text = txt, sep = ",", data.table = FALSE)

txt <- readLines(komponenten_dateien_paths[22])
txt <- gsub("\\", ",", txt, fixed = TRUE)
Komponente_K1LE2 <- fread(text = txt, sep = ",", data.table = FALSE)


txt <- readLines(komponenten_dateien_paths[23])
txt <- gsub("|", ",", txt, fixed = TRUE)
Komponente_K2ST1 <- fread(text = txt, sep = ",", data.table = FALSE)

txt <- readLines(komponenten_dateien_paths[26])
txt <- gsub("\\", ",", txt, fixed = TRUE)
Komponente_K3AG2 <- fread(text = txt, sep = ",", data.table = FALSE)


txt <- readLines(komponenten_dateien_paths[32])
txt <- gsub("\t", ",", txt, fixed = TRUE)
Komponente_K7 <- fread(text = txt, sep = ",", data.table = FALSE)

txt_komponenten_list <- list(Komponente_K1DI2, Komponente_K1LE1, Komponente_K1LE2,
                             Komponente_K2ST1, Komponente_K3AG2, Komponente_K7)

names(txt_komponenten_list) <- txt_komponenten_paths

# Aufbereiten der .txt Einzelteile Dateien

convert_txt_komponenten_to_tibble <- function(file, my_func = function(my_var, file_name){ 
  counter <- filename
  my_var
}){
  my_df <- txt_komponenten_list[[file]]
  as_tibble(my_df) %>%
    my_func(file)
}

txt_komponenten_paths_indexes <- 1:length(txt_komponenten_paths)
names(txt_komponenten_paths_indexes) <- txt_komponenten_paths

txt_komponenten <- lapply(txt_komponenten_paths, convert_txt_komponenten_to_tibble, my_func = function(my_var, file_name){
  
  z <- txt_komponenten_paths_indexes[file_name]
  
  if (z %in% c(1, 3, 5, 6)){
    #print(paste("z : ", z))
    my_var <- my_var %>%
      select(all_of(c(3, 6))) %>%
      rename("ID_K" = colnames(my_var)[3])
  } else {
    #print(paste("z : ", z))
    my_var <- my_var %>%
      select(all_of(c(3, 7))) %>%
      rename("ID_K" = colnames(my_var)[3])%>%
      rename("Fehlerhaft" = colnames(my_var)[7])
  }
  
  #print(head(my_var))
  my_var
  
})

txt_komponenten_big_df <- bind_rows(txt_komponenten)

komponenten_big_df <- bind_rows(csv_komponenten_big_df, txt_komponenten_big_df) %>%
  filter(!is.na(ID_K))

# Verknüpft die Fahrzeuge mit den Komponenten basierend auf der ID_K.
fahrzeuge_mit_komponenten <- fahrzeuge_with_rel_tab_df %>%
  inner_join(komponenten_big_df, by = "ID_K", suffix = c("_F", "_K"))

# Verknüpft die fahrzeuge_mit_Komponenten mit den komponenten_relation_tabelle basierend auf der ID_K
fahrzeuge_komponenten_mit_rel_tab <- fahrzeuge_mit_komponenten %>%
  inner_join(komponenten_relation_tabelle, by = "ID_K")

# Verknüpft die fahrzeuge_komponenten_mit_rel_tab mit einzelteile basierend auf der ID_K
fahrzeuge_komponenten_einzelteile <- fahrzeuge_komponenten_mit_rel_tab %>%
  inner_join(einzelteile_big_df, by = "ID_T", suffix = c("_F", "_T")) %>%
  select(-PLZ) %>%
  rename(Fehlerhaft_T = Fehlerhaft)

final_data_table <- fahrzeuge_komponenten_einzelteile %>%
  filter(Fehlerhaft_F == 1 | Fehlerhaft_K == 1 | Fehlerhaft_T == 1) %>%
  filter(Fehlerhaft_T == 1)%>% # Nur fehlerhafte Einzelteile interessieren uns
  mutate(fehlerhaft_year_T = year(as.Date(Fehlerhaft_Datum_T))) %>%
  filter(fehlerhaft_year_T >= 2014, fehlerhaft_year_T <= 2016) %>% # Nur fehlerhafte Einzelteile zwischen 2014 und 2016 interessieren uns
  select(-fehlerhaft_year_T, -fehlerhaft_year)

library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)
library(lubridate)
library(tidyr)
library(DT)


data <- data.frame(final_data_table)
ortsdaten <- data.frame(
  Ort = c("KOELN", "DORTMUND", "BIELEFELD", "BONN", "OBERHAUSEN", "BOCHUM", "MUENSTER", "AACHEN"),
  Lat = c(50.938107, 51.514171, 52.020578, 50.734245, 51.470317, 51.482699, 51.961908, 50.773516),
  Lon = c(6.957068,  7.464165, 8.53212, 7.099187,  6.852291, 7.217177, 7.626804, 6.0848),
  stringsAsFactors = FALSE
)

# UI für die Shiny-App
ui <- fluidPage(
  
  titlePanel(
    tags$div(
      tags$img(src = "https://media.licdn.com/dms/image/v2/C4E0BAQHliuj-kkUZ0g/company-logo_200_200/company-logo_200_200/0/1630611581654/technische_universitt_berlin_qualittswissenschaft_logo?e=2147483647&v=beta&t=bwTQCMhcNYNXi5qIdUWon1utjG1IMscNGqTNvvp4HKw", height = "50px"),
      "Case Study Appshiny Gruppe 07"
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      # Filter für den Ort
      selectInput("selected_ort", "Wählen Sie den Ort:", choices = c("Alle", "KOELN", "DORTMUND", "BIELEFELD", "BONN", "OBERHAUSEN", "BOCHUM", "MUENSTER", "AACHEN"), selected = "Alle"),
      
      selectInput("selected_teil", "Wählen Sie ein Teil:", choices = c("Alle", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40"), selected = "Alle"),
    ),      
    
    
    mainPanel(
      tabsetPanel(
        
        # Tab für das Balkendiagramm
        tabPanel("Balkendiagramm",
                 plotOutput("bar_chart"),
        ),
        
        # Tab für Ausfallverlauf und Prognose
        tabPanel("Ausfallverlauf & Prognose",
                 plotOutput("ausfall_plot"),
                 DTOutput("prognose_table")
        ),
        
        # Tab für interaktive Karte
        tabPanel("Karte",
                 leafletOutput("map")
        ),
        
        # Tab für zugrunde liegende Daten
        tabPanel("Daten",
                 DTOutput("data_table")
        )
      )
    )
  )
)

# Server-Logik für die Shiny-App
server <- function(input, output, session) {
  
  # Filtern der Daten basierend auf der Auswahl des Ortes
  filtered_data <- reactive({
    if (input$selected_ort == "Alle") {
      return(data)
    } else {
      return(data %>% filter(data$Gemeinden == input$selected_ort))
    }
  })
  selected_data <- reactive({
    data_filtered <- filtered_data()  # Erst nach Ort filtern
    
    if (input$selected_teil == "Alle") {
      return(data_filtered)
    } else {
      return(data_filtered %>% mutate(Teilenummer = gsub("-.*", "", ID_T)) %>%  # Entfernt alles nach dem Bindestric
               filter(
                 if (input$selected_teil != "Alle") {
                   # Wenn der ausgewählte Wert ein Präfix wie "1-" oder "2-" enthält, benutze einen Regex
                   if (grepl("-", input$selected_teil)) {
                     # Verwende Regex, um Teile zu filtern, die mit dem Präfix beginnen (z.B. "1-" oder "2-")
                     grepl(paste("^", input$selected_teil, sep=""), Teilenummer)
                   } else {
                     # Ansonsten vergleiche mit der vollständigen Teilenummer
                     Teilenummer == input$selected_teil
                   }
                 } else TRUE
               ))
    }
  })
  
  # Balkendiagramm erstellen
  output$bar_chart <- renderPlot({
    # Verwende die bereits gefilterten Daten
    df_filtered <- selected_data()
    
    # Stelle sicher, dass das Datum im richtigen Format ist
    df_filtered$Fehlerhaft_Datum_T <- as.Date(df_filtered$Fehlerhaft_Datum_T, format="%Y-%m-%d")
    
    # Daten von 2014 bis 2016 filtern
    df_filtered <- df_filtered %>%
      filter(year(Fehlerhaft_Datum_T) >= 2014 & year(Fehlerhaft_Datum_T) <= 2016)
    
    # Anzahl der Fehler pro Monat berechnen
    df_counts <- df_filtered %>%
      mutate(Monat = floor_date(Fehlerhaft_Datum_T, "month")) %>%
      group_by(Monat) %>%
      summarise(Anzahl_Ausfaelle = n())
    
    # Balkendiagramm erstellen
    ggplot(df_counts, aes(x = Monat, y = Anzahl_Ausfaelle)) +
      geom_bar(stat="identity", fill="steelblue") +
      labs(title="Monatliche Ausfälle von T (gefiltert nach Ort & Teil)",
           x="Monat / Jahr",
           y="Anzahl der Ausfälle") +
      theme_minimal()
  })
  
  
  # Ausfallverlauf und Prognose für das ausgewählte Teil
  
  output$ausfall_plot <- renderPlot({
    df_filtered <- selected_data()
    
    # Stelle sicher, dass das Datum im richtigen Format ist
    df_filtered$Fehlerhaft_Datum_T <- as.Date(df_filtered$Fehlerhaft_Datum_T, format="%Y-%m-%d")
    
    # Daten von 2014 bis 2016 filtern
    df_filtered <- df_filtered %>%
      filter(year(Fehlerhaft_Datum_T) >= 2014 & year(Fehlerhaft_Datum_T) <= 2016)
    
    
    # Anzahl der Fehler pro Monat berechnen
    df_counts <- df_filtered %>%
      mutate(Monat = floor_date(Fehlerhaft_Datum_T, "month")) %>%
      group_by(Monat) %>%
      summarise(Anzahl_Ausfaelle = n())
    
    ggplot(df_counts, aes(x = Monat, y = Anzahl_Ausfaelle)) +
      geom_line(color = "steelblue", size = 1) +
      geom_point(color = "steelblue", size = 2) +
      theme_minimal() +
      labs(title = paste("Ausfallverlauf für Teil Nummer ", input$selected_teil, " im Ort ", input$selected_ort), 
           x="Monat / Jahr",
           y="Anzahl der Ausfälle")
  })
  # Prognosetabelle für das ausgewählte Teil anzeigen
  # Reaktive Funktion für das Filtern der Daten
  df_filtered <- reactive({
    data %>%
      mutate(Teilenummer = gsub("-.*", "", ID_T)) %>%  # Entfernt alles nach dem Bindestrich
      filter(year(Fehlerhaft_Datum_T) %in% c(2014, 2015, 2016)) %>% 
      # 🔍 Filter für den gewählten Ort
      filter(if (input$selected_ort != "Alle") Gemeinden == input$selected_ort else TRUE) %>%
      # 🔍 Filter für die gewählte Teilenummer
      filter(
        if (input$selected_teil != "Alle") {
          # Wenn der ausgewählte Wert ein Präfix wie "1-" oder "2-" enthält, benutze einen Regex
          if (grepl("-", input$selected_teil)) {
            # Verwende Regex, um Teile zu filtern, die mit dem Präfix beginnen (z.B. "1-" oder "2-")
            grepl(paste("^", input$selected_teil, sep=""), Teilenummer)
          } else {
            # Ansonsten vergleiche mit der vollständigen Teilenummer
            Teilenummer == input$selected_teil
          }
        } else TRUE
      )
  })
  
  # Reaktive Funktion für die Berechnungen der Empfehlungen
  recommendations <- reactive({
    df_filtered() %>%
      group_by(Teilenummer) %>%
      summarise(Ausfälle = n()) %>%
      mutate(Empfohlene_Bestellung = ceiling(Ausfälle / 12)) %>%
      arrange(desc(Empfohlene_Bestellung))
  })
  
  # DT-Output für die Empfehlungen
  output$prognose_table <- renderDT({
    # Datatable wird hier mit den Empfehlungen aus recommendations() gefüllt
    datatable(recommendations(), options = list(pageLength = 10))
  })
  
  
  
  # Interaktive Karte
  
  
  # Karte rendern und Klick-Event hinzufügen
  # Reaktive Variable für den angeklickten Ort
  clicked_ort <- reactiveVal(NULL)
  
  # Karte rendern und Klick-Event hinzufügen
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(
        data = ortsdaten,
        lat = ~Lat, lng = ~Lon,
        layerId = ~Ort  # Die Ortsspalte wird als LayerID verwendet
      )
  })
  
  # Wenn ein Marker geklickt wird, den Ort speichern
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    clicked_ort(click$id)  # Speichert den geklickten Ort
  })
  
  # Filter basierend auf dem angeklickten Ort und Empfohlene Bestellung
  df_filtered2 <- reactive({
    # Hole den aktuell ausgewählten Ort
    ort <- clicked_ort()
    
    # Filtere nach dem Ort und der Teilenummer
    data %>%
      mutate(Teilenummer = gsub("-.*", "", ID_T)) %>%  # Entfernt alles nach dem Bindestrich
      filter(year(Fehlerhaft_Datum_T) %in% c(2014, 2015, 2016)) %>% 
      # Wenn ein Ort ausgewählt ist, filtere nach dem Ort
      filter(if (!is.null(ort)) Gemeinden == ort else TRUE) %>%
      filter(if (input$selected_teil != "Alle") Teilenummer == input$selected_teil else TRUE)
  })
  
  # Berechnungen für Empfehlungen basierend auf dem Filter
  recommendations2 <- reactive({
    df_filtered2() %>%
      group_by(Teilenummer, Gemeinden) %>%  # Gruppierung nach Teilenummer und Ort
      summarise(Ausfälle = n()) %>%
      mutate(Empfohlene_Bestellung = ceiling(Ausfälle / 12)) %>%
      arrange(desc(Empfohlene_Bestellung))
  })
  
  # Jetzt die Karte mit den dynamischen Popups rendern
  observe({
    # Sicherstellen, dass ein Ort ausgewählt wurde
    req(clicked_ort())  
    
    # Holen die Koordinaten des geklickten Ortes
    lon <- ortsdaten$Lon[ortsdaten$Ort == clicked_ort()]
    lat <- ortsdaten$Lat[ortsdaten$Ort == clicked_ort()]
    
    # Berechne die empfohlene Bestellung
    empfohlene_bestellung <- recommendations2() %>%
      filter(Gemeinden == clicked_ort(), Teilenummer == input$selected_teil) %>%
      pull(Empfohlene_Bestellung)
    
    # Füge Popup hinzu
    leafletProxy("map") %>%
      clearPopups() %>%  # Entferne vorherige Popups
      addPopups(
        lng = lon,
        lat = lat,
        popup = paste(
          "Ort:", clicked_ort(), "<br>",
          "Empfohlene Menge für Teil", input$selected_teil, ":",
          ifelse(length(empfohlene_bestellung) > 0, empfohlene_bestellung, "Keine Empfehlung verfügbar")
        )
      )
  })
  
  
  
  
  # Datentabelle anzeigen
  output$data_table <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 10))
  })
}

# Shiny-App starten
shinyApp(ui = ui, server = server)
