# Direct depuis NAIADES

library(downloader)
        
library(tidyverse)
                
library(utils)
                        
library(sf)

library(data.table)

`%notin%` <- Negate(`%in%`)

download(
  "https://naiades.eaufrance.fr/reports/reportsperyear/HB/Naiades_Export_France_Entiere_HB.zip",
  # "https://naiades.eaufrance.fr/reports/reportsperyear/HM/Naiades_Export_France_Entiere_HM.zip",
  dest = "data/dataset.zip",
  mode = "wb"
)
unzip("data/dataset.zip",
      exdir = "./data")
file.remove("data/cep.csv")
file.remove("data/operation.csv")
file.remove("data/resultat.csv")
file.remove("data/DescriptionDonneesHB.pdf")

france_metropolitaine <- st_read("data/FRA_adm0.shp")

Diatom <- as_tibble(fread("data/fauneflore.csv")) %>%
  dplyr::select(
    "CODE_STATION" = CdStationMesureEauxSurface,
    "Nom_groupe_taxo" = LbSupport,
    "DATE" = DateDebutOperationPrelBio,
    "SANDRE" = CdAppelTaxon,
    "Nom_latin_taxon" = NomLatinAppelTaxon,
    "RESULTAT" = RsTaxRep,
    "Code_groupe_taxo" = CdSupport
  ) %>%
  filter(Code_groupe_taxo == 10) %>%
  dplyr::select(-Code_groupe_taxo) %>%
  distinct(CODE_STATION,
           Nom_groupe_taxo,
           DATE,
           SANDRE,
           Nom_latin_taxon,
           RESULTAT) %>%
  dplyr::select(-Nom_groupe_taxo) %>%
  arrange(DATE,
          CODE_STATION,
          Nom_latin_taxon, RESULTAT) %>%
  filter(RESULTAT != 0) %>%
  mutate(DATE = as.Date(DATE)) %>%
  arrange(DATE) %>%
  group_by(CODE_STATION, DATE) %>%
  dplyr::mutate(tot = sum(RESULTAT)) %>%
  ungroup() %>%
  dplyr::mutate(RESULTAT = (RESULTAT / tot) * 1000) %>%
  dplyr::mutate(RESULTAT = round(RESULTAT, 2)) %>% # Passage des abondances en relative pour 1000
  dplyr::select(-tot) %>%
  left_join(read.csv2("data/CODE_OMNIDIA_traites.csv", stringsAsFactors = FALSE),
            by = "SANDRE") %>%
  filter(SANDRE != 0) %>%
  rename(taxon = code_omnidia) %>%
  mutate(CODE_STATION = str_remove(CODE_STATION, "^0+")) %>%
  left_join(
    as_tibble(
      read.csv2(
        "data/stations.csv",
        stringsAsFactors = FALSE,
        quote = "",
        na.strings = c("", "NA")
      )
    ) %>% select(
      CODE_STATION = CdStationMesureEauxSurface,
      commune = LbCommune,
      longitude = CoordXStationMesureEauxSurface,
      latitude = CoordYStationMesureEauxSurface
    ) %>%
      mutate(
        longitude = as.numeric(longitude),
        latitude = as.numeric(latitude)
      ) %>%
      filter(!is.na(longitude)) %>%
      filter(longitude > 0) %>%
      mutate(CODE_STATION = str_remove(CODE_STATION, "^0+")),
    by = "CODE_STATION"
  ) %>%
  drop_na() %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 2154) %>%
  st_transform(geometry, crs = 4326) %>%
  st_intersection(france_metropolitaine) %>%
  dplyr::select(CODE_STATION,
                DATE,
                SANDRE,
                Nom_latin_taxon,
                RESULTAT,
                taxon,
                commune) %>%
  tidyr::extract(geometry, c("long", "lat"), "\\((.*), (.*)\\)", convert = TRUE) %>%
  left_join(as_tibble(
    read.csv2("data/table_transcodage.csv", stringsAsFactors = FALSE)
  ) %>%
    dplyr::select(taxon = "abre", True_name = "CodeValid"),
  by = "taxon") %>%
  mutate(taxon = if_else(is.na(True_name) == T, taxon, True_name)) %>%
  dplyr::select(-True_name) %>%
  filter(!is.na(taxon)) %>%
  left_join(
    read.csv2("data/table_transcodage.csv", stringsAsFactors = FALSE) %>%
      select(taxon = CodeValid, full_name = name_valid) %>% distinct() %>%
      mutate(full_name = sub("\\_g.*", "", full_name)),
    by = "taxon"
  ) %>%
  mutate(full_name = str_replace_all(full_name, "[^[:alnum:]]", " ")) %>%
  mutate(full_name = paste0(full_name, " ", "(", taxon, ")")) %>%
  filter(commune %notin% Communes) %>%
  mutate(lon = round(long, 2), lat = round(lat, 2)) %>%
  left_join(
    read.csv2("data/table_transcodage.csv", stringsAsFactors = FALSE) %>%
      select(abre, name, taxon = CodeValid) %>% unique() %>%
      group_by(taxon) %>% filter(abre %notin% taxon) %>% mutate(list = paste0(abre, " ", sub("\\_g.*", "", name))) %>%
      mutate(taxons_apparies = paste(list, collapse = " / ")) %>%
      select(-abre,-name,-list) %>% distinct(),
    by = "taxon"
  )

save(Diatom, file = paste0("data/Donnees_compiles",Sys.Date(),".RData"))
