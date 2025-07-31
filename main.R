
# setup -------------------------------------------------------------------
# installer les différents packages utiles
install.packages("tidyverse")
install.packages("openxlsx")
#install.packages("WDI")



# library -----------------------------------------------------------------
# charger les différents packages nécessaires
library(tidyverse)
library(openxlsx)
library(dplyr)
#library(WDI)



# wrangling ---------------------------------------------------------------


# Objectif 1. Construire notre base de données de travail


# Extraire les données utiles des feuilles de la base de données wuenic24
# Commencer par identifier toutes les feuilles présentes
sheets <- getSheetNames("data/raw/data_wuenic.xlsx")


# Conserver les feuilles avec les données sur les 16 vaccins
vaccine <- sheets[sheets != "regional_global"]

# Importer les informations sur les vaccins.
# Transformer la structure de chaque feuille pour créer les variables year
# et immune_pourcentage. Consolider toutes les infos de tous les 16 vaccins
# dans une seule base de données.

data_vaccine_immune <- map_df(vaccine, ~{
  read.xlsx("data/raw/data_wuenic.xlsx", sheet = .x) %>%
    pivot_longer(
      cols = matches("^[0-9]{4}$"),
      names_to = "year",
      values_to = "immune_pourcentage"
    ) %>%
    mutate(
      year = as.integer(year),
      #source_sheet = .x
    )
})


# Importer la base de données de couverture vaccinale
data_vaccine_coverage <- read.xlsx("data/raw/data_wuenic.xlsx", sheet = "regional_global") %>%
  mutate(
    year = as.integer(year)
  )


# Traitement de données sur la base data_vaccine_coverage

# Problème 1. Le chiffre <100, <1000 dans data_vaccine_coverage
# Nous avons remarqué cette valeur, inhabituelle dans le dataset.

summary(data_vaccine_coverage)
str(data_vaccine_coverage)

data_vaccine_coverage %>%
  filter(!grepl("^[0-9]", vaccinated)) %>%
  count(vaccinated, sort = TRUE)


# Vérifions où elle se trouve.

cols_with_lt_values <- sapply(data_vaccine_coverage, function(col) {
  any(grepl("^<\\d{1,3}(,\\d{3})*$", col))
})
columns_to_check <- names(cols_with_lt_values)[cols_with_lt_values] # Uniquement la colone vaccinated.


#Vérifions notre hypothèse que vaccinated <100 ou <1000 == 0 dans cette base

subset_rows <- data_vaccine_coverage$vaccinated == "<100"

result <- all(
  ((as.numeric(gsub(",", "", data_vaccine_coverage$unvaccinated[subset_rows])) -
      as.numeric(gsub(",", "", data_vaccine_coverage$target[subset_rows]))) == 0) &
    (data_vaccine_coverage$coverage[subset_rows] == 0)
)

print(result) # True

# Nous pouvons donc remplacer dans la colonne vaccinated, <100 par 0.
data_vaccine_coverage$vaccinated[data_vaccine_coverage$vaccinated == "<100"] <- "0"


# Problème 2. Enlever les , des chiffres dans les colonnes concernées
cols_to_clean <- c("vaccinated", "unvaccinated", "target")


# Utiliser la fonction gsub pour supprimer les virgules des chiffres

data_vaccine_coverage <- data_vaccine_coverage %>%
  mutate(across(all_of(cols_to_clean), ~ as.numeric(gsub(",", "", .)))
                )

# Problème 3. Renommage des variables pour uniformiser le nommage entre les différentes tables

# Indiquer les nouveaux noms
nom_variables <- c(
  "region" = "unicef_region"
)

# Integrer les nouveaux noms
names(data_vaccine_coverage) <- 
  ifelse(names(data_vaccine_coverage) %in%
           names(nom_variables),
         nom_variables[names(data_vaccine_coverage)],
         names(data_vaccine_coverage))

# Vérifier la présence de NA dans le dataset.

any(is.na(data_vaccine_coverage))
which(is.na(data_vaccine_coverage), arr.ind = TRUE)

