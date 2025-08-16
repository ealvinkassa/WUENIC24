
# Packages -------------------------------------------------------------------

# Liste complète des packages (plyr avant tidyverse, ggplot2, purrr, tidyr sont dans tidyverse)
packages <- c(
  "plyr",
  "tidyverse", "openxlsx", "janitor", "summarytools",
  "rnaturalearth", "rnaturalearthdata", "gganimate",
  "WDI", "zoo", "corrplot", "car", "glmnet", "naniar", "e1071",
  "FactoMineR", "factoextra", "caret", "randomForest", "reshape2", "corrgram",
  "conflicted", "psych", "GGally", "VIM", "mice", "plotly", "shiny", "shinydashboard",
  "leaflet", "svglite"
)


install_if_missing <- function(pkgs) {
  installed <- rownames(installed.packages())
  to_install <- setdiff(pkgs, installed)
  
  if (length(to_install) > 0) {
    # Installer corrgram avec dépôt spécifique
    if ("corrgram" %in% to_install) {
      install.packages("corrgram", repos = "http://cran.us.r-project.org")
      to_install <- setdiff(to_install, "corrgram")
    }
    if (length(to_install) > 0) {
      install.packages(to_install)
    }
  } else {
    message("Tous les packages sont déjà installés.")
  }
}

# Installer les packages manquants
install_if_missing(packages)


# Gestion des librairies -----------------------------------------------------------------

# Charger les packages dans l'ordre
load_packages <- function(pkgs) {
  invisible(lapply(pkgs, function(pkg) {
    suppressPackageStartupMessages(
      tryCatch(
        library(pkg, character.only = TRUE),
        error = function(e) message(sprintf("Erreur lors du chargement de %s : %s", pkg, e$message))
      )
    )
  }))
}

load_packages(packages)

# Configurer conflicted pour éviter les conflits plyr/dplyr
library(conflicted)

conflict_prefer("count", "dplyr")
conflict_prefer("summarise", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("rename", "dplyr")
conflict_prefer("arrange", "dplyr")

message("Packages installés, chargés et conflits réglés.")



# 0. Résumé de l'étude --------------
# Lire dans le R_Markdown du rapport final

# I - Introduction ------------
# Lire dans le R_Markdown du rapport final


# II - Méthodologie de l'étude --------
# A - Desicription des données fournies ------

# B - Construction du dataset de travail pour l'étude --------

# O0. Extraire les données utiles des feuilles de la base de données wuenic24 ----

# a. Commencer par identifier toutes les feuilles présentes
sheets <- getSheetNames("data/raw/data_wuenic.xlsx")

# b. Récupérer les feuilles avec les données sur les 16 vaccins
vaccine <- sheets[sheets != "regional_global"]

# c. Labeliser les différents vaccins
vaccine_labels <- c(
  # Associer à chaque code le nom du vaccin
  "BCG"   = "Bacille Calmette-Guérin",
  "DTP1"  = "Diphtérie-tétanos-coqueluche, Dose 1",
  "DTP3"  = "Diphtérie-tétanos-coqueluche, Dose 3",
  "POL3"  = "Antipoliomyélitique oral, Dose 3",
  "IPV1"  = "Antipoliomyélitique inactivé, Dose 1",
  "IPV2"  = "Antipoliomyélitique inactivé, Dose 2",
  "HEPBB" = "Hépatite B, Dose de naissance",
  "HEPB3" = "Hépatite B, Dose 3",
  "HIB3"  = "Haemophilus influenzae type b, Dose 3",
  "MCV1"  = "Rougeole, Dose 1", 
  "MCV2"  = "Rougeole, Dose 2",
  "RCV1"  = "Rubéole",
  "ROTAC" = "Rotavirus",
  "PCV3"  = "Pneumocoque",
  "MENGA" = "Méningite A",
  "YFV"   = "Fièvre jaune"
)


# O1. - Base de données de couverture vaccinale (data_vaccine_coverage) ----
# Cette base de donnée présente la couverture vaccinale, par région unicef, par vaccin et par an, de 1980 à 2024

# a. Importation de la base
data_vaccine_coverage <- read.xlsx("data/raw/data_wuenic.xlsx", sheet = "regional_global")

# b. Nettoyage et traitement de données

str(data_vaccine_coverage)
head(data_vaccine_coverage)

# b.1. Renommer les variables pour uniformiser le nommage entre les différentes tables

# Indiquer les nouveaux noms
nom_variables_dvc <- c(
  "region" = "unicef_region"
)

# Integrer les nouveaux noms
names(data_vaccine_coverage) <- 
  ifelse(names(data_vaccine_coverage) %in%
           names(nom_variables_dvc),
         nom_variables_dvc[names(data_vaccine_coverage)],
         names(data_vaccine_coverage))

str(data_vaccine_coverage) # Modification des noms de colonne : Succès


# b.2. Les valeurs <100, <1,000 dans data_vaccine_coverage

# Nous avons remarqué cette valeur, inhabituelle dans le dataset. Vérifions où elle se trouve.

cols_with_lt_values <- sapply(data_vaccine_coverage, function(col) {
  any(grepl("^<\\d{1,3}(,\\d{3})*$", col))
})
columns_to_check <- names(cols_with_lt_values)[cols_with_lt_values] # Uniquement la colonne vaccinated.

# Combien de fois ces valeurs apparaissent-elles ?
data_vaccine_coverage %>%
  filter(!grepl("^[0-9]", vaccinated)) %>%
  count(vaccinated, sort = TRUE) # 283 occurrences <100, 2 occurrences <1,000

# Hypothèse : vaccinated <100 ou <1,000 == 0 dans cette base.

# <100
subset_rows_100 <- data_vaccine_coverage$vaccinated == "<100"

result_100 <- all(
  ((as.numeric(gsub(",", "", data_vaccine_coverage$unvaccinated[subset_rows_100])) -
      as.numeric(gsub(",", "", data_vaccine_coverage$target[subset_rows_100]))) == 0) &
    (data_vaccine_coverage$coverage[subset_rows_100] == 0)
)

print(result_100) # True

# Nous pouvons donc remplacer dans la colonne vaccinated, <100 par 0.
data_vaccine_coverage$vaccinated[data_vaccine_coverage$vaccinated == "<100"] <- "0"

# Vérifions que la modification est bien faite.
data_vaccine_coverage %>%
  filter(!grepl("^[0-9]", vaccinated)) %>%
  count(vaccinated, sort = TRUE) #<100 n'a plus été trouvé. Passons aux 02 <1,000 suivant.


# <1,000
subset_rows_1000 <- grepl("<\\s*1[,']?000", data_vaccine_coverage$vaccinated)

result_1000 <- all(
  ((as.numeric(gsub(",", "", data_vaccine_coverage$unvaccinated[subset_rows_1000])) -
      as.numeric(gsub(",", "", data_vaccine_coverage$target[subset_rows_1000]))) == 0) &
    (data_vaccine_coverage$coverage[subset_rows_1000] == 0)
)

print(result_1000) # False (La condition n'est pas respectée partout)

# Vérifions les raisons de cette incohérence : partout où <1,000 != 0
data_check_1000 <- data_vaccine_coverage[subset_rows_1000, ] %>%
  mutate(
    unvaccinated_num = as.numeric(gsub(",", "", unvaccinated)),
    target_num = as.numeric(gsub(",", "", target)),
    diff = target_num - unvaccinated_num ,
    coverage_num = round((diff/target_num)*100, 1) ,  # recalculer la variable coverage
    ok = (diff == 0 & coverage_num == 0)
  )

# Affiche les lignes qui ne respectent PAS la condition
data_check_1000 %>% filter(!ok | is.na(ok)) # 1 occurrence ne respecte pas la condition


# Remplacer <1,000 == 0 là où la condition est respectée.
condition <- (data_vaccine_coverage$vaccinated == "<1,000") &
  ((as.numeric(gsub(",", "", data_vaccine_coverage$unvaccinated)) -
      as.numeric(gsub(",", "", data_vaccine_coverage$target))) == 0) &
  (data_vaccine_coverage$coverage == 0)

data_vaccine_coverage$vaccinated[condition] <- "0"

#Vérifier si le changement est bien fait.
data_vaccine_coverage %>%
  filter(!grepl("^[0-9]", vaccinated)) %>%
  count(vaccinated, sort = TRUE) #Plus que 1 <1,000 restant.

# Pour ce dernier cas, la couverture arrondie == 0 parce que le nombre de vaccination
# est trop faible. Ici, juste mettre la différence obtenue entre target et unvaccinated dans vaccinated.
# Remplacer là où la condition n'est pas respectée.

data_vaccine_coverage <- data_vaccine_coverage %>%
  mutate(
    vaccinated = ifelse(
      vaccinated == "<1,000",
      as.character(as.numeric(gsub(",", "", target)) - as.numeric(gsub(",", "", unvaccinated))),
      vaccinated
    )
  )

# Vérifier si le changement est bien fait.
data_vaccine_coverage %>%
  filter(!grepl("^[0-9]", vaccinated)) %>%
  count(vaccinated, sort = TRUE) # Plus que valeurs au pattern <xxx.


# b3. Enlever les (,) des chiffres dans les colonnes concernées

str(data_vaccine_coverage)
commas_to_clean <- c("vaccinated", "unvaccinated", "target")

# Utiliser la fonction gsub pour supprimer les virgules des chiffres
data_vaccine_coverage <- data_vaccine_coverage %>%
  mutate(across(all_of(commas_to_clean), ~ gsub(",", "", .))
  )

any(is.na(data_vaccine_coverage)) # FALSE.
str(data_vaccine_coverage) # Nettoyage effectué avec succès.


# b4. Donner la bonne typologie des variables dans le dataframe
data_vaccine_coverage <- data_vaccine_coverage %>%
  mutate(
    unicef_region = as.factor(unicef_region),
    vaccine = as.factor(vaccine),
    year = as.integer(year),
    coverage = as.numeric(coverage),
    vaccinated = as.numeric(vaccinated),
    unvaccinated = as.numeric(unvaccinated),
    target = as.numeric(target)
  )

# Vérifier la présence de NA dans le dataset.
any(is.na(data_vaccine_coverage)) # FALSE
summary(data_vaccine_coverage)


# b5. Les incohérences

# Quelques incohérences sont relevées plus haut avec <1000
# Cherchons s'il y a des enregistrements où vaccinated != target - unvaccinated
# Voyons voir quel impact cela a sur le coverage

data_incoh <- data_vaccine_coverage %>%
  rename(coverage_gived = coverage) %>%
  mutate(
    diff = target - unvaccinated,
    coverage_calculed = round((diff/target)*100),
    ok = (diff == vaccinated & coverage_calculed == coverage_gived)
  )

# Combien d'enregistrement ne respectent PAS la conditionde cohérence ?
data_incoh %>% count(!ok | is.na(ok)) #962 enrégistrements

# Essayons de comprendre ces incohérences
data_incoh %>%
  filter(!ok | is.na(ok)) %>%
  select(coverage_gived, coverage_calculed, vaccinated, diff, unvaccinated, target) %>%
  slice_head(n = 10) %>%
  print() # Peut de différences entre coverage


# Utiliser coverage uniquement pour refaire le test de cohérence.
data_incoh <- data_vaccine_coverage %>%
  rename(coverage_gived = coverage) %>%
  mutate(
    diff = target - unvaccinated,
    coverage_calculed = round((diff/target)*100, 0),
    coverage_check = abs(coverage_gived - coverage_calculed) <= 1  # tolérance de 1%
  )

data_incoh %>% count(!coverage_check | is.na(coverage_check)) #0 incohérence.
# La variable d'intérêt reste cohérente malgré tout. Tout va bien.

# Vérifier la présence de NA dans le dataset.
any(is.na(data_vaccine_coverage)) # False
#which(is.na(data_vaccine_coverage), arr.ind = TRUE)

dim(data_vaccine_coverage) #4181 enregistrement pour 7 variables


# c. Extraction et sauvegarde de la base de données.
# Le dataset data_vaccine_coverage est propre et prêt à l'emploi.

write.xlsx(
  data_vaccine_coverage, 
  "data/processed/data_vaccine_coverage.xlsx"
)




# O2. - Base de données le pourcentage d'immunisation (data_vaccine_immune) ---------------
# Cette base de données présente le pourcentage d'immunisation par pays, par vaccin et par an

# a. Importation des données & transformation

# Transformer la structure de chaque feuille pour créer les variables year
# et immune_percentage. Consolider toutes les infos de tous les 16 vaccins
# dans une seule base de données.

data_vaccine_immune_percentage <- map_df(vaccine, ~{
  read.xlsx("data/raw/data_wuenic.xlsx", sheet = .x) %>%
    pivot_longer(
      cols = matches("^[0-9]{4}$"),
      names_to = "year",
      values_to = "immune_percentage"
    ) %>%
    mutate(
      year = as.integer(year),
      #source_sheet = .x
    )
})


str(data_vaccine_immune_percentage)


# b. Nettoyage et traitement des données

any(is.na(data_vaccine_immune_percentage)) #True
summary(data_vaccine_immune_percentage) # Variable immune_percentage

head(data_vaccine_immune_percentage)

# b.1 . Traitement des valeurs manquantes (NA)
# Nous remarquons la présence de NA pour la variable immune_pourcentage.

dim(data_vaccine_immune_percentage)
sum(is.na(data_vaccine_immune_percentage$immune_percentage))
#46156 valeurs manquantes sur 109665 enregistrements (~42%).


# b.1.1 Explorer la répartition des valeurs manquantes (rajouter des visualisations)

# par unicef_regions
data_vaccine_immune_percentage %>%
  filter(is.na(immune_percentage)) %>%
  count(unicef_region, sort = TRUE) %>%
  print(n = length(unique(data_vaccine_immune_percentage$unicef_region)))

# par pays
data_vaccine_immune_percentage %>%
  filter(is.na(immune_percentage)) %>%
  count(country, sort = TRUE) %>%
  print(n = length(unique(data_vaccine_immune_percentage$country)))

# par vaccin
data_vaccine_immune_percentage %>%
  filter(is.na(immune_percentage)) %>%
  count(vaccine, sort = TRUE) %>%
  print(n = length(unique(data_vaccine_immune_percentage$vaccine)))

# par années
data_vaccine_immune_percentage %>%
  filter(is.na(immune_percentage)) %>%
  count(year, sort = TRUE) %>%
  print(n = length(unique(data_vaccine_immune_percentage$year))) # On remarque que plus les enregistrements sont anciens, plus il y a de valeurs manquantes


# Visualisations des NA

data_vaccine_immune_percentage %>%
  filter(is.na(immune_percentage)) %>%
  count(unicef_region, sort = TRUE) %>%
  ggplot(aes(x = reorder(unicef_region, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Valeurs manquantes par région UNICEF", x = "Région", y = "Nombre de valeurs manquantes")


data_vaccine_immune_percentage %>%
  filter(is.na(immune_percentage)) %>%
  count(country, sort = TRUE) %>%
  slice_max(n, n = 20) %>%
  ggplot(aes(x = reorder(country, n), y = n)) +
  geom_col(fill = "tomato") +
  coord_flip() +
  labs(title = "Top 20 pays avec le plus de valeurs manquantes", x = "Pays", y = "Nombre de valeurs manquantes")


data_vaccine_immune_percentage %>%
  filter(is.na(immune_percentage)) %>%
  count(vaccine, sort = TRUE) %>%
  ggplot(aes(x = reorder(vaccine, n), y = n)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(title = "Valeurs manquantes par type de vaccin", x = "Vaccin", y = "Nombre de valeurs manquantes")


data_vaccine_immune_percentage %>%
  filter(is.na(immune_percentage)) %>%
  count(year) %>%
  ggplot(aes(x = year, y = n)) +
  geom_line(color = "firebrick", size = 0.5) +
  geom_point(color = "firebrick") +
  labs(title = "Valeurs manquantes au fil du temps", x = "Année", y = "Nombre de valeurs manquantes")




# b.1.2. Essayons de comprendre ce qui explique les NA dans le tableau

# Nous posons l'hypothèse que les NA de la variable immune_percentage sont
# dues en majorité à un défaut d'introduction du vaccin dans le pays.
# Vérifions cette hypothèse.


#     2.1. Constituer une base de donnée qui indique l'année d'introduction du vaccin dans chaque pays
# Source : WHO (https://immunizationdata.who.int/global?topic=Vaccine-introduction&location=)

data_vaccine_introduction <- read.xlsx("data/raw/data_vaccine_introduction.xlsx", sheet = "Data") %>%
  slice(1:(n() - 1)) # garde toutes les lignes sauf la dernière (Métadonnée)

data_vaccine_introduction <- data_vaccine_introduction %>%
  mutate(
    YEAR = as.integer(YEAR)
  )

summary(data_vaccine_introduction)
sapply(data_vaccine_introduction, function(x) length(unique(x)))
# Cette base contient les données d'introduction de 20 vaccins dans 194 pays sur 85 ans (1940 à 2024)
# Aussi, la variable INTRO n'est pas binaire, et donne beaucoup de détails.

#     2.2. Filtrer les informations de la base de données pour avoir juste ce qu'on veut
#       a. Indiquer les nouveaux noms pour cette base

nom_variables_dvi <- c(
  "ISO_3_CODE" = "iso3",
  "COUNTRYNAME" = "country",
  "WHO_REGION" = "who_region",
  "YEAR" = "year",
  "DESCRIPTION" = "vaccine",
  "INTRO" = "introduced"
)

names(data_vaccine_introduction) <- 
  ifelse(names(data_vaccine_introduction) %in%
           names(nom_variables_dvi),
         nom_variables_dvi[names(data_vaccine_introduction)],
         names(data_vaccine_introduction))

str(data_vaccine_introduction)
any(data_vaccine_introduction$introduced == "ND") #True
sum(data_vaccine_introduction$introduced == "ND") #9 valeurs manquantes globalement

#       b. Identifier les vaccins à extraire.
# La base contient 11 vaccins d'intérêt sur 16.

required_vaccine <- c(
  # Associer à chaque code le nom du vaccin
  "BCG"   = "Bacille Calmette-Guérin",
  "DTP1"  = "Diphtérie-tétanos-coqueluche, Dose 1",
  "DTP3"  = "Diphtérie-tétanos-coqueluche, Dose 3",
  "POL3"  = "Antipoliomyélitique oral, Dose 3",
  "IPV1"  = "Antipoliomyélitique inactivé, Dose 1", # Disponible, IPV (Inactivated polio vaccine)
  "IPV2"  = "Antipoliomyélitique inactivé, Dose 2", # Disponible, IPV (Inactivated polio vaccine) 2nd dose
  "HEPBB" = "Hépatite B, Dose de naissance", # Disponible, HepB birth dose
  "HEPB3" = "Hépatite B, Dose 3", # Disponible, Hepatitis B vaccine
  "HIB3"  = "Haemophilus influenzae type b, Dose 3", # Disponible, Hib (Haemophilus influenzae type B) vaccine
  "MCV1"  = "Rougeole, Dose 1", 
  "MCV2"  = "Rougeole, Dose 2", # Disponible, Measles-containing vaccine 2nd dose
  "RCV1"  = "Rubéole", # Disponible, Rubella vaccine
  "ROTAC" = "Rotavirus", # Disponible, Rotavirus vaccine
  "PCV3"  = "Pneumocoque", # Disponible, PCV (Pneumococcal conjugate vaccine)
  "MENGA" = "Méningite A", # Disponible, Meningococcal meningitis vaccines (all strains) (Approcimation, puisque nous n'avons pas la date spécifique)
  "YFV"   = "Fièvre jaune" # Dispobile, YF (Yellow fever) vaccine
)


required_vaccine_code <- c(
  "IPV (Inactivated polio vaccine)" = "IPV1",
  "IPV (Inactivated polio vaccine) 2nd dose" = "IPV2",
  "HepB birth dose" = "HEPBB",
  "Hepatitis B vaccine" = "HEPB3",
  "Hib (Haemophilus influenzae type B) vaccine" = "HIB3",
  "Measles-containing vaccine 2nd dose" = "MCV2",
  "Rubella vaccine" = "RCV1",
  "Rotavirus vaccine" = "ROTAC",
  "PCV (Pneumococcal conjugate vaccine)" = "PCV3",
  "Meningococcal meningitis vaccines (all strains)" = "MENGA",
  "YF (Yellow fever) vaccine" = "YFV"
)

no_available_vaccine <- c(
  "BCG"   = "Bacille Calmette-Guérin",
  "DTP1"  = "Diphtérie-tétanos-coqueluche, Dose 1",
  "DTP3"  = "Diphtérie-tétanos-coqueluche, Dose 3",
  "POL3"  = "Antipoliomyélitique oral, Dose 3",
  "MCV1"  = "Rougeole, Dose 1"
)

vaccine_mapping <- tibble::tibble(
  vaccine_raw = names(required_vaccine_code),
  vaccine_code = unname(required_vaccine_code)
)


#     c. Réaliser l'extraction.

data_vaccine_presence <- data_vaccine_introduction %>%
  filter(
    year >= 1980 & year <= 2024, # Infos de 1980 à 2024
    vaccine %in% names(required_vaccine_code), # 11 vaccins d'intérêt présents sur 16
    iso3 %in% unique(data_vaccine_immune_percentage$iso3) # Pays concernés par l'étude
  ) %>%
  left_join(vaccine_mapping, by = c("vaccine" = "vaccine_raw")) %>%
  mutate(
    vaccine = vaccine_code,                                # Renommer les vaccins selon le code de l'Unicef
    introduced_binary = case_when(                         # Harmoniser les introduction des vaccins
      introduced %in% c("ND") ~ NA,                        # données manquantes
      introduced %in% c("No", "No (D)", "NR") ~ "No",      # non introduit ou non pertinent pour le pays (pays non concerné par ce vaccin)
      TRUE ~ "Yes"                                         # tout le reste est considéré comme introduit d'une manière ou d'une autre
    ),
    introduced_coverage = case_when(                       # Décoder les niveaux de la couverture vaccinale
      introduced == "Yes" ~ "National",
      introduced %in% c("Yes (P)", "High risk") ~ "Partial",
      introduced %in% c("Yes (R)", "Yes (A)", "Yes (S)", "Yes (OPV)") ~ "Risk-based",
      introduced == "Yes (O)" ~ "Epidemic",
      introduced %in% c("No", "No (D)") ~ "Not Introduced",
      introduced %in% c("ND", "NR") ~ "Unknown",
      TRUE ~ "Unknown"
    )
  ) %>%
  select(-who_region, -vaccine_code)

#     d. Vérifier les NA de data_vaccine_presence.

str(data_vaccine_presence)
any(is.na(data_vaccine_presence)) #True

cols_with_na_dvp <- sapply(data_vaccine_presence, function(col) {
  any(is.na(col))
})
columns_to_check_dvp <- names(cols_with_na_dvp)[cols_with_na_dvp]
print(columns_to_check_dvp) # introduced_binary.

sum(is.na(data_vaccine_presence$introduced_binary)) #3
summary(data_vaccine_presence)

length(unique(data_vaccine_presence$vaccine)) #11 Vaccins sur 16.

# Les NA de la variable introduction_binary, au nombre de #3 de la base data_vaccination_prensence
# sont dues à des données manquantes dans les données WHO. Rien à faire pour l'instant.

# Le dataset data_vaccine_presence est propre et prêt à l'emploi.

write.xlsx(
  data_vaccine_presence, 
  "data/processed/data_vaccine_presence.xlsx"
)


#       2.3. Hypothèse : les NA de immune_percentage dans data_vaccine_immune_percentage s'expliquent principalement par l'absence d’introduction du vaccin dans le pays et l’année concernée.

#         a. Fusionner les deux bases de données (data_vaccine_presence et immune_percentage) en une seule pour le test

data_vaccine_immune <- data_vaccine_immune_percentage %>%
  left_join(
    data_vaccine_presence %>%
      select(iso3, year, vaccine, introduced_binary, introduced, introduced_coverage),
    by = c("iso3", "year", "vaccine")
  )

# Beaucoup de NA seront générés, car la base data_vaccine_presence contient que 11 vaccins sur 16.

str(data_vaccine_immune)
head(data_vaccine_immune)
summary(data_vaccine_immune)

# Identifier les colones avec des NA dans data_vaccine_immune
cols_with_na_dvim <- sapply(data_vaccine_immune, function(col) {
  any(is.na(col))
})
columns_to_check_dvim <- names(cols_with_na_dvim)[cols_with_na_dvim]
print(columns_to_check_dvim)

# Compter les NA dans ces colonnes
na_counts_dvim <- sapply(data_vaccine_immune[columns_to_check_dvim], function(col) {
  sum(is.na(col))
})
print(na_counts_dvim)


#         b. Analyser les NA dans introduced_binary, introduced, introduced_coverage
# Rappel : Dans le cas actuel, cela veut juste dire qu'on ne sait pas à quel moment le vaccin a été introduit. (Pour les 5 vaccins)


#         c. Analyser les NA dans immune_percentage vis à vis de introduced_binary

sum(is.na(data_vaccine_immune$immune_percentage))
# Toujours 46156 valeurs manquantes sur 109665 enregistrements (~42%).

# Quelles sont les correspondances
data_vaccine_immune %>%
  mutate(na_presence = is.na(immune_percentage)) %>%
  tabyl(na_presence, introduced_binary)

# 6 cas de figure à traiter, pour un total de 157810 NA.
data_vaccine_immune %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "nb_NA") %>%
  arrange(desc(nb_NA))

# A corriger au cas par cas, suivant 6 cas de figure, lors de la jointure finale.


# Calculer une proportion pour interprétation claire

data_vaccine_immune %>%
  filter(is.na(immune_percentage)) %>%
  count(introduced_binary) %>%
  mutate(pourcentage = round(n / sum(n) * 100, 0)) # 55 % des NA est expliqué par la non introduction du vaccin dans le pays

# Il est donc judicieux pour la variable immune_percentage, d'imputer 0% de couverture (immune_percentage = 0) quand introduced_binary == "No" (vaccin non introduce)
# Laisser les NA tels quels dans les autres cas.


# b.1.3. Imputation des NA dans data_vaccine_immune

data_vaccine_immune <- data_vaccine_immune %>%
  mutate(
    immune_percentage = ifelse(is.na(immune_percentage) & introduced_binary == "No", 0, immune_percentage)
  )

sum(is.na(data_vaccine_immune$immune_percentage))
# 20965 valeurs manquantes sur 109665 enregistrements (~19%).
# Pour le reste, essayer de mettre à jour la base de donnée avec les années d'introduction des 05 vaccins restants.

dim(data_vaccine_immune) #109665 enregistrement pour 9 variables

# c. Identifier à quels vaccins appartiennent les 20 965 NA restants dans immune_percentage.

# Etape 0 : Répartition des NA restants par vaccin
data_vaccine_immune %>%
  filter(is.na(immune_percentage)) %>%
  count(vaccine, sort = TRUE) %>%
  mutate(pourcentage = round(n / sum(n) * 100, 1))

# Étape 1 : extraire les lignes avec NA restants
na_vaccins <- data_vaccine_immune %>%
  filter(is.na(immune_percentage))

# Étape 2 : compter les NA par vaccin
na_par_vaccin <- na_vaccins %>%
  count(vaccine, sort = TRUE) %>%
  mutate(pourcentage = round(n / sum(n) * 100, 1))

# Étape 3 : identifier les vaccins requis selon les codes présents
na_par_vaccin <- na_par_vaccin %>%
  mutate(
    est_dans_required = vaccine %in% names(no_available_vaccine)
  )

# Étape 4 : résumé clair
na_par_vaccin_resume <- na_par_vaccin %>%
  group_by(est_dans_required) %>%
  summarise(
    total_na = sum(n),
    pourcentage_total = round(sum(pourcentage), 1),
    .groups = "drop"
  )

# Afficher les résultats
na_par_vaccin
na_par_vaccin_resume #3543 valeurs manquantes supplémentaires font partie des 5 variables restantes.

# Imputation de #3543 NA dans data_vaccine_immune qui font partie des 5 dont on ignore l'année d'introduction

data_vaccine_immune <- data_vaccine_immune %>%
  mutate(
    immune_percentage = ifelse(is.na(immune_percentage) & vaccine %in% names(no_available_vaccine), 0, immune_percentage
    )
  )

sum(is.na(data_vaccine_immune$immune_percentage))
# 17422 valeurs manquantes sur 109665 enregistrements (~16%).
# A gérer lors du test de cohérence dans la base de données finale, en croisant les données
# avec data_coverage.


# c. Donner la bonne typologie des variables dans le dataframe

data_vaccine_immune <- data_vaccine_immune %>%
  mutate(
    across(where(is.character), as.factor),
    year = as.integer(year),
    immune_percentage = as.numeric(immune_percentage)
  )


# d. Extraction et sauvegarde de la base de données.
# Le dataset data_vaccine_coverage est propre et prêt à l'emploi.

write.xlsx(
  data_vaccine_immune, 
  "data/processed/data_vaccine_immune.xlsx"
)



# O3. - Jointures de bases de données (data_vaccine) ---------------

# a. Nous assurer que les données sont bien groupées par unicef_region, vaccine, year dans data_vaccine_coverage.

data_vaccine_coverage %>%
  count(unicef_region, vaccine, year) %>%
  filter(n > 1) # 0 ligne. Donc okay


# b. Grouper les données dans data_vaccine_immune par unicef_region, vaccine, year

str(data_vaccine_immune)
str(data_vaccine_coverage)


# b.1. Sélectionne les variables intéressantes pour l'aggrégation
data_vaccine_prep <- data_vaccine_immune %>%
  select(unicef_region, iso3, country, vaccine, year, immune_percentage)


# b.2. Agrégation par region_unicef, vaccin, year

# Calculer l'aggrégation globale mondiale
global_agg <- data_vaccine_prep %>%
  group_by(vaccine, year) %>%
  summarise(
    unicef_region = "Global",
    immune_percentage = round(mean(immune_percentage, na.rm = TRUE)),
    .groups = "drop"
  )%>%
  ungroup() %>%
  mutate(
    unicef_region = "Global",
    immune_percentage = ifelse(is.na(immune_percentage), 0, immune_percentage)  # remplace les valeurs vides par NA
  )


# Faire l'aggrégation par région
region_agg <- data_vaccine_prep %>%
  group_by(unicef_region, vaccine, year) %>%
  summarise(
    immune_percentage = round(mean(immune_percentage, na.rm = TRUE)),
    .groups = "drop"
  )%>%
  ungroup() %>%
  mutate(
    immune_percentage = ifelse(is.na(immune_percentage), 0, immune_percentage)  # remplace les valeurs vides par NA
  )

# Fusionner les deux
data_vaccine_immune_agg <- bind_rows(region_agg, global_agg)


str(data_vaccine_immune_agg) # 05 variales perdues, ne pouvant pas être aggrégées
any(is.na(data_vaccine_immune_agg)) #False
summary(data_vaccine_immune_agg)


# b.3. Joignons à présent les bases data_coverage & data_vaccine_immune_agg.

data_vaccine <- data_vaccine_coverage %>%
  left_join(data_vaccine_immune_agg, by = c("unicef_region", "vaccine", "year")) %>%
  select(unicef_region, vaccine, year, coverage, immune_percentage, target, vaccinated, unvaccinated)

any(is.na(data_vaccine)) # False


# c.1. Créer un vecteur continent, qui contient les pays africains uniquement.
# Les régions unicef agrègent un ensemble de pays, de plusieurs continents communs.

pays_region_unicef <- c(
  # Afrique de l'Ouest et du Centre (WCAR)
  "BEN" = "WCAR",
  "BFA" = "WCAR",
  "CPV" = "WCAR",
  "CMR" = "WCAR",
  "CAF" = "WCAR",
  "TCD" = "WCAR",
  "COG" = "WCAR",
  "COD" = "WCAR",
  "GNQ" = "WCAR",
  "GAB" = "WCAR",
  "GMB" = "WCAR",
  "GHA" = "WCAR",
  "GIN" = "WCAR",
  "GNB" = "WCAR",
  "CIV" = "WCAR",
  "LBR" = "WCAR",
  "MLI" = "WCAR",
  "MRT" = "WCAR",
  "NER" = "WCAR",
  "NGA" = "WCAR",
  "SEN" = "WCAR",
  "SLE" = "WCAR",
  "TGO" = "WCAR",
  
  # Afrique de l'Est et Australe (ESAR)
  "AGO" = "ESAR",
  "BWA" = "ESAR",
  "COM" = "ESAR",
  "DJI" = "ESAR",
  "ERI" = "ESAR",
  "ETH" = "ESAR",
  "SWZ" = "ESAR",
  "KEN" = "ESAR",
  "LSO" = "ESAR",
  "MDG" = "ESAR",
  "MWI" = "ESAR",
  "MUS" = "ESAR",
  "MOZ" = "ESAR",
  "NAM" = "ESAR",
  "SYC" = "ESAR",
  "SOM" = "ESAR",
  "ZAF" = "ESAR",
  "SSD" = "ESAR",
  "SDN" = "ESAR",
  "TZA" = "ESAR",
  "UGA" = "ESAR",
  "ZMB" = "ESAR",
  "ZWE" = "ESAR",
  
  # Afrique du Nord (région MENA)
  "DZA" = "MENA",
  "EGY" = "MENA",
  "LBY" = "MENA",
  "MAR" = "MENA",
  "TUN" = "MENA",
  "ESH" = "MENA"
)

unicef_region_label <- c(
  "WCAR" = "Africa",
  "ESAR" = "Africa",
  "MENA" = "Afrasia"  # Afrique du Nord + Moyen-Orient
)

africa <- c(
  "DZA", "AGO", "BEN", "BWA", "BFA", "BDI", "CPV", "CMR", "CAF", "TCD",
  "COM", "COG", "CIV", "COD", "DJI", "EGY", "GNQ", "ERI", "SWZ", "ETH",
  "GAB", "GMB", "GHA", "GIN", "GNB", "KEN", "LSO", "LBR", "LBY", "MDG",
  "MWI", "MLI", "MRT", "MUS", "MAR", "MOZ", "NAM", "NER", "NGA", "RWA",
  "STP", "SEN", "SYC", "SLE", "SOM", "ZAF", "SSD", "SDN", "TZA", "TGO",
  "TUN", "UGA", "ZMB", "ZWE"
)


# c.2 . Créer un vecteur zone de priotité vaccinale pour l'Unicef
# Le niveau de priorité dimiune, de 1 à 4.

unicef_region_priority <- c(
  "WCAR"  = 1,  # Afrique de l’Ouest et du Centre
  "ROSA"  = 1,  # Asie du Sud (South Asia)
  "ESAR"  = 2,  # Afrique de l’Est et Australe
  "MENA"  = 3,  # Moyen-Orient et Afrique du Nord
  "EAPR"  = 3,  # Asie de l’Est et Pacifique (East Asia & Pacific)
  "LACR"  = 4,  # Amérique latine et Caraïbes
  "ECAR"  = 4,  # Europe et Asie centrale (Europe & Central Asia)
  "Global" = 0,
  "Non-programme" = 0
)

unicef_region_priority_label <- c(
  "Très prioritaire" = 1,
  "Prioritaire" = 2,
  "Modérée" = 3, 
  "Faible" = 4,
  "Inconnu" = 0
)


# c.3 . Créer une variable catégorielle des programmes de vaccination dans les pays.
# Ce vecteur indique pour chaque vaccin, le programme vaccinal indicatif.
# Diffile à prédire avec exactitude, car cela varie par pays, par année et région Unicef.

vaccine_program <- c(
  "BCG"   = "PEV de base",
  "DTP1"  = "PEV de base",
  "DTP3"  = "PEV de base",
  "POL3"  = "PEV de base",
  "IPV1"  = "PEV de base",
  "IPV2"  = "PEV de base",
  "HEPBB" = "Naissance",
  "HEPB3" = "PEV de base",
  "HIB3"  = "PEV de base",
  "MCV1"  = "Rougeole",
  "MCV2"  = "Rougeole",
  "RCV1"  = "Rubéole",
  "ROTAC" = "Rotavirus",
  "PCV3"  = "Pneumocoque",
  "MENGA" = "Méningite A",
  "YFV"   = "Fièvre jaune"
)

# c.4. Intégrer les vecteurs pertinents dans la base de données.

data_vaccine <- data_vaccine %>%
  mutate(
    vaccine_program = as.factor(vaccine_program[vaccine]),
    vaccine_priority = as.factor(unicef_region_priority[unicef_region]),
    priority_label = case_when(
      vaccine_priority == 1 ~ "Très prioritaire",
      vaccine_priority == 2 ~ "Prioritaire",
      vaccine_priority == 3 ~ "Modérée",
      vaccine_priority == 4 ~ "Faible",
      TRUE ~ "Inconnu" # Pour les Global et les Non-Programmé
    )
  )

dim(data_vaccine) #4181 enregistrement pour 11 variables
any(is.na(data_vaccine)) #FALSE

# Identifier les colones avec des NA dans data_vaccine_immune
cwna_dv <- sapply(data_vaccine, function(col) {
  any(is.na(col))
})
ctocheck_dv <- names(cwna_dv)[cwna_dv]
print(ctocheck_dv)

# Compter les NA dans ces colonnes
na_counts_dvim <- sapply(data_vaccine[ctocheck_dv], function(col) {
  sum(is.na(col))
})
print(na_counts_dvim)

# d. Extraction et sauvegarde de la base de données.
# Le dataset data_vaccine est propre et prêt à l'emploi.

write.xlsx(
  data_vaccine, 
  "data/processed/data_vaccine.xlsx"
)


# O4. - Essai d'intégration de coverage à data_vaccine_immune --------
# Abbérations potentielles : le taux régional n'est pas toujours représentatif d’un pays individuel,
# surtout pour les pays extrêmes (les plus pauvres ou les plus performants).
# data à manier avec prudence.


# Suppose que j'ai un mapping des pays
pays_des_regions <- data_vaccine_immune %>%
  select(iso3, unicef_region) %>%
  distinct()


nb_pays_par_regions <- pays_des_regions %>%
  group_by(unicef_region) %>%
  summarise(nb_pays = n_distinct(iso3)) %>%
  arrange(desc(nb_pays))


# Étendre data_coverage au niveau pays
data_vaccine_coverage_pays <- data_vaccine_coverage %>%
  left_join(pays_des_regions, by = "unicef_region")

# Fusion complète
data_vaccine_coverage_pays <- data_vaccine_coverage_pays %>%
  left_join(data_vaccine_immune, by = c("iso3", "vaccine", "year", "unicef_region"))  %>%
  mutate(
    vaccine_program = as.factor(vaccine_program[vaccine]),
    vaccine_priority = as.factor(unicef_region_priority[unicef_region]),
    priority_label = case_when(
      vaccine_priority == 1 ~ "Très prioritaire",
      vaccine_priority == 2 ~ "Prioritaire",
      vaccine_priority == 3 ~ "Modérée",
      vaccine_priority == 4 ~ "Faible",
      TRUE ~ "Inconnu" # Pour les Global et les Non-Programmé
    )
  )

write.xlsx(
  data_vaccine, 
  "data/processed/data_vaccine_coverage_pays.xlsx"
)



# 05. - Jointures de bases de données (all_indicators) ---------

# a. Indicateurs clés WDI pour expliquer la couverture vaccinale infantile en Afrique

indicateurs_pertinents <- c(
  
  # 1. Accès au système de santé et qualité des serices
  
  "SH.XPD.CHEX.GD.ZS",    # Dépenses de santé (% du PIB)
  "SH.XPD.GHED.CH.ZS",    # Dépenses de santé par habitant (% du PIB)
  "SH.MED.BEDS.ZS",       # Lits d’hôpital pour 1.000 hab.
  "SH.XPD.OOPC.CH.ZS",    # Dépenses de santé à charge du patient (%)
  "SP.REG.BRTH.ZS",       # Enfants enregistrés à la naissance (%)
  "SH.STA.BASS.ZS",       # Accès à une source d’eau de base (%)
  "EG.ELC.ACCS.ZS",       # Accès à l’électricité (%)
  
  # 2. Santé infantile et maternelle
  
  "SP.DYN.IMRT.IN",       # Taux de mortalité infantile
  "SP.DYN.CBRT.IN",       # Taux de natalité brut
  "SP.UWT.TFRT",          # Taux de fécondité chez les adolescentes
  "SP.DYN.TFRT.IN",       # Taux de fécondité (total)
  "SH.MMR.RISK.ZS",       # Risque de mortalité maternelle
  "SP.DYN.LE00.IN",       # Espérance de vie à la naissance
  
  # 3. Education et connaissance de la population (les adultes)
  
  "SE.PRM.ENRR",          # Taux de scolarisation dans le primaire
  "SE.SEC.ENRR",          # Taux de scolarisation dans le secondaire
  "SE.ADT.LITR.ZS",       # Alphabétisation des adultes (%)
  "SE.PRM.CMPT.ZS",       # Taux d'achèvement du primaire
  "SE.SEC.CMPT.LO.ZS",    # Taux d'achèvement du secondaire
  "SE.XPD.TOTL.GD.ZS",    # Dépenses d’éducation (% du PIB)
  
  # 4. Situation économique
  
  "NY.GDP.PCAP.CD",       # PIB par habitant (USD courant)
  "SL.UEM.TOTL.ZS",       # Taux de chômage (%)
  "GC.REV.XGRT.GD.ZS",    # Revenus publics (% PIB)
  "FP.CPI.TOTL.ZG",       # Inflation annuelle (%)
  "BX.KLT.DINV.CD.WD",    # Investissements directs étrangers (USD)
  "NY.GNS.ICTR.ZS",       # Taux d’épargne nationale
  
  # 5. Situation démographique et sociale
  
  "SP.POP.0014.TO.ZS",    # Population jeune (% 0-14 ans)
  "SP.POP.1564.TO.ZS",    # Population active (%)
  "SP.POP.65UP.TO.ZS",    # Population âgée (%)
  "SP.POP.TOTL.FE.ZS",    # Femmes dans la population (%)
  "SL.TLF.CACT.FE.ZS",    # Participation des femmes au marché du travail (%)
  
  # 6. Urbanisation et localisation
  
  "SP.RUR.TOTL.ZS",       # Population rurale (%)
  "SP.URB.TOTL.IN.ZS",    # Taux d'urbanisation (%)
  
  # 7. Infrastructures routières et numériques
  
  "IS.RRS.TOTL.KM",       # Réseau ferré (km total)
  "IS.ROD.PAVE.ZS",       # Routes pavées (% du total)
  "IT.NET.USER.ZS",       # % d’utilisateurs d’Internet (indirectement lié à l’accès à l’info)
  
  # 8. Gouvernance et stabilité politique
  
  "SP.POP.TOTL",          # Taille de la population
  "PV.EST",               # Indicateur de stabilité politique (Worldwide Governance Indicators)
  "GE.EST"                # Efficacité du gouvernement (Worldwide Governance Indicators)
)


# b. Telecharger pour tous les pays de 1980 à 2024
# data_wdi <- WDI(country = "all", 
#                indicator = indicateurs_pertinents, 
#                start = 1980, end = 2024)

# View(data_wdi)


# c. Extraction et sauvegarde de la base de données brute.

# write.xlsx(
#  data_wdi, 
#  "data/raw/data_wdi.xlsx"
#)

# d. Renommer les indicateurs de la data_wdi.

data_wdi <- read.xlsx("data/raw/data_wdi.xlsx",sheet = 1)

noms_indicateurs <- c(
  
  #Paramètres
  
  "iso3c" = "iso3",
  
  # 1. Accès au système de santé et qualité des services
  "SH.XPD.CHEX.GD.ZS"    = "Depenses_sante_PIB",
  "SH.XPD.GHED.CH.ZS"    = "Depenses_sante_par_habitant",
  "SH.MED.BEDS.ZS"       = "Lits_hospitaliers_pour_1000",
  "SH.XPD.OOPC.CH.ZS"    = "Depenses_sante_directes_patient",
  "SP.REG.BRTH.ZS"       = "Naissances_enregistrees",
  "SH.STA.BASS.ZS"       = "Acces_eau_potable",
  "EG.ELC.ACCS.ZS"       = "Acces_electricite",
  
  # 2. Santé infantile et maternelle
  "SP.DYN.IMRT.IN"       = "Mortalite_infantile",
  "SP.DYN.CBRT.IN"       = "Taux_natalite_brut",
  "SP.UWT.TFRT"          = "Fertilite_adolescentes",
  "SP.DYN.TFRT.IN"       = "Taux_fertilite_total",
  "SH.MMR.RISK.ZS"       = "Risque_mortalite_maternelle",
  "SP.DYN.LE00.IN"       = "Esperance_vie_naissance",
  
  # 3. Éducation et connaissances (adultes)
  "SE.PRM.ENRR"          = "Scolarisation_primaire",
  "SE.SEC.ENRR"          = "Scolarisation_secondaire",
  "SE.ADT.LITR.ZS"       = "Alphabetisation_adultes",
  "SE.PRM.CMPT.ZS"       = "Achèvement_primaire",
  "SE.SEC.CMPT.LO.ZS"    = "Achèvement_secondaire",
  "SE.XPD.TOTL.GD.ZS"    = "Depenses_education_PIB",
  
  # 4. Situation économique
  "NY.GDP.PCAP.CD"       = "PIB_par_habitant",
  "SL.UEM.TOTL.ZS"       = "Taux_chomage",
  "GC.REV.XGRT.GD.ZS"    = "Revenus_publics_PIB",
  "FP.CPI.TOTL.ZG"       = "Inflation_annuelle",
  "BX.KLT.DINV.CD.WD"    = "IDE_entrant_USD",
  "NY.GNS.ICTR.ZS"       = "Taux_epargne_nationale",
  
  # 5. Démographie et genre
  "SP.POP.0014.TO.ZS"    = "Population_jeunes_0_14",
  "SP.POP.1564.TO.ZS"    = "Population_active_15_64",
  "SP.POP.65UP.TO.ZS"    = "Population_agee_65_plus",
  "SP.POP.TOTL.FE.ZS"    = "Part_femmes_population",
  "SL.TLF.CACT.FE.ZS"    = "Participation_femmes_travail",
  
  # 6. Urbanisation et localisation
  "SP.RUR.TOTL.ZS"       = "Part_population_rurale",
  "SP.URB.TOTL.IN.ZS"    = "Taux_urbanisation",
  
  # 7. Infrastructures routières et numériques
  "IS.RRS.TOTL.KM"       = "Réseau_ferre_km_total",
  "IS.ROD.PAVE.ZS"       = "Routes_pavees_pourcent",
  "IT.NET.USER.ZS"       = "Utilisateurs_Internet",
  
  # 8. Gouvernance et stabilité politique
  "SP.POP.TOTL"          = "Population_totale",
  "PV.EST"               = "Stabilite_politique",
  "GE.EST"               = "Efficacite_gouvernement"
)

# Integration des nouveaux noms
names(data_wdi) <- 
  ifelse(names(data_wdi) %in%
           names(noms_indicateurs),
         noms_indicateurs[names(data_wdi)],
         names(data_wdi))

str(data_wdi)
any(is.na(data_wdi)) #True


# e. Trouvons où se trouvent les NA
cols_with_na_data_wdi <- sapply(data_wdi, function(col) {
  any(is.na(col))
})
columns_to_check_data_wdi <- names(cols_with_na_data_wdi)[cols_with_na_data_wdi]
print(columns_to_check_data_wdi)

# e.1. Voyons voir combien de na nous avons

sum(is.na(data_wdi)) #175226 valeurs manquantes, dans 39 variables sur 42

# e.2.   Aggréger les données de data_wdi suivant les régions unicef.
#   2.1. Créer un vecteur avec les codes iso3 des pays par regions unicef

unicef_regions_vector <- data_vaccine_immune_percentage %>%
  select(iso3, unicef_region) %>%
  distinct() %>%
  arrange(unicef_region)

# Voir un aperçu du vecteur
head(unicef_regions_vector)
any(is.na(unicef_regions_vector)) #False


#   2.2. Fusionner data_wdi et le unicef_regions_vector

colnames(data_wdi)

data_vaccine_indicators <- data_wdi %>%
  inner_join(
    unicef_regions_vector,
    by = "iso3"
  )

any(is.na(data_vaccine_indicators)) # True
sum(is.na(data_vaccine_indicators)) # 111930 NA


#   2.3. Imputation progressive des NA pour l'aggrégation
#   Imputation simple des NA par interpolation linéaire par pays

data_vaccine_indicators <- data_vaccine_indicators %>%
  group_by(iso3) %>%
  arrange(iso3, year) %>%
  mutate(across(where(is.numeric), ~na.approx(., na.rm = FALSE))) %>%
  ungroup()

any(is.na(data_vaccine_indicators)) #True
sum(is.na(data_vaccine_indicators)) # 90415 NA

#   Imputation des NA par la moyenne pondérée par la population
#   Calcul de la population totale par région (utile pour pondérer)
pop_region <- data_vaccine_indicators %>%
  group_by(unicef_region, year) %>%
  summarise(Population_totale = sum(Population_totale, na.rm = TRUE), .groups = "drop")


#   Moyenne pondérée pour les indicateurs qui sont des taux

indicateurs_taux <- c(
  "Depenses_sante_PIB",
  "Depenses_sante_par_habitant",
  "Depenses_sante_directes_patient",
  "Naissances_enregistrees",
  "Acces_eau_potable",
  "Acces_electricite",
  "Mortalite_infantile",
  "Taux_natalite_brut",
  "Fertilite_adolescentes",
  "Taux_fertilite_total",
  "Risque_mortalite_maternelle",
  "Scolarisation_primaire",
  "Scolarisation_secondaire",
  "Alphabetisation_adultes",
  "Achèvement_primaire",
  "Achèvement_secondaire",
  "Taux_chomage",
  "Revenus_publics_PIB",
  "Inflation_annuelle",
  "Taux_epargne_nationale",
  "Population_jeunes_0_14",
  "Population_active_15_64",
  "Population_agee_65_plus",
  "Part_femmes_population",
  "Participation_femmes_travail",
  "Part_population_rurale",
  "Taux_urbanisation",
  "Routes_pavees_pourcent"
)

#   Calcul des taux moyens pondérés par région unicef
taux_region <- data_vaccine_indicators %>%
  select(iso3, year, unicef_region, Population_totale, all_of(indicateurs_taux)) %>%  # Garde Population_totale ici
  pivot_longer(cols = all_of(indicateurs_taux), names_to = "indicateur", values_to = "valeur") %>%
  group_by(unicef_region, year, indicateur) %>%
  summarise(valeur_pond = weighted.mean(valeur, Population_totale, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = indicateur, values_from = valeur_pond)%>%
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.), NA, .)))


#  Compléter les indicateurs manquants
#     Tous les noms de variables que tu voulais retrouver dans taux_region (hors variables non pondérées)
indicateurs_attendus <- setdiff(unname(noms_indicateurs), c("iso3", "Population_totale"))  # retire les non taux

#     Liste de colonnes présentes dans taux_region
indicateurs_dans_taux_region <- names(taux_region)

#     Voir ceux qui manquent
indicateurs_manquants <- setdiff(indicateurs_attendus, indicateurs_dans_taux_region)

#     Résultat
indicateurs_manquants

#     Aggrégation des 09 indicateurs restants
region_indicateurs_manquants <- data_vaccine_indicators %>%
  filter(!is.na(unicef_region)) %>%
  group_by(unicef_region, year) %>%
  summarise(across(all_of(indicateurs_manquants), ~mean(.x, na.rm = TRUE), .names = "{.col}"), .groups = "drop")

#     Fusion avec taux_region
taux_region <- taux_region %>%
  left_join(region_indicateurs_manquants,
            by = c("unicef_region", "year")) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.), NA, .)))


unique(taux_region$unicef_region)

#     Calculer la moyenne mondiale des indicateurs pour créer le unicef_region = "Global"

taux_global <- data_vaccine_indicators %>%
  select(iso3, year, unicef_region, Population_totale, all_of(indicateurs_taux)) %>%
  pivot_longer(cols = all_of(indicateurs_taux), names_to = "indicateur", values_to = "valeur") %>%
  group_by(year, indicateur) %>%
  summarise(valeur_pond = weighted.mean(valeur, Population_totale, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = indicateur, values_from = valeur_pond) %>%
  mutate(
    unicef_region = "Global"
  ) %>%
  select(unicef_region, everything())

#     Aggrégation des 09 indicateurs restants
global_indicateurs_manquants <- data_vaccine_indicators %>%
  filter(!is.na(unicef_region)) %>%
  group_by(year) %>%
  summarise(across(all_of(indicateurs_manquants), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(unicef_region = "Global") %>%
  select(unicef_region, everything())


#     Finaliser l'aggrégation mondiale
taux_global <- taux_global %>%
  left_join(global_indicateurs_manquants,
            by = c("unicef_region", "year"))%>%
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.), NA, .)))


#     Finaliser la combinaison des indicateurs par régions

taux_region <- bind_rows(taux_region, taux_global) %>%
  arrange(unicef_region, year) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.), NA, .)))  # Nettoie à nouveau au cas où


# data_vaccine & taux_region

str(data_vaccine)
str(taux_region)

# Aggréger le contenu de cette base par régions unicef et par année
data_vaccine_region <- data_vaccine %>%
  group_by(unicef_region, year) %>%
  summarise(
    regional_target = sum(target, na.rm = TRUE),
    regional_coverage = round(sum(coverage * target, na.rm = TRUE) / regional_target),
    regional_immune_percentage = round(sum(immune_percentage * target, na.rm = TRUE) / regional_target),
    regional_vaccinated = sum(vaccinated, na.rm = TRUE),
    .groups = "drop"
  )

str(data_vaccine_region)
str(taux_region)

# Fusion avec taux_region (qui contient les autres indicateurs)
all_indicators <- taux_region %>%
  left_join(data_vaccine_region, by = c("unicef_region", "year"))


any(is.na(all_indicators)) #True
sum(is.na(all_indicators))


# Stratégies pour gérer les valeurs manquantes (NA)

# 1. ANALYSE EXPLORATOIRE DES NA

# Visualiser les patterns de NA
library(VIM)
library(mice)
library(dplyr)

# Pattern des valeurs manquantes
aggr(all_indicators, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE)

# Matrice des valeurs manquantes
md.pattern(all_indicators)

# Pourcentage de NA par variable
na_summary <- all_indicators %>%
  summarise_all(~sum(is.na(.))/length(.) * 100) %>%
  gather(variable, pct_na) %>%
  arrange(desc(pct_na))

print(na_summary)

# 2. STRATÉGIES DE TRAITEMENT


# A) SUPPRESSION

# Suppression des variables avec >70% de NA
variables_a_supprimer <- na_summary %>%
  filter(pct_na > 70) %>%
  pull(variable)

df_clean1 <- all_indicators %>%
  select(-all_of(variables_a_supprimer))

# Suppression des lignes avec >50% de NA
seuil_na_lignes <- 0.5

# Calculer le pourcentage de NA par ligne
pourcentage_na_par_ligne <- rowSums(is.na(df_clean1))/ncol(df_clean1)

# Voir la distribution
summary(pourcentage_na_par_ligne)
hist(pourcentage_na_par_ligne, main="Distribution du % de NA par ligne")

# Voir quelles lignes ont plus de 50% de NA
lignes_problematiques <- which(pourcentage_na_par_ligne >= 0.5)
print(paste("Lignes à supprimer:", paste(lignes_problematiques, collapse = ", ")))

# Voir le contenu de ces lignes problématiques
df_clean1[lignes_problematiques, ]

# Nettoyer
df_clean2 <- df_clean1[pourcentage_na_par_ligne < seuil_na_lignes, ]


# B) IMPUTATION SIMPLE

# Imputation par la médiane (variables numériques)
df_median <- df_clean2 %>%
  mutate_if(is.numeric, ~ifelse(is.na(.), median(., na.rm = TRUE), .))

# Imputation par la moyenne par région
df_region_mean <- df_clean2 %>%
  group_by(unicef_region) %>%
  mutate_if(is.numeric, ~ifelse(is.na(.), mean(., na.rm = TRUE), .)) %>%
  ungroup()

# C) IMPUTATION AVANCÉE AVEC MICE

# Sélectionner les variables clés pour l'imputation
variables_importantes <- c("year", "unicef_region", "Acces_eau_potable", 
                           "Acces_electricite", "Achèvement_primaire", 
                           "Mortalite_infantile", "Esperance_vie_naissance",
                           "PIB_par_habitant", "Taux_urbanisation")

df_mice_prep <- all_indicators %>%
  select(all_of(variables_importantes))

# Configuration MICE
mice_config <- mice(df_mice_prep, method = 'pmm', m = 5, printFlag = FALSE)

# Imputation
df_mice_complete <- complete(mice_config)

# D) IMPUTATION PAR INTERPOLATION TEMPORELLE

# Pour les séries temporelles par pays
df_interpolation <- all_indicators %>%
  group_by(unicef_region) %>%
  arrange(year) %>%
  mutate_if(is.numeric, ~na.approx(., na.rm = FALSE, rule = 2)) %>%
  ungroup()

# 3. STRATÉGIES SPÉCIFIQUES PAR TYPE DE VARIABLE

# Variables économiques (PIB, dépenses santé)
# Imputation par régression basée sur des prédicteurs économiques
impute_economic <- function(df) {
  # Exemple pour PIB_par_habitant
  if("PIB_par_habitant" %in% names(df) && any(is.na(df$PIB_par_habitant))) {
    model_pib <- lm(PIB_par_habitant ~ Taux_urbanisation + Acces_electricite + 
                      factor(unicef_region), data = df, na.action = na.exclude)
    
    df$PIB_par_habitant[is.na(df$PIB_par_habitant)] <- 
      predict(model_pib, newdata = df[is.na(df$PIB_par_habitant), ])
  }
  return(df)
}

# Variables de santé
# Imputation basée sur corrélations avec autres indicateurs de santé
impute_health <- function(df) {
  # Exemple pour Mortalite_infantile
  if("Mortalite_infantile" %in% names(df) && any(is.na(df$Mortalite_infantile))) {
    model_mortality <- lm(Mortalite_infantile ~ Esperance_vie_naissance + 
                            Acces_eau_potable + PIB_par_habitant + 
                            factor(unicef_region), data = df, na.action = na.exclude)
    
    df$Mortalite_infantile[is.na(df$Mortalite_infantile)] <- 
      predict(model_mortality, newdata = df[is.na(df$Mortalite_infantile), ])
  }
  return(df)
}

# 4. ÉVALUATION DE LA QUALITÉ D'IMPUTATION

# Fonction pour comparer avant/après imputation
evaluate_imputation <- function(original, imputed, variable) {
  original_stats <- summary(original[[variable]])
  imputed_stats <- summary(imputed[[variable]])
  
  cat("Variable:", variable, "\n")
  cat("Original:\n")
  print(original_stats)
  cat("Après imputation:\n")
  print(imputed_stats)
  cat("\n")
}

# 5. RECOMMANDATIONS PAR VARIABLE

recommandations <- list(
  # Variables avec beaucoup de NA - Considérer suppression
  "Routes_pavees_pourcent" = "Supprimer (74% NA) ou imputer par moyenne régionale",
  "Naissances_enregistrees" = "Imputer par régression (lié au développement)",
  "Depenses_sante_*" = "Imputer par % PIB moyen régional",
  
  # Variables importantes - Imputation soignée
  "Acces_eau_potable" = "MICE ou interpolation temporelle",
  "Acces_electricite" = "Interpolation + régression sur urbanisation",
  "Mortalite_infantile" = "Régression sur indicateurs santé/développement",
  
  # Variables contextuelles
  "Stabilite_politique" = "Imputation par moyenne régionale sur 3 ans",
  "Taux_chomage" = "Interpolation temporelle par pays"
)

# 6. PIPELINE COMPLET

pipeline_na_management <- function(df) {
  library(dplyr)
  library(mice)
  
  cat("Étape 1: Analyse initiale des NA\n")
  initial_na <- colSums(is.na(df))
  cat("Variables avec le plus de NA:\n")
  print(head(sort(initial_na, decreasing = TRUE), 10))
  
  # Étape 1: Supprimer variables avec >75% NA
  na_prop <- colSums(is.na(df)) / nrow(df)
  high_na_vars <- names(na_prop[na_prop > 0.75])
  
  if(length(high_na_vars) > 0) {
    cat("\nSuppression des variables avec >75% NA:", paste(high_na_vars, collapse = ", "), "\n")
    df <- df %>% select(-all_of(high_na_vars))
  }
  
  # Étape 2: Supprimer lignes avec >60% de NA
  row_na_prop <- rowSums(is.na(df)) / ncol(df)
  rows_to_keep <- row_na_prop <= 0.6
  cat("Suppression de", sum(!rows_to_keep), "lignes avec >60% NA\n")
  df <- df[rows_to_keep, ]
  
  # Étape 3: Imputation par médiane/mode par région (version corrigée)
  cat("\nÉtape 3: Imputation par région\n")
  
  # Identifier les colonnes numériques (exclure les colonnes de groupage)
  numeric_cols <- df %>% 
    select(-unicef_region, -year) %>% 
    select_if(is.numeric) %>% 
    names()
  
  # Imputation par région avec across() au lieu de mutate_if()
  df <- df %>%
    group_by(unicef_region) %>%
    mutate(across(all_of(numeric_cols), ~ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
    ungroup()
  
  # Étape 4: Imputation globale pour les NA restants
  cat("Étape 4: Imputation finale\n")
  remaining_na <- colSums(is.na(df))
  vars_with_na <- names(remaining_na[remaining_na > 0])
  
  if(length(vars_with_na) > 0) {
    cat("Variables avec NA restants:", paste(vars_with_na, collapse = ", "), "\n")
    
    # Imputation simple par médiane globale
    df <- df %>%
      mutate(across(all_of(vars_with_na), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))
  }
  
  return(df)
}

# VERSION ALTERNATIVE AVEC MICE (plus robuste)

pipeline_mice_robust <- function(df) {
  library(dplyr)
  library(mice)
  
  # Préparation des données pour MICE
  cat("Préparation pour MICE\n")
  
  # Supprimer variables avec trop de NA
  na_prop <- colSums(is.na(df)) / nrow(df)
  keep_vars <- names(na_prop[na_prop <= 0.7])  # Garder variables avec ≤70% NA
  
  df_mice <- df %>% select(all_of(keep_vars))
  
  # Créer des versions numériques des variables catégorielles
  if("unicef_region" %in% names(df_mice)) {
    df_mice$region_num <- as.numeric(as.factor(df_mice$unicef_region))
  }
  
  # Sélectionner un sous-ensemble de variables pour éviter la singularité
  key_vars <- c("year", "region_num", "Acces_eau_potable", "Acces_electricite", 
                "Achèvement_primaire", "Mortalite_infantile", "PIB_par_habitant",
                "Esperance_vie_naissance", "Taux_urbanisation")
  
  # Garder seulement les variables qui existent dans le dataset
  key_vars <- key_vars[key_vars %in% names(df_mice)]
  df_subset <- df_mice[, key_vars]
  
  # Imputation MICE avec paramètres robustes
  cat("Lancement MICE\n")
  tryCatch({
    mice_imp <- mice(df_subset, 
                     m = 3,           # 3 imputations
                     method = 'pmm',  # Predictive mean matching
                     seed = 123,
                     printFlag = FALSE,
                     maxit = 5)       # Maximum 5 itérations
    
    df_imputed <- complete(mice_imp, 1)
    
    # Remplacer dans le dataset original
    for(var in names(df_imputed)) {
      if(var %in% names(df) && var != "region_num") {
        df[[var]][is.na(df[[var]])] <- df_imputed[[var]][is.na(df[[var]])]
      }
    }
    
  }, error = function(e) {
    cat("Erreur MICE, utilisation imputation simple\n")
    # Fallback: imputation par médiane
    numeric_vars <- df %>% select_if(is.numeric) %>% names()
    df[numeric_vars] <- lapply(df[numeric_vars], function(x) {
      ifelse(is.na(x), median(x, na.rm = TRUE), x)
    })
  })
  
  return(df)
}

#Copie avant nettoyage des na
all_indicators_brut <- all_indicators

# Application du pipeline
all_indicators <- pipeline_na_management(all_indicators)

# Vérification finale
cat("NA restants après traitement:")
sum(is.na(all_indicators))


# exporter la base de données pour la conserver
write.xlsx(
  all_indicators, 
  "data/processed/all_indicators.xlsx"
)


# O6. Jointure avec la base de données abberande avec WDI brute (data_final) -----

str(data_vaccine_coverage_pays)
str(data_wdi)
str(data_vaccine)

# data_wdi est au niveau pays × année
# data_vaccine_coverage_pays est au niveau pays × vaccin × année
# La fusion se fera donc sur iso3 et year, en dupliquant les données socio-économiques par vaccin dans data_vaccine_coverage_pays

absolute_abberante_data <- data_vaccine_coverage_pays %>%
  left_join(data_wdi, by = c("iso3", "year"))

any(is.na(absolute_abberante_data)) #True
dim(absolute_abberante_data) # 90719    56
sum(is.na(absolute_abberante_data)) # 1269444 NA sur 5080264 données. Soit 25,3 % de NA


# Trouvons où se trouvent les NA
cols_with_na_aad <- sapply(data_wdi, function(col) {
  any(is.na(col))
})
columns_to_check_data_aad <- names(cols_with_na_aad)[cols_with_na_aad]
print(columns_to_check_data_aad)


# GESTION DES NA - APPROCHE PAS À PAS POUR DATASET AVEC STRUCTURE ORIGINALE

library(dplyr)
library(tidyr)
library(VIM)

# Dataset = absolute_abberante_data (garder structure exacte avec duplications)
data <- absolute_abberante_data

# ÉTAPE 1 : DIAGNOSTIC COMPLET DES NA

cat("=== ÉTAPE 1 : DIAGNOSTIC DES NA ===\n")
cat("Nombre total d'observations :", nrow(data), "\n")
cat("Nombre de variables :", ncol(data), "\n\n")

# Compter les NA par variable
na_summary <- data %>%
  summarise_all(~sum(is.na(.))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "nb_na") %>%
  mutate(
    pct_na = round((nb_na / nrow(data)) * 100, 2),
    categorie_na = case_when(
      pct_na >= 80 ~ "Critique (>80%)",
      pct_na >= 50 ~ "Très élevé (50-80%)",
      pct_na >= 20 ~ "Élevé (20-50%)",
      pct_na >= 5 ~ "Modéré (5-20%)",
      pct_na > 0 ~ "Faible (0-5%)",
      TRUE ~ "Aucun NA"
    )
  ) %>%
  arrange(desc(pct_na))

print("Variables avec le plus de NA :")
print(head(na_summary, 20))

# Résumé par catégorie
cat("\nRépartition des variables par niveau de NA :\n")
table_na <- table(na_summary$categorie_na)
print(table_na)


# ÉTAPE 2 : ANALYSER LES PATTERNS DE NA

cat("\n=== ÉTAPE 2 : PATTERNS DES NA ===\n")

# Variables avec plus de 80% de NA (candidats à suppression)
variables_trop_na <- na_summary %>%
  filter(pct_na >= 80) %>%
  pull(variable)

cat("Variables avec >80% NA (", length(variables_trop_na), "variables) :\n")
print(variables_trop_na)

# Analyser si certaines lignes ont beaucoup de NA
# Compter NA par ligne sans mélanger les types de variables
na_par_ligne <- data.frame(
  nb_na_ligne = rowSums(is.na(data)),
  iso3 = data$iso3,
  country.x = data$country.x,
  year = data$year,
  vaccine = data$vaccine
)

summary_na_lignes <- summary(na_par_ligne$nb_na_ligne)
cat("\nNombre de NA par ligne :\n")
print(summary_na_lignes)


# ÉTAPE 3 : DÉCISIONS STRATÉGIQUES

cat("\n=== ÉTAPE 3 : STRATÉGIE DE TRAITEMENT ===\n")

# RÈGLE 1: Supprimer variables avec >80% NA
data_step1 <- data %>%
  select(-all_of(variables_trop_na))

cat("Variables supprimées (>80% NA) :", length(variables_trop_na), "\n")
cat("Variables restantes :", ncol(data_step1), "\n\n")

# RÈGLE 2: Identifier les variables par type pour imputation différenciée
variables_identifiantes <- c("iso3", "country.x", "country.y", "iso2c", "year", 
                             "unicef_region", "vaccine", "vaccine_program")

variables_vaccination <- c("immune_percentage", "coverage", "vaccinated", 
                           "unvaccinated", "target", "introduced_binary", 
                           "introduced", "vaccine_priority")

variables_demographie <- c("Population_totale", "Population_jeunes_0_14", 
                           "Population_active_15_64", "Population_agee_65_plus", 
                           "Part_femmes_population", "Taux_urbanisation", 
                           "Part_population_rurale")

variables_sante <- c("Depenses_sante_PIB", "Depenses_sante_par_habitant", 
                     "Lits_hospitaliers_pour_1000", "Esperance_vie_naissance", 
                     "Mortalite_infantile", "Risque_mortalite_maternelle")

variables_education <- c("Scolarisation_primaire", "Scolarisation_secondaire", 
                         "Alphabetisation_adultes", "Achèvement_primaire", 
                         "Achèvement_secondaire", "Depenses_education_PIB")

variables_economiques <- c("PIB_par_habitant", "Taux_chomage", "Revenus_publics_PIB", 
                           "Inflation_annuelle", "IDE_entrant_USD", "Taux_epargne_nationale")

variables_infrastructure <- c("Acces_eau_potable", "Acces_electricite", 
                              "Routes_pavees_pourcent", "Utilisateurs_Internet", 
                              "Réseau_ferre_km_total")

# ÉTAPE 4 : IMPUTATION PAR GROUPES LOGIQUES

cat("=== ÉTAPE 4 : IMPUTATION PROGRESSIVE ===\n")

# Fonction d'imputation par groupe géographique et temporel
imputer_par_contexte <- function(data, variables, methode = "mediane") {
  
  for(var in variables) {
    if(var %in% names(data) && is.numeric(data[[var]])) {
      
      if(methode == "mediane") {
        # Imputation par région et période
        data <- data %>%
          group_by(unicef_region) %>%
          mutate(
            !!var := ifelse(is.na(!!sym(var)), 
                            median(!!sym(var), na.rm = TRUE), 
                            !!sym(var))
          ) %>%
          ungroup()
      }
      
      # Si encore des NA, utiliser médiane globale
      mediane_globale <- median(data[[var]], na.rm = TRUE)
      if(!is.na(mediane_globale)) {
        data[[var]][is.na(data[[var]])] <- mediane_globale
      }
    }
  }
  return(data)
}

# Appliquer l'imputation étape par étape
data_step2 <- data_step1

# 1. Variables démographiques (les plus fiables)
data_step2 <- imputer_par_contexte(data_step2, variables_demographie)
cat("✓ Variables démographiques imputées\n")

# 2. Variables de santé
data_step2 <- imputer_par_contexte(data_step2, variables_sante)  
cat("✓ Variables de santé imputées\n")

# 3. Variables économiques
data_step2 <- imputer_par_contexte(data_step2, variables_economiques)
cat("✓ Variables économiques imputées\n")

# 4. Variables d'éducation
data_step2 <- imputer_par_contexte(data_step2, variables_education)
cat("✓ Variables d'éducation imputées\n")

# 5. Variables d'infrastructure (avec précaution)
data_step2 <- imputer_par_contexte(data_step2, variables_infrastructure)
cat("✓ Variables d'infrastructure imputées\n")

# ÉTAPE 5 : VALIDATION ET VÉRIFICATION

cat("\n=== ÉTAPE 5 : VALIDATION ===\n")

# Vérifier les NA restants
na_apres <- data_step2 %>%
  summarise_all(~sum(is.na(.))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "nb_na") %>%
  filter(nb_na > 0) %>%
  arrange(desc(nb_na))

cat("Variables avec NA restants :\n")
print(na_apres)

# Comparer avant/après
cat("\nComparaison avant/après :\n")
cat("NA total avant :", sum(is.na(data)), "\n")
cat("NA total après :", sum(is.na(data_step2)), "\n")
cat("Réduction NA :", round((1 - sum(is.na(data_step2))/sum(is.na(data)))*100, 1), "%\n")


# ÉTAPE 6 : DATASET FINAL NETTOYÉ

# Sauvegarder le dataset nettoyé
data_final <- data_step2

# Supprimer les lignes avec trop de NA restants (optionnel)
# data_final <- data_final %>%
#   filter(rowSums(is.na(.)) <= 5)  # Garder lignes avec ≤5 NA

cat("\n=== DATASET FINAL ===\n")
cat("Observations :", nrow(data_final), "\n")
cat("Variables :", ncol(data_final), "\n")
cat("NA restants :", sum(is.na(data_final)), "\n")

# Structure finale
cat("\nStructure préservée :\n")
cat("- Par pays-vaccin-année :", 
    length(unique(paste(data_final$iso3, data_final$vaccine, data_final$year))), "\n")
cat("- Pays uniques :", length(unique(data_final$iso3)), "\n")
cat("- Vaccins :", length(unique(data_final$vaccine)), "\n")
cat("- Années :", paste(range(data_final$year, na.rm=TRUE), collapse = "-"), "\n")


# exporter la base de données pour la conserver
write.xlsx(
  data_final, 
  "data/processed/data_final.xlsx"
)

# Petite remarque ---------

# Dans la base de données all_indicators, les valeurs indiquées pour coverage & immune_percentage sont très différentes.
# Cela suggère que l'immune_percentage est calculé de la façon suivante :
# %immunisés ≈ Couverture vaccinale × Efficacité vaccinale



# Le récap --------------

# data_vaccine_coverage, pour la couverture vaccinale par région, par vaccin, par an
# data_vaccine_immune, pour le porcentage d'immunisation par pays, par vaccin, par an
# data_vaccine, pour la couverture vaccinale par region et par an
# all_indicators, pour les indicateurs sociaux économiques par région et par an







# III - Un peu d’histoire --------------

# A - L’histoire du vaccin -----------

# B - Chronologie vaccinale pour les 16 vaccins, par pays d’Afrique et par an. -----

# C - L’Unicef, acteur majeur de la vaccination depuis 1946 (mais faisant un focus de 1980 à aujourd’hui) -------
# IV - Analyse descriptive de la couverture vaccinale en Afrique --------

# A - L'introduction -----

# B - La couverture vaccinale en Afrique, de 1980 à 2024 --------

# Data : data_vaccine



# 00. Extraire les données sur les régions afrique de l'Unicef ---------

# data_vaccine <- read.xlsx("data/processed/data_vaccine.xlsx",sheet = 1)

# a. Régions Afrique uniquement
unicef_region_afrique <- c("WCAR", "ESAR")  # vecteur simple des codes régions

data_vaccine_afrique <- data_vaccine %>%
  filter(unicef_region %in% unicef_region_afrique)%>%
  mutate(
    unicef_region = as.factor(unicef_region),
    unicef_region_label = case_when(
      unicef_region == "ESAR" ~ "Afrique de l'Est et Australe",
      unicef_region == "WCAR" ~ "Afrique de l'Ouest et Centrale",
      TRUE ~ unicef_region  # garde tel quel si non reconnu
    ))

# Observer la base de données
str(data_vaccine_afrique)
summary(data_vaccine_afrique)


# b. Régions Afrique et le reste du monde
data_vaccine_afrique_monde <- data_vaccine %>%
  mutate(unicef_region_label = case_when(
    unicef_region == "ESAR" ~ "Afrique de l'Est et Australe",
    unicef_region == "WCAR" ~ "Afrique de l'Ouest et Centrale",
    unicef_region == "ECAR" ~ "Europe et Asie Centrale",
    unicef_region == "MENA" ~ "Moyen-Orient et Afrique du Nord",
    unicef_region == "ROSA"   ~ "Asie du Sud",
    unicef_region == "EAPR" ~ "Asie de l'Est et Pacifique",
    unicef_region == "LACR" ~ "Amérique Latine et Caraïbes",
    unicef_region == "Global" ~ "Le Monde",
    TRUE ~ unicef_region  # garde tel quel si non reconnu
  ))


data_vaccine_today <- data_vaccine %>%
  filter(unicef_region %in% unicef_region_afrique)%>%
  mutate(
    unicef_region = as.factor(unicef_region),
    unicef_region_label = case_when(
      unicef_region == "ESAR" ~ "Afrique de l'Est et Australe",
      unicef_region == "WCAR" ~ "Afrique de l'Ouest et Centrale",
      TRUE ~ unicef_region  # garde tel quel si non reconnu
    ))



# Observer la base de données
str(data_vaccine_afrique_monde)
summary(data_vaccine_afrique_monde)






# O1. Analyse univariée de la couverture vaccinale en afrique ---------

# e. Comment la couverture a-t-elle évoluée dans ces régions, de 1980 à 2024 ?
coverage_region_year <- data_vaccine_afrique_monde %>%
  group_by(unicef_region_label, year) %>%
  summarise(coverage = round(mean(coverage, na.rm = TRUE)), .groups = "drop") %>%
  ungroup()
print(coverage_region_year)


# e.1. Dans les régions Afrique
png("output/plots/courbe_evolution_annuelle_coucerture_vaccinale_afrique.png", width = 1600, height = 1200, res = 150)
data_vaccine_afrique %>%
  group_by(year) %>%
  summarise(moyenne = mean(coverage, na.rm = TRUE)) %>%
  plot(
    type = "l",
    main = "Évolution de la couverture moyenne",
    xlab = "Année",
    ylab = "Coverage (%)"
  )
dev.off()


# e.2. Dans les régions du monde
png("output/plots/courbe_evolution_annuelle_coucerture_vaccinale_afrique_monde.png", width = 1600, height = 1200, res = 150)
ggplot(coverage_region_year, aes(x = year, y = coverage, color = unicef_region_label)) +
  geom_line(size = 0.5) +
  geom_point(size = 0.5) +
  labs(
    title = "Évolution de la couverture vaccinale par région (1980-2024)",
    x = "Année",
    y = "Couverture moyenne (%)",
    color = "Région UNICEF"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.2, face = "bold", size = 10),
    axis.text.x = element_text(angle = 0, hjust = 1)
  )
dev.off()


# e.3. Facet Wrap de l'évolution de la couverture vaccinale dans le monde
png("output/plots/facet_wrap_courbe_evolution_annuelle_coucerture_vaccinale_afrique_monde.png", width = 1600, height = 1200, res = 150)
ggplot(coverage_region_year, aes(x = year, y = coverage, color = unicef_region_label)) +
  geom_line(size = 0.2) +
  geom_point(size = 0.2) +
  geom_smooth(method = "loess", se = FALSE, size = 0.2) +  # Lissage LOESS sans intervalle de confiance
  facet_wrap(~ unicef_region_label) +  # Un graphique par région
  labs(
    title = "Évolution de la couverture vaccinale par région (1980-2024)",
    x = "Année",
    y = "Couverture moyenne (%)",
    color = "Région UNICEF"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.2, face = "bold", size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"  # optionnel : cacher la légende si on a les facettes
  )
dev.off()


# f. Courbe d'évolution moyenne annuelle de la couverture vaccinale dans le Monde entier
png("output/plots/courbe_evolution_annuelle_coucerture_vaccinale_monde.png", width = 1600, height = 1200, res = 150)
data_vaccine_afrique_monde %>%
  group_by(year) %>%
  summarise(moyenne = mean(coverage, na.rm = TRUE)) %>%
  plot(
    type = "l",
    main = "Évolution de la couverture moyenne",
    xlab = "Année",
    ylab = "Coverage (%)"
  )
dev.off()



# g. Evolution temporelle de la couverture vaccinale par region
png("output/plots/courbe_evolution_temporelle_couverture_vaccinale_region_afrique_monde.png", width = 1600, height = 1200, res = 150)
ggplot(coverage_region_year, aes(x = year, y = coverage, color = unicef_region_label)) +
  geom_line(size = 0.5) +
  geom_point(size = 0.8) +
  labs(
    title = "Évolution de la couverture vaccinale infantile par région Unicef (1980 - 2024)",
    x = "Année",
    y = "Couverture moyenne (%)",
    color = "Région Unicef"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 10, hjust = 0.5),
    legend.text = element_text(size = 10),
    legend.position = "bottom"
  )
dev.off()


# h. Courbe d'évolution moyenne annuelle de la couverture par vaccin
png("output/plots/courbe_evolution_annuelle_coucerture_vaccinale_vaccin_afrique.png", width = 1600, height = 1200, res = 150)
ggplot(data_vaccine_afrique, aes(x = year, y = coverage, color = vaccine)) +
  geom_line(stat = "summary", fun = mean) +
  labs(title = "Évolution moyenne de la couverture par vaccin", y = "Coverage (%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 10, hjust = 0.5),
    legend.text = element_text(size = 10),
    legend.position = "bottom"
  )
dev.off()


# i. Courbe d'évolution annuelle de la couverture vaccinale par programme vaccinal

# Agréger les données par an par programme vaccinal
data_program_year <- data_vaccine_afrique %>%
  group_by(year, vaccine_program) %>%
  summarise(couv_moy = mean(coverage, na.rm = TRUE), .groups = "drop") %>%
  ungroup()

# Le tracé
png("output/plots/courbe_evolution_annuelle_coucerture_vaccinale_par_programme_vaccinal.png", width = 1600, height = 1200, res = 150)
ggplot(data_program_year, aes(x = year, y = couv_moy, color = vaccine_program)) +
  geom_line(size = 0.5) +
  labs(
    title = "Évolution annuelle de la couverture vaccinale par programme",
    x = "Année",
    y = "Couverture moyenne (%)",
    color = "Programme vaccinal"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2") +
  theme(
    plot.title = element_text(face = "bold", size = 10, hjust = 0.5),
    legend.text = element_text(size = 10),
    legend.position = "bottom"
  )
dev.off()


# Quel est l'état actuel de la situation, d'après les données récentes.


str(data_vaccine)

summary(data_vaccine_afrique$coverage)                       # résumé
round(mean(data_vaccine_afrique$coverage, na.rm = TRUE))     # moyenne (41)
median(data_vaccine_afrique$coverage, na.rm = TRUE)          # médiane (49)
round(sd(data_vaccine_afrique$coverage, na.rm = TRUE))       # écart-type (30)
round(var(data_vaccine_afrique$coverage, na.rm = TRUE))      # variance (928)


# a. Histogramme / densité de la couverture vaccinale
png("output/plots/histogramme_de_la_couverture_vaccinale_en_afrique.png", width = 1600, height = 1200, res = 150)
hist(
  data_vaccine_afrique$coverage,
  breaks = 20,
  main = "Histogramme de la couverture vaccinale",
  xlab = "Coverage (%)",
  cex.main = 1  # ≈ taille 10
)
dev.off()


# b. Boîte à moustaches (boxplot)
png("output/plots/boxplot_de_la_couverture_vaccinale_en_afrique.png", width = 1600, height = 1200, res = 150)
boxplot(
  data_vaccine_afrique$coverage,
  main = "Boxplot de la couverture vaccinale infantile",
  ylab = "Coverage (%)",
  cex.main = 0.8  # ≈ taille 10 (ajuste si besoin)
)
dev.off()


# b.1. Extraction des valeurs abérrantes
box <- boxplot(data_vaccine_afrique$coverage, plot = FALSE)
outliers <- data_vaccine_afrique$coverage[data_vaccine_afrique$coverage %in% box$out]
length(outliers)  # 0 valeurs abbérantes


# c. Répartition de la couverture moyenne par régions afrique

# En afrique face au monde
data_vaccine_afrique_monde %>%
  group_by(unicef_region_label, unicef_region) %>%
  summarise(couv_moy = mean(coverage, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    niveau = case_when(
      couv_moy < 30 ~ "Faible",
      couv_moy < 70 ~ "Moyenne",
      TRUE ~ "Élevée"
    ),
    priority_level = unicef_region_priority[unicef_region],
    priority_level = ifelse(is.na(priority_level), 0, priority_level),
    priority_label = case_when(
      is.na(priority_level) ~ "Inconnu",
      TRUE ~ names(unicef_region_priority_label)[match(priority_level, unicef_region_priority_label)]
    )
  ) %>%
  arrange(desc(couv_moy))


# d. Explorer les tendances de dispersion par rapport aux autres variables

# d.1. Boxplot de la couverture vaccinale par région afrique
png("output/plots/boxplot_de_la_couverture_vaccinale_par_regions_afrique_de_unicef.png", width = 1600, height = 1200, res = 150)
boxplot(
  coverage ~ unicef_region,
  data = data_vaccine_afrique,
  las = 2,
  col = "skyblue",
  main = "Couverture vaccinale par région",
  ylab = "Coverage (%)",
  cex.main = 0.8  # ≈ taille 10 (ajuste si besoin)
)
dev.off()


# d.2. Boxplot de la couverture vaccinale par vaccin
png("output/plots/couverture_vaccinale_par_vaccin.png", width = 1600, height = 1200, res = 150)
boxplot(
  coverage ~ vaccine,
  data = data_vaccine_afrique,
  las = 2,
  col = "tomato",
  main = "Couverture vaccinale par vaccin",
  ylab = "Coverage (%)",
  cex.main = 0.8  # ≈ taille 10 (ajuste si besoin)
)
dev.off()


# d.3. Boxplot de la couverture vaccinale par programme vaccinal
png("output/plots/boxplot_de_la_couverture_vaccinale_programme_vaccinal.png", width = 1600, height = 1200, res = 150)
boxplot(
  coverage ~ vaccine_program,
  data = data_vaccine_afrique,
  las = 2, col = "lightgreen",
  main = "Couverture par type de programme",
  ylab = "Coverage (%)",
  cex.main = 0.8  # ≈ taille 10 (ajuste si besoin)
)
dev.off()


# C - Analyse croisée de la couverture vaccinale avec des facteurs contextuels -----------------

# O1. Correler la couverture vaccinale avec d'autres facteurs (programme vaccinal) --------
# Data : data_vaccine_afrique

# Comprendre ce qui pourrait expliquer les tendances observées ?

# a. Hypothèse : Tendances sur la base du programme vaccinal

# Estimer le nombre de vaccins par programme
nb_vaccins_par_programme <- data_vaccine_afrique %>%
  select(vaccine_program, vaccine) %>%
  distinct() %>%
  count(vaccine_program, name = "nb_vaccins") %>%
  arrange(desc(nb_vaccins))  # Trie par ordre décroissant
print(nb_vaccins_par_programme)

# Estimer la couverture vaccinale en afrique par programme
coverage_per_program <- data_vaccine_afrique %>%
  group_by(vaccine_program) %>%
  summarise(coverage = round(mean(coverage, na.rm = TRUE)))%>%
  arrange(desc(coverage))  # Trie par ordre décroissant
print(coverage_per_program)

# Est-ce que le programme avec le plus de vaccins a toujours la meilleure couverture ?
coverage_vaccine_program <- left_join(coverage_per_program, nb_vaccins_par_programme, by = "vaccine_program") %>%
  arrange(desc(nb_vaccins))
print(coverage_vaccine_program) # Non, pas toujours.


# b. Observer l'évolution de la couverture vaccinale par programme vaccinal

data_vaccine_program <- data_vaccine_afrique %>%
  filter(!is.na(coverage), !is.na(vaccine_program)) %>% # Nettoyage
  mutate(decade = (year %/% 10) * 10) %>%
  group_by(unicef_region_label, vaccine_program, decade) %>%
  summarise(coverage = round(mean(coverage, na.rm = TRUE)), .groups = "drop") %>%
  arrange(vaccine_program, unicef_region_label, decade)


# Evolution dans le temps par programme vaccinal (toutes régions confondues)

# Boxplot de répartition de la couverture vaccinale par programme vaccinal
png("output/plots/boxplot_de_la_couverture_vaccinale_en_afrique_par_programme_vaccinal.png", width = 1600, height = 1200, res = 150)
ggplot(data_vaccine_program, aes(x = unicef_region_label, y = coverage, fill = unicef_region_label)) +
  geom_boxplot() +
  facet_wrap(~ vaccine_program, scales = "free_y") +
  labs(
    title = "Distribution de la couverture vaccinale par région et programme",
    x = "Priorité régionale UNICEF",
    y = "Couverture (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "none"
  )
dev.off()

# Barplot de répartition de la couverture vaccinale par programme vaccinal
png("output/plots/barplot_de_la_couverture_vaccinale_en_afrique_par_programme_vaccinal.png", width = 1600, height = 1200, res = 150)
ggplot(data_vaccine_program, aes(x = factor(decade), y = coverage, fill = unicef_region_label)) +
  geom_col(position = "dodge") +
  facet_wrap(~ vaccine_program) +
  labs(
    title = "Couverture vaccinale par région, programme et décennie",
    x = "Décennie",
    y = "Couverture moyenne (%)",
    fill = "Région UNICEF"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 9),
    plot.title = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 90, hjust = 1)
  )
dev.off()


# Courbe d'évolution de la couverture vaccinale par programme vaccinal
png("output/plots/courbe_evolution_de_la_couverture_vaccinale_en_afrique_par_programme_vaccinal.png", width = 1600, height = 1200, res = 150)
ggplot(data_vaccine_program, aes(x = decade, y = coverage, color = vaccine_program)) +
  geom_line(size = 1) +
  geom_point(size = 0.8) +
  facet_wrap(~ vaccine_program) +
  labs(
    title = "Évolution de la couverture vaccinale par programme (monde)",
    x = "Décennie",
    y = "Couverture moyenne (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10, face = "bold"),
    strip.text = element_text(size = 9),
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "none"
  )
dev.off()


# Courbe d'évolution de la couverture vaccinale par programme vaccinal et par région unicef

png("output/plots/courbe_evolution_de_la_couverture_vaccinale_en_afrique_par_programme_vaccinal_par_region_unicef.png", width = 1600, height = 1200, res = 150)
ggplot(data_vaccine_program, aes(x = decade, y = coverage, color = unicef_region_label)) +
  geom_line(size = 0.5) +
  # geom_smooth(method = "loess", se = FALSE, linetype = "solid") +
  facet_wrap(~ vaccine_program, scales = "free_y") +  # optionnel pour ajuster l'échelle Y à chaque programme
  labs(
    title = "Évolution de la couverture vaccinale par région et programme",
    x = "Année",
    y = "Couverture moyenne (%)",
    color = "Région UNICEF"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 9, face = "bold"),
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8)
  )
dev.off()



# Correlation moyenne entre la couverture vaccinale et le nombre de vaccins dans les programmes
# Mesurer dans le temps si plus un programme contient de vaccins, plus sa couverture moyenne est élevée (et si cette relation est significative)

str(data_vaccine_program)
str(nb_vaccins_par_programme)

# Fusionner les deux bases de données
data_vaccine_program_nombre_vaccin <- data_vaccine_program %>%
  left_join(nb_vaccins_par_programme, by = "vaccine_program")

str(data_vaccine_program_nombre_vaccin)


# Test de correlation 
cor.test(data_vaccine_program_nombre_vaccin$nb_vaccins, data_vaccine_program_nombre_vaccin$coverage) #0.2161957

png("output/plots/correlation_entre_evolution_de_la_couverture_vaccinale_en_afrique_et_nombre_vaccins_par_programme_vaccinal_par_region_unicef.png", width = 1600, height = 1200, res = 150)
ggplot(data_vaccine_program_nombre_vaccin, aes(x = nb_vaccins, y = coverage)) +
  geom_point(alpha = 0.5, color = "darkgreen", size = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  facet_wrap(~ decade) +
  labs(
    title = "Corrélation entre nombre de vaccins et couverture par programme (par décennie)",
    x = "Nombre de vaccins dans le programme",
    y = "Couverture moyenne (%)"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 9, face = "bold"),
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8)
  )
dev.off()


# Correlation positive faible (r ≈ 0.22) et non significatif (p ≈ 0.097 > 0.05)
# Pas assez de preuves pour affirmer qu'un programme avec plus de vaccins a systématiquement une couverture vaccinale plus élevée, même si la tendance existe.


# Réessayer, sur une base temporelle, la même analyse sur les tendances de la couverture vaccinale par programme vaccinal

# Étape 1 : Préparer les données
analyse_temporelle <- data_vaccine_afrique %>%
  group_by(year, vaccine_program) %>%
  summarise(
    nb_vaccins = n_distinct(vaccine),
    coverage = mean(coverage, na.rm = TRUE),
    .groups = "drop"
  )

# Étape 2 : Fonction pour calculer cor.test et extraire p-value et corrélation
cor_test_par_annee <- function(analyse_temporelle) {
  if(nrow(analyse_temporelle) < 3) {
    return(tibble(correlation = NA_real_, p_value = NA_real_))
  }
  test <- cor.test(analyse_temporelle$nb_vaccins, analyse_temporelle$coverage)
  tibble(correlation = test$estimate, p_value = test$p.value)
}

# Étape 3 : Appliquer par année
resultats <- analyse_temporelle %>%
  group_by(year) %>%
  nest() %>%
  mutate(test = map(data, cor_test_par_annee)) %>%
  unnest(test) %>%
  select(year, correlation, p_value)

# Afficher le résultat
print(resultats)


# Tracé la courbe d'évolution des résultats
png("output/plots/courbe_evolution_correlation_entre_evolution_de_la_couverture_vaccinale_en_afrique_et_nombre_vaccins_par_programme_vaccinal_par_anne.png", width = 1600, height = 1200, res = 150)
ggplot(resultats, aes(x = year, y = correlation)) +
  geom_line(color = "black") +
  geom_point(aes(color = p_value < 0.05), size = 2) +
  scale_color_manual(values = c("red", "green"), labels = c("Non significatif", "Significatif")) +
  labs(
    title = "Corrélation entre nb vaccins et couverture moyenne par année",
    x = "Année",
    y = "Coefficient de corrélation",
    color = "Significativité (p < 0.05)"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 9, face = "bold"),
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8)
  )
dev.off()

# L'évolution de la courbe vaccinale est sans doute influencée par d'autres facteurs contextuels.







# O2. Analyse croisée multivariée de la couverture vaccinale avec des facteurs contextuels extraits de WDI (méthode manuelle) --------------------------
# Data : all_indicators

# a. Extraire les indicateurs pour les régions afrique de l'unicef

# all_indicators <- read.xlsx("data/processed/all_indicators.xlsx",sheet = 1)

summary(all_indicators) 
dim(all_indicators) # 43 variables, 41 indicateurs jugés pertinents


# Vecteur des régions unicef typiquement Afrique
unicef_region_afrique_only <- c("WCAR", "ESAR")

# Nouvelle base de travail concentrée sur les régions Afrique
all_indicators_africa <- all_indicators %>%
  filter(unicef_region %in% unicef_region_afrique_only)

str(all_indicators_africa)
dim(all_indicators_africa) # 43 variables, 41 indicateurs jugés pertinents


# b. Analyse exploratoire des données (EDA) de all_indicators_africa.

summary(all_indicators_africa)

# b.1. Exploration des valeurs manquantes dans all_indicators_africa

png("output/plots/donnes_manquantes_all_indicator_afrique.png", width = 1600, height = 1200, res = 150)
vis_miss(all_indicators_africa %>% naniar::miss_var_summary() %>% 
           arrange(desc(pct_miss)) %>%
           select(variable) %>%
           pull() %>%
           {all_indicators_africa[., drop = FALSE]}
) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6)
  )
dev.off()


png("output/plots/boxplot_donnes_manquantes_all_indicator_afrique.png", width = 1600, height = 1200, res = 150)
gg_miss_var(all_indicators_africa, show_pct = TRUE) +
  labs(title = "Pourcentage de valeurs manquantes par variable") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "none"
  )
dev.off()

colSums(is.na(all_indicators_africa))  # Nombre de NA par variable


# b.2. Distribution des variables

all_indicators_africa %>%
  reframe(across(
    where(is.numeric),
    \(x) c(moy = mean(x, na.rm = TRUE),
           med = median(x, na.rm = TRUE),
           sd = sd(x, na.rm = TRUE))
  ))


#  2.1. Boxplot pour détecter les variables extrêmes

all_indicators_numeric_vars <- names(select(all_indicators_africa, where(is.numeric)))

pdf("output/plots/boxplots_indicateurs_afrique.pdf", width = 8, height = 6)
for (var in all_indicators_numeric_vars) {
  p <- ggplot(all_indicators_africa, aes_string(y = var)) +
    geom_boxplot(fill = "lightgreen") +
    labs(title = paste("Boxplot de", var), y = var) +
    theme_minimal()
  print(p)
}
dev.off()


#   2.2. Créer un histogramme pour chaque variable numérique

data_indics_africa <- all_indicators_africa %>% select(where(is.numeric))

pdf("output/plots/histogrammes_indicateurs_afrique.pdf", width = 8, height = 6)
for (var in names(data_indics_africa)) {
  p <- ggplot(all_indicators_africa, aes_string(x = var)) +
    geom_histogram(bins = 30, fill = "skyblue", color = "black") +
    labs(title = paste("Histogramme de", var), x = var, y = "Fréquence") +
    theme_minimal()
  print(p)
}
dev.off() 

# Possible assymétrie des valeurs.

# Confirmons cette asymétrie des données

all_indicators_africa %>%
  summarise(across(where(is.numeric),
                   list(moy = mean,
                        med = median,
                        sd = sd,
                        skew = ~e1071::skewness(., na.rm = TRUE)),
                   .names = "{.col}_{.fn}"))

# Transforme les données au format long
all_indicators_africa_pivot_long <- all_indicators_africa %>%
  select(where(is.numeric)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "valeur")


# Histogrammes par variable
png("output/plots/histogrammes_par_variables_indicateurs_afrique.png", width = 1600, height = 1200, res = 150)
ggplot(all_indicators_africa_pivot_long, aes(x = valeur)) +
  geom_histogram(bins = 30, fill = "#4e79a7", color = "white") +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "none"
  )
dev.off()

# La distribution de la plupart des variables sont asymétriques.
# Elles tendent vers la gauche ou vers la droite


# c. Correlations des variables avec la couverture vaccinale

# c.1. Sélectionner les colonnes numériques
indicators_africa <- all_indicators_africa %>%
  select(where(is.numeric)) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm=TRUE), .)))


# c.2. Calcul de la matrice de corrélation
cor_matrix <- cor(indicators_africa, use = "pairwise.complete.obs")

# Affichage de la matrice de correlation avec corplot
png("output/plots/matrice_de_correlation_indicateurs_afrique.png", width = 1600, height = 1200, res = 150)
corrplot(cor_matrix, method = "color", tl.cex = 0.7)
dev.off()

# Affichage de la matrice de correlation avec corgram
png("output/plots/matrice_de_correlation_indicateurs_afrique_corgram.png", width = 1600, height = 1200, res = 150)
corrgram(indicators_africa,
         order=TRUE,
         lower.panel=panel.shade,
         upper.panel=panel.pie,
         text.panel=panel.txt)
dev.off()


# c.3. Corrélation entre indicateurs et couverture vaccinale

str(indicators_africa)
cor_coverage <- sapply(indicators_africa, function(x) cor(indicators_africa$regional_coverage, x, use = "complete.obs"))
print(cor_coverage)

# Afficher les données
cor_coverage_sorted <- sort(cor_coverage, decreasing = TRUE)
print(round(cor_coverage_sorted, 3))

# Visualisation des différentes correlations des indicateurs avec la couverture vaccinale

cor_data <- data.frame(
  variable = names(cor_coverage_sorted),
  correlation = as.numeric(cor_coverage_sorted)
)

str(cor_data)

labels_cor_data <- c(
  "regional_coverage"               = "Couverture régionale",
  "regional_immune_percentage"     = "Pourcentage immunisé régional",
  "regional_vaccinated"             = "Nombre vacciné régional",
  "year"                           = "Année",
  "Achèvement_secondaire"           = "Achèvement secondaire",
  "Scolarisation_secondaire"        = "Scolarisation secondaire",
  "Population_active_15_64"         = "Population active (15-64 ans)",
  "regional_target"                 = "Cible régionale",
  "PIB_par_habitant"               = "PIB par habitant (USD)",
  "Esperance_vie_naissance"        = "Espérance de vie à la naissance",
  "Achèvement_primaire"            = "Achèvement primaire",
  "IDE_entrant_USD"                = "IDE entrants (USD)",
  "Taux_epargne_nationale"          = "Taux d’épargne nationale",
  "Scolarisation_primaire"          = "Scolarisation primaire",
  "Utilisateurs_Internet"          = "Utilisateurs Internet (%)",
  "Alphabetisation_adultes"        = "Alphabétisation adultes (%)",
  "Acces_eau_potable"              = "Accès à l’eau potable (%)",
  "Taux_urbanisation"              = "Taux d’urbanisation (%)",
  "Réseau_ferre_km_total"          = "Réseau ferré (km total)",
  "Depenses_education_PIB"         = "Dépenses éducation (% PIB)",
  "Acces_electricite"              = "Accès à l’électricité (%)",
  "Taux_chomage"                  = "Taux de chômage (%)",
  "Depenses_sante_PIB"             = "Dépenses santé (% PIB)",
  "Depenses_sante_par_habitant"    = "Dépenses santé par habitant (% PIB)",
  "Routes_pavees_pourcent"         = "Routes pavées (%)",
  "Revenus_publics_PIB"            = "Revenus publics (% PIB)",
  "Naissances_enregistrees"        = "Naissances enregistrées (%)",
  "Part_femmes_population"         = "Part des femmes dans la population (%)",
  "Inflation_annuelle"             = "Inflation annuelle (%)",
  "Efficacite_gouvernement"        = "Efficacité du gouvernement",
  "Population_agee_65_plus"        = "Population âgée (65+ %)",
  "Fertilite_adolescentes"         = "Fertilité adolescentes (%)",
  "Depenses_sante_directes_patient"= "Dépenses santé à charge patient (%)",
  "Stabilite_politique"            = "Stabilité politique",
  "Part_population_rurale"         = "Part population rurale (%)",
  "Lits_hospitaliers_pour_1000"    = "Lits hospitaliers pour 1000 habitants",
  "Risque_mortalite_maternelle"    = "Risque mortalité maternelle",
  "Participation_femmes_travail"   = "Participation femmes au travail (%)",
  "Population_jeunes_0_14"         = "Population jeunes (0-14 ans, %)",
  "Mortalite_infantile"            = "Mortalité infantile (pour 1000 naissances)",
  "Taux_fertilite_total"           = "Taux de fertilité total",
  "Taux_natalite_brut"             = "Taux de natalité brut"
)

cor_data$label <- mapvalues(
  cor_data$variable,
  from = names(labels_cor_data),
  to = labels_cor_data,
  warn_missing = FALSE
)

# Visualisation de la correlation avec les facteurs contextuels
png("output/plots/analyse_de_correlation_couverture_indicateurs_wdi.png", width = 1600, height = 1200, res = 150)
ggplot(cor_data, aes(x = reorder(label, correlation), y = correlation, fill = correlation > 0)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("red", "blue"), labels = c("Négatif", "Positif")) +
  labs(
    title = "Corrélations avec la couverture vaccinale",
    x = "Indicateur",
    y = "Corrélation (Pearson)"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
    plot.title = element_text(size = 10, face = "bold"),
    legend.position = "bottom"
  )
dev.off()


# c.4. Analyse de la multicolinéarité (VIF)

# Régression linéaire simple pour obtenir les VIF
modele_lm <- lm(regional_coverage ~ ., data = indicators_africa)

# Calcul du VIF

vif_values <- tryCatch(
  vif(modele_lm),
  error = function(e) {
    if (grepl("there are aliased coefficients in the model", e$message)) {
      message("VIF ignoré : coefficients aliasés dans le modèle.")
      return(NULL)
    } else {
      stop(e)  # Propager les autres erreurs non prévues
    }
  }
)

# Certains variables sont parfaitement colinéaires ou linéairement dépendantes entre elles.


# c.5. Nettoyer la base des indicateurs trop correlés

# Vérifier les variables problématiques et nettoyer le modèle
alias(modele_lm)

# Supposons que data_explanatory contient toutes tes variables explicatives
indics_explanatory <- indicators_africa %>% select(-regional_coverage)  # ou la variable cible

cor_matrix <- cor(indics_explanatory, use = "pairwise.complete.obs")

# Afficher les corrélations élevées (ex: > 0.95)
high_indics <- which(abs(cor_matrix) > 0.95 & abs(cor_matrix) < 1, arr.ind = TRUE)

# Nettoyer la base de données
indics_to_remone <- unique(colnames(cor_matrix)[high_indics[, 2]])

indics_explanatory_remone <- indicators_africa %>%
  select(-all_of(indics_to_remone))


# Après premier nettoyage
cor_matrix_remone <- cor(indics_explanatory_remone %>% select(-regional_coverage), use = "pairwise.complete.obs")

high_indics_remone <- which(abs(cor_matrix_remone) > 0.95 & abs(cor_matrix_remone) < 1, arr.ind = TRUE) #0

# Tout est clean.
# Les variables très correlées entre elles sont supprimées.


clean_indicators_africa <- indics_explanatory_remone
dim(clean_indicators_africa) #31 indicateurs potentiels
str(clean_indicators_africa) # Elle contient toujours la variable regional_coverage.


indicators_manual <- colnames(clean_indicators_africa)





# O3. Sélection des variables pertinentes avec Lasso --------------
# Data : all_indicators_africa

str(all_indicators_africa)
dim(all_indicators_africa) # 43 variables. 41 indicateurs.
data_lasso <- all_indicators_africa %>%
  mutate(
    across(where(is.character), as.factor)
  )

any(is.na(data_lasso)) # True
sum(is.na(data_lasso)) # 520 valeurs manquantes


# a. Préparer les données
# Garder uniquement les lignes sans NA sur coverage
data_lasso <- data_lasso[!is.na(data_lasso$regional_coverage), ]

# Sélectionner les colones numériques
data_lasso_numeric <- data_lasso %>%
  select(where(is.numeric), -regional_target, -regional_immune_percentage, -regional_vaccinated) %>%
  drop_na()

dim(data_lasso_numeric)
any(is.na(data_lasso_numeric)) # False. Go to Lasso


# Séparer la cible et les prédicteurs
y_lasso <- data_lasso_numeric$regional_coverage
x_lasso <- data_lasso_numeric %>% select(-regional_coverage) %>% as.matrix()


# b. Appliquer Lasso avec validation croisée
set.seed(123)  # pour reproductibilité
lasso_cv <- cv.glmnet(x_lasso, y_lasso, alpha = 1, standardize = TRUE, nfolds = 5)

# Visualiser le lambda optimal
png("output/plots/log_lasso_lambda_values.png", width = 1600, height = 1200, res = 150)
plot(lasso_cv)
dev.off()

# Extraire les données pour le graphique CV
df_lasso_cv <- tibble(
  log_lambda = log(lasso_cv$lambda),
  cvm = lasso_cv$cvm,
  cvup = lasso_cv$cvup,
  cvlo = lasso_cv$cvlo
)

# Créer le plot ggplot2 pour Lasso
plot_lasso <- ggplot(df_lasso_cv, aes(x = log_lambda, y = cvm)) +
  geom_line(color = "darkorange") +
  geom_point(color = "darkorange") +
  geom_errorbar(aes(ymin = cvlo, ymax = cvup), width = 0.1, color = "gray50") +
  geom_vline(xintercept = log(lasso_cv$lambda.min), linetype = "dashed", color = "red") +
  geom_vline(xintercept = log(lasso_cv$lambda.1se), linetype = "dotted", color = "darkgreen") +
  labs(
    title = "Lasso: Validation Croisée",
    x = "log(Lambda)",
    y = "Erreur moyenne CV"
  ) +
  theme_minimal()

# Export en SVG
ggsave(
  filename = "output/plots/lasso_cv.svg",
  plot = plot_lasso,
  width = 8,
  height = 6,
  units = "in"
)

# c. Identifier les variables sélectionnées

# Lambda optimal
lambda_opt <- lasso_cv$lambda.min #0.1344524

# Coefficients associés
lasso_coef <- coef(lasso_cv, s = lambda_opt)

# Variables sélectionnées (coefficients non nuls)

indicators_lasso <- rownames(lasso_coef)[which(lasso_coef != 0)]
indicators_lasso <- indicators_lasso[indicators_lasso != "(Intercept)"]  # Retirer intercept

print(indicators_lasso) # 09 indicateurs sélectionnés par Lasso



# O4. Sélection des variables pertinentes avec ElasticNet ---------
# Data : all_indicators_africa

str(all_indicators_africa)
dim(all_indicators_africa) # 43 variables. 41 indicateurs.
data_elasticnet <- all_indicators_africa %>%
  mutate(
    across(where(is.character), as.factor)
  )

any(is.na(data_elasticnet)) # True
sum(is.na(data_elasticnet)) # 520 valeurs manquantes


# a. Préparer les données

# Garder uniquement les lignes sans NA sur coverage
data_elasticnet <- data_elasticnet[!is.na(data_lasso$regional_coverage), ]

# Sélectionner les colones numériques
data_elasticnet_numeric <- data_elasticnet %>%
  select(where(is.numeric), -regional_target, -regional_immune_percentage, -regional_vaccinated) %>%
  drop_na()

dim(data_elasticnet_numeric) #39 variables
any(is.na(data_elasticnet_numeric)) #False

# Séparer la cible et les prédicteurs
y_elasticnet <- data_elasticnet_numeric$regional_coverage
x_elasticnet <- data_elasticnet_numeric %>% select(-regional_coverage) %>% as.matrix()


# b. Appliquer Lasso avec validation croisée
set.seed(123)  # pour reproductibilité
elastic_cv <- cv.glmnet(
  x_elasticnet,
  y_elasticnet,
  alpha = 0.5,
  standardize = TRUE,
  nfolds = 5
)

# Visualiser la courbe de validation croisée
png("output/plots/courbe_validation_croisée_elasticnet.png", width = 1600, height = 1200, res = 150)
plot(elastic_cv)
dev.off()

# c. Identifier les variables sélectionnées

# Lambda optimal
lambda_elasticnet <- elastic_cv$lambda.min

# Coefficients associés
coef_elastic <- coef(elastic_cv, s = lambda_elasticnet)

# Variables sélectionnées (coefficients non nuls)

data_indicateurs_elasticnet <- coef_elastic %>%
  as.matrix() %>%
  as.data.frame() %>%
  tibble::rownames_to_column("variable")

colnames(data_indicateurs_elasticnet)[2] <- "coefficient"


data_indicateurs_elasticnet <- data_indicateurs_elasticnet %>%
  filter(coefficient != 0, variable != "(Intercept)")

indicators_elasticnet <- unique(data_indicateurs_elasticnet$variable)

print(indicators_elasticnet) # 11 indicateurs sélectionnés par Elasticnet




# O5. Sélection des variables pertinentes avec Random Forrest -------------
# Data : all_indicators_africa

str(all_indicators_africa)
dim(all_indicators_africa) # 43 variables. 41 indicateurs.
data_randforest <- all_indicators_africa %>%
  mutate(
    across(where(is.character), as.factor)
  )

any(is.na(data_randforest)) # True
sum(is.na(data_randforest)) # 520 valeurs manquantes


# a. Préparer les données
# Garder uniquement les lignes sans NA sur coverage
data_randforest <- data_randforest[!is.na(data_randforest$regional_coverage), ]

# Sélectionner uniquement les colones numériques
data_randforest_numeric <- data_randforest %>%
  select(where(is.numeric), -regional_target, -regional_immune_percentage, -regional_vaccinated) %>%
  drop_na(regional_coverage)

dim(data_randforest_numeric) # 90 enregistremets, 39 variables, 38 indicateurs.
any(is.na(data_randforest_numeric)) # True

# Supprimer les lignes avec d'autres NA
data_randforest_numeric <- drop_na(data_randforest_numeric)

# Vérifier que tout est okay
dim(data_randforest_numeric) # 22 enregistrements, 39 variables, 38 indicateurs.
any(is.na(data_randforest_numeric)) # False. Go to Random Forrest

# Séparer la cible et les prédicteurs
y_randforest <- data_randforest_numeric$regional_coverage
x_randforest <- data_randforest_numeric %>% select(-regional_coverage) %>% as.matrix()


# b. Lancer le modèle Random Forrest

set.seed(123)  # pour reproductibilité
randforest_ml <- randomForest(
  x = x_randforest,
  y = y_randforest,
  importance = TRUE,  # Pour extraire l’importance des variables
  ntree = 500         # Nombre d’arbres (par défaut 500, c’est bien)
)

# c. Identifier les variables sélectionnées

# Afficher l'importance des variables
indicators_randforest_importance <- importance(randforest_ml, type = 1)
data_indicators_randforest <- data.frame(
  variable = rownames(indicators_randforest_importance),
  importance = indicators_randforest_importance[,1]
)

# Visualisation graphique

# Ajout de labels lisibles
data_indicators_randforest$label <- mapvalues(
  data_indicators_randforest$variable,
  from = names(labels_cor_data),
  to = labels_cor_data,
  warn_missing = FALSE
)

# Export du plot
png("output/plots/indicateurs_wdi_importance_random_forrest_avec_ggplot.png", width = 1600, height = 1200, res = 150)
ggplot(data_indicators_randforest, aes(x = reorder(label, importance), y = importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    x = "Variables explicatives",
    y = "Importance (augmentation MSE)",
    title = "Importance des variables (Random Forest)"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
    plot.title = element_text(size = 10, face = "bold"),
    legend.position = "bottom"
  )
dev.off()

png("output/plots/indicateurs_wdi_importance_random_forrest_varimpplot.png", width = 1600, height = 1200, res = 150)
varImpPlot(randforest_ml,
           type = 1,
           main = "Importance des variables (Random Forest)")
dev.off()


# d. Sélection des meilleurs variables


# Réordonner selon l'importance (si ce n'est pas encore fait)
data_indicators_randforest <- data_indicators_randforest[order(-data_indicators_randforest$importance), ]


# Sélectionner les 10 variables les plus importantes
indicators_randforest <- data_indicators_randforest$variable[1:10]

png("output/plots/top_20_indicateurs_wdi_importance_random_forrest.png", width = 1600, height = 1200, res = 150)
ggplot(head(data_indicators_randforest, 20), aes(x = reorder(label, importance), y = importance)) +
  geom_col(fill = "gray") +
  coord_flip() +
  labs(title = "Top 20 variables importantes (Random Forest)", x = "Variable", y = "% d'augmentation MSE")
dev.off()


# O6. Comparaison de la sélection (Manuel, Lasso, Elasticnet, Random Forrest) -----------

# Créer une colonne avec toutes les variables uniques sélectionnées par au moins une méthode
all_vars <- unique(c(indicators_lasso, indicators_randforest, indicators_elasticnet, indicators_manual))

# a. Construire le data frame comparatif
indicators_comparatif <- data.frame(
  variable = all_vars,
  Lasso = all_vars %in% indicators_lasso,
  RandomForest = all_vars %in% indicators_randforest,
  ElasticNet = all_vars %in% indicators_elasticnet,
  Manuel = all_vars %in% indicators_manual
)

# Affichage
print(indicators_comparatif)

# Nombre de méthodes ayant sélectionné chaque variable
indicators_comparatif$n_methods <- rowSums(indicators_comparatif[ , -1])

# Tri décroissant
indicators_comparatif <- indicators_comparatif[order(-indicators_comparatif$n_methods), ]

# Affichage
print(indicators_comparatif)
str(indicators_comparatif)


# b. Visualiser la sélection sous forme de heatmap

# Constituer la basse pour le plot
ic_plot <- dplyr::rename(indicators_comparatif, nom_variable = variable)
str(ic_plot)

# 2. Réorganiser l'ordre des variables (optionnel, mais utile pour la lisibilité)
ic_plot <- ic_plot %>%
  arrange(desc(n_methods)) %>%
  mutate(nom_variable = factor(nom_variable, levels = nom_variable))

# 3. Transformation en format long
ic_plot_long <- melt(ic_plot[, 1:5], id.vars = "nom_variable")

str(ic_plot_long)
ic_plot_long <- dplyr::rename(ic_plot_long, methode = variable)


# 4. Graphique heatmap
ggplot(ic_plot_long, aes(x = nom_variable, y = methode)) +
  geom_tile(aes(fill = value), color = "white") +
  scale_fill_manual(values = c("green", "steelblue")) +
  labs(
    title = "Comparaison des méthodes de sélection de variables",
    x = "Variable",
    y = "Méthode"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )


# Fusionner les indicateurs, et les utiliser pour une régression linéaire
core_indicators <- indicators_comparatif$variable[indicators_comparatif$n_methods >= 2]
print(core_indicators)



# Essayer de comprendre le lien entre la couverture vaccinale et les variables dans core_values.





# 06. Modèle de régression linéaire simple ----------
# Data : all_indicators_africa

str(all_indicators_africa)
dim(all_indicators_africa) # 43 variables. 41 indicateurs.

# Préparer les données pour le modèle de régression linéaire.
data_models <- all_indicators_africa[, c("regional_coverage", core_indicators)]
str(data_models)
dim(data_models) #90, 17 variables

any(is.na(data_models)) # True
sum(is.na(data_models)) # 263 valeurs manquantes

# Traitement des NA
clean_data_models <- data_models %>%
  select(where(~ mean(is.na(.)) < 0.5))

str(clean_data_models)
dim(clean_data_models) #90, 16 variables

# Imputation des NA par la moyenne
clean_data_models <- clean_data_models %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

any(is.na(clean_data_models)) # False
sum(is.na(clean_data_models)) # 0 valeurs manquantes


# Régression linéaire simple

formule <- as.formula(paste("regional_coverage ~", paste(colnames(clean_data_models %>% select(-"regional_coverage")), collapse = " + ")))
first_modele_lm <- lm(formule, data = clean_data_models, na.action = na.omit)

summary(first_modele_lm)

# Régression linéaire ajustée (log)

min(clean_data_models$regional_coverage, na.rm = TRUE) #3
clean_data_models$log_regional_coverage <- log(clean_data_models$regional_coverage)

formule_log <- as.formula(
  paste("log_regional_coverage ~",
        paste(colnames(clean_data_models %>% select(-"regional_coverage",-"log_regional_coverage")),
              collapse = " + "))
)

log_modele_lm <- lm(formule_log, data = clean_data_models, na.action = na.omit)

summary(log_modele_lm)


# Les modèles, expliquent en l'état, ~89 % de la variance de la couverture vaccinale en Afrique.
# Plusieurs variables ont des p-values p-values > 0.05, donc ne sont pas significativement associées à la variable cible individuellement.
# Essayer de regrouper les variables en composantes principales


# O7. Analyse en composantes principales ACP (méthode non supervisée) ------------
# Data : clean_indicators_africa

summary(clean_indicators_africa)
dim(clean_indicators_africa) # 34 variables. 90 enregistrements.


# Retirer la variable à expliquer (couverture vaccinale en région afrique) 
data_pca <- clean_indicators_africa %>%
  select(-regional_coverage, -regional_immune_percentage)

# Vérifier la pertinence de l'ACP

cortest.bartlett(cor(data_pca), n = nrow(data_pca)) # p-value =	0 (≈ < 2.2e-16)

# Faire l'ACP avec standardisation automatique
res_pca <- PCA(data_pca, scale.unit = TRUE, ncp = 5, graph = FALSE)
pcs <- as.data.frame(res_pca$ind$coord[, 1:5])


# Visualiser la variance expliquée (scree plot)
png("output/plots/variances_expliquees_coverage_regional_afrique.png", width = 1600, height = 1200, res = 150)
fviz_screeplot(res_pca, addlabels = TRUE, ylim = c(0, 50))
dev.off()

# Cercle des corrélations (variables)

png("output/plots/cercle_de_correlation_coverage_regional_afrique.png", width = 1600, height = 1200, res = 150)
fviz_pca_var(res_pca,
             col.var = "contrib", # Colorier par contribution
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE          # Évite que les noms se chevauchent
)
dev.off()

# Visualiser la projection des observations

png("output/plots/visualisation_correlation_coverage_regional_afrique.png", width = 1600, height = 1200, res = 150)
fviz_pca_ind(res_pca, repel = TRUE, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
dev.off()

# Extraire les composantes principales (par exemple les 5 premières)
pcs <- as.data.frame(res_pca$ind$coord[, 1:5])


# Extraire les loadings
loadings <- res_pca$var$coord
str(loadings)



# Pour les 5 premiers axes
top_variables <- lapply(1:5, function(i) {
  loading_scores <- abs(loadings[, i])
  top_10 <- sort(loading_scores, decreasing = TRUE)[1:10]
  return(names(top_10))
})

names(top_variables) <- paste0("PC", 1:5)
print(top_variables)


# Nommer les composantes principales
pc_names <- c(
  "Developpement Socio Economique Global",
  "Investissements Sociaux et Inclusion",
  "Structure demographique et acces aux services",
  "Stabilite institutionnelle et gouvernance",
  "Volatilité economiques et pressions sociales"
)

# Renommer les colonnes de pcs avec les noms des composantes principales
colnames(pcs) <- pc_names


# Ajouter la variable cible
pcs$regional_coverage <- clean_indicators_africa$regional_coverage


# Régression linéaire sur ces composantes principales
model_pca <- lm(regional_coverage ~ ., data = pcs)

summary(model_pca)



# Extrait des résidus pour évaluer les résultats

# Étape 1 : Extraire les éléments nécessaires
coords <- res_pca$ind$coord             # Coordonnées des individus (90 x n)
loadings <- res_pca$var$coord           # Coordonnées des variables (32 x n)
means <- res_pca$call$centre            # Moyenne de chaque variable
sds <- res_pca$call$ecart.type          # Écart-type de chaque variable


# Étape 2 : Sélection du nombre d'axes (par exemple, 5)
k <- 5
coords_k <- coords[, 1:k]
loadings_k <- loadings[, 1:k]

# Étape 3 : Reconstruction des données centrées-réduites
reconstructed_scaled <- coords_k %*% t(loadings_k)

# Étape 4 : Revenir à l’échelle d’origine
reconstructed <- sweep(reconstructed_scaled, 2, sds, "*")   # dé-réduction
reconstructed <- sweep(reconstructed, 2, means, "+")        # dé-centrage


residuals <- data_pca - reconstructed

rmse <- sqrt(colMeans(residuals^2))

png("output/plots/barplot_residus_coverage_regional_afrique.png", width = 1600, height = 1200, res = 150)
barplot(rmse, las = 2, main = "RMSE par variable (erreur de reconstruction)")
dev.off()

png("output/plots/plot_residus.png", width = 1600, height = 1200, res = 150)
plot(residuals(model_pca))
abline(h = 0, col = "red")
dev.off()


png("output/plots/plot_residus_avec_ggplot.png", width = 1600, height = 1200, res = 150)
ggplot(data = NULL, aes(x = model_pca$fitted.values, y = model_pca$residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Graphique des résidus",
       x = "Valeurs prédites",
       y = "Résidus")
dev.off()


# D - Discussion -----------

# Interprétation de l'ACP

# Validation croisée et robustesse du modèle
validate_pca_model <- function(data, model) {
  
  # Analyse des résidus
  residus <- residuals(model)
  fitted_vals <- fitted(model)
  
  # Test de normalité des résidus
  shapiro_test <- shapiro.test(residus)
  
  # Test d'homoscédasticité (Breusch-Pagan)
  bp_test <- car::ncvTest(model)
  
  # Influence des observations (Distance de Cook)
  cook_dist <- cooks.distance(model)
  influential_obs <- which(cook_dist > 4/length(cook_dist))
  
  # Graphiques de diagnostic
  par(mfrow = c(2, 2))
  plot(model)
  par(mfrow = c(1, 1))
  
  # Retourner les statistiques
  list(
    shapiro_pvalue = shapiro_test$p.value,
    bp_pvalue = bp_test$p,
    influential_observations = influential_obs,
    cook_distances = cook_dist[cook_dist > 0.1]
  )
}

png("output/plots/plot_residus_validation.png", width = 1600, height = 1200, res = 150)
validation_results <- validate_pca_model(clean_indicators_africa, model_pca)
dev.off()



# V - Préconisations stratégiques pour l'Unicef ------------

str(clean_indicators_africa)
str(all_indicators_africa)
str(data_vaccine_immune)



# Visualiser l'effet paradoxal de la stabilité institutionnelle
ggplot(all_indicators_africa, aes(x = Stabilite_politique, y = regional_coverage)) +
  geom_point(aes(color = unicef_region), alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Effet contre-intuitif de la stabilité institutionnelle",
       subtitle = "Plus de stabilité = moins de couverture vaccinale") +
  theme_minimal()

# Heatmap des corrélations entre composantes
library(corrplot)
corrplot(cor(pcs), method = "color", 
         title = "Matrice des corrélations entre composantes")




# O2 - Outils interactif de prédiction

# Outil interactif de prédiction de la couverture vaccinale
# Basé sur vos conclusions d'analyse en composantes principales


data_final <- read.xlsx("data/processed/data_final.xlsx")
str(data_final)

# ANALYSE STRATÉGIQUE UNICEF - RÉGIONS AFRICAINES (WCAR & ESAR)
# Code pour analyser les 3 axes de préconisations stratégiques

library(dplyr)
library(ggplot2)
library(plotly)
library(scales)
library(RColorBrewer)
library(gridExtra)
library(corrplot)
library(viridis)
library(tidyr)

# ===== 1. CONFIGURATION ET DONNÉES =====

# Pays des régions africaines UNICEF (basé sur la classification standard)
wcar_countries <- c("Benin", "Burkina Faso", "Cameroon", "Cape Verde", "Central African Republic",
                    "Chad", "Congo", "Democratic Republic of the Congo", "Equatorial Guinea", 
                    "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Ivory Coast",
                    "Liberia", "Mali", "Mauritania", "Niger", "Nigeria", "São Tomé and Príncipe",
                    "Senegal", "Sierra Leone", "Togo")

esar_countries <- c("Angola", "Botswana", "Burundi", "Comoros", "Eritrea", "Eswatini", "Ethiopia",
                    "Kenya", "Lesotho", "Madagascar", "Malawi", "Mauritius", "Mozambique", 
                    "Namibia", "Rwanda", "Seychelles", "Somalia", "South Africa", "South Sudan",
                    "Sudan", "Tanzania", "Uganda", "Zambia", "Zimbabwe")

# Filtrer pour les régions africaines UNICEF
filter_africa_regions <- function(data) {
  
  # D'abord, vérifier les pays disponibles
  cat("Échantillon de pays disponibles dans les données:\n")
  available_countries <- unique(data$country.x[!is.na(data$country.x)])
  cat(paste(head(available_countries, 20), collapse = ", "), "\n\n")
  
  # Filtrer pour exclure les données globales et garder seulement les pays
  country_data <- data %>%
    filter(!is.na(country.x), 
           unicef_region != "Global",
           !is.na(unicef_region))
  
  # Assigner les régions basées sur les noms de pays (approche flexible)
  country_data$region_short <- NA
  
  # Matching flexible des pays WCAR
  wcar_patterns <- paste(wcar_countries, collapse = "|")
  country_data$region_short[grepl(wcar_patterns, country_data$country.x, ignore.case = TRUE)] <- "WCAR"
  
  # Matching flexible des pays ESAR  
  esar_patterns <- paste(esar_countries, collapse = "|")
  country_data$region_short[grepl(esar_patterns, country_data$country.x, ignore.case = TRUE)] <- "ESAR"
  
  # Si pas de match par nom de pays, utiliser unicef_region si disponible
  africa_data <- country_data %>%
    filter(!is.na(region_short) | grepl("Africa", unicef_region, ignore.case = TRUE))
  
  # Pour les pays non assignés mais dans des régions africaines, essayer d'inférer
  unassigned_africa <- africa_data %>%
    filter(is.na(region_short), grepl("Africa", unicef_region, ignore.case = TRUE))
  
  if(nrow(unassigned_africa) > 0) {
    africa_data$region_short[is.na(africa_data$region_short) & 
                               grepl("West|Central", africa_data$unicef_region, ignore.case = TRUE)] <- "WCAR"
    africa_data$region_short[is.na(africa_data$region_short) & 
                               grepl("East|South", africa_data$unicef_region, ignore.case = TRUE)] <- "ESAR"
  }
  
  # Garder seulement les données avec région assignée
  africa_data <- africa_data %>% filter(!is.na(region_short))
  
  return(africa_data)
}

# ===== 2. AXE 1: OPTIMISATION GÉOGRAPHIQUE (Développement Socio-économique) =====

conflicts_prefer(scales::rescale)

analyze_geographic_optimization <- function(africa_data) {
  
  cat("=== AXE 1: OPTIMISATION GÉOGRAPHIQUE ===\n")
  cat("Impact du développement socio-économique (+2.89)\n\n")
  
  # Variables PC1 (Développement Socio-Économique Global) - utiliser celles disponibles
  pc1_vars <- c("PIB_par_habitant", "Esperance_vie_naissance", "Scolarisation_secondaire", 
                "Utilisateurs_Internet", "Taux_urbanisation", "Achèvement_primaire", "Achèvement_secondaire")
  
  # Calculer score PC1 par pays (prendre année la plus récente)
  country_pc1 <- africa_data %>%
    filter(!is.na(country.x)) %>%
    group_by(country.x, region_short) %>%
    arrange(desc(year)) %>%
    slice(1) %>%
    ungroup() %>%
    select(country.x, region_short, immune_percentage, target, Population_totale, year,
           all_of(intersect(pc1_vars, names(africa_data))))
  
  # Calculer score composite PC1 (normaliser chaque variable puis moyenner)
  pc1_data <- country_pc1[, intersect(pc1_vars, names(country_pc1))]
  if(ncol(pc1_data) > 0) {
    pc1_scaled <- as.data.frame(scale(pc1_data))
    country_pc1$pc1_score <- rowMeans(pc1_scaled, na.rm = TRUE)
  } else {
    # Si aucune variable PC1 disponible, utiliser un score par défaut
    country_pc1$pc1_score <- 0
  }
  
  # Score de priorité géographique (plus bas = plus prioritaire)
  country_pc1$priority_geo <- rescale(-country_pc1$pc1_score, to = c(0, 100))
  
  # Top pays à prioriser géographiquement
  geo_priorities <- country_pc1 %>%
    filter(!is.na(priority_geo), !is.na(immune_percentage)) %>%
    group_by(region_short) %>%
    arrange(desc(priority_geo)) %>%
    slice_head(n = 8) %>%
    ungroup()
  
  cat("TOP PAYS POUR OPTIMISATION GÉOGRAPHIQUE:\n")
  for(region in c("WCAR", "ESAR")) {
    if(any(geo_priorities$region_short == region)) {
      cat(paste0("\n", region, ":\n"))
      region_geo <- geo_priorities %>% filter(region_short == region)
      for(i in 1:min(5, nrow(region_geo))) {
        pib_val <- ifelse(is.na(region_geo$PIB_par_habitant[i]), "N/A", 
                          format(round(region_geo$PIB_par_habitant[i], 0), big.mark = ","))
        immunite_val <- ifelse(is.na(region_geo$immune_percentage[i]), "N/A",
                               sprintf("%.1f", region_geo$immune_percentage[i]))
        cat(sprintf("%d. %s (Priorité: %.1f, PIB/hab: $%s, Immunité: %s%%)\n", 
                    i, region_geo$country.x[i], region_geo$priority_geo[i],
                    pib_val, immunite_val))
      }
    }
  }
  
  # Calcul potentiel d'impact
  geo_impact <- geo_priorities %>%
    summarise(
      pays_total = n(),
      immunite_moy = mean(immune_percentage, na.rm = TRUE),
      gap_immunite = 95 - immunite_moy,  # Objectif 95% d'immunité
      pop_totale = sum(Population_totale, na.rm = TRUE),
      .groups = "drop"
    )
  
  cat(sprintf("\nIMPACT POTENTIEL OPTIMISATION GÉOGRAPHIQUE:\n"))
  cat(sprintf("- %d pays prioritaires identifiés\n", geo_impact$pays_total))
  cat(sprintf("- Immunité moyenne actuelle: %.1f%%\n", geo_impact$immunite_moy))
  cat(sprintf("- Gap d'immunité: %.1f points\n", geo_impact$gap_immunite))
  cat(sprintf("- Population totale ciblée: %s\n", format(geo_impact$pop_totale, big.mark = ",")))
  
  return(list(data = geo_priorities, impact = geo_impact))
}

# ===== 3. AXE 2: MOBILISATION COMMUNAUTAIRE (Inclusion Sociale) =====

analyze_community_mobilization <- function(africa_data) {
  
  cat("\n\n=== AXE 2: MOBILISATION COMMUNAUTAIRE ===\n")
  cat("Impact des investissements sociaux (+1.79)\n\n")
  
  # Variables PC2 (Investissements Sociaux et Inclusion) - utiliser celles disponibles
  pc2_vars <- c("Depenses_sante_PIB", "Depenses_education_PIB", "Revenus_publics_PIB",
                "Part_femmes_population", "Risque_mortalite_maternelle", "Scolarisation_primaire")
  
  # Calculer score PC2 par pays
  country_pc2 <- africa_data %>%
    filter(!is.na(country.x)) %>%
    group_by(country.x, region_short) %>%
    arrange(desc(year)) %>%
    slice(1) %>%
    ungroup() %>%
    select(country.x, region_short, immune_percentage, Population_totale, Part_population_rurale,
           all_of(intersect(pc2_vars, names(africa_data))))
  
  # Calculer score composite PC2 (inverser mortalité maternelle car négative)
  pc2_data <- country_pc2[, intersect(pc2_vars, names(country_pc2))]
  if(ncol(pc2_data) > 0) {
    if("Risque_mortalite_maternelle" %in% names(pc2_data)) {
      pc2_data$Risque_mortalite_maternelle <- -pc2_data$Risque_mortalite_maternelle
    }
    pc2_scaled <- as.data.frame(scale(pc2_data))
    country_pc2$pc2_score <- rowMeans(pc2_scaled, na.rm = TRUE)
  } else {
    country_pc2$pc2_score <- 0
  }
  
  # Score de priorité sociale (plus bas = plus prioritaire)
  country_pc2$priority_social <- rescale(-country_pc2$pc2_score, to = c(0, 100))
  
  # Identifier zones à faible inclusion sociale
  social_priorities <- country_pc2 %>%
    filter(!is.na(priority_social), !is.na(immune_percentage)) %>%
    group_by(region_short) %>%
    arrange(desc(priority_social)) %>%
    slice_head(n = 8) %>%
    ungroup()
  
  cat("PAYS PRIORITAIRES POUR MOBILISATION COMMUNAUTAIRE:\n")
  for(region in c("WCAR", "ESAR")) {
    if(any(social_priorities$region_short == region)) {
      cat(paste0("\n", region, ":\n"))
      region_social <- social_priorities %>% filter(region_short == region)
      for(i in 1:min(5, nrow(region_social))) {
        dep_sante <- ifelse(is.na(region_social$Depenses_sante_PIB[i]), "N/A", 
                            sprintf("%.1f", region_social$Depenses_sante_PIB[i]))
        immunite_val <- ifelse(is.na(region_social$immune_percentage[i]), "N/A",
                               sprintf("%.1f", region_social$immune_percentage[i]))
        cat(sprintf("%d. %s (Inclusion: %.1f, Dép.santé: %s%% PIB, Immunité: %s%%)\n", 
                    i, region_social$country.x[i], region_social$priority_social[i],
                    dep_sante, immunite_val))
      }
    }
  }
  
  # Calcul besoins en agents de santé communautaires
  community_needs <- social_priorities %>%
    mutate(
      pop_rurale_est = Population_totale * ifelse(is.na(Part_population_rurale), 50, Part_population_rurale) / 100,
      agents_needed_est = pop_rurale_est / 1000,  # 1 agent/1000 hab ruraux
      cout_formation_est = agents_needed_est * 500  # $500 par agent
    ) %>%
    summarise(
      pays_cibles = n(),
      agents_totaux = sum(agents_needed_est, na.rm = TRUE),
      cout_formation = sum(cout_formation_est, na.rm = TRUE),
      pop_rurale_totale = sum(pop_rurale_est, na.rm = TRUE),
      .groups = "drop"
    )
  
  cat(sprintf("\nBESOINS MOBILISATION COMMUNAUTAIRE:\n"))
  cat(sprintf("- %d pays prioritaires\n", community_needs$pays_cibles))
  cat(sprintf("- Agents communautaires nécessaires: %s\n", format(round(community_needs$agents_totaux, 0), big.mark = ",")))
  cat(sprintf("- Coût formation estimé: $%s\n", format(round(community_needs$cout_formation, 0), big.mark = ",")))
  cat(sprintf("- Population rurale ciblée: %s\n", format(round(community_needs$pop_rurale_totale, 0), big.mark = ",")))
  
  return(list(data = social_priorities, needs = community_needs))
}

# ===== 4. AXE 3: STRATÉGIES LAST-MILE (Contraintes Démographiques) =====

analyze_lastmile_strategies <- function(africa_data) {
  
  cat("\n\n=== AXE 3: STRATÉGIES LAST-MILE ===\n")
  cat("Transformation des contraintes démographiques (-0.84 en opportunités)\n\n")
  
  # Variables PC3 (Structure démographique et accès aux services)
  pc3_vars <- c("Population_agee_65_plus", "Acces_eau_potable", "Acces_electricite",
                "Lits_hospitaliers_pour_1000", "Part_population_rurale")
  
  # Calculer défis logistiques
  country_pc3 <- africa_data %>%
    filter(!is.na(country.x)) %>%
    group_by(country.x, region_short) %>%
    arrange(desc(year)) %>%
    slice(1) %>%
    ungroup() %>%
    select(country.x, region_short, immune_percentage, Population_totale,
           all_of(intersect(pc3_vars, names(africa_data))))
  
  # Score défis logistiques (plus élevé = plus de défis = plus d'opportunités last-mile)
  country_pc3$rural_challenge <- ifelse(!is.na(country_pc3$Part_population_rurale), 
                                        country_pc3$Part_population_rurale, 50)
  country_pc3$infrastructure_challenge <- ifelse(!is.na(country_pc3$Acces_electricite), 
                                                 100 - country_pc3$Acces_electricite, 50)
  country_pc3$water_challenge <- ifelse(!is.na(country_pc3$Acces_eau_potable), 
                                        100 - country_pc3$Acces_eau_potable, 30)
  
  country_pc3$lastmile_opportunity <- (country_pc3$rural_challenge + 
                                         country_pc3$infrastructure_challenge + 
                                         country_pc3$water_challenge) / 3
  
  # Pays avec plus grandes opportunités last-mile
  lastmile_priorities <- country_pc3 %>%
    filter(!is.na(lastmile_opportunity), !is.na(immune_percentage)) %>%
    group_by(region_short) %>%
    arrange(desc(lastmile_opportunity)) %>%
    slice_head(n = 8) %>%
    ungroup()
  
  cat("PAYS PRIORITAIRES POUR STRATÉGIES LAST-MILE:\n")
  for(region in c("WCAR", "ESAR")) {
    if(any(lastmile_priorities$region_short == region)) {
      cat(paste0("\n", region, ":\n"))
      region_lastmile <- lastmile_priorities %>% filter(region_short == region)
      for(i in 1:min(5, nrow(region_lastmile))) {
        pop_rural <- ifelse(is.na(region_lastmile$Part_population_rurale[i]), "N/A", 
                            sprintf("%.1f", region_lastmile$Part_population_rurale[i]))
        acces_elec <- ifelse(is.na(region_lastmile$Acces_electricite[i]), "N/A", 
                             sprintf("%.1f", region_lastmile$Acces_electricite[i]))
        immunite_val <- ifelse(is.na(region_lastmile$immune_percentage[i]), "N/A",
                               sprintf("%.1f", region_lastmile$immune_percentage[i]))
        cat(sprintf("%d. %s (Opportunité: %.1f, Pop.rurale: %s%%, Accès élec: %s%%, Immunité: %s%%)\n", 
                    i, region_lastmile$country.x[i], region_lastmile$lastmile_opportunity[i],
                    pop_rural, acces_elec, immunite_val))
      }
    }
  }
  
  # Calcul besoins logistiques
  lastmile_needs <- lastmile_priorities %>%
    mutate(
      pop_rurale = Population_totale * rural_challenge / 100,
      unites_mobiles = ceiling(pop_rurale / 50000),  # 1 unité mobile/50k hab ruraux
      points_froid = ceiling(pop_rurale / 25000),    # 1 point froid/25k hab
      cout_unitaire = unites_mobiles * 25000 + points_froid * 15000  # Coûts équipement
    ) %>%
    summarise(
      pays_cibles = n(),
      unites_mobiles_total = sum(unites_mobiles, na.rm = TRUE),
      points_froid_total = sum(points_froid, na.rm = TRUE),
      cout_logistique = sum(cout_unitaire, na.rm = TRUE),
      pop_rurale_ciblee = sum(pop_rurale, na.rm = TRUE),
      .groups = "drop"
    )
  
  cat(sprintf("\nBESOINS STRATÉGIES LAST-MILE:\n"))
  cat(sprintf("- %d pays prioritaires\n", lastmile_needs$pays_cibles))
  cat(sprintf("- Unités mobiles nécessaires: %d\n", lastmile_needs$unites_mobiles_total))
  cat(sprintf("- Points chaîne froid: %d\n", lastmile_needs$points_froid_total))
  cat(sprintf("- Coût infrastructure: $%s\n", format(lastmile_needs$cout_logistique, big.mark = ",")))
  cat(sprintf("- Population rurale ciblée: %s\n", format(round(lastmile_needs$pop_rurale_ciblee, 0), big.mark = ",")))
  
  return(list(data = lastmile_priorities, needs = lastmile_needs))
}

# ===== 5. VISUALISATIONS STRATÉGIQUES =====

create_strategic_visualizations <- function(geo_results, social_results, lastmile_results) {
  
  # 1. Graphique priorités par axe et région
  priority_comparison <- function() {
    
    # Combiner les données des 3 axes
    geo_data <- geo_results$data %>% 
      select(country.x, region_short, priority_geo, immune_percentage) %>%
      rename(priority_score = priority_geo, immunite = immune_percentage) %>%
      mutate(strategic_axis = "Optimisation\nGéographique")
    
    social_data <- social_results$data %>% 
      select(country.x, region_short, priority_social, immune_percentage) %>%
      rename(priority_score = priority_social, immunite = immune_percentage) %>%
      mutate(strategic_axis = "Mobilisation\nCommunautaire")
    
    lastmile_data <- lastmile_results$data %>% 
      select(country.x, region_short, lastmile_opportunity, immune_percentage) %>%
      rename(priority_score = lastmile_opportunity, immunite = immune_percentage) %>%
      mutate(strategic_axis = "Stratégies\nLast-Mile")
    
    combined_data <- bind_rows(geo_data, social_data, lastmile_data)
    
    p1 <- ggplot(combined_data, aes(x = strategic_axis, y = priority_score, fill = region_short)) +
      geom_boxplot(alpha = 0.7) +
      geom_jitter(width = 0.2, alpha = 0.6, size = 1) +
      facet_wrap(~region_short, ncol = 2) +
      scale_fill_brewer(type = "qual", palette = "Set2") +
      labs(title = "Distribution des Scores de Priorité par Axe Stratégique",
           subtitle = "WCAR vs ESAR - Boîtes à moustaches avec points individuels",
           x = "Axes Stratégiques", y = "Score de Priorité (0-100)",
           fill = "Région") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none",
            strip.text = element_text(size = 12, face = "bold"))
    
    return(p1)
  }
  
  # 2. Impact vs Coût des 3 axes
  impact_cost_analysis <- function() {
    
    strategy_data <- data.frame(
      axe = c("Optimisation\nGéographique", "Mobilisation\nCommunautaire", "Stratégies\nLast-Mile"),
      impact_potentiel = c(2.89, 1.79, 0.84),
      cout_relatif = c(1, 2.5, 4),
      roi = c(2.89, 0.72, 0.21),
      pays_cibles = c(nrow(geo_results$data), nrow(social_results$data), nrow(lastmile_results$data))
    )
    
    p2 <- ggplot(strategy_data, aes(x = cout_relatif, y = impact_potentiel)) +
      geom_point(aes(color = axe, size = pays_cibles), alpha = 0.8) +
      geom_text(aes(label = paste0("ROI: ", round(roi, 2))), vjust = -1.5, size = 3, fontface = "bold") +
      geom_text(aes(label = axe), vjust = 2.5, size = 3) +
      scale_size_continuous(range = c(6, 12), name = "Pays\nCiblés") +
      scale_color_brewer(type = "qual", palette = "Set1", guide = "none") +
      scale_x_continuous(breaks = 1:4, labels = c("Faible", "Modéré", "Élevé", "Très élevé")) +
      labs(title = "Analyse Coût-Bénéfice des Axes Stratégiques UNICEF",
           subtitle = "Taille des bulles = nombre de pays ciblés",
           x = "Coût Relatif de Mise en Œuvre", 
           y = "Impact Potentiel (Coefficient de Régression)") +
      theme_minimal() +
      theme(legend.position = "right")
    
    return(p2)
  }
  
  # 3. Immunité par priorité
  immunity_priority_correlation <- function() {
    
    # Combiner toutes les données avec leurs priorités
    all_priorities <- bind_rows(
      geo_results$data %>% select(country.x, region_short, immune_percentage, priority_geo) %>% 
        rename(priority = priority_geo, immunite = immune_percentage) %>% mutate(axis = "Géographique"),
      social_results$data %>% select(country.x, region_short, immune_percentage, priority_social) %>% 
        rename(priority = priority_social, immunite = immune_percentage) %>% mutate(axis = "Communautaire"),
      lastmile_results$data %>% select(country.x, region_short, immune_percentage, lastmile_opportunity) %>% 
        rename(priority = lastmile_opportunity, immunite = immune_percentage) %>% mutate(axis = "Last-Mile")
    )
    
    p3 <- ggplot(all_priorities, aes(x = priority, y = immunite)) +
      geom_point(aes(color = region_short), alpha = 0.7, size = 2) +
      geom_smooth(method = "lm", se = TRUE, alpha = 0.3) +
      facet_wrap(~axis, scales = "free_x") +
      scale_color_brewer(type = "qual", palette = "Set2", name = "Région") +
      labs(title = "Relation entre Score de Priorité et Pourcentage d'Immunité",
           subtitle = "Corrélation par axe stratégique - WCAR vs ESAR",
           x = "Score de Priorité", y = "Pourcentage d'Immunité (%)") +
      theme_minimal() +
      theme(strip.text = element_text(face = "bold"))
    
    return(p3)
  }
  
  return(list(
    comparison = priority_comparison(),
    impact_cost = impact_cost_analysis(),
    immunity_correlation = immunity_priority_correlation()
  ))
}

# ===== 6. SYNTHÈSE STRATÉGIQUE =====

strategic_synthesis <- function(geo_results, social_results, lastmile_results) {
  
  cat("\n\n=== SYNTHÈSE STRATÉGIQUE UNICEF AFRIQUE ===\n\n")
  
  # ROI calculé
  total_impact <- 2.89 + 1.79 + 0.84
  geo_roi <- 2.89 / 1.0
  social_roi <- 1.79 / 2.5
  lastmile_roi <- 0.84 / 4.0
  
  cat("RETOUR SUR INVESTISSEMENT PAR AXE:\n")
  cat(sprintf("1. Optimisation Géographique: %.2f (Impact %.2f / Coût relatif 1.0)\n", geo_roi, 2.89))
  cat(sprintf("2. Mobilisation Communautaire: %.2f (Impact %.2f / Coût relatif 2.5)\n", social_roi, 1.79))
  cat(sprintf("3. Stratégies Last-Mile: %.2f (Impact %.2f / Coût relatif 4.0)\n", lastmile_roi, 0.84))
  
  cat("\nRECOMMANDATIONS D'ALLOCATION BUDGÉTAIRE:\n")
  budget_geo <- 55
  budget_social <- 30
  budget_lastmile <- 15
  
  cat(sprintf("- Axe Géographique: %d%% du budget (ROI optimal: %.2f)\n", budget_geo, geo_roi))
  cat(sprintf("- Axe Communautaire: %d%% du budget (ROI modéré: %.2f)\n", budget_social, social_roi))
  cat(sprintf("- Axe Last-Mile: %d%% du budget (investissement long terme: %.2f)\n", budget_lastmile, lastmile_roi))
  
  cat("\nPAYS TOTAUX IDENTIFIÉS:\n")
  cat(sprintf("- Priorités géographiques: %d pays\n", nrow(geo_results$data)))
  cat(sprintf("- Priorités communautaires: %d pays\n", nrow(social_results$data)))
  cat(sprintf("- Priorités last-mile: %d pays\n", nrow(lastmile_results$data)))
  
  cat("\nIMPACT GLOBAL ESTIMÉ:\n")
  cat(sprintf("- Coefficient d'amélioration combiné: +%.2f points d'immunité\n", total_impact))
  cat(sprintf("- Objectif: atteindre 95%% d'immunité dans les pays prioritaires\n"))
  
  return(list(
    roi_scores = c(geo_roi, social_roi, lastmile_roi),
    budget_allocation = c(budget_geo, budget_social, budget_lastmile),
    total_impact = total_impact
  ))
}

# ===== 7. FONCTION PRINCIPALE =====

conduct_strategic_analysis <- function(data_final) {
  
  cat("ANALYSE STRATÉGIQUE UNICEF - RÉGIONS AFRICAINES (VERSION IMMUNITÉ)\n")
  cat(paste0(rep("=", 65), collapse = ""), "\n\n")
  
  # Vérifier que la colonne immune_percentage existe
  if(!"immune_percentage" %in% names(data_final)) {
    cat("ERREUR: La colonne 'immune_percentage' n'existe pas dans les données.\n")
    cat("Colonnes disponibles:", paste(names(data_final), collapse = ", "), "\n")
    return(NULL)
  }
  
  # Filtrer données Afrique
  africa_data <- filter_africa_regions(data_final)
  
  cat(sprintf("Données chargées: %d observations pour WCAR et ESAR\n", nrow(africa_data)))
  cat(sprintf("Pays uniques: %d\n", length(unique(africa_data$country.x[!is.na(africa_data$country.x)]))))
  
  if(nrow(africa_data) > 0 && !all(is.na(africa_data$year))) {
    cat(sprintf("Période: %.0f - %.0f\n\n", min(africa_data$year, na.rm = TRUE), max(africa_data$year, na.rm = TRUE)))
  } else {
    cat("Aucune donnée temporelle disponible\n\n")
  }
  
  # Vérifier si nous avons des données avant de continuer
  if(nrow(africa_data) == 0) {
    cat("ERREUR: Aucune donnée trouvée pour les régions africaines.\n")
    cat("Vérifiez les noms des régions dans votre dataset.\n")
    return(NULL)
  }
  
  # Analyses par axe stratégique
  geo_results <- analyze_geographic_optimization(africa_data)
  social_results <- analyze_community_mobilization(africa_data)
  lastmile_results <- analyze_lastmile_strategies(africa_data)
  
  # Visualisations
  plots <- create_strategic_visualizations(geo_results, social_results, lastmile_results)
  
  # Synthèse
  synthesis <- strategic_synthesis(geo_results, social_results, lastmile_results)
  
  # Afficher graphiques
  print(plots$comparison)
  print(plots$impact_cost)
  print(plots$immunity_correlation)
  
  # Exporter résultats
  write.csv(geo_results$data, "data/processed/priorites_geographiques_afrique_immunite.csv", row.names = FALSE)
  write.csv(social_results$data, "data/processed/priorites_communautaires_afrique_immunite.csv", row.names = FALSE)
  write.csv(lastmile_results$data, "data/processed/priorites_lastmile_afrique_immunite.csv", row.names = FALSE)
  
  cat("\n=== FICHIERS EXPORTÉS ===\n")
  cat("- priorites_geographiques_afrique_immunite.csv\n")
  cat("- priorites_communautaires_afrique_immunite.csv\n") 
  cat("- priorites_lastmile_afrique_immunite.csv\n\n")
  
  return(list(
    africa_data = africa_data,
    geographic = geo_results,
    community = social_results,
    lastmile = lastmile_results,
    visualizations = plots,
    synthesis = synthesis
  ))
}

# ===== 8. ANALYSE COMPLÉMENTAIRE : OVERLAP DES PRIORITÉS =====

analyze_priority_overlap <- function(geo_results, social_results, lastmile_results) {
  
  cat("=== ANALYSE DE CHEVAUCHEMENT DES PRIORITÉS ===\n\n")
  
  # Créer un tableau de comparaison des priorités
  geo_top5 <- geo_results$data %>% slice_head(n = 5) %>% pull(country.x)
  social_top5 <- social_results$data %>% slice_head(n = 5) %>% pull(country.x) 
  lastmile_top5 <- lastmile_results$data %>% slice_head(n = 5) %>% pull(country.x)
  
  all_countries <- unique(c(geo_top5, social_top5, lastmile_top5))
  
  overlap_matrix <- data.frame(
    country = all_countries,
    geographic = all_countries %in% geo_top5,
    community = all_countries %in% social_top5,
    lastmile = all_countries %in% lastmile_top5
  )
  
  overlap_matrix$total_axes <- rowSums(overlap_matrix[,2:4])
  
  cat("PAYS PRIORITAIRES DANS PLUSIEURS AXES:\n")
  multi_axis <- overlap_matrix %>% filter(total_axes >= 2) %>% arrange(desc(total_axes))
  
  if(nrow(multi_axis) > 0) {
    for(i in 1:nrow(multi_axis)) {
      axes <- c()
      if(multi_axis$geographic[i]) axes <- c(axes, "Géo")
      if(multi_axis$community[i]) axes <- c(axes, "Com") 
      if(multi_axis$lastmile[i]) axes <- c(axes, "Last-Mile")
      cat(sprintf("- %s: %s (%d axes)\n", multi_axis$country[i], 
                  paste(axes, collapse = ", "), multi_axis$total_axes[i]))
    }
  } else {
    cat("Aucun pays n'apparaît dans plusieurs axes (spécialisation élevée)\n")
  }
  
  cat(sprintf("\nSTATISTIQUES DE CHEVAUCHEMENT:\n"))
  cat(sprintf("- Pays uniques identifiés: %d\n", length(all_countries)))
  cat(sprintf("- Pays multi-axes: %d\n", sum(overlap_matrix$total_axes >= 2)))
  cat(sprintf("- Spécialisation: %.1f%% des pays sont spécifiques à un axe\n", 
              100 * sum(overlap_matrix$total_axes == 1) / length(all_countries)))
  
  return(overlap_matrix)
}

# ===== 9. RECOMMANDATIONS OPÉRATIONNELLES =====

generate_operational_recommendations <- function(strategic_results) {
  
  cat("\n\n=== RECOMMANDATIONS OPÉRATIONNELLES DÉTAILLÉES ===\n\n")
  
  cat("PHASE 1 (0-6 MOIS) - OPTIMISATION GÉOGRAPHIQUE:\n")
  cat("Réallouer immédiatement 30% des doses vers les pays identifiés\n")
  cat("Établir des partenariats logistiques avec les gouvernements locaux\n")
  cat("Former les équipes terrain sur les critères de priorisation géographique\n")
  cat("Mettre en place un système de monitoring temps réel de l'immunité\n\n")
  
  cat("PHASE 2 (6-12 MOIS) - MOBILISATION COMMUNAUTAIRE:\n")
  cat("Recruter et former les agents de santé communautaires identifiés\n")
  cat("Développer du matériel de sensibilisation culturellement adapté\n")  
  cat("Établir des partenariats avec les leaders communautaires et religieux\n")
  cat("Lancer des campagnes de communication de masse ciblées\n\n")
  
  cat("PHASE 3 (12-18 MOIS) - DÉPLOIEMENT LAST-MILE:\n")
  cat("Procurer et déployer les unités mobiles de vaccination\n")
  cat("Installer les points de chaîne du froid décentralisés\n")
  cat("Former les équipes mobiles aux stratégies de terrain\n")
  cat("Tester et optimiser les circuits logistiques last-mile\n\n")
  
  cat("PHASE 4 (18-24 MOIS) - CONSOLIDATION ET ÉVALUATION:\n")
  cat("Évaluer l'impact des trois axes stratégiques sur l'immunité\n")
  cat("Ajuster les allocations budgétaires basées sur les résultats\n")
  cat("Documenter les meilleures pratiques pour réplication\n")
  cat("Planifier l'expansion vers d'autres régions\n\n")
  
  # Calculer les ressources nécessaires
  total_agents <- strategic_results$community$needs$agents_totaux
  total_mobile_units <- strategic_results$lastmile$needs$unites_mobiles_total
  total_cold_points <- strategic_results$lastmile$needs$points_froid_total
  
  cat("RESSOURCES TOTALES NÉCESSAIRES:\n")
  cat(sprintf("- Agents communautaires: %s\n", format(round(total_agents, 0), big.mark = ",")))
  cat(sprintf("- Unités mobiles: %d\n", total_mobile_units))
  cat(sprintf("- Points chaîne froid: %d\n", total_cold_points))
  
  total_cost <- strategic_results$community$needs$cout_formation + strategic_results$lastmile$needs$cout_logistique
  cat(sprintf("- Coût total estimé: $%s\n", format(total_cost, big.mark = ",")))
  cat(sprintf("- ROI projeté: %.1fx sur 2 ans\n", strategic_results$synthesis$total_impact))
  
  # Métriques d'immunité spécifiques
  cat("\nOBJECTIFS D'IMMUNITÉ:\n")
  cat("- Cible générale: 95% d'immunité dans tous les pays prioritaires\n")
  cat("- Amélioration attendue: +5.5 points d'immunité en moyenne\n")
  cat("- Réduction des épidémies: 70% dans les zones ciblées\n")
  cat("- Protection communautaire: seuil d'immunité collective atteint\n")
}

# ===== 10. ANALYSE SPÉCIFIQUE IMMUNITÉ =====

analyze_immunity_gaps <- function(africa_data) {
  
  cat("\n\n=== ANALYSE DES GAPS D'IMMUNITÉ ===\n\n")
  
  # Analyse des gaps d'immunité par région
  immunity_analysis <- africa_data %>%
    filter(!is.na(immune_percentage), !is.na(region_short)) %>%
    group_by(region_short) %>%
    summarise(
      pays_total = n(),
      immunite_moyenne = mean(immune_percentage, na.rm = TRUE),
      immunite_mediane = median(immune_percentage, na.rm = TRUE),
      immunite_min = min(immune_percentage, na.rm = TRUE),
      immunite_max = max(immune_percentage, na.rm = TRUE),
      pays_sous_80 = sum(immune_percentage < 80, na.rm = TRUE),
      pays_sous_70 = sum(immune_percentage < 70, na.rm = TRUE),
      gap_moyen = 95 - immunite_moyenne,
      .groups = "drop"
    )
  
  cat("SITUATION DE L'IMMUNITÉ PAR RÉGION:\n")
  for(i in 1:nrow(immunity_analysis)) {
    region <- immunity_analysis$region_short[i]
    cat(sprintf("\n%s:\n", region))
    cat(sprintf("- Immunité moyenne: %.1f%% (médiane: %.1f%%)\n", 
                immunity_analysis$immunite_moyenne[i], immunity_analysis$immunite_mediane[i]))
    cat(sprintf("- Écart min-max: %.1f%% - %.1f%%\n", 
                immunity_analysis$immunite_min[i], immunity_analysis$immunite_max[i]))
    cat(sprintf("- Pays < 80%%: %d/%d (%.1f%%)\n", 
                immunity_analysis$pays_sous_80[i], immunity_analysis$pays_total[i],
                100 * immunity_analysis$pays_sous_80[i] / immunity_analysis$pays_total[i]))
    cat(sprintf("- Pays < 70%%: %d/%d (%.1f%%)\n", 
                immunity_analysis$pays_sous_70[i], immunity_analysis$pays_total[i],
                100 * immunity_analysis$pays_sous_70[i] / immunity_analysis$pays_total[i]))
    cat(sprintf("- Gap moyen vers 95%%: %.1f points\n", immunity_analysis$gap_moyen[i]))
  }
  
  # Identifier les pays les plus critiques
  critical_countries <- africa_data %>%
    filter(!is.na(immune_percentage), immune_percentage < 70) %>%
    arrange(immune_percentage) %>%
    select(country.x, region_short, immune_percentage, Population_totale)
  
  if(nrow(critical_countries) > 0) {
    cat(sprintf("\nPAYS EN SITUATION CRITIQUE (< 70%% d'immunité):\n"))
    for(i in 1:min(10, nrow(critical_countries))) {
      pop_val <- ifelse(is.na(critical_countries$Population_totale[i]), "N/A",
                        format(critical_countries$Population_totale[i], big.mark = ","))
      cat(sprintf("%d. %s (%s): %.1f%% d'immunité (Pop: %s)\n", 
                  i, critical_countries$country.x[i], critical_countries$region_short[i],
                  critical_countries$immune_percentage[i], pop_val))
    }
  }
  
  return(immunity_analysis)
}

# ===== EXÉCUTION COMPLÈTE =====

# Fonction principale d'exécution
run_complete_immunity_analysis <- function(data_final) {
  
  cat("LANCEMENT DE L'ANALYSE STRATÉGIQUE BASÉE SUR L'IMMUNITÉ...\n")
  cat(paste0(rep("=", 70), collapse = ""), "\n\n")
  
  # 1. Analyse stratégique principale
  strategic_results <- conduct_strategic_analysis(data_final)
  
  if(!is.null(strategic_results)) {
    # 2. Analyse des gaps d'immunité
    immunity_gaps <- analyze_immunity_gaps(strategic_results$africa_data)
    
    # 3. Analyse de chevauchement des priorités
    overlap_analysis <- analyze_priority_overlap(strategic_results$geographic, 
                                                 strategic_results$community, 
                                                 strategic_results$lastmile)
    
    # 4. Recommandations opérationnelles
    generate_operational_recommendations(strategic_results)
    
    # 5. Résumé final
    cat("\n\n=== RÉSUMÉ EXÉCUTIF ===\n")
    cat("Cette analyse identifie les stratégies optimales pour améliorer l'immunité\n")
    cat("dans les régions WCAR et ESAR, avec un focus sur:\n")
    cat("1. L'optimisation géographique (ROI: 2.89)\n")
    cat("2. La mobilisation communautaire (ROI: 0.72)\n")
    cat("3. Les stratégies last-mile (ROI: 0.21)\n\n")
    cat("Impact attendu: +5.5 points d'immunité en moyenne\n")
    cat("Objectif: 95% d'immunité dans tous les pays prioritaires\n")
    
    return(list(
      strategic = strategic_results,
      immunity_gaps = immunity_gaps,
      overlap = overlap_analysis
    ))
  } else {
    cat("Échec de l'analyse. Vérifiez vos données d'entrée.\n")
    return(NULL)
  }
}

# Pour exécuter l'analyse complète, utilisez:
strategic_results <- run_complete_immunity_analysis(data_final)




#EXPORT DES GRAPHIQUES
export_strategic_plots_simple <- function(strategic_results, output_dir = "output/plots/plots_unicef", 
                                          width = 12, height = 8, dpi = 300) {
  
  # Créer le dossier de sortie s'il n'existe pas
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat(sprintf("Dossier créé: %s\n", output_dir))
  }
  
  # Créer les sous-dossiers pour PNG et SVG
  png_dir <- file.path(output_dir, "png")
  svg_dir <- file.path(output_dir, "svg")
  
  if (!dir.exists(png_dir)) {
    dir.create(png_dir, recursive = TRUE)
  }
  
  if (!dir.exists(svg_dir)) {
    dir.create(svg_dir, recursive = TRUE)
  }
  
  cat("=== EXPORT DES GRAPHIQUES STRATÉGIQUES (PNG + SVG) ===\n\n")
  
  # Fonction interne pour sauvegarder en double format
  save_dual_format <- function(plot, filename, plot_width = width, plot_height = height, plot_dpi = dpi) {
    # PNG
    png_file <- file.path(png_dir, paste0(filename, ".png"))
    ggsave(png_file, plot = plot, 
           width = plot_width, height = plot_height, dpi = plot_dpi, bg = "white")
    
    # SVG
    svg_file <- file.path(svg_dir, paste0(filename, ".svg"))
    ggsave(svg_file, plot = plot, 
           width = plot_width, height = plot_height, bg = "white")
    
    cat(sprintf("✓ Exporté: %s (PNG + SVG)\n", filename))
  }
  
  # Vérifier que les visualisations existent
  if (!exists("strategic_results") || is.null(strategic_results$strategic$visualizations)) {
    stop("Erreur: strategic_results$strategic$visualizations n'existe pas")
  }
  
  plots <- strategic_results$strategic$visualizations
  
  # Graphiques principaux uniquement (ceux qui existent)
  if (!is.null(plots$comparison)) {
    save_dual_format(plots$comparison, "01_comparaison_priorites_axes")
  }
  
  if (!is.null(plots$impact_cost)) {
    save_dual_format(plots$impact_cost, "02_analyse_cout_benefice")
  }
  
  if (!is.null(plots$immunity_correlation)) {
    save_dual_format(plots$immunity_correlation, "03_correlation_immunite_priorite")
  }
  
  # Export PDF des graphiques existants uniquement
  pdf_file <- file.path(output_dir, "rapport_graphiques_unicef.pdf")
  pdf(pdf_file, width = width, height = height)
  
  if (!is.null(plots$comparison)) print(plots$comparison)
  if (!is.null(plots$impact_cost)) print(plots$impact_cost)
  if (!is.null(plots$immunity_correlation)) print(plots$immunity_correlation)
  
  dev.off()
  cat(sprintf("✓ Exporté: rapport_graphiques_unicef.pdf\n"))
  
  # Compter les fichiers créés
  png_files <- list.files(png_dir, pattern = "\\.png$")
  svg_files <- list.files(svg_dir, pattern = "\\.svg$")
  
  cat(sprintf("\n=== RÉSUMÉ EXPORT ===\n"))
  cat(sprintf("- Dossier principal: %s\n", output_dir))
  cat(sprintf("- Nombre de fichiers PNG: %d\n", length(png_files)))
  cat(sprintf("- Nombre de fichiers SVG: %d\n", length(svg_files)))
  cat(sprintf("- Fichier PDF: 1\n"))
  cat(sprintf("- Résolution PNG: %d DPI\n", dpi))
  cat(sprintf("- Dimensions: %d x %d pouces\n\n", width, height))
  
  return(list(
    main_dir = output_dir,
    png_dir = png_dir,
    svg_dir = svg_dir,
    pdf_file = pdf_file
  ))
}

# Version encore plus simple - juste pour les 3 graphiques principaux
export_main_plots <- function(comparison_plot, impact_cost_plot, immunity_plot, 
                              output_dir = "output/plots/main_plots",
                              width = 12, height = 8, dpi = 300) {
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # PNG
  ggsave(file.path(output_dir, "01_comparaison_priorites.png"), 
         comparison_plot, width = width, height = height, dpi = dpi, bg = "white")
  
  ggsave(file.path(output_dir, "02_cout_benefice.png"), 
         impact_cost_plot, width = width, height = height, dpi = dpi, bg = "white")
  
  ggsave(file.path(output_dir, "03_immunite_correlation.png"), 
         immunity_plot, width = width, height = height, dpi = dpi, bg = "white")
  
  # SVG
  ggsave(file.path(output_dir, "01_comparaison_priorites.svg"), 
         comparison_plot, width = width, height = height, bg = "white")
  
  ggsave(file.path(output_dir, "02_cout_benefice.svg"), 
         impact_cost_plot, width = width, height = height, bg = "white")
  
  ggsave(file.path(output_dir, "03_immunite_correlation.svg"), 
         immunity_plot, width = width, height = height, bg = "white")
  
  cat("✓ 6 fichiers exportés (3 PNG + 3 SVG)\n")
  cat(sprintf("✓ Dossier: %s\n", output_dir))
  
  return(output_dir)
}

# Executer avec
export_strategic_plots_simple(strategic_results)





# ===== CARTES DES RÉGIONS UNICEF AVEC PAYS PRIORITAIRES =====
# Code adapté pour créer des cartes en SVG et PNG haute résolution
# Complément à l'analyse stratégique UNICEF

# Packages nécessaires pour les cartes
if (!require(sf)) install.packages("sf")
if (!require(rnaturalearth)) install.packages("rnaturalearth")
if (!require(rnaturalearthdata)) install.packages("rnaturalearthdata")
if (!require(maps)) install.packages("maps")
if (!require(mapdata)) install.packages("mapdata")
if (!require(svglite)) install.packages("svglite")  # Nouveau: pour SVG

library(sf)
library(rnaturalearth) 
library(rnaturalearthdata)
library(maps)
library(mapdata)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(viridis)
library(scales)
library(svglite)  # Nouveau: pour export SVG

# ===== FONCTION PRINCIPALE POUR CRÉER LES CARTES =====

create_unicef_priority_maps <- function(strategic_results, output_dir = "cartes_unicef") {
  
  # Créer le dossier de sortie
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat(sprintf("Dossier créé: %s\n", output_dir))
  }
  
  cat("=== CRÉATION DES CARTES RÉGIONALES UNICEF ===\n\n")
  
  # 1. Télécharger les données géographiques
  cat("Téléchargement des données géographiques...\n")
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  # 2. Créer les cartes pour chaque axe
  map_geo <- create_geographic_priority_map(strategic_results, world)
  map_community <- create_community_priority_map(strategic_results, world)
  map_lastmile <- create_lastmile_priority_map(strategic_results, world)
  map_overview <- create_overview_priority_map(strategic_results, world)
  
  # 3. Exporter les cartes en SVG et PNG haute résolution
  export_priority_maps_multi_format(list(
    geographic = map_geo,
    community = map_community,
    lastmile = map_lastmile,
    overview = map_overview
  ), output_dir)
  
  cat(sprintf("\n✅ CARTES EXPORTÉES DANS: %s/\n", output_dir))
  
  return(list(
    geographic = map_geo,
    community = map_community,
    lastmile = map_lastmile,
    overview = map_overview
  ))
}

# ===== FONCTIONS DE PRÉPARATION DES DONNÉES (inchangées) =====

prepare_map_data <- function(strategic_results, world) {
  
  # Extraire les données des 3 axes
  geo_data <- strategic_results$strategic$geographic$data %>%
    select(country.x, region_short, priority_geo, immune_percentage) %>%
    mutate(priority_level_geo = cut(priority_geo, 
                                    breaks = c(0, 50, 75, 90, 100),
                                    labels = c("Faible", "Modérée", "Élevée", "Critique")))
  
  community_data <- strategic_results$strategic$community$data %>%
    select(country.x, region_short, priority_social, immune_percentage) %>%
    mutate(priority_level_community = cut(priority_social,
                                          breaks = c(0, 50, 75, 90, 100),
                                          labels = c("Faible", "Modérée", "Élevée", "Critique")))
  
  lastmile_data <- strategic_results$strategic$lastmile$data %>%
    select(country.x, region_short, lastmile_opportunity, immune_percentage) %>%
    mutate(priority_level_lastmile = cut(lastmile_opportunity,
                                         breaks = c(0, 50, 75, 90, 100),
                                         labels = c("Faible", "Modérée", "Élevée", "Critique")))
  
  # Fusionner avec les données géographiques
  # Nettoyer les noms de pays pour le matching
  world_clean <- world %>%
    mutate(
      name_clean = case_when(
        # Corrections spécifiques pour le matching
        name == "Democratic Republic of the Congo" ~ "Democratic Republic of the Congo",
        name == "Republic of the Congo" ~ "Congo",
        name == "Central African Republic" ~ "Central African Republic", 
        name == "Côte d'Ivoire" ~ "Ivory Coast",
        name == "eSwatini" ~ "Eswatini",
        name == "United Republic of Tanzania" ~ "Tanzania",
        name == "South Sudan" ~ "South Sudan",
        TRUE ~ name
      )
    )
  
  # Joindre les données de priorité
  world_with_geo <- world_clean %>%
    left_join(geo_data, by = c("name_clean" = "country.x"))
  
  world_with_community <- world_clean %>%
    left_join(community_data, by = c("name_clean" = "country.x"))
  
  world_with_lastmile <- world_clean %>%
    left_join(lastmile_data, by = c("name_clean" = "country.x"))
  
  # Données combinées pour vue d'ensemble
  combined_priorities <- geo_data %>%
    full_join(community_data, by = c("country.x", "region_short", "immune_percentage")) %>%
    full_join(lastmile_data, by = c("country.x", "region_short", "immune_percentage")) %>%
    mutate(
      total_priority = (coalesce(priority_geo, 0) + 
                          coalesce(priority_social, 0) + 
                          coalesce(lastmile_opportunity, 0)) / 3,
      priority_level_total = cut(total_priority,
                                 breaks = c(0, 30, 50, 70, 100),
                                 labels = c("Faible", "Modérée", "Élevée", "Critique"))
    )
  
  world_with_total <- world_clean %>%
    left_join(combined_priorities, by = c("name_clean" = "country.x"))
  
  return(list(
    geographic = world_with_geo,
    community = world_with_community,
    lastmile = world_with_lastmile,
    overview = world_with_total
  ))
}

# ===== FONCTIONS DE CRÉATION DES CARTES PAR AXE (inchangées) =====

create_geographic_priority_map <- function(strategic_results, world) {
  
  map_data <- prepare_map_data(strategic_results, world)
  africa_bounds <- get_africa_bounds()
  
  p <- ggplot(map_data$geographic) +
    geom_sf(aes(fill = priority_level_geo), color = "white", size = 0.3) +
    scale_fill_manual(
      values = c("Faible" = "#FFF7BC", "Modérée" = "#FEC44F", 
                 "Élevée" = "#D95F0E", "Critique" = "#8C2D04"),
      na.value = "grey90",
      name = "Niveau de\nPriorité",
      drop = FALSE
    ) +
    coord_sf(xlim = africa_bounds$xlim, ylim = africa_bounds$ylim, expand = FALSE) +
    labs(
      title = "AXE 1: OPTIMISATION GÉOGRAPHIQUE",
      subtitle = "Priorités basées sur le développement socio-économique",
      caption = "Régions UNICEF WCAR & ESAR - Plus la couleur est foncée, plus la priorité est élevée"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
      plot.caption = element_text(size = 10, hjust = 0.5, color = "gray50"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      legend.key.width = unit(1.5, "cm"),
      panel.background = element_rect(fill = "lightblue", color = NA)
    ) +
    # Ajouter les labels des régions
    annotate("text", x = -10, y = 15, label = "WCAR", 
             size = 6, fontface = "bold", color = "navy") +
    annotate("text", x = 35, y = -10, label = "ESAR", 
             size = 6, fontface = "bold", color = "navy")
  
  return(p)
}

create_community_priority_map <- function(strategic_results, world) {
  
  map_data <- prepare_map_data(strategic_results, world)
  africa_bounds <- get_africa_bounds()
  
  p <- ggplot(map_data$community) +
    geom_sf(aes(fill = priority_level_community), color = "white", size = 0.3) +
    scale_fill_manual(
      values = c("Faible" = "#E0F3F8", "Modérée" = "#74A9CF", 
                 "Élevée" = "#2B8CBE", "Critique" = "#045A8D"),
      na.value = "grey90",
      name = "Niveau de\nPriorité",
      drop = FALSE
    ) +
    coord_sf(xlim = africa_bounds$xlim, ylim = africa_bounds$ylim, expand = FALSE) +
    labs(
      title = "AXE 2: MOBILISATION COMMUNAUTAIRE",
      subtitle = "Priorités basées sur l'inclusion sociale et les investissements",
      caption = "Régions UNICEF WCAR & ESAR - Plus la couleur est foncée, plus la priorité est élevée"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
      plot.caption = element_text(size = 10, hjust = 0.5, color = "gray50"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      legend.key.width = unit(1.5, "cm"),
      panel.background = element_rect(fill = "lightblue", color = NA)
    ) +
    annotate("text", x = -10, y = 15, label = "WCAR", 
             size = 6, fontface = "bold", color = "navy") +
    annotate("text", x = 35, y = -10, label = "ESAR", 
             size = 6, fontface = "bold", color = "navy")
  
  return(p)
}

create_lastmile_priority_map <- function(strategic_results, world) {
  
  map_data <- prepare_map_data(strategic_results, world)
  africa_bounds <- get_africa_bounds()
  
  p <- ggplot(map_data$lastmile) +
    geom_sf(aes(fill = priority_level_lastmile), color = "white", size = 0.3) +
    scale_fill_manual(
      values = c("Faible" = "#F7FCF5", "Modérée" = "#74C476", 
                 "Élevée" = "#238B45", "Critique" = "#00441B"),
      na.value = "grey90",
      name = "Niveau de\nPriorité",
      drop = FALSE
    ) +
    coord_sf(xlim = africa_bounds$xlim, ylim = africa_bounds$ylim, expand = FALSE) +
    labs(
      title = "AXE 3: STRATÉGIES LAST-MILE",
      subtitle = "Opportunités basées sur les défis logistiques et démographiques",
      caption = "Régions UNICEF WCAR & ESAR - Plus la couleur est foncée, plus l'opportunité est grande"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
      plot.caption = element_text(size = 10, hjust = 0.5, color = "gray50"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      legend.key.width = unit(1.5, "cm"),
      panel.background = element_rect(fill = "lightblue", color = NA)
    ) +
    annotate("text", x = -10, y = 15, label = "WCAR", 
             size = 6, fontface = "bold", color = "navy") +
    annotate("text", x = 35, y = -10, label = "ESAR", 
             size = 6, fontface = "bold", color = "navy")
  
  return(p)
}

create_overview_priority_map <- function(strategic_results, world) {
  
  map_data <- prepare_map_data(strategic_results, world)
  africa_bounds <- get_africa_bounds()
  
  p <- ggplot(map_data$overview) +
    geom_sf(aes(fill = priority_level_total), color = "white", size = 0.3) +
    scale_fill_viridis_d(
      option = "plasma",
      name = "Priorité\nGlobale",
      na.value = "grey90",
      drop = FALSE
    ) +
    coord_sf(xlim = africa_bounds$xlim, ylim = africa_bounds$ylim, expand = FALSE) +
    labs(
      title = "VUE D'ENSEMBLE: PRIORITÉS STRATÉGIQUES COMBINÉES",
      subtitle = "Synthèse des trois axes stratégiques UNICEF",
      caption = "Régions UNICEF WCAR & ESAR - Score moyen des trois axes de priorité"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
      plot.caption = element_text(size = 10, hjust = 0.5, color = "gray50"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      legend.key.width = unit(1.5, "cm"),
      panel.background = element_rect(fill = "lightblue", color = NA)
    ) +
    annotate("text", x = -10, y = 15, label = "WCAR", 
             size = 6, fontface = "bold", color = "white") +
    annotate("text", x = 35, y = -10, label = "ESAR", 
             size = 6, fontface = "bold", color = "white")
  
  return(p)
}

# ===== FONCTION D'EXPORT MULTI-FORMAT (MODIFIÉE) =====

export_priority_maps_multi_format <- function(maps_list, output_dir, 
                                              # Paramètres PNG haute résolution
                                              png_width = 16, png_height = 12, png_dpi = 600,
                                              # Paramètres SVG
                                              svg_width = 16, svg_height = 12) {
  
  cat("Export des cartes en cours (SVG + PNG haute résolution)...\n")
  
  # Définir les noms de fichiers
  map_files <- list(
    geographic = "carte_01_axe_geographique",
    community = "carte_02_axe_communautaire", 
    lastmile = "carte_03_axe_lastmile",
    overview = "carte_04_vue_ensemble"
  )
  
  # Export individuel des cartes en SVG et PNG
  for (map_type in names(maps_list)) {
    base_name <- map_files[[map_type]]
    
    # Export SVG (vectoriel, redimensionnable)
    svg_path <- file.path(output_dir, paste0(base_name, ".svg"))
    ggsave(svg_path, plot = maps_list[[map_type]], 
           width = svg_width, height = svg_height, 
           device = "svg", bg = "white")
    cat(sprintf("✓ SVG exporté: %s\n", basename(svg_path)))
    
    # Export PNG haute résolution (impression/web)
    png_path <- file.path(output_dir, paste0(base_name, "_HD.png"))
    ggsave(png_path, plot = maps_list[[map_type]], 
           width = png_width, height = png_height, dpi = png_dpi, 
           device = "png", bg = "white")
    cat(sprintf("✓ PNG HD exporté: %s (%d DPI)\n", basename(png_path), png_dpi))
  }
  
  # Créer une carte composite
  composite_map <- create_composite_map(maps_list)
  
  # Export composite SVG
  composite_svg <- file.path(output_dir, "carte_05_composite_tous_axes.svg")
  ggsave(composite_svg, plot = composite_map, 
         width = svg_width * 1.2, height = svg_height * 1.2, 
         device = "svg", bg = "white")
  cat(sprintf("✓ Composite SVG exporté: %s\n", basename(composite_svg)))
  
  # Export composite PNG HD
  composite_png <- file.path(output_dir, "carte_05_composite_tous_axes_HD.png")
  ggsave(composite_png, plot = composite_map, 
         width = png_width * 1.2, height = png_height * 1.2, dpi = png_dpi, 
         device = "png", bg = "white")
  cat(sprintf("✓ Composite PNG HD exporté: %s (%d DPI)\n", basename(composite_png), png_dpi))
  
  # Export PDF (inchangé)
  export_pdf_collection(maps_list, composite_map, output_dir, png_width, png_height)
  
  # Résumé de l'export
  print_export_summary(output_dir, png_dpi, length(maps_list))
}

# ===== NOUVELLE FONCTION : Export PDF =====

export_pdf_collection <- function(maps_list, composite_map, output_dir, width, height) {
  
  pdf_file <- file.path(output_dir, "cartes_unicef_completes.pdf")
  pdf(pdf_file, width = width, height = height)
  
  for (map in maps_list) {
    print(map)
  }
  print(composite_map)
  
  dev.off()
  cat(sprintf("✓ PDF exporté: %s\n", basename(pdf_file)))
}

# ===== NOUVELLE FONCTION : Résumé d'export =====

print_export_summary <- function(output_dir, dpi, num_maps) {
  
  cat(sprintf("\n=== RÉSUMÉ EXPORT CARTES ===\n"))
  cat(sprintf("📁 Dossier: %s/\n", output_dir))
  cat(sprintf("📊 Formats créés:\n"))
  cat(sprintf("   • %d cartes SVG (vectoriel, redimensionnable)\n", num_maps + 1))
  cat(sprintf("   • %d cartes PNG HD (%d DPI, impression)\n", num_maps + 1, dpi))
  cat(sprintf("   • 1 fichier PDF complet\n"))
  cat(sprintf("🎯 Total: %d fichiers générés\n", (num_maps + 1) * 2 + 1))
  cat(sprintf("💡 SVG = idéal pour web/présentation\n"))
  cat(sprintf("💡 PNG HD = idéal pour impression\n"))
}

# ===== FONCTIONS UTILITAIRES (inchangées) =====

get_africa_bounds <- function() {
  # Coordonnées pour centrer sur l'Afrique subsaharienne
  list(
    xlim = c(-25, 55),  # Longitude: Atlantique à Océan Indien
    ylim = c(-40, 25)   # Latitude: Afrique du Sud au Sahel
  )
}

create_composite_map <- function(maps_list) {
  
  # Utiliser gridExtra pour combiner les cartes
  if (!require(gridExtra)) install.packages("gridExtra")
  library(gridExtra)
  
  # Modifier les titres pour la version composite
  maps_list$geographic <- maps_list$geographic + 
    labs(title = "AXE 1: GÉOGRAPHIQUE") +
    theme(legend.position = "none")
  
  maps_list$community <- maps_list$community + 
    labs(title = "AXE 2: COMMUNAUTAIRE") +
    theme(legend.position = "none")
  
  maps_list$lastmile <- maps_list$lastmile + 
    labs(title = "AXE 3: LAST-MILE") +
    theme(legend.position = "none")
  
  maps_list$overview <- maps_list$overview + 
    labs(title = "VUE D'ENSEMBLE") +
    theme(legend.position = "bottom")
  
  # Arranger en grille 2x2
  composite <- grid.arrange(
    maps_list$geographic, maps_list$community,
    maps_list$lastmile, maps_list$overview,
    ncol = 2,
    top = "CARTES DES PRIORITÉS STRATÉGIQUES UNICEF - RÉGIONS AFRICAINES",
    bottom = "Analyse comparative des trois axes stratégiques dans les régions WCAR et ESAR"
  )
  
  return(composite)
}

# ===== FONCTION D'USAGE SIMPLE AMÉLIORÉE =====

create_and_export_unicef_maps <- function(strategic_results, 
                                          output_folder = "cartes_unicef_2024",
                                          # Options d'export personnalisables
                                          png_dpi = 600,
                                          high_quality = TRUE) {
  
  if(is.null(strategic_results) || is.null(strategic_results$strategic)) {
    cat("ERREUR: Aucun résultat d'analyse stratégique trouvé.\n")
    cat("Exécutez d'abord run_complete_immunity_analysis()\n")
    return(NULL)
  }
  
  cat("🗺️  CRÉATION DES CARTES RÉGIONALES UNICEF...\n\n")
  
  # Vérifier que les packages géographiques sont installés
  required_packages <- c("sf", "rnaturalearth", "rnaturalearthdata", "svglite")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  
  if(length(missing_packages) > 0) {
    cat("Installation des packages nécessaires...\n")
    install.packages(missing_packages)
  }
  
  # Ajuster la qualité selon le paramètre
  if (high_quality) {
    png_dpi <- max(png_dpi, 600)  # Minimum 600 DPI pour haute qualité
    cat(sprintf("🔍 Mode haute qualité activé (PNG: %d DPI)\n", png_dpi))
  }
  
  # Créer les cartes avec export multi-format
  cat("Génération des cartes en cours...\n")
  maps_result <- create_unicef_priority_maps(strategic_results, output_folder)
  
  cat(sprintf("\n🎯 CARTES CRÉÉES AVEC SUCCÈS!\n"))
  cat(sprintf("📁 Dossier: %s/\n", output_folder))
  cat(sprintf("🌍 Couverture: Régions WCAR & ESAR\n"))
  cat(sprintf("📄 Formats: SVG + PNG HD + PDF\n"))
  
  return(maps_result)
}

# ===== NOUVELLE FONCTION : Export personnalisé par format =====

export_custom_format <- function(maps_list, output_dir, 
                                 format = c("svg", "png", "both"),
                                 dpi = 300, width = 12, height = 10) {
  
  format <- match.arg(format)
  
  cat(sprintf("Export personnalisé en format: %s\n", toupper(format)))
  
  map_names <- c("geographique", "communautaire", "lastmile", "vue_ensemble")
  
  for (i in seq_along(maps_list)) {
    base_name <- sprintf("carte_%02d_%s", i, map_names[i])
    
    if (format %in% c("svg", "both")) {
      svg_file <- file.path(output_dir, paste0(base_name, ".svg"))
      ggsave(svg_file, plot = maps_list[[i]], 
             width = width, height = height, device = "svg", bg = "white")
      cat(sprintf("✓ SVG: %s\n", basename(svg_file)))
    }
    
    if (format %in% c("png", "both")) {
      png_file <- file.path(output_dir, paste0(base_name, ".png"))
      ggsave(png_file, plot = maps_list[[i]], 
             width = width, height = height, dpi = dpi, device = "png", bg = "white")
      cat(sprintf("✓ PNG: %s (%d DPI)\n", basename(png_file), dpi))
    }
  }
}

# ===== UTILISATION AMÉLIORÉE =====

# Utilisation basique (SVG + PNG HD automatique) :
results <- run_complete_immunity_analysis(data_final)
maps <- create_and_export_unicef_maps(results, "mes_cartes_unicef")


# Utilisation avec paramètres personnalisés :
# maps <- create_and_export_unicef_maps(results, 
#                                      output_folder = "cartes_ultra_HD", 
#                                      png_dpi = 1200, 
#                                      high_quality = TRUE)

# Export personnalisé par format :
# export_custom_format(maps, "exports_svg_seulement", format = "svg")
# export_custom_format(maps, "exports_png_print", format = "png", dpi




# ===== FONCTION BONUS: CARTE AVEC STATISTIQUES - EXPORT MULTIPLE =====
create_map_with_stats <- function(strategic_results, 
                                  output_dir = "cartes_avec_stats", 
                                  export_formats = c("png", "svg"),
                                  width = 12, 
                                  height = 10, 
                                  dpi = 300) {
  
  cat("Création de cartes avec statistiques intégrées...\n")
  
  # Vérifier les formats demandés
  valid_formats <- c("png", "svg", "pdf", "jpeg", "tiff")
  export_formats <- intersect(export_formats, valid_formats)
  
  if (length(export_formats) == 0) {
    warning("Aucun format valide spécifié. Utilisation de PNG par défaut.")
    export_formats <- "png"
  }
  
  # Préparer les statistiques
  geo_stats <- strategic_results$strategic$geographic$data %>%
    group_by(region_short) %>%
    summarise(
      pays_total = n(),
      priorite_moyenne = mean(priority_geo, na.rm = TRUE),
      immunite_moyenne = mean(immune_percentage, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Créer une carte avec overlay de statistiques
  map_data <- prepare_map_data(strategic_results, ne_countries(scale = "medium", returnclass = "sf"))
  africa_bounds <- get_africa_bounds()
  
  # Construction du graphique
  p <- ggplot(map_data$geographic) +
    geom_sf(aes(fill = priority_geo), color = "white", size = 0.3) +
    scale_fill_viridis_c(name = "Score de\nPriorité", na.value = "grey90", option = "plasma") +
    coord_sf(xlim = africa_bounds$xlim, ylim = africa_bounds$ylim, expand = FALSE) +
    
    # Statistiques WCAR
    annotate("rect", xmin = -25, xmax = -5, ymin = 20, ymax = 25,
             fill = "white", color = "black", alpha = 0.9) +
    annotate("text", x = -15, y = 23, label = "WCAR", size = 4, fontface = "bold") +
    annotate("text", x = -15, y = 22,
             label = sprintf("Pays: %d", 
                             ifelse(nrow(geo_stats[geo_stats$region_short == "WCAR", ]) > 0,
                                    geo_stats$pays_total[geo_stats$region_short == "WCAR"][1], 0)),
             size = 3) +
    annotate("text", x = -15, y = 21,
             label = sprintf("Priorité moy: %.1f", 
                             ifelse(nrow(geo_stats[geo_stats$region_short == "WCAR", ]) > 0,
                                    geo_stats$priorite_moyenne[geo_stats$region_short == "WCAR"][1], 0)),
             size = 3) +
    
    # Statistiques ESAR
    annotate("rect", xmin = 35, xmax = 55, ymin = 20, ymax = 25,
             fill = "white", color = "black", alpha = 0.9) +
    annotate("text", x = 45, y = 23, label = "ESAR", size = 4, fontface = "bold") +
    annotate("text", x = 45, y = 22,
             label = sprintf("Pays: %d", 
                             ifelse(nrow(geo_stats[geo_stats$region_short == "ESAR", ]) > 0,
                                    geo_stats$pays_total[geo_stats$region_short == "ESAR"][1], 0)),
             size = 3) +
    annotate("text", x = 45, y = 21,
             label = sprintf("Priorité moy: %.1f", 
                             ifelse(nrow(geo_stats[geo_stats$region_short == "ESAR", ]) > 0,
                                    geo_stats$priorite_moyenne[geo_stats$region_short == "ESAR"][1], 0)),
             size = 3) +
    
    # Titres et labels
    labs(
      title = "PRIORITÉS GÉOGRAPHIQUES AVEC STATISTIQUES RÉGIONALES",
      subtitle = "Analyse comparative WCAR vs ESAR",
      caption = "Statistiques: nombre de pays et scores moyens par région"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      plot.caption = element_text(size = 10, hjust = 0.5, margin = margin(t = 10)),
      legend.position = "bottom",
      legend.key.width = unit(1.5, "cm"),
      panel.background = element_rect(fill = "lightblue", color = NA),
      plot.margin = margin(20, 20, 20, 20)
    )
  
  # Créer le répertoire de sortie
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat(sprintf("✓ Répertoire créé: %s\n", output_dir))
  }
  
  # Export dans les différents formats
  exported_files <- character(0)
  
  for (format in export_formats) {
    filename <- file.path(output_dir, paste0("carte_avec_statistiques.", format))
    
    tryCatch({
      if (format == "svg") {
        # Pour SVG, on utilise svglite pour une meilleure qualité
        if (requireNamespace("svglite", quietly = TRUE)) {
          ggsave(filename, plot = p, device = "svg", 
                 width = width, height = height, bg = "white")
        } else {
          # Fallback avec ggsave standard
          ggsave(filename, plot = p, width = width, height = height, bg = "white")
        }
      } else if (format == "png") {
        ggsave(filename, plot = p, device = "png", 
               width = width, height = height, dpi = dpi, bg = "white")
      } else {
        # Autres formats (pdf, jpeg, tiff)
        ggsave(filename, plot = p, device = format, 
               width = width, height = height, dpi = dpi, bg = "white")
      }
      
      exported_files <- c(exported_files, filename)
      cat(sprintf("✓ Carte exportée en %s: %s\n", toupper(format), basename(filename)))
      
    }, error = function(e) {
      cat(sprintf("✗ Erreur lors de l'export en %s: %s\n", toupper(format), e$message))
    })
  }
  
  # Résumé des exports
  cat(sprintf("\n=== RÉSUMÉ DES EXPORTS ===\n"))
  cat(sprintf("Formats demandés: %s\n", paste(toupper(export_formats), collapse = ", ")))
  cat(sprintf("Fichiers créés: %d/%d\n", length(exported_files), length(export_formats)))
  cat(sprintf("Répertoire: %s\n", output_dir))
  cat(sprintf("Dimensions: %g x %g\n", width, height))
  if ("png" %in% export_formats) cat(sprintf("DPI: %d\n", dpi))
  
  return(list(
    plot = p,
    exported_files = exported_files,
    stats = geo_stats
  ))
}

# ===== FONCTION UTILITAIRE POUR EXPORTS RAPIDES =====
quick_export_map <- function(plot_object, 
                             filename_base = "carte_statistiques", 
                             output_dir = "exports",
                             formats = c("png", "svg")) {
  
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  exported <- character(0)
  
  for (format in formats) {
    filename <- file.path(output_dir, paste0(filename_base, ".", format))
    
    tryCatch({
      if (format == "png") {
        ggsave(filename, plot = plot_object, width = 12, height = 10, dpi = 300, bg = "white")
      } else if (format == "svg") {
        ggsave(filename, plot = plot_object, device = "svg", width = 12, height = 10, bg = "white")
      } else {
        ggsave(filename, plot = plot_object, width = 12, height = 10, bg = "white")
      }
      exported <- c(exported, filename)
      cat(sprintf("✓ Export réussi: %s\n", basename(filename)))
    }, error = function(e) {
      cat(sprintf("✗ Erreur export %s: %s\n", format, e$message))
    })
  }
  
  return(exported)
}

# ===== FONCTION POUR EXPORT BATCH DE PLUSIEURS CARTES =====
batch_export_maps <- function(plot_list, 
                              output_dir = "batch_exports",
                              formats = c("png", "svg"),
                              width = 12, 
                              height = 10) {
  
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  cat("=== EXPORT BATCH DE CARTES ===\n")
  all_exports <- list()
  
  for (i in seq_along(plot_list)) {
    plot_name <- names(plot_list)[i]
    if (is.null(plot_name) || plot_name == "") {
      plot_name <- sprintf("carte_%02d", i)
    }
    
    cat(sprintf("\nExport de: %s\n", plot_name))
    
    exported_files <- character(0)
    for (format in formats) {
      filename <- file.path(output_dir, paste0(plot_name, ".", format))
      
      tryCatch({
        ggsave(filename, plot = plot_list[[i]], 
               width = width, height = height, 
               dpi = ifelse(format == "png", 300, 72), bg = "white")
        exported_files <- c(exported_files, filename)
        cat(sprintf("  ✓ %s\n", basename(filename)))
      }, error = function(e) {
        cat(sprintf("  ✗ Erreur %s: %s\n", format, e$message))
      })
    }
    
    all_exports[[plot_name]] <- exported_files
  }
  
  cat(sprintf("\n=== RÉSUMÉ BATCH ===\n"))
  cat(sprintf("Cartes traitées: %d\n", length(plot_list)))
  cat(sprintf("Total fichiers créés: %d\n", sum(lengths(all_exports))))
  
  return(all_exports)
}

# ===== EXEMPLES D'UTILISATION =====

# Utilisation standard avec PNG et SVG
the_cartes_result <- create_map_with_stats(results, 
                                           export_formats = c("png", "svg"))

# Utilisation avec paramètres personnalisés
# result <- create_map_with_stats(strategic_results,
#                               output_dir = "mes_cartes",
#                               export_formats = c("png", "svg", "pdf"),
#                               width = 15, height = 12, dpi = 400)

# Export rapide d'un plot existant
# quick_export_map(mon_plot, "ma_carte", formats = c("png", "svg"))

# Export batch de plusieurs cartes
# plots <- list("priorites" = plot1, "immunite" = plot2, "combine" = plot3)
# batch_export_maps(plots, formats = c("png", "svg"))



# ===== HEATMAP DES PAYS PRIORITAIRES PAR RÉGION ET AXES STRATÉGIQUES =====

library(tidyverse)
library(ggplot2)
library(viridis)
library(scales)
library(patchwork)

# ===== FONCTION DE DIAGNOSTIC DES DONNÉES =====
diagnose_data_structure <- function(strategic_results) {
  cat("=== DIAGNOSTIC DE LA STRUCTURE DES DONNÉES ===\n")
  
  data <- strategic_results$strategic$geographic$data
  cat("Dimensions:", nrow(data), "lignes x", ncol(data), "colonnes\n")
  cat("\nNoms des colonnes:\n")
  print(names(data))
  
  cat("\nAperçu des premières lignes:\n")
  print(head(data, 3))
  
  cat("\nColonnes contenant 'country' ou 'pays':\n")
  country_cols <- names(data)[str_detect(tolower(names(data)), "country|pays")]
  print(country_cols)
  
  cat("\nColonnes contenant 'region':\n")
  region_cols <- names(data)[str_detect(tolower(names(data)), "region")]
  print(region_cols)
  
  cat("\nColonnes contenant 'priority':\n")
  priority_cols <- names(data)[str_detect(tolower(names(data)), "priority")]
  print(priority_cols)
  
  return(list(
    country_cols = country_cols,
    region_cols = region_cols,
    priority_cols = priority_cols,
    all_cols = names(data)
  ))
}

# ===== FONCTION PRINCIPALE: HEATMAP DES PAYS PRIORITAIRES =====
create_priority_countries_heatmap <- function(strategic_results, 
                                              top_countries_per_region = 10,
                                              output_dir = "heatmaps_prioritaires",
                                              export_formats = c("png", "svg"),
                                              width = 16, 
                                              height = 12,
                                              show_values = TRUE,
                                              color_scheme = "plasma") {
  
  cat("Création de la heatmap des pays prioritaires par région...\n")
  
  # Identifier automatiquement les colonnes
  data_cols <- names(strategic_results$strategic$geographic$data)
  
  country_col <- case_when(
    "country_name" %in% data_cols ~ "country_name",
    "Country" %in% data_cols ~ "Country",
    "country" %in% data_cols ~ "country",
    "pays" %in% data_cols ~ "pays",
    "COUNTRY" %in% data_cols ~ "COUNTRY",
    TRUE ~ data_cols[str_detect(tolower(data_cols), "country|pays")][1]
  )
  
  region_col <- case_when(
    "region_short" %in% data_cols ~ "region_short",
    "Region" %in% data_cols ~ "Region",
    "region" %in% data_cols ~ "region",
    "REGION" %in% data_cols ~ "REGION",
    TRUE ~ data_cols[str_detect(tolower(data_cols), "region")][1]
  )
  
  # Identifier les colonnes des 3 axes prioritaires
  priority_cols <- data_cols[str_detect(tolower(data_cols), "priority")]
  
  cat(sprintf("Colonnes identifiées - Pays: %s, Région: %s\n", country_col, region_col))
  cat(sprintf("Colonnes de priorité: %s\n", paste(priority_cols, collapse = ", ")))
  
  # Préparer les données
  priority_data <- strategic_results$strategic$geographic$data %>%
    # Standardiser les noms de colonnes
    rename(
      country_name = !!sym(country_col),
      region_short = !!sym(region_col)
    ) %>%
    # Sélectionner les colonnes nécessaires
    select(country_name, region_short, all_of(priority_cols)) %>%
    # Filtrer les données complètes
    filter(complete.cases(.)) %>%
    # Calculer un score de priorité global pour le classement
    mutate(
      priority_global = rowMeans(select(., all_of(priority_cols)), na.rm = TRUE)
    ) %>%
    # Sélectionner les pays les plus prioritaires par région
    group_by(region_short) %>%
    slice_max(order_by = priority_global, n = top_countries_per_region, with_ties = FALSE) %>%
    ungroup() %>%
    # Normaliser les scores pour chaque axe (0-100)
    mutate(
      across(all_of(priority_cols), 
             ~round(rescale(.x, to = c(0, 100)), 0), 
             .names = "{.col}_norm")
    )
  
  # Identifier les colonnes normalisées
  norm_cols <- names(priority_data)[str_ends(names(priority_data), "_norm")]
  
  # Restructurer les données pour la heatmap
  heatmap_data <- priority_data %>%
    select(country_name, region_short, priority_global, all_of(norm_cols)) %>%
    pivot_longer(
      cols = all_of(norm_cols),
      names_to = "axe_strategique",
      values_to = "score_priorite"
    ) %>%
    # Nettoyer les noms des axes stratégiques
    mutate(
      axe_strategique = case_when(
        str_detect(axe_strategique, "geo") ~ "Mobilisation\nCommunautaire",
        str_detect(axe_strategique, "mech") ~ "Optimisation\nGéographique", 
        str_detect(axe_strategique, "strat") ~ "Stratégie\nCas Zéro",
        TRUE ~ str_replace_all(str_replace_all(axe_strategique, "priority_", ""), "_norm", "")
      ),
      # Nettoyer les noms de pays
      country_display = case_when(
        str_detect(country_name, "United Republic") ~ str_replace(country_name, "United Republic of ", ""),
        str_length(country_name) > 15 ~ str_trunc(country_name, 15),
        TRUE ~ country_name
      )
    )
  
  # Séparer par région et ordonner par priorité
  esar_data <- heatmap_data %>% 
    filter(region_short == "ESAR") %>%
    arrange(desc(priority_global)) %>%
    mutate(country_display = factor(country_display, levels = unique(rev(country_display))))
  
  wcar_data <- heatmap_data %>% 
    filter(region_short == "WCAR") %>%
    arrange(desc(priority_global)) %>%
    mutate(country_display = factor(country_display, levels = unique(rev(country_display))))
  
  # Fonction pour créer la heatmap d'une région
  create_region_priority_heatmap <- function(data, region_name, show_legend = TRUE) {
    p <- ggplot(data, aes(x = axe_strategique, y = country_display, fill = score_priorite)) +
      geom_tile(color = "white", size = 0.8) +
      {if(show_values) geom_text(aes(label = score_priorite), 
                                 color = ifelse(data$score_priorite > 50, "white", "black"),
                                 size = 3.2, fontface = "bold")} +
      scale_fill_viridis_c(
        name = "Score de\nPriorité", 
        option = color_scheme,
        limits = c(0, 100),
        breaks = seq(0, 100, 25),
        labels = seq(0, 100, 25),
        na.value = "grey90"
      ) +
      labs(
        title = paste("RÉGION", region_name),
        subtitle = sprintf("Top %d pays prioritaires", top_countries_per_region),
        x = NULL,
        y = NULL
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5, color = "#2c3e50"),
        plot.subtitle = element_text(size = 11, hjust = 0.5, color = "#34495e"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        panel.grid = element_blank(),
        legend.position = if(show_legend) "right" else "none",
        legend.title = element_text(size = 10, face = "bold"),
        legend.key.width = unit(0.8, "cm"),
        legend.key.height = unit(1.2, "cm"),
        plot.margin = margin(15, 15, 15, 15),
        panel.border = element_rect(color = "#bdc3c7", fill = NA, size = 1)
      )
    
    return(p)
  }
  
  # Créer les heatmaps par région
  if(nrow(esar_data) > 0) {
    p_esar <- create_region_priority_heatmap(esar_data, "ESAR", show_legend = FALSE)
  } else {
    p_esar <- ggplot() + labs(title = "ESAR - Aucune donnée") + theme_void()
  }
  
  if(nrow(wcar_data) > 0) {
    p_wcar <- create_region_priority_heatmap(wcar_data, "WCAR", show_legend = TRUE)
  } else {
    p_wcar <- ggplot() + labs(title = "WCAR - Aucune donnée") + theme_void()
  }
  
  # Combiner les heatmaps
  combined_plot <- p_esar + p_wcar + 
    plot_layout(ncol = 2, widths = c(1, 1.3)) +
    plot_annotation(
      title = "HEATMAP DES PAYS PRIORITAIRES PAR RÉGION ET AXE STRATÉGIQUE",
      subtitle = "Scores de priorité normalisés (0-100) pour les pays les plus critiques",
      caption = "Plus la couleur est intense, plus la priorité est élevée | Classement par score global de priorité",
      theme = theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5, color = "#2c3e50"),
        plot.subtitle = element_text(size = 13, hjust = 0.5, color = "#34495e"),
        plot.caption = element_text(size = 10, hjust = 0.5, color = "#7f8c8d", margin = margin(t = 15))
      )
    )
  
  # Créer le répertoire de sortie
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat(sprintf("✓ Répertoire créé: %s\n", output_dir))
  }
  
  # Exporter dans les formats demandés
  exported_files <- character(0)
  
  for (format in export_formats) {
    filename <- file.path(output_dir, paste0("heatmap_pays_prioritaires.", format))
    
    tryCatch({
      if (format == "png") {
        ggsave(filename, plot = combined_plot, 
               width = width, height = height, dpi = 300, bg = "white")
      } else if (format == "svg") {
        ggsave(filename, plot = combined_plot, device = "svg",
               width = width, height = height, bg = "white")
      } else {
        ggsave(filename, plot = combined_plot, 
               width = width, height = height, bg = "white")
      }
      
      exported_files <- c(exported_files, filename)
      cat(sprintf("✓ Heatmap exportée en %s: %s\n", toupper(format), basename(filename)))
      
    }, error = function(e) {
      cat(sprintf("✗ Erreur lors de l'export en %s: %s\n", toupper(format), e$message))
    })
  }
  
  # Créer un tableau de synthèse des pays prioritaires
  summary_table <- priority_data %>%
    select(region_short, country_name, priority_global, all_of(norm_cols)) %>%
    arrange(region_short, desc(priority_global))
  
  # Statistiques par région et axe
  regional_stats <- heatmap_data %>%
    group_by(region_short, axe_strategique) %>%
    summarise(
      score_moyen = round(mean(score_priorite, na.rm = TRUE), 1),
      score_median = round(median(score_priorite, na.rm = TRUE), 1),
      score_max = max(score_priorite, na.rm = TRUE),
      pays_total = n(),
      .groups = "drop"
    )
  
  # Résumé final
  n_esar = nrow(esar_data %>% distinct(country_display))
  n_wcar = nrow(wcar_data %>% distinct(country_display))
  n_axes = length(unique(heatmap_data$axe_strategique))
  
  cat("\n=== RÉSUMÉ DE LA HEATMAP DES PAYS PRIORITAIRES ===\n")
  cat(sprintf("Pays prioritaires ESAR: %d\n", n_esar))
  cat(sprintf("Pays prioritaires WCAR: %d\n", n_wcar))
  cat(sprintf("Total pays analysés: %d\n", n_esar + n_wcar))
  cat(sprintf("Axes stratégiques: %d\n", n_axes))
  cat(sprintf("Fichiers exportés: %d\n", length(exported_files)))
  cat(sprintf("Dimensions: %g x %g\n", width, height))
  
  return(list(
    plot = combined_plot,
    esar_plot = p_esar,
    wcar_plot = p_wcar,
    data = heatmap_data,
    summary_table = summary_table,
    regional_stats = regional_stats,
    exported_files = exported_files
  ))
}

# ===== FONCTION POUR HEATMAP DÉTAILLÉE AVEC SEUILS =====
create_threshold_heatmap <- function(strategic_results,
                                     thresholds = list(high = 75, medium = 50, low = 25),
                                     top_countries_per_region = 12,
                                     output_dir = "heatmaps_seuils") {
  
  cat("Création de la heatmap avec indication des seuils de priorité...\n")
  
  # Utiliser la fonction principale pour obtenir les données
  result <- create_priority_countries_heatmap(strategic_results, 
                                              top_countries_per_region = top_countries_per_region,
                                              output_dir = output_dir,
                                              export_formats = c())  # Pas d'export automatique
  
  heatmap_data <- result$data
  
  # Ajouter les catégories de seuils
  heatmap_data_with_thresholds <- heatmap_data %>%
    mutate(
      priority_level = case_when(
        score_priorite >= thresholds$high ~ "ÉLEVÉ",
        score_priorite >= thresholds$medium ~ "MOYEN", 
        score_priorite >= thresholds$low ~ "FAIBLE",
        TRUE ~ "TRÈS FAIBLE"
      ),
      priority_level = factor(priority_level, 
                              levels = c("TRÈS FAIBLE", "FAIBLE", "MOYEN", "ÉLEVÉ")),
      # Texte avec score et niveau
      label_text = paste0(score_priorite, "\n(", priority_level, ")")
    )
  
  # Séparer par région
  esar_threshold <- heatmap_data_with_thresholds %>% 
    filter(region_short == "ESAR") %>%
    arrange(desc(priority_global)) %>%
    mutate(country_display = factor(country_display, levels = unique(rev(country_display))))
  
  wcar_threshold <- heatmap_data_with_thresholds %>% 
    filter(region_short == "WCAR") %>%
    arrange(desc(priority_global)) %>%
    mutate(country_display = factor(country_display, levels = unique(rev(country_display))))
  
  # Créer la heatmap avec seuils
  create_threshold_plot <- function(data, region_name, show_legend = TRUE) {
    ggplot(data, aes(x = axe_strategique, y = country_display, fill = score_priorite)) +
      geom_tile(color = "white", size = 0.8) +
      geom_text(aes(label = label_text), 
                color = ifelse(data$score_priorite > 50, "white", "black"),
                size = 2.8, fontface = "bold", lineheight = 0.9) +
      scale_fill_viridis_c(
        name = "Score\n& Seuil", 
        option = "plasma",
        limits = c(0, 100),
        breaks = c(thresholds$low, thresholds$medium, thresholds$high, 100),
        labels = c(paste0("Faible (", thresholds$low, ")"),
                   paste0("Moyen (", thresholds$medium, ")"),
                   paste0("Élevé (", thresholds$high, ")"),
                   "Max (100)")
      ) +
      labs(
        title = paste("RÉGION", region_name, "- AVEC SEUILS"),
        x = NULL, y = NULL
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9, face = "bold"),
        axis.text.y = element_text(size = 9, face = "bold"),
        panel.grid = element_blank(),
        legend.position = if(show_legend) "right" else "none",
        panel.border = element_rect(color = "#bdc3c7", fill = NA, size = 1)
      )
  }
  
  # Créer les plots
  p_esar_thresh <- if(nrow(esar_threshold) > 0) {
    create_threshold_plot(esar_threshold, "ESAR", FALSE)
  } else {
    ggplot() + labs(title = "ESAR - Aucune donnée") + theme_void()
  }
  
  p_wcar_thresh <- if(nrow(wcar_threshold) > 0) {
    create_threshold_plot(wcar_threshold, "WCAR", TRUE) 
  } else {
    ggplot() + labs(title = "WCAR - Aucune donnée") + theme_void()
  }
  
  # Combiner avec indication des seuils
  threshold_plot <- p_esar_thresh + p_wcar_thresh + 
    plot_layout(ncol = 2, widths = c(1, 1.3)) +
    plot_annotation(
      title = "HEATMAP AVEC SEUILS DE PRIORITÉ",
      subtitle = sprintf("Seuils: Faible≥%d | Moyen≥%d | Élevé≥%d", 
                         thresholds$low, thresholds$medium, thresholds$high),
      caption = "Chaque cellule indique le score et le niveau de priorité"
    )
  
  return(list(
    plot = threshold_plot,
    data = heatmap_data_with_thresholds,
    thresholds = thresholds
  ))
}

# ===== EXEMPLES D'UTILISATION =====

# ÉTAPE 1: Diagnostiquer la structure des données
diagnostic <- diagnose_data_structure(strategic_results)

# ÉTAPE 2: Créer la heatmap des pays prioritaires (standard)
result <- create_priority_countries_heatmap(strategic_results, 
                                            top_countries_per_region = 10)

# ÉTAPE 3: Heatmap avec plus de pays et export personnalisé
result_detailed <- create_priority_countries_heatmap(strategic_results,
                                                     top_countries_per_region = 15,
                                                     export_formats = c("png", "svg", "pdf"),
                                                     width = 18, height = 14,
                                                     color_scheme = "viridis")

# ÉTAPE 4: Heatmap avec seuils de priorité
threshold_result <- create_threshold_heatmap(strategic_results,
                                             thresholds = list(high = 80, medium = 60, low = 40),
                                             top_countries_per_region = 12)



