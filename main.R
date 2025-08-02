
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



# Préparation des données ---------------------------------------------------------------


# Objectif 1. Construire notre base de données de travail


# Extraire les données utiles des feuilles de la base de données wuenic24
# Commencer par identifier toutes les feuilles présentes
sheets <- getSheetNames("data/raw/data_wuenic.xlsx")


# Conserver les feuilles avec les données sur les 16 vaccins
vaccine <- sheets[sheets != "regional_global"]


# A - Base de données de couverture vaccinale

# A - 1 . Importation de la base

data_vaccine_coverage <- read.xlsx("data/raw/data_wuenic.xlsx", sheet = "regional_global")

# A - 2 . Nettoyage et traitement de données

str(data_vaccine_coverage)
summary(data_vaccine_coverage)
head(data_vaccine_coverage)


# Problème 1. Renommage des variables pour uniformiser le nommage entre les différentes tables

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


# Problème 2. Les valeurs <100, <1000 dans data_vaccine_coverage.
# Nous avons remarqué cette valeur, inhabituelle dans le dataset.
# Vérifions où elle se trouve.

cols_with_lt_values <- sapply(data_vaccine_coverage, function(col) {
  any(grepl("^<\\d{1,3}(,\\d{3})*$", col))
})
columns_to_check <- names(cols_with_lt_values)[cols_with_lt_values] # Uniquement la colone vaccinated.

# Combien de fois ces valeurs apparaissent-elles ?
data_vaccine_coverage %>%
  filter(!grepl("^[0-9]", vaccinated)) %>%
  count(vaccinated, sort = TRUE)

# Vérifions notre hypothèse que vaccinated <100 ou <1000 == 0 dans cette base.


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
  count(vaccinated, sort = TRUE) #<100 n'a plus été trouvé. Passons au suivant.


# <1000
subset_rows_1000 <- grepl("<\\s*1[,']?000", data_vaccine_coverage$vaccinated)

data_check <- data_vaccine_coverage[subset_rows_1000, ] %>%
  mutate(
    unvaccinated_num = as.numeric(gsub(",", "", unvaccinated)),
    target_num = as.numeric(gsub(",", "", target)),
    diff = target_num - unvaccinated_num ,
    coverage_num = round((diff/target_num)*100, 1) ,  # au cas où c'est du texte
    ok = (diff == 0 & coverage_num == 0)
  )

# Affiche les lignes qui ne respectent PAS la condition
data_check %>% filter(!ok | is.na(ok))

# Remplacer là où la condition est respectée.

condition <- (data_vaccine_coverage$vaccinated == "<1,000") &
  ((as.numeric(gsub(",", "", data_vaccine_coverage$unvaccinated)) -
      as.numeric(gsub(",", "", data_vaccine_coverage$target))) == 0) &
  (data_vaccine_coverage$coverage == 0)

data_vaccine_coverage$vaccinated[condition] <- "0"

#Vérifier si le changement est bien fait.
data_vaccine_coverage %>%
  filter(!grepl("^[0-9]", vaccinated)) %>%
  count(vaccinated, sort = TRUE) #Plus que 1 <1,000 restant. Passons au suivant.


# Pour ce dernier cas, la couverture arrondie == 0 parce que le nombre de vaccination
# est trop faible. Ici, juste mettre la différence obtenue dans vaccinated.
# Remplacer là où la condition n'est pas respectée.

data_vaccine_coverage <- data_vaccine_coverage %>%
  mutate(
    vaccinated = ifelse(
      vaccinated == "<1,000",
      as.character(as.numeric(gsub(",", "", target)) - as.numeric(gsub(",", "", unvaccinated))),
      vaccinated
    )
  )

#Vérifier si le changement est bien fait.
data_vaccine_coverage %>%
  filter(!grepl("^[0-9]", vaccinated)) %>%
  count(vaccinated, sort = TRUE) # Plus que valeurs au pattern <xxx.


# Problème 3. Enlever les (,) des chiffres dans les colonnes concernées

str(data_vaccine_coverage)
cols_to_clean <- c("vaccinated", "unvaccinated", "target")

# Utiliser la fonction gsub pour supprimer les virgules des chiffres
data_vaccine_coverage <- data_vaccine_coverage %>%
  mutate(across(all_of(cols_to_clean), ~ gsub(",", "", .))
  )

# Vérifier la présence de NA dans le dataset.
any(is.na(data_vaccine_coverage)) # FALSE



# Problème 4. Donner la bonne typologie des variables dans le dataframe

str(data_vaccine_coverage)

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



# Problème 5. Incohérences
# Quelques incohérences relevées dans la base de données

data_check_incoherences <- data_vaccine_coverage %>%
  mutate(
    diff = target - unvaccinated,
    coverage_num = round((diff/target)*100),
    ok = (diff == vaccinated & coverage_num == coverage)
  )

# Combien d'enregistrement ne respectent PAS la conditionde cohérence
data_check_incoherences %>% count(!ok | is.na(ok)) #962 enrégistrements


# Essayons de comprendre ces incohérences

data_check_incoherences %>%
  filter(!ok | is.na(ok)) %>%
  select(coverage, coverage_num, vaccinated, diff, unvaccinated, target) %>%
  slice_head(n = 10) %>%
  print()


# Peut de différences entre coverage. Utiliser pour refaire la vérification des incohérences.

data_vaccine_coverage_check <- data_vaccine_coverage %>%
  mutate(
    diff = target - unvaccinated,
    coverage_num = round((diff/target)*100, 0),
    coverage_check = abs(coverage - coverage_num) <= 1  # tolérance de 1%
  )

data_vaccine_coverage_check %>% count(!coverage_check | is.na(coverage_check)) #0 incohérence.


# Vérifier la présence de NA dans le dataset.

any(is.na(data_vaccine_coverage))
#which(is.na(data_vaccine_coverage), arr.ind = TRUE)

# Le dataset data_vaccine_coverage est propre et prêt à l'emploi.
# Extraction et sauvegarde de la base de données.

write.xlsx(
  data_vaccine_coverage, 
  "data/processed/data_vaccine_coverage.xlsx"
  )


# B - Base de données sur les vaccins.

# B - 1 . Importation des données & transformation

# Transformer la structure de chaque feuille pour créer les variables year
# et immune_percentage. Consolider toutes les infos de tous les 16 vaccins
# dans une seule base de données.

data_vaccine_immune <- map_df(vaccine, ~{
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

# B - 2 . Nettoyage et traitement des données

str(data_vaccine_immune)
summary(data_vaccine_immune)
head(data_vaccine_immune)

# Problème 1. Donner la bonne typologie des variables dans le dataframe

str(data_vaccine_immune)

data_vaccine_immune <- data_vaccine_immune %>%
  mutate(
    unicef_region = as.factor(unicef_region),
    iso3 = as.factor(iso3),
    country = as.factor(country),
    vaccine = as.factor(vaccine),
    year = as.integer(year),
    immune_percentage = as.numeric(immune_percentage)
  )

# Extraction des données de la base
write.xlsx(
  data_vaccine_immune, 
  "data/processed/data_vaccine_immune.xlsx"
)


# Problème 2 . Traitement des valeurs manquantes (NA)
# Nous remarquons la présence de NA pour la variable immune_pourcentage.

summary(data_vaccine_immune)

sum(is.na(data_vaccine_immune$immune_percentage))
# 46156 valeurs manquantes sur 109665 enregistrements (~42%).


# 1. Explorer la répartition des valeurs manquantes

# par unicef_regions
data_vaccine_immune %>%
  filter(is.na(immune_percentage)) %>%
  count(unicef_region, sort = TRUE) %>%
  print(n = length(unique(data_vaccine_immune$unicef_region)))

# par pays
data_vaccine_immune %>%
  filter(is.na(immune_percentage)) %>%
  count(country, sort = TRUE) %>%
  print(n = length(unique(data_vaccine_immune$country)))

# par vaccin
data_vaccine_immune %>%
  filter(is.na(immune_percentage)) %>%
  count(vaccine, sort = TRUE) %>%
  print(n = length(unique(data_vaccine_immune$vaccine)))

# par années
data_vaccine_immune %>%
  filter(is.na(immune_percentage)) %>%
  count(year, sort = TRUE) %>%
  print(n = length(unique(data_vaccine_immune$year)))


# Etape 2. Essayer de comprendre ce qui explique les NA dans le tableau ?

# Nous posons l'hypothèse que les NA de la variable immune_percentage sont
# dues en majorité à un défaut d'introduction du vaccin dans le pays.
# Vérifions cette hypothèse.

# A . Constituer une base de donnée qui indique l'année d'introduction du vaccin dans chaque pays


data_vaccine_introduction <- data.frame(
  colnames(
    ""
  )
)




# C - Essaies de consolidation des deux bases de données

# Objectif : Agréger les données dans data_vaccine_immune par régions

# Calculons le immune_percentage global pour toutes les régions confondues, par vaccin et par année.
global_immune_percentage <- data_vaccine_immune %>%
  group_by(vaccine, year) %>%
  summarise(
    immune_percentage = round(mean(immune_percentage, na.rm = TRUE)),  # ignore les NA
    .groups = "drop"
  ) %>%
  ungroup() %>%
  mutate(
    unicef_region = "Global",
    immune_percentage = ifelse(is.na(immune_percentage), 0, immune_percentage)  # remplace les valeurs vides par NA
  ) %>%
  select(unicef_region, vaccine, year, immune_percentage)

# Calculons en plus, le immune_percentage par unicef_region, par vaccin et par an
regional_immune_percentage <- data_vaccine_immune %>%
  group_by(unicef_region, vaccine, year) %>%
  summarise(
    immune_percentage = round(mean(immune_percentage, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  mutate(
    immune_percentage = ifelse(is.na(immune_percentage), 0, immune_percentage)  # remplace les valeurs vides par NA
  ) %>%
  select(unicef_region, vaccine, year, immune_percentage)

# Associons ces deux bases de données avant jointure
data_vaccin_immune_region <- bind_rows(global_immune_percentage, regional_immune_percentage)

any(is.na(data_vaccine_coverage))

# Joignons à présent les différentes bases.

data_vaccine <- data_vaccine_coverage %>%
  left_join(data_vaccin_immune_region, by = c("unicef_region", "vaccine", "year")) %>%
  select(unicef_region, vaccine, year, coverage, immune_percentage, target, vaccinated, unvaccinated)

any(is.na(data_vaccine))



# D - Autres transformations pertinentes et webscrapping


# Etape 1 . Créer un vecteur continent, qui contient les pays africains uniquement.
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
  "ESH" = "MENA"  # Sahara Occidental (si présent dans ta base)
)

continent_unicef <- c(
  "WCAR" = "Africa",
  "ESAR" = "Africa",
  "MENA" = "Afrasia"  # Afrique du Nord + Moyen-Orient
)

pays_africa <- c(
  "DZA", "AGO", "BEN", "BWA", "BFA", "BDI", "CPV", "CMR", "CAF", "TCD",
  "COM", "COG", "CIV", "COD", "DJI", "EGY", "GNQ", "ERI", "SWZ", "ETH",
  "GAB", "GMB", "GHA", "GIN", "GNB", "KEN", "LSO", "LBR", "LBY", "MDG",
  "MWI", "MLI", "MRT", "MUS", "MAR", "MOZ", "NAM", "NER", "NGA", "RWA",
  "STP", "SEN", "SYC", "SLE", "SOM", "ZAF", "SSD", "SDN", "TZA", "TGO",
  "TUN", "UGA", "ZMB", "ZWE"
)


# Etape 2 . Créer un vecteur zone de priotité vaccinale pour l'Unicef
# Le niveau de priorité dimiune, de 1 à 4.

unicef_region_priority <- c(
  "WCAR"  = 1,  # Afrique de l’Ouest et du Centre
  "ROSA"  = 1,  # Asie du Sud (South Asia)
  "ESAR"  = 2,  # Afrique de l’Est et Australe
  "MENA" = 3,  # Moyen-Orient et Afrique du Nord
  "EAPR"  = 3,  # Asie de l’Est et Pacifique (East Asia & Pacific)
  "LACR"  = 4,  # Amérique latine et Caraïbes
  "ECAR"  = 4   # Europe et Asie centrale (Europe & Central Asia)
)


# Etape 3 . Créer une variable catégorielle des programmes de vaccination dans les pays.
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

# Intégrer les vecteurs pertinents dans la base de données.

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


# Extraction des données de la base
write.xlsx(
  data_vaccine, 
  "data/processed/data_vaccine.xlsx"
)


# Petite remarque.

# Dans la base de données, les valeurs indiquées pour coverage & immune_percentage sont
# très différentes. Cela suggère que l'immune_percentage est calculé de la façon suivante :
# %immunisés ≈ Couverture vaccinale × Efficacité vaccinale



# Au final, pour cette étude, nous avons les bases suivantes.

# data_vaccine_immune, pour le porcentage d'enfants immunisés par pays, par vaccin et par an
# data_vaccine, pour la couverture vaccinale par region et par an







# Analyses ---------------------------------------------------------------

# Objectif 1. Analyser les performances vaccinales en Afrique








# Extraire les données sur l'afrique.


data_vaccine_immune_africa <- data_vaccine_immune %>%
  filter(iso3 == africa)

unique(data_vaccine_immune_africa$country)
