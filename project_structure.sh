#!/bin/bash

# Script pour créer l'arborescence du projet d'analyse de couverture vaccinale
# Usage: ./create_project_structure.sh

PROJECT_NAME="WUENIC24"

echo "🔧 Création de l'arborescence du projet: $PROJECT_NAME"

# Créer le répertoire principal et naviguer dedans
mkdir -p "$PROJECT_NAME"
cd "$PROJECT_NAME"

# Créer tous les répertoires
echo "📁 Création des répertoires..."
mkdir -p config
mkdir -p functions  
mkdir -p scripts
mkdir -p data/raw
mkdir -p data/processed
mkdir -p data/final
mkdir -p outputs/figures
mkdir -p outputs/tables

# Créer les fichiers R vides avec commentaires
echo "📄 Création des fichiers R..."

# Fichier principal
touch main.R

# Fichiers de configuration
touch config/setup.R
touch config/constants.R
touch config/parameters.R

# Fichiers de fonctions
touch functions/data_utils.R
touch functions/analysis_utils.R
touch functions/plot_utils.R

# Scripts d'analyse
touch scripts/01_data_import.R
touch scripts/02_data_processing.R
touch scripts/03_analysis.R
touch scripts/04_visualization.R

echo "✅ Arborescence créée avec succès!"
echo ""
echo "Structure générée:"
echo "$PROJECT_NAME/"
echo "├── main.R"
echo "├── config/"
echo "│   ├── setup.R"
echo "│   ├── constants.R"
echo "│   └── parameters.R"
echo "├── functions/"
echo "│   ├── data_utils.R"
echo "│   ├── analysis_utils.R"
echo "│   └── plot_utils.R"
echo "├── scripts/"
echo "│   ├── 01_data_import.R"
echo "│   ├── 02_data_processing.R"
echo "│   ├── 03_analysis.R"
echo "│   └── 04_visualization.R"
echo "├── data/"
echo "│   ├── raw/"
echo "│   ├── processed/"
echo "│   └── final/"
echo "└── outputs/"
echo "    ├── figures/"
echo "    └── tables/"