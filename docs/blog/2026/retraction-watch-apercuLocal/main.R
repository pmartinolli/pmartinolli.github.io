
setwd("C:/Users/martinop/Downloads/RW_local")   # remplacer par l'emplacement de votre dossier 

retraction_watch <- read.csv("retraction_watch.csv")
df <- retraction_watch

df_local <- df[
  grepl(
    "universite de montreal|university of montreal|U de Montreal|U of Montreal|UdeM",
    tolower(iconv(df$Institution, to = "ASCII//TRANSLIT")),  # ignore capitalization + strip accents
    ignore.case = TRUE
  ),
]

# afficher la liste des titres 
cat(paste(df_local$Title, collapse = "\n"))

# Convert "MM/DD/YYYY 0:00" → Date
df_local$OriginalPaperDate <- as.Date(df_local$OriginalPaperDate,
                                     format = "%m/%d/%Y %H:%M")
# Extraire l'année
df_local$year <- format(df_local$OriginalPaperDate, "%Y")

# Draw a histogram
barplot(
  table(df_local$year),
  main = "Original Paper Dates by Year",
  xlab = "Year",
  ylab = "Count",
  las = 2
)


# Compter les occurrences des raisons
reason_counts <- sort(table(df_local$Reason), decreasing = FALSE)

# Plus d’espace pour les labels
par(mar = c(5, 30, 4, 2))   # bottom, left, top, right (agrandir surtout à gauche)

# Barplot simple
barplot(
  reason_counts,
  horiz = TRUE,
  main = "Distribution des raisons de rétractations",
  xlab = "Nombre",
  ylab = "Raisons",
  las = 1,
  cex.names = .8
)



# Extraire tous les auteurs, séparer par ";", nettoyer les espaces
all_authors <- unlist(strsplit(df_local$Author, ";"))   # sépare tous les auteurs
all_authors <- trimws(all_authors)                     # retire les espaces
all_authors <- all_authors[all_authors != ""]          # enlève les vides

# Compter la fréquence
author_counts <- table(all_authors)

# Trier par fréquence décroissante
author_counts <- sort(author_counts, decreasing = FALSE)

# Ajuster les marges pour avoir de la place pour les noms
par(mar = c(5, 20, 4, 2))  # bottom, left, top, right

# Barplot horizontal
barplot(
  author_counts,
  horiz = TRUE,
  las = 1,           # labels horizontaux
  cex.names = 0.7,   # taille des noms
  main = "Distribution des auteurs",
  xlab = "Nombre"
)




# Nettoyer les journaux
all_journals <- trimws(df_local$Journal)
all_journals <- all_journals[all_journals != ""]  # enlever vides

# Compter la fréquence
journal_counts <- table(all_journals)
journal_counts <- sort(journal_counts, decreasing = FALSE)   

# Ajuster marges pour les labels
par(mar = c(5, 25, 4, 2))  # agrandir la marge gauche pour les noms

# Barplot horizontal
barplot(
  journal_counts,
  horiz = TRUE,
  las = 1,           # labels horizontaux
  cex.names = 0.7,   # taille des noms
  main = "Distribution des revues",
  xlab = "Nombre"
)




# Extraire tous les pays, séparer par ";", nettoyer les espaces
all_countries <- unlist(strsplit(df_local$Country, ";"))   # sépare 
all_countries <- trimws(all_countries)                     # retire les espaces
all_countries <- all_countries[all_countries != ""]          # enlève les vides

# Compter la fréquence
country_counts <- table(all_countries)

# Trier par fréquence décroissante
country_counts <- sort(country_counts, decreasing = FALSE)

# Ajuster les marges pour avoir de la place pour les noms
par(mar = c(5, 20, 4, 2))  # bottom, left, top, right

# Barplot horizontal
barplot(
  country_counts,
  horiz = TRUE,
  las = 1,           # labels horizontaux
  cex.names = 0.7,   # taille des noms
  main = "Distribution des pays",
  xlab = "Nombre"
)
