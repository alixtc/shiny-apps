

load("pps.RData")
load("carte.RData")

choices_substances <- as.list(unique(condensed_df$substance))
choices_year <- as.list(unique(condensed_df$annee))
choices_dept <- as.list(unique(condensed_df$departement))


# Download the map 
# carte <- vroom::vroom("carte.tsv")
# carte <-
#   st_read(dsn = "departement/DEPARTEMENT.shp",
#                   layer = "DEPARTEMENT",
#                   quiet = TRUE) %>%
#   st_transform(2154) %>%
#   st_simplify(dTolerance = 2500) %>%
#   # changes the names of the variables
#   janitor::clean_names() %>%
#   rename("departement" = "nom_dept")

# dep_regions <- tibble(nom_reg = carte$nom_reg,
#                       departement = carte$departement)




