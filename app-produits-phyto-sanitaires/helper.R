load("pps.RData")

choices_substances <- as.list(unique(condensed_df$substance))
choices_year <- as.list(unique(condensed_df$annee))
choices_dept <- as.list(unique(condensed_df$departement))

# Get substance nmaes ordered by decreasing total quantity used
choice_subs_import_order <- 
  condensed_df %>% 
  group_by(substance) %>% 
  summarise(quantite = sum(quantite)) %>% 
  arrange(desc(quantite)) %>% 
  pull(substance) %>% 
  as.list()

# Download the map 
carte <-  
  st_read(dsn = "departement/DEPARTEMENT.shp", 
                  layer = "DEPARTEMENT",
                  quiet = TRUE) %>% 
  st_transform(2154) %>% 
  st_simplify(dTolerance = 2000) %>% 
  # changes the names of the variables
  janitor::clean_names() %>% 
  rename("departement" = "nom_dept")

dep_regions <- tibble(nom_reg = carte$nom_reg,
                      departement = carte$departement)

# ------ Formating data harvested form wikipedia to display EFFECT ------ 


