#### 1. Load data for duplication check #####
library(tidyverse);library(sf)

username <- str_split(getwd(), '/')[[1]][3]
setwd(paste0('C:/Users/', username, '/OneDrive - CZU v Praze/czu/intrplEU/'))


## Load cleaned EVA data
EVA <- './data/input/EVA.csv' %>%
  read_csv()

## Load cleaned ReSuEU
ReSuEU <- './data/input/ReSurveyEU.csv' %>%
  read_csv() 

## Load core EVA species data
species_data <- read_delim(
  './data/eva/222_PlantDiversity20241017_notJUICE/222_PlantDiversity20241017_notJUICE_species.csv',
  col_select = c(1,3,6,9,10), show_col_types = F
) %>%
  bind_rows( ## add GVRD species data
    read_delim(
      './data/eva/222_PlantDIversity20241025_GVRD/222_GVRD_20241025_notJUICE_species.csv',
      col_select = c(1,3,6,9,10), show_col_types = F
    ) 
  ) %>%
  bind_rows( ## add UKFloodplainMeadows species data
    read_delim(
      './data/eva/222_UKFloodplainMeadows20241031/222_UKFloodplainMeadows20241031_notJUICE_species.csv',
      col_select = c(1,3,6,9,10), show_col_types = F
    ) 
  ) %>%
  setNames(c('plot_id','taxongroup', 'matched_concept', 'layer', 'cover'))  %>% # rename columns
  semi_join(data.frame(plot_id = c(EVA$plot_id, ReSuEU$plot_id)), 'plot_id') # filter out only the plots we selected this far

species_data$matched_concept <- ifelse(species_data$matched_concept == 'Bidens tripartita', 'Bidens tripartitus', species_data$matched_concept)
species_data$matched_concept <- ifelse(species_data$matched_concept == 'Lactuca muralis', 'Mycelis muralis', species_data$matched_concept) 
species_data$matched_concept <- ifelse(species_data$matched_concept == 'Cerastium fontanum subsp. holosteoides', 'Cerastium fontanum', species_data$matched_concept) 
species_data$matched_concept <- ifelse(species_data$matched_concept == 'Bidens frondosus', 'Bidens frondosa', species_data$matched_concept) 
species_data$matched_concept <- ifelse(str_detect(species_data$matched_concept, 'Festuca valesiaca'), 'Festuca valesiaca', species_data$matched_concept) 

#2. prepare data from FloraVegData ####

# Load flora veg data
pth2floraveg <- paste0('C:/Users/',username,'/OneDrive - CZU v Praze/czu/eivedis/data/floraveg_edit.xlsx')
FloraVegData_head <- pth2floraveg %>%
  readxl::read_excel(n_max = 2, col_names = F) %>%
  as.data.frame()
FloraVegData_head_merge <- list()
for (i in 1:ncol(FloraVegData_head)) {
  FloraVegData_head_merge[[i]] <- paste0(FloraVegData_head[1:2,i], collapse = ' -- ')
}
FloraVegData_head_merge <- unlist(FloraVegData_head_merge) %>% str_remove_all('NOTITLE -- ')
FloraVegData <- pth2floraveg %>%
  readxl::read_excel(col_names = F, skip = 2, na='null') %>%
  setNames(FloraVegData_head_merge)
FloraVegData <- FloraVegData %>% rename(species=lat_name)
# FloraVegData <- FloraVegData %>% 
#   separate(matched_concept, into = c('species','genus'), remove = F) %>% 
#   unite('species_name',c(species, genus))

mtch <- paste0('C:/Users/',username,'/Dropbox/GRACE_Project/data/147-approved-data/spnames-merging-EUNIS-ESy-2021-10-13_Midolo.xlsx') %>%
  readxl::read_xlsx() %>% rename(matched_concept=`Matched concept`)
mtch$species <- ifelse(str_detect(mtch$species, 'Festuca valesiaca'), 'Festuca valesiaca', mtch$species) 

# species vascular in EVA
species_vascular <- species_data %>% 
  filter(taxongroup == 'Vascular plant') %>% 
  select(matched_concept) %>%
  distinct() %>%
  filter(!str_detect(matched_concept, ' species')) %>%
  left_join(mtch) 
species_vascular$species <- ifelse(is.na(species_vascular$species), species_vascular$matched_concept, species_vascular$species)


assign_1 <- species_vascular %>% select(species) %>% unique() %>% semi_join(FloraVegData) %>% left_join(FloraVegData)

missing_in_EVA_1 <- species_vascular %>% anti_join(assign_1 %>% select(species) %>% unique()) %>% 
  filter(!str_detect(species, '_species')) 

missing_in_EVA_1 <- missing_in_EVA_1%>%
  left_join(
    species_data %>% semi_join(missing_in_EVA_1) %>% group_by(matched_concept) %>% summarise(nplots=n())
  )


names(assign_1)

EUNIS_diagnostic <- assign_1[,c(1,159)]
EUNIS_diagnostic$`Diagnostic species of EUNIS habitats -- NA` <- ifelse(is.na(EUNIS_diagnostic$`Diagnostic species of EUNIS habitats -- NA` ), " ", EUNIS_diagnostic$`Diagnostic species of EUNIS habitats -- NA` )

##load habitats
habitat_conversion <- 
  paste0('./data/eva/habitat_conversion_table.csv') %>%
  read_csv(show_col_types = F) %>%
  arrange(habitat, ESy) %>%
  semi_join(data.frame(habitat=c('forest','grassland','scrub','wetland'))) %>%
  mutate(habitat = str_to_title(habitat)) %>%
  select(ESy, habitat) %>%
  semi_join(EVA) %>%
  mutate(nchar=nchar(ESy)) %>%
  filter(nchar == 3)
head(habitat_conversion)

habitat_diagnostics <- list()
for (i in habitat_conversion$ESy) {
  if (any(str_detect(EUNIS_diagnostic$`Diagnostic species of EUNIS habitats -- NA`, i))){
    habitat_diagnostics[[i]] <- data.frame(
      species = EUNIS_diagnostic[str_detect(EUNIS_diagnostic$`Diagnostic species of EUNIS habitats -- NA`, i), ]$species,
      habitat = habitat_conversion[habitat_conversion$ESy==i,]$habitat
    )
  }
  else {
    habitat_diagnostics[[i]] <- data.frame(
    )
  }

}
habitat_diagnostics <- habitat_diagnostics %>% bind_rows(.id='ESy')
habitat_diagnostics <- habitat_diagnostics %>% select(-ESy) %>% unique()
habitat_diagnostics$habitat %>% table

table(habitat_diagnostics$species %in% species_vascular$species)

habitat_diagnostics_tab <- habitat_diagnostics %>%
  mutate(v=1) %>%
  spread(habitat,v, fill=0)
names(habitat_diagnostics_tab) <- c('species',paste0(names(habitat_diagnostics_tab)[2:5],'_diagnostic'))
  

species_vascular <- species_vascular %>% left_join(habitat_diagnostics_tab)
species_vascular[is.na(species_vascular)] <- 0


#is alinen?
aliens <- assign_1[,c(1,9)]
aliens$neophyte <- ifelse(aliens$`Origin in Europe -- Neophyte`, 1, 0)
aliens$neophyte <- ifelse(is.na(aliens$`Origin in Europe -- Neophyte`), 0, aliens$neophyte)
aliens <- aliens %>% select(1,3)
sum(aliens$neophyte)

species_vascular <-  species_vascular %>% left_join(aliens)
species_vascular[is.na(species_vascular)] <- 0
species_vascular
species_vascular[,3:7] %>% colSums()

species_vascular_mtch <- species_vascular %>% select(-species) %>% unique() 
species_vascular_mtch_dup <- species_vascular_mtch %>% group_by(matched_concept) %>% filter(n()>1) %>% ungroup()%>%
  mutate(sum = Forest_diagnostic+Grassland_diagnostic +Scrub_diagnostic +Wetland_diagnostic +neophyte) %>%
  filter(sum > 0) %>% select(-sum)
species_vascular_mtch <- species_vascular_mtch %>% anti_join(species_vascular_mtch_dup,'matched_concept') %>%
  bind_rows(species_vascular_mtch_dup) %>%
  arrange(matched_concept)

#3. Count species richness of habitat specialists and neophytes ####
st=Sys.time()
species_data_mtch <- species_data %>%
  left_join(species_vascular_mtch) %>%
  select(plot_id, matched_concept, Forest_diagnostic, Grassland_diagnostic, Scrub_diagnostic, Wetland_diagnostic, neophyte) %>%
  unique()
  #semi_join(EVA %>% sample_n(10000))
species_data_mtch[is.na(species_data_mtch)] <- 0
richness_data <- species_data_mtch %>%
  group_by(plot_id) %>%
  summarise_if(is.numeric, sum)
names(richness_data) <- c('plot_id', paste0('S_',tolower(names(richness_data)[-1]))) %>% str_replace('_diagnostic','.spec')
richness_data
Sys.time()-st

# ADD richness data to already compiled list of plots
EVA <- EVA %>% left_join(richness_data, 'plot_id') %>%
  select(database:S, contains('S_'), everything())
ReSuEU <- ReSuEU %>% left_join(richness_data, 'plot_id') %>%
  select(database:S, contains('S_'), everything())


## Write data
write_csv(EVA, './data/input/EVA.csv')
write_csv(ReSuEU, './data/input/ReSurveyEU.csv')
