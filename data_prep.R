#######################################################
#                                                     #
#   R script for importing and cleaning survey data   #
#                 Version 2018-10-07                  #
#                (c) Andrew Terhorst                  #
#                                                     #
#######################################################

# *************** process node data ***************** # 

# read in raw node data

require(readxl)
require(httr)

creds <- authenticate(readline(prompt = "Enter ident: "), readline(prompt = "Enter password: "))

url <- "https://bitbucket.csiro.au/projects/ALT/repos/phd_data/raw/case_1/surveydata_case_1.xlsx"
GET(url, creds, write_disk(tf <- tempfile(fileext = ".xlsx")))
node_data_case_1 <- read_excel(tf, sheet = 1)

url <- "https://bitbucket.csiro.au/projects/ALT/repos/phd_data/raw/case_2/surveydata_case_2.xlsx"
GET(url, creds, write_disk(tf <- tempfile(fileext = ".xlsx")))
node_data_case_2 <- read_excel(tf, sheet = 1)

url <- "https://bitbucket.csiro.au/projects/ALT/repos/phd_data/raw/case_3/surveydata_case_3.xlsx"
GET(url, creds, write_disk(tf <- tempfile(fileext = ".xlsx")))
node_data_case_3 <- read_excel(tf, sheet = 1)


# merge node data

require(tidyverse)

nodes <- bind_rows(node_data_case_1 %>% mutate(case = 1) %>% rename(Occupation = Occupation1), # fix glitch with first survey
                   node_data_case_2 %>% mutate(case = 2),
                   node_data_case_3 %>% mutate(case = 3)) %>% 
  # only consider completed survey responses
  filter(completed == "Complete")

# read in organisational affiliation data

url <- "https://bitbucket.csiro.au/projects/ALT/repos/phd_data/raw/org_affiliation.csv"
GET(url, creds, write_disk(tf <- tempfile(fileext = ".csv")))
affiliation <- read.csv(tf, stringsAsFactors = F)

# preliminary cleaning

nodes_clean <- nodes %>%
  # extract numerical values
  mutate(Age = str_extract(Age, "[0-9]*"),
         Experience = str_extract(Experience, "[0-9]*"),
         Tenure = str_extract(Tenure, "[0-9]*")) %>%
  # rename column headers
  rename(gender = Gender,
         age = Age,
         work_location = Location,
         education_level = Education,
         broad_education_field = BroadEducationField,
         occupation_class = Occupation,
         current_tenure = Tenure,
         work_experience = Experience,
         identification_org = Identity1,
         identification_group = Identity2,
         identification_collab = Identity3)

# change column types

require(magrittr)

make_numeric <- c(1, 5:6, 8, 11:12, 14:52)
nodes_clean[ , make_numeric] %<>% lapply(function(x) as.numeric(as.character(x)))

# rescale and aggregate items

nodes_rescaled <- nodes_clean %>%
  # reverse scores
  mutate(Openness2 = 10 - Openness2,
         Conscientiousness1 = 10 - Conscientiousness1,
         Agreeableness2 = 10 - Agreeableness2) %>%
  # do calculations per row 
  rowwise() %>%
  # aggregate items and rescale to values between 0 and 1
  mutate(personality_openness = round((mean(c(Openness1, Openness2)) - 1) / 9, digits = 2),
         personality_conscientiousness = round((mean(c(Conscientiousness1, Conscietiousness2)) - 1) / 9, digits = 2),
         personality_agreeableness = round((mean(c(Agreeableness1, Agreeableness2)) - 1) / 9, digits = 2),
         job_competence = round((mean(c(Competence1, Competence2, Competence3)) - 1) / 9, digits = 2),
         self_determination = round((mean(c(SelfDetermination1, SelfDetermination2, SelfDetermination3)) - 1) / 9, digits = 2),
         creative_self_efficacy = round((mean(c(Creativity1, Creativity2, Creativity3, Creativity4)) - 1) / 9, digits = 2),
         amotivation = round((mean(c(Amotivation1, Amotivation2, Amotivation3)) - 1) / 9, digits = 2),
         extrinsic_regulation_social = round((mean(c(ExtrinsicRegulationSocial1, ExtrinsicRegulationSocial2, ExtrinsicRegulationSocial3)) - 1) / 9, digits = 2),
         extrinsic_regulation_material = round((mean(c(ExtrinsicRegulationMaterial1, ExtrinsicRegulationMaterial2, ExtrinsicRegulationMaterial3)) - 1) / 9, digits = 2),
         introjected_regulation = round((mean(c(IntrojectedRegulation1, IntrojectedRegulation2, IntrojectedRegulation3, IntrojectedRegulation4)) - 1) / 9, digits = 2),
         identified_regulation = round((mean(c(IdentifiedRegulation1, IdentifiedRegulation2, IdentifiedRegulation3)) - 1) / 9, digits = 2),
         intrinsic_motivation = round((mean(c(IntrinsicMotivation1, IntrinsicMotivation2, IntrinsicMotivation3)) -1 ) / 9, digits =  2),
         identification_group = round((identification_group - 1) / 9, digits = 2),
         identification_org = round((identification_org - 1) / 9, digits = 2),
         identification_collab = round((identification_collab - 1) / 9, digits = 2),
         controlled_motivation = round(mean(c(extrinsic_regulation_social, extrinsic_regulation_material, introjected_regulation)), digits = 2),
         autonomous_motivation = round(mean(c(identified_regulation, intrinsic_motivation)), digits = 2)) %>%
  # subset aggregated scale items 
  dplyr::select(case, id, name, gender, age, work_location, education_level, broad_education_field, occupation_class, work_experience, current_tenure,
         personality_openness, personality_conscientiousness, personality_agreeableness, job_competence, creative_self_efficacy, job_competence,
         amotivation, extrinsic_regulation_social, extrinsic_regulation_material, introjected_regulation, identified_regulation, intrinsic_motivation,
         identification_group, identification_org, identification_collab, controlled_motivation, autonomous_motivation) %>%
  # add organisation affiation
  left_join(affiliation, by = c("case" = "case", "name" = "name")) 


# geocode nodes

require(ggmap)

register_google(key = Sys.getenv("GeocodingAPI"))

nodes_geocode <- nodes_rescaled %>%
  # add country information for non-Australian residents
  mutate(country = case_when(name == "Gerd Uitdewilligen" ~ "United States",
                             name == "Ron Mulder" ~ "New Zealand",
                             name == "Martin Paley" ~ "New Zealand",
                             name == "Max Suckling" ~ "New Zealand",
                             name == "Flore Mas" ~ "New Zealand",
                             name == "Taylor Welsh" ~ "New Zealand",
                             name == "Rachael Horner" ~ "New Zealand",
                             name == "Kerstin Vollmer" ~ "Sweden",
                             name == "Mikael Kartunen" ~ "Sweden",
                             name == "Martin Palmqvist" ~ "Sweden",
                             name == "Mauricio Quesada" ~ "Mexico",
                             name == "Oliverio Delgado-Carrillo" ~ "Mexico",
                             name == "Tobias Landmann" ~ "Kenya",
                             name == "Pamela Ochungo" ~ "Kenya",
                             name == "David Makori" ~ "Kenya",
                             name == "Nkoba Kiatoko" ~ "Kenya",
                             name == "Akira Nagase" ~ "Japan",
                             name == "Gustavo Pessin" ~ "Brazil",
                             name == "Giorgio Venturieri" ~ "Brazil",
                             name == "Daniel Santiago Pereira" ~ "Brazil",
                             name == "Stephan Carvalho" ~ "Brazil",
                             TRUE ~ "Australia"),
         # insert missing postcode
         work_location = if_else(name == "Kerstin Vollmer", "SE-14721", work_location)) %>%
  # generate postcode + country variable
  unite(place, work_location, country, sep = " ") %>%
  # geocode place
  mutate_geocode(place, sensor = F, output = "latlon", source = "google", force = T) 

# *************** process edge data ***************** # 

# read in raw edge data  

url <- "https://bitbucket.csiro.au/projects/ALT/repos/phd_data/raw/case_1/surveydata_case_1.xlsx"
GET(url, creds, write_disk(tf <- tempfile(fileext = ".xlsx")))
edge_data_case_1 <- read_excel(tf, sheet = 2)

url <- "https://bitbucket.csiro.au/projects/ALT/repos/phd_data/raw/case_2/surveydata_case_2.xlsx"
GET(url, creds, write_disk(tf <- tempfile(fileext = ".xlsx")))
edge_data_case_2 <- read_excel(tf, sheet = 2)

url <- "https://bitbucket.csiro.au/projects/ALT/repos/phd_data/raw/case_3/surveydata_case_3.xlsx"
GET(url, creds, write_disk(tf <- tempfile(fileext = ".xlsx")))
edge_data_case_3 <- read_excel(tf, sheet = 2)

# merge edge data

edges <- bind_rows(edge_data_case_1 %>% mutate(case = 1),
                   edge_data_case_2 %>% mutate(case = 2),
                   edge_data_case_3 %>% mutate(case = 3))

# aggregate and rescale tacitness

edges_rescale <- edges %>%
  mutate_all(as.numeric, as.character) %>%
  rowwise() %>%
  mutate(Codified = 10 - Codified,
         tacitness = case_when(relationship_set_knowledge_sharing == 1 ~ round((mean(c(Codified, Complexity, Observability)) - 1) / 9, digits = 2))) %>%
  select(-c(ownernode, Codified, Complexity, Observability))

#  calculate edge distance

require(geosphere)

coords <- nodes_geocode %>% select(case, id, lat, lon)

edges_dist <- edges_rescale %>%
  # create long data set
  gather(network, 
         flag, 
         c(relationship_set_knowledge_sharing,
           relationship_set_idea_generation,
           relationship_set_idea_realisation,
           relationship_set_affectbased_trust,
           relationship_set_cognitionbased_trust,
           relationship_set_prior_relationships,
           relationship_set_managers))%>%
  # remove redundant data
  filter(flag == 1) %>%
  # geocode from, to
  inner_join(coords %>% rename(from_lat = lat, from_lon = lon), by = c("from" = "id", "case")) %>%
  inner_join(coords %>% rename(to_lat = lat, to_lon = lon), by = c("to" = "id", "case")) %>%
  # do calculations per row 
  rowwise() %>%
  # calculate great circle distance and threshold tacitness
  mutate(distance = round(distHaversine(c(from_lon, from_lat), c(to_lon, to_lat)) / 1000, 0)) %>%
  # rename networks
  mutate(network = str_replace(network, "relationship_set_", ""),
         network = if_else(network == "knowledge_sharing" & tacitness >= 0.5, "predominantly_tacit_knowledge_provider", network),
         network = if_else(network == "knowledge_sharing" & tacitness < 0.5, "predominantly_explicit_knowledge_provider", network),
         network = if_else(network == "idea_generation", "idea_provider", network)) %>%
  # subset columns
  select(case, from, to, network, tacitness, distance) 

# ***************** build networks ****************** # 

require(tidygraph)

# case 1

nodes_case_1 <- nodes_geocode %>%
  filter(case == 1) %>%
  select(-c(case, place)) %>%
  mutate(id = as.character(id))

edges_case_1 <- edges_dist %>%
  filter(case == 1) %>%
  mutate_at(vars(to, from), as.character) %>%
  select(-case) 

network_case_1 <- tbl_graph(nodes = nodes_case_1, edges = edges_case_1, directed = T) %>%
  activate(edges) %>%
  # reverse edges
  reroute(from = if_else(network == "predominantly_tacit_knowledge_provider", to, from),
          to = if_else(network == "predominantly_tacit_knowledge_provider", from, to)) %>%
  reroute(from = if_else(network == "predominantly_explicit_knowledge_provider", to, from),
          to = if_else(network == "predominantly_explicit_knowledge_provider", from, to)) %>%
  reroute(from = if_else(network == "idea_provider", to, from),
          to = if_else(network == "idea_provider", from, to)) 

# case 2

nodes_case_2 <- nodes_geocode %>%
  filter(case == 2) %>%
  select(-c(case, place)) %>%
  mutate(id = as.character(id))

edges_case_2 <- edges_dist %>%
  filter(case == 2) %>%
  mutate_at(vars(to, from), as.character) %>%
  # remove orphan edges
  inner_join(nodes_case_2, by = c("from" = "id", "to" = "id")) %>%
  select(-case) 

network_case_2 <- tbl_graph(nodes = nodes_case_2, edges = edges_case_2, directed = T) %>%
  activate(edges) %>%
  # reverse edges
  reroute(from = if_else(network == "predominantly_tacit_knowledge_provider", to, from),
          to = if_else(network == "predominantly_tacit_knowledge_provider", from, to)) %>%
  reroute(from = if_else(network == "predominantly_explicit_knowledge_provider", to, from),
          to = if_else(network == "predominantly_explicit_knowledge_provider", from, to)) %>%
  reroute(from = if_else(network == "idea_provider", to, from),
          to = if_else(network == "idea_provider", from, to)) 

# case 3

nodes_case_3 <- nodes_geocode %>%
  filter(case == 3) %>%
  select(-c(case, place)) %>%
  mutate(id = as.character(id))

edges_case_3 <- edges_dist %>%
  filter(case == 3) %>%
  mutate_at(vars(to, from), as.character) %>%
  # remove orphan edges
  inner_join(nodes_case_3, by = c("from" = "id", "to" = "id")) %>%
  select(-case) 

network_case_3 <- tbl_graph(nodes = nodes_case_3, edges = edges_case_3, directed = T) %>%
  activate(edges) %>%
  # reverse edges
  reroute(from = if_else(network == "predominantly_tacit_knowledge_provider", to, from),
          to = if_else(network == "predominantly_tacit_knowledge_provider", from, to)) %>%
  reroute(from = if_else(network == "predominantly_explicit_knowledge_provider", to, from),
          to = if_else(network == "predominantly_explicit_knowledge_provider", from, to)) %>%
  reroute(from = if_else(network == "idea_provider", to, from),
          to = if_else(network == "idea_provider", from, to)) 


# *********** generate proximity matrices *********** # 

# case 1

geoproximity_case_1 <- network_case_1 %>%
  activate(nodes) %>%
  as.tibble() %>%
  # create two new columns with the same ids
  mutate(id1 = id, id2 = id) %>%
  # expand into all combinations of names
  expand(id1, id2) %>%
  # remove id1 equals to id2
  filter(id1 != id2) %>%
  # add lon, lat coords for id1
  left_join(network_case_1 %>% 
              activate(nodes) %>% 
              as.tibble() %>% 
              select(id, lon, lat) %>%
              rename(from_lat = lat, from_lon = lon), by = c("id1" = "id")) %>%
  # add lon, lat coords for id2
  left_join(network_case_1 %>% 
              activate(nodes) %>% 
              as.tibble() %>% 
              select(id, lon, lat) %>%
              rename(to_lat = lat, to_lon = lon), by = c("id2" = "id")) %>%
  rowwise() %>%
  # compute great circle distance
  mutate(distance = round(distHaversine(c(from_lon, from_lat), c(to_lon, to_lat)) / 1000, 0),
         log_distance = round(log1p(distance), 4)) %>%
  rename(from = id1, to = id2) %>%
  arrange(as.numeric(from), as.numeric(to)) %>%
  select(from, to, distance, log_distance) %>%
  as_tbl_graph(directed = F)

# case 2

geoproximity_case_2 <- network_case_2 %>%
  activate(nodes) %>%
  as.tibble() %>%
  select(id) %>%
  # create two new columns with the same ids
  mutate(id1 = id, id2 = id) %>%
  # expand into all combinations of names
  expand(id1, id2) %>%
  # remove id1 equals to id2
  filter(id1 != id2) %>%
  # add lon, lat coords for id1
  left_join(network_case_2 %>% 
              activate(nodes) %>% 
              as.tibble() %>% 
              select(id, lon, lat) %>%
              rename(from_lat = lat, from_lon = lon), by = c("id1" = "id")) %>%
  # add lon, lat coords for id2
  left_join(network_case_2 %>% 
              activate(nodes) %>% 
              as.tibble() %>% 
              select(id, lon, lat) %>%
              rename(to_lat = lat, to_lon = lon), by = c("id2" = "id")) %>%
  rowwise() %>%
  # compute great circle distance
  mutate(distance = round(distHaversine(c(from_lon, from_lat), c(to_lon, to_lat)) / 1000, 0),
         log_distance = round(log1p(distance), 4)) %>%
  rename(from = id1, to = id2) %>%
  arrange(as.numeric(from), as.numeric(to)) %>%
  select(from, to, distance, log_distance) %>%
  as_tbl_graph(directed = F)

# case 3

geoproximity_case_3 <- network_case_3 %>%
  activate(nodes) %>%
  as.tibble() %>%
  select(id) %>%
  # create two new columns with the same ids
  mutate(id1 = id, id2 = id) %>%
  # expand into all combinations of names
  expand(id1, id2) %>%
  # remove id1 equals to id2
  filter(id1 != id2) %>%
  # add lon, lat coords for id1
  left_join(network_case_3 %>% 
              activate(nodes) %>% 
              as.tibble() %>% 
              select(id, lon, lat) %>%
              rename(from_lat = lat, from_lon = lon), by = c("id1" = "id")) %>%
  # add lon, lat coords for id2
  left_join(network_case_3 %>% 
              activate(nodes) %>% 
              as.tibble() %>% 
              select(id, lon, lat) %>%
              rename(to_lat = lat, to_lon = lon), by = c("id2" = "id")) %>%
  rowwise() %>%
  # compute great circle distance
  mutate(distance = round(distHaversine(c(from_lon, from_lat), c(to_lon, to_lat)) / 1000, 0),
         log_distance = round(log1p(distance), 4)) %>%
  rename(from = id1, to = id2) %>%
  arrange(as.numeric(from), as.numeric(to)) %>%
  select(from, to, distance, log_distance) %>%
  as_tbl_graph(directed = F)

 
# ************* save pre-processed data ************* #

save(nodes_geocode, 
     edges_dist, 
     network_case_1, 
     network_case_2, 
     network_case_3, 
     geoproximity_case_1, 
     geoproximity_case_2, 
     geoproximity_case_3, 
     file = "~/ownCloud/phd_data/pre_processed_data.RData")

unlink(tf)

# *************** end of script ********************* #







