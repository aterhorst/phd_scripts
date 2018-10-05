#######################################################
#                                                     #
#   R script for importing and cleaning survey data   #
#                 Version 2018-10-07                  #
#                (c) Andrew Terhorst                  #
#                                                     #
#######################################################

# read in raw node data

require(readxl)

nodes_case_1 <- read_excel("~/ownCloud/Innovation Network Analysis/Quantitative Data/Case 1/surveydata.xlsx", sheet = 1)
nodes_case_2 <- read_excel("~/ownCloud/Innovation Network Analysis/Quantitative Data/Case 2/surveydata.xlsx", sheet = 1)
nodes_case_3 <- read_excel("~/ownCloud/Innovation Network Analysis/Quantitative Data/Case 3/surveydata.xlsx", sheet = 1)

# merge node data

require(tidyverse)

nodes <- bind_rows(nodes_case_1 %>% mutate(case = 1) %>% rename(Occupation = Occupation1),
                   nodes_case_2 %>% mutate(case = 2),
                   nodes_case_3 %>% mutate(case = 3))

# preliminary cleaning

require(stringr)

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

reclass <- c(1, 5:6, 8, 11:12, 14:52)
nodes_clean[,reclass] %<>% lapply(function(x) as.numeric(as.character(x)))

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
         identification_group = round((identification_group - 1) / 9, digits =2),
         identification_org = round((identification_org - 1) / 9, digits =2),
         identification_collab = round((identification_collab - 1) / 9, digits =2),
         controlled_motivation = round(mean(c(extrinsic_regulation_social, extrinsic_regulation_material, introjected_regulation)), digits = 2),
         autonomous_motivation = round(mean(c(identified_regulation, intrinsic_motivation)), digits = 2)) %>%
  # subset data columns
  select(case, id, name, gender, age, work_location, education_level, broad_education_field, occupation_class, work_experience, current_tenure,
         personality_openness, personality_conscientiousness, personality_agreeableness, job_competence, creative_self_efficacy, job_competence,
         amotivation, extrinsic_regulation_social, extrinsic_regulation_material, introjected_regulation, identified_regulation, intrinsic_motivation,
         identification_group, identification_org, identification_collab, controlled_motivation, autonomous_motivation) %>%
  # remove rows containing NA values
  drop_na()
         

# geocode nodes

require(ggmap)

register_google(key = "AIzaSyCLSTfR7wUOB2QxMaoAwIrhvKNKVgrjF28")

nodes_geocode <- nodes_rescaled %>%
  # add country information for non-Australian residents
  mutate(country = case_when(name == "Gerd Uitdewilligen" ~ "United States",
                             name == "Ron Mulder" ~ "New Zealand",
                             name == "Martin Paley" ~ "New Zealand",
                             name == "Max Suckling" ~ "New Zealand",
                             name == "Flore Mas" ~ "New Zealand",
                             name == "Taylor Welsh" ~ "New Zealand",
                             name == "Rachael Horner" ~ "New Zealand",
                             name == "Ron Mulder" ~ "New Zealand",
                             name == "Ron Mulder" ~ "New Zealand",
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

# save processed node data

write.csv(nodes_geocode, file = "~/ownCloud/Innovation Network Analysis/Quantitative Data/geocoded_node_table.csv", row.names = F)

# read in raw edge data  

edges_case_1 <- read_excel("~/ownCloud/Innovation Network Analysis/Quantitative Data/Case 1/surveydata.xlsx", sheet = 2)
edges_case_2 <- read_excel("~/ownCloud/Innovation Network Analysis/Quantitative Data/Case 2/surveydata.xlsx", sheet = 2)
edges_case_3 <- read_excel("~/ownCloud/Innovation Network Analysis/Quantitative Data/Case 3/surveydata.xlsx", sheet = 2)

# merge edge data

edges <- bind_rows(edges_case_1 %>% mutate(case = 1),
                   edges_case_2 %>% mutate(case = 2),
                   edges_case_3 %>% mutate(case = 3))

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
           relationship_set_managers)) %>%
  filter(flag == 1) %>%
  # geocode from, to
  inner_join(coords %>% rename(from_lat = lat, from_lon = lon), by = c("from" = "id", "case")) %>%
  inner_join(coords %>% rename(to_lat = lat, to_lon = lon), by = c("to" = "id", "case")) %>%
  # do calculations per row 
  rowwise() %>%
  # calculate great circle distance
  mutate(distance = round(distHaversine(c(from_lon, from_lat), c(to_lon, from_lat)) / 1000, 0)) %>%
  # subset columns
  select(case, from, to, network, tacitness, distance)

# save processed edge data

write.csv(edges_dist, file = "~/ownCloud/Innovation Network Analysis/Quantitative Data/edge_table_dist.csv", row.names = F)

# build multilayer networks for each case


