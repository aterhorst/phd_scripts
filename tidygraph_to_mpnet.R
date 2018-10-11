#######################################################
#                                                     #
#       R script for exporting tidygraph data         #
#             to MPNet for ERGM analayis              #
#                 Version 2018-10-07                  #
#                (c) Andrew Terhorst                  #
#                                                     #
#######################################################

# load pre-processed data

load("~/ownCloud/phd_data/pre_processed_data.RData")

require(tidyverse)
require(tidygraph)


# *******************   case 1   ******************* #

# export binary, continuous, and categorical actor attributes

continuous_data_case_1 <- network_case_1 %>% 
  activate(nodes) %>%
  as.tibble() %>%
  select(age,
         education_level,
         work_experience,
         current_tenure,
         personality_openness,
         personality_conscientiousness,
         personality_agreeableness,
         job_competence,
         creative_self_efficacy,
         amotivation,
         extrinsic_regulation_social,
         extrinsic_regulation_social,
         introjected_regulation,
         identified_regulation,
         intrinsic_motivation,
         controlled_motivation,
         autonomous_motivation,
         identification_group,
         identification_org,
         identification_collab)

write.table(continuous_data_case_1, "~/ownCloud/phd_data/case_1/continuous_data.txt", row.names = F, col.names = T, sep = "\t", quote = F)

categorical_data_case_1 <- network_case_1 %>%
  activate(nodes) %>%
  as.tibble() %>%
  select(broad_education_field,
         occupation_class,
         org_affiliation)

write.table(categorical_data_case_1, "~/ownCloud/phd_data/case_1/categorical_data.txt", row.names = F, col.names = T, sep = "\t", quote = F)

binary_data_case_1 <- network_case_1 %>%
  activate(nodes) %>%
  as.tibble() %>%
  select(gender)

write.table(binary_data_case_1, "~/ownCloud/phd_data/case_1/binary_data.txt", row.names = F, col.names = T, sep = "\t", quote = F)

# export adjacency matrices

# predominantly tacit knowledge provider network

adjaceny_matrix_tacit_case_1 <- network_case_1 %>%
  activate(edges) %>%
  filter(network == "predominantly_tacit_knowledge_provider") %>%
  igraph::get.adjacency(type = "both", names = F)

MASS::write.matrix(adjaceny_matrix_tacit_case_1, file = "~/ownCloud/phd_data/case_1/tacit_knowledge_net.txt")

# predominantly explicit knowledge provider network

adjaceny_matrix_explicit_case_1 <- network_case_1 %>%
  activate(edges) %>%
  filter(network == "predominantly_explicit_knowledge_provider") %>%
  igraph::get.adjacency(type = "both", names = F)

MASS::write.matrix(adjaceny_matrix_explicit_case_1, file = "~/ownCloud/phd_data/case_1/explicit_knowledge_net.txt")

# idea provider network

adjaceny_matrix_ideation_case_1 <- network_case_1 %>%
  activate(edges) %>%
  filter(network == "idea_provider") %>%
  igraph::get.adjacency(type = "both", names = F)

MASS::write.matrix(adjaceny_matrix_ideation_case_1, file = "~/ownCloud/phd_data/case_1/idea_provider_net.txt")

# idea realisation network

adjaceny_matrix_realisation_case_1 <- network_case_1 %>%
  activate(edges) %>%
  filter(network == "idea_realisation") %>%
  igraph::get.adjacency(type = "both", names = F)

MASS::write.matrix(adjaceny_matrix_realisation_case_1, file = "~/ownCloud/phd_data/case_1/idea_realisation_net.txt")

# affect-based trust network

adjaceny_matrix_affect_case_1 <- network_case_1 %>%
  activate(edges) %>%
  filter(network == "affectbased_trust") %>%
  igraph::get.adjacency(type = "both", names = F)

MASS::write.matrix(adjaceny_matrix_affect_case_1, file = "~/ownCloud/phd_data/case_1/affect_based_trust_net.txt")

# cognition-based trust network

adjaceny_matrix_cognition_case_1 <- network_case_1 %>%
  activate(edges) %>%
  filter(network == "cognitionbased_trust") %>%
  igraph::get.adjacency(type = "both", names = F)

MASS::write.matrix(adjaceny_matrix_cognition_case_1, file = "~/ownCloud/phd_data/case_1/cognition_based_trust_net.txt")

# prior relationships network

adjaceny_matrix_prior_case_1 <- network_case_1 %>%
  activate(edges) %>%
  filter(network == "prior_relationships") %>%
  igraph::get.adjacency(type = "both", names = F)

MASS::write.matrix(adjaceny_matrix_prior_case_1, file = "~/ownCloud/phd_data/case_1/prior_relationships_net.txt")

# reporting network

adjaceny_matrix_report_case_1 <- network_case_1 %>%
  activate(edges) %>%
  filter(network == "managers") %>%
  igraph::get.adjacency(type = "both", names = F)

MASS::write.matrix(adjaceny_matrix_report_case_1, file = "~/ownCloud/phd_data/case_1/report_to_net.txt")

# geoproximity network

distance_matrix_case_1 <- geoproximity_case_1 %>%
  activate(edges) %>% 
  igraph::get.adjacency(sparse = F, attr = "log_distance", type = "both", names = F)

MASS::write.matrix(distance_matrix_case_1, file = "~/ownCloud/phd_data/case_1/distance_net.txt")

# export dyadic co-variates

fn <- "~/ownCloud/phd_data/case_1/dyadic_covariates.txt" 
cat("", file = fn) # create empty file
cat("tacit_knowledge_net \n", file = fn, append = T)
write.table(as.data.frame(as.matrix(adjaceny_matrix_tacit_case_1)), file = fn, append = T, sep = "\t", row.names = F, col.names = F)
cat("explicit_knowledge_net \n", file = fn, append = T)
write.table(as.data.frame(as.matrix(adjaceny_matrix_explicit_case_1)), file = fn, append = T, sep = "\t", row.names = F, col.names = F)
cat("idea_provider_net \n", file = fn, append = T)
write.table(as.data.frame(as.matrix(adjaceny_matrix_ideation_case_1)), file = fn, append = T, sep = "\t", row.names = F, col.names = F)
cat("idea_realisation_net \n", file = fn, append = T)
write.table(as.data.frame(as.matrix(adjaceny_matrix_realisation_case_1)), file = fn, append = T, sep = "\t", row.names = F, col.names = F)
cat("affect_based_trust_net \n", file = fn, append = T)
write.table(as.data.frame(as.matrix(adjaceny_matrix_affect_case_1)), file = fn, append = T, sep = "\t", row.names = F, col.names = F)
cat("cognition_based_tust_net \n", file = fn, append = T)
write.table(as.data.frame(as.matrix(adjaceny_matrix_cognition_case_1)), file = fn, append = T, sep = "\t", row.names = F, col.names = F)
cat("prior_relationships_net \n", file = fn, append = T)
write.table(as.data.frame(as.matrix(adjaceny_matrix_prior_case_1)), file = fn, append = T, sep = "\t", row.names = F, col.names = F)
cat("report_to_net \n", file = fn, append = T)
write.table(as.data.frame(as.matrix(adjaceny_matrix_report_case_1)), file = fn, append = T, sep = "\t", row.names = F, col.names = F)
cat("log_geoproximity_net \n", file = fn, append = T)
write.table(as.data.frame(as.matrix(distance_matrix_case_1)), file = fn, append = T, sep = "\t", row.names = F, col.names = F)


# *******************   case 2   ******************* #
