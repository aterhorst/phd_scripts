#######################################################
#                                                     #
#       R script for exporting tidygraph data         #
#  to MPNet for ERGM analysis for Leuven University   #
#                 Version 2019-08-03                  #
#                (c) Andrew Terhorst                  #
#                                                     #
#######################################################

# load pre-processed data

load("~/ownCloud/phd_data/pre_processed_data.RData")

# extract and combine tacit and explicit knowledge networks

tk_1 <-network_case_1 %>%
  activate(edges) %>%
  filter(network == "predominantly_tacit_knowledge_provider") %>%
  igraph::get.adjacency(type = "both", names = F)

ek_1 <-network_case_1 %>%
  activate(edges) %>%
  filter(network == "predominantly_explicit_knowledge_provider") %>%
  igraph::get.adjacency(type = "both", names = F)

tek_1 <- tk_1 + ek_1

tk_2 <-network_case_2 %>%
  activate(edges) %>%
  filter(network == "predominantly_tacit_knowledge_provider") %>%
  igraph::get.adjacency(type = "both", names = F)

ek_2 <-network_case_2 %>%
  activate(edges) %>%
  filter(network == "predominantly_explicit_knowledge_provider") %>%
  igraph::get.adjacency(type = "both", names = F)

tek_2 <- tk_2 + ek_2

tk_3 <-network_case_3 %>%
  activate(edges) %>%
  filter(network == "predominantly_tacit_knowledge_provider") %>%
  igraph::get.adjacency(type = "both", names = F)

ek_3 <-network_case_3 %>%
  activate(edges) %>%
  filter(network == "predominantly_explicit_knowledge_provider") %>%
  igraph::get.adjacency(type = "both", names = F)

tek_3 <- tk_3 + ek_3

# create structural zeros

ones_1 <- matrix(1, nrow = 18, ncol = 18)
pad_right_zeros <- matrix(0, nrow = 18, ncol = 65)
ones_2 <- matrix(1, nrow = 25, ncol = 25)
pad_middle_left <- matrix(0, nrow = 25, ncol = 18)
pad_middle_right <- matrix(0, nrow = 25, ncol = 40)
ones_3 <- matrix(1, nrow = 40, ncol = 40)
pad_left_zeros <- matrix(0, nrow = 40, ncol = 43)

top_panel_mask = cbind(ones_1, pad_right_zeros)
middle_panel_mask = cbind(pad_middle_left, ones_2, pad_middle_right)
bottom_panel_mask = cbind(pad_left_zeros, ones_3)

structural_zeros_mask = rbind(top_panel_mask, middle_panel_mask, bottom_panel_mask)

MASS::write.matrix(structural_zeros_mask, file = "~/ownCloud/phd_data/structural_zeros_mask.txt")


# create big knowledge sharing network

top_panel_net = cbind(tek_1, pad_right_zeros)
middle_panel_net = cbind(pad_middle_left, tek_2, pad_middle_right)
bottom_panel_net = cbind(pad_left_zeros, tek_3)

combined_networks = rbind(top_panel_net, middle_panel_net, bottom_panel_net)

MASS::write.matrix(combined_networks, file = "~/ownCloud/phd_data/combined_networks.txt")


# extract continuous data

continuous_data_case_1 <- network_case_1 %>% 
  activate(nodes) %>%
  as.tibble() %>%
  select(amotivation,
         extrinsic_regulation_material,
         extrinsic_regulation_social,
         introjected_regulation,
         identified_regulation,
         intrinsic_motivation,
         controlled_motivation,
         autonomous_motivation)

continuous_data_case_2 <- network_case_2 %>% 
  activate(nodes) %>%
  as.tibble() %>%
  select(amotivation,
         extrinsic_regulation_material,
         extrinsic_regulation_social,
         introjected_regulation,
         identified_regulation,
         intrinsic_motivation,
         controlled_motivation,
         autonomous_motivation)

continuous_data_case_3 <- network_case_3 %>% 
  activate(nodes) %>%
  as.tibble() %>%
  select(amotivation,
         extrinsic_regulation_material,
         extrinsic_regulation_social,
         introjected_regulation,
         identified_regulation,
         intrinsic_motivation,
         controlled_motivation,
         autonomous_motivation)

continuous_data <- bind_rows(continuous_data_case_1,
                             continuous_data_case_2,
                             continuous_data_case_3)

write.table(continuous_data, "~/ownCloud/phd_data/continuous_data_all.txt", row.names = F, col.names = T, sep = "\t", quote = F)

