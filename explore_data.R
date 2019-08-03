#######################################################
#                                                     #
#        R script for exploratory data analysis       #
#                 Version 2018-10-07                  #
#                (c) Andrew Terhorst                  #
#                                                     #
#######################################################

# This script does four things:
# 1. It generates bar charts showing responses to survey scale items (all three cases)
# 2. It examines the correlation between survey items (all three cases)
# 3. It generates network diagrams for each case (tacit and explicit knowledge provider, idea provider)
# 4. It generates demographic plots (all three cases)


# ************ load pre-processed data ************** #

load("~/ownCloud/phd_data/pre_processed_data.RData")

# ******** assess responses to scale items ********** #

require(tidyverse)

nodes_clean %>% 
  # select likert scale items
  select(14:52) %>%
  # reverse score items
  mutate(Openness2 = 10 - Openness2,
         Conscientiousness1 = 10 - Conscientiousness1,
         Agreeableness2 = 10 - Agreeableness2) %>%
  # rename columns
  rename(identificationGroup = identification_group,
         identificationOrg = identification_org,
         identificationCollab = identification_collab) %>%
  # create long data set
  gather(item, response, 1:38) %>%
  # define factors, re-order likert items
  mutate(case = factor(case),
         item = factor(item, levels = c("Agreeableness1",
                                        "Agreeableness2",
                                        "Conscientiousness1",
                                        "Conscientiousness2",
                                        "Openness1",
                                        "Openness2",
                                        "Competence1",
                                        "Competence2",
                                        "Competence3",
                                        "Creativity1",
                                        "Creativity2",
                                        "Creativity3",
                                        "Creativity4",
                                        "SelfDetermination1",
                                        "SelfDetermination2",
                                        "SelfDetermination3",
                                        "identificationGroup",
                                        "identificationOrg",
                                        "identificationCollab",
                                        "Amotivation1",
                                        "Amotivation2",
                                        "Amotivation3",
                                        "ExtrinsicRegulationMaterial1",
                                        "ExtrinsicRegulationMaterial2",
                                        "ExtrinsicRegulationMaterial3",
                                        "ExtrinsicRegulationSocial1",
                                        "ExtrinsicRegulationSocial2",
                                        "ExtrinsicRegulationSocial3",
                                        "IntrojectedRegulation1",
                                        "IntrojectedRegulation2",
                                        "IntrojectedRegulation3",
                                        "IntrojectedRegulation4",
                                        "IdentifiedRegulation1",
                                        "IdentifiedRegulation2",
                                        "IdentifiedRegulation3",
                                        "IntrinsicMotivation1",
                                        "IntrinsicMotivation2",
                                        "IntrinsicMotivation3"))) %>% 
  # summarise likert responses
  group_by(item, response, case) %>%
  count() %>%
  # plot likert responses
  ggplot() + 
  geom_col(aes(x = response, y = n, fill = case)) +
  facet_wrap(~ item) +
  scale_x_continuous(breaks = c(1:10))

# ********* correlation between scale items ********** #

require(ggcorrplot)

# extract scale items

survey_scores <- nodes_geocode %>%
  select(-c(case, 
            id, 
            name, 
            gender, 
            age, 
            place, 
            education_level, 
            broad_education_field, 
            occupation_class, 
            work_experience,
            current_tenure,
            org_affiliation,
            lon,
            lat))

write.csv(survey_scores, "~/downloads/survey_scores.csv", row.names = F)


dat <- nodes_geocode %>% 
  select(-c(case, 
            id, 
            name, 
            gender, 
            age, 
            place, 
            education_level, 
            broad_education_field, 
            occupation_class, 
            work_experience,
            current_tenure,
            org_affiliation,
            lon,
            lat,
            autonomous_motivation,
            controlled_motivation))

# compute correlation matrix

corr <- round(cor(dat), 1)

# compute correlation matrix of p-values

p.mat <- cor_pmat(dat)

# visualise correlation matrix

ggcorrplot(corr, title = "Correlation matrix for survey items", lab=TRUE, p.mat = p.mat, sig.level = .05)

# *** calculate graph statistics and plot networks *** #

# case 1

# compute ev-brokerage case 1

require(tidygraph)

tacit_network_case_1 <- network_case_1 %>%
  activate(edges) %>%
  # extract predominantly tacit knowledge provider network
  filter(network == "predominantly_tacit_knowledge_provider") %>%
  # compute node measures (in-degree and out-degree centrality, EV brokerage score)
  activate(nodes) %>%
  arrange(org_affiliation) %>%
  # compute in-degree, out-degree, and betweenness centrality 
  mutate(betweenness = centrality_betweenness(),
         in_degree = centrality_degree(mode = "in"),
         out_degree = centrality_degree(mode = "out"),
         in_reach = local_size(order = graph_order(), mode = "in") - 1,
         out_reach = local_size(order = graph_order(), mode = "out") - 1) %>%
  # compute everett-valente brokerage score
  mutate(ev_in = if_else(betweenness != 0, betweenness + in_reach, betweenness),
         ev_in = if_else(ev_in != 0, ev_in / in_degree, ev_in),
         ev_out = if_else(betweenness != 0, betweenness + out_reach, betweenness),
         ev_out = if_else(ev_out != 0, ev_out / out_degree, ev_out),
         ev_brokerage = (ev_in + ev_out) / 2)


explicit_network_case_1 <- network_case_1 %>%
  activate(edges) %>%
  filter(network == "predominantly_explicit_knowledge_provider") %>%
  activate(nodes) %>%
  arrange(org_affiliation) %>%
  # compute in-degree, out-degree, and betweenness centrality 
  mutate(betweenness = centrality_betweenness(),
         in_degree = centrality_degree(mode = "in"),
         out_degree = centrality_degree(mode = "out"),
         in_reach = local_size(order = graph_order(), mode = "in") - 1,
         out_reach = local_size(order = graph_order(), mode = "out") - 1) %>%
  # compute everett-valente brokerage score
  mutate(ev_in = if_else(betweenness != 0, betweenness + in_reach, betweenness),
         ev_in = if_else(ev_in != 0, ev_in / in_degree, ev_in),
         ev_out = if_else(betweenness != 0, betweenness + out_reach, betweenness),
         ev_out = if_else(ev_out != 0, ev_out / out_degree, ev_out),
         ev_brokerage = (ev_in + ev_out) / 2) 


idea_network_case_1 <- network_case_1 %>%
  activate(edges) %>%
  filter(network == "idea_provider") %>%
  activate(nodes) %>%
  arrange(org_affiliation) %>%
  # compute in-degree, out-degree, and betweenness centrality 
  mutate(betweenness = centrality_betweenness(),
         in_degree = centrality_degree(mode = "in"),
         out_degree = centrality_degree(mode = "out"),
         in_reach = local_size(order = graph_order(), mode = "in") - 1,
         out_reach = local_size(order = graph_order(), mode = "out") - 1) %>%
  # compute everett-valente brokerage score
  mutate(ev_in = if_else(betweenness != 0, betweenness + in_reach, betweenness),
         ev_in = if_else(ev_in != 0, ev_in / in_degree, ev_in),
         ev_out = if_else(betweenness != 0, betweenness + out_reach, betweenness),
         ev_out = if_else(ev_out != 0, ev_out / out_degree, ev_out),
         ev_brokerage = (ev_in + ev_out) / 2) 

# plot case 1 networks

require(ggraph)
require(gridExtra)
require(ggpubr)

lo1 <- create_layout(idea_network_case_1, layout = "circle") %>% select(x,y)

p1 <- ggraph(explicit_network_case_1, layout = "manual", node.position = lo1) +
  geom_edge_link(aes(width = distance, alpha = ..index..), color = "dark grey") +
  geom_node_point(aes(size = ev_brokerage, color = factor(org_affiliation))) +
  geom_node_text(aes(label = id), size = 4, nudge_x = 0.07, nudge_y = -0.07) +
  theme_graph() +
  geom_label(x = min(lo1$x) - 0.06,
             y = max(lo1$y) - 0.04,
             label = paste(paste("nodes = ", igraph::vcount(explicit_network_case_1)),
                           paste("edges = ", igraph::ecount(explicit_network_case_1)),
                           paste("graph density = ", round(igraph::graph.density(explicit_network_case_1), digits = 3)), sep = "\n"),
             hjust = 0,
             size = 2,
             label.padding = unit(0.4, "lines")) +
  scale_edge_alpha("Edge direction", guide = "edge_direction") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), panel.border = element_rect(color = "black", fill = NA)) +
  ggtitle("Explicit Knowledge Provider")

p2 <- ggraph(tacit_network_case_1, layout = "manual", node.position = lo1) +
  geom_edge_link(aes(width = distance, alpha = ..index..), color = "dark grey") +
  geom_node_point(aes(size = ev_brokerage, color = factor(org_affiliation))) +
  geom_node_text(aes(label = id), size = 4, nudge_x = 0.07, nudge_y = -0.07) +
  theme_graph() +
  geom_label(x = min(lo1$x) - 0.06,
             y = max(lo1$y) - 0.04,
             label = paste(paste("nodes = ", igraph::vcount(tacit_network_case_1)),
                           paste("edges = ", igraph::ecount(tacit_network_case_1)),
                           paste("graph density = ", round(igraph::graph.density(tacit_network_case_1), digits = 3)), sep = "\n"),
             hjust = 0,
             size = 2,
             label.padding = unit(0.4, "lines")) +
  scale_edge_alpha("Edge direction", guide = "edge_direction") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), panel.border = element_rect(color = "black", fill = NA)) +
  ggtitle("Tacit Knowledge Provider")

p3 <- ggraph(idea_network_case_1, layout = "manual", node.position = lo1) +
  geom_edge_link(aes(width = distance, alpha = ..index..), color = "dark grey") +
  geom_node_point(aes(size = ev_brokerage, color = factor(org_affiliation))) +
  geom_node_text(aes(label = id), size = 4, nudge_x = 0.07, nudge_y = -0.07) +
  theme_graph() +
  geom_label(x = min(lo1$x) - 0.06,
             y = max(lo1$y) - 0.04,
             label = paste(paste("nodes = ", igraph::vcount(idea_network_case_1)),
                           paste("edges = ", igraph::ecount(idea_network_case_1)),
                           paste("graph density = ", round(igraph::graph.density(idea_network_case_1), digits = 3)), sep = "\n"),
             hjust = 0,
             size = 2,
             label.padding = unit(0.4, "lines")) +
  scale_edge_alpha("Edge direction", guide = "edge_direction") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), panel.border = element_rect(color = "black", fill = NA)) +
  ggtitle("Idea Contributor")

c1 <- arrangeGrob(p1, p2, p3, nrow = 1)

ggsave("~/owncloud/phd_plots/networks_case_1.png", width = 40, height = 15, units = "cm", dpi = 600, c1)

# case 2

# compute ev-brokerage case 2

tacit_network_case_2 <- network_case_2 %>%
  activate(edges) %>%
  # extract predominantly tacit knowledge provider network
  filter(network == "predominantly_tacit_knowledge_provider") %>%
  # compute node measures (in-degree and out-degree centrality, EV brokerage score)
  activate(nodes) %>%
  arrange(org_affiliation) %>%
  # compute in-degree, out-degree, and betweenness centrality 
  mutate(betweenness = centrality_betweenness(),
         in_degree = centrality_degree(mode = "in"),
         out_degree = centrality_degree(mode = "out"),
         in_reach = local_size(order = graph_order(), mode = "in") - 1,
         out_reach = local_size(order = graph_order(), mode = "out") - 1) %>%
  # compute everett-valente brokerage score
  mutate(ev_in = if_else(betweenness != 0, betweenness + in_reach, betweenness),
         ev_in = if_else(ev_in != 0, ev_in / in_degree, ev_in),
         ev_out = if_else(betweenness != 0, betweenness + out_reach, betweenness),
         ev_out = if_else(ev_out != 0, ev_out / out_degree, ev_out),
         ev_brokerage = (ev_in + ev_out) / 2)


explicit_network_case_2 <- network_case_2 %>%
  activate(edges) %>%
  filter(network == "predominantly_explicit_knowledge_provider") %>%
  activate(nodes) %>%
  arrange(org_affiliation) %>%
  # compute in-degree, out-degree, and betweenness centrality 
  mutate(betweenness = centrality_betweenness(),
         in_degree = centrality_degree(mode = "in"),
         out_degree = centrality_degree(mode = "out"),
         in_reach = local_size(order = graph_order(), mode = "in") - 1,
         out_reach = local_size(order = graph_order(), mode = "out") - 1) %>%
  # compute everett-valente brokerage score
  mutate(ev_in = if_else(betweenness != 0, betweenness + in_reach, betweenness),
         ev_in = if_else(ev_in != 0, ev_in / in_degree, ev_in),
         ev_out = if_else(betweenness != 0, betweenness + out_reach, betweenness),
         ev_out = if_else(ev_out != 0, ev_out / out_degree, ev_out),
         ev_brokerage = (ev_in + ev_out) / 2) 


idea_network_case_2 <- network_case_2 %>%
  activate(edges) %>%
  filter(network == "idea_provider") %>%
  activate(nodes) %>%
  arrange(org_affiliation) %>%
  # compute in-degree, out-degree, and betweenness centrality 
  mutate(betweenness = centrality_betweenness(),
         in_degree = centrality_degree(mode = "in"),
         out_degree = centrality_degree(mode = "out"),
         in_reach = local_size(order = graph_order(), mode = "in") - 1,
         out_reach = local_size(order = graph_order(), mode = "out") - 1) %>%
  # compute everett-valente brokerage score
  mutate(ev_in = if_else(betweenness != 0, betweenness + in_reach, betweenness),
         ev_in = if_else(ev_in != 0, ev_in / in_degree, ev_in),
         ev_out = if_else(betweenness != 0, betweenness + out_reach, betweenness),
         ev_out = if_else(ev_out != 0, ev_out / out_degree, ev_out),
         ev_brokerage = (ev_in + ev_out) / 2) 

# plot case 2 networks 

lo2 <- create_layout(idea_network_case_2, layout = "circle") %>% select(x,y)

p4 <- ggraph(explicit_network_case_2, layout = "manual", node.position = lo2) +
  geom_edge_link(aes(width = distance, alpha = ..index..), color = "dark grey") +
  geom_node_point(aes(size = ev_brokerage, color = factor(org_affiliation))) +
  geom_node_text(aes(label = id), size = 4, nudge_x = 0.07, nudge_y = -0.07) +
  theme_graph() +
  geom_label(x = min(lo1$x) - 0.06,
             y = max(lo1$y) - 0.02,
             label = paste(paste("nodes = ", igraph::vcount(explicit_network_case_2)),
                           paste("edges = ", igraph::ecount(explicit_network_case_2)),
                           paste("graph density = ", round(igraph::graph.density(explicit_network_case_2), digits = 3)), sep = "\n"),
             hjust = 0,
             size = 2,
             label.padding = unit(0.4, "lines")) +
  scale_edge_alpha("Edge direction", guide = "edge_direction") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), panel.border = element_rect(color = "black", fill = NA)) +
  ggtitle("Explicit Knowledge Provider")

p5 <- ggraph(tacit_network_case_2, layout = "manual", node.position = lo2) +
  geom_edge_link(aes(width = distance, alpha = ..index..), color = "dark grey") +
  geom_node_point(aes(size = ev_brokerage, color = factor(org_affiliation))) +
  geom_node_text(aes(label = id), size = 4, nudge_x = 0.07, nudge_y = -0.07) +
  theme_graph() +
  geom_label(x = min(lo1$x) - 0.06,
             y = max(lo1$y) - 0.02,
             label = paste(paste("nodes = ", igraph::vcount(tacit_network_case_2)),
                           paste("edges = ", igraph::ecount(tacit_network_case_2)),
                           paste("graph density = ", round(igraph::graph.density(tacit_network_case_2), digits = 3)), sep = "\n"),
             hjust = 0,
             size = 2,
             label.padding = unit(0.4, "lines")) +
  scale_edge_alpha("Edge direction", guide = "edge_direction") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), panel.border = element_rect(color = "black", fill = NA)) +
  ggtitle("Tacit Knowledge Provider")

p6 <- ggraph(idea_network_case_2, layout = "manual", node.position = lo2) +
  geom_edge_link(aes(width = distance, alpha = ..index..), color = "dark grey") +
  geom_node_point(aes(size = ev_brokerage, color = factor(org_affiliation))) +
  geom_node_text(aes(label = id), size = 4, nudge_x = 0.07, nudge_y = -0.07) +
  theme_graph() +
  geom_label(x = min(lo1$x) - 0.06,
             y = max(lo1$y) - 0.02,
             label = paste(paste("nodes = ", igraph::vcount(idea_network_case_2)),
                           paste("edges = ", igraph::ecount(idea_network_case_2)),
                           paste("graph density = ", round(igraph::graph.density(idea_network_case_2), digits = 3)), sep = "\n"),
             hjust = 0,
             size = 2,
             label.padding = unit(0.4, "lines")) +
  scale_edge_alpha("Edge direction", guide = "edge_direction") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), panel.border = element_rect(color = "black", fill = NA)) +
  ggtitle("Idea Contributor")

c2 <- arrangeGrob(p4, p5, p6, nrow = 1)

ggsave("~/owncloud/phd_plots/networks_case_2.png", width = 40, height = 15, units = "cm", dpi = 600, c2)


# case 3

# compute ev-brokerage case 3

tacit_network_case_3 <- network_case_3 %>%
  activate(edges) %>%
  # extract predominantly tacit knowledge provider network
  filter(network == "predominantly_tacit_knowledge_provider") %>%
  # compute node measures (in-degree and out-degree centrality, EV brokerage score)
  activate(nodes) %>%
  arrange(org_affiliation) %>%
  # compute in-degree, out-degree, and betweenness centrality 
  mutate(betweenness = centrality_betweenness(),
         in_degree = centrality_degree(mode = "in"),
         out_degree = centrality_degree(mode = "out"),
         in_reach = local_size(order = graph_order(), mode = "in") - 1,
         out_reach = local_size(order = graph_order(), mode = "out") - 1) %>%
  # compute everett-valente brokerage score
  mutate(ev_in = if_else(betweenness != 0, betweenness + in_reach, betweenness),
         ev_in = if_else(ev_in != 0, ev_in / in_degree, ev_in),
         ev_out = if_else(betweenness != 0, betweenness + out_reach, betweenness),
         ev_out = if_else(ev_out != 0, ev_out / out_degree, ev_out),
         ev_brokerage = (ev_in + ev_out) / 2)


explicit_network_case_3 <- network_case_3 %>%
  activate(edges) %>%
  filter(network == "predominantly_explicit_knowledge_provider") %>%
  activate(nodes) %>%
  arrange(org_affiliation) %>%
  # compute in-degree, out-degree, and betweenness centrality 
  mutate(betweenness = centrality_betweenness(),
         in_degree = centrality_degree(mode = "in"),
         out_degree = centrality_degree(mode = "out"),
         in_reach = local_size(order = graph_order(), mode = "in") - 1,
         out_reach = local_size(order = graph_order(), mode = "out") - 1) %>%
  # compute everett-valente brokerage score
  mutate(ev_in = if_else(betweenness != 0, betweenness + in_reach, betweenness),
         ev_in = if_else(ev_in != 0, ev_in / in_degree, ev_in),
         ev_out = if_else(betweenness != 0, betweenness + out_reach, betweenness),
         ev_out = if_else(ev_out != 0, ev_out / out_degree, ev_out),
         ev_brokerage = (ev_in + ev_out) / 2) 


idea_network_case_3 <- network_case_3 %>%
  activate(edges) %>%
  filter(network == "idea_provider") %>%
  activate(nodes) %>%
  arrange(org_affiliation) %>%
  # compute in-degree, out-degree, and betweenness centrality 
  mutate(betweenness = centrality_betweenness(),
         in_degree = centrality_degree(mode = "in"),
         out_degree = centrality_degree(mode = "out"),
         in_reach = local_size(order = graph_order(), mode = "in") - 1,
         out_reach = local_size(order = graph_order(), mode = "out") - 1) %>%
  # compute everett-valente brokerage score
  mutate(ev_in = if_else(betweenness != 0, betweenness + in_reach, betweenness),
         ev_in = if_else(ev_in != 0, ev_in / in_degree, ev_in),
         ev_out = if_else(betweenness != 0, betweenness + out_reach, betweenness),
         ev_out = if_else(ev_out != 0, ev_out / out_degree, ev_out),
         ev_brokerage = (ev_in + ev_out) / 2) 

#  plot case 3 networks 

lo3 <- create_layout(idea_network_case_3, layout = "circle") %>% select(x,y)

p7 <- ggraph(explicit_network_case_3, layout = "manual", node.position = lo3) +
  geom_edge_link(aes(width = distance, alpha = ..index..), color = "dark grey") +
  geom_node_point(aes(size = ev_brokerage, color = factor(org_affiliation))) +
  geom_node_text(aes(label = id), size = 4, nudge_x = 0.07, nudge_y = -0.07) +
  theme_graph() +
  geom_label(x = min(lo1$x) - 0.06,
             y = max(lo1$y) - 0.02,
             label = paste(paste("nodes = ", igraph::vcount(explicit_network_case_3)),
                           paste("edges = ", igraph::ecount(explicit_network_case_3)),
                           paste("graph density = ", round(igraph::graph.density(explicit_network_case_3), digits = 3)), sep = "\n"),
             hjust = 0,
             size = 2,
             label.padding = unit(0.4, "lines")) +
  scale_edge_alpha("Edge direction", guide = "edge_direction") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), panel.border = element_rect(color = "black", fill = NA)) +
  ggtitle("Explicit Knowledge Provider")

p8 <- ggraph(tacit_network_case_3, layout = "manual", node.position = lo3) +
  geom_edge_link(aes(width = distance, alpha = ..index..), color = "dark grey") +
  geom_node_point(aes(size = ev_brokerage, color = factor(org_affiliation))) +
  geom_node_text(aes(label = id), size = 4, nudge_x = 0.07, nudge_y = -0.07) +
  theme_graph() +
  geom_label(x = min(lo1$x) - 0.06,
             y = max(lo1$y) - 0.02,
             label = paste(paste("nodes = ", igraph::vcount(tacit_network_case_3)),
                           paste("edges = ", igraph::ecount(tacit_network_case_3)),
                           paste("graph density = ", round(igraph::graph.density(tacit_network_case_3), digits = 3)), sep = "\n"),
             hjust = 0,
             size = 2,
             label.padding = unit(0.4, "lines")) +
  scale_edge_alpha("Edge direction", guide = "edge_direction") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), panel.border = element_rect(color = "black", fill = NA)) +
  ggtitle("Tacit Knowledge Provider")

p9 <- ggraph(idea_network_case_3, layout = "manual", node.position = lo3) +
  geom_edge_link(aes(width = distance, alpha = ..index..), color = "dark grey") +
  geom_node_point(aes(size = ev_brokerage, color = factor(org_affiliation))) +
  geom_node_text(aes(label = id), size = 4, nudge_x = 0.07, nudge_y = -0.07) +
  theme_graph() +
  geom_label(x = min(lo1$x) - 0.06,
             y = max(lo1$y) - 0.02,
             label = paste(paste("nodes = ", igraph::vcount(idea_network_case_3)),
                           paste("edges = ", igraph::ecount(idea_network_case_3)),
                           paste("graph density = ", round(igraph::graph.density(idea_network_case_3), digits = 3)), sep = "\n"),
             hjust = 0,
             size = 2,
             label.padding = unit(0.4, "lines")) +
  scale_edge_alpha("Edge direction", guide = "edge_direction") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), panel.border = element_rect(color = "black", fill = NA)) +
  ggtitle("Idea Contributor")

c3 <- arrangeGrob(p7, p8, p9, nrow = 1)

ggsave("~/owncloud/phd_plots/networks_case_3.png", width = 40, height = 15, units = "cm", dpi = 600, c3)


# Plot demographics

require(ggthemes)
require(ggThemeAssist)

summary_stats <- nodes_geocode %>%
  group_by(case) %>%
  summarise(n = n(),
            min_age = min(age),
            max_age = max(age),
            age_range = max_age - min_age,
            med_age = median(age),
            min_exp = min(work_experience),
            max_exp = max(work_experience),
            exp_range = max_exp - min_exp,
            med_exp = median(work_experience),
            min_tenure = min(current_tenure),
            max_tenure = max(current_tenure),
            tenure_range = max_tenure - min_tenure,
            med_tenure = median(current_tenure))

case_id <- c("1" = "Case 1", 
             "2" = "Case 2", 
             "3" = "Case 3")

ed_level <- c("Secondary Education",
              "Certificate Level",
              "Diploma/Advanced Diploma",
              "Bachelors Degree",
              "Graduate Certificate/Diploma", 
              "Masters Degree",
              "Doctoral Degree")

ed_field <- c("Natural & Physical Sciences", 
              "Information Technology", 
              "Engineering & Related Technologies",
              "Architecture & Building", 
              "Agricultural, Environmental & Related Studies",
              "Health","Education", 
              "Management & Commerce", 
              "Society & Culture", 
              "Creative Arts",
              "Food, Hospitality & Personal Services", 
              "Mixed Field Programmes")


p10 <- nodes_geocode %>%
  select(case, id, age, work_experience, current_tenure) %>%
  gather(var, value, c(age, work_experience, current_tenure)) %>%
  ggplot() +
  geom_boxplot(aes(x = factor(var), y = value, fill = var), width = 0.25, outlier.color = NA) +
  geom_jitter(alpha = 0.3, aes(x=factor(var), y = value), color= "black",position = position_jitter(width = .05)) +
  scale_x_discrete(name = "", labels = c("Age", "Work \nexperience", "Job \ntenure")) +
  theme_fivethirtyeight() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ case, labeller = as_labeller(case_id))

ggsave("~/owncloud/phd_plots/age_demographics.png", width = 18, height = 12, units = "cm", dpi = 600, p10)

p11 <- nodes_geocode %>%
  select(case, education_level) %>%
  group_by(case) %>%
  count(education_level) %>%
  ggplot(aes(factor(education_level), n, fill = factor(education_level))) +
  geom_bar(stat = "identity", position = "dodge", color = "white") + 
  geom_text(aes(label = n), nudge_y = 0.7, size = 3) +
  coord_polar() +
  scale_y_sqrt(breaks = c(4,16,32)) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        legend.title.align = 0.5,
        plot.margin = grid::unit(c(0,0,0,0), "mm")) +
  guides(fill = guide_legend(ncol = 2, title.position = "top")) +
  scale_fill_discrete(name="EDUCATION LEVEL",
                      breaks=c(2:8),
                      labels= ed_level) +
  facet_wrap(~ case, labeller = as_labeller(case_id))

ggsave("~/owncloud/phd_plots/ed_level.png", width = 18, height = 12, units = "cm", dpi = 600, p11)

p12 <- nodes_geocode %>%
  select(case, broad_education_field) %>%
  group_by(case) %>%
  count(broad_education_field) %>%
  ggplot(aes(factor(broad_education_field), n, fill = factor(broad_education_field))) +
  geom_bar(stat = "identity", position = "dodge", color = "white") + 
  geom_text(aes(label = n), nudge_y = 0.7, size = 3) +
  coord_polar() +
  scale_y_sqrt(breaks = c(4,16,32)) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        legend.title.align = 0.5,
        plot.margin = grid::unit(c(0,0,0,0), "mm")) +
  guides(fill = guide_legend(ncol = 2, title.position = "top")) +
  scale_fill_discrete(name="EDUCATION FIELD",
                      breaks=c(1:12),
                      labels= ed_field) +
  facet_wrap(~ case, labeller = as_labeller(case_id)) 

ggsave("~/owncloud/phd_plots/ed_field.png", width = 18, height = 12, units = "cm", dpi = 600, p12)

# *************** end of script ********************* #
  
  


