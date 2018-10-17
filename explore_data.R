#######################################################
#                                                     #
#        R script for exploratory data analysis       #
#                 Version 2018-10-07                  #
#                (c) Andrew Terhorst                  #
#                                                     #
#######################################################


# load pre-processed data

load("~/ownCloud/phd_data/pre_processed_data.RData")

# ******** assess responses to scale items ********** #

source("https://raw.githubusercontent.com/janhove/janhove.github.io/master/RCode/sortLvls.R")

nodes_clean %>% 
  # select scale items
  select(14:52) %>%
  # reverse score items
  mutate(Openness2 = 10 - Openness2,
         Conscientiousness1 = 10 - Conscientiousness1,
         Agreeableness2 = 10 - Agreeableness2) %>%
  # rename columns
  rename(Conscientiousness2 = Conscietiousness2,
         identificationGroup = identification_group,
         identificationOrg = identification_org,
         identificationCollab = identification_collab) %>%
  # create long data set
  gather(question, response, 1:38) %>%
  mutate(question = factor(question),
         facets = factor(question, levels = c("Agreeableness1",
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
  group_by(facets, response, case) %>%
  count() %>%
  ggplot() + 
  geom_col(aes(x = response, y = n, fill = factor(case))) +
  facet_wrap(~ facets) +
  scale_x_continuous(breaks = c(1:10))

# ********* correlation between scale items ********** #

require(ggcorrplot)

# extract scale items

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
            lat))

# compute correlation matrix

corr <- round(cor(dat), 1)

# compute correlation matrix of p-values

p.mat <- cor_pmat(dat)

# visualise correlation matrix

ggcorrplot(corr, method = "circle", type = "upper")
ggcorrplot(corr, hc.order = TRUE, outline.col = "white")
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE)

ggcorrplot(corr, p.mat = p.mat, hc.order = TRUE,
           type = "lower", insig = "blank", method = "circle")


# ****** compute centrality/brokerage measures ****** #

require(tidygraph)

tacit_network_case_1 <- network_case_1 %>%
  activate(edges) %>%
  # extract predominantly tacit knowledge provider network
  filter(network == "predominantly_tacit_knowledge_provider") %>%
  # compute node measures (in-degree and out-degree centrality, EV brokerage score)
  activate(nodes) %>%
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

# plot networks

require(ggraph)

lo <- create_layout(idea_network_case_1, layout = "nicely")

p1 <- ggraph(tacit_network_case_1, layout = "manual", node.position = lo) +
  geom_edge_link(aes(color = log1p(distance)), arrow = arrow(length = unit(2, "mm"), type = "closed")) +
  geom_node_point(aes(size = ev_brokerage, color = factor(org_affiliation))) +
  geom_node_text(aes(label = id), size = 2) +
  theme_graph() +
  geom_label(x = min(lo$x), 
             y = max(lo$y),
             label = paste(paste("nodes = ", igraph::vcount(tacit_network_case_1)),
                           paste("edges = ", igraph::ecount(tacit_network_case_1)),
                           paste("graph density = ", round(igraph::graph.density(tacit_network_case_1), digits = 3)), sep = "\n"), 
             hjust = 0,
             label.padding = unit(0.5, "lines")) +
#  theme(legend.position = "none") +
  ggtitle("Predominantly Tacit Knowledge Provider")

p2 <- ggraph(explicit_network_case_1, layout = "manual", node.position = lo) +
  geom_edge_link(aes(color = log1p(distance)), arrow = arrow(length = unit(2, "mm"), type = "closed")) +
  geom_node_point(aes(size = ev_brokerage, color = factor(org_affiliation))) +
  geom_node_text(aes(label = id), size = 2) +
  theme_graph() +
  geom_label(x = min(lo$x), 
             y = max(lo$y),
             label = paste(paste("nodes = ", igraph::vcount(explicit_network_case_1)),
                           paste("edges = ", igraph::ecount(explicit_network_case_1)),
                           paste("graph density = ", round(igraph::graph.density(explicit_network_case_1), digits = 3)), sep = "\n"), 
             hjust = 0,
             label.padding = unit(0.5, "lines")) +
#  theme(legend.position = "none") +
  ggtitle("Predominantly Explicit Knowledge Provider")

p3 <- ggraph(idea_network_case_1, layout = "manual", node.position = lo) +
  geom_edge_link(aes(color = log1p(distance)), arrow = arrow(length = unit(2, "mm"), type = "closed")) +
  geom_node_point(aes(size = ev_brokerage, color = factor(org_affiliation))) +
  geom_node_text(aes(label = id), size = 2) +
  theme_graph() +
  geom_label(x = min(lo$x), 
             y = max(lo$y),
             label = paste(paste("nodes = ", igraph::vcount(idea_network_case_1)),
                           paste("edges = ", igraph::ecount(idea_network_case_1)),
                           paste("graph density = ", round(igraph::graph.density(idea_network_case_1), digits = 3)), sep = "\n"), 
             hjust = 0,
             label.padding = unit(0.5, "lines")) +
  ggtitle("Idea Provider")


  
  


