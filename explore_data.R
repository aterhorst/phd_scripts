#######################################################
#                                                     #
#        R script for exploratory data analysis       #
#                 Version 2018-10-07                  #
#                (c) Andrew Terhorst                  #
#                                                     #
#######################################################


# load pre-processed data

load("~/ownCloud/phd_data/pre_processed_data.RData")

# ***** compute centrality/brokerage measures ******* #

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


  
  


