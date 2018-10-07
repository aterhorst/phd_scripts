#######################################################
#                                                     #
#        R script for exploratory data analysis       #
#                 Version 2018-10-07                  #
#                (c) Andrew Terhorst                  #
#                                                     #
#######################################################


# plot networks

require(ggraph)

subset <- c("predominantly_tacit_knowledge_provider", 
            "predominantly_explicit_knowledge_provider", 
            "idea_provider")

ggraph(network_case_1 %>% activate(edges) %>% filter(network %in% subset), layout = "kk") +
  geom_edge_link(aes(color = distance), arrow = arrow(length = unit(4, 'mm')), end_cap = circle(3, 'mm')) +
  geom_node_point(size = 6) +
  geom_node_text(aes(label = id), colour = "white", size = 4) +
  theme_graph() + facet_wrap(~ network)