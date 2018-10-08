#######################################################
#                                                     #
#              R script to check E-V brokerage        #
#              is being correctly calculated          #
#                 Version 2018-10-07                  #
#                (c) Andrew Terhorst                  #
#                                                     #
#######################################################

# load campnet data

require(RCurl)

campnet <- as.matrix(read.csv(text = getURL("https://raw.githubusercontent.com/kateto/Network_Analysis_R_Examples/master/Sample%20Data/campnet.csv"), header = T, row.names = 1, as.is = T))
campnet.attr <- read.csv(text = getURL("https://raw.githubusercontent.com/kateto/Network_Analysis_R_Examples/master/Sample%20Data/campnet-attr.csv"), header = T, as.is = T) 

# create graph object

require(tidygraph)

g <- as_tbl_graph(campnet, directed = TRUE) %>% activate(nodes) %>% left_join(campnet.attr, by = c("name" = "Name"))

# compute node measures (in-degree and out-degree centrality, EV brokerage score)

g <- g %>%
  activate(nodes) %>%
  # compute in-degree, out-degree, and betweenness centrality 
  mutate(in_degree = centrality_degree(mode = "in"),
         out_degree = centrality_degree(mode = "out"),
         betweenness = centrality_betweenness(),
         in_reach = local_size(order = graph_order(), mode = "in") - 1,
         out_reach = local_size(order = graph_order(), mode = "out") - 1) %>%
  # compute everett-valente brokerage score
  mutate(ev_in = case_when(betweenness == 0 ~ if_else(betweenness + in_reach == 0,
                                                      betweenness + in_reach,
                                                      (betweenness + in_reach) / in_degree),
                           betweenness != 0 ~ betweenness / in_degree),
         ev_out = case_when(betweenness == 0 ~ if_else(betweenness + out_reach == 0,
                                                       betweenness + out_reach,
                                                       (betweenness + out_reach) / out_degree),
                            betweenness != 0 ~ betweenness / out_degree),
         ev_brokerage = (ev_in + ev_out) / 2) 

g %>% as.tibble()


