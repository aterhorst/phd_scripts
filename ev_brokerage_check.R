#######################################################
#                                                     #
#           R script to check E-V brokerage           #
#            is being correctly calculated            #
#               Version 2018-10-10                    #
#              (c) Andrew Terhorst                    #
#                                                     #
#######################################################

#' @article{everett2016bridging,
#'   title={Bridging, brokerage and betweenness},
#'   author={Everett, Martin G and Valente, Thomas W},
#'   journal={Social Networks},
#'   volume={44},
#'   pages={202--208},
#'   year={2016},
#'   publisher={Elsevier}
#' }

# **************  undirected network  ****************#

# create granovetter hypothetical network edgelist

edgelist <- data.frame(from = c(1,1,1,2,2,2,3,3,3,3,4,4,4,4,5,5,5,6,6,6,7,7,8,8,8,8,9,
                                         9,10,10,10,11,11,11,11,11,12,12,12,13,13,13,13,14,14,
                                         14,14,15,15,15,16,16,17,17,17,18,18,18,18,19,19,20,20,
                                         20,20,20,21,21,22,22,22,23,23,23,24,24,24,25,25,25,25),
                                to = c(2,3,24,1,3,4,1,2,4,5,2,3,5,6,3,4,6,5,5,7,6,8,9,10,11,
                                       14,8,10,9,8,11,10,8,12,14,13,11,14,13,11,12,14,15,8,11,
                                       12,13,13,16,17,15,17,15,16,18,17,19,20,21,18,20,19,18,
                                       21,25,22,18,20,20,25,23,24,25,22,1,25,23,24,23,22,20))
# create graph object

g <- igraph::graph_from_edgelist(as.matrix(edgelist), directed = F) %>% simplify()

# calculate EV brokerage score

g <- as_tbl_graph(g) %>%
  activate(nodes) %>%
  # compute brokerage
  mutate(betweenness = centrality_betweenness(),
         degree = centrality_degree(),
         ev_condition = if_else(betweenness != 0, betweenness * 2 + graph_order() - 1, betweenness),
         ev_brokerage = if_else(ev_condition != 0, ev_condition / degree, ev_condition))

# present results

g %>% as.tibble()


# ***************  directed network  *****************#

# load campnet data

require(RCurl)

campnet <- as.matrix(read.csv(text = getURL("https://raw.githubusercontent.com/kateto/Network_Analysis_R_Examples/master/Sample%20Data/campnet.csv"), header = T, row.names = 1, as.is = T))
campnet.attr <- read.csv(text = getURL("https://raw.githubusercontent.com/kateto/Network_Analysis_R_Examples/master/Sample%20Data/campnet-attr.csv"), header = T, as.is = T) 

# create graph object

require(tidygraph)

g <- as_tbl_graph(campnet, directed = TRUE) %>% activate(nodes) %>% left_join(campnet.attr, by = c("name" = "Name"))

# calculate EV brokerage score

g <- g %>%
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

# present results

g %>% as.tibble()

# *****************  end of script  ******************#

