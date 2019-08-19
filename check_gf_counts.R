
# load campnet data

require(RCurl)

campnet <- as.matrix(read.csv(text = getURL("https://raw.githubusercontent.com/kateto/Network_Analysis_R_Examples/master/Sample%20Data/campnet.csv"), header = T, row.names = 1, as.is = T))
campnet.attr <- read.csv(text = getURL("https://raw.githubusercontent.com/kateto/Network_Analysis_R_Examples/master/Sample%20Data/campnet-attr.csv"), header = T, as.is = T) 

# create graph object

require(tidygraph)

g <- as_tbl_graph(campnet, directed = TRUE) %>% activate(nodes) %>% left_join(campnet.attr, by = c("name" = "Name"))

h <- g %>%
  asNetwork()

i <- brokerage(h, h %v% "Role")

aj <- g %>% igraph::get.adjacency(type = "both", names = F)

MASS::write.matrix(aj, file = "~/ownCloud/phd_data/campnet_mpnet.txt")

cat <- g %>%
  activate(nodes) %>%
  as_tibble() %>%
  select(Role)

write.table(cat, "~/ownCloud/phd_data/campnet_att.txt", row.names = F, col.names = T, sep = "\t", quote = F)


