#####################################################
#                                                   #
#               R script to perform                 #
#        Gould-Fernandez brokerage analysis         #
#               Version 2019-08-08                  #
#                                                   #
#####################################################


# load preprocessed data

load("~/ownCloud/phd_data/pre_processed_data.RData")
# load("//OSM-05-MEL/OSM_MEL_DPS_OI_Network_work/ownCloud/phd_data/pre_processed_data.RData")

# perform GF analysis

require(tidyverse)
require(tidygraph)
require(intergraph)
require(sna)

# case 1

gf_tk_1 <- network_case_1 %>%
  activate(edges) %>%
  # extract predominantly tacit knowledge provider network
  filter(network == "predominantly_tacit_knowledge_provider") %>%
  # convert to sna graph
  asNetwork() %>%
  # compute brokerage
  brokerage(. %v% "org_affiliation")

gf_ek_1 <- network_case_1 %>%
  activate(edges) %>%
  # extract predominantly explicit knowledge provider network
  filter(network == "predominantly_explicit_knowledge_provider") %>%
  # convert to sna graph
  asNetwork() %>%
  # compute brokerage
  brokerage(. %v% "org_affiliation")

# case 2
              
gf_tk_2 <- network_case_2 %>%
  activate(edges) %>%
  # extract predominantly tacit knowledge provider network
  filter(network == "predominantly_tacit_knowledge_provider") %>%
  # convert to sna graph
  asNetwork() %>%
  # compute brokerage
  brokerage(. %v% "org_affiliation")

gf_ek_2 <- network_case_2 %>%
  activate(edges) %>%
  # extract predominantly explicit knowledge provider network
  filter(network == "predominantly_explicit_knowledge_provider") %>%
  # convert to sna graph
  asNetwork() %>%
  # compute brokerage
  brokerage(. %v% "org_affiliation")

# case 3

gf_tk_3 <- network_case_3 %>%
  activate(edges) %>%
  # extract predominantly tacit knowledge provider network
  filter(network == "predominantly_tacit_knowledge_provider") %>%
  # convert to sna graph
  asNetwork() %>%
  # compute brokerage
  brokerage(. %v% "org_affiliation")

gf_ek_3 <- network_case_3 %>%
  activate(edges) %>%
  # extract predominantly tacit knowledge provider network
  filter(network == "predominantly_explicit_knowledge_provider") %>%
  # convert to sna graph
  asNetwork() %>%
  # compute brokerage
  brokerage(. %v% "org_affiliation")

# synthesise gf data

gf_1_tk <- gf_tk_1$raw.nli %>% 
  as_tibble() %>%
  bind_cols(network_case_1 %>% activate(nodes) %>% select(id, name) %>% as_tibble()) %>%
  mutate(case = 1, network = "Tacit knowledge") 

gf_1_ek <- gf_ek_1$raw.nli %>%
  as_tibble() %>%
  bind_cols(network_case_1 %>% activate(nodes) %>% select(id, name) %>% as_tibble()) %>%
  mutate(case = 1, network = "Explicit knowledge")

gf_2_tk <- gf_tk_2$raw.nli %>% 
  as_tibble() %>%
  bind_cols(network_case_2 %>% activate(nodes) %>% select(id, name) %>% as_tibble()) %>%
  mutate(case = 2, network = "Tacit knowledge") 

gf_2_ek <- gf_ek_2$raw.nli %>% 
  as_tibble() %>%
  bind_cols(network_case_2 %>% activate(nodes) %>% select(id, name) %>% as_tibble()) %>%
  mutate(case = 2, network = "Explicit knowledge")

gf_3_tk <- gf_tk_3$raw.nli %>% 
  as_tibble() %>%
  bind_cols(network_case_3 %>% activate(nodes) %>% select(id, name) %>% as_tibble()) %>%
  mutate(case = 3, network = "Tacit knowledge") 

gf_3_ek <- gf_ek_3$raw.nli %>%
  as_tibble() %>%
  bind_cols(network_case_3 %>% activate(nodes) %>% select(id, name) %>% as_tibble()) %>%
  mutate(case = 3, network = "Explicit knowledge")

gf <- rbind(gf_1_tk, gf_1_ek, gf_2_tk, gf_2_ek, gf_3_tk, gf_3_ek)

# plot graphs

# set up plot

gf_gather <- gf %>%
  gather(pattern, count, c(w_I, w_O, b_IO, b_OI, b_O)) %>%
  select(case, pattern, count, network) %>%
  group_by(case, network, pattern) %>%
  summarise(freq = sum(count)) %>%
  mutate(freq = if_else(network == "Explicit knowledge", -freq, freq))

gf_temp <- gf_gather %>%
  filter(network == "Tacit knowledge", case == 2) %>%
  arrange(freq)

the_order <- gf_temp$pattern


# generate plot

g <- gf_gather %>%
  ggplot(aes(x = pattern, y = freq, group = network, fill = network)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_x_discrete(limits = the_order, 
                   labels = c("b_O" = expression(italic(b)[O]), 
                              "b_OI" = expression(italic(b)[OI]),
                              "b_IO" = expression(italic(b)[IO]),
                              "w_O" = expression(italic(w)[O]),
                              "w_I" = expression(italic(w)[I]))) +
  scale_fill_brewer(palette = "Set1") +
  theme_fivethirtyeight() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
   facet_wrap(~case, 
              scales = "free",
              labeller = as_labeller(c("1" = "Case 1", 
                                       "2" = "Case 2", 
                                       "3" = "Case 3"))) 

ggsave("~/owncloud/phd_plots/gf_brokerage.png", width = 18, height = 12, units = "cm", dpi = 600, g)

# *************** end of script ********************* #

ek_sna_3 <- network_case_2 %>%
  activate(edges) %>%
  # extract predominantlyexplicit knowledge provider network
  filter(network == "predominantly_tacit_knowledge_provider") %>%
  # convert to sna graph
  asNetwork()

