
require(tidyverse)
require(tidyr)

# read in gould-fernadez results

case1 <- read.csv("~/owncloud/innovation network analysis/quantitative data/case 1/gould-fernandez.csv", stringsAsFactors = F) %>%
 mutate(case = 1, partner = n_distinct(employer))
case2 <- read.csv("~/owncloud/innovation network analysis/quantitative data/case 2/gould-fernandez.csv", stringsAsFactors = F) %>%
  mutate(case = 2, partner = n_distinct(employer))
case3 <- read.csv("~/owncloud/innovation network analysis/quantitative data/case 3/gould-fernandez.csv", stringsAsFactors = F) %>%
  mutate(case = 3, partner = n_distinct(employer))

# merge data

positions <- c("coordinator", "representative", "gatekeeper", "external_mediator", "liaison")


cases <- bind_rows(case1, case2, case3) %>%
  rename(gatekeeper = b_OI, representative = b_IO, external_mediator = w_O, coordinator = w_I, liaison = b_O) %>%
  mutate(net = factor(net, levels = c("Explicit Knowledge Provider", "Tacit Knowledge Provider", "Idea Contributor")),
         case = case_when(case == 1 ~ "Case 1", case == 2 ~ "Case 2", case == 3 ~ "Case 3")) %>%
  gather(key = broker_role, value = role_count, -c(name, partner, case, net)) %>%
  group_by(case, net, broker_role, partner) %>%
  summarise(n = n(),
            broker_role_count = sum(role_count),
            normalised_count = broker_role_count/n*partner) 


%>%
  group_by(case, employer) %>%
  summarise(p = n())

  ggplot() +
  geom_col(aes(x = broker_role, y = broker_role_count, fill = net), position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust =1)) +
  scale_x_discrete(limits = positions) +
  scale_y_continuous(limits = c(0,200)) +
  facet_wrap(~ case)



