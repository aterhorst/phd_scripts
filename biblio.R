
#######################################################
#                                                     #
#       R script for generating open innovation       #
#                    bibliometrics                    #
#                 Version 2019-08-26                  #
#                (c) Andrew Terhorst                  #
#                                                     #
#######################################################

# Perform search in Scopus
# goto https://www.scopus.com/search/form.uri?display=basic
# run two queries:
# TITLE-ABS-KEY ("open innovation")
# TITLE-ABS-KEY ("tacit knowledge" AND open innovation")
# Analyse query results:
# https://www.scopus.com/term/analyzer.uri
# Export results as csv file

# read in Scopus search results

require(tidyverse)


oi <- read.csv("~/owncloud/innovation network analysis/bibliography/scopus-4938-analyze-year.csv", skip = 7, header = T) %>% 
  rename(Publications = X, Year = YEAR) %>%
  filter(Year >= 2003, Year < 2019)



tacit_oi <- read.csv("~/owncloud/innovation network analysis/bibliography/scopus-18-analyze-year.csv", skip = 7, header = T) %>% 
  rename(Publications = X, Year = YEAR) %>%
  filter(Year >= 2003, Year < 2019)

p <- ggplot(NULL, aes(x = Year, y = Publications)) + 
  geom_col(aes(fill = "oi"), data = oi, alpha = 0.6) +
  geom_col(aes(fill = "tacit_oi"), data = tacit_oi, alpha = 0.4)  +
  scale_y_log10() +
  scale_x_continuous(breaks = 2003:2018) +
  scale_fill_manual(values = c("#3399FF", "#FF33FF"), 
                    name = "Topic",
                    labels = c("Open Innovation", "Open Innovation & Tacit Knowledge")) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) + 
  xlab(NULL) +
  ylab(expression("Publications "*"("*"log"[10]*" scale)"))
  
ggsave("~/owncloud/phd_data/tk_oi.pdf", width = 9, height = 6, units = "in", p)

