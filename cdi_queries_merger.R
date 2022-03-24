
cdi_queries <- read_csv("cdi_queries_Mar22_resolvedCL.csv") %>%
  mutate(object = basic_level,
         in_cdi = ifelse(is.na(CDIform), F, T))

wordlist <- read_csv("Data/in_cdi_Wordlist.csv") # Read in CDI wordlist that matches sibsdata$basic_level with words on the CDI

wordlist <- rbind(cdi_queries, wordlist) %>%
  distinct(basic_level, .keep_all = T) %>%
  write_csv("Data/in_cdi_Wordlist.csv")
         