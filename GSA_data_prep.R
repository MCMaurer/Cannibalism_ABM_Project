# goal here is to take data from database and create some tables with outcomes and parameters

library(tidyverse)
library(RSQLite)
library(DBI)
library(dbplyr)

con <- dbConnect(SQLite(), dbname = "Cann_ABM_GSA_1.sqlite3")

counts <- tbl(con, "counts")
params <- tbl(con, "params")
p <- params %>% arrange(run_number) %>% collect()
p[5076:5080,]
p <- p %>% distinct()

test_counts <- counts %>%
  filter(run_number %in% 1:10) %>%
  collect()

test_counts <- test_counts %>%
  mutate(total_pop = count_inf_juveniles + count_juveniles + count_infecteds + count_uninfecteds) %>% 
  mutate(total_inf = count_inf_juveniles + count_infecteds) %>% 
  mutate(inf_prop = total_inf / total_pop)

counts <- counts %>%
  mutate(total_pop = count_inf_juveniles + count_juveniles + count_infecteds + count_uninfecteds) %>% 
  mutate(total_inf = count_inf_juveniles + count_infecteds) %>% 
  mutate(inf_prop = total_inf / total_pop)


# time until extinction ---------------------------------------------------

# test version
te <- test_counts %>% 
  filter(total_pop == 0) %>% 
  group_by(run_number) %>% 
  summarise(first_extinct = min(step)) %>% 
  left_join(p)

# this finds the instances where time until extinction is 10,000+
full_runs <- unique(test_counts$run_number[!(test_counts$run_number %in% te$run_number)])

# this adds those instances back to the resulting dataframe
te <- data.frame(run_number = full_runs, first_extinct = rep(10000, length(full_runs))) %>% 
  left_join(p) %>% 
  rbind(te) %>% 
  arrange(run_number)

te

# full version

te <- counts %>% 
  filter(total_pop == 0) %>% 
  group_by(run_number) %>% 
  summarise(first_extinct = min(step)) %>% 
  collect() %>% 
  left_join(p)

# this finds the instances where time until extinction is 10,000+
all_runs <- 1:10000
full_runs <- all_runs[!(1:10000 %in% te$run_number)]
full_runs


# this adds those instances back to the resulting dataframe
te <- data.frame(run_number = full_runs, first_extinct = rep(10001, length(full_runs))) %>% 
  left_join(p) %>% 
  rbind(te) %>% 
  arrange(run_number)

saveRDS(te, "cleaned_data/time_extinct.rds")


# mean infection % --------------------------------------------------------

# test version
test_counts %>% 
  filter(total_pop > 0) %>% 
  group_by(run_number) %>% 
  summarise(mean_inf_prop = mean(inf_prop))

# full version

mip <- counts %>% 
  filter(total_pop > 0) %>% 
  group_by(run_number) %>% 
  summarise(mean_inf_prop = mean(inf_prop), med_inf_prop = median(inf_prop)) %>% 
  collect() %>% 
  left_join(p)

saveRDS(mip, "cleaned_data/mean_inf_prop.rds")

# mean/median juvenile prop --------------------------------------------------------

# test version
test_counts %>% 
  mutate(total_juv = count_inf_juveniles + count_juveniles) %>% 
  filter(total_pop > 0) %>% 
  group_by(run_number) %>% 
  summarise(mean_juv_prop = mean(total_juv / total_pop))

# full version
msr <- counts %>% 
  mutate(total_juv = count_inf_juveniles + count_juveniles) %>% 
  filter(total_pop > 0) %>% 
  group_by(run_number) %>% 
  summarise(mean_juv_prop = mean(total_juv / total_pop), med_juv_prop = median(total_juv / total_pop)) %>% 
  collect() %>% 
  left_join(p)

saveRDS(msr, "cleaned_data/mean_juv_prop.rds")

# mean/median pop size ---------------------------------------------------

# test version
test_counts %>% 
  filter(total_pop > 0) %>% 
  group_by(run_number) %>% 
  summarise(mean_pop = mean(total_pop), med_pop = median(total_pop))

# full version
mps <- counts %>% 
  filter(total_pop > 0) %>% 
  group_by(run_number) %>% 
  summarise(mean_pop = mean(total_pop), med_pop = median(total_pop)) %>% 
  collect() %>% 
  left_join(p)

saveRDS(mps, "cleaned_data/mean_pop_size.rds")

# ok something funky ------------------------------------------------------
length(all_runs)
all_runs[!(1:10000 %in% msr$run_number)]
# need to look at run 5078

p %>% 
  filter(run_number == 5078)

# there is no run 5078... not sure why this is, but I don't think it should matter. Each entry in the DB got it's run_number column DIRECTLY from the filename, and then it got split, so the parameter values and counts should still match up fine. 


# test plots --------------------------------------------------------------

myplot <- test_counts %>% 
  ggplot(aes(x = step, y = total_pop, color = factor(run_number, ordered = T))) +
  geom_line() +
  MCMsBasics::minimal_ggplot_theme() +
  scale_color_viridis_d()
plotly::ggplotly(myplot)    

