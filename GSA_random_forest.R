# now actually doing the random forest stuff

library(tidyverse)
library(caret)
library(doParallel)

imp_to_tibble <- function(x){
  x_imp <- varImp(x) %>% 
    as(Class = "list")
  x_imp[[1]] %>% 
    as.data.frame() %>% 
    rownames_to_column("parameter") %>% 
    as_tibble() %>% 
    arrange(desc(Overall))
}
plot_imp <- function(x){
  imp_to_tibble(x) %>% 
    ggplot(aes(y = Overall, x = reorder(parameter, Overall))) +
    geom_point() +
    geom_segment(aes(xend=reorder(parameter, Overall), y=0, yend=Overall), size = 0.4) +
    coord_flip() +
    MCMsBasics::minimal_ggplot_theme()
}

tte <- readRDS("cleaned_data/time_extinct.rds")

preds <- names(tte)[-c(1,2)]
preds_together <- paste(preds, collapse = " + ")

# first up, time to extinction --------------------------------------------
formula <- paste0("first_extinct ~ ", preds_together)
formula <- as.formula(formula)


tte <- tte %>% 
  filter(run_number != 5078)


# cl <- makePSOCKcluster(3)
# registerDoParallel(cl)
# tte_rf <- train(formula, 
#                 data = tte,
#                  method = "rf",
#                  importance = TRUE)
# BRRR::skrrrahh()
# stopCluster(cl)
# saveRDS(tte_rf, "tte_rf.rds")

tte_rf <- read_rds("tte_rf.rds")

tte_var_imp <- varImp(tte_rf)

p1 <- plot_imp(tte_rf) + 
  ylab("ABM Parameter") +
  xlab("Random Forest Importance Score") +
  ggtitle("Time to Exctinction")
p1


# mean infection prop ---------------------------------------------------

mip <- read_rds("cleaned_data/mean_inf_prop.rds")
mip

formula <- paste0("mean_inf_prop ~ ", preds_together)
formula <- as.formula(formula)


mip <- mip %>% 
  filter(run_number != 5078)


# cl <- makePSOCKcluster(3)
# registerDoParallel(cl)
# mip_rf <- train(formula, 
#                 data = mip,
#                 method = "rf",
#                 importance = TRUE)
# BRRR::skrrrahh()
# stopCluster(cl)
# saveRDS(mip_rf, "mip_rf.rds")

mip_rf <- read_rds("mip_rf.rds")

varImp(mip_rf)



p2 <- plot_imp(mip_rf)+ 
  ylab("ABM Parameter") +
  xlab("Random Forest Importance Score") +
  ggtitle("Mean % Infected")
p2

# mean pop size ----------------------------------------------------------

mps <- read_rds("cleaned_data/mean_pop_size.rds")

mps

formula <- paste0("mean_pop ~ ", preds_together)
formula <- as.formula(formula)


mps <- mps %>% 
  filter(run_number != 5078)

# cl <- makePSOCKcluster(3)
# registerDoParallel(cl)
# mps_rf <- train(formula, 
#                 data = mps,
#                 method = "rf",
#                 importance = TRUE)
# BRRR::skrrrahh()
# stopCluster(cl)
# saveRDS(mps_rf, "mps_rf.rds")

mps_rf <- read_rds("mps_rf.rds")

varImp(mps_rf)

p3 <- plot_imp(mps_rf)+ 
  ylab("ABM Parameter") +
  xlab("Random Forest Importance Score") +
  ggtitle("Mean Pop Size")
p3

# mean juv prop -----------------------------------------------------------

mjp <- read_rds("cleaned_data/mean_juv_prop.rds")

mjp

formula <- paste0("mean_juv_prop ~ ", preds_together)
formula <- as.formula(formula)


mjp <- mjp %>% 
  filter(run_number != 5078)

# cl <- makePSOCKcluster(3)
# registerDoParallel(cl)
# mjp_rf <- train(formula, 
#                 data = mjp,
#                 method = "rf",
#                 importance = TRUE)
# BRRR::skrrrahh()
# stopCluster(cl)
# saveRDS(mjp_rf, "mjp_rf.rds")

mjp_rf <- read_rds("mjp_rf.rds")

p4 <- plot_imp(mjp_rf)+ 
  ylab("ABM Parameter") +
  xlab("Random Forest Importance Score") +
  ggtitle("Mean % Juveniles")
p4


imp_to_tibble(tte_rf) %>% mutate(metric = "Time to Extinction") %>% 
  rbind(imp_to_tibble(mip_rf) %>% mutate(metric = "Mean % Infected")) %>% 
  rbind(imp_to_tibble(mps_rf) %>% mutate(metric = "Mean Pop Size")) %>% 
  rbind(imp_to_tibble(mjp_rf) %>% mutate(metric = "Mean % Juvenile")) %>% 
  group_by(metric) %>% 
  arrange(metric, desc(Overall)) %>% 
  mutate(order = row_number()) %>%
  ggplot(aes(x = drlib::reorder_within(parameter, desc(order), metric), y = Overall)) +
  geom_point() +
  geom_segment(aes(xend=drlib::reorder_within(parameter, desc(order), metric), y=0, yend=Overall), size = 0.4) +
  drlib::scale_x_reordered() +
  ylab("Random Forest Importance Metric") +
  xlab("ABM Parameter") +
  coord_flip() +
  facet_wrap(~metric, scales = "free") +
  MCMsBasics::minimal_ggplot_theme() +
  theme(text = element_text(size = 18)) +
  theme(axis.text = element_text(size = 10)) +
  theme(panel.border = element_rect(colour = "black", fill = NA))
ggsave("RF_imp_all.tiff", width = 10, height = 5)

 
drlib::reorder_within()



  
  
  
  
  
  
