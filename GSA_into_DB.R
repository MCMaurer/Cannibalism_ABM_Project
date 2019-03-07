# I have a total of 13GB of CSV files and need to figure out how to work with all of them without reading them into R's memory. I have a few ideas:
# 1) read_csv each file into R, send it through a manipulation pipeline, pull out the relevant data (extinction time, parameters, etc.), and only keep that info

# 2) read_csv each file into R, clean it up (basically just add a column for run number), then send this into an SQLite database that I can then pull from. This option

library(tidyverse)
library(RSQLite)
library(dbplyr)


# clean one csv -----------------------------------------------------------
# read first CSV into R, figure out how to clean it
directory <- "../../../../Volumes/My Passport for Mac/Cann_ABM/"
files <- list.files(directory)

d <- read_csv(paste0(directory, files[1]), skip = 6) %>% 
  rename(run_number = `[run number]`, step = `[step]`, count_uninfecteds = `count uninfecteds`, count_infecteds = `count infecteds`, count_inf_juveniles = `count inf-juveniles`, count_juveniles = `count juveniles`) %>% mutate(run_number = as.numeric(str_match(files[1], "t([0-9]+).")[,2]))

d_params <- d %>% filter(step == 1) %>% select(-c(step, count_inf_juveniles, count_juveniles, count_infecteds, count_uninfecteds))
d_params

d_counts <- d %>% select(run_number, step, count_inf_juveniles, count_juveniles, count_infecteds, count_uninfecteds)
d_counts

(object.size(d_counts) + object.size(d_params)) / object.size(d)


# create the original db --------------------------------------------------

#mydb <- src_sqlite("../../../../Volumes/My Passport for Mac/Cann_ABM/Cann_ABM_GSA_1.sqlite3", create = T)
#copy_to(mydb, d_counts, "counts", temporary = F)
#copy_to(mydb, d_params, "params", temporary = F)
?dbWriteTable

# this is how to access the DB
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "../../../../Volumes/My Passport for Mac/Cann_ABM/Cann_ABM_GSA_1.sqlite3")

cann_counts <- tbl(con, "counts")
cann_params <- tbl(con, "params")

cann_counts %>% filter(count_infecteds >= 30) %>% collect()



# write cleaning function -------------------------------------------------

clean_csv <- function(file, directory = "../../../../Volumes/My Passport for Mac/Cann_ABM/"){
  d <- read_csv(paste0(directory, file), skip = 6) %>% 
    rename(run_number = `[run number]`, step = `[step]`, count_uninfecteds = `count uninfecteds`, count_infecteds = `count infecteds`, count_inf_juveniles = `count inf-juveniles`, count_juveniles = `count juveniles`) %>% mutate(run_number = as.numeric(str_match(file, "t([0-9]+).")[,2]))
  
params <- d %>% 
  filter(step == 1) %>% 
  select(-c(step, count_inf_juveniles, count_juveniles, count_infecteds, count_uninfecteds))
  
counts <- d %>% 
  select(run_number, step, count_inf_juveniles, count_juveniles, count_infecteds, count_uninfecteds)

return(list(counts = counts, params = params))
}

files <- list.files("../../../../Volumes/My Passport for Mac/Cann_ABM/")
files <- files[str_detect(files, "Output")]

d <- clean_csv(files[1])

# function to clean and append to DB --------------------------------------
# use cleaning function along with function to append to DB, then make this into one big function



dbListTables(con)

add_to_db <- function(file, directory = "../../../../Volumes/My Passport for Mac/Cann_ABM/", con = con){
  
  data <- clean_csv(file = file, directory = directory)
  
  dbWriteTable(conn = con, name = "counts", value = data$counts, append = T)
  dbWriteTable(conn = con, name = "params", value = data$params, append = T)
}


# iterate across CSVs -----------------------------------------------------
# map this across all the CSV files, they do NOT get stored, they get read in, cleaned, and put into DB, never kept in memory after put into DB
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "../../../../Volumes/My Passport for Mac/Cann_ABM/Cann_ABM_GSA_1.sqlite3")

directory <- "../../../../Volumes/My Passport for Mac/Cann_ABM/"
files <- list.files(directory)
files <- files[str_detect(files, "Output")]

map(files, add_to_db, con = con)


counts <- tbl(con, "counts")
params <- tbl(con, "params")
params %>% arrange(run_number) %>% collect()

counts %>% tally()


