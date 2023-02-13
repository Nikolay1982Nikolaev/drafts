rm(list = ls())
library(haven)
library(dplyr)
library(assertthat)
library(tidyverse)
library(stringr)

source("/home/bceuser/lenkovnn/GDM/R/files_managment.R")
source("/home/bceuser/lenkovnn/GDM/R/messages_managment.R")
source("/home/bceuser/lenkovnn/GDM/R/transform_variables.R")
####################################################################################################
#create_MH <- function() {
domain           <- "MH"
f = "TEXT_OUTPUT/MH_text_output.txt"

path_to_data_dir <- path_to_data_dir()
path_output_dir  <- path_output_dir()
path_text_file   <- path_text_file(domain)
write_header(f)

data_files       <- list.files(path_to_data_dir)
gdm_df           <- take_gdm_df(data_files)
copy_gdm         <- copy_gdm(gdm_df)
n_row            <- nrow(copy_gdm)
MH               <- create_domain_variable(domain, n_row)



# create columns
# STUDYID ###############################
check <- check_needed_colum("PROTO")
MH <- create_new_var(check)
MH <- MH %>% rename("STUDYID" = new_col)

wrtie_created_new_var(new_var = "STUDYID", f)
write_basic_statisitcs("STUDYID", MH, f)
write_dashes(f)
#########################################
# USUBJID
#L <- c()











#return(MH)
#}
#MH <- create_MH()
#MH
####################################################################################################




















