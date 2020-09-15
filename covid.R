library(ggplot2) 
install.packages("ggalt", dependencies=TRUE)
library(ggalt)   
library(tidyverse)
library(dplyr)
library(foreign)

file.choose()
dataset = read.spss("/home/emily/Downloads/W64_Mar20/ATP W64.sav", to.data.frame=TRUE)
blue <- "#0171CE"
red <- "#DE4433"
len <- nrow(dataset)
dataset = select(dataset,COVID_ACT_R_a_W64,COVID_ACT_R_b_W64,COVID_ACT_R_c_W64,COVID_ACT_R_d_W64,COVID_ACT_R_e_W64,F_PARTY_FINAL)
print(dataset)