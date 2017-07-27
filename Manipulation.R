library(tidyr)
library(readr)
library(ggplot2)
library(stringr)
library(reshape2)

# Create directories for outputs
dir.create("rawdata", FALSE, TRUE)
dir.create("processeddata", FALSE, TRUE)
dir.create("figures", FALSE, TRUE)
dir.create("tables", FALSE, TRUE)

# Read the inputs
CVC_1701 <- data.frame(read_csv("1701_CvC.csv"), check.names = FALSE)
HVC_1701 <- data.frame(read_csv("1701_HVC.csv"), check.names = FALSE)

# Sanity check that import and conversion was ok
dim(CVC_1701)
dim(HVC_1701)

# Convert to factors
CVC_1701$winner<-as.factor(CVC_1701$winner)
HVC_1701$winner<-as.factor(HVC_1701$winner)
HVC_1701$comp<-as.factor(HVC_1701$comp)

# Add columns containing moves as individual strings
CVC_1701<-CVC_1701%>%
	mutate(Moves_split = str_split(moves, " \\d\\. "))%>%
	mutate(Moves_split = str_replace_all(Moves_split, "1. ", ""))

HVC_1701<-HVC_1701%>%
	mutate(Moves_split = str_split(moves, " \\d\\. "))%>%
	mutate(Moves_split = str_replace_all(Moves_split, "1. ", ""))

# 

