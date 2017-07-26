library(rchess)
library(tidyr)
library(readr)
library(ggplot2)
library(stringr)
library(reshape2)
library(dplyr)

X1701_CvC <- data.frame(read_csv("1701_CvC.csv"), check.names = FALSE)
X1701_HVC <- data.frame(read_csv("1701_HVC.csv"), check.names = FALSE)

df_CvC <- X2016_CvC%>%select(Game, White, Black, WhiteIsComp, BlackIsComp, `Result-Winner`, Commentaries,ECO)
df_CvH <- X2016_CvH%>%select(Game, White, Black, WhiteIsComp, BlackIsComp, `Result-Winner`, Commentaries,ECO)