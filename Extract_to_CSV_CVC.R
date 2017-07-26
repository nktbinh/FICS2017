library(dplyr)

con=file("ficsgamesdb_201701_CvC.pgn", "r")
all_lines = readLines(con)
close(con)
all_lines<-all_lines%>%subset(!all_lines=="")

res = list()
for(this_line in all_lines)
{
    if(grepl("^\\[", this_line, perl=T))
    {
      field = sub("\\[(\\w+).+", "\\1", this_line)
      value = sub("\\[\\w+ \\\"(.+)\\\"\\]", "\\1", this_line)
      print(field)
      res[[tolower(field)]] = c(res[[tolower(field)]], value)
    }else
    {
      print(this_line)
    }
    if(grepl("^1\\.", this_line, perl=T))
    {
      res[["move_list"]] = c(res[["move_list"]], this_line)
    }
}


# Turn list into dataframe - Solution 3 without additional package
try3<-sapply(res, '[', seq(max(sapply(res, length))))
game17<-as.data.frame(try3)

chess1701_CVC<-game17%>%select(-whiteiscomp,-blackiscomp,-ficsgamesdbgameno,-site)%>%
  rename(moves=move_list)

# Extract to .csv file
write.csv(chess1701_CVC,"1701_CVC.csv")



# ----------------------------------------------------------------
# Turn list into dataframe - Solution 1 with stringi
try1<-t(stri_list2matrix(res, byrow=TRUE))
game17<-as.data.frame(try1)

# Turn list into dataframe - Solution 2 with plyr 
try2<-t(ldply(res, rbind)) # additional 1st id row

# Turn list into dataframe - Solution 3 without additional package
try3<-sapply(res, '[', seq(max(sapply(res, length))))
game17<-as.data.frame(try3)
# ----------------------------------------------------------------




