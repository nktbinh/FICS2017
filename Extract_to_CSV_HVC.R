library(dplyr)

pgn <- read.table("ficsgamesdb_201701_CvH.pgn", quote="", sep="\n", stringsAsFactors=FALSE)

con=file("ficsgamesdb_201701_CvH.pgn", "r")
all_lines = readLines(con)
close(con)
all_lines<-all_lines%>%subset(!all_lines=="")

res = list();
comp<-vector(,length=4970)
count<-1
for(this_line in all_lines)
{
  if(grepl("^\\[", this_line, perl=T))
  {
    field = sub("\\[(\\w+).+", "\\1", this_line)
    value = sub("\\[\\w+ \\\"(.+)\\\"\\]", "\\1", this_line)
    print(field);
    res[[tolower(field)]] = c(res[[tolower(field)]], value)
    ifelse(field=="WhiteIsComp",comp[count]<-"white",comp[count]<-"black")
    ifelse(field=="WhiteIsComp"|field=="BlackIsComp",count<-count+1,count<-count)
  }else
  {
    print(this_line)
  }
  if(grepl("^1\\.", this_line, perl=T))
  {
    res[["move_list"]] = c(res[["move_list"]], this_line);
  }
}
comp<-comp[1:4970]

# Turn list into dataframe - Solution 3 without additional package
try3<-sapply(res, '[', seq(max(sapply(res, length))))
game17<-as.data.frame(try3)


game17$comp<-comp
chess1701_HVC<-game17%>%select(-whiteiscomp,-blackiscomp,-ficsgamesdbgameno,-site)%>%
  rename(moves=move_list)

# Extract to .csv file
write.csv(chess1701_HVC,"1701_HVC.csv")


