library(dplyr)

pgn <- read.table("ficsgamesdb_201701_CvH.pgn", quote="", sep="\n", stringsAsFactors=FALSE)

# Read file
con=file("rawdata/ficsgamesdb_201701_CvH.pgn", "r")
all_lines = readLines(con)
close(con)
all_lines<-all_lines%>%subset(!all_lines=="")

# Store file inputs in lists
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
res_df<-sapply(res, '[', seq(max(sapply(res, length))))
chess1701_HVC<-as.data.frame(res_df)

# Add additional columns
chess1701_HVC$comp<-comp

game <- vector(,4970)
for(i in 1:length(game)){
	game[i] = str_c(chess1701_HVC$white[i],chess1701_HVC$black[i],sep = " vs ")
}
chess1701_HVC$Game <- game

winner <- vector(,4970)
for (i in 1:length(winner)){
	if(chess1701_HVC$result[i] == "0-1"){
		winner[i] <- "Black"
	} 
	else if(chess1701_HVC$result[i] =="1-0"){
		winner[i] <- "White"
	} 
	else if(chess1701_HVC$result[i] =="1/2-1/2"){
		winner[i] <- "Draw"
	}
}
chess1701_HVC$Winner <- winner

# Keep the relevant columns
chess1701_HVC<-chess1701_HVC%>%select(-whiteiscomp,-blackiscomp,-ficsgamesdbgameno,-site)%>%
	select(Game, White=white, Black=black, Comp=comp, Winner, ECO=eco, Moves=move_list)

# Extract to .csv file
write.csv(chess1701_HVC,"1701_HVC.csv",row.names = F)


