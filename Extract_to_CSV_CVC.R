library(dplyr)

# Read file
con=file("rawdata/ficsgamesdb_201701_CvC.pgn", "r")
all_lines = readLines(con)
close(con)
all_lines<-all_lines%>%subset(!all_lines=="")

# Store file inputs in lists
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
res_df<-sapply(res, '[', seq(max(sapply(res, length))))
chess1701_CVC<-as.data.frame(res_df)

# Add additional columns
game <- vector(,3287)
for(i in 1:length(game)){
	game[i] = str_c(chess1701_CVC$white[i],chess1701_CVC$black[i],sep = " vs ")
}
chess1701_CVC$Game <- game

winner <- vector(,3287)
for (i in 1:length(winner)){
	if(chess1701_CVC$result[i] == "0-1"){
		winner[i] <- "Black"
	} 
	else if(chess1701_CVC$result[i] =="1-0"){
		winner[i] <- "White"
	} 
	else if(chess1701_CVC$result[i] =="1/2-1/2"){
		winner[i] <- "Draw"
	}
}
chess1701_CVC$Winner <- winner

# Keep the relevant columns
chess1701_CVC<-chess1701_CVC%>%select(-whiteiscomp,-blackiscomp,-ficsgamesdbgameno,-site)%>%
	select(Game, White=white, Black=black, Winner, ECO=eco, Moves=move_list)

# Extract to .csv file
write.csv(chess1701_CVC,"1701_CVC.csv",row.names = F)



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




