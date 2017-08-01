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

# Turn list into dataframe
res_df<-sapply(res, '[', seq(max(sapply(res, length))))
HVC_1701<-as.data.frame(res_df)

# Add comp column
HVC_1701$comp<-comp

# Add winner column
winner <- vector(,4970)
for (i in 1:length(winner)){
  if(HVC_1701$result[i] == "0-1"){
    winner[i] <- "Black"
  } 
  else if(HVC_1701$result[i] =="1-0"){
    winner[i] <- "White"
  } 
  else if(HVC_1701$result[i] =="1/2-1/2"){
    winner[i] <- "Draw"
  }
}
HVC_1701$winner <- winner

# Keep the relevant columns
HVC_1701<-HVC_1701%>%select(-whiteiscomp,-blackiscomp,-ficsgamesdbgameno,-site)%>%
  select(game, white, black, comp, whiteelo, blackelo , eco, result, winner, moves=move_list)

# Convert to factors
HVC_1701$winner<-as.factor(HVC_1701$winner)
HVC_1701$comp<-as.factor(HVC_1701$comp)

# Add players & winorlose columns, convert them to factor
Players<-vector()
for (i in 1:nrow(HVC_1701)){
  ifelse(HVC_1701$comp[i]=="white",Players[i]<-HVC_1701$black[i],Players[i]<-HVC_1701$white[i])
}

HVC_1701$players<-Players
winorlose<-vector()
for (i in 1:nrow(HVC_1701)){
  if (HVC_1701$winner[i] == "Draw"){
    winorlose[i] = "draw"
  } else if (tolower(HVC_1701$winner[i])!=HVC_1701$comp[i]){
    winorlose[i] = "win"
  } else if (tolower(HVC_1701$winner[i])==HVC_1701$comp[i]){
    winorlose[i] = "lose"
  }
}

HVC_1701$winorlose<-winorlose

# Extract to .csv file
write.csv(HVC_1701,"HVC_1701.csv",row.names = F)