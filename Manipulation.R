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

# -----------------------------------------------
# Overall Distribution of the dataset
# Number of forfeits by disconnection
forfeit_HVC<-HVC_1701%>%filter(grepl("forfeits by disconnection",moves))

# Number of move before forfeiting by disconnection
forfeit_count<-adply(forfeit_HVC, .margin = 1, function(x) {
	forfeit<-str_split(x$moves, "forfeits by disconnection")
	forfeit_n<-str_count(forfeit,"\\d\\.{1,}")
}, .parallel=T, .inform=T)


forfeit_count<-vector() # For loop solution
for(i in 1:nrow(forfeit_HVC)){
	forfeit<-str_split(forfeit_HVC$moves[i], "forfeits by disconnection")
	forfeit_count[i]<-str_count(forfeit,"\\d\\.{1,}")
}

# Distribution of forfeiting by disconnection (percentage of human vs computer)
firstforfeit<-str_split(forfeit_HVC$moves[1], "forfeits by disconnection")

identify_forfeit<-adply(forfeit_HVC,.margin=1,function(x){
	forfeit<-str_split(x$moves, "forfeits by disconnection")
	if (str_detect(forfeit, "\\{White")==TRUE){
		forfeit_HVC$forfeit_player<-"White"
	} else if (str_detect(forfeit, "\\{Black")==TRUE){
		forfeit_HVC$forfeit_player<-"Black"
	}
})

table(forfeit_HVC$forfeit_player)
ggplot(forfeit_HVC) + geom_bar(aes(x=forfeit_player),position="identity")

forfeit_player<-data.frame(White=0,Black=0)
for (i in 1:nrow(forfeit_HVC)){ # For loop solution
	forfeit<-str_split(forfeit_HVC$moves[i], "forfeits by disconnection")
	ifelse(str_detect(forfeit, "\\{White")==TRUE, forfeit_player$White<-forfeit_player$White+1,
				 forfeit_player$Black<-forfeit_player$Black+1)
}


# On average does color make a difference in a player's chance of winning

# Does color make a differnce for top computer/human players

# Does color make a differnce for bottom computer/human players

# Overall do computers tend to win more or do humans tend to win more when playing against each other?

# What are the most common opening (ECO: Encyclopedia of Chess Openings)

# ----------------------------------------------------------------------
# We take a look at top human players
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
topplayers_stat<-HVC_1701%>%group_by(players)%>%
	summarize(n=n(),win=sum(winorlose=="win"),draw=sum(winorlose=="draw"),lose=sum(winorlose=="lose"))%>%
	arrange(desc(n))%>%mutate(win_rate=win/n)%>%
	mutate(win_rate_2=win/sum(HVC_1701$winorlose=="win"))%>%head(5)

# We test the correlation between winning/losing and playing as white/black for these players
top_players<-topplayers_stat$players

mycolor2 <- RColorBrewer::brewer.pal(9, "BrBG") # color palette

ggplot())+geom_bar(HVC_1701%>%filter(players %in% top_players,aes())





