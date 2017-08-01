####################
###							####
###		FORFEIT		####
####################

# Number of forfeits by disconnection
sum(grepl("forfeits",HVC$moves), na.rm=TRUE)
table(HVC[grepl("forfeits",HVC$moves)==T,]$comp)

forfeit_HVC<-turn_count_df%>%filter(grepl("forfeits by disconnection",moves))

# Group by players and summarize by average turn_count before forfeiting
forfeit_HVC%>%group_by(players)%>%summarize(turn_count=mean(turn_count),win=sum(winorlose=="win"),n=n())%>%
	arrange(desc(turn_count))

#
ggplot(forfeit_HVC)+
	geom_boxplot(aes(x=winorlose,y=turn_count))+
	scale_fill_brewer(type = "seq", palette="Blues")+
	ylab("Move Counts Before Forfeiting")+
	xlab("Outcomes")+
	theme_minimal()


# Distribution of forfeiting by disconnection by color
cl = createCluster(6, export = ls(), lib = list("stringr","plyr"))
identify_forfeit<-adply(forfeit_HVC,.margin=1,function(x){
	forfeit<-str_split(x$moves, "forfeits by disconnection")
	if (str_detect(forfeit, "\\{White")==TRUE){
		forfeit_HVC$forfeit_player<-"White"
	} else if (str_detect(forfeit, "\\{Black")==TRUE){
		forfeit_HVC$forfeit_player<-"Black"
	}
})

stopCluster(cl)

forfeit_player<-data.frame(White=0,Black=0)
for (i in 1:nrow(forfeit_HVC)){ # For loop solution
	forfeit[i]<-str_split(forfeit_HVC$moves[i], "forfeits by disconnection")
	ifelse(str_detect(forfeit, "\\{White")==TRUE, forfeit_player$White[i]<-forfeit_player$White[i]+1,
				 forfeit_player$Black<-forfeit_player$Black+1)
}

table(forfeit_HVC$forfeit_player)
ggplot(forfeit_HVC) + geom_bar(aes(x=forfeit_player),position="identity")

####################
###							####
###	RESIGNATION	####
####################
# Games ended by Resignation
sum(grepl("resigns",HVC$moves), na.rm=TRUE)
table(HVC[grepl("resigns",HVC$moves)==T,]$comp)

HVC_resign<-HVC%>%filter(grepl("resigns",moves))

# Resign distribution by color
resign<-vector()
for(i in 1:length(HVC_resign)){
	if(grepl("Black resigns",HVC_resign[i])==T){
		resign[i]<-"Black"
	} else if (grepl("White resigns",HVC_resign[i])==T){
		resign[i]<-"White"
	}
}

####################
###							####
###	 CHECKMATE	####
####################
# Games ended by checkmate
sum(grepl("checkmated",HVC$moves), na.rm=TRUE)
table(HVC[grepl("checkmated",HVC$moves)==T,]$comp) 
