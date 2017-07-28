# Non parallel computing solution
dfchess <- adply(chess1701, .margins = 1,function(x){
	chss <- Chess$new()
	chss$load_pgn(x$moves)
	chss$history_detail()
}, .parallel = F, .inform = T)

# For loop solution for applying rchess function to every row of a data frame
datalist<-list()
for (i in 1:nrow(chess1701)){
	chss<- Chess$new()
	chss$load_pgn((chess1701$moves[i]))
	dat<-as.data.frame(chss$history_detail())
	datalist[[i]]<-dat
}
big_data <- bind_rows(datalist)

# Add columns containing moves as individual strings
CVC_1701<-CVC_1701%>%
	mutate(Moves_split = str_split(moves, " \\d\\. "))%>%
	mutate(Moves_split = str_replace_all(Moves_split, "1. ", ""))

HVC_1701<-HVC_1701%>%
	mutate(Moves_split = str_split(moves, " \\d\\. "))%>%
	mutate(Moves_split = str_replace_all(Moves_split, "1. ", ""))