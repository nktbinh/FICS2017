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