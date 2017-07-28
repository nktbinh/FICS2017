library(rchess)
library(plyr)
library(dplyr)
library(RColorBrewer)
options(tibble.width = Inf)

# Form the data
chessdata<-HVC_1701%>%filter(!grepl("resigns",moves))%>%
	filter(!grepl("forfeits by disconnection",moves))
chessdata <- chessdata %>% mutate(game_id = seq(nrow(.)))

chess1701<-chessdata[1:100,]
chess1701<-chess1701 %>% select(moves, game_id)

dfchess_parallel <- adply(chess1701, .margins = 1,function(x){
	chss <- Chess$new()
	chss$load_pgn(x$moves)
	chss$history_detail()
}, .parallel = T, .inform = T)

stopCluster(cl)

dfchess<-tbl_df(dfchess_parallel) %>% select(-moves)

# ----------------------------------------------------
# Piece Movements
dfboard <- rchess:::.chessboarddata() %>%
	select(cell, col, row, x, y, cc)

dfpaths <- dfchess %>%
	left_join(dfboard %>% rename(from = cell, x.from = x, y.from = y),
						by = "from") %>%
	left_join(dfboard %>% rename(to = cell, x.to = x, y.to = y) %>% select(-cc, -col, -row),
						by = "to") %>%
	mutate(x_gt_y = abs(x.to - x.from) > abs(y.to - y.from),
				 xy_sign = sign((x.to - x.from)*(y.to - y.from)) == 1,
				 x_gt_y_equal_xy_sign = x_gt_y == xy_sign)

# ----------------------------------------------------
# Plot movements of White King
WhiteKing_plot<-ggplot() +
	geom_tile(data = dfboard, aes(x, y, fill = cc)) +
	geom_curve(data = dfpaths %>% filter(piece == "White King", x_gt_y_equal_xy_sign),
						 aes(x = x.from, y = y.from, xend = x.to, yend = y.to),
						 position = position_jitter(width = 0.2, height = 0.2),
						 curvature = 0.50, angle = -45, alpha = 0.1, color = "white", size = 1.05) +
	geom_curve(data = dfpaths %>% filter(piece == "White King", !x_gt_y_equal_xy_sign),
						 aes(x = x.from, y = y.from, xend = x.to, yend = y.to),
						 position = position_jitter(width = 0.2, height = 0.2),
						 curvature = -0.50, angle = 45, alpha = 0.1, color = "white", size = 1.05) +
	scale_fill_manual(values =  c("gray10", "gray20")) +
	ggtitle("White King") +
	coord_equal()

# ---------------------------------------------------------
# Survival Rates
dfsurvrates <- dfchess %>%
	filter(!is.na(status)) %>%
	group_by(piece) %>%
	summarize(games = n(),
						was_captured = sum(status == "captured")) %>%
	mutate(surv_rate = 1 - was_captured/games)

dfsurvrates %>% arrange(desc(surv_rate)) %>% head() # Validation since the kings are never captured

dfsurvrates <- dfsurvrates %>%
	left_join(rchess:::.chesspiecedata() %>% select(start_position, piece = name, color, unicode),
						by = "piece") %>%
	full_join(dfboard %>% rename(start_position = cell),
						by = "start_position")

# Auxillary data to plot the board
dfboard2 <- data_frame(x = 0:8 + 0.5, y = 0 + 0.5, xend = 0:8 + 0.5, yend = 8 + 0.5)

# color palette
mycolor1 <- RColorBrewer::brewer.pal(9, "PuBu")

# Survival Rate Plot
survival_plot<-ggplot(dfsurvrates) +
	geom_tile(data = dfsurvrates %>% filter(!is.na(surv_rate)),
						aes(x, y, fill = surv_rate)) +
	scale_fill_gradient(low = pubu[9],  high = pubu[1]) +
	geom_text(data = dfsurvrates %>% filter(!is.na(surv_rate)),
						aes(x, y, label = scales::percent(surv_rate)),
						color = "coral3", size = 5) +
	scale_x_continuous(breaks = 1:8, labels = letters[1:8]) +
	scale_y_continuous(breaks = 1:8, labels = 1:8)  +
	geom_segment(data = dfboard2, aes(x, y, xend = xend, yend = yend), color = "gray70") +
	geom_segment(data = dfboard2, aes(y, x, xend = yend, yend = xend), color = "gray70") +
	ggtitle("Survival Rates for each piece") + 
	coord_equal() + 
	theme_minimal() +
	theme(legend.position = "none")

# Survival Rate Plot with text replaced by piece's icons
survival_plot2 <- ggplot(dfsurvrates) +
	geom_tile(data = dfsurvrates %>% filter(!is.na(surv_rate)),
						aes(x, y, fill = 100*surv_rate)) +
	scale_fill_gradient(NULL, low = pubu[9],  high = pubu[1]) +
	geom_text(data = dfsurvrates %>% filter(!is.na(surv_rate)),
						aes(x, y, label = unicode), size = 11, color = "gray20", alpha = 0.7) +
	scale_x_continuous(breaks = 1:8, labels = letters[1:8]) +
	scale_y_continuous(breaks = 1:8, labels = 1:8)  +
	geom_segment(data = dfboard2, aes(x, y, xend = xend, yend = yend), color = "gray70") +
	geom_segment(data = dfboard2, aes(y, x, xend = yend, yend = xend), color = "gray70") +
	ggtitle("Survival Rates for each piece") + 
	coord_equal() +
	theme_minimal() +
	theme(legend.position = "right", legend.direction = "vertical")

# ----------------------------------------------------------------
# Square Usage by player
players<-vector()
for (i in 1:nrow(chessdata)){
ifelse(chessdata$comp=="white",players[i]<-chessdata$black[i],players[i]<-chessdata$white[i])
}
players<-as.data.frame(players)
chessdata$players<-players
players <- chessdata %>% count(players) %>% arrange(desc(n)) %>% .$players %>% head(4)


dfchess_players <- ldply(players, function(p){ # p <- sample(players, size = 1)
	games <- chess1701 %>% filter(players == p) %>% .$game_id
	dfres <- dfchess %>%
		filter(game_id %in% games, !is.na(to)) %>%
		count(to) %>%
		mutate(player = p,
					 p = n/length(games))
	dfres
})

dfchess_players <- dfchess_players %>%
	rename(cell = to) %>%
	left_join(dfboard, by = "cell")

ggplot(dfchess_players) +
	geom_tile(aes(x, row, fill = p)) +
	scale_fill_gradient("Movements to every cell\n(normalized by number of games)",
											low = "white",  high = "darkred") +
	geom_text(aes(x, row, label = round(p, 1)), size = 3, color = "grey70", alpha = 0.5) +
	facet_wrap(~player) +
	scale_x_continuous(breaks = 1:8, labels = letters[1:8]) +
	scale_y_continuous(breaks = 1:8, labels = 1:8)  +
	geom_segment(data = dfboard2, aes(x, y, xend = xend, yend = yend), color = "gray70") +
	geom_segment(data = dfboard2, aes(y, x, xend = yend, yend = xend), color = "gray70") +
	coord_equal() +
	theme_minimal() +
	theme(legend.position = "bottom")

# ----------------------------------------------------
# Distribution for the first movements
# Using the piece_number_move and number_move, we can obtain the distribution for the first
# movement for each piece

piece_lvls <- rchess:::.chesspiecedata() %>%
	mutate(col = str_extract(start_position, "\\w{1}"),
				 row = str_extract(start_position, "\\d{1}")) %>%
	arrange(desc(row), col) %>%
	.$name

dfchess_first_mvm <- dfchess %>%
	mutate(piece = factor(piece, levels = piece_lvls),
				 number_move_2 = ifelse(number_move %% 2 == 0, number_move/2, (number_move + 1)/2 )) %>%
	filter(piece_number_move == 1)

ggplot(dfchess_first_mvm) +
	geom_density(aes(number_move_2), fill = "#4292C6", alpha = 0.8, color = NA) +
	scale_y_continuous(breaks = NULL) +
	facet_wrap(~piece, nrow = 4, ncol = 8, scales = "free_y")  +
	xlab("Density") + ylab("Number Move") + 
	xlim(0, 50) +
	theme_gray() +
	theme(panel.background = element_rect(fill = "gray90"))

