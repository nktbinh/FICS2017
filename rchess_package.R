library(rchess)
library(plyr)
library(dplyr)
library(RColorBrewer)
source("common.R")
options(tibble.width = Inf)

HVC_small <- turn_count_df %>% mutate(game_id = seq(nrow(.)))
HVC_small <- HVC_small %>% select(moves, game_id,turn_count)
cl2 = createCluster(6, export = ls(), lib = list("stringr","plyr"))
HVC_small<-adply(HVC_small, .margin = 1, function(x) {
  x$moves<-str_replace(x$moves, "\\{.*$" ,"")
}, .parallel=T, .inform=T)

stopCluster(cl2)

# Save game_id of top 10 active players into a list and then unlist
turnlist<-list()
for (i in 1:length(top_players)){
  turnlist[[i]]<-turn_count_df %>% mutate(game_id = seq(nrow(.))) %>% filter(players == top_players[i]) %>% .$game_id
}
allturns<-unlist(turnlist)

# Select only the game_id which has more than 40 turn counts
list_id<-vector()
for(i in 1:length(allturns)){
  if(HVC_small[HVC_small$game_id==allturns[i],]$turn_count>40){
    list_id<-append(list_id,allturns[i])
  }
}

# Subset chessdata with the list_id
chessdata<-HVC_small%>%filter(game_id %in% list_id)

# Sample 100 observations
set.seed(123)
require(caTools, quietly = TRUE)
split = sample.split(chessdata$game_id, SplitRatio = 0.25)
chessdata<-subset(chessdata,split == TRUE)

cl3 = createCluster(6, export = ls(), lib = list("foreach","doSNOW","plyr","rchess"))
chessdata <- adply(chessdata, .margins = 1,function(x){
	chss <- Chess$new()
	chss$load_pgn(x$moves)
	chss$history_detail()
}, .parallel = T, .inform = T)

stopCluster(cl3)

dfchess<-tbl_df(chessdata) %>% select(-moves)

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

# Plot movements of White King
WhiteKing_plot<-ggplot() +
	geom_tile(data = dfboard, aes(x, y, fill = cc)) +
	geom_curve(data = dfpaths %>% filter(piece == "White King", x_gt_y_equal_xy_sign),
						 aes(x = x.from, y = y.from, xend = x.to, yend = y.to),
						 position = position_jitter(width = 0.2, height = 0.2),
						 curvature = 0.50, angle = -45, alpha = 0.2, color = "white", size = 1.05) +
	geom_curve(data = dfpaths %>% filter(piece == "White King", !x_gt_y_equal_xy_sign),
						 aes(x = x.from, y = y.from, xend = x.to, yend = y.to),
						 position = position_jitter(width = 0.2, height = 0.2),
						 curvature = -0.50, angle = 45, alpha = 0.2, color = "white", size = 1.05) +
	scale_fill_manual(values =  c("gray10", "gray20")) +
	ggtitle("White King") +
	coord_equal()

ggsave("WhiteKing_plot.png",WhiteKing_plot, device = "png")

# ---------------------------------------------------------
# Survival Rates
dfsurvival <- dfchess %>%
  filter(!is.na(status)) %>%
  group_by(piece) %>%
  summarize(games = n(),
            was_captured = sum(status == "captured")) %>%
  mutate(surv_rate = 1 - was_captured/games)

dfsurvival %>% arrange(desc(surv_rate)) %>% head() # Validation since the kings are never captured

dfsurvival <- dfsurvival %>%
  left_join(rchess:::.chesspiecedata() %>% select(start_position, piece = name, color, unicode), by = "piece") %>%
  full_join(dfboard %>% rename(start_position = cell), by = "start_position")

# Auxillary data to plot the board
dfboard2 <- data_frame(x = 0:8 + 0.5, y = 0 + 0.5, xend = 0:8 + 0.5, yend = 8 + 0.5)

# Color palette
mycolor1 <- RColorBrewer::brewer.pal(9, "Blues")

# Survival Rate Plot
survival_plot<-ggplot(dfsurvival) +
  geom_tile(data = dfsurvival %>% filter(!is.na(surv_rate)),
            aes(x, y, fill = surv_rate)) +
  scale_fill_gradient(low = mycolor1[9],  high = mycolor1[1]) +
  geom_text(data = dfsurvival %>% filter(!is.na(surv_rate)),
            aes(x, y, label = scales::percent(surv_rate)),
            color = "yellow2", size = 5) +
  scale_x_continuous(breaks = 1:8, labels = letters[1:8]) +
  scale_y_continuous(breaks = 1:8, labels = 1:8)  +
  geom_segment(data = dfboard2, aes(x, y, xend = xend, yend = yend), color = "gray70") +
  geom_segment(data = dfboard2, aes(y, x, xend = yend, yend = xend), color = "gray70") +
  ggtitle("Survival Rates for each piece") + 
  coord_equal() + 
  theme_minimal() +
  theme(legend.position = "none")

ggsave("survival_plot.png",survival_plot, device = "png" , width = 14, height = 12)

# Survival Rate Plot with text replaced by piece's icons
survival_plot2 <- ggplot(dfsurvival) +
	geom_tile(data = dfsurvival %>% filter(!is.na(surv_rate)),
						aes(x, y, fill = 100*surv_rate)) +
	scale_fill_gradient(NULL, low = mycolor1[9],  high = mycolor1[1]) +
	geom_text(data = dfsurvival %>% filter(!is.na(surv_rate)),
						aes(x, y, label = unicode), size = 11, color = "red2", alpha = 0.7) +
	scale_x_continuous(breaks = 1:8, labels = letters[1:8]) +
	scale_y_continuous(breaks = 1:8, labels = 1:8)  +
	geom_segment(data = dfboard2, aes(x, y, xend = xend, yend = yend), color = "gray70") +
	geom_segment(data = dfboard2, aes(y, x, xend = yend, yend = xend), color = "gray70") +
	ggtitle("Survival Rates for each piece") + 
	coord_equal() +
	theme_minimal() +
	theme(legend.position = "right", legend.direction = "vertical")

ggsave("survival_plot2.png",survival_plot2, device = "png" , width = 14, height = 12)

# ----------------------------------------------------------------
# Square Usage by player
dfchess_players <- ldply(top_players, function(p){ # p <- sample(top_players, size = 1)
  dfres <- dfchess %>%
    filter(game_id %in% list_id, !is.na(to)) %>%
    count(to) %>%
    mutate(player = p,
           p = n/length(list_id))
  dfres
})

dfchess_players <- dfchess_players %>%
  rename(cell = to) %>%
  left_join(dfboard, by = "cell")

squareusage_plot<-function(id){
  plot<-ggplot(dfchess_players %>% filter(player == id)) +
    geom_tile(aes(x, row, fill = p)) +
    scale_fill_gradient("Movements to every cell\n(normalized by number of games)",
                        low = "white",  high = "darkblue") +
    geom_text(aes(x, row, label = round(p, 1)), size = 3, color = "red1", alpha = 0.5) +
    scale_x_continuous(breaks = 1:8, labels = letters[1:8]) +
    scale_y_continuous(breaks = 1:8, labels = 1:8)  +
    geom_segment(data = dfboard2, aes(x, y, xend = xend, yend = yend), color = "gray70") +
    geom_segment(data = dfboard2, aes(y, x, xend = yend, yend = xend), color = "gray70") +
    coord_equal() +
    theme_minimal() +
    theme(legend.position = "bottom")
  print(plot)
}

# Square usage plot for foosballfan
foosballfan_plot<-ggplot(dfchess_players %>% filter(player == "foosballfan")) +
  geom_tile(aes(x, row, fill = p)) +
  scale_fill_gradient("Movements to every cell\n(normalized by number of games)",
                      low = "white",  high = "darkblue") +
  geom_text(aes(x, row, label = round(p, 1)), size = 3, color = "red1", alpha = 0.5) +
  scale_x_continuous(breaks = 1:8, labels = letters[1:8]) +
  scale_y_continuous(breaks = 1:8, labels = 1:8)  +
  geom_segment(data = dfboard2, aes(x, y, xend = xend, yend = yend), color = "gray70") +
  geom_segment(data = dfboard2, aes(y, x, xend = yend, yend = xend), color = "gray70") +
  coord_equal() +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave("foosballfan_plot.png",foosballfan_plot, device = "png" , width = 14, height = 12)

# Square usage plot for Geforce
Geforce_plot<-ggplot(dfchess_players %>% filter(player == "Geforce")) +
  geom_tile(aes(x, row, fill = p)) +
  scale_fill_gradient("Movements to every cell\n(normalized by number of games)",
                      low = "white",  high = "darkblue") +
  geom_text(aes(x, row, label = round(p, 1)), size = 3, color = "red1", alpha = 0.5) +
  scale_x_continuous(breaks = 1:8, labels = letters[1:8]) +
  scale_y_continuous(breaks = 1:8, labels = 1:8)  +
  geom_segment(data = dfboard2, aes(x, y, xend = xend, yend = yend), color = "gray70") +
  geom_segment(data = dfboard2, aes(y, x, xend = yend, yend = xend), color = "gray70") +
  coord_equal() +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave("Geforce_plot.png",Geforce_plot, device = "png" , width = 14, height = 12)

# Square usage plot for FedorEmelianenko
FedorEmelianenko_plot<-ggplot(dfchess_players %>% filter(player == "FedorEmelianenko")) +
  geom_tile(aes(x, row, fill = p)) +
  scale_fill_gradient("Movements to every cell\n(normalized by number of games)",
                      low = "white",  high = "darkblue") +
  geom_text(aes(x, row, label = round(p, 1)), size = 3, color = "red1", alpha = 0.5) +
  scale_x_continuous(breaks = 1:8, labels = letters[1:8]) +
  scale_y_continuous(breaks = 1:8, labels = 1:8)  +
  geom_segment(data = dfboard2, aes(x, y, xend = xend, yend = yend), color = "gray70") +
  geom_segment(data = dfboard2, aes(y, x, xend = yend, yend = xend), color = "gray70") +
  coord_equal() +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave("FedorEmelianenko_plot.png",FedorEmelianenko_plot, device = "png" , width = 14, height = 12)

# ----------------------------------------------------
# Distribution for the first movements
# Using the piece_number_move and number_move, we can obtain the distribution for the first
# movement for each piece

piece_lvls <- rchess:::.chesspiecedata() %>%
  mutate(col = str_extract(start_position, "\\w{1}"),
         row = str_extract(start_position, "\\d{1}")) %>%
  arrange(desc(row), col) %>%
  .$name

dfmoves_first_mvm <- dfchess %>%
  mutate(piece = factor(piece, levels = piece_lvls),
         number_move_2 = ifelse(number_move %% 2 == 0, number_move/2, (number_move + 1)/2 )) %>%
  filter(piece_number_move == 1)

firstmove_plot <- ggplot(dfmoves_first_mvm) +
  geom_density(aes(number_move_2), fill = "#B71C1C", alpha = 0.8, color = NA) +
  scale_y_continuous(breaks = NULL) +
  facet_wrap(~piece, nrow = 4, ncol = 8, scales = "free_y")  +
  xlab("Density") + ylab("Number Move") + 
  xlim(0, 60) +
  theme_gray() +
  theme(panel.background = element_rect(fill = "gray90"))

ggsave("firstmove_plot.png",firstmove_plot, device = "png" , width = 14, height = 12)

