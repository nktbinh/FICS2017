library(tidyr)
library(readr)
library(ggplot2)
library(ggthemes)
library(stringr)
library(forcats)
library(reshape2)
library(pander)
library(rchess)
library(plyr)
library(dplyr)
library(RColorBrewer)
library(foreach)
library(doSNOW)
source("common.R")

# Create directories for outputs
dir.create("rawdata", FALSE, TRUE)
dir.create("processeddata", FALSE, TRUE)
dir.create("figures", FALSE, TRUE)
dir.create("tables", FALSE, TRUE)

# Read the inputs
CVC_1701 <- data.frame(read_csv("CVC_1701.csv"), check.names = FALSE)
HVC_1701 <- data.frame(read_csv("HVC_1701.csv"), check.names = FALSE)

# Sanity check that import and conversion was ok
dim(CVC_1701)
dim(HVC_1701)

# General summary of players' stats
players_stat<-HVC_1701 %>% group_by(players) %>%
  summarize(games_played=n(),win=sum(winorlose=="win"),draw=sum(winorlose=="draw"),lose=sum(winorlose=="lose")) %>%
  arrange(desc(games_played)) %>% 
  mutate(win_rate=win/games_played) %>%
  mutate(win_rate_overallwin=win/sum(HVC_1701$winorlose=="win"))

# Top 5 most active players
players_stat_top <- as.data.frame(head(players_stat,5))
pandoc.table(players_stat_top, style = "simple")

# Top active players' names
top_players<-as.character(players_stat_top$players)
# Least active players' names
bottom_players<-as.character(players_stat$players[642:651])

players_stat_top2<-players_stat_top %>% select(players,win,draw,lose)
players_stat_top2<-melt(players_stat_top2,vars = players)

players_stat_top2_plot<-ggplot(players_stat_top2, aes(x = fct_reorder2(players, variable, value), y = value, fill = variable)) + 
  geom_col() + 
  scale_fill_brewer(type = "div", palette = "Set1") +
  ylab("Total # of matches")+
  xlab("Top Players")+
  labs(fill="Result Types")+
  theme_few(base_size = 15, base_family = "mono")+
  theme(panel.grid = element_line(colour = "grey75", size = .25))+
  coord_flip()

ggsave("players_stat_top2_plot.png",players_stat_top2_plot, device = "png" , width = 14, height = 12)

# Same plot but result types are stacked side-by-side bars
ggplot(players_stat_top2, aes(x=fct_reorder2(players,variable,value),fill=variable, y = value))+
  geom_col(position="dodge")+
  scale_fill_brewer(type = "div", palette="Set1")+
  ylab("Total # of matches")+
  xlab("Top Players")+
  labs(fill="Result Types")+
  theme_minimal()+
  coord_flip()

# Distribution of wins by colors of top players
p1<-ggplot(HVC%>%filter(players %in% top_players,winorlose=="win"))+
  geom_bar(aes(x=fct_reorder(players,games_played),fill=winner),position="dodge")+
  scale_fill_brewer(palette="Dark2",direction=1)+
  ylab("Total # of wins")+
  xlab("Top Players")+
  labs(fill="Black or White")+
  theme_few(base_size = 15, base_family = "mono")+
  coord_flip()

p2<-ggplot(HVC%>%filter(players %in% top_players,winorlose=="lose"))+
  geom_bar(aes(x=fct_reorder(players,games_played),fill=winner),position="dodge")+
  scale_fill_brewer(palette="Paired",direction = -1)+
  ylab("Total # of losses")+
  xlab("Top Players")+
  labs(fill="Black or White")+
  theme_few(base_size = 15, base_family = "mono")+
  coord_flip()

require(grid)
multiplot(p1,p2)

# Create a copy of HVC_1701 
HVC<-HVC_1701
HVC<-HVC%>%left_join(players_stat,by="players")

# On average does color make a difference in a player's chance of winning

# Does color make a differnce for top computer/human players

# Does color make a differnce for bottom computer/human players

# Overall do computers tend to win more or do humans tend to win more when playing against each other?

# What are the most common opening (ECO: Encyclopedia of Chess Openings)

# We take a look at bottom human players
table(players_stat[players_stat$games_played==1,]$win)

# ------------------------------------------------------------------
# Add turn_count column
turn_count_df<-adply(HVC, .margin = 1, function(x) {
  str_count(sub("\\{.*", "", x$moves),"\\d\\.{1,}")
}, .parallel=F, .inform=T)

turn_count_df<-turn_count_df%>%rename(turn_count=V1)
turn_count_df<-na.omit(turn_count_df)

# Summary statistic grouped by players
turncount_stat<-turn_count_df%>%group_by(players)%>%
  summarize(games_played=n(),win=sum(winorlose=="win"),win_rate=win/games_played,turn_count=round(mean(turn_count,na.rm=T)),eco_count=n_distinct(eco))%>%arrange(desc(turn_count))

# Players by top turn_count
pandoc.table(head(as.data.frame(turncount_stat),20), style = "simple")

# Summary statistic when human plays white (go first)
turncount_white<-turn_count_df[turn_count_df$comp=="black",] %>% 
  group_by(players)%>%
  summarize(games_played=n(),win=sum(winorlose=="win"),win_rate=win/games_played,turn_count=round(mean(turn_count,na.rm=T)),eco=n_distinct(eco)) %>%
  arrange(desc(games_played))

# Computer plays white
turncount_black<-turn_count_df[turn_count_df$comp=="white",] %>% 
  group_by(players) %>%
  summarize(games_played=n(),win=sum(winorlose=="win"),win_rate=win/games_played,turn_count=round(mean(turn_count,na.rm=T)),eco=n_distinct(eco)) %>%
  arrange(desc(games_played))

t1<-head(turncount_white,10)
t2<-head(turncount_black,10)

require(gridExtra)
grid.arrange(
  tableGrob(t1, theme = ttheme_default()),
  tableGrob(t2, theme = ttheme_default())
)

# Distributin of turn count vs win count
p3<-ggplot(turncount_white,aes(x=turn_count,y=win,color=players))+
  geom_point()+
  coord_cartesian(ylim = c(0, 45))+
  ylab("Win Count")+
  xlab("Turn Count")+
  ggtitle("Number of win vs Number of turns, Human White")+
  theme_few(base_size = 15, base_family = "mono")+
  theme(legend.position = "none", panel.grid = element_line(colour = "grey75", size = .25))

p4<-ggplot(turncount_black,aes(x=turn_count,y=win,color=players))+
  geom_point()+
  coord_cartesian(ylim = c(0, 40))+
  ylab("Win Count")+
  xlab("Turn Count")+
  ggtitle("Number of win vs Number of turns, Computer White")+
  theme_few(base_size = 15, base_family = "mono")+
  theme(legend.position = "none", panel.grid = element_line(colour = "grey75", size = .25))

multiplot(p3,p4)
		
# Distribution of turn count vs distince eco count
p5<-ggplot(turncount_white,aes(x=eco,y=win,color=players))+
  geom_point()+
  coord_cartesian(ylim = c(0, 45))+
  ylab("Win Count")+
  xlab("Unique Opening Count")+
  ggtitle("Number of win vs Number of unique opening counts, Human White")+
  theme_few(base_size = 15, base_family = "mono")+
  theme(legend.position = "none", panel.grid = element_line(colour = "grey75", size = .25))

p6<-ggplot(turncount_black,aes(x=eco,y=win,color=players))+
  geom_point()+
  coord_cartesian(ylim = c(0, 40))+
  ylab("Win Count")+
  xlab("Unique Opening Count")+
  ggtitle("Number of win vs Number of unique opening counts, Computer White")+
  theme_few(base_size = 15, base_family = "mono")+
  theme(legend.position = "none", panel.grid = element_line(colour = "grey75", size = .25))

multiplot(p5,p6)

# win count vs eco
ggplot(turncount_white,aes(x=eco,y=win,color=players))+
	geom_point()+
	coord_cartesian(ylim = c(0, 60))+
	ylab("Win Count")+
	xlab("Turn Count")+
	labs(fill="Result Types")+
	theme_minimal()+
	theme(legend.position = "none")

# Most common openings
head(sort(table(turn_count_df[turn_count_df$comp=="white",]$eco),decreasing = T),5) # by computer

head(sort(table(turn_count_df[turn_count_df$comp=="black",]$eco),decreasing = T),5) # by human

p7<-turn_count_df[turn_count_df$comp=="black",]%>%
  filter(eco %in% c("A00","A45","D00","C00","A46"))%>%
  ggplot(aes(x=eco,fill=winorlose))+
  geom_bar(position="dodge")+
  scale_fill_brewer(type = "div", palette = "Set1") +
  ylab("Result Count")+
  xlab("Opening Count")+
  ggtitle("Most Common Openings among All Players, played by All Players")+
  theme_minimal()

p8<-turn_count_df[turn_count_df$comp=="black",]%>%
  filter(eco %in% c("B06","A45","A46","C00","A41"))%>%
  ggplot(aes(x=eco,fill=winorlose))+
  geom_bar(position="dodge")+
  scale_fill_brewer(type = "div", palette = "Set1") +
  ylab("Result Count")+
  xlab("Opening Count")+
  ggtitle("Most Common Openings among Most Active Players, played by All Players")+
  theme_minimal()

multiplot(p7,p8)

# Most common openings by top players
head(sort(table(turn_count_df[turn_count_df$players %in% top_players&turn_count_df$comp=="black",]$eco),decreasing = T),5)

p9 <- turn_count_df[turn_count_df$players %in% top_players&turn_count_df$comp=="black",]%>%
  filter(eco %in% c("A00","A45","D00","C00","A46"))%>%
  ggplot(aes(x=eco,fill=winorlose))+
  geom_bar(position="dodge")+
  scale_fill_brewer(type = "div", palette = "Set1") +
  ylab("Result Count")+
  xlab("Opening Count")+
  ggtitle("Most Common Openings among All Players, played by Most Active Players")+
  theme_minimal()

p10 <- turn_count_df[turn_count_df$players %in% top_players&turn_count_df$comp=="black",]%>%
  filter(eco %in% c("B06","A45","A46","C00","A41"))%>%
  ggplot(aes(x=eco,fill=winorlose))+
  geom_bar(position="dodge")+
  scale_fill_brewer(type = "div", palette = "Set1") +
  ylab("Result Count")+
  xlab("Opening Count")+
  ggtitle("Most Common Openings among Most Active Players, played by Most Active Players")+
  theme_minimal()

multiplot(p9,p10)
	
# This helps us answering the question "which common chess opening choices are considered to be 'toxic'?"

