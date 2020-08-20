# FULL PITCHERS CODE

# Load R libraries
library(ggplot2)
library(sqldf)
library(dplyr)
library(RColorBrewer)

setwd("C:/Users/jakes/OneDrive/Desktop/Pitch Mirroring Project")

# Read CSV file with wOBA changes based on count, and rename columns
count_woba <- read.csv(file="Count Summary.csv", 
                      header=TRUE, sep=",")
names(count_woba) <- c("season","balls","strikes","count","woba","woba_strike","delta_strike","woba_ball", 
                       "delta_ball", "tot_delta", "woba_scale", "strike_run_value", "ball_run_value", "tot_run_value")

# Graph a bar plot of the wOBA changes
count_19_piv <- read.csv(file="Count Summary 2019 Pivot.csv", 
                         header=TRUE, sep=",")
ggplot(count_19_piv, aes(x = count, y = run_value, fill = pitch)) +
  geom_col(position = "dodge") +
  xlab("Count") +
  ylab("Run Value") +
  scale_y_continuous(breaks=seq(-0.4,0.3,0.05)) +
  scale_fill_manual(values=c("blue","red"))

# Read CSV files with Statcast Scrape data for three years
all_15 <- read.csv(file="C:\\Users\\jakes\\OneDrive\\Desktop\\Baseball Research\\Pitch Sequencing Project\\all_15_dataframe.csv",
                   header=TRUE, sep=",")
all_16 <- read.csv(file="C:\\Users\\jakes\\OneDrive\\Desktop\\Baseball Research\\Pitch Sequencing Project\\all_16_dataframe.csv",
                  header=TRUE, sep=",")
all_17 <- read.csv(file="C:\\Users\\jakes\\OneDrive\\Desktop\\Baseball Research\\Pitch Sequencing Project\\all_17_dataframe.csv",
                  header=TRUE, sep=",")
all_18 <- read.csv(file="C:\\Users\\jakes\\OneDrive\\Desktop\\Baseball Research\\Pitch Sequencing Project\\all_18_dataframe.csv",
                  header=TRUE, sep=",")
all_19 <- read.csv(file="C:\\Users\\jakes\\OneDrive\\Desktop\\Baseball Research\\Pitch Sequencing Project\\all_19_dataframe.csv",
                   header=TRUE, sep=",")

# Merge three years into one table
statcast_5yr <- rbind.data.frame(all_15,all_16,all_17,all_18,all_19)

# Create master table with necessary attributes, incl appending pitch type and location, necessary info from prev pitch
# Creates two zones from original Statcast zones by dividing zone into high and low portions
# Grouped by pitcher
pitch_master_tbl <- sqldf("
	select statcast_5yr_adj.player_name,
		   statcast_5yr_adj.pitcher,
		   statcast_5yr_adj.game_date, 
		   statcast_5yr_adj.game_year, 
		   statcast_5yr_adj.at_bat_number, 
		   statcast_5yr_adj.pitch_number, 
		   statcast_5yr_adj.balls, 
		   statcast_5yr_adj.strikes, 
		   statcast_5yr_adj.pitch_type||cast(round(statcast_5yr_adj.high_low,0) as int) as pitch_type, 
		   sa2.pitch_type||cast(round(sa2.high_low,0) as int) prev_pitch_type, 
		   statcast_5yr_adj.high_low,
		   statcast_5yr_adj.plate_x,
		   statcast_5yr_adj.plate_z,
		   statcast_5yr_adj.p_throws,
		   statcast_5yr_adj.stand, 
		   statcast_5yr_adj.description, 
		   sa2.description prev_description, 
		   statcast_5yr_adj.events, 
		   statcast_5yr_adj.bb_type, 
		   statcast_5yr_adj.estimated_woba_using_speedangle, 
		   case when (statcast_5yr_adj.description like '%strike%' or (statcast_5yr_adj.description = 'foul_tip' and statcast_5yr_adj.strikes = 2)) then count_woba.strike_run_value 
		   		when statcast_5yr_adj.description like '%ball%' then count_woba.ball_run_value 
		   		when statcast_5yr_adj.description = 'foul' and statcast_5yr_adj.strikes = 2 then 0 
		   		when statcast_5yr_adj.description like '%foul%' and statcast_5yr_adj.strikes < 2 then count_woba.strike_run_value 
		   		else ((statcast_5yr_adj.estimated_woba_using_speedangle - count_woba.woba) / count_woba.woba_scale) 
		   		end as run_value 
	from (select *,
				case when plate_z >= 2.50 then 1
		   		when plate_z < 2.50 then 2
		   		end as high_low
		  from statcast_5yr) statcast_5yr_adj
		left join (select *, pitch_number + 1 pitch_number_adj,
				case when plate_z >= 2.50 then 1
		   		when plate_z < 2.50 then 2
		   		end as high_low
		  from statcast_5yr) sa2 on sa2.pitcher = statcast_5yr_adj.pitcher and sa2.game_date = statcast_5yr_adj.game_date
		    and sa2.at_bat_number = statcast_5yr_adj.at_bat_number and sa2.pitch_number_adj = statcast_5yr_adj.pitch_number
		join count_woba on count_woba.season = statcast_5yr_adj.game_year
			and statcast_5yr_adj.balls = count_woba.balls 
			and statcast_5yr_adj.strikes = count_woba.strikes 
	group by statcast_5yr_adj.player_name,
		   statcast_5yr_adj.pitcher,
		   statcast_5yr_adj.game_date, 
		   statcast_5yr_adj.game_year, 
		   statcast_5yr_adj.at_bat_number, 
		   statcast_5yr_adj.pitch_number, 
		   statcast_5yr_adj.statcast_5yr_adj.balls, 
		   statcast_5yr_adj.statcast_5yr_adj.strikes, 
		   statcast_5yr_adj.pitch_type||cast(round(statcast_5yr_adj.high_low,0) as int), 
		   statcast_5yr_adj.high_low,
		   statcast_5yr_adj.plate_x,
		   statcast_5yr_adj.plate_z,
		   statcast_5yr_adj.p_throws,
		   statcast_5yr_adj.stand, 
		   statcast_5yr_adj.description, 
		   statcast_5yr_adj.events, 
		   statcast_5yr_adj.bb_type, 
		   statcast_5yr_adj.estimated_woba_using_speedangle
	order by statcast_5yr_adj.pitcher, statcast_5yr_adj.game_date, statcast_5yr_adj.at_bat_number, statcast_5yr_adj.pitch_number
	")

bartolo <- filter(pitch_master_tbl, player_name == "Bartolo Colon") %>% arrange(game_date,at_bat_number,pitch_number)

# Create table that for all pitches, calculates run value per 100 pitches based on wOBA changes
pitcher_all <- sqldf("
	select player_name,
		   pitcher,
		   p_throws,
		   count(case when pitch_type = 'FF1' then 'FF1' end) count_high_fb,
		   count(case when pitch_type = 'FF1' or pitch_type = 'FF2' then 'FF' end) count_all_fb,
		   count(case when pitch_type = 'CU2' then 'CU2' end) count_low_cu,
		   count(case when pitch_type = 'CU2' or pitch_type = 'CU1' then 'CU' end) count_all_cu,
		   count(*) count_pitch, 
		   sum(run_value) tot_run_value, 
		   ((sum(run_value) * 100) / count(pitch_type)) run_value_100 
	from pitch_master_tbl 
	group by pitcher
	order by pitcher, count_pitch desc
	")

# Create table that for high four-seam and low curveballs, calculates run value per 100 pitches based on wOBA changes
pitcher_vac <- sqldf("
	select player_name,
		   pitcher,
		   p_throws,
		   count(case when pitch_type = 'FF1' then 'FF1' end) count_fb,
		   sum(case when pitch_type = 'FF1' then run_value end) fb_run_value,
		   ((sum(case when pitch_type = 'FF1' then run_value end) * 100) / count(case when pitch_type = 'FF1' then 'FF1' end)) fb_run_value_100,
		  count(case when pitch_type = 'CU2' then 'CU2' end) count_cu,
		   sum(case when pitch_type = 'CU2' then run_value end) cu_run_value,
		   ((sum(case when pitch_type = 'CU2' then run_value end) * 100) / count(case when pitch_type = 'CU2' then 'CU2' end)) cu_run_value_100,
		   count(*) count_pitch, 
		   sum(run_value) tot_run_value, 
		   ((sum(run_value) * 100) / count(pitch_type)) run_value_100 
	from pitch_master_tbl 
	where pitch_type = 'FF1' or pitch_type = 'CU2'
	group by pitcher
	order by pitcher, count_pitch desc
	")

# Create table that for a two-pitch sequence of high four-seam and low curveball, calculates run value per 100 pitches based on wOBA changes
pitcher_seq <- sqldf("
	select player_name,
		   pitcher,
		   p_throws,
		   count(pitch_type) count_seq, 
		   sum(run_value) tot_run_value, 
		   ((sum(run_value) * 100) / count(pitch_type)) run_value_100,
		   ((sum(case when (pitch_type = 'CU2' and prev_pitch_type = 'FF1') then run_value end) * 100) / count(case when (pitch_type = 'CU2' and prev_pitch_type = 'FF1') then 'fb_cu' end)) fb_cu_run_value_100,
		   ((sum(case when (pitch_type = 'FF1' and prev_pitch_type = 'CU2') then run_value end) * 100) / count(case when (pitch_type = 'FF1' and prev_pitch_type = 'CU2') then 'cu_fb' end)) cu_fb_run_value_100
	from pitch_master_tbl 
	where (pitch_type = 'FF1' and prev_pitch_type = 'CU2') 
	  or (pitch_type = 'CU2' and prev_pitch_type = 'FF1')
	group by pitcher
	order by pitcher, count_seq desc
	")

# Create table that for each two-pitch sequence, calculates the difference of runs per 100
# between that pitch type when preceded by a particular pitch type and that pitch type on its own
# This gives us the isolated sequencing effect
pitcher_seqval <- sqldf("
	select ps.player_name,
		   ps.pitcher,
		   ps.p_throws,
		   pa.count_pitch,
		   pv.count_fb,
		   pv.count_cu,
		   cast(cast(pa.count_high_fb as float) / cast(pa.count_pitch as float) as float) high_fb_pct,
		   cast(cast(pa.count_low_cu as float) / cast(pa.count_pitch as float) as float) low_cu_pct,
		   ps.count_seq, 
		   cast(cast(ps.count_seq as float) / cast(pa.count_pitch as float) as float) seq_pct,
		   pv.fb_run_value_100,
		   pv.cu_run_value_100,
		   ps.run_value_100 seq_runs_100, 
		   pv.run_value_100 vac_runs_100, 
		   pa.run_value_100 all_runs_100,
		   (ps.run_value_100 - pv.run_value_100) seq_value_100, 
		   (ps.run_value_100 - pa.run_value_100) seq_value_100_v2 
	from pitcher_all pa
	  join pitcher_vac pv on pa.pitcher = pv.pitcher
		join pitcher_seq ps on pv.pitcher = ps.pitcher
	group by ps.pitcher
	order by ps.pitcher
	")

#### Results Analysis
# Top 15 pitchers by overall run value per 100 (min 2500 pitches)
pitcher_all %>% filter(count_pitch > 2500) %>% arrange(run_value_100) %>% head(15)
# Bottom 15
pitcher_all %>% filter(count_pitch > 2500) %>% arrange(run_value_100) %>% tail(15)

ggplot(pitcher_all %>% filter(count_pitch > 2500), aes(x = "Pitchers",y = run_value_100)) +
  geom_boxplot()

pitcher_all %>% filter(count_pitch > 2500) %>% summary()

# Top 15 high fastballs by run value per 100 (min 250 fastballs)
pitcher_vac %>% filter(count_fb > 250) %>% summary()
pitcher_vac %>% filter(count_fb > 250) %>% arrange(fb_run_value_100) %>% head(15)

# Top 15 low curveballs by run value per 100 (min 250 curveballs)
pitcher_vac %>% filter(count_cu > 250) %>% arrange(cu_run_value_100) %>% head(15)

fb_cu_values <- pitcher_vac %>% filter(count_fb > 250 & count_cu > 250)
ggplot(data = fb_cu_values, 
       aes(x = fb_run_value_100, y = cu_run_value_100)) +
  geom_text(
    label = fb_cu_values$player_name, 
    check_overlap = T
  ) +
  xlab("High FB Run Value per 100") +
  ylab("Low CU Run Value per 100")

# Top 15 pitchers by high fastball/low curveball sequence run value per 100 (min 100 sequences)
pitcher_seq %>% filter(count_seq > 100) %>% summary()
pitcher_seq %>% filter(count_seq > 100) %>% arrange(run_value_100) %>% head(15)

# Top 15 pitchers by isolated high fastball/low curveball sequence value per 100 (min 100 sequences)
pitcher_seqval %>% filter(count_seq > 100) %>% summary()
pitcher_seqval %>% filter(count_seq > 100) %>% arrange(seq_value_100_v2) %>% head(15)

seq_values <- pitcher_seqval %>% filter(count_seq > 100)
ggplot(data = seq_values,
       aes(x = seq_runs_100, y = seq_value_100_v2)) +
  geom_text(
    label = seq_values$player_name, 
    check_overlap = T
  ) +
  xlab("Sequence Run Value per 100") +
  ylab("Isolated Sequence Run Value per 100")
# Over a large enough sample, the sequence run value and isolated sequence run value tend to be near-equivalent

### Top 15 leaderboard
write.csv(data.frame(
  pitcher_all %>% filter(count_pitch > 2500) %>% arrange(run_value_100) %>% head(15) %>% select(player_name),
  pitcher_all %>% filter(count_pitch > 2500) %>% arrange(run_value_100) %>% head(15) %>% select(run_value_100),
  pitcher_vac %>% filter(count_fb > 250) %>% arrange(fb_run_value_100) %>% head(15) %>% select(player_name),
  pitcher_vac %>% filter(count_fb > 250) %>% arrange(fb_run_value_100) %>% head(15) %>% select(fb_run_value_100),
  pitcher_vac %>% filter(count_cu > 250) %>% arrange(cu_run_value_100) %>% head(15) %>% select(player_name),
  pitcher_vac %>% filter(count_cu > 250) %>% arrange(cu_run_value_100) %>% head(15) %>% select(cu_run_value_100),
  pitcher_seq %>% filter(count_seq > 100) %>% arrange(run_value_100) %>% head(15) %>% select(player_name),
  pitcher_seq %>% filter(count_seq > 100) %>% arrange(run_value_100) %>% head(15) %>% select(run_value_100),
  pitcher_seqval %>% filter(count_seq > 100) %>% arrange(seq_value_100_v2) %>% head(15) %>% select(player_name),
  pitcher_seqval %>% filter(count_seq > 100) %>% arrange(seq_value_100_v2) %>% head(15) %>% select(seq_value_100_v2)
), "Top 15.csv")

perc.rank <- function(x) trunc(rank(x))/length(x)

### Calculate percentile ranks for each pitch value metric
overall_rv100 <- data.frame(
  pitcher_all %>% filter(count_pitch > 2500) %>% arrange(run_value_100) %>% select(player_name) %>% rename(pitcher = player_name),
  pitcher_all %>% filter(count_pitch > 2500) %>% arrange(run_value_100) %>% select(run_value_100),
  percentile = 1 - perc.rank(pitcher_all %>% filter(count_pitch > 2500) %>% arrange(run_value_100) %>% select(run_value_100))
                    / nrow(pitcher_all %>% filter(count_pitch > 2500) %>% arrange(run_value_100) %>% select(run_value_100)))

fb_rv100 <- data.frame(
  pitcher_vac %>% filter(count_fb > 250) %>% arrange(fb_run_value_100) %>% select(player_name) %>% rename(pitcher = player_name),
  pitcher_vac %>% filter(count_fb > 250) %>% arrange(fb_run_value_100) %>% select(fb_run_value_100),
  percentile = 1 - perc.rank(pitcher_vac %>% filter(count_fb > 250) %>% arrange(fb_run_value_100) %>% select(fb_run_value_100))
  / nrow(pitcher_vac %>% filter(count_fb > 250) %>% arrange(fb_run_value_100) %>% select(fb_run_value_100)))
  
cu_rv100 <- data.frame(
  pitcher_vac %>% filter(count_cu > 250) %>% arrange(cu_run_value_100) %>% select(player_name) %>% rename(pitcher = player_name),
  pitcher_vac %>% filter(count_cu > 250) %>% arrange(cu_run_value_100) %>% select(cu_run_value_100),
  percentile = 1 - perc.rank(pitcher_vac %>% filter(count_cu > 250) %>% arrange(cu_run_value_100) %>% select(cu_run_value_100))
  / nrow(pitcher_vac %>% filter(count_cu > 250) %>% arrange(cu_run_value_100) %>% select(cu_run_value_100)))
  
seq_rv100 <- data.frame(
  pitcher_seq %>% filter(count_seq > 100) %>% arrange(run_value_100) %>% select(player_name) %>% rename(pitcher = player_name),
  pitcher_seq %>% filter(count_seq > 100) %>% arrange(run_value_100) %>% select(run_value_100),
  percentile = 1 - perc.rank(pitcher_seq %>% filter(count_seq > 100) %>% arrange(run_value_100) %>% select(run_value_100))
  / nrow(pitcher_seq %>% filter(count_seq > 100) %>% arrange(run_value_100) %>% select(run_value_100)))

iso_seq_rv100 <- data.frame(
  pitcher_seqval %>% filter(count_seq > 100) %>% arrange(seq_value_100_v2) %>% select(player_name) %>% rename(pitcher = player_name),
  pitcher_seqval %>% filter(count_seq > 100) %>% arrange(seq_value_100_v2) %>% select(seq_value_100_v2),
  percentile = 1 - perc.rank(pitcher_seqval %>% filter(count_seq > 100) %>% arrange(seq_value_100_v2) %>% select(seq_value_100_v2))
  / nrow(pitcher_seqval %>% filter(count_seq > 100) %>% arrange(seq_value_100_v2) %>% select(seq_value_100_v2)))


######## Location plots
ggplot(data = pitch_master_tbl %>% filter(player_name == "Blaine Hardy",
                                          pitch_type == "FF1" | pitch_type == "CU2"),
       aes(x = plate_x, y = plate_z)) +
  geom_point(aes(color = pitch_type)) +
  geom_segment(aes(x = -0.708333, y = 1.492, xend = 0.708333, yend = 1.492)) +
  geom_segment(aes(x = -0.708333, y = 3.373, xend = 0.708333, yend = 3.373)) +
  geom_segment(aes(x = -0.708333, y = 1.492, xend = -0.708333, yend = 3.373)) +
  geom_segment(aes(x = 0.708333, y = 1.492, xend = 0.708333, yend = 3.373)) +
  
  geom_segment(aes(x = -0.708333, y = 2.119, xend = 0.708333, yend = 2.119)) +
  geom_segment(aes(x = -0.708333, y = 2.746, xend = 0.708333, yend = 2.746)) +
  geom_segment(aes(x = -0.236111, y = 1.492, xend = -0.236111, yend = 3.373)) +
  geom_segment(aes(x = 0.236111, y = 1.492, xend = 0.236111, yend = 3.373))

###make heat map colors
brewer.pal(11, "RdYlBu")
buylrd <- rev(brewer.pal(11,"RdYlBu"))

# All fastballs
smoothScatter(pitch_master_tbl$plate_z[pitch_master_tbl$player_name == "Blaine Hardy" &
                                       (pitch_master_tbl$pitch_type == "FF1" | pitch_master_tbl$pitch_type == "FF2")]
              ~ pitch_master_tbl$plate_x[pitch_master_tbl$player_name == "Blaine Hardy" &
                                           (pitch_master_tbl$pitch_type == "FF1" | pitch_master_tbl$pitch_type == "FF2")],
              nbin = 1000,
              colramp = colorRampPalette(c(buylrd)),
              cex = .7,
              main="Blaine Hardy Fastball Location",
              xlab="Horizontal Location", 
              ylab="Vertical Location", 
              xlim=c(-2,2), 
              ylim=c(-2,6))
rect(-0.708333, 1.492, 0.708333, 3.373, border="black", lty="dotted", lwd=3)
abline(h=0, lwd=3)

smoothScatter(pitch_master_tbl$plate_z[pitch_master_tbl$player_name == "Nick Anderson" &
                                         (pitch_master_tbl$pitch_type == "FF1" | pitch_master_tbl$pitch_type == "FF2")]
              ~ pitch_master_tbl$plate_x[pitch_master_tbl$player_name == "Nick Anderson" &
                                           (pitch_master_tbl$pitch_type == "FF1" | pitch_master_tbl$pitch_type == "FF2")],
              nbin = 1000,
              colramp = colorRampPalette(c(buylrd)),
              cex = .7,
              main="Nick Anderson Fastball Location",
              xlab="Horizontal Location", 
              ylab="Vertical Location", 
              xlim=c(-2,2), 
              ylim=c(-2,6))
rect(-0.708333, 1.492, 0.708333, 3.373, border="black", lty="dotted", lwd=3)
abline(h=0, lwd=3)

# Just high fastballs
smoothScatter(pitch_master_tbl$plate_z[pitch_master_tbl$player_name == "Blaine Hardy" & pitch_master_tbl$pitch_type == "FF1"]
              ~ pitch_master_tbl$plate_x[pitch_master_tbl$player_name == "Blaine Hardy" & pitch_master_tbl$pitch_type == "FF1"],
              nbin = 1000,
              colramp = colorRampPalette(c(buylrd)),
              cex = .7,
              main="Blaine Hardy Fastball Location",
              xlab="Horizontal Location", 
              ylab="Vertical Location", 
              xlim=c(-2,2), 
              ylim=c(2.43,6))
rect(-0.708333, 1.492, 0.708333, 3.373, border="black", lty="dotted", lwd=3)

smoothScatter(pitch_master_tbl$plate_z[pitch_master_tbl$player_name == "Daniel Mengden" & pitch_master_tbl$pitch_type == "FF1"]
              ~ pitch_master_tbl$plate_x[pitch_master_tbl$player_name == "Daniel Mengden" & pitch_master_tbl$pitch_type == "FF1"],
              nbin = 1000,
              colramp = colorRampPalette(c(buylrd)),
              cex = .7,
              main="Daniel Mengden Fastball Location",
              xlab="Horizontal Location", 
              ylab="Vertical Location", 
              xlim=c(-2,2), 
              ylim=c(2.43,6))
rect(-0.708333, 1.492, 0.708333, 3.373, border="black", lty="dotted", lwd=3)

smoothScatter(pitch_master_tbl$plate_z[pitch_master_tbl$player_name == "Aaron Sanchez" & pitch_master_tbl$pitch_type == "FF1"]
              ~ pitch_master_tbl$plate_x[pitch_master_tbl$player_name == "Aaron Sanchez" & pitch_master_tbl$pitch_type == "FF1"],
              nbin = 1000,
              colramp = colorRampPalette(c(buylrd)),
              cex = .7,
              main="Aaron Sanchez Fastball Location",
              xlab="Horizontal Location", 
              ylab="Vertical Location", 
              xlim=c(-2,2), 
              ylim=c(2.43,6))
rect(-0.708333, 1.492, 0.708333, 3.373, border="black", lty="dotted", lwd=3)

smoothScatter(pitch_master_tbl$plate_z[pitch_master_tbl$player_name == "Nick Anderson" & pitch_master_tbl$pitch_type == "FF1"]
              ~ pitch_master_tbl$plate_x[pitch_master_tbl$player_name == "Nick Anderson" & pitch_master_tbl$pitch_type == "FF1"],
              nbin = 1000,
              colramp = colorRampPalette(c(buylrd)),
              cex = .7,
              main="Nick Anderson Fastball Location",
              xlab="Horizontal Location", 
              ylab="Vertical Location", 
              xlim=c(-2,2), 
              ylim=c(2.43,6))
rect(-0.708333, 1.492, 0.708333, 3.373, border="black", lty="dotted", lwd=3)

# Just low curveballs
smoothScatter(pitch_master_tbl$plate_z[pitch_master_tbl$player_name == "Blaine Hardy" & pitch_master_tbl$pitch_type == "CU2"]
              ~ pitch_master_tbl$plate_x[pitch_master_tbl$player_name == "Blaine Hardy" & pitch_master_tbl$pitch_type == "CU2"],
              nbin = 1000,
              colramp = colorRampPalette(c(buylrd)),
              cex = .7,
              main="Blaine Hardy Curveball Location",
              xlab="Horizontal Location", 
              ylab="Vertical Location", 
              xlim=c(-2,2), 
              ylim=c(-2,2.43))
rect(-0.708333, 1.492, 0.708333, 3.373, border="black", lty="dotted", lwd=3)

smoothScatter(pitch_master_tbl$plate_z[pitch_master_tbl$player_name == "Daniel Mengden" & pitch_master_tbl$pitch_type == "CU2"]
              ~ pitch_master_tbl$plate_x[pitch_master_tbl$player_name == "Daniel Mengden" & pitch_master_tbl$pitch_type == "CU2"],
              nbin = 1000,
              colramp = colorRampPalette(c(buylrd)),
              cex = .7,
              main="Daniel Mengden Curveball Location",
              xlab="Horizontal Location", 
              ylab="Vertical Location", 
              xlim=c(-2,2), 
              ylim=c(-2,2.43))
rect(-0.708333, 1.492, 0.708333, 3.373, border="black", lty="dotted", lwd=3)

smoothScatter(pitch_master_tbl$plate_z[pitch_master_tbl$player_name == "Aaron Sanchez" & pitch_master_tbl$pitch_type == "CU2"]
              ~ pitch_master_tbl$plate_x[pitch_master_tbl$player_name == "Aaron Sanchez" & pitch_master_tbl$pitch_type == "CU2"],
              nbin = 1000,
              colramp = colorRampPalette(c(buylrd)),
              cex = .7,
              main="Aaron Sanchez Curveball Location",
              xlab="Horizontal Location", 
              ylab="Vertical Location", 
              xlim=c(-2,2), 
              ylim=c(-2,2.43))
rect(-0.708333, 1.492, 0.708333, 3.373, border="black", lty="dotted", lwd=3)

smoothScatter(pitch_master_tbl$plate_z[pitch_master_tbl$player_name == "Nick Anderson" & pitch_master_tbl$pitch_type == "CU2"]
              ~ pitch_master_tbl$plate_x[pitch_master_tbl$player_name == "Nick Anderson" & pitch_master_tbl$pitch_type == "CU2"],
              nbin = 1000,
              colramp = colorRampPalette(c(buylrd)),
              cex = .7,
              main="Nick Anderson Curveball Location",
              xlab="Horizontal Location", 
              ylab="Vertical Location", 
              xlim=c(-2,2), 
              ylim=c(-2,2.43))
rect(-0.708333, 1.492, 0.708333, 3.373, border="black", lty="dotted", lwd=3)

### Jeff Hoffman vs. Charlie Morton
smoothScatter(pitch_master_tbl$plate_z[pitch_master_tbl$player_name == "Jeff Hoffman" & (pitch_master_tbl$pitch_type == "FF1" | pitch_master_tbl$pitch_type == "FF2") & pitch_master_tbl$prev_pitch_type == "CU2"]
              ~ pitch_master_tbl$plate_x[pitch_master_tbl$player_name == "Jeff Hoffman" & (pitch_master_tbl$pitch_type == "FF1" | pitch_master_tbl$pitch_type == "FF2") & pitch_master_tbl$prev_pitch_type == "CU2"],
              nbin = 1000,
              colramp = colorRampPalette(c(buylrd)),
              cex = .7,
              main="Jeff Hoffman Fastball Location",
              xlab="Horizontal Location", 
              ylab="Vertical Location", 
              xlim=c(-2,2), 
              ylim=c(0,6))
rect(-0.708333, 1.492, 0.708333, 3.373, border="black", lty="dotted", lwd=3)

smoothScatter(pitch_master_tbl$plate_z[pitch_master_tbl$player_name == "Charlie Morton" & (pitch_master_tbl$pitch_type == "FF1" | pitch_master_tbl$pitch_type == "FF2") & pitch_master_tbl$prev_pitch_type == "CU2"]
              ~ pitch_master_tbl$plate_x[pitch_master_tbl$player_name == "Charlie Morton" & (pitch_master_tbl$pitch_type == "FF1" | pitch_master_tbl$pitch_type == "FF2") & pitch_master_tbl$prev_pitch_type == "CU2"],
              nbin = 1000,
              colramp = colorRampPalette(c(buylrd)),
              cex = .7,
              main="Charlie Morton Fastball Location",
              xlab="Horizontal Location", 
              ylab="Vertical Location", 
              xlim=c(-2,2), 
              ylim=c(0,6))
rect(-0.708333, 1.492, 0.708333, 3.373, border="black", lty="dotted", lwd=3)

smoothScatter(pitch_master_tbl$plate_z[pitch_master_tbl$player_name == "Jeff Hoffman" & pitch_master_tbl$pitch_type == "FF1" & pitch_master_tbl$prev_pitch_type == "CU2"]
              ~ pitch_master_tbl$plate_x[pitch_master_tbl$player_name == "Jeff Hoffman" & pitch_master_tbl$pitch_type == "FF1" & pitch_master_tbl$prev_pitch_type == "CU2"],
              nbin = 1000,
              colramp = colorRampPalette(c(buylrd)),
              cex = .7,
              main="Jeff Hoffman Fastball Location",
              xlab="Horizontal Location", 
              ylab="Vertical Location", 
              xlim=c(-2,2), 
              ylim=c(0,6))
rect(-0.708333, 1.492, 0.708333, 3.373, border="black", lty="dotted", lwd=3)

smoothScatter(pitch_master_tbl$plate_z[pitch_master_tbl$player_name == "Charlie Morton" & pitch_master_tbl$pitch_type == "FF1" & pitch_master_tbl$prev_pitch_type == "CU2"]
              ~ pitch_master_tbl$plate_x[pitch_master_tbl$player_name == "Charlie Morton" & pitch_master_tbl$pitch_type == "FF1" & pitch_master_tbl$prev_pitch_type == "CU2"],
              nbin = 1000,
              colramp = colorRampPalette(c(buylrd)),
              cex = .7,
              main="Charlie Morton Fastball Location",
              xlab="Horizontal Location", 
              ylab="Vertical Location", 
              xlim=c(-2,2), 
              ylim=c(0,6))
rect(-0.708333, 1.492, 0.708333, 3.373, border="black", lty="dotted", lwd=3)

### Graph the FB% and CU% by season for Hardy, Sanchez, Mengden
trio_pc <- sqldf("
	select game_year,
	     player_name,
		   pitcher,
		   p_throws,
		   count(case when pitch_type = 'FF1' then 'FF1' end) count_high_fb,
		   count(case when pitch_type = 'FF1' or pitch_type = 'FF2' then 'FF' end) count_all_fb,
		   count(case when pitch_type = 'CU2' then 'CU2' end) count_low_cu,
		   count(case when pitch_type = 'CU2' or pitch_type = 'CU1' then 'CU' end) count_all_cu,
		   count(*) count_pitch
	from pitch_master_tbl 
	where player_name in ('Aaron Sanchez','Blaine Hardy','Daniel Mengden')
	group by pitcher, game_year
	order by pitcher, game_year, count_pitch desc
	")

trio_pc$high_fb_pct <- trio_pc$count_high_fb / trio_pc$count_pitch
trio_pc$all_fb_pct <- trio_pc$count_all_fb / trio_pc$count_pitch
trio_pc$low_cu_pct <- trio_pc$count_low_cu / trio_pc$count_pitch
trio_pc$all_cu_pct <- trio_pc$count_all_cu / trio_pc$count_pitch

ggplot(trio_pc %>% filter(player_name == "Aaron Sanchez"), aes(game_year)) + 
  geom_line(aes(y = high_fb_pct, colour = "High FB%"), size = 2) + 
  geom_line(aes(y = low_cu_pct, colour = "Low CU%"), size = 2) +
  xlab("Season") +
  ylab("Pitch Frequency") +
  ylim(0,0.35) +
  ggtitle("Aaron Sanchez") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(trio_pc %>% filter(player_name == "Blaine Hardy"), aes(game_year)) + 
  geom_line(aes(y = high_fb_pct, colour = "High FB%"), size = 2) + 
  geom_line(aes(y = low_cu_pct, colour = "Low CU%"), size = 2) +
  xlab("Season") +
  ylab("Pitch Frequency") +
  ylim(0,0.35) +
  ggtitle("Blaine Hardy") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(trio_pc %>% filter(player_name == "Daniel Mengden"), aes(game_year)) + 
  geom_line(aes(y = high_fb_pct, colour = "High FB%"), size = 2) + 
  geom_line(aes(y = low_cu_pct, colour = "Low CU%"), size = 2) +
  xlab("Season") +
  ylab("Pitch Frequency") +
  ylim(0,0.35) +
  ggtitle("Daniel Mengden") +
  theme(plot.title = element_text(hjust = 0.5))


