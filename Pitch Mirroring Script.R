library(ggplot2)
library(plotly)
library(dplyr)
library(humaniformat)

df <- read.csv("Pitch Mirroring Data.csv", header = TRUE)
names(df) <- c("pitcher_team","pitcher","spin_axis","rel_dist","move","spin_eff")

str(df)

# clusters <- kmeans(df[,3:6], 8)
# df$cluster <- clusters$cluster
# 
# aggregate(df[,3:6], list(df$cluster), mean)
# 
# 
# df[,3:7] %>% 
#   group_by(cluster) %>% 
#   summarize(min = min(spin_axis),
#             q1 = quantile(spin_axis, 0.25),
#             mean = mean(spin_axis),
#             median = median(spin_axis),
#             q3 = quantile(spin_axis, 0.75),
#             max = max(spin_axis))

### Using percentiles
perc.rank <- function(x) trunc(rank(x))/length(x)

df_pct <- df[,1:6]
df_pct$rel_dist <- 1 - perc.rank(df$rel_dist)
df_pct$move <- perc.rank(df$move)
df_pct$spin_eff <- 1 - perc.rank(df$spin_eff)

### Pitchers with mirrored spin axes
level_one <- df_pct %>%
  filter(spin_axis >= -195 
         & spin_axis <= -165)

### Pitchers with mirrored spin axes and spin efficiency
level_two <- df_pct %>%
  filter(spin_axis >= -195 
         & spin_axis <= -165
         & spin_eff >= .8)

### Pitchers with mirrored spin axes, spin efficiency, and tunneling
level_three <- df_pct %>%
  filter(spin_axis >= -195 
         & spin_axis <= -165
         & spin_eff >= .8
         & rel_dist >= .8)

### Pitchers with mirrored spin axes and one criterion away
close <- df_pct %>%
  filter(spin_axis >= -195 
         & spin_axis <= -165
         & (
           (rel_dist >= .8
           & move >= .8)
           | (rel_dist >= .8
              & spin_eff >= .8)
           | (move >= .8
              & spin_eff >= .8)
         ))

### Pitchers who need to fix tunneling to achieve pitch mirroring
tunnel <- df_pct %>%
  filter(spin_axis >= -195 
         & spin_axis <= -165
         & spin_eff >= .8
         & rel_dist < .8
         & move >= .8) 

### Pitchers with all criteria
mirror_pct <- df_pct %>%
  filter(spin_axis >= -195 
         & spin_axis <= -165
         & spin_eff >= .8
         & rel_dist >= .8
         & move >= .8) 

mirror_pct$pitcher <- format_reverse(mirror_pct$pitcher)


# Original metrics
mirror_org <- df %>%
  filter(pitcher %in% c("Erlin, Robbie",
                        "Bundy, Dylan",
                        "Gonzalez, Gio",
                        "Mengden, Daniel",
                        "Sanchez, Aaron",
                        "Lyles, Jordan",
                        "Hoffman, Jeff",
                        "Hardy, Blaine",
                        "Fiers, Mike",
                        "Duffy, Danny",
                        "Mikolas, Miles",
                        "Ferguson, Caleb",
                        "Beede, Tyler"))
mirror_org$pitcher <- format_reverse(mirror_org$pitcher)

### Bring in pitch sequencing data
mirror_seq <- pitcher_seqval %>%
  filter(player_name %in% c("Robbie Erlin",
                            "Dylan Bundy",
                            "Gio Gonzalez",
                            "Daniel Mengden",
                            "Aaron Sanchez",
                            "Jordan Lyles",
                            "Jeff Hoffman",
                            "Blaine Hardy",
                            "Mike Fiers",
                            "Danny Duffy",
                            "Miles Mikolas",
                            "Caleb Ferguson",
                            "Tyler Beede")) %>%
  select(player_name, high_fb_pct, low_cu_pct, seq_pct, fb_run_value_100, 
         cu_run_value_100, seq_runs_100, seq_value_100_v2) %>%
  rename(pitcher = player_name)

# Create master table of 13 pitch mirror candidates
mirror_tot <- merge(mirror_org, mirror_pct, by = c("pitcher_team","pitcher")) %>%
  select(pitcher_team, pitcher, spin_axis.x, rel_dist.x, rel_dist.y,
         move.x, move.y, spin_eff.x, spin_eff.y)
mirror_tot <- merge(mirror_tot, mirror_seq, by = "pitcher") %>%
  select(pitcher_team, pitcher, spin_axis.x, rel_dist.x, rel_dist.y,
         move.x, move.y, spin_eff.x, spin_eff.y, high_fb_pct, low_cu_pct,
         seq_pct, fb_run_value_100, cu_run_value_100, seq_runs_100, seq_value_100_v2)
mirror_tot <- merge(mirror_tot, overall_rv100, by = "pitcher") %>%
  rename(overall_rv100 = run_value_100, overall_rv100_pct = percentile)
mirror_tot <- merge(mirror_tot, fb_rv100 %>% select(pitcher, percentile), by = "pitcher") %>%
  rename(fb_rv100_pct = percentile)
mirror_tot <- merge(mirror_tot, cu_rv100 %>% select(pitcher, percentile), by = "pitcher") %>%
  rename(cu_rv100_pct = percentile)
mirror_tot <- merge(mirror_tot, seq_rv100 %>% select(pitcher, percentile), by = "pitcher") %>%
  rename(seq_rv100_pct = percentile)
mirror_tot <- merge(mirror_tot, iso_seq_rv100 %>% select(pitcher, percentile), by = "pitcher") %>%
  rename(iso_seq_rv100_pct = percentile)

  
names(mirror_tot) <- c("pitcher","team","spin_axis","rel_dist",
                       "rel_dist_pct","move","move_pct","spin_eff","spin_eff_pct",
                       "high_fb_pct", "low_cu_pct", "seq_pct", "fb_run_value_100",
                       "cu_run_value_100", "seq_runs_100", "iso_seq_value_100",
                       "overall_run_value_100", "overall_rv100_pct","fb_rv100_pct",
                       "cu_rv100_pct","seq_rv100_pct","iso_seq_rv100_pct")
mirror_tot <- mirror_tot[-c(2,10), ]
write.csv(mirror_tot, "Mirroring Candidates.csv")


# Graph the Sequence% by the Iso Sequence RV100 to identify candidates for pitch mix change
# Daniel Mengden, Aaron Sanchez, and Blaine Hardy
ggplot(mirror_tot, aes(x = seq_pct, y = iso_seq_value_100)) +
  geom_text(
    label = mirror_tot$pitcher, 
    check_overlap = F) +
  geom_smooth(data = mirror_tot[-c(1,2,3),], method = "lm", se = FALSE, fullrange = TRUE) +
  xlab("Sequence%") +
  xlim(0.000, 0.120) +
  ylab("Isolated Sequence RV100")




