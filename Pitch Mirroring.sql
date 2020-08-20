select pitcher, 
	tagged_pitch_type, 
	count(pitch_uid) num_pitch,
	avg(spin_rate) avg_spin_rate, 
	avg(spin_axis) avg_spin_axis, 
	avg(rel_height) avg_rel_height, 
	avg(rel_side) avg_rel_side
from trackman.trackman_data_corrected
where year(date) = 2019
	and league in ('AL','NL')
	and level = 'MLB'
	and pitcher_team not in ('AL_ASG','NL_ASG')
group by pitcher, tagged_pitch_type
order by pitcher, tagged_pitch_type;



-------------- Spin Axis
WITH pivoted as 
(select pitcher_team, 
	pitcher, 
	Fastball,
	Sinker,
	Slider,
	Curveball,
	Changeup,
	Cutter
from (select *,
		case when tagged_pitch_type = 'Curveball' and spin_axis < 180 then spin_axis + 360 else spin_axis end spin_axis_adj
	from trackman.trackman_data t
	where year(t.date) = 2019
		and t.league in ('AL','NL')
		and level = 'MLB'
		and pitcher_team not in ('AL_ASG','NL_ASG')) t
pivot
(avg(spin_axis_adj) for tagged_pitch_type in (Fastball, Sinker, Slider, Curveball, Changeup, Cutter)) p)

select pitcher_team,
	pitcher,
	avg(Fastball) fb_axis,
	avg(Sinker) si_axis,
	avg(Slider) sl_axis,
	avg(Curveball) cu_axis,
	avg(Changeup) ch_axis,
	avg(Cutter) ct_axis
from pivoted
group by pitcher_team, pitcher
order by pitcher_team, pitcher;


------------ Release Height
WITH pivoted as 
(select pitcher_team, 
	pitcher, 
	Fastball,
	Sinker,
	Slider,
	Curveball,
	Changeup,
	Cutter
from (select *
	from trackman.trackman_data t
	where year(t.date) = 2019
		and t.league in ('AL','NL')
		and level = 'MLB'
		and pitcher_team not in ('AL_ASG','NL_ASG')) t
pivot
(avg(rel_height) for tagged_pitch_type in (Fastball, Sinker, Slider, Curveball, Changeup, Cutter)) p)

select pitcher_team,
	pitcher,
	avg(Fastball) fb_rel_h,
	avg(Sinker) si_rel_h,
	avg(Slider) sl_rel_h,
	avg(Curveball) cu_rel_h,
	avg(Changeup) ch_rel_h,
	avg(Cutter) ct_rel_h
from pivoted
group by pitcher_team, pitcher
order by pitcher_team, pitcher;

------------ Release Side
WITH pivoted as 
(select pitcher_team, 
	pitcher, 
	Fastball,
	Sinker,
	Slider,
	Curveball,
	Changeup,
	Cutter
from (select *
	from trackman.trackman_data t
	where year(t.date) = 2019
		and t.league in ('AL','NL')
		and level = 'MLB'
		and pitcher_team not in ('AL_ASG','NL_ASG')) t
pivot
(avg(rel_side) for tagged_pitch_type in (Fastball, Sinker, Slider, Curveball, Changeup, Cutter)) p)

select pitcher_team,
	pitcher,
	avg(Fastball) fb_rel_s,
	avg(Sinker) si_rel_s,
	avg(Slider) sl_rel_s,
	avg(Curveball) cu_rel_s,
	avg(Changeup) ch_rel_s,
	avg(Cutter) ct_rel_s
from pivoted
group by pitcher_team, pitcher
order by pitcher_team, pitcher;

------------ Horiz Break
WITH pivoted as 
(select pitcher_team, 
	pitcher, 
	Fastball,
	Sinker,
	Slider,
	Curveball,
	Changeup,
	Cutter
from (select *
	from trackman.trackman_data t
	where year(t.date) = 2019
		and t.league in ('AL','NL')
		and level = 'MLB'
		and pitcher_team not in ('AL_ASG','NL_ASG')) t
pivot
(avg(horz_break) for tagged_pitch_type in (Fastball, Sinker, Slider, Curveball, Changeup, Cutter)) p)

select pitcher_team,
	pitcher,
	avg(Fastball) fb_horz,
	avg(Sinker) si_horz,
	avg(Slider) sl_horz,
	avg(Curveball) cu_horz,
	avg(Changeup) ch_horz,
	avg(Cutter) ct_horz
from pivoted
group by pitcher_team, pitcher
order by pitcher_team, pitcher;

------------ Vert Break
WITH pivoted as 
(select pitcher_team, 
	pitcher, 
	Fastball,
	Sinker,
	Slider,
	Curveball,
	Changeup,
	Cutter
from (select *
	from trackman.trackman_data t
	where year(t.date) = 2019
		and t.league in ('AL','NL')
		and level = 'MLB'
		and pitcher_team not in ('AL_ASG','NL_ASG')) t
pivot
(avg(induced_vert_break) for tagged_pitch_type in (Fastball, Sinker, Slider, Curveball, Changeup, Cutter)) p)

select pitcher_team,
	pitcher,
	avg(Fastball) fb_vert,
	avg(Sinker) si_vert,
	avg(Slider) sl_vert,
	avg(Curveball) cu_vert,
	avg(Changeup) ch_vert,
	avg(Cutter) ct_vert
from pivoted
group by pitcher_team, pitcher
order by pitcher_team, pitcher;

select date, auto_pitch_type, tagged_pitch_type, spin_axis, horz_break, induced_vert_break
from trackman.trackman_data
where pitcher = 'Anderson, Nick'
	and year(date) = 2019
	and level = 'MLB'
order by date