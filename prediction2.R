library(tidyverse)
library("ggmap")
library(maptools)
library(maps)
library(lme4)
require(gridExtra)
library(xtable)

# to run this part, we need data from the original Finnish source

# d = read_csv("finnish_data/births.csv")
# 
# # Use Convert Lats Lons.ipynb to get the good lat lon
# # convert using the genealogy repo from github.com/ekQ/genealogy.git
# parishes = read_csv("finnish_data/parish_with_lat_lon.csv") %>%
#   select(parish_id = id, name, lon=good_lon, lat=good_lat)
# 
# d = left_join(d, parishes) %>%
#   filter(birth_year > 1000)
# length(unique(d$parish_id))
# 
# d = select(d, birth_year, 
#                   parish_id,
#                   child_first_nameN,
#                   dad_last_nameN,
#            lon, lat)
# write_csv(d, "data/finnish_data_selected.csv")

d = read_csv("data/finnish_data_selected.csv")

min(d$birth_year)
max(d$birth_year)
summary(d$birth_year)

d$birth_year_cut = cut(d$birth_year, breaks = c(-Inf, 1730, 1780, 1830, 1880, Inf),
                       labels=c("up to 1730", "1730-1780", "1780-1830", "1830-1880", "post-1880"))

table(d$birth_year_cut) %>%
  xtable()
# sample in groups of 50
samp_num = 50
set.seed(42)
s = group_by(d, parish_id, birth_year_cut) %>%
  mutate(count = n()) %>%
  filter(count >= samp_num) %>%
  sample_n(samp_num)

length(table(s$parish_id))
nrow(s)

# first name entropy
first.ent = group_by(s, child_first_nameN, parish_id, birth_year_cut, lon, lat) %>%
  summarise(freq = n()/samp_num) %>%
  group_by(parish_id, birth_year_cut, lon, lat) %>%
  summarise(first.ent = -sum(freq*log2(freq)),
            n=n()) 

first.ent.early = first.ent 
first.ent.early$first.ent.normalize = scale(first.ent.early$first.ent)


p1 = ggplot(first.ent.early, aes(x=lon, y=lat, colour=first.ent)) + 
  borders(regions = c("Finland"), colour = "gray50", fill = "gray50") +
  geom_point(size=1) + 
  coord_quickmap() + 
  scale_colour_gradient2(
    low = ("red"),
    mid = "white",
    high = ("blue"),
    midpoint = median(first.ent.early$first.ent),
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour"
  ) +
  theme_bw() + 
  facet_wrap(~birth_year_cut, ncol=5) +
  xlab("Longitude") + 
  ylab("Latitude") + 
  labs(colour="Prefix-name entropy") +
  theme(legend.position="bottom")
p1
ggsave("imgs/first_name_ent.png", width=7, height=4)  

summary(lm(data=first.ent.early, first.ent ~ lon))
summary(lm(data=first.ent.early, first.ent ~ lat))

first.ent.early$birthyearnum = as.numeric(as.factor(first.ent.early$birth_year_cut))
l = lmer(data=first.ent.early, 
         first.ent ~ birthyearnum * lon + (birthyearnum | parish_id),
         REML= F)
l0 = lmer(data=first.ent.early, 
          first.ent ~ birthyearnum + lon + (birthyearnum | parish_id),
          REML=F)
anova(l, l0)
summary(l)

##########
d$hasLast = is.na(d$dad_last_nameN) == F

has.last.parishes = d %>%
  group_by(parish_id, lon, lat, birth_year_cut) %>%
  summarise(mean.hasLast = mean(hasLast, na.rm=T)) %>%
  ungroup() %>%
  mutate(`Proportion with hereditry patronym` = mean.hasLast) #scale(mean.hasLast)[, 1])

summary(lm(data=filter(has.last.parishes, birth_year_cut == "up to 1730"),
           mean.hasLast ~ lon))  
d$birth_year_cut_num = as.numeric(as.factor(d$birth_year_cut))

d$east.vs.west = ifelse(d$lon > median(d$lon, na.rm=T), "east", "west")
group_by(d, east.vs.west, birth_year_cut) %>%
  summarise(mean.hasLast = mean(hasLast)) %>%
  filter(is.na(east.vs.west) == F) %>%
  spread(east.vs.west, mean.hasLast) %>%
  xtable()

has.last.parishes$birth_year_cut_num = as.numeric(as.factor(has.last.parishes$birth_year_cut))
l.par = lmer(mean.hasLast ~ lon * birth_year_cut_num + (1 + birth_year_cut_num|parish_id),
             data=has.last.parishes)
l.par.noint = lmer(mean.hasLast ~ lon + birth_year_cut_num + (1 + birth_year_cut_num|parish_id),
             data=has.last.parishes)

summary(l.par)
anova(l.par, l.par.noint)

p2 = ggplot(has.last.parishes, aes(x=lon, y=lat, colour=`Proportion with hereditry patronym`)) + 
  borders(regions = "Finland", colour = "gray50", fill = "gray50") +
  geom_point() + 
  coord_quickmap() + 
  scale_colour_gradient2(
    low = ("red"),
    mid = "white",
    high = ("blue"),
    midpoint = .5,
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour"
  )  +
  theme_bw(12) +
  facet_wrap(~birth_year_cut, ncol=5) +

  xlab("Longitude") + 
  ylab("Latitude") +
  theme(legend.position="bottom")
p2

grid.arrange(p2, p1, ncol=1)

summary(lm(data=has.last.parishes, mean.hasLast ~ lon))

# compare parishes lat name pct with first name ent
first.last = left_join(has.last.parishes, first.ent.early)
first.last$scale.first.ent = scale(first.last$first.ent)[, 1]
first.last$scale.pat = scale(first.last$`Proportion with hereditry patronym`)[, 1]
ggplot(first.last, aes(x=`scale.pat`, y=scale.first.ent)) +
  geom_point() + 
  geom_smooth(method=lm)
summary(lm(first.last$scale.pat ~  first.last$scale.first.ent))

select(first.last, scale.pat, lon, scale.first.ent, birth_year_cut) %>%
  na.omit() %>%
  group_by(birth_year_cut) %>%
  summarise(`Corr: Patronyms:FirstNameEnt`=cor(scale.pat, scale.first.ent, method="spearman"),
            `Corr: Patronyms:Longitude` = cor(scale.pat, lon, method="spearman"),
            `Corr: FirstNameEnt:Longitude` = cor(scale.first.ent, lon, method="spearman")) %>%
  xtable()

select(first.last, scale.pat, lon, birth_year_cut) %>%
  na.omit() %>%
  group_by(birth_year_cut) %>%
  summarise(cor=cor(scale.pat, lon, method="spearman")) %>%
  xtable()


select(first.last, scale.first.ent, lon, birth_year_cut) %>%
  na.omit() %>%
  group_by(birth_year_cut) %>%
  summarise(cor=cor(scale.first.ent, lon, method="spearman")) %>%
  xtable()

