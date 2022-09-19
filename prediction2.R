library(tidyverse)
library("ggmap")
library(maptools)
library(maps)
library(lme4)
require(gridExtra)

d = read_csv("finnish_data/births.csv")

# Use Convert Lats Lons.ipynb to get the good lat lon
# convert using the genealogy repo from github.com/ekQ/genealogy.git
parishes = read_csv("finnish_data/parish_with_lat_lon.csv") %>%
  select(parish_id = id, name, lon=good_lon, lat=good_lat)

d = left_join(d, parishes) %>%
  filter(birth_year > 1000)

# view data
select(filter(d, lon > 30), dad_first_nameN, dad_patronymN,
       dad_last_nameN, mom_patronymN, child_first_nameN) %>%
  ungroup() %>%
  sample_n(10)

select(filter(d, lon < 21), dad_first_nameN, dad_patronymN, 
       dad_last_nameN, mom_patronymN,child_first_nameN) %>%
  ungroup() %>%
  sample_n(10)



select(filter(d, lon < 21), dad_first_nameN, dad_patronymN, 
       dad_last_nameN, mom_patronymN,child_first_nameN) %>%
  ungroup() %>%
  filter(is.na(dad_last_nameN) == F) %>%
  sample_n(20)


select(filter(d, lon > 30), dad_first_nameN, dad_patronymN, 
       dad_last_nameN, mom_patronymN,child_first_nameN) %>%
  ungroup() %>%
  filter(is.na(dad_last_nameN) == F) %>%
  sample_n(20)


length(unique(d$parish_id))

min(d$birth_year)
max(d$birth_year)

summary(d$birth_year)
d$birth_year_cut = cut(d$birth_year, breaks = c(-Inf, 1730, 1780, 1830, 1880, Inf),
                       labels=c("up to 1730", "1730-1780", "1780-1830", "1830-1880", "post-1880"))

# sample in groups of 50
samp_num = 50
set.seed(42)
s = group_by(d, parish_id, birth_year_cut) %>%
  mutate(count = n()) %>%
  filter(count >= samp_num) %>%
  sample_n(samp_num)

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
ggsave("first_name_ent.png", width=7, height=4)  

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


##########
d$hasLast = is.na(d$dad_last_nameN) == F

has.last.parishes = d %>%
  group_by(parish_id, lon, lat, birth_year_cut) %>%
  summarise(mean.hasLast = mean(hasLast, na.rm=T)) %>%
  ungroup() %>%
  mutate(`Proportion with hereditry patronym` = mean.hasLast) #scale(mean.hasLast)[, 1])

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
select(first.last, scale.pat, scale.first.ent, birth_year_cut) %>%
  na.omit() %>%
  summarise(cor=cor(scale.pat, scale.first.ent, method="spearman"))
