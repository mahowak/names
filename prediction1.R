library(tidyverse)

korea = read_csv("Data/Korea/ korea.csv", col_names = NA)
vietnam = read_csv("Data/Vietnam (US 2000)/viet usa 2000.csv")
vietnam$asian.count = vietnam$count * vietnam$pctapi/100
krank = rev(sort(korea$X6))[1:50]
vietrank = rev(sort(vietnam$asian.count))[1:50]
plot(krank, vietrank)
cor(krank, vietrank)
summary(lm(krank ~ vietrank))

###############################################
beith = read_csv("Data/Scotland/beith.csv", col_names = NA)
dingwall = read_csv("Data/Scotland/dingwall.csv", col_names = NA)
govan = read_csv("Data/Scotland/govan.csv", col_names = NA)
earlstone = read_csv("Data/Scotland/earlstone.csv", col_names = NA)
nengland = read_csv("Data/northern_england.csv", col_names = NA)
################################################
usn = read_csv("Data/yob2010.txt", col_names=F)

fins = read_csv("finnish_data/births.csv")
fins_post1900 = filter(fins, birth_year > 1900) %>%
  group_by(child_first_nameN) %>%
  summarise(name_count=n()) %>%
  arrange(-name_count)

fins_pre1800 = filter(fins, birth_year < 1800) %>%
  group_by(child_first_nameN) %>%
  summarise(name_count=n()) %>%
  arrange(-name_count)

###################################################
# summary data
sum.d = tibble(region = c("Korea", "Vietnam", "Beith", 
                          "Govan", "Dingwall", "Earlstone",
                          "Northern England",
                          "Finland Pre-1800", "Finland Post-1900"),
              `name count` = c(length(unique(korea$X1)),
                                length(unique(vietnam$name)),
                                length(unique(beith$X1)),
                                length(unique(govan$X1)),
                                length(unique(dingwall$X4)),
                                length(unique(earlstone$X4)),
                                length(unique(nengland$X1)),
                                length(unique(fins_pre1800$child_first_nameN)),
                                length(unique(fins_post1900$child_first_nameN))
              ),
               `population` = c(sum(korea$X6, na.rm=T),
                                round(sum(vietnam$asian.count, na.rm=T)),
                                sum(beith$X4, na.rm=T),
                                sum(govan$X4, na.rm=T),
                                sum(dingwall$X4, na.rm=T),
                                sum(earlstone$X4, na.rm=T),
                                sum(nengland$X2, na.rm=T),
                                sum(fins_pre1800$name_count, na.rm=T),
                                sum(fins_post1900$name_count, na.rm=T))
)
sum.d %>%
  mutate(population = as.character(population)) %>%
  xtable() %>%
  print(include.rownames=FALSE, digits=0)

d = tibble(Korean=krank, `Vietnamese-American`=vietrank,
           Beith=beith$X4[1:50], Dingwall=dingwall$X4[1:50], 
           Govan=govan$X4[1:50], Earlstone=earlstone$X4[1:50],
           US=usn$X3[1:50], 
           `Northern England` = nengland$X2[1:50],
           FinnsPost1900 = fins_post1900$name_count[1:50],
           FinnsPre1800 = fins_pre1800$name_count[1:50])


###################################################
# SI plot pairwise
ggplot(d, aes(x=Korean, y=`Vietnamese-American`)) + geom_point() + 
  theme_classic(12) + 
  geom_smooth(method=lm, se=F) + scale_x_log10() + scale_y_log10()

d.g = d %>%
  gather(variable, value) %>%
  filter(variable %in% c("Northern England",
                            "Dingwall",
                            "Korean",
                            "Vietnamese-American",
                            "US")) %>%
  group_by(variable) %>%
  mutate(r= rank(-value)) 
d.g2 = rename(d.g, variable2 = variable, value2=value)
d.g3 = left_join(d.g, d.g2) 
d.g3 = mutate(d.g3, value = log10(value),
              value2=log10(value2))

ggplot(d.g3, aes(x=value, y=value2)) + geom_point() + 
  theme_bw(12) + 
  #geom_smooth(method=lm, se=F) + 
  scale_x_log10() + scale_y_log10() + 
  facet_grid(variable ~ variable2, scales = "free")  +
  xlab("Log frequency") + ylab("Log frequency")
ggsave("imgs/pairwise_sample.png", width=7, height=8)


##################################################
# SI plot pairwise
ggplot(d, aes(x=Korean, y=`Vietnamese-American`)) + geom_point() + 
  theme_classic(12) + 
  geom_smooth(method=lm, se=F) + scale_x_log10() + scale_y_log10()

d.g = d %>%
  gather(variable, value) %>%
  filter(variable %in% c("Beith",
                         "Dingwall",
                         "Earlstone",
                         "Govan")) %>%
  group_by(variable) %>%
  mutate(r= rank(-value)) 
d.g2 = rename(d.g, variable2 = variable, value2=value)
d.g3 = left_join(d.g, d.g2) 
d.g3 = mutate(d.g3, value = log10(value),
              value2=log10(value2))

ggplot(d.g3, aes(x=value, y=value2)) + geom_point() + 
  theme_bw(12) + 
  #geom_smooth(method=lm, se=F) + 
  scale_x_log10() + scale_y_log10() + 
  facet_grid(variable ~ variable2, scales = "free") +
  xlab("Log frequency") + ylab("Log frequency")
ggsave("imgs/scottish_pairwise.png", width=7, height=8)
###################################################


d.sum = gather(d, Locale, value) %>%
  group_by(Locale) %>%
  mutate(count=value,
          value=value/max(value),
         r = 1:n())

ag2 = ggplot(d.sum, 
             aes(x=r, y=value, group=Locale, colour=Locale)) + 
  geom_line() + 
  geom_point(size=1) +
  ylab("Proportion Relative to Top Name") +
  xlab("Name rank") +
  theme_bw(12) + 
  scale_colour_manual(values = c("Vietnamese-American" =  "blue", "Korean" = "lightblue",
                                 "Earlstone" = "orange", "Govan" = "darkorange",
                                 "Dingwall" = "sienna", "Beith" = "#BF360C",
                                 "Northern England" = "#B45F06",
                                 "FinnsPost1900" = "darkgray",
                                 "FinnsPre1800" = "gray",
                                 "US" = "red"))
ag2
ggsave("imgs/proportion_relative_to_top.png", width=8, height=4)

# get entropies
ents = group_by(d.sum, Locale) %>%
  mutate(locale.sum = sum(value),
         prob = value/locale.sum) %>%
  summarise(ent=-sum(prob*log2(prob)))
arrange(ents, -ent)

