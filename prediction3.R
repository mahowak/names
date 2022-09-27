library(tidyverse)
library(ggrepel)
library(gridExtra)
library(stringdist)

d = read.csv("data/downsampled_scinames.csv")
d$first.init = substr(d$first, 1, 1)
d$last.init = substr(d$last, 1, 1)
d$first.plus.last = paste(d$first, d$last)
d$first.init.last = paste(d$first.init, d$last)
d$first.last.init = paste(d$first, d$last.init)
d = separate(d, first, sep="[ -]", into=c("x", "middle"), remove=F)
d[is.na(d$middle), "middle"] = ""
d$korean.first.two.last = paste(toupper(substr(d$first, 1, 1)), toupper(substr(d$middle, 1, 1)), d$last)
group_by(d, first) %>% summarise(n=n()) %>% arrange(-n)

# get median by country
s = median(table(d$country))

# Plot duplicate counts for each type of name
d.dupe = group_by(d, country) %>%
  filter(!grepl("patronym", country)) %>%
  summarise(`Given` = sum(duplicated(first)),
            `Family` = sum(duplicated(last)),
            `Given Init. + \nFamily` = sum(duplicated(first.init.last)),
            `Given + \nFamily Init.` = sum(duplicated(first.last.init))) %>%
  gather(variable, value, -country) %>%
  mutate(mean.value.per1000 = 1000 * (value/425))

d.dupe$variable = gsub("\n", "", d.dupe$variable)

dupe.helper = read_csv("data/plot_dupes_helper.csv")
d.dupe$variable = gsub("\n", "", d.dupe$variable)
d.dupe.helper = left_join(d.dupe, dupe.helper)

d.dupe.helper$IsClear = d.dupe.helper$value < 30
d.dupe.helper$Nonwestern = d.dupe.helper$country %in% c("korean", "china")
d.dupe.helper$hasplus = grepl("\\+", d.dupe.helper$variable)

d.dupe.helper$CountryImproved = factor(d.dupe.helper$CountryImproved, levels=c("China", "South Korea", "Finland", "France", "Russia", "U.S."))
d.dupe.helper$full.family = ifelse(d.dupe.helper$variable %in% c("Family", "Given Init. + Family"),
                                   "Full Family Name",
                                   "Full Given Name")
d.dupe.helper$has.init = ifelse(grepl("Init", d.dupe.helper$variable), "Initals", "No initials")
p1 = ggplot(d.dupe.helper, aes(x=CountryImproved, y=value, alpha=IsClear, group=variable)) + geom_bar(stat='identity', position=position_dodge()) +
  theme_bw(12) +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none")  +
  coord_flip() +
  geom_text(data=d.dupe.helper, aes(x=CountryImproved, y=0, label=example, group=variable),hjust=1, position= position_dodge(width = .9))  +
  scale_y_continuous(breaks=c(0, 100, 200,300), limits=c(-60, 350)) + 
  ylab("Number of Duplications") + xlab("") + 
  scale_alpha_discrete(range=c(.5, 1)) + 
  facet_grid( full.family ~ ., scales="free",space="free", drop=T) #+
ggsave("imgs/dupe_graph2.png", width=7, height=4)


maxdist = rep(1/max(table(d$country)), max(table(d$country)))
maxent = -sum(maxdist * log2(maxdist))

korean.new = d[d$country == "korean", ]
korean.new$country = "Korea-2"
korean.new$first.init.last = paste(toupper(substr(korean.new$first, 1, 1)),
                                   toupper(substr(korean.new$middle, 1, 1)),
                                   korean.new$last )

russia.pat = d[d$country == "russia_patronyms", ]
russia.pat$country = "Russia-2"
russia.pat$first.init.last = paste(toupper(substr(russia.pat$first, 1, 1)),
                                   toupper(substr(russia.pat$patronym, 1, 1)),
                                   russia.pat$last )


d.ent.base = bind_rows(d[grepl("patr", d$country) == F, ], korean.new, russia.pat)

first.ent = group_by(d.ent.base, first.last.init, country) %>%
  summarise(freq = n()/s) %>%
  group_by(country) %>%
  summarise(first.ent = -sum(freq*log2(freq))) 

last.ent = group_by(d.ent.base, first.init.last, country) %>%
  summarise(freq = n()/s) %>%
  group_by(country) %>%
  summarise(last.ent = -sum(freq*log2(freq)))

d.ents = left_join(first.ent, last.ent)

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

d.ents[d.ents$country == "us", "country"] = "U.S."
d.ents[d.ents$country == "koraen", "country"] = "Korea"

d.ents$country = firstup(d.ents$country)

p2 = ggplot(d.ents, aes(x=first.ent, y=last.ent, label=country)) +
  geom_text_repel(size=4) + 
  geom_hline(yintercept=maxent, lty="solid", colour="darkred", size=1, alpha=.5) +
  geom_vline(xintercept=maxent, lty="solid", colour="darkred", size=1, alpha=.5) + 
  geom_point() + 
  xlab("given name + hereditary initial entropy\n(Charles D.)") + ylab("given initial + hereditary name entropy\n(C. Darwin)")+
  theme_bw(14) 

png("imgs/double_scinames.png", width=1000, height=400)
grid.arrange(p1, p2, ncol=2)
dev.off()

# get number of neighbors for each word
get.neighbors.per.word = function(ctry, column) {
  co = filter(d, country == ctry)
  group_by(co, country) %>%
    mutate(neighbors = 
             sum(sapply(co[, column], function(x) 
             {stringdist(x, rlang::sym(column))}) == 1)
    ) %>%
    select(rlang::sym(column), neighbors)
}

china = filter(d, country == "china")
china.usway = group_by(china, country) %>%
  mutate(neigh = 
           rowSums(sapply(china$first.init.last, function(x) 
           {stringdist(x, first.init.last)}) <= 1) - 1
  ) %>%
  select(name=first.init.last, neigh) %>%
  mutate(mode = "China (given init. + family name):\nX. Li",
         type = "given init.")

china = filter(d, country == "china")
china.chinaway = group_by(china, country) %>%
  mutate(neigh = 
           rowSums(sapply(china$first.last.init, function(x) 
           {stringdist(x, first.last.init)}) <= 1) - 1
  ) %>%
  select(name=first.last.init, neigh) %>%
  mutate(mode = "China (family init. + given name):\nL. Xiaoping",
         type = "family init.")

us = filter(d, country == "us")
us.usway = group_by(us, country) %>%
  mutate(neigh = 
           rowSums(sapply(us$first.last.init, function(x) 
           {stringdist(x, first.last.init)}) <= 1) - 1
  ) %>%
  select(name=first.last.init, neigh) %>%
  mutate(mode = "US (family init. + given name):\nJamal S.",
         type = "family init.")

us = filter(d, country == "us")
us.chinaway = group_by(us, country) %>%
  mutate(neigh = 
           rowSums(sapply(us$first.init.last, function(x) 
           {stringdist(x, first.init.last)}) <= 1) - 1
  ) %>%
  select(name=first.init.last, neigh) %>%
  mutate(mode = "US (given init. + family name):\nJ. Smith",
         type = "given init.")

korea = filter(d, country == "korean")
korea.usway = group_by(korea, country) %>%
  mutate(neigh = 
           rowSums(sapply(korea$first.last.init, function(x) 
           {stringdist(x, first.last.init)}) <= 1) - 1
  ) %>%
  select(name=first.last.init, neigh) %>%
  mutate(mode = "Korea (given init. + family name):\nK. Sang Yong",
         type = "family init.")

korea = filter(d, country == "korean")
korea.chinaway = group_by(korea, country) %>%
  mutate(neigh = 
           rowSums(sapply(korea$first.init.last, function(x) 
           {stringdist(x, first.init.last)}) <= 1) - 1
  ) %>%
  select(name=first.init.last, neigh) %>%
  mutate(mode = "Korea (family init. + given name):\nS. Kim",
         type = "given init.")


france = filter(d, country == "france")
france.usway = group_by(france, country) %>%
  mutate(neigh = 
           rowSums(sapply(france$first.last.init, function(x) 
           {stringdist(x, first.last.init)}) <= 1) - 1
  ) %>%
  select(name=first.last.init, neigh) %>%
  mutate(mode = "France (given + patronym init.):\nM. Curie",
         type = "family init.")

france.chinaway = group_by(france, country) %>%
  mutate(neigh = 
           rowSums(sapply(france$first.init.last, function(x) 
           {stringdist(x, first.init.last)}) <= 1) - 1
  ) %>%
  select(name=first.init.last, neigh) %>%
  mutate(mode = "France (given init. + patronym):\nMarie C.",
         type = "given init.")


russia = filter(d, country == "russia")
russia.usway = group_by(russia, country) %>%
  mutate(neigh = 
           rowSums(sapply(russia$first.last.init, function(x) 
           {stringdist(x, first.last.init)}) <= 1) - 1
  ) %>%
  select(name=first.last.init, neigh) %>%
  mutate(mode = "Russia (given + patronym init.):\nAndrey K.",
         type = "family init.")

russia.chinaway = group_by(russia, country) %>%
  mutate(neigh = 
           rowSums(sapply(russia$first.init.last, function(x) 
           {stringdist(x, first.init.last)}) <= 1) - 1
  ) %>%
  select(name=first.init.last, neigh) %>%
  mutate(mode = "Russia (given init. + family):\nA. Kolmogorov",
         type = "given init.")

finnish = filter(d, country == "finland")
finnish.usway = group_by(finnish, country) %>%
  mutate(neigh = 
           rowSums(sapply(finnish$first.last.init, function(x) 
           {stringdist(x, first.last.init)}) <= 1) - 1
  ) %>%
  select(name=first.last.init, neigh) %>%
  mutate(mode = "Finland (given + patronym init.):\nEero S.",
         type = "family init.")

finnish.chinaway = group_by(finnish, country) %>%
  mutate(neigh = 
           rowSums(sapply(finnish$first.init.last, function(x) 
           {stringdist(x, first.init.last)}) <= 1) - 1
  ) %>%
  select(name=first.init.last, neigh) %>%
  mutate(mode = "Finland (given init. + family):\nE. Saarinen",
         type = "given init.")


neighs = bind_rows(china.usway, us.usway, us.chinaway, china.chinaway,
                   korea.usway, korea.chinaway, france.usway, france.chinaway,
                   russia.usway, russia.chinaway,
                   finnish.usway, finnish.chinaway)


neighs$type.pretty = ifelse(neighs$type == "given init.", "given init. + family name (C. Darwin / X. Li)",
                            "given name + family init. (Charles D. / Xiaoping L.)")

neighbors.per.1000 = group_by(neighs, country, mode) %>%
  summarise(mean.neighbors.per.100=1000 * mean(neigh)/n())

group_by(neighs, mode) %>%
  summarise(m=mean(neigh))

group_by(neighs, mode) %>%
  mutate(m=max(neigh)) %>%
  filter(neigh == m) %>%
  data.frame()


# Study a common name in China

a = sapply(china$first.init.last, function(x) 
{stringdist(x, "W Wang")})
xwang = tibble(count=a, name = names(a))
filter(xwang, count <= 1) %>% select(name) %>% 
  arrange(name) %>% data.frame()

filter(d, country == "china", first.init.last == "S Wang") %>%
  nrow()