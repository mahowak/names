library(tidyverse)
library(poweRlaw)

korea = read_csv("Data/Korea/ korea.csv", col_names = NA)
vietnam = read_csv("Data/Vietnam (US 2000)/viet usa 2000.csv")
krank = rev(sort(korea$X6))[1:50]
vietrank = rev(sort(vietnam$count))[1:50]
plot(krank, vietrank)
cor(krank, vietrank)
summary(lm(krank ~ vietrank))

###############################################
beith = read_csv("Data/Scotland/beith.csv", col_names = NA)
dingwall = read_csv("Data/Scotland/dingwall.csv", col_names = NA)
govan = read_csv("Data/Scotland/govan.csv", col_names = NA)
earlstone = read_csv("Data/Scotland/earlstone.csv", col_names = NA)

################################################
usn = read_csv("Data/yob2010.txt", col_names=F)

fins = read_csv("finnish_data/births.csv")
fins_post1900 = filter(fins, birth_year > 1900) %>%
  group_by(child_first_nameN) %>%
  summarise(name_count=n()) %>%
  arrange(-name_count)
d$FinnsPost1900 = fins_post1900$name_count[1:50]

fins_pre1800 = filter(fins, birth_year < 1800) %>%
  group_by(child_first_nameN) %>%
  summarise(name_count=n()) %>%
  arrange(-name_count)
d$FinnsPre1800 = fins_pre1800$name_count[1:50]

###################################################

d = tibble(Korean=krank, `Vietnamese-American`=vietrank,
           Beith=beith$X4[1:50], Dingwall=dingwall$X4[1:50], 
           Govan=govan$X4[1:50], Earlstone=earlstone$X4[1:50],
           US=usn$X3[1:50], 
           FinnsPost1900 = fins_post1900$name_count[1:50],
           FinnsPre1800 = fins_pre1800$name_count[1:50])

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
                                 "FinnsPost1900" = "darkgray",
                                 "FinnsPre1800" = "gray",
                                 "US" = "red"))
ag2
ggsave("proportion_relative_to_top.png", width=8, height=4)

# get entropies
ents = group_by(d.sum, Locale) %>%
  mutate(locale.sum = sum(value),
         prob = value/locale.sum) %>%
  summarise(ent=-sum(prob*log2(prob)))
arrange(ents, -ent)
