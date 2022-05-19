library(tm)
library(wordcloud)
library(plyr)
library(corpus)
setwd("/Users/ben/Documents/Data Science/Assignment/ASDM/Task4")
Review <- read.csv("tourist_accommodation_reviews.csv", header= TRUE)
Acc <- subset(Review, Location == " Rawai")
positive_lexicon <- read.csv("positive-lexicon.txt")
negative_lexicon <- read.csv("negative-lexicon.txt")
stopword <- gsub("[[:punct:]]", "", stopwords(kind="smart"))
words <- c("ismore","andmore","rawai", "phuket", "themore", "thb",
           "fufuucfufufubbfufuuffufuau")
par(mar=c(0,0,0,0))

write.csv(Acc,"attempt1.csv", quote = TRUE, row.names = FALSE)


Hotels = c("A Spoonful of Sugar", "Green Tamarind Kitchen", "The Family Restaurant",
           "Rossovivo Ristorante Italiano E Pizzeria", "The Islander", "Cashew Nuts Food",
           "Kook Restaurant","The Breakfast Hut", "Le Celtique", "Rawai View Cafe' & Bar",
           "Moo Restaurant", "Sala Mexicali", "The Oceanfront Restaurant and Bar at Kata Rocks",
           "Il Tagliere da Massimo", "Restaurant Treffpunkt", "Mando Restaurant & Steakhouse",
           "Surin Bay Inn Restaurant", "Delish Cafe", "Atsumi Raw Cafe", "Mookdee Seafood", 
           "COCONUT Bar & Restaurant - Rawai Beach", "Mamasita Mexican",  "Nikitas Beach Restaurant",
           "Hooters Phuket", "Red Chopsticks At Patong", "Lucky 13 Sandwich Rawai", "Norbu's Steakhouse",
           "Cafe Java", "Bon Island Restaurant", "Khun Pha")

#Hotel 1
SpoonfulSugar <- subset(Acc, Hotel.Restaurant.name == "A Spoonful of Sugar")
SpoonfulSugar_Text <- SpoonfulSugar$Review
SpoonfulSugar_Text <- iconv(enc2utf8(SpoonfulSugar_Text),sub="byte")
SpoonfulSugar_Text <- gsub("[[:punct:]]|http\\S+\\s*|[[:digit:]]|^ | $", "", SpoonfulSugar_Text)
SpoonfulSugar_Text <- gsub("\n", " ",SpoonfulSugar_Text)
SpoonfulSugar_Text <- gsub('", "', " ",SpoonfulSugar_Text)

SpoonfulSugar_Text <- Corpus(VectorSource(SpoonfulSugar_Text))
SpoonfulSugar_Text <- tm_map(SpoonfulSugar_Text, stripWhitespace)
SpoonfulSugar_Text <- tm_map(SpoonfulSugar_Text, stem_snowball)
SpoonfulSugar_Text <- tm_map(SpoonfulSugar_Text, tolower)
SpoonfulSugar_Text <- tm_map(SpoonfulSugar_Text, removeWords, stopword)
SpoonfulSugar_Text <- tm_map(SpoonfulSugar_Text, removeWords, words)

source("Setiment.R")
sentiment(SpoonfulSugar_Text)

#Hotel 2
GreenKitchen <- subset(Acc, Hotel.Restaurant.name == "Green Tamarind Kitchen")
GreenKitchen_Text <- GreenKitchen$Review
GreenKitchen_Text <- iconv(enc2utf8(GreenKitchen_Text),sub="byte")
GreenKitchen_Text <- gsub("[[:punct:]]|http\\S+\\s*|[[:digit:]]|^ | $", "", GreenKitchen_Text)
GreenKitchen_Text <- gsub("\n", " ",GreenKitchen_Text)
GreenKitchen_Text <- gsub('", "', " ",GreenKitchen_Text)

GreenKitchen_Text <- Corpus(VectorSource(GreenKitchen_Text))
GreenKitchen_Text <- tm_map(GreenKitchen_Text, stripWhitespace)
GreenKitchen_Text <- tm_map(GreenKitchen_Text, stem_snowball)
GreenKitchen_Text <- tm_map(GreenKitchen_Text, tolower)
GreenKitchen_Text <- tm_map(GreenKitchen_Text, removeWords, stopword)
GreenKitchen_Text <- tm_map(GreenKitchen_Text, removeWords, words)

source("Setiment.R")
sentiment(GreenKitchen_Text)

#Hotel 3
FamRest <- subset(Acc, Hotel.Restaurant.name == "The Family Restaurant")
FamRest_Text <- FamRest$Review
FamRest_Text <- iconv(enc2utf8(FamRest_Text),sub="byte")
FamRest_Text <- gsub("[[:punct:]]|http\\S+\\s*|[[:digit:]]|^ | $", "", FamRest_Text)
FamRest_Text <- gsub("\n", " ",FamRest_Text)
FamRest_Text <- gsub('", "', " ",FamRest_Text)

FamRest_Text <- Corpus(VectorSource(FamRest_Text))
FamRest_Text <- tm_map(FamRest_Text, stripWhitespace)
FamRest_Text <- tm_map(FamRest_Text, stem_snowball)
FamRest_Text <- tm_map(FamRest_Text, tolower)
FamRest_Text <- tm_map(FamRest_Text, removeWords, stopword)
FamRest_Text <- tm_map(FamRest_Text, removeWords, words)

source("Setiment.R")
sentiment(FamRest_Text)

#Hotel 4
Rossovivo <- subset(Acc, Hotel.Restaurant.name == "Rossovivo Ristorante Italiano E Pizzeria")
Rossovivo_Text <- Rossovivo$Review
Rossovivo_Text <- iconv(enc2utf8(Rossovivo_Text),sub="byte")
Rossovivo_Text <- gsub("[[:punct:]]|http\\S+\\s*|[[:digit:]]|^ | $", "", Rossovivo_Text)
Rossovivo_Text <- gsub("\n", " ",Rossovivo_Text)
Rossovivo_Text <- gsub('", "', " ",Rossovivo_Text)

Rossovivo_Text <- Corpus(VectorSource(Rossovivo_Text))
Rossovivo_Text <- tm_map(Rossovivo_Text, stripWhitespace)
Rossovivo_Text <- tm_map(Rossovivo_Text, stem_snowball)
Rossovivo_Text <- tm_map(Rossovivo_Text, tolower)
Rossovivo_Text <- tm_map(Rossovivo_Text, removeWords, stopword)
Rossovivo_Text <- tm_map(Rossovivo_Text, removeWords, words)

source("Setiment.R")
sentiment(Rossovivo_Text)

#Hotel 5
Islander <- subset(Acc, Hotel.Restaurant.name == "The Islander")
Islander_Text <- Islander$Review
Islander_Text <- iconv(enc2utf8(Islander_Text),sub="byte")
Islander_Text <- gsub("[[:punct:]]|http\\S+\\s*|[[:digit:]]|^ | $", "", Islander_Text)
Islander_Text <- gsub("\n", " ",Islander_Text)
Islander_Text <- gsub('", "', " ",Islander_Text)

Islander_Text <- Corpus(VectorSource(Islander_Text))
Islander_Text <- tm_map(Islander_Text, stripWhitespace)
Islander_Text <- tm_map(Islander_Text, stem_snowball)
Islander_Text <- tm_map(Islander_Text, tolower)
Islander_Text <- tm_map(Islander_Text, removeWords, stopword)
Islander_Text <- tm_map(Islander_Text, removeWords, words)

source("Setiment.R")
sentiment(Islander_Text)

#Hotel 6
Cashew <- subset(Acc, Hotel.Restaurant.name == "Cashew Nuts Food")
Cashew_Text <- Cashew$Review
Cashew_Text <- iconv(enc2utf8(Cashew_Text),sub="byte")
Cashew_Text <- gsub("[[:punct:]]|http\\S+\\s*|[[:digit:]]|^ | $", "", Cashew_Text)
Cashew_Text <- gsub("\n", " ",Cashew_Text)
Cashew_Text <- gsub('", "', " ",Cashew_Text)

Cashew_Text <- Corpus(VectorSource(Cashew_Text))
Cashew_Text <- tm_map(Cashew_Text, stripWhitespace)
Cashew_Text <- tm_map(Cashew_Text, stem_snowball)
Cashew_Text <- tm_map(Cashew_Text, tolower)
Cashew_Text <- tm_map(Cashew_Text, removeWords, stopword)
Cashew_Text <- tm_map(Cashew_Text, removeWords, words)

source("Setiment.R")
sentiment(Cashew_Text)

#Hotel7
Kook <- subset(Acc, Hotel.Restaurant.name == "Kook Restaurant")
Kook_Text <- Kook$Review
Kook_Text <- iconv(enc2utf8(Kook_Text),sub="byte")
Kook_Text <- gsub("[[:punct:]]|http\\S+\\s*|[[:digit:]]|^ | $", "", Kook_Text)
Kook_Text <- gsub("\n", " ",Kook_Text)
Kook_Text <- gsub('", "', " ",Kook_Text)

Kook_Text <- Corpus(VectorSource(Kook_Text))
Kook_Text <- tm_map(Kook_Text, stripWhitespace)
Kook_Text <- tm_map(Kook_Text, stem_snowball)
Kook_Text <- tm_map(Kook_Text, tolower)
Kook_Text <- tm_map(Kook_Text, removeWords, stopword)
Kook_Text <- tm_map(Kook_Text, removeWords, words)

source("Setiment.R")
sentiment(Kook_Text)

#Hotel 8
BreakfastHut <- subset(Acc, Hotel.Restaurant.name == "The Breakfast Hut")
BreakfastHut_Text <- BreakfastHut$Review
BreakfastHut_Text <- iconv(enc2utf8(BreakfastHut_Text),sub="byte")
BreakfastHut_Text <- gsub("[[:punct:]]|http\\S+\\s*|[[:digit:]]|^ | $", "", BreakfastHut_Text)
BreakfastHut_Text <- gsub("\n", " ",BreakfastHut_Text)
BreakfastHut_Text <- gsub('", "', " ",BreakfastHut_Text)

BreakfastHut_Text <- Corpus(VectorSource(BreakfastHut_Text))
BreakfastHut_Text <- tm_map(BreakfastHut_Text, stripWhitespace)
BreakfastHut_Text <- tm_map(BreakfastHut_Text, stem_snowball)
BreakfastHut_Text <- tm_map(BreakfastHut_Text, tolower)
BreakfastHut_Text <- tm_map(BreakfastHut_Text, removeWords, stopword)
BreakfastHut_Text <- tm_map(BreakfastHut_Text, removeWords, words)

source("Setiment.R")
sentiment(BreakfastHut_Text)

#Hotel9
LeCeltique <- subset(Acc, Hotel.Restaurant.name == "Le Celtique")
LeCeltique_Text <- LeCeltique$Review
LeCeltique_Text <- iconv(enc2utf8(LeCeltique_Text),sub="byte")
LeCeltique_Text <- gsub("[[:punct:]]|http\\S+\\s*|[[:digit:]]|^ | $", "", LeCeltique_Text)
LeCeltique_Text <- gsub("\n", " ",LeCeltique_Text)
LeCeltique_Text <- gsub('", "', " ",LeCeltique_Text)

LeCeltique_Text <- Corpus(VectorSource(LeCeltique_Text))
LeCeltique_Text <- tm_map(LeCeltique_Text, stripWhitespace)
LeCeltique_Text <- tm_map(LeCeltique_Text, stem_snowball)
LeCeltique_Text <- tm_map(LeCeltique_Text, tolower)
LeCeltique_Text <- tm_map(LeCeltique_Text, removeWords, stopword)
LeCeltique_Text <- tm_map(LeCeltique_Text, removeWords, words)

source("Setiment.R")
sentiment(LeCeltique_Text)

#Hotel10

RawaiCafe <- subset(Acc, Hotel.Restaurant.name == "Rawai View Cafe' & Bar")
RawaiCafe_Text <- RawaiCafe$Review
RawaiCafe_Text <- iconv(enc2utf8(RawaiCafe_Text),sub="byte")
RawaiCafe_Text <- gsub("[[:punct:]]|http\\S+\\s*|[[:digit:]]|^ | $", "", RawaiCafe_Text)
RawaiCafe_Text <- gsub("\n", " ",RawaiCafe_Text)
RawaiCafe_Text <- gsub('", "', " ",RawaiCafe_Text)

RawaiCafe_Text <- Corpus(VectorSource(RawaiCafe_Text))
RawaiCafe_Text <- tm_map(RawaiCafe_Text, stripWhitespace)
RawaiCafe_Text <- tm_map(RawaiCafe_Text, stem_snowball)
RawaiCafe_Text <- tm_map(RawaiCafe_Text, tolower)
RawaiCafe_Text <- tm_map(RawaiCafe_Text, removeWords, stopword)
RawaiCafe_Text <- tm_map(RawaiCafe_Text, removeWords, words)

source("Setiment.R")
sentiment(RawaiCafe_Text)

#Hotel11

Moo <- subset(Acc, Hotel.Restaurant.name == "Moo Restaurant")
Moo_Text <- Moo$Review
Moo_Text <- iconv(enc2utf8(Moo_Text),sub="byte")
Moo_Text <- gsub("[[:punct:]]|http\\S+\\s*|[[:digit:]]|^ | $", "", Moo_Text)
Moo_Text <- gsub("\n", " ",Moo_Text)
Moo_Text <- gsub('", "', " ",Moo_Text)

Moo_Text <- Corpus(VectorSource(Moo_Text))
Moo_Text <- tm_map(Moo_Text, stripWhitespace)
Moo_Text <- tm_map(Moo_Text, stem_snowball)
Moo_Text <- tm_map(Moo_Text, tolower)
Moo_Text <- tm_map(Moo_Text, removeWords, stopword)
Moo_Text <- tm_map(Moo_Text, removeWords, words)

source("Setiment.R")
sentiment(Moo_Text)

#Hotel12

Sala <- subset(Acc, Hotel.Restaurant.name == "Sala Mexicali")
Sala_Text <- Sala$Review
Sala_Text <- iconv(enc2utf8(Sala_Text),sub="byte")
Sala_Text <- gsub("[[:punct:]]|http\\S+\\s*|[[:digit:]]|^ | $", "", Sala_Text)
Sala_Text <- gsub("\n", " ",Sala_Text)
Sala_Text <- gsub('", "', " ",Sala_Text)

Sala_Text <- Corpus(VectorSource(Sala_Text))
Sala_Text <- tm_map(Sala_Text, stripWhitespace)
Sala_Text <- tm_map(Sala_Text, stem_snowball)
Sala_Text <- tm_map(Sala_Text, tolower)
Sala_Text <- tm_map(Sala_Text, removeWords, stopword)
Sala_Text <- tm_map(Sala_Text, removeWords, words)

source("Setiment.R")
sentiment(Sala_Text)

#Hotel13

Oceanfront <- subset(Acc, Hotel.Restaurant.name == "The Oceanfront Restaurant and Bar at Kata Rocks")
Oceanfront_Text <- Oceanfront$Review
Oceanfront_Text <- iconv(enc2utf8(Oceanfront_Text),sub="byte")
Oceanfront_Text <- gsub("[[:punct:]]|http\\S+\\s*|[[:digit:]]|^ | $", "", Oceanfront_Text)
Oceanfront_Text <- gsub("\n", " ",Oceanfront_Text)
Oceanfront_Text <- gsub('", "', " ",Oceanfront_Text)

Oceanfront_Text <- Corpus(VectorSource(Oceanfront_Text))
Oceanfront_Text <- tm_map(Oceanfront_Text, stripWhitespace)
Oceanfront_Text <- tm_map(Oceanfront_Text, stem_snowball)
Oceanfront_Text <- tm_map(Oceanfront_Text, tolower)
Oceanfront_Text <- tm_map(Oceanfront_Text, removeWords, stopword)
Oceanfront_Text <- tm_map(Oceanfront_Text, removeWords, words)

source("Setiment.R")
sentiment(Oceanfront_Text)

#Hotel14

Tagliere <- subset(Acc, Hotel.Restaurant.name == "Il Tagliere da Massimo")
Tagliere_Text <- Tagliere$Review
Tagliere_Text <- iconv(enc2utf8(Tagliere_Text),sub="byte")
Tagliere_Text <- gsub("[[:punct:]]|http\\S+\\s*|[[:digit:]]|^ | $", "", Tagliere_Text)
Tagliere_Text <- gsub("\n", " ",Tagliere_Text)
Tagliere_Text <- gsub('", "', " ",Tagliere_Text)

Tagliere_Text <- Corpus(VectorSource(Tagliere_Text))
Tagliere_Text <- tm_map(Tagliere_Text, stripWhitespace)
Tagliere_Text <- tm_map(Tagliere_Text, stem_snowball)
Tagliere_Text <- tm_map(Tagliere_Text, tolower)
Tagliere_Text <- tm_map(Tagliere_Text, removeWords, stopword)
Tagliere_Text <- tm_map(Tagliere_Text, removeWords, words)

source("Setiment.R")
sentiment(Tagliere_Text)

#Hotel15
treffpunkt <- subset(Acc, Hotel.Restaurant.name == "Restaurant Treffpunkt")
treffpunkt_Text <- treffpunkt$Review
treffpunkt_Text <- iconv(enc2utf8(treffpunkt_Text),sub="byte")
treffpunkt_Text <- gsub("[[:punct:]]|http\\S+\\s*|[[:digit:]]|^ | $", "", treffpunkt_Text)
treffpunkt_Text <- gsub("\n", " ",treffpunkt_Text)
treffpunkt_Text <- gsub('", "', " ",treffpunkt_Text)

treffpunkt_Text <- Corpus(VectorSource(treffpunkt_Text))
treffpunkt_Text <- tm_map(treffpunkt_Text, stripWhitespace)
treffpunkt_Text <- tm_map(treffpunkt_Text, stem_snowball)
treffpunkt_Text <- tm_map(treffpunkt_Text, tolower)
treffpunkt_Text <- tm_map(treffpunkt_Text, removeWords, stopword)
treffpunkt_Text <- tm_map(treffpunkt_Text, removeWords, words)

source("Setiment.R")
sentiment(treffpunkt_Text)

#Hotel16
surin <- subset(Acc, Hotel.Restaurant.name == "Surin Bay Inn Restaurant")
surin_Text <- surin$Review
surin_Text <- iconv(enc2utf8(surin_Text),sub="byte")
surin_Text <- gsub("[[:punct:]]|http\\S+\\s*|[[:digit:]]|^ | $", "", surin_Text)
surin_Text <- gsub("\n", " ",surin_Text)
surin_Text <- gsub('", "', " ",surin_Text)

surin_Text <- Corpus(VectorSource(surin_Text))
surin_Text <- tm_map(surin_Text, stripWhitespace)
surin_Text <- tm_map(surin_Text, stem_snowball)
surin_Text <- tm_map(surin_Text, tolower)
surin_Text <- tm_map(surin_Text, removeWords, stopword)
surin_Text <- tm_map(surin_Text, removeWords, words)

source("Setiment.R")
sentiment(surin_Text)

#Hotel17
DelishCafe <- subset(Acc, Hotel.Restaurant.name == "Delish Cafe")
DelishCafe_Text <- DelishCafe$Review
DelishCafe_Text <- iconv(enc2utf8(DelishCafe_Text),sub="byte")
DelishCafe_Text <- gsub("[[:punct:]]|http\\S+\\s*|[[:digit:]]|^ | $", "", DelishCafe_Text)
DelishCafe_Text <- gsub("\n", " ",DelishCafe_Text)
DelishCafe_Text <- gsub('", "', " ",DelishCafe_Text)

DelishCafe_Text <- Corpus(VectorSource(DelishCafe_Text))
DelishCafe_Text <- tm_map(DelishCafe_Text, stripWhitespace)
DelishCafe_Text <- tm_map(DelishCafe_Text, stem_snowball)
DelishCafe_Text <- tm_map(DelishCafe_Text, tolower)
DelishCafe_Text <- tm_map(DelishCafe_Text, removeWords, stopword)
DelishCafe_Text <- tm_map(DelishCafe_Text, removeWords, words)

source("Setiment.R")
sentiment(DelishCafe_Text)

#Hotel18
Mando <- subset(Acc, Hotel.Restaurant.name == "Mando Restaurant & Steakhouse")
Mando_Text <- Mando$Review
Mando_Text <- iconv(enc2utf8(Mando_Text),sub="byte")
Mando_Text <- gsub("[[:punct:]]|http\\S+\\s*|[[:digit:]]|^ | $", "", Mando_Text)
Mando_Text <- gsub("\n", " ",Mando_Text)
Mando_Text <- gsub('", "', " ",Mando_Text)

Mando_Text <- Corpus(VectorSource(Mando_Text))
Mando_Text <- tm_map(Mando_Text, stripWhitespace)
Mando_Text <- tm_map(Mando_Text, stem_snowball)
Mando_Text <- tm_map(Mando_Text, tolower)
Mando_Text <- tm_map(Mando_Text, removeWords, stopword)
Mando_Text <- tm_map(Mando_Text, removeWords, words)

source("Setiment.R")
sentiment(Mando_Text)

#Hotel19

Atsumi <- subset(Acc, Hotel.Restaurant.name == "Atsumi Raw Cafe")
Atsumi_Text <- Atsumi$Review
Atsumi_Text <- iconv(enc2utf8(Atsumi_Text),sub="byte")
Atsumi_Text <- gsub("[[:punct:]]|http\\S+\\s*|[[:digit:]]|^ | $", "", Atsumi_Text)
Atsumi_Text <- gsub("\n", " ",Atsumi_Text)
Atsumi_Text <- gsub('", "', " ",Atsumi_Text)

Atsumi_Text <- Corpus(VectorSource(Atsumi_Text))
Atsumi_Text <- tm_map(Atsumi_Text, stripWhitespace)
Atsumi_Text <- tm_map(Atsumi_Text, stem_snowball)
Atsumi_Text <- tm_map(Atsumi_Text, tolower)
Atsumi_Text <- tm_map(Atsumi_Text, removeWords, stopword)
Atsumi_Text <- tm_map(Atsumi_Text, removeWords, words)

source("Setiment.R")
sentiment(Atsumi_Text)

#Hotel20
Mookdee <- subset(Acc, Hotel.Restaurant.name == "Mookdee Seafood")
Mookdee_Text <- Mookdee$Review
Mookdee_Text <- iconv(enc2utf8(Mookdee_Text),sub="byte")
Mookdee_Text <- gsub("[[:punct:]]|http\\S+\\s*|[[:digit:]]|^ | $", "", Mookdee_Text)
Mookdee_Text <- gsub("\n", " ",Mookdee_Text)
Mookdee_Text <- gsub('", "', " ",Mookdee_Text)

Mookdee_Text <- Corpus(VectorSource(Mookdee_Text))
Mookdee_Text <- tm_map(Mookdee_Text, stripWhitespace)
Mookdee_Text <- tm_map(Mookdee_Text, stem_snowball)
Mookdee_Text <- tm_map(Mookdee_Text, tolower)
Mookdee_Text <- tm_map(Mookdee_Text, removeWords, stopword)
Mookdee_Text <- tm_map(Mookdee_Text, removeWords, words)

source("Setiment.R")
sentiment(Mookdee_Text)

#Hotel21

CoconutBar <- subset(Acc, Hotel.Restaurant.name == "COCONUT Bar & Restaurant - Rawai Beach")
CoconutBar_Text <- CoconutBar$Review
CoconutBar_Text <- iconv(enc2utf8(CoconutBar_Text),sub="byte")
CoconutBar_Text <- gsub("[[:punct:]]|http\\S+\\s*|[[:digit:]]|^ | $", "", CoconutBar_Text)
CoconutBar_Text <- gsub("\n", " ",CoconutBar_Text)
CoconutBar_Text <- gsub('", "', " ",CoconutBar_Text)

CoconutBar_Text <- Corpus(VectorSource(CoconutBar_Text))
CoconutBar_Text <- tm_map(CoconutBar_Text, stripWhitespace)
CoconutBar_Text <- tm_map(CoconutBar_Text, stem_snowball)
CoconutBar_Text <- tm_map(CoconutBar_Text, tolower)
CoconutBar_Text <- tm_map(CoconutBar_Text, removeWords, stopword)
CoconutBar_Text <- tm_map(CoconutBar_Text, removeWords, words)

source("Setiment.R")
sentiment(CoconutBar_Text)

#Hotel22

Mamasita <- subset(Acc, Hotel.Restaurant.name == "Mamasita Mexican")
Mamasita_Text <- Mamasita$Review
Mamasita_Text <- iconv(enc2utf8(Mamasita_Text),sub="byte")
Mamasita_Text <- gsub("[[:punct:]]|http\\S+\\s*|[[:digit:]]|^ | $", "", Mamasita_Text)
Mamasita_Text <- gsub("\n", " ",Mamasita_Text)
Mamasita_Text <- gsub('", "', " ",Mamasita_Text)

Mamasita_Text <- Corpus(VectorSource(Mamasita_Text))
Mamasita_Text <- tm_map(Mamasita_Text, stripWhitespace)
Mamasita_Text <- tm_map(Mamasita_Text, stem_snowball)
Mamasita_Text <- tm_map(Mamasita_Text, tolower)
Mamasita_Text <- tm_map(Mamasita_Text, removeWords, stopword)
Mamasita_Text <- tm_map(Mamasita_Text, removeWords, words)

source("Setiment.R")
sentiment(Mamasita_Text)

#Hotel23
Nikitas <- subset(Acc, Hotel.Restaurant.name == "Nikitas Beach Restaurant")
Nikitas_Text <- Nikitas$Review
Nikitas_Text <- iconv(enc2utf8(Nikitas_Text),sub="byte")
Nikitas_Text <- gsub("[[:punct:]]|http\\S+\\s*|[[:digit:]]|^ | $", "", Nikitas_Text)
Nikitas_Text <- gsub("\n", " ",Nikitas_Text)
Nikitas_Text <- gsub('", "', " ",Nikitas_Text)

Nikitas_Text <- Corpus(VectorSource(Nikitas_Text))
Nikitas_Text <- tm_map(Nikitas_Text, stripWhitespace)
Nikitas_Text <- tm_map(Nikitas_Text, stem_snowball)
Nikitas_Text <- tm_map(Nikitas_Text, tolower)
Nikitas_Text <- tm_map(Nikitas_Text, removeWords, stopword)
Nikitas_Text <- tm_map(Nikitas_Text, removeWords, words)

source("Setiment.R")
sentiment(Nikitas_Text)

#Hotel24
Hooters <- subset(Acc, Hotel.Restaurant.name == "Hooters Phuket")
Hooters_Text <- Hooters$Review
Hooters_Text <- iconv(enc2utf8(Hooters_Text),sub="byte")
Hooters_Text <- gsub("[[:punct:]]|http\\S+\\s*|[[:digit:]]|^ | $", "", Hooters_Text)
Hooters_Text <- gsub("\n", " ",Hooters_Text)
Hooters_Text <- gsub('", "', " ",Hooters_Text)

Hooters_Text <- Corpus(VectorSource(Hooters_Text))
Hooters_Text <- tm_map(Hooters_Text, stripWhitespace)
Hooters_Text <- tm_map(Hooters_Text, stem_snowball)
Hooters_Text <- tm_map(Hooters_Text, tolower)
Hooters_Text <- tm_map(Hooters_Text, removeWords, stopword)
Hooters_Text <- tm_map(Hooters_Text, removeWords, words)

source("Setiment.R")
sentiment(Hooters_Text)

#Hotel25
RedChopsticks <- subset(Acc, Hotel.Restaurant.name == "Red Chopsticks At Patong")
RedChopsticks_Text <- RedChopsticks$Review
RedChopsticks_Text <- iconv(enc2utf8(RedChopsticks_Text),sub="byte")
RedChopsticks_Text <- gsub("[[:punct:]]|http\\S+\\s*|[[:digit:]]|^ | $", "", RedChopsticks_Text)
RedChopsticks_Text <- gsub("\n", " ",RedChopsticks_Text)
RedChopsticks_Text <- gsub('", "', " ",RedChopsticks_Text)

RedChopsticks_Text <- Corpus(VectorSource(RedChopsticks_Text))
RedChopsticks_Text <- tm_map(RedChopsticks_Text, stripWhitespace)
RedChopsticks_Text <- tm_map(RedChopsticks_Text, stem_snowball)
RedChopsticks_Text <- tm_map(RedChopsticks_Text, tolower)
RedChopsticks_Text <- tm_map(RedChopsticks_Text, removeWords, stopword)
RedChopsticks_Text <- tm_map(RedChopsticks_Text, removeWords, words)

source("Setiment.R")
sentiment(RedChopsticks_Text)

#Hotel26
Lucky13 <- subset(Acc, Hotel.Restaurant.name == "Lucky 13 Sandwich Rawai")
Lucky13_Text <- Lucky13$Review
Lucky13_Text <- iconv(enc2utf8(Lucky13_Text),sub="byte")
Lucky13_Text <- gsub("[[:punct:]]|http\\S+\\s*|[[:digit:]]|^ | $", "", Lucky13_Text)
Lucky13_Text <- gsub("\n", " ",Lucky13_Text)
Lucky13_Text <- gsub('", "', " ",Lucky13_Text)

Lucky13_Text <- Corpus(VectorSource(Lucky13_Text))
Lucky13_Text <- tm_map(Lucky13_Text, stripWhitespace)
Lucky13_Text <- tm_map(Lucky13_Text, stem_snowball)
Lucky13_Text <- tm_map(Lucky13_Text, tolower)
Lucky13_Text <- tm_map(Lucky13_Text, removeWords, stopword)
Lucky13_Text <- tm_map(Lucky13_Text, removeWords, words)

source("Setiment.R")
sentiment(Lucky13_Text)

#Hotel27
Norbu <- subset(Acc, Hotel.Restaurant.name == "Norbu's Steakhouse")
Norbu_Text <- Norbu$Review
Norbu_Text <- iconv(enc2utf8(Norbu_Text),sub="byte")
Norbu_Text <- gsub("[[:punct:]]|http\\S+\\s*|[[:digit:]]|^ | $", "", Norbu_Text)
Norbu_Text <- gsub("\n", " ",Norbu_Text)
Norbu_Text <- gsub('", "', " ",Norbu_Text)

Norbu_Text <- Corpus(VectorSource(Norbu_Text))
Norbu_Text <- tm_map(Norbu_Text, stripWhitespace)
Norbu_Text <- tm_map(Norbu_Text, stem_snowball)
Norbu_Text <- tm_map(Norbu_Text, tolower)
Norbu_Text <- tm_map(Norbu_Text, removeWords, stopword)
Norbu_Text <- tm_map(Norbu_Text, removeWords, words)

source("Setiment.R")
sentiment(Norbu_Text)

#Hotel28
CafeJava <- subset(Acc, Hotel.Restaurant.name == "Cafe Java")
CafeJava_Text <- CafeJava$Review
CafeJava_Text <- iconv(enc2utf8(CafeJava_Text),sub="byte")
CafeJava_Text <- gsub("[[:punct:]]|http\\S+\\s*|[[:digit:]]|^ | $", "", CafeJava_Text)
CafeJava_Text <- gsub("\n", " ",CafeJava_Text)
CafeJava_Text <- gsub('", "', " ",CafeJava_Text)

CafeJava_Text <- Corpus(VectorSource(CafeJava_Text))
CafeJava_Text <- tm_map(CafeJava_Text, stripWhitespace)
CafeJava_Text <- tm_map(CafeJava_Text, stem_snowball)
CafeJava_Text <- tm_map(CafeJava_Text, tolower)
CafeJava_Text <- tm_map(CafeJava_Text, removeWords, stopword)
CafeJava_Text <- tm_map(CafeJava_Text, removeWords, words)

source("Setiment.R")
sentiment(CafeJava_Text)

#Hotel29
BonIsland <- subset(Acc, Hotel.Restaurant.name == "Bon Island Restaurant")
BonIsland_Text <- BonIsland$Review
BonIsland_Text <- iconv(enc2utf8(BonIsland_Text),sub="byte")
BonIsland_Text <- gsub("[[:punct:]]|http\\S+\\s*|[[:digit:]]|^ | $", "", BonIsland_Text)
BonIsland_Text <- gsub("\n", " ",BonIsland_Text)
BonIsland_Text <- gsub('", "', " ",BonIsland_Text)

BonIsland_Text <- Corpus(VectorSource(BonIsland_Text))
BonIsland_Text <- tm_map(BonIsland_Text, stripWhitespace)
BonIsland_Text <- tm_map(BonIsland_Text, stem_snowball)
BonIsland_Text <- tm_map(BonIsland_Text, tolower)
BonIsland_Text <- tm_map(BonIsland_Text, removeWords, stopword)
BonIsland_Text <- tm_map(BonIsland_Text, removeWords, words)

source("Setiment.R")
sentiment(BonIsland_Text)

#Hotel30
KhunPha <- subset(Acc, Hotel.Restaurant.name == "Khun Pha")
KhunPha_Text <- KhunPha$Review
KhunPha_Text <- iconv(enc2utf8(KhunPha_Text),sub="byte")
KhunPha_Text <- gsub("[[:punct:]]|http\\S+\\s*|[[:digit:]]|^ | $", "", KhunPha_Text)
KhunPha_Text <- gsub("\n", " ",KhunPha_Text)
KhunPha_Text <- gsub('", "', " ",KhunPha_Text)

KhunPha_Text <- Corpus(VectorSource(KhunPha_Text))
KhunPha_Text <- tm_map(KhunPha_Text, stripWhitespace)
KhunPha_Text <- tm_map(KhunPha_Text, stem_snowball)
KhunPha_Text <- tm_map(KhunPha_Text, tolower)
KhunPha_Text <- tm_map(KhunPha_Text, removeWords, stopword)
KhunPha_Text <- tm_map(KhunPha_Text, removeWords, words)

source("Setiment.R")
sentiment(KhunPha_Text)
