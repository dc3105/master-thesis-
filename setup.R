
install.packages("readr")
library(readr)

keyword <- as.data.frame(read_csv("~/Downloads/tfidf.csv"))
rownames(keyword) <- keyword[,1]
keyword <- keyword[,-1]
keyword <- scale(keyword)

covoting <- as.data.frame(read_csv("~/Downloads/covoting.csv"))
rownames(covoting) <- covoting[,1]
covoting <- covoting[,-1]

attributes <- read_csv("~/Downloads/attributes.csv")

cosponsorship <- as.data.frame(read_csv("~/Downloads/cosponsorship.csv"))
rownames(cosponsorship) <- cosponsorship[,1]
cosponsorship <- cosponsorship[,-1]

caucus <- as.data.frame(read_csv("~/Downloads/caucus.csv"))
rownames(caucus) <- caucus[,1]
caucus <- caucus[,-1]

committee <- as.data.frame(read_csv("~/Downloads/committee.csv"))
rownames(committee) <- committee[,1]
committee <- committee[,-1]

legislatorid <- as.data.frame(read_csv("~/Downloads/legislatorid.csv"))

