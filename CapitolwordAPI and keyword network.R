options(SunlightLabsKey = "7444c86f280d405f855df9a0072b7489")
install.packages("rsunlight")
library("rsunlight")

#CapitolwordAPI
all_words <- c()
for (h in 1:nrow(legislator_names)) {
  print(h)
  #h=1
  for (i in 0:1) {
    i=0
    temp <- cw_phrases(entity_type='legislator', entity_value='H000981', per_page = 50, page = i, sort = 'count desc')
    all_words <- c(all_words,temp[,3])
  }
}
temp2 <- cw_dates(phrase = 'have', congress = '111', bioguide_id = 'H000981', granularity = 'year')
length(unique(all_words))

result <- as.data.frame(matrix(nrow = nrow(legislator_names), ncol = length(unique(all_words))))
colnames(result) <- unique(all_words)

colnames(keywords_111) <- keywords_111[1,]
rownames(keywords_111) <- keywords_111[,1]
keywords_111 <- keywords_111[-1,-1]

colnames(MC_111)[4] <- "party"
MC_111 <- MC_111[,-3]

#count
for (h in 1:nrow(legislator_names)) {
  print(h)
  #h=1
  legislator_words <- c()
  for (i in 0:1) {
    temp <- cw_phrases(entity_type='legislator', entity_value=as.character(legislator_names[h,1]), per_page = 50, page = i, sort = 'count desc')
    legislator_words <- c(legislator_words,temp[,3])
  }
  for (word in 1:length(legislator_words)) {
    #word=1
    temp2 <- cw_dates(phrase = legislator_words[word], congress = '111', bioguide_id = as.character(legislator_names[h,1]), granularity = 'year')
    b_count <- colSums(temp2[,-4])[1]
    result[h,legislator_words[word]] <- b_count
  }
}

rownames(result) <- legislator_names[,1]

#percentage 
for (h in 1:nrow(legislator_names)) {
  +   print(h)
  +   #h=1
    +   legislator_words <- c()
    +   for (i in 0:1) {
      +     temp <- cw_phrases(entity_type='legislator', entity_value=as.character(legislator_names[h,1]), per_page = 50, page = i, sort = 'count desc')
      +     legislator_words <- c(legislator_words,temp[,3])
      +   }
    +   for (word in 1:length(legislator_words)) {
      +     #word=1
        +     temp3 <- cw_dates(phrase = legislator_words[word], congress = '111', bioguide_id = as.character(legislator_names[h,1]), granularity = 'year')
        +     b_means <- colMeans(temp3[,-4])[2]
        +     result_1[h,legislator_words[word]] <- b_means
        +   }
    + }

legislator_names <- as.data.frame(legislator_names)
final <- as.data.frame(matrix(nrow = nrow(legislator_names), ncol = nrow(legislator_names)))
colnames(final) <- legislator_names[,1]
rownames(final) <- legislator_names[,1]

install.packages('gtools')
library(gtools)

loop_set <- combinations(n=433,r=2,v=legislator_names[,1])

#tfidf
for (i in 1:nrow(loop_set)) {
  print(i)
  legis_x <- loop_set[i,1]
  legis_y <- loop_set[i,2]
  legis_x_words <- colnames(counts)[which(counts[legis_x,]>0)]
  legis_y_words <- colnames(counts)[which(counts[legis_y,]>0)]
  same_words <- intersect(legis_x_words,legis_y_words)
  
  temp=0
  if (length(same_words)!=0) {
    for (word in 1:length(same_words)) {
      #word=1
      temp <- temp + min(counts[legis_x,same_words[word]] * weights[legis_x,same_words[word]], counts[legis_y,same_words[word]] * weights[legis_y,same_words[word]])
    }
    final[legis_x,legis_y] <- temp
  }
}


#NA
for (i in 1:nrow(legislator_names)) {
  for (j in 1:nrow(legislator_names)) {
    ifelse(is.na(final1[as.character(legislator_names[i,1]), as.character(legislator_names[j,1])]), final1[as.character(legislator_names[i,1]), as.character(legislator_names[j,1])] <- final1[as.character(legislator_names[j,1]), as.character(legislator_names[i,1])], NA)
  }
  
}

final2 <- scale(final1)

##cosponsorship
install.packages('gtools')
library(gtools)

final <- as.data.frame(matrix(nrow = 433, ncol = 433))
colnames(final) <- legislatorid[,1]
rownames(final) <- legislatorid[,1]

for (i in 1:ncol(cosponsorship)) {
  print(i)
  temp <- c()
  for (j in 1:433) {
    if(cosponsorship[j,i] == 2) {
      temp <- c(temp, rownames(cosponsorship[j,])) 
    } 
  }
  temp1 <- combinations(n = length(temp), r=2, v=temp)
  for (h in 1: nrow(temp1)) {
    final [temp1[h,1], temp1[h,2]] <- 1
  }
}

for (i in 1:nrow(legislatorid)) {
  for (j in 1:nrow(legislatorid)) {
    ifelse(is.na(final[as.character(legislatorid[i,1]), as.character(legislatorid[j,1])]), final[as.character(legislatorid[i,1]), as.character(legislatorid[j,1])] <- final[as.character(legislatorid[j,1]), as.character(legislatorid[i,1])], NA)
  }
  
}

for (i in 1:433) {
  cosponsorship[i,i] <- 1
  
}

write.csv(cosponsorship, file = "cosponsorship.csv")

##caucus
for (i in 1:ncol(caucus)) {
  print(i)
  temp <- c()
  for (j in 1:433) {
    if(caucus[j,i] == 1) {
      temp <- c(temp, rownames(caucus[j,])) 
    } 
  }
  temp1 <- combinations(n = length(temp), r=2, v=temp)
  for (h in 1: nrow(temp1)) {
    final [temp1[h,1], temp1[h,2]] <- 1
  }
}

for (i in 1:nrow(legislatorid)) {
  for (j in 1:nrow(legislatorid)) {
    ifelse(is.na(final[as.character(legislatorid[i,1]), as.character(legislatorid[j,1])]), final[as.character(legislatorid[i,1]), as.character(legislatorid[j,1])] <- final[as.character(legislatorid[j,1]), as.character(legislatorid[i,1])], NA)
  }
  
}

for (i in 1:426) {
  caucus[i,i] <- 1
  
}

write.csv(caucus, file = "caucus.csv")

##committee
install.packages('gtools')
library(gtools)

final <- as.data.frame(matrix(nrow = 449, ncol = 449))
colnames(final) <- legislatorid[,1]
rownames(final) <- legislatorid[,1]

for (i in c("102", "104", "106", "113", "115", "124", "128", "134", "138", "142", "156", "164", "173", "176", "182", "184", "186", "192", "196", "242", "251", "252", "500", "501", "503", "507", "661")) {
  print(i)
  temp <- as.character(committee_111[committee_111$`committee code`== i,2])
  temp1 <- combinations(n = length(temp), r=2, v=temp)
  for (h in 1: nrow(temp1)) {
    final [temp1[h,1], temp1[h,2]] <- 1
  }
}

for (i in 1:nrow(legislatorid)) {
  for (j in 1:nrow(legislatorid)) {
    ifelse(is.na(final[as.character(legislatorid[i,1]), as.character(legislatorid[j,1])]), final[as.character(legislatorid[i,1]), as.character(legislatorid[j,1])] <- final[as.character(legislatorid[j,1]), as.character(legislatorid[i,1])], NA)
  }
  
}

for (i in 1:449) {
  committee[i,i] <- 1
  
}

write.csv(committee, file = "committee.csv")


#na -> 0
result[is.na(result)] <- 0
result_1[is.na(result_1)] <- 0






