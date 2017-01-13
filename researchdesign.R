
summary(as.vector(as.matrix(keyword)))

keyword_binary <- as.data.frame(ifelse(keyword < 0.320,0,1))

##ERGM
install.packages("RCurl")
install.packages("ergm")

library(RCurl)
library(ergm)

net_cosponsorship <- network(cosponsorship, vertex.attr = attributes, vertex.attrnames = colnames(attributes), directed = F, hyper = F, loops = F, multiple = F, bipartite = F)
net_committee <- network(committee, vertex.attr = attributes, vertex.attrnames = colnames(attributes), directed = F, hyper = F, loops = F, multiple = F, bipartite = F)
net_caucus <- network(caucus, vertex.attr = attributes, vertex.attrnames = colnames(attributes), directed = F, hyper = F, loops = F, multiple = F, bipartite = F)

net_keyword <- network(keyword_binary, vertex.attr = attributes, vertex.attrnames = colnames(attributes), directed = F, hyper = F, loops = F, multiple = F, bipartite = F)
ergm_keyword <- ergm(net_keyword~edges + nodematch("party") + nodematch("state") + absdiff("dwnom1") + nodefactor("party") + nodefactor("state") + nodecov("dwnom1") + edgecov(net_caucus) +edgecov(net_committee) + edgecov(net_cosponsorship), control=control.ergm(MCMC.burnin = 50000, MCMC.interval = 5000))
summary(ergm_keyword)


#covoting
library(igraph)
covoting_ig = graph.adjacency(as.matrix(covoting_111_threshood), mode="undirected", weighted = NULL)
testatt = MC_111

V(covoting_ig)$name
V(covoting_ig)$state=testatt$state[match(V(covoting_ig)$name,testatt$ID)]
V(covoting_ig)$party=testatt$party[match(V(covoting_ig)$name,testatt$ID)]
V(covoting_ig)$dwnom1=testatt$dwnom1[match(V(covoting_ig)$name,testatt$ID)]

degree <- degree(covoting_ig, loops=T, normalized=F)
btwn <- betweenness(covoting_ig,  directed = F)
close <- closeness(covoting_ig, mode = c("all"))
eigen <- evcent(covoting_ig)
bon <- bonpow(covoting_ig)
dd <- data.frame(name = V(covoting_ig)$name)
cb1 <- cbind(dd, degree, close, btwn, eigen, bon, V(covoting_ig)$party, V(covoting_ig)$state, V(covoting_ig)$dwnom1)
cb2 <- cbind(degree, close, btwn, eigen$vector,bon)
cor(cb2)
cb1 <- cb1[,-c(6:26)]
summary(cb1)


set.seed(12)
ig1 <- layout.kamada.kawai(covoting_ig)
par(mfrow=c(1,1))
oldMargins<-par("mar")
par(mar=c(1,1,1,1))
V(covoting_ig)[V(covoting_ig)$party== 100]$color <- "pink"
V(covoting_ig)[V(covoting_ig)$party== 200]$color <- "dodgerblue"
V(covoting_ig)$size <- sqrt(degree(covoting_ig))

plot(covoting_ig, layout = ig1)

#keywords
library(igraph)
keywords_ig = graph.adjacency(as.matrix(keywords_111_threshood), mode="undirected", weighted = NULL)
testatt = MC_111

V(keywords_ig)$name
V(keywords_ig)$state=testatt$state[match(V(keywords_ig)$name,testatt$ID)]
V(keywords_ig)$party=testatt$party[match(V(keywords_ig)$name,testatt$ID)]
V(keywords_ig)$dwnom1=testatt$dwnom1[match(V(keywords_ig)$name,testatt$ID)]


set.seed(12)
ig2 <- layout.kamada.kawai(keywords_ig)
par(mfrow=c(1,1))
oldMargins<-par("mar")
par(mar=c(1,1,1,1))
V(keywords_ig)[V(keywords_ig)$party== 100]$color <- "pink"
V(keywords_ig)[V(keywords_ig)$party== 200]$color <- "dodgerblue"
V(keywords_ig)$size <- sqrt(degree(keywords_ig))

plot(keywords_ig, layout = ig2)

##descriptive statistics of keywords
filter_keywords <- keywordslist.2[which(!(keywordslist.2$V1 %in% stopwords(kind = "en"))),]




