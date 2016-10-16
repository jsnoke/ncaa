library(igraph)
library(RColorBrewer)

ncaaFootball = read.csv("~/Documents/ncaa_meanderings/ncaaFootballAll.csv")
teamInd = read.csv("~/Documents/ncaa_meanderings/teamIndAllv2.csv")

ncaaFootball$scoreDiff = ncaaFootball$score1 - ncaaFootball$score2
networkDF = ncaaFootball[, c(3, 6, 4, 9)]

ncaaNet = graph_from_data_frame(networkDF, directed = T, vertices = teamInd)
summary(ncaaNet)
V(ncaaNet)$name = V(ncaaNet)$team

#V(ncaaNet)$color = as.numeric(teamInd$conference)

V(ncaaNet)$color = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33", "#a65628")[as.numeric(teamInd$conference)]
#E(ncaaNet)$scoreDiff
#V(ncaaNet)
#ncaaNet$scoreDiff

## influence measures
alphaInfluence = alpha_centrality(ncaaNet, weights = V(ncaaNet)$scoreDiff, loops = F)
#sort(alphaInfluence)

pageRank = page_rank(ncaaNet, weights = V(ncaaNet)$scoreDiff)
#sort(pageRank$vector)

#cor(alphaInfluence, pageRank$vector)

#betweenRank = betweenness(ncaaNet, weights = V(ncaaNet)$scoreDiff)
#sort(betweenRank)

#cor(alphaInfluence, betweenRank)
#cor(pageRank$vector, betweenRank)

powerRank = power_centrality(ncaaNet)
#sort(powerRank, decreasing = T)
#cor(powerRank, pageRank$vector)
#cor(powerRank, alphaInfluence)
#cor(powerRank, betweenRank)

closenessRankOut = closeness(ncaaNet, mode = "out", weights = V(ncaaNet)$scoreDiff)
#sort(closenessRankOut, decreasing = T)
closenessRankIn = closeness(ncaaNet, mode = "in", weights = V(ncaaNet)$scoreDiff)
#sort(closenessRankIn)


## make df of all
rankDF = cbind(alphaInfluence, pageRank$vector, 
                          powerRank, closenessRankOut, closenessRankIn)
rankDF = data.frame(scale(rankDF))

## reduce to two dimensions
library(FactoMineR)

redInfluence = PCA(rankDF, ncp = 2)
summary(redInfluence$ind$coord)
summary(redInfluence)

redInfluence2 = PCA(rankDF[, c(2,4:5)], ncp = 2)
summary(redInfluence2$ind$coord)
summary(redInfluence2)

##
rankDF$overallRank = (rankDF$closenessRankOut - rankDF$alphaInfluence) * 5 + 25
View(rankDF)

rankDF$overallRankDR = (redInfluence$ind$coord[,2] - redInfluence$ind$coord[,1]) * 5 + 25
View(rankDF)

rankDF$overallRankDR2 = (-redInfluence2$ind$coord[,1]) * 5 + 30
View(rankDF)

#overallRank = data.frame(cbind(c(1:128), row.names(rankDF[order(rankDF$overallRank, decreasing = T),]), 
#                               sort(rankDF$overallRank, decreasing = T)))
overallRank = data.frame(cbind(c(1:128), row.names(rankDF[order(rankDF$overallRankDR2, decreasing = T),]), 
                                sort(rankDF$overallRankDR2, decreasing = T), 
                               V(ncaaNet)$conference[order(rankDF$overallRankDR2, decreasing = T)]))
colnames(overallRank) = c("Rank", "Team", "Coefficient", "Conference")
write.csv(overallRank, file = "overallRank_10.16.16.csv", quote = F, row.names = F)

## plot
plotLayout = layout_with_fr(ncaaNet, dim = 2, weights = E(ncaaNet)$scoreDiff, start.temp = vcount(ncaaNet)^(1/2))
#plotLayout = layout_nicely(ncaaNet, dim = 2, weights = E(ncaaNet)$scoreDiff)

plot(ncaaNet, layout = plotLayout, edge.arrow.size = 0.025, rescale = F,
     xlim = range(plotLayout[, 1]), ylim = range(plotLayout[, 2]), vertex.label.dist = 1, 
     vertex.label.color = "blue", vertex.label.cex = 0.5, 
     vertex.size = ( rankDF$overallRankDR2 ))










