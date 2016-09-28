library(igraph)
library(RColorBrewer)

ncaaFootball = read.csv("~/Documents/ncaa_meanderings/ncaaFootballAll.csv")
teamInd = read.csv("~/Documents/ncaa_meanderings/teamIndAll.csv")

ncaaFootball$scoreDiff = ncaaFootball$score1 - ncaaFootball$score2
networkDF = ncaaFootball[, c(3, 6, 4, 9)]

ncaaNet = graph_from_data_frame(networkDF, directed = T, vertices = teamInd)
summary(ncaaNet)
V(ncaaNet)$name = V(ncaaNet)$team

#V(ncaaNet)$color = as.numeric(teamInd$conference)

#generate a cool palette for the graph (darker colors = older nodes)
YlOrBr.pal = colorRampPalette(brewer.pal(10,"YlOrRd"))
#colors for the nodes are chosen from the very beginning
V(ncaaNet)$color = rev(YlOrBr.pal(vcount(ncaaNet)))[as.numeric(teamInd$conference)]
V(ncaaNet)$color = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", 
                     "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a", "#ffff99")[as.numeric(teamInd$conference)]
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

redInfluence = PCA(rankDF)
summary(redInfluence$ind$coord)

##
rankDF$overallRank = (rankDF$closenessRankOut - rankDF$alphaInfluence) * 5 + 25
View(rankDF)

rankDF$overallRankDR = (redInfluence$ind$coord[,2] - redInfluence$ind$coord[,1]) * 5 + 20
View(rankDF)

#overallRank = data.frame(cbind(c(1:128), row.names(rankDF[order(rankDF$overallRank, decreasing = T),]), 
#                               sort(rankDF$overallRank, decreasing = T)))
overallRank = data.frame(cbind(c(1:128), row.names(rankDF[order(rankDF$overallRankDR, decreasing = T),]), 
                                sort(rankDF$overallRankDR, decreasing = T)))
colnames(overallRank) = c("Rank", "Team", "Coefficient")
write.csv(overallRank, file = "overallRank_09.28.16.csv", quote = F, row.names = F)

## plot
plotLayout = layout_with_fr(ncaaNet, dim = 2, weights = E(ncaaNet)$scoreDiff)
plotLayout = layout_nicely(ncaaNet, dim = 2, weights = E(ncaaNet)$scoreDiff)

plot(ncaaNet, layout = plotLayout, edge.arrow.size = 0.025, rescale = F,
     xlim = range(plotLayout[, 1]), ylim = range(plotLayout[, 2]), vertex.label.dist = 1, 
     vertex.label.color = "blue", vertex.label.cex = 0.5, 
     vertex.size = ( rankDF$overallRankDR ))










