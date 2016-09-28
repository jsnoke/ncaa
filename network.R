library(igraph)

ncaaFootball = read.csv("~/Documents/ncaa_meanderings/ncaaFootball.csv")
teamInd = read.csv("~/Documents/ncaa_meanderings/teamInd.csv")

ncaaFootball$scoreDiff = ncaaFootball$score1 - ncaaFootball$score2
networkDF = ncaaFootball[, c(3, 6, 4, 9)]

ncaaNet = graph_from_data_frame(networkDF, directed = T, vertices = teamInd)
summary(ncaaNet)
V(ncaaNet)$name = V(ncaaNet)$team
V(ncaaNet)$color = as.numeric(teamInd$conference)
#E(ncaaNet)$scoreDiff
#V(ncaaNet)
#ncaaNet$scoreDiff

## influence measures
alphaInfluence = alpha_centrality(ncaaNet, weights = V(ncaaNet)$scoreDiff, loops = F)
sort(alphaInfluence)

pageRank = page_rank(ncaaNet, weights = V(ncaaNet)$scoreDiff)
sort(pageRank$vector)

cor(alphaInfluence, pageRank$vector)

betweenRank = betweenness(ncaaNet, weights = V(ncaaNet)$scoreDiff)
sort(betweenRank)

cor(alphaInfluence, betweenRank)
cor(pageRank$vector, betweenRank)

powerRank = power_centrality(ncaaNet)
sort(powerRank, decreasing = T)
cor(powerRank, pageRank$vector)
cor(powerRank, alphaInfluence)
cor(powerRank, betweenRank)

closenessRankOut = closeness(ncaaNet, mode = "out", weights = V(ncaaNet)$scoreDiff)
sort(closenessRankOut, decreasing = T)
closenessRankIn = closeness(ncaaNet, mode = "in", weights = V(ncaaNet)$scoreDiff)
sort(closenessRankIn)

## make df of all
rankDF = cbind(alphaInfluence, pageRank$vector, betweenRank, 
                          powerRank, closenessRankOut, closenessRankIn)
rankDF = data.frame(scale(rankDF))

rankDF$overallRank = (rankDF$powerRank - rankDF$alphaInfluence) * 5 + 25
View(rankDF)

## plot
plotLayout = layout_with_fr(ncaaNet, dim = 2, weights = E(ncaaNet)$scoreDiff)

plot(ncaaNet, layout = plotLayout, edge.arrow.size = 0.025, rescale = F,
     xlim = range(plotLayout[, 1]), ylim = range(plotLayout[, 2]), vertex.label.dist = 1, 
     vertex.label.color = "blue", vertex.label.cex = 0.75, 
     vertex.size = ( rankDF$overallRank ))










