ncaaFootball = read.csv("~/Documents/ncaa_meanderings/ncaaFootballAll.csv")
teamInd = read.csv("~/Documents/ncaa_meanderings/teamIndAllv2.csv")

ncaaFootball$scoreDiff = ncaaFootball$score1 - ncaaFootball$score2

ncaaFootball$week = NA
ncaaFootball$week[1:41] = 1
ncaaFootball$week[42:87] = 2
ncaaFootball$week[88:142] = 3
ncaaFootball$week[143:194] = 4
ncaaFootball$week[195:253] = 5
ncaaFootball$week[254:307] = 6
ncaaFootball$week[308:360] = 7
ncaaFootball$week[361:414] = 8

#####
## rpi optimization
#####
library(optimx)
getWinPer = function(team, dataf){
    winPer = nrow(subset(dataf, team1.index == team)) / 
        nrow(subset(dataf, team1.index == team | team2.index == team))
    
    return(winPer)
}

getVariables = function(team, dataf, winPer){
    
    subDF = subset(dataf, team1.index == team | team2.index == team)
    opp = c(setdiff(subDF$team1.index, team), setdiff(subDF$team2.index, team))
    
    oppWin = mean(winPer[is.element(teamInd$index, opp)])
    
    oppOpp = NULL
    for(a in 1:length(opp)){
        oppSubDF = subset(dataf, team1.index == opp[a] | team2.index == opp[a])
        oppOpp = c(oppOpp, c(setdiff(subDF$team1.index, opp[a]), setdiff(subDF$team2.index, opp[a])))
    }
    oppOpp = unique(oppOpp)
    
    oppOppWin = mean(winPer[is.element(teamInd$index, oppOpp)])
    
    scoreDiff = sum(subDF$scoreDiff[subDF$team1.index == team]) - sum(subDF$scoreDiff[subDF$team2.index == team])
    
    weightVar = c(oppWin, oppOppWin, scoreDiff)
    return(weightVar)
}


internalOptimalWeights = function(weightz, varz, dataf){
    rpi = rep(NA, nrow(varz))
    temp = scale(varz)
    for(a in 1:nrow(varz)){
        rpi[a] = sum(weightz * temp[a, ])
    }
    
    predWin = rep(NA, nrow(dataf))
    for(a in 1:nrow(dataf)){
        predWin[a] =  sum(which.max(c(rpi[dataf[a, 3]], rpi[dataf[a, 6]])) == 1)
    }
    
    return(mean(predWin))
}

optimalWeights = function(weightz, traindataf, testdataf){
    teamWinPer = sapply(c(1:128), getWinPer, traindataf)
    teamVar = cbind.data.frame(teamWinPer, t(sapply(c(1:128), getVariables, traindataf, teamWinPer)))
    
    foo = optimx(weightz, internalOptimalWeights, varz = teamVar[, 1:3], dataf = testdataf, 
                 control = list(maximize = TRUE))
    
    return(foo)
}


optimalWeights(maxRpiWeights[8,], ncaaFootball[ncaaFootball$week < 8, ], ncaaFootball[ncaaFootball$week == 8, ])

maxWeights = vector("list", 8)
for(a in 2:8){
    if(a == 2){
        maxWeights[[a]] = optimalWeights(rep(0.3333, 3), ncaaFootball[ncaaFootball$week < a, ],
                                         ncaaFootball[ncaaFootball$week == a, ])
    } else{
        maxWeights[[a]] = optimalWeights(unlist(maxWeights[[(a - 1)]][1, 1:3]), ncaaFootball[ncaaFootball$week < a, ],
                                         ncaaFootball[ncaaFootball$week == a, ])
    }
    
}
maxRpiWeights = matrix(NA, nrow = nlevels(as.factor(ncaaFootball$week)), ncol = 3)
for(a in 2:nrow(maxRpiWeights)){
    maxRpiWeights[a, ] = unlist(maxWeights[[a]][1, 1:3])
}



##### 
##### pairwise
##### 
pairWise = function(team, rpiInd){
    wins = 0
    for(a in 1:nrow(teamInd)){
        rpiWin = sum(teamInd[teamInd$index == team, rpiInd] > teamInd[teamInd$index == a, rpiInd])
        
        vsWin = nrow(subset(ncaaFootball, team1.index == team & team2.index == a))
        
        vsLoss = nrow(subset(ncaaFootball, team1.index == a & team2.index == team))
        
        ## common opp
        subDF = subset(ncaaFootball, team1.index == team | team2.index == team)
        subOppDF = subset(ncaaFootball, team1.index == a | team2.index == a)
        commOpp = intersect(c(setdiff(subDF$team1.index, team), setdiff(subDF$team2.index, team)), 
                            c(setdiff(subOppDF$team1.index, a), setdiff(subOppDF$team2.index, a)))
        
        if(length(commOpp) == 0){
            selfWP = 0
            oppWP = 0
        } else{
            selfWP = nrow(subset(subDF, team1.index == team & is.element(team2.index, commOpp))) / 
                nrow(subset(subDF, (team1.index == team | team2.index == team) &
                                (is.element(team1.index, commOpp) | is.element(team2.index, commOpp))))
            
            oppWP = nrow(subset(subOppDF, team1.index == a & is.element(team2.index, commOpp))) / 
                nrow(subset(subOppDF, (team1.index == a | team2.index == a) &
                                (is.element(team1.index, commOpp) | is.element(team2.index, commOpp)))) 
        }
        
        commWin = sum(selfWP > oppWP)
        commLoss = sum(oppWP > selfWP)
        
        ## aggregate
        if((vsWin + rpiWin + commWin) > (vsLoss + commLoss)){
            wins = wins + 1
        }
        
    }
    return(wins)
}

teamWinPer = sapply(c(1:128), getWinPer, ncaaFootball)
teamVar = cbind.data.frame(teamWinPer, t(sapply(c(1:128), getVariables, ncaaFootball, teamWinPer)))

teamInd$rpi = rep(NA, nrow(teamInd))
for(a in 1:nrow(teamInd)){
    teamInd$rpi[a] = sum(maxRpiWeights[8, ] * teamVar[a, 1:3])
}
teamInd$pairwise = sapply(c(1:128), pairWise, rpiInd = "rpi")

overallRank = cbind.data.frame(c(1:128), teamInd$team[order(teamInd$pairwise, decreasing = T)], 
                               sort(teamInd$pairwise, decreasing = T), 
                               teamInd$conference[order(teamInd$pairwise, decreasing = T)])
colnames(overallRank) = c("Rank", "Team", "Pairwise Wins", "Conference")
write.csv(overallRank, file = "pairwiseRank_10.25.16.csv", quote = F, row.names = F)

#####
## predict wins
#####
rpiPredWin = vector("list", nrow(maxRpiWeights))
pairwisePredWin = vector("list", nrow(maxRpiWeights))
for(b in 4:8){
    prevDF = ncaaFootball[ncaaFootball$week < b, c(3, 6)]
    nextDF = ncaaFootball[ncaaFootball$week == b, c(3, 6)]

    teamInd$predRpi = rep(NA, nrow(teamInd))
    for(a in 1:nrow(teamInd)){
        teamInd$predRpi[a] = sum(maxRpiWeights[(b - 1), ] * teamVar[a, 1:3])
    }
    predPairwise = sapply(c(1:128), pairWise, rpiInd = "predRpi")
    
    rpiPredWin[[b]] = rep(NA, nrow(nextDF))
    pairwisePredWin[[b]] = rep(NA, nrow(nextDF))
    for(a in 1:nrow(nextDF)){
        rpiPredWin[[b]][a] =  sum(which.max(c(teamInd$predRpi[nextDF[a, 1]], teamInd$predRpi[nextDF[a, 2]])) == 1)
        pairwisePredWin[[b]][a] =  sum(which.max(c(predPairwise[nextDF[a, 1]], predPairwise[nextDF[a, 2]])) == 1)
    }
}
sapply(rpiPredWin, mean)
sapply(pairwisePredWin, mean)


#######################################################

##### 
##### rpi functions
##### 
rpi = function(team, weights, winPer, dataf){
    
    temp = winPer[team]
    
    subDF = subset(dataf, team1.index == team | team2.index == team)
    opp = c(setdiff(subDF$team1.index, team), setdiff(subDF$team2.index, team))
    
    oppWin = mean(winPer[is.element(teamInd$index, opp)])
    
    oppOpp = NULL
    for(a in 1:length(opp)){
        oppSubDF = subset(dataf, team1.index == opp[a] | team2.index == opp[a])
        oppOpp = c(oppOpp, c(setdiff(subDF$team1.index, opp[a]), setdiff(subDF$team2.index, opp[a])))
    }
    oppOpp = unique(oppOpp)
    
    oppOppWin = mean(winPer[is.element(teamInd$index, oppOpp)])
    
    rpi = sum(weights * c(temp, oppWin, oppOppWin))
    
    return(rpi)
}

rpiPredictor = function(ind, maxx, selfW, dataf, winPer){
    
    testRpi = sapply(c(1:128), rpi, weights = c(selfW, oppSeq[ind], (maxx - oppSeq[ind])), winPer, dataf = dataf)
    
    predWin = rep(NA, nrow(dataf))
    for(a in 1:nrow(dataf)){
        predWin[a] =  sum(which.max(c(testRpi[dataf[a, 3]], testRpi[dataf[a, 6]])) == 1)
    }
    return(mean(predWin))
}

predFunc = function(ind, selfW, dataf){
    winPer = rep(NA, 128)
    for(a in 1:128){
        winPer[a] = nrow(subset(dataf, team1.index == a)) / 
            nrow(subset(dataf, team1.index == a | team2.index == a))
    }
    
    maxx = (1 - selfSeq[ind])
    oppSeq = seq(0.01, (maxx - 0.01), by = 0.01)
    
    seqPredictor = sapply(c(1:length(oppSeq)), rpiPredictor, maxx, selfSeq[ind], 
                          dataf, winPer)
    
    return(seqPredictor)
}


##
selfSeq = seq(0.02, 0.98, by = 0.02)
weeklyMax = vector("list", nlevels(as.factor(ncaaFootball$week)))

## predict max given all data
library(doSNOW)
library(foreach)
cl = makeCluster(4)
registerDoSNOW(cl)

for(a in 1:length(weeklyMax)){
    prevDF = ncaaFootball[ncaaFootball$week <= a, ]
    time = proc.time()
    weeklyMax[[a]] = foreach(N = 1:length(selfSeq), .export = c("rpiPredictor", "rpi")) %dopar% {
        predFunc(N, selfSeq, prevDF)
    }
    out = proc.time() - time
    cat(c(a, out[3]), "\n")
}

stopCluster(cl)

save(weeklyMax, file = "weeklyMax_10.24.16.RData")

#####
## get max rpis
#####
load("weeklyMax_10.24.16.RData")
plotDF = vector("list", nlevels(as.factor(ncaaFootball$week)))
maxRpiWeights = matrix(NA, nrow = nlevels(as.factor(ncaaFootball$week)), ncol = 4)
colnames(maxRpiWeights) = c("accuracy", "winPer", "oppPer", "oppOppPer")
for(c in 1:length(plotDF)){
    plotDF[[c]] = data.frame(matrix(NA, nrow = length(unlist(weeklyMax[[c]])), ncol = 4))
    colnames(plotDF[[c]]) = c("accuracy", "winPer", "oppPer", "oppOppPer")
    
    b = 1
    for(a in 1:length(weeklyMax[[c]])){
        maxx = (1 - selfSeq[a])
        plotDF[[c]][b:(b + length(weeklyMax[[c]][[a]]) - 1), ] = cbind(weeklyMax[[c]][[a]], selfSeq[a], 
                                                                       seq(0.01, (maxx - 0.01), by = 0.01), 
                                                                       maxx - seq(0.01, (maxx - 0.01), by = 0.01))
        b = b + length(weeklyMax[[c]][[a]])
    }    
    
    topRpis = plotDF[[c]][plotDF[[c]]$accuracy == max(plotDF[[c]]$accuracy), ]
    if(c == 1){
        maxRpiWeights[c, ] = unlist(plotDF[[c]][names(which.max(apply(plotDF[[c]][plotDF[[c]]$accuracy == 
                                                                                      max(plotDF[[c]]$accuracy), ], 1, 
                                                                      min))), ])
    }
    else{
        maxRpiWeights[c, ] = unlist(plotDF[[c]][names(which.min(rowMeans((topRpis[, c(2:4)] - 
                                                                              maxRpiWeights[1,c(2:4)])^2))), ])
    }
    
}

#library(ggplot2)
#pp = ggplot(data = plotDF, aes(x = oppPer, y = winPer, colour = accuracy)) + geom_point() + 
#    scale_colour_gradientn(colours = terrain.colors(10))

#teamInd$rpi = sapply(c(1:128), rpi,  weights = c(0.44, 0.17, 0.39), winPer, ncaaFootball[ncaaFootball$week != 8, ])



