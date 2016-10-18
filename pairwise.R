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


winPer = rep(NA, 128)
for(a in 1:128){
    winPer[a] = nrow(subset(ncaaFootball, team1.index == a)) / 
        nrow(subset(ncaaFootball, team1.index == a | team2.index == a))
}

rpi = function(team, weights, winPer){
    temp = winPer[team]
    
    subDF = subset(ncaaFootball, team1.index == team | team2.index == team)
    opp = c(setdiff(subDF$team1.index, team), setdiff(subDF$team2.index, team))
    
    oppWin = mean(winPer[is.element(teamInd$index, opp)])
    
    oppOpp = NULL
    for(a in 1:length(opp)){
        oppSubDF = subset(ncaaFootball, team1.index == opp[a] | team2.index == opp[a])
        oppOpp = c(oppOpp, c(setdiff(subDF$team1.index, opp[a]), setdiff(subDF$team2.index, opp[a])))
    }
    oppOpp = unique(oppOpp)
    
    oppOppWin = mean(winPer[is.element(teamInd$index, oppOpp)])
    
    rpi = sum(weights * c(temp, oppWin, oppOppWin))
    
    return(rpi)
}

teamInd$rpi = sapply(c(1:128), rpi, weights = c(0.5, 0.21, 0.29), winPer)

pairWise = function(team, rpiInd){
    wins = 0
    for(a in 1:nrow(teamInd)){
        rpiWin = sum(teamInd[teamInd$index == team, rpiInd] > teamInd[teamInd$index == a, rpiInd])
        vsWin = nrow(subset(ncaaFootball, team1.index == team & team2.index == a))
        vsLoss = nrow(subset(ncaaFootball, team1.index == a & team2.index == team))
        if((vsWin + rpiWin) > vsLoss){
            wins = wins + 1
        }
    }
    return(wins)
}

teamInd$pairwise = sapply(c(1:128), pairWise)

## best rpi predictor of past weeks
oppSeq = seq(0.01, 0.74, by = 0.01)
prevDF = ncaaFootball[ncaaFootball$week != 7, c(3, 6)]

rpiPredictor = function(ind, maxx, selfW){
    testRpi = sapply(c(1:128), rpi, weights = c(selfW, oppSeq[ind], (maxx - oppSeq[ind])), winPer)
    
    predWin = rep(NA, nrow(prevDF))
    for(a in 1:nrow(prevDF)){
        predWin[a] =  sum(which.max(c(testRpi[prevDF[a,1]], testRpi[prevDF[a,2]])) == 1)
    }
    return(mean(predWin))
}

seqPredictor = sapply(c(1:length(oppSeq)), rpiPredictor)
plot(oppSeq, seqPredictor)

##
selfSeq = seq(0.02, 0.98, by = 0.02)
seqPredictor = vector("list", length(selfSeq))
for(a in 1:length(selfSeq)){
    maxx = (1 - selfSeq[a])
    oppSeq = seq(0.01, (maxx - 0.01), by = 0.01)
    
    seqPredictor[[a]] = sapply(c(1:length(oppSeq)), rpiPredictor, maxx, selfSeq[a])
    
    cat(a,'\n')
}

#library(ggplot2)

#plotDF = data.frame(matrix(NA, nrow = 2401, ncol = 4))
#colnames(plotDF) = c("")

maxes = lapply(seqPredictor, max)
which.max(unlist(maxes))
maxes = lapply(seqPredictor, which.max)
unlist(maxes)[21]
seqPredictor[[21]][6]
unlist(maxes)[22]
seqPredictor[[22]][17]

selfSeq[21]
(1 - selfSeq[21])
seq(0.01, (1 - selfSeq[21]), by = 0.01)[6] # weights = c(0.42, 0.06, 0.52)

selfSeq[22]
(1 - selfSeq[22])
seq(0.01, (1 - selfSeq[22]), by = 0.01)[17] # weights = c(0.44, 0.17, 0.39)


teamInd$rpi1 = sapply(c(1:128), rpi,  weights = c(0.42, 0.06, 0.52), winPer)
teamInd$pairwise1 = sapply(c(1:128), pairWise, rpiInd = 4)

teamInd$rpi2 = sapply(c(1:128), rpi,  weights = c(0.44, 0.17, 0.39), winPer)
teamInd$pairwise2 = sapply(c(1:128), pairWise, rpiInd = 6)




