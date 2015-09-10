assignment <- function(players,values,team.size) {
  num.players<-length(players)
  A <- matrix(rep(0, (team.size) * (num.players)), ncol=team.size)
  for (j in 1:num.players) {
    for (Y in 1:team.size) {
      #if player isn't assigned
      #if team's positions aren't filled
      #if other constraints are met
      if (values[j] > Y)
        A[j+1, Y+1] = A[j, Y+1]
      else
        A[j+1, Y+1] = max(A[j, Y+1], values[j] + A[j, Y- w[j]+1])
    }
  }
  return(A)
}

assignment.one.team <- function(players) {
  num.players<-length(players$id)
  values <- players$total.points
  player.limits <- list(QB=1,WR=2,RB=2,TE=1,DEF=1,K=1,FLEX=1,BENCH=7)
  flex.positions <- c("WR","RB","TE","FLEX")
  total.players <- Reduce('+',player.limits)
  required.players <- total.players - player.limits$BENCH
  A <- matrix(rep(0,(num.players +1) * 2), ncol=num.players + 1)
  
  QB <- which(players$pos=="QB")+1
  RB <- which(players$pos=="RB")+1
  WR <- which(players$pos=="WR")+1
  TE <- which(players$pos=="TE")+1
  K <- which(players$pos=="K")+1
  DEF <- which(players$pos=="DEF")+1
  FLEX <- which(players$pos %in% c("WR","RB","TE"))+1
  
  for (j in 1:num.players) {
    position <- players$pos[j]
    cat(paste("Player",j,"is a",position))
    same.position <- which(players$pos==position)+1
    
#     required <- sum(max(player.limits$QB - rowSums(A[,QB]>0)[2],0),
#                     max(player.limits$WR - rowSums(A[,WR]>0)[2],0),
#                     max(player.limits$RB - rowSums(A[,RB]>0)[2],0),
#                     max(player.limits$TE - rowSums(A[,TE]>0)[2],0),
#                     max(player.limits$K - rowSums(A[,K]>0)[2],0),
#                     max(player.limits$DEF - rowSums(A[,DEF]>0)[2],0),
#                     max(Reduce('+',player.limits[flex.positions]) - rowSums(A[,FLEX]>0)[2],0))
    
    limit.at.pos <- (rowSums(A[,same.position]>0)[2] < player.limits[position])[[1]]
    limit.at.flex <- (rowSums(A[,FLEX]>0)[2] < Reduce('+',player.limits[flex.positions]))
    is.flex <- position %in% flex.positions
    limit.at.team <- (total.players - required - rowSums(A>0)[2] > 0)
    
    cat(paste(" ",limit.at.pos,limit.at.flex & is.flex,limit.at.team,"\n"))
    
    if (limit.at.pos | (limit.at.flex & is.flex) | limit.at.team) {
      A[2,j+1] = max(A[1,j]+A[1,j], values[j] + A[2,j])
    } else {
      A[1,j+1] = A[1,j] + A[2,j]
    }
  }
  return(A)
}

A <- assignment.one.team(d)
players.assigned <- d$name[which(A[2,]>0)-1]
num.players.assigned <- rowSums(A>0)[2]
