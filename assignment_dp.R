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
#   pos <- players$pos
  val <- players$total.points
  player.limits <- list(QB=1,WR=2,RB=2,TE=1,DEF=1,K=1,FLEX=1,BENCH=7)
  total.players <- Reduce('+',player.limits)
  A <- matrix(rep(0,(num.players +1) * 2), ncol=num.players + 1)
  for (j in 1:num.players) {
    position <- players$pos[j]
      #if team's positions aren't filled
      if (rowSums(A[which(players$pos==position)+1,]>0)[2] < player.limits[position])
        A[2,j+1] = max(A[1,j]+A[1,j], values[j] + A[2,j])
      else if (rowSums(A[which(players$pos==c("WR","RB","TE"))+1,]>0)[2] < 
          sum(player.limits[c("WR","RB","TE","FLEX")]))
        A[2,j+1] = max(A[1,j]+A[1,j], values[j] + A[2,j])  
      else if (rowSums(A>0)[2] < total.players)
        A[2,j+1] = max(A[1,j]+A[1,j], values[j] + A[2,j])
      else 
        A[1,j+1] = A[1,j] + A[2,j]
      #if other constraints are met
#       if (rowSums(A>0)[2] >= players.per.team)
        #keep the same amount of fantasy points
#         A[1,j+1] = A[1,j] + A[2,j]
#       else
#         A[2,j+1] = max(A[1,j]+A[1,j], values[j] + A[2,j])
  }
  return(A)
}

A <- assignment.one.team(d)
players.assigned <- d$name[which(A[2,]>0)-1]
