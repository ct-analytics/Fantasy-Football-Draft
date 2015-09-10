#Constraints
# Each team needs 
# 1QB
# 2RB
# 2WR
# 1RB/WR/TE
# 1DEF
# 1K
# 1TE
# 7BE
# 
# max fantasy values
setwd("~/Documents/Fantasy Football Draft")
d<-read.csv(file=paste(getwd(),"/Data/ESPN-Projections.csv", sep=""))
d$id <- as.integer(factor(paste(d$name,d$team)))

require("lpSolve");require("lpSolveAPI")

#number of teams in the league
num.teams<-10
teams<-seq(1,num.teams)

num.players<-length(unique(d$id))
players<-unique(d$id)

vars<-data.frame(player.id=rep(players,num.teams))
vars<-merge(x=vars,y=d,by.y="id",by.x="player.id")
vars<-vars[,c("player.id","pos","name")]
vars$team.id<-rep(seq(1,num.teams),num.players)

ip <- make.lp(0,num.players*num.teams)

set.type(ip,seq(1,num.players*num.teams),type="binary")

set.objfn(ip,rep(d$total.points,num.teams))
lp.control(ip,sense="max")

for (p in players) {
  #each player can only be on one team
  add.constraint(ip,
                 rep(1,num.teams),
                 "<=",
                 1,
                 which(vars$player.id==p)
                 )
}

for (t in teams) {
  #each team needs at least 1 QB  
  add.constraint(ip,
                 rep(1,sum(vars$pos=="QB")/num.teams),
                 ">=",
                 1,
                 which(vars$team.id==t & vars$pos=="QB")
  )
  #each team needs at least 2 WR
  add.constraint(ip,
                 rep(1,sum(vars$pos=="WR")/num.teams),
                 ">=",
                 2,
                 which(vars$team.id==t & vars$pos=="WR")
  )
  #each team needs at least 2 RB
  add.constraint(ip,
                 rep(1,sum(vars$pos=="RB")/num.teams),
                 ">=",
                 2,
                 which(vars$team.id==t & vars$pos=="RB")
  )
  #each team needs at least 1 DEF
  add.constraint(ip,
                 rep(1,sum(vars$pos=="DEF")/num.teams),
                 ">=",
                 1,
                 which(vars$team.id==t & vars$pos=="DEF")
  )
  #each team needs at least 1 K
  add.constraint(ip,
                 rep(1,sum(vars$pos=="K")/num.teams),
                 ">=",
                 1,
                 which(vars$team.id==t & vars$pos=="K")
  )
  #each team needs at least 1 TE
  add.constraint(ip,
                 rep(1,sum(vars$pos=="TE")/num.teams),
                 ">=",
                 1,
                 which(vars$team.id==t & vars$pos=="TE")
  )
  #each team needs a flex player
  add.constraint(ip,
                 rep(1,sum(vars$pos=="TE",vars$pos=="RB",vars$pos=="WR")/num.teams),
                 ">=",
                 6,
                 which(vars$team.id==t & (vars$pos=="TE" | vars$pos=="RB" | vars$pos=="WR"))
  )
  #each team needs 16 players
  add.constraint(ip,
                 rep(1,num.players),
                 "=",
                 16,
                 which(vars$team.id==t)
  )
}

write.lp(ip,paste(getwd(),"/modelformulation.txt",sep=""),type="lp",use.names=T)
solve(ip)
get.objective(ip)
get.variables(ip)

get.constraints(ip)

sol<-vars[get.variables(ip)==1,c("name","team.id","pos")]
View(sol[order(sol$team.id,sol$pos),])

