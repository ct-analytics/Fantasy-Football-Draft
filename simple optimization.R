#Constraints
# Each team needs 
# 1QB
# 2RB
# 3WR
# 1RB/WR/TE
# 1DEF
# 1TE
# 
# max fantasy values
setwd("~/Documents/GitHub/Fantasy-Football-Draft")

d<-read.csv(file=paste(getwd(),"/Data/DKSalaries.csv", sep=""),header=T)
require("lpSolve");require("lpSolveAPI")

#number of teams in the league
num.teams<-3
teams<-seq(1,num.teams)

num.players<-length(d$Name)
#set arbitrary player id's
d$id=seq(1,num.players)
players<-unique(d$id)

vars<-data.frame(player.id=rep(players,num.teams))
vars<-merge(x=vars,y=d,by.y="id",by.x="player.id")
vars<-vars[,c("player.id","Position","Name")]
vars$team.id<-rep(seq(1,num.teams),num.players)

ip <- make.lp(0,num.players*num.teams)

set.type(ip,seq(1,num.players*num.teams),type="binary")

set.objfn(ip,rep(d$AvgPointsPerGame,num.teams))
lp.control(ip,sense="max")

for (p in players) {
  #each player can only be on one team
  add.constraint(ip,
                 rep(1,num.teams),
                 "<=",
                 2,
                 which(vars$player.id==p)
                 )
}

for (t in teams) {
  #each team needs at least 1 QB  
  add.constraint(ip,
                 rep(1,sum(vars$Position=="QB")/num.teams),
                 "=",
                 1,
                 which(vars$team.id==t & vars$Position=="QB")
  )
  #each team needs at least 3 WR
  add.constraint(ip,
                 rep(1,sum(vars$Position=="WR")/num.teams),
                 ">=",
                 3,
                 which(vars$team.id==t & vars$Position=="WR")
  )
  #each team needs at least 2 RB
  add.constraint(ip,
                 rep(1,sum(vars$Position=="RB")/num.teams),
                 ">=",
                 2,
                 which(vars$team.id==t & vars$Position=="RB")
  )
  #each team needs at least 1 DST
  add.constraint(ip,
                 rep(1,sum(vars$Position=="DST")/num.teams),
                 ">=",
                 1,
                 which(vars$team.id==t & vars$Position=="DST")
  )
  
  #each team needs at least 1 TE
  add.constraint(ip,
                 rep(1,sum(vars$Position=="TE")/num.teams),
                 ">=",
                 1,
                 which(vars$team.id==t & vars$Position=="TE")
  )
  #each team needs a flex player
  #add.constraint(ip,
  #               rep(1,sum(vars$Position=="TE",vars$Position=="RB",vars$Position=="WR")/num.teams),
  #               "=",
  #               1,
  #               which(vars$team.id==t & (vars$Position=="TE" | vars$Position=="RB" | vars$Position=="WR"))
  #)
  
  #each team needs 9 players
  add.constraint(ip,
                 rep(1,num.players),
                 "=",
                 9,
                 which(vars$team.id==t)
  )
  
  #must stay under salary cap per team
  add.constraint(ip,
                 d$Salary,
                 "<=",
                 50000,
                 which(vars$team.id==t))
  
}

write.lp(ip,paste(getwd(),"/modelformulation.txt",sep=""),type="lp",use.names=T)
solve(ip)
get.objective(ip)
get.variables(ip)

get.constraints(ip)

sol<-vars[get.variables(ip)==1,c("Name","team.id","Position")]
View(sol[order(sol$team.id,sol$Position),])

