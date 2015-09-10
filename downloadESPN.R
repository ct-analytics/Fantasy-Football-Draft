require("XML")
require("stringr")
require("ggplot2")
require("plyr")
options(stringsAsFactors = FALSE)

clean <- function(pos) {
  link<-"http://games.espn.go.com/ffl/tools/projections?&seasonTotals=true&seasonId=2014&slotCategoryId="

  x<-readHTMLTable(paste(link,pos,sep=""))$playertable_0
  
  #update the column names
  names(x)<-c("position.rank","name.team.pos",
              "passing.completions.per.attempt","passing.yards","passing.tds","passing.interceptions",
              "rushing.rushes","rushing.yards","rushing.tds",
              "receiving.catches","receiving.yards","receiving.tds",
              "total.points")
  
  #remove header row
  x<-x[-1,]
  #separate out the completions and attempts
  x$passing.completions <- str_sub(x$passing.completions.per.attempt, 
                                   end=str_locate(string=x$passing.completions.per.attempt, '/')[,1]-1)
  x$passing.attempts <- str_sub(x$passing.completions.per.attempt,
                                start=str_locate(string=x$passing.completions.per.attempt, '/')[,1]+1)
  x[,"passing.completions.per.attempt"]<-NULL
  
  #separate out the names and teams
  if (pos!="16") {
    x$name <- str_sub(x$name.team.pos, end=str_locate(string=x$name.team.pos, ',')[,1]-1)
    x$team <- str_sub(x$name.team.pos, 
                      start=str_locate(string=x$name.team.pos, ',')[,1]+2, 
                      end = str_locate(string=x$name.team.pos, ',')[,1]+4)
    x$team <- str_trim(x$team, side="right")
    x$team <- toupper(x$team) 
  }  else {
    x$name <- str_sub(x$name.team.pos, end=str_locate(string=x$name.team.pos, ' ')[,1]-1)
    team.lookup<-as.data.frame(matrix(c("DEN","Broncos","GB","Packers","NO","Saints","CAR","Panthers",
                                        "WSH","Redskins","DET","Lions","IND","Colts","PHI","Eagles","OAK","Raiders",
                                        "SEA","Seahawks","SF","49ers","DAL","Cowboys","ATL","Falcons","NE","Patriots",
                                        "SD","Chargers","MIN","Vikings","CHI","Bears","KC","Chiefs","CIN","Bengals",
                                        "PIT","Steelers","NYG","Giants","ARI","Cardinals","MIA","Dolphins",
                                        "BAL","Ravens","TB","Bucaneers","CLE","Browns","HOU","Texans","STL","Rams",
                                        "BUF","Bills","NYJ","Jets","TEN","Titans","JAC","Jaguars"),ncol=2,byrow=TRUE))
    names(team.lookup)<-c("team","name")
    x<-merge(x=x,y=team.lookup,by="name")
  } 
 
  
  x[,"name.team.pos"]<-NULL
  
  #change from character to numeric
  for (c in names(x)) {
    if (!(c %in% c("pos","name","team"))) x[,c]<-as.numeric(x[,c])
    x[is.na(x[,c]),c]<-0
  }

  return(x)
}

#Download fantasy football projections from ESPN.com
qb <- clean("0")
rb <- rbind(clean("2"),clean("2&startIndex=40"),clean("2&startIndex=80"))
wr <- rbind(clean("4"),clean("4&startIndex=40"),clean("4&startIndex=80"))
te <- clean("6")
def <- clean("16")
k <- clean("17")

#Add variable for player position
qb$Position <- as.factor("QB")
rb$Position <- as.factor("RB")
wr$Position <- as.factor("WR")
te$Position <- as.factor("TE")
def$Position <- as.factor("DEF")
k$Position <- as.factor("K")

#Merge players across positions
d <- rbind(qb,rb,wr,te,def,k)

#Remove duplicate cases
#d[d$name %in% d[duplicated(d$name),"name"],]

d$rank <- rank(-d$total.points, ties.method="min")

#Order players by overall rank
d <- d[order(d$rank),]

#Density Plot
# g<-ggplot(d, aes(x=total.points)) 
# g + geom_density(fill="blue", alpha=.3) 
# g + xlab("Player's Projected Points") 
# g + ggtitle("Density Plot of ESPN Projected Points")
# 
# ggsave(paste(getwd(),"/Figures/ESPN projections.jpg", sep=""))


#Save file
setwd("~/Documents/Fantasy Football Draft")
save(d, file = paste(getwd(),"/Data/ESPN-Projections.RData", sep=""))
write.csv(d, file=paste(getwd(),"/Data/ESPN-Projections.csv", sep=""), row.names=FALSE)

#clean up memory
rm(def)
rm(k)
rm(qb)
rm(rb)
rm(te)
rm(wr)
rm(clean)
