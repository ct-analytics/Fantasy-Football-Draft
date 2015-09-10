require("XML")
require("stringr")
require("ggplot2")
require("plyr")
options(stringsAsFactors = FALSE)


link<-"http://fftoday.com/rankings/playerproj.php?Season=2014&PosID=10&LeagueID=26955"

x<-readHTMLTable(link)
qb <- x[11]$'NULL'
qb <- qb[-1,-1]
names(qb)<-c("Name","Team","Bye.Week","Passing.Completions","Passing.Attempts",
             "Passing.Yards","Passing.TDs","Passing.INTs",
             "Rushing.Attempts","Rushing.Yards","Rushing.TDs","FFPTs")

qb$Name<-sapply(qb$Name,function(x) substring(x,3))

link<-"http://fftoday.com/rankings/playerproj.php?Season=2014&PosID=20&LeagueID=26955"
x<-readHTMLTable(link)
rb <- x[11]$'NULL'
rb <- rb[-1,-1]
names(rb)<-c("Name","Team","Bye.Week","Rushing.Attempts","Rushing.Yards","Rushing.TDs",
             "Receiving.Catches","Receiving.Yards","Receiving.TDs","FFPTs")
rb$Name<-sapply(rb$Name,function(x) substring(x,3))

link<-"http://fftoday.com/rankings/playerproj.php?Season=2014&PosID=30&LeagueID=26955"
x<-readHTMLTable(link)
wr <- x[11]$'NULL'
wr <- wr[-1,-1]
names(wr)<-c("Name","Team","Bye.Week","Receiving.Catches","Receiving.Yards","Receiving.TDs",
             "Rushing.Attempts","Rushing.Yards","Rushing.TDs","FFPTs")
wr$Name<-sapply(wr$Name,function(x) substring(x,3))

link<-"http://fftoday.com/rankings/playerproj.php?Season=2014&PosID=40&LeagueID=26955"
x<-readHTMLTable(link)
te <- x[11]$'NULL'
te <- te[-1,-1]
names(te)<-c("Name","Team","Bye.Week","Receiving.Catches","Receiving.Yards","Receiving.TDs","FFPTs")
te$Name<-sapply(te$Name,function(x) substring(x,3))

link<-"http://fftoday.com/rankings/playerproj.php?Season=2014&PosID=80&LeagueID=26955"
x<-readHTMLTable(link)
k <- x[11]$'NULL'
k <- k[-1,-1]
names(k)<-c("Name","Team","Bye.Week","Field.Goals.Made","Field.Goals.Attempts","Field.Goals.Pct",
             "Extra.Points.Made","Extra.Points.Attempts","FFPTs")
k$Name<-sapply(k$Name,function(x) substring(x,3))

link<-"http://fftoday.com/rankings/playerproj.php?Season=2014&PosID=99&LeagueID=26955"
x<-readHTMLTable(link)
def <- x[11]$'NULL'
def <- def[-1,-1]
names(def)<-c("Name","Bye.Week","Sacks","Fumble.Recovered","Interceptions",
            "Defensive.TDs","Points.Against","Passing.Yards.per.Game","Rushing.Yards.per.Game",
            "Safety","Kicking.TDs","FFPTs")
def$Name<-sapply(def$Name,function(x) tail(strsplit(x,split=" ")[[1]],1))
team.lookup<-as.data.frame(matrix(c("DEN","Broncos","GB","Packers","NO","Saints","CAR","Panthers",
                                    "WSH","Redskins","DET","Lions","IND","Colts","PHI","Eagles","OAK","Raiders",
                                    "SEA","Seahawks","SF","49ers","DAL","Cowboys","ATL","Falcons","NE","Patriots",
                                    "SD","Chargers","MIN","Vikings","CHI","Bears","KC","Chiefs","CIN","Bengals",
                                    "PIT","Steelers","NYG","Giants","ARI","Cardinals","MIA","Dolphins",
                                    "BAL","Ravens","TB","Buccaneers","CLE","Browns","HOU","Texans","STL","Rams",
                                    "BUF","Bills","NYJ","Jets","TEN","Titans","JAC","Jaguars"),ncol=2,byrow=TRUE))
names(team.lookup)<-c("Team","Name")
def<-merge(x=def,y=team.lookup,by="Name")

rm(x);rm(link);rm(team.lookup);

qb$Position <- as.factor("QB")
rb$Position <- as.factor("RB")
wr$Position <- as.factor("WR")
te$Position <- as.factor("TE")
def$Position <- as.factor("DEF")
k$Position <- as.factor("K")

d <- rbind.fill(qb,rb,wr,te,def,k)
d$FFPTs <- as.numeric(d$FFPTs)
d[is.na(d)]<-0

d$id <- as.integer(factor(paste(d$Name,d$Team)))
rm(list=c("qb","rb","wr","te","def","k"))
