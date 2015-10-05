library("BradleyTerry2")

nfl_games <- read.csv("nfl_games_2012.csv") #,stringsAsFactors=FALSE)
nfl_teams <- read.csv("nfl_teams.csv") #,stringsAsFactors=FALSE)
nfl_teams <- data.frame(team_id=nfl_teams$team_id,team_name=as.character(nfl_teams$team_name))
nfl_opponents <- data.frame(opponent_id=nfl_teams$team_id,opponent_name=as.character(nfl_teams$team_name))

nfl_games <- merge(nfl_games,nfl_teams,by="team_id")
nfl_games <- merge(nfl_games,nfl_opponents,by="opponent_id")

dim(nfl_games)
#nfl_games$team_name <- as.factor(nfl_games$team_name)
#nfl_games$opponent_name <- as.factor(nfl_games$opponent_name)
nfl_games$outcome <- (nfl_games$team_score > nfl_games$opponent_score)

team1 <- as.character(nfl_games$team_name)
team2 <- as.character(nfl_games$opponent_name)

t <- unique(append(team1,team2))

n <- length(t)

g <- data.frame(team=nfl_games$team_name,opponent=nfl_games$opponent_name,outcome=nfl_games$outcome)

dummy_games_won <- data.frame(team=rep(" Dummy",n),opponent=t,outcome=rep(1.0,n))

dummy_games_lost <- data.frame(team=t,opponent=rep(" Dummy",n),outcome=rep(1.0,n))

g <- rbind(g,dummy_games_won)
g <- rbind(g,dummy_games_lost)

#g$team <- as.factor(g$team)
#g$opponent <- as.factor(g$opponent)
dim(g)

fit <- BTm(outcome,team,opponent,data=g)
btl <- as.data.frame(BTabilities(fit))

btl <- btl[with(btl, order(-ability)), ]

btl <- subset(btl,TRUE,select=c(ability))
d <- btl[" Dummy",]
btl$ability <- btl$ability-d

btl$exp_ability <- exp(btl$ability)
btl$p <- btl$exp_ability/(btl$exp_ability+1.0)
btl
sum(btl$p)

quit("no")
