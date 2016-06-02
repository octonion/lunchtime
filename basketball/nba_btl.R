library("BradleyTerry2")

games <- read.csv("nba_2016.csv",stringsAsFactors=FALSE)

#library("RPostgreSQL")

#drv <- dbDriver("PostgreSQL")

#con <- dbConnect(drv, dbname="basketball")

#query <- dbSendQuery(con, "
#select
#team_id as team,
#opponent_id as opponent,
#team_score as team_score,
#opponent_score as opponent_score,
#(case when team_score>opponent_score then TRUE
# else FALSE end) as outcome
#from bbref.results
#where year=2016
#;")

#games <- fetch(query,n=-1)
dim(games)

fit <- BTm(outcome,team,opponent,data=games)

fit

out <- as.data.frame(BTabilities(fit))
out <- out[with(out, order(-ability)), ]

out <- subset(out,TRUE,select=c(ability))
out$ability <- exp(out$ability)

out$team <- rownames(out)
out

scored <- aggregate(team_score ~ team, games, sum)
allowed <- aggregate(opponent_score ~ team, games, sum)

df <- merge(out,scored)
df <- merge(df,allowed)

fit <- lm(log(ability) ~ log(team_score/opponent_score), df)

fit
quit("no")
