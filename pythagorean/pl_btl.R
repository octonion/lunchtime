library("BradleyTerry2")

library("RPostgreSQL")

drv <- dbDriver("PostgreSQL")

con <- dbConnect(drv, dbname="soccer")

query <- dbSendQuery(con, "
select
team_name as team,
opponent_name as opponent,
team_score as team_score,
opponent_score as opponent_score,
(case when team_score>opponent_score then 1.0
 else 0.0 end) as outcome
from club.results
where year=2015
;")

games <- fetch(query,n=-1)
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
