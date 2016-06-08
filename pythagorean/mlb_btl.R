library("BradleyTerry2")

games <- read.csv("mlb_2014.csv",stringsAsFactors=FALSE)
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
