install.packages("devtools") 
install.packages("bigchess")
install.packages("dplyr")
devtools::install_github("dryguy/rbitr")
library(rbitr)
library(bigchess)
library(dplyr)

rm(list = ls())

setwd("/Users/franciszekkaczmarek/Desktop/Praca licencjacka/Algorytm/1 Podejście/Dane")
engine_path <- "/opt/homebrew/Cellar/stockfish/15/bin/stockfish"
PlayerNames <-c("Carlsen, Magnus", "Nepomniachtchi, Ian", "Ding, Liren")
listpgn <- dir(pattern = "*.pgn")

for (k in 1:length(listpgn)){
  
# importing data
player <- get_pgn(listpgn[k])

#cleaning data
PlayerToAnalyse <- PlayerNames[k]
player <- as.data.frame(player)
player <- player[,c("Event","EventType","White","WhiteElo","Black","BlackElo","Result","Movetext")]
player[,"Movetext"] <- clean_movetext(player[,"Movetext"])
PlayerToAnalyseElo <- rep(NA, nrow(player))
for (i in 1:nrow(player)){
  if (player$White[i]==PlayerNames[k]) {
    PlayerToAnalyseElo[i] <- player$WhiteElo[i]
  } else if (player$Black[i]==PlayerNames[k]) {
    PlayerToAnalyseElo[i] <- player$BlackElo[i]
  }
}
OpponentElo <- rep(NA, nrow(player))
for (i in 1:nrow(player)){
  if (player$White[i]!=PlayerNames[k]) {
    OpponentElo[i] <- player$WhiteElo[i]
  } else if (player$Black[i]!=PlayerNames[k]) {
    OpponentElo[i] <- player$BlackElo[i]
  }
}
PlayerToAnalyseACPL <- rep(NA, nrow(player))
OpponentACPL <- rep(NA, nrow(player))
player <- data.frame(player$Event, player$EventType, player$White, player$Black, PlayerToAnalyseElo, PlayerToAnalyseACPL, 
                     OpponentElo, OpponentACPL, player$Result, player$Movetext)
player <- player [complete.cases(player[, c(2,5)]),]

player <- arrange(player, player.EventType, PlayerToAnalyseElo)
player_clasical <- subset(player, player.EventType=="team" | player.EventType=="match" | player.EventType=="team-swiss" | 
                            player.EventType=="tourn" | player.EventType=="swiss" | player.EventType=="k.o." | player.EventType=="game" | 
                            player.EventType=="team-schev" | player.EventType=="schev" | player.EventType=="team-tourn" | 
                            player.EventType=="team-match")
player_clasical <- arrange(player_clasical, PlayerToAnalyseElo)

player_rapid <- subset(player, player.EventType=="game (rapid)" | player.EventType=="k.o. (rapid)" | 
                         player.EventType=="match (rapid)" | player.EventType=="rapid" | player.EventType=="swiss (rapid)" | 
                         player.EventType=="tourn (rapid)")
player_rapid <- arrange(player_rapid, PlayerToAnalyseElo)

player_blitz <- subset(player, player.EventType=="blitz" | player.EventType=="k.o. (blitz)" | player.EventType=="match (blitz)" | 
                         player.EventType=="swiss (blitz)" | player.EventType=="tourn (blitz)")
player_blitz <- arrange(player_blitz, PlayerToAnalyseElo)

#creating ACPL for player which we are analyzing and his opponent
##clasical
nc <- nrow(player_clasical)
pb <- txtProgressBar(min = 0, max = nc, style = 3)

for (i in 1:nc) {
  Sys.sleep(0.1)
  gamelog <- evaluate_game(player_clasical[i,10], engine_path, limiter = 'nodes',
                           limit = 2250000, mute = TRUE)
  scores <- unlist(parse_gamelog(gamelog, 'score'))
  evals <- convert_scores(scores)
  if (player_clasical$player.White[i]==PlayerNames[k]) {
    player_clasical$PlayerToAnalyseACPL[i] <- get_acpl(evals, 'white', cap = 1000, cap_action = 'replace')
  } else {player_clasical$PlayerToAnalyseACPL[i] <- get_acpl(evals, 'black', cap = 1000, cap_action = 'replace')
  }
  if (player_clasical$player.White[i]!=PlayerNames[k]) {
    player_clasical$OpponentACPL[i] <- get_acpl(evals, 'white', cap = 1000, cap_action = 'replace')
  } else {player_clasical$OpponentACPL[i] <- get_acpl(evals, 'black', cap = 1000, cap_action = 'replace')
  }
  setTxtProgressBar(pb, i)
}

write.csv(player_clasical, paste0("/Users/franciszekkaczmarek/Desktop/Praca licencjacka/Algorytm/1 Podejście/Dane/Dane1/", PlayerNames[k],"_classical",".csv"))

##rapid
nr <- nrow(player_rapid)
pb1 <- txtProgressBar(min = 0, max = nr, style = 3)
for (i in 1:nr) {
  Sys.sleep(0.1)
  gamelog <- evaluate_game(player_rapid[i,10], engine_path, limiter = 'nodes',
                           limit = 2250000, mute = FALSE)
  scores <- unlist(parse_gamelog(gamelog, 'score'))
  evals <- convert_scores(scores)
  if (player_rapid$player.White[i]==PlayerNames[k]) {
    player_rapid$PlayerToAnalyseACPL[i] <- get_acpl(evals, 'white', cap = 1000, cap_action = 'replace')
  } else {player_rapid$PlayerToAnalyseACPL[i] <- get_acpl(evals, 'black', cap = 1000, cap_action = 'replace')
  }
  if (player_rapid$player.White[i]!=PlayerNames[k]) {
    player_rapid$OpponentACPL[i] <- get_acpl(evals, 'white', cap = 1000, cap_action = 'replace')
  } else {player_rapid$OpponentACPL[i] <- get_acpl(evals, 'black', cap = 1000, cap_action = 'replace')
  }
  setTxtProgressBar(pb1, i)
}

write.csv(player_rapid, paste0("/Users/franciszekkaczmarek/Desktop/Praca licencjacka/Algorytm/1 Podejście/Dane/Dane1/", PlayerNames[k],"_rapid",".csv"))

##blitz
nb <- nrow(player_blitz)
pb2 <- txtProgressBar(min = 0, max = nb, style = 3)
for (i in 1:nb) {
  Sys.sleep(0.1)
  gamelog <- evaluate_game(player_blitz[i,10], engine_path, limiter = 'nodes',
                           limit = 2250000, mute = FALSE)
  scores <- unlist(parse_gamelog(gamelog, 'score'))
  evals <- convert_scores(scores)
  if (player_blitz$player.White[i]==PlayerNames[k]) {
    player_blitz$PlayerToAnalyseACPL[i] <- get_acpl(evals, 'white', cap = 1000, cap_action = 'replace')
  } else {player_blitz$PlayerToAnalyseACPL[i] <- get_acpl(evals, 'black', cap = 1000, cap_action = 'replace')
  }
  if (player_blitz$player.White[i]!=PlayerNames[k]) {
    player_blitz$OpponentACPL[i] <- get_acpl(evals, 'white', cap = 1000, cap_action = 'replace')
  } else {player_blitz$OpponentACPL[i] <- get_acpl(evals, 'black', cap = 1000, cap_action = 'replace')
  }
  setTxtProgressBar(pb2, i)
}

write.csv(player_blitz, paste0("/Users/franciszekkaczmarek/Desktop/Praca licencjacka/Algorytm/1 Podejście/Dane/Dane1/", PlayerNames[k],"_blitz",".csv"))


}






