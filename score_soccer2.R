install.packages("rvest")

install.packages("googlesheets4")

require(rvest)
require(dplyr)
require(googlesheets)
require(googlesheets4)

### define our macro variables here
league <- "BIP"
#match_dates <- c("Nov 29, 2019", "Nov 30, 2019", "Dec 1, 2019", "Dec 2, 2019")
#match_dates <- c("Dec 3, 2019", "Dec 4, 2019", "Dec 5, 2019")
#match_dates <- c("Dec 6, 2019", "Dec 7, 2019", "Dec 8, 2019", "Dec 9, 2019")
#match_dates <- c("Dec 13, 2019", "Dec 14, 2019", "Dec 15, 2019", "Dec 16, 2019")
#match_dates <- c("Dec 17, 2019", "Dec 18, 2019")
#match_dates <- c("Dec 19, 2019", "Dec 20, 2019", "Dec 21, 2019", "Dec 22, 2019")
#match_dates <- c("Dec 26, 2019", "Dec 27, 2019")
#match_dates <- c("Dec 28, 2019", "Dec 29, 2019")
#match_dates <- c("Jan 3, 2020", "Jan 4, 2020", "Jan 5, 2020", "Jan 6, 2020")
#match_dates <- c("Jan 10, 2020", "Jan 11, 2020", "Jan 12, 2020", "Jan 13, 2020")
#match_dates <- c("Jan 17, 2020", "Jan 18, 2020", "Jan 19, 2020", "Jan 20, 2020")
#match_dates <- c("Jan 24, 2020", "Jan 25, 2020", "Jan 26, 2020", "Jan 27, 2020")
#match_dates <- c("Jan 31, 2020", "Feb 1, 2020", "Feb 2, 2020", "Feb 3, 2020")
#match_dates <- c("Feb 7, 2020", "Feb 8, 2020", "Feb 9, 2020", "Feb 10, 2020", "Feb 14, 2020", "Feb 15, 2020", "Feb 16, 2020", "Feb 17, 2020")
#match_dates <- c("Feb 14, 2020", "Feb 15, 2020", "Feb 16, 2020", "Feb 17, 2020")
#match_dates <- c("Feb 21, 2020", "Feb 22, 2020", "Feb 23, 2020", "Feb 24, 2020")
#match_dates <- c("Feb 29, 2020", "Mar 1, 2020", "Mar 8, 2020", "Mar 9, 2020")
#match_dates <- c("Feb 28, 2020", "Feb 29, 2020", "Mar 1, 2020", "Mar 2, 2020")
#match_dates <- c("Mar 6, 2020", "Mar 7, 2020", "Mar 8, 2020", "Mar 9, 2020")
#match_dates <- c("May 16, 2020", "May 17, 2020", "May 18, 2020")
#match_dates <- c("May 22, 2020", "May 23, 2020", "May 24, 2020")
#match_dates <- c("May 26, 2020", "May 27, 2020")
#match_dates <- c("May 28, 2020", "May 29, 2020", "May 30, 2020", "May 31, 2020")
#match_dates <- c("Jun 5, 2020", "Jun 6, 2020", "Jun 7, 2020")
match_dates <- c("Thu 6/11", "Fri 6/12", "Sat 6/13", "Sun 6/14")
match_dates <- c("Mon 6/15", "Tue 6/16", "Wed 6/17", "Thu 6/18")
match_dates <- c("Fri 6/19", "Sat 6/20", "Sun 6/21", "Mon 6/22")

matchday <- "MD26"
search_len <- 3

if (league == "FBW") {
  comp <- "Bund"
  sheet <- "2019/20 Fußball-Weltliga"
  sheet_cd <- "1mzaY2MU6hBNsHo6jz33BC-iNh9mIS1A9JoG3KkOrM1A"
  player_count <- 72
  lineup_range <- "E2:I74"
} else if (league == "BIP") {
  comp <- "Prem"
  sheet <- "2019/20 British Isles Premiership"
  sheet_cd <- "11ms9w92Ntn3GpsoU4Uv7ECAy-WzCteSy9odckmOywPs"
  player_count <- 72
  lineup_range <- "E2:I74"
} else if (league == "LPP") {
  comp <- "La Liga"
  sheet <- "2019/20 Liga Primera de la Península"
  sheet_cd <- "1w-GCE-OZZJiJHKtOtRShx4JZbGXepzU0CXbMgH9GoYg"
  player_count <- 72
  lineup_range <- "E2:I74"
} else if (league == "LPI") {
  comp <- "Serie A"
  sheet <- "2019/20 Lega Primo d'Italia"
  sheet_cd <- "1QWaRuZAUkafbKg0a_siUI335gZtatv3WbJ6DpX19sX0"
  player_count <- 72
  lineup_range <- "E2:I74"
}

# first, import our lineups for the week
lineups <- googlesheets4::sheets_read(ss = as_sheets_id(sheet_cd), sheet = "[DATA] Lineups", range = lineup_range)

# then, tack on the 2019 suffix to the URL to make sure we default to the right screen (this matters later in the season)
lineups$espn <- paste0(lineups$espn, "?season=2019")

# now, initialize some blank vectors
appear <- as.character(rep("", player_count))
goals <- assists <-
  cs <- gc <- win <- draw <- saves <-
    rep(NA, player_count)

vectors <- data.frame(appear, goals, assists, cs, gc, win, draw, saves, stringsAsFactors = FALSE)

toscore <- cbind(lineups, vectors)



for (i in 1:player_count) {
  # bring in the player's match log
  node_count <- length(read_html(as.character(toscore[i,]$espn)) %>% html_nodes("table"))
  
  read_in <- read_html(as.character(toscore[i,]$espn)) %>% html_nodes("table") %>% .[node_count] %>%
    html_nodes("td") %>% html_text()
  
  # get a list of the player's league matches
  matchIndex <- which(read_in %in% comp)
  
  # now, identify the 
  for (j in 1:search_len) {
    date <- read_in[matchIndex[j]-2]
    
    if (date %in% match_dates) {
      toscore[i,]$appear <- read_in[matchIndex[j]+2]
      
      # goalkeepers
      if (i %% 9 == 1) {
        toscore[i,]$goals <- as.numeric(read_in[matchIndex[j]+6])
        toscore[i,]$assists <- as.numeric(read_in[matchIndex[j]+7])
        
        result <- read_in[matchIndex[j]+1]
        
        if (substr(result, 1, 1) == "W") {
          toscore[i,]$win <- 1
          toscore[i,]$draw <- 0
        } else if (substr(result, 1, 1) == "D") {
          toscore[i,]$win <- 0
          toscore[i,]$draw <- 1
        } else {
          toscore[i,]$win <- 0
          toscore[i,]$draw <- 0
        }
        
        toscore[i,]$saves <- as.numeric(read_in[matchIndex[j]+4])
        toscore[i,]$gc <- as.numeric(read_in[matchIndex[j]+5])
        
      }
      
      # outfield players
      else {
        toscore[i,]$goals <- as.numeric(read_in[matchIndex[j]+3])
        toscore[i,]$assists <- as.numeric(read_in[matchIndex[j]+4])
        
      # defenders
        if (toscore[i,]$pos == "D") {
          result <- read_in[matchIndex[j]+1]
          
          if (substr(result, 1, 1) %in% c("W", "D")) {
            toscore[i,]$gc <- min(as.numeric(substr(result, 2, 2)), as.numeric(substr(result, 4, 4)))
          } else {
            toscore[i,]$gc <- max(as.numeric(substr(result, 2, 2)), as.numeric(substr(result, 4, 4)))
          }
          
          if (is.na(toscore[i,]$gc)) {
            toscore[i,]$cs <- NA
          }
          else if (toscore[i,]$gc == 0) {
            toscore[i,]$cs <- 1
          } else {
            toscore[i,]$cs <- 0
          }
        }
        
      }
      
      # if the player didn't play, blank out all result values
      if (toscore[i,]$appear == "Unused Substitute") {
        toscore[i,]$win <- toscore[i,]$draw <- toscore[i,]$gc <- toscore[i,]$cs <- toscore[i,]$win <- toscore[i,]$draw <- toscore[i,]$saves <- NA
      }
      
    } else {
      
    }
  }
  
  
  
  # # first, score the goalkeepers
  # if (i %% 9 == 1) {
  #   
  #   # did the player play this matchday?
  #   for (j in 1:search_len) {
  #     x <- (11*j) - 11 # index where we're starting from -- we want this to be 0 for the first record we check. there are 14 records for each outfield player match
  #     
  #     if (game_dates[j] %in% match_dates && competitions[j] == comp) {
  #       toscore[i,]$appear <- read_in[x+6]
  #       toscore[i,]$saves <- as.numeric(read_in[x+7])
  #       
  #       result <- read_in[x+5]
  #       
  #       if (substr(result, 1, 1) == "W") {
  #         toscore[i,]$win <- 1
  #         toscore[i,]$gc <- min(as.numeric(substr(result, 3, 3)), as.numeric(substr(result, 5, 5)))
  #       } else if (substr(result, 1, 1) == "D") {
  #         toscore[i,]$draw <- 1
  #         toscore[i,]$gc <- as.numeric(substr(result, 5, 5))
  #       } else {
  #         toscore[i,]$gc <- max(as.numeric(substr(result, 3, 3)), as.numeric(substr(result, 5, 5)))
  #       }
  #       
  #       # if the player didn't play, blank out all result values
  #       if (toscore[i,]$appear == "Unused Substitute") {
  #         toscore[i,]$win <- toscore[i,]$draw <- toscore[i,]$gc <- toscore[i,]$cs <- NA
  #       }
  #       #break
  #     } 
  #     #else if (j == search_len) {
  #     #  toscore[i,]$appear <- "Not in Squad"
  #     #}
  #     else {}
  #   }
  # }
  # 
  # # then score outfield players
  # else {
  #   # bring in the player's match log
  #   node_count <- length(read_html(as.character(toscore[i,]$espn)) %>% html_nodes("table"))
  #   
  #   read_in <- read_html(as.character(toscore[i,]$espn)) %>% html_nodes("table") %>% .[node_count] %>%
  #     html_nodes("td") %>% html_text()
  #   
  #   # when were the player's most recent games? in what competitions were they played?
  #   game_dates <- c(read_in[3], read_in[17], read_in[31], read_in[45], read_in[59], read_in[73], read_in[87], read_in[101], read_in[115], read_in[129])
  #   competitions <- c(read_in[4], read_in[18], read_in[32], read_in[46], read_in[60], read_in[74], read_in[88], read_in[102], read_in[116], read_in[130])
  #   
  #     # did the player play this matchday?
  #   for (j in 1:search_len) {
  #     x <- (14*j) - 14 # index where we're starting from -- we want this to be 0 for the first record we check. there are 14 records for each outfield player match
  #     
  #     if (game_dates[j] %in% match_dates && competitions[j] == comp) {
  #       toscore[i,]$appear <- read_in[x+6]
  #       toscore[i,]$goals <- as.numeric(read_in[x+7])
  #       toscore[i,]$assists <- as.numeric(read_in[x+8])
  #       
  #       # is the player a defender?
  #       if (toscore[i,]$pos == "D") {
  #         result <- read_in[x+5]
  #         
  #         if (substr(result, 1, 1) %in% c("W", "D")) {
  #           toscore[i,]$gc <- min(as.numeric(substr(result, 3, 3)), as.numeric(substr(result, 5, 5)))
  #         } else {
  #           toscore[i,]$gc <- max(as.numeric(substr(result, 3, 3)), as.numeric(substr(result, 5, 5)))
  #         }
  #         
  #         if (toscore[i,]$gc == 0) {
  #           toscore[i,]$cs <- 1
  #         } else {
  #           toscore[i,]$cs <- 0
  #         }
  #       }
  #       # if the player didn't play, blank out all result values
  #       if (toscore[i,]$appear == "Unused Substitute") {
  #         toscore[i,]$gc <- toscore[i,]$cs <- NA
  #       }
  #       #break
  #     } 
  #     #else if (j == search_len) {
  #     #  toscore[i,]$appear <- "Not in Squad"
  #     #} 
  #     else {}
  #   }
  # }
  rm(read_in)
}

# now, clean up our dataset
toexport <- toscore %>% select(espn, appear, goals, assists, cs, gc, win, draw, saves) %>% mutate_if(is.numeric, as.character)
toexport$appear <- ifelse(toexport$appear == "", "Not in Squad", toexport$appear)
toexport[is.na(toexport)] <- ""

# Then, push back to where we're going
googlesheets4::write_sheet(toexport, ss = as_sheets_id(sheet_cd), sheet = "[DATA] Scores")
#gs_edit_cells(ss = gs_title(sheet), ws = matchday, input = toexport, anchor = "AQ10")
