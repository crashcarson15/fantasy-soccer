install.packages("rvest")

require(rvest)
require(dplyr)
require(googlesheets)

### define our macro variables here
league <- "FBW"

match_dates <- c("May 18, 2019")
comp <- "Bund"


# first, import our lineups for the week
lineups <- gs_read(ss = gs_title("2019/20 Fußball-Weltliga"), ws = "[DATA] Lineups", range = "E2:I74")

# then, tack on the 2019 suffix to the URL to make sure we default to the right screen (this matters later in the season)
lineups$espn <- paste0(lineups$espn, "?season=2018")

# now, initialize some blank vectors
appear <- as.character(rep("", 72))
goals <- assists <-
  cs <- gc <- win <- draw <- saves <-
    rep(NA, 72)

vectors <- data.frame(appear, goals, assists, cs, gc, win, draw, saves, stringsAsFactors = FALSE)

toscore <- cbind(lineups, vectors)



for (i in 1:72) {
  # first, score the goalkeepers
  if (i %% 9 == 1) {
    # bring in the player's match log
    node_count <- length(read_html(as.character(toscore[i,]$espn)) %>% html_nodes("table"))
    
    read_in <- read_html(as.character(toscore[i,]$espn)) %>% html_nodes("table") %>% .[node_count] %>%
      html_nodes("td") %>% html_text()
    
    # when were the player's most recent games? in what competitions were they played?
    game_dates <- c(read_in[3], read_in[14])
    competitions <- c(read_in[4], read_in[15])
    
    # did the player play this matchday?
    if (game_dates[1] %in% match_dates && competitions[1] == "Bund") {
      toscore[i,]$appear <- read_in[6]
      toscore[i,]$saves <- as.numeric(read_in[7])
      
      result <- read_in[5]
        
        if (substr(result, 1, 1) == "W") {
          toscore[i,]$win <- 1
          toscore[i,]$gc <- as.numeric(substr(result, 5, 5))
        } else if (substr(result, 1, 1) == "D") {
          toscore[i,]$win <- 1
          toscore[i,]$gc <- as.numeric(substr(result, 5, 5))
        } else {
          toscore[i,]$gc <- as.numeric(substr(result, 3, 3))
        }
      
    } else if (game_dates[2] %in% match_dates && competitions[2] == "Bund") { # double check the previous entry in case we're late scoring
      toscore[i,]$appear <- read_in[17]
      toscore[i,]$saves <- as.numeric(read_in[18])
      
      result <- read_in[16]
      
      if (substr(result, 1, 1) == "W") {
        toscore[i,]$win <- 1
        toscore[i,]$gc <- min(as.numeric(substr(result, 3, 3)), as.numeric(substr(result, 5, 5)))
      } else if (substr(result, 1, 1) == "D") {
        toscore[i,]$win <- 1
        toscore[i,]$gc <- as.numeric(substr(result, 5, 5))
      } else {
        toscore[i,]$gc <- max(as.numeric(substr(result, 3, 3)), as.numeric(substr(result, 5, 5)))
      }
      
    } else {
      toscore[i,]$appear <- "Not in Squad"
    }
  }

  # then score outfield players
  else {
  # bring in the player's match log
  node_count <- length(read_html(as.character(toscore[i,]$espn)) %>% html_nodes("table"))
  
  read_in <- read_html(as.character(toscore[i,]$espn)) %>% html_nodes("table") %>% .[node_count] %>%
    html_nodes("td") %>% html_text()
  
  # when were the player's most recent games? in what competitions were they played?
  game_dates <- c(read_in[3], read_in[17])
  competitions <- c(read_in[4], read_in[18])
  
    # did the player play this matchday?
    if (game_dates[1] %in% match_dates && competitions[1] == "Bund") {
      toscore[i,]$appear <- read_in[6]
      toscore[i,]$goals <- as.numeric(read_in[7])
      toscore[i,]$assists <- as.numeric(read_in[8])
      
      # is the player a defender?
      if (toscore[i,]$pos == "D") {
        result <- read_in[5]
        
        if (substr(result, 1, 1) %in% c("W", "D")) {
          toscore[i,]$gc <- min(as.numeric(substr(result, 3, 3)), as.numeric(substr(result, 5, 5)))
        } else {
          toscore[i,]$gc <- max(as.numeric(substr(result, 3, 3)), as.numeric(substr(result, 5, 5)))
        }
        
        if (toscore[i,]$gc == 0) {
          toscore[i,]$cs <- 1
        } else {
          toscore[i,]$cs <- 0
        }
      }
    } else if (game_dates[2] %in% match_dates && competitions[2] == "Bund") { # double check the previous entry in case we're late scoring
      toscore[i,]$appear <- read_in[20]
      toscore[i,]$goals <- as.numeric(read_in[21])
      toscore[i,]$assists <- as.numeric(read_in[22])
      
      # is the player a defender?
      if (toscore[i,]$pos == "D") {
        result <- read_in[19]
        
        if (substr(result, 1, 1) %in% c("W", "D")) {
          toscore[i,]$gc <- min(as.numeric(substr(result, 3, 3)), as.numeric(substr(result, 5, 5)))
        } else {
          toscore[i,]$gc <- max(as.numeric(substr(result, 3, 3)), as.numeric(substr(result, 5, 5)))
        }
        
        if (toscore[i,]$gc == 0) {
          toscore[i,]$cs <- 1
        } else {
          toscore[i,]$cs <- 0
        }
      }
    } else {
      toscore[i,]$appear <- "Not in Squad"
    }
  
  }
}

# now, clean up our dataset
toexport <- toscore %>% select(espn, appear, goals, assists, cs, gc, win, draw, saves) %>% mutate_if(is.numeric, as.character)
toexport[is.na(toexport)] <- ""

# Then, push back to where we're going
gs_edit_cells(ss = gs_title("2019/20 Fußball-Weltliga"), ws = "MD1", input = toexport, anchor = "AQ11")
