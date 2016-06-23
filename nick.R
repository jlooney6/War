### How long does an average game of War take? ###

# Play a single game and return the number of seconds it took
play_one_game <- function(deck, secs_per_hand, secs_per_war) {
  # Shuffle and deal
  deck <- sample(deck)
  hand1 <- deck[1:26]
  hand2 <- deck[27:52]
  
  # Start game
  total_seconds <- 0
  i <- 1 # Index of cards to compare
  while (length(hand1) > 0 && length(hand2) > 0) {
    if (get_rank(hand1[i]) > get_rank(hand2[i])) {
      # Player 1 gets cards
      hand1 <- c(hand1, hand1[1:i], hand2[1:i])
      hand1 <- tail(hand1, length(hand1) - i) # Move top i cards to bottom
      hand2 <- tail(hand2, length(hand2) - i) # Lose first i cards
      i <- 1
      total_seconds <- total_seconds + secs_per_hand
    } else if (get_rank(hand1[i]) < get_rank(hand2[i])) {
      # Player 2 gets cards
      hand2 <- c(hand2, hand2[1:i], hand1[1:i])
      hand2 <- tail(hand2, length(hand2) - i)
      hand1 <- tail(hand1, length(hand1) - i)
      i <- 1
      total_seconds <- total_seconds + secs_per_hand
    } else {
      # War!
      i <- i + 4
      total_seconds <- total_seconds + secs_per_war
      if (i > min(length(hand1), length(hand2))) {
        break
      }
    }
  }
  
  # Return number of seconds
  total_seconds
}

# Play n games and return vector of seconds taken for each
play_n_games <- function(n, deck, secs_per_hand, secs_per_war) {
  require(purrr)
  i <- 0
  res <- purrr::rerun(n, {
    i <- i + 1
    if(i %% 100 == 0) message("Run ", i, " / ", n)
    play_one_game(deck, secs_per_hand, secs_per_war)
  })
  unlist(res)
}

# Build deck of cards (unshuffled!)
build_deck <- function() {
  ranks <- c("2", "3", "4", "5", "6", "7", "8", "9", "t", "j", "q", "k", "a")
  suits <- c("s", "h", "d", "c")
  paste0(rep(ranks, 4), rep(suits, 13)) # Vectorize, vectorize, vectorize! 
}

# Given a card, get it's numeric rank for comparison to another card
get_rank <- function(card) {
  ranks <- c("2", "3", "4", "5", "6", "7", "8", "9", "t", "j", "q", "k", "a")
  which(substr(card, 1, 1) == ranks)
}

deck <- build_deck()

avg_game_lengths <- play_n_games(10000, deck, secs_per_hand = 4, secs_per_war = 8)
message("Average game length (mins): ", mean(avg_game_lengths) / 60)
