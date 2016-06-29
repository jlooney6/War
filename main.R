### How long does an average game of War take? ###

run_jim <- function(deck, n_runs, n_games_per_run, secs_per_hand, secs_per_war) {
  for (num_runs in 1:n_runs) {
    times <- c()
    for (num_games in 1:n_games_per_run) {
      deck <- sample(deck) # Why TF does "sample" mean randomize?
      hand1 <- deck[1:26] # NOT zero-indexed!
      hand2 <- deck[27:52]
      
      # MAIN GAME LOOP
      total_seconds <- 0
      i <- 1 # Index of cards to compare
      while (length(hand1) > 0 && length(hand2) > 0) {
        if (get_rank(hand1[i], ranks) > get_rank(hand2[i], ranks)) {
          # Player 1 gets cards
          hand1 <- c(hand1, hand1[1:i], hand2[1:i])
          hand1 <- tail(hand1, length(hand1) - i) # Move top i cards to bottom
          hand2 <- tail(hand2, length(hand2) - i) # Lose first i cards
          i <- 1
          total_seconds <- total_seconds + secs_per_hand
        } else if (get_rank(hand1[i], ranks) < get_rank(hand2[i], ranks)) {
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
      times <- c(times, total_seconds / 60)
      all_times <- c(all_times, total_seconds / 60)
    }
    averages <- c(averages, mean(times))
    message("Run ", num_runs)
  }
  
  # Return vector of average game lengths
  averages
}

# Initialize some stuff
all_times <- c()
averages <- c()

# Set up deck
ranks <- c("2", "3", "4", "5", "6", "7", "8", "9", "t", "j", "q", "k", "a")
suits <- c("s", "h", "d", "c")
deck <- paste0(rep(ranks, 4), rep(suits, 13)) # Vectorize, vectorize, vectorize!

# Given a card, get it's numeric rank for comparison to another card
get_rank <- function(card, ranks) {
  which(substr(card, 1, 1) == ranks)
}

avg_game_lengths <- run_jim(deck, 100, 100, secs_per_hand = 4, secs_per_war = 8)
message("Average game length (mins): ", mean(avg_game_lengths))
