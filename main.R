# How long does an average game of War take?
# Assumes 4 seconds for a 2-card hand, +8 seconds for a war
all_times <- c()
averages <- c()
# Setting up deck
deck <- c()
ranks <- c("2", "3", "4", "5", "6", "7", "8", "9", "t", "j", "q", "k", "a")
suits <- c("s", "h", "d", "c")
for (rank in ranks) {
  for (suit in suits) {
    deck <- c(deck, paste(rank, suit, sep = "")) # No space btwn rank and suit, pleez
  }
}
for (num_runs in 1:100) {
  times <- c()
  for (num_games in 1:100) {
    deck <- sample(deck) # Why TF does "sample" mean randomize?
    hand1 <- deck[1:26] # NOT zero-indexed!
    hand2 <- deck[27:52]
    
    # MAIN GAME LOOP
    total_seconds <- 0
    i <- 1 # Index of cards to compare
    while (length(hand1) > 0 && length(hand2) > 0) {
      if (which(substr(hand1[i], 1, 1) == ranks)[[1]] > which(substr(hand2[i], 1, 1) == ranks)[[1]]) {
        # Player 1 gets cards
        hand1 <- c(hand1, hand1[1:i], hand2[1:i])
        hand1 <-
          tail(hand1, length(hand1) - i) # Moved first card to back
        hand2 <- tail(hand2, length(hand2) - i) # Lost first card
        i <- 1
        total_seconds <- total_seconds + 4
      } else if (which(substr(hand1[i], 1, 1) == ranks)[[1]] < which(substr(hand2[i], 1, 1) == ranks)[[1]]) {
        # Player 2 gets cards
        hand2 <- c(hand2, hand2[1:i], hand1[1:i])
        hand2 <- tail(hand2, length(hand2) - i)
        hand1 <- tail(hand1, length(hand1) - i)
        i <- 1
        total_seconds <- total_seconds + 4
      } else {
        # War!
        i <- i + 4
        total_seconds <- total_seconds + 8
        if (i > length(hand1)) {
          break
        } else if (i > length(hand2)) {
          break
        }
      }
    }
    times <- c(times, total_seconds / 60)
    all_times <- c(all_times, total_seconds / 60)
  }
  averages <- c(averages, mean(times))
  print(num_runs)
}
print(mean(averages))