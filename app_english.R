# ============================================================
# Heads-Up Texas Hold'em vs. GTO-Inspired Bot
# Practice game and post-flop labs with staged card reveals.
# All game logic uses the complete board; display reveals it sequentially.
# ============================================================

library(shiny)

# ============================================================
# CONSTANTS
# ============================================================

SB <- 1
BB <- 2
STARTING_STACK <- 200
HUMAN <- "Human"
BOT <- "Bot"
STREETS <- c("Pre-flop", "Flop", "Turn", "River")

# ============================================================
# CARD / DECK FUNCTIONS
# ============================================================

create_deck <- function() {
  ranks <- c("2", "3", "4", "5", "6", "7", "8", "9", "T", "J", "Q", "K", "A")
  suits <- c("c", "d", "h", "s")
  deck <- expand.grid(rank = ranks, suit = suits, stringsAsFactors = FALSE)
  deck$card <- paste0(deck$rank, deck$suit)
  deck$card
}

shuffle_deck <- function(deck) {
  sample(deck, length(deck), replace = FALSE)
}

card_rank <- function(card) {
  substr(card, 1, 1)
}

card_suit <- function(card) {
  substr(card, 2, 2)
}

rank_value <- function(rank) {
  lookup <- c(
    "2" = 2, "3" = 3, "4" = 4, "5" = 5,
    "6" = 6, "7" = 7, "8" = 8, "9" = 9,
    "T" = 10, "J" = 11, "Q" = 12,
    "K" = 13, "A" = 14
  )
  unname(lookup[rank])
}

card_label <- function(card) {
  suit_symbol <- c(c = "♣", d = "♦", h = "♥", s = "♠")[card_suit(card)]
  paste0(card_rank(card), suit_symbol)
}

# ============================================================
# HAND EVALUATION
# ============================================================

evaluate_five <- function(cards) {
  ranks <- sort(
    vapply(cards, function(x) rank_value(card_rank(x)), numeric(1)),
    decreasing = TRUE
  )
  suits <- vapply(cards, card_suit, character(1))
  counts <- table(ranks)
  unique_ranks <- sort(unique(ranks), decreasing = TRUE)
  
  # Wheel: A-2-3-4-5
  if (14 %in% unique_ranks &&
      all(c(2, 3, 4, 5) %in% unique_ranks)) {
    straight_high <- 5
  } else if (
    length(unique_ranks) == 5 &&
    max(unique_ranks) - min(unique_ranks) == 4
  ) {
    straight_high <- max(unique_ranks)
  } else {
    straight_high <- NA
  }
  is_flush <- length(unique(suits)) == 1
  # Straight flush / Royal flush
  if (!is.na(straight_high) && is_flush) {
    return(list(
      category = 8,
      name = ifelse(straight_high == 14, "Royal Flush", "Straight Flush"),
      tiebreak = straight_high
    ))
  }
  
  # Four of a kind
  if (4 %in% counts) {
    quad <- as.numeric(names(counts[counts == 4]))
    kicker <- max(as.numeric(names(counts[counts != 4])))
    return(list(category = 7, name = "Four of a Kind", tiebreak = c(quad, kicker)))
  }
  
  # Full house
  triples <- as.numeric(names(counts[counts == 3]))
  pairs <- as.numeric(names(counts[counts == 2]))
  if (
    length(triples) >= 1 &&
    (length(pairs) >= 1 || length(triples) >= 2)
  ) {
    trips <- max(triples)
    if (length(triples) >= 2) {
      pair_rank <- max(triples[triples != trips])
    } else {
      pair_rank <- max(pairs)
    }
    return(list(category = 6, name = "Full House", tiebreak = c(trips, pair_rank)))
  }
  
  # Flush
  if (is_flush) {
    return(list(category = 5, name = "Flush", tiebreak = ranks))
  }
  
  # Straight
  if (!is.na(straight_high)) {
    return(list(category = 4, name = "Straight", tiebreak = straight_high))
  }
  
  # Three of a kind
  if (3 %in% counts) {
    trips <- max(as.numeric(names(counts[counts == 3])))
    kickers <- sort(as.numeric(names(counts[counts != 3])), decreasing = TRUE)
    return(list(category = 3, name = "Three of a Kind", tiebreak = c(trips, kickers)))
  }
  
  # Two pair
  pair_ranks <- sort(as.numeric(names(counts[counts == 2])), decreasing = TRUE)
  if (length(pair_ranks) >= 2) {
    kicker <- max(as.numeric(names(counts[counts != 2])))
    return(list(category = 2, name = "Two Pair", tiebreak = c(pair_ranks[1:2], kicker)))
  }
  
  # One pair
  if (length(pair_ranks) == 1) {
    kickers <- sort(as.numeric(names(counts[counts != 2])), decreasing = TRUE)
    return(list(category = 1, name = "One Pair", tiebreak = c(pair_ranks, kickers)))
  }
  
  # High card
  list(category = 0, name = "High Card", tiebreak = ranks)
}

compare_hand_rank <- function(a, b) {
  if (a$category != b$category) {
    return(sign(a$category - b$category))
  }
  n <- max(length(a$tiebreak), length(b$tiebreak))
  aa <- c(a$tiebreak, rep(0, n - length(a$tiebreak)))
  bb <- c(b$tiebreak, rep(0, n - length(b$tiebreak)))
  for (i in seq_len(n)) {
    if (aa[i] != bb[i]) {
      return(sign(aa[i] - bb[i]))
    }
  }
  0
}

evaluate_seven <- function(cards) {
  if (length(cards) < 5) {
    return(NULL)
  }
  combos <- combn(cards, 5)
  evaluations <- lapply(
    seq_len(ncol(combos)),
    function(i) {
      evaluate_five(combos[, i])
    }
  )
  best <- evaluations[[1]]
  if (length(evaluations) > 1) {
    for (i in 2:length(evaluations)) {
      if (
        compare_hand_rank(evaluations[[i]], best) > 0
      ) {
        best <- evaluations[[i]]
      }
    }
  }
  best
}

# ============================================================
# BOT STRATEGY
# ============================================================

preflop_strength <- function(cards) {
  r1 <- rank_value(card_rank(cards[1]))
  r2 <- rank_value(card_rank(cards[2]))
  suited <- card_suit(cards[1]) ==
    card_suit(cards[2])
  high <- max(r1, r2)
  low <- min(r1, r2)
  if (r1 == r2) {
    if (high >= 14) return(1.00)
    if (high >= 13) return(0.95)
    if (high >= 11) return(0.88)
    if (high >= 9) return(0.78)
    if (high >= 7) return(0.65)
    return(0.52)
  }
  if (high == 14 && low >= 10) {
    return(ifelse(suited, 0.94, 0.88))
  }
  if (high == 13 && low >= 10) {
    return(ifelse(suited, 0.86, 0.76))
  }
  if (high == 12 && low >= 10) {
    return(ifelse(suited, 0.77, 0.65))
  }
  if (high == 11 && low >= 10) {
    return(ifelse(suited, 0.69, 0.58))
  }
  gap <- high - low
  if (suited && gap == 1 && high >= 6) {
    return(0.62)
  }
  if (suited && gap == 2 && high >= 7) {
    return(0.53)
  }
  if (suited && low >= 5) {
    return(0.47)
  }
  if (high >= 10) {
    return(0.45)
  }
  if (high >= 8) {
    return(0.35)
  }
  0.22
}

classify_postflop <- function(hole, board) {
  eval <- evaluate_seven(c(hole, board))
  category <- eval$category
  if (category >= 3) {
    return("Strong Made Hand")
  }
  if (category == 2) {
    return("Medium Made Hand")
  }
  if (category == 1) {
    board_values <- vapply(board, function(x) rank_value(card_rank(x)), numeric(1))
    hole_values <- vapply(hole, function(x) rank_value(card_rank(x)), numeric(1))
    if (max(hole_values) >= max(board_values)) {
      return("Medium Made Hand")
    }
    return("Weak Made Hand")
  }
  cards <- c(hole, board)
  suits <- vapply(cards, card_suit, character(1))
  if (any(table(suits) >= 4)) {
    return("Draw")
  }
  values <- sort(unique(vapply(cards, function(x) rank_value(card_rank(x)), numeric(1))))
  if (14 %in% values) {
    values <- c(1, values)
  }
  if (length(values) >= 4) {
    for (i in seq_len(length(values) - 3)) {
      if (
        max(values[i:(i + 3)]) -
        min(values[i:(i + 3)]) <= 4
      ) {
        return("Draw")
      }
    }
  }
  "Air"
}

board_texture <- function(board) {
  if (length(board) < 3) {
    return("Dry")
  }
  ranks <- vapply(board, function(x) rank_value(card_rank(x)), numeric(1))
  suits <- vapply(board, card_suit, character(1))
  if (any(table(suits) >= 3)) {
    return("Wet")
  }
  if (any(table(ranks) >= 2)) {
    return("Paired")
  }
  "Dry"
}

bot_preflop_action <- function(
    hole,
    amount_to_call,
    stack
) {
  strength <- preflop_strength(hole)
  if (amount_to_call == 0) {
    if (strength >= 0.80) {
      return(sample(c("raise", "check"), 1, prob = c(0.75, 0.25)))
    }
    if (strength >= 0.55) {
      return(sample(c("raise", "check"), 1, prob = c(0.55, 0.45)))
    }
    if (strength >= 0.38) {
      return(sample(c("raise", "check"), 1, prob = c(0.30, 0.70)))
    }
    return("check")
  }
  if (strength >= 0.82) {
    return(sample(c("raise", "call"), 1, prob = c(0.65, 0.35)))
  }
  if (strength >= 0.60) {
    if (stack > amount_to_call &&
        runif(1) < 0.20) {
      return("raise")
    }
    return("call")
  }
  if (strength >= 0.43) {
    if (runif(1) < 0.60) {
      return("call")
    }
    return("fold")
  }
  if (runif(1) < 0.18) {
    return("call")
  }
  "fold"
}

bot_postflop_action <- function(
    hole,
    board,
    amount_to_call,
    pot,
    stack
) {
  tier <- classify_postflop(hole, board)
  texture <- board_texture(board)
  
  # -------------------------------
  # Facing a check
  # -------------------------------
  if (amount_to_call == 0) {
    if (tier == "Strong Made Hand") {
      p <- ifelse(texture == "Wet", 0.72, 0.62)
      return(ifelse(runif(1) < p, "raise", "check"))
    }
    if (tier == "Medium Made Hand") {
      p <- ifelse(texture == "Dry", 0.45, 0.30)
      return(ifelse(runif(1) < p, "raise", "check"))
    }
    if (tier == "Draw") {
      return(ifelse(runif(1) < 0.40, "raise", "check"))
    }
    return(ifelse(runif(1) < 0.20, "raise", "check"))
  }
  
  # -------------------------------
  # Facing a bet
  # -------------------------------
  call_ratio <- amount_to_call /
    max(pot, 1)
  if (tier == "Strong Made Hand") {
    if (
      stack > amount_to_call &&
      runif(1) < 0.30
    ) {
      return("raise")
    }
    return("call")
  }
  if (tier == "Medium Made Hand") {
    if (call_ratio <= 0.35 &&
        runif(1) < 0.72) {
      return("call")
    }
    if (call_ratio <= 0.65 &&
        runif(1) < 0.48) {
      return("call")
    }
    return("fold")
  }
  if (tier == "Weak Made Hand") {
    if (
      call_ratio <= 0.30 &&
      runif(1) < 0.50
    ) {
      return("call")
    }
    return("fold")
  }
  if (tier == "Draw") {
    if (
      call_ratio <= 0.50 &&
      runif(1) < 0.65
    ) {
      return("call")
    }
    if (
      stack > amount_to_call &&
      runif(1) < 0.20
    ) {
      return("raise")
    }
    return("fold")
  }
  if (
    call_ratio <= 0.20 &&
    runif(1) < 0.15
  ) {
    return("call")
  }
  "fold"
}

# ============================================================
# GAME STATE FUNCTIONS
# ============================================================

total_pot <- function(state) {
  sum(unlist(state$committed))
}

current_bet_to_call <- function(
    state,
    player
) {
  opponent <- ifelse(player == HUMAN, BOT, HUMAN)
  max(0, state$street_bets[[opponent]] - state$street_bets[[player]])
}

minimum_raise_to <- function(
    state,
    player
) {
  current_max <- max(state$street_bets[[HUMAN]], state$street_bets[[BOT]])
  current_max +
    state$last_raise_size
}

put_chips <- function(
    state,
    player,
    amount
) {
  amount <- min(max(amount, 0), state$stacks[[player]])
  state$stacks[[player]] <-
    state$stacks[[player]] - amount
  state$street_bets[[player]] <-
    state$street_bets[[player]] + amount
  state$committed[[player]] <-
    state$committed[[player]] + amount
}

log_message <- function(
    state,
    message
) {
  state$log <- c(state$log, paste0("Hand ", state$hand_number, " — ", message))
}

deal_one <- function(state) {
  card <- state$deck[1]
  state$deck <- state$deck[-1]
  card
}

deal_flop <- function(state) {
  
  # Burn
  state$deck <- state$deck[-1]
  state$board <- c(state$board, state$deck[1:3])
  state$deck <- state$deck[-c(1:3)]
  state$street <- "Flop"
  state$street_bets <- list(Human = 0, Bot = 0)
  state$last_raise_size <- BB
}

deal_turn <- function(state) {
  state$deck <- state$deck[-1]
  state$board <- c(state$board, state$deck[1])
  state$deck <- state$deck[-1]
  state$street <- "Turn"
  state$street_bets <- list(Human = 0, Bot = 0)
  state$last_raise_size <- BB
}

deal_river <- function(state) {
  state$deck <- state$deck[-1]
  state$board <- c(state$board, state$deck[1])
  state$deck <- state$deck[-1]
  state$street <- "River"
  state$street_bets <- list(Human = 0, Bot = 0)
  state$last_raise_size <- BB
}

# ============================================================
# STREET TRANSITION
# ============================================================

advance_street <- function(state) {
  
  # All-in: deal remaining board immediately.
  if (
    state$stacks[[HUMAN]] == 0 ||
    state$stacks[[BOT]] == 0
  ) {
    while (length(state$board) < 5) {
      if (length(state$board) == 0) {
        deal_flop(state)
      } else if (length(state$board) == 3) {
        deal_turn(state)
      } else if (length(state$board) == 4) {
        deal_river(state)
      }
    }
    
    # Neither player has another betting decision after an all-in
    # call. Complete the runout AND settle the hand now. Leaving
    # hand_over FALSE and to_act NULL caused the empty-length error.
    resolve_showdown(state)
    return(invisible(NULL))
  }
  if (state$street == "Pre-flop") {
    deal_flop(state)
  } else if (state$street == "Flop") {
    deal_turn(state)
  } else if (state$street == "Turn") {
    deal_river(state)
  } else {
    # If this helper is reached on the river, finish the hand
    # instead of leaving a live game with nobody to act.
    resolve_showdown(state)
    return(invisible(NULL))
  }
  
  # Big blind acts first post-flop.
  state$to_act <- ifelse(state$button == HUMAN, BOT, HUMAN)
  state$actions_this_street <- 0
}

# ============================================================
# SHOWDOWN / FOLD
# ============================================================

resolve_showdown <- function(state) {
  if (state$hand_over) {
    return()
  }
  
  # Heads-up only: when a shorter stack calls all-in for less than
  # the full raise, any uncalled excess belongs to the covering
  # player. It must not be awarded through the showdown pot.
  if (state$stacks[[HUMAN]] == 0 || state$stacks[[BOT]] == 0) {
    contributions <- c(Human = state$committed[[HUMAN]], Bot = state$committed[[BOT]])
    excess <- abs(contributions[[HUMAN]] - contributions[[BOT]])
    if (excess > 0) {
      covering_player <- if (contributions[[HUMAN]] > contributions[[BOT]]) HUMAN else BOT
      state$committed[[covering_player]] <- state$committed[[covering_player]] - excess
      state$stacks[[covering_player]] <- state$stacks[[covering_player]] + excess
      log_message(state, paste0("Uncalled $", excess, " returned to ", covering_player, "."))
    }
  }
  human_rank <- evaluate_seven(c(state$human_cards, state$board))
  bot_rank <- evaluate_seven(c(state$bot_cards, state$board))
  comparison <- compare_hand_rank(human_rank, bot_rank)
  pot <- total_pot(state)
  if (comparison > 0) {
    state$stacks[[HUMAN]] <-
      state$stacks[[HUMAN]] + pot
    state$winner <- HUMAN
  } else if (comparison < 0) {
    state$stacks[[BOT]] <-
      state$stacks[[BOT]] + pot
    state$winner <- BOT
  } else {
    half <- floor(pot / 2)
    state$stacks[[HUMAN]] <-
      state$stacks[[HUMAN]] + half
    state$stacks[[BOT]] <-
      state$stacks[[BOT]] + pot - half
    state$winner <- "Tie"
  }
  state$showdown <- TRUE
  state$hand_over <- TRUE
  state$to_act <- NULL
}

resolve_fold <- function(
    state,
    folder
) {
  winner <- ifelse(folder == HUMAN, BOT, HUMAN)
  pot <- total_pot(state)
  state$stacks[[winner]] <-
    state$stacks[[winner]] + pot
  state$winner <- winner
  state$fold_winner <- winner
  state$hand_over <- TRUE
  state$showdown <- FALSE
  state$to_act <- NULL
}

# ============================================================
# INITIALIZE HAND
# ============================================================

initialize_hand <- function(state) {
  state$hand_number <-
    state$hand_number + 1
  # Alternate button.
  state$button <- ifelse(state$hand_number %% 2 == 1, HUMAN, BOT)
  state$deck <- shuffle_deck(create_deck())
  state$board <- character(0)
  state$human_cards <- character(0)
  state$bot_cards <- character(0)
  state$street <- "Pre-flop"
  state$committed <- list(Human = 0, Bot = 0)
  state$street_bets <- list(Human = 0, Bot = 0)
  state$last_raise_size <- BB
  state$actions_this_street <- 0
  state$hand_over <- FALSE
  state$showdown <- FALSE
  state$winner <- NULL
  state$fold_winner <- NULL
  # ----------------------------------------------------------
  # Post blinds
  # ----------------------------------------------------------
  if (state$button == HUMAN) {
    put_chips(state, HUMAN, SB)
    put_chips(state, BOT, BB)
    
    # SB/button acts first pre-flop.
    state$to_act <- HUMAN
  } else {
    put_chips(state, BOT, SB)
    put_chips(state, HUMAN, BB)
    state$to_act <- BOT
  }
  
  # ----------------------------------------------------------
  # Deal four hole cards, alternating.
  # ----------------------------------------------------------
  for (i in 1:2) {
    state$human_cards <- c(state$human_cards, deal_one(state))
    state$bot_cards <- c(state$bot_cards, deal_one(state))
  }
}

# ============================================================
# EXECUTE ACTION
#
# This function is the central state-transition function.
# After a human action, the server schedules bot_take_turn()
# with a five-second pause whenever it becomes the bot's turn.
# ============================================================

execute_action <- function(
    state,
    player,
    action,
    raise_to = NULL
) {
  if (isTRUE(state$hand_over) || !identical(state$to_act, player)) {
    return(FALSE)
  }
  opponent <- ifelse(player == HUMAN, BOT, HUMAN)
  to_call <- current_bet_to_call(state, player)
  
  # Count only VALID actions. Previously, an invalid check or raise
  # still incremented this counter and could end a street early.
  # ----------------------------------------------------------
  # Fold
  # ----------------------------------------------------------
  if (action == "fold") {
    log_message(state, paste0(player, " folds."))
    resolve_fold(state, player)
    return(TRUE)
  }
  
  # ----------------------------------------------------------
  # Check
  # ----------------------------------------------------------
  if (action == "check") {
    if (to_call > 0) {
      return(FALSE)
    }
    state$actions_this_street <- state$actions_this_street + 1L
    log_message(state, paste0(player, " checks."))
    if (state$actions_this_street >= 2) {
      if (state$street == "River") {
        resolve_showdown(state)
      } else {
        advance_street(state)
      }
    } else {
      state$to_act <- opponent
    }
    return(TRUE)
  }
  
  # ----------------------------------------------------------
  # Call
  # ----------------------------------------------------------
  if (action == "call") {
    if (to_call <= 0) {
      return(FALSE)
    }
    state$actions_this_street <- state$actions_this_street + 1L
    amount <- min(to_call, state$stacks[[player]])
    put_chips(state, player, amount)
    log_message(state, paste0(player, " calls $", format(amount, nsmall = 0), "."))
    
    # Heads-up pre-flop exception: when the button/SB limps, the
    # BB still has the option to check or raise. Matching the blind
    # does NOT immediately deal the flop unless a player is all-in.
    if (identical(state$street, "Pre-flop") &&
        identical(player, state$button) &&
        state$actions_this_street == 1L &&
        state$stacks[[HUMAN]] > 0 &&
        state$stacks[[BOT]] > 0) {
      state$to_act <- opponent
      return(TRUE)
    }
    
    # Other calls close the betting round when bets are matched,
    # or when the caller has gone all-in (finish the runout).
    if (
      state$street_bets[[HUMAN]] ==
      state$street_bets[[BOT]] ||
      state$stacks[[player]] == 0
    ) {
      if (state$street == "River") {
        resolve_showdown(state)
      } else {
        advance_street(state)
      }
    } else {
      state$to_act <- opponent
    }
    return(TRUE)
  }
  
  # ----------------------------------------------------------
  # Raise
  # ----------------------------------------------------------
  if (action == "raise") {
    # In heads-up play, there is nobody left to raise after the
    # opponent is all-in. The only live choices are call/fold.
    if (state$stacks[[opponent]] <= 0) return(FALSE)
    current_max <- max(state$street_bets[[HUMAN]], state$street_bets[[BOT]])
    max_raise_to <-
      state$street_bets[[player]] +
      state$stacks[[player]]
    min_raise_to <-
      current_max +
      state$last_raise_size
    if (is.null(raise_to)) {
      raise_to <- min_raise_to
    }
    if (length(raise_to) != 1L || !is.numeric(raise_to) ||
        !is.finite(raise_to)) return(FALSE)
    raise_to <- min(raise_to, max_raise_to)
    
    # Permit an all-in even when it is below the
    # formal minimum raise.
    if (
      raise_to < min_raise_to &&
      raise_to < max_raise_to
    ) {
      return(FALSE)
    }
    amount <-
      raise_to -
      state$street_bets[[player]]
    if (amount <= to_call) {
      return(FALSE)
    }
    old_max <- current_max
    put_chips(state, player, amount)
    new_max <-
      state$street_bets[[player]]
    state$last_raise_size <-
      max(BB, new_max - old_max)
    log_message(state, paste0(player, " raises to $", format(new_max, nsmall = 0), "."))
    
    # Raise reopens action for opponent.
    state$actions_this_street <- 1
    state$to_act <- opponent
    return(TRUE)
  }
  FALSE
}

# ============================================================
# BOT TURN
# ============================================================

bot_fallback_action <- function(state, to_call, pot, stack) {
  
  # This is the bot's emergency decision rule. It is deliberately
  # simple and deterministic: if the normal decision process ever
  # exceeds the 15-second limit, the bot immediately uses this rule.
  if (state$street == "Pre-flop") {
    strength <- preflop_strength(state$bot_cards)
    if (to_call == 0) {
      if (strength >= 0.65) {
        return("raise")
      }
      return("check")
    }
    if (strength >= 0.82) {
      return("raise")
    }
    if (strength >= 0.50) {
      return("call")
    }
    return("fold")
  }
  tier <- classify_postflop(state$bot_cards, state$board)
  if (to_call == 0) {
    if (tier == "Strong Made Hand") {
      return("raise")
    }
    if (tier == "Draw") {
      return("raise")
    }
    if (tier == "Medium Made Hand") {
      return("check")
    }
    return("check")
  }
  if (tier == "Strong Made Hand") {
    return("raise")
  }
  if (tier == "Medium Made Hand") {
    if (to_call <= pot * 0.50) {
      return("call")
    }
    return("fold")
  }
  if (tier == "Draw") {
    if (to_call <= pot * 0.50) {
      return("call")
    }
    return("fold")
  }
  "fold"
}

# ============================================================
# BOT RAISE SIZING
# ============================================================

round_to_dollar <- function(x) {
  floor(x + 0.5)
}

choose_preflop_raise_to <- function(state) {
  current_max <- max(state$street_bets[[HUMAN]], state$street_bets[[BOT]])
  max_raise_to <-
    state$street_bets[[BOT]] +
    state$stacks[[BOT]]
  min_raise_to <-
    current_max + state$last_raise_size
  # Pre-flop raises are expressed as an integer multiple of the
  # $2 big blind: 2.5 BB, 3 BB, 4 BB, 5 BB, etc.
  # Choose only sizes that are legal in the current spot.
  min_multiplier <- max(2.5, ceiling((min_raise_to / BB) * 2) / 2)
  max_multiplier <-
    floor((max_raise_to / BB) * 2) / 2
  # A short stack can have no legal standard-size raise. Avoid a
  # descending seq(..., by = +0.5), which otherwise throws an error.
  if (max_multiplier < min_multiplier) {
    return(round_to_dollar(max_raise_to))
  }
  candidates <- seq(min_multiplier, max_multiplier, by = 0.5)
  
  # Bias toward conventional heads-up opening / re-raising sizes,
  # while still allowing larger sizes when facing a larger raise.
  preferred <- candidates[
    candidates %in% c(2.5, 3, 4, 5, 6)
  ]
  # Index by sample.int(): sample(5, 1) incorrectly samples from 1:5
  # when there is only one numeric candidate, which can yield an illegal size.
  multiplier <- if (length(preferred) > 0) {
    preferred[sample.int(length(preferred), 1L)]
  } else {
    candidates[sample.int(length(candidates), 1L)]
  }
  round_to_dollar(min(multiplier * BB, max_raise_to))
}

choose_postflop_raise_to <- function(state) {
  pot <- total_pot(state)
  current_max <- max(state$street_bets[[HUMAN]], state$street_bets[[BOT]])
  max_raise_to <-
    state$street_bets[[BOT]] +
    state$stacks[[BOT]]
  min_raise_to <-
    current_max + state$last_raise_size
  # Post-flop sizing menu.
  # 1 = pot-sized bet/raise.
  sizing_options <- c(quarter = 0.25, third   = 1 / 3, half    = 0.50, pot     = 1.00)
  chosen_name <- sample(names(sizing_options), 1)
  fraction <- sizing_options[[chosen_name]]
  target <- current_max + fraction * pot
  target <- round_to_dollar(target)
  
  # All-in is always an explicit available sizing.
  if (runif(1) < 0.12) {
    return(round_to_dollar(max_raise_to))
  }
  
  # Keep the selected sizing legal.
  if (max_raise_to <= min_raise_to) {
    return(round_to_dollar(max_raise_to))
  }
  if (target < min_raise_to) {
    target <- ceiling(min_raise_to)
  }
  target <- min(target, max_raise_to)
  round_to_dollar(target)
}

# ============================================================
# BOT TURN
# ============================================================

# One bot decision per invocation. The server waits five seconds before
# each decision, including a pre-flop fold and consecutive-street actions.
bot_take_turn <- function(state) {
  if (state$hand_over || is.null(state$to_act) ||
      !identical(state$to_act, BOT)) return(invisible(FALSE))
  to_call <- current_bet_to_call(state, BOT)
  pot <- total_pot(state)
  stack <- state$stacks[[BOT]]
  if (state$street == "Pre-flop") {
    action <- bot_preflop_action(state$bot_cards, to_call, stack)
  } else {
    action <- bot_postflop_action(state$bot_cards, state$board, to_call, pot, stack)
  }
  if (identical(action, "raise")) {
    raise_to <- if (state$street == "Pre-flop") {
      choose_preflop_raise_to(state)
    } else {
      choose_postflop_raise_to(state)
    }
    success <- execute_action(state, BOT, "raise", raise_to)
  } else {
    success <- execute_action(state, BOT, action)
  }
  
  # Never leave the bot's turn stuck on an invalid raise/action.
  if (!isTRUE(success) && !state$hand_over &&
      identical(state$to_act, BOT)) {
    fallback <- if (to_call > 0) "call" else "check"
    success <- execute_action(state, BOT, fallback)
  }
  if (!isTRUE(success) && !state$hand_over &&
      identical(state$to_act, BOT)) {
    success <- execute_action(state, BOT, "fold")
  }
  invisible(success)
}

# ============================================================
# PRE-FLOP REFERENCE CHARTS
# Simplified heads-up strategy reference.
# These are not solver-exact GTO ranges.
# ============================================================

preflop_grid_action <- function(row_i, col_j, position) {
  ranks <- c("A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2")
  rv <- c(
    A = 14, K = 13, Q = 12, J = 11, T = 10,
    `9` = 9, `8` = 8, `7` = 7, `6` = 6,
    `5` = 5, `4` = 4, `3` = 3, `2` = 2
  )
  r1 <- rv[[ranks[row_i]]]
  r2 <- rv[[ranks[col_j]]]
  high <- max(r1, r2)
  low <- min(r1, r2)
  if (row_i == col_j) {
    type <- "pair"
  } else if (row_i < col_j) {
    type <- "suited"
  } else {
    type <- "offsuit"
  }
  score <-
    high * 4 +
    low * 1.5 +
    ifelse(type == "suited", 11, 0) +
    ifelse(type == "pair", 32, 0) +
    ifelse(high == 14, 8, 0) +
    ifelse(high - low == 1, 6, 0) +
    ifelse(high - low == 2, 3, 0)
  if (position == "SB") {
    # Heads-up SB/button opens very wide.
    if (
      type == "pair" ||
      score >= 62 ||
      (high == 14 && low >= 5)
    ) {
      if (
        score >= 78 ||
        (type == "suited" && high >= 10)
      ) {
        return("R")
      }
      return("M")
    }
    if (score >= 49) {
      return("M")
    }
    return("F")
  }
  
  # BB response to an SB/button open.
  strong_3bet <-
    (type == "pair" && high >= 10) ||
    (high == 14 && low >= 11) ||
    (type == "suited" && high == 14 && low >= 8) ||
    (type == "suited" && high == 13 && low >= 11)
  if (strong_3bet) {
    return("3B")
  }
  if (score >= 55) {
    return("C")
  }
  if (score >= 46) {
    return("M")
  }
  "F"
}

make_preflop_matrix <- function(position) {
  ranks <- c("A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2")
  out <- matrix("", nrow = length(ranks), ncol = length(ranks), dimnames = list(ranks, ranks))
  for (i in seq_along(ranks)) {
    for (j in seq_along(ranks)) {
      out[i, j] <- preflop_grid_action(i, j, position)
    }
  }
  out
}

SB_PRE_FLOP <- make_preflop_matrix("SB")
BB_PRE_FLOP <- make_preflop_matrix("BB")

preflop_legend_item <- function(css_class, label) {
  span(class = "legend-item",
       span(class = paste("legend-swatch", css_class), `aria-hidden` = "true"),
       span(label))
}

preflop_chart_html <- function(mat) {
  ranks <- rownames(mat)
  header <- paste0("<tr><th></th>", paste0("<th>", ranks, "</th>", collapse = ""), "</tr>")
  rows <- vapply(
    seq_along(ranks),
    function(i) {
      cells <- vapply(
        seq_along(ranks),
        function(j) {
          action <- mat[i, j]
          class_name <- switch(
            action,
            "R" = "pf-open",
            "3B" = "pf-3bet",
            "C" = "pf-call",
            "M" = "pf-mix",
            "F" = "pf-fold"
          )
          action_label <- switch(
            action,
            "R" = "Open / Raise",
            "3B" = "3-Bet",
            "C" = "Call / Defend",
            "M" = "Mix / Frequency-dependent",
            "F" = "Fold"
          )
          # Upper-right: suited; lower-left: offsuit; diagonal: pairs.
          # Always put the higher rank first (98s / 98o, not 89s / 89o).
          hand <- if (i == j) {
            paste0(ranks[i], ranks[i])
          } else if (i < j) {
            paste0(ranks[i], ranks[j], "s")
          } else {
            paste0(ranks[j], ranks[i], "o")
          }
          hand_type <- if (i == j) "pocket pair" else if (i < j) "suited" else "offsuit"
          paste0("<td class='", class_name, "' title='", hand, " (", hand_type,
                 "): ", action_label, "'>", hand, "</td>")
        },
        character(1)
      )
      paste0("<tr><th>", ranks[i], "</th>", paste0(cells, collapse = ""), "</tr>")
    },
    character(1)
  )
  HTML(
    paste0(
      "<table class='preflop-grid'>",
      "<thead>",
      header,
      "</thead>",
      "<tbody>",
      paste0(rows, collapse = ""),
      "</tbody>",
      "</table>"
    )
  )
}

# ============================================================
# POST-FLOP ANALYSIS LAB
# Exact runout enumeration + opponent-range equity simulation.
# This module is educational: it is not a GTO solver.
# ============================================================

LAB_CATEGORY_NAMES <- c(
  "High card", "One pair", "Two pair", "Three of a kind",
  "Straight", "Flush", "Full house", "Four of a kind",
  "Straight flush"
)
LAB_RANKS <- c("2"=2L,"3"=3L,"4"=4L,"5"=5L,"6"=6L,"7"=7L,
               "8"=8L,"9"=9L,"T"=10L,"J"=11L,"Q"=12L,"K"=13L,"A"=14L)
LAB_DECK <- create_deck()
LAB_CARD_CHOICES <- c(
  "— Select a card —"="",
  setNames(LAB_DECK, vapply(LAB_DECK, card_label, character(1)))
)

# Evaluates any 5–7 cards. Produces a fixed six-number vector:
# category (0–8), then lexicographically sorted tie-break values.
# Faster than checking all 21 possible five-card subsets for every trial.
lab_straight_high <- function(values) {
  values <- unique(values)
  if (14L %in% values) values <- c(1L, values)
  for (h in 14L:5L) {
    if (all(seq.int(h - 4L, h) %in% values)) return(as.integer(h))
  }
  0L
}

lab_rank <- function(cards) {
  v <- as.integer(unname(LAB_RANKS[substr(cards, 1L, 1L)]))
  suits <- substr(cards, 2L, 2L)
  ct <- tabulate(v, nbins=14L)
  present <- which(ct > 0L)
  pack <- function(category, tb) {
    as.integer(c(category, head(c(tb, rep(0L, 5L)), 5L)))
  }
  suit_counts <- table(suits)
  fs <- names(suit_counts)[suit_counts >= 5L]
  if (length(fs) > 0L) {
    flush_values <- sort(v[suits == fs[1L]], decreasing=TRUE)
    sf <- lab_straight_high(flush_values)
    if (sf > 0L) return(pack(8L, sf))
  }
  quads <- sort(which(ct == 4L), decreasing=TRUE)
  if (length(quads) > 0L)
    return(pack(7L, c(quads[1L], max(present[present != quads[1L]]))))
  trips <- sort(which(ct >= 3L), decreasing=TRUE)
  if (length(trips) > 0L) {
    pair_pool <- sort(setdiff(which(ct >= 2L), trips[1L]), decreasing=TRUE)
    if (length(pair_pool) > 0L) return(pack(6L, c(trips[1L], pair_pool[1L])))
  }
  if (length(fs) > 0L) return(pack(5L, head(flush_values, 5L)))
  st <- lab_straight_high(present)
  if (st > 0L) return(pack(4L, st))
  if (length(trips) > 0L) {
    kickers <- head(sort(setdiff(present, trips[1L]), decreasing=TRUE), 2L)
    return(pack(3L, c(trips[1L], kickers)))
  }
  pairs <- sort(which(ct >= 2L), decreasing=TRUE)
  if (length(pairs) >= 2L) {
    k <- max(setdiff(present, pairs[1L:2L]))
    return(pack(2L, c(pairs[1L:2L], k)))
  }
  if (length(pairs) == 1L) {
    k <- head(sort(setdiff(present, pairs[1L]), decreasing=TRUE), 3L)
    return(pack(1L, c(pairs[1L], k)))
  }
  pack(0L, head(sort(present, decreasing=TRUE), 5L))
}

lab_compare <- function(a, b) {
  for (i in seq_len(6L)) {
    if (a[i] > b[i]) return(1L)
    if (a[i] < b[i]) return(-1L)
  }
  0L
}

lab_rank_name <- function(rank) {
  if (rank[1L] == 8L && rank[2L] == 14L) return("Royal flush")
  LAB_CATEGORY_NAMES[rank[1L] + 1L]
}

# Count final BEST-hand categories over every possible board runout.
# If the opponent's exact cards are known, exclude both blockers.
lab_exact_runouts <- function(hero, board, villain=character(0)) {
  remaining <- setdiff(LAB_DECK, c(hero, board, villain))
  missing <- 5L - length(board)
  if (missing == 0L) {
    categories <- lab_rank(c(hero, board))[1L]
  } else if (missing == 1L) {
    categories <- vapply(remaining, function(x) {
      lab_rank(c(hero, board, x))[1L]
    }, integer(1))
  } else {
    runouts <- combn(remaining, 2L)
    categories <- vapply(seq_len(ncol(runouts)), function(i) {
      lab_rank(c(hero, board, runouts[,i]))[1L]
    }, integer(1))
  }
  counts <- tabulate(categories + 1L, nbins=9L)
  setNames(counts / sum(counts), LAB_CATEGORY_NAMES)
}

# Uniformly select legal two-card combos from a transparent, *heuristic*
# pre-flop range. These thresholds are not solver-generated frequencies.
lab_opponent_combos <- function(remaining, style) {
  combos <- combn(remaining, 2L)
  if (style == "random") return(combos)
  cutoff <- if (style == "tight") 0.65 else 0.38
  keep <- vapply(seq_len(ncol(combos)), function(i) {
    preflop_strength(combos[, i]) >= cutoff
  }, logical(1))
  combos[, keep, drop=FALSE]
}

lab_equity <- function(hero, board, style, villain=character(0), n=750L) {
  missing <- 5L - length(board)
  dead <- c(hero, board)
  remaining <- setdiff(LAB_DECK, dead)
  is_exact <- length(villain) == 2L
  if (is_exact) {
    # Exact villain: enumerate ALL legal board completions (<= 990).
    runout_deck <- setdiff(remaining, villain)
    if (missing == 0L) {
      runouts <- matrix(character(0), nrow=0L, ncol=1L)
    } else if (missing == 1L) {
      runouts <- matrix(runout_deck, nrow=1L)
    } else {
      runouts <- combn(runout_deck, 2L)
    }
    n_eff <- ncol(runouts)
    exact <- TRUE
  } else {
    combos <- lab_opponent_combos(remaining, style)
    if (ncol(combos) == 0L) stop("No opponent combos remain for this range.")
    # On the river enumerate every remaining opponent combo exactly.
    exact <- (missing == 0L)
    n_eff <- if (exact) ncol(combos) else as.integer(n)
  }
  wins <- 0L
  ties <- 0L
  hero_river <- if (missing == 0L) lab_rank(c(hero, board)) else NULL
  for (i in seq_len(n_eff)) {
    if (is_exact) {
      opp <- villain
      extra <- if (missing > 0L) runouts[,i] else character(0)
    } else {
      opp <- combos[, if (exact) i else sample.int(ncol(combos), 1L)]
      extra <- if (missing > 0L) {
        sample(setdiff(remaining, opp), missing, replace=FALSE)
      } else {
        character(0)
      }
    }
    final_board <- c(board, extra)
    h <- if (missing == 0L) hero_river else lab_rank(c(hero, final_board))
    o <- lab_rank(c(opp, final_board))
    comparison <- lab_compare(h, o)
    if (comparison == 1L) wins <- wins + 1L
    if (comparison == 0L) ties <- ties + 1L
  }
  list(
    win=wins/n_eff, tie=ties/n_eff, loss=(n_eff-wins-ties)/n_eff,
    equity=(wins + ties/2)/n_eff,
    n=n_eff, exact=exact
  )
}

lab_immediate_improvement <- function(hero, board, villain=character(0)) {
  if (length(board) == 5L) return(NULL)
  current <- lab_rank(c(hero, board))[1L]
  unseen <- setdiff(LAB_DECK, c(hero, board, villain))
  next_cat <- vapply(unseen, function(card) {
    lab_rank(c(hero, board, card))[1L]
  }, integer(1))
  list(n=sum(next_cat > current), total=length(unseen),
       p=mean(next_cat > current), current=LAB_CATEGORY_NAMES[current + 1L])
}

lab_texture <- function(board) {
  r <- as.integer(unname(LAB_RANKS[substr(board,1L,1L)]))
  suits <- substr(board,2L,2L)
  features <- character(0)
  if (any(tabulate(r,nbins=14L) >= 2L)) features <- c(features,"paired")
  if (max(table(suits)) >= 3L) {
    features <- c(features,"three or more of a suit")
  } else if (max(table(suits)) == 2L) {
    features <- c(features,"two-tone")
  } else {
    features <- c(features,"rainbow")
  }
  if (lab_straight_high(r) > 0L || any(vapply(5L:14L, function(h) {
    sum(seq.int(h-4L,h) %in% unique(c(r, if (14L %in% r) 1L))) >= 3L
  }, logical(1)))) features <- c(features,"connected")
  paste(features, collapse=", ")
}

lab_hand_features <- function(hero, board) {
  rank <- lab_rank(c(hero,board))
  cat <- rank[1L]
  suits <- substr(c(hero,board),2L,2L)
  flush_draw <- length(board) < 5L && any(table(suits) == 4L)
  v <- as.integer(unname(LAB_RANKS[substr(c(hero,board),1L,1L)]))
  possible_straight <- length(board) < 5L && lab_straight_high(v)==0L &&
    any(vapply(2L:14L, function(x) lab_straight_high(c(v,x)) > 0L, logical(1)))
  if (cat >= 3L) {
    tier <- "Strong made hand"
  } else if (cat == 2L) {
    tier <- "Medium made hand"
  } else if (cat == 1L) {
    tier <- "Pair"
  } else if (flush_draw || possible_straight) {
    tier <- "Draw"
  } else {
    tier <- "Unmade hand"
  }
  draws <- c(if (flush_draw) "four-card flush draw" else NULL,
             if (possible_straight) "one-card straight possibility" else NULL)
  list(rank=rank, tier=tier, draws=draws)
}

lab_strategy_text <- function(street, hero, board, pos, facing, pot, bet, eq) {
  info <- lab_hand_features(hero, board)
  cat <- info$rank[1L]
  texture <- lab_texture(board)
  wet <- grepl("connected|two-tone|three or more", texture)
  street_note <- switch(street,
                        "Flop"="Flop: think about range advantage, nut advantage and which turn cards change the board.",
                        "Turn"="Turn: reassess the turn card and how much of each range continues to the river.",
                        "River"="River: no draws remain; think in terms of thin value bets, bluff-catchers, and blocker effects.")
  if (facing == "check") {
    baseline <- if (cat >= 3L) {
      if (wet) {
        "Consider value betting, including larger sizes when worse made hands and draws can continue. Also protect some strong hands in checking ranges."
      } else {
        "Consider value betting at a small-to-medium size on this board, while keeping some strong hands in a checking range."
      }
    } else if (cat == 2L || cat == 1L) {
      "Checking can protect medium-strength hands and avoid inflating the pot; smaller value/protection bets may also fit some ranges."
    } else if (length(info$draws) > 0L) {
      "A draw may sometimes be semi-bluffed and sometimes checked. Consider fold equity, board coverage, and whether your draw has showdown value."
    } else {
      "Check often with unmade holdings. Selective bluffs need appropriate blockers, range advantage, and credible value hands."
    }
  } else {
    baseline <- if (cat >= 3L) {
      "Strong made hands can often continue, but the exact mix of calling and raising depends on board, sizing, and the opponent's value range."
    } else if (cat >= 1L) {
      "Compare the opponent's represented range to your made hand; some pairs bluff-catch, while vulnerable pairs may fold to larger bets."
    } else if (length(info$draws) > 0L) {
      "Compare your draw equity and implied odds to the price. Some draws can call or semi-bluff; avoid assuming every visible draw has clean winning outs."
    } else {
      "An unmade hand generally needs credible bluff-catching value or a carefully selected bluff-raise; folding is often relevant."
    }
  }
  pos_text <- if (pos == "ip") {
    "You are in position post-flop: you observe the opponent's action first."
  } else {
    "You are out of position post-flop: consider protecting your checking range."
  }
  if (facing == "bet" && bet > 0L) {
    pot_odds <- bet/(pot+2*bet)
    mdf <- pot/(pot+bet)
    comparison <- if (eq >= pot_odds) {
      "Modeled showdown equity is above the raw pot-odds threshold."
    } else {
      "Modeled showdown equity is below the raw pot-odds threshold."
    }
    math <- sprintf("Facing $%d into a $%d pre-bet pot: call threshold %.1f%%; minimum defense frequency %.1f%% (range-level concept). %s This alone does NOT identify the GTO action or account for later betting.",
                    round(bet), round(pot), 100*pot_odds, 100*mdf, comparison)
  } else {
    sizes <- pmin(round(pot * c(.25,1/3,.5,.75,1)), 999999L)
    math <- sprintf("Illustrative sizes for a $%d pot: quarter $%d · third $%d · half $%d · three-quarter $%d · pot $%d. These are options, not solver frequencies.",
                    round(pot), sizes[1],sizes[2],sizes[3],sizes[4],sizes[5])
  }
  draws <- if (length(info$draws)) paste(info$draws,collapse="; ") else "none detected"
  list(street=street_note, baseline=baseline, position=pos_text,
       math=math, feature=sprintf("%s · %s · Draw indicators: %s",lab_rank_name(info$rank),texture,draws))
}

# Clear beginner-facing prompts. These are study prompts, not solver actions.
lab_beginner_guidance <- function(street, hero, board, facing, pos) {
  info <- lab_hand_features(hero, board)
  category <- info$rank[1L]
  has_draw <- length(info$draws) > 0L
  if (facing == "bet") {
    if (category >= 3L) {
      headline <- "Strong made hand: call or raise?"
      explanation <- paste(
        "Compare the size of the bet with the hands your opponent could have.",
        "Calling often keeps weaker hands involved; raising builds a bigger pot",
        "but can make weaker hands fold. A strong hand is not unbeatable."
      )
    } else if (category >= 1L) {
      headline <- "Pair or two pair: assess bluff-catching value"
      explanation <- paste(
        "Think about what your opponent would bet with.",
        "A smaller bet is easier to consider calling than a large bet,",
        "especially if your hand is vulnerable."
      )
    } else if (has_draw) {
      headline <- "Drawing hand: compare equity with pot odds"
      explanation <- paste(
        "Some future cards may complete your draw, but those cards do not",
        "always win the hand. Compare the cost of calling with your chance",
        "of finishing a hand that beats your opponent."
      )
    } else {
      headline <- "Unmade hand: is continuing justified?"
      explanation <- paste(
        "Ask whether you have a specific reason to continue.",
        "Without one, folding is often a sensible option."
      )
    }
  } else {
    if (category >= 3L) {
      headline <- "Strong made hand: identify your value-bet targets"
      explanation <- paste(
        "A value bet aims to get called by weaker hands.",
        "If weaker hands rarely call here, checking can make more sense.",
        "Also consider whether the strongest hand comes mostly from the board."
      )
    } else if (category >= 1L) {
      headline <- "Pair or two pair: value bet or pot control?"
      explanation <- paste(
        "Checking can control the pot with a medium-strength hand.",
        "A small bet may also make sense when weaker hands will call."
      )
    } else if (has_draw) {
      headline <- "Drawing hand: semi-bluff or check?"
      explanation <- paste(
        "Betting a draw can win now if your opponent folds, or later",
        "if your draw completes. Checking keeps the pot smaller",
        "and may let you see another card more cheaply."
      )
    } else {
      headline <- "Unmade hand: check or select a bluff?"
      explanation <- paste(
        "Checking is a straightforward starting point.",
        "A bluff needs a reason to believe your opponent might fold."
      )
    }
  }
  street_tip <- switch(street,
                       "Flop"="Two more community cards can come. Think about which turn cards help you or your opponent.",
                       "Turn"="Just one card remains. Ask which river cards could change who is ahead.",
                       "River"="There are no more cards to come. Focus on what weaker hands might call or stronger hands might fold."
  )
  position_tip <- if (pos == "ip") {
    "You are in position: you act after your opponent, so you get to see their action first."
  } else {
    "You are out of position: you usually act first on each street. Checking sometimes lets you see how your opponent responds before putting in more money."
  }
  board_terms <- strsplit(lab_texture(board), ", ", fixed=TRUE)[[1L]]
  replacements <- c(
    "paired"="paired",
    "two-tone"="two-tone",
    "rainbow"="rainbow",
    "three or more of a suit"="3+ cards of one suit",
    "connected"="connected"
  )
  board_easy <- paste(unname(replacements[board_terms]), collapse="; ")
  draw_easy <- character(0)
  if ("four-card flush draw" %in% info$draws)
    draw_easy <- c(draw_easy, "flush draw")
  if ("one-card straight possibility" %in% info$draws)
    draw_easy <- c(draw_easy, "possible straight draw")
  list(headline=headline, explanation=explanation, street_tip=street_tip,
       position_tip=position_tip, board=board_easy,
       draws=if (length(draw_easy)) paste(draw_easy,collapse=" + ") else "None detected")
}

# Repeated tab layout: the three streets have separate inputs and results.
lab_street_tab <- function(street, n_board, prefix) {
  default_hero <- c("Ah","Qh")
  default_board <- c("Kh","Jh","2c","5d","9s")
  # Selectize custom renderers color HEARTS and DIAMONDS in both the
  # open menu and the selected value; actual returned values remain Ah, 2d, etc.
  suit_renderer <- I('{
    option: function(item, escape) {
      var red = /[dh]$/.test(item.value || "");
      var color = red ? "#c62828" : "#182d26";
      return `<div class="lab-suit-option" style="color:${color}">` +
        escape(item.label) + `</div>`;
    },
    item: function(item, escape) {
      var red = /[dh]$/.test(item.value || "");
      var color = red ? "#c62828" : "#182d26";
      return `<div class="lab-suit-item" style="color:${color}">` +
        escape(item.label) + `</div>`;
    }
  }')
  select_card <- function(id, title, value) {
    div(class="lab-suit-select",
        selectizeInput(paste0(prefix,"_",id), title, LAB_CARD_CHOICES,
                       selected=value, width="100%",
                       options=list(render=suit_renderer, maxOptions=60, create=FALSE)))
  }
  board_inputs <- lapply(seq_len(n_board), function(i) {
    div(class="lab-card-select",
        select_card(paste0("board",i),paste("Board",i),default_board[i]))
  })
  tabPanel(street,
           fluidPage(class="preflop-page",
                     div(class="preflop-card",
                         h3(paste0("♠ ",street," Strategy & Equity Lab")),
                         p("Pick your cards, then see which hands you could make by the river and ",
                           "estimate your chance of winning vs. one opponent."),
                         div(class="preflop-note",
                             strong("Strategy accuracy: "),
                             "The probabilities are calculated, but the betting advice is educational ",
                             "and GTO-inspired, not a solver's exact answer.")),
                     fluidRow(
                       column(4,
                              div(class="preflop-card",
                                  h4("1 · Set up the situation"),
                                  actionButton(paste0(prefix,"_load"),"Use cards from Practice Game",
                                               class="btn-default",width="100%"),
                                  helpText("Copies your visible hole cards and this street's board from the game, if available."),
                                  fluidRow(column(6,select_card("hero1","Your card 1",default_hero[1])),
                                           column(6,select_card("hero2","Your card 2",default_hero[2]))),
                                  h4("Community cards"),
                                  div(class="lab-card-grid", board_inputs),
                                  tags$hr(),
                                  selectInput(paste0(prefix,"_villain"),"Opponent model",
                                              c("Any two cards (random)"="random",
                                                "Tight heuristic range"="tight",
                                                "Wide heuristic range"="loose",
                                                "Exact opponent hand"="exact"),selected="random"),
                                  conditionalPanel(
                                    condition=sprintf("input.%s_villain == 'exact'",prefix),
                                    fluidRow(column(6,select_card("opp1","Opponent card 1","")),
                                             column(6,select_card("opp2","Opponent card 2","")))
                                  ),
                                  tags$details(class="lab-more-details",
                                               tags$summary("How opponent models work"),
                                               p("Tight and wide are simplified estimates based on pre-flop hand strength; ",
                                                 "they do not model a real opponent's decisions. Every possible two-card ",
                                                 "combination in the chosen range is treated equally.")),
                                  selectInput(paste0(prefix,"_n"),"Monte Carlo trials (when needed)",
                                              c("300 · quick"=300,"750 · standard"=750,"1500 · detailed"=1500),
                                              selected=750),
                                  actionButton(paste0(prefix,"_run"),"Calculate probabilities",
                                               class="btn-success btn-lg",width="100%"),
                                  br(), br(),
                                  uiOutput(paste0(prefix,"_validation"))
                              ),
                              div(class="preflop-card",
                                  h4("2 · Betting scenario"),
                                  selectInput(paste0(prefix,"_pos"),"Your post-flop position",
                                              c("In position (button / SB)"="ip", "Out of position (BB)"="oop")),
                                  selectInput(paste0(prefix,"_facing"),"Action you face",
                                              c("Checked to you / first to act"="check", "Opponent bets"="bet")),
                                  numericInput(paste0(prefix,"_pot"),"Pot BEFORE opponent bets ($)",
                                               value=3,min=1,step=1),
                                  conditionalPanel(
                                    condition=sprintf("input.%s_facing == 'bet'",prefix),
                                    numericInput(paste0(prefix,"_bet"),"Opponent's bet ($)",
                                                 value=10,min=1,step=1)
                                  ),
                                  numericInput(paste0(prefix,"_stack"),"Effective stack remaining ($)",
                                               value=199,min=1,step=1),
                                  helpText("Click Calculate probabilities again after changing inputs.")
                              )
                       ),
                       column(8,
                              div(class="preflop-card",
                                  h4("3 · Equity and exact hand probabilities"),
                                  uiOutput(paste0(prefix,"_results")),
                                  plotOutput(paste0(prefix,"_distribution"),height="295px")
                              ),
                              div(class="preflop-card",
                                  h4("4 · Post-flop strategy"),
                                  uiOutput(paste0(prefix,"_strategy"))
                              )
                       )
                     ),
                     div(class="preflop-card",
                         h4(paste0(street," · Opponent adjustments & alternatives")),
                         p(switch(street,
                                  "Flop"="Start by asking who is more likely to have connected with these three cards. Both players can have strong hands, weak hands, and draws.",
                                  "Turn"="Reassess your hand after the fourth card. One more community card can change everything, so think about your river plan.",
                                  "River"="Your hand cannot improve now. Your choice comes down to whether a weaker hand calls a bet, a stronger hand folds, or you want to reach showdown.")),
                         fluidRow(
                           column(4,div(class="lab-alt-card",
                                        h5("GTO-inspired baseline"),
                                        p("Mix betting and checking so your actions do not always reveal how strong your hand is."))),
                           column(4,div(class="lab-alt-card",
                                        h5("vs. a passive opponent"),
                                        p("Against a calling station, value bet strong and some medium-strength hands when worse hands will call. Reduce low-equity bluffs because passive opponents tend to fold less often."))),
                           column(4,div(class="lab-alt-card",
                                        h5("vs. an aggressive opponent"),
                                        p("Against a frequent bluffer, defend with suitable bluff-catchers instead of folding automatically to pressure. You can trap with some strong hands; consider blockers and bet sizing when choosing a call or raise.")))
                         ),
                         tags$details(class="lab-more-details",
                                      tags$summary("Important terms and limitations"),
                                      p(strong("Range:")," the different hands an opponent might have, not just one guessed hand."),
                                      p(strong("Value bet:")," a bet you hope a weaker hand will call."),
                                      p(strong("Bluff:")," a bet you hope a stronger hand will fold to."),
                                      p("The percentages assume a showdown without future betting. They do not account for folds, future bets, rake, or exact GTO strategy. A card that improves your hand does not necessarily give you the winning hand.")
                         )
                     )
           )
  )
}

# ============================================================
# UI
# ============================================================

ui <- navbarPage(
  title = "Texas Hold'em: Heads-Up",
  header = tags$head(
    tags$script(HTML("
      Shiny.addCustomMessageHandler('poker_action_controls', function(msg) {
        ['fold', 'check', 'call', 'raise', 'raise_amount'].forEach(function(id) {
          var control = document.getElementById(id);
          if (control) control.disabled = !!msg.disabled;
        });
      });
    ")),
    tags$style(HTML("
      body {
        background-color: #f5f7fa;
      }
      .title-panel {
        background: #202938;
        color: white;
        padding: 18px 24px;
        border-radius: 10px;
        margin-bottom: 18px;
      }
      .table-panel {
        background: #075e35;
        border-radius: 18px;
        padding: 30px;
        color: white;
        box-shadow: 0 4px 16px rgba(0,0,0,0.18);
      }
      .card {
        display: inline-block;
        background: white;
        color: black;
        border-radius: 8px;
        width: 62px;
        height: 86px;
        margin: 4px;
        padding-top: 18px;
        text-align: center;
        font-size: 25px;
        font-weight: bold;
        box-shadow: 0 2px 5px rgba(0,0,0,0.25);
      }
      .card-red {
        color: #c62828;
      }
      .stat-box,
      .action-panel {
        background: white;
        border-radius: 10px;
        padding: 15px;
        margin-bottom: 12px;
        box-shadow: 0 1px 5px rgba(0,0,0,0.08);
      }
      .log-box {
        background: #111827;
        color: #d1d5db;
        border-radius: 8px;
        padding: 12px;
        height: 260px;
        overflow-y: auto;
        font-family: monospace;
        font-size: 13px;
      }
      .winner-box {
        background: #fff3cd;
        color: #664d03;
        border: 1px solid #ffecb5;
        border-radius: 8px;
        padding: 15px;
        margin-top: 15px;
      }
      .bot-card {
        background: #374151;
        color: white;
      }
      .table-player-label {
        display: flex;
        justify-content: center;
        align-items: center;
        gap: 9px;
        flex-wrap: wrap;
        margin-bottom: 6px;
      }
      .table-player-label h4 {
        margin: 0;
        font-weight: 800;
      }
      /* Show the current street at left and the pot centered above the cards. */
      .board-heading {
        position: relative;
        display: flex;
        align-items: center;
        justify-content: center;
        min-height: 36px;
        margin-top: 4px;
        margin-bottom: 7px;
      }
      .board-cards .card:last-child {
        animation: board-card-reveal 0.32s ease-out;
      }
      @keyframes board-card-reveal {
        from { opacity: 0; transform: translateY(-8px) rotateY(55deg); }
        to { opacity: 1; transform: translateY(0) rotateY(0); }
      }
      .board-street {
        position: absolute;
        left: 4px;
        top: 50%;
        transform: translateY(-50%);
        font-size: 22px;
        font-weight: 800;
        color: #fff0bf;
      }
      .board-pot {
        font-size: 22px;
        font-weight: 850;
        color: #fff0bf;
        font-variant-numeric: tabular-nums;
      }
      .action-badge {
        display: inline-flex;
        align-items: center;
        border-radius: 6px;
        background: #ffcf57;
        color: #30240a;
        border: 2px solid #fff0b5;
        box-shadow: 0 0 0 3px rgba(255, 208, 82, 0.35);
        font-size: 13px;
        font-weight: 900;
        letter-spacing: 0.09em;
        padding: 6px 11px;
      }
      .table-player-label .shiny-html-output { display: inline-flex; align-items: center; gap: 7px; }
      .action-label-active { outline: 2px solid #ffcf57; }
      .thinking-indicator {
        color: #fff4c9;
        font-size: 12px;
        font-weight: 650;
      }
      .thinking-indicator::after {
        content: '';
        animation: thinking-dots 1.25s steps(4, end) infinite;
      }
      @keyframes thinking-dots {
        0% { content: ''; }
        25% { content: '.'; }
        50% { content: '..'; }
        75%, 100% { content: '...'; }
      }
      .table-stack-inline {
        font-size: 16px;
        font-weight: 700;
        opacity: 0.95;
      }
      .blind-badge {
        display: inline-block;
        padding: 3px 8px;
        border-radius: 999px;
        background: rgba(255,255,255,0.95);
        color: #184d35;
        font-size: 11px;
        font-weight: 900;
        letter-spacing: 0.5px;
      }
      .preflop-page {
        padding: 24px;
      }
      .preflop-card {
        background: white;
        border-radius: 10px;
        padding: 18px;
        margin-bottom: 18px;
        box-shadow: 0 1px 6px rgba(0,0,0,0.08);
      }
      .preflop-card h3,
      .preflop-card h4 {
        margin-top: 0;
        font-weight: 800;
      }
      .preflop-scroll {
        overflow-x: auto;
      }
      .preflop-grid {
        border-collapse: separate;
        border-spacing: 2px;
        min-width: 620px;
      }
      .preflop-grid th {
        font-size: 12px;
        font-weight: 800;
        text-align: center;
        padding: 5px;
      }
      .preflop-grid td {
        width: 42px;
        height: 35px;
        text-align: center;
        font-size: 12px;
        font-weight: 900;
        border-radius: 3px;
        cursor: help;
      }
      .pf-open {
        background: #b9e8c1;
        color: #184e24;
      }
      .pf-3bet {
        background: #f2c66d;
        color: #684500;
      }
      .pf-call {
        background: #bfdaf4;
        color: #123f67;
      }
      .pf-mix {
        background: #ddd5f1;
        color: #4b3c70;
      }
      .pf-fold {
        background: #eeeeee;
        color: #777777;
      }
      .preflop-legend {
        display: flex;
        gap: 8px 14px;
        flex-wrap: wrap;
        margin-top: 12px;
        font-size: 12px;
        font-weight: 650;
      }
      .preflop-legend .legend-item {
        display: inline-flex;
        align-items: center;
        gap: 6px;
      }
      .preflop-legend .legend-swatch {
        display: inline-block;
        width: 17px;
        height: 17px;
        border: 1px solid rgba(0, 0, 0, 0.13);
        border-radius: 4px;
        flex-shrink: 0;
      }
      .preflop-chart-note {
        margin: 8px 0 0;
        color: #4b5563;
        font-size: 12px;
        line-height: 1.5;
      }
      .preflop-note {
        background: #fff8e6;
        border-left: 4px solid #d9a300;
        border-radius: 6px;
        padding: 12px;
        margin-top: 14px;
        font-size: 13px;
      }
      .preflop-list {
        margin-bottom: 0;
      }
      /* Lab input cards wrap rather than shrinking to unreadable widths. */
      .lab-card-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(95px, 1fr));
        gap: 6px 10px;
      }
      .lab-card-grid .form-group { margin-bottom: 8px; }
      /* Card suit colors also apply after a card has been selected. */
      .lab-suit-select .selectize-input { min-height: 35px; }
      .lab-suit-select .selectize-dropdown-content { max-height: 230px; }
      .lab-suit-option, .lab-suit-item { font-weight: 750; }
      .lab-summary-callout {
        background: #eaf4ed;
        border-left: 5px solid #278351;
        border-radius: 8px;
        padding: 15px 16px;
        margin-bottom: 16px;
      }
      .lab-summary-callout h4 { margin: 7px 0; font-size: 19px; }
      .lab-summary-callout p { margin-bottom: 0; line-height: 1.5; }
      .lab-eyebrow {
        font-size: 11px;
        text-transform: uppercase;
        letter-spacing: 1px;
        font-weight: 800;
        color: #246342;
      }
      .lab-guide-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(190px, 1fr));
        gap: 12px;
        margin-bottom: 14px;
      }
      .lab-guide-box {
        border: 1px solid #e3e7eb;
        border-radius: 8px;
        padding: 13px;
      }
      .lab-guide-box h5, .lab-math-card h5 {
        margin: 0 0 9px;
        font-size: 14px;
        font-weight: 800;
      }
      .lab-guide-box p { margin-bottom: 7px; line-height: 1.5; }
      .lab-math-card {
        background: #f4f7fb;
        border: 1px solid #e0e9f1;
        padding: 13px;
        border-radius: 8px;
        margin-bottom: 12px;
      }
      .lab-size-menu {
        display: flex;
        flex-wrap: wrap;
        gap: 8px;
        margin: 10px 0;
      }
      .lab-size-chip {
        flex: 1 0 104px;
        text-align: center;
        background: white;
        padding: 8px 6px;
        border: 1px solid #dbe4ee;
        border-radius: 8px;
      }
      .lab-size-chip strong { display: block; font-size: 16px; }
      .lab-size-chip small { display: block; color: #566879; }
      .lab-outcomes { display: flex; gap: 6px; margin-top: 9px; }
      .lab-outcome { flex: 1; min-width: 0; background: white; border: 1px solid #dee7ef;
                     border-radius: 7px; text-align: center; padding: 6px 3px; }
      .lab-outcome span { display: block; font-size: 10px; font-weight: 800;
                          letter-spacing: .7px; color: #526577; }
      .lab-outcome strong { display: block; font-size: 17px; margin-top: 3px; color: #173b54; }
      .lab-odds-number { font-size: 24px; font-weight: 800; color: #184e37; margin: 4px 0; }
      .lab-more-details { border-top: 1px solid #e3e7eb; padding: 12px 0 3px; }
      .lab-more-details summary { cursor: pointer; font-weight: 750; color: #285f52; }
      .lab-more-details p { margin-top: 10px; line-height: 1.5; }
      .lab-alt-card {
        padding: 13px;
        border: 1px solid #e3e7eb;
        background: #fafcfd;
        border-radius: 8px;
        margin-bottom: 12px;
        min-height: 140px;
      }
      .lab-alt-card h5 { font-weight: 800; margin: 0 0 7px; }
      .lab-alt-card p { margin: 0; }
      @media (max-width: 991px) {
        .lab-card-grid { grid-template-columns: repeat(auto-fit, minmax(100px, 1fr)); }
      }
    "))
  ),
  
  # ==========================================================
  # TAB 1: Practice GAME
  # ==========================================================
  
  tabPanel(
    "Practice Game",
    fluidPage(
      div(
        class = "title-panel",
        h2("♠ Heads-up vs. a GTO-Inspired Bot", 
           style = "margin-top:0;"),
        p("Stop trying to exploit the bot. This is where degens learn some probability.",
          style = "margin-bottom:0;"
        )
      ),
      fluidRow(
        
        # ========================================================
        # LEFT SIDE
        # ========================================================
        
        column(
          3,
          div(
            class = "stat-box",
            h4("Game"),
            textOutput("hand_number"),
            textOutput("button"),
            textOutput("street"),
            textOutput("pot"),
            tags$hr(),
            h4("Stacks"),
            textOutput("blinds"),
            textOutput("human_stack"),
            textOutput("bot_stack")
          ),
          # Keep the highlighted result and Next Hand on the left,
          # but show whose turn it is beside the players at the table.
          uiOutput("result_ui"),
          div(class = "stat-box", h4("Game Log"), uiOutput("game_log"))
        ),
        
        # ========================================================
        # TABLE / ACTIONS
        # ========================================================
        
        column(
          9,
          div(
            class = "table-panel",
            div(
              style = "text-align:center;",
              div(
                class = "table-player-label",
                uiOutput("bot_action_badge", inline = TRUE),
                h4("Bot Hand"),
                span(
                  class = "table-stack-inline",
                  textOutput("bot_table_stack", inline = TRUE)
                ),
                uiOutput("bot_blind_badge", inline = TRUE)
              ),
              uiOutput("bot_hand_ui"),
              br(),
              div(
                class = "board-heading",
                span(class = "board-street", textOutput("board_table_street", inline = TRUE)),
                span(class = "board-pot", textOutput("board_table_pot", inline = TRUE))
              ),
              uiOutput("board_ui"),
              br(),
              div(
                class = "table-player-label",
                uiOutput("human_action_badge", inline = TRUE),
                h4("Your Hand"),
                span(
                  class = "table-stack-inline",
                  textOutput("human_table_stack", inline = TRUE)
                ),
                uiOutput("human_blind_badge", inline = TRUE)
              ),
              uiOutput("human_hand_ui")
            )
          ),
          div(
            class = "action-panel",
            h4("Your Action"),
            fluidRow(
              column(
                3,
                actionButton("fold", "Fold", class = "btn-danger btn-lg", width = "100%")
              ),
              column(
                3,
                actionButton("check", "Check", class = "btn-secondary btn-lg", width = "100%")
              ),
              column(
                3,
                actionButton("call", "Call", class = "btn-primary btn-lg", width = "100%")
              ),
              column(
                3,
                actionButton("raise", "Raise", class = "btn-success btn-lg", width = "100%")
              )
            ),
            br(),
            sliderInput(
              "raise_amount",
              "Raise To ($)",
              min = BB,
              max = STARTING_STACK,
              value = 10,
              step = 1
            ),
            uiOutput("action_help"),
            br(),
            actionButton("next_hand", "Next Hand", class = "btn-dark btn-lg", width = "100%")
          )
        )
      )
    )
  ),
  
  # ==========================================================
  # TAB 2: PRE-FLOP
  # ==========================================================
  
  tabPanel(
    "Pre-Flop",
    fluidPage(
      class = "preflop-page",
      div(
        class = "preflop-card",
        h3("Heads-Up Pre-Flop Strategy"),
        p(
          "In heads-up No-Limit Hold'em, the button is also the Small Blind. ",
          "The SB/button acts first before the flop and last on every post-flop street."
        ),
        p(
          "Because there is only one opponent, opening and defending ranges are much wider ",
          "than they would be at a full table."
        ),
        div(
          class = "preflop-note",
          strong("Important: "),
          "The charts below are simplified GTO-inspired/common-strategy reference charts ",
          "for this application. They are not solver-exact equilibrium ranges. Exact ranges ",
          "depend on effective stack depth, raise sizes, rake, antes, and the specific game tree."
        )
      ),
      fluidRow(
        column(
          6,
          div(
            class = "preflop-card",
            h4("SB / Button — Open Range"),
            p(
              "The SB is first to act pre-flop. In heads-up play, the button can open a very wide range."
            ),
            div(class = "preflop-scroll", preflop_chart_html(SB_PRE_FLOP)),
            div(class = "preflop-legend",
                preflop_legend_item("pf-open", "Open / Raise"),
                preflop_legend_item("pf-mix", "Mix / Frequency-dependent"),
                preflop_legend_item("pf-fold", "Fold")),
            p(class = "preflop-chart-note",
              "Above diagonal: suited (98s) · Below diagonal: offsuit (98o) · ",
              "Diagonal: pocket pairs (99). Hover over a hand to see its action.")
          )
        ),
        column(
          6,
          div(
            class = "preflop-card",
            h4("BB — Defend vs. SB Open"),
            p(
              "The BB has already posted $2 and is defending against only one opponent, ",
              "so calling ranges can be quite wide. Stronger holdings can also be used for 3-bets."
            ),
            div(class = "preflop-scroll", preflop_chart_html(BB_PRE_FLOP)),
            div(class = "preflop-legend",
                preflop_legend_item("pf-3bet", "3-Bet"),
                preflop_legend_item("pf-call", "Call / Defend"),
                preflop_legend_item("pf-mix", "Mix"),
                preflop_legend_item("pf-fold", "Fold")),
            p(class = "preflop-chart-note",
              "Above diagonal: suited (98s) · Below diagonal: offsuit (98o) · ",
              "Diagonal: pocket pairs (99). Hover over a hand to see its action.")
          )
        )
      ),
      div(
        class = "preflop-card",
        h4("What to Know About Heads-Up Pre-Flop Play"),
        tags$ul(
          class = "preflop-list",
          tags$li(
            strong("Position matters: "),
            "the SB/button has to act first pre-flop but has position after the flop."
          ),
          tags$li(
            strong("Ranges are wide: "),
            "there is only one opponent, so hands such as weak aces, kings, suited connectors, ",
            "and many pairs can have meaningful pre-flop value."
          ),
          tags$li(
            strong("BB defense is wide: "),
            "the BB gets a favorable price because it has already invested the big blind."
          ),
          tags$li(
            strong("3-bets are part of both value and bluff ranges: "),
            "a balanced strategy does not reserve 3-bets exclusively for premium hands."
          ),
          tags$li(
            strong("Sizing changes the range: "),
            "a larger SB open generally gives the BB a worse price and can change which hands continue."
          ),
          tags$li(
            strong("Stack depth matters: "),
            "as effective stacks become shorter, more hands approach commitment decisions and ",
            "pre-flop all-ins become more relevant."
          ),
          tags$li(
            strong("Think in frequencies: "),
            "some borderline hands can mix between raising, calling, and folding rather than always taking one action."
          ),
          tags$li(
            strong("Do not evaluate a decision only by the outcome: "),
            "short-term poker results contain substantial variance, so a sound pre-flop decision can still lose the hand."
          )
        )
      )
    )
  ),
  # Independent calculators for each post-flop street.
  lab_street_tab("Flop", 3L, "flop"),
  lab_street_tab("Turn", 4L, "turn"),
  lab_street_tab("River", 5L, "river")
)

# ============================================================
# BOARD REVEAL TIMING
# Each flop card appears separately; longer pauses separate streets.
# ============================================================

board_reveal_delay <- function(card_number) {
  if (card_number == 1L) return(0.35)  # First flop card
  if (card_number %in% c(2L, 3L)) return(0.40)
  if (card_number %in% c(4L, 5L)) return(1.35)  # Turn / river
  stop("Board card index must be between 1 and 5.")
}

board_reveal_street <- function(number_shown) {
  if (number_shown == 0L) return("Pre-flop")
  if (number_shown <= 3L) return("Flop")
  if (number_shown == 4L) return("Turn")
  "River"
}

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  state <- reactiveValues(
    hand_number = 0,
    button = HUMAN,
    deck = character(0),
    human_cards = character(0),
    bot_cards = character(0),
    board = character(0),
    street = "Pre-flop",
    stacks = list(Human = STARTING_STACK, Bot = STARTING_STACK),
    committed = list(Human = 0, Bot = 0),
    street_bets = list(Human = 0, Bot = 0),
    last_raise_size = BB,
    to_act = HUMAN,
    actions_this_street = 0,
    hand_over = FALSE,
    showdown = FALSE,
    winner = NULL,
    fold_winner = NULL,
    log = character(0)
  )
  
  # ==========================================================
  # BOT THINKING TIMER
  # ==========================================================
  # later runs the callback without freezing the Shiny session. Each
  # decision receives its own five-second pause, even after a street
  # transition when the bot also acts first on the next street.
  bot_turn_pending <- FALSE
  bot_turn_token <- 0L
  bot_is_thinking <- shiny::reactiveVal(FALSE)
  
  # Game logic deals/settles immediately; only the visible board is
  # animated. This avoids altering betting/showdown rules for animation.
  # Hold results and the next decision until the displayed runout finishes.
  shown_board <- shiny::reactiveVal(character(0))
  shown_street <- shiny::reactiveVal("Pre-flop")
  reveal_in_progress <- shiny::reactiveVal(FALSE)
  reveal_token <- 0L
  sync_board_reveal <- function() {
    shiny::isolate({
      target <- state$board
      visible <- shown_board()
      if (identical(target, visible)) return(invisible(FALSE))
      if (length(target) == 0L) {
        reveal_token <<- reveal_token + 1L
        shown_board(character(0))
        shown_street("Pre-flop")
        reveal_in_progress(FALSE)
        return(invisible(TRUE))
      }
      # Normally the displayed cards are a prefix of the logical board.
      # An unexpected reset invalidates outstanding callbacks safely.
      if (length(visible) > length(target) ||
          !identical(visible, head(target, length(visible)))) {
        shown_board(character(0))
        shown_street("Pre-flop")
      }
      reveal_token <<- reveal_token + 1L
      this_token <- reveal_token
      this_hand <- state$hand_number
      reveal_in_progress(TRUE)
      finish_reveal <- function() {
        if (isTRUE(session$isClosed())) return(invisible(NULL))
        shiny::withReactiveDomain(session, shiny::isolate({
          if (!identical(this_token, reveal_token) ||
              !identical(this_hand, state$hand_number)) return(invisible(NULL))
          reveal_in_progress(FALSE)
          # If the bot acts first on the new street, its five-second
          # thinking timer begins AFTER the community cards appear.
          if (!isTRUE(state$hand_over) && identical(state$to_act, BOT)) {
            schedule_bot_turn()
          }
        }))
      }
      reveal_one <- function() {
        if (isTRUE(session$isClosed())) return(invisible(NULL))
        shiny::withReactiveDomain(session, shiny::isolate({
          if (!identical(this_token, reveal_token) ||
              !identical(this_hand, state$hand_number)) return(invisible(NULL))
          next_card <- length(shown_board()) + 1L
          shown_board(target[seq_len(next_card)])
          shown_street(board_reveal_street(next_card))
          if (next_card < length(target)) {
            later::later(reveal_one, delay = board_reveal_delay(next_card + 1L))
          } else {
            # Give the final card a moment before revealing hole cards
            # or showing a settled all-in result.
            later::later(finish_reveal, delay = 0.70)
          }
        }))
      }
      next_card <- length(shown_board()) + 1L
      later::later(reveal_one, delay = board_reveal_delay(next_card))
      invisible(TRUE)
    })
  }
  schedule_bot_turn <- function() {
    shiny::isolate({
      if (bot_turn_pending || isTRUE(reveal_in_progress()) ||
          state$hand_over || !identical(state$to_act, BOT))
        return(invisible(FALSE))
      bot_turn_pending <<- TRUE
      bot_is_thinking(TRUE)
      bot_turn_token <<- bot_turn_token + 1L
      this_token <- bot_turn_token
      this_hand <- state$hand_number
      later::later(function() {
        if (isTRUE(session$isClosed())) return(invisible(NULL))
        shiny::withReactiveDomain(session, {
          shiny::isolate({
            if (!identical(this_token, bot_turn_token))
              return(invisible(NULL))
            # Turn off the OLD thinking indicator before changing the
            # game state; after a street transition there may be a NEW
            # bot decision, which receives its own clearly labeled timer.
            bot_turn_pending <<- FALSE
            bot_is_thinking(FALSE)
            if (state$hand_over || !identical(state$to_act, BOT) ||
                !identical(state$hand_number, this_hand))
              return(invisible(NULL))
            bot_take_turn(state)
            # Queue every newly dealt community card. If the board
            # advanced, sync_board_reveal() will schedule the next bot
            # decision after the reveal; otherwise keep the short reset.
            sync_board_reveal()
            if (!isTRUE(reveal_in_progress()) && !isTRUE(state$hand_over) &&
                identical(state$to_act, BOT)) {
              later::later(function() {
                if (isTRUE(session$isClosed())) return(invisible(NULL))
                shiny::withReactiveDomain(session, shiny::isolate({
                  if (identical(this_token, bot_turn_token) &&
                      identical(this_hand, state$hand_number) &&
                      !isTRUE(state$hand_over) &&
                      identical(state$to_act, BOT)) schedule_bot_turn()
                }))
              }, delay = 0.4)
            }
          })
        })
      }, delay = 5)
      invisible(TRUE)
    })
  }
  session$onSessionEnded(function() {
    bot_turn_token <<- bot_turn_token + 1L
    reveal_token <<- reveal_token + 1L
    bot_turn_pending <<- FALSE
    shiny::isolate({
      bot_is_thinking(FALSE)
      reveal_in_progress(FALSE)
    })
  })
  
  # ==========================================================
  # START FIRST HAND
  # ==========================================================
  
  observe({
    if (state$hand_number == 0) {
      initialize_hand(state)
      log_message(
        state,
        paste0(ifelse(state$button == HUMAN, "You", "Bot"), " have the button.")
      )
      log_message(state, "Blinds posted: $1 / $2.")
      
      # Important: if Bot has the button, Bot is SB and
      # acts first pre-flop.
      if (identical(state$to_act, BOT)) {
        schedule_bot_turn()
      }
    }
  })
  
  # ==========================================================
  # HUMAN ACTION HANDLER
  #
  # Every human action ends here. If it becomes the bot's turn,
  # show the ACTION badge and schedule its decision in five seconds.
  # ==========================================================
  
  handle_human_action <- function(
    action,
    raise_to = NULL
  ) {
    if (isTRUE(reveal_in_progress()) || isTRUE(state$hand_over) ||
        !identical(state$to_act, HUMAN)) return(invisible(FALSE))
    success <- execute_action(state, HUMAN, action, raise_to)
    if (!isTRUE(success)) return(invisible(FALSE))
    sync_board_reveal()
    
    # Centralized bot response.
    #
    # This is the important fix for the bot failing to act
    # after a flop/turn/river bet.
    if (
      !isTRUE(reveal_in_progress()) && !isTRUE(state$hand_over) &&
      identical(state$to_act, BOT)
    ) {
      schedule_bot_turn()
    }
  }
  observeEvent(
    input$fold,
    {
      handle_human_action("fold")
    },
    ignoreInit = TRUE
  )
  observeEvent(
    input$check,
    {
      handle_human_action("check")
    },
    ignoreInit = TRUE
  )
  observeEvent(
    input$call,
    {
      handle_human_action("call")
    },
    ignoreInit = TRUE
  )
  observeEvent(
    input$raise,
    {
      handle_human_action("raise", input$raise_amount)
    },
    ignoreInit = TRUE
  )
  
  # ==========================================================
  # NEXT HAND
  # ==========================================================
  
  start_next_hand <- function() {
    if (!isTRUE(state$hand_over) || isTRUE(reveal_in_progress())) return()
    # Invalidate any callbacks from the completed hand.
    reveal_token <<- reveal_token + 1L
    shown_board(character(0))
    shown_street("Pre-flop")
    
    # Reset match if someone has busted.
    if (
      state$stacks[[HUMAN]] <= 0 ||
      state$stacks[[BOT]] <= 0
    ) {
      state$stacks <- list(Human = STARTING_STACK, Bot = STARTING_STACK)
      log_message(state, "A player ran out of chips. Stacks reset to $200.")
    }
    initialize_hand(state)
    log_message(
      state,
      paste0("New hand. ", ifelse(state$button == HUMAN, "You", "Bot"), " have the button.")
    )
    log_message(state, "Blinds posted: $1 / $2.")
    if (identical(state$to_act, BOT)) {
      schedule_bot_turn()
    }
  }
  observeEvent(
    input$next_hand,
    {
      start_next_hand()
    },
    ignoreInit = TRUE
  )
  observeEvent(
    input$next_hand_result,
    {
      start_next_hand()
    },
    ignoreInit = TRUE
  )
  
  # ==========================================================
  # FLOP / TURN / RIVER LAB SERVER
  # ==========================================================
  for (spec in list(list(id="flop",n=3L),list(id="turn",n=4L), list(id="river",n=5L))) local({
    prefix <- spec$id
    board_n <- spec$n
    field <- function(suffix) input[[paste0(prefix,"_",suffix)]]
    # Synchronize from the practice game only on an explicit button press.
    observeEvent(field("load"), {
      if (length(state$board) < board_n || length(state$human_cards) != 2L) {
        showNotification(paste0("Play through the ",prefix,
                                " in Practice Game first."),type="warning")
        return()
      }
      updateSelectizeInput(session,paste0(prefix,"_hero1"),selected=state$human_cards[1])
      updateSelectizeInput(session,paste0(prefix,"_hero2"),selected=state$human_cards[2])
      for (i in seq_len(board_n)) {
        updateSelectizeInput(session,paste0(prefix,"_board",i),selected=state$board[i])
      }
      showNotification("Practice Game cards copied. Click Calculate probabilities.",
                       type="message")
    },ignoreInit=TRUE)
    validated_input <- reactive({
      hero <- c(field("hero1"),field("hero2"))
      board <- vapply(seq_len(board_n), function(i) {
        value <- field(paste0("board",i))
        if (is.null(value)) "" else as.character(value)
      }, character(1))
      style <- field("villain")
      if (length(hero)!=2L || any(!nzchar(hero)) || any(!nzchar(board)))
        return(list(error="Choose your two cards and every community card."))
      opp <- if (identical(style,"exact")) {
        c(field("opp1"),field("opp2"))
      } else {
        character(0)
      }
      if (identical(style,"exact") && (length(opp)!=2L || any(!nzchar(opp))))
        return(list(error="Choose BOTH opponent cards for an exact-hand analysis."))
      used <- c(hero,board,opp)
      if (anyDuplicated(used))
        return(list(error="The same card cannot appear twice. Change the highlighted scenario."))
      if (!all(used %in% LAB_DECK))
        return(list(error="Invalid card selection."))
      pot <- suppressWarnings(as.numeric(field("pot")))
      bet <- suppressWarnings(as.numeric(field("bet")))
      stack <- suppressWarnings(as.numeric(field("stack")))
      if (length(pot)!=1L || length(stack)!=1L ||
          !is.finite(pot) || pot <= 0 || !is.finite(stack) || stack <= 0)
        return(list(error="Pot and effective stack must be positive numbers."))
      facing <- field("facing")
      if (identical(facing,"bet") &&
          (length(bet)!=1L || !is.finite(bet) || bet <= 0))
        return(list(error="Enter a positive opponent bet."))
      if (identical(facing,"bet") && bet > stack)
        return(list(error="For this simplified heads-up scenario, opponent bet cannot exceed the effective stack."))
      list(hero=hero,board=board,opp=opp,style=style,pot=pot,
           bet=if (identical(facing,"bet")) bet else 0,
           stack=stack,facing=facing,pos=field("pos"),
           n=as.integer(field("n")))
    })
    output[[paste0(prefix,"_validation")]] <- renderUI({
      x <- validated_input()
      if (is.null(x$error)) return(NULL)
      div(class="preflop-note",style="border-left-color:#c62828;",
          strong("Fix your cards / scenario: "),x$error)
    })
    calculation <- eventReactive(field("run"), {
      x <- validated_input()
      if (!is.null(x$error)) return(list(error=x$error))
      tryCatch({
        # Exact hand-distribution enumeration is distinct from showdown equity.
        dist <- lab_exact_runouts(x$hero,x$board,x$opp)
        improved <- lab_immediate_improvement(x$hero,x$board,x$opp)
        eq <- lab_equity(x$hero,x$board,x$style,x$opp,x$n)
        list(input=x, dist=dist, improved=improved, equity=eq,
             hand=lab_hand_features(x$hero,x$board))
      },error=function(e) list(error=paste("Calculation error:",conditionMessage(e))))
    },ignoreInit=TRUE)
    output[[paste0(prefix,"_results")]] <- renderUI({
      r <- calculation()
      req(!is.null(r))
      if (!is.null(r$error))
        return(div(class="preflop-note",style="border-left-color:#c62828;",r$error))
      e <- r$equity
      d <- r$dist
      fmt <- function(p) sprintf("%.1f%%",100*p)
      uncertainty <- if (e$exact) "Exact enumeration" else "Monte Carlo estimate"
      extra <- if (e$exact) NULL else
        paste0("Monte Carlo percentages fluctuate with each run. ",
               "Run more trials for greater precision.")
      next_info <- if (is.null(r$improved)) {
        "River: no cards remain to be dealt."
      } else {
        sprintf("Next-card category improvement: %d of %d unseen cards (%s). This is NOT the number of clean winning outs.",
                r$improved$n,r$improved$total,fmt(r$improved$p))
      }
      card_odds <- tags$div(
        tags$b("Exact river hand-category probabilities"),
        tags$p(sprintf("One pair: %s | Two pair: %s | Trips: %s | Straight: %s",
                       fmt(d[2]),fmt(d[3]),fmt(d[4]),fmt(d[5]))),
        tags$p(sprintf("Flush: %s | Full house: %s | Quads: %s | Straight flush: %s",
                       fmt(d[6]),fmt(d[7]),fmt(d[8]),fmt(d[9]))),
        tags$p(sprintf("Best hand is a straight/straight flush: %s · Flush/straight flush: %s",
                       fmt(d[5]+d[9]),fmt(d[6]+d[9]))),
        tags$p(sprintf("Full house or higher (hand ranking): %s",fmt(sum(d[7:9]))))
      )
      div(
        div(style="display:flex;gap:10px;flex-wrap:wrap;margin-bottom:14px;",
            div(style="flex:1;min-width:130px;border-radius:9px;background:#e2f2e7;padding:13px;",
                tags$b("Showdown equity"),
                h2(fmt(e$equity),style="margin:5px 0;color:#14633a;")),
            div(style="flex:1.3;min-width:220px;border-radius:9px;background:#eff4fa;padding:13px;",
                tags$b("Showdown probabilities"),
                div(class="lab-outcomes",
                    div(class="lab-outcome", span("WIN"), strong(fmt(e$win))),
                    div(class="lab-outcome", span("TIE"), strong(fmt(e$tie))),
                    div(class="lab-outcome", span("LOSS"), strong(fmt(e$loss)))))
        ),
        p(strong(uncertainty),sprintf("across %s legal %s.",
                                      format(e$n,big.mark=","),
                                      if (e$exact) "outcomes" else "trials")),
        p(strong("Current made hand: "),lab_rank_name(r$hand$rank)),
        p(next_info),
        card_odds,
        if (!is.null(extra)) tags$small(extra),
        tags$hr(),
        tags$small("Hand-category probabilities count your BEST five-card hand at the river; ",
                   "the straight-flush category includes royal flushes. ",
                   "For heuristic ranges, runout distribution ignores unknown opponent blockers; ",
                   "an exact opponent hand is accounted for.")
      )
    })
    output[[paste0(prefix,"_distribution")]] <- renderPlot({
      r <- calculation()
      req(!is.null(r), is.null(r$error))
      vals <- 100*r$dist
      old <- par(mar=c(8,4.5,2,1))
      on.exit(par(old),add=TRUE)
      barplot(vals, names.arg=names(vals), las=2, col="#6ba98b",
              border=NA, ylab="Exact probability (%)",cex.names=.78,
              main="Best hand by river",ylim=c(0,max(5,vals)*1.15))
    })
    output[[paste0(prefix,"_strategy")]] <- renderUI({
      r <- calculation()
      req(!is.null(r))
      if (!is.null(r$error)) return(NULL)
      x <- r$input
      street_name <- switch(prefix,flop="Flop",turn="Turn",river="River")
      st <- lab_strategy_text(street_name,x$hero,x$board,x$pos,
                              x$facing,x$pot,x$bet,r$equity$equity)
      easy <- lab_beginner_guidance(street_name,x$hero,x$board, x$facing,x$pos)
      fmt_money <- function(z) paste0("$",format(round(z),big.mark=","))
      
      # Dollar examples use the pot and effective stack entered in
      # panel 2, rounded to the nearest dollar and capped by the stack.
      if (x$facing == "check") {
        fractions <- c("1/4 pot"=.25, "1/3 pot"=1/3,
                       "1/2 pot"=.5, "3/4 pot"=.75, "Full pot"=1)
        amounts <- pmin(floor(x$pot * fractions + .5), floor(x$stack + .5))
        sizes_ui <- div(class="lab-math-card",
                        h5("Bet sizing examples"),
                        p("For your entered pot and effective stack (rounded to dollars):"),
                        div(class="lab-size-menu",
                            lapply(seq_along(fractions), function(i)
                              div(class="lab-size-chip",
                                  strong(fmt_money(amounts[i])),
                                  tags$small(names(fractions)[i]))),
                            div(class="lab-size-chip", strong(fmt_money(x$stack)), tags$small("All-in"))),
                        tags$small("These are illustrative bet sizes, not solver recommendations. Tiny pot fractions may round below the Practice Game's $2 minimum bet.")
        )
      } else {
        call_price <- x$bet/(x$pot+2*x$bet)
        mdf <- x$pot/(x$pot+x$bet)
        sizes_ui <- div(class="lab-math-card",
                        h5("Pot odds · Break-even equity"),
                        p(sprintf("Your opponent bets %s into a %s pot. Calling costs %s.",
                                  fmt_money(x$bet),fmt_money(x$pot),fmt_money(x$bet))),
                        p("The minimum equity required by pot odds, assuming no further betting:"),
                        div(class="lab-odds-number",sprintf("%.1f%%",100*call_price)),
                        p(sprintf("Your estimated showdown equity vs. the selected initial opponent range: %.1f%%.",
                                  100*r$equity$equity)),
                        tags$small("Important: an opponent who bets may have a different set of hands than the initial range you selected. This comparison does NOT tell you automatically to call or fold."))
      }
      div(
        div(class="lab-summary-callout",
            div(class="lab-eyebrow","Start here · Strategic consideration"),
            h4(easy$headline),
            p(easy$explanation)),
        div(class="lab-guide-grid",
            div(class="lab-guide-box",
                h5("1. Hand strength & board texture"),
                p(strong("Current best hand: "),lab_rank_name(r$hand$rank)),
                p(strong("Board texture: "),easy$board),
                p(strong("Draws: "),easy$draws)),
            div(class="lab-guide-box",
                h5("2. Position & street plan"),
                p(easy$street_tip),
                p(easy$position_tip))
        ),
        sizes_ui,
        tags$details(class="lab-more-details",
                     tags$summary("More strategy detail (optional)"),
                     p(strong("GTO-inspired reasoning: "),st$baseline),
                     p(strong("Why this street matters: "),st$street),
                     if (x$facing == "bet") {
                       p(sprintf("Advanced: minimum defense frequency for a %s bet into a %s pot is %.1f%%. This is about your ENTIRE range, not a requirement to call with this exact hand.",
                                 fmt_money(x$bet),fmt_money(x$pot),100*mdf))
                     } else {
                       p("A balanced strategy sometimes checks strong hands and sometimes bets draws; exact frequencies depend on both players' ranges and bet sizes.")
                     },
                     p(strong("Reminder: "),"These are study prompts, not solver-calculated GTO actions or frequencies."))
      )
    })
  })
  
  # ==========================================================
  # OUTPUTS
  # ==========================================================
  
  output$hand_number <- renderText({
    paste0("Hand: ", state$hand_number)
  })
  output$button <- renderText({
    paste0("Button: ", ifelse(state$button == HUMAN, "You", "Bot"))
  })
  output$street <- renderText({
    paste0("Street: ", shown_street())
  })
  output$pot <- renderText({
    paste0("Pot: $", format(total_pot(state), nsmall = 0))
  })
  output$board_table_pot <- renderText({
    paste0("$", format(total_pot(state), nsmall = 0))
  })
  output$board_table_street <- renderText({
    shown_street()
  })
  output$blinds <- renderText({
    "Blinds: $1/$2"
  })
  output$human_stack <- renderText({
    paste0("Your stack: $", format(state$stacks[[HUMAN]], nsmall = 0))
  })
  output$bot_stack <- renderText({
    paste0("Bot stack: $", format(state$stacks[[BOT]], nsmall = 0))
  })
  output$bot_table_stack <- renderText({
    paste0("$", format(state$stacks[[BOT]], nsmall = 0))
  })
  output$human_table_stack <- renderText({
    paste0("$", format(state$stacks[[HUMAN]], nsmall = 0))
  })
  output$bot_blind_badge <- renderUI({
    span(class = "blind-badge", ifelse(state$button == BOT, "SB", "BB"))
  })
  output$human_blind_badge <- renderUI({
    span(class = "blind-badge", ifelse(state$button == HUMAN, "SB", "BB"))
  })
  
  # Only the player who needs to act displays the ACTION badge.
  # Once a hand ends both badges disappear; the result appears at left.
  output$bot_action_badge <- renderUI({
    if (isTRUE(reveal_in_progress()) || isTRUE(state$hand_over) ||
        !identical(state$to_act, BOT)) return(NULL)
    tagList(
      span(class="action-badge", "ACTION"),
      if (isTRUE(bot_is_thinking()))
        span(class="thinking-indicator", paste0("Thinking · ", state$street))
    )
  })
  output$human_action_badge <- renderUI({
    if (isTRUE(reveal_in_progress()) || isTRUE(state$hand_over) ||
        !identical(state$to_act, HUMAN)) return(NULL)
    span(class="action-badge", "ACTION")
  })
  
  # ==========================================================
  # CARD UI
  # ==========================================================
  
  render_cards <- function(
    cards,
    hidden = FALSE
  ) {
    if (hidden) {
      return(
        paste0("<span class='card bot-card'>🂠</span>", "<span class='card bot-card'>🂠</span>")
      )
    }
    if (length(cards) == 0) {
      return("<span style='opacity:.6;'>No cards</span>")
    }
    pieces <- vapply(
      cards,
      function(card) {
        cls <- ifelse(card_suit(card) %in% c("d", "h"), "card card-red", "card")
        paste0("<span class='", cls, "'>", card_label(card), "</span>")
      },
      character(1)
    )
    paste0(pieces, collapse = "")
  }
  output$human_hand_ui <- renderUI({
    HTML(render_cards(state$human_cards))
  })
  output$bot_hand_ui <- renderUI({
    HTML(
      render_cards(state$bot_cards, hidden = !state$showdown || isTRUE(reveal_in_progress()))
    )
  })
  output$board_ui <- renderUI({
    visible <- shown_board()
    if (length(visible) == 0L) {
      HTML(
        if (isTRUE(reveal_in_progress()))
          "<span style='opacity:.7;'>Dealing the flop...</span>"
        else
          "<span style='opacity:.7;'>Waiting for flop...</span>"
      )
    } else {
      HTML(paste0("<div class='board-cards'>", render_cards(visible), "</div>"))
    }
  })
  
  # ==========================================================
  # RESULT
  # ==========================================================
  
  output$result_ui <- renderUI({
    if (!isTRUE(state$hand_over)) return(NULL)
    if (isTRUE(reveal_in_progress())) {
      return(div(class = "winner-box", strong("Revealing the board...")))
    }
    if (state$showdown) {
      human_rank <- evaluate_seven(c(state$human_cards, state$board))
      bot_rank <- evaluate_seven(c(state$bot_cards, state$board))
      result_text <- switch(
        state$winner,
        Human =
          paste0("You win $", total_pot(state), " with ", human_rank$name, "."),
        Bot =
          paste0("Bot wins $", total_pot(state), " with ", bot_rank$name, "."),
        Tie =
          paste0("Split pot. Both players have ", human_rank$name, ".")
      )
    } else {
      result_text <- paste0(
        ifelse(state$winner == HUMAN, "You", "Bot"),
        " win the hand by fold."
      )
    }
    div(
      class = "winner-box",
      strong(result_text),
      br(),
      actionButton("next_hand_result", "Next Hand", class = "btn-warning")
    )
  })
  
  # ==========================================================
  # ACTION HELP
  # ==========================================================
  
  output$action_help <- renderUI({
    if (isTRUE(reveal_in_progress())) {
      return(p("Dealing community cards...", style = "color:#6b7280;"))
    }
    if (state$hand_over) {
      return(p("Hand complete. Start the next hand.", style = "color:#6b7280;"))
    }
    if (!identical(state$to_act, HUMAN)) {
      return(
        p(
          if (identical(state$to_act, BOT)) {
            if (isTRUE(bot_is_thinking())) {
              paste0("Bot is thinking · ", state$street, "...")
            } else {
              "Bot finished the previous action; preparing the next street..."
            }
          } else "Waiting for the hand to finish...",
          style = "color:#6b7280;"
        )
      )
    }
    to_call <- current_bet_to_call(state, HUMAN)
    if (state$stacks[[BOT]] <= 0) {
      return(p(paste0("Bot is all-in. Call $", to_call,
                      " or fold; raising is not available.")))
    }
    min_raise <- minimum_raise_to(state, HUMAN)
    if (to_call == 0) {
      p(paste0("You can check or bet. Minimum raise-to: $", min_raise, "."))
    } else {
      p(paste0("To call: $", to_call, ". Minimum raise-to: $", min_raise, "."))
    }
  })
  
  # Disable controls while cards are revealed or the bot is acting.
  observe({
    busy <- isTRUE(reveal_in_progress()) || isTRUE(state$hand_over) ||
      !identical(state$to_act, HUMAN)
    session$sendCustomMessage("poker_action_controls", list(disabled = busy))
  })
  
  # ==========================================================
  # RAISE SLIDER
  # ==========================================================
  observe({
    if (state$hand_over) {
      return()
    }
    min_raise <- max(BB, minimum_raise_to(state, HUMAN))
    max_raise <-
      state$street_bets[[HUMAN]] +
      state$stacks[[HUMAN]]
    max_raise <- max(min_raise, max_raise)
    current_value <- isolate(input$raise_amount)
    if (is.null(current_value)) {
      current_value <- min_raise
    }
    current_value <- min(max(current_value, min_raise), max_raise)
    updateSliderInput(
      session,
      "raise_amount",
      min = min_raise,
      max = max_raise,
      value = current_value,
      step = 1
    )
  })
  
  # ==========================================================
  # GAME LOG
  # ==========================================================
  
  output$game_log <- renderUI({
    if (length(state$log) == 0) {
      return(HTML("<div style='opacity:.6;'>No actions yet.</div>"))
    }
    entries <- vapply(
      rev(state$log),
      function(x) {
        paste0("<div style='margin-bottom:5px;'>", x, "</div>")
      },
      character(1)
    )
    HTML(paste0("<div class='log-box'>", paste0(entries, collapse = ""), "</div>"))
  })
}

# ============================================================
# RUN
# ============================================================

shinyApp(ui = ui, server = server)
