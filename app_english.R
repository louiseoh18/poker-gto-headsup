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
# with a two-second pause whenever it becomes the bot's turn.
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

# One bot decision per invocation. The server waits two seconds before
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

# River-only analysis: compare every legal opponent holding against your
# actual best five-card hand. Compare complete ranks (including kickers).
lab_river_rank_label <- function(rank) {
  # ONE formatter for every hand shown in the River tab, including
  # your own hand, exact opponent hands, and every possible winning group.
  # Rank notation is restricted to 23456789TJQKA: no spelled-out ranks.
  symbols <- c("2", "3", "4", "5", "6", "7", "8", "9", "T", "J", "Q", "K", "A")
  sym <- function(value) symbols[as.integer(value) - 1L]
  values <- rank[-1L]
  values <- values[values > 0L]
  category <- rank[1L]
  
  if (category == 8L) {
    if (values[1L] == 14L) return("Royal flush")
    return(sprintf("Straight flush: %s-high", sym(values[1L])))
  }
  if (category == 7L)
    return(sprintf("Quads: %s (%s kicker)", sym(values[1L]), sym(values[2L])))
  if (category == 6L) {
    # Concise display convention requested for ALL full houses:
    # show the two ranks high-to-low, regardless of which rank is trips.
    # KKK22 and 222KK both DISPLAY as "Full house: K2"; their true
    # five-card rankings are compared separately before grouping.
    full_house_ranks <- sort(values[1L:2L], decreasing=TRUE)
    return(sprintf("Full house: %s%s",
                   sym(full_house_ranks[1L]), sym(full_house_ranks[2L])))
  }
  if (category == 5L)
    return(sprintf("Flush: %s", paste(sym(values), collapse="")))
  if (category == 4L)
    return(sprintf("Straight: %s-high", sym(values[1L])))
  if (category == 3L)
    return(sprintf("Triple: %s (%s, %s kickers)",
                   sym(values[1L]), sym(values[2L]), sym(values[3L])))
  if (category == 2L)
    return(sprintf("Two pair: %s%s (%s kicker)",
                   sym(values[1L]), sym(values[2L]), sym(values[3L])))
  if (category == 1L)
    return(sprintf("Pair: %s (%s kickers)",
                   sym(values[1L]), paste(sym(values[-1L]), collapse=", ")))
  if (category == 0L)
    return(sprintf("High card: %s", paste(sym(values), collapse="")))
  stop("Unknown poker hand category")
}

lab_river_better_hands <- function(hero, board, style, villain=character(0)) {
  stopifnot(length(hero) == 2L, length(board) == 5L)
  mine <- lab_rank(c(hero, board))
  remaining <- setdiff(LAB_DECK, c(hero, board))
  exact <- length(villain) == 2L
  combos <- if (exact) matrix(villain, nrow=2L) else lab_opponent_combos(remaining, style)
  n <- ncol(combos)
  if (n == 0L) stop("No opponent hands remain in the selected range.")
  ranks <- matrix(0L, nrow=6L, ncol=n)
  comparison <- integer(n)
  for (i in seq_len(n)) {
    ranks[, i] <- lab_rank(c(combos[, i], board))
    comparison[i] <- lab_compare(mine, ranks[, i])
  }
  better <- which(comparison < 0L)
  holes <- vapply(seq_len(n), function(i) {
    paste(vapply(combos[, i], card_label, character(1)), collapse=" ")
  }, character(1))
  groups <- data.frame(hand=character(0), combos=integer(0), examples=character(0),
                       stringsAsFactors=FALSE)
  if (length(better)) {
    # Group by FULL showdown rank, not just "straight"/"flush". Rank ordering
    # accounts for the actual high cards and kickers within each category.
    keys <- vapply(better, function(i) paste(ranks[, i], collapse=":"), character(1))
    split_ix <- split(better, keys)
    grouped <- lapply(split_ix, function(ix) {
      r <- ranks[, ix[1L]]
      list(rank=r, name=lab_river_rank_label(r), indices=ix)
    })
    sorting <- do.call(rbind, lapply(grouped, function(g) -g$rank))
    ord <- do.call(order, as.data.frame(sorting))
    grouped <- grouped[ord]
    
    # Short high-to-low full-house labels deliberately hide which rank is
    # trips. Merge those labels into ONE row rather than displaying two
    # identical-looking "Full house: K2" rows with different combo counts.
    names_by_rank <- vapply(grouped, `[[`, character(1), "name")
    display_names <- unique(names_by_rank)
    groups <- do.call(rbind, lapply(display_names, function(label) {
      match_ix <- which(names_by_rank == label)
      exact_groups <- grouped[match_ix]
      combo_ix <- unlist(lapply(exact_groups, `[[`, "indices"), use.names=FALSE)
      data.frame(
        hand=label,
        combos=as.integer(length(combo_ix)),
        examples=paste(head(unique(holes[combo_ix]), 2L), collapse=" / "),
        stringsAsFactors=FALSE
      )
    }))
    rownames(groups) <- NULL
  }
  list(total=n, better=length(better), tied=sum(comparison == 0L),
       exact=exact, opponent_label=if (exact) lab_river_rank_label(ranks[, 1L]) else NULL,
       outcome=if (exact) comparison[1L] else NULL,
       your_label=lab_river_rank_label(mine), groups=groups)
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


# Flop-only, board-based texture cue. A true assessment also depends on
# both players' ranges; these simple labels are teaching aids.
lab_flop_texture_summary <- function(board) {
  stopifnot(length(board) == 3L)
  ranks <- as.integer(unname(LAB_RANKS[substr(board, 1L, 1L)]))
  suits <- substr(board, 2L, 2L)
  suit_max <- max(table(suits))
  paired <- any(duplicated(ranks))
  # Include ace-low wheels when checking three-rank connectivity.
  values <- unique(c(ranks, if (14L %in% ranks) 1L))
  connected <- !paired && any(vapply(5L:14L, function(h)
    sum(seq.int(h - 4L, h) %in% values) == 3L, logical(1)))
  tone <- if (suit_max == 3L) "monotone" else
    if (suit_max == 2L) "two-tone" else "rainbow"
  texture <- if (suit_max == 3L || connected) "Wet" else
    if (suit_max == 2L) "Semi-wet" else "Dry"
  features <- c(if (paired) "paired" else NULL,
                tone,
                if (connected) "connected" else NULL)
  list(label=texture, features=paste(features, collapse=" · "))
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

# Spot-specific decision prompts. These are teaching cues, not solver actions.
lab_beginner_guidance <- function(hero, board, facing) {
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
  list(headline=headline, explanation=explanation, board=board_easy,
       draws=if (length(draw_easy)) paste(draw_easy,collapse=" + ") else "None detected")
}


# ============================================================
# NEXT-STREET EXPLORERS (FLOP AND TURN)
# Classification concerns changes to YOUR actual best hand. Exact
# win/tie/loss labels require a specified, legal opponent holding.
# ============================================================

# Short hand labels fit within small river-card tiles. The full hand notation
# remains available via each tile's tooltip / accessible label.
lab_river_rank_brief <- function(rank) {
  ranks <- c("2","3","4","5","6","7","8","9","T","J","Q","K","A")
  sym <- function(v) ranks[as.integer(v)-1L]
  v <- rank[-1L]
  category <- rank[1L]
  if (category == 0L) return(paste0("High ",sym(v[1L])))
  if (category == 1L) return(paste0("Pair ",sym(v[1L])))
  if (category == 2L) return(paste0("2P ",sym(v[1L]),sym(v[2L])))
  if (category == 3L) return(paste0("Trips ",sym(v[1L])))
  if (category == 4L) return(paste0("St ",sym(v[1L])))
  if (category == 5L) return(paste0("Fl ",paste(sym(v[1L:5L]),collapse="")))
  if (category == 6L) {
    pair <- sort(v[1L:2L],decreasing=TRUE)
    return(paste0("FH ",paste(sym(pair),collapse="")))
  }
  if (category == 7L) return(paste0("Quads ",sym(v[1L])))
  if (category == 8L)
    return(if (v[1L]==14L) "Royal" else paste0("SF ",sym(v[1L])))
  stop("Unknown hand category")
}

lab_next_street_cards <- function(hero, board, villain=character(0)) {
  stopifnot(length(hero)==2L, length(board) %in% c(3L,4L),
            length(villain) %in% c(0L,2L),
            !anyDuplicated(c(hero,board,villain)))
  cards <- setdiff(LAB_DECK, c(hero, board, villain))
  current <- lab_rank(c(hero, board))
  current_draws <- lab_hand_features(hero, board)$draws
  on_turn <- length(board) == 4L
  rows <- lapply(cards, function(card) {
    next_board <- c(board, card)
    next_rank <- lab_rank(c(hero, next_board))
    change <- if (next_rank[1L] > current[1L]) {
      "Made hand improves"
    } else if (lab_compare(next_rank, current) > 0L) {
      "Kicker improves"
    } else {
      "Made hand unchanged"
    }
    new_draws <- if (on_turn) character(0) else
      setdiff(lab_hand_features(hero, next_board)$draws, current_draws)
    if (!on_turn && length(new_draws) && change != "Made hand improves")
      change <- "New draw"
    # Exact winning cards can only be named on the turn, with the
    # opponent's actual cards supplied. This is NOT inferred from
    # a made-hand improvement alone.
    result <- ""
    equity_after <- NA_real_
    if (length(villain)==2L) {
      if (on_turn) {
        vs <- lab_compare(next_rank,lab_rank(c(villain,next_board)))
        result <- if (vs>0L) "WIN" else if (vs==0L) "TIE" else "LOSS"
      } else {
        # There is one further card after the hypothetical turn;
        # enumerate it exactly against this known opponent.
        e <- lab_equity(hero,next_board,"random",villain,n=1L)
        equity_after <- e$equity
      }
    }
    data.frame(card=card,
               hand=lab_river_rank_label(next_rank),
               brief=lab_river_rank_brief(next_rank),
               change=change,
               new_draw=if (length(new_draws)) paste(new_draws,collapse=" + ") else "",
               result=result, equity_after=equity_after,
               stringsAsFactors=FALSE)
  })
  out <- do.call(rbind,rows)
  rownames(out) <- NULL
  # Read the explorer from high card to low card; suit ordering stable.
  out <- out[order(-unname(LAB_RANKS[substr(out$card,1L,1L)]),
                   match(substr(out$card,2L,2L),c("s","h","d","c"))),,drop=FALSE]
  rownames(out) <- NULL
  out
}

lab_next_street_ui <- function(data, prefix, villain=character(0)) {
  stopifnot(prefix %in% c("flop","turn"))
  exact <- length(villain)==2L
  is_turn <- identical(prefix,"turn")
  group_names <- if (is_turn && exact) c("WIN","TIE","LOSS") else
    c("Made hand improves","New draw","Kicker improves","Made hand unchanged")
  group_label <- function(g) {
    if (g=="WIN") "Winning river cards" else
      if (g=="TIE") "Tying river cards" else
        if (g=="LOSS") "Losing river cards" else g
  }
  groups <- if (is_turn && exact) data$result else data$change
  active_groups <- group_names[group_names %in% groups]
  content <- lapply(seq_along(active_groups), function(i) {
    grp <- active_groups[i]
    selected <- data[groups==grp,,drop=FALSE]
    # A click on a tile copies the entire current scenario plus this
    # next community card to Turn/River. No automatic solver claim.
    tiles <- lapply(seq_len(nrow(selected)), function(j) {
      r <- selected[j,,drop=FALSE]
      card <- r$card[1L]
      suit_red <- substr(card,2L,2L) %in% c("d","h")
      tags$button(type="button",class="lab-explorer-tile",
                  `data-from`=prefix,`data-card`=card,
                  title=paste(card_label(card),"—",r$hand[1L],
                              if (is_turn && exact) paste("—",r$result[1L]) else "",
                              "(click to inspect)",sep=" "),
                  `aria-label`=paste(card_label(card),"produces",r$hand[1L],
                                     if (is_turn && exact) r$result[1L] else "", "click to inspect"),
                  span(class=if(suit_red) "lab-explorer-rank red" else "lab-explorer-rank",
                       card_label(card)),
                  span(class="lab-explorer-hand",if (is_turn) r$brief[1L] else r$hand[1L]),
                  if (is_turn && exact) {
                    span(class=paste("lab-explorer-result",tolower(r$result[1L])),r$result[1L])
                  } else if (!is_turn && exact) {
                    span(class="lab-explorer-meta",sprintf("%.1f%% equity",100*r$equity_after[1L]))
                  } else if (nzchar(r$new_draw[1L])) {
                    span(class="lab-explorer-meta","+ draw")
                  }
      )
    })
    tags$details(class="lab-explorer-group",
                 open=if (is_turn || i==1L) "open" else NULL,
                 tags$summary(tagList(strong(group_label(grp)),span(class="lab-explorer-count",nrow(selected)," cards"))),
                 div(class="lab-explorer-tiles",tiles))
  })
  div(class=if (is_turn) "lab-explorer lab-explorer-turn" else "lab-explorer",
      div(class="lab-explorer-headline",
          h4(if(is_turn) "Explore possible river cards" else "Explore possible turn cards"),
          tags$small(sprintf("%d legal next cards",nrow(data)))),
      p(class="lab-explorer-help",
        if (is_turn && exact)
          "Results are exact against the opponent's selected cards. Click any river card to examine that final board."
        else if (is_turn)
          "See how every river card changes your final hand. Select an exact opponent hand to reveal winning, tying and losing river cards."
        else if (exact)
          "Changes to your hand and exact turn-to-showdown equity versus this specific opponent. Click a turn card to continue."
        else
          "See what each turn card does to your hand or draws. These are NOT automatically winning outs. Click a card to continue."),
      content)
}

# ============================================================
# SINGLE-SOURCE POKER GLOSSARY
# Shared by the searchable tab, short street glossaries, and inline links.
# Educational definitions describe standard No-Limit Hold'em terminology;
# they do NOT imply that this app is a GTO solver.
# ============================================================

poker_glossary <- local({
  add <- function(id, term, category, definition, example, related="")
    data.frame(id=id, term=term, category=category, definition=definition,
               example=example, related=related, stringsAsFactors=FALSE)
  out <- do.call(rbind, list(
    add("texas-hold-em", "Texas Hold'em", "Rules & hands", "A poker game in which each player combines two private hole cards with five shared community cards to make the best five-card hand.", "You can use both, one, or neither of your hole cards.", ""),
    add("no-limit", "No-Limit", "Rules & hands", "A betting structure in which you may bet or raise up to the chips you have available, subject to the minimum-raise rules.", "A player can move all-in rather than choose a fixed betting limit.", "all-in"),
    add("hole-cards", "Hole cards", "Rules & hands", "The two private cards dealt to you in Texas Hold'em.", "A♠ K♠ are your hole cards.", ""),
    add("community-cards", "Community cards", "Rules & hands", "The five shared cards placed face up: three on the flop, one on the turn, and one on the river.", "Any player still in the hand can use the board.", "flop,turn,river"),
    add("board", "Board", "Rules & hands", "The community cards currently showing; board texture affects which hands and draws are possible.", "A♠ 7♠ 2♦ is a two-tone flop.", "community-cards,board-texture"),
    add("pre-flop", "Pre-flop", "Rules & hands", "The betting round after hole cards are dealt and before any community cards appear.", "In heads-up play, the SB/button acts first pre-flop.", "button"),
    add("flop", "Flop", "Rules & hands", "The first three community cards and the betting round that follows.", "K♠ 8♠ 3♦ is a possible flop.", "community-cards"),
    add("turn", "Turn", "Rules & hands", "The fourth community card and its betting round.", "The turn can complete a flush draw.", "flush-draw"),
    add("river", "River", "Rules & hands", "The fifth and last community card, followed by the final betting round.", "There are no more cards to come after the river.", "showdown"),
    add("showdown", "Showdown", "Rules & hands", "The point where remaining players reveal their hole cards and the best five-card hand wins, unless the pot is split.", "Two players call on the river and compare hands.", "hand-ranking"),
    add("hand-ranking", "Hand ranking", "Rules & hands", "The order used to compare five-card poker hands, including category and kickers where applicable.", "Straight flush > quads > full house > flush > straight > trips > two pair > pair > high card.", "kicker"),
    add("high-card", "High card", "Rules & hands", "A hand with no pair, straight, flush, or higher category; compare its five highest card ranks.", "A-K-J-9-4 high.", ""),
    add("one-pair", "One pair", "Rules & hands", "Two cards of one rank, plus the three highest available kickers.", "Pair of Queens with A-J-8 kickers.", "kicker"),
    add("two-pair", "Two pair", "Rules & hands", "Two distinct pairs plus the best available kicker.", "Two pair K8 with A kicker.", "kicker"),
    add("trips", "Trips", "Rules & hands", "Three cards of one rank; kickers break ties when needed.", "Triple: K (8, 6 kickers).", "set,kicker"),
    add("set", "Set", "Rules & hands", "Three of a kind formed using a pocket pair and a third card of that rank on the board.", "You hold 8♣ 8♦ and the flop contains 8♠.", "trips"),
    add("straight", "Straight", "Rules & hands", "Five consecutive ranks, regardless of suit; an ace can be high or low.", "A-2-3-4-5 is a 5-high straight; T-J-Q-K-A is ace-high.", "wheel"),
    add("wheel", "Wheel", "Rules & hands", "An A-2-3-4-5 straight, with the five as the high card.", "The ace counts low only in the wheel.", "straight"),
    add("flush", "Flush", "Rules & hands", "Five cards of one suit; compare the five highest ranks in the flush.", "A♠ Q♠ T♠ 7♠ 2♠ beats a king-high flush.", "nut-flush"),
    add("full-house", "Full house", "Rules & hands", "Three of one rank plus two of another rank; compare the trips rank first, then the pair.", "KKK22 beats QQQAA. The River table may display both ranks high-to-low as K2.", "trips"),
    add("quads", "Quads", "Rules & hands", "Four of a kind plus a kicker; four of a higher rank beats four of a lower rank.", "Quads: K (9 kicker).", "kicker"),
    add("straight-flush", "Straight flush", "Rules & hands", "Five consecutive ranks of the same suit.", "7♠ 8♠ 9♠ T♠ J♠ is a jack-high straight flush.", "straight,flush"),
    add("royal-flush", "Royal flush", "Rules & hands", "An ace-high straight flush: T-J-Q-K-A in one suit.", "T♥ J♥ Q♥ K♥ A♥.", "straight-flush"),
    add("kicker", "Kicker", "Rules & hands", "An unpaired card used to break ties between hands in the same category.", "With the same pair, an ace kicker may beat a king kicker.", ""),
    add("chop-split-pot", "Chop / split pot", "Rules & hands", "A tie where two or more players share the pot according to the house rules.", "If the best five cards are entirely on the board, the pot can be split.", "play-the-board"),
    add("play-the-board", "Play the board", "Rules & hands", "Using all five community cards as your best hand.", "If the board shows the best straight available to everyone, you may play the board.", "board"),
    add("nut-hand", "Nut hand", "Rules & hands", "The strongest possible hand on the current board, considering all legal opponent cards.", "The nut hand may change when the turn or river appears.", "nuts"),
    add("nuts", "Nuts", "Rules & hands", "Short for the nut hand: the strongest possible current holding.", "The nuts on one board may be a straight, and on another a full house.", "nut-hand"),
    add("counterfeit", "Counterfeit", "Rules & hands", "When a new board card makes one of your hole-card-based pairs less useful in your best five-card hand.", "You hold 8-7 on K-8-7, but a second K can counterfeit part of your two-pair advantage.", "two-pair"),
    add("heads-up", "Heads-up", "Game structure", "Poker between two players; the button posts the small blind, acts first pre-flop, and acts last post-flop.", "The BB acts first after the flop in heads-up.", "button,small-blind-sb,big-blind-bb"),
    add("six-max", "Six-max", "Game structure", "A table format with at most six seats, often with wider ranges than a full-ring table.", "A six-max cutoff is one seat before the button.", "cutoff-co"),
    add("full-ring", "Full-ring", "Game structure", "A standard larger-table format, typically eight or nine players depending on the room.", "Early-position ranges are commonly tighter than in heads-up.", "position"),
    add("cash-game", "Cash game", "Cash games", "Poker in which chips represent cash value and players may generally join or leave between hands, subject to house rules.", "A $1/$2 No-Limit cash game uses $1 and $2 blinds.", "buy-in"),
    add("tournament", "Tournament", "Tournaments", "An event with escalating blinds and a payout structure tied to finishing position, unlike ordinary cash-game chips.", "A cash-game stack and tournament stack have different strategic considerations.", "cash-game"),
    add("blinds", "Blinds", "Game structure", "The forced pre-flop wagers posted to start a hand; normally one small blind and one big blind.", "In this app, the blinds are $1/$2.", "small-blind-sb,big-blind-bb"),
    add("small-blind-sb", "Small blind (SB)", "Game structure", "The smaller forced blind bet, normally posted immediately to the left of the dealer button; in heads-up the button posts it.", "With $1/$2 blinds, the SB posts $1.", "button"),
    add("big-blind-bb", "Big blind (BB)", "Game structure", "The larger forced blind bet, usually twice the small blind, although structures vary.", "With $1/$2 blinds, the BB posts $2.", "small-blind-sb"),
    add("button", "Button", "Game structure", "The nominal dealer position; in heads-up this player posts the SB, acts first pre-flop, and acts last post-flop.", "The button normally has positional advantage after the flop.", "position"),
    add("in-position-ip", "In position (IP)", "Game structure", "Acting after your opponent on the current betting street, allowing you to see their action first.", "The button is in position post-flop in heads-up.", "out-of-position-oop"),
    add("out-of-position-oop", "Out of position (OOP)", "Game structure", "Acting before your opponent on the current betting street.", "The BB is out of position after the flop in heads-up.", "in-position-ip"),
    add("position", "Position", "Game structure", "A player's betting order relative to other players; acting later provides more information.", "The cutoff and button are late positions in multiplayer.", "button,cutoff-co"),
    add("under-the-gun-utg", "Under the gun (UTG)", "Game structure", "The first player to act pre-flop at a conventional multiplayer table; the exact seat depends on table size.", "UTG is an early position in full-ring games.", "position"),
    add("hijack-hj", "Hijack (HJ)", "Game structure", "The seat two places to the right of the button in common multiplayer layouts.", "At a six-max table, the hijack precedes the cutoff.", "cutoff-co"),
    add("cutoff-co", "Cutoff (CO)", "Game structure", "The seat immediately to the right of the button in multiplayer play.", "The cutoff may open a wider range than early positions.", "button"),
    add("blind-level", "Blind level", "Game structure", "The amount of the mandatory blinds; cash games often keep it fixed while tournaments typically increase it.", "$1/$2 is one blind level.", "big-blind-bb"),
    add("ante", "Ante", "Game structure", "A forced contribution to the pot in addition to blinds, when used.", "Some games collect a big-blind ante.", "pot"),
    add("stack", "Stack", "Game structure", "The chips a player has available to wager.", "A $200 stack at $1/$2 equals 100 big blinds.", "effective-stack"),
    add("effective-stack", "Effective stack", "Game structure", "The smaller of the two relevant players' remaining stacks for heads-up betting; it limits how much one player can win from the other.", "If you have $150 and your opponent has $90, the effective stack is $90.", "stack"),
    add("stack-depth", "Stack depth", "Game structure", "A stack measured in big blinds, often useful for comparing decisions across stakes.", "A $200 stack with a $2 BB is 100 BB deep.", "effective-stack"),
    add("buy-in", "Buy-in", "Game structure", "The money exchanged for chips when joining a cash game or entering a tournament.", "A $200 buy-in at $1/$2 is 100 BB.", "stack"),
    add("pot", "Pot", "Game structure", "All chips wagered into the current hand, subject to returning uncalled excess at settlement.", "Both players contribute $10, creating a $20 pot.", "pot-odds"),
    add("side-pot", "Side pot", "Game structure", "A separate pot when one player is all-in for less than other players wager; side pots require at least three players.", "Heads-up games normally return the unmatched excess rather than creating a contested side pot.", "all-in"),
    add("check", "Check", "Betting", "Decline to bet when no amount is owed, while keeping your hand active.", "The BB can check after the SB completes pre-flop.", "call"),
    add("bet", "Bet", "Betting", "Put chips into the pot when no bet has yet been made on the current street.", "You bet half the pot on the flop.", "bet-sizing"),
    add("call", "Call", "Betting", "Match the amount required to continue when facing a bet or raise; a short all-in call may be for less.", "You call a $10 river bet.", "pot-odds"),
    add("raise", "Raise", "Betting", "Increase the current bet, subject to minimum-raise and stack rules.", "An opponent bets $10; you raise to $30.", "3-bet"),
    add("fold", "Fold", "Betting", "Give up the hand and forfeit your claim to the current pot.", "You fold when facing a bet you do not want to call.", "showdown"),
    add("all-in", "All-in", "Betting", "Wager all your remaining chips; an opponent cannot win more from you than you have committed.", "A short-stack all-in call can be below the usual minimum raise.", "effective-stack"),
    add("open-raise", "Open-raise", "Betting", "Make the first voluntary raise in the pre-flop betting round.", "The button opens to 2.5 BB.", "opening-range"),
    add("limp", "Limp", "Betting", "Enter a pre-flop pot by calling the big blind instead of raising when no player has raised.", "The SB/button completes to the BB in heads-up.", "open-raise"),
    add("iso-raise", "Iso-raise", "Betting", "Raise over one or more limpers to try to play against fewer opponents.", "You raise after someone limps in a cash game.", "limp"),
    add("3-bet", "3-bet", "Betting", "The first re-raise after an initial pre-flop open-raise, or a third betting action in the applicable sequence.", "SB opens to 3 BB; BB 3-bets to 10 BB.", "4-bet"),
    add("4-bet", "4-bet", "Betting", "A re-raise of a 3-bet, usually pre-flop.", "SB opens, BB 3-bets, then SB 4-bets.", "3-bet"),
    add("squeeze", "Squeeze", "Betting", "Re-raise pre-flop after one player has raised and at least one other has called.", "Player A opens, B calls, and C makes a squeeze 3-bet.", "3-bet"),
    add("check-raise", "Check-raise", "Betting", "Check, then raise after the opponent bets on the same street.", "BB checks the flop, button bets, BB raises.", "check"),
    add("continuation-bet-c-bet", "Continuation bet (c-bet)", "Betting", "A bet by the player who was the aggressor on the preceding street, often the pre-flop raiser betting the flop.", "The button opens pre-flop and c-bets the flop.", "barrel"),
    add("donk-bet", "Donk bet", "Betting", "A bet made into the previous street's aggressor before that player has a chance to act.", "BB leads into the pre-flop raiser on the flop.", "continuation-bet-c-bet"),
    add("probe-bet", "Probe bet", "Betting", "A bet by a player, often out of position, after the prior street's aggressor declined to continuation bet.", "BB bets the turn after button checked back the flop.", "continuation-bet-c-bet"),
    add("lead", "Lead", "Betting", "Make the first bet on a street rather than checking.", "BB leads into the flop.", "donk-bet"),
    add("barrel", "Barrel", "Betting", "Make another aggressive bet across successive streets.", "C-bet the flop, then barrel the turn.", "double-barrel"),
    add("double-barrel", "Double barrel", "Betting", "Bet both flop and turn, often as the player who took the lead pre-flop.", "A flop c-bet followed by a turn bet is a double barrel.", "barrel"),
    add("triple-barrel", "Triple barrel", "Betting", "Bet flop, turn, and river on the same hand.", "A player continues their bluff through all three streets.", "barrel"),
    add("overbet", "Overbet", "Betting", "Bet more than the size of the pot, usually expressed as a percentage of the pot.", "Betting $150 into a $100 pot is a 150% pot overbet.", "bet-sizing"),
    add("bet-sizing", "Bet sizing", "Betting", "The amount wagered relative to the pot, stack, or previous bet.", "Quarter-pot, half-pot, pot, and all-in are possible sizes.", "pot"),
    add("minimum-raise", "Minimum raise", "Betting", "The smallest legal raise under the game rules; ordinary raises must increase by at least the previous full bet or raise increment.", "A short all-in may be below the full minimum without reopening action.", "raise"),
    add("pot-control", "Pot control", "Betting", "Using checks or smaller bets to keep the pot manageable with a hand that may not want to play for a large stack.", "Checking a medium pair behind on the turn can be pot control.", "check,showdown-value"),
    add("value-bet", "Value bet", "Betting", "A bet designed to be called by enough worse hands to make betting profitable.", "Bet top pair when weaker pairs often call.", "thin-value-bet"),
    add("thin-value-bet", "Thin value bet", "Betting", "A value bet with a relatively narrow margin: only a modest set of worse hands calls often enough.", "A small river bet with second pair may be thin value.", "value-bet"),
    add("bluff", "Bluff", "Betting", "A bet or raise that primarily aims to make better hands fold.", "Bet with an unmade hand when credible stronger hands are in your range.", "fold-equity"),
    add("semi-bluff", "Semi-bluff", "Betting", "A bet or raise with a hand that may improve if called, such as a flush draw.", "Bet a flush draw on the flop.", "bluff"),
    add("bluff-catcher", "Bluff-catcher", "Betting", "A hand that mostly beats bluffs but loses to the opponent's value-betting range.", "Call a river bet with a medium pair when bluffs are sufficiently common.", "bluff"),
    add("slow-play-trap", "Slow-play / trap", "Betting", "Play a strong hand passively to invite opponent bets or preserve weaker hands.", "Check a strong set to induce a bet; doing so carries risk.", "set"),
    add("check-back", "Check back", "Betting", "Check while in position, ending the street when your opponent has already checked.", "Button checks back the flop after BB checks.", "in-position-ip"),
    add("starting-hand-notation", "Starting hand notation", "Pre-flop", "Two rank symbols plus s for suited or o for offsuit, or doubled ranks for pocket pairs; T denotes ten.", "98s = suited 9-8, AKo = offsuit A-K, 99 = pocket nines.", "suited,offsuit,pocket-pair"),
    add("suited", "Suited", "Pre-flop", "Two hole cards of the same suit, shown with an s in starting-hand notation.", "KQs means suited K-Q.", "starting-hand-notation"),
    add("offsuit", "Offsuit", "Pre-flop", "Two hole cards of different suits, shown with an o in starting-hand notation.", "AKo means offsuit A-K.", "starting-hand-notation"),
    add("pocket-pair", "Pocket pair", "Pre-flop", "Two hole cards of the same rank.", "99 denotes pocket nines.", "set"),
    add("suited-connector", "Suited connector", "Pre-flop", "Two consecutive-rank hole cards of the same suit.", "98s is a suited connector.", "suited-gapper"),
    add("suited-gapper", "Suited gapper", "Pre-flop", "Two suited hole cards with one or more ranks separating them.", "97s is a one-gap suited hand.", "suited-connector"),
    add("broadway-cards", "Broadway cards", "Pre-flop", "T, J, Q, K, and A; a Broadway straight is T-J-Q-K-A.", "KQs contains two Broadway cards.", "straight"),
    add("premium-hand", "Premium hand", "Pre-flop", "A comparatively strong starting hand, with the exact group depending on position and stack depth.", "AA, KK, and AK are commonly considered premium.", "opening-range"),
    add("opening-range", "Opening range", "Pre-flop", "The set of starting hands used to make the first pre-flop raise from a given seat.", "Heads-up SB/button opens many more hands than typical early-position full-ring.", "range"),
    add("defending-range", "Defending range", "Pre-flop", "The hands a player continues with against an opponent's bet or raise, by calling or raising.", "BB can call or 3-bet when defending against a button open.", "3-bet"),
    add("3-bet-range", "3-bet range", "Pre-flop", "The subset of hands used to re-raise an initial raise pre-flop.", "A 3-bet range can contain strong value hands and selected bluffs.", "3-bet"),
    add("steal", "Steal", "Pre-flop", "An open-raise intended to win the blinds uncontested, often from late position.", "The button raises when earlier players folded.", "open-raise"),
    add("resteal", "Resteal", "Pre-flop", "A re-raise intended to contest a potential blind steal.", "BB 3-bets a frequent button opener.", "steal"),
    add("limped-pot", "Limped pot", "Pre-flop", "A pre-flop pot in which at least one player called the BB and nobody raised.", "In heads-up, SB completes and BB checks.", "limp"),
    add("mixed-strategy", "Mixed strategy", "Pre-flop", "Using different actions with the same hand at specified frequencies rather than always taking one action.", "Some hands mix between opening and folding.", "frequency"),
    add("open-size", "Open size", "Pre-flop", "The total size of the initial pre-flop raise, often measured in BB.", "Opening to 2.5 BB at $1/$2 means raising to $5.", "big-blind-bb"),
    add("board-texture", "Board texture", "Post-flop", "Features of the community cards that affect possible made hands and draws, such as paired, connected, dry, or two-tone.", "J♠ T♠ 9♦ is connected and two-tone.", "two-tone,connected-board"),
    add("dry-board", "Dry board", "Post-flop", "A board with relatively few immediate straight or flush possibilities; texture is always relative to ranges.", "K♣ 7♦ 2♠ is relatively dry.", "wet-board"),
    add("wet-board", "Wet board", "Post-flop", "A board with many plausible draws and changing turn or river cards.", "J♠ T♠ 9♦ is relatively wet.", "dry-board"),
    add("rainbow-board", "Rainbow board", "Post-flop", "A flop with three different suits, so no immediate flop flush draw is possible using two suited hole cards.", "A♣ 8♦ 2♠ is rainbow.", "two-tone"),
    add("two-tone", "Two-tone", "Post-flop", "A flop with two cards of one suit and one of another.", "A♠ T♠ 3♦ is two-tone.", "flush-draw"),
    add("monotone", "Monotone", "Post-flop", "A flop with all three cards of the same suit.", "A♥ 7♥ 2♥ is monotone.", "flush"),
    add("connected-board", "Connected board", "Post-flop", "A board containing closely spaced ranks, increasing some straight possibilities.", "9-T-J is highly connected.", "straight-draw"),
    add("paired-board", "Paired board", "Post-flop", "A board containing at least two cards of the same rank.", "Q-Q-5 is a paired flop.", "full-house"),
    add("made-hand", "Made hand", "Post-flop", "A currently completed hand rather than an unfinished draw, although it may still lose.", "Top pair is a made hand.", "draw"),
    add("draw", "Draw", "Post-flop", "A hand that could complete a stronger category with future cards.", "Four hearts after the turn is a flush draw.", "out"),
    add("flush-draw", "Flush draw", "Post-flop", "Four cards of one suit with at least one card left to come; not every flush card is a winning out.", "A♠ J♠ on K♠ 7♠ 2♦ is a flush draw.", "out"),
    add("straight-draw", "Straight draw", "Post-flop", "A holding that could complete a straight on a future card.", "8-9 on a 6-7-K flop has an open-ended straight draw.", "open-ended-straight-draw-oesd"),
    add("open-ended-straight-draw-oesd", "Open-ended straight draw (OESD)", "Post-flop", "Four consecutive ranks that can complete a straight at either end.", "6-7-8-9 can improve with a 5 or T.", "gutshot"),
    add("gutshot", "Gutshot", "Post-flop", "An inside straight draw that needs a particular middle rank to complete a straight.", "6-7-9-T needs an 8.", "open-ended-straight-draw-oesd"),
    add("backdoor-draw", "Backdoor draw", "Post-flop", "A potential hand that requires favorable cards on both the turn and river.", "Two hearts in hand and one heart on the flop can make a backdoor flush.", "runner-runner"),
    add("runner-runner", "Runner-runner", "Post-flop", "Completing a hand using both remaining community cards.", "You hit runner-runner hearts to make a flush.", "backdoor-draw"),
    add("top-pair", "Top pair", "Post-flop", "A pair using one hole card and the highest board rank.", "You hold A-K on K-8-3.", "kicker"),
    add("middle-pair", "Middle pair", "Post-flop", "A pair using a hole card and the middle board rank on an unpaired flop.", "You hold Q-8 on A-8-3.", "top-pair"),
    add("bottom-pair", "Bottom pair", "Post-flop", "A pair using a hole card and the lowest board rank on an unpaired flop.", "You hold Q-3 on A-8-3.", "top-pair"),
    add("overpair", "Overpair", "Post-flop", "A pocket pair higher than every card on the current board.", "QQ on J-8-3 is an overpair.", "pocket-pair"),
    add("overcard", "Overcard", "Post-flop", "A hole card higher in rank than every card currently on the board.", "A and K are overcards to a Q-7-2 flop.", "top-pair"),
    add("blocker", "Blocker", "Post-flop", "A card you hold that reduces the number of legal opponent combinations containing that rank or suit.", "Holding A♠ blocks opponent hands that require A♠.", "card-removal"),
    add("card-removal", "Card removal", "Post-flop", "The effect of known cards making certain opponent combinations impossible or less numerous.", "Your own A♥ means an opponent cannot hold A♥.", "blocker"),
    add("nut-flush", "Nut flush", "Post-flop", "The highest possible flush on a given board, taking known cards into account.", "When the board allows a flush, holding the ace of its suit may be relevant to the nut flush.", "flush"),
    add("showdown-value", "Showdown value", "Post-flop", "The ability of your current hand to win at showdown without making a better hand or bluffing.", "Second pair often has some showdown value.", "bluff-catcher"),
    add("out", "Out", "Math & odds", "An unseen card that improves your hand in a specified way; a clean winning out also makes you the winner against the considered opponent range.", "A flush draw commonly has nine suit-completing cards before accounting for blockers.", "flush-draw"),
    add("equity", "Equity", "Math & odds", "Your expected share of the pot at showdown against a specified opposing hand or range, including half the pot on a two-player tie.", "40% win and 10% tie gives 45% equity.", "showdown-equity"),
    add("showdown-equity", "Showdown equity", "Math & odds", "Equity calculated assuming the hand reaches showdown under the chosen opposing hand or range; it does not model future betting decisions.", "A draw's current showdown equity may exceed its immediate made-hand chance.", "equity"),
    add("pot-odds", "Pot odds", "Math & odds", "The price of a call relative to the final pot if you call; compare the required equity to realistic winning equity.", "Facing $20 into $60, call $20 to contest a final $100 pot: 20% threshold.", "equity"),
    add("implied-odds", "Implied odds", "Math & odds", "Additional chips you may win on later streets after completing your draw; they can improve the value of a call.", "A draw may win extra river bets when it hits.", "pot-odds"),
    add("reverse-implied-odds", "Reverse implied odds", "Math & odds", "Potential additional losses when you make a seemingly strong but second-best hand.", "A non-nut flush can lose extra money to a higher flush.", "implied-odds"),
    add("fold-equity", "Fold equity", "Math & odds", "Expected value gained from the possibility that an opponent folds to your bet or raise.", "A semi-bluff combines fold equity with the chance to improve.", "semi-bluff"),
    add("expected-value-ev", "Expected value (EV)", "Math & odds", "The long-run average gain or loss of a decision, weighted across its possible outcomes and associated probabilities.", "A positive-EV call can lose an individual hand.", "variance"),
    add("break-even-equity", "Break-even equity", "Math & odds", "The minimum winning-equity share needed for a call to break even under an explicitly defined pot-odds model.", "Calling $25 after a $25 bet into a $50 pot needs 25% equity without further action.", "pot-odds"),
    add("minimum-defense-frequency-mdf", "Minimum defense frequency (MDF)", "Math & odds", "A range-level reference fraction pot/(pot+bet) under simplified zero-equity-bluff assumptions; it is not an instruction to defend every individual hand.", "Versus a pot-sized bet, the simplified MDF benchmark is 50%.", "pot-odds"),
    add("monte-carlo-simulation", "Monte Carlo simulation", "Math & odds", "Estimate probabilities by sampling many random legal outcomes instead of listing every possible one.", "The flop lab can sample opponent hands and future runouts.", "exact-enumeration"),
    add("combo", "Combo / combination", "Math & odds", "One particular two-card holding with exact suits. Counting combos prevents grouping distinct legal opponent holdings into one guess.", "Without blockers, AKs has four combos and AKo has twelve.", "blocker,range"),
    add("exact-enumeration", "Exact enumeration", "Math & odds", "Calculate across every legal outcome within a defined model rather than random samples.", "With a known opponent on the river, compare the two final hands directly.", "monte-carlo-simulation"),
    add("variance", "Variance", "Math & odds", "Variation in short-run results even when a strategy has a stable long-run expected value.", "A favorable all-in can still lose.", "expected-value-ev"),
    add("sample-size", "Sample size", "Math & odds", "The number of simulated trials or observed hands used in an estimate.", "More Monte Carlo trials generally reduce simulation noise.", "monte-carlo-simulation"),
    add("equity-realization", "Equity realization", "Math & odds", "How much of your raw showdown equity you can actually convert into value given future betting, position, and potential folds.", "Being out of position can make a draw harder to realize.", "equity"),
    add("stack-to-pot-ratio-spr", "Stack-to-pot ratio (SPR)", "Math & odds", "Effective remaining stack divided by the current pot, commonly computed on a post-flop street.", "A $120 effective stack with a $40 pot gives SPR = 3.", "effective-stack"),
    add("range", "Range", "Strategy", "A collection of possible hands assigned to a player rather than one exact guess.", "An opening range may include pairs, broadway hands, and suited connectors.", "opening-range"),
    add("range-advantage", "Range advantage", "Strategy", "When one player's plausible range performs better on a particular board than the other's under a defined model.", "A pre-flop raiser may have range advantage on some ace-high boards.", "range"),
    add("nut-advantage", "Nut advantage", "Strategy", "When one player's plausible range contains more of the strongest hands on a board than the other's.", "Some connected boards favor the caller's strongest combinations.", "nut-hand"),
    add("gto-game-theory-optimal", "GTO (game theory optimal)", "Strategy", "An equilibrium-based strategy concept intended to resist exploitation under a fully specified game model; this app uses heuristics, not a solved equilibrium.", "Solver output depends on the chosen rake, stacks, bet sizes, and game tree.", "exploitative-play"),
    add("gto-inspired", "GTO-inspired", "Strategy", "A simplified heuristic that borrows selected strategic ideas from GTO without providing solver-exact equilibrium actions or frequencies.", "The practice bot uses randomized hand-strength and board-based rules.", "gto-game-theory-optimal"),
    add("exploitative-play", "Exploitative play", "Strategy", "Adjusting strategy to target specific tendencies in an opponent rather than treating every opponent alike.", "Value bet more frequently when an opponent calls too often.", "gto-game-theory-optimal"),
    add("balanced-range", "Balanced range", "Strategy", "A set of actions that contains a purposeful mix of strong hands, bluffs, and checks so the line is not trivially read from one action.", "A river betting range may include value hands and selected bluffs.", "mixed-strategy"),
    add("polarized-range", "Polarized range", "Strategy", "A betting range weighted toward very strong hands and bluffs, with fewer medium-strength hands.", "A large river bet is sometimes constructed as polarized.", "merged-range"),
    add("merged-range", "Merged range", "Strategy", "A betting or raising range that includes a broad band of hands, often strong and medium-strength value hands.", "A small river value bet may use a merged range.", "polarized-range"),
    add("frequency", "Frequency", "Strategy", "How often a strategy takes an action with a hand under specified conditions.", "A mixed strategy might bet 30% and check 70%.", "mixed-strategy"),
    add("passive-opponent", "Passive opponent", "Strategy", "A player who checks and calls relatively often and bets or raises relatively infrequently.", "A calling station is one common passive tendency.", "calling-station"),
    add("aggressive-opponent", "Aggressive opponent", "Strategy", "A player who bets and raises relatively often; aggression alone does not specify whether their hands are strong.", "Against frequent bluffs, some bluff-catchers may gain value.", "bluff-catcher"),
    add("calling-station", "Calling station", "Strategy", "An opponent who calls bets too often relative to the situation and folds too infrequently.", "Value betting often becomes more attractive against a calling station.", "value-bet"),
    add("nit", "Nit", "Poker slang", "Informal poker slang for a very tight, risk-averse style of play.", "A nit may fold many marginal pre-flop hands.", "tight-range"),
    add("tight-range", "Tight range", "Strategy", "A relatively small selection of starting hands or other possible holdings.", "A tight pre-flop range may emphasize strong aces and pocket pairs.", "range"),
    add("wide-range", "Wide range", "Strategy", "A relatively large and varied selection of possible holdings.", "A heads-up button often uses a wide opening range.", "range"),
    add("range-construction", "Range construction", "Strategy", "Selecting combinations and action frequencies to form a coherent betting, calling, or checking range.", "Building a river bluffing range requires enough believable value combinations.", "range"),
    add("value-to-bluff-ratio", "Value-to-bluff ratio", "Strategy", "The balance of value hands versus bluffs in a betting range, which depends on bet size and assumptions.", "Different river bet sizes imply different theoretical bluff proportions.", "polarized-range"),
    add("floating", "Floating", "Strategy", "Calling on an earlier street with a plan to contest a later street, often by betting after a check.", "A player floats the flop and bets a checked turn.", "bluff"),
    add("protection-bet", "Protection bet", "Strategy", "A bet made partly to deny opponents inexpensive opportunities to improve, while sometimes also extracting value.", "A small bet with a vulnerable pair can deny free cards.", "value-bet"),
    add("check-call", "Check-call", "Strategy", "Check and then call an opponent's bet on the same street.", "You check-call the turn with a bluff-catcher.", "bluff-catcher"),
    add("check-fold", "Check-fold", "Strategy", "Check and then fold when the opponent bets on the same street.", "You check-fold a weak river hand against a strong value-heavy range.", "fold"),
    add("check-raise-line", "Check-raise line", "Strategy", "Check and then raise on the same street as a deliberate strategic line.", "You check-raise a strong draw on the flop.", "check-raise"),
    add("dealer", "Dealer", "Game structure", "The person or system that deals the cards and handles the pot; the dealer button identifies nominal position.", "At a casino a dedicated dealer can deal every hand.", "button"),
    add("deck", "Deck", "Rules & hands", "The standard 52-card pack used in Texas Hold’em, without jokers.", "A deck has 13 ranks in each of four suits.", "hole-cards"),
    add("burn-card", "Burn card", "Rules & hands", "A card placed facedown and not used immediately before dealing the flop, turn or river.", "Live games ordinarily burn one card before each board street.", "flop,turn,river"),
    add("muck", "Muck", "Rules & hands", "To discard your hand without showing it, or the pile of discarded cards.", "After folding, your hole cards go into the muck.", "fold"),
    add("live-hand", "Live hand", "Rules & hands", "A hand that has not been folded or otherwise ruled dead.", "Two players have live hands going to the river.", "fold"),
    add("action", "Action", "Betting", "A player’s turn to make a betting decision, or betting activity generally.", "Action is on the big blind after the button calls.", "bet,call,raise"),
    add("betting-round", "Betting round", "Rules & hands", "One phase in which players may check, bet, call, raise or fold.", "The flop betting round ends before the turn is dealt.", "flop,turn,river"),
    add("show-hand", "Show hand", "Rules & hands", "To reveal your hole cards, typically at showdown.", "A called river bettor may need to show their hand.", "showdown"),
    add("hand-history", "Hand history", "Rules & hands", "A record of a hand’s positions, stacks, cards and betting actions.", "Review a hand history to understand a difficult river decision.", "showdown"),
    add("freeroll", "Freeroll", "Rules & hands", "A situation in which you cannot lose the main pot but can sometimes win it outright; also a tournament with no entry fee.", "Two players share a made straight but one has a flush draw as well.", "equity"),
    add("broadway-straight", "Broadway", "Rules & hands", "An ace-high straight: T-J-Q-K-A. Also used for the high-card group T through A.", "A♠ K♦ on Q-J-T makes Broadway.", "straight,broadway-cards"),
    add("boat", "Boat", "Poker slang", "Common nickname for a full house.", "KKK22 is a boat.", "full-house"),
    add("top-set", "Top set", "Post-flop", "A set made with a pocket pair matching the highest rank on the board.", "You hold KK on K-8-2.", "set"),
    add("bottom-set", "Bottom set", "Post-flop", "A set made with a pocket pair matching the lowest rank on the flop.", "You hold 22 on K-8-2.", "set"),
    add("underpair", "Underpair", "Post-flop", "A pocket pair lower than at least one card on the board; the exact usage varies with context.", "77 on a Q-T-4 flop is an underpair.", "overpair"),
    add("top-kicker", "Top kicker", "Post-flop", "The strongest available kicker paired with a particular made pair.", "With A-K on A-7-2, your pair of aces has a king kicker.", "kicker,top-pair"),
    add("double-gutshot", "Double gutshot", "Post-flop", "A straight draw with two different interior ranks that each make a straight.", "Unlike an ordinary gutshot, two distinct rank values can complete it.", "gutshot,straight-draw"),
    add("nut-draw", "Nut draw", "Post-flop", "A draw that would make the strongest possible hand of its type if completed, although the board may change.", "A♠ Q♠ on K♠ 7♠ 2♦ offers a draw to the nut flush.", "nut-flush,flush-draw"),
    add("brick", "Brick", "Poker slang", "A turn or river card that does not appear to improve the important hands or draws.", "A harmless-looking offsuit 2 is a brick on some boards.", "turn,river"),
    add("bink", "Bink", "Poker slang", "To hit a desired card or win a key pot, often used excitedly.", "You bink your flush on the river.", "flush-draw"),
    add("multiway-pot", "Multiway pot", "Game structure", "A pot contested by three or more players who remain in the hand.", "A four-way flop is a multiway situation.", "heads-up"),
    add("short-handed", "Short-handed", "Game structure", "A table with fewer players than its standard full complement, such as three or four at a six-max table.", "Ranges often expand as a game becomes short-handed.", "six-max"),
    add("deep-stacked", "Deep-stacked", "Game structure", "Playing with a relatively large stack compared with the blinds.", "200 BB stacks create different post-flop options than 30 BB stacks.", "stack-depth"),
    add("short-stacked", "Short-stacked", "Game structure", "Playing with a relatively small stack compared with the blinds.", "A short-stacked player may face earlier all-in decisions.", "stack-depth"),
    add("chip-leader", "Chip leader", "Tournaments", "The player with the largest stack in a tournament or at a specific table.", "The chip leader covers every other stack at the table.", "stack"),
    add("starting-stack", "Starting stack", "Game structure", "The chips you begin a session, hand, or tournament with, depending on context.", "A tournament awards the same starting stack to each entrant.", "buy-in"),
    add("forced-bet", "Forced bet", "Game structure", "A mandatory contribution that helps seed the pot, including blinds and antes.", "The two blinds are forced bets in this app.", "blinds,ante"),
    add("missed-blind", "Missed blind", "Cash games", "A blind you owe under a room’s rules after sitting out or missing your scheduled blind.", "Returning to a cash-game table can require posting a missed blind.", "blinds"),
    add("button-straddle", "Button straddle", "Cash games", "A straddle posted by the button when the room permits it; precise action rules vary.", "Some cash rooms permit a voluntary button straddle.", "straddle,button"),
    add("dead-money", "Dead money", "Math & odds", "Chips already in the pot that no longer belong to a player who can actively defend them, or money likely to be uncontested.", "Antes and folded players’ contributions add dead money.", "pot,ante"),
    add("uncalled-bet", "Uncalled bet", "Betting", "The part of a wager no opponent matches; this excess is normally returned to the bettor.", "A $70 shove against a $40 stack returns the uncalled $30 when appropriate.", "all-in,side-pot"),
    add("string-bet", "String bet", "Rules & hands", "An improperly made raise executed in separate motions without proper prior declaration; live rules vary.", "Announce the raise amount clearly to avoid a string bet.", "raise"),
    add("out-of-turn", "Out of turn", "Rules & hands", "Acting before it is your turn, which can affect the action under house rules.", "A player mistakenly says “call” while another player is still deciding.", "action"),
    add("table-talk", "Table talk", "Poker slang", "Conversation at the table, including banter and sometimes discussion of hands within house rules.", "Some players engage in table talk to gather information.", "tell"),
    add("run-it-twice", "Run it twice", "Cash games", "An agreed all-in arrangement where the remaining board is dealt twice and the pot is divided into separate runouts, subject to room rules.", "Two players all-in on the flop agree to run it twice.", "all-in,variance"),
    add("bomb-pot", "Bomb pot", "Cash games", "A house-approved hand where players contribute a set amount before the flop and action usually starts on the flop; formats vary.", "Everyone antes $5 to play a bomb pot.", "ante,flop"),
    add("min-buy-in", "Minimum buy-in", "Cash games", "The fewest chips a room allows you to bring to a specific cash-game table.", "A $1/$2 table may have a posted minimum buy-in.", "buy-in"),
    add("max-buy-in", "Maximum buy-in", "Cash games", "The most chips a room allows you to bring to a specific cash-game table initially or when topping up.", "Room-specific caps may differ at the same blind level.", "buy-in"),
    add("top-up", "Top-up", "Cash games", "Adding chips to a stack between hands, subject to the maximum buy-in and room rules.", "You top up your $1/$2 stack before the next deal.", "max-buy-in"),
    add("reload", "Reload", "Cash games", "Buying more chips after your stack decreases or runs out, following the room’s rules.", "A player reloads after losing an all-in pot.", "top-up,bankroll"),
    add("session", "Session", "Cash games", "One stretch of cash-game play, often used for tracking hours and results.", "A four-hour cash session contains many individual hands.", "win-rate"),
    add("tip", "Tip", "Cash games", "An optional gratuity for a dealer or other staff, subject to local practice.", "Players may tip a live dealer after winning a pot.", "cash-game"),
    add("bad-beat-jackpot", "Bad-beat jackpot", "Cash games", "A room promotion that pays qualifying participants when a very strong hand loses to an even stronger hand under stated rules.", "Specific minimum losing hands and qualification conditions vary.", "bad-beat"),
    add("freezeout", "Freezeout", "Tournaments", "A tournament where you cannot rebuy or re-enter after losing all your chips.", "Busting from a freezeout ends your participation.", "tournament"),
    add("rebuy", "Rebuy", "Tournaments", "An additional tournament purchase of chips while still eligible under the event rules.", "A rebuy event may permit extra chips during an early registration period.", "tournament"),
    add("re-entry", "Re-entry", "Tournaments", "Entering a tournament again after elimination when the event permits it, usually with a fresh starting stack.", "A player re-enters during late registration.", "tournament"),
    add("add-on", "Add-on", "Tournaments", "An optional tournament purchase of additional chips at a designated time, if permitted.", "An add-on may be offered at the end of the rebuy period.", "rebuy"),
    add("late-registration", "Late registration", "Tournaments", "A period after tournament start during which new entries or re-entries are still accepted.", "Some events allow late registration for several blind levels.", "tournament,re-entry"),
    add("satellite", "Satellite", "Tournaments", "A qualifying tournament that awards entry to another event rather than only direct cash prizes.", "Winning a satellite seat qualifies you for a larger tournament.", "tournament"),
    add("sit-and-go-sng", "Sit-and-go (SNG)", "Tournaments", "A tournament that starts once the required number of entrants or seats is filled rather than at a fixed time.", "A nine-player sit-and-go begins when the table fills.", "tournament"),
    add("multi-table-tournament-mtt", "Multi-table tournament (MTT)", "Tournaments", "A tournament that runs across multiple tables, consolidating players as participants are eliminated.", "A large Sunday event may be an MTT.", "tournament"),
    add("bounty-tournament", "Bounty tournament", "Tournaments", "A tournament that pays a prize for eliminating other entrants as well as any regular placement payouts.", "Knocking out a player can earn a bounty.", "tournament"),
    add("progressive-knockout-pko", "Progressive knockout (PKO)", "Tournaments", "A bounty format where eliminating players generally awards some bounty immediately and increases your own bounty; exact rules vary.", "Your displayed bounty rises after you knock someone out.", "bounty-tournament"),
    add("bubble", "Bubble", "Tournaments", "The stage just before players reach a paid finishing position or another major payout milestone.", "One elimination remains before everyone left is in the money.", "in-the-money-itm"),
    add("in-the-money-itm", "In the money (ITM)", "Tournaments", "Finishing in a tournament place that pays a prize under its payout schedule.", "Reaching ITM does not guarantee profit after entry costs.", "bubble"),
    add("min-cash", "Min-cash", "Tournaments", "The smallest paid tournament prize or finishing position eligible for that prize.", "Surviving the bubble may lock up a min-cash.", "in-the-money-itm"),
    add("pay-jump", "Pay jump", "Tournaments", "An increase in the tournament payout received by moving to a higher finishing position.", "Pay jumps can affect risk-taking near the final table.", "icm-independent-chip-model"),
    add("final-table", "Final table", "Tournaments", "The last table remaining in a multi-table tournament.", "Nine players may remain at a nine-handed final table.", "multi-table-tournament-mtt"),
    add("icm-independent-chip-model", "Independent Chip Model (ICM)", "Tournaments", "A model estimating tournament prize equity from chip stacks and payout structure; chips do not map linearly to prize money.", "ICM can affect calling ranges near significant pay jumps.", "pay-jump,equity"),
    add("shove-fold", "Shove/fold", "Tournaments", "A simplified short-stack strategy focused largely on going all-in or folding before the flop.", "At low stack depths, shove/fold charts become relevant.", "all-in,short-stacked"),
    add("underbet", "Underbet", "Betting", "A bet that is small compared with the current pot, with exact conventions varying.", "A $10 bet into a $100 pot is an underbet.", "bet-sizing"),
    add("block-bet", "Block bet", "Betting", "A relatively small out-of-position bet, often designed to set a manageable price for reaching showdown.", "A small river lead may function as a block bet.", "out-of-position-oop,showdown"),
    add("delayed-cbet", "Delayed c-bet", "Betting", "A continuation bet made on a later street after declining to c-bet the previous street.", "The pre-flop raiser checks the flop and bets the turn.", "continuation-bet-c-bet,turn"),
    add("check-through", "Check through", "Betting", "A betting round that ends without any player betting.", "Both players check and the flop checks through.", "check"),
    add("snap-call", "Snap-call", "Poker slang", "An immediate call made with little visible hesitation.", "Someone snap-calls your river shove.", "call"),
    add("snap-fold", "Snap-fold", "Poker slang", "An immediate fold made with little visible hesitation.", "You snap-fold a hopeless river hand to a bet.", "fold"),
    add("hero-call", "Hero call", "Poker slang", "A difficult call made with a relatively weak hand because you suspect the opponent is bluffing.", "You call a river overbet with third pair after reading the situation.", "bluff-catcher"),
    add("hero-fold", "Hero fold", "Poker slang", "A difficult fold of a strong-looking hand because you believe the opponent has an even stronger holding.", "You fold a small full house on an extremely threatening board.", "fold"),
    add("pot-committed", "Pot-committed", "Strategy", "A situation in which the pot size and remaining stacks make folding unusually costly or strategically difficult; not an absolute rule.", "A player with one-tenth pot remaining may be pot-committed in many spots.", "stack-to-pot-ratio-spr"),
    add("dominated-hand", "Dominated hand", "Pre-flop", "A hand that shares a high card with another holding but has an inferior kicker and limited ways to improve past it.", "A-J is dominated by A-K before the flop in many runouts.", "kicker"),
    add("range-bet", "Range bet", "Strategy", "A strategy of betting nearly all or all of your relevant range at a particular node, often with one small sizing.", "A pre-flop raiser may use a frequent small c-bet on some dry flops.", "range,continuation-bet-c-bet"),
    add("capped-range", "Capped range", "Strategy", "A range that contains few or no combinations of the strongest hands in the current situation.", "Prior passive actions can sometimes make a range appear capped.", "range,nuts"),
    add("uncapped-range", "Uncapped range", "Strategy", "A range that can still credibly contain some of the strongest hands on the current board.", "The aggressor may retain nut hands on particular turn runouts.", "range,nuts"),
    add("overfold", "Overfold", "Strategy", "Folding more often than an appropriate benchmark strategy in the same situation.", "Overfolding to river bluffs can be costly against frequent bluffers.", "minimum-defense-frequency-mdf"),
    add("underbluff", "Underbluff", "Strategy", "Bluffing less often than an appropriate baseline in a given situation.", "An opponent who rarely river bluffs may have an underbluffed river betting range.", "bluff"),
    add("overbluff", "Overbluff", "Strategy", "Bluffing more often than an appropriate baseline in a given situation.", "Frequent large river bluffs can shift bluff-catching incentives.", "bluff,bluff-catcher"),
    add("rule-of-four-and-two", "Rule of 4 and 2", "Math & odds", "A rough shortcut for approximating drawing chances from outs: multiply by about 4 with two cards to come, or 2 with one card to come.", "Nine flush outs are roughly 36% with two cards to come; actual probabilities differ.", "out,equity"),
    add("donkey-donk", "Donkey (donk)", "Poker slang", "A derogatory poker term for a player perceived to make very poor or ill-considered plays. A “donk bet” is a separate technical term and does not automatically insult a player.", "Calling someone a donk is usually an insult, not a rigorous assessment of skill.", "fish,donk-bet"),
    add("fish", "Fish", "Poker slang", "Informal label for a player perceived to be inexperienced or losing; can be insulting.", "A table regular might call a novice a fish.", "shark,whale"),
    add("shark", "Shark", "Poker slang", "Informal label for a skilled player, especially one who seeks profitable games.", "A shark might study ranges and player tendencies.", "fish"),
    add("whale", "Whale", "Poker slang", "Poker slang for a player who wagers very large amounts and is perceived as a particularly attractive opponent, often a wealthy recreational player; can be insulting.", "A high-stakes table may attract a whale.", "fish"),
    add("regular-reg", "Regular (reg)", "Poker slang", "A player who appears frequently in a poker game or player pool; does not necessarily imply skill.", "A cash-game reg plays at the same stakes most nights.", "recreational-player"),
    add("recreational-player", "Recreational player (rec)", "Poker slang", "Someone who plays mainly for enjoyment rather than professional income; ability can vary widely.", "A strong recreational player may study strategy but play occasionally.", "regular-reg"),
    add("professional-pro", "Professional (pro)", "Poker slang", "A person who plays poker professionally for a substantial part of their income.", "A poker pro may track their long-term win rate.", "win-rate"),
    add("grinder", "Grinder", "Poker slang", "A player who puts in many hours or many hands, often pursuing steady results.", "A tournament grinder enters many events each week.", "variance"),
    add("maniac", "Maniac", "Poker slang", "A very loose and extremely aggressive player who often bets or raises.", "A maniac might raise a broad range of hands.", "aggressive-opponent"),
    add("rock", "Rock", "Poker slang", "A very tight and conservative player who plays few hands and typically avoids marginal spots.", "A rock may wait a long time before entering a pot.", "nit"),
    add("tight-aggressive-tag", "Tight-aggressive (TAG)", "Strategy", "A style that plays relatively few starting hands but bets or raises assertively with the hands selected.", "A TAG raises strong openings rather than routinely limping.", "tight-range,aggressive-opponent"),
    add("loose-aggressive-lag", "Loose-aggressive (LAG)", "Strategy", "A style that enters relatively many pots and frequently uses bets and raises to apply pressure.", "A LAG may open wide ranges from late position.", "wide-range,aggressive-opponent"),
    add("loose-passive", "Loose-passive", "Strategy", "A style that plays many hands but tends to call rather than bet or raise.", "A loose-passive player may enter many pots by calling.", "calling-station,wide-range"),
    add("tight-passive", "Tight-passive", "Strategy", "A style that plays relatively few hands and often calls or checks rather than applying pressure.", "A tight-passive player may call a narrow range.", "nit,passive-opponent"),
    add("tilt", "Tilt", "Poker slang", "Emotion-driven play that interferes with normal decision-making, often after a frustrating outcome.", "A bad beat can trigger tilt and impulsive bets.", "bad-beat,bankroll-management"),
    add("bad-beat", "Bad beat", "Poker slang", "A loss in which a previously strong or heavily favored holding is overtaken or loses unexpectedly.", "A large favorite loses when the river completes an unlikely draw.", "variance"),
    add("cooler", "Cooler", "Poker slang", "A situation where two strong holdings collide and one player is likely to lose a large pot under ordinary play.", "Set over set can be a cooler.", "set,variance"),
    add("suck-out", "Suck out", "Poker slang", "To win after being a substantial underdog by catching favorable card(s).", "A two-outer on the river is often called a suck-out.", "out,bad-beat"),
    add("heater", "Heater", "Poker slang", "An extended streak of unusually favorable poker results.", "Running hot across several sessions is a heater.", "variance"),
    add("run-good", "Run good", "Poker slang", "To experience unusually favorable short-term outcomes relative to underlying chances.", "Winning multiple coin flips in succession is running good.", "variance"),
    add("run-bad", "Run bad", "Poker slang", "To experience unusually unfavorable short-term outcomes relative to underlying chances.", "A stretch of missed draws may feel like running bad.", "variance"),
    add("spew", "Spew", "Poker slang", "To give away chips through poorly justified or impulsive plays.", "Calling repeated large bets without a reason may be described as spew.", "tilt"),
    add("punt", "Punt", "Poker slang", "To lose many chips with an especially poor or unnecessarily risky decision.", "An unjustified river shove is sometimes called a punt.", "spew"),
    add("angle-shooting", "Angle shooting", "Poker slang", "Using misleading, borderline rule-abusing behavior to gain an advantage without necessarily breaking an explicit rule.", "A deceptive chip movement intended to provoke a reaction may be angle shooting.", "string-bet"),
    add("slowroll", "Slowroll", "Poker slang", "Unnecessarily delaying the reveal of an almost-certain winning hand at showdown, generally considered poor etiquette.", "Holding the nuts while pretending to lose before showing is a slowroll.", "showdown,nuts"),
    add("hit-and-run", "Hit and run", "Poker slang", "Leaving shortly after winning a sizable pot or arriving briefly and departing with profits; etiquette views vary.", "A player wins a large pot and leaves after one orbit.", "cash-game"),
    add("poker-face", "Poker face", "Poker slang", "Facial expression and demeanor intended not to reveal information about a hand.", "A poker face matters less against a bot with no visual reads.", "tell"),
    add("tell", "Tell", "Poker slang", "A behavioral cue that may indicate something about a player’s hand, intentions or emotional state; tells are fallible.", "A repeated timing pattern may be treated as a possible tell.", "poker-face"),
    add("read", "Read", "Poker slang", "An inference about an opponent’s likely cards or strategy based on observed behavior and game context.", "A player forms a read after watching several betting lines.", "range,tell"),
    add("soul-read", "Soul read", "Poker slang", "Exaggerated slang for an unusually accurate guess about an opponent’s holding or intention.", "Calling a bluff with high card may be celebrated as a soul read.", "read,hero-call"),
    add("pocket-rockets", "Pocket rockets", "Poker slang", "Nickname for pocket aces (AA).", "You are dealt A♠ A♥ pre-flop.", "pocket-pair"),
    add("big-slick", "Big Slick", "Poker slang", "Nickname for A-K, suited or offsuit.", "A♠ K♣ is Big Slick offsuit.", "starting-hand-notation"),
    add("cowboys", "Cowboys", "Poker slang", "Nickname for pocket kings (KK).", "K♠ K♥ is a pair of Cowboys.", "pocket-pair"),
    add("snowmen", "Snowmen", "Poker slang", "Nickname for pocket eights (88).", "8♠ 8♦ is a pair of Snowmen.", "pocket-pair"),
    add("ducks", "Ducks", "Poker slang", "Nickname for pocket twos (22).", "2♠ 2♦ is a pair of Ducks.", "pocket-pair"),
    add("degen", "Degenerate (degen)", "Poker slang", "Poker slang for a person who takes excessive gambling risks or enthusiastically gambles. Often used self-deprecatingly, but can be insulting.", "Someone jokes about chasing every draw and calls themselves a degen.", "variance,bankroll-management"),
    add("rake", "Rake", "Cash games", "A fee collected by the card room from eligible pots or time charges, depending on the house rules.", "Rake makes some marginal calls less profitable.", "cash-game"),
    add("rakeback", "Rakeback", "Cash games", "Rewards or rebates that return part of the rake paid, when a room offers them.", "Rakeback may affect a player's net cash-game results.", "rake"),
    add("straddle", "Straddle", "Cash games", "An extra blind-like wager, commonly posted before cards are dealt; details and action order vary by room.", "An optional UTG straddle can increase the effective stakes.", "ante"),
    add("table-stakes", "Table stakes", "Cash games", "A rule limiting wagers to chips the player has at the table when the hand begins, subject to house rules.", "You generally cannot pull cash from your pocket during a live hand.", "all-in"),
    add("bankroll", "Bankroll", "Cash games", "Money set aside for playing poker rather than a single table stack.", "A bankroll is separate from a single buy-in.", "buy-in"),
    add("bankroll-management", "Bankroll management", "Cash games", "Selecting stakes and buy-ins with consideration for risk of losing your playing funds and natural variance.", "A player's reserve matters because even positive-EV play has downswings.", "variance"),
    add("downswing", "Downswing", "Cash games", "A stretch of unfavorable short-term poker results.", "Losing several sessions can happen despite sound decisions.", "variance"),
    add("win-rate", "Win rate", "Cash games", "Average money or big blinds won over a specified number of hands or hours; noisy over small samples.", "A cash-game tracker may report BB won per 100 hands.", "variance"),
    add("bb-100", "BB/100", "Cash games", "Big blinds won per 100 hands, a common cash-game win-rate unit.", "A 5 BB/100 sample rate means five big blinds per 100 hands over that sample.", "win-rate"),
    add("pot-limit", "Pot limit", "Cash games", "A structure where maximum raises depend on the pot size; included to distinguish it from No-Limit.", "Pot-Limit Omaha uses pot-limit betting.", "no-limit"),
    add("time-rake", "Time rake", "Cash games", "A cash-game fee collected periodically per player instead of taken from every pot, depending on room rules.", "Some rooms collect a fixed charge every half-hour.", "rake")
  ))
  out[order(tolower(out$term)), , drop=FALSE]
})
stopifnot(!anyDuplicated(poker_glossary$id))

GLOSSARY_CATEGORIES <- c("Rules & hands", "Game structure", "Betting",
                         "Pre-flop", "Post-flop", "Math & odds",
                         "Strategy", "Cash games", "Tournaments", "Poker slang")

# Specific multi-word concepts are linked automatically in instructional copy.
# Each paragraph is capped at a few links so the app stays readable.
GLOSSARY_INLINE_TERMS <- c(
  "poker face" = "poker-face",
  "cash tournament" = "tournament",
  "donkey" = "donkey-donk",
  "degen" = "degen",
  "angle shooting" = "angle-shooting",
  "slowroll" = "slowroll",
  "hero call" = "hero-call",
  "multiway pot" = "multiway-pot",
  "snap-call" = "snap-call",
  "minimum defense frequency" = "minimum-defense-frequency-mdf",
  "starting-hand notation" = "starting-hand-notation",
  "reverse implied odds" = "reverse-implied-odds",
  "frequency-dependent" = "frequency",
  "aggressive opponent" = "aggressive-opponent",
  "equity realization" = "equity-realization",
  "suited connectors" = "suited-connector",
  "exact enumeration" = "exact-enumeration",
  "suited connector" = "suited-connector",
  "defending ranges" = "defending-range",
  "passive opponent" = "passive-opponent",
  "community cards" = "community-cards",
  "showdown equity" = "showdown-equity",
  "effective stack" = "effective-stack",
  "defending range" = "defending-range",
  "3-betting range" = "3-bet-range",
  "out of position" = "out-of-position-oop",
  "range advantage" = "range-advantage",
  "calling station" = "calling-station",
  "opening ranges" = "opening-range",
  "mixed strategy" = "mixed-strategy",
  "bluff-catchers" = "bluff-catcher",
  "expected value" = "expected-value-ev",
  "opening range" = "opening-range",
  "nut advantage" = "nut-advantage",
  "board texture" = "board-texture",
  "straight draw" = "straight-draw",
  "backdoor draw" = "backdoor-draw",
  "bluff-catcher" = "bluff-catcher",
  "value betting" = "value-bet",
  "double barrel" = "double-barrel",
  "triple barrel" = "triple-barrel",
  "effective stacks" = "effective-stack",
  "opponent range" = "range",
  "pot control" = "pot-control",
  "pocket pairs" = "pocket-pair",
  "GTO-inspired" = "gto-inspired",
  "paired board" = "paired-board",
  "implied odds" = "implied-odds",
  "merged range" = "merged-range",
  "fold equity" = "fold-equity",
  "stack depth" = "stack-depth",
  "pocket pair" = "pocket-pair",
  "3-bet range" = "3-bet-range",
  "small blind" = "small-blind-sb",
  "in position" = "in-position-ip",
  "Monte Carlo" = "monte-carlo-simulation",
  "hole cards" = "hole-cards",
  "open range" = "opening-range",
  "the button" = "button",
  "flush draw" = "flush-draw",
  "open-ended" = "open-ended-straight-draw-oesd",
  "value bets" = "value-bet",
  "thin value" = "thin-value-bet",
  "semi-bluff" = "semi-bluff",
  "full house" = "full-house",
  "bet sizing" = "bet-sizing",
  "cash games" = "cash-game",
  "3-betting" = "3-bet",
  "4-betting" = "4-bet",
  "big blind" = "big-blind-bb",
  "SB/button" = "button",
  "connected" = "connected-board",
  "made hand" = "made-hand",
  "value bet" = "value-bet",
  "polarized" = "polarized-range",
  "cash game" = "cash-game",
  "heads-up" = "heads-up",
  "no-limit" = "no-limit",
  "pot odds" = "pot-odds",
  "two-tone" = "two-tone",
  "blockers" = "blocker",
  "two pair" = "two-pair",
  "side pot" = "side-pot",
  "showdown" = "showdown",
  "variance" = "variance",
  "pre-flop" = "pre-flop",
  "offsuit" = "offsuit",
  "rainbow" = "rainbow-board",
  "gutshot" = "gutshot",
  "blocker" = "blocker",
  "kickers" = "kicker",
  "overbet" = "overbet",
  "suited" = "suited",
  "3-bets" = "3-bet",
  "4-bets" = "4-bet",
  "blinds" = "blinds",
  "equity" = "equity",
  "kicker" = "kicker",
  "triple" = "trips",
  "3-bet" = "3-bet",
  "4-bet" = "4-bet",
  "draws" = "draw",
  "outs" = "out",
  "quads" = "quads",
  "trips" = "trips",
  "antes" = "ante",
  "c-bet" = "continuation-bet-c-bet",
  "river" = "river",
  "rake" = "rake",
  "combos" = "combo",
  "combo" = "combo",
  "turn" = "turn",
  "flop" = "flop",
  "MDF" = "minimum-defense-frequency-mdf",
  "GTO" = "gto-game-theory-optimal",
  "SPR" = "stack-to-pot-ratio-spr"
)

# Keep explanatory text readable. Vocabulary is defined in the dedicated
# section glossaries and searchable full Glossary tab, not linked in prose.
glossary_link <- function(id, label=NULL, extra_class="", interactive=FALSE) {
  i <- match(id, poker_glossary$id)
  stopifnot(!is.na(i))
  if (is.null(label)) label <- poker_glossary$term[i]
  # Previously these were clickable info icons. Without inline linking they
  # should disappear, not leave unexplained symbols in field labels.
  if (!interactive) {
    if (label %in% c("ⓘ", "↗ Hand rankings")) return(NULL)
    return(label)
  }
  tags$a(href="#", class=paste("poker-term-link", extra_class),
         `data-glossary-term`=id,
         title=paste("Open Glossary:", poker_glossary$term[i]),
         label)
}

# Retain the old function interface for existing static/dynamic explanations,
# but do not inject links into every occurrence of poker terminology.
glossary_inline <- function(text, max_links=4L) text

STREET_GLOSSARY <- list(
  "Pre-Flop"=c("starting-hand-notation","suited","offsuit","pocket-pair",
               "suited-connector","opening-range","defending-range","3-bet",
               "mixed-strategy","position","effective-stack","big-blind-bb"),
  "Flop"=c("board-texture","two-tone","rainbow-board","made-hand",
           "flush-draw","straight-draw","backdoor-draw","continuation-bet-c-bet",
           "range-advantage","equity","out","pot-odds"),
  "Turn"=c("double-barrel","blocker","pot-odds","equity-realization",
           "implied-odds","reverse-implied-odds","semi-bluff","flush-draw",
           "effective-stack","stack-to-pot-ratio-spr","showdown-equity","fold-equity"),
  "River"=c("showdown","hand-ranking","kicker","bluff-catcher",
            "thin-value-bet","blocker","polarized-range","value-bet",
            "pot-odds","minimum-defense-frequency-mdf","full-house","quads")
)
stopifnot(all(unlist(STREET_GLOSSARY,use.names=FALSE) %in% poker_glossary$id))

street_glossary_ui <- function(street) {
  term_ids <- STREET_GLOSSARY[[street]]
  stopifnot(length(term_ids)>0L)
  div(class="preflop-card street-glossary",
      div(class="street-glossary-head",
          h4(paste0(street," · Quick glossary")),
          tags$a(href="#",class="poker-open-glossary", "Browse full glossary →")),
      tags$details(class="lab-more-details",
                   tags$summary(sprintf("Show %d key terms", length(term_ids))),
                   div(class="street-glossary-grid",
                       lapply(term_ids, function(id) {
                         e <- poker_glossary[match(id,poker_glossary$id),,drop=FALSE]
                         div(class="street-glossary-item",
                             strong(e$term),
                             span(e$definition))
                       }))))
}

glossary_entry_ui <- function(e, selected=FALSE) {
  refs <- strsplit(e$related, ",", fixed=TRUE)[[1L]]
  refs <- refs[nzchar(refs) & refs %in% poker_glossary$id]
  tags$details(id=paste0("glossary-entry-",e$id),
               class=paste("glossary-entry",if (selected) "glossary-target" else ""),
               open=if (selected) TRUE else NULL,
               tags$summary(span(class="glossary-term-name",e$term),
                            span(class="glossary-category-pill",e$category)),
               div(class="glossary-entry-content",
                   p(e$definition),
                   if (nzchar(e$example))
                     p(class="glossary-example",tags$b("Example: "),e$example),
                   if (length(refs))
                     div(class="glossary-related",tags$b("Related: "),
                         tagList(lapply(seq_along(refs), function(i) {
                           tagList(if(i>1L) " · " else NULL,glossary_link(refs[i], interactive=TRUE))
                         }))))
  )
}

poker_glossary_tab <- function() {
  tabPanel("Glossary", value="Glossary",
           fluidPage(class="preflop-page glossary-page",
                     div(class="preflop-card",
                         div(class="glossary-title-row",
                             div(h2("♠ Glossary",style="margin-top:0;"),
                                 p("From basic rules to table slang: search a term or choose a category. Slang can be playful—or rude—depending on context."))),
                         fluidRow(
                           column(6,textInput("glossary_search","Search terms",placeholder="Try: range, pot odds, bluff-catcher...",width="100%")),
                           column(4,selectInput("glossary_category","Category",
                                                choices=c("All categories"="ALL",setNames(GLOSSARY_CATEGORIES,GLOSSARY_CATEGORIES)),
                                                selected="ALL",width="100%")),
                           column(2,selectInput("glossary_letter","A–Z",
                                                choices=c("All"="ALL", "#"="#", setNames(LETTERS,LETTERS)),
                                                selected="ALL",width="100%"))
                         ),
                         textOutput("glossary_count")),
                     uiOutput("glossary_entries")
           ))
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
                         h3(if (street == "Flop") "♠ Flop: Equity Lab"
                            else if (street == "Turn") "♠ Turn: River Outcome Explorer"
                            else "♠ River: Showdown Analysis"),
                         p(glossary_inline(if (street == "River")
                           "Choose your cards to see the exact opponent holdings that beat you."
                           else if (street == "Turn")
                             "Choose your cards, inspect every possible river, then explore any final board."
                           else "Choose your cards to calculate hand probabilities and showdown equity.")),
                         tags$details(class="lab-more-details",
                                      tags$summary("Model assumptions & limitations"),
                                      p(glossary_inline(paste0("Equity assumes a showdown without future betting. Opponent ranges are ",
                                                               "heuristic, and strategic notes are not solver-generated GTO actions."))))),
                     fluidRow(
                       column(4,
                              div(class="preflop-card",
                                  h4("1 · Enter Cards & Select Opponent"),
                                  actionButton(paste0(prefix,"_load"),"Use cards from Practice Game",
                                               class="btn-default",width="100%"),
                                  helpText("Copies your visible hole cards and this street's board from the game, if available."),
                                  fluidRow(column(6,select_card("hero1","Your card 1",default_hero[1])),
                                           column(6,select_card("hero2","Your card 2",default_hero[2]))),
                                  h4("Community cards"),
                                  div(class="lab-card-grid", board_inputs),
                                  tags$hr(),
                                  selectInput(paste0(prefix,"_villain"),
                                              tagList("Opponent model ",glossary_link("range","ⓘ")),
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
                                               p(glossary_inline(paste0("Tight and wide are simplified estimates based on pre-flop hand strength; ",
                                                                        "they do not model a real opponent's decisions. Every possible two-card ",
                                                                        "combination in the chosen range is treated equally.")))),
                                  # River equity enumerates every legal opponent holding;
                                  # Monte Carlo trials apply only before the river.
                                  if (street != "River")
                                    selectInput(paste0(prefix,"_n"),
                                                tagList("Monte Carlo trials (when needed) ",glossary_link("monte-carlo-simulation","ⓘ")),
                                                c("300 · quick"=300,"750 · standard"=750,"1500 · detailed"=1500),
                                                selected=750),
                                  actionButton(paste0(prefix,"_run"),"Calculate probabilities",
                                               class="btn-success btn-lg",width="100%"),
                                  br(), br(),
                                  uiOutput(paste0(prefix,"_validation"))
                              )
                       ),
                       column(8,
                              div(class="preflop-card",
                                  h4("2 · Showdown Equity"),
                                  uiOutput(paste0(prefix,"_results")),
                                  # The original Flop chart is visible by default. Unlike
                                  # the draft, no extra click is needed to see probabilities.
                                  if (street == "Flop")
                                    plotOutput(paste0(prefix,"_distribution"),height="295px"),
                                  if (street == "Turn")
                                    tags$details(class="lab-more-details",
                                                 tags$summary("Full final-hand probability chart"),
                                                 plotOutput(paste0(prefix,"_distribution"),height="270px"))
                              ),
                              if (street == "Turn")
                                div(class="preflop-card",
                                    h4("3 · River Outcome Explorer"),
                                    uiOutput(paste0(prefix,"_explorer"))
                                )
                              else
                                div(class="preflop-card",
                                    h4(if (street == "River") "3 · Opponent Hands That Beat Yours"
                                       else "3 · Hand Analysis & Strategy"),
                                    uiOutput(paste0(prefix,"_strategy"))
                                )
                       )
                     ),
                     street_glossary_ui(street)
           )
  )
}

# ============================================================
# UI
# ============================================================

ui <- navbarPage(
  title = "Texas Hold'em: Heads-Up",
  id = "poker_nav",
  # The tab bar stays visible even when scrolling deep into a strategy tab.
  position = "fixed-top",
  header = tags$head(
    tags$script(HTML("
      // Click next-card tiles to explore the next street with the same hand.
      $(document).on('click', 'button.lab-explorer-tile', function(ev) {
        ev.preventDefault();
        Shiny.setInputValue('lab_transfer_card', {
          from: $(this).attr('data-from'),
          card: $(this).attr('data-card'),
          nonce: Date.now().toString() + Math.random().toString()
        }, {priority: 'event'});
      });
      // Delegated handlers also work with Shiny's dynamically rendered text.
      $(document).on('click', 'a.poker-term-link, a.poker-open-glossary', function(ev) {
        ev.preventDefault();
        Shiny.setInputValue('glossary_open', {
          id: $(this).attr('data-glossary-term') || '',
          nonce: Date.now().toString() + Math.random().toString()
        }, {priority: 'event'});
      });
      Shiny.addCustomMessageHandler('poker_glossary_focus', function(msg) {
        var attempt = 0;
        function focusEntry() {
          if (!msg.id) { window.scrollTo({top: 0, behavior: 'smooth'}); return; }
          var el = document.getElementById('glossary-entry-' + msg.id);
          if (!el) {
            if (++attempt < 24) setTimeout(focusEntry, 125);
            return;
          }
          el.open = true;
          el.classList.add('glossary-target');
          el.scrollIntoView({behavior: 'smooth', block: 'center'});
        }
        setTimeout(focusEntry, 150);
      });
    ")),
    tags$style(HTML("
      /* Next-street explorers are compact and keyboard-clickable. */
      .lab-explorer-headline {display:flex;align-items:center;justify-content:space-between;
          flex-wrap:wrap;gap:8px;margin-bottom:6px;}
      .lab-explorer-headline h4 {font-size:16px;font-weight:800;margin:0;color:#233a31;}
      .lab-explorer-headline small,.lab-explorer-help {color:#586776;font-size:12px;}
      .lab-explorer-group {border:1px solid #dce6df;border-radius:8px;margin:10px 0;
          background:#f9fcf9;overflow:hidden;}
      .lab-explorer-group summary {cursor:pointer;padding:12px 14px;display:flex;justify-content:space-between;
          align-items:center;background:#eef7f0;color:#234b35;}
      .lab-explorer-count {font-size:12px;background:#dceee1;border-radius:14px;padding:4px 9px;}
      .lab-explorer-tiles {display:grid;grid-template-columns:repeat(auto-fill,minmax(145px,1fr));gap:9px;padding:12px;}
      .lab-explorer-tile {background:#fff;border:1px solid #d4e1d9;border-radius:8px;
          display:flex;flex-direction:column;align-items:flex-start;text-align:left;padding:10px;
          min-height:92px;gap:3px;color:#213d2e;cursor:pointer;transition:border-color .15s,background .15s;}
      .lab-explorer-tile:hover,.lab-explorer-tile:focus {border-color:#227849;background:#e9f7ee;
          outline:2px solid #b8dfc6;outline-offset:1px;}
      .lab-explorer-rank {font-size:21px;font-weight:900;line-height:1.1;}
      .lab-explorer-rank.red {color:#bf2626;}
      .lab-explorer-hand {font-size:11px;color:#334b3d;line-height:1.3;}
      .lab-explorer-meta {font-size:11px;color:#246c48;}
      .lab-explorer-result {font-size:10px;font-weight:800;letter-spacing:.6px;}
      .lab-explorer-result.win {color:#14633a;}.lab-explorer-result.tie {color:#93630a;}
      .lab-explorer-result.loss {color:#a83232;}
      /* Compact Turn cards: around 7–9 per row at normal desktop width. */
      .lab-explorer-turn .lab-explorer-tiles {
          display:grid;grid-template-columns:repeat(auto-fill,minmax(82px,1fr));
          gap:5px;padding:7px;
      }
      .lab-explorer-turn .lab-explorer-group {margin:6px 0;}
      .lab-explorer-turn .lab-explorer-group summary {padding:7px 10px;}
      .lab-explorer-turn .lab-explorer-tile {
          box-sizing:border-box;min-width:0;min-height:68px;padding:6px 4px;
          gap:2px;align-items:center;text-align:center;border-radius:6px;
      }
      .lab-explorer-turn .lab-explorer-rank {font-size:19px;line-height:1.1;}
      .lab-explorer-turn .lab-explorer-hand {
          display:block;width:100%;min-width:0;max-width:100%;
          overflow:hidden;text-overflow:ellipsis;white-space:nowrap;
          font-size:10px;line-height:1.2;
      }
      .lab-explorer-turn .lab-explorer-result,
      .lab-explorer-turn .lab-explorer-meta {font-size:9px;line-height:1.1;}
      @media (max-width: 650px) {
          .lab-explorer-turn .lab-explorer-tiles {
              grid-template-columns:repeat(auto-fill,minmax(68px,1fr));gap:4px;padding:5px;
          }
          .lab-explorer-turn .lab-explorer-tile {min-height:64px;padding:5px 3px;}
      }
      /* Only terms are clickable; the four labs retain their normal layout. */
      .poker-term-link { color: #13644a; font-weight: 700; text-decoration: underline;
                         text-decoration-style: dotted; text-underline-offset: 2px; cursor: pointer; }
      .poker-term-link:hover, .poker-term-link:focus { color: #07402e; }
      label .poker-term-link {font-size:12px;}
      .glossary-control-label {margin-bottom:5px;}
      .poker-open-glossary { font-weight: 750; font-size: 13px; color: #13644a; }
      .street-glossary-head,.glossary-title-row {display:flex;align-items:center;
                      justify-content:space-between;gap:14px;flex-wrap:wrap;}
      .glossary-hint {color:#667085;font-size:13px;}
      .street-glossary-chips {display:flex;flex-wrap:wrap;gap:7px;margin:11px 0;}
      a.glossary-chip {display:inline-block;background:#edf5f0;border:1px solid #cee2d6;
                       padding:5px 9px;border-radius:6px;font-size:12px;text-decoration:none;}
      .street-glossary-grid {display:grid;grid-template-columns:repeat(auto-fit,minmax(260px,1fr));gap:8px 15px;}
      .street-glossary-item {display:flex;flex-direction:column;gap:3px;border-top:1px solid #e8eceb;padding:9px 0;}
      .street-glossary-item span {font-size:12px;color:#53616a;line-height:1.45;}
      .glossary-page {max-width:1350px;margin:0 auto;}
      .glossary-group {margin-bottom:20px;}
      .glossary-group h3 {font-weight:800;font-size:19px;border-bottom:2px solid #c9ddcf;padding:0 0 7px;}
      .glossary-entry {background:white;border:1px solid #e3e9e6;border-radius:7px;margin:7px 0;
                       overflow:hidden;scroll-margin-top:72px;}
      .glossary-entry summary {cursor:pointer;display:flex;justify-content:space-between;align-items:center;
                               flex-wrap:wrap;gap:8px;padding:12px 16px;}
      .glossary-entry summary:hover {background:#f5faf7;}
      .glossary-term-name {font-weight:800;color:#233a31;}
      .glossary-category-pill {font-size:11px;background:#edf5f0;color:#35694f;border-radius:99px;padding:4px 9px;}
      .glossary-entry-content {padding:0px 16px 13px;line-height:1.55;}
      .glossary-entry-content p {margin:8px 0;}
      .glossary-example {color:#4c5964;}
      .glossary-related {margin-top:7px;font-size:13px;}
      .glossary-target {border:2px solid #2c9a65;box-shadow:0 0 0 3px #ddf5e7;}
      @media(max-width:700px) {.street-glossary-grid {grid-template-columns:1fr;}}
    ")),
    # Account for a multi-line navbar or expanded mobile menu automatically.
    # Only navbar position/spacing is changed; no game/reactive logic is touched.
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function() {
        var bar = document.querySelector('nav.navbar-fixed-top');
        if (!bar) return;
        function updateNavbarSpacing() {
          var offset = Math.ceil(bar.getBoundingClientRect().height) + 14;
          document.documentElement.style.setProperty('--poker-navbar-offset', offset + 'px');
        }
        updateNavbarSpacing();
        if (window.ResizeObserver) {
          new ResizeObserver(updateNavbarSpacing).observe(bar);
        } else {
          window.addEventListener('resize', updateNavbarSpacing);
        }
      });
    ")),
    # Keep the creator credit opposite the app title across all tabs.
    # A small DOM insert places it in Shiny's navbar, not a single tab.
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function() {
        var navbar = document.querySelector('.navbar > .container-fluid, .navbar > .container');
        if (!navbar || navbar.querySelector('.poker-creator-credit')) return;
        var credit = document.createElement('span');
        credit.className = 'poker-creator-credit';
        credit.textContent = 'Created by Louise Oh (2024)';
        navbar.appendChild(credit);
      });
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('poker_action_controls', function(msg) {
        ['fold', 'check', 'call', 'raise', 'raise_amount'].forEach(function(id) {
          var control = document.getElementById(id);
          if (control) control.disabled = !!msg.disabled;
        });
      });
    ")),
    tags$style(HTML("
      :root { --poker-navbar-offset: 70px; }
      body {
        background-color: #f5f7fa;
        /* Prevent fixed navbar from covering the top of every tab. */
        padding-top: var(--poker-navbar-offset);
      }
      html { scroll-padding-top: var(--poker-navbar-offset); }
      .glossary-entry { scroll-margin-top: var(--poker-navbar-offset); }
      /* Subtle attribution at the far-right end of the main navbar. */
      .poker-creator-credit {
        float: right;
        padding: 15px 0;
        margin-left: 18px;
        color: #777;
        font-size: 12px;
        font-weight: 600;
        line-height: 20px;
        white-space: nowrap;
      }
      @media (max-width: 767px) {
        .poker-creator-credit {
          float: none;
          display: block;
          clear: both;
          text-align: right;
          padding: 0 14px 8px;
          margin-left: 0;
        }
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
      .lab-threat-scroll { max-height: 440px; overflow: auto; margin: 8px 0 12px; }
      .lab-threat-table { margin: 0; font-size: 13px; }
      .lab-threat-table thead th { position: sticky; top: 0; background: #edf5f0; z-index: 1; }
      .lab-threat-table th { white-space: nowrap; }
      .lab-threat-table td:first-child { font-weight: 650; }
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
        p("Don't try to exploit the bot. It follows probabilities, not your poker face.", style = "margin-bottom:0;"
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
        h3("♠ Pre-Flop: Starting Hand Strategy"),
        p(
          glossary_inline(paste0("In heads-up No-Limit Hold'em, the button is also the Small Blind. ",
                                 "The SB/button acts first before the flop and last on every post-flop street."))
        ),
        p(
          glossary_inline(paste0("Because there is only one opponent, opening and defending ranges are much wider ",
                                 "than they would be at a full table."))
        ),
        div(
          class = "preflop-note",
          strong("Important: "),
          glossary_inline(paste0("The charts below are simplified GTO-inspired/common-strategy reference charts ",
                                 "for this application. They are not solver-exact equilibrium ranges. Exact ranges ",
                                 "depend on effective stack depth, raise sizes, rake, antes, and the specific game tree."), max_links=5L)
        )
      ),
      fluidRow(
        column(
          6,
          div(
            class = "preflop-card",
            h4("SB / Button — Open Range"),
            p(
              glossary_inline("The SB is first to act pre-flop. In heads-up play, the button can open a very wide range.")
            ),
            div(class = "preflop-scroll", preflop_chart_html(SB_PRE_FLOP)),
            div(class = "preflop-legend",
                preflop_legend_item("pf-open", "Open / Raise"),
                preflop_legend_item("pf-mix", "Mix / Frequency-dependent"),
                preflop_legend_item("pf-fold", "Fold")),
            p(class = "preflop-chart-note",
              glossary_inline(paste0("Above diagonal: suited (98s) · Below diagonal: offsuit (98o) · ",
                                     "Diagonal: pocket pairs (99). Hover over a hand to see its action.")))
          )
        ),
        column(
          6,
          div(
            class = "preflop-card",
            h4("BB — Defend vs. SB Open"),
            p(
              glossary_inline(paste0("The BB has already posted $2 and is defending against only one opponent, ",
                                     "so calling ranges can be quite wide. Stronger holdings can also be used for 3-bets."))
            ),
            div(class = "preflop-scroll", preflop_chart_html(BB_PRE_FLOP)),
            div(class = "preflop-legend",
                preflop_legend_item("pf-3bet", "3-Bet"),
                preflop_legend_item("pf-call", "Call / Defend"),
                preflop_legend_item("pf-mix", "Mix"),
                preflop_legend_item("pf-fold", "Fold")),
            p(class = "preflop-chart-note",
              glossary_inline(paste0("Above diagonal: suited (98s) · Below diagonal: offsuit (98o) · ",
                                     "Diagonal: pocket pairs (99). Hover over a hand to see its action.")))
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
            glossary_inline("the SB/button has to act first pre-flop but has position after the flop.")
          ),
          tags$li(
            strong("Ranges are wide: "),
            glossary_inline(paste0("there is only one opponent, so hands such as weak aces, kings, suited connectors, ",
                                   "and many pairs can have meaningful pre-flop value."))
          ),
          tags$li(
            strong("BB defense is wide: "),
            glossary_inline("the BB gets a favorable price because it has already invested the big blind.")
          ),
          tags$li(
            strong(glossary_link("3-bet","3-bets")," are part of both value and bluff ranges: "),
            glossary_inline("a balanced strategy does not reserve 3-bets exclusively for premium hands.")
          ),
          tags$li(
            strong("Sizing changes the range: "),
            glossary_inline("a larger SB open generally gives the BB a worse price and can change which hands continue.")
          ),
          tags$li(
            strong(glossary_link("stack-depth","Stack depth")," matters: "),
            glossary_inline(paste0("as effective stacks become shorter, more hands approach commitment decisions and ",
                                   "pre-flop all-ins become more relevant."))
          ),
          tags$li(
            strong("Think in frequencies: "),
            glossary_inline("some borderline hands can mix between raising, calling, and folding rather than always taking one action.")
          ),
          tags$li(
            strong("Do not evaluate a decision only by the outcome: "),
            glossary_inline("short-term poker results contain substantial variance, so a sound pre-flop decision can still lose the hand.")
          )
        )
      ),
      street_glossary_ui("Pre-Flop")
    )
  ),
  # Independent calculators for each post-flop street.
  lab_street_tab("Flop", 3L, "flop"),
  lab_street_tab("Turn", 4L, "turn"),
  lab_street_tab("River", 5L, "river"),
  poker_glossary_tab()
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
  # Glossary links navigate without resetting tab inputs or recalculating hands.
  glossary_selected <- reactiveVal(NULL)
  
  filtered_glossary <- reactive({
    entries <- poker_glossary
    category <- if (is.null(input$glossary_category)) "ALL" else input$glossary_category
    letter <- if (is.null(input$glossary_letter)) "ALL" else input$glossary_letter
    query <- if (is.null(input$glossary_search)) "" else tolower(trimws(input$glossary_search))
    if (!identical(category,"ALL")) entries <- entries[entries$category==category,,drop=FALSE]
    if (identical(letter,"#")) {
      entries <- entries[grepl("^[0-9]",entries$term),,drop=FALSE]
    } else if (!identical(letter,"ALL")) {
      entries <- entries[toupper(substr(entries$term,1L,1L))==letter,,drop=FALSE]
    }
    if (nzchar(query)) {
      searchable <- tolower(paste(entries$term,entries$definition,entries$category,
                                  entries$example,sep=" | "))
      entries <- entries[grepl(query, searchable, fixed=TRUE),,drop=FALSE]
    }
    # An incoming contextual link wins over old filter settings until reset.
    selected <- glossary_selected()
    if (!is.null(selected) && selected %in% poker_glossary$id &&
        !(selected %in% entries$id)) {
      entries <- rbind(entries,poker_glossary[poker_glossary$id==selected,,drop=FALSE])
    }
    entries[order(tolower(entries$term)),,drop=FALSE]
  })
  
  output$glossary_count <- renderText({
    sprintf("%d of %d terms", nrow(filtered_glossary()),nrow(poker_glossary))
  })
  output$glossary_entries <- renderUI({
    entries <- filtered_glossary()
    if (!nrow(entries)) return(div(class="preflop-card",p("No matching terms. Try another search or category.")))
    letters <- toupper(substr(entries$term,1L,1L))
    selected <- glossary_selected()
    tagList(lapply(unique(letters), function(letter) {
      rows <- entries[letters==letter,,drop=FALSE]
      div(class="glossary-group",
          h3(letter),
          tagList(lapply(seq_len(nrow(rows)),function(i) {
            glossary_entry_ui(rows[i,,drop=FALSE], selected=identical(rows$id[i],selected))
          })))
    }))
  })
  observeEvent(input$glossary_open, {
    id <- input$glossary_open$id
    if (length(id)!=1L || !(identical(id,"") || id %in% poker_glossary$id)) return()
    glossary_selected(if (nzchar(id)) id else NULL)
    updateTextInput(session,"glossary_search",value="")
    updateSelectInput(session,"glossary_category",selected="ALL")
    updateSelectInput(session,"glossary_letter",selected="ALL")
    updateNavbarPage(session,"poker_nav",selected="Glossary")
    session$onFlushed(function() {
      session$sendCustomMessage("poker_glossary_focus",list(id=id))
    }, once=TRUE)
  })
  observeEvent(input$glossary_search, {
    if (!is.null(input$glossary_search) && nzchar(trimws(input$glossary_search)))
      glossary_selected(NULL)
  },ignoreInit=TRUE)
  observeEvent(list(input$glossary_category,input$glossary_letter), {
    if ((!is.null(input$glossary_category) && input$glossary_category!="ALL") ||
        (!is.null(input$glossary_letter) && input$glossary_letter!="ALL"))
      glossary_selected(NULL)
  },ignoreInit=TRUE)
  
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
  # decision receives its own two-second pause, even after a street
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
          # If the bot acts first on the new street, its two-second
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
      }, delay = 2)
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
  # show the ACTION badge and schedule its decision in two seconds.
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
  # NEXT-CARD TRANSFER: preserve every relevant current lab input.
  # A user click is the only trigger; it does not touch Practice Game.
  # ==========================================================
  observeEvent(input$lab_transfer_card, {
    from <- input$lab_transfer_card$from
    card <- input$lab_transfer_card$card
    if (length(from)!=1L || !(from %in% c("flop","turn")) ||
        length(card)!=1L || !(card %in% LAB_DECK)) return()
    target <- if (from=="flop") "turn" else "river"
    n_current <- if (from=="flop") 3L else 4L
    get <- function(s) isolate(input[[paste0(from,"_",s)]])
    hero <- c(get("hero1"),get("hero2"))
    board <- vapply(seq_len(n_current),function(i) {
      x <- get(paste0("board",i))
      if (is.null(x)) "" else as.character(x)
    },character(1))
    exact <- identical(get("villain"),"exact")
    opp <- if(exact) c(get("opp1"),get("opp2")) else character(0)
    if (length(hero)!=2L || any(!nzchar(c(hero,board))) ||
        (exact && (length(opp)!=2L || any(!nzchar(opp)))) ||
        anyDuplicated(c(hero,board,opp,card)) ||
        !all(c(hero,board,opp,card) %in% LAB_DECK)) {
      showNotification("Cards have changed. Check your scenario and choose another card.",type="warning")
      return()
    }
    updateSelectizeInput(session,paste0(target,"_hero1"),selected=hero[1L])
    updateSelectizeInput(session,paste0(target,"_hero2"),selected=hero[2L])
    for (i in seq_len(n_current+1L)) {
      new_card <- c(board,card)[i]
      updateSelectizeInput(session,paste0(target,"_board",i),selected=new_card)
    }
    for (key in c("villain","n")) {
      if (key != "n" || target != "river")
        updateSelectInput(session,paste0(target,"_",key),selected=get(key))
    }
    if (exact) {
      updateSelectizeInput(session,paste0(target,"_opp1"),selected=opp[1L])
      updateSelectizeInput(session,paste0(target,"_opp2"),selected=opp[2L])
    }
    updateTabsetPanel(session,"poker_nav",selected=if(from=="flop") "Turn" else "River")
    showNotification("Cards copied. Recalculate equity to analyze this board.",duration=4)
  },ignoreInit=TRUE)
  
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
      list(hero=hero, board=board, opp=opp, style=style,
           n=if (prefix == "river") NA_integer_ else as.integer(field("n")))
    })
    output[[paste0(prefix,"_validation")]] <- renderUI({
      x <- validated_input()
      if (is.null(x$error)) return(NULL)
      div(class="preflop-note",style="border-left-color:#c62828;",
          strong("Check your cards: "),x$error)
    })
    calculation <- eventReactive(field("run"), {
      x <- validated_input()
      if (!is.null(x$error)) return(list(error=x$error))
      tryCatch({
        # Exact hand-distribution enumeration is distinct from showdown equity.
        dist <- lab_exact_runouts(x$hero,x$board,x$opp)
        improved <- lab_immediate_improvement(x$hero,x$board,x$opp)
        eq <- lab_equity(x$hero,x$board,x$style,x$opp,x$n)
        threats <- if (identical(prefix, "river"))
          lab_river_better_hands(x$hero, x$board, x$style, x$opp) else NULL
        list(input=x, dist=dist, improved=improved, equity=eq, threats=threats,
             hand=lab_hand_features(x$hero,x$board))
      },error=function(e) list(error=paste("Calculation error:",conditionMessage(e))))
    },ignoreInit=TRUE)
    if (prefix == "turn") {
      output[[paste0(prefix,"_explorer")]] <- renderUI({
        x <- validated_input()
        if (!is.null(x$error))
          return(p(class="lab-explorer-help","Select valid cards to explore possible next cards."))
        data <- lab_next_street_cards(x$hero,x$board,x$opp)
        lab_next_street_ui(data,prefix,x$opp)
      })
    }
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
        NULL
      } else {
        sprintf("Next-card category improvement: %d of %d unseen cards (%s). This is NOT the number of clean winning outs.",
                r$improved$n,r$improved$total,fmt(r$improved$p))
      }
      card_odds <- tags$details(class="lab-more-details",
                                tags$summary("Exact hand-category breakdown"),
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
                tags$b(glossary_link("showdown-equity","Showdown equity")),
                h2(fmt(e$equity),style="margin:5px 0;color:#14633a;")),
            div(style="flex:1.3;min-width:220px;border-radius:9px;background:#eff4fa;padding:13px;",
                tags$b("Showdown probabilities"),
                div(class="lab-outcomes",
                    div(class="lab-outcome", span("WIN"), strong(fmt(e$win))),
                    div(class="lab-outcome", span("TIE"), strong(fmt(e$tie))),
                    div(class="lab-outcome", span("LOSS"), strong(fmt(e$loss)))))
        ),
        p(glossary_link(if (e$exact) "exact-enumeration" else "monte-carlo-simulation",uncertainty),sprintf(" across %s legal %s.",
                                                                                                            format(e$n,big.mark=","),
                                                                                                            if (e$exact) "outcomes" else "trials")),
        if (prefix == "flop") p(strong(glossary_link("made-hand","Current made hand"),": "), lab_rank_name(r$hand$rank)),
        if (prefix == "turn") p(strong(glossary_link("made-hand","Current made hand"),": "), lab_river_rank_label(r$hand$rank)),
        if (prefix != "river") p(glossary_inline(next_info)),
        if (prefix != "river") card_odds,
        if (!is.null(extra)) tags$small(extra),
        NULL
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
      # The lab is for cards, equity and river outcomes—not a betting calculator.
      # The Practice Game keeps its separate, fully interactive betting controls.
      if (prefix == "turn") return(NULL)
      r <- calculation()
      req(!is.null(r))
      if (!is.null(r$error)) return(NULL)
      
      if (prefix == "flop") {
        x <- r$input
        # Hand-based educational guidance; there are no additional
        # position/action controls or betting calculations in this tab.
        guidance <- lab_beginner_guidance(x$hero,x$board,"check")
        texture <- lab_flop_texture_summary(x$board)
        texture_note <- switch(texture$label,
                               "Wet"="More straight or flush possibilities can develop on this flop.",
                               "Semi-wet"="A flush draw is possible from the board's suit pattern.",
                               "Dry"="There are fewer obvious draws on this flop.")
        return(div(
          div(class="lab-summary-callout",
              div(class="lab-eyebrow","Your current situation"),
              h4(guidance$headline),
              p(guidance$explanation)),
          div(class="lab-guide-grid",
              div(class="lab-guide-box",
                  h5("Your cards & board"),
                  p(strong("Made hand: "),lab_rank_name(r$hand$rank)),
                  p(strong("Board: "),guidance$board),
                  p(strong("Texture: "),texture$label),
                  if (guidance$draws != "None detected")
                    p(strong("Draws: "),guidance$draws))
          ),
          tags$small(class="lab-explorer-help",
                     texture_note," These are educational cues, not solver-generated actions.")
        ))
      }
      
      # Exact opponent outcome or every legal better holding on the river.
      # Preserve the existing compact rank notation and original combinations.
      t <- r$threats
      if (isTRUE(t$exact)) {
        result <- if (t$outcome < 0L) "OPPONENT WINS" else
          if (t$outcome > 0L) "YOU WIN" else "TIE"
        return(div(class="lab-math-card",
                   p(strong("Your final hand: "), t$your_label),
                   p(strong("Opponent's final hand: "), t$opponent_label),
                   div(class="lab-odds-number", result)))
      }
      threat_table <- function(data) {
        tags$div(class="lab-threat-scroll",
                 tags$table(class="table table-striped table-condensed lab-threat-table",
                            tags$thead(tags$tr(tags$th("Winning hand"), tags$th("Combos"),
                                               tags$th("Example opponent cards"))),
                            tags$tbody(lapply(seq_len(nrow(data)), function(i) {
                              tags$tr(
                                tags$td(data$hand[i],
                                        title=if (startsWith(data$hand[i], "Full house:"))
                                          "High-to-low display notation merges both trip arrangements; exact strengths are compared separately."
                                        else NULL),
                                tags$td(data$combos[i]),
                                tags$td(data$examples[i]))
                            }))))
      }
      div(class="lab-math-card",
          p(strong("Your final hand: "), t$your_label),
          p(sprintf("%s of %s legal opponent combinations beat your hand in the selected range.",
                    format(t$better, big.mark=","), format(t$total, big.mark=","))),
          if (t$better == 0L) {
            p("No possible opponent holding in this range beats you.")
          } else {
            threat_table(t$groups)
          },
          tags$small("Possible holdings, not a prediction of an opponent's actions. Full houses use compact high-to-low rank notation.")
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
