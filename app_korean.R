# ============================================================
# KOREAN VERSION
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
            "R" = "오픈 / 레이즈",
            "3B" = "3벳",
            "C" = "콜 / 디펜드",
            "M" = "믹스 / 상황에 따라 혼합",
            "F" = "폴드"
          )
          # 오른쪽 위: 수딧, 왼쪽 아래: 오프수딧, 대각선: 포켓페어.
          # 숫자가 큰 랭크부터 표기합니다(98s, 98o).
          hand <- if (i == j) {
            paste0(ranks[i], ranks[i])
          } else if (i < j) {
            paste0(ranks[i], ranks[j], "s")
          } else {
            paste0(ranks[j], ranks[i], "o")
          }
          hand_type <- if (i == j) "포켓페어" else if (i < j) "수딧" else "오프수딧"
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

# Display translations only. Game-state values remain English for compatibility.
ko_street <- function(street, bilingual = FALSE) {
  ko <- switch(street,
               "Pre-flop" = "프리플랍", "Flop" = "플랍",
               "Turn" = "턴", "River" = "리버", street)
  if (bilingual) paste0(ko, " (", street, ")") else ko
}

ko_hand <- function(name) {
  translations <- c(
    "High Card"="하이 카드", "One Pair"="원 페어", "Two Pair"="투 페어",
    "Three of a Kind"="트리플", "Straight"="스트레이트",
    "Flush"="플러시", "Full House"="풀하우스",
    "Four of a Kind"="포카드", "Straight Flush"="스트레이트 플러시",
    "Royal Flush"="로열 플러시"
  )
  result <- unname(translations[name])
  ifelse(is.na(result), name, result)
}

# Log messages are translated at display time; the recorded actions do not change.
ko_game_log <- function(entry) {
  entry <- sub("^Hand ([0-9]+) — ", "핸드 \\1 · ", entry)
  entry <- sub("A player ran out of chips\\. Stacks reset to \\$200\\.",
               "스택이 소진되어 양쪽 스택을 $200으로 초기화했습니다.", entry)
  entry <- sub("New hand\\. ", "새 핸드 · ", entry)
  entry <- sub("Blinds posted: \\$1 / \\$2\\.", "블라인드: $1 / $2", entry)
  entry <- sub("Uncalled (\\$[0-9,]+) returned to (Human|Bot)\\.",
               "콜되지 않은 \\1 반환 → \\2", entry)
  entry <- sub("You have the button\\.", "내가 버튼입니다.", entry)
  entry <- sub("Bot have the button\\.", "봇이 버튼입니다.", entry)
  entry <- sub("Human folds\\.", "내가 폴드", entry)
  entry <- sub("Bot folds\\.", "봇 폴드", entry)
  entry <- sub("Human checks\\.", "내가 체크", entry)
  entry <- sub("Bot checks\\.", "봇 체크", entry)
  entry <- sub("Human calls (\\$[0-9,]+)\\.", "내가 \\1 콜", entry)
  entry <- sub("Bot calls (\\$[0-9,]+)\\.", "봇 \\1 콜", entry)
  entry <- sub("Human raises to (\\$[0-9,]+)\\.", "내가 \\1까지 레이즈", entry)
  entry <- sub("Bot raises to (\\$[0-9,]+)\\.", "봇 \\1까지 레이즈", entry)
  entry <- gsub("Human", "플레이어", entry, fixed=TRUE)
  entry <- gsub("Bot", "봇", entry, fixed=TRUE)
  entry
}

# ============================================================
# POST-FLOP ANALYSIS LAB
# Exact runout enumeration + opponent-range equity simulation.
# This module is educational: it is not a GTO solver.
# ============================================================

LAB_CATEGORY_NAMES <- c(
  "하이 카드", "원 페어", "투 페어", "트리플",
  "스트레이트", "플러시", "풀하우스", "포카드",
  "스트레이트 플러시"
)
LAB_RANKS <- c("2"=2L,"3"=3L,"4"=4L,"5"=5L,"6"=6L,"7"=7L,
               "8"=8L,"9"=9L,"T"=10L,"J"=11L,"Q"=12L,"K"=13L,"A"=14L)
LAB_DECK <- create_deck()
LAB_CARD_CHOICES <- c(
  "— 카드 선택 —"="",
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
  if (rank[1L] == 8L && rank[2L] == 14L) return("로열 플러시")
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
                        "Flop"="플랍: 레인지 우위와 넛 우위를 생각하고, 어떤 턴 카드가 보드 상황을 바꾸는지 살펴보세요.",
                        "Turn"="턴: 턴 카드를 반영해 핸드를 다시 평가하고, 양쪽 레인지에서 어떤 핸드가 리버까지 이어질지 생각하세요.",
                        "River"="리버: 드로우는 더 이상 없습니다. 씬 밸류벳, 블러프 캐처, 블로커 효과를 생각하세요.")
  if (facing == "check") {
    baseline <- if (cat >= 3L) {
      if (wet) {
        "상대의 약한 메이드 핸드나 드로우가 콜할 수 있다면 큰 사이즈를 포함한 밸류벳을 고려하세요. 강한 핸드 일부는 체크 레인지에도 남겨 두세요."
      } else {
        "이 보드에서는 작거나 중간 크기의 밸류벳을 고려하되, 강한 핸드 일부는 체크 레인지에 포함하세요."
      }
    } else if (cat == 2L || cat == 1L) {
      "체크는 중간 강도 핸드로 팟이 지나치게 커지는 것을 막을 수 있습니다. 레인지에 따라 작은 밸류벳이나 프로텍션벳도 가능합니다."
    } else if (length(info$draws) > 0L) {
      "드로우는 세미블러프로 베팅하거나 체크할 수 있습니다. 폴드 에퀴티, 보드 커버리지, 쇼다운 밸류를 함께 고려하세요."
    } else {
      "아직 메이드 핸드가 없다면 체크를 우선 고려하세요. 블러프를 선택할 때는 블로커, 레인지 우위, 그리고 함께 베팅할 밸류 핸드를 생각해야 합니다."
    }
  } else {
    baseline <- if (cat >= 3L) {
      "강한 메이드 핸드는 대체로 계속 플레이할 수 있지만, 콜과 레이즈의 비율은 보드·베팅 사이즈·상대 밸류 레인지에 따라 달라집니다."
    } else if (cat >= 1L) {
      "상대가 그 베팅으로 보여 주는 레인지를 생각하세요. 내 페어가 블러프 캐처 역할을 할지, 큰 베팅에는 폴드해야 할지를 판단해 보세요."
    } else if (length(info$draws) > 0L) {
      "콜 비용에 비해 드로우 에퀴티와 임플라이드 오즈가 충분한지 보세요. 드로우를 완성하는 카드가 모두 승리로 이어지는 것은 아닙니다."
    } else {
      "메이드 핸드가 없다면 블러프 캐처로 활용할 근거가 있는지, 블러프 레이즈가 적절한지 따져 보세요. 그렇지 않다면 폴드도 고려해야 합니다."
    }
  }
  pos_text <- if (pos == "ip") {
    "포스트플랍에서 인 포지션(IP)입니다. 상대 행동을 먼저 보고 결정할 수 있습니다."
  } else {
    "포스트플랍에서 아웃 오브 포지션(OOP)입니다. 체크 레인지를 보호하는 것도 고려하세요."
  }
  if (facing == "bet" && bet > 0L) {
    pot_odds <- bet/(pot+2*bet)
    mdf <- pot/(pot+bet)
    comparison <- if (eq >= pot_odds) {
      "추정 쇼다운 에퀴티가 단순 팟 오즈 기준보다 높습니다."
    } else {
      "추정 쇼다운 에퀴티가 단순 팟 오즈 기준보다 낮습니다."
    }
    math <- sprintf("베팅 전 팟 $%2$d에 상대가 $%1$d 베팅: 콜 기준 %3$.1f%%, 최소 방어 빈도 %4$.1f%%(레인지 전체 개념). %5$s 이 수치만으로 GTO 액션을 결정할 수 없으며 이후 베팅은 반영하지 않습니다.",
                    round(bet), round(pot), 100*pot_odds, 100*mdf, comparison)
  } else {
    sizes <- pmin(round(pot * c(.25,1/3,.5,.75,1)), 999999L)
    math <- sprintf("$%d 팟에서의 예시 베팅: 1/4 $%d · 1/3 $%d · 1/2 $%d · 3/4 $%d · 풀팟 $%d. 솔버가 제시한 액션 빈도는 아닙니다.",
                    round(pot), sizes[1],sizes[2],sizes[3],sizes[4],sizes[5])
  }
  draws <- if (length(info$draws)) paste(info$draws,collapse="; ") else "감지되지 않음"
  list(street=street_note, baseline=baseline, position=pos_text,
       math=math, feature=sprintf("%s · %s · 드로우 지표: %s",lab_rank_name(info$rank),texture,draws))
}

# Clear beginner-facing prompts. These are study prompts, not solver actions.
lab_beginner_guidance <- function(street, hero, board, facing, pos) {
  info <- lab_hand_features(hero, board)
  category <- info$rank[1L]
  has_draw <- length(info$draws) > 0L
  if (facing == "bet") {
    if (category >= 3L) {
      headline <- "강한 메이드 핸드: 콜 vs. 레이즈"
      explanation <- paste(
        "상대가 가질 수 있는 핸드와 베팅 사이즈를 비교하세요.",
        "콜은 상대의 약한 핸드를 계속 참여시키고, 레이즈는 팟을 키웁니다.",
        "다만 레이즈하면 약한 핸드가 폴드할 수 있습니다. 강한 핸드도 항상 이기는 것은 아닙니다."
      )
    } else if (category >= 1L) {
      headline <- "페어·투 페어: 블러프 캐칭 가치 판단"
      explanation <- paste(
        "상대가 어떤 핸드로 베팅할지 생각하세요.",
        "작은 베팅일수록 필요한 콜 비용이 적습니다.",
        "특히 취약한 페어일수록 베팅 사이즈를 신중하게 봐야 합니다."
      )
    } else if (has_draw) {
      headline <- "드로우: 에퀴티와 팟 오즈 비교"
      explanation <- paste(
        "앞으로 나올 카드가 드로우를 완성할 수 있지만,",
        "그렇다고 반드시 승리하는 것은 아닙니다. 콜에 드는 비용과",
        "상대보다 강한 핸드를 완성할 가능성을 비교하세요."
      )
    } else {
      headline <- "미완성 핸드: 계속 플레이할 근거가 있나요?"
      explanation <- paste(
        "계속 플레이할 명확한 이유가 있는지 생각하세요.",
        "그렇지 않다면 폴드도 합리적인 선택입니다."
      )
    }
  } else {
    if (category >= 3L) {
      headline <- "강한 메이드 핸드: 밸류벳 대상 찾기"
      explanation <- paste(
        "밸류벳은 더 약한 핸드에게 콜을 받기 위한 베팅입니다.",
        "약한 핸드가 거의 콜하지 않는 상황이라면 체크가 나을 수 있습니다.",
        "강한 조합이 주로 보드 카드로 만들어졌는지도 고려하세요."
      )
    } else if (category >= 1L) {
      headline <- "페어·투 페어: 밸류벳 vs. 팟 컨트롤"
      explanation <- paste(
        "중간 강도 핸드로 체크하면 팟 크기를 조절할 수 있습니다.",
        "더 약한 핸드가 콜한다면 작은 밸류벳도 고려할 만합니다."
      )
    } else if (has_draw) {
      headline <- "드로우: 세미블러프 vs. 체크"
      explanation <- paste(
        "드로우로 베팅하면 상대가 폴드할 때 바로 이기거나,",
        "드로우가 완성되었을 때 이길 수 있습니다. 체크하면 팟을 작게 유지하고",
        "다음 카드를 더 저렴하게 볼 수도 있습니다."
      )
    } else {
      headline <- "미완성 핸드: 체크 vs. 선택적 블러프"
      explanation <- paste(
        "기본적으로 체크를 고려하세요.",
        "블러프하려면 상대가 폴드할 만한 이유가 있어야 합니다."
      )
    }
  }
  street_tip <- switch(street,
                       "Flop"="앞으로 턴과 리버 카드가 남아 있습니다. 어떤 턴 카드가 내 레인지나 상대 레인지에 유리할지 생각하세요.",
                       "Turn"="남은 카드는 한 장입니다. 어떤 리버 카드가 우위를 뒤집을지 생각하세요.",
                       "River"="리버에서는 더 나올 카드가 없습니다. 어떤 핸드로 밸류벳하고, 어떤 핸드를 블러프 캐처로 사용할지 생각하세요."
  )
  position_tip <- if (pos == "ip") {
    "인 포지션(IP): 상대의 액션을 확인한 뒤 결정할 수 있습니다."
  } else {
    "아웃 오브 포지션(OOP): 각 스트리트에서 보통 먼저 행동합니다. 체크하면 추가 칩을 넣기 전에 상대 반응을 볼 수 있습니다."
  }
  board_terms <- strsplit(lab_texture(board), ", ", fixed=TRUE)[[1L]]
  replacements <- c(
    "paired"="페어 보드",
    "two-tone"="투톤",
    "rainbow"="레인보우",
    "three or more of a suit"="같은 수트 3장 이상",
    "connected"="커넥티드"
  )
  board_easy <- paste(unname(replacements[board_terms]), collapse="; ")
  draw_easy <- character(0)
  if ("four-card flush draw" %in% info$draws)
    draw_easy <- c(draw_easy, "플러시 드로우")
  if ("one-card straight possibility" %in% info$draws)
    draw_easy <- c(draw_easy, "스트레이트 드로우 가능성")
  list(headline=headline, explanation=explanation, street_tip=street_tip,
       position_tip=position_tip, board=board_easy,
       draws=if (length(draw_easy)) paste(draw_easy,collapse=" + ") else "감지되지 않음")
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
        select_card(paste0("board",i),paste("보드",i),default_board[i]))
  })
  tabPanel(ko_street(street, bilingual=TRUE),
           fluidPage(class="preflop-page",
                     div(class="preflop-card",
                         h3(paste0("♠ ", ko_street(street), " · 전략 및 에퀴티 분석")),
                         p("홀 카드와 보드를 선택하면 리버에서 완성될 수 있는 족보와 ",
                           "선택한 상대 레인지 기준의 쇼다운 에퀴티를 확인할 수 있습니다."),
                         div(class="preflop-note",
                             strong("전략 안내: "),
                             "확률은 카드와 상대 레인지 가정에 따라 계산·추정됩니다. 전략 설명은 학습용 ",
                             "GTO 참고 가이드로, 솔버가 제시하는 정답은 아닙니다.")),
                     fluidRow(
                       column(4,
                              div(class="preflop-card",
                                  h4("1 · 상황 설정"),
                                  actionButton(paste0(prefix,"_load"),"연습 게임 카드 불러오기",
                                               class="btn-default",width="100%"),
                                  helpText("연습 게임에서 내 홀 카드와 해당 스트리트까지 공개된 보드 카드를 가져옵니다."),
                                  fluidRow(column(6,select_card("hero1","내 카드 1",default_hero[1])),
                                           column(6,select_card("hero2","내 카드 2",default_hero[2]))),
                                  h4("커뮤니티 카드"),
                                  div(class="lab-card-grid", board_inputs),
                                  tags$hr(),
                                  selectInput(paste0(prefix,"_villain"),"상대 핸드 / 레인지",
                                              c("무작위 핸드 (Random)"="random",
                                                "타이트 레인지 (추정)"="tight",
                                                "루스 레인지 (추정)"="loose",
                                                "상대 홀 카드 직접 입력"="exact"),selected="random"),
                                  conditionalPanel(
                                    condition=sprintf("input.%s_villain == 'exact'",prefix),
                                    fluidRow(column(6,select_card("opp1","상대 카드 1","")),
                                             column(6,select_card("opp2","상대 카드 2","")))
                                  ),
                                  tags$details(class="lab-more-details",
                                               tags$summary("상대 레인지 가정 알아보기"),
                                               p("타이트/루스 레인지는 프리플랍 핸드 강도를 기준으로 단순화한 모델입니다. ",
                                                 "실제 상대의 행동을 예측하는 것은 아니며, 선택한 레인지에 포함되는 모든 ",
                                                 "두 장짜리 핸드를 같은 확률로 선택합니다.")),
                                  selectInput(paste0(prefix,"_n"),"몬테카를로 시뮬레이션 횟수",
                                              c("300회 · 빠름"=300,"750회 · 기본"=750,"1,500회 · 더 정밀"=1500),
                                              selected=750),
                                  actionButton(paste0(prefix,"_run"),"확률 계산하기",
                                               class="btn-success btn-lg",width="100%"),
                                  br(), br(),
                                  uiOutput(paste0(prefix,"_validation"))
                              ),
                              div(class="preflop-card",
                                  h4("2 · 베팅 상황"),
                                  selectInput(paste0(prefix,"_pos"),"포스트플랍 포지션",
                                              c("인 포지션 (버튼/SB)"="ip", "아웃 오브 포지션 (BB)"="oop")),
                                  selectInput(paste0(prefix,"_facing"),"현재 액션 상황",
                                              c("상대가 체크함 / 아직 베팅 없음"="check", "상대가 베팅함"="bet")),
                                  numericInput(paste0(prefix,"_pot"),"상대 베팅 전 팟 ($)",
                                               value=3,min=1,step=1),
                                  conditionalPanel(
                                    condition=sprintf("input.%s_facing == 'bet'",prefix),
                                    numericInput(paste0(prefix,"_bet"),"상대 베팅액 ($)",
                                                 value=10,min=1,step=1)
                                  ),
                                  numericInput(paste0(prefix,"_stack"),"남은 유효 스택 ($)",
                                               value=199,min=1,step=1),
                                  helpText("입력값을 바꿨다면 「확률 계산하기」를 다시 눌러 결과를 갱신하세요.")
                              )
                       ),
                       column(8,
                              div(class="preflop-card",
                                  h4("3 · 쇼다운 에퀴티와 족보 확률"),
                                  uiOutput(paste0(prefix,"_results")),
                                  plotOutput(paste0(prefix,"_distribution"),height="295px")
                              ),
                              div(class="preflop-card",
                                  h4("4 · 스트리트별 전략 분석"),
                                  uiOutput(paste0(prefix,"_strategy"))
                              )
                       )
                     ),
                     div(class="preflop-card",
                         h4(paste0(ko_street(street), " · 상대 성향에 따른 전략 조정")),
                         p(switch(street,
                                  "Flop"="플랍이 누구의 레인지에 더 유리한지 판단하세요. 양쪽 모두 강한 핸드나 드로우를 가질 수 있습니다.",
                                  "Turn"="턴 카드가 나오면 내 핸드와 양쪽 레인지를 다시 평가하세요. 남은 리버 카드까지 고려해 플랜을 세워 보세요.",
                                  "River"="리버에서는 더 이상 드로우를 완성할 수 없습니다. 어떤 핸드가 내 베팅에 콜하거나 폴드할지 고려하세요.")),
                         fluidRow(
                           column(4,div(class="lab-alt-card",
                                        h5("GTO 참고 · 기본 전략"),
                                        p("상황에 따라 베팅과 체크를 섞어, 내 액션만으로 핸드 강도가 쉽게 읽히지 않도록 합니다."))),
                           column(4,div(class="lab-alt-card",
                                        h5("vs. 패시브 플레이어"),
                                        p("콜을 자주 하는 패시브 플레이어에게는 약한 핸드의 콜을 받을 수 있는 핸드로 밸류벳하세요. 폴드가 적으므로 무리한 블러프는 줄이는 편이 좋습니다."))),
                           column(4,div(class="lab-alt-card",
                                        h5("vs. 어그레시브 플레이어"),
                                        p("블러프가 많은 어그레시브 플레이어를 상대로는 블러프 캐처의 가치가 높아질 수 있습니다. 강한 핸드 일부는 트랩에 활용하고, 콜·레이즈를 결정할 때는 블로커와 베팅 사이즈를 함께 보세요.")))
                         ),
                         tags$details(class="lab-more-details",
                                      tags$summary("주요 용어와 분석의 한계"),
                                      p(strong("레인지 (Range):")," 상대가 가질 수 있는 모든 핸드의 범위입니다. 단일 핸드를 맞히는 개념이 아닙니다."),
                                      p(strong("밸류벳 (Value bet):")," 더 약한 핸드의 콜을 유도하는 베팅입니다."),
                                      p(strong("블러프 (Bluff):")," 더 강한 핸드를 폴드시키려는 베팅입니다."),
                                      p("표시된 확률은 남은 카드가 공개되고 추가 베팅 없이 쇼다운에 간다고 가정합니다. 상대가 폴드할 확률, 이후 베팅, 레이크, 솔버 기반 GTO 전략은 반영하지 않습니다. 족보가 좋아져도 반드시 승리하는 것은 아닙니다.")
                         )
                     )
           )
  )
}

# ============================================================
# UI
# ============================================================

ui <- navbarPage(
  title = "텍사스 홀덤 · 헤즈업",
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
    "연습 게임",
    fluidPage(
      div(
        class = "title-panel",
        h2("♠ 연습 게임", style = "margin-top:0;"),
        p(
          "GTO 개념을 참고한 봇과 연습하는 헤즈업 노리밋 홀덤",
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
            h4("게임 정보"),
            textOutput("hand_number"),
            textOutput("button"),
            textOutput("street"),
            textOutput("pot"),
            tags$hr(),
            h4("스택"),
            textOutput("blinds"),
            textOutput("human_stack"),
            textOutput("bot_stack")
          ),
          # Keep the highlighted result and Next Hand on the left,
          # but show whose turn it is beside the players at the table.
          uiOutput("result_ui"),
          div(class = "stat-box", h4("액션 기록"), uiOutput("game_log"))
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
                h4("봇의 핸드"),
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
                h4("내 핸드"),
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
            h4("내 액션"),
            fluidRow(
              column(
                3,
                actionButton("fold", "폴드", class = "btn-danger btn-lg", width = "100%")
              ),
              column(
                3,
                actionButton("check", "체크", class = "btn-secondary btn-lg", width = "100%")
              ),
              column(
                3,
                actionButton("call", "콜", class = "btn-primary btn-lg", width = "100%")
              ),
              column(
                3,
                actionButton("raise", "레이즈", class = "btn-success btn-lg", width = "100%")
              )
            ),
            br(),
            sliderInput(
              "raise_amount",
              "레이즈 목표 금액 ($)",
              min = BB,
              max = STARTING_STACK,
              value = 10,
              step = 1
            ),
            uiOutput("action_help"),
            br(),
            actionButton("next_hand", "다음 핸드", class = "btn-dark btn-lg", width = "100%")
          )
        )
      )
    )
  ),
  
  # ==========================================================
  # TAB 2: PRE-FLOP
  # ==========================================================
  
  tabPanel(
    "프리플랍 (Pre-flop)",
    fluidPage(
      class = "preflop-page",
      div(
        class = "preflop-card",
        h3("헤즈업 프리플랍 전략"),
        p(
          "헤즈업 노리밋 홀덤에서는 버튼 플레이어가 스몰 블라인드(SB)를 냅니다. ",
          "버튼(SB)은 프리플랍에서는 먼저 액션하고, 플랍 이후에는 마지막에 액션합니다."
        ),
        p(
          "상대가 한 명뿐이므로 오픈 및 디펜드 레인지가 여러 명이 플레이하는 테이블보다 ",
          "더 넓어지는 편입니다."
        ),
        div(
          class = "preflop-note",
          strong("중요: "),
          "아래 차트는 GTO 개념을 참고해 단순화한 학습용 차트입니다. ",
          "솔버가 계산한 정확한 GTO 레인지는 아니며, 실제 전략은 ",
          "유효 스택, 레이즈 사이즈, 레이크, 앤티, 게임 트리에 따라 달라집니다."
        )
      ),
      fluidRow(
        column(
          6,
          div(
            class = "preflop-card",
            h4("SB / 버튼 · 오픈 레인지"),
            p(
              "헤즈업에서 버튼(SB)은 프리플랍에 먼저 액션하지만, 비교적 넓은 레인지로 오픈할 수 있습니다."
            ),
            div(class = "preflop-scroll", preflop_chart_html(SB_PRE_FLOP)),
            div(class = "preflop-legend",
                preflop_legend_item("pf-open", "오픈 / 레이즈"),
                preflop_legend_item("pf-mix", "믹스 (혼합 전략)"),
                preflop_legend_item("pf-fold", "폴드")),
            p(class = "preflop-chart-note",
              "대각선 위: 수딧 (98s) · 대각선 아래: 오프수딧 (98o) · ",
              "대각선: 포켓페어 (99). 셀에 마우스를 올리면 전략이 표시됩니다.")
          )
        ),
        column(
          6,
          div(
            class = "preflop-card",
            h4("BB · SB 오픈에 대한 디펜스"),
            p(
              "BB는 이미 $2를 낸 상태에서 상대 한 명만 상대하므로 ",
              "다양한 핸드로 콜할 수 있습니다. 강한 핸드 일부는 3벳에 활용할 수도 있습니다."
            ),
            div(class = "preflop-scroll", preflop_chart_html(BB_PRE_FLOP)),
            div(class = "preflop-legend",
                preflop_legend_item("pf-3bet", "3벳"),
                preflop_legend_item("pf-call", "콜 / 디펜드"),
                preflop_legend_item("pf-mix", "믹스 (혼합 전략)"),
                preflop_legend_item("pf-fold", "폴드")),
            p(class = "preflop-chart-note",
              "대각선 위: 수딧 (98s) · 대각선 아래: 오프수딧 (98o) · ",
              "대각선: 포켓페어 (99). 셀에 마우스를 올리면 전략이 표시됩니다.")
          )
        )
      ),
      div(
        class = "preflop-card",
        h4("헤즈업 프리플랍 핵심 원칙"),
        tags$ul(
          class = "preflop-list",
          tags$li(
            strong("포지션의 중요성: "),
            "SB/버튼은 프리플랍에서 먼저 행동하지만 플랍 이후에는 인 포지션입니다."
          ),
          tags$li(
            strong("넓은 레인지: "),
            "상대가 한 명이므로 약한 Ax·Kx, 수딧 커넥터, ",
            "여러 포켓페어도 프리플랍에서 플레이할 가치가 있습니다."
          ),
          tags$li(
            strong("넓은 BB 디펜드: "),
            "BB는 이미 블라인드를 냈으므로 상대의 오픈에 더 좋은 팟 오즈로 디펜드할 수 있습니다."
          ),
          tags$li(
            strong("3벳의 역할: "),
            "3벳은 프리미엄 핸드에만 쓰지 않습니다. 밸류 핸드와 적절한 블러프 핸드를 함께 포함할 수 있습니다."
          ),
          tags$li(
            strong("사이징에 따른 레인지 변화: "),
            "SB의 오픈 사이즈가 커지면 BB의 콜 비용이 높아지므로, 계속 플레이할 레인지도 달라집니다."
          ),
          tags$li(
            strong("스택 깊이: "),
            "유효 스택이 짧아질수록 올인을 고려해야 하는 상황이 많아지고, ",
            "프리플랍에서의 올인 판단도 더 중요해집니다."
          ),
          tags$li(
            strong("액션 빈도로 생각하기: "),
            "일부 경계선 핸드는 레이즈·콜·폴드를 고정하지 않고 빈도에 따라 섞어 플레이합니다."
          ),
          tags$li(
            strong("결과만으로 판단하지 않기: "),
            "좋은 프리플랍 결정이 항상 승리로 이어지는 것은 아닙니다. 단기 결과에는 분산이 큽니다."
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
        showNotification(paste0("연습 게임에서 ", ko_street(switch(prefix, flop="Flop", turn="Turn", river="River")),
                                "까지 진행한 뒤 카드를 불러오세요."),type="warning")
        return()
      }
      updateSelectizeInput(session,paste0(prefix,"_hero1"),selected=state$human_cards[1])
      updateSelectizeInput(session,paste0(prefix,"_hero2"),selected=state$human_cards[2])
      for (i in seq_len(board_n)) {
        updateSelectizeInput(session,paste0(prefix,"_board",i),selected=state$board[i])
      }
      showNotification("연습 게임의 카드를 가져왔습니다. 「확률 계산하기」를 눌러 주세요.",
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
        return(list(error="내 카드 2장과 커뮤니티 카드를 모두 선택하세요."))
      opp <- if (identical(style,"exact")) {
        c(field("opp1"),field("opp2"))
      } else {
        character(0)
      }
      if (identical(style,"exact") && (length(opp)!=2L || any(!nzchar(opp))))
        return(list(error="상대 핸드 직접 지정 시 카드 2장을 모두 선택하세요."))
      used <- c(hero,board,opp)
      if (anyDuplicated(used))
        return(list(error="같은 카드를 두 번 선택할 수 없습니다. 카드 구성을 수정하세요."))
      if (!all(used %in% LAB_DECK))
        return(list(error="잘못된 카드를 선택했습니다."))
      pot <- suppressWarnings(as.numeric(field("pot")))
      bet <- suppressWarnings(as.numeric(field("bet")))
      stack <- suppressWarnings(as.numeric(field("stack")))
      if (length(pot)!=1L || length(stack)!=1L ||
          !is.finite(pot) || pot <= 0 || !is.finite(stack) || stack <= 0)
        return(list(error="팟과 유효 스택은 0보다 큰 숫자여야 합니다."))
      facing <- field("facing")
      if (identical(facing,"bet") &&
          (length(bet)!=1L || !is.finite(bet) || bet <= 0))
        return(list(error="상대 베팅액에 0보다 큰 숫자를 입력하세요."))
      if (identical(facing,"bet") && bet > stack)
        return(list(error="이 헤즈업 예시에서는 상대 베팅액이 유효 스택을 초과할 수 없습니다."))
      list(hero=hero,board=board,opp=opp,style=style,pot=pot,
           bet=if (identical(facing,"bet")) bet else 0,
           stack=stack,facing=facing,pos=field("pos"),
           n=as.integer(field("n")))
    })
    output[[paste0(prefix,"_validation")]] <- renderUI({
      x <- validated_input()
      if (is.null(x$error)) return(NULL)
      div(class="preflop-note",style="border-left-color:#c62828;",
          strong("카드 / 상황을 확인하세요: "),x$error)
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
      },error=function(e) list(error=paste("계산 오류:",conditionMessage(e))))
    },ignoreInit=TRUE)
    output[[paste0(prefix,"_results")]] <- renderUI({
      r <- calculation()
      req(!is.null(r))
      if (!is.null(r$error))
        return(div(class="preflop-note",style="border-left-color:#c62828;",r$error))
      e <- r$equity
      d <- r$dist
      fmt <- function(p) sprintf("%.1f%%",100*p)
      uncertainty <- if (e$exact) "정확한 경우의 수 계산" else "몬테카를로 추정"
      extra <- if (e$exact) NULL else
        paste0("몬테카를로 추정치는 실행할 때마다 달라질 수 있습니다. ",
               "반복 횟수를 늘리면 추정 오차를 줄일 수 있습니다.")
      next_info <- if (is.null(r$improved)) {
        "리버: 더 나올 카드가 없습니다."
      } else {
        sprintf("다음 카드에서 족보가 올라갈 확률: 남은 %2$d장 중 %1$d장 (%3$s). 반드시 이기는 클린 아웃의 수는 아닙니다.",
                r$improved$n,r$improved$total,fmt(r$improved$p))
      }
      card_odds <- tags$div(
        tags$b("리버 최종 족보 확률 (정확 계산)"),
        tags$p(sprintf("원 페어: %s | 투 페어: %s | 트리플: %s | 스트레이트: %s",
                       fmt(d[2]),fmt(d[3]),fmt(d[4]),fmt(d[5]))),
        tags$p(sprintf("플러시: %s | 풀하우스: %s | 포카드: %s | 스트레이트 플러시: %s",
                       fmt(d[6]),fmt(d[7]),fmt(d[8]),fmt(d[9]))),
        tags$p(sprintf("스트레이트 또는 스트레이트 플러시: %s · 플러시 또는 스트레이트 플러시: %s",
                       fmt(d[5]+d[9]),fmt(d[6]+d[9]))),
        tags$p(sprintf("풀하우스 이상 (족보 순위): %s",fmt(sum(d[7:9]))))
      )
      div(
        div(style="display:flex;gap:10px;flex-wrap:wrap;margin-bottom:14px;",
            div(style="flex:1;min-width:130px;border-radius:9px;background:#e2f2e7;padding:13px;",
                tags$b("쇼다운 에퀴티"),
                h2(fmt(e$equity),style="margin:5px 0;color:#14633a;")),
            div(style="flex:1.3;min-width:220px;border-radius:9px;background:#eff4fa;padding:13px;",
                tags$b("쇼다운 결과 확률"),
                div(class="lab-outcomes",
                    div(class="lab-outcome", span("승리"), strong(fmt(e$win))),
                    div(class="lab-outcome", span("무승부"), strong(fmt(e$tie))),
                    div(class="lab-outcome", span("패배"), strong(fmt(e$loss)))))
        ),
        p(strong(uncertainty),
          if (e$exact)
            paste0(" · 합법적인 결과 ", format(e$n,big.mark=","), "개를 계산했습니다.")
          else
            paste0(" · ", format(e$n,big.mark=","), "회 시뮬레이션했습니다.")),
        p(strong("현재 최고 족보: "),lab_rank_name(r$hand$rank)),
        p(next_info),
        card_odds,
        if (!is.null(extra)) tags$small(extra),
        tags$hr(),
        tags$small("족보별 확률은 리버에서 만들 수 있는 최상의 5장 족보를 기준으로 합니다. ",
                   "스트레이트 플러시에는 로열 플러시도 포함됩니다. ",
                   "추정 레인지에서는 상대의 미확인 블로커를 보드 완성 확률에 반영하지 않습니다. ",
                   "상대 카드를 직접 지정하면 해당 블로커가 반영됩니다.")
      )
    })
    output[[paste0(prefix,"_distribution")]] <- renderPlot({
      r <- calculation()
      req(!is.null(r), is.null(r$error))
      vals <- 100*r$dist
      old <- par(mar=c(8,4.5,2,1))
      on.exit(par(old),add=TRUE)
      barplot(vals, names.arg=names(vals), las=2, col="#6ba98b",
              border=NA, ylab="정확 확률 (%)",cex.names=.78,
              main="리버 최종 족보",ylim=c(0,max(5,vals)*1.15))
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
        fractions <- c("1/4 팟"=.25, "1/3 팟"=1/3,
                       "1/2 팟"=.5, "3/4 팟"=.75, "풀팟"=1)
        amounts <- pmin(floor(x$pot * fractions + .5), floor(x$stack + .5))
        sizes_ui <- div(class="lab-math-card",
                        h5("베팅 사이즈 예시"),
                        p("입력한 팟과 유효 스택을 기준으로 계산했습니다 (1달러 단위 반올림)."),
                        div(class="lab-size-menu",
                            lapply(seq_along(fractions), function(i)
                              div(class="lab-size-chip",
                                  strong(fmt_money(amounts[i])),
                                  tags$small(names(fractions)[i]))),
                            div(class="lab-size-chip", strong(fmt_money(x$stack)), tags$small("올인"))),
                        tags$small("예시 사이즈이며 솔버 추천값은 아닙니다. 팟이 작으면 반올림된 금액이 연습 게임의 최소 베팅액 $2보다 작을 수 있습니다.")
        )
      } else {
        call_price <- x$bet/(x$pot+2*x$bet)
        mdf <- x$pot/(x$pot+x$bet)
        sizes_ui <- div(class="lab-math-card",
                        h5("팟 오즈 · 손익분기 에퀴티"),
                        p(sprintf("상대가 %2$s 팟에 %1$s를 베팅했습니다. 콜 비용은 %3$s입니다.",
                                  fmt_money(x$bet),fmt_money(x$pot),fmt_money(x$bet))),
                        p("이후 베팅이 없다고 가정할 때, 팟 오즈 기준으로 필요한 최소 에퀴티:"),
                        div(class="lab-odds-number",sprintf("%.1f%%",100*call_price)),
                        p(sprintf("선택한 최초 상대 레인지 대비 추정 쇼다운 에퀴티: %.1f%%",
                                  100*r$equity$equity)),
                        tags$small("주의: 실제로 베팅한 상대의 레인지는 처음 선택한 레인지와 다를 수 있습니다. 두 수치를 비교한 결과만으로 콜/폴드를 자동 결정해서는 안 됩니다."))
      }
      div(
        div(class="lab-summary-callout",
            div(class="lab-eyebrow","우선 확인할 전략 포인트"),
            h4(easy$headline),
            p(easy$explanation)),
        div(class="lab-guide-grid",
            div(class="lab-guide-box",
                h5("1. 핸드 강도 및 보드 텍스처"),
                p(strong("현재 최고 족보: "),lab_rank_name(r$hand$rank)),
                p(strong("보드 텍스처: "),easy$board),
                p(strong("드로우: "),easy$draws)),
            div(class="lab-guide-box",
                h5("2. 포지션 및 스트리트 플랜"),
                p(easy$street_tip),
                p(easy$position_tip))
        ),
        sizes_ui,
        tags$details(class="lab-more-details",
                     tags$summary("심화 전략 보기 (선택)"),
                     p(strong("GTO 참고 설명: "),st$baseline),
                     p(strong("이 스트리트의 핵심: "),st$street),
                     if (x$facing == "bet") {
                       p(sprintf("심화: %2$s 팟에 대한 %1$s 베팅의 최소 방어 빈도(MDF)는 %3$.1f%%입니다. 이는 레인지 전체의 기준이지, 지금 이 핸드로 반드시 콜해야 한다는 뜻은 아닙니다.",
                                 fmt_money(x$bet),fmt_money(x$pot),100*mdf))
                     } else {
                       p("균형 잡힌 전략은 강한 핸드로도 체크하고 드로우로도 베팅합니다. 정확한 빈도는 양쪽 레인지와 베팅 사이즈에 따라 달라집니다.")
                     },
                     p(strong("참고: "),"학습용 안내이며 솔버가 계산한 GTO 액션이나 빈도는 아닙니다."))
      )
    })
  })
  
  # ==========================================================
  # OUTPUTS
  # ==========================================================
  
  output$hand_number <- renderText({
    paste0("핸드: ", state$hand_number)
  })
  output$button <- renderText({
    paste0("버튼: ", ifelse(state$button == HUMAN, "나", "봇"))
  })
  output$street <- renderText({
    paste0("스트리트: ", ko_street(shown_street()))
  })
  output$pot <- renderText({
    paste0("팟: $", format(total_pot(state), nsmall = 0))
  })
  output$board_table_pot <- renderText({
    paste0("$", format(total_pot(state), nsmall = 0))
  })
  output$board_table_street <- renderText({
    ko_street(shown_street())
  })
  output$blinds <- renderText({
    "블라인드: $1/$2"
  })
  output$human_stack <- renderText({
    paste0("내 스택: $", format(state$stacks[[HUMAN]], nsmall = 0))
  })
  output$bot_stack <- renderText({
    paste0("봇 스택: $", format(state$stacks[[BOT]], nsmall = 0))
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
      span(class="action-badge", "액션"),
      if (isTRUE(bot_is_thinking()))
        span(class="thinking-indicator", paste0("생각 중 · ", ko_street(state$street)))
    )
  })
  output$human_action_badge <- renderUI({
    if (isTRUE(reveal_in_progress()) || isTRUE(state$hand_over) ||
        !identical(state$to_act, HUMAN)) return(NULL)
    span(class="action-badge", "액션")
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
      return("<span style='opacity:.6;'>카드 없음</span>")
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
          "<span style='opacity:.7;'>플랍 공개 중...</span>"
        else
          "<span style='opacity:.7;'>플랍을 기다리는 중...</span>"
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
      return(div(class = "winner-box", strong("커뮤니티 카드 공개 중...")))
    }
    if (state$showdown) {
      human_rank <- evaluate_seven(c(state$human_cards, state$board))
      bot_rank <- evaluate_seven(c(state$bot_cards, state$board))
      result_text <- switch(
        state$winner,
        Human =
          paste0("승리! $", total_pot(state), " · 족보: ", ko_hand(human_rank$name), "."),
        Bot =
          paste0("봇 승리 · $", total_pot(state), " · 족보: ", ko_hand(bot_rank$name), "."),
        Tie =
          paste0("팟 분배 · 양쪽 족보: ", ko_hand(human_rank$name), ".")
      )
    } else {
      result_text <- paste0(
        ifelse(state$winner == HUMAN, "플레이어", "봇"),
        " · 상대 폴드로 승리"
      )
    }
    div(
      class = "winner-box",
      strong(result_text),
      br(),
      actionButton("next_hand_result", "다음 핸드", class = "btn-warning")
    )
  })
  
  # ==========================================================
  # ACTION HELP
  # ==========================================================
  
  output$action_help <- renderUI({
    if (isTRUE(reveal_in_progress())) {
      return(p("커뮤니티 카드 공개 중...", style = "color:#6b7280;"))
    }
    if (state$hand_over) {
      return(p("핸드가 종료되었습니다. 다음 핸드를 시작하세요.", style = "color:#6b7280;"))
    }
    if (!identical(state$to_act, HUMAN)) {
      return(
        p(
          if (identical(state$to_act, BOT)) {
            if (isTRUE(bot_is_thinking())) {
              paste0("봇이 생각 중 · ", ko_street(state$street), "...")
            } else {
              "다음 스트리트를 준비하는 중..."
            }
          } else "핸드 종료 대기 중...",
          style = "color:#6b7280;"
        )
      )
    }
    to_call <- current_bet_to_call(state, HUMAN)
    if (state$stacks[[BOT]] <= 0) {
      return(p(paste0("봇이 올인했습니다. $", to_call,
                      "를 콜하거나 폴드하세요. 레이즈는 할 수 없습니다.")))
    }
    min_raise <- minimum_raise_to(state, HUMAN)
    if (to_call == 0) {
      p(paste0("체크하거나 베팅할 수 있습니다. 최소 레이즈 목표액: $", min_raise, "."))
    } else {
      p(paste0("콜 필요액: $", to_call, ". 최소 레이즈 목표액: $", min_raise, "."))
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
      return(HTML("<div style='opacity:.6;'>아직 액션이 없습니다.</div>"))
    }
    entries <- vapply(
      rev(state$log),
      function(x) {
        paste0("<div style='margin-bottom:5px;'>", htmltools::htmlEscape(ko_game_log(x)), "</div>")
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
