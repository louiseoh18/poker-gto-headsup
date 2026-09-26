# Texas Hold'em Heads-Up Simulator & Strategy App

## Why I Built This

I wanted to play poker for fun without going to a casino, so I recruited my 
cousin. Since she was my only opponent, heads-up poker it was. Unfortunately, 
she only knew the basic rules and would somehow go all-in with 72o but fold AKs 
pre-flop. Playing against her was essentially playing against a random number 
generator.

I built this app for both of us, but mostly for her, to get a better feel for 
when to raise, call, check, or fold, and understand the probabilities behind 
those decisions.

Hopefully, our future games will involve a little more strategy and a lot less 
gambling..... :P

## App Overview

The app is built using **R Shiny** and consists of five tabs.

### 1. Practice Game

Play heads-up No-Limit Texas Hold'em against a GTO-inspired bot.

- Play complete hands from pre-flop to showdown.
- Practice folding, checking, calling, raising, and choosing bet sizes.
- Play against a bot that adjusts its decisions based on hand strength, board texture, and betting situations.
- Review betting actions and hand results.

### 2. Pre-Flop Strategy

Explore simplified heads-up pre-flop strategy charts.

- Review opening ranges from the Small Blind (SB)/Button.
- Explore Big Blind (BB) defending ranges and 3-betting strategies.
- Learn how position, starting hands, and stack depth influence pre-flop decisions.

### 3–5. Flop, Turn & River Analysis

Analyze different post-flop scenarios using interactive probability calculations and strategy explanations.

- **Hand selection:** Import hands directly from the Practice Game to analyze hands encountered during game play (or manually enter your hole and community cards).
- **Probability analysis:** Estimate your showdown equity and the probability of making different poker hands.
- **Opponent simulations:** Compare your hand against different opponent assumptions.
- **Betting scenarios:** Adjust the pot size, effective stack, and opponent's betting action.
- **Strategy explanations:** Explore pot odds, bet sizing, value betting, bluffing, and other considerations for each street.

These tabs let you experiment with hypothetical scenarios or revisit hands from the Practice Game to understand how each additional community card changes your hand strength and potential betting decisions.

## Important Note

This app uses simplified, GTO-inspired strategies rather than an actual GTO solver. Its purpose is to provide an interactive environment for practicing poker and developing an understanding of probability and betting decisions.