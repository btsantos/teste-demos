if(FALSE) {
  shuffle <- function(cards) {
    random <- sample(1:52, size = 52)
    cards[random, ]
  }
  deck2 <- shuffle(deck)
  deal(deck2)
}

deck <- read.csv("~/R_code/deck.csv", stringsAsFactors=FALSE)
setup <- function(deck) {
  DECK <- deck
  DEAL <- function() {
    card <- deck[1, ]
    assign("deck", deck[-1, ], envir = parent.env(environment()))
    card
  }
  SHUFFLE <- function(){
    random <- sample(1:52, size = 52)
    assign("deck", DECK[random, ], envir = parent.env(environment()))
  }
  list(deal = DEAL, shuffle = SHUFFLE)
}
