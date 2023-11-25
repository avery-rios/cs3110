type suit = Club | Diamond | Heart | Spade

type rank =
  R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10
  | Jack
  | Queen
  | King
  | Ace

type card = {suit: suit; rank: rank}

let ace_clubs = {suit= Club; rank = Ace}

let queen_hearts = {suit = Heart; rank = Queen}

let two_diamod = {suit = Diamond; rank = R2}

let seven_spade = {suit = Spade; rank = R7}