NB. Yet another game of life implementation.
NB. Allows for many different rules.

NB. =========================================================
NB. Neighborhood verbs
NB. When given an array, a neighborhood verb should return the sum of
NB. values in a point's neighborhood.

NB. 1-dimensional neighborhood
n1dim=:3 +/\ (0,,&0)
NB. Moore neighborhood in 2d
neighbors=: -~ [: n1dim"1 n1dim
NB. Von Neumann neighborhood in 2d
VN_neighbors=: +: -~ n1dim"1 + n1dim

NB. Moore neighborhood in multidimensions
neighborsndim=: -~ n1dim@:(0&|:)^:(#@$)

NB. =========================================================
NB. Execute a step of Conway's game.
life=: [: (0.5 >: 3 |@- ]) -:+neighbors

pad =: ([: 0&,^:(+./@{.) |.@|:)^:4
NB. Give the next x states of Conway's game starting at y.
prog=: ([: {&' '&.> life@pad&.>@]^:(<@[)) <

NB. =========================================================
NB. x is rule in standard 2d CA form:
NB. 2 boxes consiting of number of neighbors for birth, survival.
NB. For Conway's game of life use (3;2 3).
NB. Uses the Moore neighborhood.
NB. Advances position y by one step.
cellauto=:4 :0
next=.(e. {::&x)"0
(next~ neighbors) y
)

NB. ---------------------------------------------------------
NB. u is a neighborhood verb.
NB. x contains a box for each state:
NB. each box contains (value returned by neighbor,:value to become).
NB. An omitted value is assumed to be 0; if second
NB. row is omitted values are assumed to be 1.
NB. Advances position y by one step.
CAgeneral=:1 :0
:
rule=.,:&1^:(1>:#@$) &.> x
(u y) (((i.~ {.) { ::0: {:@]) {::&rule)"0 y
)

NB. ---------------------------------------------------------
NB. x is a 1-dimensional rule as a binary list or decimal.
NB. Advance position y by one step.
CA1dim=:4 :0
rule=.|. _8 {. #:^:(0=#@$) x
inp=.(,~ 0 #~ 0 >. 2 - i.&1)@:|.^:2 y
3 (rule {~ #.)\ (0,,&0) inp
)
NB. m is the rule to use.
NB. Give an array of the next x states starting at y.
CA1dimprog=:1 :0
:
p=.m CA1dim^:(<x) y
rule=.|. _8 {. #:^:(0=#@$) m
(|."0 1~ i.&.-@#)^:(1{rule) p
)
