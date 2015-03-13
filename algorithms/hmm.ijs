NB. Tools for hidden Markov models

NB. =========================================================
NB. Given an HMM, produce a sample output.

NB. select a random element given a normalized probability distribution.
rand=:(+/\ I. ?@0:)

norm=:(%+/)"1

NB. x is the number of elements to generate.
NB. y is the generating rule, ie.
NB. (starting state probabilities);(transition probability matrix);(output probability matrix)
NB. so that if s and o are the number of states and outputs,
NB. ($&.> y) = s;(2#s);(s,o)
NB. Compute a random sequence using these probabilities
NB. and output (states);(observables)
generate=:4 :0
'start trans out'=.y
trans=.trans,start
state=._1
seq=.0 2$0

while. x>#seq do.
  state=. rand state{trans
  seq=. seq , state, rand state{out
end.
|:seq
)

NB. =========================================================
NB. Viterbi algorithm
NB. x is the generating rule and y is a sequence of observables.
NB. Return the most probable hidden state sequence.
viterbi=:4 :0
'start trans out'=.x

paths=.,. states=. i. #start
prob=:start * ({.y) {"1 out

for_o. }.y do.
  p=. |: (o {"1 out) *"1 prob * trans
  choice=. (i.>./)"1 p
  prob=:>./"1 p
  paths=.(choice{paths) ,"1 0 states
end.

paths {~ (i. >./) prob
)

NB. =========================================================
NB. Forward-backward algorithm
NB. For each method,
NB. x is the generating rule and y is a sequence of observables.
NB. forward and backward perform forward and backward passes.
NB. (<a b){(x fbprob y) gives the probability of state a at observable b.
NB. fbprob gives a list of the most probable state at each step.

forward=:4 :0
'start trans out'=.x
prob=.,: p=. start * ({.y) {"1 out

for_o. }.y do.
  p=. (o {"1 out) * p +/ .* trans
  prob=.prob , p
end.
)

backward=:4 :0&.|.
'start trans out'=.|.x
prob=.,:p=. trans +/@:*"1 ({.y) {"1 out

for_o. }.y do.
  p=. trans +/@:*"1 (o {"1 out) * p
  prob=.prob , p
end.
)

fbprob=: [: norm forward * backward
fwdbwd=: [: (i.>./)"1 forward * backward


NB. =========================================================
NB. Baum-Welch algorithm.
NB. x is the number of states, y is a sequence of observables.
NB. Return the most probable generating probabilities, with the same
NB. form as the right input to generate.
baum_welch=:4 :0
nstates=.x
nout=.>: >./ seq=. y

P=. ($%) nstates
A=. norm >: =/~ i. nstates
E=. norm >: (i.@[ =/ (| i.))/ nstates, nout
score=.__
s=:,p=. 0

while. p>score do.
  'start trans out'=.rule=.P;A;E
  'f b' =.rule (forward ; backward) seq

  score=.p
  p=. +/ {: f
  s=:s,p

  gamma=. norm f*b
  bout=. (}.b) * }:seq{"0 2|:out
  xi=. trans *"2 (gamma %&}: b) */"1 bout

  P=. {.gamma
  A=. xi %&(+/) }:gamma
  E=. (+/gamma) %~ |: seq +//. gamma
end.

(start;trans;out)
)
