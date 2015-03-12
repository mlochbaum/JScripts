NB. Lempel-Ziv-Welch compression and extraction.

NB. ---------------------------------------------------------
NB. y is an array of integers less than (2^initial bit size)
NB. x gives (initial bit size),(final bit size)
compress=:4 :0
'init fin' =. x
assert. (*./@:>:&0 *. *./@:<&(2^init)) y
dic=. i. max=. 2^init
output =. $0

while. #y do.
  l=. >:^:(dic e.~ max #. {.!.max&y)^:_  ]1

  if. (2^fin) > $dic do. dic=.dic, max #. l{.y end.
  output=.output, dic i. max #. (<:l){.y
  y=. (<:l) }. y
end.
output
)


NB. ---------------------------------------------------------
NB. y is a compressed string of integers
NB. x is the input used to compress y
extract=:4 :0
'init fin'=.x
dic=. i. max=. 2^init

input=.,start=. {.y
for_r. }.y do.
  if. r = #dic do.
    dic=.dic,s=. (max*start)+ {. max #.^:_1 start
    else. dic=.dic,(max*start)+ {. max #.^:_1 s=.r{dic
  end.
  input =. input , max #.^:_1 start=.s
end.
)
