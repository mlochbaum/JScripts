NB. Linear algebra utilities, for real and integer matrices

NB. matrix product
mp =: +/ .*

NB. ---------------------------------------------------------
NB. Convert matrix y to row-echelon form.
rowreduce =: 3 :0
if. 0=*/$y do. y return. end.
lr=. 0 (i.&1@:~:) {."1 y
if. lr=#y do. 0,.rowreduce }."1 y return. end.
y =. lr ((%{.)@:{ , {. , (>:@[}.])) y
y =. (- {. */~ (0 , }. % {.)@:({."1)) y
({.y) , rowreduce}.y
)

rank =: rowreduce #@-. 0$~$@{.
nullity =: #-rank

NB. =========================================================
NB. Row reduction for integer matrices
rowreduceint =: 3 :0
if. 0=*/$y do. y return. end.
y=.(* <:@+:@(>&0)@{."1) y

n=.+/*{."1 y
if. n=0 do. 0,.rowreduceint }."1 y return. end.
while. n>1 do.
  i=.(i.<./) (+ _*=&0) {."1 y
  y=.y - (i{y)*/~ 0 i} (<.@% i&{) {."1 y
  n=.+/*{."1 y
end.
y=. \:~ y
({.y) , 0,. rowreduceint 1 1}. y
)

NB. Find the first nonzero element in each row
firstel =: ({~ 0&(i.&1@:~:))"1
NB. Gives some numbers related to the image and kernel of an integer matrix.
span =: rank ({. ;&:firstel }.) rowreduceint@:(,. =/~@i.@#)

backsub =: 3 :0
n=.# (-. 0$~$@{.) y
firstind=. i.&1@:~:&0
for_i. i.&.<: n do.
  'ind pivot'=.(firstind ([ , {) ]) i{y
  mult=.n {. - pivot <.@%~ ind ([ {"1 {.) y
  y=. (mult */ i{y) + y
end.
)

NB. =========================================================
NB. y is a symmetric matrix
NB. returns r;s with r diagonal, s lower triangular
NB. such that y=s.r.s(T)
symreduce =: 3 :0
if. 1 1-:$y do. y;(1 1$1) return. end.
n=. #y
I=. =/~i.n NB. identity matrix
i=. <:+:I  NB. 1 on diagonal, _1 elsewhere--i*p = (%.p) for Gaussian p

m=.n {. % (<0 0){y
NB. p*i = %.p is a gaussian matrix so that pi.y.pi(T) has only
NB. a diagonal element in the first row and column
p=.(I * -. n{.1) + m *"1 y
y=.pi +/ .* y +/ .* |:pi=.p*i
'r0 s0'=.symreduce 1 1}.y

r=.(n{. (<0 0){y) ,. 0, r0 NB. diagonal matrix r
s=.({."1 p) ,. 0, s0 NB. lower triangular s
r;s
)

NB. =========================================================
NB. y is a list of integers
NB. returns a matrix m such that ((# {. +./) y) -: m +/ .* y
GCD =: 3 :0
's y'=.(([: <:@+: >&0) ,: |) y
r=.(% +./) y
m=.r,. I=.=i.#r
while. 1 < +/@:*: r do.
  'i n'=. ((i.,]) <./@(-.&0)) r
  m0=.(i{m) */~ 0 i} r<.@%n
  r=.{."1 m=. m - m0
end.
(s*"1 }."1 m) {~ (, (i.#r)&-.) r i. 1
)

NB. ---------------------------------------------------------
extendedGCD =: 3 :0
's y'=.(([: <:@+: >&0) ,: |) y
r=.(% +./) y

NB. find the smallest set to give a GCD of 1
arr=.0
whilst. n>1 do.
  arr=.r +./ arr
  ind=.($ #: (i. <./)@,) arr
  n=.(<ind) { arr
end.

p=. ind , ind2=.(i.@#r)-.ind
  
m1=.GCD ind{r
m2=.((_1&([ |. |."1)) - 1&|."1) (* =@i.@#) ind2{r

s *"1 p /:"1~ m1 (((#r){.[) ,"1 ((-#r){.])) m2
)

NB. ---------------------------------------------------------
NB. Places the given matrix in 
rowreduceint1 =: 3 :0
step=. +/ .*~ GCD@:({."1)
if. 0=*/$y do. y return. end.
if. 0=+./{."1 y do. 0,.rowreduceint1 }."1 y return. end.

while. 1 < +/ ~:&0 {."1 y do.
  y=. step&.|:@step y
end.
({.y) , 0,. rowreduceint1 1 1}. y
)
