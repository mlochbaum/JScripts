NB. Perlin noise generators in 1, 2, and 3 dimensions.
NB. init must be run before any noise function--it initializes all of the
NB. random values used.
NB. For each noise function, y is an array of points at which to sample.
NB. Except for noise1, where scalars are used for points, the points must
NB. have length corresponding to the noise function used.
NB. Thus the argument to (e.g.) noise2 may be any array of numbers whose
NB. shape ends in 2.

B =: 16b100
s =: 0 0 3 _2&p.

noise1 =: 3 : 0
b =. B | 0 1+/<.y
r =. 0 _1+/1|y
((,&,:~-.) s {.r) +/@:* g1 {~ p {~ b
)

NB. noise2 is faster but more complicated.
noise2 =: 3 : 0 &. |:
R =. ("_1 _)("_ _1)
b =. B | 0 1 +R <.y
r =. 0 _1 +R ]1|y
bij =. p {~ (+R {&p)~/ b
a =. (,"0 R~/ r) +/"1@:* g2 {~ bij
'sx sy' =. s {."_1 r
((,&,:~-.) sy) +/@:* ((,&,:~-.) sx) +/@:(*"_ _1"_1) a
)
noise2a =: 3 : 0
b =. B | 0 1 +/~"1 <.y
r =. 0 _1 +/~"1 ]1|y
bij =. (+"0 _ {&p)/"2 b
a =. (,"0"0 _/"2 r) +/"1@:* g2 {~ bij
'sx sy' =. (|:~ _1 |. i.@#@$) s {."1 r
((,"0~-.) sy) +/"1@:* ((,"0~-.) sx) +/"2@:* a
)

noise3 =: 3 : 0
b =. B | 0 1 +/~"1 <.y
r =. 0 _1 +/~"1 ]1|y
bij =. (+"0 _ {&p)/"2 b
a =. ((,"0 1"0 _)`(,"0/)/"2 r) +/"1@:* g3 {~ bij
't sy sz' =. (|:~ _1 |. i.@#@$) s {."1 r
((,"0~-.) sz) +/"1@:* ((,"0~-.) sy) +/"2@:* ((,"0~-.) t) +/"3@:* a
)

init =: 3 : 0
sh =. (+:>:B)&$
p =: sh ?~B
g1=: sh <:+:B ?@$ 0
'g2 g3' =: sh@:(% +/&.:*:"1)@:([: <:@+: 0 ?@$~ B&,)&.> 2 3
EMPTY
)

init ''

NB. Sample generation of noise at many scales
NB. Add the layers of n to get fractal noise
NB. g2 =. (+:>:B) $ (% +/&.:*:"1) B {. (#~ 1>:+/&.:*:"1) <:+:((2*B),2)?@$0
NB. n =. (2^i.8) ([*[noise2@:%~?@[+])"0 _ ,"0/~ i.512
