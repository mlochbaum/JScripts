NB. Formal parse for J.
NB. To parse a sentence s,
NB. (parse splitwords s)
NB. Returns an entity as described below.

NB. =========================================================
0 :0
  An entity is either:
  A primitive, which is a formal object with a type,
    represented by <(type);(name)
  An application, represented in lisp-like style by
    <(type);(name of operator);(boxed list of arguments)
)
type =: (0 {:: >)"0
name =: (1 {   >)"0
args =: (2 {:: >)"0
order =: 2>.type
0 :0
  A type is an integer:
    -2: (
    -1: )
    0 : noun
    1 : verb
    2 : adverb
    3 : conjunction
    _ : error
  The order of the function, if it has (type e. i.4), is given by (2>.type) .
  Applications obey:
    (1 2 e.~ order)
    (order *./@:>: order@>@:args)  NB. equality holds if it is a fork

  To parse an input string, first split into words and represent
    each as a primitive. Then apply the parsing rules until there
    is only one primitive left.
)

NB. =========================================================
NB. primitive formation
str =: 3 :'5!:5<''y'''
gettype =: 3 :0 "0@:(boxopen"1)
  if. y e. parens do. _2+parens i.y
  elseif. y e. nouns do. 0
  elseif. ({.>y) e. '_0123456789''' do. 0
  elseif. y e. verbs do. 1
  elseif. y e. adverbs do. 2
  elseif. y e. conjunctions do. 3
  elseif. do. (4!:0 y) { 0 2 3 1 _ _
    NB. noun, adverb, conjunction, verb, invalid, unused
  end.
)

parens=: ;:'()'
nouns=: ;:'a.a:'
verbs=: ;: LF -.~ 0 :0
=<<.<:>>.>:_:++.+:**.*:--.-:%%.%:^^.$$.$:~.~:||.|:,,.,:;;:##.#:!
/:\:[[:]{{.{:{::}.}:".":??.A.C.e.E.i.i:I.j.L.o.p.p..p:q:r.s:u:x:
_9:_8:_7:_6:_5:_4:_3:_2:_1:0:1:2:3:4:5:6:7:8:9:
)
adverbs=: ;:'~//.\\.}b.f.M.t.t:'
conjunctions=: <;._1 LF -.~ 0 :0
 ^: . .. .: : :. :: ;. !. !: " ` `: @ @. @: & &. &.: &:
 d. D. D: H. L: S: T.
)

splitwords =: [:(gettype ;&.> ]) ;:

NB. =========================================================
NB. parsing
NB. we assume that =. and =: are not present.
reduce =: 1 :0
  u/ y
  :
  for_z. x do. y=.z u y end.
)

parse =: }.@}: @: (parsestep/) @: ((<_2;,'('),,&(<_1;,')'))
NB. pattern matching of types for the parser
match =: ([: *./ e.&>~)"1
any =. _2+i.2+4
matches =: ".;._2 ]0 :0
_2      ; 1    ; 0    ; any
_2 0 1 2; 1    ; 1    ; 0
_2 0 1 2; 0    ; 1    ; 0
_2 0 1 2; 0 1  ; 2    ; any
_2 0 1 2; 0 1  ; 3    ; 0 1
_2 0 1 2; 0 1  ; 1    ; 1
_2      ; (i.4); (i.4); any
any     ; _2   ; (i.4); _1
)

monad=:dyad=:adverb=:conjunction=:fork=:hook=:paren=:]
functions =: monad`({.,monad@}.)`dyad`adverb`conjunction`fork`hook`paren`]
parsestep =: 4 :0
  pattern =. type x , 3{.!.(<_;'') y
  number  =. matches (1 i.~ match"1) pattern
  y =. functions@.number y
  if. number<#matches do. x parsestep y return. end.
  x,y
)

isfork =: 3 :0
  if. (1=type y)*.(3=#>y)*.(1=type name y) do. 1 = type {: args y else. 0 end.
)
'atop bond under' =: <"0 splitwords '@:&:&.:'
hookc =: <3;'hook'

NB. m is (result type),(number of arguments)
apply =: 1 :0
  (({.m) <@; (, <@:(({:m)&{.))) , ({:m)}.]
)
monad =: ({. $: }.) : (}.@] ,~ (4 :0 {.))
  if. 0 = type x do. x return. end.
  if. isfork x do. (name x) dyad (args x) {.@monad"0 y return. end.
  if. 3 = type name x do.
    if. atop -: name x do. (args x) monad reduce y return. end.
    if. hookc -: name x do. (args x) ({:@[ dyad ] , {.@[ monad ]) y return. end.
  end.
  x 0 1 apply y
)
dyad =: (1&{ $: {.,2&}.) : ((2}.]) ,~ (4 :0 (2&{.)))
  if. isfork x do. (name x) dyad (args x) {.@dyad"0 y return. end.
  if. 3 = type name x do.
    if. atop -: name x do. (args x) ({.@[ monad {:@[ dyad ]) y return. end.
    if. hookc -: name x do. (args x) ({.@[ dyad {.@] , {:@[ monad {:@]) y return. end.
  end.
  x 0 2 apply y
)
adverb =: (1&{ $: {.,2&}.) : (4 :0)
  x 1 1 apply y
)
conjunction =: (1&{ $: {.,2&}.) : (4 :0)
  x 1 2 apply y
)
fork =: (1&{ $: {.,2&}.) : (4 :0)
  if. ({.y) -: <1;'[:' do. atop conjunction x,}.y return. end.
  x 1 2 apply y
)
hook =: (1&{ $: {.,2&}.) : (4 :0)
  hookc conjunction x,y
)
paren =: 1&{ , 3&}.
