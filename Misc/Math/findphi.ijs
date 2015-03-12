NB. Find a way to write a number approximately in terms of the
NB. golden ratio.

phi=:-:>:%:5
op=:],>:,<:,+:,-:,*:,%:,+&phi,-&phi,*&phi,%&phi
opp=: <;._2'] >: <: +: -: *: %: phi+ phi-~ phi* phi%~ '
opi=: <;._2'] <: >: -: +: %: *: phi-~ phi+ phi%~ phi* '
fapply=:4 :'opg@.x y'"0 _1
finvapply=:4 :'opgi@.x y'"0 _1
opg=:]`>:`<:`+:`-:`*:`%:`(+&phi)`(-&phi)`(*&phi)`(%&phi)
opgi=:({.opg), ,/ _2|.\ }.opg
nop=:11

NB. x is the depth at which to evaluate.
NB. y is the number to find.
NB. Return a sentence which, when run, gives y
NB. (usually accurate to around 2*x digits)
findphi=:3 :0
4 findphi y
:
p=:op^:x phi
r=.op^:x y
n=.((]{~ [: (i.<./) |@-)"0 1 (__,_,~/:~p)&([{~[:(,.<:)I.)) r
f=. (i.nop)&finvapply
nn=. (x#nop)$n
for_i. >:i.-x do. nn=.f"i nn end.
ir=.(i.<./)y|@-~ ,nn
ip=.p i. ir{n
'phi' ,~ ;:^:_1 (|.opi{~nop&#.^:_1 ir), (opp{~nop&#.^:_1 ip)
)
