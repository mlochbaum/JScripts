NB. routine for bracketing a minimum of a function
NB. given initial guess

NB. adverb. u is the function
NB. y is (list of two starting x values) , tol

NB. tolerance should not be lower than the square root
NB. of floating-point precision

NB. finds the bounds for the minimum so that the distance
NB. between them is less than twice the tolerance.
NB. returns the lowest found value

NB. returns (x,f x) for the lowest point found

   linmin=:1 :0

GOLD=.-:>:%:5 NB. golden ratio (phi)
f=.u f. "0
val=. }: y
tol=. {: y
fval=.f val
'val fval'=.(val ,: fval) \:"1 fval

NB. moves downhill, increasing segment length
NB. by a ratio of phi each step
NB. stop when a min is bracketed
whilst. >:/ }. fval
do.
val=.(, {: + GOLD * -~/) _2 {. val
fval=.(_2 {. fval) , f {:val
end.

val=. val,.fval


NB. Use Brent's method to decrease bounding interval

NB. keeps track of bounds, x, the lowest point, and
NB. previous values of x (array xwv).

NB. At each step, the method creates a parabolic
NB. interpolation through xwv and determines
NB. whether to use it.
NB. If it is unusable, the method uses a default of
NB. multiplying the largest difference between x
NB. and the bounds by CGOLD, splitting it into golden
NB. sections.

ITMAX=.100
CGOLD=.-:3-%:5  NB. 2-golden ratio
e=.0 NB. keeps track of movement on the step before last

bounds=.({."1) 0 _1 { val=./:~ val
xwv=.3 # ,: 1{val NB. all x values are initially set equal

iter=.0
while. ITMAX > iter=.>:iter
do.

x=.(<0 0) { xwv
xm=.-:+/bounds
NB. test for done
if. (x |@- xm) <: (+:tol) - (-: -~/ bounds) do. {.xwv return. end.

if. ((|e) > tol)*.iter>2 do.
  etemp=.e
  e=.d
  NB. construct a parabolic interpolation
  rq=.(,:|.)/ |: (0 -"1&({&xwv) 1 2)
  d=.- (-~/ (* *:)~/ rq) % (+: -~/ */ rq)

  NB. determine if fit is acceptable:
  NB. fit must be within bounds (and not within tol of them),
  NB. move from x less than half of the movement on the step before last (etemp)
  if. (d >:&| -:etemp) +. (bounds + 1 _1 * tol) (1~:I.) x+d do.
    d=.CGOLD * e=. ({~ </@:|) bounds - x NB. unnacceptable: use default
  end.

else.
  d=.CGOLD * e=. ({~ </@:|) bounds - x NB. no interpolation made: default
end.

NB. assign new point to u after ensuring that it is more than
NB. tol away from x.
u=.(,f) x + (tol >. |d) * *!.0 d

NB. move points around
x=.{.xwv
NB. replace the appropriate bound with the higher (function value)
NB. of x and u so that the lower is within the bounding interval. 
bounds =. /:~ (bounds ,: x,&{.u)&({"0 1~) (~:/ , {:) (>&0 u-x)
NB. move u to its proper place in xwv
uind =: (~. xwv) I.&:({:"1) u
xwv=. }: (uind&{. , u , uind&}.) xwv

end.
'too many iterations'
)
