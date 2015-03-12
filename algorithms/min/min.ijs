NB. Routine for multidimensional minimization using Powell's method
NB. as defined in Numerical Recipes in C
NB. uses Brent's method linear minimization


NB.================================================================================

NB. linear minimizer in multidimensions

NB. y is (initial value ,: direction vector);(tolerance)

NB. returns (lowest point);(function value)

   linmin=:1 :0

GOLD=.-:>:%:5 NB. golden ratio (phi)
'pts tol' =. y
'start dir' =. pts
f=.u@:(start + dir&*) "0
val=. _1 1
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
if. (__ = (<0 1){xwv) +. (x |@- xm) <: (+:tol) - (-: -~/ bounds)
  do. ({: ;~ start + dir * {.) {.xwv return. end.

if. ((|e) > tol)*.iter>2 do.
  etemp=.e
  e=.d
  NB. construct a parabolic interpolation
  rq=.(,:|.)/ |: (0 -"1&({&xwv) 1 2)
  d=.- ((* *:)~/ rq) %&:(-~/) :: _: (+: */ rq)

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

({: ;~ start + dir * {.) x
)


NB.================================================================================

dot=:+/@:*"1
mag=:+/&.:*:"1
mp=:+/ .*
op=:(,.@[ mp ,:@])"1


NB. Powell's method multidimensional minimization in n dimensions

NB. adverb: u is function,
NB. y is (initial guess),(starting minimization step size),
NB. (tolerance on linear minimization), (final tolerance).

NB. final tolerance determines decrease in function value before
NB. search is terminated

NB. records and updates an nXn matrix xi giving a set of directions
NB. at each step, performs a linear minimization in each direction
NB. then updates xi based on results

   powell=: 1 :0

p=._3 }. y NB. starting value
'step ltol tol'=. _3 {. y NB. step size, linmin tolerance, final tolerance
func=:u "1

fret=.func p NB. records current function value

pt=.p

xi=. =/~ i. #p NB. start xi with standard basis vectors

ITMAX=.50
iter=.0
while. ITMAX > iter=.>:iter do.
  fp=.fret
  d=.$0 NB. will record changes in function value

  NB. for each direction, perform a linear minimization, move to the minimum,
  NB. and record the size of decrease in the function
  for_xit. xi do.
    'p newfret'=. func linmin (p,:step*xit) ; ltol
    d=. d , (fret=.newfret) -~ fret
  end.

'del ibig' =. ((],i.) >./) d NB. largest decrease;index of decrease

  NB. termination: end if total function change is less than fractional tolerance
  if. (+: | fp-fret) <: tol * fp +&mag fret do. p;fret return. end.

  NB. pt is the new point, that is, p
  NB. xit is the overall direction the minimization went in
  NB. ptt is the point projected along this direction
  'ptt xit pt' =. (3 2$2 _1 1 _1 1 0) (+/ .*) (p,:pt)
  fptt=.func ptt

  NB. if the function continues decreasing through ptt and does not appear to have
  NB. a large second derivitive, replace the direction of largest increase with
  NB. xit, the direction obtained from the previous applications of linmin.
  NB. otherwise keep the old direction set.
  NB. the direction of largest increase is removed to avoid linear dependence in the
  NB. direction set.
  if. (fptt < fp) do.
    t=. (del* *:fp-fptt) -~ +: (fp+fptt - +:fret)* *: fp- fret+del
    if. t<0 do.
      'p fret'=. func linmin (p,:step*xit) ; ltol
      xi=. xit ibig} xi
    end.
  end.
end.

NB. if iterations are expended, return current lowest point.
p;fret
)


NB.================================================================================

NB. Conjugate gradient minimization in multidimensions

NB. same input as powell's method, except u is the gradient
NB. and v is the function

NB. u is rank one with an output the same shape as its input.

NB. tolerance specifies the  size of gradient when algorithm is finished.

   conjgrad=: 2 :0
grad=:u
func=:v
pt=._3 }. y
'step ltol tol'=. _3 {. y

NB. g is (current gradient ,: previous gradient)
NB. d is direction
g=.,:~ 0:^:(_=|) grad pt
d=.($pt)$0

while. tol < mag {.g  do. NB. end when gradient is small
B=.(({. dot -/) % dot~@:{:) g NB. Polak-Ribiere algorithm
d=.(-{.g) + B*d NB. compute new direction

NB. linear minimization
f1dim=:func@:(pt + d&*)
pt=. 0 {:: func linmin (pt,:step*d) ; ltol

g=.(0:^:(_=|) grad pt) ,: {.g
end.
pt
)


NB.================================================================================

NB. BFGS Quasi-Newton method in multidimensions

NB. same input as conjugate gradient method
NB. optional x provides starting inverse Hessian matrix (otherwise I is used)

   BFGS=: 2 :0
(=/~ i. 3-~#y) u BFGS v y
:
grad=:u
func=:v
pt=._3 }. y
'step ltol tol'=. _3 {. y
B=.x NB. inverse of Hessian (approximate
g=.grad pt

ITMAX=.50
iter=.0
while. ITMAX > iter=.>:iter do.

if. tol >: mag g do. pt return. end.

d=.- B dot g NB. direction for linear minimization

NB. linear minimization
f1dim=:func@:(pt + d&*)
ptt=. 0 {:: func linmin (pt,:step*d) ; ltol

NB. update pt, grad, set dp and dg to their differences
dp=.(pt=.ptt)-pt
dg=.(g=.grad pt)-g

NB. update inverse Hessian matrix
inner=.dp dot dg
outer=.dp op dg
U=. (op dp) * 1 + inner %~ (dot B&dot) dg
V=. (B mp |:outer) + (outer mp B)
B=. B + inner %~ U-V
end.
pt
)