NB. Cast from one type to another, if possible.

NB. =========================================================
NB. This script defines the verb typecast.
NB. x is the type to cast to (a power of two, as in the output of 3!:0).
NB. y is any noun.
NB. typecast will either convert y to type x or throw an error.
NB. If an error is not thrown, then (y -: x typecast y) returns 1.
NB. Furthermore, if any value z of type x satisfies (y -: z), then
NB. typecast may not fail.
NB. Note that (-:) uses tolerant comparison.

typecast =: typecast_ptype_

NB. The verb numcast accepts only numeric types and finds the closest
NB. number of type x to y (that is, minimizes (|y - x numcast y)).
NB. If multiple such numbers exist, the largest is chosen (e.g. numcast
NB. rounds up).
NB. numcast may not fail on numeric types.

numcast =: numcast_ptype_

NB. =========================================================
cocurrent 'ptype'

NB. Turn a text table into a boxed array
totab =: [: ((a: -.~ <@deb;._1)"1~ *./@:=&' ') ' ' ,. ];._2 

NB. Type numbers and nicknames
TYPE_TAB =: totab 0 : 0
NUM      NAME    SHORT  FULLNAME
1        B01     B      boolean
2        LIT     C      literal (character)
4        INT     I      integer
8        FL      F      double (IEEE floating point)
16       CMPX    Z      complex
32       BOX     A      boxed
64       XNUM    X      extended precision integer
128      RAT     Q      rational number
256      BIT     BT     bit boolean
1024     SB01    PB     sparse boolean
2048     SLIT    PC     sparse literal (character)
4096     SINT    PI     sparse integer
8192     SFL     PD     sparse floating point
16384    SCMPX   PZ     sparse complex
32768    SBOX    PA     sparse boxed
65536    SBT     SB     symbol
131072   C2T     C2     unicode (2-byte characters)
)
('TYPE_'&,&.> {.TYPE_TAB) =: (".@>&.>@{. , }.) <"_1 |:}.TYPE_TAB
tonum =: (TYPE_NUM {~ TYPE_SHORT&i.)@:boxopen@:,
toname=: TYPE_FULLNAME {~ TYPE_NUM&i.

NB. Check if a type is sparse
is_sparse =: [: (10&<: *. 15&>:) 2 <.@^. ]
NB. Convert to dense type
to_dense =: <.@%&1024

NB. =========================================================
NB. Conversion tables
NB. The two tables following define all possible conversions between
NB. dense (i.e. non-sparse) types.
NB. The verbs (c) and (ct) are defined in the body of (get_typecast).

NB. ---------------------------------------------------------
NB. Numeric conversions

NB. Zeros of various types
I0 =: -~2
F0 =: -~1.1
Z0 =: j.0
Q0 =: 0 * 1r2

Re =: 9&o.

makeconvtab =: [: (tonum@>@:({."1) ,&< }."1) }.@:totab
NUM_CONV =: makeconvtab 0 : 0 NB. Numeric conversions
     <-B-->   <--I--->   <-F-->   <-Z-->   <------X------->   <---Q---->
B      ]        I0&+      F0&+     Z0&+           x:             Q0&+
I    c 0&~:      ]       c F0&+   c Z0&+          x:             Q0&+
F    c 0&~:   c(ct<.)      ]       Z0&+     x:@:(c(ct<.))         x:
Z    c 0&~:   c(ct<.)     c Re      ]      x:@:(c(ct<.@Re))   x:@:(c Re)
X    c 0&~:   ct _1&x:   c F0&+   c Z0&+          ]              Q0&+
Q    c 0&~:   ct _1&x:   c F0&+   c Z0&+         c <.             ]
)

NB. ---------------------------------------------------------
CHAR_CONV =: makeconvtab 0 : 0 NB. Character conversions
     <C->  <C2>
C     ]    2&u:
C2   5&u:   ]
)

NB. List of all conversions
CONVS =: NUM_CONV,:CHAR_CONV

NB. =========================================================
NB. u and v are types. Get a verb to convert from type v to u.
get_typecast =: 2 : 0
  if. u=v do. ] return. end.

  NB. Check verbs
  NB. The left argument is the original y and the right is the converted y.
  NB. Return the right argument, or fail if it does not meet a condition.
  ct =. [:`]@.(u=3!:0@])  NB. Check type
  c  =. [:`]@.-:          NB. Check value

  NB. If exactly one argument is sparse, use dense conversion.
  sp =. is_sparse uv =. u,v
  uv =. ((-*./) sp) to_dense^:["0 uv

  NB. Look up in CONVS tables
  cn =. uv (e.&> i. 1:)"0 _ {."1 CONVS
  mesg =. u ('typecast: Incompatible types: ',[,' and ',])&(>@:toname) v
  mesg assert (=/ *. _1~:{.) cn
  'ts tab' =. ({.cn) { CONVS
  NB. There must be a better way to get a verb from a string...
  ". 'v =. ',tab {::~ ts i. |.uv

  NB. Tack on conversion to/from sparse array
  select. #. sp
    case. 1 do. v@:($.^:_1) f.
    case. 2 do. $.@:v f.
    case. do.   v f.
  end.
)


CAST_TEMPLATE =: (0 : 0)
  'NAME: x must be a valid type' assert x e. TYPE_NUM
  tc =. x GET_CAST ty =. 3!:0 y
  try.
    tc y
  catch.
    0 assert~ ty ('NAME: Error converting ',[,' to ',])&(>@:toname) x
  end.
)
makecast =: 2 :'4 :(CAST_TEMPLATE rplc ''GET_CAST'';v;''NAME'';u) " 0 _'
typecast =: 'typecast' makecast 'get_typecast'


NB. =========================================================
NB. Conversion table for numcast

rox =: 1r2 <.@:+ ]
MAX =: ->: MIN =: <.-: +:^:(4=3!:0)^:_ ]_1
round =: [: (((MAX*-.@]) + [: <. MIN>.*) <:&MAX) 0.5&+
clip =: MIN >. MAX <. ]

NUMCAST_CONV =: makeconvtab 0 : 0 NB. Numeric conversions
    <--B-->   <-----I----->   <F->   <Z->   <----X---->   <-Q-->
B      ]           I0&+       F0&+   Z0&+        x:        Q0&+
I     >:&1          ]         F0&+   Z0&+        x:        Q0&+
F    >:&0.5       round        ]     Z0&+     rox@:x:       x:
Z   0.5<:Re      round@Re     9&o.    ]     rox@:x:@:Re   x:@:Re
X     >:&1      _1 x:clip     F0&+   Z0&+        ]         Q0&+
Q    >:&1r2   _1 x:rox@clip   F0&+   Z0&+       rox         ]
)

NB. =========================================================
get_numcast =: 2 : 0
  'ts tab' =. NUMCAST_CONV
  'numcast: Types must be numeric' assert (u,v) *./@:e. ts
  if. u=v do. ] return. end.

  NB. If exactly one argument is sparse, use dense conversion.
  sp =. is_sparse uv =. u,v
  uv =. ((-*./) sp) to_dense^:["0 uv

  ". 'v =. ',tab {::~ ts i. |.uv

  NB. Tack on conversion to/from sparse array
  select. #. sp
    case. 1 do. v@:($.^:_1) f.
    case. 2 do. $.@:v f.
    case. do.   v f.
  end.
)

numcast =: 'numcast' makecast 'get_numcast'
