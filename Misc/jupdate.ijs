NB. Code must be pure functional (no side effects) with a few exceptions:
NB. - Global assignment with =:
NB. - require (not load!) statements at the beginning of the script

addscript =: addscript_pjupdate_
update =: update_pjupdate_

NB. =========================================================
cocurrent 'pjupdate'
coinsert 'base'

NB. Locale of current script
SCRIPT =: a:

create =: 3 : 0
  READ =: WRITE =: 0$a: NB. To record reads and writes
  NAMES=: 0$a:    NB. List of names defined in script
  DEFD =: 0$0     NB. Line on which each name was defined
  TEMP =: 0       NB. Index used for temp variables (TEMP0, TEMP1, ...)
  LINES=: 0$a:    NB. Lines in script (after modification)
  DEPS =: 0 2$a:  NB. Write, read for each line
  TIMES=: 0 6$0   NB. Last time each line was executed
)

NB. ---------------------------------------------------------
NB. y is the name of a verb from the base locale.
NB. Change y in pjupdate so that it has the side effect of calling u.
declareside =: 1 : 0
  (y) =: (y,'_base_')~ [ u
  EMPTY
)
NB. u is a name. Add the side effect of appending the argument to u.
declareappend =. (2 :'v rplc ''X'';u' ('X =: X , boxopen y'))(3 :)(@]) declareside
NB. Declare reads from and writes to files.
declareread  =: 'READ'  declareappend
declarewrite =: 'WRITE' declareappend

NB. ---------------------------------------------------------
NB. y is a filename containing a J script.
addscript =: 3 : 0
  NB. Read, strip trailing spaces, drop blank lines
  y =. a: -.~ <@:({.~ #<.1+(~:i:1:)&' ');._2 ]1!:1 boxopen y
  NB. Drop first-line #!
  'Error: empty script' assert 1<# y =. }.^:('#!' -: 2{.>@{.) y

  NB. Separate require statements and execute them
  'Requires must come first' assert (-: \:~) rm =. 'require'&([-:#@[{.])@> y
  'r y' =. #&y&.> (;-.)rm
  do_base_&.> r

  NB. Create locale
  SCRIPT =: '' conew 'pjupdate'
  for_l. y do. addline__SCRIPT >l end.
  SCRIPT
)

NB. ---------------------------------------------------------
NB. Utilities

NB. Check which of files y were updated after time x
updatedafter =: ((-:\:~)@:, (1 {:: 0{1!:0)@>)"1 0

NB. Create a new temp variable
gettemp =: 3 : 0
  'TEMP',":<:TEMP=:>:TEMP
)
NB. Change names in an assignment target to the base locale
inbase =: [: ,&'_base_'&.> ;:^:(1<L.)


NB. ---------------------------------------------------------
NB. Called for each line of the script.
NB. Update LINES, NAMES, DEFS, DEFD, etc.
addline =: 3 : 0
  yw =. ;:^:(0=L.) y  NB. y words
  pl =. _1 |. +/\ 1 _1 0 {~ (,&.>'()') i. yw  NB. paren level

  NB. Extract nested assignments and run addline on each of those
  while. (#yw) > i =. yw i: <'=:' do.
    NB. a and b mark the bounds of the substatement
    'a b' =. i (((<i.1:) {.)@:}. , ({ (>:i:1:) {.)) pl
    NB. pad is 1 if there is a set of parentheses around it to remove
    b =. b - pad =. ((<:b)&{ *./@:< b }. (a+i)&{.) pl
    NB. Left-hand and right-hand side of assignment
    l =. (b+pad) }. i {. yw
    r =. (a-pad) {. i }. yw
    NB. Assignment starts at the beginning of the statement--exit.
    if. b=0 do. break. end.
    NB. Add the subexpression.
    if. 1>:#l do. NB. Single assignment
      addline l , r
    else. NB. Multiple assignment TODO handle string l
      TEMP =. ,<gettemp''
      addline TEMP , r
      addline l , '=:' ; TEMP
      l =. TEMP
    end.
    NB. Replace subexpression with the single name l
    yw =. (b&{. , l , (a+i)&}.) yw
    pl =. (b&{. , b&{ , (a+i)&}.) pl
  end.

  NB. Determine which variables are assigned to
  if. i = #yw do. NB. No assignment
    l =. ''
    WRITE =: $0
    r =. yw
  else.
    if. 1>:#l do. NB. Single assignment
      WRITE =: l
      l =. ,&'_base_'&.> l
    else. NB. Multiple assignment
      WRITE =: ;:^:(0=L.) ". ;:^:_1 l
      l =. (,&.>'()') ({.@[,],{:@[) 'inbase ' ; l
    end.
  end.

  NB. Add names, defined on this line
  NAMES =: NAMES ~.@:, WRITE
  DEFD =: (#LINES) (NAMES i. WRITE)} (#NAMES) {. DEFD

  READ =: $0
  NB. Execute (may append to READ and WRITE)
  NB. Note explicit do verb to avoid interference from local names
  3 :'".y' y1 =. ;:^:_1 l,r

  NB. Check timestamps
  TIMES =: TIMES , ct =. 6!:0 ''
  'Timestamp error' assert 1 -.@e. ct updatedafter READ

  LINES =: LINES , <y1
  DEPS =: DEPS  ,  WRITE ,&< (r ([#~e.) NAMES),READ
  y1
)

NB. ---------------------------------------------------------
NB. y is a list of names which have been changed.
NB. File changes are detected automatically.
update =: 3 : 0
  y =. , ;:^:(0=L.) y
  redo =. 0"0 LINES  NB. Lines to be reevaluated
  recn =. 0"0 NAMES  NB. Names to be recomputed
  for_i. i.#LINES do.
    'wr re' =. i { DEPS
    if. (e.&y +.&(+./) (i{TIMES) updatedafter (#~ '/'={.@>)) re do.
      y =. y , wr
      recn =. recn +. (NAMES e. wr) *. DEFD > i
      redo =. 1 i} redo
    end.
  end.
  for_i. I.redo do.
    ct =. 6!:0 ''
    'Timestamp error' assert 1 -.@e. ct updatedafter READ
    3 :'".y' i{::LINES
    TIMES =: ct i} TIMES
  end.
)
