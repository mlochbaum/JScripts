0 : 0
Jupdate has a simple goal. Take a J script that has some inputs (variables
and files), and update the outputs when the inputs change. It is meant for
scripts which are expensive to run, and need to be run many times with
small changes.

To load a script, use
   sc =: addscript 'path.ijs'
where addscript returns a locale. Then the script can be re-run with
   update__sc 'list of names'
.

Code must be pure functional (no side effects) with a few exceptions:
- Global assignment with (=:).
- require (not load!) statements at the beginning of the script.
- Declared read and write verbs (see below).

Read and write verbs function like fread and fwrite, but are allowed to
do additional processing. Read verbs must have one argument which consists
of one or more filepaths (in a boxed list of strings, or, if there is only
one file, a boxed or unboxed string). Write verbs must have two arguments,
where the left has the same form. To declare a read or write verb, use
(read declareread__sc) or (write declarewrite__sc).

Three types of changes are possible:
- Changes to global variables. Currently you may only change names which
  are assigned somewhere in the program; this replaces the first
  assignment to that name.
- Changes to files which are read with a declared read verb. Jupdate uses
  timestamps to track whether files have been updated, so it will fail in
  various ways if (6!:0) and the file timestamps are not consistent and
  causal.
- Changes to the script itself. This is not yet supported.
)

addscript =: addscript_pjupdate_
update =: update_pjupdate_
declareread  =: declareread_pjupdate_
declarewrite =: declarewrite_pjupdate_
declaredo    =: declaredo_pjupdate_
declareset   =: declareset_pjupdate_

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

  for_l. y do. addline >l end.

  DEPM =: getdepm DEPS  NB. Dependency matrix
  EMPTY
)

NB. ---------------------------------------------------------
NB. y is the name of a verb from the base locale.
NB. Change y in pjupdate so that it has the side effect of calling u.
declareside =: 1 : 0
  (y) =: (y,'_base_')~ [ u
  EMPTY
)
NB. u is a name. Add the side effect of appending the argument to u.
rplcXV =. 2 :'u rplc (;:''X V'') ,@,. 2{.boxopen v'
declareappend =. ('X =: X , V boxopen y' rplcXV)(3 :)(@]) declareside
NB. Declare reads from and writes to files.
declareread  =: ('READ' ;'''@'',&.>') declareappend
declarewrite =: ('WRITE';'''@'',&.>') declareappend
NB. Declare reads from and writes to variables.
declaredo    =: 'READ'  declareappend
declareset   =: 'WRITE' declareappend

NB. ---------------------------------------------------------
NB. y is a filename containing a J script.
addscript =: 3 : 0
  NB. Read, strip trailing spaces, drop blank lines
  y =. a: -.~ <@:({.~ #<.1+(~:i:1:)&' ');._2 ]1!:1 FILE =: boxopen y
  NB. Drop first-line #!
  'Error: empty script' assert 1<# y =. }.^:('#!' -: 2{.>@{.) y

  NB. Separate require statements and execute them
  'Requires must come first' assert (-: \:~) rm =. 'require'&([-:#@[{.])@> y
  'r y' =. #&y&.> (;-.)rm
  do_base_&.> r

  NB. Create locale
  SCRIPT =: y conew 'pjupdate'
)

NB. ---------------------------------------------------------
NB. Utilities

NB. Check which of files y were updated after time x
updatedafter =: ((-.@-:\:~)@:,: (1 {:: 0{1!:0@}.)@>)"1 0  (#~ '@'={.@>)

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

  if. 0=#WRITE do.
    echo 'Line ',(":#LINES),' has no detected side effects.'
    echo 'Make your dependencies explicit or remove the line.'
    assert 0
  end.

  NB. Check timestamps
  TIMES =: TIMES , ct =. 6!:0 ''
  'Timestamp error' assert 1 -.@e. ct updatedafter READ

  LINES =: LINES , <y1
  DEPS =: DEPS  ,  WRITE ,&< (r ([#~e.) NAMES),READ
  y1
)

NB. ---------------------------------------------------------
NB. Compute dependency matrix from DEPS
getdepm =: 3 : 0
  tc =. +./ .*.~^:_  NB. Adjacency matrix transitive closure
  NB. Matrix of forwards dependencies
  fd =. tc (+. =@i.@#) +./@:e.&>/~/ |:y
  NB. Backwards dependencies
  bd =. |: (e.~&> <@i.@#) (#@>(I.@:) ([</.{~) i.~@:;) {."1 y
  if. #e =. 4$.$. (|:bd) +./"1@:> fd do.
    echo 'Redefinition in line A makes line B obselete.'rplc,'AB';"_1 ":|.|:e
    echo 'Make your dependencies explicit or remove the line.'
    assert 0
  end.
  tc fd +. bd  NB. Dependency matrix
)

NB. ---------------------------------------------------------
NB. y is a list of names which have been changed.
NB. File changes are detected automatically.
update =: 3 : 0
  y =. , ;:^:(0=L.) y

  NB. Redo a line if an input is in y or a file was changed.
  redo =. TIMES ((e.&y@] +./@:, updatedafter) >@{:)"_1 DEPS
  redo =. DEPM +./ .*. redo
  for_i. I.redo do.
    ct =. 6!:0 ''
    'Timestamp error' assert 1 -.@e. ct updatedafter READ
    3 :'".y' i{::LINES
    TIMES =: ct i} TIMES
  end.
  EMPTY
)
