NB. A simple stack-based language.

require 'strings'

outofbrackets =: 0=[:+/\ 3&|&.>:@>: @: ('[]'&i.)
stripspace =: }.~ (' ' i.&1@:~: ])
words =: <@stripspace/.~ [:+/\ ' '&=*.outofbrackets
bracket =: '[',,&']'
debracket =: }.@}:^:('['={.)

setequal =: 4 :'i.0 0[ dict =: ((#~ (<x)~:{."1) dict),(x=.,x);y'

isconst =: '0123456789[' e.~ {.

NB. assumes the global variables stack, commands. Apply the word y to the stack.
push=:3 :'stack=:stack,boxopen y' :. pop
pop=:3 :0 :. push
  head=.>{:stack
  if. #y do. (y)=:head end.
  stack=:}:stack
  head
:
  head=.(-x){.stack
  if. #y do. (y)=:head end.
  stack=:(-x)}.stack
  head
)
apply =: 3 :0
  if. isconst y do. push y
  elseif. (<y) e. ;:'<: >: +: -: *: %:' do. ]&.".@:(y&,)&.pop ''
  elseif. (<y) e. ;:'+ - * % < > = ~:' do. push ]&.". (pop''),y,(pop'')
  elseif. (<y) e. {."1 dict do. commands=:commands, |.words dict rplc~ y
  elseif. y -: 'pop' do. pop ''
  elseif. y -: 'dup' do. push {: stack
  elseif. y -: 'dip' do. commands=:commands, ('dequote' ; bracket pop''),|.words debracket pop''
  elseif. y -: 'swap' do. push |.2 pop ''
  elseif. y -: 'apply' do. commands=:commands, |.words debracket pop''
  elseif. y -: 'select' do. push (".@pop { words@debracket@pop) ''
  elseif. y -: 'dequote' do. words@debracket &. pop ''
  elseif. y -: '^:' do. commands=:commands, (|.words debracket pop'') (]$~(*$))~ (".pop'')
  elseif. y -: 'list' do. push bracket ;:^:_1 debracket&.> ''pop~ ".pop''
  end.
)
commands=:a:
execute =: 3 :0
  stack=:0$a:
  commands=:|.words y
  while. #commands do.
    head=. >{:commands
    commands=:}:commands
    apply head
  end.
  stack
)

dict =: 0 2$a:

'#' setequal 'dup 0 = [[[dup] dip <: #] [pop pop]] select apply'
NB. '^:' setequal 'dup 0 = [[[dup [apply] dip] dip <: ^:] [pop pop]] select apply'
'i.' setequal '<: dup [dup <:] swap ^:'
'i.l' setequal 'dup [i.] dip list'

'fact' setequal 'dup [dup <: [dup <:] swap ^:] dip [*] swap <: ^:'
'fibhelp' setequal '[swap] dip swap [[swap [dup] dip + fibhelp] [+]] select apply'
'fib' setequal '<: 1 swap 0 swap # 1 fibhelp'
