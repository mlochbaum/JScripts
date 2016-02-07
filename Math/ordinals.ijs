NB. Utilities for ordinals and rooted trees

NB. The ordinals are an extension of the natural numbers which allows
NB. for various infinite values. In the Cantor normal form, each ordinal
NB. can be expressed uniquely in terms of the smallest infinite ordinal
NB. ω (omega), as a sum of terms of the form (ω^b)*c where b is an
NB. ordinal and c a natural number.

NB. We represent an ordinal in Cantor normal form as a list of boxes,
NB. each containing an ordinal, in descending order. The ordinal
NB. represented by such a list b is, in pseudo-J, (ω +/@:^ b).
NB. With the base case that 0 is an empty list (since it has no terms in
NB. its Cantor normal form), this allows us to represent all ordinals
NB. which do not have an infinite tower of exponents.
NB. It is a particularly convenient form because the ordering on the
NB. ordinals is simply J's total array order.

NB. Such a list of boxes also defines a rooted tree in which each list
NB. of boxes is a node whose children are its unboxed elements.
NB. A more convenient representation of a tree for some purposes is a
NB. list of parent indices, as defined in the first section.

NB. =========================================================
NB. Tree utilities
NB. In this section, a "tree" with n nodes is a list of length n.
NB. Each element of the list (we identify such elements with the nodes of
NB. the tree) is an integer giving the index of that node's parent. If
NB. the node is the root of the tree, then its value is _1 instead.
NB. Such lists can also represent forests, as it has one component for
NB. each _1 it contains.

NB. Generate a "random" tree with y nodes
gentree =: _1 , [:? i.&.:<:

NB. Utilities
inlist =: (&.>) (;@:)
listiterate =: (&.>) (^:a:) (;@:) (&:<)

NB. For s, succ, and succtree, x is a tree and y is an element index.
NB. Find indices of immediate successors of y.
s =: I.@:=
NB. Find indices of all descendents of y.
succ =: (<@[ s inlist ])^:(*@#@]) listiterate
NB. Return the tree of y and its descendents.
succtree =: [ filterby i.@#@[ e. succ
NB. y is a tree. Return a list of boxes such that box i contains the
NB. indices of elements in y with depth i.
tolevels =: [: }: (<@[ s inlist ])^:(*@#@]) &.>^:a: &:< &0

NB. y is a tree. Return the depth of each element of y.
getdepth =: (]<. >:@{)^:_ (0 {.!._~ #)
NB. y is a tree, and x is a binary list. Return the subtree consisting
NB. of elements of y for which the corresponding element of x is 1.
filterby =: #~ { _1 ,~ [:<:+/\@]

NB. =========================================================
NB. y is a tree, in list form. Convert to box form.
tobox =: 3 : '\:~@:($:&.>) ^: (*@#)@(y&s) 0'

NB. =========================================================
NB. boxtoord displays a boxed ordinal in mathematical notation,
NB. and getordinal does the same for a list-form tree ordinal.

NB. Find which elements of string y are not enclosed in parentheses.
outofparens =: 0=[:+/\ 3&|&.>:@>: @: ('()'&i.)
NB. Collapse each parenthesized part of y to a single closed parenthesis.
collapseparen =: #~ outofparens

NB. Parenthesize y if it contains + or * operations.
paren =: ('(',,&')')^:([:+./ '+*'e.collapseparen) @:,
NB. "Multiply" two strings
times =: ([,'*',])`]`[@.(2<.(++:)&(-:&(,'1'))) &(,@:":)
NB. "Sum" a list of strings
combine =: [: }:@:; [: (,&'+')&.> #/.~ times&.>~ ~.
NB. ω&^ for a string
omegapower =: ('1'"_)`('ω'"_)`(('ω^')&,) @. ((<;._2'0 1 ') i. <) @:paren

boxtoord =: combine@:(omegapower@:$:&.>)`((,'0')"_)@.(0=#)
getordinal =: boxtoord@:tobox


NB. =========================================================
NB. Operations on boxed ordinals

NB. Inequalities
ge=: 0 1 -: \:@,&<
gt=: 1 0 -: /:@,&<
le=: 0 1 -: /:@,&<
lt=: 1 0 -: \:@,&<
eq=: -:!.0

NB. Addition
add =: ([ #~ (ge"0 {.)) , ]

NB. "Subtraction"
NB. (a u subtract v b) yields some c such that ((c add b) le a).
subtract =: 2 :0
:
for_o. |.y do.
  ind =. <: x (I.+e.~) o
  if. _1=ind do.
    x=.x (u subtract v) o (v subtract u) ,a:
  else.
    head =. ind{.x
    tail =. (>:ind)}.x
    if. o eq ind{x do.
      res =. 0$a:
    else.
      res =. (ind{x) u o
    end.
    x=. head add res add tail
  end.
end.
x
)

NB. Not totally sure what these do...
underopen =: 1 :(':';'u&.:((0&{::) :. (,@<)^:x) y')
cutatlevel =: (-~L.) ]underopen ]
addatlevel =: (-~L.) ,&a:underopen ]
