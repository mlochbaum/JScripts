NB. List of boxed element names
ELEMENTS =: <;._1 LF-.~0 :0
|H|He|Li|Be|B|C|N|O|F|Ne|Na|Mg|Al|Si|P|S
|Cl|Ar|K|Ca|Sc|Ti|V|Cr|Mn|Fe|Co|Ni|Cu|Zn
|Ga|Ge|As|Se|Br|Kr|Rb|Sr|Y|Zr|Nb|Mo|Tc|Ru
|Rh|Pd|Ag|Cd|In|Sn|Sb|Te|I|Xe|Cs|Ba|Lu|Hf
|Ta|W|Re|Os|Ir|Pt|Au|Hg|Tl|Pb|Bi|Po|At|Rn
|Fr|Ra|Lr|Rf|Db|Sg|Bh|Hs|Mt|Ds|Rg|Cn|Uut
|Uuq|Uup|Uuh|Uus|Uuo|La|Ce|Pr|Nd|Pm|Sm|Eu
|Gd|Tb|Dy|Ho|Er|Tm|Yb|Ac|Th|Pa|U|Np|Pu|Am
|Cm|Bk|Cf|Es|Fm|Md|No
)

NB. findpattern
NB. x is a list of words (default ELEMENTS)
NB. y is a (boxed) string to split up
NB. Find all possible ways to write y as a combination of words from x.
issubs =: [ -: ($~$)~
findpattern =: 3 :0 "1 0
ELEMENTS findpattern y
:
if. 0=#>y do. ,:'' return. end.
match=. x ([#~ issubs&:tolower&>) y=.boxopen y
if. 0=#match do. 0 0$'' return. end.
; match ,"1&.> x <@findpattern y (}.~#)&.> match
)
