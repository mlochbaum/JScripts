NB. Tools for working with colors

NB. Convert between rgb and Lab color spaces
labfrgb_lin =: _16 0 0 + (3 3$0 116 0 500 _500 0 0 200 _200)&(+/ .*)
labfrgb =: labfrgb_lin@:(3%:256%~])"1
rgbflab =: labfrgb^:_1

NB. Convert between rgb and hex
rgbfhex =: 256&#.^:_1"0 :. hexfrgb
hexfrgb =: 256&#."1     :. rgbfhex

NB. Convert from rgb to hsv
hsvfrgb =: (((((60*-/@}.) (],%) >./-<./)@:|.~ + 2*]) (i.>./)) , 0.3 0.59 0.11&(+/@:*))"1
