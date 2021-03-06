NB. Sutherland-Hodgman polygon clipping
NB. http://rosettacode.org/wiki/Sutherland-Hodgman_polygon_clipping#J

NB. assumes counterclockwise orientation.
NB. determine whether point y is inside edge x
isinside=:0< [:-/ .* {.@[ -~"1 {:@[,:]

NB. (p0,:p1) intersection (p2,:p3)
intersection=:|:@[ (+/ .* (,-.)) [:{. ,.&(-~/) %.~ -&{:

NB. x and y are polygons (2xN lists of points).
SutherlandHodgman=:4 :0 NB. clip S-H subject
NB. Return a new polygon clipping x by y.
clip=.2 ]\ (,{.) x
subject=.y
for_edge. clip do.
  S=.{:input=.subject
  subject=.0 2$0
  for_E. input do.
    if. edge isinside E do.
      if. -.edge isinside S do.
        subject=.subject,edge intersection S,:E end.
      subject=.subject,E
    elseif. edge isinside S do.
      subject=.subject,edge intersection S,:E end.
    S=.E
  end.
end.
subject
)
