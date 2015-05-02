pro cwi_flex_collapse,slice,xprofile,yprofile,continuum=continuum

buffer = 10

sz = size(slice,/dim)

xprofile = total(slice,2)
yprofile = total(slice[buffer:sz[0]-buffer,*],1)

if keyword_set(continuum) then begin
   medyprofile = median(yprofile,25)
   yprofile -= medyprofile
endif                           ; continuum 

end; cwi_flex_collapse
