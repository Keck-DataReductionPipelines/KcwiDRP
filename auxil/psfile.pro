pro psfile,file,color=color,portrait=portrait
;
set_plot,'ps'

device,file=file+'.ps'

if keyword_set(portrait) then $
	device,/inches,yoffset=1.0,ysize=9.0,/portrait $
else	device,/land

if keyword_set(color) then $
	device,/color

return
end
