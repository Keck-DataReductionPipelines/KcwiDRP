pro kcwi_oplot_slices
;
yd = !y.crange[1]-!y.crange[0]
for i=0,23 do begin
	if i lt 23 then $
		oplot,[(i+1)*5-0.5,(i+1)*5-0.5],!y.crange,linesty=5
	if i lt 10 then $
		xyouts,(i+1)*5-3.5,!y.crange[0]+yd*0.05,strn(i) $
	else	xyouts,(i+1)*5-4.0,!y.crange[0]+yd*0.05,strn(i)
endfor
return
end
