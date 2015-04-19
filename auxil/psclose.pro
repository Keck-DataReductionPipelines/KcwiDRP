pro psclose,nox=nox
;
device,/close
device,encap=0
if not keyword_set(nox) then $
	set_plot,'x'
return
end
