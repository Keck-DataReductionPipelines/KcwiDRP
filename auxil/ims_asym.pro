pro ims_asym, vec, mean, sigma, weight, siglim=siglim
;
if keyword_set(siglim) then $
	sl = siglim $
else	sl = [3.,3.]
weight = intarr(n_elements(vec))+1
mean = vec[0]
sigma= vec[0]
if total(weight) gt 2 then mystats,vec,mean,sigma,weights=weight
for i=1,20 do begin
	u = mean + sl[0] * sigma
	l = mean - sl[1] * sigma
	weight = intarr(n_elements(vec))
	t = where(vec gt l and vec lt u, count)
	if count gt 0 then weight[t] = 1
	if total(weight) gt 2 then mystats,vec,mean,sigma,weights=weight
endfor
return
end
