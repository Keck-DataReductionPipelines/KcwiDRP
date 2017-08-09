pro mystats, vec, mean, sigma, kurt, weights=weights
;
w = intarr(n_elements(vec))+1
if keyword_set(weights) then w = weights
n = total(w,/nan)
d = n - total(w^2,/nan) / n
mean = total(w*vec,/nan) / n
sigma = sqrt(total(w*(vec - mean)^2,/nan) / d)
kurt = total(w*(vec - mean)^4,/nan)/ d / sigma^4 - 3.0
return
end
