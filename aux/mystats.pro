pro mystats, vec, mean, sigma, kurt, weights=weights
;
w = intarr(n_elements(vec))+1
if keyword_set(weights) then w = weights
n = total(w)
d = n - total(w^2) / n
mean = total(w*vec) / n
sigma = sqrt(total(w*(vec - mean)^2) / d)
kurt = total(w*(vec - mean)^4)/ d / sigma^4 - 3.0
return
end
