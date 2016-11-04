function erfcfit,x,p
; p[0] = center
; p[1] = width of square
; p[2] = sigma of gaussian
; p[3] = sigma of other gaussian
; p[4] = height
; p[5] = constant offset


vl=p[4]*0.5*(erfc((x-p[0]-p[1]/2.0)/(sqrt(2.0)*p[2])) - erfc( (x-p[0]+p[1]/2.0)/(sqrt(2.0)*p[3])))+p[5]

return,vl

end
