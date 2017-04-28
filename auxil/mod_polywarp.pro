;
;+
; NAME : 
;         MOD_POLYWARP
;
; PURPOSE : 
;         Generate a pair of warping polynomial kernels in two
;         dimensions (Kx, Ky) taking  starting coordinates (Xo,Yo) to
;         ending coordinates (Xi,Yi). The polynomial degrees in the
;         two dimensions are allowed to differ:
;
;         Xi = Sum (i,0,Degree[1]) (j,0,Degree[0]) Kx[i,j] Xo^j Yo^i
;         Yi = Sum (i,0,Degree[1]) (j,0,Degree[0]) Ky[i,j] Xo^j Yo^i
;        
;         Unlike the IDL POLYWARP function, the degrees need not be
;         equal, but the number of points provided must be equal to 
;         
;         number of points = (Degree[0]+1)*(Degree[1]+1)
;  
; CALLING SEQUENCE : 
;         MOD_POLYWARP,Xi,Yi,Xo,Yo,Degree,Kx,Ky, /DOUBLE, STATUS = status, /VERB
;
; INPUTS :
;         (Xi, Yi) : the destination coordinates
;         (Xo, Yo) : the origin coordinates
;         
;         Degree : one ore two element array designating the
;                  dimensionality of the fit. Degree[0] corresponds to
;                  the X axis, Degree[1] to the Y axis.
;      
;         The number of points must be ge
;         (Degree[0]+1)x(Degree[1]+1). If it is smaller, this
;         procedure will throw a fit, if it is larger it will just
;         truncate the array for you. It's kind like that.
;
; OPTIONS : 
;         /DOUBLE : double precision calculations. otherwise float
;         STATUS = status  :  returns any problems from matrix
;                             inversion
;         /VERB : should we be loud? 
;
;
; OUTPUTS :
;         Kx, Ky   (degree[0]+1 x degree[1]+1) coefficient matrixes of
;         the warp.
;
; MODIFICATION HISTORY: 
; 
; MKM, Caltech 2011 -- initial write
;          
;-
;
pro MOD_POLYWARP,xi,yi,xo,yo,deg,kx,ky,$
                 double=isdouble, $
                 status=outstatus, $
                 verb=verb

if keyword_set(verb) then talk=1 else talk=0
n=0
m=0
if n_elements(deg) eq 1 then begin
n=deg+1
m=deg+1
endif

if n_elements(deg) eq 2 then begin
   m=deg[0]+1
   n=deg[1]+1
endif

outstatus = 0


;if n_elements(xi) ne m*n then begin
;   message,'Oooops. Wrong number of points!'
;endif

npts = n_elements(xi)

if n eq 0 then begin
   message,'Please feed me the right number of degrees.'
endif

if talk then message,'Invoked!',/continue

W = dblarr(npts,m*n)
X = dblarr(npts,2)

medxo = median(abs(xo))
medyo = median(abs(yo))
;maxxo = max(abs(xi))
;maxxi = max(abs(xi))

;medxo = 1
;medyo = 1

X[*,0] = xi/medxo
X[*,1] = yi/medyo

xx = dblarr(m)
xx[0]=1.0D
for ix = 1,m-1 do xx[ix] = xx[ix-1]/medxo
yy = dblarr(n)
yy[0] = 1.0D
for iy = 1,n-1 do yy[iy] = yy[iy-1]/medyo

zz = yy # xx
;help,zz

U=X
U[*,0] = xo/medxo
U[*,1] = yo/medyo
;xx = xo
;yy = yo
; this is not optimal, but easy to understand.
for iy = 0,n-1 do begin
   for ix = 0,m-1 do begin
      row = ix*n+iy
      W[*,row] = U[*,1]^iy*U[*,0]^ix
   endfor; ix
endfor; iy
W = transpose(W)
;invw = inverse(W)
WW = W#transpose(W)

MM = la_invert(WW,/double)

;print,WW
;;print
;print
;;print,round(MM#WW)
;print



MM = transpose(temporary(MM)) # (W)


	kx = zz*(dblarr(n,m) + (MM # x[*,0]))*medxo
	ky = zz*(dblarr(n,m) + (MM # x[*,1]))*medyo

;print,kx,ky

;help,kx,ky

end
