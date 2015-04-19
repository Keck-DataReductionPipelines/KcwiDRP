function colordex,incolor,rgb=rgb,verbose=verbose

;+
;NAME:
;      COLORDEX
;
;PURPOSE
;      To return the number you should feed to "plot" or to the
;        system variable "!p.color" in order to make things the
;        color that you want.
;
;CALLING SEQUENCE
;      col=colordex(color)
;
;             or
;
;      col=colordex(rgb=[r,g,b])
;
;PARAMETERS
;      color = The name of the color you want.  This should be either a
;                     single letter, or a string, and it must be one of the
;                     standard 18 deepsearch colors.  These colors are set
;                     up by deepcolor.pro.  If you use deepcolor's default
;                     behavior (which is what happens in the standard
;                     startup sequence), then all 18 colors are available.
;                     Otherwise, only a subset may be avialable.  If you ask
;                     for an unavailable color, you get whatever !top_color
;                     is (usually white).  You will always get the color you
;                     ask for if you are using a 24-bit IDL display mode.
;                     If you ask for an unknown color, you get !top_color
;                     on an 8-bit display, and white on a 24-bit display.
;
;      rgb        = Alternatively, you may specify rgb values, where
;                     each is on a scale of 0..255.  You will
;                     get the color which is *closest* to the color you
;                     ask for on an 8-bit display, or exactly the
;                     color you ask for on a 24-bit display.
;OPTIONAL INPUTS
;      verbose    = If set, when you use "rgb=..." on an 8-color display,
;                     if an exact match isn't available you are told
;                     how far off the color you're given is compared
;                     to what you asked for.  (The 3d distance in colorspace,
;                     where each color is measured from 0-255, is printed
;                     to the screen.)
;
;RETURNS
;      The number you can set !p.color to, or that you can feed into
;      the "color" parameter of "plot", or to the fourth parameter
;      of "boxdata", or whatever.
;
;MODIFICATION HISTORY:
;      1999 July : RKNOP, written
;-

; ********************
;  Set up a way of specifiying standard colors by giving a single
;   letter.  Four arrays, c, r, b, and g give, respectively, give 
;   the letter and the red, green, and blue byte values for the colors.


; This table is stolen from deepcolor.pro

c=strarr(23)
r=bytarr(23)
g=bytarr(23)
b=bytarr(23)

c[0]='K'       & r[ 0]=255  & g[ 0]=190  & b[ 0]=190       ;pinK
c[1]='M'       & r[ 1]=255  & g[ 1]=  0  & b[ 1]=110       ;Magenta
c[2]='R'       & r[ 2]=255  & g[ 2]=  0  & b[ 2]=  0       ;Red or Ired
c[3]='H'       & r[ 3]=255  & g[ 3]=110  & b[ 3]= 90       ;peacH
c[4]='O'       & r[ 4]=255  & g[ 4]=127  & b[ 4]=  0       ;Orange
c[5]='T'       & r[ 5]=255  & g[ 5]=160  & b[ 5]=100       ;Tan
c[6]='N'       & r[ 6]=255  & g[ 6]=190  & b[ 6]= 60       ;browN
c[7]='Y'       & r[ 7]=255  & g[ 7]=255  & b[ 7]=  0       ;Yellow
c[8]='F'       & r[ 8]=175  & g[ 8]=255  & b[ 8]=  0       ;Forest green
c[9]='G'       & r[ 9]=  0  & g[ 9]=255  & b[ 9]=  0       ;Green
c[10]='L'      & r[10]=140  & g[10]=190  & b[10]=140       ;Lime green
c[11]='Q'      & r[11]=  0  & g[11]=255  & b[11]=160       ;turQuoise
c[12]='A'      & r[12]=  0  & g[12]=255  & b[12]=255       ;Aquamarine
c[13]='S'      & r[13]=120  & g[13]=190  & b[13]=255       ;Sky blue
c[14]='B'      & r[14]=  0  & g[14]=  0  & b[14]=255       ;Blue
c[15]='V'      & r[15]=165  & g[15]=  0  & b[15]=255       ;Violet
c[16]='P'      & r[16]=255  & g[16]=  0  & b[16]=255       ;Purple
c[17]='W'      & r[17]=255  & g[17]=255  & b[17]=255       ;White
c[18]='C'      & r[18]=0    & g[18]=0    & b[18]=0         ;blaCk
c[19]='U'      & r[19]=0    & g[19]=102    & b[19]=153         ;Ublue
c[20]='E'      & r[20]=102  & g[20]=153  & b[20]=102         ;ggrEen
c[21]='X'      & r[21]=255  & g[21]=153  & b[21]=0         ;rorangeX
c[22]='Z'      & r[22]=153   & g[22]=51   & b[22]=102         ;Zpurple

; **********************************************************************
;  Color specified by name

if (n_elements(incolor) gt 0) then begin

    color=strupcase(incolor)
    if (strlen(color) eq 1) then clr=color $
    else begin
        case 1 of
            color eq 'PINK'           : clr='K'
            color eq 'MAGENTA'        : clr='M'
            color eq 'RED'            : clr='R'
            color eq 'PEACH'          : clr='H'
            color eq 'ORANGE'         : clr='O'
            color eq 'TAN'            : clr='T'
            color eq 'BROWN'          : clr='N'
            color eq 'YELLOW'         : clr='Y'
            color eq 'FOREST GREEN'   : clr='F'
            color eq 'GREEN'          : clr='G'
            color eq 'LIME GREEN'     : clr='L'
            color eq 'TURQUOISE'      : clr='Q'
            color eq 'AQUAMARINE'     : clr='A'
            color eq 'SKY BLUE'       : clr='S'
            color eq 'BLUE'           : clr='B'
            color eq 'VIOLET'         : clr='V'
            color eq 'PURPLE'         : clr='P'
            color eq 'WHITE'          : clr='W'
            color eq 'BLACK'          : clr='C'
            color eq 'UBLUE'          : clr='U'
            color eq 'GGREEN'         : clr='E'
            color eq 'RORANGE'        : clr='X'
            color eq 'IRED'           : clr='R'
            color eq 'ZPURPLE'        : clr='Z'
            else                      : clr='?'
        endcase
    endelse

    w=where(clr eq c,nw)
    if (nw lt 1) then begin
        print,'COLORDEX ERROR : unknown color '+color+', trying white.'
        w=where(clr eq 'W')
        clr='W'
        color='WHITE'
    endif
    w=w[0]

    if (!d.n_colors eq 16777216) then begin
        colorindex=1l*r[w] + 256l*g[w] + 65536l*b[w]
        return,colorindex

    endif else begin
        if (strlen(!colorstr) eq 0) then begin
            print,'COLORDEX ERROR : color '+color+' not available.'
            return,!top_color
        endif

        dex=-1
        for i=0,strlen(!colorstr)-1 do begin
            if (clr eq strmid(!colorstr,i,1)) then begin
                dex=i
                goto,FALSEBREAK
            endif
        endfor

        FALSEBREAK :

        if (dex lt 0) then begin
            print,'COLORDEX ERROR : color '+color+' not available.'
            return,!top_color
        endif
        return,!top_color+1+dex
    endelse

endif else begin

; **********************************************************************
;  Color specified by RGB

    if (n_elements(rgb) ne 3) then begin
        print,' '
        print,'COLORDEX ERROR: must specify either color name or'
        print,'  three values in rgb=.'
        print,' '
        return,!top_color
    endif

    if (rgb[0] gt 255) then rgb[0]=255
    if (rgb[1] gt 255) then rgb[1]=255
    if (rgb[2] gt 255) then rgb[2]=255
    if (rgb[0] lt 0) then rgb[0]=0
    if (rgb[1] lt 0) then rgb[1]=0
    if (rgb[2] lt 0) then rgb[2]=0

    if (!d.n_colors eq 16777216) then begin
        colorindex=1l*rgb[0] + 256l*rgb[1] + 65536l*rgb[2]
        return,colorindex

    endif else begin
        tvlct,ctr,ctg,ctb,/get

        ; Try for an exact match

        w=where(ctr eq rgb[0] and ctg eq rgb[1] and ctb eq rgb[2],nw)
        if (nw gt 0) then begin
            w=w[0]
            return,w
        endif

        ; Otherwise, go for minimum distance

        dist=fltarr(n_elements(ctr))
        for i=0,n_elements(ctr)-1 do begin
            dist[i]=sqrt((float(ctr[i])-rgb[0])^2. + $
                         (float(ctg[i])-rgb[1])^2. + $
                         (float(ctb[i])-rgb[2])^2.)
        endfor
        mindist=min(dist,wmin)

        if (keyword_set(verbose)) then begin
            print,'COLORDEX WARNING : Color error is ',mindist
        endif
        return,wmin
    endelse
endelse


return,0              ; Should never get to this return
end

