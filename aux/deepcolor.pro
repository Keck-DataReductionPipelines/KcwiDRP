pro deepcolor,short=short,colorstr=colorstr

;+
; NAME:
;       MK_CLRMRK
;
; PURPOSE:
;       To set up some standard color handling.  What it does depends
;         on whether the current display (set with "set_plot") is
;         X, Postscript, or NULL.  If the display is X, what it
;         does depends on whether you're using an 8 or 24-bit display.
;
;       FOR NULL (or anything other than X or Postscript):
;          Does nothing other than return.
;
;       FOR 8-BIT X DISPLAYS:
;          Sets aside 19 colors at the top of the colormap.  By
;            default, these are the standard "deepsearch" colors,
;            and are documented somewhere below
;            http://panisse.lbl.gov/groupwork/documentation.
;            Sets !top_color to !d.n_colors-20.  Uses short_gscale
;            to make a greyscale from 0 to !top_color.
;
;            Normally, IDL grabs all free colors in the colormap.  If
;            you want IDL to use fewer colors (so that, say, you can
;            run two IDLs at once without flashing), edit your
;            .Xdefaults (or .Xresources; read your .xinitrc to figure
;            out which) to include a line like:
;               idl.colors : 64
;            In that case, !d.n_colors will be 64, and !top_color
;            will be 44.
;
;            TO USE COLORS: The obselete way is to tell IDL to
;              use color number !top_color+1 through !top_color+whatever,
;              in order to get green, blue, red, etc.  DO NOT DO THIS
;              ANY MORE.  It will NOT work on 24-bit displays!
;              Instead, see the routine "colordex":
;                 IDL> dlib,'colordex'
;
;       FOR 24-BIT X DISPLAYS:
;          Sets the color map to a 256 level greyscale.  This will
;            be used in routines like "frame".  In order to use
;            colors like green, blue, or whatever, see the
;            routine "colordex":
;               IDL> dlib,'colordex'
;
;      FOR POSTSCRIPT:
;          Same as 8-bit X display.
;
;OPTIONAL INPUTS
;      SHORT    = Only matters for a PS or 8-bit display.  Sets aside
;                  only 11 colors at the top of the colormap, and
;                  makes !top_color=!d.n_colors-12.  Ignored
;                  if you pass COLORSTR.
;
;      COLORSTR = Specify your own set of colors to use.  This
;                  is not recommended, because the routine colordex
;                  assumes that you're using the standard set....
;                  This parameter has no effect on a 24-bit display.
;
;SIDE EFFECTS:
;      Loads !top_color and one of (!top_colorx or !top_colorps).
;
;MODIFICATION HISTORY:
;      1999-July : Written to replace mk_clrmkr and short_gscale
;                     by RKNOP
;
;-

if (!d.name ne 'X' and !d.name ne 'PS') then return

; ********************
;  Set up a way of specifiying standard colors by giving a single
;   letter.  Four arrays, c, r, b, and g give, respectively, give 
;   the letter and the red, green, and blue byte values for the colors.

; Note -- this table is also hardcoded into colordex.pro

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
c[19]='U'      & r[19]=0    & g[19]=102  & b[19]=153         ;Ublue
c[20]='E'      & r[20]=102  & g[20]=153  & b[20]=102         ;ggrEen
c[21]='X'      & r[21]=255  & g[21]=153  & b[21]=0         ;rorangeX
c[22]='Z'      & r[22]=153   & g[22]=51   & b[22]=102         ;Zpurple


; ********************
; Do

if (!d.name eq 'PS') then begin
    device,/color
    ncolors=256
endif else begin
    tvlct,0,0,0,0                      ; Force the device to initialize
    ncolors=!d.n_colors
endelse

if (!d.n_colors eq 16777216) then begin        ; 24-bit display
    iarr=indgen(256)
    tvlct,iarr,iarr,iarr                      ; The map is greyscale
    !top_color=256
    !colorstr=''
    !p.color=16777215

endif else begin

    if (keyword_set(colorstr)) then begin
        colors=colorstr
    endif else begin
        if (keyword_set(short)) then begin
            colors='GSRKCLYBOPW'
        endif else begin
            colors='GSRKCLYBOPQVMANHTFWUEXZ'
        endelse
    endelse
    !colorstr=colors

    if (ncolors gt 256) then begin
        print,' '
        print,"ERROR: Color isn't 24-bit, color isn't 8-bit!"
        print,'  Your display is screwed.  Hit ".c" to continue,'
        print,'  or just panic.'
        print,' '
        stop
    endif

    rarr=intarr(ncolors)
    barr=intarr(ncolors)
    garr=intarr(ncolors)
    !top_color=ncolors-strlen(colors)-1

    if (!top_color le 0) then begin
        print,' '
        print,'ERROR: IDL has too few colors to work with!'
        print,' Type ".c" to continue, or just panic.'
        print,' '
        stop
        !top_color=0
    endif

    for i=0,!top_color do begin
       ;this used to say int() but I changed it to fix -Andy
        val=fix(i*255./!top_color)
        rarr[i]=val
        garr[i]=val
        barr[i]=val
    endfor

    for i=!top_color+1,ncolors-1 do begin
        j=i-!top_color-1

        w=where(strmid(colors,j,1) eq c,nw)
        if (nw eq -1) then begin
            print,' '
            print,'DEEPCOLOR: Unknown color '+strmid(colors,j,1)+'.'
            print,'            Substituting white.'
            w=where(c eq 'W')
        endif
        w=w[0]
        rarr[i]=r[w]
        garr[i]=g[w]
        barr[i]=b[w]
    endfor
    
    tvlct,rarr,garr,barr
    !p.color=!top_color
endelse

; set one of !top_colorx or !top_colorps

if (!d.name eq 'X') then begin
    !top_colorx=!top_color
    !colorstr_x=!colorstr
endif else begin
    !top_colorps=!top_color
    !colorstr_ps=!colorstr
endelse

return
end
