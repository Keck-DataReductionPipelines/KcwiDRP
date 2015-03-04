PRO QuerySimbad, name, ra, de, id, Found = found, NED = ned, ERRMSG = errmsg, $
    Verbose = verbose, CADC = cadc
;+
; NAME: 
;   QUERYSIMBAD
;
; PURPOSE: 
;   Query the SIMBAD or NED astronomical name resolver to obtain coordinates
;
; EXPLANATION: 
;   Uses the IDL SOCKET command to query either the SIMBAD or NED nameserver 
;   over the Web to return J2000 coordinates.    
;
;   For details on the SIMBAD service, see http://simbad.u-strasbg.fr/Simbad 
;   and for the NED service, see http://ned.ipac.caltech.edu/
;
; CALLING SEQUENCE: 
;    QuerySimbad, name, ra, dec, [ id, Found=, /NED, /CADC, ERRMSG=, /VERBOSE]
;
; INPUTS: 
;    name - a scalar string containing the target name in SIMBAD (or NED)
;           nomenclature. For SIMBAD details see
;           http://vizier.u-strasbg.fr/cgi-bin/Dic-Simbad .
;
; OUTPUTS: 
;     ra - the right ascension of the target in J2000.0 in *degrees* 
;     dec - declination of the target in degrees
;
; OPTIONAL INPUT KEYWORD:
;     /CADC - if set, then use the Simbad server at the Canada Astronomical
;             Data Center rather than the default server in Strasbourg, France.
;     /NED - if set, then nameserver of the NASA Extragalactic database is
;           used to resolve the name and return coordinates.   Note that
;           /NED cannot be used with Galactic objects
;     /VERBOSE - If set, then the HTTP-GET command is displayed 
; OPTIONAL OUTPUT: 
;     id - the primary SIMBAD (or NED) ID of the target, scalar string
;
; OPTIONAL KEYWORD OUTPUT:
;     found - set to 1 if the translation was successful, or to 0 if the
;           the object name could not be translated by SIMBAD or NED
;     Errmsg - if supplied, then any error messages are returned in this
;            keyword, rather than being printed at the terminal.   May be either
;            a scalar or array.
;            
; EXAMPLES:
;     (1) Find the J2000 coordinates for the ultracompact HII region
;         G45.45+0.06 
;
;      IDL> QuerySimbad,'GAL045.45+00.06', ra, dec
;      IDL> print, adstring(ra,dec,1)
;           ===>19 14 20.77  +11 09  3.6
; PROCEDURES USED:
;       REPSTR(), WEBGET()
;
; NOTES:
;     The actual SIMBAD query is made to the Sesame name resolver 
;     ( see http://cdsweb.u-strasbg.fr/doc/sesame.htx ).     The Sesame
;     resolver first searches the Simbad name resolver, then  NED and then
;     Vizier.    However, as of 6-Feb-2007 then NED link through Simbad was 
;     not working, so that one must specify the /NED keyword to contact the
;     NED server directly.
; MODIFICATION HISTORY: 
;     Written by M. Feldt, Heidelberg, Oct 2001   <mfeldt@mpia.de>
;     Minor updates, W. Landsman   August 2002
;     Added option to use NED server, better parsing of SIMBAD names such as 
;          IRAS F10190+5349    W. Landsman  March 2003
;     Turn off extended name search for NED server, fix negative declination
;     with /NED    W. Landsman  April 2003
;     Use Simbad Sesame sever, add /Verbose, /CADC keywords 
;       B. Stecklum, TLS Tautenburg/ W. Landsman, Feb 2007
;    Update NED query to account for new IPAC format, A. Barth  March 2007
;    Update NED query to account for another new IPAC format, A. Barth  
;                                                   July 2007
;
;-
  compile_opt idl2
  if N_params() LT 3 then begin
       print,'Syntax - QuerySimbad, name, ra, dec, [ id, ]
       print,'                 Found=, /CADC, /NED, ERRMSG=, /VERBOSE]'
       print,'   Input - object name, scalar string'
       print,'   Output -  Ra, dec of object (degrees)'
       return
  endif
  ;;
  printerr = not arg_present(errmsg)
  object = repstr(name,'+','%2B')
    if keyword_set(NED) then begin
 object = repstr(strcompress(object),' ','+')
 QueryURL = "http://nedwww.ipac.caltech.edu/cgi-bin/nph-objsearch?objname=" + $
           strtrim(object,2) + '&img_stamp=NO&list_limit=0&extend=no&of=pre_text' 
  endif else begin
 object = repstr(strcompress(object),' ','%20')
 if keyword_set(Cadc) then base = 'vizier.hia.nrc.ca' else $
 base = 'vizier.u-strasbg.fr'

 QueryURL = "http://" + base + "/viz-bin/nph-sesame/-o/?" + $
               strcompress(object,/remove)

  endelse
  ;;
  if keyword_set(verbose) then print,queryURL
  Result = webget(QueryURL)
  found = 0
  ;;
  if keyword_set(NED) THEN BEGIN
      if (strmid(result.text[5],1,3) NE 'PRE') and $
         (N_Elements(result.text) GE 16) THEN BEGIN
            found = 1
             t = result.text[16]
            headpos = strpos(t,'A>')
            t = strmid(t,headpos+2,80)
             id = strtrim( strmid(t,0,32),2)
            hr = fix(strmid(t,33,2))
            mn = fix(strmid(t,36,2))
            sc = float(strmid(t,39,4))
            ra = ten(hr,mn,sc)*15.0d
            dsgn = strmid(t,45,1)
            deg = fix(strmid(t,46,2))
            dmn = fix(strmid(t,49,2))            
            dsc = fix(strmid(t,52,2))
            de = ten(deg,dmn,dsc)
            if dsgn EQ '-' then de = -de

      endif else begin

      errmsg = 'No objects returned by NED server'
      if printerr then  message, errmsg, /info
     endelse
 endif else begin

  Result=Result.Text
; look for J2000 coords
  idx=where(strpos(Result, '%J ') ne -1,cnt)
 
  if cnt EQ 1 then begin 
    found=1   
    ra = 0.0d & de = 0.0d
    reads,strmid(Result[idx],2),ra,de
     idx2= where(strpos(Result, '%I.0 ') ne -1,cnt)
     if cnt GT 0 then id = strtrim(strmid(Result[idx2],4),2) else $
        message,'Warning - could not determine primary ID',/inf 
  ENDIF ELSE BEGIN 
      errmsg = ['No objects returned by SIMBAD.   The server answered:' , $
                 strjoin(result)]
      if printerr then begin
         message, errmsg[0], /info	
	 message,strjoin(result),/info
      endif	 
  ENDELSE
  ENDELSE
  
  return 
END 
  
