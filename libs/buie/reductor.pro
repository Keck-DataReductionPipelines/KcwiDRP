;+
; NAME:
;  reductor
; PURPOSE:
;  Automated photometry reduction tool.
; DESCRIPTION:
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;  reductor
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  CALIBPATH-   Directory path pointing to calibration information,
;                  default=datadir+'calib/'  (datadir dymnamically determined
;                  by program).
;  DATAMNTS-    Special mount points to locate image data directories,
;                  a special list is available by default per instrument.
;                  DATAMNTS is an array of strings, leading with a slash.
;  DATATABLE-   Name of table to record photometry- this is passed 
;                  transparently to dbphot (which supplies the default)
;                  via colorsol, ltcrv and ltcrv2.
;                  It allows specification of database as well as table
;                  through a 'ddd.ttt' notation.
;  DVERBOSE-    Code, passed to gettran and other routine to print db queries.
;  DEBUG-          Turn on debug information output, default=OFF
;  DUMPOBJ-     Name of object to dump complete reduction information on.
;                  If set to 'all', all objects are dumped.
;  FLUSH_INFO-  Flush all cached information, force a clean start
;  FORCE     -  Flag, if true, ignores "ok" in instructions
;  FULL      -  Flag passed to colorsol that generates a more involved listing
;                  of the reductions.
;  INSTDB    -  List of data bases to probe to find instrument corresponding
;                  to rundate. It is an array of strings. The default is
;                  ['pccd2', 'nasacam', 'pccd'] and the data bases are probed 
;                  in that order.
;  MAGRESID   - Edit bad values by mag residual, not sigma residuals
;  NOEDIT     - Suppress all bad point editing.
;  NOSAVE     - Flag, if true, suppresses saving results to data bases
;                  and files. It does not affect editing, printing or plots.
;                  Reduc.log entries are not generated. The flag is passed to 
;                  the subsidiary programs that process rules.
;                  Note that NOSAVE does not inhibit promotion of files
;                  or the updating of the common block.
;  PRINT      - Send all plot output to the default printer (PORTRAIT.PRO),
;                  The display device is set to X (with a call to DISPLAY.PRO)
;                  upon exit.  If FORCE is also set, then DATAMON.pro will be
;                  called at the end to generate a plot from the .log1 file.
;  RESETBAD   - Flag, if true, turns off all bad flags.
;  SAVEALLPLOTS-Flag, if set causes a number of things to happen.  This
;                  flag implies /FORCE, /NOEDIT, PRINT=0, and NOSAVE=0 and
;                  all plots generated are saved to postscript files in same
;                  directory as the reduc.inf file.  The names of the
;                  postscript files are derived from the names of the rules
;                  in reduc.inf.
; TRANSFTABLE - Name of table to record and retrieve transformation 
;                  coefficients- this is passed 
;                  transparently to gettran (which supplies the default)
;                  directly and to puttran via transf.
;                  It allows specification of database as well as table
;                  through a 'ddd.ttt' notation.
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
;  ERRSTR   -   string, if not '', indicates a 1 line reductor error status
; COMMON BLOCKS:
;   COM_REDUCTOR - This is used to save information between multiple runs
;                    of this program.  Considerable information is cached to
;                    speed execution on the same night of data.
; SIDE EFFECTS:
; Reductor writes various files, some in the local reduction directory, and some
;    in special repositories, according to the processing rules invoked.
;    It will modify its own control file, reduc.inf, to mark when the night's
;    data directory is relocated. 
;    The programs called by reductor generate 
;    plot output and also place reduced observations in the 'data' table of the
;    phot data base. Reductor generates a 'RefID', which is a field in the
;    data table, which is always the 6 digit rundate, a hypen, and the 
;    instrument code, eg '020418-PCCD'. These RefID's are special in that
;    corresponding entries are not generated in the reference table, unlike
;    externally submitted photometry data. To avoid conflicts, it is 
;    essential that RefID's for external data sets be forced to have a
;    different format (such as three substrings separated by 2 hyphens).
;     A time and user stamped log of reductor rules run is 
;    generated, called reduc.log, in the reduction directory.
; RESTRICTIONS:
; Code to write initial reduc.inf still needs to be fixed to use wrreduc call.
; PROCEDURE:
;  The operation of the program is controlled by the contents of a file
;     named reduc.inf that is in the current directory.  If not found, this
;     program will attempt to build a relevant file to get started.  In normal
;     use you will run this program many times on the way to getting your
;     photometry reduced.  What steps are done and how they are done are
;     controlled by lines in the reduc.inf file.
;
;  The contents of reduc.inf are as follows:
;     Line 1 - REDUCTOR v1.1          File id line
;     Line 2 - LCCD/OSU2              Instrument id (no spaces)
;     Line 3 - /cdrom/lo_mwb_0022/    Directory where RAW image data reside
;     Line 4 - 920611                 YYMMDD, UT date code
;     Line 5 - 15.0 25.0 100.0 39.4 11.3   Photometry extraction parameters,
;                                          Object radius, inner sky, outer sky, 
;                                          gain and readout noise.
;     Line 6 to end, these are "rule" lines that invoke different steps of
;        reduction operations.  In general, these lines start with a two
;        character operation code followed by the information needed for that
;        step.
;
;     The operators are:
;          2c - Two color lightcurve against all sky standards
;          dp - Differential photometric reduction (like Pluto)
;          lc - Single color lightcurve against all sky standards.
;          sl - Block for reducing Stars (in Landolt system) (color unknown).
;          tr - Transformation against Landolt standard stars.
;
;     In addition to the stated arguments, all operator lines can be terminated
;        with the word "ok" (no quotes).  Any line marked in this way is
;        ignored unless /FORCE is specified.
;
; ------------------------------------------------------------------------------
; 2c - Two color lightcurve against all sky standards
; ------------------------------------------------------------------------------
; 2c OBJECT SERNO FILCOD FILNAM STDFIL COLOR1 COLOR2
;                 FILCOD FILNAM STDFIL COLOR1 COLOR2 STDCOL COLERR
; 1  OBJECT - Standard object code name to reduce
; 2  SERNO  - Serial number of ojbect to reduce (usually 0)
; 3  FILCOD - filter code (string) as in photometry log file
; 4  FILNAM - filter name (best if 1 character)
; 5  STDFIL - color code for Landolt system 01234 = UBVRI
; 6  COLOR1 - First color code for color to reduce against
; 7  COLOR2 - Second color code (ex: B-V is color1=1, color2=2)
; 8  FILCOD - filter code (string) as in photometry log file
; 9  FILNAM - filter name (best if 1 character)
;10  STDFIL - color code for Landolt system 01234 = UBVRI
;11  COLOR1 - First color code for color to reduce against
;12  COLOR2 - Second color code (ex: B-V is color1=1, color2=2)
;
; ------------------------------------------------------------------------------
; dp - Differential photometric reduction (like Pluto)
; ------------------------------------------------------------------------------
; dp FILCOD FILNAM COLNAM OBJECT SERNO COLOR COMP [k2 V1 E1] [ct V2 E2] [k V1 E1] [td DATE]
; 1  FILCOD - filter code (string) as in photometry log file
; 2  FILNAM - filter name (1 character)
; 3  COLNAM - name of color (2 character, ie., BV or VR)
; 4  OBJECT - Standard object code name to reduce
; 5  SERNO  - Serial number of ojbect to reduce (usually 0)
; 6  COLOR  - Standard color of object
; 7  COMP   - Standard object name for comparison star (replace blanks with _)
;  argument flags:
;    k2     - override second order extinction coefficient.
;    ct     - Override color term.
;    k      - Override extinction.
;    td     - Override transformation with values from another date.
;  optional
;    V1,E1  - Second order extinction and error
;    V2,E2  - Color term
;    DATE   - YYMMDD for date to use transformation from
; 
; ------------------------------------------------------------------------------
; lc - Single color lightcurve against all sky standards.
; ------------------------------------------------------------------------------
; lc OBJECT SERNO FILCOD FILNAM STDFIL COLOR1 COLOR2 STDCOL COLERR
; 1  OBJECT - Standard object code name to reduce
; 2  SERNO  - Serial number of ojbect to reduce (usually 0)
; 3  FILCOD - filter code (string) as in photometry log file
; 4  FILNAM - filter name (best if 1 character)
; 5  STDFIL - color code for Landolt system 01234 = UBVRI
; 6  COLOR1 - First color code for color to reduce against
; 7  COLOR2 - Second color code (ex: B-V is color1=1, color2=2)
; 8  STDCOL - Standard color of object
; 9  COLERR - Uncertainty of color
;
; ------------------------------------------------------------------------------
; sl - Block for reducing Stars (in Landolt system) (color unknown)
; ------------------------------------------------------------------------------
; sl FILCOD1 FILCOD2 FILNAM1 FILNAM2 STDFIL1 STDFIL2
; 1  FILCOD1 - filter code (string) as in photometry log file for color 1
; 2  FILCOD2 - filter code (string) as in photometry log file for color 2
; 3  FILNAM1 - filter name (best if 1 character) for color 1
; 4  FILNAM2 - filter name (bset if 1 character) for color 2
; 5  STDFIL1 - color code for Landolt system 01234 = UBVRI for color 1
; 6  STDFIL2 - color code for Landolt system 01234 = UBVRI for color 2
;
; ------------------------------------------------------------------------------
; tr - Transformation against Landolt standard stars.
; ------------------------------------------------------------------------------
; tr FILCOD FILNAM STDFIL COLOR1 COLOR2 [k2 V1 E1] [ct V2 E2] [kt]
; 1  FILCOD - filter code (string) as in photometry log file
; 2  FILNAM - filter name (best if 1 character)
; 3  STDFIL - color codes for Landolt system 01234 = UBVRI
; 4  COLOR1 - First color code for color to reduce against
; 5  COLOR2 - Second color code (ex: B-V is color1=1, color2=2)
;
;  argument flags:
;    k2     - override second order extinction coefficient.
;    ct     - Override color term.
;    kt     - turn on time dependent extinction.
;  optional
;    V1,E1  - Second order extinction and error
;    V2,E2  - Color term
;
; MODifICATION HISTORY:
;  Written by Marc W. Buie, 96/10/16, Lowell Observatory
;  96/10/31, MWB, added "bad" flag support from rdphalt
;  97/01/24, MWB, added MAGRESID flag
;  92/02/27, MWB, added "dp" rule for differential photometry reductions.
;  2000/01/25, John Mattox, Boston University, added missing dumpall clause.
;  2000/07/06, MWB, added '9 = M = Methane' filter to "dp" rule.
;  2000/07/14, MWB, fixed bug that was killing new sky signal in log file.
;                 Also added printing of datamon.pro plot if force and print set.
;  2000/10/16, MWB, fixed bug that was ignoring forced extintion in dp rule.
;  2001/11/06, MWB, allowed for cached ephemeris calculations
;  2003/10/01, MWB, converted my Delfile call to system file_delete routine
;                   converted my Mkdir calls to IDL file_mkdir calls
;  2004/02/09, MWB, changed fixed paths to data files and reduction areas
;
;  2006/05/12  Peter L. Collins, Lowell Observatory
;                add readout noise handling; reduc,inf file format is now 1.1
;                although 1.0 accepted; 'self' added to badpar calls.
;  2006/06/22  PLC, added explicit call to reducprmt to promote reduc.inf-
;                   reductor prompts for read noise if inf file 1.0. Added
;                   nominal instrument default for nasacam.
;  2006/08/03, MWB, changed to use database for transformation information
;  2006/08/15, PLC, changed to use findrawfits to locate image data resident
;                   on cluster, and data bases to map rundate to instrument.
;                   Reduc.inf is rewritten if data directory relocates.
;  2006/08/24, PLC, enhancements for use of reductor with REDUC widget. 
;                   1. ERRSTR output keyword
;                   2. GUI flag
;                   3. Introduced rdreduc and wrresduc routines.
;  2006/08/29, PLC, more enhancements for REDUC and better instrument default.
;  2006/09/01, PLC, replaced findrawfits with more general routine finddata.
;  2006/09/08, PLC, removed /gw/data7 as a data mount pt and also
;                   reunited a revision "branch" in reductor.pro.
;  2006/09/26, PLC, pass info.inst to ltcrv, ltcrv2, and colorsol via the  
;                   INSTRUMENT keyword (for use in dbphot).
;  2006/10/02, PLC, add support for 'reduc.log' (timestamped rules run).
;  2006/10/05, PLC, mods to reduc.log output.
;  2006/10/15, PLC, add NOSAVE flag.
;  2006/12/07, PLC, support new format of data table in phot database.
;  2006/12/10, PLC, DATATABLE keyword.
;  2006/12/18, MWB, added SAVEALLPLOTS keyword
;  2006/12/27, PLC, add TRANSFTABLE and DVERBOSE keywords.
;  2007/01/18, MWB, fixed problem with non-database supported instruments.
;  2007/02/15, PLC, mods to 2c processing to add a color record to phot db.
;  2008/06/11, MWB, added support for roboccd
;-

pro reductor, DEBUG=debug, DATATABLE=datatable,DUMPOBJ=fulldump, $
              FLUSH_INFO=flush_info, $
              FORCE=force, FULL=full, MAGRESID=magresid, PRINT=printit, $
              RESETBAD=resetbad, NOEDIT=noedit, CALIBPATH=calibpath, $
              DATAMNTS=datamnts,INSTDB=instdb,ERRSTR=errstr,GUI=gui, $
              NOSAVE=nosave, TRANSFTABLE=transftable, $
              DVERBOSE=dverbose,SAVEALLPLOTS=saveallplots

   common com_reductor, info, pdata

;  instruments 
   instrs = ['PCCD','Nasacam','Loral/2.5','SITE2k','SMARTS','ccd21big']

   mntpts_PCCD=['/net/amber/raid1']
;   mntpts_Nasacam =  ['/media/usbdisk']
   mntpts_Nasacam =  ['/net/fiona/raid1','/net/amber/raid1']
   mntpts_site2k =  ['/net/amber/raid1']
   datadirs = ['buie/rawfits','buie/rawfits/ccd', $
               'buie/rawfits/roboccd','buie/rawfits/site', $
               'buie/rawfits/smarts','buie/rawfits/bigelow']

; defaults in that order- 
;  PCCD
;  Nasacam
;  Loral/2.5
;  SITE2k
;  the last set of entries is for any other instrument.
   ;                  rad   sky1  sky2   gain  rdnoise
   instr_defaults = [ [ 10.0, 20.0,  80.0, 2.60, 16.0], $
                      [ 18.0, 25.0, 130.0, 2.15,  6.2], $
                      [ 15.0, 25.0, 100.0, 2.65, 12.0], $
                      [ 15.0, 25.0, 100.0, 3.87,  8.0], $
                      [ 10.0, 20.0, 100.0, 3.10,  4.7], $
                      [ 18.0, 30.0, 100.0, 3.40,  8.5], $
                      [  5.0, 25.0, 40.0, 1.00, -1.00] ] 
   obscd = ['688','688','688','688','807','688','698']

   self='REDUCTOR: '
   if badpar(debug, [0,1,2,3],    0,   caller=self + '(DEBUG) ', $
             default=0) then return
   if badpar(datamnts,  [0,7],    1,   caller=self + '(DATAMNTS) ', $
             default=['']) then return
   if badpar(datatable,[0,7],    0,  caller=self + '(DATATABLE) ' )  $
                        then return
   if badpar(dverbose,   [0,1,2,3],   0,   caller=self + '(DVERBOSE) ') then $
                             return
   if badpar(instdb,    [0,7],    1,   caller=self + '(INSTDB) ', npts=ndb, $
             default=['roboccd','pccd2obs','nasacam','pccdobs']) then return
   if badpar(fulldump,   [0,7],   0,   caller=self + '(DUMPOBJ) ', $
             default='[[[none]]]') then return
   if badpar(full,   [0,1,2,3],   0,   caller=self + '(FULL) ', $
             default=0) then return
   if badpar(printit,[0,1,2,3],   0,   caller=self+'(PRINTIT) ', $
             default=0) then return
   if badpar(noedit,[0,1,2,3],    0,   caller=self + '(NOEDIT) ', $
             default=0) then return
   if badpar(gui,[0,1,2,3],       0,   caller=self + '(GUI) ', $
             default=0) then return
   if badpar(force,[0,1,2,3],    0,   caller=self + '(FORCE) ', $
             default=0) then return
   if badpar(nosave,[0,1,2,3],    0,   caller=self + '(NOSAVE) ', $
             default=0) then return
   if badpar(saveallplots,[0,1,2,3],    0,   caller=self + '(SAVEALLPLOTS) ', $
             default=0) then return
   if badpar(transftable,[0,7],    0,  caller=self + '(TRANSFTABLE) ' )  $
                        then return

   if saveallplots then begin
      force=1
      noedit=1
      nosave=0
      printit=0
      save_d_name = !d.name
      set_plot,'ps'
      device,/Helvetica
   endif

   errstr=''

   if printit then begin
      portrait
   endif

   setusym,1
   !x.style=3
   !y.style=3
   !z.style=3

   infofile='reduc.inf'
   infologfile='reduc.log'
   inst=''
   ddir=''
   data=''
   plotsdone=0
   badisdirty=0
   landfil=['U','B','V','R','I']
   blanks='                     '

;   ; This is the GPS position for the 42", derived 1993 Sep 08
;   lat = (35.0+5.0/60.0+48.740/3600.0)/180.0*!pi
;   lon = (111.0+32.0/60.0+10.601/3600.0)/180.0*!pi

   ; First, look for reduc.inf in the current directory
   ; if not found, ask questions to create one
   if not exists(infofile) then begin
      if nosave then begin
         errstr = self + 'Cannot build reduc.inf, NOSAVE flag set.'
         print, errstr
         return
      endif
      ; Get current directory, this is used to find the data directory.
      cd,'.',current=cdir
      data=strmid(cdir,strpos(cdir,'/',/reverse_search)+1,99)
      if not stregex(data,'^[901][0-9]([1][0-2]|[0][1-9])' + $
                     '([0][1-9]|[12][0-9]|[3][01])',/BOOLEAN) then begin
         errstr = self + 'Cannot build reduc.inf, current dir ' + $
                          data + ' not a rundate'
         print, errstr
         return
      endif

      ; Check the date using db queries to identify the instrument used.
      ; The db's are polled in the order presented in instdb array
      ; and the runstat table is checked for
      ; an entry with the given rundate. On pccdobs.runstat only, it is
      ; necessary to check inst as well to see it is 'L' (Lowell Obs).
      for i=0,ndb-1 do begin
         openmysql,dblun,instdb[i]
         query = [ 'select rundate from runstat where ',  $
                   ' rundate = ' + quote( data) ]
         if strcmp( instdb[i], 'pccdobs',/FOLD_case) then $
            query = [ query, ' and inst = ' + quote('L') ]
         if strcmp( instdb[i], 'nasacam',/FOLD_case) then $
            inst = 'Nasacam' $
         else if strcmp( instdb[i], 'roboccd',/FOLD_case) then $
            inst = 'Nasacam' $
         else $
            inst = 'PCCD'
         dbpick=i

         query = [query, ';' ]
         
         mysqlquery, dblun, query, rundate, format='a'

         free_lun, dblun

         ; rundate[0] will be an empty string if nothing found.
         if rundate[0] eq data then break
      endfor

      if i eq ndb then begin
         errstr = self + $
                'cannot find instrument run record corresponding to '+ data
         print, errstr
         ans=''
         read,ans,prompt='Enter the instrument code to continue (default=quit) '
         ans=strtrim(ans,2)
         if ans eq '' then return
         errstr=''
         inst=ans
      endif

      if inst eq 'Nasacam' then begin
         default_oplines = [ $
            'tr B B 1 1 2 ok', $
            'tr V V 2 1 2 k2 0. 0. ok', $ 
            'tr V V 2 2 3 k2 0. 0. ok', $
            'tr R R 3 2 3 k2 0. 0. ok', $
            'sl B V B V 1 2 ok', $
            'sl V R V R 2 3 ok'  $ 
;            '2c a???? 0 2 V 2 2 3 3 R 3 2 3 ok', $
;            'lc a???? 0 3 R 3 2 3 0.4? 0.0? ok', $
;            'dp 1 B BV p9 0 0.842 SAO_160288 ok', $
;            'dp 2 V BV p9 0 0.842 SAO_160288 ok', $
;            'dp 9 M BV p9 0 0.0   SAO_160288 k2 0. 0. ct 0. 0. ok' ]
             ]
      endif else begin
         default_oplines = [ $
            'tr 1 B 1 1 2 ok', $
            'tr 2 V 2 1 2 k2 0. 0. ok', $ 
            'tr 2 V 2 2 3 k2 0. 0. ok', $
            'tr 3 R 3 2 3 k2 0. 0. ok', $
            'sl 1 2 B V 1 2 ok', $
            'sl 2 3 V R 2 3 ok'  $ 
;            '2c a???? 0 2 V 2 2 3 3 R 3 2 3 ok', $
;            'lc a???? 0 3 R 3 2 3 0.4? 0.0? ok', $
;            'dp 1 B BV p9 0 0.842 SAO_160288 ok', $
;            'dp 2 V BV p9 0 0.842 SAO_160288 ok', $
;            'dp 9 M BV p9 0 0.0   SAO_160288 k2 0. 0. ct 0. 0. ok' ]
             ]
      endelse

      ; see if the data directory can be located. If not, leave the 
      ; ddir line blank and inform the user.
      ddir=''

      print, 'Looking for data directory for ', data, ' on ', inst
      if inst eq 'Nasacam' then $
         mntpts = mntpts_Nasacam $
      else if inst eq 'SITE2k' then $
         mntpts = mntpts_site2k $
      else $
         mntpts = mntpts_PCCD
      rawpath = ['']
      target = data
      finddata, mntpts,datadirs,target,datapath, /DIRONLY,COUNT=count,/debug

      if count eq 0 then print, 'no data for ',data, ' in standard places' 
      if count gt 1 then begin
         errstr = ['Multiple repositories for '+data+' images exist:', $
                   datapath, $
                   'please resolve this problem by putting the right directory in reduc.inf']
         print,errstr
         return
      endif
      if count eq 1 then $
         ddir=strmid(datapath[0],0,(strpos(datapath[0],'/',1,/REVERSE_OFFSET, $
                                     /REVERSE_SEARCH) + 1))

      ii = where( inst eq instrs)
      ii = ii[0]
      if ii lt 0 then begin
          sz = size(instr_defaults,/DIMENSIONS)
          ii = sz[1] - 1
      endif

      newrad    =  instr_defaults[0,ii]
      newsky1   =  instr_defaults[1,ii]
      newsky2   = instr_defaults[2,ii]
      newgain   =   instr_defaults[3,ii]
      newrdnoise   =  instr_defaults[4,ii]

      ; prompt (if possible) for readnoise.
      if newrdnoise lt 0.0 then begin
         newrdnoise=10.0
         if gui then newrdnoise = $
                     qinput(/FLOATING,PROMPT='Please enter read noise', $
                                   DEFAULT=10.0) $
         else read,prompt='Read Noise (e-)? ',newrdnoise
      endif
      wrreduc, infofile, inst,ddir,data,newrad,newsky1,newsky2,newgain, $
               newrdnoise,default_oplines
      jdstr, systime(/JULIAN), 0, tis
      spawn, '/usr/bin/whoami', whois
      openw, loglun,infologfile, /GET_LUN,/APPEND
      printf, loglun, tis + ' '  +  whois + ' ' +  'creating reduc.inf: ' +   $
                      string( newrad,newsky1,newsky2,newgain,newrdnoise)
      free_lun, loglun

      errstr = 'The file, '+infofile+ $
               ', has been created, review/edit and run REDUCTOR again.'
      print,errstr
      return
   endif

; If common is empty, fill in the structure with empty stuff.
   if keyword_set(flush_info) then info=0
   sz_info=size(info)
   if sz_info[n_elements(sz_info)-2] ne 8 then begin
      info = { $
         cwd:        '', $
         data:       '', $
         ddir:       '', $
         inst:       '', $
         newrad:    0.0, $
         newsky1:   0.0, $
         newsky2:   0.0, $
         newgain:   0.0, $
         newrdnoise: 0.0, $
         obscode:    '', $
         oplist:    strarr(50), $
         needcoord:   0, $
         logdate: '' $
         }
   endif

; Is the directory the same as the last time?  If so, reset some of the info.
   cd,'.',current=cwd
   if cwd ne info.cwd then info.logdate=''
   info.cwd = cwd

   rdreduc, infofile, inst, ddir, data, newrad, newsky1, newsky2,newgain, $
            newrdnoise,oplines,GUI=gui
   ; check ddir validity
   if ddir ne '' and not exists(addslash(ddir) + data) then ddir = ''

   reddir = ddir eq ''
   if reddir then begin
      print, 'Looking for data directory for ', data, ' on ', inst
      if inst eq 'Nasacam' then $
         mntpts = mntpts_Nasacam $
      else if inst eq 'SITE2k' then $
         mntpts = mntpts_site2k $
      else $
         mntpts = mntpts_PCCD
      rawpath = ['']
      target = data
;      if inst eq 'Nasacam' then target = '20' + data
      finddata, mntpts,datadirs,target,datapath, /DIRONLY,COUNT=count

      repos = 'no'
      if count gt 0 then repos = strn( count)

      if count ne 1 then begin
         errstr=self + ' There are '+repos+' repositories of data for ' + $ 
                         data + ', please resolve'
         print, errstr
         return
      endif else begin
         ddir=strmid(datapath[0],0,(strpos(datapath[0],'/',1,/REVERSE_OFFSET, $
                                     /REVERSE_SEARCH) + 1))
         print, 'Data directory was relocated to ', ddir
      endelse
   endif

   z=where(inst eq instrs,count)
   if count eq 1 then obscode = obscd[z[0]] else obscode='688'

   info.inst=inst
   info.ddir=addslash(ddir)
   info.data=data
   info.newrad=newrad
   info.newsky1=newsky1
   info.newsky2=newsky2
   info.newgain=newgain
   info.newrdnoise=newrdnoise
   info.obscode=obscode
   info.oplist = oplines
   if oplines[0] eq '' then numop = 0 $
   else numop = n_elements(oplines)

   ;if the ddir was repositioned, we need 
   ;to rewrite the info file- existing lines are copied character for character.
   if reddir then begin
      if nosave then begin
         errstr = self + 'Cannot rewrite reduc.inf, NOSAVE flag set, aborting.'
         print, errstr
         return
      endif
      print, 'Rewriting ', infofile
      wrreduc, infofile, inst, ddir, data, newrad, newsky1, newsky2,newgain, $
               newrdnoise,oplines
   endif

   refid = info.data + '-' + info.inst  ;for photometry data base
   logfile=info.data+'.log'
   log1file=info.data+'.log1'
   namesfile=info.data+'.names'
   fncache=info.data+'.eph'

   ; Check for .log file, can't proceed if not present
   if not exists(logfile) then begin
      errstr = self + 'ERROR! ' + logfile + $
                      ' does not exist, need to run CCDPHOT'
      print, errstr
      if exists(fncache) then file_delete,fncache,/quiet,/noexpand_path
      return
   endif

   ; Check for .log1 file, see if need to run REPHOT
   if not exists(log1file) then begin
      redo_phot=1
   endif else begin
      rdphalt,log1file,filename,obj,fil,jd,exptime,gain,rad,sky1,sky2, $
      skip_serial,skip_xpos,skip_ypos,skip_fwhm,ski_maxcnt,skip_sky, $
      skip_skyerr,skip_mag,skip_err,skip_bad,rdnoise,NUMLINE=1
      if fix(gain[0]*100.0+0.5) ne fix(info.newgain[0]*100.0+0.5) or $
         fix(rad[0]*100.0+0.5) ne fix(info.newrad[0]*100.0+0.5) or $
         fix(sky1[0]*100.0+0.5) ne fix(info.newsky1[0]*100.0+0.5) or $
         fix(sky2[0]*100.0+0.5) ne fix(info.newsky2[0]*100.0+0.5) or $
         fix(rdnoise[0]*100.0+0.5) ne fix(info.newrdnoise[0]*100.0+0.5) $
      then redo_phot=1 $
      else redo_phot=0
      if redo_phot and not saveallplots then begin
         if nosave then begin
            errstr = self + 'Cannot recreate log1, NOSAVE flag set, aborting.'
            print, errstr
            return
         endif
      print, 'Rewriting ', infofile
         ans=''
         read,prompt='log1 file needs to be recreated, do you want to proceed? ',ans
         if ans eq 'yes' then begin
            file_delete,log1file,/quiet,/noexpand_path
         endif else begin
            errstr='Operation aborted by user input, nothing done.'
            return
         endelse
      endif
   endelse

   ; when trying to save all the final plots, it is an error to need rephot
   ;  post message and return if this is the case
   if saveallplots and redo_phot then begin
      errstr = [ $
         'It appears that the log1 file is out of date and needs to be regenerated.', $
         'However, this activity is not allowed when you are trying to generate the', $
         'final plots with the SAVEALLPLOTS flag to reductor.  If you were running', $
         'this function directly, then you need to work through all the rules first', $
         'before trying to save the plots.  If you were trying to promote the data', $
         'to a "reduced" state you should abort that operation and go back and finish', $
         'processing the data before trying again.' $
         ]
      return
   endif

   ; Rerun REPHOT if required.
   if redo_phot then begin
      if exists(fncache) then file_delete,fncache,/quiet,/noexpand_path
      if nosave then begin
         errstr = self + 'Cannot recreate log1, NOSAVE flag set, aborting.'
         print, errstr
         return
      endif
      print, 'Rewriting ', infofile
      ; log the event
      jdstr, systime(/JULIAN), 0, tis
      spawn, '/usr/bin/whoami', whois
      openw, loglun,infologfile, /GET_LUN,/APPend
      printf, loglun, tis + ' '  +  whois + ' ' +  'doing rephot:' +   $
                      string( info.newrad,info.newsky1,info.newsky2, $
                              info.newgain,info.newrdnoise)
      free_lun, loglun
      rephot,logfile,log1file,'files.cal',path=info.ddir+info.data, $
         newgain=info.newgain, newrad=info.newrad,newrdnoise=info.newrdnoise, $
         pscale=1.0,newsky1=info.newsky1,newsky2=info.newsky2,/silent, $
         calibpath=calibpath
      if not exists(log1file) then begin
         errstr = 'Error doing the rephot to recreate log1file'
         print, errstr
         return
      endif
   endif

   ; Check to see if log1 file needs to be loaded
   spawn,'ls -l '+log1file,result
   if result[0] ne info.logdate then begin

      rdobscod,code,alllon,rhosinp,rhocosp,obsname,valid,FILE=obsfile
      if valid then begin
         idx=where(info.obscode eq code,count)
         idx=idx[0]
         if (count eq 1) then begin
            lon = (360.0-alllon[idx])/180.0*!pi
            lat = atan(rhocosp[idx],rhosinp[idx])
            name=strtrim(obsname[idx],2)
         endif else begin
            info.obscode = '688'
            name=''
         endelse
      endif else begin
         print,'Observatory code file ',obsfile,' not found.'
         info.obscode = '688'
         name=''
      endelse
      ; Hardcoded position for 42" to get the program running on failure.
      IF obscode eq '688' and name eq '' THEN BEGIN
         ; This is the GPS position for the 42", derived 1993 Sep 08
         lat = (35.0+5.0/60.0+48.740/3600.0)/180.0*!pi
         lon = (111.0+32.0/60.0+10.601/3600.0)/180.0*!pi
         name= 'Lowell Observatory - Anderson Mesa Station'
      ENDIF
      print,obscode,' ',name

      print,'(re)loading photometry data'
      rdphalt,log1file,filename,obj,fil,jd,exptime,gain, $
         rad,sky1,sky2,serial,xpos,ypos,fwhm,maxcnt,sky,skyerr,mag,err,bad, $
         rdnoise
      nobs=n_elements(jd)
      idx=lindgen(nobs)
      pdata = { $
         filename: filename, $
         nobs:     nobs, $
         obj:      obj, $
         fil:      fil, $
         jd:       jd, $
         exptime:  exptime, $
         gain:     gain, $
         rdnoise:  rdnoise, $
         lat:      lat, $
         lon:      lon, $
         rad:      rad, $
         sky1:     sky1, $
         sky2:     sky2, $
         serial:   serial, $
         xpos:     xpos, $
         ypos:     ypos, $
         fwhm:     fwhm, $
         maxcnt:   maxcnt, $
         sky:      sky, $
         skyerr:   skyerr, $
         mag:      mag, $
         err:      err, $
         bad:      bad, $
         idx:      idx, $
         used:     bytarr(nobs), $
         ra:       fltarr(nobs), $
         dec:      fltarr(nobs), $
         stand:    strarr(nobs), $
         time:     fltarr(nobs), $
         am:       fltarr(nobs) $
         }
      info.logdate=result[0]
      info.needcoord=1
   endif

   if keyword_set(resetbad) then begin
      badisdirty=1
      pdata.bad[*]=0
   endif

   ; Check to see if the names correspondence file exists.
   if not exists(namesfile) then begin
      objlist=pdata.obj[uniq(pdata.obj,sort(pdata.obj))]
      origlist=objlist
      ; Try to clean up the names based on some simple rules
      for i=0,n_elements(objlist)-1 do begin
         if strmid(objlist[i],0,2) eq 'HD' and $
            strmid(objlist[i],2,1) ne ' ' then $
            objlist[i] = 'HD '+strmid(objlist[i],2,99) $
         else if strmid(objlist[i],0,3) eq 'RU ' then $
            objlist[i] = 'Rubin '+strmid(objlist[i],3,99) $
         else if strmid(objlist[i],0,3) eq 'SAO' and $
                 strmid(objlist[i],3,1) ne ' ' then $
            objlist[i] = 'SAO '+strmid(objlist[i],3,99) $
         else if strmid(objlist[i],0,2) eq 'PG' and $
                 strmid(objlist[i],2,1) ne ' ' then $
            objlist[i] = 'PG '+strmid(objlist[i],2,99) $
         else if strmid(objlist[i],0,2) eq 'SA' and $
                 strmid(objlist[i],2,1) ne ' '  and $
                 strmid(objlist[i],2,1) ne 'O' then $
            objlist[i] = 'SA '+strmid(objlist[i],2,99) $
         else if strmid(objlist[i],0,3) eq 'M67' and $
                 strmid(objlist[i],0,5) ne 'M67 I' and $
                 strmid(objlist[i],0,5) ne 'M67 F' then $
            objlist[i] = 'M67 F'+strmid(objlist[i],4,99)
      endfor
      getstars,objlist,tra,tdec,found,std=std_names
      z=where(found ne 1,count)
      if count ne 0 then begin
         for i=0,count-1 do begin
            ans=''
            if objlist[z[i]] eq 'Pluto' then begin
               ans='p9'
            endif else begin
               read,ans,prompt='Standard name for '+objlist[z[i]]+' ',format='(a)'
            endelse
            std_names[z[i]]=ans
         endfor
      endif
      idx=sort(std_names)
      wrmatch,std_names[idx],origlist[idx],namesfile
   endif

   ; Generate object coordinates, if needed
   if info.needcoord then begin
      coord,pdata.jd,info.obscode,pdata.obj,namesfile,ra,dec, $
         stand=stand,/ofdate,DEBUG=debug,/cache,fncache=fncache
      sz=size(ra)
      if sz[0] eq 0 then begin
         errstr='No coordinates could be generated for observations.'
         print,errstr
         return
      endif
      info.needcoord=0
      pdata.ra    = ra
      pdata.dec   = dec
      pdata.stand = stand
      pdata.time  = (pdata.jd - long(pdata.jd[0]-0.5d0)-0.50d0)*24.0
      pdata.am    = airmass(pdata.jd,pdata.ra,pdata.dec,pdata.lat,pdata.lon,0.6,594.0,0.0,0.50)
   endif

   ; Check to see if all is ok with airmass information
   z=where(pdata.am lt 1.0 or pdata.am gt 9.0,count)
   if count ne 0 then begin
      errstr = 'Bad airmass values, something needs to be fixed.'
      print, errstr
      for i=0,count-1 do begin
         rastr,pdata.ra[z[i]],1,str1
         rastr,pdata.dec[z[i]],0,str2
         print,i,pdata.stand[z[i]],pdata.time[z[i]], $
            str1,str2,pdata.am[z[i]], $
            format='(i3,1x,a16,1x,f8.5,1x,a,1x,a,1x,g10.3)'
      endfor
      info.needcoord=1
      return
   endif

   ;Cross check the filter reduction information against what is in the
   ;  photometry log file.
   newfils = pdata.fil[uniq(pdata.fil,sort(pdata.fil))]
   objlist = pdata.stand[uniq(pdata.stand,sort(pdata.stand))]

   print,info.data,', data in filters: ',newfils

   if fulldump ne '[[[none]]]' then begin
      if fulldump eq 'all' then begin
         for io=0,n_elements(objlist)-1 do begin
            z=where(pdata.stand eq objlist[io],count)
            for i=0,count-1 do begin
               print,pdata.filename[z[i]],pdata.stand[z[i]],pdata.time[z[i]], $
                     pdata.am[z[i]],pdata.fil[z[i]], $
                     pdata.mag[z[i]],pdata.err[z[i]],pdata.bad[z[i]], $
                     format='(a,1x,a16,1x,f8.5,1x,f5.3,1x,a2,1x,f7.4,1x,f6.4,1x,i1)'
            endfor
         endfor
      endif else begin
         z=where(pdata.stand eq fulldump,count)
         FOR i=0,count-1 do begin
            print,pdata.filename[z[i]],pdata.stand[z[i]],pdata.time[z[i]], $
                  pdata.am[z[i]],pdata.fil[z[i]], $
                  pdata.mag[z[i]],pdata.err[z[i]],pdata.bad[z[i]], $
                  format='(a,1x,a16,1x,f8.5,1x,f5.3,1x,a2,1x,f7.4,1x,f6.4,1x,i1)'
         endfor
      endelse
   endif

   for i=0,numop-1 do begin
      print,i,' ',info.oplist[i]
   endfor

   ; Now do the work required
   for i=0,numop-1 do begin

      rulerr = 0
      op = strsplit(strcompress(strtrim(info.oplist[i],2)),' ',/extract)
      if op[n_elements(op)-1] eq 'ok' then begin
         if force then $
            op = op[0:n_elements(op)-2] $
         else $
            goto,skipit
      endif

      case op[0] OF


; ------------------------------------------------------------------------------
; tr - Transformation against Landolt standard stars.
; ------------------------------------------------------------------------------

         'tr': begin
            ; Parameter validation
            ; enough arguments?
            if n_elements(op) lt 6 then begin
               print,'bad rule: ',info.oplist[i]
               rulerr=1
               goto,skipit
            endif

            ; data in the selected filter
            zfil = where(pdata.fil eq op[1],count)
            if count eq 0 then begin
               print,'No data found in filter ',op[1]
               rulerr=1
               goto,skipit
            endif

            ; regularize the optional arguments
            ; #6 could be k2 or ct, followed by two numbers
            forcek2=0
            forcect=0
            forcekt=0
            opnum=6
            k2=[999.0,0.]
            ct=[999.0,0.]
            while opnum lt n_elements(op) do begin
               if op[opnum] eq 'k2' and not forcek2 and $
                                        opnum+2 lt n_elements(op) then begin
                  forcek2 = 1
                  k2=[float(op[opnum+1:opnum+2])]
                  opnum = opnum+3
               endif else if op[opnum] eq 'ct' and $
                             not forcect and opnum+2 lt n_elements(op) then begin
                  forcect=1
                  ct=[float(op[opnum+1:opnum+2])]
                  opnum = opnum+3
               endif else if op[opnum] eq 'kt' and not forcekt then begin
                  forcekt=1
                  opnum = opnum+1
               endif else begin
                  print,'bad rule args: ',info.oplist[i]
                  rulerr=1
                  goto,skipit
               endelse
            endwhile

            if saveallplots then begin
               psname = 'tr.'+op[3]+','+op[4]+'-'+op[5]+'.ps'
               device,filename=psname
            endif else begin
               psname=''
            endelse

            if !d.name eq 'PS' then setpage,/portrait

            tmpbad=pdata.bad[zfil]
            ; log tr rule
            if not nosave then begin
               jdstr, systime(/JULIAN), 0, tis
               spawn, '/usr/bin/whoami', whois
               openw, loglun,infologfile, /GET_LUN,/APPend
               printf,loglun,tis+' '+whois+' '+strjoin(op, ' ')+' '+psname
               free_lun, loglun
            endif
            transf,pdata.stand[zfil],pdata.jd[zfil], $
               fix(op[3]),fix(op[4]),fix(op[5]), $
               pdata.am[zfil],pdata.mag[zfil],pdata.err[zfil],tmpbad, $
               title=info.inst+' '+info.data,other=pdata.time[zfil], $
               olab='UT Time',k2=k2,cterm=ct,ktime=forcekt,NOEDIT=noedit, $
               /chi,tagdate=info.data,taginst=info.inst,magresid=magresid, $
               NOSAVE=nosave, DB=(nosave eq 0),TABLENAME=transftable, $
               DVERBOSE=dverbose
            z1=where(tmpbad ne pdata.bad[zfil],countchange)
            if countchange gt 0 and not badisdirty then badisdirty=1
            pdata.bad[zfil]=tmpbad

            if not saveallplots then plotsdone=1

            ; Just for information, look at what is NOT a standard and summarize
            ;  to the screen.
            stdcheck,'/net/frakir/raid/buie/photometry/landolt/landphot.dat', $
               pdata.stand[zfil], $
               [ [ replicate(fix(op[3]),count) ], $
                  [ replicate(fix(op[4]),count) ], $
                  [ replicate(fix(op[5]),count) ]    ], $
               stdflg
            z=where(stdflg eq 0, count0)
            if count0 ne 0 then begin
               tmpnam=pdata.stand[zfil[z]]
               tmpnam=tmpnam[uniq(tmpnam,sort(tmpnam))]
               print,' Objects to reduce for ',landfil[fix(op[3])], $
                  ' wrt (',landfil[fix(op[4])],'-',landfil[fix(op[5])],')'
               for j=0,n_elements(tmpnam)-1 do print,' (0)  ',tmpnam[j]
            endif
            z=where(stdflg eq 3, count3)
            if count3 ne 0 then begin
               tmpnam=pdata.stand[zfil[z]]
               tmpnam=tmpnam[uniq(tmpnam,sort(tmpnam))]
               if count0 eq 0 then $
                  print,' Objects to reduce for ',landfil[fix(op[3])], $
                     ' wrt (',landfil[fix(op[4])],'-',landfil[fix(op[5])],')'
               for j=0,n_elements(tmpnam)-1 do print,' (3)  ',tmpnam[j]
            endif

            ; flag all observations that were used
            z=where(stdflg eq 1, count0)
            if count0 ne 0 then begin
               pdata.used[zfil[z]] = 1
               print,'tr rule, marking ',strn(count0),' as used.  ', $
                     'Total now ',strn(long(total(pdata.used))),'/',strn(pdata.nobs), $
                     ' used (', $
                     strtrim(string(float(total(pdata.used))/ $
                                    float(pdata.nobs)*100.0,format='(f10.1)'),2), $
                     '%)'
            endif

            if saveallplots then begin
               device,/close_file
            endif

         endcase ; Transformation block

; ------------------------------------------------------------------------------
; lc - Single color lightcurve against all sky standards.
; ------------------------------------------------------------------------------

         'lc': begin
            if n_elements(op) ne 10 then begin
               print,'bad rule: ',info.oplist[i]
               rulerr=1
               goto,skipit
            endif

            ;temporary expedient
            colorname =''
            if  op[6] eq '1' and op[7] eq '2' then colorname = 'B-V'
            if  op[6] eq '2' and op[7] eq '3' then colorname = 'V-R'
            
            ; First, must fetch the transformation coefficients for this obsn
            gettran,info.inst,info.data,fix(op[5]),fix(op[6]),fix(op[7]), $
               tran,transig,jdref,/DB,TABLENAME=transftable,VERBOSE=dverbose
            sz=size(tran)
            if sz[0] eq 0 then begin
               errstr='No transformation record was found.  Aborting.'
               print,errstr
               return
            endif
            transig[3]=0. ; turn off zeropoint error

            ; Now, generate lightcurve
            lnam=strlowcase(op[1])
            dnam=info.data+'.'+lnam
            oldbad=pdata.bad
            if saveallplots then begin
               psname = 'lc.'+op[1]+':'+op[2]+'_'+op[5]+','+op[6]+'-'+op[7]+'.ps'
               device,filename=psname
            endif else begin
               psname=''
            endelse
            if !d.name eq 'PS' then setpage,/landscape
            tmpbad=pdata.bad
            ; log the LC rule.
            if not nosave then begin
               jdstr, systime(/JULIAN), 0, tis
               spawn, '/usr/bin/whoami', whois
               openw, loglun,infologfile, /GET_LUN,/APPend
               printf,loglun,tis+' '+whois+' '+strjoin(op, ' ')+' '+psname
               free_lun, loglun
            endif
            ltcrv2,pdata.stand,pdata.fil,pdata.jd,pdata.time,pdata.am, $
                  pdata.serial,pdata.mag,pdata.err, $
                  float(op[8]),float(op[9]),op[1],fix(op[2]),op[3], $
                  tran,transig,jdref, $
                  filtname=op[4],file=dnam,badflags=tmpbad,DB=(nosave eq 0), $
                  NOSAVE=nosave,REFID=refid,TABLE=datatable,PLOTWIN=4, $
                  DVERBOSE=dverbose,COLORNAME=colorname,NOEDIT=noedit
            pdata.bad=tmpbad
            z1=where(oldbad ne pdata.bad,countchange)
            if countchange gt 0 and not badisdirty then badisdirty=1
            if not saveallplots then plotsdone=1

            if not nosave then begin
               ; Park the output files in the final data area
               onam=strmid(lnam,1,99)
               ; Numbered asteroid
               if strcompress(string(fix(onam)),/remove_all) eq onam then $
                  fdir = '/net/frakir/raid/buie/neo/a'+onam $
               ; unnumbered asteroid
               else $
                  fdir = '/net/frakir/raid/buie/neo/'+onam
               if not exists(fdir) then file_mkdir,fdir
               cmd='cp -p '+dnam+' '+fdir+'/'+info.data+'.001'
               print,cmd
               spawn,cmd
            endif
            ; flag anything that was used
            z=where(pdata.stand eq op[1] and pdata.serial eq fix(op[2]) and $
                    pdata.fil eq op[3],count0)
            if count0 ne 0 then begin
               pdata.used[z] = 1
               print,'lc rule, marking ',strn(count0),' as used.  ', $
                     'Total now ',strn(long(total(pdata.used))),'/',strn(pdata.nobs), $
                     ' used (', $
                     strtrim(string(float(total(pdata.used))/ $
                                    float(pdata.nobs)*100.0,format='(f10.1)'),2), $
                     '%)'
            endif
            if saveallplots then begin
               device,/close_file
            endif
         endcase ; single color lightcurve

; ------------------------------------------------------------------------------
; 2c - Two color lightcurve against all sky standards
; ------------------------------------------------------------------------------

         '2c': begin
            if n_elements(op) ne 13 then begin
               print,'bad rule: ',info.oplist[i]
               rulerr=1
               goto,skipit
            endif

            ; First, must fetch the transformation coefficients for this obsn
            gettran,info.inst,info.data,fix(op[5]),fix(op[6]),fix(op[7]), $
             tran,transig,jdref,/DB,TABLENAME=transftable,VERBOSE=dverbose
            sz=size(tran)
            if sz[0] eq 0 then begin
               errstr='No transformation record was found.  Aborting.'
               print,errstr
               return
            endif
            transig[3]=0. ; turn off zeropoint error
            gettran,info.inst,info.data,fix(op[10]),fix(op[11]),fix(op[12]), $
               tran1,transig1,jdref1,/db,VERBOSE=dverbose
            sz=size(tran1)
            if sz[0] eq 0 then begin
               errstr='No transformation record was found.  Aborting.'
               print,errstr
               return
            endif
            transig1[3]=0. ; turn off zeropoint error
            tran=[[tran],[tran1]]
            transig=[[transig],[transig]]
            jdref=[jdref,jdref1]

            ; Now, generate lightcurve
            lnam=strlowcase(op[1])
            dnam=info.data+'.'+lnam
            oldbad=pdata.bad
            if saveallplots then begin
               psname = '2c.'+op[1]+':'+op[2]+'_'+op[5]+','+op[10]+'.ps'
               device,filename=psname
            endif else begin
               psname=''
            endelse
            if !d.name eq 'PS' then setpage,/portrait
            tmpbad=pdata.bad
            ; log the 2C rule.
            if not nosave then begin
               jdstr, systime(/JULIAN), 0, tis
               spawn, '/usr/bin/whoami', whois
               openw, loglun,infologfile, /GET_LUN,/APPend
               printf,loglun,tis+' '+whois+' '+strjoin(op, ' ')+' '+psname
               free_lun, loglun
            endif
            ltcrv2c,pdata.stand,pdata.fil,pdata.jd,pdata.time,pdata.am, $
                  pdata.serial,pdata.mag,pdata.err, $
                  op[1],fix(op[2]),[op[3],op[8]],tran,transig,jdref, $
                  color,colorsig,badflags=tmpbad, $
                  filtname=[op[4],op[9]],error=error,file=[dnam,dnam], $
                  /DB,NOSAVE=nosave,REFID=refid,TABLE=datatable,PLOTWIN=4, $
                  DVERBOSE=dverbose,NOEDIT=noedit,JDOBS=jdcolor
            pdata.bad=tmpbad
            if error then goto,bailout
            z1=where(oldbad ne pdata.bad,countchange)
            if countchange gt 0 and not badisdirty then badisdirty=1
            if not saveallplots then plotsdone=1

            if not nosave then begin
               ; Save the final color to a file.
               colorfile=lnam+'.'+op[4]+'-'+op[9]
               colorinfo=string(info.data,' ',color,colorsig)
               openw,lun,colorfile,/get_lun
               printf,lun,colorinfo
               free_lun,lun
               print,'Final color written to file ',colorfile
               ; Save the final color to the data base.
               meanjd = mean(jdcolor)
               colornm = op[4] + '-' + op[9]
               print, 'updating ',  datatable, ' for ', op[1], $
                       ' in color ', colornm, ' for RefID ',refid
               print, ' mean jd of ', meanjd, ' for ', n_elements(z), ' points'
               ; bad flag implicitly 0 because this is clean data.
               ; there is no 'color for the color'.
               dbphot, refid, op[1], meanjd, colornm, color, colorsig, $
                    TABLE=datatable,/CLEANBYOBJFIL,NREMOV=nr,VERBOSE=dverbose
               print, nr, ' previous observations for ', op[1],  $
                     'in ', colornm, ' were removed.'
   
               ; Park the output files in the final data area
               onam=strmid(lnam,1,99)
               ; Numbered asteroid
               if strcompress(string(fix(onam)),/remove_all) eq onam then $
                  fdir = '/net/frakir/raid/buie/neo/a'+onam $
               ; unnumbered asteroid
               else $
                  fdir = '/net/frakir/raid/buie/neo/'+onam
               if not exists(fdir) then file_mkdir,fdir
               cmd='cp -p '+dnam+' '+fdir+'/'+info.data+'.001'
               print,cmd
               spawn,cmd
   
               ; Update average color for this object for this night
               print,'update ',fdir,'/',colorfile
               repwrite,fdir+'/'+colorfile,info.data,colorinfo
            endif
            ; flag anything that was used
            z=where(pdata.stand eq op[1] and pdata.serial eq fix(op[2]) and $
                    (pdata.fil eq op[3] or pdata.fil eq op[8]),count0)
            if count0 ne 0 then begin
               pdata.used[z] = 1
               print,'2c rule, marking ',strn(count0),' as used.  ', $
                     'Total now ',strn(long(total(pdata.used))),'/',strn(pdata.nobs), $
                     ' used (', $
                     strtrim(string(float(total(pdata.used))/ $
                                    float(pdata.nobs)*100.0,format='(f10.1)'),2), $
                     '%)'
            endif
            if saveallplots then begin
               device,/close_file
            endif
         endcase

; ------------------------------------------------------------------------------
; sl - Block for reducing Stars (in Landolt system) (color unknown)
; ------------------------------------------------------------------------------

         'sl': begin
            if n_elements(op) ne 7 then begin
               print,'bad rule: ',info.oplist[i]
               rulerr=1
               goto,skipit
            endif

            fil1 = fix(op[5])
            fil2 = fix(op[6])

            ; B, B-V,   V, B-V
            if fil1 eq 1 and fil2 eq 2 then begin
               fa=1
               fb=2
               c1=1
               c2=2
               f1='B'
               f2='V'
            endif else $

            ; V, V-R,   R, V-R
            if fil1 eq 2 and fil2 eq 3 then begin
               fa=2
               fb=3
               c1=2
               c2=3
               f1='V'
               f2='R'
            endif else begin
               rulerr = 1
               goto,skipit
            endelse

            ; First, must fetch the transformation coefficients for this obsn
            gettran,info.inst,info.data,fa,c1,c2,tran1,tran1sig,jdref1, $
                  /DB,TABLENAME=transftable,VERBOSE=dverbose
            gettran,info.inst,info.data,fb,c1,c2,tran2,tran2sig,jdref2, $
                  /DB,TABLENAME=transftable,VERBOSE=dverbose
            sz=size(tran1sig)
            if sz[0] eq 0 then begin
               errstr='No transformation record was found.  Aborting.'
               print,errstr
               return
            endif
            sz=size(tran2sig)
            if sz[0] eq 0 then begin
               errstr='No transformation record was found.  Aborting.'
               print,errstr
               return
            endif
            tran1sig[3]=0. ; turn off zeropoint error
            tran2sig[3]=0. ; turn off zeropoint error

            ; Next select observations to reduce.

            ; Get data in the selected filters that are not planetary
            zfil = where( (pdata.fil eq op[1] or pdata.fil eq op[2]) and $
                          strmid(pdata.stand,0,1) ne 'a' and $
                          strmid(pdata.stand,0,1) ne 'c' and $
                          strmid(pdata.stand,0,1) ne 'p' ,count)
            if count eq 0 then begin
               print,'No data found in filter ',op[1],' and ',op[2]
               goto,skipit
            endif

            ; Now must remove those that are standards
            stdcheck,'/net/frakir/raid/buie/photometry/landolt/landphot.dat', $
               pdata.stand[zfil], $
               [ [ replicate(c1,count) ], $
                 [ replicate(c2,count) ]    ], $
               stdflg

            z = where(stdflg ne 1,countdo)
            if countdo eq 0 then begin
               print,'No non-standard objects to reduce in ',op[1],' and ',op[2]
               rulerr=1
               goto,skipit
            endif
            zfil=zfil[z]

            ; Reduce `em
            oldbad=pdata.bad
            tmpbad=pdata.bad[zfil]
            ; log the SL rule.
            if not nosave then begin
               jdstr, systime(/JULIAN), 0, tis
               spawn, '/usr/bin/whoami', whois
               openw, loglun,infologfile, /GET_LUN,/APPend
               printf,loglun,tis+' '+whois+' '+strjoin(op, ' ') 
               free_lun, loglun
            endif
            colorsol,pdata.stand[zfil],pdata.fil[zfil],pdata.jd[zfil], $
               pdata.am[zfil],pdata.serial[zfil],pdata.mag[zfil], $
               pdata.err[zfil],op[1],op[2],tran1,tran1sig,jdref1, $
               tran2,tran2sig,jdref2,NOEDIT=noedit, $
               filter1=f1,filter2=f2,badflags=tmpbad, $
               path='/net/frakir/raid/buie/photometry/stars',FULL=full, $
               DB=(nosave eq 0),REFID=refid,SAVE=( nosave eq 0), $
               TABLE=datatable,DVERBOSE=dverbose
            pdata.bad[zfil]=tmpbad
            pdata.used[zfil] = 1
            print,'sl rule, marking ',strn(n_elements(zfil)),' as used.  ', $
                  'Total now ',strn(long(total(pdata.used))),'/',strn(pdata.nobs), $
                  ' used (', $
                  strtrim(string(float(total(pdata.used))/ $
                                 float(pdata.nobs)*100.0,format='(f10.1)'),2), $
                  '%)'
            z1=where(oldbad ne pdata.bad,countchange)
            if countchange gt 0 and not badisdirty then badisdirty=1
         endcase

; ------------------------------------------------------------------------------
; dp - Differential photometric reduction (like Pluto)
; ------------------------------------------------------------------------------

         'dp': begin
            if n_elements(op) lt 8 then begin
               print,'bad rule: ',info.oplist[i]
               rulerr=1
               goto,skipit
            endif

            case op[2] OF
               'B': fa=1
               'V': fa=2
               'R': fa=3
               'M': fa=9
               else: begin
                     print,'bad rule: ',info.oplist[i]
                     print,'Unrecognized filter [',op[2],']'
                     rulerr=1
                     goto,skipit
                  endelse
            endcase

            c1=strmid(op[3],0,1)
            colorname = c1 + '-'
            case c1 OF
               'B': c1=1
               'V': c1=2
               'R': c1=3
               else: begin
                     print,'bad rule: ',info.oplist[i]
                     print,'Unrecognized filter in color [',op[3],']'
                     rulerr=1
                     goto,skipit
                  endelse
            endcase

            c2=strmid(op[3],1,1)
            colorname += c2
            case c2 OF
               'B': c2=1
               'V': c2=2
               'R': c2=3
               else: begin
                     print,'bad rule: ',info.oplist[i]
                     print,'Unrecognized filter in color [',op[3],']'
                     rulerr=1
                     goto,skipit
                  endelse
            endcase



            ; regularize the optional arguments

            loadtrans=1
            transdate=info.data
            forcek2=0
            forcect=0
            forcek =0
            k2=[999.,0.]
            ct=[999.,0.]
            k =[999.,0.]
            opnum=8
            while opnum lt n_elements(op) do begin
               case op[opnum] OF

                  ; Override second order extinction, turns off auto load of
                  ;   transformation values, color set to zero if not already
                  ;   overridden.  Can't combine with td.
                  'k2': begin
                        if loadtrans and info.data ne transdate then begin
                           print,'bad rule: ',info.oplist[i]
                           print,'Cannot combine "td" with "k2"'
                           rulerr=1
                           goto,skipit
                        endif else if opnum+2 ge n_elements(op) then begin
                           print,'bad rule: ',info.oplist[i]
                           print,'Need two arguments for "k2"'
                           rulerr=1
                           goto,skipit
                        endif else begin
                           loadtrans=0
                           forcek2 = 1
                           k2=[float(op[opnum+1:opnum+2])]
                           opnum = opnum+3
                           if not forcect then begin
                              forcect=1
                              ct=[0.,0.]
                           endif
                        endelse
                     end
                  'ct': begin
                        if loadtrans and info.data ne transdate then begin
                           print,'bad rule: ',info.oplist[i]
                           print,'Cannot combine "td" with "ct"'
                           rulerr=1
                           goto,skipit
                        endif else if opnum+2 ge n_elements(op) then begin
                           print,'bad rule: ',info.oplist[i]
                           print,'Need two arguments for "ct"'
                           rulerr=1
                           goto,skipit
                        endif else begin
                           loadtrans=0
                           forcect = 1
                           ct=[float(op[opnum+1:opnum+2])]
                           opnum = opnum+3
                           if not forcek2 then begin
                              forcek2=1
                              k2=[0.,0.]
                           endif
                        endelse
                     end
                  'k': begin
                        if opnum+2 ge n_elements(op) then begin
                           print,'bad rule: ',info.oplist[i]
                           print,'Need two arguments for "k"'
                           rulerr=1
                           goto,skipit
                        endif else if forcek then begin
                           print,'bad rule: ',info.oplist[i]
                           print,'"k" can only be specified once'
                           rulerr=1
                           goto,skipit
                        endif else begin
                           forcek = 1
                           k=[float(op[opnum+1:opnum+2])]
                           opnum = opnum+3
                        endelse
                     end
                  'td': begin
                        if opnum+1 ge n_elements(op) then begin
                           print,'bad rule: ',info.oplist[i]
                           print,'Need one argument for "td"'
                           rulerr=1
                           goto,skipit
                        endif else if loadtrans and info.data ne transdate then begin
                           print,'bad rule: ',info.oplist[i]
                           print,'"td" can only be specified once'
                           rulerr=1
                           goto,skipit
                        endif else if forcek2 or forcect then begin
                           print,'bad rule: ',info.oplist[i]
                           print,'Cannot combine "td" with "k2" or "ct"'
                           rulerr=1
                           goto,skipit
                        endif else begin
                           transdate = op[opnum+1]
                           opnum = opnum+2
                        endelse
                     end
                  else: begin
                        print,'bad rule args: ',info.oplist[i]
                        print,'Unrecognized optional argument: [',op[opnum],']'
                        rulerr=1
                        goto,skipit
                     endelse
               endcase
            endwhile

            if fa ne 9 and loadtrans then begin
               gettran,info.inst,transdate,fa,c1,c2,tran,transig,jdref, $
                     /DB,TABLENAME=transftable,VERBOSE=dverbose
               sz=size(tran)
               if sz[0] eq 0 then begin
                  errstr='No transformation found for '+transdate+', '+ $
                         op[2]+' - '+op[3]
                  print,errstr
                  return
               endif
               k2 = [tran[1],transig[1]]
               ct = [tran[2],transig[2]]
            endif else colorname =''

            ; Load my standards and see if the magnitude is there and valid
            if fa ne 9 then begin
               rdphocat,'/net/frakir/raid/buie/photometry/buiestd.dat',lname,lmags,lcod
               cleanname = repchar(op[7],'_',' ')
               zstd = where(cleanname eq lname)
               if zstd[0] ge 0 then begin
                  smag = lmags[fa-1,zstd[0]]
                  scolor = lmags[c1-1,zstd[0]]-lmags[c2-1,zstd[0]]
               endif else begin
                  rdland2,lname,lmags,lcod
                  zstd = where(cleanname eq lname)
                  if zstd[0] ge 0 then begin
                     smag = lmags[fa-1,zstd[0]]
                     scolor = lmags[c1-1,zstd[0]]-lmags[c2-1,zstd[0]]
                  endif else begin
                     print,'bad rule args: ',info.oplist[i]
                     print,'Standard star ',cleanname,' not found.'
                     rulerr=1
                     goto,skipit
                  endelse
               endelse
            endif else begin
               smag = 0.0
               scolor = 0.0
            endelse

            stdname = repchar(op[7],'_',' ')
            op[4] = repchar(op[4],'_',' ')
            objname = naifname(op[4])
            dnam=info.data+'_'+op[2]+'.'+objname

            if not forcek then k = [999.,0.]

            if saveallplots then begin
               psname = 'dp.'+op[4]+':'+op[5]+'_'+op[2]+','+op[3]+'.ps'
               device,filename=psname
            endif else begin
               psname=''
            endelse
            if !d.name eq 'PS' then setpage,/portrait

            ; log the DP rule.
            if not nosave then begin
               jdstr, systime(/JULIAN), 0, tis
               spawn, '/usr/bin/whoami', whois
               openw, loglun,infologfile, /GET_LUN,/APPend
               printf,loglun,tis+' '+whois+' ' +strjoin(op, ' ')+' '+psname
               free_lun, loglun
            endif
            print, 'datable = ', datatable
            oldbad=pdata.bad
            tmpbad=pdata.bad
            ltcrv,pdata.stand,pdata.fil,pdata.jd,pdata.am,pdata.serial, $
               pdata.mag,pdata.err,op[4],fix(op[5]),stdname,op[1],jdout, $
               K2=k2,CTERM=ct,OCOLOR=[float(op[6]),0.],SCOLOR=[scolor,0.], $
               STDMAG=[smag,0.],FILTNAME=op[2],OBJNAME=objname, $
               FILE=dnam,BAD=tmpbad,FORCE=k,DB=(nosave eq 0), $
               NOSAVE=nosave,REFID=refid,TABLE=datatable,PLOTWIN=4, $
               DVERBOSE=dverbose,NOEDIT=noedit,COLORNAME=colorname
            sz=size(jdout)
            if sz[0] eq 0 then begin
               errstr='No lightcurve data was found.  Aborting.'
               print,errstr
               return
            endif

            pdata.bad=tmpbad
            z1=where(oldbad ne pdata.bad,countchange)
            if countchange gt 0 and not badisdirty then badisdirty=1

            if not saveallplots then plotsdone=1

            if objname eq 'Pluto' and not nosave then begin
               ; Put the output photometry in the final data area
               fdir='/net/frakir/raid/buie/pluto/Photometry/'
               cmd='cat '+info.data+'_*.'+objname+' | sort > '+fdir+info.data+'.001'
               print,cmd
               spawn,cmd
               ; Update the master history file for this dataset
               histname=fdir+'master.zpt'
               tag=string(info.data,op[2]+blanks,format='(a,1x,a2)')
               outinfo=string(info.inst+blanks,op[7]+blanks,smag,scolor, $
                                 format='(1x,a10,1x,a16,1x,f7.4,1x,f6.4)')
               print,tag+outinfo
               repwrite,histname,tag,tag+outinfo
            endif
            ; flag anything that was used
            z=where( ((pdata.stand eq op[4] and pdata.serial eq fix(op[5])) or $
                      (pdata.stand eq stdname)) and pdata.fil eq op[1],count0)
            if count0 ne 0 then begin
               pdata.used[z] = 1
               print,'dp rule, marking ',strn(count0),' as used.  ', $
                     'Total now ',strn(long(total(pdata.used))),'/',strn(pdata.nobs), $
                     ' used (', $
                     strtrim(string(float(total(pdata.used))/ $
                                    float(pdata.nobs)*100.0,format='(f10.1)'),2), $
                     '%)'
            endif
            if saveallplots then begin
               device,/close_file
            endif
         end

         else: begin
            print,'unknown rule: ',info.oplist[i]
            rulerr=1
         endelse
      endcase

skipit:
   if rulerr then begin
      errstr= ' Error in performing rule: '+ info.oplist[i]
      print,errstr
   endif
   endfor

   if force and (printit or saveallplots) then begin
      if saveallplots then begin
         psname = 'datamon.ps'
         device,filename=psname
      endif

      datamon,log1file,/print,/noqueue

      if not saveallplots then plotsdone=1 $
      else device,/close_file
   endif
   
   if !d.name eq 'PS' and plotsdone then begin
      hardcopy
      print,'Plot sent to the default printer.'
   endif
   if printit then begin
      display
   endif

bailout:
   if saveallplots then begin
      z=where(pdata.used eq 0,count)
      if count ne 0 then begin
         errstr = ['Not all of the data in the photometry log file has been processed', $
                   'by the defined rules.  There are '+strn(count)+' measurements'+ $
                     ' not used.', $
                   '', $
                   'Object         nobs filters' ]
         names = pdata.stand[z[uniq(pdata.stand[z],sort(pdata.stand[z]))]]
         for i=0,n_elements(names)-1 do begin
            zz=where(pdata.stand[z] eq names[i],count1)
            fils = pdata.fil[z[zz]]
            fillist = fils[uniq(fils,sort(fils))]
            fillist = strjoin(fillist,',')
            str = string(names[i]+blanks,count1,fillist,format='(a15,1x,i3,1x,a)')
            errstr = [errstr,str]
         endfor
         errstr = [errstr,'']
         for i=0,count-1 do begin
            str = pdata.filename[z[i]]
            jdstr,pdata.jd[z[i]],0,jds
            str += ' '+jds
            str += ' '+pdata.fil[z[i]]
            str += ' '+string(pdata.exptime[z[i]],format='(f5.1)')
            str += ' '+pdata.obj[z[i]]
            errstr = [errstr,str]
         endfor
         print,names
      endif
      set_plot,save_d_name
   endif

   if badisdirty and not nosave then begin
      print,'Updating ',log1file,' with new "bad" flags'
      fmt1 = '(a,1x,"''",a,"''",1x,a,1x,f13.5,1x,f8.3,1x,f6.2,1x,f6.2,' + $
               '1x,f7.3,1x,f7.3,' + $
             '1x,f7.3,1x,i4.4,1x,f8.3,1x,f8.3,1x,f5.2,1x,f7.1,1x,f8.2,1x,f6.2,' + $
             '1x,f8.4,1x,f7.4,1x,i1)'
      ; this is written v1.1 format
      openw,lun,log1file,/get_lun
      printf,lun,'PHOTFILE v1.1'
      for i=0,n_elements(pdata.jd)-1 do begin
         printf,lun,FORMAT=fmt1, $
         pdata.filename[i], pdata.obj[i], pdata.fil[i], pdata.jd[i], $
         pdata.exptime[i], pdata.gain[i], pdata.rdnoise[i], pdata.rad[i], $
         pdata.sky1[i], pdata.sky2[i], pdata.serial[i], $
         pdata.xpos[i], pdata.ypos[i], pdata.fwhm[i], $
         pdata.maxcnt[i], pdata.sky[i], pdata.skyerr[i], $
         pdata.mag[i], pdata.err[i], pdata.bad[i]
      endfor
      free_lun,lun
      spawn,'ls -l '+log1file,result
      info.logdate=result[0]
   endif

end
