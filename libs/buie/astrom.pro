;+
; NAME:
;  astrom
; PURPOSE:
;  Astrometry from a digital image.
; DESCRIPTION:
;
;  This program is designed to permit doing astrometry and catalog driven
;   photometry of digital images.  It is implicitly assumed that these are
;   true digital images, ie., that the images are strictly linear up to some
;   signal level.  The possiblities supported by this program are quite
;   extensive.  Read the PROCEDURE section below for more details.
;
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  astrom,root,fileno
;
; INPUTS:
;  root   - Root of data file name (ie., 970309), must be a string.
;  fileno - File number to load (0-999), integer.
;             If the file number is not provided then "root" is assumed
;             to contain the entire file name.
;
;  Normally I would use a call like:
;     astrom,'080501',1
;  and this would look for the file 080501.001 in the current directory.
;  If the data are somewhere else (this is normal) I might use:
;     astrom,'080501',1,path='/net/spikard/data1/buie/rawfits'
;  In this case it would look for the file:
;     /net/spikard/data1/buie/rawfits/080501/080501.001   (root is added to path)
;  However,  you could do the same thing with:
;     astrom,'/net/spikard/data1/buie/rawfits/080501/080501',1
;  or
;     astrom,'/net/spikard/data1/buie/rawfits/080501/080501.001'
;  Note: if the file is actually named 080501.001.fits the commands above
;  would not change and it would still find the file.
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  AUTODR   - Flag, if set, sets DRTHRESH automatically.  Obviously this only
;                applies if there is a pre-existing source list.  But,
;                DRTHRESH is set to half the mean spacing between sources in
;                the image.  But, the minimum allowable value is taken from
;                the input DRTHRESH variable (with its own default).
;
;  BATCH    - Flag, if set supresses ALL interactive operations.  This
;                doesn't mean it will all work right, rather, that when a
;                problem is found it quits so that processing on other frames
;                may continue.
;  BINFAC   - Binning factor for displayed image.  Default=2
;
;  BORDER   - Optional inset from each edge of array to be considered valid
;                for measurements.  The default is 20 pixels in from each
;                edge.  The value can either be scalar which is applied to
;                all edges, or a 4 element vector that specifies the inset
;                relative to [left,right,top,bottom]
;
;  CATPAD   - Amount of extra padding for catalog extraction in arcsec.  The
;                default is zero.
;
;  CENTER   - Controls what is used for center of image for star extraction
;                0 - (default)  with objects, uses object ephemeris as center
;                               no objects, interactive query for choice.
;                1 - Use the header coordinates, no corrections.
;                2 - Use the header coordinates after taking out last known
;                       position offset.
;                3 - Use the known center of the image if it is known, if
;                       it isn't known, reverts to center=2
;
;  CLEAN    - Flag, if set requests filtering out
;                cosmic ray strikes.  This step doesn't take too long on
;                a Sun Ultra 1/170 but may be prohibitive on slower machines.
;
;  DRTHRESH - Threshold on the radial distance (in pixels) of the catalog
;                to source matching step when working with a pre-computed
;                source list.  (Default=4.3)
;
;  EDIT     - Flag, if set allows interactive culling of bad astrometric
;                measurements in the reference net.
;
;  EXTLIST  - If image is a multi-extension FITS image, this list will
;                force the reduction of only the extension numbers listed.
;                The default is to do all the extensions, one at a time.
;
;  FNSRC    - Name of source file to use for auto correlation.  The default
;                is to use the original scheme where the files are
;                automatically located.  If you want to change the default
;                name and/or location, use this keyword.
;
;  FORCETERMS - String array, if provided, is a list of terms (from "terms"
;                  input variable) that will be forced to a constant
;                  value rather than being fitted.  See astsolve.pro for a
;                  complete description of the forcing process.  However,
;                  this program does not support all of the options provided
;                  by astsolve.  The only mode that is supported is the use
;                  of the external forcecoeff.dat file that is keyed by
;                  filter.
;
;  GAIN     - Gain (e-/ADU) of the image.  Default=1.0
;
;  IGNORESRC - Flag, if set suppresses using available src files in support
;                of a twostar reduction.
;
;  KEYLIST  - Name of a file containing a correspondence list. This list
;                associates a set of standard names with the actual keyword
;                names found in a FITS file header. If this keyword is
;                omitted, a default list is used, as if a file with the
;                following contents had been supplied:
;                   AIRMASS   K  AIRMASS
;                   DATE      K  DATE-OBS
;                   DATETMPL  T  DD-MM-YYYY
;                   EXPDELTA  V  0.0
;                   EXPTIME   K  EXPTIME
;                   FILTER    K  FILTERS
;                   FILENAME  K  CCDFNAME
;                   OBJECT    K  OBJECT
;                   UT        K  UT 
;                   RA        K  RA
;                   DEC       K  DEC
;                The middle column is a flag. It may be K, for Keyword,
;                T, for Template, or V, for Value. If it is V, the contents
;                of the third field on that line should make sense for the
;                name in the first field.
;
;  KILLREF  - Delete the existing Refstars/nnnXn.ref file, thus forcing
;                a recomputation of the initial astrometric registration.
;
;  MAGLIM   - Limiting (faint) magnitude for catalog extraction (default=30.0)
;
;  MAXBADENNE - Maximum number of bad enneeants allowed without a confirmation
;                 to proceed being asked.  Default=2
;
;  MAXOFFSET - Maximum offset between the catalog and source list that will
;                 be allowed without triggering a request for a twostar
;                 reduction.  (Default=no maximum)  This has no effect if
;                 the batch keyword is set.
;
;  MAXPHOTSIG - Maximum signal that is considered to be photometric or
;                 at least useful for astrometry.  If the peak signal in
;                 a given is above this value, that star is not used in the
;                 astrometric fit.  This has no effect on object measurements.
;                 Default value is 13000.  If you aren't sure about this
;                 value, set it to something large so that nothing gets
;                 excluded.  Normally you use this to automatically exclude
;                 reference stars whose cores are saturated.
;
;  MAXRCRIT - Maximum value of rcrit for which SHELLS-style solution is to
;                 be used on.  Default=no limit.  This is needed to control
;                 when the shells solution is used.  As the field density drops
;                 there may not be enough stars to do the initial
;                 cross-correlation and the program will fail.  But, as the
;                 field density drops there is less need to use the shells
;                 solution.  For Mosaic data, this cross-over is poorly
;                 defined but is near 30 pixels.
;
;  MAXSTARS - Maximum number of reference stars to measure. (default=all)
;
;  MINREFRESH - Flag, if set will minimize the amount of screen refreshes.
;
;  NEWCAT   - Flag, if set forces the recreation of the STARFILE.
;
;  NODISPLAY - Flag, if set supresses all graphical display.  This flag
;                implies /NOOBJECTS,/NOREMIND,/BATCH,TWOSTAR=0,EDIT=0
;
;  NOSCALE  - Flag, if set suppresses conversion from integer to floating point
;                when the FITS file is read.
;
;  NOOBJECTS - Flag, if set suppresses the final step of measuring astrometric
;                unknowns.
;
;  NOREMIND - Flag, if set suppresses the query for the reminder location.
;
;  NOSTRICTAPER - Flag, if set will suppress the protection that requires
;                    the object aperature (OBJRAD) to match the aperture used
;                    for the astrometry solution as found in the .ref files.
;                    This is not considered appropriate when working with
;                    CCD data but may be ok for scanned photographic plates.
;
;  OBJNAME  - Name of object to collect astrometry for.  By default, the
;                FITS header from the image is scanned for the OBJECT keyword
;                and this becomes the OBJNAME after compressing multiple blanks,
;                trimming leading and trailing blanks, and replacing single
;                blanks by "_".
;
;  OBJRAD   - Radius (in pixels) of object aperture for astrometry and
;                photometry.  Default=10.  The sky aperture is set between
;                objrad+10 and objrad+30.
;
;  OBSCODE  - Observatory code used for plast and ephem.  Default = 688
;
;  PATH     - String, this is the name of the directory where the data are
;                stored.  The actual data directory used is PATH+'/'+root.
;                The default is '' (blank) and the file would be root.NNN
;                which would permit putting a leading path on the root.
;
;  PLASTFILE - This gives the name of the "plast" output file.  The default
;                is OBJNAME.pla.  This file contains a list of asteroids that
;                may be found on the image and is created by a separate
;                program.  To disable this feature, set PLASTFILE='none'.
;
;  PHOTOGRAPHIC - Flag, if set will use photphot.pro for centroid and photometry
;                   calculations rather than basphote.  This is for use on
;                   scanned photographic data.
;
;  PHOTSTARS - Flag.  If set, turns on a special mode that performs photometry
;                on all good astrometric reference stars.  The photometry is
;                added to the root+'.log' file.  Multiple reductions are
;                weeded out and the photometry log file is left sorted by
;                file.  The log file is in the ALTLOG format (see basphote).
;                The star name is automatically created from its coordinate.
;                Ex: a star at 12:12:34.2 and +04:23:45 would be named
;                NV1212342+042345.  The position used for the name comes from
;                the catalog, not astrometry from the image.
;
;  PRETTY    - String, if set, will cause the final image with overlays to
;                be sent to a color postscript output file.  This will only
;                be used if you are measuring objects (id., NOOBJECTS not set).
;                If not specified, this file will not be created.
;
;  QUEUE     - String, name of printer to send output to.  If supplied, a
;                hardcopy of the image is generated along with a list of
;                stars identified.  If blank or not specified, no printed output
;                is generated.
;
;  REFFILE  - Reference file name.  This is used to store the cross referenced
;                list (stars vs. sources) for the input image.  By default
;                the file is named suff.ref where suff is the suffix of the
;                data file.  Note that the files are always placed in the
;                Refstars directory.  You can override this is needed as
;                might be the case for file names that are not particularly
;                helpful for this scheme.
;
;  RESFILE  - Filename where astrometric measurements are written to.  The
;                default file name is OBJNAME.ast.  Only one line per image
;                is allowed.  Subsequent measurements made by astrom will
;                override the measures for this image.
;
;  ROAM      - Flag, if set will provide a continuous running display of the
;                RA and Dec of the cursor when you are in the object
;                measurement loop.
;
;  ROTSCAN   - Optional control of the automatic catalog-source correlation.
;                If the initial correlation at the nominal rotation angle
;                fails, the program will call FRMDXYR to try rotations angle
;                near the nominal angle.  To use this keyword you need to
;                provide a two element vector [maxangle,stepsize,tolerance],
;                where maxangle is the largest amount to scan about nominal,
;                stepsize is the angular step to take, and tolerance is the
;                angular convergence required.  This causes the
;                program to run a loop from -maxangle to maxangle at stepsize
;                increments.  Once a useful rotation is seen it will do a
;                binary search from there down to the tolerance level provided.
;                Be careful in using this since you can really
;                slow the program down a lot by scanning a large range of
;                angles.  The default value for this is [1.0,0.1,0.001] and the
;                values must be provided in degrees.
;
;  SAVECLEAN - Flag, if set will save the cleaned image to disk.
;
;  SHELLS   - Solve for astrometry in radial shells.  The default is to take
;               the entire image all at once.  This is either a scalar or
;               vector quantity.  Valid entries are between 0 and 1, exclusive.
;               If any entry is out of range, behavior reverts to the default.
;               If provided as a vector, the values should be in increasing
;               order.  The first entry is always solved as a linear plate
;               solution.  Values less than 0.35 are also treated as linear.
;               Values less than 0.75 are solved by adding in the quadratic
;               terms.  Above 0.75 uses a full cubic solution.  The last
;               solution (using all available points) will use the terms as
;               specified by TERMS.  Example: [0.3,0.65] would
;               lead to a three step solution: 1) the first 30% of the image
;               sorted by R with a linear solution, 2) 65% of the image with
;               quadratic added, 3) all sources, solution using requested
;               terms.  Note, this keyword has no effect if you have requested
;               a pure linear solution.
;  
;  SKIPGOOD - Flag, if set will make the program skip any frame (or extension)
;               that already has a viable solution.  A good solution is defined
;               as one with a center in centers.dat and if there is a .ref file
;               for that frame.
;
;  SKIPOBJ -  Flag, if set, suppresses processing of any .obj files found
;               that may be related to the current image.  This can give you
;                substantial speed improvements if you aren't yet at at point
;                where you care about the .ast files.
;
;  SPOT     - Array containing explicit x,y coordinates.  Default=none.
;                After the astrometric fit is complete, the RA,DEC of each
;                x,y pair (2xN array) is computed, printed to the screen, and
;                saved to an ancillary file, spot.dat, in the current directory.
;
;  STARFILE - Filename where a list of astrometric reference stars is to
;                be found.  The default file name is OBJNAME.cat.  If this file
;                is not found, then this program will attempt to create the
;                file by calling "refnet", a program that accesses the
;                USNO A1.0 star catalog provided by David Monet of USNOFS.
;
;  SUBEXP   - This keyword controls reducing images with multiple exposures.
;                This keyword should contain one or more strings that will
;                serve to identify the multiple exposures.  Ideally, this
;                id string would be a single character, eg., 'a', 'b', etc.
;                This program will loop over the string list for multiple
;                reductions of the frame.  The id string will be appended to
;                the frame # where ever it is used.  So, in the .ast file and
;                fitcoeff.dat file the file name will be root.suffix_tag.  In
;                Refstars, the files are suffix_tag.ref.  The default is to
;                process one exposure per image and the _tag will not be added
;                to any names.
;
;  TERMS    - Vector that lists the terms to use in mapping from x,y to
;                xi,eta.  (not case sensitive)
;                default=['CONST','X','Y'] which is a pure linear fit.
;                See ASTTERMS.PRO for a description of the terms available.
;
;                Note: Version of this program prior to Nov. 2009 used
;                XITERMS and ETATERMS instead (and they were allowed to be
;                different).  To translate from the old vector to the new
;                format use:
;                names=['CONST','X','Y','R','XX','YY','XXX','XXY','XYY','YYY']
;                z=where(xiterms eq 1)
;                terms=names[z]
;                Also note that  while 'R' is still supported, its use is
;                not encouraged.  A better choice is 'XY'.   Lastly, there
;                is no support for mixing different settings of terms within
;                one night of data.  To use a different setting you must
;                remove the fitcoeff.dat file before changing.
;
;  TOHEADER - Flag, if set, the astrometric solution and photometric
;                zero-point are added to or updated in the header of the
;                image file.
;
;  TRUSTCENTER - Flag, if set indicates that the 2-star solution and the
;                   previously known plate center are to be trusted.  This
;                   removes the need to do the catalog star identifications.
;                   If the astrom.inf file is not found, or, if the plate
;                   center is not found in the centers.dat file, then this
;                   flag is ignored.  This flag is also ignored if there
;                   is a valid Refstars file.
;
;  TWEAKOBJ - Flag, if set, takes you to window 13 after measuring object
;               in window 0 to allow you to tweak the position in case the
;               automatic centroided position is not to your liking.  In
;               this window, left is an explicit position, middle is
;               redo automatic at this spot, and right is done (use last
;               position).
;
;  TWOMASS  - String.  Set to 'J', 'H', or 'K' to request 2MASS point-source
;                   catalog data in that filter.
;
;  TWOSTAR  - Flag, if set suppresses the automatic correlation of the source
;                   list and catalog and switches to an interactive two star
;                   solution to get the initial image location.  This should
;                   be done on the first frame of a night.  It also seems to
;                   be necessary for large fields with non-linear distortions.
;                   This flag has no effect if there are no pre-existing
;                   source lists generated by findsrc.pro.
;
;  XCENTER  - Optional override of location of center of optical axis in
;                pixel coordinates.  The default is the center of the array.
;                This location is considered the location of the tangent plane
;                and the location of x=y=0 for the (x,y) <--> (xi,eta)
;                transformation.
;
;  WINDOW   - window number to display image into.
;
;  YCENTER  - Optional override of location of center of optical axis in
;                pixel coordinates.  The default is the center of the array.
;                This location is considered the location of the tangent plane
;                and the location of x=y=0 for the (x,y) <--> (xi,eta)
;                transformation.
;
; OUTPUTS:
;
;  output is graphical and to a series of files.
;     astrom.inf  - Records the last 2-star astrometric solution.
;                     If the image being reduced is a multi-extension FITS
;                     file, this file will be named astromNN.inf where
;                     NN is the image extension of interest.
;     centers.dat - Records the image center for all measured frames along with
;                     the header coordinate and the offset between header and
;                     measured center.  Column 1 is the file/extension name,
;                     column 2&3 are the measured image center, column 4&5 are
;                     the header center, and column 6&7 are the offsets in the
;                     sense of measured-header converted to radians [cos(dec)
;                     is included in the ra offset].
;     root.log    - Photometry (if PHOTSTARS set)
;     objname.ast - Astrometry of object
;     objname.cat - Star catalog extraction
;     objname.pla - Output of PLAST (list of asteroids on image).
;     root.stars  - List of stars and coordinates for those where photometry
;                      was measured.  Intended for inclusion in starcat.dat
;                      (see GETSTARS.PRO or LOADSTAR.PRO).
;     Refstars/fileno.ref - Binary file containing positions, mag, fwhm for
;                           all catalog stars measured.  This file will be
;                           be reused in later runs of ASTROM on this image
;                           as long as the object aperture radius is the same.
;     fitcoeff.dat- List of fit coefficients for each of the xi,eta axes.
;     position.dat- List of x,y positions for all objects measured.
;
; KEYWORD OUTPUT PARAMETERS:
;
;  FITDATA  - Anonymous structure containing all information pertaining
;               to the astrometric fit.  If this reduction is for a simple
;               fits file then the variable will contain a structure with
;               the information.  If the file is a multi-group fits file
;               then the tags will be arrays of values.
;
; COMMON BLOCKS:
;  MWB_ASTROMCOL  (used internal to this program)
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
;  Input files must all be FITS and the file names must be of the form:
;    root.NNN where "root" is some string and NNN is a 3-digit number.
;
;  The file name can also have an additional tag of .fits added to the above
;    name.  This form is search for automatically and will be used
;    preferentially if found.
;
; PROCEDURE:
;
;  This program automates astrometric reductions of CCD images.  Once the
;  astrometric solution is determined for the image, you can then proceed
;  to measure any source in the image to ascertain it's position.  As you
;  might guess from the above list, there are far too many options to this
;  program.  It is rare that you will use all the options, instead, some
;  subset can be tweaked and tuned to _your_ data to make the process run
;  as quickly as possible and with as little user interaction as possible.
;  Under certain circumstances this program can run complete automatically
;  but only if a great deal is already known about the images.
;
;  To illustrate one use of the program consider doing astrometry of 1 or
;  more objects on an image.  Typically on the first invocation on a new
;  image that you know little about you will not use any optional information.
;  However, the first and most important optional input you can provide is
;  the FITS keyword correspondence information through the KEYLIST option.
;  If you have a decent header, then the program will have a good object name,
;  time for the image, and (hopefully) a good coordinate for the image center.
;
;  The object name is important because it is used to form file names for
;  the output astrometry and the ancillary star catalog.  If the images you
;  are reducing have the same name for different sky locations then this
;  program will get hopelessly confused.
;
;  Next, ASTROM will look for a special file, astrom.inf, that contains clues
;  about the image scale and orientation.  If not found, you will be prompted
;  for the needed information.  This file can be edited after creation if you
;  need to try other guesses (such as flipping image, trying different scales,
;  trying different rotation angles, etc.).  This step can be very frustrating
;  if you don't have much information about your image.  I quite often find
;  it necessary to edit this file many times and re-run ASTROM on the same
;  image until it makes sense.
;
;  Next, ASTROM will look for a list of stars that should be on the frame and
;  will serve as the astrometric reference network.  If found, the file is
;  read.  If not found, then ASTROM will try to create this file using another
;  external program (refnet).  The set of stars requested will depend on the
;  scale and orientation known at this point so if you get it wrong, delete the
;  the star catalog and start over.  The center for the star search comes
;  from a number of places.  This is what ASTROM tries, in order, (1)
;  using the object name (first one if given an array), try to get an
;  ephemeris position for the object for the time of the image, if this
;  fails --> (2) ask for RA and DEC of image center.  (3) If NOOBJECTS is set,
;  (1) and (2) are bypassed and the RA,DEC from the header are used (after
;  precessing to J2000, if needed).  If you aren't doing objects, and the
;  header value is bad, then you may need to insert your guess for the
;  center directly into the "centers.dat" file.  Sometimes this is the only
;  way to proceed if the headers are really screwy.
;
;  Next, ASTROM will try to get a list of
;  known asteroids that _might_ be on the image.  For this to work, you
;  must have an external program that ASTROM will invoke to collect this
;  information to a file.  If the file already exists, then it will be
;  read directly without the need for the external program.  Note: this
;  external program is non-trivial and cannot be easily exported away from
;  Lowell Observatory.  Fortunately, it can be disabled, but, if you can
;  use it then you will see positional overlays of the expected locations
;  of any asteroids on the frame along with their line of variation scaled
;  by the orbit uncertainty (green line) and a 1-hour motion vector (yellow
;  line).
;
;  With this information in hand, ASTROM begins to work with the screen image.
;  The image is displayed with a fairly hard linear stretch, -7 sigma to
;  +16 sigma about the mean sky signal.  The border is drawn and you are
;  (possibly) asked to indicate a location on the image to be remembered.  I
;  find this useful in marking the location of a specific object that will
;  be highlighted throughout the analysis.  This is not used with NOOBJECTS
;  set.
;
;  If you allow it, ASTROM will proceed to remove cosmic rays from the image.
;  This step is almost always useful but can sometimes take a long time to
;  run depending on the size of the image and the speed of your computer.
;  On a SUN Ultra 1/170E, a 4k x 2k image can take 10-20 seconds to clean.
;  After cleaning the image is redisplayed with the same scaling.  If you watch
;  carefully you will see the cosmic ray strikes "blink off".
;
;  Next, the asteroid overlay is generated and plotted.  This involves
;  generating ephemeris positions for all the asteroids for the time of this
;  frame.  Again, an external program is called to generate these positions
;  and you find the relevant documentation in my IDL front-end program, EPHEM.
;
;  Now, we get on to the steps of getting the astrometric grid in place.
;  The image center, scale and orientation are used in calculating the locations
;  of all catalog stars.  Red diamonds, scaled in size by magnitude, are
;  plotted on the image at these predicted locations.  Your job at this
;  point is to match up the overlay with the image.  This can be either
;  very easy, very hard, or anywhere in between.  You may find need to
;  tweak the contents of astrom.inf, change MAGLIM, or more depending on
;  the situation.  Sometimes the center is no good.  One of two things will
;  happen, (1) the wrong star overlay is plotted on the image, or (2) no
;  stars are plotted.  In the latter case, you will be prompted for a new
;  image center.  If you're lucky, this will allow you to proceed.
;
;  Assuming all goes well, you now must establish the correspondence between
;  the overlay and the image.  At this step you must identify two stars
;  in the image and the same two stars in the overlay.  The prompts from ASTROM
;  will indicate what information it next desires.  Just remember that at
;  any time ASTROM is looking for a mouse click to proceed, a right click will
;  exit the program directly at that point.  After you identify the first star,
;  the overlay is replotted so that the overlay star sits over the image.
;  After you identify the second star ASTROM has enough information to predict
;  the location of all the other catalog stars and can proceed to measure
;  their locations.  Once all the catalog stars are measured, an astrometric
;  function is fit to the positions.  See, ASTTERMS for all the possible
;  terms.  The default is a linear solution which is usually pretty good for
;  most CCD images.  At this step you may need to fiddle with the aperture
;  radius used to measure the stars.  A radius (OBJRAD) that is too big
;  or too small can lead to excessive scatter in the astrometry.  I usually
;  try to set OBJRAD=fwhm or just slightly under the fwhm.  These positions
;  the subsequent fit are saved to a couple of files so that if you come
;  back to the image later, the fit is regenerated much quicker and you get
;  directly to the next step, that of measuring unknown objects.
;
;  Note that the fit to the star positions is done in a robust fashion.  Stars
;  with large residuals, unusual fwhm, signal too weak, or signal too strong,
;  are avoided in the fit.  These stars will plot as red circles in the final
;  overlay while the good stars will overplot with yellow circles.  You can
;  control the "too strong" threshold with MAXPHOTSIG or you can do a purely
;  interactive editing of the star positions used with /EDIT.  Using /EDIT
;  is required only if there just aren't that many stars (say 3-6) and there's
;  no statistical basis for chucking out anything.  If there are lots of stars,
;  the automatic stuff works just fine.
;
;  If you find that the header gives consistently good predictions of the
;  catalog star locations, then use the TRUSTCENTER keyword to bypass the
;  2-star interactive step.  Be careful, you need to have pretty good
;  for this to work effectively.  Also note that if you turn on the PHOTSTARS
;  flag, ASTROM will automatically collect and save aperture photometry data
;  on _all_ the catalog stars.
;
;  At this point, you've arrived at the time you can measure new objects.
;  You are prompted to click left on the object to measure.  Nothing is saved
;  until you click middle (to go to next object) or click right (to quit).
;  So you can feel free to poke around in the image measuring lots of objects
;  without saving all of it.  When you click left it measures the location
;  and computes RA,DEC, and, it puts up a small window on the object with the
;  aperture location overplotted.  In this window, there is a non-linear
;  stretch so that you can see the wings of the PSF as well as the core.
;  You also get a radial profile for additional diagnostic information.
;  If you continue with a middle click, ASTROM will step through the OBJNAME
;  array (if provided) or when it runs of out names it knows, it will begin
;  prompting you for a name.  This name is used to form the file name for
;  all saved astrometry.  Once you have processed an entire nights worth of
;  data then you can use the ASTCOL program to collection it all and save it
;  to another master storage location.  The ASTCOL step is where the
;  observatory code information is added to the astrometry.
;
;  If you set /PRETTY, the very last thing done is to create a fancy postscript
;  output image file.  This image has all the overlays and asteroid locations
;  and is occasionally useful.
;
;  This description is by no means exhaustive.  There are a very large number
;  of options that must be tweaked to get the most out of the reduction.  I
;  ususally find it useful to create a master script that contains all the
;  flags and options as I develop a means to reduce some data.
;
;  Here's one example:
;      astrom,fdir,fnum,maglim=17, $
;         binfac=1,objrad=objrad,path=d,key='../site.key',objname=o, $
;         /noremind,noobjects=noob,gain=2.5,plastfile='none',trustcenter=trust, $
;         xi=[1,1,1,0,0,0,0,0,0,0],eta=[1,1,1,0,0,0,0,0,0,0],maxphotsig=23000.0
;
;  Here I've set the limiting magnitude for the reference stars to 17.0,
;  no cosmic ray cleaning, don't ask for reminder location, set gain
;  of CCD, turn off looking for field asteroids, force linear fit, and set
;  saturation level of CCD.  The other options are either obvious or set
;  to fixed values in the script handling this call.  Note that by splitting
;  the file name into root and suffix, you can generate a loop on the suffix
;  and step through all the images you have.
;
; MODIFICATION HISTORY:
;  97/04/05, Written by Marc W. Buie, Lowell Observatory
;  97/06/13, MWB, added keylist fits header reading generalization
;  97/06/14, MWB, fixed line of variations plotting bug, also added saving
;                 information on reference stars used.
;  97/06/17, MWB, added controls over terms in Xi and Eta fits.
;  97/06/18, MWB, added saving fit coeffcients and object positions.
;  97/06/19, MWB, added NOREMIND keyword
;  97/06/20, MWB, added TRUSTCENTER keyword
;  97/09/09, MWB, added SUBEXP keyword
;  97/10/08, MWB, added X,YCENTER keywords.
;  97/10/16, MWB, added SPOT keyword.
;  97/10/21, MWB, rewrote initial 2-star fit.
;  97/11/24, MWB, added MAXSTARS keyword.
;  98/01/06, MWB, added support for pre-cleaned images.
;  98/03/13, MWB, some heavy rewriting.  NOCLEAN  is now CLEAN (default none)
;                   plus internal cleanups, some changes to ancillary plots,
;                   added support for external lists of image sources.
;  98/06/24, MWB, added TWOSTAR flag.
;  98/08/26, MWB, added ROAM keyword.
;  98/10/07, MWB, a few optimizations and some bug fixes and enhancements
;                   for multi-extension files.
;  98/10/08, MWB, added DRTHRESH keyword
;  98/11/04, MWB, added NOSCALE keyword
;  98/12/02, MWB, changed usage of PATH so that you can append 'root' or not.
;                   This now allows the image file name roots to be different
;                   from the name of the directory they are stored in.
;  99/03/30, MWB, fixed bug with precession of header coordinates.
;  99/03/30, MWB, added KILLREF keyword.
;  2000/01/18, MWB, modification for new version of rdastfc
;  2000/02/04, MWB, added .fits optional tag on file name
;  2000/02/07, MWB, added OBSCODE keyword
;  2000/04/12, MWB, added new information output to centers.dat, also added
;                      BATCH keyword.
;  2000/05/11, MWB, added CATPAD keyword.
;  2000/08/01, MWB, added NODISPLAY keyword.
;  2000/09/12, MWB, rewrite of auto-astrometry for high order fits
;  2000/09/17, MWB, added SKIPGOOD, SHELLS, AUTODR, and MAXRCRIT keywords.
;  2000/09/18, MWB, added SKIPOBJ keyword.
;  2000/09/19, MWB, fixed cos(dec) header offset problem
;  2000/10/25, MWB, fixed lingering MAC--addslash bug.
;  2001/03/28, MWB, added support of properly using a 24-bit display.
;  2001/03/29, MWB, limit size of ephemeris error to twice field size.
;  2001/04/20, MWB, changes to support new ephem/geteph version
;  2001/10/23, MWB, changed constraint on minimum number of stars, was forced
;                     to be 10, now it can be lower if you don't enable all
;                     the cross terms.
;  2002/02/06, MWB, removed CATPATH keyword
;  2002/04/06, MWB, WATCHOUT!  I changed the program so that if you are using
;                     the twostar reduction, it will save the refstars AFTER
;                     the fit, thus excluding the ill-fitting objects from
;                     the saved set.  This might be dangerous in some cases
;                     so make sure to do a /killref if redoing the solution
;                     where you think too many have been deleted.  I have a
;                     suspicion that this will break some other case for the
;                     program but I can't see where right now.  (Probably in
;                     cases where it takes a very long time to do the
;                     photometry on the fields stars in this program.)
;
;                     Also, added IGNORESRC keyword.
;  2002/04/22, MWB, restricted the number of catalog stars used during
;                     shells solution.
;  2002/04/24, MWB, added a two star solution helper to the shells operation
;                     if the initial linear correlation fails.
;  2002/08/16, MWB, modified to use new rdplast routine.
;  2002/09/09, MWB, added support for string obscode values
;  2003/02/26, MWB, merged with orphan code from Corwin, should be just
;                     changes in comments and other documentation.
;  2003/05/30, MWB, changed some loop variables to LONG
;  2003/06/01, MWB, converted my Delfile calls to IDL file_delete calls
;  2003/10/01, MWB, converted my Mkdir calls to IDL file_mkdir calls
;  2004/9/21, MWB, removed obsolete call to Findfile
;  2005/06/26, MWB, added TWOMASS (2MASS) catalog support
;  2005/06/28, MWB, fixed obscure bug affecting window 13 when interactively
;                    measureing objects.  No impact on output data.
;  2006/01/12, MWB, added MAXOFFSET keyword
;  2006/02/01, MWB, changed behavior if fileno.  Now if fileno is undefined
;                     then root is taken to be the entire file name (.fits
;                     tag is optional).
;  2006/02/02, MWB, added FNSRC keyword (override on where src file names)
;  2006/02/03, MWB, added output keyword FITDATA.
;  2006/04/26, MWB, fixed FNSRC bug from previous change that affected 
;                     multi-group fits files.
;  2007/10/29, MWB, added PHOTOGRAPIC keyword on scanned plate data (incomplete)
;                     added TWEAKOBJ keyword
;  2007/11/06, MWB, fixed bug with partial solutions where you have to help
;                     with a two star solution.  Problem if help led to a
;                     significant change in rotation.
;  2007/11/07, MWB, added NOSTRICTAPER keyword for photographic plate data
;  2008/06/17, MWB, FNSRC input was being ignored, fixed.
;                   Also modified to use rdstarc (which will modify .cat files)
;  2008/10/31, MWB, fixed a minor problem affecting astrom.inf information
;                      when the frame is really close to 0h RA (more likely at
;                      higher Dec).
;  2009/07/24, MWB, fixed bug related to correlating star lists with a source
;                      list when a field rotation is needed in the auto
;                      correlation.
;  2009/12/02, MWB, big rewrite to change the selection and control of
;                      fitting terms.  xiterms and etaterms are consolidated
;                      and TOHEADER keyword was added.
;  2009/12/09, MWB, another epoch of substantial changes to the program.
;                      The routine astinvrt was retired in favor of the new
;                      program, astsn2xy.  This version has been pretty
;                      well tested against images with non-zero but weak
;                      optical distortions.
;  2009/12/30, MWB, added ability to force terms in the fit using the
;                      keyword FORCETERMS
;  2010/07/26, MWB, minor mod for new frmdxdy error codes
;  2010/09/15, MWB, added REFFILE keyword
;  2010/11/22, MWB, added ROTSCAN keyword
;-

; This routine takes charge of getting the coordinate of the center of the
;   image.  The file 'centers.dat' in the current directory is used as a
;   repository for all centers in this directory's reductions.  If no center
;   is found the caller is notified of this lack.
;
; Inputs:
;  thisexp - name of the exposure to get center for
;  suff    - Suffix of the file name, note this information is contained
;               within thisexp and could be derived from it but normally
;               the calling routine will already know this field and the
;               work need not be replicated here.
; Outputs:
;  cra     - Right Ascension of center (radians)
;  cdec    - Declination of center (radians)
;  found_center - Flag, true if a center was successfully located.
pro astrom_getctr,thisexp,suff,cra,cdec,found_center

   ; Set some initial flags
   thisfile=-1
   last=''
   found_center=0

;print,'gA [',thisexp,'] [',suff,']'

   ; Look for image center from previous reduction.
   if exists('centers.dat') then begin

      ; Open the cache file for reading
      openr,lun,'centers.dat',/get_lun
      line=''
      done=0

      ; Scan the file, one line at a time looking for the requested exposure.
      while not eof(lun) and not done do begin

         ; Read line and compress out all extraneous blanks
         readf,lun,line,format='(a)'
         cline=strcompress(strtrim(line,2))

         ; Only process non-blank lines
         if cline ne '' then begin

            ; Break up the line into words, delimited by blanks.  Only the
            ;  first three words are of interest here.
            words=strsplit(cline,' ',/extract)
;print,'gB ',words

            ; The first word contains the file name with an optional tag.
            fnf=strsplit(words[0],'.',/extract)
            if n_elements(fnf) eq 1 then begin
               fnf=[fnf,'']
            endif

            ; It is a failure to not see at least three words in the line
            ;   or if the the file name won't break into two or more fields.
            if n_elements(words) lt 3 or n_elements(fnf) lt 2 then begin
               print,'ASTROM: Fatal error occurred reading the centers.dat file.  The last line'
               print,'read is:'
               print,'[',line,']'
               print,'This should have three blank separated fields, the first would be a file'
               print,'name of the form, root.suffix, and the next two fields are the RA and Dec'
               print,'of the center.  You must fix this file before astrom can continue.'
               print,''
               free_lun,lun
               return
            endif

            ; check the second field of the first word.  The files are all in
            ;  sorted order.  That means if you get past the desired field there
            ;  is no point in looking any further in the file.
;print,'gC ',thisexp,' ',words[0],' ',fnf[1],' ',suff
;            if thisexp gt words[0] and fnf[1] gt suff then begin
            if words[0] gt thisexp then begin
               done=1
;print,'gC done'

            ; Check to see if this might be the right entry
            endif else begin

               ; If the file name from this line matches the goal then
               ;   read the ra and dec and we're done.
               if thisexp eq words[0] then begin
                  cra=raparse(words[1])
                  cdec=decparse(words[2])
                  found_center=1
                  done=1
               endif else last=line
            endelse

         endif ; end line processing section

      endwhile ; end of file scanning loop
      free_lun,lun

   endif

   ; This part just prints some diagnostic information.
   if found_center then begin
      rastr,cra,1,ras
      decstr,cdec,0,decs
      print,' *** Loading derived coordinates for image center: ', $
            ras,' ',decs
   endif else begin
      print,' !!! Unable to find derived coordinates for image center.'
   endelse

end

; This routine takes care of refreshing the display window.  The current image
;  is put on in the window, a border is plotted on the image and the coordinate
;  system is left setup for additional plot overlays.
; Inputs:
;  window - IDL window number to display into
;  bim    - Pre-scaled byte image to be put in window (no scaling applied).
;  binfac - Binning factor between original image and byte image.
;  border - 4-element vector with border, [left,right,bottom,top]
pro astrom_dim,window,bim,binfac,border
   common mwb_astromcol,color0,color1,color2,color3,color4,n_colors,savec
   setwin,window
   sz=size(bim)
   tv,bim
   plot,[0],[1],/nodata,xmargin=[0,0],ymargin=[0,0], $
      xr=[0,sz[1]*binfac-1],yr=[0,sz[2]*binfac-1],xstyle=5,ystyle=5,/noerase
   oplot,[border[0],sz[1]*binfac-border[1],sz[1]*binfac-border[1],border[0],border[0]], $
         [border[3],border[3],sz[2]*binfac-border[2],sz[2]*binfac-border[2],border[3]],color=color4
end

; Save the center information to the centers.dat file.  The information in
;  centers.dat by field is:
;     (1)   file name (with possible extension tag)
;     (2)   RA of center of image
;     (3)   Dec of center of image
;     (4)   RA from header
;     (5)   Dec from header
;     (6)   ra offset from header to center in radians
;     (7)   dec offset from header to center in radians
;
; Inputs:
;  thisexp - string with name of file
;  ra      - RA of center of image.
;  dec     - Dec of center of image.
;  linfo   - Structure with information about the astrometric solution
;              Tags used:
;                 pscale - plate scale of image, arcsec/pixel.
;                 rang   - rotation angle of image on plane of sky
;  hdrinfo - Structure with information about the image from its header
;              Tags used:
;                 ra     - RA of image from header
;                 dec    - Dec of image from header
pro astrom_svctr,thisexp,ra,dec,linfo,hdrinfo

   ; Convert coordinates to strings
   rastr,ra,4,ras
   decstr,dec,3,decs
   rastr,hdrinfo.ra,4,hras
   decstr,hdrinfo.dec,3,hdecs

   ; Compute the offset (in radians) between the known center of the image
   ;   and the center read from the header.
   raoff=ra-hdrinfo.ra
   if raoff gt      !pi then raoff=raoff-2.0d0*!dpi
   if raoff lt -1.0*!pi then raoff=raoff+2.0d0*!dpi
   raoff=raoff*cos(dec)
   decoff=dec-hdrinfo.dec

   ; Information for the user to see
   print,' '
   print,'Image@',ras,decs,'scale',linfo.pscale,'arcsec/pixel', $
         linfo.rang*!radeg,'deg rotation', $
         format='(a,1x,a,1x,a,1x,a,f6.3,1x,a,1x,f7.2,1x,a)'

   ; save information to the centers.dat file
   cinfo=thisexp+' '+ras+' '+decs+' '+hras+' '+hdecs+' ' + $
                 strn(raoff,format='(e15.7)',length=15) + ' ' + $
                 strn(decoff,format='(e15.7)',length=15)
   repwrite,'centers.dat',thisexp,cinfo
end

; This routine will (re)generate a two-star linear plate solution given the
;   input quantities.  It will then update this information in the relevant
;   locations in the linfo structure.
;  Inputs:
;     xpos  - Vector of x source positions
;     ypos  - Vector of y source positions
;     sra   - Vector of RA of sources that match (xpos,ypos)
;     sdec  - Vector of Dec of sources that match (xpos,ypos)
;     zs1   - Index into xpos,ypos and sra,dec for one position to use
;     zs2   - Index into xpos,ypos and sra,dec for second position to use
;     linfo - Information structure, fields read from
;              xflip  - x flip flag
;              yflip  - y flip flag
;              xcref  - x coordinate of ra reference
;              ycref  - y coordinate of dec reference
;              xc     - x position of center of chip
;              yc     - y position of center of chip
;              xcopt  - x position of optical axis
;              ycopt  - y position of optical axis
;  Outputs:
;     linfo - Information structure, fields modified:
;              raref  - ra of eta,xi reference
;              decref - dec of eta,xi reference
;              pscale - effective linear plate scale (arcsec/pixel)
;              rang   - rotation angle of image on plane of the sky (radians)
;              racen  - ra of image center
;              deccen - dec of image center
;              raopt  - ra of optical axis
;              decopt - dec of optical axis
pro astrom_regen,xpos,ypos,sra,sdec,zs1,zs2,linfo

   ; Get first and second positions stored into more convienient variables
   xs1m = xpos[zs1]
   xs2m = xpos[zs2]
   ys1m = ypos[zs1]
   ys2m = ypos[zs2]

   ; Use the first position as the xi,eta reference, this is just temporary
   linfo.raref  = sra[zs1]
   linfo.decref = sdec[zs1]

   ; Compute pixel offset from position 1 to 2.
   dx      = linfo.xflip*(xs2m-xs1m)
   dy      = linfo.yflip*(ys2m-ys1m)
   pixsep = sqrt(dx^2+dy^2)

   ; convert ra,dec of position 2 to xi,eta
   astrd2sn,sra[zs2],sdec[zs2],linfo.raref,linfo.decref,xi2,eta2,/ARCSEC

   ; compute angular separation in arcsec
   angsep = sqrt(xi2^2+eta2^2)

   ; compute scale and position angle
   linfo.pscale  = angsep/pixsep
   linfo.rang    = atan(dy*xi2-dx*eta2,dx*xi2+dy*eta2)

   ; setup a simple linear solution, remember that position 1 is at
   ;  x=y=xi=eta=0 by design.
   terms0 = ['x','y']
   cxi0   = [cos(linfo.rang),sin(linfo.rang)]*linfo.pscale
   ceta0  = [-sin(linfo.rang),cos(linfo.rang)]*linfo.pscale

   ; compute position of reference center relative to position 1
   dx   = linfo.xflip*(linfo.xcref - xs1m)
   dy   = linfo.yflip*(linfo.ycref - ys1m)

   ; compute xi,eta of reference center, again relative to position 1
   xi0  = asteval(dx,dy,cxi0,terms0)/3600.0d0*!dpi/180.0d0
   eta0 = asteval(dx,dy,ceta0,terms0)/3600.0d0*!dpi/180.0d0

   ; convert from xi,eta to ra,dec.  This is saved back into the linfo
   ;   structure for the new raref,decref position
   astsn2rd,xi0,eta0,linfo.raref,linfo.decref,ra,dec
   linfo.raref  = ra[0]
   linfo.decref = dec[0]

   ; compute position of optical axis relative to position 1
   dx   = linfo.xflip*(linfo.xcopt - xs1m)
   dy   = linfo.yflip*(linfo.ycopt - ys1m)

   ; compute xi,eta of optical axis, again relative to position 1
   xi0  = asteval(dx,dy,cxi0,terms0)/3600.0d0*!dpi/180.0d0
   eta0 = asteval(dx,dy,ceta0,terms0)/3600.0d0*!dpi/180.0d0

   ; convert from xi,eta to ra,dec.  This is saved back into the linfo
   ;   structure for the new raopt,decopt position
   astsn2rd,xi0,eta0,linfo.raref,linfo.decref,ra,dec
   linfo.raopt  = ra
   linfo.decopt = dec

   ; compute position of center relative to position 1
   dx   = linfo.xflip*(linfo.xc - xs1m)
   dy   = linfo.yflip*(linfo.yc - ys1m)

   ; compute xi,eta of optical axis, again relative to position 1
   xi0  = asteval(dx,dy,cxi0,terms0)/3600.0d0*!dpi/180.0d0
   eta0 = asteval(dx,dy,ceta0,terms0)/3600.0d0*!dpi/180.0d0

   ; convert from xi,eta to ra,dec.  This is saved back into the linfo
   ;   structure for the new raopt,decopt position
   astsn2rd,xi0,eta0,linfo.raref,linfo.decref,ra,dec
   linfo.racen  = ra
   linfo.deccen = dec
end

; Save the astrometric reference net used to determine the plate solution
;  Inputs:
;     reffile - String, name of file to write data to
;     o       - Object aperture radius (float, scalar)
;     c       - Number of objects (long, scalar)
;     x       - X position of source (float, vector)
;     y       - Y position of source (float, vector)
;     f       - FWHM of source (float, vector)
;     m       - instrumental magnitude of source (float, vector)
;     e       - mag uncertainty (float, vector)
;     mx      - peak signal on source (float, vector)
;     ra      - J2000 ra of source in radians (double, vector)
;     dec     - J2000 dec of source in radians (double, vector)
;     sm      - Catalog magnitude of source (float, vector)
;
; The data are saved as binary so the byte order is set to be little endian.
;
pro astrom_svref,reffile,o,c,x,y,f,m,e,mx,ra,dec,sm
   wrref,reffile,{objrad:float(o), nstars:long(c), $
                  xpos:float(x), ypos:float(y), fwhm:float(f), $
                  mag:float(m), err:float(e), sig_max:float(mx), $
                  ra:double(ra), dec:double(dec), catmag:float(sm)}
end

; x,y Pixel coordinates of catalog stars
; valid - Flag, true if successful, if not true, do not continue.
pro astrom_twostar,dv,linfo,sra,sdec,sx,sy,ssz,valid

   common mwb_astromcol,color0,color1,color2,color3,color4,n_colors,savec

   valid=0

retry:

   ; Force xi,eta reference point to be center of image
   linfo.raref  = linfo.racen
   linfo.decref = linfo.deccen
   linfo.xcref  = linfo.xc
   linfo.ycref  = linfo.yc

   ; simple linear conversion
   astrd2xy,sra,sdec,linfo,sx,sy,XI=sxi,ETA=seta

   astrom_dim,dv.win,dv.bim,dv.binfac,dv.border
   for i=0L,n_elements(sx)-1 do $
      oplot,[sx[i]],[sy[i]],psym=4,color=color2,symsize=ssz[i]

   ; Select first anchor star from catalog list
   print,'     Click left on symbol for first anchor star (right=quit).'
   cursor,xs1,ys1,3
   if !mouse.button eq 4 then return
   dist = sqrt((xs1-sx)^2+(ys1-sy)^2)
   zs1 = where(dist eq min(dist))
   zs1 = zs1[0]
   oplot,[sx[zs1]],[sy[zs1]],psym=4,color=color4,symsize=2
   oldx = sx[zs1]
   oldy = sy[zs1]

   ; Click on the star in the image
   print,'     Click left on the star in the image (right=quit).'
   cursor,xs1,ys1,3
   if !mouse.button eq 4 then return
   basphote,dv.gain,dv.image,dv.exptime,xs1,ys1, $
      dv.objrad,dv.objrad+10,dv.objrad+30, $
      /nolog,/silent,xcen=xs1m,ycen=ys1m

   ; Update overlay based on first star.
   linfo.raref  = sra[zs1]
   linfo.decref = sdec[zs1]
   linfo.xcref  = xs1m
   linfo.ycref  = ys1m
   astrd2xy,sra,sdec,linfo,sx,sy,XI=sxi,ETA=seta
   if sqrt((oldx-xs1m)^2+(oldy-ys1m)^2) gt 4.0 then begin
      astrom_dim,dv.win,dv.bim,dv.binfac,dv.border
      for i=0L,n_elements(sx)-1 do $
         oplot,[sx[i]],[sy[i]],psym=4,color=color2,symsize=ssz[i]
      oplot,[xs1m],[ys1m],psym=4,color=color1
   endif

   ; Select second anchor star from catalog list
   print,'     Click left on symbol for second anchor star '+ $
         '(middle=restart, right=quit).'
   cursor,xs2,ys2,3
   if !mouse.button eq 2 then goto,retry
   if !mouse.button eq 4 then return
   dist = sqrt((xs2-sx)^2+(ys2-sy)^2)
   zs2 = where(dist eq min(dist))
   zs2 = zs2[0]
   oplot,[sx[zs2]],[sy[zs2]],psym=4,color=color4,symsize=2

   ; Click on the star in the image
   print,'     Click left on the star in the image (right=quit).'
   cursor,xs2,ys2,3
   if !mouse.button eq 4 then return
   basphote,dv.gain,dv.image,dv.exptime,xs2,ys2,dv.objrad,dv.objrad+10,dv.objrad+30, $
      /nolog,/silent,xcen=xs2m,ycen=ys2m
   oplot,[xs2m],[ys2m],psym=4,color=color1

   ; Separation in pixels
   dx      = linfo.xflip*(xs2m-xs1m)
   dy      = linfo.yflip*(ys2m-ys1m)
   pixsep = sqrt(dx^2+dy^2)

   ; Compute standard coordinates of two stars
   ;  for simplicity, force tangent point to be on star 1, that
   ;  means x1=y1=0 and xi1=eta1=0
   astrd2sn,sra[zs2],sdec[zs2],linfo.raref,linfo.decref,xi2,eta2,/ARCSEC
   angsep = sqrt(xi2^2+eta2^2)

   ; plate scale in arcsec/pixel and image rotation angle.
   pscale  = angsep/pixsep
   rang    = atan(dy*xi2-dx*eta2,dx*xi2+dy*eta2)

   ; cast into standard transformation
   terms0 = ['x','y']
   cxi0   = [cos(rang),sin(rang)]*pscale
   ceta0  = [-sin(rang),cos(rang)]*pscale

   ; Save values
   linfo.pscale = pscale
   linfo.rang   = rang

   ; Redetermine the ra and dec center
   dx   = linfo.xflip*(linfo.xc - xs1m)
   dy   = linfo.yflip*(linfo.yc - ys1m)
   xi0  = asteval(dx,dy,cxi0,terms0)/3600.0d0*!dpi/180.0d0
   eta0 = asteval(dx,dy,ceta0,terms0)/3600.0d0*!dpi/180.0d0
   astsn2rd,xi0,eta0,linfo.raref,linfo.decref,ra,dec
   linfo.racen  = ra
   linfo.deccen = dec

   ; Redetermine the ra and dec of the optical center
   dx   = linfo.xflip*(linfo.xcopt - xs1m)
   dy   = linfo.yflip*(linfo.ycopt - ys1m)
   xi0  = asteval(dx,dy,cxi0,terms0)/3600.0d0*!dpi/180.0d0
   eta0 = asteval(dx,dy,ceta0,terms0)/3600.0d0*!dpi/180.0d0
   astsn2rd,xi0,eta0,linfo.raref,linfo.decref,ra,dec
   linfo.raopt  = ra
   linfo.decopt = dec

   ; simple linear conversion
   astrd2xy,sra,sdec,linfo,sx,sy,XI=sxi,ETA=seta

   valid=1

end

pro astrom_objectzoom,dv,linfo,xm,ym,fwhm0

   common mwb_astromcol,color0,color1,color2,color3,color4,n_colors,savec

   xm0=fix(xm+0.5)
   ym0=fix(ym+0.5)
   dw=fix(ceil(dv.objrad*3.5))
   theta=findgen(361.0)/!radeg
   xcirc=dv.objrad*cos(theta)
   ycirc=dv.objrad*sin(theta)

   ; Extract image sub-section around object, watch out for image
   ;   edges.
   x10=xm0-dw
   x20=xm0+dw
   y10=ym0-dw
   y20=ym0+dw
   x1=max([x10,0])
   x2=min([x20,dv.nx-1])
   y1=max([y10,0])
   y2=min([y20,dv.ny-1])
   swd=2*dw+1
   robomean,dv.image[x1:x2,y1:y2],3.0,0.5,meansky
   subim=replicate(meansky,swd,swd)
   subim[x1-x10,y1-y10] = dv.image[x1:x2,y1:y2]

   if fwhm0 gt 1.5 then begin
      radp,subim,xm-x1,ym-y1,r,i,rfwhm,rcoefs,rfit,fzwid=fwhm0
   endif else begin
      radp,subim,xm-(xm0-dw),ym-(ym0-dw),r,i,rfwhm,rcoefs,rfit,fzwid=meanfwhm
   endelse

   idx=sort(r)
   setwin,12
   plot,r[idx],i[idx],psym=4
   oplot,r[idx],rfit[idx],color=color3
   xyouts,0.6,0.8,'FWHM='+ $
      strcompress(string(fwhm0,format='(f6.1)'),/remove)+ $
      ' pixels',/normal
   xyouts,0.6,0.75,'FWHM='+ $
      strcompress(string(fwhm0*linfo.pscale, $
      format='(f6.1)'),/remove)+' arcsec',/normal
   zbin=10
   zsz=zbin*(2*dw+1)
   setwin,13,xsize=zsz,ysize=zsz,/show
   spotim=subim
   robomean,spotim,3.0,0.5,meansky,dummy1,meanskysig
   spotim=(((spotim-(meansky-3.0*meanskysig)) > 0)+10)^0.1

   tv,rebin(bytscl(spotim,min=min(spotim),max=max(spotim), $
                   top=n_colors-1-savec)+savec,zsz,zsz,/sample)
   plot,[0],[1],/nodata,xmargin=[0,0],ymargin=[0,0], $
      xr=[xm0-dw-0.5,xm0+dw+0.5],yr=[ym0-dw-0.5,ym0+dw+0.5], $
      xstyle=5,ystyle=5,/noerase
   oplot,xcirc+xm,ycirc+ym,color=color1
   oplot,xcirc*0.5+xm,ycirc*0.5+ym,color=color1

end

pro astrom,root,fileno, $
       AUTODR      = autodr, $
       BATCH       = batch, $
       BINFAC      = binfac, $
       BORDER      = border, $
       CATPAD      = catpad, $
       CENTER      = center, $
       CLEAN       = clean, $
       DRTHRESH    = in_drthresh, $
       EDIT        = edit, $
       EXTLIST     = extlist, $
       FITDATA     = fitdata, $
       FNSRC       = in_fnsrc, $
       FORCETERMS  = forceterms, $
       GAIN        = gain, $
       IGNORESRC   = ignoresrc, $
       KEYLIST     = keylist, $
       KILLREF     = in_killrefstars, $
       MAGLIM      = maglim, $
       MAXBADENNE  = maxbadenne, $
       MAXOFFSET   = maxoffset, $
       MAXPHOTSIG  = maxphotsig, $
       MAXRCRIT    = maxrcrit, $
       MAXSTARS    = maxstars, $
       MINREFRESH  = minrefresh, $
       NEWCAT      = newcat, $
       NODISPLAY   = nodisplay, $
       NOOBJECTS   = noobjects, $
       NOREMIND    = noremind, $
       NOSCALE     = noscale, $
       NOSTRICTAPER= nostrictaper, $
       OBJNAME     = in_objname, $
       OBJRAD      = objrad, $
       OBSCODE     = obscode, $
       PATH        = in_path, $
       PHOTOGRAPHIC= photographic, $
       PHOTSTARS   = photstars, $
       PLASTFILE   = plastfile, $
       PRETTY      = pretty, $
       QUEUE       = queue, $
       RESFILE     = resfile, $
       REFFILE     = in_reffile, $
       ROAM        = roam, $
       ROTSCAN     = rotscan, $
       SAVECLEAN   = saveclean, $
       SHELLS      = shells, $
       SKIPGOOD    = skipgood, $
       SKIPOBJ     = skipobj, $
       SPOT        = spot, $
       STARFILE    = starfile, $
       SUBEXP      = subexp, $
       TERMS       = terms, $
       TOHEADER    = toheader, $
       TRUSTCENTER = in_trustcenter, $
       TWEAKOBJ    = tweakobj, $
       TWOMASS     = twomass, $
       TWOSTAR     = twostar, $
       WINDOW      = window, $
       XCENTER     = in_xcenter, $
       YCENTER     = in_ycenter

   common mwb_astromcol,color0,color1,color2,color3,color4,n_colors,savec

   self='ASTROM: '
   if badpar(root,7,0,CALLER='ASTROM: (root) ') then return
   if badpar(fileno,[0,2,3],0,CALLER='ASTROM: (fileno) ',type=fileno_type) then return

   if badpar(maglim,[0,2,3,4,5],0,CALLER='ASTROM: (MAGLIM) ', $
                              default=30.0) then return
   if badpar(in_drthresh,[0,2,3,4,5],0,CALLER='ASTROM: (DRTHRESH) ', $
                              default=4.3) then return
   if badpar(maxrcrit,[0,2,3,4,5],0,CALLER='ASTROM: (MAXRCRIT) ', $
                              default=-1) then return
   if badpar(binfac,[0,1,2,3],0,CALLER='ASTROM: (BINFAC) ', $
                              default=2) then return
   if badpar(edit,[0,1,2,3],0,CALLER='ASTROM: (EDIT) ',default=0) then return
   if badpar(batch,[0,1,2,3],0,CALLER='ASTROM: (BATCH) ',default=0) then return
   if badpar(maxbadenne,[0,1,2,3],0,CALLER='ASTROM: (MAXBADENNE) ', $
                              default=0) then return
   if badpar(maxoffset,[0,2,3,4,5],0,CALLER='ASTROM: (MAXOFFSET) ', $
                              default=-1) then return
   if badpar(newcat,[0,1,2,3],0,CALLER='ASTROM: (NEWCAT) ', $
                              default=0) then return
   if badpar(clean,[0,1,2,3],0,CALLER='ASTROM: (CLEAN) ', $
                              default=0) then return
   if badpar(noobjects,[0,1,2,3],0,CALLER='ASTROM: (NOOBJECTS) ', $
                              default=0) then return
   if badpar(in_killrefstars,[0,1,2,3],0,CALLER='ASTROM: (KILLREF) ', $
                              default=0) then return
   if badpar(in_objname,[0,7],[0,1],CALLER='ASTROM: (OBJNAME) ', $
                             default='[[DEFAULT]]') then return
   if badpar(objrad,[0,1,2,3,4,5],0,CALLER='ASTROM: (OBJRAD) ', $
                              default=10) then return
   if badpar(in_path,[0,7],0,CALLER='ASTROM: (PATH) ', $
                             default='[[DEFAULT]]') then return
   if badpar(maxphotsig,[0,2,3,4,5],0,CALLER='ASTROM: (MAXPHOTSIG) ', $
                              default=13000.0) then return
   if badpar(plastfile,[0,7],0,CALLER='ASTROM: (PLASTFILE) ', $
                              default='[[DEFAULT]]') then return
   if badpar(photstars,[0,1,2,3],0,CALLER='ASTROM: (PHOTSTARS) ', $
                              default=0) then return
   if badpar(pretty,[0,7],0,CALLER='ASTROM: (PRETTY) ', $
                              default='[[DEFAULT]]') then return
   if badpar(ignoresrc,[0,1,2,3],0,CALLER='ASTROM: (IGNORESRC) ', $
                              default=0) then return
   if badpar(roam,[0,1,2,3],0,CALLER='ASTROM: (ROAM) ', $
                              default=0) then return
   if badpar(rotscan,[0,4,5],1,CALLER='ASTROM: (ROTSCAN) ', $
                              default=[1.0,0.1,0.001],npts=nrotscan) then return
   if badpar(queue,[0,7],0,CALLER='ASTROM: (QUEUE) ', $
                             default='') then return
   if badpar(resfile,[0,7],0,CALLER='ASTROM: (RESFILE) ', $
                             default='[[DEFAULT]]') then return
   if badpar(starfile,[0,7],0,CALLER='ASTROM: (STARFILE) ', $
                              default='[[DEFAULT]]') then return
   if badpar(keylist,[0,7],0,CALLER='ASTROM: (KEYLIST) ', $
                              default='[[DEFAULT]]') then return
   if badpar(window,[0,1,2,3],0,CALLER='ASTROM: (WINDOW) ', $
                              default=0) then return
   if badpar(border,[0,1,2,3],[0,1],CALLER='ASTROM: (BORDER) ', $
                              default=20,rank=brank) then return
   if badpar(terms,[0,7],1,CALLER='ASTROM: (TERMS) ', $
                        default=['CONST','X','Y'],npts=nterms) then return
   if badpar(saveclean,[0,1,2,3],0,CALLER='ASTROM: (SAVECLEAN) ', $
                                default=0) then return
   if badpar(noremind,[0,1,2,3],0,CALLER='ASTROM: (NOREMIND) ', $
                                default=0) then return
   if badpar(toheader,[0,1,2,3],0,CALLER='ASTROM: (TOHEADER) ', $
                                default=0) then return
   if badpar(in_trustcenter,[0,1,2,3],0,CALLER='ASTROM: (TRUSTCENTER) ', $
                                default=0) then return
   if badpar(gain,[0,2,3,4,5],0,CALLER='ASTROM: (GAIN) ', $
                                default=1.0) then return
   if badpar(debug,[0,1,2,3],0,CALLER='ASTROM: (DEBUG) ',default=0) then return
   if badpar(in_fnsrc,[0,7],0,CALLER='ASTROM: (FNSRC) ', $
                             default='[[DEFAULT]]') then return
   if badpar(in_reffile,[0,7],0,CALLER='ASTROM: (REFFILE) ', $
                             default='[[DEFAULT]]') then return
   if badpar(subexp,[0,7],[0,1],CALLER='ASTROM: (SUBEXP) ', $
                             default='[[DEFAULT]]') then return
   if badpar(spot,[0,2,3,4,5],[1,2],CALLER='ASTROM: (SPOT) ', $
                             default=[-1.0e9,-1.0e9]) then return
   if badpar(minrefresh,[0,1,2,3],0,CALLER='ASTROM: (MINREFRESH) ', $
                              default=0) then return
   if badpar(maxstars,[0,1,2,3],0,CALLER='ASTROM: (MAXSTARS) ', $
                              default=0) then return
   if badpar(extlist,[0,1,2,3],[0,1],CALLER='ASTROM: (EXTLIST) ', $
                              default=-1) then return
   if badpar(center,[0,1,2,3],0,CALLER='ASTROM: (CENTER) ', $
                              default=0) then return
   if badpar(twomass,[0,7],0,CALLER='ASTROM: (TWOMASS) ', $
                              default='') then return
   if badpar(twostar,[0,1,2,3],0,CALLER='ASTROM: (TWOSTAR) ', $
                              default=0) then return
   if badpar(noscale,[0,1,2,3],0,CALLER='ASTROM: (NOSCALE) ', $
                              default=0) then return
   if badpar(nostrictaper,[0,1,2,3],0,CALLER='ASTROM: (NOSTRICTAPER) ', $
                              default=0) then return
   if badpar(skipgood,[0,1,2,3],0,CALLER='ASTROM: (SKIPGOOD) ', $
                              default=0) then return
   if badpar(skipobj,[0,1,2,3],0,CALLER='ASTROM: (SKIPOBJ) ', $
                              default=0) then return
   if badpar(autodr,[0,1,2,3],0,CALLER='ASTROM: (AUTODR) ', $
                              default=0) then return
   if badpar(obscode,[0,1,2,3,7],0,CALLER='ASTROM: (OBSCODE) ', $
                              default=688,type=codetype) then return
   if badpar(catpad,[0,2,3,4,5],0,CALLER='ASTROM: (CATPAD) ', $
                             default=0) then return
   if badpar(shells,[0,4,5],[0,1],CALLER='ASTROM: (SHELLS) ', $
                             default=-1.0) then return
   if badpar(photographic,[0,1,2,3],0,CALLER='ASTROM: (PHOTOGRAPHIC) ', $
                              default=0) then return
   if badpar(tweakobj,[0,1,2,3],0,CALLER='ASTROM: (TWEAKOBJ) ', $
                              default=0) then return
   if badpar(forceterms,[0,7],[0,1],CALLER='ASTROM: (FORCETERMS) ', $
                              default='') then return

   if codetype ne 7 then begin
      obscode = strn(obscode,length=3,padchar='0')
   endif else begin
      obscode = strupcase(obscode)
   endelse

   if binfac le 0 then begin
      binfac=1
      print,'WARNING: Illegal binfac value provided, setting to 1.'
   endif

   if nrotscan ne 3 then begin
      print,'ERROR!  NROTSCAN keyword must have exactly three elements'
      return
   endif
   rotscan=abs(rotscan)

   if ignoresrc then twostar=1

   doshells = nterms gt 3 and min(shells) gt 0.0 and max(shells) lt 1.0

   residcap = 3.0

   errfile = 'astrom.err'

   ; Special handling for NODISPLAY keyword.  If this is set a bunch of
   ;   other stuff must be set (or overridden).
   if badpar(nodisplay,[0,2,3,4,5],0,CALLER='ASTROM: (NODISPLAY) ', $
                             default=0) then return
   if nodisplay then begin
      print,'Display disabled.'
      batch    = 1
      edit     = 0
      noobjects= 1
      noremind = 1
      twostar  = 0
   endif

;===========================================
; This section is only run once upon starting.

   setusym,1
   maglim=float(maglim) ; force type to float
   do_objects = not noobjects
   fullrefresh = not minrefresh
   ans=''
   blanks='              '
   bel=STRING( 7B )
   n_colors = !d.n_colors
   savec=5
   if n_colors gt 256 then begin
      n_colors=256
      savec=0
   endif

   ; Get header correspondence list.
   loadkeys,keylist,hdrlist

   ; First, detangle the input information and get pointed at the data file
   ;  and directory.
   if in_path eq '[[DEFAULT]]' then begin
      path = ''
   endif else begin
      if fileno_type ne 0 and exists(addslash(addslash(in_path)+root)) then $
         path = addslash(addslash(in_path)+root) $
      else $
         path = addslash(in_path)
   endelse

   if not exists('Refstars') then file_mkdir,'Refstars'

   if brank eq 0 then begin
      border=replicate(border,4)
   endif else begin
      if n_elements(border ne 4) then begin
         print,'ASTROM:  Illegal border, must be scalar or 4-element vector.'
         print,'         [left,right,top,bottom] in pixels from edge.'
         return
      endif
      if min(border) lt 0 then begin
         print,'ASTROM:  Illegal border, all values must be greater than 0.'
         return
      endif
   endelse

   if fileno_type eq 0 then begin
      suffix=''
   endif else begin
      numtoflist,fileno,suffix   ; suffix now includes the dot
;      suffix=string(fileno,format='(i3.3)')
   endelse

   ; if precleaned version exists we'll use that (regardless of flag).
   imfile=root+'c'+suffix
   ft=''
   if not exists(path+imfile+ft) then begin
      if fileno_type eq 0 then begin
         imfile=root
      endif else begin
         imfile=root+suffix
;         imfile=root+'.'+suffix
      endelse
      if exists(path+imfile+'.fits') then ft='.fits' else ft=''
      preclean=0
      if not exists(path+imfile+ft) then begin
         print,'ASTROM: Image ',path+imfile,' not found.'
         print,'  Aborting.'
         return
      endif
   endif else begin
      preclean=1
   endelse

   ; Check header of image to see if it is a multi-extension image.
   hdr=headfits(path+imfile+ft)
   numext=sxpar(hdr,'NEXTend')
   if numext eq 0 then begin
      extlist=0
   endif else begin
      if extlist[0] eq -1 then begin
         extlist=indgen(numext)+1
      endif else begin
         if max(extlist) gt numext then begin
            print,'ASTROM: Input extension list is incompatible with the number of extensions'
            print,'in the file.  This file had ',numext,' extensions and the largest item in'
            print,'your list is ',max(extlist)
            print,'Aborting.'
            return
         endif else if min(extlist) le 0 then begin
            print,'ASTROM: Input extension list is invalid.  You have one or more values less'
            print,'than or equal to zero.'
            print,'Aborting.'
            return
         endif
      endelse
   endelse
   numext=n_elements(extlist)

   for ext=0,numext-1 do begin
      goodsolution = 1
;print,'B goodsolution=',goodsolution

      if extlist[ext] eq 0 then begin
         astinf = 'astrom.inf'
         exttag = ''
         extstr = ''
      endif else begin
         extstr = strb36(extlist[ext])
         astinf = 'astrom'+extstr+'.inf'
         exttag = 'x'+extstr
         if plastfile ne 'none' then plastfile = '[[DEFAULT]]'
         starfile  = '[[DEFAULT]]'
      endelse

      ; Read in image, and some header values
      print,'Loading ',path+imfile+ft,' ',exttag
      raw=0.
;      raw=readfits(path+imfile,hdr,exten_no=extlist[ext],/silent,noscale=noscale)
      fits_read,path+imfile+ft,raw,hdr,exten_no=extlist[ext],noscale=noscale
      parsekey,hdr,hdrlist,hdrinfo
      jdstr,hdrinfo.jd,0,jds

      sz=size(raw)
      if (sz[1]/binfac)*binfac ne sz[1] or (sz[2]/binfac)*binfac ne sz[2] then $
         raw=raw[0:(sz[1]/binfac)*binfac-1,0:(sz[2]/binfac)*binfac-1]
      if in_objname[0] eq '[[DEFAULT]]' then $
         objname=nobname(strtrim(strcompress(hdrinfo.object),2)) $
      else $
         objname=in_objname
      objname=strlowcase(objname)
      fnobj = objname[0]+exttag+'.obj'
      exptime=hdrinfo.exptime
      if exptime eq 0. then exptime=1.0
      if badpar(in_xcenter,[0,2,3,4,5],[0,1],CALLER='ASTROM: (XCENTER) ', $
                   default=sz[1]/2.0) then return
      if badpar(in_ycenter,[0,2,3,4,5],[0,1],CALLER='ASTROM: (YCENTER) ', $
                   default=sz[2]/2.0) then return

      if extlist[ext] eq 0 then begin

         xcenter = in_xcenter[0]
         ycenter = in_ycenter[0]

      endif else begin

         if n_elements(in_xcenter) eq 1 then $
            xcenter=in_xcenter $
         else if extlist[ext] le n_elements(in_xcenter) then $
            xcenter = in_xcenter[extlist[ext]-1] $
         else begin
            print,'ASTROM: xcenter does not have enough entries, no value present for'
            print,'        extension',strcompress(extlist[ext]),' unable to continue.'
            return
         endelse

         if n_elements(in_ycenter) eq 1 then $
            ycenter=in_ycenter $
         else if extlist[ext] le n_elements(in_ycenter) then $
            ycenter = in_ycenter[extlist[ext]-1] $
         else begin
            print,'ASTROM: ycenter does not have enough entries, no value present for'
            print,'        extension',strcompress(extlist[ext]),' unable to continue.'
            return
         endelse

      endelse

      if hdrinfo.ra ge 0. then begin
         if hdrinfo.epoch NE 2000.0 then begin
            ra=hdrinfo.ra
            dec=hdrinfo.dec
            precess,ra,dec,hdrinfo.epoch,2000.0,/radian
            if ra lt 0.0 and hdrinfo.ra ge 0.0 then $
               ra = ra + 2.0d0*!dpi
            hdrinfo.ra=ra
            hdrinfo.dec=dec
            print,' *** Header coordinates precessed to J2000 from ', $
               strtrim(string(hdrinfo.epoch,format='(f10.2)'),2)
            hdrinfo.epoch=2000.0
         endif
      endif

      if starfile eq '[[DEFAULT]]' then begin
         starfile = objname[0] + exttag + twomass + '.cat'
      endif

      if plastfile eq '[[DEFAULT]]' then begin
         plastfile = objname[0] + exttag + '.pla'
      endif

      ; See if PLAST file exists and set a flag for future use.
      doplast = exists(plastfile) and plastfile ne 'none'

      ; Read in astrometry information saved from last run.  If not found,
      ;   ask for information.
      if exists(astinf) then begin
         version=''
         openr,lun,astinf,/get_lun
         readf,lun,version,format='(a)'
         if version ne 'ASTROM v1.0' then begin
            print,'Illegal astrom.inf file, version tag is wrong.'
            return
         endif
         readf,lun,raoff,decoff
         readf,lun,pscale0
         readf,lun,rang0
         xflip=1
         yflip=1
         readf,lun,xflip,yflip,format='(i2,1x,i2)'
         free_lun,lun
      endif else begin
         raoff  = 0.0
         decoff = 0.0
         read,pscale0,prompt='Initial plate scale guess (arcsec/pixel)  '
         read,rang0,prompt='Rotation angle (degrees)  '
         read,ans,prompt='Is the X-axis flipped with respect to the sky? '
         if strmid(ans,0,1) eq 'y' then xflip=-1 else xflip=1
         read,ans,prompt='Is the Y-axis flipped with respect to the sky? '
         if strmid(ans,0,1) eq 'y' then yflip=-1 else yflip=1

         openw,lun,astinf,/get_lun
         printf,lun,'ASTROM v1.0'
         printf,lun,raoff,' ',decoff
         printf,lun,pscale0
         printf,lun,rang0
         printf,lun,xflip,yflip,format='(i2,1x,i2)'
         free_lun,lun
         in_trustcenter=0
         goodsolution = 0
      endelse
;print,'C goodsolution=',goodsolution

      ; If starcat or plast file is needed, get a center to use for the
      ;   extraction.
      if not exists(starfile) or newcat or (not doplast and plastfile ne 'none') then begin

         ; First try to get the location of the object, this assumes that the object
         ;   was the intended target and the pointing was set to this object.  This
         ;   protects against a bad header value.  If object name does not yield a
         ;   valid position then we'll have to resort to other means to get the center.
         if do_objects and center eq 0 then begin
            ephem,hdrinfo.jd,obscode,2+50,'e'+objname[0],eph
            objra=eph[0]
            objdec=eph[1]
            rastr,objra,1,ras
            decstr,objdec,0,decs
            print,'Object location: ',ras,' ',decs
         endif else begin
            objra  = -99.9
            objdec = -99.0
         endelse

         if objra lt 0.0 and center eq 0 then begin
            if batch then begin
               countgood=0
               goto,next_extension
            endif
            rastr,hdrinfo.ra,1,ras
            decstr,hdrinfo.dec,0,decs
            print,' Header indicates ra,dec = ',ras,' ',decs,' @ ',jds
            rastr,hdrinfo.ra+raoff/cos(hdrinfo.dec),1,ras
            decstr,hdrinfo.dec+decoff,0,decs
            print,'   with offset -- ra,dec = ',ras,' ',decs,' @ ',jds
            read,ans,prompt='Use raw header (R), with offset (O), or manual (default)'
            ans=strlowcase(strmid(ans,0,1))
            if ans eq 'r' then begin
               objra  = hdrinfo.ra
               objdec = hdrinfo.dec
            endif else if ans eq 'o' then begin
               objra  = hdrinfo.ra+raoff/cos(hdrinfo.dec)
               objdec = hdrinfo.dec+decoff
            endif else begin
               read,ans,prompt='RA of image center  (J2000) ? ',format='(a)'
               objra=raparse(strcompress(ans,/remove_all))
               read,ans,prompt='Dec of image center (J2000) ? ',format='(a)'
               objdec=decparse(strcompress(ans,/remove_all))
            endelse
         endif else if center eq 0 then begin
            print,' *** Using nominal object position for star catalog extraction.'
         endif else if center eq 1 then begin
            print,' *** Using raw header for star catalog extraction.'
            objra  = hdrinfo.ra
            objdec = hdrinfo.dec
         endif else if center eq 2 then begin
            objra  = hdrinfo.ra+raoff/cos(hdrinfo.dec)
            objdec = hdrinfo.dec+decoff
            rastr,objra,1,ras
            decstr,objdec,0,decs
            print,' *** Using offset from header for star catalog extraction: ', $
                  ras,' ',decs
         endif else begin
            astrom_getctr,root+suffix+exttag,suffix+exttag,objra,objdec,found_center
            if not found_center then begin
               print,' *** Using offset from header for star catalog extraction.'
               objra  = hdrinfo.ra+raoff/cos(hdrinfo.dec)
               objdec = hdrinfo.dec+decoff
            endif else begin
               print,' *** Using known center for star catalog extraction.'
            endelse
         endelse

;         psize=fix(sqrt(sz[1]^2+sz[2]^2)*pscale0*1.05+0.5+catpad)

      endif
      psize=fix(sqrt(sz[1]^2+sz[2]^2)*pscale0*1.05+0.5+catpad)

      if not exists(starfile) or newcat then begin
         killrefstars=1
         print,'       --> running refnet....'
         refnet,objra,objdec,psize,psize,maglim,maglim,starfile,TWOMASS=twomass
         goodsolution = 0
      endif else begin
         killrefstars=0
      endelse
;print,'D goodsolution=',goodsolution

      killrefstars = killrefstars or in_killrefstars

      if not exists(starfile) then begin
         print,'ASTROM: The star catalog file '+starfile+ $
               ' does not exist.  Aborting'
         return
      endif

      if not doplast and plastfile ne 'none' then begin
         print,'       --> running plast....'
         plast,hdrinfo.jd,objra,objdec,psize,psize,plastfile,title=imfile,obscode=obscode
         doplast = exists(plastfile)
      endif

      ;Setup display image and coordinate system on display.
      if not nodisplay then begin
         dispim = 0.
         dispim = rebin(raw,sz[1]/binfac,sz[2]/binfac)
         setwin,window,xsize=fix(sz[1]/binfac),ysize=fix(sz[2]/binfac)
         skysclim,dispim,lowval,hival,meanval,sigma

         lowval = max([meanval-3*sigma,min(dispim)])
         hival  = min([meanval+8*sigma,max(dispim)])
         bim=0B
         bim = bytscl(dispim,min=lowval,max=hival,top=n_colors-1-savec)+savec
         if savec gt 0 then begin
            loadct,0,bottom=savec,/silent
            tvlct,red,gre,blu,/get
            ; 0 black
            ; 1 green
            ; 2 reddish
            ; 3 yellow
            ; 4 red
            red[0]=0
            gre[0]=0
            blu[0]=0
            red[1]=0
            gre[1]=255
            blu[1]=0
            red[2]=200
            gre[2]=50
            blu[2]=80
            red[3]=255
            gre[3]=255
            blu[3]=0
            red[4]=255
            gre[4]=0
            blu[4]=0
            color0 = 0
            color1 = 1
            color2 = 2
            color3 = 3
            color4 = 4
            tvlct,red,gre,blu
         endif else begin
            color0='000000'xl
            color1='00ff00'xl
            color2='5032c8'xl
            color3='00ffff'xl
            color4='0000ff'xl
         endelse
      endif

      if fullrefresh and not nodisplay then astrom_dim,window,bim,binfac,border

      if not clean or preclean then begin
         image=raw
      endif else begin
         print,'       --> cleaning cosmic rays from image....'
         acre,raw,image,5,4
         if saveclean then begin
            savename=root+'c'+suffix
            writefits,path+savename,image,hdr
         endif
      endelse

      if not nodisplay then begin
         dispim = rebin(image,sz[1]/binfac,sz[2]/binfac,sample=0)
         bim = bytscl(dispim,min=lowval,max=hival,top=n_colors-1-savec)+savec
      endif else begin
         bim=0  ; dummy value for structure when display turned off.
      endelse

      if fullrefresh and not nodisplay then astrom_dim,window,bim,binfac,border

      ; Setup a structure with commonly used image information
      dv = { $
         nx: sz[1], $
         ny: sz[2], $
         binfac: binfac, $
         win: window, $
         bim: bim, $
         border: border, $
         objrad: objrad, $
         gain: gain, $
         image: image, $
         exptime: exptime }

      ; Get plast information and collect the information for plotting
      if doplast then begin
         rdplast,plastfile,pastname,astcode,pastmag,numobj
         if numobj eq 0 then doplast=0
      endif

      if doplast then begin
         ephem,replicate(hdrinfo.jd,numobj),obscode,23+50,astcode,eph
         astra=eph[0,*]
         astdec=eph[1,*]
         astvra=eph[18,*]
         astvdec=eph[19,*]
         asterr=eph[20:21,*] < (2.0*psize)
         ephem,replicate(hdrinfo.jd+2.0/24.0d0,numobj),obscode,2+50,astcode,eph
         astrap1=eph[0,*]
         astdecp1=eph[1,*]
         astrate=sqrt((astrap1-astra)^2+(astdecp1-astdec)^2)*!radeg*3600.0/2.0
         astra=astra[*]
         astdec=astdec[*]
         astvra=astvra[*]
         astvdec=astvdec[*]
         astrap1=astrap1[*]
         astdecp1=astdecp1[*]
         c1=sqrt((astvra*cos(astdec))^2+astvdec^2)
         astvra=astvra*cos(astdec)/c1
         astvdec=astvdec/c1
         asterr=asterr/3600.0d0*!dpi/180.0d0
      endif

   ;===========================================
   ; This section is run for each sub-exposure.
      for ie=0,n_elements(subexp)-1 do begin

         if subexp[ie] eq '[[DEFAULT]]' then begin
            if fileno_type eq 0 then begin
               words=strsplit(root,'.',/extract)
               suff = words[n_elements(words)-1]+exttag
               ;suff = root+exttag
;print,'Ha suff=',suff
            endif else begin
               ; suff does NOT contain the dot
               if fileno lt 1000 then begin
                  suff = string(fileno,format='(i3.3)')
               endif else begin
                  suff = string(fileno,format='(i4.4)')
               endelse
               suff = suff+exttag
;               suff = suffix+exttag
;print,'Hb suff=',suff
            endelse
         endif else begin
            if fileno_type eq 0 then begin
               suff = root+exttag+'_'+subexp[ie]
;print,'Hc suff=',suff
            endif else begin
               if fileno lt 1000 then begin
                  suff = string(fileno,format='(i3.3)')
               endif else begin
                  suff = string(fileno,format='(i4.4)')
               endelse
               suff = suff+exttag+'_'+subexp[ie]
;print,'Hd suff=',suff
            endelse
            if ie ne 0 then print,''
            print,'Sub-exposure ',subexp[ie]
         endelse
;print,'H suff=',suff,' suffix=',suffix

         trustcenter=in_trustcenter

         cd,current=current
         if in_reffile eq '[[DEFAULT]]' then begin
            reffile=addslash(current)+addslash('Refstars')+suff+'.ref'
         endif else begin
            reffile=addslash(current)+addslash('Refstars')+in_reffile
         endelse
         if in_fnsrc eq '[[DEFAULT]]' then begin
            fnsrc = imfile+'.src'+exttag
         endif else begin
            fnsrc = in_fnsrc
         endelse
         if fileno_type eq 0 then begin
            thisexp=root
         endif else begin
            thisexp=root+suffix+exttag
;            thisexp=root+'.'+suff
         endelse
         if exists(reffile) and killrefstars then file_delete,reffile

         ; Look for image center from previous reduction.
         astrom_getctr,thisexp,suff,cra,cdec,found_center
;print,'G goodsolution=',goodsolution,' found_center=',found_center

         headercoord=0
         if not found_center then begin
            goodsolution = 0
            hra=hdrinfo.ra
            hdec=hdrinfo.dec
            if hra ge 0. then begin
               headercoord=1
            endif else begin
               if batch then begin
                  countgood=0
                  goto,next_extension
               endif
               read,ans,prompt='RA of image center  (J2000) ? ',format='(a)'
               hra=raparse(ans)
               read,ans,prompt='Dec of image center (J2000) ? ',format='(a)'
               hdec=decparse(ans)
               headercoord=0
               trustcenter=0
               cra=hra
               cdec=hdec
            endelse
            rastr,hra,1,hras
            decstr,hdec,0,hdecs
         endif
;print,'E goodsolution=',goodsolution

         ; display image
         if fullrefresh and not nodisplay then astrom_dim,window,bim,binfac,border

         first=1

         if headercoord then begin
            cra=hra+raoff/cos(hdec)
            cdec=hdec+decoff
            rastr,cra,1,ras
            decstr,cdec,0,decs
            print,' *** Using header coordinates for image center: ', $
                  ras,' ',decs
         endif

         ; xcref,ycref is almost always teh center of the actual image.
         ; When not, it is a temporary value for a one-star setup on the
         ; way to a solution in the interactive case.  This is needed to
         ; support the simple linear mapping and should not be used
         ; anywhere else.  racen,deccen are derived from the "current"
         ; solution and the value of xcref,ycref.
         linfo = { $
            ; center of image, used by this program only
            racen:  cra, $        ; RA of center of image
            deccen: cdec, $       ; Dec center of image
            xc:     sz[1]/2.0, $  ; X center of image
            yc:     sz[2]/2.0, $  ; Y center of image
            ; Scale description, linear only
            pscale: pscale0, $    ; Scale of image, arcsec/pixel
            rang:   rang0/!radeg, $ ; Rotation angle of image (radians).
            xflip:  xflip, $      ; 1 = no flip, -1 = X-axis flipped
            yflip:  yflip, $      ; 1 = no flip, -1 = Y-axis flipped
            raref:  cra, $        ; RA of xi,eta reference point.
            decref: cdec, $       ; Dec of xi,eta reference point.
            xcref:  sz[1]/2.0, $  ; X reference location
            ycref:  sz[2]/2.0, $  ; Y reference location
            ; Optical axis, used locally in this structure
            raopt:  cra, $        ; RA of optical axis (might not be cra)
            decopt: cdec, $       ; Dec of optical axis (might not be cdec)
            xcopt:  xcenter, $    ; X location of optical axis (might not be xc)
            ycopt:  ycenter }     ; Y location of optical axis (might not be yc)

         if not found_center then begin
            ; Compute the RA,Dec of the optical axis with linear approximation
            astxy2rd,linfo.xcopt,linfo.ycopt,linfo,raopt,decopt
            linfo.raopt  = raopt
            linfo.decopt = decopt
         endif

         ; Mark reminder spot
         if first and do_objects and not noremind then begin
            print,'     Click a spot to remember'
            cursor,xloc,yloc,3
            if !mouse.button eq 4 then return
            first=0
         endif
         if do_objects and not noremind then begin
            oplot,[-20,20,20,-20,-20]+xloc,[-20,-20,20,20,-20]+yloc,color=color4
            empty ; flush graphics buffer output
         endif

         if doplast then begin
            if numobj gt 0 then $
               print,'Asteroids found in plast support file that may be on image:'
            for i=0,numobj-1 do begin
               rastr,astra[i],1,str1
               decstr,astdec[i],0,str2
               if asterr[0,i]*!radeg*3600.0 gt (2.0*psize-0.1) then str3='>' else str3=' '
               print,i,pastname[i],str1,str2,str3,asterr[0,i]*!radeg*3600.0,'"', $
                  astrate[i],'"/hr  R=',pastmag[i], $
                  format='(i3,1x,a12,1x,a,1x,a,2x,a,f7.1,a,2x,f4.1,a,f4.1)'
            endfor
         endif

         ; At this point, we need to establish the coordinate transformation
         ;  from pixels to ra,dec.  There are three ways to get there.  All
         ;  three methods require have a list of (x,y) positions for a set of
         ;  catalog stars.
         ; (1) TWOSTAR: interactively identify the catalog <-> image matchup,
         ;     measure the catalog stars.
         ; (2) Auto from src: read an external list of sources and match to
         ;     the source catalog.
         ; (3) Refstars: read the correspondence list that was saved from the
         ;     last run.
         ; Once the list is in place we then proceed to fit for the
         ;  transformation.  #3 always takes precedence over any flags.  To
         ;  suppress this, the reference list must be deleted at the start
         ;  (use KILLREF).  If #3 isn't possible, then #2 is tried next unless
         ;  TWOSTAR has been specifically requested.  #1 is done as a last
         ;  resort.

         ; Avoid going any further if a good solution exists and the SKIPGOOD
         ;   keyword has been set.
         if exists(reffile) and goodsolution and skipgood then begin
            print,'SKIPGOOD set. Skipping frame, reduction is already ok.'
            countgood=0
            goto,next_extension
         endif

         ; Try to load the star measurements from the last run.
         if exists(reffile) then begin
            rdref,reffile,ref,referr
            if referr ne 0 then goto,next_extension
            nstars = ref.nstars
            sv_objrad = ref.objrad
            xpos = ref.xpos
            ypos = ref.ypos
            fwhm = ref.fwhm
            mag  = ref.mag
            err  = ref.err
            sig_max = ref.sig_max
            sra  = ref.ra
            sdec = ref.dec
            smag = ref.catmag

            if abs(sv_objrad-objrad) gt 0.1 and not nostrictaper then begin
               if batch then begin
                  countgood=0
                  goto,next_extension
               endif
               print,'The object aperture radius in the saved star data is ', $
                     sv_objrad
               print,'  You have requested an object radius of ',objrad
               print,'  To continue, the saved star data will be deleted and '+ $
                     'will be regenerated'
               print,'  with the new radius.  If this is not what you want to'+ $
                     ' do, then you should'
               print,'  quit and run again with OBJRAD=',sv_objrad
               print,''
               read,ans,prompt='Do you want to delete and continue (y/n) ?'
               if strmid(ans,0,1) ne 'y' then return
               print,'Deleting ',reffile
               file_delete,reffile
            endif else begin
               if abs(sv_objrad-objrad) gt 0.1 then $
                  print,'Warning, SV_OBJRAD <> OBJRAD. sv_objrad=', $
                        sv_objrad,' objrad=',objrad
               bad  = bytarr(nstars)
               z    = where(bad ne 1 and sig_max le maxphotsig)
            endelse
         endif

         ; If no old stars, we'll need to read in the raw catalog list.
         if not exists(reffile) then begin
            if twomass eq '' then begin
               ; Read in list of stars (this is the REFNET output format).
               print,'       --> reading star catalog [',starfile,']....'
               rdstarc,starfile,sra,sdec,dummy,smag,nfound
               if nfound eq 0 then begin
                  print,'ASTROM: Fatal error!  Star catalog ',starfile, $
                        ' did not return any stars.'
                  return
               endif
               bad=intarr(nfound)
               z=where(smag lt maglim,count)
               if count eq 0 then begin
                  print,'All catalog stars are fainter than the limit,',maglim
                  return
               endif
               sra=sra[z]
               sdec=sdec[z]
               smag=smag[z]
               bad=bad[z]
            endif else begin
               ; Read in list of stars (this is the REFNET2 output format).
               print,'       --> reading 2MASS star catalog....'
               readcol,starfile,in_ras,in_decs,smag, $
                  format='a,a,f',/silent
               bad=intarr(n_elements(smag))
               z=where(smag lt maglim or smag eq 0.,count)
               if count eq 0 then begin
                  print,'All catalog stars are fainter than the limit,',maglim
                  return
               endif
               sra=raparse(in_ras[z])
               sdec=decparse(in_decs[z])
               smag=smag[z]
               bad=bad[z]
            endelse

            ; compute star symbol sizes
            if maglim eq 30.0 then $
               ssz = (20.0-smag+1)/2 + 0.5 $
            else $
               ssz = (maglim-smag+1)/2 + 0.5
            z=where(ssz gt 4,count)
            if count ne 0 then ssz[z]=3.0
            z=where(ssz lt 0.5,count)
            if count ne 0 then ssz[z]=0.5

            ; Force xi,eta reference point to be center of image
            linfo.raref  = linfo.racen
            linfo.decref = linfo.deccen
            linfo.xcref  = linfo.xc
            linfo.ycref  = linfo.yc

            ; simple linear conversion
            astrd2xy,sra,sdec,linfo,sx,sy,XI=sxi,ETA=seta
         endif

         ; If no old stars, and there is a source list, use it.
         mantwostar=0
         if not exists(reffile) and exists(fnsrc) and ignoresrc eq 0 then begin
            print,'       --> reading source list [',fnsrc,']....'
            list=readfits(fnsrc,hdrsrc,/silent)
            gap=sxpar(hdrsrc,'GAP')
            if gap ne objrad then begin
               print,'Warning, GAP <> OBJRAD. gap=',gap,' objrad=',objrad
               if not nostrictaper then return
            endif
            nlist=n_elements(list)/6
            xpos=reform(list[*,0],nlist)
            ypos=reform(list[*,1],nlist)
            fwhm=reform(list[*,2],nlist)
            mag =reform(list[*,3],nlist)
            err =reform(list[*,4],nlist)
            sig_max=reform(raw[fix(xpos+0.5),fix(ypos+0.5)],nlist)
            zz=where(sig_max lt maxphotsig,nlist0)
            if nlist0 ne nlist then begin
               xpos=xpos[zz]
               ypos=ypos[zz]
               fwhm=fwhm[zz]
               mag =mag[zz]
               err = err[zz]
               sig_max=sig_max[zz]
               nlist=nlist0
            endif
            nlist0 = long(min([1.3*n_elements(sx),n_elements(xpos)]))
            if nlist0 ne nlist then begin
               zz = sort(mag)
               zz = zz[0:nlist0-1]
               xpos=xpos[zz]
               ypos=ypos[zz]
               fwhm=fwhm[zz]
               mag =mag[zz]
               err = err[zz]
               sig_max=sig_max[zz]
               nlist=nlist0
            endif
            rcrit = sqrt( n_elements(image)/!pi/float(nlist) )
            if autodr then begin
               drthresh = rcrit/2.0 > in_drthresh
            endif else begin
               drthresh = in_drthresh
            endelse
            print,' --------> Critical radius=',strn(rcrit,format='(f10.1)'), $
                     ',  drthresh=',strn(drthresh,format='(f10.1)'), $
                     ', objrad=',strn(objrad)

; This block handles automatically cross-referencing the catalog and source
;   list.  The two-star solution must be pretty good, especially in rotation
;   for this to work.
            if not twostar then begin

               if not nodisplay then begin
                  astrom_dim,window,bim,binfac,border
                  oplot,xpos,ypos,psym=5,color=color1,symsize=2.0
               endif

               print,'       --> determine source/catalog position offset....'

               ; Select a subset of stars if solution will be non-linear
               if doshells and ( maxrcrit lt 0 or (maxrcrit gt 0 and rcrit lt maxrcrit)) then begin

                  ;!!  The cross-correlation doesn't quite get the two-star
                  ;!!  solution right, so that regions of the reference stars
                  ;!!  (like in the corner) are missed in large numbers.  This
                  ;!!  is probably only important for images with appreciable
                  ;!!  non-linearities.  (eg., LONEOS, Mosaic).

                  rper = shells[0]
                  r = sqrt((xpos-linfo.xcopt)^2 + (ypos-linfo.ycopt)^2)
                  zr = sort(r)
                  nr = long(n_elements(r)*rper)
                  r2 = sqrt((sx-linfo.xcopt)^2 + (sy-linfo.ycopt)^2)
                  zr2 = where(r2 le max(r[zr[nr-1]])*1.01 and $
                              sx ge -linfo.xc*0.10  and $
                              sx le  linfo.xc*2.10  and $
                              sy ge -linfo.yc*0.10  and $
                              sy le  linfo.yc*2.10, count)
                  if count eq 0 then zr2 = lindgen(n_elements(sx))
                  print,'         > Using first ', $
                        strn(rper*100,format='(f10.1)'), $
                        ' percent of sources in radial extent'
                  print,'         > ', $
                        strn(n_elements(zr2)),' sources to correlate, ', $
                        strn(nlist),' in src list, ', $
                        strn(n_elements(sx)),' in cat list'
                  partial_solution = 1

               endif else begin

                  nr = n_elements(xpos)
                  zr = lindgen(nr)
                  zr2 = lindgen(n_elements(sx))

                  partial_solution = 0

               endelse


               frmdxdy,sx[zr2],sy[zr2],xpos[zr],ypos[zr], $
                   xoff,yoff,xyerror,nx=sz[1],ny=sz[2]
               print,'     shf > ',xoff,yoff,xyerror
               angoff=0.
               if xyerror gt 0 then begin
                  frmdxyr,sx[zr2],sy[zr2],xpos[zr],ypos[zr], $
                     -1.0*rotscan[0],rotscan[0],rotscan[1], $
                     xoff,yoff,angoff,xyerror,nx=sz[1],ny=sz[2], $
                     tolerance=rotscan[2],/silent
                  print,'     shfr> ',xoff,yoff,angoff,xyerror
               endif

            endif else begin
               nxc = 0
               nyc = 0
               xoff = 0
               yoff = 0
               partial_solution=0
               xyerror=0
               angoff=0.0
            endelse

            ; If the correlation didn't work do it interactively.
            if twostar or xyerror eq 1 then begin
               if not twostar then $
                  print,bel,'Warning!  auto correlation was not good, try manual.'
               if batch then begin
                  countgood=0
                  goto,next_extension
               endif
               if not twostar then $
                  logerror,errfile,'Bad auto correlation, file '+imfile+exttag
               if partial_solution then begin
                  astrom_twostar,dv,linfo,sra,sdec,sx,sy,ssz,valid
               endif else begin
                  astrom_twostar,dv,linfo,sra,sdec,sx,sy,ssz,valid
               endelse
               if not valid then begin
                  logerror,errfile,'Manual solution aborted, file '+imfile+exttag
                  return
               endif
               ; Redetermine ra and dec offset for "good" header coordinates.
               if headercoord then begin
                  raoff=linfo.racen-hra
                  if raoff gt      !pi then raoff=raoff-2.0d0*!dpi
                  if raoff lt -1.0*!pi then raoff=raoff+2.0d0*!dpi
                  raoff=raoff*cos(linfo.deccen)
                  decoff=linfo.deccen-hdec
                  print,'new raoff,decoff ',raoff,decoff
                  print,'new scale,angle ',linfo.pscale,linfo.rang
               endif
            endif else begin
               angle_r = angoff/!radeg
               old_sx = sx
               old_sy = sy
               sx =  (old_sx-sz[1]/2)*cos(angle_r) + $
                     (old_sy-sz[2]/2)*sin(angle_r) + $
                         sz[1]/2 + xoff
               sy = -(old_sx-sz[1]/2)*sin(angle_r) + $
                     (old_sy-sz[2]/2)*cos(angle_r) +  $
                         sz[2]/2 + yoff
            endelse

            if partial_solution then begin
               for ishell=0,n_elements(shells)-1 do begin
restart_shell:
                  rper=shells[ishell]
                  print,'       --> cross-link source list and catalog, pass ', $
                     strn(ishell+1),' (r<',strn(rper,format='(f10.2)'),')....'
                  nr = long(n_elements(r)*rper)
                  sidx=replicate(-1L,n_elements(sx))
                  tzr=zr[0:nr-1]
                  if not nodisplay then begin
                     astrom_dim,window,bim,binfac,border
                     ; plot the star catalog overlay
                     for i=0L,n_elements(sx)-1 do $
                        oplot,[sx[i]],[sy[i]],psym=4,color=color2,symsize=ssz[i]
                     ; plot the extracted source list
                     oplot,xpos[tzr],ypos[tzr],psym=5,color=color1,symsize=0.8
                  endif
                  for i=0L,n_elements(sx)-1 do begin
                     dr = (xpos[tzr]-sx[i])^2 + (ypos[tzr]-sy[i])^2
                     zt = where(dr eq min(dr) and dr lt drthresh^2)
                     zt=zt[0]
                     if zt ge 0 then sidx[i] = tzr[zt]
                     if zt ge 0 and not nodisplay then $
                        oplot,[xpos[sidx[i]]],[ypos[sidx[i]]],psym=4, $
                              color=color3,symsize=2.2
                  endfor
                  zy = where(sidx ne -1,nstars)
                  if nstars lt nterms then begin
                     logerror,errfile, $
                        ['Fatal error!  pass 1 not enough stars lined up.  Quitting on image', $
                         '   '+imfile+exttag]
                     countgood=0
                     goto,next_extension
                  endif

                  ; take short list of objects, collecting x,y and eta,xi and
                  ;   solve for the astrometric terms
                  renormfac=sqrt(double(sz[1])^2+double(sz[2])^2)
                  dx = (xpos[sidx[zy]]-linfo.xcopt)/renormfac
                  dy = (ypos[sidx[zy]]-linfo.ycopt)/renormfac
                  ; Convert from ra,dec to xi,eta
                  astrd2sn,sra,sdec,linfo.raopt,linfo.decopt,xi,eta,/ARCSEC
                  if ishell eq 0 or rper lt 0.35 then begin
                     tterms=['CONST','X','Y']
                  endif else if rper lt 0.75 then begin
                     tterms=['CONST','X','Y','XX','YY','XY']
                  endif else begin
                     tterms=terms
                  endelse
                  tbad=bytarr(n_elements(zy))
                  astsolve,dx,dy,xi[zy],eta[zy],tterms,renormfac, $
                     tbad,cxi,ceta,edit=edit,xflip=linfo.xflip ne 1, $
                     yflip=linfo.yflip ne 1,worstresid=worstresid, $
                     forceterms=forceterms,forcefilter=hdrinfo.filter
                  if worstresid gt residcap then begin
                     if batch or ishell ne 0 then begin
                        logerror,errfile, $
                           'Excessive errors in astrometric fit, quitting on '+$
                           imfile+exttag
                        countgood=0
                        goto,next_extension
                     endif else begin
                        logerror,errfile, $
                           'Excessive errors in astrometric fit, try a twostar correlation.'
                        astrom_twostar,dv,linfo,sra,sdec,sx,sy,ssz,valid
                        if not valid then begin
                           logerror,errfile,'Manual solution aborted, file '+imfile+exttag
                           countgood=0
                           goto,next_extension
                        endif else begin
                           astrd2xy,sra,sdec,linfo,sx,sy,XI=sxi,ETA=seta
                           goto,restart_shell
                        endelse
                     endelse

                  endif

                  ; Using terms, compute x,y from xi,eta for next shell of objects
                  ;   (including previously used objects).
                  tinfo={renormfac:renormfac,cxi:cxi,ceta:ceta,terms:tterms, $
                         xcref:linfo.xcopt,ycref:linfo.ycopt, $
                         raref:linfo.raopt,decref:linfo.decopt}
                  astsn2xy,xi,eta,tinfo,sx,sy,/arcsec,/full
                  if not nodisplay then begin
                     astrom_dim,window,bim,binfac,border
                     for i=0L,n_elements(sx)-1 do $
                        oplot,[sx[i]],[sy[i]],psym=4,color=color2,symsize=ssz[i]
                     oplot,sx[zy],sy[zy],psym=5,color=color1,symsize=0.8
                  endif

               endfor

               ; Correlate final list against source list.  Leave behind
               ;   the list correlation for the fall through processing to
               ;   get the final list.
               print,'       --> cross-link source list and catalog, final pass....'
               sidx=replicate(-1L,n_elements(sx))
               if not nodisplay then begin
                  astrom_dim,window,bim,binfac,border
                  for i=0L,n_elements(sx)-1 do $
                     oplot,[sx[i]],[sy[i]],psym=4,color=color2,symsize=ssz[i]
                  oplot,xpos,ypos,psym=5,color=color1,symsize=0.8
               endif

               for i=0L,n_elements(sx)-1 do begin
                  dr = (xpos-sx[i])^2 + (ypos-sy[i])^2
                  zt = where(dr eq min(dr) and dr lt drthresh^2)
                  sidx[i] = zt[0]
                  if zt[0] ne -1 and not nodisplay then $
                     oplot,[xpos[zt[0]]],[ypos[zt[0]]],psym=4,color=color3,symsize=2.2 $
                  else begin
                     zt = where(dr eq min(dr) and dr lt (drthresh^2)*4)
                     if zt[0] ne -1 and not nodisplay then begin
                        oplot,[xpos[zt[0]]],[ypos[zt[0]]],psym=4,color=color3,symsize=5
                     endif
                  endelse
               endfor
               zy = where(sidx ne -1,nstars)
               if nstars lt nterms then begin
                  logerror,errfile, $
                     ['Fatal error!  final pass not enough stars lined up.  Quitting on image', $
                      '   '+imfile+exttag]
                  countgood=0
                  goto,next_extension
               endif
            endif else begin

               print,'       --> cross-link source list and catalog....'
               sidx=replicate(-1L,n_elements(sx))
               if not nodisplay then begin
                  astrom_dim,window,bim,binfac,border
                  for i=0L,n_elements(sx)-1 do $
                     oplot,[sx[i]],[sy[i]],psym=4,color=color2,symsize=ssz[i]
                  oplot,xpos,ypos,psym=5,color=color1,symsize=0.8
               endif

               for i=0L,n_elements(sx)-1 do begin
                  dr = (xpos-sx[i])^2 + (ypos-sy[i])^2
                  zt = where(dr eq min(dr) and dr lt drthresh^2)
                  sidx[i] = zt[0]
                  if zt[0] ne -1 and not nodisplay then $
                     oplot,[xpos[zt[0]]],[ypos[zt[0]]],psym=4,color=color3,symsize=2.2 $
                  else begin
                     zt = where(dr eq min(dr) and dr lt (drthresh^2)*4)
                     if zt[0] ne -1 and not nodisplay then begin
                        oplot,[xpos[zt[0]]],[ypos[zt[0]]],psym=4,color=color3,symsize=5
                     endif
                  endelse
               endfor
               zy = where(sidx ne -1,nstars)
               if nstars lt nterms then begin
                  logerror,errfile, $
                     ['Fatal error!  not enough stars lined up.  Quitting on image', $
                      '   '+imfile+exttag]
                  countgood=0
                  goto,next_extension
               endif
            endelse

            oldx = sx
            oldy = sy
            oldssz = ssz
            oldxpos = xpos
            oldypos = ypos
            oldfwhm = fwhm
            oldmag  = mag
            olderr  = err
            oldsig_max = sig_max
            oldsra  = sra
            oldsdec = sdec
            oldsmag = smag

            sx = sx[zy]
            sy = sy[zy]
            ssz = ssz[zy]

            xpos = xpos[sidx[zy]]
            ypos = ypos[sidx[zy]]
            fwhm = fwhm[sidx[zy]]
            mag  = mag[sidx[zy]]
            err  = err[sidx[zy]]
            sig_max = sig_max[sidx[zy]]
            sra  = sra[zy]
            sdec = sdec[zy]
            smag = smag[zy]
            bad  = intarr(nstars)
            if not nodisplay then oplot,xpos,ypos,psym=4,color=color3,symsize=3.2
            z = where(bad ne 1)

            ; Compute a coverage statistic
            xq = fix(3*xpos/float(dv.nx))
            yq = fix(3*ypos/float(dv.ny))
            qu = xq + yq*3
            qhist = histogram(qu,min=0,max=8)
            qmed  = median(qhist)
            zq=where(qhist lt qmed/2 or qhist eq 0,countzq)
            if countzq ne 0 then begin
               print,bel
               logerror,errfile,'Warning! poor star coverage on'+ $
                  strcompress(countzq)+' enneanants:  '+imfile+exttag
            endif

            if countzq gt maxbadenne then begin
               if batch then begin
                  logerror,errfile,'Batch processing on '+imfile+exttag+' aborted.'
                  countgood=0
                  goto,next_extension
               endif
               read,ans,prompt='Do you want to continue (default=n, twostar=t)? '
               if strmid(ans,0,1) eq 't' then begin
                  mantwostar=1
                  sx   = oldx   
                  sy   = oldy   
                  ssz  = oldssz 
                  xpos = oldxpos
                  ypos = oldypos
                  fwhm = oldfwhm
                  mag  = oldmag 
                  err  = olderr 
                  sig_max = oldsig_max
                  sra  = oldsra 
                  sdec = oldsdec
                  smag = oldsmag
                  nstars = n_elements(xpos)
                  bad  = intarr(nstars)
                  z = where(bad ne 1)
                  astrom_twostar,dv,linfo,sra,sdec,sx,sy,ssz,valid
                  if not valid then begin
                     countgood=0
                     goto,next_extension
                  endif
                  ; Redetermine ra and dec offset for "good" header coordinates.
                  if headercoord then begin
                     raoff=linfo.racen-hra
                     if raoff gt      !pi then raoff=raoff-2.0d0*!dpi
                     if raoff lt -1.0*!pi then raoff=raoff+2.0d0*!dpi
                     raoff=raoff*cos(linfo.deccen)
                     decoff=linfo.deccen-hdec
                  endif
               endif else if strmid(ans,0,1) ne 'y' then begin
                  countgood=0
                  goto,next_extension
               endif
            endif

         endif

;         ; need to regenerate some of the info structure information
;         ;   for the sn2xy transformation if using source lists.
;         if (exists(reffile) or (exists(fnsrc) and ignoresrc ne 1)) and $
;               not mantwostar then begin
;            ; first select two stars to setup a good two-star solution
;            idx = sort(xpos[z]^2+ypos[z]^2)
;            loptr=fix(0.1*n_elements(idx))
;            hiptr=fix(0.9*n_elements(idx))
;            il = max([(loptr-5),0]) - loptr
;            ih = min([(hiptr+5),n_elements(idx)-1]) - hiptr
;            angs=replicate(1000.0,ih-il+1)
;;print,'entry ',linfo.rang*!radeg,linfo.pscale
;            for ii=il,ih do begin
;               zs1 = z[idx[loptr+ii]]
;               zs2 = z[idx[hiptr+ii]]
;               if zs1 ge 0 and zs1 lt n_elements(xpos) and $
;                  zs2 ge 0 and zs2 lt n_elements(xpos) then begin
;                  astrom_regen,xpos,ypos,sra,sdec,zs1,zs2,linfo
;                  angs[ii-il]=linfo.rang
;               endif
;            endfor
;            gangs=angs[where(angs lt 500.0)]
;            zit=where(angs eq median(gangs))
;;print,angs[sort(angs)]*!radeg
;
;            zs1 = z[idx[fix(0.15*n_elements(idx))+zit[0]+il]]
;            zs2 = z[idx[fix(0.85*n_elements(idx))+zit[0]+il]]
;            astrom_regen,xpos,ypos,sra,sdec,zs1,zs2,linfo
;;print,'end   ',linfo.rang*!radeg,linfo.pscale
;
;         endif

         ; No reference star file and no source list  (usually input /twostar)
         if exists(reffile) ne 1 and (exists(fnsrc) ne 1 or ignoresrc) then begin

      retry:

            ; Force xi,eta reference point to be center of image
            linfo.raref  = linfo.racen
            linfo.decref = linfo.deccen
            linfo.xcref  = linfo.xc
            linfo.ycref  = linfo.yc

            ; simple linear conversion
            astrd2xy,sra,sdec,linfo,sx,sy,XI=sxi,ETA=seta

            z = where(sx gt border[0] and sx lt sz[1]-border[1] and $
                      sy gt border[3] and sy lt sz[2]-border[2],count)
            rastr,linfo.racen,1,ras1
            decstr,linfo.deccen,0,decs1
            print,' Center used: ',ras1,' ',decs1
            if count eq 0 then begin
               rastr,hdrinfo.ra,1,ras
               decstr,hdrinfo.dec,0,decs
               print,' Header indicates ra,dec = ',ras,' ',decs,' @ ',jds
               print,'No stars on frame.'
               if batch then begin
                  countgood=0
                  goto,next_extension
               endif
               read,ans,prompt='Do you want to (re)enter a corrected center? '
               if strmid(ans,0,1) eq 'n' then return
               read,ans,prompt='RA of image center? ',format='(a)'
               hra=raparse(ans)
               read,ans,prompt='Dec of image center? ',format='(a)'
               hdec=decparse(ans)
               linfo.racen=hra
               linfo.deccen=hdec
               headercoord=0
               trustcenter=0
               goto,retry
            endif

            ; Do the catalog/field correlation for non-trusted center
            if nodisplay then begin
               print,'Source lists not found, cannot proceed in batch/nodisplay mode.'
               countgood=0
               goto,next_extension
            endif
            if not trustcenter then begin
               astrom_twostar,dv,linfo,sra,sdec,sx,sy,ssz,valid
               if not valid then return
               ; Redetermine ra and dec offset for "good" header coordinates.
               if headercoord then begin
                  raoff=linfo.racen-hra
                  if raoff gt      !pi then raoff=raoff-2.0d0*!dpi
                  if raoff lt -1.0*!pi then raoff=raoff+2.0d0*!dpi
                  raoff=raoff*cos(linfo.deccen)
                  decoff=linfo.deccen-hdec
               endif
            endif ; end of catalog/field correlation for non-trusted center

            if fullrefresh and not nodisplay then begin
               astrom_dim,window,bim,binfac,border
               if do_objects and not noremind then $
                  oplot,[-20,20,20,-20,-20]+xloc,[-20,-20,20,20,-20]+yloc,color=color4
            endif
            ; simple linear conversion
            astrd2xy,sra,sdec,linfo,sx,sy
            if not nodisplay then begin
               for i=0L,n_elements(sx)-1 do $
                  oplot,[sx[i]],[sy[i]],psym=4,color=color2,symsize=ssz[i]
            endif
            ; Select those stars on the frame
            nstars=n_elements(sra)
            xpos=fltarr(nstars)
            ypos=xpos
            perr=ypos+9999.0
            mag=perr
            err=perr
            fwhm=perr
            sig_max=fltarr(nstars)

            z = where(sx le border[0] or sx ge sz[1]-border[1] or $
                      sy le border[3] or sy ge sz[2]-border[2],count)
            if count ne 0  then bad[z]=1

            z = where(sx gt border[0] and sx lt sz[1]-border[1] and $
                      sy gt border[3] and sy lt sz[2]-border[2] and bad eq 0,count)
            ; only proceed if there appear to be stars on the frame
            if count ne 0 then begin

               ; Check to see if the number of stars is being limited for
               ;   constraining the solution.  If they are to be limited, then
               ;   an extra temporary variable is setup (zz).
               if maxstars gt 0 then begin
                  zz=z
                  numtodo=maxstars
               endif else numtodo=count

               ; If maximum allowed is more than number present, reset so
               ;   that we do them all.
               if numtodo gt count then numtodo=count

               ; Loop over all the stars we want to use (numtodo).
               for i=0L,numtodo-1 do begin

                  ; If we're doing all the stars, just take the next one in the
                  ;   z index array.  Set the relevant index to iz for later.
                  if numtodo eq count then begin
                     iz=z[i]

                  ; Otherwise, pick a random element from zz (this a list that
                  ;   starts out the same as z but gets depleted for each pass
                  ;   through the loop).  The index gets set to iz.
                   endif else begin
                     i1=0
                     i4=n_elements(zz)-1
                     is=fix(randomu(seed)*(i4+1))
                     i2=is-1
                     i3=is+1
                     iz=zz[is]
                     if i eq 0 then begin
                        z=iz
                     endif else begin
                        z=[z,iz]
                     endelse
                     if i1 gt i2 then begin
                        zz=zz[i3:i4]
                     endif else if i3 gt i4 then begin
                        zz=zz[i1:i2]
                     endif else begin
                        zz=[zz[i1:i2],zz[i3:i4]]
                     endelse
                  endelse

                  ; Do photometry on the selected catalog star.  Use its
                  ;   predicted location on the image as the starting point
                  ;   for the photometry.  This may or may not be the right
                  ;   spot but we'll sort this out later.
                  basphote,gain,image,exptime,sx[iz],sy[iz], $
                     objrad,objrad+10,objrad+30,/nolog,/silent, $
                     xcen=xm,ycen=ym,fwhm=fwhm0,mag=mag0,err=err0, $
                     max=max0,boxmrad=max([5,objrad])

;photographic refinement

                  ; Save the photometry results to master arrays for later use.
                  xpos[iz]=xm
                  ypos[iz]=ym
                  perr[iz]= sqrt((xm-sx[iz])^2 + (ym-sy[iz])^2)*linfo.pscale
                  fwhm[iz]=fwhm0
                  mag[iz]=mag0
                  err[iz]=err0
                  sig_max[iz]=max0
                  rastr,sra[iz],1,ras
                  decstr,sdec[iz],0,decs
                  if not nodisplay then begin
                     setusym,-1
                     oplot,[xpos[iz]],[ypos[iz]],psym=8,color=color3,symsize=2.5
                     setusym,1
                     empty ; flush graphics buffer output
                  endif
               endfor ; end of star measuring loop.
               if numtodo ne count then count=numtodo
               if not nodisplay then oplot,sx[z],sy[z],psym=4,color=color2
            endif else begin
               print,'No stars on frame, unable to continue.'
               countgood=0
               goto,next_extension
            endelse

            ; Save the catalog star measurements to a binary file.
            astrom_svref,reffile,objrad,count,xpos[z],ypos[z],fwhm[z], $
               mag[z],err[z],sig_max[z],sra[z],sdec[z],smag[z]
            gap=objrad
         endif ; end block for measuring star network with
               ;   no reference star file and no source list

         ; Convert from ra,dec to xi,eta
         astrd2sn,sra,sdec,linfo.raopt,linfo.decopt,xi,eta,/ARCSEC

         robomean,fwhm[z],3.0,0.5,meanfwhm,avgdev,fwhmstddev
         print,'Seeing --> (FWHM) ',meanfwhm,' +/- ', $
               fwhmstddev,meanfwhm*linfo.pscale, $
               format='(a,f4.1,a,f3.1," pixels  ",f4.1," arcsec")'
         print,' '

         ; Exclude objects with discrepant sizes.
         zz=where(abs(fwhm - meanfwhm) gt 3.0*fwhmstddev or fwhm le 0.7,countzz)
         if countzz ne 0 then bad[zz]=1

         ; Exclude excessivly faint objects.
         zz=where(abs(mag gt 80.0) or err gt 0.1,countzz)
         if countzz ne 0 then bad[zz]=1

         ; Exclude saturated objects.
         zz=where(sig_max gt maxphotsig,countzz)
         if countzz ne 0 then bad[zz]=1

         renormfac=sqrt(double(sz[1])^2+double(sz[2])^2)
         dx = (xpos-linfo.xcopt)/renormfac
         dy = (ypos-linfo.ycopt)/renormfac
         astsolve,dx,dy,xi,eta,terms,renormfac,bad,cxi,ceta, $
            edit=edit,xflip=linfo.xflip ne 1,yflip=linfo.yflip ne 1, $
            worstresid=worstresid,xiscat=xiscat,etascat=etascat, $
            forceterms=forceterms,forcefilter=hdrinfo.filter
         if worstresid gt residcap then begin
            logerror,errfile, $
               'Excessive errors in astrometric fit, quitting on '+imfile+exttag
            countgood=0
            magdiff = 0.
            xiscat  = 0.
            etascat = 0.
            goto,next_extension
         endif

         zg = where(bad eq 0,countgood)

         if extlist[ext] eq 0 then begin
            fitdata = { $
               renormfac: renormfac, $
               cxi:       cxi, $ ;
               ceta:      ceta, $ ;
               prot:      0.0d0, $
               terms:     terms, $ ;
               xcref:     linfo.xcopt, $
               ycref:     linfo.ycopt, $
               raref:     linfo.raopt, $ ; 
               decref:    linfo.decopt, $ ; 
               nstars:    countgood, $ ; number of stars used in the fit
               xi_scat:   xiscat,  $ ; 
               eta_scat:  etascat,  $ ; 
               photzp:    0.0  $ ; 
               }
            ; Compute the ra,dec of the optical axis
            astxy2rd,linfo.xcopt,linfo.ycopt,fitdata,newracen,newdeccen,/full
         endif else begin
            if ext eq 0 then begin
               fitdata = replicate({ $
                  renormfac: renormfac, $
                  cxi:       cxi, $ ;
                  ceta:      ceta, $ ;
                  prot:      0.0d0, $
                  terms:     terms, $ ;
                  xcref:     linfo.xcopt, $
                  ycref:     linfo.ycopt, $
                  raref:     linfo.raopt, $ ; 
                  decref:    linfo.decopt, $ ; 
                  nstars:    countgood, $ ; number of stars used in the fit
                  xi_scat:   xiscat,  $ ; 
                  eta_scat:  etascat,  $ ; 
                  photzp:    0.0  $ ; 
                  },numext)
            endif else begin
               fitdata[ext].renormfac = 0.0d0
               fitdata[ext].cxi       = cxi
               fitdata[ext].ceta      = ceta
               fitdata[ext].prot      = 0.0d0
               fitdata[ext].terms     = terms
               fitdata[ext].xcref     = linfo.xcopt
               fitdata[ext].ycref     = linfo.ycopt
               fitdata[ext].raref     = linfo.raopt
               fitdata[ext].decref    = linfo.decopt
               fitdata[ext].nstars    = countgood
               fitdata[ext].xi_scat   = xiscat
               fitdata[ext].eta_scat  = etascat
               fitdata[ext].photzp    = 0.0
            endelse
         endelse

         dx = (xpos[zg]-linfo.xcopt)/renormfac
         dy = (ypos[zg]-linfo.ycopt)/renormfac
         xfit = asteval(dx,dy,cxi,terms)
         efit = asteval(dx,dy,ceta,terms)

         if not nodisplay then begin
            setwin,10,xsize=400,ysize=800
            !p.multi=[0,1,7]
            ytitle='Xi resid (arcsec)'
            cs=1.5
            plot,dx*renormfac,xi[zg]-xfit,psym=7,symsize=0.5, $
                 xtitle='dx (pixels)',ytitle=ytitle,charsize=cs
            plot,dy*renormfac,xi[zg]-xfit,psym=7,symsize=0.5, $
                 xtitle='dy (pixels)',ytitle=ytitle,charsize=cs
            plot,sqrt(dx^2+dy^2)*renormfac,xi[zg]-xfit,psym=7,symsize=0.5, $
                 xtitle='r (pixels)',ytitle=ytitle,charsize=cs
            plot,(dx*renormfac)^2,xi[zg]-xfit,psym=7,symsize=0.5, $
                 xtitle='dx*dx (pixels)',ytitle=ytitle,charsize=cs
            plot,(dy*renormfac)^2,xi[zg]-xfit,psym=7,symsize=0.5, $
                 xtitle='dy*dy (pixels)',ytitle=ytitle,charsize=cs
            plot,mag[zg],xi[zg]-xfit,psym=7,symsize=0.5, $
                 xtitle='Magnitude',ytitle=ytitle,charsize=cs
            plot,fwhm[zg],xi[zg]-xfit,psym=7,symsize=0.5, $
                 xtitle='fwhm (pixels)',ytitle=ytitle,charsize=cs
         endif

         if not nodisplay then begin
            ytitle='Eta resid (arcsec)'
            cs=1.5
            setwin,11,xsize=400,ysize=800
            plot,dx*renormfac,eta[zg]-efit,psym=7,symsize=0.5, $
                 xtitle='dx (pixels)',ytitle=ytitle,charsize=cs
            plot,dy*renormfac,eta[zg]-efit,psym=7,symsize=0.5, $
                 xtitle='dy (pixels)',ytitle=ytitle,charsize=cs
            plot,sqrt(dx^2+dy^2)*renormfac,eta[zg]-efit,psym=7,symsize=0.5, $
                 xtitle='r (pixels)',ytitle=ytitle,charsize=cs
            plot,(dx*renormfac)^2,eta[zg]-efit,psym=7,symsize=0.5, $
                 xtitle='dx*dx (pixels)',ytitle=ytitle,charsize=cs
            plot,(dy*renormfac)^2,eta[zg]-efit,psym=7,symsize=0.5, $
                 xtitle='dy*dy (pixels)',ytitle=ytitle,charsize=cs
            plot,mag[zg],eta[zg]-efit,psym=7,symsize=0.5, $
                 xtitle='Magnitude',ytitle=ytitle,charsize=cs
            plot,fwhm[zg],eta[zg]-efit,psym=7,symsize=0.5, $
                 xtitle='fwhm (pixels)',ytitle=ytitle,charsize=cs
            !p.multi=0
         endif

         if fullrefresh and not nodisplay then begin
            setwin,14
            plot,xpos[zg],ypos[zg],psym=8,symsize=0.5, $
               xtitle='X (pixels)',ytitle='Y (pixels)', $
               title='Xi (x-dir) and Eta (y-dir) error plot'
            sfac=200.0/max([xi[zg]-xfit,eta[zg]-efit])
            for i=0L,n_elements(zg)-1 do begin
               oplot,xpos[zg[i]]+sfac*[0.,xi[zg[i]]-xfit[i]], $
                     ypos[zg[i]]+sfac*[0.,eta[zg[i]]-efit[i]]
            endfor
         endif

         if doplast then begin
;            astrd2xy,astra,astdec,linfo,astx,asty
            astrd2xy,astra,astdec,fitdata[ext],astx,asty,/full
            astrd2xy,astra+astvra*asterr[0,*]/cos(astdec), $
                     astdec+astvdec*asterr[0,*], $
                     linfo,astvxp,astvyp
            astrd2xy,astra-astvra*asterr[0,*]/cos(astdec), $
                     astdec-astvdec*asterr[0,*], $
                     linfo,astvxm,astvym
;            astrd2xy,astrap1,astdecp1,linfo,astxp1,astyp1
            astrd2xy,astrap1,astdecp1,fitdata[ext],astxp1,astyp1,/full
            astxvar=[[astvxm],[astx],[astvxp]]
            astyvar=[[astvym],[asty],[astvyp]]
         endif

;read,ans,prompt='continue -->'
         if not nodisplay then begin
            astrom_dim,window,bim,binfac,border
            if do_objects and not noremind then $
               oplot,[-20,20,20,-20,-20]+xloc,[-20,-20,20,20,-20]+yloc,color=color4
            if doplast then begin
;               oplot,astx,asty,psym=8,color=color1,symsize=0.2
               for i=0,numobj-1 do begin
                  xyouts,astx[i],asty[i],' '+strtrim(string(i),2),color=color1
                  if asterr[0,i]*!radeg*3600.0/linfo.pscale gt 5.0 then begin
                     oplot,astxvar[i,*],astyvar[i,*],color=color1
                     oplot,[astx[i],astxp1[i]],[asty[i],astyp1[i]],color=color3
                  endif else begin
                     oplot,[astx[i]],[asty[i]],psym=4,color=color1,symsize=2.0
                     if asty[i] gt astyp1[i] then $
                        yoffset = -6*binfac $
                     else $
                        yoffset = 6*binfac
                     oplot,[astx[i],astxp1[i]], $
                           [asty[i],astyp1[i]]+yoffset,color=color3
                  endelse
               endfor
            endif
         endif

         if not nodisplay then begin
            setusym,-1
            for i=0L,nstars-1 do begin
               if bad[i] eq 0 then begin
                  oplot,[xpos[i]],[ypos[i]],psym=8,color=color3,symsize=2.5
               endif else begin
                  oplot,[xpos[i]],[ypos[i]],psym=8,color=color2,symsize=2.5
               endelse
            endfor
         endif

         zok=where((sig_max le maxphotsig) and (bad eq 0) and $
                   (err lt 0.15),countok)
         if countok gt 0 then begin
            robomean,smag[zok]-mag[zok],3.0,0.5,magdiff,avgdev,stddev
            zok1=where(sig_max le maxphotsig and bad eq 0 and $
                      abs(smag-mag-magdiff) lt 2.0*stddev,countok1)
            meanerr,smag[zok1]-mag[zok1],err[zok1],magdiff,avgdev,stddev
            print,' '
            print,'Mean catalog magnitude difference ',magdiff, $
                  stddev/sqrt(countok1-1),'from',strn(countok1),'stars', $
                  format='(a,f7.3," +/- ",f6.3,1x,a,1x,a,1x,a)'
            if not nodisplay then begin
               setwin,15
               if twomass eq '' then begin
                  xtitle='USNO catalog red magnitude'
               endif else begin
                  xtitle='2MASS catalog '+twomass+' magnitude'
               endelse
               plot,smag[zok],mag[zok],psym=4, $
                  xtitle=xtitle,ytitle='Instrument magnitude'
               oplot,smag[zok1],mag[zok1],psym=4,color=color1
            endif
            fwhmrange=minmax(fwhm[zok])
            zok=where((sig_max le maxphotsig) and (err lt 0.15) and $
                      fwhm ge fwhmrange[0] and fwhm le fwhmrange[1],countok)
         endif else begin
            magdiff = 0.0
            print,'Warning: no stars in valid photometric range, magdiff set to 0.'
            print,'Range in peak counts: ',minmax(sig_max[where(bad eq 0)])
         endelse
         fitdata[ext].photzp = magdiff

            ; Save the catalog star measurements to a binary file.
;         if exists(reffile) ne 1 or twostar then begin
         if not exists(reffile) then begin
            astrom_svref,reffile,gap,countgood,xpos[zg],ypos[zg],fwhm[zg], $
               mag[zg],err[zg],sig_max[zg],sra[zg],sdec[zg],smag[zg]
         endif

         ; Save 2-star solution for next run.
         openw,lun,astinf,/get_lun
         printf,lun,'ASTROM v1.0'
         printf,lun,raoff,' ',decoff
         printf,lun,linfo.pscale
         printf,lun,linfo.rang*!radeg
         printf,lun,linfo.xflip,linfo.yflip,format='(i2,1x,i2)'
         free_lun,lun

         ; Save xi and eta coefficients to the master file.

         if exists('fitcoeff.dat') then begin
            ; promotes file type version of fitcoeff.dat
            astprmt,renormfac=renormfac

            ; read the current file
            rdastfc,'fitcoeff.dat',t_ffn,t_ftype,t_xc,t_yc,t_prot,t_renormfac, $
                                   t_cra, t_cdec,t_photzp,t_terms,t_coeffarr, $
                                   t_ncoeffs,t_nlines,ERROR=error
            if error then begin
               print,self,'Error reading the fitcoeff.dat file, Aborting.'
               return
            endif
         endif else begin
            ; creates a dummy place for this image to go.
            t_ffn=replicate(thisexp,2)
            t_ftype=['eta','xi']
            t_xc=dblarr(2)
            t_yc=dblarr(2)
            t_prot=dblarr(2)
            t_renormfac=dblarr(2)
            t_cra=dblarr(2)
            t_cdec=dblarr(2)
            t_photzp=dblarr(2)
            t_terms=terms
            t_coeffarr=dblarr(2,nterms)
            t_ncoeffs=nterms
            t_nlines=2
         endelse

         ; find slot for current image to overwrite
         ze=trimrank(where(t_ffn eq thisexp and t_ftype eq 'eta'))
         zx=trimrank(where(t_ffn eq thisexp and t_ftype eq 'xi'))

         ; if not found then add it in at the end
         if ze eq -1 and zx eq -1 then begin
            t_ffn       = [t_ffn,       thisexp, thisexp]
            t_ftype     = [t_ftype,     'eta','xi']
            t_xc        = [t_xc,        linfo.xcopt,linfo.xcopt]
            t_yc        = [t_yc,        linfo.ycopt,linfo.ycopt]
            t_prot      = [t_prot,      0.0,0.0]
            t_renormfac = [t_renormfac, renormfac,renormfac]
            t_cra       = [t_cra,       linfo.raopt,linfo.raopt]
            t_cdec      = [t_cdec,      linfo.decopt,linfo.decopt]
            t_photzp    = [t_photzp,    magdiff,magdiff]
            t_coeffarr  = [t_coeffarr,  transpose([[ceta],[cxi]])]
            t_nlines    = t_nlines+2
         endif else if ze ge 0 and zx ge 0 then begin
            t_xc[ze]         = linfo.xcopt
            t_yc[ze]         = linfo.ycopt
            t_prot[ze]       = 0.0
            t_renormfac[ze]  = renormfac
            t_cra[ze]        = linfo.raopt
            t_cdec[ze]       = linfo.decopt
            t_photzp[ze]     = magdiff
            t_coeffarr[ze,*] = ceta
            t_xc[zx]         = linfo.xcopt
            t_yc[zx]         = linfo.ycopt
            t_prot[zx]       = 0.0
            t_renormfac[zx]  = renormfac
            t_cra[zx]        = linfo.raopt
            t_cdec[zx]       = linfo.decopt
            t_photzp[zx]     = magdiff
            t_coeffarr[zx,*] = cxi
         endif else begin
            print,self,'Error!  Corrupted fitcoeff.dat file.  Aborting.'
            return
         endelse

         ; sort
         zsort=sort(t_ffn+' '+t_ftype)

         ; save to file
         wrastfc,'fitcoeff.dat',t_ffn[zsort],t_ftype[zsort], $
                 t_xc[zsort],t_yc[zsort],t_prot[zsort],t_renormfac[zsort], $
                 t_cra[zsort],t_cdec[zsort],t_photzp[zsort], $
                 t_terms,t_coeffarr[zsort,*]

         if photstars and countok gt 0 then begin
            if fullrefresh and not nodisplay then begin
               astrom_dim,window,bim,binfac,border
               setusym,-1
               for i=0L,countok-1 do begin
                  oplot,[xpos[zok[i]]],[ypos[zok[i]]], $
                        psym=8,color=color3,symsize=2.5
                  xyouts,xpos[zok[i]],ypos[zok[i]], $
                        '  '+strtrim(string(i),2),color=color3
               endfor
               setusym,1
            endif

            hardim,bytscl(image,min=lowval,max=hival,top=255),0,255, $
               title=imfile+' '+objname[0],file=imfile+'.ps', $
               autosize=1,queue=queue,/negative,width=18.0,/noprint,/noclose
            setusym,-1
            for i=0L,countok-1 do begin
               oplot,[xpos[zok[i]]],[ypos[zok[i]]],psym=8,symsize=1.5
               xyouts,xpos[zok[i]],ypos[zok[i]],'  '+strtrim(string(i),2),/data, $
                  charsize=0.5
            endfor
            setusym,1
            device,/close
            display

            if queue ne '' then hardcopy,queue=queue

            logfile=root+'.log'
            starfile=root+'.stars'
            openw,lunstar,starfile,/append,/get_lun
            openw,lunlist,imfile+'.lst',/get_lun
            printf,lunlist,'Image: ',imfile,'  object: ',objname[0]
            printf,lunlist,' '
            print,'Doing automatic photometry on the good stars to ',logfile
            ; Construct object names
            starname=strarr(countok)
            for i=0L,countok-1 do begin
               rastr,sra[zok[i]],1,srastr
               decstr,sdec[zok[i]],0,sdecstr
               starname[i]='NVt'+strmid(srastr,0,2)+strmid(srastr,3,2)+ $
                                strmid(srastr,6,2)+strmid(srastr,9,1)+ $
                                strmid(sdecstr,0,3)+strmid(sdecstr,4,2)+ $
                                strmid(sdecstr,7,2)
               printf,lunstar,starname[i],' ',srastr,' ',sdecstr,'  0.000  0.000 '
               printf,lunlist,i,' ',starname[i],' ',srastr,' ',sdecstr
            endfor
            free_lun,lunstar
            free_lun,lunlist
            if queue ne '' then hardcopy,imfile+'.lst',queue=queue
            if !version.os_family eq 'unix' then begin
               spawn,'sort '+starfile+' | uniq > tmp.xxx; mv tmp.xxx '+starfile
            endif
            filter=hdrinfo.filter
            if exists(logfile) then $
               logmanip,logfile,/JUSTCLEAN,DELETEFILE=imfile
            objnum=10
            basphote,gain,image,exptime,xpos[zok],ypos[zok], $
                     objrad,objrad+10,objrad+30,logfile,objnum,/ALTLOG,/SILENT, $
                     FNAME=imfile,FILTER=filter,JD=hdrinfo.jd,NAME=starname, $
                     PSCALE=linfo.pscale
; photographic refinement
            logmanip,logfile,/JUSTCLEAN
         endif ; photstars block

         if spot[0] gt -0.9e9 then begin
            spot=double(spot)
            nspots=n_elements(spot)/2
            for sn=0,nspots-1 do begin
               xi = asteval((spot[0,sn]-linfo.xcopt)/renormfac, $
                            (spot[1,sn]-linfo.ycopt)/renormfac, $
                            cxi,terms)/3600.0d0*!dpi/180.0d0
               eta= asteval((spot[0,sn]-linfo.xcopt)/renormfac, $
                            (spot[1,sn]-linfo.ycopt)/renormfac, $
                            ceta,terms)/3600.0d0*!dpi/180.0d0
               astsn2rd,xi,eta,linfo.raopt,linfo.decopt,ra,dec
               rastr,ra,4,ras
               decstr,dec,3,decs
               infostr=string(thisexp,jds,spot[0,sn],spot[1,sn], $
                              format='(a,1x,a,1x,f6.1,1x,f6.1)')
               print,infostr,' ',ras,' ',decs
               repwrite,'spot.dat',infostr,infostr+' '+ras+' '+decs
            endfor
         endif

         ; Look for an object list file, these are object positions that
         ;   have been located elsewhere.  Re-measure the position, generate
         ;   ra,dec and save.
         objid=0
         objtag=''
         if fileno_type ne 0 and exists(fnobj) and not skipobj then begin
            objprmt,fnobj,path=path,keylist=keylist
            rdoblist,fnobj,nobj,files,dt,offvals,xyvals,flags,nfiles

            zf=where(imfile eq files)
            zf=zf[0]
            if zf ne -1 then begin

               words=strsplit(files[0],'.',/extract)
               objtag=strmid(words[0],strlen(root)-2,2) + $
                      strmid(words[1],strlen(suffix)-3,3) + extstr
               for objid=0,nobj-1 do begin
                  tobjname=objtag+strb36(objid,pad=2)
                  resfile = tobjname + '.ast'
                  if flags[objid] eq 'y' then begin
                     x = xyvals[zf*2,objid]
                     y = xyvals[zf*2+1,objid]
                     tag=strcompress(thisexp+' '+tobjname+' '+ $
                                       string(objrad,format='(f10.1)'))
                     if x ge 0.0 and y ge 0.0 then begin
                        basphote,gain,image,exptime,x,y,objrad,objrad+10,objrad+30, $
                           /exact,/nolog,/silent,xcen=xm,ycen=ym,mag=mag0,fwhm=fwhm0
; photographic refinement
                        if not nodisplay then begin
                           oplot,[xm],[ym],psym=5,color=color4,symsize=1.5
                           xyouts,xm,ym,'  '+strtrim(string(objid),2),color=color4
                        endif
                        astmag=(mag0+magdiff) < 99.9
                        xi = asteval((xm-linfo.xcopt)/renormfac,(ym-linfo.ycopt)/renormfac, $
                                       cxi,terms)/3600.0d0*!dpi/180.0d0
                        eta= asteval((xm-linfo.xcopt)/renormfac,(ym-linfo.ycopt)/renormfac, $
                                       ceta,terms)/3600.0d0*!dpi/180.0d0
                        astsn2rd,xi,eta,linfo.raopt,linfo.decopt,ra,dec
                        rastr,ra,4,ras
                        decstr,dec,3,decs
                        infostr=string(thisexp,hdrinfo.jd,ras,decs,astmag, $
                                       format='(a,1x,f13.5,1x,a,1x,a,1x,f6.3)')
                        print,tobjname,infostr,' FWHM ',fwhm0*linfo.pscale,'"', $
                              format='(2x,a,1x,a,a,f4.1,a)'
                        repwrite,resfile,thisexp,infostr
                        resfile='[[DEFAULT]]'
                        infostr=strcompress(string(xm,ym,format='(f10.3,1x,f10.3)'))
                        repwrite,'position.dat',tag,tag+infostr
                        if photstars then begin
                           basphote,gain,image,exptime,xm,ym, $
                                    objrad,objrad+10,objrad+30, $
                                    logfile,objnum,/ALTLOG,/SILENT,FNAME=imfile, $
                                    FILTER=filter,JD=hdrinfo.jd,NAME=tobjname, $
                                    PSCALE=linfo.pscale,/EXACT
; photographic refinement
                        endif
                     endif else begin
                        repwrite,resfile,thisexp,''
                        repwrite,'position.dat',tag,''
                     endelse
                  endif else if flags[objid] eq '?' then begin
                     print,'WARNING: object line ',objid,' has not yet been validated'
                     print,'             in file ',fnobj,', skipping.'
                     if exists(resfile) then begin
                        print,'deleting ',resfile
                        file_delete,resfile
                     endif
                  endif else begin
                     if exists(resfile) then begin
                        print,'deleting ',resfile
                        file_delete,resfile
                     endif
                  endelse
               endfor

            endif else begin
               print,'ERROR!  There is no valid file match in object file ',fnobj
            endelse

         endif else begin
            objres=file_search(objname[0]+'*.obj')
            if objres[0] ne '' and not skipobj then begin
               print,'WARNING!: There was no object list file found for this image.'
               print,'Object name is ',objname[0]
            endif
         endelse

         if doplast then begin
            astrd2xy,astra,astdec,linfo,astx,asty
            astrd2xy,astra+astvra*asterr[0,*]/cos(astdec), $
                     astdec+astvdec*asterr[0,*], $
                     linfo,astvxp,astvyp
            astrd2xy,astra-astvra*asterr[0,*]/cos(astdec), $
                     astdec-astvdec*asterr[0,*], $
                     linfo,astvxm,astvym
            astrd2xy,astrap1,astdecp1,linfo,astxp1,astyp1
            astxvar=[[astvxm],[astx],[astvxp]]
            astyvar=[[astvym],[asty],[astvyp]]
         endif

         objnum=0
         objcnt=0
         if objtag eq '' then begin
            cobjname=objname[0]
         endif else begin
            cobjname=objtag+strb36(objid,pad=2)
         endelse
         if do_objects then begin
            print,bel
            ; Get ready for astrometry on unknown
            doneone=0
            repeat begin
               setwin,window
               if fullrefresh then tv,bim
               plot,[0],[1],/nodata,xmargin=[0,0],ymargin=[0,0], $
                  xr=[0,sz[1]-1],yr=[0,sz[2]-1],xstyle=5,ystyle=5,/noerase
               if fullrefresh then begin
                  oplot,[border[0],sz[1]-border[1],sz[1]-border[1], $
                         border[0],border[0]], $
                        [border[3],border[3],sz[2]-border[2], $
                         sz[2]-border[2],border[3]],color=color4
                  if not noremind then $
                     oplot,[-20,20,20,-20,-20]+xloc,[-20,-20,20,20,-20]+yloc,color=color4
                  if doplast then begin
;                     oplot,astx,asty,psym=8,color=color1,symsize=0.2
                     for i=0,numobj-1 do begin
                        xyouts,astx[i],asty[i],' '+strtrim(string(i),2),color=color1
                        if asterr[0,i]*!radeg*3600.0/linfo.pscale gt 5.0 then begin
                           oplot,astxvar[i,*],astyvar[i,*],color=color1
                           oplot,[astx[i],astxp1[i]],[asty[i],astyp1[i]],color=color3
                        endif else begin
                           oplot,[astx[i]],[asty[i]],psym=4,color=color1,symsize=2.0
                           if asty[i] gt astyp1[i] then $
                              yoffset = -6*binfac $
                           else $
                              yoffset = 6*binfac
                           oplot,[astx[i],astxp1[i]], $
                                 [asty[i],astyp1[i]]+yoffset,color=color3
                        endelse
                     endfor
                  endif

                  setusym,-1
                  for i=0L,nstars-1 do begin
                     if bad[i] eq 0 then begin
                        oplot,[xpos[i]],[ypos[i]],psym=8,color=color3,symsize=2.5
                     endif else begin
                        oplot,[xpos[i]],[ypos[i]],psym=8,color=color2,symsize=2.5
                     endelse
                  endfor
               endif
               if doneone then oplot,[xm],[ym],psym=4,color=color4
               setusym,1
               print,'Left click '+cobjname+ $
                     ' to measure (middle-new object, right-exit)'
               if roam then begin
                  cr = string("15b)  ;"
                  fmt="($,a,a,2x,a)"
                  repeat begin
                     cursor,x,y,2
                     if !mouse.button eq 0 then begin
                        xi = asteval((x-linfo.xcopt)/renormfac,(y-linfo.ycopt)/renormfac, $
                                     cxi,terms)/3600.0d0*!dpi/180.0d0
                        eta= asteval((x-linfo.xcopt)/renormfac,(y-linfo.ycopt)/renormfac, $
                                     ceta,terms)/3600.0d0*!dpi/180.0d0
                        astsn2rd,xi,eta,linfo.raopt,linfo.decopt,ra,dec
                        rastr,ra,4,ras
                        decstr,dec,3,decs
                        print,format=fmt,cr,ras,decs
                     endif
                  endrep until !mouse.button ne 0
                  print,cr,'                                  ',cr,format=fmt
               endif else begin
                  cursor,x,y,3
               endelse

               if !mouse.button eq 1 then begin
                  basphote,gain,image,exptime,x,y,objrad,objrad+10,objrad+30, $
                     /nolog,/silent,xcen=xm,ycen=ym,mag=mag0,fwhm=fwhm0

                  if photographic then begin
                     dw = fix(2.0*fwhm0+0.5) > 5
                     photphot,image,xm,ym,dw,xcen,ycen,xfwhm,yfwhm,xflux, $
                        yflux,xback,yback,xcenerr,ycenerr,xfluxerr,yfluxerr,/debug
                     xm = xcen[0]
                     ym = ycen[0]
                     flx2mag,xflux/exptime,xfluxerr/exptime, $
                             xmag,xmagerr,zeropt=24.0
                     flx2mag,yflux/exptime,yfluxerr/exptime, $
                             ymag,ymagerr,zeropt=24.0
                     fwhm0 = (xfwhm[0]+yfwhm[0])/2.0
                     mag0  = (xmag[0]+ymag[0])/2.0
                  endif

                  astmag=(mag0+magdiff) < 99.9

                  tweakdone = tweakobj eq 0
                  tweaked=0

                  repeat begin
                     astrom_objectzoom,dv,linfo,xm,ym,fwhm0
                     if tweakobj then begin
                        print,'13> Click left (manual), middle (auto), right (done)'
                        cursor,xs1,ys1,3
                        if !mouse.button eq 1 then begin
                           xm = xs1
                           ym = ys1
                           basphote,gain,image,exptime,xs1,ys1, $
                               objrad,objrad+10,objrad+30,/exact, $
                              /nolog,/silent,mag=mag0,fwhm=fwhm0
                           astmag=(mag0+magdiff) < 99.9
                           tweaked=1
                        endif else if !mouse.button eq 2 then begin
                           basphote,gain,image,exptime,xs1,ys1, $
                               objrad,objrad+10,objrad+30,boxmrad=2, $
                              /nolog,/silent,xcen=xm,ycen=ym,mag=mag0,fwhm=fwhm0
                           astmag=(mag0+magdiff) < 99.9
                           tweaked=1
                        endif else begin
                           tweakdone = 1
                        endelse
                     endif
                  endrep until tweakdone
                  if tweakobj then !mouse.button=1

                  xi = asteval((xm-linfo.xcopt)/renormfac,(ym-linfo.ycopt)/renormfac, $
                               cxi,terms)/3600.0d0*!dpi/180.0d0
                  eta= asteval((xm-linfo.xcopt)/renormfac,(ym-linfo.ycopt)/renormfac, $
                               ceta,terms)/3600.0d0*!dpi/180.0d0
                  astsn2rd,xi,eta,linfo.raopt,linfo.decopt,ra,dec
                  rastr,ra,4,ras
                  decstr,dec,3,decs
                  infostr=string(thisexp,hdrinfo.jd,ras,decs,astmag, $
                                 format='(a,1x,f13.5,1x,a,1x,a,1x,f6.3)')
                  print,infostr,' FWHM ',fwhm0*linfo.pscale,' arcsec', $
                        format='(3x,a,a,f4.1,a)'

                  doneone=1

               endif else begin ; new object or quit

                  ; Save data if something was measured.
                  if doneone then begin

                     ; Save final astrometry to the object file.
                     if resfile eq '[[DEFAULT]]' then begin
                        resfile = cobjname + '.ast'
                     endif
                     repwrite,resfile,thisexp,infostr
                     resfile='[[DEFAULT]]'

                     ; Save the x,y for the object to a single master file.
                     infostr=strcompress(string(xm,ym,format='(f10.3,1x,f10.3)'))
                     tag=strcompress(thisexp+' '+cobjname+' '+ $
                                     string(objrad,format='(f10.1)'))
                     repwrite,'position.dat',tag,tag+infostr

                     doneone=0
                     if photstars then begin
                        basphote,gain,image,exptime,xm,ym, $
                                 objrad,objrad+10,objrad+30, $
                                 logfile,objnum,/ALTLOG,/SILENT,FNAME=imfile, $
                                 FILTER=filter,JD=hdrinfo.jd,NAME=cobjname, $
                                 PSCALE=linfo.pscale
; photographic refinement
                     endif
                  endif

                  ; Ask for new name if middle click.
                  if !mouse.button eq 2 then begin
                     objcnt=objcnt+1
                     if objcnt ge n_elements(objname) and objtag eq '' then begin
                        read,cobjname,prompt='New object name? '
                     endif else if objtag ne '' then begin
                        objid=objid+1
                        cobjname=objtag+strb36(objid,pad=2)
                     endif else begin
                        print,bel
                        cobjname=objname[objcnt]
                     endelse
                     cobjname=nobname(strtrim(strcompress(cobjname),2))
                  endif

               endelse
            endREP UNTIL !mouse.button eq 4 ; loop on measuring object


         endif  ; do_objects block

      endfor ; sub-exposure loop

   ;==================================

      ; Generate a final pretty postcript plot output file if requested
      if pretty ne '[[DEFAULT]]' then begin
         print,'       --> creating final postscript plot file....'
         hardim,bytscl(image,min=lowval,max=hival,top=255),0,255, $
            title=imfile+' '+strupcase(cobjname),/color, $
            autosize=1,queue='chani',width=18.0,/noprint,file=pretty
         if savec gt 0 then tvlct,red,gre,blu
         oplot,[border[0],sz[1]-border[1],sz[1]-border[1],border[0],border[0]], $
               [border[3],border[3],sz[2]-border[2],sz[2]-border[2],border[3]], $
               color=color4
         if not noremind then $
            oplot,[-20,20,20,-20,-20]+xloc,[-20,-20,20,20,-20]+yloc,color=color4
         if doplast then begin
;            oplot,astx,asty,psym=8,color=color1,symsize=0.2
            for i=0,numobj-1 do begin
               xyouts,astx[i],asty[i],' '+strtrim(string(i),2),color=color1
               if asterr[0,i]*!radeg*3600.0/linfo.pscale gt 5.0 then begin
                  oplot,astxvar[i,*],astyvar[i,*],color=color1
                  oplot,[astx[i],astxp1[i]],[asty[i],astyp1[i]],color=color3
               endif else begin
                  oplot,[astx[i]],[asty[i]],psym=4,color=color1,symsize=2.0
                  if asty[i] gt astyp1[i] then $
                     yoffset = -6*binfac $
                  else $
                     yoffset = 6*binfac
                  oplot,[astx[i],astxp1[i]], $
                        [asty[i],astyp1[i]]+yoffset,color=color3
               endelse
            endfor
         endif
         setusym,-1
         for i=0L,nstars-1 do begin
            if bad[i] eq 0 then begin
               oplot,[xpos[i]],[ypos[i]],psym=8,color=color1,symsize=1.5
            endif else begin
               oplot,[xpos[i]],[ypos[i]],psym=8,color=color2,symsize=1.5
            endelse
         endfor
         device,/close
         print,'       --> postscript device now closed.'
         display
      endif

next_extension:

      if countgood gt 0 then begin
;print,'F countgood=',countgood

         ; Compute the ra,dec of the optical axis
         astxy2rd,linfo.xcopt,linfo.ycopt,fitdata[ext],newracen,newdeccen,/full
         ; Save the final center
         astrom_svctr,thisexp,newracen,newdeccen,linfo,hdrinfo

      endif

   endfor  ; End of extension for loop block

;print,'A countgood=',countgood,'  toheader=',toheader

   ; update/append astrometric solution to fits file (if desired).
   if toheader and countgood gt 0 then begin
      filename = path+imfile+ft
      if extlist[0] eq 0 then begin
         print,'       --> Update header with astrometric solution.'
         hdr=headfits(filename)
         astinfo,hdr,fitdata,/toheader
         sxaddpar,hdr,'ASTPHOZP',fitdata.photzp, $
            ' Photometric zero-point from astrometric catalog'
         sxaddpar,hdr,'ASTAPER',objrad, $
            ' Photometric aperture for zero-point'
         modfits,filename,0,hdr
      endif else begin
         fits_open,filename,ftlun,/update
         for ext=0,numext-1 do begin
            print,'Update header for extension ',strn(extlist[ext]), $
                  ' with astrometric solution.'
            fits_read,ftlun,0,hdr,/header_only,exten_no=extlist[ext],/No_PDU
            astinfo,hdr,fitdata[ext],/toheader
            modfits,ftlun,0,hdr,exten_no=extlist[ext]
            sxaddpar,hdr,'ASTPHOZP',fitdata[ext].photzp, $
               ' Photometric zero-point from astrometric catalog'
            sxaddpar,hdr,'ASTAPER',objrad, $
               ' Photometric aperture for zero-point'
         endfor
         fits_close,ftlun
      endelse
   endif

end

