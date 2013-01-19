	PROGRAM GDPLOT
C************************************************************************
C* PROGRAM GDPLOT							*
C*									*
C* This program draws contours and/or plots vectors/barbs from gridded 	*
C* data.  Multiple sets of contours and vectors can be generated for	*
C* each frame.  This program was adapted from GDCNTR and GDWIND.	*
C**									*
C* Log:									*
C* G. Krueger/EAI	 5/93	Multiple overlay/plot ability		*
C* G. Krueger/EAI	 8/93	Added REFVEC, HILO, HLSYM, & CLRBAR	*
C* S. Jacobs/EAI	 9/93	Changed IN_CBAR & GR_CBAR to GG_CBAR	*
C* G. Krueger/EAI	10/93	Changed GFUNC & GVECT,fixed CLRBAR parse*
C* S. Jacobs/EAI	10/93	Changed call to IN_RVEC and plotting of *
C*				   reference arrow			*
C* S. Jacobs/EAI         2/94   Added COLADD flag to DG_OFIL            *
C* S. Jacobs/NMC         3/94   Added satellite display routines        *
C* L. Williams/EAI       3/94	Clean up declarations of user input	*
C*				variables				*
C* S. Jacobs/NMC         4/94   Removed unused variables        	*
C* S. Jacobs/NMC         6/94   DEVICE*12 --> *72                       *
C* L. Williams/EAI	 7/94	Removed call to GDBUPD and added shrttl	*
C*				to the user input variables		*
C* P. Bruehl/Unidata	 8/94	Increased size of character string tmfnd*
C*				from 20 to 36 for grids w/2 times	*
C*				Added .and.clear to IF for drop sat img	*
C* P. Bruehl/Unidata	 9/94	Added more checks for drop sat img	*
C* S. Jacobs/NMC	10/94	Check for proces=.true. before GDBTIM	*
C* P. Bruehl/Unidata	12/94	Fixed call to GR_VSCL			*
C* J. Cowie/COMEt	 8/95	Changed GSATIM to IM_DROP, add IM_LUTF,	*
C*				use idrpfl				*
C* P. Bruehl/NWS	12/95	Added code to allow looping		*
C* D. Keiser/GSC	12/95	Added STNPLT as a parameter		*
C* S. Jacobs/NCEP	 5/96	Fixed vector staggering			*
C* D. Keiser/GSC	 8/96	Added FL_MFIL to search for file type	*
C* G. Krueger/EAI	 8/96	Increased user variables to 72 chars.	*
C* K. Tyle/GSC		 8/96	Added ER_WMSG call after FL_MFIL call,	*
C*				use filnam in call to GDBDSP		*
C* D.W.Plummer/NCEP	10/96	Reorganized and cleaned up code.	*
C* D.W.Plummer/NCEP	11/96	Reorganized code for NMAP access.	*
C* D.W.Plummer/NCEP	 1/97	Added POSN, COLORS, MARKER, GRDLBL	*
C* D.W.Plummer/NCEP	 2/97	Moved CALL IM_LUTF into GDPLTB.f	*
C* D.W.Plummer/NCEP	 7/97	Increased input string sizes		*
C* D.W.Plummer/NCEP	 7/97	Added FILTER filter processing          *
C* S. Maxwell/GSC        7/97   Increased input character length        *
C* D.W.Plummer/NCEP	 8/98	Changed calling sequence of GDPTMS	*
C* T. Lee/GSC		 7/99	Implemented cycle			*
C* D.W.Plummer/NCEP	 4/00	Changes for multiple fcst hr processing	*
C* T. Lee/GSC		 7/00	Increased array size for satfnd & radfnd*
C* K. Brill/HPC		12/02	Add IJSKIP				*
C* K. Brill/HPC		 4/03	CALL DG_INTL				*
C* M. Li/SAIC		11/03	Added color bar for images		*
C* R. Tian/SAIC         11/03   Added nuflg to DG_INTL call             *
C* R. Tian/SAIC          2/04   Removed nuflg fromo DG_INTL call        *
C* T. Piper/SAIC	08/04	Added mscale				*
C* R. Tian/SAIC         12/04   Removed call to DG_CLAL			*
C* T. Lee/SAIC		01/06	GDPLOT3					*
C* T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'gdplot.cmn'
C*
C*	Input parameter string definitions.
C*
C*	These may contain filenames
C*
	CHARACTER	gdfile*256, satfil*256, radfil*256, stnplt*256
C*
C*	These are ordinary
C*
	CHARACTER	gdatim*(LLMXLN), glevel*(LLMXLN),
     +			gvcord*(LLMXLN), gdpfun*(LLMXLN),
     +			cint*(LLMXLN), line*(LLMXLN), map*(LLMXLN),
     +			title*(LLMXLN), device*(LLMXLN), proj*(LLMXLN),
     +			garea*(LLMXLN), panel*(LLMXLN), text*(LLMXLN),
     +			scale*(LLMXLN), latlon*(LLMXLN), 
     +		        contur*(LLMXLN), wind*(LLMXLN), refvec*(LLMXLN),
     +			skip*128, fline*(LLMXLN), type*(LLMXLN),
     +			hilo*(LLMXLN), hlsym*(LLMXLN), clrbar*(LLMXLN),
     +			lutfil*(LLMXLN), stream*(LLMXLN), fint*(LLMXLN),
     +			posn*(LLMXLN), colors*(LLMXLN), marker*(LLMXLN),
     +			grdlbl*(LLMXLN), filter*(LLMXLN),
     +			ijskip*(LLMXLN), imcbar*(LLMXLN),
     +			mscale*(LLMXLN), boxlin*(LLMXLN),
     +			region*(LLMXLN), txtcol*(LLMXLN),
     +			txtype*(LLMXLN), txtfil*(LLMXLN), 
     +			txtloc*(LLMXLN), column*(LLMXLN), 
     +			shape*(LLMXLN), info*(LLMXLN), loci*144,
     +			anotln*(LLMXLN), anotyp*(LLMXLN)
C*
C*	Other variables
C*
	LOGICAL		clear
C*
	CHARACTER	timfnd(LLMXGT)*256, gdfilo*256,
     +			satfnd (LLMXGT)*256, radfnd (LLMXGT)*256
	LOGICAL		respnd, done, proces, first
	LOGICAL		ltmp
C*
	CHARACTER       satfls(MXLOOP)*132, radfls(MXLOOP)*132
C*
	CHARACTER	gdfilx*256, prfxtt*20
C*
C-----------------------------------------------------------------------
C*  Initialize TAE.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .eq. 0 )  THEN
	    CALL IP_IDNT ('GDPLOT3', ier)
C
C*  Initialize GEMPLT.
C
	    CALL GG_INIT  ( 1, ier )
	    IF  ( ier .eq. 0 )  THEN
C
C*  Initialize grid library common area grdcmn.cmn
C 
		CALL GD_INIT  ( ier )
C
C*  Initialize the DG library.
C
		CALL DG_INTL ( ier )
C
		prfxtt = " "
C
		CALL GDPSTT ( 'PLOT_MAP', .true., ier )
		CALL GDPSTT (  'VERBOSE', .true., ier )
		done = .false.
	    ELSE
		done = .true.
	    END IF
	ELSE
	    done = .true.
	END IF
C
C*	Main loop to cycle through reading input parameters and plotting.
C
	DO WHILE  ( .not. done )
C
C*	    Set flag to indicate processing will be done.
C
	    proces = .true.
C*	    Read in the input parameters.
C
	    CALL GDBINP ( gdfile, gdatim, glevel, gvcord, gdpfun,
     +			  cint, line, map, mscale, title, device, proj,
     +			  garea, clear, panel, text, imcbar, scale,
     +			  latlon, contur, wind, refvec, skip, fint,
     +			  fline, type, hilo, hlsym, clrbar, stnplt,
     +                    satfil, radfil, lutfil, stream, posn, colors, 
     +			  marker, grdlbl, filter, ijskip, boxlin, region, 
     +			  txtcol, txtype, txtfil, txtloc, column, shape,
     +			  info, loci, anotln, anotyp, iperr )
C
C*	    Set up the graphics device.
C
	    CALL GG_SDEV  ( device, iret )
	    IF  ( iret .ne. 0 )  proces = .false.
C
	    proces = iret .eq. 0
C
	    IF ( proces )  THEN
C
C*              Set up parameters.
C
		CALL ST_LCUC ( proj, proj, iret )
		CALL GDPSTP (   'PROJ',   proj, iret )
		CALL GDPSTP (  'GAREA',  garea, iret )
		CALL GDPSTP ( 'IJSKIP', ijskip, iret )
		CALL GDPSTP (    'MAP',    map, iret )
		CALL GDPSTP ( 'MSCALE', mscale, iret )
		CALL GDPSTP ( 'LATLON', latlon, iret )
		CALL GDPSTP (   'TEXT',   text, iret )
		CALL GDPSTP ( 'IMCBAR', imcbar, iret )
		CALL GDPSTP ( 'CONTUR', contur, iret )
		CALL GDPSTP (   'WIND',   wind, iret )
		CALL GDPSTP ( 'REFVEC', refvec, iret )
		CALL GDPSTP (   'SKIP',   skip, iret )
		CALL GDPSTP (   'HILO',   hilo, iret )
		CALL GDPSTP (  'HLSYM',  hlsym, iret )
		CALL GDPSTP ( 'CLRBAR', clrbar, iret )
		CALL GDPSTP ( 'STNPLT', stnplt, iret )
		CALL GDPSTP ( 'STREAM', stream, iret )
		CALL GDPSTP (   'POSN',   posn, iret )
		CALL GDPSTP ( 'COLORS', colors, iret )
		CALL GDPSTP ( 'MARKER', marker, iret )
		CALL GDPSTP ( 'GRDLBL', grdlbl, iret )
		CALL GDPSTP ( 'GDFILE', gdfile, iret )
		CALL GDPSTP ( 'GLEVEL', glevel, iret )
		CALL GDPSTP ( 'GVCORD', gvcord, iret )
		CALL GDPSTP ( 'GDPFUN', gdpfun, iret )
		CALL GDPSTP (   'CINT',   cint, iret )
		CALL GDPSTP (   'LINE',   line, iret )
		CALL GDPSTP (  'TITLE',  title, iret )
		CALL GDPSTP (  'PANEL',  panel, iret )
		CALL GDPSTP (  'SCALE',  scale, iret )
		CALL GDPSTP (   'FINT',   fint, iret )
		CALL GDPSTP (  'FLINE',  fline, iret )
		CALL GDPSTP (   'TYPE',   type, iret )
		CALL GDPSTP ( 'LUTFIL', lutfil, iret )
                CALL GDPSTP ( 'FILTER', filter, iret )
                CALL GDPSTP ( 'BOXLIN', boxlin, iret )
                CALL GDPSTP ( 'REGION', region, iret )
                CALL GDPSTP ( 'TXTCOL', txtcol, iret )
                CALL GDPSTP ( 'TXTYPE', txtype, iret )
                CALL GDPSTP ( 'TXTFIL', txtfil, iret )
                CALL GDPSTP ( 'TXTLOC', txtloc, iret )
                CALL GDPSTP ( 'COLUMN', column, iret )
                CALL GDPSTP (  'SHAPE',  shape, iret )
                CALL GDPSTP (   'INFO',   info, iret )
                CALL GDPSTP (   'LOCI',   loci, iret )
                CALL GDPSTP ( 'ANOTLN', anotln, iret )
                CALL GDPSTP ( 'ANOTYP', anotyp, iret )
C
C*	        Get cycle and times based on first bang entry of GDFILE.
C
	        CALL GDPTMS ( gdatim, gdfile, ' ', LLMXGT,
     +			      ntimes, timfnd, iret )
C
		proces = iret .eq. 0
		IF ( .not. proces )  
     +		    write(6,*) "Unable to process GDATTIM = ", gdatim
C
	    END IF
C
	    IF ( proces )  THEN
C
		IF ( verbos )  THEN
                    write(6,*)  "Number of times = ", ntimes
                    DO  n = 1, ntimes
                        CALL ST_LSTR ( timfnd(n), ltf, iret )
                        write(6,*)  n, " ", timfnd(n)(:ltf)
                    END DO
                END IF
C
	        gdfilo = gdfilx
C
C*	        Loop through times to set up image files for each time, 
C*		i.e., if projection = SAT or RAD, check for multiple 
C*		image files.  First set up images file parameters.
C
		CALL GDPSTP ( 'SATFIL', satfil, iret )
		CALL GDPSTP ( 'RADFIL', radfil, iret )
C
		DO  itime = 1, ntimes		
C
		    satfnd(itime) = ' '
		    radfnd(itime) = ' '
C
		    DO  n = 1, MAXB
C
                        IF ( pro(n)(1:3) .eq. 'SAT' .or. 
     +			     pro(n)(1:3) .eq. 'RAD' ) THEN
C
                            CALL ST_FLST ( sat(n), ';', ' ', MXLOOP, 
     +					   satfls, nsatim, iret )
		            CALL ST_LSTR ( satfnd(itime), lensat, iret )
		            CALL ST_LSTR ( satfls(itime), lenfil, iret )
		            satfnd(itime) = satfnd(itime)(1:lensat) // 
     +				satfls(itime)(1:lenfil)	// '!'
C
                            CALL ST_FLST ( rad(n), ';', ' ', MXLOOP, 
     +					   radfls, nradim, iret )
		            CALL ST_LSTR ( radfnd(itime), lenrad, iret )
		            CALL ST_LSTR ( radfls(itime), lenfil, iret )
		            radfnd(itime) = radfnd(itime)(1:lenrad) // 
     +				radfls(itime)(1:lenfil) // '!'
C
			  ELSE
C
		            CALL ST_LSTR ( satfnd(itime), lensat, iret )
		            satfnd(itime) = satfnd(itime)(1:lensat)//'!'
		            CALL ST_LSTR ( radfnd(itime), lenrad, iret )
		            radfnd(itime) = radfnd(itime)(1:lenrad)//'!'
C
                        END IF
C
		    END DO
C
		END DO
C
C*	        Loop through times to call actual plot routines.
C
		DO  itime = 1, ntimes		
C
		    IF ( itime .eq. 1 )  THEN
			first = .true.
			CALL GSTANM ( iret )
		      ELSE
			first = .false.
			CALL GSPLOT ( iret )
		    END IF
C
		    ltmp = clear .or. ( first .and. ntimes .gt. 1 )
		    CALL GDPSTT ( 'CLEAR', ltmp, iret )
C
		    ltmp = clear .or. ( first .and. ntimes .gt. 1 )
		    CALL GDPSTT ( 'SHORT_TITLE', ltmp, iret )
C
C*	            Set up time and image files.
C
		    CALL GDPSTP ( 'GDATTIM', timfnd(itime), iret )
		    CALL GDPSTP (  'SATFIL', satfnd(itime), iret )
		    CALL GDPSTP (  'RADFIL', radfnd(itime), iret )
C
C*	            All parameters set up, call main GDPLOT function.
C
		    CALL GDPLTB ( itime, prfxtt, iret )
C
		    CALL GEPLOT ( iret )
C
C*		END DO for times (counter itime)
C
		END DO
C
		CALL GENANM ( iret )
C
	    END IF
C
C*	    Prompt for next contour/wind plot to be done.
C
	    CALL IP_DYNM  ( done, ier )
C
C*	    Print general error messages if necessary.
C
	    IF (iperr .ne. 0)
     +		CALL ER_WMSG ( 'GDPLOT', iperr, ' ', ier )
	    iret  = 0
	    ier  = 0
	    proces = .false.
	END DO
C
C*	Exit from GEMPLT and the TAE.
C
	CALL GENDP  ( 0, iret )
	CALL IP_EXIT  ( iret )
C*
	END
