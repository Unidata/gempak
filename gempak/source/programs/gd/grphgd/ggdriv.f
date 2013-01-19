	SUBROUTINE GGDRIV ( grid, grid1, kx, ky, hist, work1, work2,
     +			    work3, data, infoflg, iret )
C************************************************************************
C* GGDRIV                                                               *
C*                                                                      *
C* This subroutine takes all the original GRPHGD input parameters and	*
C* processes it into a GEMPAK grid file.				*
C*                                                                      *
C* GGDRIV ( GRID, GRID1, KX, KY, HIST, WORK1, WORK2, WORK3, DATA	*
C*          INFOFLG, IRET )						*
C*                                                                      *
C* Input parameters:                                                    *
C*                                                                      *
C* Output parameters:                                                   *
C*      GRID(*)         REAL            Data grid			*
C*      GRID1(*)        REAL            Data grid1			*
C*      KX              INTEGER         Data grid x-dimension		*
C*      KY              INTEGER         Data grid y-dimension		*
C*      HIST(*)         REAL            History data grid		*
C*      WORK1(*)        REAL            Latitude in degrees             *
C*      WORK2(*)        REAL            Longitude in degrees            *
C*      WORK3(*)        REAL            Cosine of latitude              *
C*      DATA(*)         REAL            Surface data			*
C*      INFOFLG         LOGICAL         Info flag			*
C*      IRET            INTEGER         Return code                     *
C*					= >0  - misc warnings		*
C*					= -1  - misc error 		*
C*					= -8  - unable to create gd file*
C*					= -13 - grid write problem	*
C*                                                                     	*
C**                                                                     *
C* Log:                                                                 *
C* D.W.Plummer/NCEP	10/99   From the original GRPHGD program	*
C* D.W.Plummer/NCEP	10/99   Remove user option to exit calls	*
C* S. Jacobs/NCEP	11/99	Changed GSMPRJ to GSGPRJ		*
C* D.W.Plummer/NCEP	 2/00	Added grid,kx,ky to calling sequence	*
C* D.W.Plummer/NCEP	 3/00	Added subarea option to CPYFIL		*
C* D.W.Plummer/NCEP	 5/00	Calling sequence change; use IP library	*
C* T. Lee/GSC		 6/00	Checked navigation and error message	*
C* T. Lee/GSC		10/00	Output default grid value		*
C* S. Jacobs/NCEP	 3/01	Increased file template to 48 chars	*
C* D.W.Plummer/NCEP	12/02	Changes for bounds, minmax, gen upgrade	*
C* D.W.Plummer/NCEP	 7/03	Add calls to GR_FIXA and GR_MAPS	*
C* D.W.Plummer/NCEP	 7/03	Chgs for intersection efficiency	*
C* D.W.Plummer/NCEP	 9/03	Chgs for relative mm, bounds		*
C* T. Lee/SAIC		 7/04	Added type, oaattr and oaguess to GRGINP*
C* H. Zeng/SAIC         03/05   Added catmap to GRGINP                  *
C* D.W.Plummer/NCEP     03/05   Increased grid limit to LLMXTG          *
C* R. Tian/SAIC          3/05   Modfied to adapt new file/time mgnt     *
C* H. Zeng/SAIC		04/05	Added discrete&dlines to grpinp		*
C* M. Li/SAIC		05/05	Process vector information		*
C* D.W.Plummer/NCEP	 5/05	Add DG_INTL; add ER_WMSG after DG_NWDT	*
C* D.W.Plummer/NCEP	 6/05	Bug fix to assign RMISSD properly	*
C* M. Li/SAIC		11/05	Updated error msg for ggrdinfo		*
C* m.gamazaychikov/SAIC	12/05	Add call to GG_RDVGF			*
C* T. Lee/SAIC          12/05   Initialized ighdr                       *
C* M. Li/SAIC		04/06	Added input to IN_DLINQ			*
C* S. Gilbert/NCEP	10/06	Added another ER_WMSG after GDGCFL	*
C* M. Li/SAIC		03/07	Added new input parameter EDGEOPTS	*
C* M. Li/SAIC		04/07   Added new function GGBNDCONDS		*
C* F. J. Yen/NCEP	01/08	Added parameters OLKDAY and KEYLINE	*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE		'grphgd.cmn'

	INTEGER 	iret, ier, kx, ky
C
	REAL 		grid(*), grid1(*), hist(*), work1(*), work2(*),
     +  		work3(*), data(*)
        LOGICAL		infoflg
C*
        CHARACTER       gdfile*(LLMXLN), proj*(LLMXLN), cpyfil*(LLMXLN),
     +                  gdarea*(LLMXLN), anlyss*(LLMXLN), kxky*(LLMXLN),
     +                  maxgrd*(LLMXLN), gparm*(LLMXLN),
     +                  cntrfl*(LLMXLN), gdatim*(LLMXLN),
     +                  gvcord*(LLMXLN), glevel*(LLMXLN),
     +                  ckeycl*(LLMXLN), gglimt*(LLMXLN),
     +                  hstgrd*(LLMXLN), bounds*(LLMXLN),
     +			type*(LLMXLN),   catmap*(LLMXLN),
     +			discrete*(LLMXLN), dlines*(LLMXLN),
     +                  guess*(LLMXLN),  guesfun*(LLMXLN),
     +                  cgamma*(LLMXLN), csrch*(LLMXLN),
     +                  pass*(LLMXLN),   cqcntl*(LLMXLN),
     +                  ggvgf*(LLMXLN),  edgeopts*(LLMXLN),
     +                  olkday*(LLMXLN), keylin*(LLMXLN)
C*
        CHARACTER       newfil*256, filnam*(LLMXLN)
        LOGICAL         proces, exist, gottm
C
	INTEGER		ighdr(LLGDHD), level(2), ivcord
	CHARACTER	parm*12, gdattm(2)*20, vparm*(LLMXLN),
     +			vgsub(2)*20
	LOGICAL		rewrit/.true./, hstflg
C
	REAL		grdmin, grdmax
	INTEGER		nmiss, ngpts
	CHARACTER	garout*72, prjout*72, imgfil*72/' '/,
     +	                gpack*12/' '/
C*
C-----------------------------------------------------------------------
C
	iret = 0
	proces = .true.
C
C*	Initialize DG library
C
	CALL DG_INTL ( ier )
C
C*	Get user input
C
        CALL GRGINP   ( gdfile, guess, proj, gdarea, kxky, maxgrd, 
     +			cpyfil, anlyss, cntrfl, gdatim, gparm,
     +			glevel, gvcord, ckeycl, gglimt,
     +			hstgrd, bounds, type, cgamma, csrch,
     +			pass, cqcntl, guesfun, catmap, discrete,
     +			dlines, ggvgf, edgeopts, olkday, keylin, iperr )


        CALL ST_NULL ( ggvgf, ggvgf, lens, ier )
	IF  ( iperr .ne. 0 )  THEN
	    CALL ER_WMSG ( 'GRPHGD', iperr, ' ', ier )
	    iret = -1
	    RETURN
	END IF
C
        CALL ST_NULL ( catmap, catmap, lens, ier )
	CALL IN_CATMINP ( catmap, ier )
C
        CALL ST_NULL ( discrete, discrete, lens, ier )
	CALL IN_DISCRETE ( discrete, ier )
C
        CALL ST_NULL ( dlines, dlines, lens, ier )
	CALL IN_DLINES ( dlines, ier )
C
C*	Process LIMITS input.
C
	CALL GLIMIT ( gglimt, ier )
C
C*      Create the grid file
C
	CALL FL_INQR ( gdfile, exist, newfil, ier )

	IF ( .not. exist ) THEN
	    CALL GDGCFL ( gdfile, proj, gdarea, kxky, maxgrd, cpyfil,
     +                    anlyss, ier )
	    IF ( ier .ne. 0 ) THEN
	        CALL ER_WMSG  ( 'GDCFIL', ier, ' ', ier1 )
	        iret = -8
	        CALL ER_WMSG  ( 'GRPHGD', iret, gdfile, ier )
	        RETURN
	    END IF
	END IF
C
C*      Process the GDFILE input
C
	IF ( proces ) THEN
            CALL DG_NFIL ( gdfile, gdfile, ier )
            IF ( ier .ne. 0 ) THEN
                CALL ER_WMSG ( 'DG', ier, ' ', irr )
                proces = .false.
            END IF
	END IF
C
C*      Process the GDATTIM input
C
	IF ( proces ) THEN
            CALL DG_NDTM ( gdatim, ier )
            IF ( ier .ne. 0 ) THEN
                CALL ER_WMSG ( 'DG', ier, gdatim, irr )
                proces = .false.
            END IF
	END IF
C
C*      Get the next time to process.
C
	IF ( proces ) THEN
            CALL DG_NTIM ( .true., .false., gdattm, gottm, ier )
            proces = ( ier .eq. 0 .and. gottm )
	END IF
C
C*      Set the map projection and graphics area.
C
	IF ( proces )  THEN
            CALL DG_FIXA  ( 'dset', ' ', garout, prjout, ier )
 	    CALL GG_MAPS ( prjout, garout, imgfil, idrpfl, ier )
	END IF
C
	CALL DG_QKXY ( kx, ky, ier )
C
C*      Check to see if grid is too large.
C
        IF ( ( kx * ky ) .gt. LLMXTG )  THEN
            iret = +1
            CALL ER_WMSG  ( 'GRPHGD', iret, ' ', ier )
            WRITE(6,*) 'Number of grid points (', kx*ky,
     &                  ') exceeds maximum allowed (', LLMXTG, ')'
            proces = .false.
        END IF
C
C*      Check to see if grid dimensions are too big for 
C*	intersection arrays.
C

        IF ( ( kx + ky ) .gt. MAXDIM )  THEN
	    iret = +2
	    CALL ER_WMSG  ( 'GRPHGD', iret, ' ', ier )
         WRITE(6,*) 'Total of grid dimensions (', kx+ky, 
     & 			') exceeds maximum allowed (', MAXDIM, ')'
	    proces = .false.
        END IF
C
	DO ii = 1, LLGDHD
	    ighdr ( ii ) = 0
	END DO
C
	IF ( proces )  THEN
	    CALL ST_LCUC  ( hstgrd, hstgrd, ier )
	    hstflg = hstgrd (1:1) .eq. 'Y'
C
C*	    Initialize history and data arrays
C
	    DO  ij = 1, kx*ky
		hist(ij) = INIT
		grid(ij) = RMISSD
		grid1(ij) = RMISSD
		work1(ij) = RMISSD
		work2(ij) = RMISSD
		work3(ij) = RMISSD
	    END DO
C
C*	    Get level value, vertical coord
C
	    CALL ST_ILST ( glevel, ':', -1, 2, level, num, ier )
	    CALL ST_LCUC ( gvcord, gvcord, ier )
	    CALL LV_CORD ( gvcord, vparm, ivcord, ier )
C
            imn = 1
	    imx = kx
	    jmn = 1
	    jmx = ky
C
C*	    Get keycol
C
	    CALL ST_ILST ( ckeycl, ' ', 0, 1, keycol, knum, ier )
C
C*	    Get iolkdy from GRPHGD parameter OLKDAY
C
	    CALL ST_ILST ( olkday, ' ', 0, 1, iolkdy, knum, ier )
C
C*	    Get and store vgtype (in kvgtyp) and subtyp (in ksubtp) of
C*	    line from GRPHGD parameter KEYLINE, which defines the
C*	    line types to process.  KEYLINE can be 0 or vgtype/subtyp.
C*	    Allowable values for vgtype are 'LINE' and 'SPLN'.
C*	    Invalid or default value for KEYLINE will be 0, which
C*	    means process all line types.
C
	    CALL ST_CLST ( keylin, '/', '0', 2, vgsub, knum, ier )
	    CALL ST_NULL (vgsub(1), vgsub(1), lens, ier)
	    CALL ST_LCUC ( vgsub(1), vgsub(1), ier )
	    IF ( vgsub(1)(1:lens) .eq. 'LINE' ) THEN
	        CALL ST_ILST ( vgsub(2), ' ', 0, 1, ksubtp, knum, ier )
		IF ( ksubtp .lt. 1 .or. ksubtp .gt. 9 ) THEN
		    ksubtp = 0
		    kvgtyp = 0
		ELSE
		    kvgtyp = 1
		END IF
	      ELSE IF ( vgsub(1)(1:lens) .eq. 'SPLN' ) THEN
	        CALL ST_ILST ( vgsub(2), ' ', 0, 1, ksubtp, knum, ier )
		IF ( ksubtp .lt. 1 .or. ksubtp .gt. 26 ) THEN
		    ksubtp = 0
		    kvgtyp = 0
		ELSE
		    kvgtyp = 20
		END IF
	      ELSE 
		kvgtyp = 0
		ksubtp = 0
	    END IF
C
C*	    Read in the VGF file
C
            CALL ST_NULL ( cntrfl, cntrfl, lens, ier )
            CALL GG_RDVGF ( ggvgf, cntrfl, catmap, ier )
C
	    IF  ( ier .ne. 0 )  THEN
	         CALL ER_WMSG ( 'GRPHGD', ier, ' ', ier2 )
	         iret = -1
	         RETURN
	    END IF
C
C*	    Read in contour line information into common
C
	    CALL GGRDINFO ( cntrfl, keycol, kvgtyp, ksubtp, iolkdy, ier)
C
            IF ( ier .ne. 0 ) THEN
                CALL ER_WMSG ( 'GRPHGD', ier, cntrfl, ier2 )
		iret = -1
                RETURN
            END IF
C
C*          Create a surface file with vector information
C
	    CALL ST_LCUC ( type, type, ier )
  	    filnam = 'g2g_' // gdatim(:6) // gdatim(8:9) //
     +               '.sfc' 
	    IF ( type(1:1) .eq. 'A' .or. type(1:1) .eq. 'B' )
     +	        CALL GGOACR ( filnam, gdatim(:11), type, ier )
C
	    IF ( type(1:1) .ne. 'A' .and. type(1:1) .ne. 'B' )  THEN
C
		IF ( hstflg )
     &		    WRITE(6,*) 'Total number of lines = ', nlines,
     &		    ' (',nint((nlines*100.0)/MAXLIN),'% of MAX ALLOWED)'
C
	        IF ( nlines .eq. 0 )  THEN
C
C*		    Set data array to default value and write to the file.
C
		    DO  ij = 1, kx*ky
		        grid(ij) = defalt
		    END DO
	            WRITE(6,*) 'Set data array to default = ', defalt
C
		    parm = gparm(:12)

                    CALL DG_NWDT ( grid, gdattm, level, ivcord, parm,
     +                             ighdr, gpack, rewrit, ier )
C
		    proces = ier .eq. 0
		    IF ( hstflg .and. proces )  THEN
		        parm = 'hist' // gparm(:8)
                        CALL DG_NWDT ( hist, gdattm, level, ivcord,
     +                             parm, ighdr, gpack, rewrit, ier )
		    END IF
		    proces = .false.
C
		END IF
C
	    END IF
C
	END IF
C
	IF ( proces )  THEN
	    bnds = bounds
C
C*	    Perform BOUNDS SEARCH
C
            CALL GBOUND ( imn, imx, jmn, jmx, grid, hist, kx, ky, ier )
            IF ( hstflg ) 
     +        WRITE (6,*) 'BOUNDS SEARCH COMPLETE, ier =', ier
C
	    IF ( type(1:1) .eq. 'A' .or. type(1:1) .eq. 'B' ) THEN
C
C*	        Process vector.
C
	        CALL IP_PUTV ( 'SFFILE', filnam, ier )
	        CALL IP_PUTV ( 'GDFILE', gdfile, ier ) 
	        CALL IP_PUTV ( 'SFPARM', 'UWND;VWND', ier ) 
	        CALL IP_PUTV ( 'DATTIM', gdatim(:11), ier ) 
	        CALL IP_PUTV ( 'DTAAREA', '-90;-180;90;180', ier ) 
C
 	        CALL OABSDR ( work1, work2, work3, data, infoflg, ier ) 

	        hstflg = .false.
C
C*	        Delete temporary surface data file.
C
		CALL ST_NULL ( filnam, filnam, len, ier )
		CALL GGOADL ( filnam, ier )
C
 	    ELSE
C
C*	       Process scalar.
C*	       Scan contour lines for all intersections; put into common.
C
	       CALL GGEDGE ( edgeopts, ier )
	       CALL GGBNDCONDS ( kx, ky, ier )
	       CALL GGGINT ( grid, hist, kx, ky, ier )
               IF ( hstflg )  THEN
                  WRITE (6,*)'INTERSECTIONS SEARCH COMPLETE, ier =', ier
		  numints = 0
		  DO  jj = 1, 4
		    ntot = 0
		    DO  ii = 1, intdim(jj)
		    DO  nn = 1, intptrs(ii,jj,1)
		        ntot = ntot + 1
			numints = numints + 1
		    END DO
		    END DO
		    WRITE(6,*) 'Total number of ints in dir ',jj,
     &			' = ',ntot
		  END DO
		  WRITE(6,*)'TOTAL NUMBER of INTERSECTIONS = ', numints,
     &		'  (',nint((numints*100.0)/MAXINT),'% of MAX ALLOWED)'
 	       END IF
C
C*	    Check whether it is discrete or continuous
C
	    CALL IN_DISCQ ( istate, ier )
C
C
C*	    The following is for discrete case
C
	    IF ( istate .eq. 1 ) THEN
C
C*	      Perform DLINE SEARCH
C
	      CALL GDLINE ( imn, imx, jmn, jmx, grid, hist, kx, ky, ier )
	      IF ( hstflg ) 
     +	        WRITE (6,*) 'DLINE SEARCH COMPLETE, ier =', ier
C
C*	      Perform DLINE WEIGHTED SEARCH
C
	      CALL GDLWEI ( imn, imx, jmn, jmx, grid, hist, kx, ky, ier )
	      IF ( hstflg ) 
     +	        WRITE (6,*) 'DLINE WEIGHTED SEARCH COMPLETE, ier =', ier
C
	    ELSE
C
C*	      The following is for continuous case
C
C*	       Perform RADIAL SEARCH
C
	       CALL GRADLS (imn, imx, jmn, jmx, grid, hist, kx, ky, ier)
	       IF ( hstflg ) 
     +	         WRITE (6,*) 'RADIAL SEARCH COMPLETE, ier =', ier
C
C*	       Perform LIKE-VALUE SEARCH
C
	      CALL GLVALU ( imn, imx, jmn, jmx, grid, hist, kx,
     +							ky, ier )
	       IF ( hstflg ) 
     +	         WRITE (6,*) 'LIKE-VALUE SEARCH COMPLETE, ier =', ier
C
C*	      Check the state of DLINES. If yes, add calls to
C*            GDLINE and GWEIGS
C
	      CALL IN_DLINQ ( istate, istateL, epsi, ier )
C
	      IF ( istate .eq. 1 ) THEN
C
C*	        Perform DLINE SEARCH
C
	        CALL GDLINE ( imn, imx, jmn, jmx, grid, hist, kx,
     +							ky, ier )
	        IF ( hstflg ) 
     +	          WRITE (6,*) 'DLINE SEARCH COMPLETE, ier =', ier
C
	      END IF
C
C*	       Perform WEIGHTED SEARCH
C
 	       CALL GWEIGS (imn, imx, jmn, jmx, grid, hist, kx, ky, ier)
 	       IF ( hstflg ) 
     +	         WRITE (6,*) 'WEIGHTED SEARCH COMPLETE, ier =', ier
C
C*	        Perform SMOOTHING
C
 	        CALL GSMOOT (imn, imx, jmn, jmx, grid, hist, kx, ky, ier)
 	        IF ( hstflg ) 
     +	          WRITE (6,*) 'SMOOTHING COMPLETE, ier =', ier
C
C*	        Apply limits, if any.
C 
 	        CALL GGLIMS (imn, imx, jmn, jmx, grid, hist, kx, ky, ier)
 	        IF ( hstflg ) 
     +	          WRITE (6,*) 'LIMITS COMPLETE, ier =', ier
C
C*	      This is the end of continous case
C
	    ENDIF
C	    
C*	        Write computed grid and history grid to grid file.
C
	        parm = gparm(:12)
                CALL DG_NWDT ( grid, gdattm, level, ivcord, parm,
     +                         ighdr, gpack, rewrit, ier )
	        IF ( hstflg )  WRITE (6,*) 'RETURN from DG_NWDT =', ier
		IF  ( ier .ne. 0 )  THEN
	    	    CALL ER_WMSG ( 'DG', ier, ' ', ier )
		    iret = -13
	    	    RETURN
		END IF
	        proces = ier .eq. 0
C
C*	        Figure whether history grid needs to be written.
C
                IF  ( hstflg .and. proces )  THEN
C
	          parm = 'hist' // gparm(:8)
                  CALL DG_NWDT ( hist, gdattm, level, ivcord, parm,
     +                           ighdr, gpack, rewrit, ier )
	          IF ( hstflg )  WRITE(6,*) 'RETURN from DG_NWDT =', ier
		  IF  ( ier .ne. 0 )  THEN
	    	    CALL ER_WMSG ( 'DG', ier, ' ', ier )
		    iret = -13
	    	    RETURN
		  END IF
C
	        END IF

	    END IF
C
	END IF
C
        IF  ( hstflg .and. proces )  THEN
C
	    nmiss = 0
	    grdmin =  1.0E10
	    grdmax = -1.0E10
	    ngpts = (imx-imn+1)*(jmx-jmn+1)
	    DO  ii = 1, ngpts
		IF ( grid(ii) .ne. RMISSD )  THEN
		    grdmin = min(grdmin,grid(ii))
		    grdmax = max(grdmax,grid(ii))
		ELSE
		    nmiss = nmiss + 1
		END IF
	    END DO
	    WRITE(6,*) 'Total number of grid points = ', ngpts
	    WRITE(6,*) 'Grid minimum and maximum = ', grdmin, grdmax
	    WRITE(6,*) 'Number of missing values = ', nmiss
C
	END IF
C
	CALL DG_NEND ( ier )
C
	RETURN
C*
	END
