	PROGRAM  GDPOINT
C************************************************************************
C* PROGRAM GDPOINT							*
C*									*
C* Interpolates any gridded quantity to the location specified and      *
C* prints the output.                                                   *
C*									*
C**									*
C* Log:									*
C* T.W. Barker/WR/MSO	 9/96	Created from GDTHGT				*
C* S. Chiswell/Unidata	11/03	Fixed typo in gr_sscl call			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(LLMXLN)	gdfile, gpoint, gdatim, scale, gfunc,
     +                  glevel, gvcord, pfunc, parm
C
	REAL	        rgx(1), rgy(1), rlat(1), rlon(1),
     +			hgrd(LLMXGD), yy(1), rnvblk ( LLNNAV )
	CHARACTER	timfnd*36,
     +			time(2)*20, firstm*20, lastim*20
	INTEGER		lev(2)
	LOGICAL		respnd, done, proces, first, gintp, gottm

	INCLUDE		'ERMISS.FNC'
C*
C-----------------------------------------------------------------------
C*	Initialize TAE and GEMPLT.
C
	CALL IP_INIT  ( respnd, iperr )
	CALL IP_IDNT  ( 'GDPOINT', ier )
	IF ( iperr .eq. 0 )  THEN
	   mode = 1
	   CALL GG_INIT  ( mode , iperr )
	   IF (iperr.eq.0) THEN
	      done = .false.
	   ELSE 
	      done = .true.
	   ENDIF
	ELSE
	   done = .true.
	END IF
C
C*	Initialize the DG library.
C
	CALL DG_INTL ( ier )
C
C*	Main loop to read in TAE parameters
C
	DO WHILE  ( .not. done )
C
C*	    Set flag to indicate processing will be done.
C
	    proces = .true.
C
C*	    Read in the variables from the TAE.
C
	    CALL GDPTIN  ( gdatim, gpoint, gdfile, scale,
     +                     glevel, gvcord, gfunc, iperr )
C
C*	    Exit if there is an error.
C
	    IF ( iperr .ne. 0 )  THEN
		done = .true.
	    ELSE
C
C*		Process the GDFILE input.
C
		CALL DG_NFIL ( gdfile, ' ', ier )
		IF ( ier .ne. 0 ) THEN
                        CALL ER_WMSG ( 'DG', ier, ' ', irr )
                        proces = .false.
                END IF
C
C*		Process the GDATTIM input; setup the time server.
C
		CALL DG_NDTM ( gdatim, ier )
		IF ( ier .ne. 0 ) THEN
                    CALL ER_WMSG ( 'DG', ier, gdatim, irr )
                    proces = .false.
                END IF
C
C*		Loop over times.
C
		itime = 1
		gottm = proces
		first=.true.
		DO WHILE ( gottm )
C
C*		   Get the next time to process from the time server.
C
		   CALL DG_NTIM ( .true., .true., time, gottm, iret )
		   proces = ( iret .eq. 0 .and. gottm )
		   IF ( iret .ne. 0 ) THEN
		      write (*,*) 'Time not found ',time(1)
		   END IF

		   CALL TG_DUAL ( time, timfnd, iret )
		   IF ( proces .and. first ) THEN
                       CALL DG_FLNO  ( gfunc, iflnos, ier )
                       CALL DG_QDTM ( iflnos, firstm, lastim, ier )
                       IF ( firstm .eq. ' ' ) THEN
                           proces = .false.
                       END IF
                   END IF
C
C*		   Find plotting location
C
		   IF ( proces ) THEN
		       CALL GR_PLOC ( gpoint, rgx (1), rgy (1), 
     +				rlat (1), rlon (1), iret )
		       IF ( iret .ne. 0) THEN
			   CALL ER_WMSG ( 'GR', iret, ' ', ier )
			   proces = .false.
		       END IF
		   END IF

C
C*      Set a new output reference navigation using gpoint as the center
C*      of a 9 point grid. This allows vector components to be interpolated
C*      to the desired grid point first within DG_GRID before the quantity is
C*      calculated, rather than interpolated from surrounding points after
C*      the computation is done. This will improve calculations involving
C*      vector components.
C*
C*	Note: This isn't good for functions such as HIGH(), LOWS(), GWFS(),
C*	SM5S(), SM9S(), which use an area of points
C
	           CALL GR_MNAV ( 'CED', 3, 3, rlat(1) - .01, 
     +			rlon(1) - .01, rlat(1) + .01, 
     +			rlon(1) + .01, 0., 0., 0., .true.,
     +          	rnvblk, iret )
        	   IF ( iret .eq. 0 ) CALL DGCONAV ( rnvblk, iret )
        	   IF ( iret .ne. 0 ) THEN
		       write(*,*) 'Using interpolation of grid results.'
            	       gintp = .true.
                     ELSE
		       gintp = .false.
        	   END IF
C
		   CALL CWRAP_GCALC ( timfnd, glevel, gvcord,
     +			gfunc, scale, gintp, rgy, rgy, yy, iret )
C
c		   CALL DG_GRID(timfnd, glevel, gvcord, gfunc,
c     +                          pfunc, hgrd, kx, ky, time,
c     +                          lev, ivc, parm, iret)
C
                   IF (iret .eq. 0) THEN
c                      CALL IN_SCAL(scale,iscals,iscalv,iret)
c                      CALL GR_SSCL(iscals,kx,ky,1,1,kx,ky,hgrd,
c     +				rmin, rmax, iret)
C
c		      IF ( gintp ) THEN
c                          CALL GR_INTP(1,rgx,rgy,1,kx,ky,hgrd,yy, iret)
c			else
c			  yy(1) = hgrd(5)
c		      END IF
                      IF (first) THEN
                          print 200
                          print 300,gdfile
                          print 301,gpoint
                          print 302,gvcord
                          print 303,glevel
                          print 304,gfunc
                          print 305,scale
 200                      format(' ')
 300                      format('GDFILE: ',a50)
 301                      format('GPOINT: ',a50)
 302                      format('GVCORD: ',a50)
 303                      format('GLEVEL: ',a50)
 304                      format('GFUNC : ',a50)
 305                      format('SCALE : ',a50)
                          first=.false.
                       END IF
                       print 201,time(1),yy(1)
 201                   format('     ',a15,' : ',f12.5)
                   END IF
                END DO

		CALL GDPTUP ( gdatim, gpoint, gdfile, scale, 
     +                        glevel, gvcord, gfunc, iret)
		CALL IP_DYNM ( done, ier )
	    END IF
	END DO
	CALL GENDP ( 1, iret )
	CALL IP_EXIT ( iret )

	END
