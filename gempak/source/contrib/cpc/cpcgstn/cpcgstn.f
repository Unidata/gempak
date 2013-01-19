	PROGRAM CPCGSTN
C************************************************************************
C* PROGRAM CPCGSTN							*
C*									*
C* This program lists the stations in the given areas in US.		* 
C*									*
C* Log:									*
C* M. Li/SAIC		08/01						*
C* B. Yin/SAIC           3/04   Changed SS_GTIM to CSS_GTIM             *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	vgfile*(LLMXLN), output*(LLMXLN),
     +			stnfil*(LLMXLN), dattim*(LLMXLN)
C*
	CHARACTER	datcur*48, proj*(LLMXLN)
	CHARACTER	times*20, device*(20), dfilnam*(LLMXLN)
	LOGICAL 	respnd, done
	INTEGER		istnm (LLSTFL), iflag(LLSTFL), iclr(LLSTFL)
	INTEGER		itime
C------------------------------------------------------------------------
C*	Initilaize user interface.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'CPCGSTN', iperr, ' ', ier )
	    CALL SS_EXIT
	END IF
	CALL IP_IDNT  ( 'CPCGSTN', ier )

C
C* 	Set device, garea, and projection
C
	CALL GINITP  ( 1, istat, ier )

	iunit = 1
	device = 'GN'
	dfilnam = 'CPCGSTN'
	itype = 1
	xsize = 500
 	ysize = 500
	
	CALL GSDEVA (device, iunit, dfilnam, itype, xsize, ysize, ier)

	proj = 'STR'
	ang1 = 90
	ang2 = -105
	ang3 = 0
	fllat = 10
	fllon = -180
	urlat = 75
	urlon = -50

	CALL GSMPRJ (proj, ang1, ang2,ang3, fllat, fllon, 
     +			urlat, urlon, ier)
C
C*	Main loop.
C
	done  = .false.
	iflno = 0
	DO WHILE (.not. done)
C
C*	  Get user input and exit if there is an error.
C
	  CALL CPCGINP  ( vgfile, dattim, stnfil, output, iperr )
	  IF  ( iperr .ne. 0 )  THEN
	      CALL ER_WMSG  ( 'CPCGSTN', iperr, ' ', ier )
	      CALL SS_EXIT
	  END IF

C
C*  	  Get time
C
	  itime = 1
    	  CALL CSS_GTIM (itime, datcur, ier)
	  CALL TI_STAN (dattim, datcur, times, ier)
	  IF  ( ier .ne. 0 )  THEN
              CALL ER_WMSG  ( 'TI', ier, ' ', ier )
              CALL SS_EXIT
          END IF

C
C*	  Search the stations and output the results
C
	  CALL ST_NULL (stnfil, stnfil, lens, ier)
	  CALL ST_NULL (vgfile, vgfile, lens, ier)

	  CALL CPCG_SRCH(stnfil, vgfile, nstn, istnm, iflag, iclr, ier)

C	  IF ( ier .eq. 0 ) THEN
	    CALL CPCGOUT ( output, times, nstn, istnm, iflag, iclr, ier)
C	  END IF

C
C*        Call the dynamic tutor.
C
          CALL IP_DYNM  ( done, ier )
	END DO
C
C*	Exit.
C
  	CALL IP_EXIT  ( iret )
	END
