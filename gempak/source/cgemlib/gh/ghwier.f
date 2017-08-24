	SUBROUTINE GH_WIER ( timstr, ocean, origc, wwnd, fdate, mxwd,
     +                       nc, nstrm, iyaxis, prblvl, proces, itwnd, 
     +                       iret )
C************************************************************************
C* GH_WIER								*
C*									*
C* This subroutine calculates the intensity probability points for a 	*
C* single advisory.							*
C*									*
C* GH_WIER (  TIMSTR, OCEAN, ORIGC, WWND, FDATE, MXWD, NC, NSTRM,       *
C*            IYAXIS, PRBLVL, PROCES, ITWND, IRET )			*
C*									*
C* Input parameters:							*
C*	TIMSTR 		CHAR*		Advisory valid time string	*
C*	OCEAN		CHAR*           Current ocean designation	*
C*	ORIGC		CHAR*           Issuing center                  *
C*	WWND 		CHAR*		Current max sustained wind	*
C*      FDATE (NC,*)    CHAR*           Forecast date/time strings      *
C*      MXWD (NC,*)     INTEGER         Forecast max wind               *
C*      NC              INTEGER		Number of reports		*
C*      NSTRM		INTEGER		Num. of single stormadvisories  *
C*									*
C* Output parameters:							*
C*	IYAXIS (5,*)	INTEGER		Number of decoded files		*
C*      PRBLVL(*)	INTEGER         Array of probability labels	*
C*	PROCES		LOGICAL		Flag for finding data		*
C*      ITWND           INTEGER		Number of forecasted winds	*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* A. Hardy/GSC		 3/01  	Created					*
C* A. Hardy/SAIC	 8/01   Fixed if only current wind is reported  *
C* A. Hardy/NCEP	 8/02   Set 115 kts wind to 135 mph		*
C* m.gamazaychikov/SAIC	06/06	Added probability table for CP storms	*
C* S. Gilbert/NCEP	 7/06   Added new argument origc                *
C* M. Sardi/NHC	        10/02   Set 115 kts wind to 130 mph		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	timstr, fdate(nc,*), wwnd, ocean, origc
	INTEGER		mxwd(nc,*), iyaxis(5,*), prblvl(*)
        LOGICAL         proces
C*
     	CHARACTER       fctdat(6)*14, table*128, tbtyp*128
	INTEGER		errs (6,6,6), fctwnd(6), wndcat(6)
C-----------------------------------------------------------------------
	iret = 0
C
C*      Read in intensity probability table. 
C
        IF ( ocean(1:2) .eq. 'AL' ) THEN
            table = 'intprba.tbl'
          ELSE IF ( ocean(1:2) .eq. 'CP' ) THEN
            table = 'intprbc.tbl'
          ELSE IF ( ocean(1:2) .eq. 'EP' ) THEN
            table = 'intprbp.tbl'
        END IF
        IF ( origc(1:4) .eq. 'CPHC' ) THEN
            table = 'intprbc.tbl'
        END IF
        tbtyp = 'hcnadv'
        CALL FL_TBOP ( table, tbtyp, lun, iret)
        IF ( iret .eq. 0 ) THEN
            DO ii = 1, 6
                DO jj = 1, 6
                    READ (lun, *) ione, itwo, ithr, ifor, ifiv, isix
                    errs(ii,jj,1) = ione
                    errs(ii,jj,2) = itwo
                    errs(ii,jj,3) = ithr
                    errs(ii,jj,4) = ifor
                    errs(ii,jj,5) = ifiv
                    errs(ii,jj,6) = isix
                END DO
            END DO
            CALL FL_REWD ( lun, iret )
            CALL FL_CLOS ( lun, iret )
          ELSE
            CALL ER_WMSG ( 'GPTPC', -11, ' ', ier )
            RETURN
        END IF
C
C*	Set the time/date, max winds at 3, 12, 24, 36, 48 and 72 hrs.
C*      Also, convert the max wind from knots to MPH. Set all 115 knot
C*      to 130 mph.
C
        iwind = 0
        proces = .true.
        IF ( iret .eq. 0 ) THEN
            fctdat (1) = timstr
            CALL ST_NUMB ( wwnd, fctwnd (1), ier)
            rwnd  = PR_KNMH (FLOAT(fctwnd(1) ) )
            IF ( ( rwnd .gt. 131.18 ) .and. ( rwnd .lt. 133.0 ) ) THEN
                fctwnd(1) = 130
              ELSE
                itpwnd  = INT ( rwnd )
                iwind = ( itpwnd / 5 ) * 5
                IF ( ( ( AMOD (rwnd,5.0) ) ) .ge. 2.5 ) THEN
                    fctwnd(1) = iwind + 5
                  ELSE
                    fctwnd(1) = iwind 
                END IF
            END IF
C
            im = 2
            DO jj = 1, 5
                IF ( mxwd(jj,nstrm) .gt. 0) THEN
                    fctdat (im) = fdate(jj,nstrm)
                    rwnd  = PR_KNMH (FLOAT(mxwd(jj,nstrm) ) )
                    IF ( ( rwnd .gt. 131.18 ) .and. 
     +				           ( rwnd .lt. 133.0 ) ) THEN
                        fctwnd(im) = 130
                      ELSE
                        itpwnd  = INT ( rwnd )
                        iwind = ( itpwnd / 5 ) * 5
                        IF ( ( ( AMOD (rwnd,5.0) ) ) .ge. 2.5 ) THEN
                             fctwnd(im) = iwind + 5
                          ELSE
                             fctwnd(im) = iwind 
                        END IF
                    END IF
                    im = im + 1
                END IF
            END DO
        END IF
        itwnd = im - 1
C
C*      Check if last forecast or extratropical at 12H.
C
        DO ii = 1, itwnd
            IF ( ( fctwnd (ii) .lt. 0) .or. ( itwnd .eq. 1) ) THEN
                proces = .false.
            END IF
        END DO
C
        IF ( proces ) THEN
C
C*	Determine the wind intensity range.
C
            maxint = 0
            minfct = 200
            minint = 200
            mininf = 200
C
            DO jj = 1, itwnd
                IF ( fctwnd (jj) .le. 40 ) THEN
                    wndcat(jj) = 1
                  ELSE IF ( fctwnd(jj) .le. 60 ) THEN
                    wndcat(jj) = 2
                  ELSE IF ( fctwnd(jj) .le. 75 ) THEN
                    wndcat(jj) = 3
                  ELSE IF ( fctwnd(jj) .le. 95 ) THEN
                    wndcat(jj) = 4
                  ELSE IF ( fctwnd(jj) .le. 115 ) THEN
                    wndcat(jj) = 5
                  ELSE
                    wndcat(jj) = 6
                END IF
C
C*              10% LEVEL
C
                IF ( fctwnd(jj) .ne. 0) THEN
                   minfct = MIN0 ( mininf, fctwnd (jj) )
                   maxint = MAX0 ( maxint, fctwnd (jj)  
     +                             + errs (1, jj, wndcat (jj) ) )
                END IF
C
C*              90% LEVEL
C
                IF ( fctwnd(jj) .ne. 0 ) THEN
                    minint = MIN0 ( minint, fctwnd (jj)  
     +                              + errs ( 6, jj, wndcat (jj) ) )  
                END IF
            END DO 
C
C*          Determine whether to use 20% or 30% contours along 
C*          with 10% and 90%
C*          1 = 10% 2=20% 3=30% 4=70% (30%) 5=80% (20%) 6=90%
C
 	    prblvl (1) = 1
            IF ( ( ( itwnd .eq. 6 ) .and. fctwnd(itwnd) .ge. 100 ) 
     +          .or. ( minfct .le. 40 ) 
     +          .or. ( ocean(1:2) .eq. 'EP' ) ) THEN
 	        prblvl (2) = 2
 	        prblvl (3) = 5
              ELSE
 	        prblvl (2) = 3
 	        prblvl (3) = 4
            END IF
 	    prblvl (4) = 6
C
            DO ii = 1,4
                DO jj = 1,itwnd
                    IF ( fctwnd(jj) .ne. 0 ) THEN
                        iypos = INT( FLOAT( fctwnd(jj) + 
     +                          errs (prblvl(ii), jj, wndcat(jj)))) 
                        iyaxis(ii,jj) = MAX0 ( iypos, 0 )
                    END IF
                END DO    
            END DO    
C
C*          Store forecast (thick NHC line)
C
            DO jj = 1,itwnd
                IF ( fctwnd(jj) .ne. 0 ) THEN
                    iyaxis(5,jj) = fctwnd(jj) 
                END IF
            END DO
        END IF
C*
	RETURN
	END
