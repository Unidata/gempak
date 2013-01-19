	SUBROUTINE GGRDINFO ( cntrfl, keycol, kvgtyp, ksubtp, iolkdy,
     +			      iret )
C************************************************************************
C* GGRDINFO                                                             *
C*                                                                      *
C* This subroutine reads in the contour line information.		*
C* Points of relative minima and maxima are also processed.		*
C* Only those lines or minima/maxima with color keycol are processed.	*
C* If keycol=0, all lines are processed.  If a line has no color	*
C* indicated, it will be processed.  If kvgtyp and ksubtp are not 0,	*
C* then only those lines having VGTYPE of kvgtyp and SUBTYP of ksubtp	*
C* will be processed.  If iolkdy is not 0, then only lines for that day	*
C* will be processed.							*
C*                                                                      *
C* GGRDINFO ( CNTRFL, KEYCOL, KVGTYP, KSUBTP, IOLKDY,IRET )		*
C*                                                                      *
C* Input parameters:                                                    *
C*      CNTRFL		CHAR*		Input file			*
C*      KEYCOL		INTEGER		Key color 			*
C*	KVGTYP		INTEGER		VGTYPE from parameter KEYLINE	*
C*	KSUBTP		INTEGER		SUBTYP from parameter KEYLINE	*
C*	IOLKDY		INTEGER		Day of extended outlook (OLKDAY)*
C*                                                                      *
C* Output parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C**                                                                     *
C* Log:                                                                 *
C* D.W.Plummer/NCEP      9/98                                           *
C* D.W.Plummer/NCEP      2/99	Added parameter KEYCOL			*
C* T. Lee/GSC		 6/00	Returned if file does not exist		*
C* W.D.Plummer/NCEP     12/02	Added processing of minima/maxima	*
C* D.W.Plummer/NCEP     07/03	Chg MAXPTS to MAXPPL			*
C* D.W.Plummer/NCEP     09/03	Add check for closed lines		*
C* D.W.Plummer/NCEP     10/03	Bug fix for KEYCOL option		*
C* T. Lee/SAIC		08/04	Processed wind information		*
C* H. Zeng/SAIC		03/05	added call to in_catmmap		*
C* M. Li/SAIC		04/05	Rename GGRDLN to GGRDINFO		*
C* M. Li/SAIC		11/05	Included CATMAP name in error messge	*
C* M. Li/SAIC		11/05	Close file before any RETURN		*
C* F. J. Yen/NCEP	01/08	Added parameters KEYLINE and OLKDAY(CSC)*
C************************************************************************
C
        INCLUDE         'GEMPRM.PRM'
        INCLUDE         'grphgd.cmn'
C
	CHARACTER*(*)	cntrfl
C
	CHARACTER    	carr(7)*20, line*80
	LOGICAL		proces
C*
C-----------------------------------------------------------------------
C
C       This section reads in the contours and sets
C       the values in the hist and grid arrays
C
        CALL FL_SOPN ( cntrfl, lun, iret )
	IF  ( iret .ne. 0 )  THEN
	    iret = -11
	    CALL ER_WMSG ( 'FL', iret, cntrfl, ier )
	    CALL FL_CLOS(lun,iret)
	    RETURN
	END IF
C
        iostat = 0
        nlines = 0
        nmm    = 0
        nwnd   = 0
        DO ii = 1, MAXWND
           wlat(ii)  = RMISSD
           wlon(ii)  = RMISSD
           wdrct(ii) = RMISSD
           wsped(ii) = RMISSD
        END DO
C
        DO WHILE ( iostat .eq. 0 )
C
            READ(lun,'(A)',iostat=iostat)  line
C
            IF ( iostat .eq. 0 ) THEN
C
              CALL ST_CLST(line, ' ', ' ', 7, carr, nc, iret)
C
              CALL ST_NUMB ( carr(1), np, iret )
C
	      proces = .true.
	      itype  = 0
	      IF ( carr(5) .eq. ' ' )  THEN
C
C*		Wind element
C
		CALL ST_NUMB ( carr(2), itype, iret )
	        IF  ( iret .ne. 0 )  THEN
	            CALL ER_WMSG ( 'ST', iret, ' ', ier )
		    CALL FL_CLOS(lun,iret)
	            RETURN
	        END IF
	      ELSE IF ( carr(5) .ne. ' ' ) THEN
C
C*		Symbol or line element	
C
		IF ( keycol .ne. 0 )  THEN
                    CALL ST_NUMB ( carr(5), icolor, iret )
		    proces = icolor .eq. keycol
		END IF
C
		IF ( proces ) THEN
		    IF ( kvgtyp .ne. 0 ) THEN
		      IF (carr(6) .ne. ' ' .and. carr(7) .ne. ' ') THEN
                        CALL ST_NUMB ( carr(6), ivgtyp, iret )
                        CALL ST_NUMB ( carr(7), isubtp, iret )
		        proces = kvgtyp .eq. ivgtyp .and.
     +				     ksubtp .eq. isubtp 
		      END IF
		    END IF
		END IF
C
		IF ( proces ) THEN
		    IF ( iolkdy .ne. 0 ) THEN
		      IF ( carr(2)(1:1) .eq. 'D' .or.
     +			   carr(2)(1:1) .eq. 'd' ) THEN
		        CALL ST_NULL ( carr(2), carr(2), lens, ier )
			IF ( lens .eq. 2 ) THEN
			  CALL ST_NUMB ( carr(2)(2:2), idayb, iret)
			  IF (iret .eq. 0 )
     +				proces = idayb .eq. iolkdy
			ELSE IF ( lens .eq. 4 ) THEN
			  IF ( carr(2)(3:3) .eq. '-' ) THEN 
			    CALL ST_NUMB ( carr(2)(2:2), idayb, iret)
			    CALL ST_NUMB ( carr(2)(4:4), idaye, iret)
			    IF ( iret .eq. 0 ) THEN
			      proces = idayb .le. iolkdy .and.
     +					     iolkdy .le. idaye
			    END IF
			  END IF
			END IF 
		      END IF   
		    END IF
		END IF
C
	      END IF
C
 	      IF ( ( itype .eq. 8 ) .or. ( itype .eq. 9 ) )  THEN
C
		  IF ( nwnd .lt. MAXWND .and. proces ) THEN
C
C*		    process wind data.
C
		    nwnd = nwnd + 1
		    CALL ST_NUMB ( carr (2), wcolr (nwnd), iret )
	            IF  ( iret .ne. 0 )  THEN
	                CALL ER_WMSG ( 'ST', iret, ' ', ier )
			CALL FL_CLOS(lun,iret)
	                RETURN
	            END IF
		    wtype (nwnd) = itype
C
		    IF  ( iostat .eq. 0 )  THEN
		      READ ( lun,*,iostat=iostat ) 
     +		      wlat (nwnd), wlon (nwnd), wdrct(nwnd), wsped(nwnd)
		    END IF
		  END IF
C
	      ELSE IF ( np .eq. 1 )  THEN
C
 	          IF ( nmm .lt. MAXMM .and. proces )  THEN
C
C*		    Process a relative minima and maxima.
C
		    nmm = nmm + 1
C
		    CALL ST_NULL ( carr(2), carr(2), lens, ier )
		    CALL IN_CATMMAP ( carr(2), valuemm(nmm), iret )
		    CALL ST_RNUL ( carr(2), carr(2), lens, ier )
C
	            IF  ( iret .ne. 0 )  THEN
	                CALL ER_WMSG ( 'IN', iret, carr(2), ier )
			iret = -12
			CALL FL_CLOS(lun,iret)
	                RETURN
	            END IF
C
		    CALL ST_CRNM ( carr(3),  flatmm(nmm), iret )
		    CALL ST_CRNM ( carr(4),  flonmm(nmm), iret )
C
		  END IF
C
	      ELSE 
C
		  IF ( nlines .ge. MAXLIN )  proces = .false.
C
		  IF ( proces )  THEN
C
C*		    Process a contour line.
C
		    CALL ST_NULL ( carr(2), carr(2), lens, ier )
		    CALL IN_CATMMAP ( carr(2), v, iret )
		    CALL ST_RNUL ( carr(2), carr(2), lens, ier )
C
	            IF  ( iret .ne. 0 )  THEN
C
			IF ( carr(2) .ne. 'UNLABELED' )
     +			    CALL ER_WMSG ( 'IN', iret, carr(2), ier )
			proces = .false.
C
		    ELSE
C
		        nlines = nlines + 1
		        value(nlines) = v
C
                        CALL ST_NUMB ( carr(3),  ismth(nlines), iret )
                        CALL ST_NUMB ( carr(4), closed(nlines), iret )
C
		        IF ( np .ge. MAXPPL )  THEN
		            npts(nlines) = MAXPPL - 1
		        ELSE
		            npts(nlines) = np
		        END IF
C
	            END IF
C
		  END IF
C
                  DO  n = 1, np
C
                      IF ( iostat .eq. 0 )  THEN
C
			IF ( n .le. MAXPPL .and. proces )  THEN
                            READ(lun,*,iostat=iostat)
     +                          flat(n, nlines), flon(n, nlines)
			ELSE
			    READ(lun,*,iostat=iostat) dum1, dum2
			END IF
C
                      END IF
C
                  END DO
C
		  IF (proces) THEN
		    IF ( closed(nlines) .eq. CLSD  .and.
     &        ( (flat(1,nlines) .ne. flat(npts(nlines),nlines)) .or.
     &          (flon(1,nlines) .ne. flon(npts(nlines),nlines)) ) ) THEN
C
		      npts(nlines) = npts(nlines) + 1
		      flat(npts(nlines),nlines) = flat(1,nlines)
		      flon(npts(nlines),nlines) = flon(1,nlines)
C
		    END IF
C
		  END IF
C
              ENDIF
C
            ENDIF
C
        END DO
C
        CALL FL_CLOS(lun,iret)
C
	RETURN
	END
