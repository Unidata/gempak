	SUBROUTINE GH_WTBL ( timstr, wname, origc, wadnm, tzone, nc,
     +                       wwnd, mxwd, wocen, nstrm, iret)
C************************************************************************
C* GH_WTBL								*
C*									*
C* This subroutine creates the wind speed intensity table.		*
C*									*
C* GH_WTBL ( TIMSTR, WNAME, ORIGC, WADNM, TZONE, NC, WWND, MXWD, WOCEN, *
C*	     NSTRM, IRET) 						*
C*									*
C* Input parameters:							*
C*	TIMSTR    	CHAR*		Date/time string		*
C*	WNAME 		CHAR*		Name of tropical storm		*
C*	ORIGC 		CHAR*		Issuing center			*
C*	WADNM 		CHAR*		Advisory number			*
C*	TZONE		CHAR*		Local time zone			*
C*      NC		INTEGER         Total number of storms		*
C*	WWND 		CHAR*		Current advisory max wind speed	*
C*	MXWD (NC,*) 	INTEGER		Forecasted max wind speed	*
C*	WOCEN 	       	CHAR*           Ocean designation		*
C*	NSTRM 		INTEGER		Number of advisories for a storm*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* A. Hardy/GSC		 4/01	Created					*
C* A. Hardy/GSC		 5/01	Added GQLINE, GSLINE, modified GQ/SSPCL *
C* D. Kidwell/NCEP	 4/02	Changed PS check                        *
C* A. Hardy/NCEP	 9/02	Removed call to GH_NOPB			*
C* m.gamazaychikov/SAIC	06/06	Change setting for table, GH_WTBG CS	*
C* S. Gilbert/NCEP	 7/06	Added new argument origc                *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
        CHARACTER*(*)	timstr, wname, wadnm, tzone, wwnd, wocen, origc
        INTEGER         mxwd(nc,*)
C*
 	CHARACTER       table*128, tbtyp*128, ocean*6,strnam*15,
     +                  advnm*3, datstr*15, ddev*12
        INTEGER         prob (5,32,8), fctwnd(5), ipnum(5,8), ivalue
        LOGICAL         proces
C------------------------------------------------------------------------
 	iret = 0
C
C*	Query current settings.
C
	CALL GQCOLR ( jcolr, ier )
	CALL GQSPCL ( szspcl, jspwid, ier )
        CALL GQLINE ( jltyp, jlthw, jwidth, jwhw, ier )
	CALL GQTEXT ( jtxfn, jtxhw, siztx, jtxwid, jbrdr,
     +		      jrrotn, jjust, ier )
C
C*      Reverse black and white if device is for postscript.
C

        CALL GQDEV  ( ddev, iunit, iatyp, iret )
        IF  ( ddev(:2) .eq. 'PS' )  THEN
                iwht = 32
                iblk = 1
              ELSE
                iwht = 31
                iblk = 32
        END IF
C
C*      Read in wind speed probability table. 
C
       ocean = wocen
       IF ( ocean(1:2) .eq. 'AL' ) THEN
            table = 'wndprba.tbl'
          ELSE IF ( ocean(1:2) .eq. 'CP' ) THEN
            table = 'wndprbc.tbl'
            tzone = 'H'
          ELSE IF ( ocean(1:2) .eq. 'EP' ) THEN
            table = 'wndprbp.tbl'
            tzone = 'P'
        END IF
        IF ( origc(1:4) .eq. 'CPHC' ) THEN
            table = 'wndprbc.tbl'
            tzone = 'H'
        END IF
        tbtyp = 'hcnadv'
        CALL FL_TBOP ( table, tbtyp, lun, iret)
        IF ( iret .eq. 0 ) THEN
            DO ii = 1, 5
                DO jj = 1, 32
                    READ (lun, *) ip1, ip2, ip3, ip4, ip5, ip6, ip7, ip8
                    prob(ii,jj,1) = ip1
                    prob(ii,jj,2) = ip2
                    prob(ii,jj,3) = ip3
                    prob(ii,jj,4) = ip4
                    prob(ii,jj,5) = ip5
                    prob(ii,jj,6) = ip6
                    prob(ii,jj,7) = ip7
                    prob(ii,jj,8) = ip8
                END DO
            END DO
            CALL FL_REWD ( lun, iret )
            CALL FL_CLOS ( lun, iret )
        END IF
C
C*	Set the time/date, max winds at 3, 12, 24, 36, 48 and 72 hrs.
C
        proces = .true.
        IF ( iret .eq. 0 ) THEN
            itwnd = 1
            DO jj = 1, 5
                IF ( mxwd(jj,nstrm) .lt. 0)  mxwd(jj,nstrm) = 0
 	        fctwnd(jj) = mxwd(jj,nstrm)
                fctwnd(jj) = NINT( FLOAT( fctwnd(jj) ) / 5.0 )+1
                itwnd = itwnd + 1
            END DO
        END IF
C
C*          Check if there is a forecast at 12H.
C
        DO ii = 1, 5
            IF ( (ii .eq. 1 ) .and. ( fctwnd (ii) .eq. 0) )
     +                proces = .false.
        END DO
C
C*      Write out probabilities.
C
        IF ( proces ) THEN
            DO jtime = 1,5
               DO jcat = 1,8
                    ivalue =  prob(jtime,fctwnd(jtime),jcat)
                 IF ( ivalue .eq. -1 ) THEN 
                     ipnum( jtime,jcat) = -1
                   ELSE IF ( ivalue .le. 1 ) THEN 
                     ipnum( jtime,jcat) = 1
                   ELSE IF ( ivalue .eq. 2 ) THEN 
                     ipnum( jtime,jcat) = 2
                   ELSE IF ( ivalue .eq. 3 ) THEN 
                     ipnum( jtime,jcat) = 3
                   ELSE IF ( ivalue .ge. 99 ) THEN 
                     ipnum( jtime,jcat) = 99
                   ELSE IF ( ivalue .ge. 98 ) THEN 
                     ipnum( jtime,jcat) = 98
                   ELSE IF ( ivalue .ge. 97 ) THEN 
                     ipnum( jtime,jcat) = 97
                   ELSE  
                    ipnum( jtime,jcat) = 5 *
     +                    (int((prob(jtime,fctwnd(jtime),jcat)+2.5)/5))
                 END IF
	       END DO
	    END DO
C
C*	    Draw the table.
C
            strnam = wname
            advnm = wadnm
            datstr = timstr
            CALL ST_LCUC ( strnam, strnam, ier )
	    CALL GH_WTBG ( origc, datstr, strnam, advnm, tzone, nstrm,
     +                     iwht, iblk, iret)
C
C*	    Plot the data values.
C
 	    CALL GH_WTPL ( ipnum, iblk, iret )
        END IF
C
C*      Plot NOAA logo.
C
        CALL GLOGO ( 'N', .90, .70, 2.5, 2, 1, ier )
C
C*      Plot NWS logo.
C
        CALL GLOGO ( 'N', .10, .70, 2.5, 2, 2, ier )
C
C*	Reset the saved attributes.
C
	CALL GSCOLR ( jcolr, ier )
        CALL GSSPCL ( szspcl, jspwid, ier )
        CALL GSLINE ( jltyp, jlthw, jwidth, jwhw, ier )
        CALL GSTEXT ( jtxfn, jtxhw, siztx, jtxwid, jbrdr,
     +                jrrotn, jjust, ier )
C*
        RETURN
	END
