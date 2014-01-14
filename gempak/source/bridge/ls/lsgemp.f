        SUBROUTINE LS_GEMP ( gemfil, stntbl, iadstn, maxtim, 
     +                       lunf, nparm, parms, iret )
C************************************************************************
C* LS_GEMP                                                              *
C*                                                                      *
C* This subroutine rounds the report time to the nearest hour, converts *
C* the report time to GEMPAK format, and opens the GEMPAK file for      *
C* output, and checks to see if the station is already in the output    *
C* file.                                                                *
C* 								        *
C* LS_GEMP ( GEMFIL, STNTBL, IADSTN, MAXTIM, LUNF, NPARM, PARMS, IRET ) *
C*								        *
C* Input parameters:                                                    *
C*      GEMFIL         CHAR*          Output file name template         *
C*      STNTBL         CHAR*          Synoptic land station table       *
C*      IADSTN         INTEGER        Number of additional stations     *
C*      MAXTIM         INTEGER        Max. # of times allowed in file   *
C*                                    GEMPAK output file                *
C*								        *
C* Output parameters:						        *
C*      LUNF           INTEGER        Unit number for GEMPAK file       *
C*      NPARM          INTEGER        Number of parms in packing table  *
C*      PARMS (*)      CHAR*          List of parms in packing table    *
C*      IRET           INTEGER        Return code                       *
C*                                      0 = Normal return               *
C*                                     -1 = Report time invalid         *
C*                                     -2 = GEMPAK file problem         *
C*                                     -3 = Duplicate report            *
C*                                     -4 = Station not in station table*
C*                                                                      *
C**                                                                     *
C* Log:							                *
C* R. Hollern/NCEP      6/96                                            *
C* D. Kidwell/NCEP      1/98	Cleaned up, modified for new interface  *
C* D. Kidwell/NCEP      3/98	Rounded time to 3 hrs, new call sequence*
C* A. Hardy/GSC         3/99    Added priority parameter to SF_QSTN     *
C* A. Hardy/GSC         3/99    Removed ispri = 0                       *
C* D. Kidwell/NCEP      4/00	Added text output                       *
C* S. Jacobs/NCEP	8/13	Added check for creating a 1 hour file	*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
        INCLUDE  	'lscmn.cmn'
C*
        CHARACTER*(*) 	gemfil, stntbl, parms(*)
C*
        CHARACTER       filnam*132, rpttim*12, stid*8, stat*2, coun*2
        INTEGER   	jrptdt(5), krptdt(5)
        LOGICAL   	addstn, cirflg, datflg
C-----------------------------------------------------------------------
        iret = 0
C
C*      Round the observation time to the nearest 3 hours.
C
        DO i = 1, 4
            krptdt ( i ) = irptdt ( i )
        END DO
C
        krptdt ( 5 ) = 0
        jhr = irptdt ( 4 )
C
C*	Check for the value of iadstn that indicates to create a
C*	1-hour file. Any other value will create a 3-hour file.
C
	IF ( iadstn .ne. 1 ) THEN
	    IF ( MOD ( jhr, 3 ) .eq. 1 ) THEN
		krptdt ( 4 ) = jhr - 1
	      ELSE IF ( MOD ( jhr, 3 ) .eq. 2 ) THEN
		krptdt ( 4 ) = jhr + 1
		IF ( krptdt ( 4 ) .ge. 24 ) THEN
		    DO i = 1, 3
			jrptdt ( i ) = krptdt ( i )
		    END DO
		    jrptdt ( 4 ) = 0
		    jrptdt ( 5 ) = 0
C
C*                  Add one day to report time.
C
		    CALL TI_ADDD ( jrptdt, krptdt, ier )
		END IF
	    END IF
	END IF
C
C*      Convert report observation time to GEMPAK time.
C
        CALL TI_ITOC ( krptdt, rpttim, ier )
C
        IF ( ier .ne. 0 ) THEN
            CALL DC_WLOG ( 2, 'TI', ier, ' ', ierr )
            iret = -1
            RETURN
        END IF
C
C*      Make a file name from the template and the date/time variables.
C
        CALL  FL_MNAM ( rpttim, gemfil, filnam, ier )
C
C*      Check if filnam is open, and if not, set the unit number for
C*      filnam to one from the list of open files or choose a new one.
C
        iflsrc = MFSYNP + MFTEXT
        CALL DC_FCYL ( filnam, iflsrc, stntbl, iadstn, maxtim, lunf,
     +                 nparm, parms, ier )
C
C*      Check that the file was opened properly.
C
        IF ( ier .ne. 0 ) THEN
C
C*          If not, write an error to the decoder log file.
C
            CALL DC_WLOG ( 0, 'SF', ier, filnam, ierr )
            iret = -2
            RETURN
        END IF
C
C*      Check if station is in GEMPAK station table.
C
        stid ( 1:5 ) = civals ( icstid ) ( 1:5 )
        stid ( 6:8 ) = '0'
C
        CALL SF_TSTN ( lunf, stid, ietstn )
C
        IF ( ietstn .eq. -10 ) THEN
	    CALL DC_WLOG ( 2, 'SF', ietstn, stid (1:5), ierr )
            iret = -4
            RETURN
        END IF
C
C*      Set the station and time in the output file.
C
        cirflg = .false.
        addstn = .true. 
C
        CALL DC_TMST ( lunf, rpttim, stid, addstn, cirflg, datflg, ier )
C
C*      Check for an error.
C
        IF ( ier .ne. 0 )  THEN
            iret = -2
            IF  ( ier .eq. -23 )  logmsg = rpttim
            IF  ( ier .eq. -24 )  logmsg = stid
            CALL DC_WLOG ( 4, 'SF', ier, logmsg (1:12), ierr )
	    RETURN
C
C*          If the data has already been decoded and this is not
C*          a correction, do not decode again.
C
          ELSE IF  ( datflg .and. ( rivals ( ircorn ) .eq. 0. ) ) THEN
            iret = -3
	    RETURN
        END IF
C
C*	Get country for decoding regional data.
C
	CALL SF_QSTN ( lunf, stid, istnm, slat, slon, selv, ispri, stat, 
     +		       coun, ier )
	IF ( ier .eq. 0 ) THEN
	    kcoun = coun
C
C*	    Store interface values for this station.
C
	    rivals ( irslat ) = slat
	    rivals ( irslon ) = slon
	    rivals ( irselv ) = selv
	    IF ( istnm .ne. IMISSD ) THEN
		istn = istnm / 10
		kblk = istn / 1000
		kstn = istn - ( 1000 * kblk ) 
		rivals ( irwmob ) = FLOAT ( kblk )
		rivals ( irwmos ) = FLOAT ( kstn )
C
C*              Set the wind units if they are not reported for 
C*		WMO blocks 70 -74.
C
		IF ( iuwind .eq. IMISSD ) THEN
		    IF ( ( kblk .le. 74 ) .and. ( kblk .ge. 70 )  ) 
     +                     iuwind = 4
                END IF
C
C*		Get WMO region number based on istn.
C
		IF ( istn .le. 19998 ) THEN
		    kwmo = 6
		  ELSE IF ( istn .ge. 20001 .and. istn .le. 20099 ) THEN
		    kwmo = 2
		  ELSE IF ( istn .le. 20199 ) THEN
		    kwmo = 6
		  ELSE IF ( istn .le. 21998 ) THEN
		    kwmo = 2
		  ELSE IF ( istn .ge. 22001 .and. istn .le. 22998 ) THEN
		    kwmo = 6
		  ELSE IF ( istn .ge. 23001 .and. istn .le. 25998 ) THEN
		    kwmo = 2
		  ELSE IF ( istn .ge. 26001 .and. istn .le. 27998 ) THEN
		    kwmo = 6
		  ELSE IF ( istn .ge. 28001 .and. istn .le. 32998 ) THEN
		    kwmo = 2
		  ELSE IF ( istn .ge. 33001 .and. istn .le. 34998 ) THEN
		    kwmo = 6
		  ELSE IF ( istn .ge. 35001 .and. istn .le. 36998 ) THEN
		    kwmo = 2
		  ELSE IF ( istn .ge. 37001 .and. istn .le. 37998 ) THEN
		    kwmo = 6
		  ELSE IF ( istn .ge. 38001 .and. istn .le. 39998 ) THEN
		    kwmo = 2
		  ELSE IF ( istn .ge. 40001 .and. istn .le. 40349 ) THEN
		    kwmo = 6
		  ELSE IF ( istn .le. 48599 ) THEN
		    kwmo = 2
		  ELSE IF ( istn .le. 48799 ) THEN
		    kwmo = 5
		  ELSE IF ( istn .le. 49998 ) THEN
		    kwmo = 2
		  ELSE IF ( istn .ge. 50001 .and. istn .le. 59998 ) THEN
		    kwmo = 2
		  ELSE IF ( istn .ge. 60001 .and. istn .le. 69998 ) THEN
		    kwmo = 1
		  ELSE IF ( istn .ge. 70001 .and. istn .le. 79998 ) THEN
		    kwmo = 4
		  ELSE IF ( istn .ge. 80001 .and. istn .le. 88998 ) THEN
		    kwmo = 3
		  ELSE IF ( istn .ge. 89001 .and. istn .le. 89998 ) THEN
		    kwmo = 7
		  ELSE IF ( istn .ge. 90001 .and. istn .le. 98998 ) THEN
		    kwmo = 5
	 	END IF
	    END IF
	    rivals ( irsuws ) = iuwind
	END IF
C*
	RETURN
	END
