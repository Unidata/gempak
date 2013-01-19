        SUBROUTINE MA_GEMP ( gemfil, stntbl, iadstn, maxtim, rpttim,
     +                       lunf, nparm, parms, iret )
C************************************************************************
C* MA_GEMP                                                              *
C*                                                                      *
C* This subroutine rounds the report time to the nearest hour (for non- *
C* ships) or to the preceding 6-hourly interval (for ships), converts   *
C* the report time to GEMPAK format, and opens the GEMPAK file for      *
C* output.                                                              *
C* 								        *
C* MA_GEMP ( GEMFIL, STNTBL, IADSTN, MAXTIM, RPTTIM, LUNF, NPARM,       *
C*           PARMS, IRET )                                              *
C*								        *
C* Input parameters:                                                    *
C*      GEMFIL         CHAR*      Output file name template		*
C*      STNTBL         CHAR*      Marine station table			*
C*      IADSTN         INTEGER    Flag for 6-hourly file                *
C*      MAXTIM         INTEGER    Number of times allowed           	*
C*								        *
C* Output parameters:						        *
C*      RPTTIM         CHAR*      Report time in GEMPAK format		*
C*      LUNF           INTEGER    Unit number for GEMPAK file		*
C*      NPARM          INTEGER    Number of parameters			*
C*      PARMS	       CHAR*      List of parameters			*
C*      IRET           INTEGER    Return code                       	*
C*                                  0 = normal return			*
C*                                 -1 = report time invalid		*
C*                                 -2 = GEMPAK file problem		*
C**                                                                     *
C* Log:							                *
C* R. Hollern/NCEP       6/96                                           *
C* K. Tyle/GSC		 4/97	Cleaned up				*
C* D. Kidwell/NCEP	10/97	Cleaned up				*
C* D. Kidwell/NCEP	 4/00	Added text output                       *
C* D. Kidwell/NCEP	 5/00	Added 6-hour ship processing;cleaned up *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
        INCLUDE		'macmn.cmn'
C*
        CHARACTER*(*)   gemfil, stntbl, rpttim, parms 
C*
        CHARACTER	filnam*132
        INTEGER   	krptdt(5)
C-----------------------------------------------------------------------
        iret = 0
C
C*	Adjust the observation time depending on the report type,
C
	IF ( iadstn .ne. 6 ) THEN
C
C*          This report is to be saved in an hourly file.
C*	    Round the observation time to the nearest hour.
C
            DO i = 1, 4
	        krptdt ( i ) = irptdt ( i )
            END DO
C
	    jmins = irptdt ( 5 )
            krptdt ( 5 ) = 0
C
            IF ( jmins .ge. 45 ) THEN
	        krptdt ( 4 ) = krptdt ( 4 ) + 1
	        IF ( krptdt ( 4 ) .ge. 24 ) THEN
		    krptdt ( 4 ) = 0
C
C*                  Add one day to report time.
C
                    CALL TI_ADDD ( krptdt, krptdt, ier )
	        END IF 
            END IF 
	  ELSE
C
C*	    This is a ship report.  Adjust the time to the nearest 
C*	    preceding 6-hourly interval.
C
	    CALL MA_ADJT ( krptdt, ier )
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
C*      Make a file name from the template and the time.
C*	Open the file as a part of the open file list.
C
        CALL FL_MNAM ( rpttim, gemfil, filnam, ier )
        iflsrc = MFSHIP + MFTEXT
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
        END IF 
C*
	RETURN
	END
