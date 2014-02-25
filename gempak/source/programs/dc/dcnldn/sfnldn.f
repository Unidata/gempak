	SUBROUTINE SFNLDN ( isffln, idate, itime, stid, slat, slon, 
     +				ihhmm, sfdata, iret )
C************************************************************************
C* SFNLDN								*
C*									*
C* This subroutine prepares the station header for adding to a NLDN	*
C* lightning data file.  Calls DMNLDN to do the writing to the file	*
C*									*
C* Generated from SF_WSDD by altering and then streamlining for real 	*
C* time decoding.							*
C*									*
C* SFNLDN ( ISFFLN, IDATE, ITIME, SLAT, SLON, IHHMM, SFDATA, IRET )	*
C*									*
C* Input parameters:							*
C*	ISFFLN		INTEGER		Surface file number		*
C*	IDATE		INTEGER		GEMPAK date			*
C*	ITIME		INTEGER		GEMPAK time			*
C*	STID		CHAR*4		Station symbol			*
C*	SLAT		REAL		Flash latitude			*
C*	SLON		REAL		Flash longitude			*
C*	IHHMM		INTEGER		Flash time  (HHMM)		*
C*	SFDATA (NPARM)	REAL		Flash data			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				    	   0 = normal return		*
C*				   	  -1 = Out of space in data file*
C*				   	  -2 = Error with data file	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 2/88						*
C* P. Bruehl/Unidata	 4/94   Modified for real time lightning ingest *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sfcmn.cmn'
	INCLUDE		'nldn.prm'
C*
	CHARACTER	error*160, stid*4
	REAL		sfdata (*)
C*
	INTEGER		iheadr (MMKEY)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
C
C*	If this is the FIRST time through this subroutine, make a 
C*	series of checks.  After the first time, do not do this again.
C
	IF ( first ) THEN
		icount = 0
		idata = 0
		ihead = 0
C
C*	Don't know position of next open header
C
		ipos = 0
C
C*	Check that file is open
C
		CALL SF_CHKF  ( isffln, iret )
		IF  ( iret .ne. 0 )  THEN
			iret = -2
			RETURN
		ENDIF
C
C*	Check that times and stations are intermixed.
C
		IF  ( dttype (isffln) .ne. sttype (isffln) )  THEN
		    iret = -2
		    RETURN
		END IF
C
C*	Check for valid number of parameters.
*	Put number of parameters in nldn.prm common
C
		nword = kparm( isffln )
		IF  ( nword .le. 0 ) THEN
			iret = -2
			RETURN
		ENDIF
C
C*	Done with FIRST time checks
C
	ENDIF
C
C*	Initialize header.
C
	DO i = 1, MMKEY
	    iheadr (i) = IMISSD
	END DO
C
C*	Put time information into header.
C
	iheadr ( kdate (isffln) ) = idate
	iheadr ( ktime (isffln) ) = itime
C
C* 	No elevation, state or country with lightning data
C*	No station information to put into header.
C*	Use " " blank for station id stid, and 0 for stnm
C
CC	istnm = 0
CC	stid = ' '
CC	CALL ST_CTOI  ( stid, 1, istid, ier )
CC	iheadr ( kstid (isffln) ) = istid
CC	iheadr ( kstnm (isffln) ) = istnm
	CALL ST_CTOI  ( stid, 1, istid, ier )
	iheadr ( kstid (isffln) ) = istid 
C
C*	Lat/Lon of flash
C
	islat = slat * 100
	islon = slon * 100
        iheadr ( kslat (isffln) ) = islat
        iheadr ( kslon (isffln) ) = islon
C
C*	CALL DMNLDN to write header, data, pointer to data, and
C*	data management block to the file
C
	CALL DMNLDN ( isffln, ihhmm, iheadr, sfdata, iret )
C
	IF ( iret .eq. -12 ) THEN
	  WRITE (error,*) "DMNLDN write error--out of space ", iret
	  CALL DC_WLOG (2, 'DCNLDN', iret, error, iout)
	  iret = -1
	  RETURN
	ELSE IF ( iret .eq. -10 ) THEN
	  WRITE (error,*) "DMNLDN problem with data file ", iret
	  CALL DC_WLOG (0, 'DCNLDN', iret, error, iout)
	  iret = -2
	  RETURN
	ELSE IF ( (iret .eq. -1) .or. (iret .eq. -2) ) THEN
	  WRITE (error,*) "DMNLDN tried writing data to file--",
     +      "unsuccesful.  Data file could be corrupt"
	  CALL DC_WLOG (0, 'DCNLDN', iret, error, iout)
	  iret = -2
	  RETURN
	ENDIF
C
	iret = 0
	RETURN
	END

