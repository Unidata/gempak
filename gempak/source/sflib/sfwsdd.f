	SUBROUTINE SF_WSDD  ( isffln, dattim, stid, istnm, slat, slon,
     +			      selv,   stat,   coun, ihhmm, sfdata,
     +			      iret )
C************************************************************************
C* SF_WSDD								*
C*									*
C* This subroutine adds a station header and station data to a 		*
C* ship surface data file.						*
C*									*
C* SF_WSDD  ( ISFFLN, DATTIM, STID,   ISTNM, SLAT, SLON, SELV, STAT,	*
C*            COUN,   IHHMM,  SFDATA, IRET )				*
C*									*
C* Input parameters:							*
C*	ISFFLN		INTEGER		Surface file number		*
C*	DATTIM		CHAR*		GEMPAK time			*
C*	STID		CHAR*8		Station identifier		*
C*	ISTNM		INTEGER		Station number			*
C*	SLAT		REAL		Station latitude		*
C*	SLON		REAL		Station longitude		*
C*	SELV		REAL		Station elevation		*
C*	STAT		CHAR*2	 	State				*
C*	COUN		CHAR*2		Country				*
C*	IHHMM		INTEGER		Station time  (HHMM)		*
C*	SFDATA (NPARM)	REAL		Station data			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				    	   0 = normal return		*
C*				   	  -3 = file not open		*
C*				   	  -5 = too many reports		*
C*					 -12 = DM error			*
C*					 -20 = invalid time		*
C*				  	 -21 = not single dim file	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 2/88						*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* K. Brill/NMC		 8/93	Changes for 8-char stn ID		*
C* D. Kidwell/NCEP	 7/99	Save row, col to common                 *
C* S. Jacobs/NCEP	 2/01	Added error check after DM_WCLH		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sfcmn.cmn'
C*
	CHARACTER*(*)	stid, stat, coun, dattim
	REAL		sfdata (*)
C*
	INTEGER		iheadr (MMKEY), idthdr (LLSTHL), istid (2)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	CALL SF_CHKF  ( isffln, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Check that times and stations are intermixed.
C
	IF  ( dttype (isffln) .ne. sttype (isffln) )  THEN
	    iret = -21
	    RETURN
	END IF
C
C*	Initialize header.
C
	DO i = 1, MMKEY
	    iheadr (i) = IMISSD
	END DO
C
C*	Put time information into header.
C
	CALL TI_IDTM  ( dattim, iheadr ( kdate (isffln) ),
     +				iheadr ( ktime (isffln) ), ier )
	IF  ( ier .ne. 0 )  THEN
	    iret = -20
	    RETURN
	END IF
C
C*	Put station information into header.
C
	CALL ST_STOI  ( stid, 8, nv, istid, ier )
	IF  ( kstid (isffln) .gt. 0 )
     +			iheadr ( kstid (isffln) ) = istid (1)
	IF  ( kstd2 (isffln) .gt. 0 )
     +			iheadr ( kstd2 (isffln) ) = istid (2)
	IF  ( kstnm (isffln) .gt. 0 )
     +			iheadr ( kstnm (isffln) ) = istnm
	IF  ( kslat (isffln) .gt. 0 )  THEN
	    IF  ( ERMISS ( slat ) )  THEN
		islat = IMISSD
	      ELSE
		islat = slat * 100
	    END IF
	    iheadr ( kslat (isffln) ) = islat
	END IF
C*
	IF  ( kslon (isffln) .gt. 0 )  THEN
	    IF  ( ERMISS ( slon ) )  THEN
		islon = IMISSD
	      ELSE
		islon = slon * 100
	    END IF
	    iheadr ( kslon (isffln) ) = islon
	END IF
C*
	IF  ( kselv (isffln) .gt. 0 )  THEN
	    IF  ( ERMISS (selv) )  THEN
		iselv = IMISSD
	      ELSE
		iselv = selv 
	    END IF
	    iheadr ( kselv (isffln) ) = iselv
	END IF
C*
	CALL ST_CTOI  ( stat, 1, istat, ier )
	IF  ( kstat (isffln) .gt. 0 ) iheadr ( kstat (isffln) ) = istat
	CALL ST_CTOI  ( coun, 1, icoun, ier )
	IF  ( kcoun (isffln) .gt. 0 ) iheadr ( kcoun (isffln) ) = icoun
C
C*	Add header to file.
C
	CALL DM_WCLH  ( isffln, 0, iheadr, icol, ier )
	IF  ( ier .ne. 0 )  THEN
	    iret = -12
	    RETURN
	END IF
	kcol ( isffln ) = icol
	krow ( isffln ) = 1
C
C*	Write data to the file.
C
	irow = 1
	idthdr (1) = ihhmm
	CALL DM_WDTR  ( isffln, irow, icol, 'SFDT', ihhmm, sfdata, 
     +			kparm (isffln), ier )
	IF  ( ier .ne. 0 )  iret = -12
C*
	RETURN
	END

