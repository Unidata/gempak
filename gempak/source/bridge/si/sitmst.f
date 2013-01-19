	SUBROUTINE SI_TMST ( isffln, dattim, stid, addstn, slat, slon,
     +			     iret )
C************************************************************************
C* SI_TMST								*
C*									*
C* This subroutine sets the time and station in a surface data file.	*
C* A station not already in the file will be added only if ADDSTN is	*
C* set.  A time not already in the file will be added.			*
C*									*
C* SI_TMST  ( ISFFLN, DATTIM, STID, ADDSTN, SLAT, SLON, IRET )		*
C*									*
C* Input parameters:							*
C*	ISFFLN		INTEGER		Sounding file number		*
C*	DATTIM		CHAR*15		Nominal date/time		*
C*	STID		CHAR*		Station identifier		*
C*	ADDSTN		LOGICAL		Add station flag		*
C*	SLAT		REAL		Latitue				*
C*	SLON		REAL		Longitude			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -3 = time cannot be set	*
C*					 -4 = station cannot be set	*
C**									*
C* Log:									*
C* T. Lee/SAIC		 8/02	Created
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	dattim, stid
	LOGICAL		addstn
C*
	CHARACTER	stat*4, coun*4
C*
	DATA		stat, coun  / 2 * '--' /  
	DATA		selv  / 0. /
C------------------------------------------------------------------------
	iret   = 0
	ispri = 0
C
C*	Find the time in the file.
C
	CALL SF_FTIM  ( isffln, dattim, iret )
C
C*	If time is not found, add time.
C
	IF  ( iret .ne. 0 )  THEN
	    CALL SF_ATIM  ( isffln, dattim, iret )
	    IF  ( iret .eq. 0 )  THEN
		CALL SF_FTIM  ( isffln, dattim, ier )
	      ELSE
		iret = -3
		RETURN
	    END IF
	END IF
C
C*	Find the station.
C
	CALL SF_FSTN  ( isffln, stid, iret )
C
C*	If station is not found, add to file.
C
	IF  ( ( iret .ne. 0 )  .and. ( addstn ) )  THEN
	    CALL ST_NUMB ( stid, istnum, iret )		
	    CALL ST_LSTR ( stid, istr, iret )
	    IF  ( istr .eq. 3 )  stid = '0' // stid
	    CALL SF_ASTN  ( isffln, 1, stid, istnum, slat, slon, selv,
     +			    stat, coun, ispri, nadd, iret )
	    IF  ( iret .eq. 0 )  THEN
		CALL SF_FSTN  ( isffln, stid, ier )
	      ELSE
		iret  = -4
		RETURN
	    END IF
	END IF
C*
	RETURN
	END
