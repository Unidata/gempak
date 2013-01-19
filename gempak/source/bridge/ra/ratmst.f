	SUBROUTINE RA_TMST  ( isffln, dattim, stid, addstn, cirflg,
     +			      datflg, iret )
C************************************************************************
C* RA_TMST								*
C*									*
C* This subroutine sets the time and station in a surface data file.	*
C* If the station has already reported, the flag DATFLG is set.		*
C* A station not already in the file will be added only if ADDSTN is	*
C* set.  A time not already in the file will be added if there is	*
C* room.  If there is no room and CIRFLG is set, the earliest time	*
C* in the file will be deleted.						*
C*									*
C* RA_TMST  ( ISFFLN, DATTIM, STID, ADDSTN, CIRFLG, DATFLG, IRET )	*
C*									*
C* Input parameters:							*
C*	ISFFLN		INTEGER		Sounding file number		*
C*	DATTIM		CHAR*15		Nominal date/time		*
C*	STID		CHAR*		Station identifier		*
C*	ADDSTN		LOGICAL		Add station flag		*
C*	CIRFLG		LOGICAL		Circular file flag		*
C*									*
C* Output parameters:							*
C*	DATFLG		LOGICAL		Data already in file flag	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = time cannot be set	*
C*					 -5 = station cannot be set	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/89	GEMPAK 5				*
C* J. Whistler/SSAI	 3/91	Changed STID from a *4 to a *(*)	*
C* K. Brill/NMC		 8/93	Added ISPRI for SF_ASTN			*
C* K. Tyle/GSC		 5/97	Initialize data as missing		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	dattim, stid
	LOGICAL		addstn, cirflg, datflg
C*
	CHARACTER	stat*4, coun*4
	CHARACTER*20    timlst (LLMXTM)
C*
	DATA		stat, coun  / 2 * ' ' /  
	DATA		slat, slon, selv  / 3 * RMISSD /
	DATA		istnm  / IMISSD /
C------------------------------------------------------------------------
	iret   = 0
	datflg = .false.
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
	    IF  ( iret .eq. 0 )  CALL SF_FTIM  ( isffln, dattim, ier )
C
C*	    If no more times can be added, delete earliest time.
C
	    IF  ( ( iret .eq. -4 ) .and. ( cirflg ) )  THEN
C
C*		Get list of times in the file.
C
		CALL SF_GTIM  ( isffln, LLMXTM, ntime, timlst, ier )
		IF  ( ier .eq. 0 )  THEN
C
C*		    Delete earliest time.
C
		    CALL SF_DTIM  ( isffln, timlst (1), iret )
C
C*		    Add new time.
C
		    IF  ( iret .eq. 0 )  THEN
			CALL SF_ATIM  ( isffln, dattim, iret )
			CALL SF_FTIM  ( isffln, dattim, iret )
		    END IF
		END IF
	    END IF
	END IF
C
C*	Return error if time could not be set.
C
	IF  ( iret .ne. 0 )  THEN  
	    iret = -4
	    RETURN
	END IF
C
C*	Set the station.
C
	CALL SF_FSTN  ( isffln, stid, iret )
C
C*	If station is not found, add to file.
C
	IF  ( ( iret .ne. 0 )  .and. ( addstn ) )  THEN
	    CALL SF_ASTN  ( isffln, 1, stid, istnm, slat, slon, selv,
     +			    stat, coun, ispri, nadd, iret )
	    CALL SF_FSTN  ( isffln, stid, ier )
	END IF
C
C*	Return error if station could not be set.
C
	IF  ( iret .ne. 0 )  THEN
	    iret = -5
	    RETURN
	END IF
C
C*	Check to see if station has already reported.
C
	CALL SF_QDAT  ( isffln, datflg, ier )
C*
	RETURN
	END
