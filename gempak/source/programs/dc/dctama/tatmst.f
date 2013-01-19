	SUBROUTINE TA_TMST  ( isnfln, dattim, acrn, iacrn, alat, alon,
     +				aelv, addstn, cirflg, datflg, iret )
C************************************************************************
C* TA_TMST								*
C*									*
C* This subroutine sets the time and station in a soundings data file.	*
C* If the station has already reported, the flag DATFLG is set.		*
C* A station not already in the file will be added only if ADDSTN is	*
C* set.  A time not already in the file will be added if there is	*
C* room.  If there is no room and CIRFLG is set, the earliest time	*
C* in the file will be deleted.						*
C*									*
C* TA_TMST  ( ISNFLN, DATTIM, ACRN, IACRN, ALAT, ALON, AELV, ADDSTN,	*
C*						 CIRFLG, DATFLG, IRET )	*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER		Sounding file number		*
C*	DATTIM		CHAR*		Nominal date/time		*
C*	ACRN            CHAR*           Aircraft identifier             *
C*      IACRN           INTEGER         Aircraft number                 *
C*	ALAT		REAL		Aircraft latitude		*
C*	ALON		REAL		Aircraft longitude		*
C*	AELV		REAL		Aircraft elevation		*
C*	ADDSTN		LOGICAL		Add new aircraft?		*
c*	CIRFLG		LOGICAL		Remove oldest time?		*
C* Output paramteres:							*
C*	DATFLG		LOGICAL		Data present in file?		*
C*	IRET		INTEGER		Return status code		*
C**									*
C* Log:									*
C* T. Piper/SAIC	09/08		Modeled after dctmst.f		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	dattim, acrn 
	LOGICAL		addstn, cirflg, datflg
C*
	CHARACTER	stat*4, coun*4
	CHARACTER*20    timlst (LLMXTM)
C*
	DATA		stat, coun  / 2 * ' ' /  
C------------------------------------------------------------------------
	iret   = 0
	datflg = .false.
	ispri = 0
C
C*	Find the time in the file.
C
	CALL SN_FTIM  ( isnfln, dattim, iret )
C
C*	If time is not found, add time.
C
	IF  ( iret .ne. 0 )  THEN
	    CALL SN_ATIM  ( isnfln, dattim, iret )
	    IF  ( iret .eq. 0 )  THEN
		CALL SN_FTIM  ( isnfln, dattim, ier )
	    END IF
C
C*	    If no more times can be added, delete earliest time.
C
	    IF  ( ( iret .eq. -4 ) .and. ( cirflg ) )  THEN
C
C*		Get list of times in the file.
C
		CALL SN_GTIM  ( isnfln, LLMXTM, ntime, timlst, ier )
		IF  ( ier .eq. 0 )  THEN
C
C*		    Delete earliest time.
C
		    CALL SN_DTIM  ( isnfln, timlst(1), iret )
C
C*		    Add new time.
C
		    IF  ( iret .eq. 0 )  THEN
			CALL SN_ATIM  ( isnfln, dattim, iret )
			CALL SN_FTIM  ( isnfln, dattim, iret )
		    END IF
		END IF
	    END IF
	END IF
C
C*	Return error if time could not be set.
C
	IF  ( iret .ne. 0 )  THEN  
	    iret = -23
	    RETURN
	END IF
C
C*	Set the station.
C
	CALL SN_FSTN  ( isnfln, acrn, iret )
C
C*	If station is not found, add to file.
C
	IF  ( ( iret .ne. 0 )  .and. ( addstn ) )  THEN
	    CALL SN_ASTN  ( isnfln, 1, acrn, iacrn, alat, alon, aelv,
     +			    stat, coun, nadd, iret )
	    CALL SN_FSTN  ( isnfln, acrn, ier )
	END IF
C
C*	Return error if station could not be set.
C
	IF  ( iret .ne. 0 )  THEN
	    iret = -24
	    RETURN
	END IF
C
C*	Check to see if station has already reported.
C
	CALL SN_QDAT  ( isnfln, datflg, ier )
C*
	RETURN
	END
