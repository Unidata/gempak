	SUBROUTINE RU_TMST  ( isnfln, dattim, istnm, stid, xlat, xlon, 
     +			      iret )
C************************************************************************
C* RU_TMST								*
C*									*
C* This subroutine sets the time and station in a sounding data file	*
C* which is being used for collection of real-time data.  If the time	*
C* is not already in the dataset, it will be added only if there is     *
C* room for more times.  If the station is not in the file, it will     *
C* also be added. The STID will be used if station number is set to 	*
C* IMISSD.								*
C*									*
C* The following error may occur when a time is added:			*
C*									*
C*     -800 + xx  where xx is the error from SN_ATIM			*
C*									*
C* RU_TMST  ( ISNFLN, DATTIM, ISTNM, STID, XLAT, XLON, IRET )		*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER		Sounding file number		*
C*	DATTIM		CHAR*15		Nominal date/time		*
C*	ISTNM		INTEGER		Station number			*
C*	STID		CHAR*		Station ID			*
C*	XLAT		REAL		Dropsonde latitude		*
C*	XLON		REAL		Dropsonde longitude		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = error in setting station	*
C*					 -5 = time could not be added   *
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/87						*
C* K. Brill/NMC		 8/93	stid*4 -> stid*8			*
C* K. Tyle/GSC		 5/97	Use RMISSD in DATA statement		*
C* D. Kidwell/NCEP	10/98	Do not delete earliest time		*
C* D. Kidwell/NCEP	 3/01	Added xlat, xlon to call; fixed prologue*
C* S. Chiswell/Unidata	 5/01	Added stid as passed parameter		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	dattim, stid
C*
	CHARACTER	stn*8, stat*4, coun*4
C*
	DATA		stat, coun / 2 * ' ' /  
	DATA		selv / RMISSD /
C------------------------------------------------------------------------
	iret = 0
C
C*	Find the time in the file.
C
	CALL SN_FTIM  ( isnfln, dattim, ier )
C
C*	If time is not found, add to file.
C
	IF  ( ier .ne. 0 )  THEN
C 
	    CALL SN_ATIM  ( isnfln, dattim, ier )
C
C*	    If no more times can be added, error return.
C
	    IF  ( ier .eq. -5 )  THEN
C 
		iret = ier
C
C*		Check for error other than no room from SN_ATIM.
C
	      ELSE IF  ( ier .ne. 0 )  THEN
		iret = ier - 800
	    END IF    
	END IF
C
C*	Return error if time could not be set.
C
	IF  ( iret .ne. 0 )  THEN  
	    RETURN
	  ELSE
	    CALL SN_FTIM  ( isnfln, dattim, ier )
	END IF
C
C*	Set the station.
C
	IF ( istnm .ne. IMISSD ) THEN
	    CALL ST_INCH  ( istnm, stn, ier )
	    CALL SN_FSTN  ( isnfln, stn, ier )
	  ELSE
	    CALL SN_FSTN  ( isnfln, stid, ier)
	END IF
C
C*	If station is not found, add to file.
C
	IF  ( ier .ne. 0 )  THEN
	    CALL SN_ASTN  ( isnfln, 1, stid, istnm, xlat, xlon, selv,
     +			    stat, coun, nadd, ier )
	END IF
C
C*	Return error if station could not be set.
C
	IF  ( ier .ne. 0 )  THEN
	    iret = -4
	    RETURN
	  ELSE
	    IF ( istnm .ne. IMISSD ) THEN
	        CALL SN_FSTN  ( isnfln, stn, ier )
	      ELSE
	        CALL SN_FSTN  ( isnfln, stid, ier)
	    END IF
	END IF
C*
	RETURN
	END
