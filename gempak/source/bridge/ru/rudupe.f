	SUBROUTINE RU_DUPE  ( isnfln, dattim, istnm, stid, part, ihhmm, 
     +                        nlev, data, zwind, iret )
C************************************************************************
C* RU_DUPE								*
C*									*
C* This subroutine checks to see if the station, time and part are      *
C* already in the output file.                                          *
C*									*
C* RU_DUPE  ( ISNFLN, DATTIM, ISTNM, STID, PART, IHHMM, NLEV, DATA,	*
C*	      ZWIND, IRET )						*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER		Sounding file number		*
C*	DATTIM		CHAR*15		Nominal date/time		*
C*	ISTNM		INTEGER		Station number			*
C*	STID		CHAR*8		Station ID			*
C*	PART		CHAR*4		Part name                       *
C*	IHHMM		INTEGER		Station time (HHMM)             *
C*									*
C* Output parameters:							*
C*	NLEV		INTEGER		Number of levels		*
C*	DATA(3,*)	REAL		Sounding data array		*
C*	ZWIND		LOGICAL		Flag for sig wind in z coord	*
C*	IRET		INTEGER		Return code			*
C*					  1 = already in file           *
C*					  0 = normal return		*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 2/01	                              		*
C* S. Chiswell/Unidata	 5/01	Added stid to calling sequence       	*
C* m.gamazaychikov/SAIC	07/05	Added nlev, data, zwind to calling seq	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	dattim, part, stid
	LOGICAL		zwind
	REAL		data (3,*)
C*
	CHARACTER	stn*8
C------------------------------------------------------------------------
	iret = 0
C
C*	Find the time in the file.
C
	CALL SN_FTIM ( isnfln, dattim, ier )
C
C*	If time is not found, return.
C
	IF ( ier .ne. 0 ) RETURN 
C 
C*	Find the station in the file.
C
	IF ( istnm .ne. IMISSD ) THEN
	   CALL ST_INCH  ( istnm, stn, ier )
	   CALL SN_FSTN  ( isnfln, stn, ier )
	ELSE
	   CALL SN_FSTN  ( isnfln, stid, ier )
	END IF
C
C*	If station is not found, return.
C
	IF ( ier .ne. 0 ) RETURN
C
C*	If both time and station were found, check for matches on part
C*	and report time.
C
	CALL SN_RPRT ( isnfln, part, jhhmm, nlev, data, zwind, ier )
	IF ( ier .eq. 0 ) THEN 
	    IF ( ihhmm .eq. jhhmm ) iret = 1
	END IF
C*
	RETURN
	END
