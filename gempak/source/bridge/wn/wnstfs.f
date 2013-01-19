	SUBROUTINE WN_STFS  ( bultim, istarr, edttim, cnties, minalw,
     +			      mindif, bultin, errgrp, strtim, stptim,
     +			      county, numcty, iret )
C************************************************************************
C* WN_STFS								*
C*									*
C* This subroutine decodes the severe thunderstorm warning, tornado,	*
C* flash flood and severe local storm start/stop times and county	*
C* warning areas.							*
C*									*
C* WN_STFS ( BULTIM, ISTARR, EDTTIM, CNTIES, MINALW, MINDIF, BULTIN,	*
C*	     ERRGRP, STRTIM, STPTIM, COUNTY, NUMCTY, IRET)		*
C*									*
C* Input parameters:							*
C*	BULTIM          CHAR*		Issuing day and time            *
C*	ISTARR (5)	INTEGER         System date and time with year	*
C*	EDTTIM		CHAR*		Ending day and time		*
C*	CNTIES		CHAR*		Coded county ids		*
C*	MINALW		INTEGER		Valid time interval in minutes	*
C*	MINDIF		INTEGER		Default time difference in min	*
C*	BULTIN          CHAR*		WMO bulletin			*
C*	ERRGRP          CHAR*		Error group			*
C*									*
C* Output parameters:							*
C*	STRTIM		CHAR*		GEMPAK start date/time		*
C*	STPTIM		CHAR*		GEMPAK stop date/time		*
C*	COUNTY		CHAR*		County names in warning area    *
C*	NUMCTY		INTEGER         Number of warning counties	*
C*	IRET		INTEGER		Return code			*
C*					  2 = Probable bad end time	*
C*									*
C**									*
C* Log:									*
C* A. Hardy/GSC		11/00	Created from WN_TSTM			*
C* F. J. Yen/NCEP	 1/01	Removed unnecessary wncmn.cmn include	*
C* F. J. Yen/NCEP	 3/02	Fixed year & month for issue time; check*
C*				for invalid end time and used DC_ITIM.	*
C* M. Li/SAIC		08/02	WN_CNTY -> BR_CNTY			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)   bultim, edttim, cnties, county, strtim, stptim
	CHARACTER*(*)	bultin, errgrp
        INTEGER		istarr(5)
C*
	CHARACTER	errstr*80
	INTEGER		jtarr(5), jotarr (5)
C------------------------------------------------------------------------
	iret = 0
C
C*      Use the times from the bulletin header. 
C*      Create the start time string from the issue time.
C
	CALL ST_NUMB ( bultim(1:2), irday, ier )
	CALL ST_NUMB ( bultim(3:4), irhour, ier )
	CALL ST_NUMB ( bultim(5:6), irmin, ier )
	CALL DC_ITIM ( istarr, irday, irhour, irmin, jtarr, ier )
	CALL TI_ITOC ( jtarr, strtim, ier )
C
C*      Create the end time string.  If end time is not reasonable
C*	(time difference is greater than minalw) or not valid,
C*      use start time + mindif.
C
	IF ( edttim .eq. '999999' ) THEN
	    CALL TI_ADDM ( jtarr, mindif, jotarr, ier )
	  ELSE
	    CALL ST_NUMB ( edttim(1:2), irday, ier )
	    CALL ST_NUMB ( edttim(3:4), irhour, ier )
	    CALL ST_NUMB ( edttim(5:6), irmin, ier )
	    CALL DC_ITIM ( istarr, irday, irhour, irmin, jotarr, ier )
	    CALL TI_MDIF ( jotarr, jtarr, nmin, ier )
	    IF ( nmin .gt. minalw .or. nmin .le. 0 ) THEN
		CALL TI_ADDM ( jtarr, mindif, jotarr, ier ) 
		CALL ST_UNPR ( bultin (:80), 80, errstr, len1, ier )
		iret = 2
		CALL DC_WLOG ( 2, errgrp, iret, errstr(:len1), ier )
	    END IF
	END IF
	CALL TI_ITOC ( jotarr, stptim, ier )
C
C*	Find the counties that are under the warning.
C
	CALL BR_CNTY ( cnties, county, numcty, iret ) 
C*
	RETURN
	END
