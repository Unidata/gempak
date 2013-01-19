	SUBROUTINE MN_FCTM ( bultin, date, ibpnt, gemftm, iret )
C************************************************************************
C* MN_FCTM								*
C*									*
C* This subroutine determines the forecast times for the NGM MOS report *
C* and converts them into GEMPAK format.				*
C*									*
C* MN_FCTM ( BULTIN, DATE, IBPNT, GEMFTM, IRET )			*
C*									*
C* Input parameters:							*
C*	BULTIN		CHAR*		NGM MOS bulletin		*
C*	DATE		CHAR*		Model date			*
C*									*
C* Input and output parameters:						*
C*	IBPNT		INTEGER		Pointer to bulletin		*
C*									*
C* Output parameters:							*
C*	GEMFTM(*)	CHAR*		Forecast times in GEMPAK format *
C*	IRET		INTEGER		Return Code			*
C*					  -3 = date cannot be decoded	*
C**									*
C* Log:									*
C* D. Keiser/GSC	 2/96						*
C* D. Keiser/GSC	 5/96		Declared FCSTTM as integer	*
C* F. J. Yen/NCEP	11/98		Restructured based on AV_FCTM	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	INTEGER		FCSTTM
	PARAMETER       ( FCSTTM = 19 )
C*
	CHARACTER*(*)	bultin, date, gemftm(*)
C
	INTEGER		idtarr(5), iymd(3)
	CHARACTER	tmpstr*3, chhour*4, chutc*3, chday*3
	DATA		chhour / 'HOUR' /
	DATA		chutc / 'UTC' /, chday / 'DAY'/
C------------------------------------------------------------------------
	iret = 0
C
C*	Parse the date.	
C
	CALL ST_ILST ( date, '/', IMISSD, 3, iymd, inum, ier )
	IF ( ( ier .ne. 0 ) .or.  ( inum .ne. 3 ) ) THEN
             iret = -3
	     RETURN
	END IF
C
	IF ( iymd (3) .gt. 50 ) THEN
	     idtarr (1) = 1900 + iymd (3)
	  ELSE
	     idtarr (1) = 2000 + iymd (3)
	END IF
C
	idtarr (2) = iymd (1)
	idtarr (3) = iymd (2)
	idtarr (5) = 0
C
C*	Position to HOUR record.
C
	ilf = INDEX ( bultin,  chhour)
	IF ( ilf .eq. 0 ) THEN
	    ilf2 = INDEX ( bultin, chday )
	    ilf = INDEX ( bultin( ilf2: ), chutc )
	    ilf = ilf + ilf2 - 1
	END IF
	ibpnt = ilf + 6
C
C*	Convert the forecast times into GEMPAK format.
C
	DO i = 1, FCSTTM
	    tmpstr = bultin (ibpnt:(ibpnt+2))
	    CALL ST_NUMB ( tmpstr, iftim, ier )
	    idtarr (4) = iftim
	    IF ( iftim .eq. 0 ) THEN
	        CALL TI_ADDD ( idtarr, idtarr, ier )
	    END IF
	    CALL TI_ITOC ( idtarr, gemftm (i), ier )
	    ibpnt = ibpnt + 3
	END DO
	ibpnt = INDEX ( bultin ( ibpnt: ), CHLF ) + 1
C*
	RETURN
	END
