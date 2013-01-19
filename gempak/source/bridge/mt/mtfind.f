	SUBROUTINE MT_FIND  ( bultin, nchar, ibpnt, bultyp,
     +			      mhr, mmin, iret )
C************************************************************************
C* MT_FIND							       	*
C*							 	       	*
C* This subroutine determines if the bulletin contains METAR or SPECI  	*
C* data (or neither).  It looks for an AWIPS identifier of the form     *
C* MTRxxx and for the characters METAR or SPECI either at the end of the*
C* bulletin header or at the start of the first report in the bulletin.	*
C* If the characters METAR or SPECI are found at the end of the header, *
C* a check is made to see if a date follows.                          	*
C*								       	*
C* MT_FIND ( BULTIN, NCHAR, IBPNT, BULTYP, MHR, MMIN, IRET )     	*
C*								       	*
C* Input parameters:						       	*
C*	BULTIN		CHAR*		Bulletin		       	*
C*	NCHAR		INTEGER		Pointer to end of bull header  	*
C*								       	*
C* Input and output parameters:					       	*
C*	IBPNT		INTEGER		Pointer in bulletin	       	*
C*								       	*
C* Output parameters:						       	*
C*	BULTYP		CHAR*		Bulletin type                  	*
C*	MHR		INTEGER 	Hour of METAR from bulletin    	*
C*	MMIN		INTEGER		Minutes of METAR from bulletin 	*
C*	IRET		INTEGER		Return code		       	*
C*					  0 = normal return	       	*
C**								       	*
C* Log:								       	*
C* Kidwell/NCEP 	03/96	Original author                      	*
C* Kidwell/NCEP 	08/96	Removed check for uncommissioned ASOS	*
C*                              (MTT) for operational execution      	*
C* K. Tyle/GSC		11/96	Assume METAR even if "METAR/SPECI" does *
C*				not appear in bulletin 			*
C* K. Tyle/GSC		 1/97	Reorganized header and comments		*
C* K. Tyle/GSC		 4/97	Always assign bultyp			*
C* D. Kidwell/NCEP 	 6/97	Added explicit char length specification*
C* D. Kidwell/NCEP       4/98   Removed obsolete ASOS check             *
C* D. Kidwell/NCEP       3/00   Added check for AWIPS id; cleaned up    *
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE 	'mtcmn.cmn'
C*
	CHARACTER*(*) 	bultin, bultyp
C*	
	CHARACTER 	hdrid*6
	LOGICAL		awipid
C------------------------------------------------------------------------
	iret   = 0
	bultyp = ' '
	mhr    = IMISSD
	mmin   = IMISSD
	nchr   = nchar
C
C*	Check for AWIPS id of form MTRxxx after header.  Reset pointer 
C*	if found.
C
	IF ( bultin ( nchr+1:nchr+3 ) .eq. 'MTR' ) THEN
	    awipid = .true.
	    DO ii = nchr+4, nchr+6
		CALL ST_ALNM ( bultin ( ii:ii ), ityp, jret )
		IF ( ityp .eq. 0 ) awipid = .false.
	    END DO
	    IF ( awipid ) THEN
	        nchr  = nchr + 9
	        ibpnt = ibpnt + 7
	    END IF
	END IF
C
	hdrid = bultin ( nchr + 1:nchr + 6)
	indxmt = INDEX ( hdrid, 'METAR' )
	indxsp = INDEX ( hdrid, 'SPECI' )
C
C*	See if we have found METAR or SPECI identification.
C
	indx = MAX ( indxmt, indxsp )
	IF ( indx .eq. 0 ) THEN
C
C*	    METAR/SPECI not found; assume METAR.
C
	    bultyp = 'MT'
	    ibpnt  = ibpnt + 2
C
	  ELSE IF ( ( hdrid ( 6:6 ) .eq. CHCR ) .or.
     +              ( ( hdrid ( 6:6 ) .eq. ' ' ) .and. 
     +		      ( bultin (nchr+7:nchr+7) .eq. CHCR ) ) )  THEN
C
C*	    Have carriage return, or space and carriage return,
C*	    following METAR or SPECI.  Set pointer and bulletin type.
C
	    bultyp = 'MT'
	    IF ( indxsp .ne. 0 ) bultyp = 'SP'
	    ibpnt = ibpnt + 8
	  ELSE
C 
C*	    Have either METAR or SPECI followed by a date, OR
C*	    METAR or SPECI as start of report.
C
	    nstart = nchr + 7
	    CALL ST_INTG ( bultin ( nstart:nstart + 3 ), intg, jret )
	    IF ( jret .eq. ( -2 ) ) THEN
C
C*		Not a time, so probably a station id / start of report.
C*		If report is a SPECI, this will be noted in MT_GRPT.
C
		ibpnt = ibpnt + 2
		bultyp = 'MT'
  	      ELSE
C
C*		Probable time field following chars METAR or SPECI.
C
		bultyp = 'MT'
		IF ( indxsp .ne. 0 ) bultyp = 'SP'
		irpnt = nstart
		CALL MT_DATE (bultin ( :irpnt + 6 ), irpnt, mda, mhr, 
     +                        mmin, idum, jret)
		ibpnt = ibpnt + 9 + ( irpnt - nstart )
	    END IF
	END IF
C*
	RETURN
	END
