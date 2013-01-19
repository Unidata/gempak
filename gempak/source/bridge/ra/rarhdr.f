	SUBROUTINE RA_RHDR  ( irpntr, stid, rpttyp, corflg, autotp,
     +			      ihour, iminut, iret )
C************************************************************************
C* RA_RHDR								*
C*									*
C* This subroutine gets the header information from an airways		*
C* report.								*
C*									*
C* RA_RHDR  ( IRPNTR, STID, RPTTYP, CORFLG, AUTOTP, IHOUR, IMINUT,	*
C*            IRET )							*
C*									*
C* Output parameters:							*
C*	IRPNTR		INTEGER		First field after header	*
C*	STID		CHAR*		Station identifier		*
C*	RPTTYP		CHAR*		Report type			*
C*	CORFLG		LOGICAL		Correction flag			*
C*	AUTOTP		CHAR*		Automatic station type		*
C*	IHOUR		INTEGER		Hour				*
C* 	IMINUT		INTEGER		Minute				*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -2 = incomplete report		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/89	GEMPAK 5				*
C* P. Bruehl/Unidata	 3/94	Added ASOS to AUTOTP			*
C************************************************************************
	INCLUDE		'racmn.cmn'
C*
	CHARACTER*(*)	stid, rpttyp, autotp
	LOGICAL		corflg
C------------------------------------------------------------------------
	iret   = 0
	corflg = .false.
	stid   = ' '
	rpttyp = ' '
	autotp = ' '
C
C*	Exit if there are not at least four fields.
C
	IF  ( nfield .lt. 4 )  THEN
	    iret = -2
	    RETURN
	END IF
C
C*	The first field is the station id.
C
	stid = cfield (1)
C
C*	Check for FINO.
C
	CALL ST_FIND  ( 'FINO', cfield, nfield, iposf, ier )
	CALL ST_FIND  ( 'MISSING', cfield, nfield, iposm, ier )
	IF  ( ( ( iposf .gt. 0 ) .and. ( iposf .lt. 6 ) ) .or.
     +	      ( ( iposm .gt. 0 ) .and. ( iposm .lt. 6 ) ) )  THEN
	    iret = -2
	    RETURN
	END IF
C
C*	The second field is the report type unless it is COR.
C
	IF  ( cfield (2) .eq. 'COR' )  THEN
	    corflg = .true.
	    rpttyp = cfield (3)
	    irpntr = 4
	  ELSE
	    rpttyp = cfield (2)
	    IF  ( cfield (3) .eq. 'COR' )  THEN
		corflg = .true.
		irpntr = 4
	      ELSE
		irpntr = 3
	    END IF
	END IF
C
C*	The next field is the report time in the form HHMM.
C*	First get the field sizes.
C
	isiz1 = ifsize ( irpntr )
	IF  ( iftype ( irpntr + 1 ) .eq. 2 )  THEN
	    isiz2 = ifsize ( irpntr + 1 )
	  ELSE
	    isiz2 = 0
	END IF
C
C*	Decode time, putting reports together if necessary.
C
	IF  ( iftype ( irpntr ) .ne. 2 )  THEN
	    iret = -2
	    RETURN
	  ELSE IF  ( isiz1 .eq. 4 )  THEN
	    ihour  = ifintg ( irpntr ) / 100
	    iminut = ifintg ( irpntr ) - ihour * 100
	  ELSE IF  ( isiz1 + isiz2 .eq. 4 )  THEN
	    itime  = ifintg ( irpntr ) * 10 ** ( 4 - isiz1 ) +
     +		    ifintg ( irpntr + 1 )
	    ihour  = itime / 100
	    iminut = itime - ihour * 100
	    irpntr = irpntr + 1
	  ELSE
	    iret = -2
	    RETURN
	END IF
	irpntr = irpntr + 1
C
C*	Check for automatic station type in next field.
C
	IF  ( ( cfield (irpntr) ( :4) .eq. 'AUTO' ) .or.
     +	      ( cfield (irpntr) ( :5) .eq. 'RAMOS' ) .or.
     +	      ( cfield (irpntr) ( :4) .eq. 'AMOS' )  .or.
     +        ( cfield (irpntr) ( :4) .eq. 'AO' ) )  THEN
	    autotp = cfield (irpntr)
	    irpntr = irpntr + 1
C
C*	    Check for AUTOX.
C
	    IF  ( ( autotp .eq. 'AUTO' ) .and. 
     +		  ( iftype (irpntr ) .eq. 2 ) .and.
     +		  ( ifendp (irpntr-1) + 1 .eq. ifstrt (irpntr) ) )  THEN
		autotp = 'AUTO' // cfield (irpntr)
		irpntr = irpntr + 1
	    END IF
C
C*	    Check for ASOS: AO2 or AO2A
C
	    IF ( ( autotp .eq. 'AO' ) .and.
     +		 ( ifintg (irpntr) .eq. 2 ) ) THEN
		autotp = 'AO2'
		irpntr = irpntr + 1
	    	IF ( ( iftype(irpntr) .eq. 1 ) .and.
     +		     ( cfield (irpntr) .eq. 'A' ) ) THEN
		   autotp = 'AO2A'
		   irpntr = irpntr + 1
		END IF
	    END IF
C
	END IF
C*
	RETURN 
	END
