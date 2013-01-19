	SUBROUTINE GR_LABL  ( cval, iform, idec, text, nchar, iret )
C************************************************************************
C* GR_LABL								*
C*									*
C* This subroutine converts real numbers into text strings.		*
C*									*
C* GR_LABL  ( CVAL, IFORM, IDEC, TEXT, NCHAR, IRET )			*
C*									*
C* Input parameters:							*
C*	CVAL 		REAL		Value to encode			*
C*	IFORM		INTEGER		Format to use			*
C*					  0 = select in subroutine	*
C*					  1 = I format			*
C*					  2 = F format using IDEC	*
C*					  3 = PE format			*
C*	IDEC		INTEGER		Number of decimal places	*
C*					  0 = select in subroutine	*
C*									*
C* Output parameters:							*
C*	TEXT 		CHAR*		Encoded value			*
C*	NCHAR		INTEGER		Number of characters in TEXT	*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* G. Chatters/RDS	 9/82	CPTLBL					*
C* M. desJardins/GSFC	 7/85	Adapted from AOIPS code for GEMPAK 3.1	*
C* M. desJardins/GSFC	 3/89	Replaced ENCODE and <> in FORMATS	*
C* M. desJardins/GSFC	 5/89	Simplify algorithm			*
C* M. desJardins/GSFC	 6/89	CLABEL for GEMPAK 5			*
C* M. desJardins/GSFC	 7/90	Check for large numbers before NINT	*
C* S. Jacobs/EAI	10/93	Renamed from CLABEL			*
C************************************************************************
	CHARACTER*(*)	text
C------------------------------------------------------------------------
	iret = 0
C
C*	Check for form to use:
C
	aval  = ABS  ( cval )
C
C*	Check for numbers too large or too small to use F or I format.
C
	IF  ( ( aval .ge. 100000. ) .or. 
     +	      ( ( aval .lt. .001 ) .and. ( aval .ne. 0. ) ) )  THEN
	    jform = 3
	  ELSE
	    icval = NINT ( aval )
	    jform = iform
	    jdec  = idec
C
C*	Choose appropriate form.
C
	    IF  ( jform .le. 0 )  THEN
		IF  ( cval .eq. 0. )  THEN
		    jform  = 1
		  ELSE IF  ( aval .ge. 100. )  THEN
		    jform  = 1
		  ELSE IF  ( aval .eq. FLOAT ( icval ) )  THEN
		    jform  = 1
		  ELSE
		    jform  = 2
		END IF
	    END IF
	END IF
C
C*	Encode value.
C
	IF  ( jform .eq. 1 )  THEN
	    icval = NINT ( cval )
	    CALL ST_INCH  ( icval, text, ier )
	  ELSE IF  ( jform .eq. 2)  THEN 
	    IF  ( jdec .le. 0 )  jdec = 3
	    CALL ST_RLCH  ( cval, jdec, text, ier )
	    CALL ST_LSTR  ( text, nchar, ier )
	    ichars = nchar
	  ELSE IF  ( jform .eq. 3 )  THEN
	    WRITE  ( text, 2, IOSTAT = iostat ) cval 
2	    FORMAT ( 1PE8.1 )
	END IF
C
C*	If format was optional, eliminate trailing zeros.
C
	IF  ( ( jform .eq. 2 ) .and. ( idec .le. 0 ) )  THEN
	    DO WHILE  ( ( ichars .gt. 2 ) .and. 
     +			( ( text (ichars:ichars) .eq. '0' ) .or.
     +			  ( text (ichars:ichars) .eq. '.' ) ) )
		text (ichars:ichars) = ' '
		ichars = ichars - 1
	    END DO
	END IF
C
C*	Get number of characters to return.
C
	CALL ST_LSTR  ( text, nchar, ier )
C*
	RETURN
	END
