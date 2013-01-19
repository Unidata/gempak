	SUBROUTINE GR_NDCP  ( np, axary, ndec, iret )
C************************************************************************
C* GR_NDCP								*
C*									*
C* This subroutine computes the number of decimal places to use with	*
C* an array of data.							*
C*									*
C* GR_NDCP  ( NP, AXARY, NDEC, IRET )					*
C*									*
C* Input parameters:							*
C*	NP		INTEGER		Number of data values		*
C*	AXARY (NP)	REAL		Data values			*
C*									*
C* Output parameters:							*
C*	NDEC		INTEGER		Number of decimal places	*
C*	IRET		INTEGER		Return code			*
C**									*
C* M. desJardins/GSFC	 7/89						*
C* M. desJardins/GSFC	 7/90	Check for large numbers before NINT	*
C* S. Jacobs/EAI	10/93	Renamed from CNDECP			*
C************************************************************************
	INCLUDE		'ERROR.PRM'
C*
	REAL		axary (*)
C------------------------------------------------------------------------
	iret = NORMAL
	ndec = 0
C
C*	First check for large values.
C
	DO  i = 1, np
	    IF  ( axary (i) .gt. 1.E5 )  THEN
		ndec = -1
		RETURN
	    END IF
	END DO
C
C*	Loop through all values checking for number of decimal places.
C
	DO  i = 1, np
	    iax    = NINT ( axary (i) )
	    dif1   = ABS ( FLOAT ( iax ) - axary (i) )
	    ax10   = axary (i) * 10.
	    iax10  = NINT ( ax10 )
	    dif10  = ABS ( FLOAT ( iax10 ) - ax10 )
	    ax100  = axary (i) * 100.
	    iax100 = NINT ( ax100 )
	    dif100 = ABS ( FLOAT ( iax100 ) - ax100 )
	    IF  ( ( ndec .le. 0 ) .and. 
     +		  ( dif1 .lt. .001 ) )  THEN
	      ELSE IF  ( ( ndec .le. 1 ) .and.
     +			 ( dif10 .lt. .01 ) )  THEN
		ndec = 1
	      ELSE IF  ( ( ndec .le. 2 ) .and.
     +			 ( dif100 .lt. .1 ) )  THEN
		ndec = 2
	      ELSE
		ndec = 3
		RETURN
	    END IF
	END DO
C*
	RETURN
	END
