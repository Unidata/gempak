	SUBROUTINE GR_PACK  ( gpack, ipktyp, nbits, iret )
C************************************************************************
C* GR_PACK								*
C*									*
C* This subroutine decodes the user input for grid packing into the	*
C* packing type and number of bits / precision.  The valid packing	*
C* types are GRIB, DEC, DIF and NONE.  If the packing type is DEC,	*
C* NBITS is the precision; otherwise, NBITS is the number of bits.	*
C*									*
C* GRIB packing is the default with 16 bits.				*
C*									*
C* Default values:							*
C*		Packing type	Number of bits				*
C*		    GRIB	      16				*
C*		    DEC		       2 (decimal places)		*
C*		    DIF 	      16				*
C*		    NONE	      32				*
C*									*
C* GR_PACK  ( GPACK, IPKTYP, NBITS, IRET )				*
C*									*
C* Input parameters:							*
C*	GPACK		CHAR*		Packing type / number of bits	*
C*									*
C* Output parameters:							*
C*	IPKTYP		INTEGER		GEMPAK packing type		*
C*	NBITS		INTEGER		Number of bits			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 3/89						*
C* S. Jacobs/EAI	 3/93		Specify ranges for nbits for 	*
C*					  different pack types		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	gpack
C*
	CHARACTER	part(2)*12, temp*12
C------------------------------------------------------------------------
	iret   = 0
	ipktyp = MDGGRB
	nbits  = 16
C
C*	Return if the values are blank.
C
	IF  ( gpack .eq. ' ' )  RETURN
C
	CALL ST_LCUC  ( gpack, gpack, ier )
C
C*	Break the input string into two substrings.
C
	CALL ST_CLST  ( gpack, '/', ' ', 2, part, n, ier )
C
C*	Decode the integer.  Try the first string.
C
	CALL ST_NUMB  ( part (1), ival, ier )
	IF  ( part (1) .eq. ' ' )  ier = 0
C
C*	If the first part is not an integer, swap the words and
C*	try again.
C
	IF  ( ier .ne. 0 )  THEN
	    temp     = part (1)
	    part (1) = part (2)
	    part (2) = temp
	    CALL ST_NUMB  ( part (1), ival, ier )
	    IF  ( part (1) .eq. ' ' )  ier = 0
	END IF
C
C*	Get the input for packing type. Set the number of bits
C*	accordingly.
C
	IF  ( part (2) .eq. 'NONE' )  THEN
	    ipktyp = MDGNON
	    nbits  = 32
	ELSE IF  ( part (2) .eq. 'DEC' )  THEN
	    ipktyp = MDGDEC
	    IF  ( ( ival .ge. -5 ) .and. ( ival .le. 5 ) )  THEN
		nbits = ival
	    ELSE
		nbits = 2
	    END IF
	ELSE IF  ( part (2) .eq. 'DIF' )  THEN
	    ipktyp = MDGDIF
	    IF  ( ( ival .gt. 1 ) .and. ( ival .le. 32 ) )  THEN
		nbits = ival
	    ELSE
		nbits = 16
	    END IF
	ELSE IF  ( ( part (2) .eq. 'GRIB' ) .or.
     +		   ( part (2) .eq.    ' ' ) )  THEN
	    ipktyp = MDGGRB
	    IF  ( ( ival .ge. 1 ) .and. ( ival .le. 32 ) )  THEN
		nbits = ival
	    ELSE
		nbits = 16
	    END IF
	ELSE
	    CALL ER_WMSG ( 'GR', 4, gpack, ier )
	END IF
C*
	RETURN
	END
