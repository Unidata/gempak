	SUBROUTINE LV_GRNG  ( start, stop, inc, nexp, rlevel, nlev, 
     +			      iret )
C************************************************************************
C* LV_GRNG								*
C*									*
C* This subroutine finds the levels in a range with an increment.	*
C* START, STOP and INC are decoded as integers and the levels are	*
C* computed.								*
C*									*
C* LV_GRNG  ( START, STOP, INC, NEXP, RLEVEL, NLEV, IRET )		*
C*									*
C* Input parameters:							*
C*	START		CHAR*		Start of range			*
C*	STOP		CHAR*		End of range			*
C*	INC		CHAR*		Increment			*
C*	NEXP		INTEGER		Maximum number of levels	*
C*									*
C* Output parameters:							*
C*	RLEVEL (NLEV)	REAL		Levels in range			*
C*	NLEV		INTEGER		Number of levels		*
C*	IRET		INTEGER		Return code			*
C*					  1 = more than NEXP values	*
C*					  0 = normal return		*
C*					 -1 = input cannot be decoded	*
C*					 -5 = 0 invalid in range w inc	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/88						*
C* G. Huffman/GSC	 4/89	Error -5				*
C************************************************************************
	CHARACTER*(*)	start, stop, inc
	REAL		rlevel (*)
C------------------------------------------------------------------------
	iret = 0
	nlev = 0
C
C*	Decode the input for START, STOP, INC.
C
	CALL ST_NUMB  ( start, istart, ier1 )
	CALL ST_NUMB  ( stop,  istop,  ier2 )
	CALL ST_NUMB  ( inc,   iinc,   ier3 )
	IF  ( ( ier1 .ne. 0 ) .or. ( ier2 .ne. 0 ) .or. 
     +	      ( ier3 .ne. 0 ) .or. ( iinc .eq. 0 ) )  THEN
	    iret = -1
	    RETURN
	END IF
	IF  ( ( istart .eq. 0 ) .or. ( istop .eq. 0 ) )  THEN
	    iret = -5
	    RETURN
	  ELSE IF  ( ( istart .eq. -1 ) .or. ( istop .eq. -1 ) )  THEN
	    iret = -1
	    RETURN
	END IF
C
C*	Make sure that increment is positive and that start is less
C*	than stop.
C
	IF  ( istop .lt. istart )  THEN
	    itemp  = istart
	    istart = istop
	    istop  = itemp
	END IF
	iinc = ABS  ( iinc )
C
C*	Add levels to array.
C
	level = istart
	DO WHILE  ( level .le. istop )
C
C*	    Check for space in array.
C
	    IF  ( nlev .lt. nexp )  THEN
		nlev = nlev + 1
		rlevel (nlev) = FLOAT ( level )
	      ELSE
		iret  = 1
		level = istop
	    END IF
C
C*	    Increment level.
C
	    level = level + iinc
	END DO
C*
	RETURN
	END
