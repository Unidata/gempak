	SUBROUTINE  GR_GTIM  ( gdattm, firstm, lasttm, gdtim1, gdtim2, 
     +			       iret )
C************************************************************************
C* GR_GTIM								*
C*									*
C* This subroutine changes the user input for grid time into two	*
C* GEMPAK times.  These two times are separated with a colon (:)	*
C* and indicate the two times used to compute the grid function. 	*
C*									*
C* GR_GTIM  ( GDATTM, FIRSTM, LASTTM, GDTIM1, GDTIM2, IRET )		*
C*									*
C* Input parameters:							*
C*	GDATTM		CHAR*		Grid time input			*
C*	FIRSTM		CHAR*		First time in grid file		*
C*	LASTTM		CHAR*		Last time in grid file		*
C*									*
C* Output parameters:							*
C*	GDTIM1		CHAR*		First input time		*
C*	GDTIM2		CHAR*		Second input time		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = invalid input time	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 2/85						*
C* M. desJardins/GSFC	 7/88	Eliminated LIST option			*
C* M. desJardins/GSFC	 4/89	Added first time option			*
C************************************************************************
	CHARACTER*(*)	gdattm, firstm, lasttm, gdtim1, gdtim2
C------------------------------------------------------------------------
C*	Convert to upper case.
C
	iret   = 0
	gdtim1 = ' '
	gdtim2 = ' '
C
C*	Check for colon, which separates two times.
C
	ipt   = INDEX ( gdattm, ':' )
	IF  ( ipt .ne. 0 )  THEN
	    CALL TG_FULL  ( gdattm ( : ipt-1 ), firstm, lasttm, gdtim1,
     +			    ier )
	    IF  ( ier .ne. 0 )  iret = -1
	    CALL TG_FULL  ( gdattm ( ipt+1 : ), firstm, lasttm, gdtim2,
     +			    ier )
	    IF  ( ier .ne. 0 )  iret = -1
	  ELSE
	    CALL TG_FULL  ( gdattm , firstm, lasttm, gdtim1, ier )
	    IF  ( ier .ne. 0 )  iret = -1
	    gdtim2 = ' '
	END IF
C*
	RETURN
	END
