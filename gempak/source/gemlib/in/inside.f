	SUBROUTINE IN_SIDE  ( cside, iside, iret )
C************************************************************************
C* IN_SIDE								*
C*									*
C* This subroutine determines the side of the axis to label.  The	*
C* possible values are:							*
C*									*
C*	0            no labeling					*
C*	1, L, B      left or bottom					*
C*	2, R, T      right or top					*
C*	3, LR, BT    both the left and right or top and bottom		*
C*									*
C* The output value is the integer; the default is 1.			*
C*									*
C* IN_SIDE  ( CSIDE, ISIDE, IRET )					*
C*									*
C* Input parameters:							*
C*	CSIDE		CHAR*		Side of plot input		*
C*									*
C* Output parameters:							*
C*	ISIDE		INTEGER		Label side			*
C*					  0 = no labels			*
C*					  1 = left / bottom		*
C*					  2 = right / top		*
C*					  3 = left,right / top,bottom	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* G. Huffman/USRA	 5/89						*
C************************************************************************
	CHARACTER*(*)	cside
C*
	CHARACTER	uside*2
C*
	CHARACTER	choice ( 12 )*2
	DATA		choice / '3', 'LR', 'RL', 'BT', 'TB', 
     +			         '2', 'R',  'T',  '1',  'L', 'B', '0' /
C------------------------------------------------------------------------
	iret = 0
C
C*	Quickie test for the usual case (blank, defaulting to '1').
C
	IF  ( cside .eq. ' ' )  THEN
	    iside = 1
C
C*	  Convert to upper case and find which string position matches.
C
	  ELSE
	    CALL ST_LCUC  ( cside, uside, ier )
	    CALL ST_FIND  ( uside, choice, 12, ipos, ier )
C
C*	    Set the return integer.  The order of tests are intended to
C*	    test the expected choices first; IF's are one-sided because
C*	    all lower values have already been used.
C
	    IF  ( ipos .le. 0 )  THEN
		iside = 1
	      ELSE IF  ( ipos .le. 5 )  THEN
		iside = 3
	      ELSE IF  ( ipos .le. 8 )  THEN
		iside = 2
	      ELSE IF  ( ipos .le. 11 )  THEN
		iside = 1
	      ELSE
		iside = 0
	    END IF
	END IF
C*
	RETURN
	END
