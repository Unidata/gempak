	SUBROUTINE ST_RLCH  ( rlnum, np, string, iret )
C************************************************************************
C* ST_RLCH								*
C*									*
C* This subroutine encodes a real number in a character string.  NP	*
C* contains the number of decimal places to be included in the output	*
C* string.  RLNUM is rounded to NP decimal places.			*
C*									*
C* ST_RLCH  ( RLNUM, NP, STRING, IRET )					*
C*									*
C* Input parameters:							*
C*	RLNUM		REAL		Real number 			*
C*	NP 		INTEGER		Number of decimal places 	*
C*									*
C* Output parameters: 							*
C*	STRING		CHAR*		Output string			*
C*	IRET		INTEGER		Return code			*
C*				   	  0 = normal return		*
C**									*
C* Log:									*
C* I. Graffman/CSC 	12/82	STR_RLCHR				*
C* I. Graffman/RDS 	 2/84	Use new GEMPAK routines			*
C* M. desJardins/GSFC	 4/84	Change encode to write			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* M. desJardins/GSFC	11/88	Add rounding				*
C* M. desJardins/GSFC	 3/89	Changed to use case stmt; rounding	*
C*				didn't work				*
C* J. Wu/GSC             7/00   Removed logic to add a blank at the end *
C* F. J. Yen/NCEP	 7/01	Included GEMPRM.PRM.(S.Chiswell/Unidata)*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'	
	CHARACTER*(*)	string
	CHARACTER	str*20
C------------------------------------------------------------------------
	iret = 0
C
C*	Encode the number.
C
	ier = 0
	IF  ( np .eq. 0 )  THEN
	    WRITE   ( str, 10, IOSTAT = ier )  rlnum
10	    FORMAT  ( F20.0 )
	  ELSE IF  ( np .eq. 1 )  THEN
	    WRITE   ( str, 1, IOSTAT = ier )  rlnum
1	    FORMAT  ( F20.1 )
	  ELSE IF  ( np .eq. 2 )  THEN
	    WRITE   ( str, 2, IOSTAT = ier )  rlnum
2	    FORMAT  ( F20.2 )
	  ELSE IF  ( np .eq. 3 )  THEN
	    WRITE   ( str, 3, IOSTAT = ier )  rlnum
3	    FORMAT  ( F20.3 )
	  ELSE IF  ( np .eq. 4 )  THEN
	    WRITE   ( str, 4, IOSTAT = ier )  rlnum
4	    FORMAT  ( F20.4 )
	  ELSE IF  ( np .eq. 5 )  THEN
	    WRITE   ( str, 5, IOSTAT = ier )  rlnum
5	    FORMAT  ( F20.5 )
	  ELSE IF  ( np .eq. 6 )  THEN
	    WRITE   ( str, 6, IOSTAT = ier )  rlnum
6	    FORMAT  ( F20.6 )
	  ELSE IF  ( np .eq. 7 )  THEN
	    WRITE   ( str, 7, IOSTAT = ier )  rlnum
7	    FORMAT  ( F20.7 )
	  ELSE IF  ( np .eq. 8 )  THEN
	    WRITE   ( str, 8, IOSTAT = ier )  rlnum
8	    FORMAT  ( F20.8 )
	  ELSE IF  ( np .eq. 9 )  THEN
	    WRITE   ( str, 9, IOSTAT = ier )  rlnum
9	    FORMAT  ( F20.9 )
	  ELSE
	    ier = -1
	END IF
C
C*	On error, return missing data value.
C
	IF  ( ier .ne. 0 )  THEN
	    WRITE  ( str, 1, IOSTAT = ier )  RMISSD
	END IF
C
C*	Eliminate leading spaces.
C
	CALL ST_LDSP  ( str, string, nc, iret )
C*
	RETURN
	END
