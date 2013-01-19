	SUBROUTINE RU_PPMW  ( field, above, pres, numb, iret )
C************************************************************************
C* RU_PPMW								*
C*									*
C* This subroutine decodes a pressure field for mandatory wind		*
C* reports (PPAA or PPCC).  The pressures reporting are returned.	*
C*									*
C* RU_PPMW  ( FIELD, ABOVE, PRES, NUMB, IRET )				*
C*									*
C* Input parameters:							*
C*	FIELD		CHAR*5		Pressure field 			*
C*	ABOVE		LOGICAL		Above 100 mb flag		*
C*									*
C* Output parameters:							*
C*	PRES (NUMB)	REAL		Pressures			*
C*	NUMB		INTEGER		Number of pressures		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -3 = invalid field		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/87						*
C* K. Brill/NMC		12/91	New 925 mb mandatory level		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		pres ( * )
	LOGICAL		above
	CHARACTER*(*)	field
C*
	CHARACTER*2	bpres ( 11 ), apres ( 10 ), pchar
	REAL		rbprs ( 11 ), raprs ( 10 )
C*
	DATA		bpres / '00', '92', '85', '70', '50', '40',
     +				'30', '25', '20', '15', '10' /,
     +			apres / '70', '50', '30', '20', '10',
     +				'07', '05' ,'03', '02', '01' /
	DATA		rbprs / 1000., 925., 850., 700., 500., 400.,
     +				 300., 250., 200., 150., 100. /,
     +			raprs / 70., 50., 30., 20., 10.,
     +				 7.,  5.,  3.,  2.,  1. /
C------------------------------------------------------------------------
	iret = 0
	numb = 0
C
C*	Check that first two characters are 44 or 55.
C
	IF  (( field (1:2) .ne. '44' ) .and. ( field (1:2) .ne. '55' ))
     +								THEN
	    iret = -3
	    RETURN
	END IF
C
C*	Decode number which is the third character.
C
	CALL ST_INTG  ( field (3:3), numb, ier )
	IF  ( ( ier .ne. 0 ) .or. ( numb .le. 0 ) .or. ( numb .gt. 3 ) )
     +								THEN
	    iret = -3
	    RETURN
	END IF
C
C*	Check first pressure against list.
C
	pchar = field ( 4:5 )
	ifirst = 0
	IF  ( .not. above )  THEN
	    DO  i = 1, 11
		IF  ( pchar .eq. bpres (i) ) ifirst = i
	    END DO
	  ELSE
	    DO  i = 1, 10
		IF  ( pchar .eq. apres (i) ) ifirst = i
	    END DO
	END IF
C
C*	Return error if this is not a valid pressure.
C
	IF  ( ifirst .eq. 0 )  THEN
	    iret = -3
	    RETURN
	END IF
C
C*	Return correct number of pressures.
C
	DO  i = 1, numb
	    IF  ( .not. above )  THEN
		IF  ( ifirst .gt. 11 )  iret = -3
		pres ( i ) = rbprs ( ifirst )
	      ELSE
		IF  ( ifirst .gt. 10 )  iret = -3
		pres ( i ) = raprs ( ifirst )
	    END IF
	    ifirst = ifirst + 1
	END DO
C
C*	Check for invalid return code.
C
	IF  ( iret .ne. 0 )  THEN
	    numb = 0
	END IF
C*
	RETURN
	END
