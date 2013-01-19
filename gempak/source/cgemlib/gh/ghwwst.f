	SUBROUTINE GH_WWST ( ityp, istart, iend, iact, ibkact, iret )
C************************************************************************
C* GH_WWST                                                              *
C*									*
C* This subroutine stores a pair of breakpoint values to an array.      *
C*                                                                      *
C* GH_WWST ( ITYP, ISTART, IEND, IACT, IBKACT, IRET )                   *
C*                                                                      *
C* Input parameters:                                                    *
C*	ITYP		INTEGER		Watch/warning type code         *
C*	ISTART		INTEGER		First bkpt value in pair        *
C*	IEND		INTEGER		Second bkpt value in pair       *
C*                                                                      *
C* Input and output parameters:                                         *
C*	IACT		INTEGER		Number of bkpts for this type   *
C*									*
C* Output parameters:                                                   *
C*	IBKACT(4,*)	INTEGER		Breakpoint pairs for this type  *
C*	IRET		INTEGER		Return code                     *
C*									*
C* Log:									*
C* D. Kidwell/NCEP	11/03						*
C************************************************************************
	INTEGER		ibkact (4,*)
C------------------------------------------------------------------------
	iret = 0
C
	iact = iact + 1
	ibkact ( ityp, iact ) = istart
	iact = iact + 1
	ibkact ( ityp, iact ) = iend 
C*
	RETURN
	END
