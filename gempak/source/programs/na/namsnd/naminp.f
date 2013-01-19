	SUBROUTINE  NAMINP  ( snbufr, snoutf, sfoutf,
     +			      snprmf, sfprmf, timstn, iret )
C************************************************************************
C* NAMINP								*
C*									*
C* This subroutine gets the input variables for NAMSND.			*
C*									*
C* NAMINP  ( SNBUFR, SNOUTF, SFOUTF, SNPRMF, SFPRMF, TIMSTN, IRET )	*
C*                                                                      *
C* Input parameters:                                                    *
C*      SNBUFR		CHAR*		Model sounding file name        *
C*      SNOUTF		CHAR*		Output sounding file name       *
C*	SFOUTF		CHAR*		Output surface file name        *
C*	SNPRMF		CHAR*		Sounding parm packing file name *
C*      SFPRMF 		CHAR*		Surface parm packing file name  *
C*	TIMSTN		CHAR*		Maximum times / stations        *
C*	IRET		INTEGER		Return code                     *
C*					  0 = normal return             *
C*					 -2 = input vbl not received    *
C**									*
C* Log:									*
C* K. F. Brill/NMC	12/93						*
C* D. Kidwell/NCEP	11/98	Corrected prologue, SNEFIL -> SNBUFR    *
C* D. Kidwell/NCEP	12/98	SNMINP -> NAMINP, SNMODL -> NAMSND      *
C************************************************************************
	CHARACTER*(*)	snbufr, snoutf, sfoutf, snprmf, sfprmf, timstn
C------------------------------------------------------------------------
C*	Get the input variables.
C
	CALL IP_STR  ( 'SNBUFR', snbufr, ier1 )
	CALL IP_STR  ( 'SNOUTF', snoutf, ier2 )
	CALL IP_STR  ( 'SFOUTF', sfoutf, ier3 )
	CALL IP_STR  ( 'SNPRMF', snprmf, ier4 )
	CALL IP_STR  ( 'SFPRMF', sfprmf, ier5 )
	CALL IP_STR  ( 'TIMSTN', timstn, ier6 )
C
	iret = ier1 + ier2 + ier3 + ier4 + ier5 + ier6
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
