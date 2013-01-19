	SUBROUTINE GDIGIT  ( ival, ibase, ndig, idigs, iret )
C************************************************************************
C* GDIGIT 								*
C*									*
C* This subroutine computes the individual digits of a decimal input	*
C* number in any arbitrary base.					*
C*									*
C* The digits are ordered beginning with the one's digit; so, IDIGS (1) *
C* is multiplied by IBASE**0 = 1, IDIGS (2) by IBASE**1, and so on, in  *
C* recovering the value in the new base.				*
C*									*
C* GDIGIT   ( IVAL, IBASE, NDIG, IDIGS, IRET )				*
C*									*
C* Input parameters:							*
C*	IVAL		INTEGER		Input decimal value		*
C*	IBASE		INTEGER		Base for the output digits	*
C*									*
C* Input and output parameter:						*
C*	NDIG		INTEGER		Input: max # of digits allowed	*
C*					Output: # of digits needed	*
C*									*
C* Output parameters:							*
C*	IDIGS (NDIG)	INTEGER		Output digits			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = base cannot be < 2	*
C*					 -2 = not enough digits		*
C*					 -3 = input value < 0		*
C**									*
C* Log:									*
C* K. Brill/HPC		 8/99						*
C************************************************************************
	INTEGER		idigs (*)
C*
	DOUBLE PRECISION	v, b
C------------------------------------------------------------------------
	IF ( ibase .lt. 2 ) THEN
	    iret = -1
	    RETURN
	END IF
	IF ( ival .lt. 0 ) THEN
	    iret = -3
	    RETURN
	END IF
	iret = 0
	nmax = ndig
	DO i = 1, nmax
	    idigs (i) = 0
	END DO
C*
	v = DFLOAT ( ival )
	b = DFLOAT ( ibase )
	ndig = INT ( DLOG ( v ) / DLOG ( b ) ) + 1
	itest = ibase ** ndig
	IF ( itest .lt. ival ) ndig = ndig + 1
	IF ( ndig .gt. nmax ) THEN
	    iret = -2
	    RETURN
	END IF
	iv = ival
	idig = ndig
	DO WHILE ( idig .gt. 0 )
	    npwr = idig - 1
	    id = ibase ** npwr
	    idigs (idig) = iv / id
	    iv = iv - idigs (idig) * id
	    idig = idig - 1
	END DO
C*
	RETURN
	END
