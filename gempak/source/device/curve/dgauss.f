	SUBROUTINE DGAUSS ( np, amtrx, coeff, xtng, ytng, iret )
C************************************************************************
C* DGAUSS								*
C*									*
C* This subroutine performs a Gaussian elimination on the coefficient	*
C* and transformation matrices to compute the tangent vectors for the 	*
C* relaxed end point condition.						*
C*									*
C* DGAUSS ( NP, AMTRX, COEFF, XTNG, YTNG, IRET )			*
C*									*
C* Input parameters:							*
C*	NP		  INTEGER	Number of input points		*
C*	AMTRX (LLMXPT,3)  REAL		Transformation matrix		*
C*	COEFF (LLMXPT,2)  REAL		Coefficient matrix		*
C*									*
C* Output parameters:							*
C*	XTNG (NP)	REAL		X comp of tangent vectors	*
C*	YTNG (NP)	REAL		Y comp of tangent vectors	*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* S. Jacobs/NCEP	 2/98						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		amtrx (LLMXPT,3), coeff (LLMXPT,2),
     +			xtng (*), ytng (*)
C------------------------------------------------------------------------
	iret = 0
C
C*	Reduce the matrices.
C
	DO  i = 2, np
	    IF ( amtrx(i,1) .ne. 0.0 )  THEN
		d = 1.0 / amtrx(i,1)
		amtrx(i,1) = 0.0
		q = amtrx(i,2) * d - amtrx(i-1,3)
		amtrx(i,2) = 1.0
		coeff(i,1) = ( coeff(i,1) * d - coeff(i-1,1) ) / q
		coeff(i,2) = ( coeff(i,2) * d - coeff(i-1,2) ) / q
		amtrx(i,3) = ( amtrx(i,3) * d ) / q
	    END IF
	END DO
C
C*	Solve for the tangent vectors.
C
	xtng (np) = coeff (np,1)
	ytng (np) = coeff (np,2)
	DO  i = np-1, 1, -1
	    xtng(i) = coeff(i,1) - amtrx(i,3) * xtng(i+1)
	    ytng(i) = coeff(i,2) - amtrx(i,3) * ytng(i+1)
	END DO
C
	RETURN
	END
