	SUBROUTINE DCGAUS ( np, amtrx, coeff, xtng, ytng, iret )
C************************************************************************
C* DCGAUS								*
C*									*
C* This subroutine performs a Gaussian elimination on the coefficient	*
C* and transformation matrices to compute the tangent vectors for the	*
C* cyclic end point condition.						*
C*									*
C* NOTE: This routine has a problem with more that ~32 points. It is	*
C*       no longer called from DSPLIN.					*
C*									*
C* DCGAUS ( NP, AMTRX, COEFF, XTNG, YTNG, IRET )			*
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
C* S. Jacobs/NCEP	 4/98	Added warning to prolog			*
C* I. Durham/GSC	 4/98	Changed equations to eliminate problem  *
C*				that led to previous warning (>32 pts)  *
C* S. Jacobs/NCEP	 4/98	Restored original function and warning	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		amtrx (LLMXPT,3), coeff (LLMXPT,2),
     +			xtng (*), ytng (*)
C*
	REAL		col (LLMXPT)
C------------------------------------------------------------------------
	iret = 0
C
C*	For all rows but first and last add two rows together to
C*	eliminate amtrx(i,1).
C
	npm1   = np   - 1
	npm2   = npm1 - 1
	npm3   = npm2 - 1
	row1   = amtrx(npm1,3)
	col(1) = 1.0
C
	DO  i = 2, npm2
	    d = ( 1.0 / amtrx(i,1) ) * amtrx(i-1,2)
 	    amtrx(i,1) = 0.0
	    amtrx(i,2) = amtrx(i,2) * d - amtrx(i-1,3)
	    amtrx(i,3) = amtrx(i,3) * d
	    col(i) = - col(i-1)
C
	    coeff(i,1) = coeff(i,1) * d - coeff(i-1,1)
	    coeff(i,2) = coeff(i,2) * d - coeff(i-1,2)
	END DO
C
	amtrx(npm2,2) = amtrx(npm2,2) + col(npm2)
C
C*	Manipulate last equation to eliminate all but last 2 columns.
C
	DO  i = 1, npm3
	    d = ( 1.0 / row1 ) * amtrx(i,2)
	    row1 = - amtrx(i,3)
	    amtrx(npm1,1) = amtrx(npm1,1) * d
	    amtrx(npm1,2) = amtrx(npm1,2) * d - col(i)
C
	    coeff(npm1,1) = coeff(npm1,1) * d - coeff(i,1)
	    coeff(npm1,2) = coeff(npm1,2) * d - coeff(i,2)
	END DO
C
	amtrx(npm1,1) = amtrx(npm1,1) + row1
C
C*	Last two equations have only 2 non-zero elements. Eliminate
C*	and solve.
C
	d = ( 1.0 / amtrx(npm1,1) ) * amtrx(npm2,2)
	amtrx(npm1,2) = amtrx(npm1,2) * d - amtrx(npm2,3)
C
	coeff(npm1,1) = coeff(npm1,1) * d - coeff(npm2,1)
	coeff(npm1,2) = coeff(npm1,2) * d - coeff(npm2,2)
C
C*	Solve for the tangent vectors.
C
	xvn = coeff(npm1,1) / amtrx(npm1,2)
	yvn = coeff(npm1,2) / amtrx(npm1,2)
	xtng(npm1) = xvn
	ytng(npm1) = yvn
C
	d = 1.0 / amtrx(npm2,2)
	xtng(npm2) = ( coeff(npm2,1) - amtrx(npm2,3) * xvn ) * d
	ytng(npm2) = ( coeff(npm2,2) - amtrx(npm2,3) * yvn ) * d
C
	DO  i = npm3, 1, -1
	    d = 1.0 / amtrx(i,2)
	    xtng(i) = (coeff(i,1)-amtrx(i,3)*xtng(i+1)-col(i)*xvn) * d
	    ytng(i) = (coeff(i,2)-amtrx(i,3)*ytng(i+1)-col(i)*yvn) * d
	END DO
C
	xtng(np) = xtng(1)
	ytng(np) = ytng(1)
C
	RETURN
	END
