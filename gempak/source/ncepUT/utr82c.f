	SUBROUTINE UT_R82C  ( r8ary, nr8ary, cstg, lcstg, iret )
C************************************************************************
C* UT_R82C								*
C*									*
C* This subroutine converts a REAL*8 array into a character string.	*
C* On output, LCSTG = 0 denotes that the conversion was unsuccessful.	*
C*									*
C* UT_R82C  ( R8ARY, NR8ARY, CSTG, LCSTG, IRET )			*
C*									*
C* Input parameters:							*
C*	R8ARY (*)	REAL*8		REAL*8 array			*
C*	NR8ARY		INTEGER		Size of R8ARY			*
C*									*
C* Output parameters:							*
C*	CSTG		CHARACTER*(*)	Character string		*
C*	LCSTG		INTEGER		Size of CSTG			*
C*					  0 = unsuccessful conversion	*
C*	IRET		INTEGER		Return code:			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* J. Ator/NP12		06/97						*
C* J. Ator/NP12		12/97	Cleaned up, style changes		*
C************************************************************************
	CHARACTER*(*)	cstg
	CHARACTER*8	cval
C*
	REAL*8		r8ary (*)
	REAL*8		r8val
C*
	EQUIVALENCE	( r8val, cval )
C*-----------------------------------------------------------------------
	iret = 0
	lcstg = 0
	IF  ( nr8ary .le. 0 )  THEN
	    RETURN
	ENDIF
C
	is = -7
	ie = 0
	DO jj = 1, nr8ary
	    r8val = r8ary ( jj )
	    is = is + 8
	    ie = ie + 8
	    cstg ( is : ie ) = cval (1:8)
	ENDDO
	lcstg = ie
C*
	RETURN
	END
