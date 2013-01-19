	SUBROUTINE PC_GLEV  ( levnum, data, nparm, datlev, iret )
C************************************************************************
C* PC_GLEV								*
C*									*
C* This subroutine pulls a single level of data from a two-		*
C* dimension array.							*
C*									*
C* PC_GLEV  ( LEVNUM, DATA, NPARM, DATLEV, IRET )			*
C*									*
C* Input parameters:							*
C*	LEVNUM		INTEGER		Level to return			*
C*	DATA (NPARM,*)	REAL		2-dimensional data array	*
C*	NPARM		INTEGER		Number of parms (dimension 1)	*
C*									*
C* Output parameters:							*
C*	DATLEV (NPARM)	REAL		Data at LEVNUM			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/84						*
C* M. desJardins/GSFC	 9/88	GEMPAK4					*
C************************************************************************
	REAL		data (*), datlev (*)
C------------------------------------------------------------------------
	iret = 0
C
C*	Get start point, treating array as a single-dimensioned array.
C
	istart = ( levnum - 1 ) * nparm
	DO  i = 1, nparm
	   istart = istart + 1
	   datlev (i) = data ( istart )
	ENDDO
C*
	RETURN
	END
