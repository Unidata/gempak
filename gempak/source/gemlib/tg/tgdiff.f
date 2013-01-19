	SUBROUTINE TG_DIFF  ( dattm1, dattm2, nmin, iret )
C************************************************************************
C* TG_DIFF								*
C*									*
C* This subroutine computes the time difference in minutes between 	*
C* two GEMPAK grid times.  The time difference is time1 - time2		*
C* and may be computed for a maximum of one year.			*
C*									*
C* TG_DIFF  ( DATTM1, DATTM2, NMIN, IRET )				*
C*									*
C* Input parameters:							*
C*	DATTM1		CHAR*		First GEMPAK grid time		*
C*	DATTM2		CHAR*		Second GEMPAK  grid time	*
C*									*
C* Output parameters:							*
C*	NMIN		INTEGER		Difference in minutes		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*				  	-12 = invalid time range	*
C**									*
C* Log:									*
C* K. Brill/GSC		11/89						*
C************************************************************************
	CHARACTER*(*)	dattm1, dattm2
	CHARACTER*20    vdttm1, vdttm2
C*
	INTEGER		idtar1 (5), idtar2 (5)
C------------------------------------------------------------------------
	iret = 0
	nmin = 0
C
C*	Convert the grid times to valid times in GEMPAK standard format.
C
	CALL TG_VALD ( dattm1, vdttm1, iret )
	IF ( iret .ne. 0 ) RETURN
C*
	CALL TG_VALD ( dattm2, vdttm2, iret )
	IF ( iret .ne. 0 ) RETURN
C
C*	Convert GEMPAK times to integers.
C
	CALL TI_CTOI  ( vdttm1, idtar1, iret )
	IF  ( iret .ne. 0 )  RETURN
C*
	CALL TI_CTOI  ( vdttm2, idtar2, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Call subroutine to do calculation with integer variables.
C
	CALL TI_MDIF  ( idtar1, idtar2, nmin, iret )
C*
	RETURN
	END
