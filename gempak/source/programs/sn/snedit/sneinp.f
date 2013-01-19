	SUBROUTINE SNEINP  ( snefil, snfile, timstn, iret )
C************************************************************************
C* SNEINP								*
C*									*
C* This subroutine gets the input variables for SNEDIT.			*
C*									*
C* SNEINP  ( SNEFIL, SNFILE, TIMSTN, IRET )				*
C**									*
C* Log:									*
C* M. desJardins/GSFC	10/88						*
C************************************************************************
	CHARACTER*(*)	snefil, snfile, timstn
C------------------------------------------------------------------------
C*	Get the input variables.
C
	CALL IP_STR  ( 'SNEFIL', snefil, ier1 )
	CALL IP_STR  ( 'SNFILE', snfile, ier2 )
	CALL IP_STR  ( 'TIMSTN', timstn, ier3 )
C
	iret = ier1 + ier2 + ier3
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
