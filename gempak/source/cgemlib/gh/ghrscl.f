	SUBROUTINE GH_RSCL ( iret )
C************************************************************************
C* GH_RSCL								*
C*									*
C* This subroutine restores the current settings for colors. The colors *
C* 1, 31, 32 and the background color 101 can not be changed due to the *
C* needs of the Post Script driver color bank.				*
C*									*
C* GH_RSCL ( IRET )  							*
C*									*
C* Input parameters:							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* A. Hardy/GSC		 6/01   					*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ghcmn.cmn'
C-----------------------------------------------------------------------
        iret = 0
C
C*	Reset the saved attributes.
C
        DO ii = 2, 30
            CALL GSCRGB ( ii, jred(ii), jgreen(ii), jblue(ii), ier )
        END DO
C*
	RETURN
	END
