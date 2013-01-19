	SUBROUTINE GH_SVCL ( iret )
C************************************************************************
C* GH_SVCL								*
C*									*
C* This subroutine stores the current settings for color.  The colors   *
C* 1, 31, 32 and the background color 101 can not be changed due to the *
C* needs of the Post Script driver color bank.				*
C*									*
C* GH_SVCL ( IRET )  							*
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
C*	Query current color settings.
C
        DO ii = 2, 30
            CALL GQCOMP ( ii, color(ii), jred(ii), jgreen(ii), 
     +                    jblue(ii), xname(ii), ier )
        END DO
C*
	RETURN
	END
