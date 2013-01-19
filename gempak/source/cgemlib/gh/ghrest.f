	SUBROUTINE GH_REST ( iret )
C************************************************************************
C* GH_REST								*
C*									*
C* This subroutine restores the current settings for lines, 		*
C* text and special markers. 						*
C*									*
C* GH_REST ( IRET )  							*
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
        CALL GSSPCL ( szghspcl, jghspwd, ier )
        CALL GSLINE ( jghltyp, jghlthw, jghwdth, jghwhw, ier )
        CALL GSTEXT ( jghtxfn, jghtxhw, ghsztx, jghtxwd, jghbrdr,
     +                jghrotn, jghjust, ier )
C*
	RETURN
	END
