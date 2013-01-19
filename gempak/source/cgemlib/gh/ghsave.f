	SUBROUTINE GH_SAVE ( iret )
C************************************************************************
C* GH_SAVE								*
C*									*
C* This subroutine stores the current settings for lines, color, 	*
C* text and special markers. 						*
C*									*
C* GH_SAVE ( IRET )  							*
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
C*	Query current settings.
C
	CALL GQSPCL ( szghspcl, jghspwd, ier )
        CALL GQLINE ( jghltyp, jghlthw, jghwdth, jghwhw, ier )
	CALL GQTEXT ( jghtxfn, jghtxhw, ghsztx, jghtxwd, jghbrdr,
     +		      jghrotn, jghjust, ier )
C*
	RETURN
	END
