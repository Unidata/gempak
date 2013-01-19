	SUBROUTINE GD_SWRT ( iacss, wrtflg, iret )
C************************************************************************
C* GD_SWRT								*
C*									*
C* This subroutine sets the internal write flag for a grid file.  If	*
C* the file is being changed from READ ONLY to WRITE access, DM_CHNG	*
C* will close it and reopen it for WRITE access.			*
C*									*
C* GD_SWRT ( IACSS, WRTFLG, IRET )					*
C*									*
C* Input parameters:							*
C*	IACSS 		INTEGER		Grid acess  number		*
C*	WRTFLG		LOGICAL		Write flag			*
C*					  T = write			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. Brill/NMC		 8/90						*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C* R. Tian/SAIC          1/04   Added GD_FCHK call                      *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'grdcmn.cmn'
C*
	LOGICAL		wrtflg, shrflg
C------------------------------------------------------------------------
	iret   = 0
	shrflg = .false.
C
C*      Convert access number to DM number.
C
        CALL GD_FCHK ( iacss, igdfln, iret )
        IF ( iret .ne. 0 ) THEN
            RETURN
        END IF
C
C*	Set the flag in GRDCMN.CMN.
C
	gdwrt ( igdfln ) = wrtflg
C*
	CALL DM_CHNG ( igdfln, wrtflg, shrflg, iret )
C*
	RETURN
	END
