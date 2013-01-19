	SUBROUTINE GD_GANL  ( iacss, anlblk, ianlsz, iret )
C************************************************************************
C* GD_GANL								*
C*									*
C* This subroutine returns the analysis block.				*
C*									*
C* GD_GANL  ( IACSS, ANLBLK, IANLSZ, IRET )				*
C*									*
C* Input parameters:							*
C*	IACSS 		INTEGER		Grid access number		*
C*									*
C* Output parameters:							*
C*	ANLBLK (IANLSZ)	REAL		Analysis block			*
C*	IANLSZ		INTEGER		Length of anl block		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/88						*
C* R. Tian/SAIC          1/04   Added GD_FCHK call                      *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE 	'grdcmn.cmn'
C*
	REAL		anlblk (*)
C-----------------------------------------------------------------------
        iret = 0
C
C*      Convert access number to DM number.
C
        CALL GD_FCHK ( iacss, igdfln, iret )
        IF ( iret .ne. 0 ) THEN
            RETURN
        END IF
C
C*	Get analysis block from common.
C
	ianlsz = lanlbl ( igdfln )
	DO  ij = 1, ianlsz
	    anlblk ( ij ) = savanl ( ij, igdfln )
	END DO
C*
	RETURN
	END
