	SUBROUTINE GD_GNAV  ( iacss, rnvblk, navsz, iret )
C************************************************************************
C* GD_GNAV								*
C*									*
C* This subroutine returns the navigation block.			*
C*									*
C* GD_GNAV  ( IACSS, RNVBLK, NAVSZ, IRET )				*
C*									*
C* Input parameters:							*
C*	IACSS 		INTEGER		Grid access number		*
C*									*
C* Output parameters:							*
C*	RNVBLK (NAVSZ)	REAL		Navigation block		*
C*	NAVSZ		INTEGER		Length of nav block		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not open		*
C*					 -6 = read/write error		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/88						*
C* R. Tian/SAIC          1/04   Added GD_FCHK call                      *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE 	'grdcmn.cmn'
C*
	REAL		rnvblk (*)
C-----------------------------------------------------------------------
C
C*      Convert access number to DM number.
C
        CALL GD_FCHK ( iacss, igdfln, iret )
        IF ( iret .ne. 0 ) THEN
            RETURN
        END IF
C
C*	Get navigation from common.
C
	navsz = lnavbl ( igdfln )
	DO  ij = 1, navsz
	    rnvblk ( ij ) = savnav ( ij, igdfln )
	END DO
C*
	RETURN
	END
