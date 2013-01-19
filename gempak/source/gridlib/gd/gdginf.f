	SUBROUTINE GD_GINF ( igdfln, anlblk, rnvblk, ngrd, iret )
C************************************************************************
C* GD_GINF								*
C*									*
C* This subroutine opens an existing GEMPAK grid file.			*
C*									*
C* GD_GINF  ( IGDFLN, ANLBLK, RNVBLK, NGRD, IRET )			*
C*									*
C* Input parameters:							*
C*	IGDFLN		INTEGER		File number			*
C*									*
C* Output parameters:							*
C*	ANLBLK (*)	REAL		Analysis block			*
C*	RNVBLK (*)	REAL		Navigation block		*
C*	NGRD		INTEGER		Number of grids			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*                                       -8 = nav cannot be read        *
C**									*
C* Log:									*
C* D.W.Plummer/NCEP	 4/00						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE 	'grdcmn.cmn'
C
	REAL		rnvblk (*), anlblk (*)
C
C------------------------------------------------------------------------
C
C*	Retrieve navigation block.
C
	CALL DM_RFHR  ( igdfln, 'NAVB', LLNNAV, rnvblk, navsz, iret )
	IF  ( iret .ne. 0 )  THEN
	    iret = -8
	    CALL ER_WMSG ( 'GD', iret, ' ', ier )
	    RETURN
	END IF
C
C*	Retrieve analysis block.
C
	CALL DM_RFHR  ( igdfln, 'ANLB', LLNANL, anlblk, ianlsz, ier )
C
C*	Retrieve the number of grids
C
	ngrd = kgrid ( igdfln )
C
C*
	RETURN
	END
