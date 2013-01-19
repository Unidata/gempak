	SUBROUTINE UPDGDM  ( iret )
C************************************************************************
C* UPDGDM								*
C*									*
C* This subroutine updates the grid common area for grids in earth	*
C* coordinates.								*
C*									*
C* UPDGDM  ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET			INTEGER		Return code		*
C**									*
C* Log:									*
C* M. desJardins/GSFC							*
C* M. desJardins/GSFC	 6/88	Modified to call common update modules	*
C* M. Linda/GSC		 9/97	Changed a key word in the prologue	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'XYDEF.CMN'
C------------------------------------------------------------------------
	iret = NIPROJ
C
C*	Call the correct subroutine to set up different projection
C*	classes.
C
	IF  ( mgclas .eq. MCCON ) THEN
	    CALL UPDCON  ( .false., iret )
	  ELSE IF  ( mgclas .eq. MCAZM ) THEN
	    CALL UPDAZM  ( .false., iret )
	  ELSE IF  ( mgclas .eq. MCCYL ) THEN
	    CALL UPDCYL  ( .false., iret )
	END IF
C
C*	Compute bounds in linear intermediate coordinates.
C
	IF  ( iret .eq. NORMAL )  CALL UPDGDI  ( iret )
C
C*	Set flag if grid coordinate definition had error.
C
	IF  ( iret .ne. NORMAL )  mgset = .false.
C*
	RETURN
	END
