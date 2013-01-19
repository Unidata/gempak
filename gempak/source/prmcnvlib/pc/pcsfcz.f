	SUBROUTINE PC_SFCZ ( datain, height, iret )
C************************************************************************
C* PC_SFCZ								*
C*									*
C* This subroutine returns the surface height.  First, the elevation	*
C* set in PC_SSTN is checked.  If it is either 0.0 or missing, the	*
C* value in the dataset is used.					*
C*									*
C* PC_SFCZ ( DATAIN, HEIGHT, IRET )					*
C*									*
C* Input parameters:							*
C*	DATAIN		REAL		Station data			*
C*	 (NPARM,NLEV)							*
C*									*
C* Output parameters:							*
C*	HEIGHT		REAL		Surface height			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/84						*
C* M. desJardins/GSFC	 9/88	Cleaned up				*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C* D. Kidwell/NCEP	 5/99	PC_DSET -> PC_SSTN in prologue          *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'pccmn.cmn'
C*
	REAL		datain (*)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
C*	Check for missing height.  If height is not in dataset, return
C*	0.
C
	IF  ( ( .not. ERMISS ( telv ) ) .and. ( telv .gt. 0. ) )  THEN
	    height = telv
	  ELSE IF  ( jhght .gt. 0 )  THEN
	    height = datain ( jhght )
	  ELSE
	    height = 0.0
	ENDIF
C*
	RETURN
	END
