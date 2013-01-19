	SUBROUTINE UPDMAP  ( iret )
C************************************************************************
C* UPDMAP								*
C*									*
C* This subroutine calls the appropriate subroutines to update		*
C* the map common area for map projections.				*
C*									*
C* UPDMAP  ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC							*
C* B. Doty/RDS	 	 5/87	Added oblique Mercator			*
C* M. desJardins/GSFC	 6/88	Cleaned up				*
C* K. Brill/NMC		 7/91	Check for invalid lat bounds		*
C* A. Taylor/ARL	12/93	Relax lat bound check for oblique azm	*
C* K. Brill/NMC		 1/94	Cleanup					*
C* M. Linda/GSC		 9/97	Changed a key word in the prologue	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'XYDEF.CMN'
C------------------------------------------------------------------------
	iret = NIPROJ
C
C*	Check for invalid latitude bounds.
C
	IF ( ( clats .gt. 90. .or. clats .lt. -90. ) .or.
     +	     ( clatn .gt. 90. .or. clatn .lt. -90. ) ) THEN
	    iret = NIPBND
	    RETURN
	END IF
C
C*	Check that map class and projection type are valid.
C
	IF  ( mclass .eq. MCCON )  THEN
	    CALL UPDCON ( .true., iret )
	  ELSE IF  ( mclass .eq. MCAZM )  THEN
	    CALL UPDAZM ( .true., iret ) 
	  ELSE IF  ( mclass .eq. MCCYL )  THEN
	    CALL UPDCYL  ( .true., iret )
	  ELSE IF  ( mclass .eq. MCMER )  THEN
	    CALL UPDMER  ( iret ) 
	END IF
C
C*	Set flag if projection could not be defined.
C
	IF  ( iret .ne. NORMAL )  mset = .false.
C*
	RETURN
	END
