	SUBROUTINE VI_INIT  ( iret )
C************************************************************************
C* VI_INIT								*
C*									*
C* This subroutine performs initialization for the vertical		*
C* interpolation.							*
C*									*
C* VI_INIT ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		  INTEGER	Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. Brill/NMC      08/92						*
C* G. Hull/SAIC      03/08   add cmn_data to remove Grid size limit     *
C*
C************************************************************************
	INCLUDE		'vicmn.cmn'

C-----------------------------------------------------------------------
	iret = 0
	dbglun=0
	dbgid =1
C
C*	Get input and output vertical coordinates.
C
	CALL VI_VCRD ( iret )
	IF ( iret .ne. 0 ) RETURN
C
C*	Get the input and output vertical levels and the parameters to
C*	be interpolated.
C
	CALL VG_LVPR ( iret )

	IF ( iret .ne. 0 ) RETURN
C
C*	Set the output write flags for the parameters.
C
	DO ip = 1, np
	    lprmwr (ip) = .true.
	END DO
	IF ( vcordo .eq. NPRES .or. vcordo .eq. NSGMA .or.
     +	     vcordo .eq. NETA ) THEN
	    lprmwr (1) = .false.
	ELSE IF ( vcordo .eq. NHGHT .or. vcordo .eq. NZAGL ) THEN
	    lprmwr (3) = .false.
	ELSE IF ( vcordo .eq. NTHTA ) THEN
	    lprmwr (2) = .false.
	END IF
C*
	icalvi = 1
	iprcnt = 0

	buildz = .false.
C*
	DO il = 1, MAXLVL
	    subsfc (il) = .false.
	END DO
C*
	RETURN
	END
