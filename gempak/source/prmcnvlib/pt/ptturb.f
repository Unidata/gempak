	CHARACTER*(*) FUNCTION PT_TURB  ( tpot )
C************************************************************************
C* PT_TURB								*
C*									*
C* This function converts the turbulence type number into a character   *
C* string.								*
C*									*
C* CHARACTER*(*) PT_TURB  ( TPOT )					*
C*									*
C* Input parameters:							*
C*	TPOT		REAL		Type of turbulence		*
C*									*
C* Output parameters:							*
C*	PT_TURB		CHAR*		Turbulence type symbol          *
C**									*
C* Log:									*
C* A. Hardy/GSC		7/99						*
C* J. Wu/GSC            07/00   Moved INCLUDE 'ERMISS.FNC' before the   *
C*                              DATA statement                          *
C* A. Hardy/SAIC	10/01   Added range check ; removed unused vars *
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
C*
        CHARACTER       cvis(3)*3
C*
        INCLUDE         'ERMISS.FNC'
        DATA   cvis / 'CAT', 'CP', 'L' /
C*
C------------------------------------------------------------------------
        PT_TURB = ' '
        iturb = NINT ( tpot )
C
C*      Check for missing data.
C
        IF  ( ERMISS ( tpot ) .or. ( iturb .lt. 1 ) .or. 
     +                                        ( iturb .gt. 3 ) ) THEN
            RETURN
C
C*        Convert to character string.  
C
          ELSE
            PT_TURB =  cvis (iturb)
        END IF

C*
	RETURN
	END
