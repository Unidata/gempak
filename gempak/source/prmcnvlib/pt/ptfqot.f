	CHARACTER*(*) FUNCTION PT_FQOT  ( fqot )
C************************************************************************
C* PT_FQOT								*
C*									*
C* This function converts the turbulence frequency number into a        *
C* character string.							*
C*									*
C* CHARACTER*(*) PT_FQOT  ( FQOT )					*
C*									*
C* Input parameters:							*
C*	FQOT		REAL		Turbulence frequency            *
C*									*
C* Output parameters:							*
C*	PT_FQOT		CHAR*		Turbulence frequency symbol     *
C**									*
C* Log:									*
C* A. Hardy/GSC		7/99						*
C* J. Wu/GSC            07/00   Moved INCLUDE 'ERMISS.FNC' before the   * 
C*                              DATA statement                          *
C* A. Hardy/SAIC	10/01   Added range check ; removed unused vars *
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
C*
        CHARACTER       cvis(3)*1
C*
        INCLUDE         'ERMISS.FNC'
        DATA   cvis / 'O', 'I', 'C' /
C*
C------------------------------------------------------------------------
        PT_FQOT = ' '
        ifqot = NINT ( fqot )
C
C*      Check for missing data.
C
        IF  ( ERMISS ( fqot ) .or. ( ifqot .lt. 1 ) .or. 
     +                                     ( ifqot .gt. 3 ) ) THEN
            RETURN
C
C*        Convert to character string.  
C
          ELSE
            PT_FQOT =  cvis (ifqot)
        END IF

C*
	RETURN
	END
