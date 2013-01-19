	CHARACTER*(*) FUNCTION PT_TICE  ( tpoi )
C************************************************************************
C* PT_TICE								*
C*									*
C* This function converts the icing type number into a character string.*
C*									*
C* CHARACTER*(*) PT_TICE  ( TPOI )					*
C*									*
C* Input parameters:							*
C*	TPOI		REAL		Type of icing			*
C*									*
C* Output parameters:							*
C*	PT_TICE		CHAR*		Icing type symbol		*
C**									*
C* Log:									*
C* A. Hardy/GSC		7/99						*
C* J. Wu/GSC            07/00   Moved INCLUDE 'ERMISS.FNC' before the   * 
C*                              DATA statement                          *
C* A. Hardy/SAIC	10/01   Added range check ; removed unused vars *
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
C*
        CHARACTER       cvis(9)*2
C*
        INCLUDE         'ERMISS.FNC'
        DATA   cvis / 'R', 'CL', 'M', 'R', 'CL', 'M', 'R', 'CL', 'M' /
C*
C------------------------------------------------------------------------
        PT_TICE = ' '
        itice = NINT ( tpoi )
C
C*      Check for missing data.
C
        IF  ( ERMISS ( tpoi ) .or. ( itice .lt. 1 ) .or. 
     +                                  ( itice .gt. 9 ) ) THEN
            RETURN
C
C*        Convert to character string.  
C
          ELSE
            PT_TICE =  cvis ( itice )
        END IF

C*
	RETURN
	END
