	SUBROUTINE CS_FLVL ( string, flvl, iptr, iret )
C************************************************************************
C* CS_FLVL  								*
C*									*
C* This subroutine gets the flight level(s) for convective sigmet       *
C* report.       							*
C*                                                                      *
C* CS_FLVL ( STRING, FLVL, IPTR, IRET )                                 *
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		Report string                   *
C*									*
C* Output parameters:							*
C*	FLVL(2)		CHAR*		Flight level(s)                 *
C*	IPTR		INTEGER		Pointer following last flt level*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*									*
C**									*
C* Log:									*
C* A. Hardy/NCEP	 8/02	Created					*
C* A. Hardy/NCEP	 8/02	Shortened string for search of tops/FL  * 
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	string, flvl (*)
C*
C------------------------------------------------------------------------
	iret       = 0
	flvl ( 1 ) = ' '
	flvl ( 2 ) = ' '
	iptr       = 0
        ifnd       = 0 
	itpsto     = 0  
	itpt       = 0 
	itpsab     = 0 
	itpv       = 0 
        itops      = 0 
        itp        = 0
        iflvl      = 0
C
C*	Get the levels.
C
	CALL ST_LSTR ( string, lens, ier )
        icon = INDEX ( string ( :lens ), 'CONVECTIVE SIGMET' )
        IF ( icon .eq. 0 ) THEN
           icon = lens
        END IF
C
        itpsto = INDEX ( string ( :icon ), 'TOPS TO' )
        IF ( itpsto .eq. 0 ) THEN
            itpt = INDEX ( string ( :icon ), 'TOP TO' )
            IF ( itpt .eq. 0 ) THEN
                itpsab = INDEX ( string ( :icon ), 'TOPS ABV' )
                IF ( itpsab .eq. 0 ) THEN
                    itpv = INDEX ( string ( :icon ), 'TOP ABV' )
                    IF ( itpv .eq. 0 ) THEN
                        itops = INDEX ( string ( :icon ), 'TOPS' )
                        IF ( itops .eq. 0 ) THEN
                            itp = INDEX ( string ( :icon ), 'TOP' )
                        END IF
                    END IF
                END IF
            END IF
        END IF
C
        ifnd = itpsto + itpt + itpsab + itpv + itops + itp
C
        IF ( ifnd .gt. 0 ) THEN
            iflvl = INDEX ( string (ifnd:icon ), 'FL' )
            IF ( iflvl .ne. 0 ) THEN
                flvl (1) = string(ifnd+iflvl+1:ifnd+iflvl+3 ) 
              ELSE
                flvl (1) = string(ifnd+ifnd+8:ifnd+ifnd+10)
            END IF
        END IF 
C
	iptr = iflvl + 6
C*
	RETURN
	END
