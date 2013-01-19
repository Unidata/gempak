        SUBROUTINE  MA_LSCD  ( mszrpt, marrpt, ipt, iret )
C************************************************************************
C* MA_LSCD					 	                *
C*							                *
C* This subroutine gets the length and starting position of each of the *
C* sections 1-4 in the ZZYY drifting buoy report.  If a section is      *
C* missing, then its length and starting position are set to zero.      *
C*								        *
C* MA_LSCD  ( MSZRPT, MARRPT, IPT, IRET )                               *
C*								        *
C* Input parameters:						        *
C*      MSZRPT          INTEGER         Length of report in bytes       *
C*      MARRPT          CHAR*           Report array                    *
C*      IPT             INTEGER         Pointer to Q(t) in 6Q(i)Qt)//   *
C*								        *
C* Output parameters:						        *
C*	LSEC1           INTEGER         Length of section 1 in report   *
C*      ISEC1           INTEGER         Pointer to start of section 1   *
C*	LSEC2           INTEGER         Length of section 2 in report   *
C*      ISEC2           INTEGER         Pointer to start of section 2   *
C*	LSEC3           INTEGER         Length of section 3 in report   *
C*      ISEC3           INTEGER         Pointer to start of section 3   *
C*	LSEC4           INTEGER         Length of section 4 in report   *
C*      ISEC4           INTEGER         Pointer to start of section 4   *
C*	IRET		INTEGER         Return code                     *
C*				          0 = normal return             *
C*                                        1 = problems                  *
C**								        *
C* Log:								        *
C* R. Hollern/NCEP       6/96                                           *
C* R. Hollern/NCEP       8/96 	Added check on section lengths          *
C* R. Hollern/NCEP      11/96  	Modified logic to compute length of     *
C*                             	sections                                *
C* D. Kidwell/NCEP       4/97	Cleaned up code and documentation       *
C************************************************************************
        INCLUDE 	'macmn.cmn'
C*
        CHARACTER*(*)   marrpt
C------------------------------------------------------------------------
        iret  = 0
        lsec1 = 0
        lsec2 = 0
        lsec3 = 0
        lsec4 = 0
        isec1 = 0
        isec2 = 0
        isec3 = 0
        isec4 = 0
C
C*      If there is a section 444, determine its length.
C
        j1   = ipt
        lrpt = mszrpt
C
        i444 = INDEX ( marrpt (j1:lrpt), ' 444 ' )
C
        IF ( i444 .gt. 0 ) THEN
            ka    = j1 + i444
            isec4 = ka + 2
            lsec4 = lrpt - isec4 + 1
            lrpt  = ka - 1
        END IF
C
C*      If there is a section 333, determine its length.
C
        i333 = INDEX ( marrpt (j1:lrpt), ' 333' )
C
        IF ( i333 .gt. 0 ) THEN
            ka    = j1 + i333
            isec3 = ka + 3
            lsec3 = lrpt - isec3 + 1
            lrpt  = ka - 1
        END IF
C
C*      If there is a section 222, determine its length.
C
        i222 = INDEX ( marrpt (j1:lrpt), ' 222' )
C
        IF ( i222 .gt. 0 ) THEN
            ka    = j1 + i222
            isec2 = ka + 3
            lsec2 = lrpt - isec2 + 1
            lrpt  = ka - 1
        END IF
C
C*      If there is a section 111, determine its length.
C
        i111 = INDEX ( marrpt (j1:lrpt), ' 111' )
C
        IF ( i111 .gt. 0 ) THEN
            ka    = j1 + i111
            isec1 = ka + 3
            lsec1 = lrpt - isec1 + 1
            lrpt  = ka - 1
        END IF
C
C*      If section lengths are too long, reject report.
C*      This can happen if reports are not separated from
C*      each other by a report separator.
C
        IF ( lsec1 .gt. 50 .or. lsec2 .gt. 35 ) iret = 1
C*
        RETURN 
        END
