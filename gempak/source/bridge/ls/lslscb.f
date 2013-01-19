        SUBROUTINE  LS_LSCB ( lszrpt, lsfrpt, ipt, iret )
C************************************************************************
C* LS_LSCB						                *
C*							                *
C* This subroutine gets the length and starting position of each of the *
C* sections 1-5 in the WMO FM12 report.  If a section is missing, then  *
C* its length and starting position are set to zero.                    *
C*								        *
C* LS_LSCB  ( LSZRPT, LSFRPT, IPT, IRET )                               *
C*								        *
C* Input parameters:						        *
C*      LSZRPT          INTEGER         Report size                     *
C*      LSFRPT          CHAR*           Report array                    *
C*      IPT             INTEGER         Pointer to space before the     *
C*                                      i(R)i(x)hVV group               *
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
C*	LSEC5           INTEGER         Length of section 5 in report   *
C*      ISEC5           INTEGER         Pointer to start of section 5   *
C*	IRET		INTEGER         Return code                     *
C*				         0 = normal return              *
C*                                       1 = problems                   *
C* 								        *
C**								        *
C* Log:								        *
C* R. Hollern/NCEP       4/96                                           *
C* R. Hollern/NCEP       8/96  Added check on section lengths           *
C* R. Hollern/NCEP      12/96  Corrected error computing section lengths*
C* R. Hollern/NCEP       1/98  Cleaned up code                          *
C* A. Hardy/GSC          1/98  Added GEMINC                             *
C* R. Hollern/NCEP       2/98  Redefined the length of section 1        *
C************************************************************************
        INCLUDE 	'GEMPRM.PRM'
        INCLUDE 	'lscmn.cmn'
C*
        CHARACTER*(*)   lsfrpt
C*
        LOGICAL  more1
C------------------------------------------------------------------------
        iret  = 0
        more1 = .true.
        lsec1 = 0
        lsec2 = 0
        lsec3 = 0
        lsec4 = 0
        lsec5 = 0
        isec1 = 0
        isec2 = 0
        isec3 = 0
        isec4 = 0
        isec5 = 0
        lrpt  = lszrpt
C
C*      If there is a section 555, determine its length.
C
        i555 = INDEX ( lsfrpt ( 1:lszrpt ), ' 555 ' )
C
        IF ( i555 .gt. 0 ) THEN
            isec5 = i555 + 3
            lsec5 = lszrpt - isec5
            lrpt  = i555 
        END IF
C
        i444 = INDEX ( lsfrpt ( 1:lrpt ), ' 444 ' )
C
        IF ( i444 .gt. 0 ) THEN
            isec4 = i444 + 3
            lsec4 = lrpt - isec4
            lrpt  = i444 
        END IF
C
        i333 = INDEX ( lsfrpt ( 1:lrpt ), ' 333 ' )
C
        IF ( i333 .gt. 0 ) THEN
            isec3 = i333 + 3
            lsec3 = lrpt - isec3 
            lrpt  = i333 
        END IF
C
        i222 = INDEX ( lsfrpt ( 16:lrpt ), ' 222' )
C
        IF ( i222 .gt. 0 ) THEN
            i222  = i222 + 15
            isec2 = i222 + 4
            lsec2 = lrpt - isec2
            lrpt  = i222 
        END IF
C
C*      Start and length of Section 1.
C
        isec1 = ipt
        lsec1 = lrpt - ipt + 1

C*      If section lengths are too long, reject report.
C*      This can happen if reports are not separated from
C*      each other by a report separator.

        IF ( lsec1 .gt. 75   .or.  lsec2 .gt. 70   .or.
     +       lsec3 .gt. 100 ) iret = 1
C*
        RETURN 
        END
