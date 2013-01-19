        SUBROUTINE  MA_LSCB ( mszrpt, marrpt, ipt, iret )
C************************************************************************
C* MA_LSCB				 		                *
C*							                *
C* This subroutine gets the length and starting position of each of the *
C* sections 1-5 in the WMO FM13 report.  If a section is missing, then  *
C* its length and starting position are set to zero.                    *
C*								        *
C* MA_LSCB  ( MSZRPT, MARRPT, IPT, IRET )                               *
C*								        *
C* Input parameters:						        *
C*      MSZRPT          INTEGER         Length of report in bytes       *
C*      MARRPT          CHAR*           Report array                    *
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
C*				          0 = Normal return             *
C*                                        1 = Problems                  *
C**								        *
C* Log:								        *
C* R. Hollern/NCEP      4/96                                            *
C* R. Hollern/NCEP      8/96    Added check on section lengths          *
C* R. Hollern/NCEP     12/96    Modified logic to compute length of     *
C*                              sections                                *
C* D. Kidwell/NCEP	4/97	Cleaned up code and documentation       *
C* D. Kidwell/NCEP     10/97	Documentation                           *
C* R. Hollern/NCEP      1/98    Corrected decoding problem finding start*
C*                              of the marine section in report         *
C* R. Hollern/NCEP      5/00    Corrected problem with finding the start*
C*                              of section 2 in CMAN report when sect 1 *
C*                              was less than 35 characters in length   *
C* C. Caruso Magee/NCEP 6/01    Corrected problem w/ finding length of  *
C*                              each section (was returning length that *
C*                              was short by one character, so wasn't   *
C*                              decoding some data as section length    *
C*                              would occasionally return as 0 instead  *
C*                              of 1). Added 1 to lsec1, 2, 3, and 4.	*
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
        lsec5 = 0
        isec1 = 0
        isec2 = 0
        isec3 = 0
        isec4 = 0
        isec5 = 0
        lrpt  = mszrpt
C
C*      If there is a section 555, determine its length.
C
        i555 = INDEX ( marrpt (1:mszrpt), ' 555 ' )
C
        IF ( i555 .gt. 0 ) THEN
           isec5 = i555 + 3
           lsec5 = mszrpt - isec5
           lrpt  = i555 
        END IF
C
        i444 = INDEX ( marrpt (1:lrpt), ' 444 ' )
C
        IF ( i444 .gt. 0 ) THEN
           isec4 = i444 + 3
           lsec4 = lrpt - isec4
           lrpt  = i444 
        END IF
C
        i333 = INDEX ( marrpt (1:lrpt), ' 333 ' )
C
        IF ( i333 .gt. 0 ) THEN
           isec3 = i333 + 3
           lsec3 = lrpt - isec3
           lrpt  = i333 
        END IF
C
        IF ( ibrtyp .eq. 2 ) THEN
            i222 = INDEX ( marrpt (1:lrpt), ' 222//' )
            IF ( i222 .gt. 0 ) THEN
                isec2 = i222 + 4
                lsec2 = lrpt - isec2
                lrpt  = i222
            END IF
          ELSE
            i222 = INDEX ( marrpt (35:lrpt), ' 222' )
            IF ( i222 .gt. 0 ) THEN
                i222 = i222 + 34
                isec2 = i222 + 4
                lsec2 = lrpt - isec2
                lrpt  = i222
            END IF
        END IF
C
C*      Get start and length of Section 1.
C
        isec1 = ipt
        lsec1 = lrpt - ipt
C
C*      If section lengths are too long, reject report.
C*	This can happen if reports are not separated from
C*	each other by a report separator.
C
        IF ( lsec1 .gt. 75   .or.  lsec2 .gt. 70   .or.
     +       lsec3 .gt. 100 )  iret = 1
C*
        RETURN 
        END
