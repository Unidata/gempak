        SUBROUTINE DC_BSRH ( ival, kray, nelem, lpn, jret )
C************************************************************************
C* DC_BSRH                                                              *
C*                                                                      *
C* This subroutine finds the position L of an integer I in an array K   *
C* using the binary search method.                                      *
C*                                                                      *
C* DC_BSRH ( IVAL, KRAY, NELEM, LPN, JRET )                             *
C*                                                                      *
C* Input parameters:                                                    *
C*      IVAL           INTEGER      Search target                       *
C*      KRAY (NELEM)   INTEGER      Ascending array of integers         *
C*      NELEM          INTEGER      Number of integers in KRAY          *
C*					                                *
C* Output parameters:                                                   *
C*      LPN            INTEGER      Position of IVAL in KRAY            *
C*	JRET	       INTEGER      Return code                         *
C*				      0 = Found IVAL in KRAY            *
C**                                  -1 = Did not find IVAL in KRAY     *
C* Log:                                                                 *
C* R. Hollern/NCEP       6/96                                           *
C* R. Hollern/NCEP      12/97 	Cleaned up code                         *
C* D. Kidwell/NCEP      12/97	Renamed from UT_BSRH, cleaned up        *
C************************************************************************
        INTEGER   kray(*)
C*
        LOGICAL   again
C------------------------------------------------------------------------
        jret = 0
        lpn = 0
        again = .true.
C
        IF ( nelem .le. 0 ) THEN
            jret = -1
            RETURN
        END IF
C
        IF ( nelem .eq. 1 ) THEN
            IF ( ival .eq. kray (1) ) THEN
                lpn = 1
              ELSE
                jret = -1
            END IF
            RETURN
        END IF
C
        ibeg = 1
        iend = nelem
C
        DO WHILE ( again )
C
            mid = (ibeg + iend) / 2
C
            IF ( ival .eq. kray (mid) ) THEN
                lpn = mid
                again = .false.
              ELSE IF ( iend .eq. (ibeg+1) ) THEN
                again = .false.
C
C*              Ibeg and iend are consecutive integers.
C
                IF ( ival .eq. kray (iend) ) THEN
                    lpn = iend
                  ELSE IF ( ival .eq. kray (ibeg) ) THEN
                   lpn = ibeg
                  ELSE
C
C*                  No match was found.
C
                    jret = -1
                END IF
              ELSE IF ( ival .lt. kray (mid) ) THEN
C
C*              Iblkst is in first half of table search.
C
                iend = mid
              ELSE 
C
C*              Iblkst is in second half of table search.
C
                ibeg = mid
            END IF

        END DO
C*
	RETURN
	END
