        SUBROUTINE  LS_AST0 ( lszrpt, lsfrpt, iaaxx, istarr, ipt, iret )
C************************************************************************
C* LS_AST0                                                              *
C*                                                                      *
C* This subroutine gets the  block/station ID, the report GMT obs time  *
C* and the indicator for the source and units of wind speed.            *
C*                                                                      *
C* LS_AST0 ( LSZRPT, LSFRPT, IAAXX, ISTARR, IPT, IRET )                 *
C*                                                                      *
C* Input parameters:                                                    *
C*      LSZRPT         INTEGER           Report size                    *
C*      LSFRPT         CHAR*             Report array                   *
C*      IAAXX          CHAR*             YYGGi(w) group in AAXX line    *
C*      ISTARR (*)     INTEGER           System time - YYYY,MM,DD,HH,MM *
C*					                                *
C* Output parameters:                                                   *
C*      CIVALS(ICSTID) CHAR*             Report ID                      *
C*      RIVALS(IRCORN) REAL              Corrected report indicator     *
C*      IPT            INTEGER           Pointer to start of next group *
C*      IRET           INTEGER           Return code                    *
C*                                         0 = normal return            *
C*                                         1 = problems                 *
C**                                                                     *
C* Log:                                                                 *
C* R. Hollern/NCEP       7/96                                           *
C* R. Hollern/NCEP       9/96   Added RCTS to the receipt time data     *
C* R. Hollern/NCEP      10/96   Added corrected report indicator logic  *
C* R. Hollern/NCEP       1/98   Changed interface, LS_RTIM -> DC_RTIM   *
C* A. Hardy/GSC          1/98   Added GEMINC, cleaned up                *
C* R. Hollern/NCEP       8/99   Added date/time data to interface array *
C* D. Kidwell/NCEP	 7/02	Moved decode of iaaxx data to LS_AAXX   *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
        INCLUDE         'lscmn.cmn'
C*
        CHARACTER*(*)   lsfrpt, iaaxx
        INTEGER         istarr (*)
C*
        CHARACTER       stnid*8
        LOGICAL         more
C------------------------------------------------------------------------
        iret = 0
        more = .true.
        jp = 0
        ip = 0
        stnid = ' '
C
C*      Get the corrected report indicator.
C
        IF ( bbb ( 1:3 ) .eq. 'COR' ) THEN
            rivals ( ircorn ) = 1.0
          ELSE
            rivals ( ircorn ) = 0.0
        END IF
C
C*      Get block/station ID.
C*      ip points to start of ID. Locate next space, the end of ID.
C
        DO  WHILE ( more )
            ip = ip + 1
            IF ( lsfrpt ( ip:ip ) .ne. ' ' ) THEN
                jp = jp + 1
                IF ( jp .lt. 6 ) stnid ( jp:jp ) = lsfrpt ( ip:ip )
              ELSE
                more = .false.
            END IF
        END DO
C
C*      Save report ID.
C
        civals ( icstid ) = stnid
C
        ipt = ip
C
C*      Decode the data in the iaaxx string (the YYGGi(w) group).
C
	CALL LS_AAXX ( iaaxx, istarr, ier )
C
	IF ( ier .ne. 0 ) THEN
            iret   = 1
	    ierrno = 3
	    CALL LS_ERRS ( ierrno, lsfrpt, ierr )
        END IF
C*
	RETURN
	END
