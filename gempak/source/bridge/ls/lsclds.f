        SUBROUTINE  LS_CLDS ( lsfrpt, lvl, ipt, iret )
C************************************************************************
C* LS_CLDS                                                              *
C*                                                                      *
C* This subroutine decodes the section 3 cloud group 8N(s)Ch(s)h(s).    *
C* This group contains the height of the base of the cloud, the cloud   *
C* type, and the height of the cloud.                                   *
C*                                                                      *
C* LS_CLDS  ( LSFRPT, LVL, IPT, IRET ) 	                                *
C*                                                                      *
C* Input parameters:                                                    *
C*      LSFRPT          CHAR*           Report array                    *
C*      LVL             INTEGER         Cloud layer being decoded (1-4) *
C*                                                                      *
C* Input and Output parameters:                                         *
C*	IPT		INTEGER		On input, points to 'N' in 8NChh*
C*					group; on output, to last 'h'   *
C*                                                                      *
C* Output parameters:                                                   *
C*      XCLDS (*)       REAL            Array containing section 3      *
C*                                      cloud data for up to 4 layers   *
C*      RIVALS(IRVRTM)  REAL            Vertical visibility in meters   *
C*	IRET            INTEGER         Return code                     *
C*				   	  0 = normal return 	        *
C*                                        1 = problems                  *
C*                                                                      *
C**								        *
C* Log:							                *
C* R. Hollern/NCEP      6/96                                            *
C* R. Hollern/NCEP      8/96    Added check on number of cloud levels   *
C* R. Hollern/NCEP     12/96    Replaced ST_C2R with ST_INTG            *
C* R. Hollern/NCEP      1/98	Changes based on MA_CLDS                *
C* A. Hardy/GSC         1/98    Reordered calling sequence              *
C* R. Hollern/NCEP      8/99	Added vertical visibility logic         *
C************************************************************************
        INCLUDE 	'GEMPRM.PRM'
        INCLUDE 	'lscmn.cmn'
C*
        CHARACTER*(*)   lsfrpt
C*
        CHARACTER   	fld2*2, fld1*1
	REAL		yverts(0:9)
C*
        SAVE
C*
C*      Vertical significance from WMO BUFR table 0 08 002 
C*
        DATA  yverts / 9., 9., 9., 8., 8., 8., 7., 7., 7., 4. /
C------------------------------------------------------------------------
        iret = 0
C
C*      Get cloud amount referencing WMO BUFR table 0 20 011.
C
        IF ( lsfrpt ( ipt:ipt ) .ne. '/' ) THEN
            fld1 = lsfrpt ( ipt:ipt )
            CALL  ST_INTG ( fld1, ival, ier )
            IF ( ier .eq. 0 )  xclds ( 2, lvl ) = FLOAT ( ival )
          ELSE
C
C*          Cloud amount is indiscernible. 
C
            xclds ( 2, lvl ) = 15.
        END IF
C
C*      Get cloud type referencing WMO code table 500 (WMO BUFR table
C*	0 20 012).
C
        ipt = ipt + 1
        IF ( lsfrpt ( ipt:ipt ) .ne. '/' ) THEN
            fld1 = lsfrpt ( ipt:ipt )
            CALL  ST_INTG ( fld1, ival, ier )
            IF ( ier .eq. 0 ) THEN
                xclds ( 3, lvl ) = FLOAT ( ival )
                xclds ( 1, lvl ) = yverts ( ival )
C
C*              Check to see if it is a cumulonimbus layer.
C
                IF ( ival .eq. 9 ) THEN
                    xclds ( 1, lvl ) = 4.0
                    xclds ( 3, lvl ) = FLOAT ( ival )
                END IF
            END IF
          ELSE
C
C*          Clouds are not visible because of darkness, fog, etc.
C
            xclds ( 3, lvl ) = 59.
        END IF
C
C*      Get height of base of cloud in meters. See WMO table 1677.
C
        ipt = ipt + 1
        fld2 = lsfrpt ( ipt:ipt+1 )
        IF ( fld2 .ne. '//' ) THEN
            CALL  ST_INTG ( fld2, ival, ier )
            IF ( ier .eq. 0 .and.
     +           ( ival .ge. 0 .and. ival .le. 89 ) ) THEN
                xval = FLOAT ( ival )
                IF ( ival .ge. 0 .and. ival .le. 50 ) THEN
                    xx = 30. * xval 
                  ELSE IF ( ival .ge. 56 .and. ival .le. 80 ) THEN
                    xx = 300. * ( xval - 50. )
                  ELSE IF ( ival .ge. 81 .and. ival .le. 88 ) THEN
                    xx = 1500. * ( xval - 80. ) + 9000.
                  ELSE IF ( ival .eq. 89 ) THEN
                    xx = 22000.
                END IF
                IF ( lsfrpt ( ipt-2:ipt-2) .eq. '9' ) THEN
C
C*                  The sky is obscured, and hence, the h(s)h(s) is
C*                  the vertical visibility in meters.
C
                    rivals ( irvrtm ) = xx
                  ELSE
                    xclds ( 4, lvl ) = xx
                END IF
            END IF
        END IF
C
        ipt = ipt + 1   
C*
        RETURN
        END
