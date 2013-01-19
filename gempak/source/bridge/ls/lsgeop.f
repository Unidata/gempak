        SUBROUTINE  LS_GEOP ( lsfrpt, isurf, ipt, iret )
C************************************************************************
C*                                                                      *
C* LS_GEOP                                                              *
C*                                                                      *
C* This subroutine decodes the group 4a(3)hhh, which contains the       *
C* geopotential height of an agreed standard isobaric surface.          *
C*                                                                      *
C* LS_GEOP  ( LSFRPT, ISURF, IPT, IRET )                                *
C*                                                                      *
C* Input parameters:                                                    *
C*      LSFRPT         CHAR*	         Report array                   *
C*      ISURF          CHAR*1	         WMO Table 0264 code figure     *
C*					                                *
C* Input and Output parameters:                                         *
C*      IPT            INTEGER           On input, points to 'a'; on    *
C*                                       output, points to the last h   *
C*                                       in the group 4ahhh             *
C*					                                *
C* Output parameters:                                                   *
C*      RIVALS(IRGHTM) REAL              Geopotential height            *
C*      RIVALS(IRPRLC) REAL              Standard isobaric surface for  *
C*                                       the geopotential               *
C*	IRET           INTEGER           Return code                    *
C*				   	 0 = normal return 	        *
C*                                                                      *
C**                                                                     *
C* Log:							                *
C* R. Hollern/NCEP      11/96                                           *
C* R. Hollern/NCEP       1/98   Changed interface, cleaned up code      *
C* A. Hardy/GSC          1/98	Reordered calling sequence              *
C************************************************************************
        INCLUDE          'GEMPRM.PRM'
        INCLUDE          'lscmn.cmn'
C*
        CHARACTER*(*)     lsfrpt
C*
        CHARACTER         fld3*3,   isurf*1
C------------------------------------------------------------------------
        iret = 0
        iflg = 0
           
        fld3 = lsfrpt ( ipt+1:ipt+3 )
        ipt = ipt + 3
        CALL  ST_INTG ( fld3, ival, ier )
C
        IF ( ier .ne. 0 ) RETURN
C
        rval = FLOAT ( ival )
C
        IF ( isurf .eq. '8' ) THEN
C
C*          Decoding the 850 mb surface.
C
            hgtm = rval + 1000.
            prlc = 850.
            iflg = 1
          ELSE IF ( isurf .eq. '7' ) THEN
C
C*          Decoding the 700 mb surface.
C
            IF ( rval .lt. 500 ) THEN
                hgtm = rval + 3000.
              ELSE
                hgtm = rval + 2000.
            END IF
            prlc = 700.
            iflg = 1
          ELSE IF ( isurf .eq. '1' ) THEN
C
C*          Decoding the 1000 mb surface.
C
            hgtm = rval
            prlc = 1000.
            iflg = 1
          ELSE IF ( isurf .eq. '2' ) THEN
C
C*          Decoding the 925 mb surface.
C
            hgtm = rval
            prlc = 925.
            iflg = 1
          ELSE IF ( isurf .eq. '5' ) THEN
C
C*          500 mb surface (cannot tell if height below 5000m).
C
        END IF
C
        IF ( iflg .eq. 1 ) THEN
            rivals ( irhgtm ) = hgtm
            rivals ( irprlc ) = prlc
        ENDIF
C*
        RETURN
        END
