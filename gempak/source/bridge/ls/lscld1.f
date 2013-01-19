        SUBROUTINE  LS_CLD1( lsfrpt, ipt, iret )
C************************************************************************
C* LS_CLD1                                                              *
C*                                                                      *
C* This subroutine decodes the section 1 cloud group 8N(h)C(L)C(M)C(H). *
C* The height of the lowest cloud level observed is determined from     *
C* the three levels.                                                    *
C*                                                                      *
C* LS_CLD1  ( LSFRPT, IPT, IRET )                                       *
C*                                                                      *
C* Input parameters:                                                    *
C*                                                                      *
C*      LSFRPT          CHAR*	        Report array                    *
C*      IPT             INTEGER         Points to 'N' in 8NCCC group    *
C*      HGTLCL          REAL            Height in hundredths of feet    *
C*                                      of the lowest cloud base        *
C*                                                                      *
C* Input and Output parameters:                                         *
C*      IPT             INTEGER         On input, points to 'N' in 8NCCC*
C*                                      group; on output, to the last C *
C*                                                                      *
C* Output parameters:                                                   *
C*      RIVALS(IRCFRL)  REAL            Low or mid cloud amount, WMO    *
C*                                      code table 2700                 *
C*      RIVALS(IRCTYL)  REAL            Low cloud genera, WMO table 0513*
C*      RIVALS(IRCLHL)  REAL            Low cloud height, 100s of feet  *
C*      RIVALS(IRCTYM)  REAL            Mid cloud genera, WMO table 0515*
C*      RIVALS(IRCLHM)  REAL            Mid cloud height, 100s of feet  *
C*      RIVALS(IRCTYH)  REAL            High cloud genera, WMO tbl 0509 *
C*      RIVALS(IRCLHH)  REAL            High cloud height, 100s of feet *
C*      RIVALS(IRNCLO)  REAL            Number of layers of cloud data  *
C*      RIVALS(IRVSSO)  REAL            Vertical significance           *
C*      RIVALS(IRCLAM)  REAL            Cloud amount                    *
C*      RIVALS(IRCLTP)  REAL            Cloud type                      *
C*      RIVALS(IRHOCB)  REAL            Height of base of cloud         *
C*	IRET            INTEGER         Return code                     *
C*				   	 0 = normal return 	        *
C*                                       1 = problems                   *
C*                                                                      *
C**                                                                     *
C* Log:							                *
C* R. Hollern/NCEP       4/96                                           *
C* R. Hollern/NCEP      12/96   Replaced ST_C2R with ST_INTG            *
C* R. Hollern/NCEP       1/98   Changed interface                       *
C************************************************************************
        INCLUDE          'GEMPRM.PRM'
        INCLUDE          'lscmn.cmn'
C*
        CHARACTER*(*)    lsfrpt
C*
        CHARACTER        fld1*1
C------------------------------------------------------------------------
        iret = 0
C
        IF ( lsfrpt(ipt:ipt+3) .ne. '////' ) THEN
C
C*          Specify vertical significance per WMO BUFR TABLE 0 08 002.
C
            xverts = 0.0
          ELSE
            ipt = ipt + 3
            RETURN
        END IF
C
C*      Get amount of the low clouds or middle clouds present.
C
        IF ( lsfrpt ( ipt:ipt ) .ne. '/' ) THEN
            fld1 = lsfrpt (ipt:ipt)
            CALL  ST_INTG ( fld1, ival, ier )
            IF ( ier .eq. 0 ) THEN
                xcfrl = FLOAT ( ival )
                rivals (ircfrl ) = xcfrl
            END IF
          ELSE
C
C*          Cloud amount is indiscernible.
C
            xcfrl = 15.
        END IF
C
        iflag = 0
C
C*      Get low-level cloud type.
C
        ipt = ipt + 1
        rivals ( irvsso ( 1 ) ) = xverts
C
        IF ( lsfrpt ( ipt:ipt ) .ne. '/' ) THEN
            fld1 = lsfrpt ( ipt:ipt )
            CALL  ST_INTG ( fld1, ival, ier )
            IF ( ier .eq. 0 ) THEN
                rivals ( irctyl ) = FLOAT ( ival )
                rivals ( ircltp ( 1 ) ) = rivals ( irctyl ) + 30.
                IF ( lsfrpt ( ipt:ipt ) .ne. '0' ) THEN
                    iflag = 1
                    rivals ( irclam ( 1 ) ) = xcfrl
                    IF ( hgtlcl .ne. RMISSD )  THEN
                        rivals ( irclhl ) = hgtlcl
                        rivals ( irhocb ( 1 ) ) = hgtlcx
                    END IF
                END IF
            END IF
          ELSE
C
C*          Clouds are invisible owing to darkness, fog, or etc.
C
            rivals ( ircltp ( 1 ) ) = 62.
        END IF
C
C*      Get middle-level cloud type.
C
        ipt = ipt + 1
        rivals ( irvsso ( 2 ) ) = xverts
C
        IF ( lsfrpt ( ipt:ipt ) .ne. '/' ) THEN
            fld1 = lsfrpt ( ipt:ipt )
            CALL  ST_INTG ( fld1, ival, ier )
            IF ( ier .eq. 0 ) THEN
                rivals ( irctym ) = FLOAT ( ival )
                rivals ( ircltp ( 2 ) ) = rivals ( irctym ) + 20.
                IF ( lsfrpt (ipt:ipt) .ne. '0' .and. iflag .eq. 0 ) THEN
                    iflag = 1
                    rivals ( irclam ( 2 ) ) = xcfrl
                    IF ( hgtlcl .ne. RMISSD )  THEN
                        rivals ( irclhm ) = hgtlcl
                        rivals ( irhocb ( 2 ) ) = hgtlcx
                    END IF
                END IF
            END IF
          ELSE
            rivals ( ircltp ( 2 ) ) = 61.
        END IF
C
C*      Get high-level cloud type.
C
        ipt = ipt + 1
        rivals ( irvsso ( 3 ) ) = xverts
C
        IF ( lsfrpt ( ipt:ipt ) .ne. '/' ) THEN
            fld1 = lsfrpt ( ipt:ipt )
            CALL  ST_INTG( fld1, ival, ier )
            IF ( ier .eq. 0 ) THEN
                rivals ( irctyh ) = FLOAT ( ival )
                rivals ( ircltp ( 3 ) ) = rivals ( irctyh ) + 10.
                IF ( lsfrpt (ipt:ipt) .ne. '0' .and. iflag .eq. 0 ) THEN
                    IF ( hgtlcl .ne. RMISSD )  THEN
                        rivals ( irclhh ) = hgtlcl
                        rivals ( irhocb ( 3 ) ) = hgtlcx
                    END IF
                END IF
            END IF
          ELSE
            rivals ( ircltp ( 3 ) ) = 60.
        END IF
C
C*      Store number of layers of cloud data.
C
        rivals ( irnclo ) = 3
C*
        RETURN
        END
