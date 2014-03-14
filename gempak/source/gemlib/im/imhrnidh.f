	SUBROUTINE IM_HRNIDH  ( imgfil, ihdr, iret )
C************************************************************************
C* IM_HRNIDH								*
C*									*
C* This subroutine parses the header information from a raw Higher      *
C* Resolution NIDS file	and sets the navigation.			*
C*									*
C* IM_HRNIDH  ( IMGFIL, IHDR, IRET )					*
C*									*
C* Input parameters:							*
C*	IMGFIL		CHAR		Image file name			*
C*	IHDR (39)	INTEGER		HR NIDS header information	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = Invalid image product	*
C*					 -5 = Invalid image navigation	*
C*                                                                      *
C* High Res Data Files Supported by this routine:                       *
C 32 = DHR
C 94 = N0Q
C 99 = N0U
C 134 = DVL
C 135 = EET
C 138 = DSP
C
C * Full high res product list 
C*      32 DHR                                                          *
C*      81 DPA
C*      93 ??
C*      94 N0U,N1U,...  
C*      99 N0Q,N1Q,...   
C*      134 DVL
C*      135 EET
C*      138 DSP
C*      153 ??
C*      154 ??
C*      155 ??
C*      194 ??
C*      195 ??
C*      199 ??
C**									*
C* Log:									*
C* X. Guo/CWS           05/10   Copy from IM_NIDH                       *
C* M. James/Unidata     07/11   Removed down-sampling to 8-bit          *
C* M. James/Unidata     01/14   Fixed reflectivity scale and flagged    *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'IMGDEF.CMN'
	INCLUDE		'AREAFL.CMN'
C*
	CHARACTER*(*)	imgfil
	INTEGER 	ihdr (39)
C*
	BYTE     	ibarr (36)
	INTEGER		idtarr (3), idlvls (16)
	INTEGER*2	iarr2 (78), ivtim2 (2),  ibzsiz2 (2)
	INTEGER*4	inhead (39), ivtim4, ibzsiz4, bzarr(9)
        REAL*4          fphead (39)
	CHARACTER       prdnam*8, units*8, desc*20
	INTEGER		dmask, dscale, doffs, tmask
C*
        EQUIVALENCE     (inhead, iarr2)
	EQUIVALENCE	(ivtim4, ivtim2)
	EQUIVALENCE	(ibzsiz4, ibzsiz2)
	EQUIVALENCE	(bzarr, ibarr)
C------------------------------------------------------------------------
	iret = 0
C
C*	Get the header array into the local array variable.
C
        DO  i = 1, 39
            inhead (i) = ihdr (i)
        END DO
C
C*      Determine whether file needs bytes flipped. If so, swap and save
C*      the four byte data values before flipping each two-byte pair.
C*      The time variable crosses a word boundary, so treat it specially.
C
        IF ( (iarr2 (1) .gt. 255) .or. (iarr2 (1) .lt. 0) ) THEN
            CALL MV_SWP4 ( 1, inhead (3), imlenf )
            CALL MV_SWP4 ( 1, inhead (6), iradlt )
            CALL MV_SWP4 ( 1, inhead (7), iradln )
            CALL MV_SWP4 ( 1, inhead (28), isymof )
            CALL MV_SWP4 ( 1, inhead (29), igrfof )
            CALL MV_SWP4 ( 1, inhead (30), itabof )
            CALL MV_SWP4 ( 1, inhead (34), idllen )
            CALL MV_SWP4 ( 39, inhead, fphead )
            CALL MV_SWP2 ( 39, inhead, inhead )
            ivtim2 (1) = iarr2 (23)
            ivtim2 (2) = iarr2 (22)
            ivtime = ivtim4
            ibzsiz2 (1) = iarr2 (53)
            ibzsiz2 (2) = iarr2 (52)
            ibzsiz = ibzsiz4
            imbswp = 1
        ELSE
            iradlt = inhead (6)
            iradln = inhead (7)
            isymof = inhead (28)
            idllen = inhead (34)
            ivtim2 (1) = iarr2 (22)
            ivtim2 (2) = iarr2 (23)
            ivtime = ivtim4
            imbswp = 0
        END IF

C
C*      Fill 2-byte variables
C
        iprod  = iarr2 (16)
	immode = iarr2 (17)
C
C*      If equal 0 (maintenance mode), do not need to process
C
        IF ( immode .eq. 0 ) THEN 
            iret = -4
            RETURN
        END IF
        ivdate = iarr2 (21)
        ipd3   = iarr2 (30)
        ipd8   = iarr2 (51)

        IF ( ipd8 .eq. 1 ) THEN
           nbret = 36
           CALL IM_BZSEC ( imgfil, ibzsiz, imprsz, isymof, ibarr,
     +       nbret, ier)
           IF ( ier .eq. 0 ) THEN
              DO i=1,9
                 inhead(30+i) = bzarr(i)
              END DO
              IF ( imbswp .eq. 1 ) THEN
                  CALL MV_SWP4 ( 1, inhead (34), idllen )
                  CALL MV_SWP2 ( 9, inhead(31), inhead(31) )
              ELSE
                  idllen = inhead(34)
              END IF
           END IF
        END IF

	DO i = 1, 16
           idlvls (i) = iarr2 (30 + i)
        END DO
C
        ipkcd1 = iarr2 (69)
C
C*      Check for the current higher resolution pack code
C
        IF ( ipkcd1 .ne. 16 ) THEN
            iret = -4
            RETURN
        END IF
C
C*	Radial product-specific variables
C
	nrbins = iarr2 (71) 
C
        imnpix = nrbins * 2
        imnlin = imnpix
        imrdfl = -1
        imdoff = isymof * 2
        imldat = idllen
C
C*	Set product info
C
	imsorc = 7
	imdpth = 1
	imtype = iprod
	isrc   = iarr2 (7)
	CALL TB_NIDS  ( isrc, imtype, prdnam, nlev, units, res, desc,
     +			ierr )
	IF  ( ierr .ne. 0 )  THEN
	    CALL ER_WMSG ( 'TB', ierr, ' ', ier )
	    iret = -4
	    RETURN
	END IF
	CALL ST_RNUL(prdnam, prdnam, lens, ier )
	CALL ST_RNUL(units, units, lens, ier )
	CALL ST_RNUL(desc, desc, lens, ier )
C
C*	Set image calibration (units)
C
	cmcalb = 'RAW '
C
C*	Save radar beam elevation angle for base products
C
	IF ( (imtype .eq. 94) .or. (imtype .eq. 99) .or.
     +        (imtype .eq. 134) .or. (imtype .eq. 135)) THEN
	    rmbelv = ipd3 * 0.1
	ELSE
	    rmbelv = RMISSD
	END IF
C
C*	Determine date. Date stored with the raw data is the number of
C*	days from 1/1/70. Put it into YYYYMMDD form.
C
	nyear = (ivdate - 1 + .5) / 365.25	    
	jyear = 1970 + nyear
	jday  = ivdate - INT (nyear * 365.25 + .25)
	CALL TI_JTOI ( jyear, jday, idtarr, ier )
	imdate =  idtarr (1) * 10000 + idtarr (2) * 100 + idtarr (3)
C
C*	Time stored is number of seconds past UTC midnight. Put it into
C*	HHMMSS form. Use volume scan time.
C
	imtime = (10000 * (ivtime / 3600)) + (100 * MOD (ivtime/60,60)) 
     +		 + MOD (ivtime,60)
C	 
C*	Build data level values
C
	imndlv = nlev
	cmbunt = units
C
	IF ( iprod .eq. 138 ) THEN
	   amin = idlvls ( 1 ) / 100.
	   ainc = idlvls ( 2 ) / 100.
	ELSE
	   amin = idlvls ( 1 ) / 10.
	   ainc = idlvls ( 2 ) / 10.
	END IF

        SELECT CASE (iprod)
           CASE (134)
C      DVL 
C
C*        0 = below threshold
C*        1 = flagged
C*        2-254 = kg/m^2
C*        255 reserved
C
              CALL short_ieee(idlvls(1), fscale)
              CALL short_ieee(idlvls(2), foffset)
              flogstart = float(idlvls(3))
              CALL short_ieee(idlvls(4), flogscale)
              CALL short_ieee(idlvls(5), flogoffset)
              DO i = 1,nlev
                 cmblev ( i ) = ''
              END DO
              DO idl = 1,nlev
                 IF ( ( idl - 1 ) .lt. flogstart ) THEN
                    fval = ( (idl - 1 ) - foffset ) / fscale 
                 ELSE
                    fval = idl - 1
                    fval = exp( ( fval - flogoffset ) / flogscale)
                 ENDIF
                 CALL ST_RLCH ( fval, 2, cmblev (idl), ier )
              END DO
              cmblev(1) = 'ND'
              imndlv = nlev
           CASE (32,94,99)
C    32 = DHR
C    94 = digital reflectivity (N0Q)
C    99 = digital velocity 
C
C*      8-bit reflectivity
C*      0 = missing data = -9999
C*      1 = flagged data = -9999
C*      2-255 = dBZ values
C
C*      for idl = 1, imndlv, imndlv = 256, 
C*      so begin at idl = 3
C
              DO idl = 1, imndlv
                 IF ( idl .eq. 1 ) THEN
                    cmblev (idl) = 'ND'
                 ELSE IF ( idl .gt. 2 ) THEN
                    val  = amin + ( idl - 3 ) * ainc
                    CALL ST_INCH ( int(val), cmblev (idl), ier )
                 END IF
              END DO
           CASE (135)
C       135 = EET Enhanced Echo Tops
C halfword 31 contains the DATA_MASK 127 or 0x7f (hex) identifying the
C    data bits
C halfword 32 contains the DATA_SCALE 1
C halfword 33 contains the DATA_OFFSET 2
C halfword 34 contains the TOPPED_MASK 128 or 0x80
C
              DO i = 1,nlev
                 cmblev ( i ) = ''
              END DO
              dmask = idlvls ( 1 ) 
              dscal = idlvls ( 2 ) 
              doffs = idlvls ( 3 ) 
              tmask = idlvls ( 4 ) 
              DO idl = 2,71
                 val = ( IAND( idl , dmask ) / dscal ) - doffs
                 CALL ST_INCH ( int(val), cmblev (idl), ier )
              END DO
              DO idl = 130,199 
                 val = ( IAND( idl , dmask ) / dscal ) - doffs
                 CALL ST_INCH ( int(val), cmblev (idl), ier )
              END DO
              CALL ST_INCH ( int(70), cmblev (199), ier )
              CALL ST_INCH ( int(70), cmblev (71), ier )
              cmblev ( 130 ) = 'TOP'
           CASE (138)
C
C*      138 = DSP
C
C*      0 = no accumulation
C* for max accumulation =< 2.55
C*      1-255 = accumulation in steps of 0.01 inches 
C* for max accumulation > 2.55 and =< 5.10
C*      1-255 = accumulation in steps of 0.02 inches
C* for max accumulation > 5.10 and =< 7.65
C*      1-255 = accumulation in steps of 0.03 inches
C*      and so on...
C
C*      this is a to-do
              iinc = imndlv / 16.
              DO idl = 1, imndlv
                 val = amin + ( idl - 1 ) * ainc  
                 IF ( MOD ( ( idl - 1 ), iinc ) .eq. 0 ) THEN
                    CALL ST_RLCH ( val, 2, cmblev ( idl ), ier )
                 ELSE
                    cmblev ( idl ) = ' '
                 END IF
              END DO
              cmblev ( 1 ) = 'ND'
           CASE DEFAULT 
C       OTHER
C
	      DO idl = 1, imndlv
                 IF ( idl .eq. 1 ) THEN
                    cmblev (idl) = 'ND'
                 ELSE
                    val = amin + ( idl - 1 ) * ainc
                    CALL ST_RLCH ( val, 1, cmblev (idl), ier )
                 END IF
	      END DO
	END SELECT 
C
C*	Set image subset bound to whole image right now.
C
	imleft = 1
	imtop  = 1
	imrght = imnpix
	imbot  = imnlin
C
	iadir (6)  = 1
	iadir (7)  = 1
	iadir (12) = 1
	iadir (13) = 1
	    
	CALL ST_CTOI ( 'RADR', 1, ianav (1), ier )
	ianav (2)  = imnlin / 2
	ianav (3)  = imnpix / 2
C
C*	Convert floating point lat/lon to DDDMMSS format
C
	deg = ABS ( iradlt * .001)
	ideg = INT ( deg * 100000 )
	idec = MOD ( ideg, 100000 )
	irem = (idec * 3600 + 50000 ) / 100000
 	
	idms = ( ideg / 100000 + irem / 3600 ) * 10000 +
     +	         MOD (irem, 3600) / 60 * 100 + MOD (irem, 60)
	IF ( iradlt .lt. 0 ) idms = -idms
	ianav (4)  = idms

	deg = ABS ( iradln * .001)
	ideg = INT ( deg * 100000 )
	idec = MOD ( ideg, 100000 )
	irem = (idec * 3600 + 50000 ) / 100000
 	
	idms = ( ideg / 100000 + irem / 3600 ) * 10000 +
     +		MOD (irem, 3600) / 60 * 100 + MOD (irem, 60)
C
C*	Set west lon (neg) to match MCIDAS west lon (pos)
C
	IF ( iradln .gt. 0 ) idms = -idms
	ianav (5)  = idms

	ianav (6)  = res * 1000
	ianav (7)  = 0
	ianav (8)  = 0

	CALL GSATMG ( imgfil, iadir, ianav, imleft, imtop, imrght,
     +			  imbot, ier )
C
	IF ( ier .ne. 0 ) iret = -5
C
	RETURN
	END
