	SUBROUTINE IM_NIDH  ( imgfil, ihdr, iret )
C************************************************************************
C* IM_NIDH								*
C*									*
C* This subroutine parses the header information from a raw NIDS file	*
C* and sets the navigation.						*
C*									*
C* IM_NIDH  ( IMGFIL, IHDR, IRET )					*
C*									*
C* Input parameters:							*
C*	IMGFIL		CHAR		Image file name			*
C*	IHDR (39)	INTEGER		NIDS header information		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = Invalid image product	*
C*					 -5 = Invalid image navigation	*
C**									*
C* Log:									*
C* J. Cowie/COMET	 2/95						*
C* C. Lin/EAI	 	 6/95  	change input filename -> lunmf		*
C* J. Cowie/COMET	10/95	Add image size maximum check, set xy	*
C*				image scaling to 1.0			*
C* J. Cowie/COMET	 4/96	Added calib setting			*
C* J. Cowie/COMET	 1/97	Changed IMGDEF common variable names	*
C* S. Chiswell/UNIDATA	12/97	Handle byte flipping			*
C* J. Cowie/COMET	12/97	Added imradf, removed unused variables,	*
C*				add cross section products		*
C* S. Jacobs/NCEP	12/97	Added TB_NIDS; Removed NIPRM.PRM	*
C* T. Piper/GSC		11/98	Updated prolog				*
C* S. Jacobs/NCEP	 5/99	Added setting of imbswp for byte-swap	*
C* S. Jacobs/NCEP	 2/00	Use the 1st time in the file, not 2nd	*
C* S. Jacobs/NCEP	11/00	Copied from IM_NIHD			*
C* J. Cowie/COMET	12/00	Added MCRADR projection type		*
C* S. Jacobs/NCEP	12/00	Moved setting imleft,imrght,imbot,imtop	*
C* S. Jacobs/NCEP	12/00	Added include of AREAFL.CMN		*
C* S. Jacobs/NCEP	 7/01	Use the SCAN date/time (2nd)		*
C* D.W.Plummer/NCEP      9/01   Set imbswp=0 when no byte swapping      *
C* S. Chiswell/Unidata	10/01	Added immode				*
C* S. Chiswell/Unidata	 6/04	Added non-raster/radial products	*
C* S. Chiswell/Unidata	12/08	Added flip check when iprod lt 0	*
C* S. Chiswell/Unidata	 2/10	Added TDWR/high res NIDS support        *
C* M. James/Unidata	 2/10	Modified imdoff                         *
C* M. James/Unidata	06/10	Moved label frequency logic to IMCBAR   *
C* X. Guo/CWS		04/10   Added codes to support 94 product       *      
C* X. Guo/CWS           05/10   Added IM_HRNIDH to handle the higher    *
C*                              resolution products			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'IMGDEF.CMN'
	INCLUDE		'AREAFL.CMN'
C*
	PARAMETER	( MXBINS = 460, MXROWS = 464 )
C*
	CHARACTER*(*)	imgfil
	INTEGER		ihdr (39)
C*
	BYTE		ibarr(36)
	INTEGER		idtarr (3), idlvls (16)
	INTEGER*2	iarr2 (78), ivtim2 (2), radial, raster (4),
     +			ibzsiz2 (2), digrad
	INTEGER*4	inhead (39), ivtim4, ibzsiz4, bzarr(9)
	CHARACTER       prdnam*8, units*8, desc*20
	LOGICAL		crossx
C*
	DATA		radial / X'AF1F' /
	DATA		raster / X'BA0F', X'BA07', X'8000', X'00C0' /
	DATA		digrad / 16 /
C*
	EQUIVALENCE	(inhead, iarr2)
	EQUIVALENCE	(ivtim4, ivtim2)
	EQUIVALENCE	(ibzsiz4, ibzsiz2)
	EQUIVALENCE	(bzarr, ibarr)
C------------------------------------------------------------------------
	iret = 0
	rmxysc = 1.0
	crossx = .false.
	imradf = 1
C
C*	Get the header array into the local array variable.
C
	DO  i = 1, 39
	    inhead (i) = ihdr (i)
	END DO
C
C*	Determine whether file needs bytes flipped. If so, swap and save
C*	the four byte data values before flipping each two-byte pair.
C*	The time variable crosses a word boundary, so treat it specially.
C
        IF ( (iarr2 (1) .gt. 255) .or. (iarr2 (1) .lt. 0 )) THEN
            CALL MV_SWP4 ( 1, inhead (3), imlenf )
            CALL MV_SWP4 ( 1, inhead (6), iradlt )
            CALL MV_SWP4 ( 1, inhead (7), iradln )
            CALL MV_SWP4 ( 1, inhead (28), isymof )
            CALL MV_SWP4 ( 1, inhead (29), igrfof )
            CALL MV_SWP4 ( 1, inhead (30), itabof )
            CALL MV_SWP4 ( 1, inhead (34), idllen )
	    CALL MV_SWP2 ( 39, inhead, inhead )
	    ivtim2 (1) = iarr2 (23)
	    ivtim2 (2) = iarr2 (22)
	    ivtime = ivtim4
            ibzsiz2 (1) = iarr2 (53)
            ibzsiz2 (2) = iarr2 (52)
            ibzsiz = ibzsiz4
	    imbswp = 1
	ELSE
            imlenf = inhead (3)
            iradlt = inhead (6)
            iradln = inhead (7)
            isymof = inhead (28)
            igrfof = inhead (29)
            itabof = inhead (30)
            idllen = inhead (34)
	    ivtim2 (1) = iarr2 (22)
	    ivtim2 (2) = iarr2 (23)
	    ivtime = ivtim4
            ibzsiz2 (1) = iarr2 (52)
            ibzsiz2 (2) = iarr2 (53)
            ibzsiz = ibzsiz4
	    imbswp = 0
        END IF
C
C*      Fill 2-byte variables
C
        iprod  = iarr2 (16)
	immode = iarr2 (17)
C
C*      For Higher Resolution product, call IM_HRNIDH.
C
C 32 = DHR
C 94 = N0Q
C 99 = N0U
C 134 = DVL
C 135 = EET
C 138 = DSP
C
        SELECT CASE (iprod)
           CASE (32,94,99,134,135,138) 
              CALL IM_HRNIDH (imgfil, ihdr, iret)
              imdoff = 2 * isymof
              RETURN
C
C *     Dual pol products
C
C 159 = ZDR
C 161 = DCC
C 163 = KDP
C 165 = DHC
C 170 = DAA
C 172 = DTA
C 173 = DUA
C 174 = DOD
C 175 = DSD
C 176 = INST/DPR (not supported)
C 177 = HHC
C
           CASE (159,161,163,165,170,172,173,174,175,177)
              CALL IM_DPNIDH (imgfil, ihdr, iret)
              imdoff = 2 * isymof
              RETURN
        END SELECT
C        END IF
        ivdate = iarr2 (21)
        ipd3   = iarr2 (30)
        ipd4   = iarr2 (47)
        ipd5   = iarr2 (48)
        ipd6   = iarr2 (49)
        ipd7   = iarr2 (50)
        ipd8   = iarr2 (51)
C
C*	Check for products using compression
C
C
C 171 = PTA
C 171 = PTA
	IF ( ( iprod .eq. 48 ) .or.
     +		( iprod .eq. 149 ) .or. ( iprod .eq. 152 ) .or.
     +		( iprod .eq. 153 ) .or. ( iprod .eq. 154 ) .or.
     +		( iprod .eq. 155 ) .or. ( iprod .eq. 180 ) .or.
     +		( iprod .eq. 171 ) .or. ( iprod .eq. 169 ) .or.
     +		( iprod .eq. 182 ) .or. ( iprod .eq. 186 ) ) THEN 
	   IF ( ipd8 .eq. 1 ) THEN
	      nbret = 36
	      CALL IM_BZSEC ( imgfil, ibzsiz, imprsz, isymof, ibarr, 
     +		nbret, ier)
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
	END IF
C
C*	Threhold values for specific products
C
	DO i = 1, 16
            idlvls (i) = iarr2 (30 + i)
	END DO
C
        ipkcd1 = iarr2 (69)
C
C*	Radial product-specific variables
C
	nrbins = iarr2 (71) 
C
C*	Raster product-specific variables
C
        ipkcd2 = iarr2 (70)
        ipkcd3 = iarr2 (71)
	ixscal = iarr2 (74)
	iyscal = iarr2 (76)
	nrows  = iarr2 (78)
C
C*	Store block offsets
C
	imnblk = iarr2(9)
	imboffs(1) = isymof
	imboffs(2) = igrfof
	imboffs(3) = itabof
C
C*	Make sure this is a radial or raster message format. Check
C*	product size too.
C
	IF ( ( ipkcd1 .eq. radial ) .or . ( ipkcd1 .eq. digrad ) ) THEN
            rmxres = iarr2 (74) / 1000.0
            rmyres = iarr2 (74) / 1000.0
C
	    IF ( nrbins .lt. 1) THEN
C		write(*,*) 'invalid radial image bins ',nrbins
		iret = -4
		RETURN
	    END IF
C
	    imnpix = nrbins * 2
	    imnlin = imnpix
	    IF ( ipkcd1 .eq. digrad ) THEN
	       imrdfl = -1
C	       imdoff = imprsz + isymof
	       imdoff = 2 * isymof
	       imldat = idllen
	    ELSE
	       imrdfl = 1
	       imdoff = isymof * 2 + 28
	       imldat = idllen - 14
	    END IF
C
	ELSE IF	(( ipkcd1 .eq. raster (1)  .or. 
     +	           ipkcd1 .eq. raster (2)) .and.
     +	           ipkcd2 .eq. raster (3)  .and.
     +	           ipkcd3 .eq. raster (4)) THEN
C
	    IF ( (nrows .lt. 1) .or. (nrows .gt. MXROWS) ) THEN
		iret = -4
		RETURN
	    END IF
C
	    imnlin = nrows
C
C*	    Check for cross section image.
C
	    IF ( ( iprod .eq. 50 ) .or.
     +		 ( iprod .eq. 51 ) .or.
     +		 ( iprod .eq. 52 ) .or.
     +		 ( iprod .eq. 85 ) .or.
     +		 ( iprod .eq. 86 )) THEN
C
		crossx = .true.
C
C*		Calculate the distance of the x-section using law of
C*		Cosines.
C
		az1 = ipd4 / 10.
		ra1 = ipd5 / 10.
		az2 = ipd6 / 10.
		ra2 = ipd7 / 10.
C		
		angle = ABS ( az1 - az2 ) * DTR
		CALL PRNLNR ( 1, angle, ier )
		dist = SQRT ( ra1 * ra1 + ra2 * ra2 -
     +				2 * ra1 * ra2 * COS ( angle ) )
C
C*		Convert to km. This is the width of the image in pixels.
		imnpix = (dist / 0.54) + .5
C
C*		Set the image scaling
C
		rmxysc = REAL (ixscal) / iyscal
	    ELSE
		imnpix = imnlin
	    END IF
	    imrdfl = 0
	    imdoff = isymof * 2 + 34
	    imldat = idllen - 20
	ELSE
C
C*	    not radial or raster - use packet codes
C
	    imnlin = 460
	    imnpix = imnlin
	    imldat = 0
C
C*	    Use imldat to signal a non-radaial image
C*	    unfortunately, we are keying on imnlin,imnpix below
C
	    imrdfl = 2
	    imdoff = isymof * 2
c	    iret = -4
c	    RETURN
	END IF
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
	    write(*,*) 'got a non-zero tbnids value ',iprod,ierr
	    IF ( imrdfl .lt. 2 ) THEN
		CALL ER_WMSG ( 'TB', ierr, ' ', ier )
		iret = -4
		RETURN
	    ELSE
		nlev = 0
		cmbunt = ' '
		res = 1.0
	    END IF
	END IF
C
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
	IF ( ( imtype .ge. 16 .and. imtype .le. 30 ) .or.
     +		( imtype .eq. 180 ) .or. ( imtype .eq. 181 ) .or.
     +		( imtype .eq. 182 ) .or. ( imtype .eq. 183 ) .or.
     +		( imtype .eq. 185 ) .or. ( imtype .eq. 186 ) .or.
     +		( imtype .eq. 187 ) ) 
     +		THEN
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
	cmbunt = units
C
	IF ( iprod .eq. 81 ) THEN
	   dbamin = idlvls ( 1 ) / 10.
	   dbainc = idlvls ( 2 ) / 1000.
	   imndlv = idlvls ( 3 )
	   iinc = imndlv
	   IF ( iinc .gt. 16 ) THEN
		iinc = imndlv / 16
	   END IF
	   DO idl = 1, imndlv
	      IF ( ( idl .eq. 1 ) .or. ( idl .eq. imndlv ) .or.
     +		 ( MOD ( ( idl - 1 ), iinc ) .eq. 0 ) ) THEN
	         dba = dbamin + ( idl - 1 ) * dbainc
C		 dBA to mm
                 val = 10. ** ( dba / 10. )
	         CALL ST_RLCH ( val, 1, cmblev (idl), ier )
	      ELSE
	         cmblev ( idl ) = ' '
	      END IF
	   END DO
        ELSE IF ( ( iprod .eq. 32 ) .or. ( iprod .eq. 93 ) .or.
     +		( iprod .eq. 138 ) .or. ( iprod .eq. 153 ) .or.
     +		( iprod .eq. 154 ) .or. ( iprod .eq. 155 ) .or.
     +		( iprod .eq. 180 ) .or. ( iprod .eq. 182 ) .or.
     +		( iprod .eq. 186 ) ) THEN
	   IF ( iprod .eq. 138 ) THEN
               amin = idlvls ( 1 ) / 100.
               ainc = idlvls ( 2 ) / 100.
	   ELSE
               amin = idlvls ( 1 ) / 10.
               ainc = idlvls ( 2 ) / 10.
	   ENDIF
           imndlv = idlvls ( 3 )
           iinc = imndlv
C           IF ( imndlv .gt. 16 ) THEN
C              iinc = imndlv / 16
C           END IF

           DO idl = 1, imndlv
C              IF ( ( idl .eq. 1 ) .or. ( idl .eq. imndlv ) .or.
C     +           ( MOD ( ( idl - 1 ), iinc ) .eq. 0 ) ) THEN
                 val = amin + ( idl - 1 ) * ainc
                 CALL ST_RLCH ( val, 1, cmblev (idl), ier )
C              ELSE
C                 cmblev ( idl ) = ' '
C              END IF
           END DO
        ELSE IF ( iprod .eq. 134 ) THEN
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
           cmblev(1) = 'ND'
C          cmblev(2) = 'flagged'
           cmblev(2) = ' '
           DO idl = 3,256
               cmblev ( idl ) = ' '
           END DO
           DO idl = 3,256,5
                IF ( ( idl - 1 ) .lt. flogstart ) THEN
                   fval = ( (idl - 1 ) - foffset ) / fscale 
                ELSE
                   fval = idl - 1
                   fval = exp( ( fval - flogoffset ) / flogscale)
                ENDIF
                CALL ST_RLCH ( fval, 2, cmblev (idl), ier )
           END DO
           imndlv = 256
        ELSE
C
C*         Color bar levels etc for radial and raster products
C
            IF ( imrdfl .lt. 2 ) THEN
	       imndlv = nlev
            DO idl = 1, imndlv
                cmblev(idl) = ' '
            END DO
	        DO idl = 1, 16
                    iinc = ( idl - 1 ) * ( imndlv / 16) + 1
C
C*	    Data value (or flagged code) is in least signigicant byte
C
	            ival = IAND ( idlvls (idl), X'FF' )
C
C*	    High-order bit might be flag for one of these codes
C
	            IF ( BTEST ( idlvls (idl), 15 )) THEN
		        IF ( ival .eq. 0 ) cmblev (iinc) = 'BLANK'
			IF ( ival .eq. 1 ) cmblev (iinc) = 'TH'
			IF ( ival .eq. 2 ) cmblev (iinc) = 'ND'
			IF ( ival .eq. 3 ) cmblev (iinc) = 'RF'
	    	    ELSE
C
C*		Check for scaled data. If bit 13 is set, scale by .05,
C*		for bit 12, scale by 0.1
C
		        IF ( BTEST ( idlvls (idl), 13 ) ) THEN
		            val = ival * .05
		            CALL ST_RLCH ( val, 1, cmblev (iinc), ier )
		        ELSE IF ( BTEST ( idlvls (idl), 12 )) THEN
		            val = ival * .1
		            CALL ST_RLCH ( val, 1, cmblev (iinc), ier )
		        ELSE
		            CALL ST_INCH ( ival, cmblev (iinc), ier )
		        END IF
C
C*		Special character prefixes to data values
C
		        IF ( BTEST ( idlvls (idl), 11 ))
     +		            cmblev (iinc) = '>'//cmblev (iinc)(1:)
		        IF ( BTEST ( idlvls (idl), 10 ))
     +		            cmblev (iinc) = '<'//cmblev (iinc)(1:)
		        IF ( BTEST ( idlvls (idl),  9 ))
     +		            cmblev (iinc) = '+'//cmblev (iinc)(1:)
		        IF ( BTEST ( idlvls (idl),  8 ))
     +		            cmblev (iinc) = '-'//cmblev (iinc)(1:)
	            END IF
	        END DO
	    END IF
	END IF
C
C*	Set image subset bound to whole image right now.
C
	imleft = 1
	imtop  = 1
	imrght = imnpix
	imbot  = imnlin
C
C*	Set up graph coordinates for cross section. Save the azimuth/
C*	range values in the color level array.
C
	IF ( crossx ) THEN
C
	    CALL GSMODE  ( 2, ier )
	    CALL GSGMGN  ( 0., 4., 0., 1., ier )
	    CALL ST_INCH ( INT (az1 + .5), cmblev (imndlv + 1), ier )
	    CALL ST_INCH ( INT (ra1 + .5), cmblev (imndlv + 2), ier )
	    CALL ST_INCH ( INT (az2 + .5), cmblev (imndlv + 3), ier )
	    CALL ST_INCH ( INT (ra2 + .5), cmblev (imndlv + 4), ier )
C
C*	For plan images, set the mapping
C
	  ELSE
	    iadir (6)  = 1
	    iadir (7)  = 1
	    iadir (12) = 1
	    iadir (13) = 1
	    
	    CALL ST_CTOI ( 'RADR', 1, ianav (1), ier )
	    ianav (2)  = imnlin / 2
	    ianav (3)  = imnpix / 2
C
C*	    Convert floating point lat/lon to DDDMMSS format
C
	    deg = ABS ( iradlt * .001)
	    ideg = INT ( deg * 100000 )
	    idec = MOD ( ideg, 100000 )
	    irem = (idec * 3600 + 50000 ) / 100000
 	
	    idms = ( ideg / 100000 + irem / 3600 ) * 10000 +
     +		    MOD (irem, 3600) / 60 * 100 + MOD (irem, 60)
	    IF ( iradlt .lt. 0 ) idms = -idms
	    ianav (4)  = idms

	    deg = ABS ( iradln * .001)
	    ideg = INT ( deg * 100000 )
	    idec = MOD ( ideg, 100000 )
	    irem = (idec * 3600 + 50000 ) / 100000
 	
	    idms = ( ideg / 100000 + irem / 3600 ) * 10000 +
     +		    MOD (irem, 3600) / 60 * 100 + MOD (irem, 60)
C
C*	    Set west lon (neg) to match MCIDAS west lon (pos)
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
	END IF
C
	RETURN
	END
C
        SUBROUTINE short_ieee (ishort, fval)
C
        INTEGER*2       smask, emask, fmask
        DATA            smask / X'8000' /
        DATA            emask / X'7C00' /
        DATA            fmask / X'03FF' /
C
        isval = ISHFT(IAND ( ishort, smask),-15)
        IF ( isval .eq. 0 ) THEN
           fmul = 1.0
        ELSE
           fmul = -1.0
        ENDIF
        ieval = ISHFT(IAND ( ishort,emask),-10)
        ifval = IAND ( ishort,fmask )
        IF ( ieval .eq. 0 ) THEN
           fval = fmul * 2.0 * ( ifval / 2.0**10)
        ELSE
           fval = fmul * (2.0**(ieval - 16)) * 
     +                                  (1 + ( ifval / 2.0**10))
        ENDIF
        RETURN
        END
C

      subroutine byteswapr4(r)
      integer*1 ii(4), jj(4)
      real*4 r, s, t
      equivalence (s,ii)
      equivalence (t,jj)
      s = r
      jj(1) = ii(4)
      jj(2) = ii(3)
      jj(3) = ii(2)
      jj(4) = ii(1)
      r = t
      end
