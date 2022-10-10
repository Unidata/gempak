	SUBROUTINE IM_NIDH  ( imgfil, ihdr, iret )
C************************************************************************
C* IM_NIDH2								*
C*	- RADMAP wants two decimals places				*
C*									*
C* This subroutine parses the header information from a raw NIDS file	*
C* and sets the navigation.						*
C*									*
C* IM_NIDH  ( IHDR, IRET )						*
C*									*
C* Input parameters:							*
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
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'IMGDEF.CMN'
	INCLUDE		'AREAFL.CMN'
C*
	PARAMETER	( MXBINS = 460, MXROWS = 464 )
C*
	CHARACTER	imgfil
	INTEGER		ihdr (39)
C*
	INTEGER		idtarr (3), idlvls (16)
	INTEGER*2	iarr2 (78), ivtim2 (2), radial, raster (4)
	INTEGER*4	inhead (39), ivtim4
	CHARACTER       prdnam*8, units*8, desc*20
	LOGICAL		crossx
C*
	DATA		radial / X'AF1F' /
	DATA		raster / X'BA0F', X'BA07', X'8000', X'00C0' /
C*
	EQUIVALENCE	(inhead, iarr2)
	EQUIVALENCE	(ivtim4, ivtim2)
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
        IF ( iarr2 (1) .gt. 255 ) THEN
            CALL MV_SWP4 ( 1, inhead (6), iradlt )
            CALL MV_SWP4 ( 1, inhead (7), iradln )
            CALL MV_SWP4 ( 1, inhead (28), isymof )
            CALL MV_SWP4 ( 1, inhead (34), idllen )
	    CALL MV_SWP2 ( 39, inhead, inhead )
	    ivtim2 (1) = iarr2 (23)
	    ivtim2 (2) = iarr2 (22)
	    ivtime = ivtim4
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
        ivdate = iarr2 (21)
        ipd3   = iarr2 (30)
C
	DO i = 1, 16
            idlvls (i) = iarr2 (30 + i)
	END DO
C
        ipd4   = iarr2 (47)
        ipd5   = iarr2 (48)
        ipd6   = iarr2 (49)
        ipd7   = iarr2 (50)
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
C*	Make sure this is a radial or raster message format. Check
C*	product size too.
C
	IF ( ipkcd1 .eq. radial ) THEN
C
	    IF ( (nrbins .lt. 1) .or. (nrbins .gt. MXBINS) ) THEN
		iret = -4
		RETURN
	    END IF
C
	    imnpix = nrbins * 2
	    imnlin = imnpix
	    imrdfl = 1
	    imdoff = isymof * 2 + 28
	    imldat = idllen - 14
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
     +		 ( iprod .eq. 86 ) ) THEN
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
C
		imnpix = (dist / 0.54) + .5
C
C*		Set the image scaling
C
		rmxysc = REAL (ixscal) / iyscal
	    ELSE
		imnpix = imnlin
	    END IF
C
	    imrdfl = 0
	    imdoff = isymof * 2 + 34
	    imldat = idllen - 20
C
	ELSE
	    iret = -4
	    RETURN
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
	IF ( imtype .ge. 16 .and. imtype .le. 30 ) THEN
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
C*	Time stored is number of seconds past UTC midnight.  Put it into
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
	DO idl = 1, imndlv
C
C*	    Data value (or flagged code) is in least signigicant byte
C
	    ival = IAND ( idlvls (idl), X'FF' )
C
C*	    High-order bit might be flag for one of these codes
C
	    IF ( BTEST ( idlvls (idl), 15 )) THEN
		IF ( ival .eq. 0 ) cmblev (idl) = 'BLANK'
		IF ( ival .eq. 1 ) cmblev (idl) = 'TH'
		IF ( ival .eq. 2 ) cmblev (idl) = 'ND'
		IF ( ival .eq. 3 ) cmblev (idl) = 'RF'
	    ELSE
C
C*		Check for scaled data.  If bit 13 is set, scale by .05,
C*		for bit 12, scale by 0.1
C
		IF ( BTEST ( idlvls (idl), 13 ) ) THEN
		    val = ival * .05
		    CALL ST_RLCH ( val, 2, cmblev (idl), ier )
		ELSE IF ( BTEST ( idlvls (idl), 12 )) THEN
		    val = ival * .1
		    CALL ST_RLCH ( val, 2, cmblev (idl), ier )
		ELSE
		    CALL ST_INCH ( ival, cmblev (idl), ier )
		END IF
C
C*		Special character prefixes to data values
C
		IF ( BTEST ( idlvls (idl), 11 ))
     +		    cmblev (idl) = '>'//cmblev (idl)(1:)
		IF ( BTEST ( idlvls (idl), 10 ))
     +		    cmblev (idl) = '<'//cmblev (idl)(1:)
		IF ( BTEST ( idlvls (idl),  9 ))
     +		    cmblev (idl) = '+'//cmblev (idl)(1:)
		IF ( BTEST ( idlvls (idl),  8 ))
     +		    cmblev (idl) = '-'//cmblev (idl)(1:)		
	    END IF
	END DO
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
