	SUBROUTINE IM_DBNIDS2GM  ( imgfil, iret )
C************************************************************************
C* IM_DBNIDS2GM								*
C*									*
C* This subroutine reads radar image info from AWIPS II metadata  	*
C* database and sets the navigation.					*
C*									*
C* IM_DBNIDS2GM  ( IMGFIL, IRET )					*
C*									*
C* Input parameters:							*
C*	IMGFIL		CHAR		Image file name			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = Invalid image product	*
C*					 -5 = Invalid image navigation	*
C**									*
C* Log:									*
C* m.gamazaychikov/CWS	01/10		Created				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'IMGDEF.CMN'
	INCLUDE		'AREAFL.CMN'
C*
	PARAMETER	( MXBINS = 460, MXROWS = 464 )
C*
	CHARACTER*(*)	imgfil
C*
	INTEGER		idlvls (16)
	CHARACTER       prdnam*8, units*8, desc*20
C*
        CHARACTER       dbhst*50, qtype*11, src*21, dbtag*15,
     +                  icao*64, prodname*64, elangle*64, radtime*12,
     +                  prmlist(10)*64, radhdr*1024, hdrlist(14)*100,
     +                  ttemp(2)*10, prodnumb*10,
     +                  resoltn*10, levlist(16)*10
        DATA            src   /'RADAR'/
        DATA            qtype /'radHdr'/
C------------------------------------------------------------------------
	iret = 0
	rmxysc = 1.0
	imradf = 1
C
C*      Get the name of AWIPS II database host from prefs table
C
        CALL ST_NULL ( 'A2DB_HOST_NAME', dbtag, lens, ier )
        dbhst = ' '
        CALL CTB_PFSTR ( dbtag, dbhst, ier )
        IF ( ier .ne. 0 ) THEN
           iret = -6
           RETURN
        END IF
        CALL ST_UCLC ( dbhst, dbhst, ier )
        radhdr=" "
        nocc = 1
        CALL ST_NULL ( dbhst, dbhst, ldbhst, ier )
        CALL ST_NULL ( qtype, qtype, lenq, ier )
        CALL ST_NULL ( src, src, lenq, ier )

C
C*      Process the imgfil, and split the content into strings
C*      corresponding to columns in AWIPS II database table:
C*       - icao         - icao
C*       - prodnumb     - product icode number
C*       - elangle      - true elevation angle
C*       - resoltn      - gate resolution
C*       - radtime      - the time stamp of the image
C
        CALL ST_NOCC ( imgfil, '|', nocc, ibar, ier)
        IF ( ibar .ne. 0 .and. ier .eq. 0 ) THEN
C
C*         GPMAP
C
           nexp = 10
           CALL ST_CLSL (imgfil, '|', ' ', nexp, prmlist, nprm, ier)
C
C*         Process the station id into icao
C
           CALL ST_UCLC ( prmlist(1), icao,  ier )
           CALL ST_LSTR ( icao, lenq, ier )
           IF ( lenq .eq. 3 ) icao = 'k' // icao
           CALL ST_NULL ( icao, icao,    lenq, ier )
C
C*         Process the product name and resolution to
C*         get product code number
C
           CALL ST_LCUC ( prmlist(2), prodname,  ier )
           CALL ST_NULL ( prmlist(3), resoltn, lenq, ier )
           CALL ST_NULL ( prodname, prodname, lenq, ier )
           CALL TB_NIDSDB ( prodname, resoltn, iprod, ierr )
	   IF  ( ierr .ne. 0 )  THEN
	     CALL ER_WMSG ( 'TB', ierr, ' ', ier )
	     iret = -4
	     RETURN
	   END IF
           CALL ST_INCH ( iprod, prodnumb, ier )
           CALL ST_NULL ( prodnumb, prodnumb, lenq, ier )
           CALL ST_NULL ( prmlist(4), elangle, lenq, ier )
           CALL ST_NULL ( prmlist(5), radtime, lenq, ier )
        ELSE
C
C*         NMAP
C
           nexp = 10
           CALL ST_CLSL ( imgfil, '/', ' ', nexp, prmlist, nprm, ier)
C
C*         Process the station id into icao
C
           CALL ST_UCLC ( prmlist(1), icao,  ier )
           CALL ST_LSTR ( icao, lenq, ier )
           IF ( lenq .eq. 3 ) icao = 'k' // icao
           CALL ST_NULL ( icao, icao,    lenq, ier )
C
C*         Process the product name and resolution to
C*         get product code number
C
           CALL ST_LCUC ( prmlist(2), prmlist(2),  ier )
           nexp = 2
           CALL ST_CLSL ( prmlist(2), '_', ' ', nexp, ttemp, nt, ier )
 
           CALL ST_NULL ( ttemp(1), prodname, lenq, ier )
           CALL ST_NULL ( ttemp(2), resoltn, lenq, ier )
           CALL TB_NIDSDB ( prodname, resoltn, iprod, ierr )
           IF  ( ierr .ne. 0 )  THEN
             CALL ER_WMSG ( 'TB', ierr, ' ', ier )
             iret = -4
             RETURN
           END IF
           CALL ST_INCH ( iprod, prodnumb, ier )
           CALL ST_NULL ( prodnumb, prodnumb, lenq, ier )
           CALL ST_NULL ( prmlist(3), elangle, lenq, ier )
           CALL ST_LSTR ( prmlist(4), lsttim, ier)
           nexp = 2
           CALL ST_CLSL ( prmlist(4)(:lsttim), '_', ' ',
     +                    nexp, ttemp, nt, ier )
           CALL ST_LSTR ( ttemp(1), ildate, ier )
           CALL ST_LSTR ( ttemp(2), iltime, ier )
           radtime = ttemp(1)(3:ildate) // '/' // ttemp(2)(:iltime)
           CALL ST_NULL ( radtime, radtime, lenq, ier )
        END IF
C
C*      Get the meta information about the image
C
        CALL DB_GRADHDR  ( dbhst, qtype, src, icao, prodnumb, resoltn,
     +                     elangle, radtime, radhdr, lradhdr, ier )
        nexp = 14
        CALL ST_CLSL (radhdr(:lradhdr), ';', ' ', nexp,
     +                hdrlist, nprm, ier)
        IF  ( ier .ne. 0 )  THEN
            iret = -1
            RETURN
        END IF
        
        CALL ST_CRNM ( hdrlist(1), aradlt, ier )
        CALL ST_CRNM ( hdrlist(2), aradln, ier )
C
C*      Set the site coordinates
C
        iradlt = aradlt*1000
        iradln = aradln*1000

C
C*      Set the product number
C
        CALL ST_NUMB ( hdrlist(3), iprod, ier )

C
C*      Set the levels
C
        nexp = 16
        CALL ST_CLSL (hdrlist(8), ':', ' ', nexp,
     +                levlist, nlevs, ier)
	DO i = 1, nlevs
            CALL ST_NUMB ( levlist(i), idlvls(i), ier )
	END DO
C
	IF ( hdrlist (6) .eq. "Radial" ) THEN
C
C*	    Radial product-specific variables
C
            CALL ST_NUMB ( hdrlist(5), nrbins, ier )
C
	    IF ( (nrbins .lt. 1) .or. (nrbins .gt. MXBINS) ) THEN
		iret = -4
		RETURN
	    END IF
C
	    imnpix = nrbins * 2
	    imnlin = imnpix
	    imrdfl = 1
C
        ELSE IF ( hdrlist (6) .eq. "Raster" ) THEN
C
C*          Raster product-specific variables
C
            CALL ST_NUMB ( hdrlist(5), nrows, ier )
            imnlin = nrows
            imnpix = imnlin
            IF ( (nrows .lt. 1) .or. (nrows .gt. MXROWS) ) THEN
                iret = -4
                RETURN
            END IF
C
            imnlin = nrows
	ELSE
	    iret = -4
	    RETURN
	END IF
C
C*	Set product info
C
	imsorc = 7
	imdpth = 1
        imldat = imnlin * ( imnpix * imdpth )
	imtype = iprod
        CALL ST_NUMB (  hdrlist(9), isrc , ier ) 
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
        CALL ST_CRNM ( hdrlist(4), rmbelv1, ier )
	IF ( imtype .ge. 16 .and. imtype .le. 30 ) THEN
	    rmbelv = rmbelv1
	ELSE
	    rmbelv = RMISSD
	END IF
C
C*      Get the date and time.
C
        CALL ST_LSTR(hdrlist(7), lsttim, ier)
        nexp = 2
        CALL ST_CLSL (hdrlist(7)(:lsttim), '/', ' ',nexp,ttemp,nt,ier)
        CALL ST_NUMB(ttemp(1), imdate, ier)
        CALL ST_NUMB(ttemp(2), imtime, ier)
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
C*		Check for scaled data. If bit 13 is set, scale by .05,
C*		for bit 12, scale by 0.1
C
		IF ( BTEST ( idlvls (idl), 13 ) ) THEN
		    val = ival * .05
		    CALL ST_RLCH ( val, 1, cmblev (idl), ier )
		ELSE IF ( BTEST ( idlvls (idl), 12 )) THEN
		    val = ival * .1
		    CALL ST_RLCH ( val, 1, cmblev (idl), ier )
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
C
C*	For plan images, set the mapping
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
     +		    MOD (irem, 3600) / 60 * 100 + MOD (irem, 60)
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
