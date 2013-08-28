	SUBROUTINE IM_SIMG ( cproj, imgfil, iret )
C************************************************************************
C* IM_SIMG								*
C*									*
C* This subroutine reads an image file header and sets the navigation	*
C* for an image file.							*
C*									*
C* IM_SIMG ( CPROJ, IMGFIL, IRET )					*
C*									*
C* Input parameters:							*
C*	CPROJ		CHAR*		Projection name			*
C*	IMGFIL		CHAR*		Image file name			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 +1 = No entry found for image	*
C*					 -1 = Image file not found	*
C*					 -3 = Invalid image file format *
C*					 -4 = Invalid image product	*
C*					 -5 = Invalid navigation	*
C*					 -7 = Could not open imtyp table*
C**									*
C* Log:									*
C* J. Cowie/COMET	 2/95	Modified from GG_SAT()			*
C* C. Lin/EAI		 6/95	Restructure				*
C* J. Cowie/COMET	10/95	Accomodate small image products		*
C* J. Cowie/COMET	11/95	Add GINI file check			*
C* S. Jacobs/NCEP	 7/96	Changed IM_ARHD -> IM_AR2GM and		*
C*				IM_GNHD -> IM_GI2GM; Modified checks	*
C*				for different image types; Added MV_BTOI*
C* S. Jacobs/NCEP	 7/96	Fixed call to IM_GI2GM			*
C* R. Lindsay/GSC	12/96	Added code for UNISYS NIDS composites	*
C* J. Cowie/COMET	 1/97	Changed IMGDEF common variable names	*
C* S. Jacobs/NCEP	 4/97	Added call to IM_RTBL to read image tbl	*
C* S. Chiswell/Unidata	12/97	Added byte swap check for nids for OSF	*
C* S. Jacobs/NCEP	 2/98	Removed check for valid natl/regl prods	*
C* S. Jacobs/NCEP	 4/98	Fixed array size for ipflip		*
C* S. Jacobs/NCEP	 5/99	Added byte swap check for Unisys format	*
C* S. Jacobs/NCEP	10/99	Added check for NetCDF format images	*
C* R. Curtis/EAI	 8/00   Updated for NetCDF files                *
C* S. Chiswell/Unidata	11/00	Added compressed NIDS format		*
C* S. Chiswell/Unidata	 2/02	Added imcmn.cmn for imcalbar init	*
C* J. Levit/NCEP        10/07   Increased amount of available radar     *
C*                              products from 149 to 999.               *
C* m.gamazaychikov/CWS	01/10	Add calls to IM_DBGINI2GM, IM_DBNIDS2GM,*
C*                              IM_DBMOSAIC2GM, IM_DBMCIDAS2GM providing*
C*				ability to display GINI, NIDS, MOSAIC   *
C                               MCIDAS images stored in AWIPS II	*
C*				database				*
C* M. James/Unidata      2/10   Modified imdoff def. for FOS/WMO hdrs   *
C* X. Guo/CWS           04/10   Added codes to support 94 product       *
C* X. Guo/CWS           04/10   Added codes to support 99,134 and 135   *
C*                              product. Also set imftyp to IFHINIDS    *
C* S. Jacobs/NCEP       4/11    Removed AWIPS II database access        *
C* M. James/Unidata     1/11    Add support for both 4 & 8-bit NIDS     *
C* M. James/Unidata     8/13    Removed IM_DBGINI2GM, IM_DBNIDS2GM,     *
C*                              IM_DBMOSAIC2GM, IM_DBMCIDAS2GM          *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'IMGDEF.CMN'
	INCLUDE		'imcmn.cmn'
C*
	CHARACTER*(*)	cproj, imgfil
C*
	CHARACTER	temfil*132, tmproj*4
	LOGICAL		exist, found, negflg
	REAL		rnvblk ( LLNNAV )
C*
	INTEGER		iarr4 ( 8 ), ignhdr ( 135 )
	INTEGER*2	iarr2 ( 16 ), ipflip ( 16 )
C*
	EQUIVALENCE	( iarr4, iarr2 )
C------------------------------------------------------------------------
	iret   = 0
	imftyp = IFINVD
	found  = .false.
C
C*	Check projection type.
C
	CALL ST_LCUC ( cproj, tmproj, ier )
	IF  ( tmproj .eq. 'SAT' ) THEN
	    imbank = 1
	  ELSE IF ( tmproj .eq. 'RAD' ) THEN
	    imbank = 2
	  ELSE
	    iret = -5
	    RETURN
	END IF
C
C*	Get the actual file name to use.
C
	CALL FL_INQR ( imgfil, exist, temfil, ier )
	IF  ( .not. exist ) THEN
	    iret = -1
	    RETURN
	END IF
C
C*	Open the file and read enough information to detect the format.
C
	CALL FL_DOPN ( temfil, 8, .false., lunif, iret )
	IF  ( iret .ne. 0 ) THEN
	    iret = -1
	    RETURN
	END IF
	CALL FL_READ ( lunif, 1, 8, iarr4, iret )
	CALL FL_CLOS ( lunif, ierr )
C
C*	Verify we were able to read a header OK.
C
	IF ( iret .ne. 0 ) THEN
	    iret = -1
	    RETURN
	END IF
C
C*	Initialize calibration flag for color bar
C
	imcalbar = 0
C
C*	Check for AREA file.	Rules: Word 1 = 0, Word 2 = 4 (might be
C*				byte-swapped).
C
	ier = MV_SWP4 ( 1, iarr4 (2), iswap2 )
	IF  ( (   iarr4 (1) .eq. 0 ) .and.
     +	      ( ( iarr4 (2) .eq. 4 ) .or. ( iswap2 .eq. 4 ) ) ) THEN
C
	    imftyp = IFAREA
	    found  = .true.
C
C*	    Read header and set navigation for AREA files.
C
	    CALL IM_AR2GM ( temfil, rnvblk, iret )
	END IF
C
	IF  ( .not. found ) THEN
C
C*	    Check for GINI images.	Rules: Look at the WMO header.
C*					Char 1 is T (ascii 84), and
C*					there should be a LF in byte
C*					21 (or 25).
C
	    nbytes = 1
	    negflg = .false.
	    istart = 1 - 1
	    CALL MV_BTOI ( iarr4, istart, nbytes, negflg, ihdr1, ier )
	    istart = 21 - 1
	    CALL MV_BTOI ( iarr4, istart, nbytes, negflg, ihdr21, ier )
	    istart = 25 - 1
	    CALL MV_BTOI ( iarr4, istart, nbytes, negflg, ihdr25, ier )
	    IF  ( (   ihdr1  .eq. 84 ) .and.
     +		  ( ( ihdr21 .eq. 10 ) .or. ( ihdr25 .eq. 10 ) ) ) THEN
C
		imftyp = IFGINI
		found  = .true.
C
C*		Read header and set navigation for GINI files.
C
		CALL IM_GI2GM ( temfil, ignhdr, rnvblk, iret )
C
C*		Use the satellite color bank for these images (even radar!)
C
		imbank = 1
C
	    END IF
	END IF
C
C*	S. Chiswell, Updated for new products, up to 299
C
        IF  ( ( .not. found ) .and.
     +	      ( ( iarr2(1)   .eq. iarr2(16) ) .and.
     +		( ( iarr2(1) .ge.        16 ) .and.
     +		  ( iarr2(1) .le.       149 ) ) .and.
     +		( iarr2(10)  .eq.        -1 ) .and.
     +		( iarr2(7)   .lt.     10000 ) ) ) THEN
C
C*	    Check for raw NIDS format.	Rules: halfword 1 = halfword 16,
C*	    in the valid NIDS product range, halfword 10 = -1, and
C*	    halfword 7 less than 10000.
C
	    imftyp = IFNIDS
	    found  = .true.
C
C*	    Read header and set navigation for NIDS files.
C
            IF ( (iarr2(1) .eq. 94) .or. (iarr2(1) .eq. 99) 
     +           .or. (iarr2(1) .eq. 134) .or. (iarr2(1) .eq. 135)
     +           .or. (iarr2(1) .eq. 138)) THEN
                   imftyp = IFHINIDS
                   CALL ST_NULL ( temfil, temfil, lens, iret )
                   CALL IM_NEXBZ ( temfil, itype, ioff, iret )
               ELSE
                   CALL IM_NIHD ( temfil, iret )
            END IF
	END IF
C
	CALL MV_SWP2 ( 8, iarr4 (1), ipflip )
        IF  ( ( .not. found ) .and.
     +	      ( ( ipflip(1)   .eq. ipflip(16) ) .and.
     +		( ( ipflip(1) .ge.         16 ) .and.
     +		  ( ipflip(1) .le.        149 ) ) .and.
     +		( ipflip(10)  .eq.         -1 ) .and.
     +		( ipflip(7)   .lt.      10000 ) ) ) THEN
C
C*	    Check for raw NIDS format with the bytes flipped.
C*	    Rules: halfword 1 = halfword 16, in the valid NIDS product
C*	    range, halfword 10 = -1, and halfword 7 less than 10000.
C
	    imftyp = IFNIDS
	    found  = .true.
C
C*	    Read header and set navigation for NIDS files.
C
            IF ( (ipflip(1) .eq. 94) .or. (ipflip(1) .eq. 99) 
     +         .or. (ipflip(1) .eq. 134) .or. (ipflip(1) .eq. 135)
     +           .or. (iarr2(1) .eq. 138)) THEN
                   imftyp = IFHINIDS
                   CALL ST_NULL ( temfil, temfil, lens, iret )
                   CALL IM_NEXBZ ( temfil, itype, ioff, iret )
               ELSE
	           CALL IM_NIHD ( temfil, iret )
            END IF
	END IF
C
	IF  ( ( .not. found ) .and.
     +	      ( ( iarr2(1)   .eq. iarr2(16) ) .and.
     +		( ( iarr2(1) .ge.         5 ) .and.
     +		  ( iarr2(1) .le.       999 ) ) .and.
     +		( iarr2(10)  .eq.        -1 ) .and.
     +		( iarr2(7)   .ge.     10000 ) ) ) THEN
C
C*	    Check for UNISYS Mosaics (Alaska, Hawaii, National, and Regional).
C*	    Rules:  halfword 1 = halfword 16, and in the valid NIDS
C*		    product range, and halfword 10 = -1 and halfword 7
C*		    greater than or equal 10000 and first 2 bytes are
C*		    a valid product ID (see DATA statement).
C
	    imftyp = IFNIDS
	    found  = .true.
C
C*	    Read header and set navigation for UNISYS NIDS products.
C
	    CALL IM_UNHD ( temfil, iret )
	END IF
C
	CALL MV_SWP2 ( 8, iarr4 (1), ipflip )
	IF  ( ( .not. found ) .and.
     +	      ( ( ipflip(1)   .eq. ipflip(16) ) .and.
     +		( ( ipflip(1) .ge.          5 ) .and.
     +		  ( ipflip(1) .le.        999 ) ) .and.
     +		( ipflip(10)  .eq.         -1 ) .and.
     +		( ipflip(7)   .ge.      10000 ) ) ) THEN
C
C*	    Check for UNISYS Mosaics (Alaska, Hawaii, National, and Regional).
C*	    Rules:  halfword 1 = halfword 16, and in the valid NIDS
C*		    product range, and halfword 10 = -1 and halfword 7
C*		    greater than or equal 10000 and first 2 bytes are
C*		    a valid product ID (see DATA statement).
C
	    imftyp = IFNIDS
	    found  = .true.
C
C*	    Read header and set navigation for UNISYS NIDS products.
C
	    CALL IM_UNHD ( temfil, iret )
	END IF
C
	IF  ( .not. found ) THEN
C
C*	    Check for NOWrad images.	Rules: First three bytes are
C*					0 (0x00), 240 (0xf0), and
C*					9 (0x09) to indicate the
C*					broadcast image header.
	    nbytes = 1
	    negflg = .false.
	    istart = 1 - 1
	    CALL MV_BTOI ( iarr4, istart, nbytes, negflg, ihdr1, ier )
	    istart = 2 - 1
	    CALL MV_BTOI ( iarr4, istart, nbytes, negflg, ihdr2, ier )
	    istart = 3 - 1
	    CALL MV_BTOI ( iarr4, istart, nbytes, negflg, ihdr3, ier )
	    IF  ( ( ihdr1 .eq. 0 ) .and. ( ihdr2 .eq. 240 ) .and.
     +		  ( ihdr3 .eq. 9 ) ) THEN
C
		imftyp = IFNOWR
		found  = .true.
C
C*		Read header and set navigation for WSI NOWrad products.
C
		CALL IM_NOHD ( temfil, iret )
	    END IF
	END IF
C
	IF  ( .not. found )  THEN
C
C*	    Check for NetCDF images.	Rules: First four bytes are
C*					'C', 'D', 'F', 0x01.
C
	    CALL MV_SWP2 ( 1, iarr4(1), ipflip )
	    IF  ( ( (  iarr2(1) .eq. 17220 ) .and.
     +		    (  iarr2(2) .eq. 17921 ) ) .or.
     +		  ( ( ipflip(1) .eq. 17220 ) .and.
     +		    ( ipflip(2) .eq. 17921 ) ) )  THEN
C
		imftyp = IFNCDF
		found  = .true.
C
C*		Read header and set navigation for NetCDF products.
C
		CALL IM_NCDF ( temfil, iret )
		IF  ( iret .ne. 0 )  THEN
		    imftyp = IFINVD
		    found  = .false.
		ELSE
C
C*		    Use the satellite color bank for these images (even qpe radar!)
C
		    imbank = 1
		END IF
	    END IF
	END IF
C
	IF  ( .not. found )  THEN
C
C*	    Check to see if we have a FOS header before the product
C*	    actually starts.
C*
C*	    First 4 bytes are SOH, cr, cr, nl
C*
C
	    IF  ( ( ( iarr2(1) .eq.  269 ) .and.
     +		    ( iarr2(2) .eq. 3338 ) ) .or.
     +		  ( ( iarr2(1) .eq. 3329 ) .and.
     +		    ( iarr2(2) .eq. 2573 ) ) )  THEN
C
		CALL ST_NULL ( temfil, temfil, lens, iret )
		CALL IM_NEXZ ( temfil, 4, itype, ioff, iret )
		IF  ( iret .eq. 0)  THEN
		    found = .true.
		    imftyp = itype
		    IF ( ( imftyp .eq. IFNIDS ) .or.
     +		    ( imftyp .eq. IFNEXZ ) ) THEN
		    	imdoff = imdoff + ioff
		    END IF
		END IF
C
	    END IF
	END IF
C
	IF  ( .not. found )  THEN
C
C*	    Check to see if we have a WMO header before the product
C*	    actually starts.
C*
C*	    First 4 bytes are 'S', 'D', 'U', 'S'
C*
C
	    IF  ( ( ( iarr2(1) .eq. 21316 ) .and.
     +		    ( iarr2(2) .eq. 21843 ) ) .or.
     +		  ( ( iarr2(1) .eq. 17491 ) .and.
     +		    ( iarr2(2) .eq. 21333 ) ) )  THEN
C
		CALL ST_NULL ( temfil, temfil, lens, iret )
		CALL IM_NEXZ ( temfil, 2, itype, ioff, iret )
		IF  ( iret .eq. 0)  THEN
		    found = .true.
		    imftyp = itype
		    IF (imftyp .eq. IFNIDS ) THEN 
                        imdoff = imdoff + ioff
		    END IF
		END IF
C
	    END IF
	END IF
C
	IF  ( .not. found )  THEN
C
C*	    Check for NEXRAD Level II Archive format
C*
C*	    First 8 bytes are A   R   C   H   I   V   E   2
C*	    or
C*	    With Build5 ROC header are A   R   2   V  x   x   x   x
C
	    CALL MV_SWP2 ( 1, iarr4(1), ipflip )
            IF  ( ( (  iarr2(1) .eq. 16722 ) .and.
     +              (  iarr2(2) .eq. 17224 ) .and.
     +		    (  iarr2(3) .eq. 18774 ) .and.
     +		    (  iarr2(4) .eq. 17714 ) ) .or.
     +            ( ( ipflip(1) .eq. 16722 ) .and.
     +              ( ipflip(2) .eq. 17224 ) .and.
     +		    ( ipflip(3) .eq. 18774 ) .and.
     +              ( ipflip(4) .eq. 17714 ) ) .or.
     +		  ( (  iarr2(1) .eq. 16722 ) .and.
     +		    (  iarr2(2) .eq. 12886 ) ) .or.
     +            ( ( ipflip(1) .eq. 16722 ) .and.
     +              ( ipflip(2) .eq. 12886 ) ) )  THEN

		CALL IM_NEX2 ( temfil, iret )
		IF ( iret .eq. 0 ) THEN
		    found = .true.
		    imftyp = IFNEX2
C
C*		    Use the satellite color bank for these images
C
		    imbank = 1
		END IF
	    END IF
	END IF
C
C*	Else, don't know this format.
C
	IF  ( .not. found )  THEN
	    iret = -3
	  ELSE
C
C*	    Read the image table.
C
	    CALL IM_RTBL ( iret )
C
C*	    NIDS non graphic products OK if not in table
C
	    IF  ( ( iret .ne. 0 ) .and.
     +		( ( imradf .eq. 1 ) .and. ( imrdfl .eq. 2 ) ) ) THEN
		iret = 0
	    END IF
	END IF
C
C*	If successfully down to here, set CMFILE.
C
	cmfile = temfil
C
C*      Use sat color bank if imndlv exceeds 20 for radar
C
        IF ( ( ( imbank .eq. 2 ) .and. ( imndlv .gt. 20 ) ) .or.  
     +	 ( imftyp .eq. IFHINIDS ) ) THEN
           imbank = 1
        END IF
C*
	RETURN
	END
