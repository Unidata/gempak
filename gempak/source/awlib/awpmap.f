	SUBROUTINE AW_PMAP ( bdata, irept, iff, iret )
C************************************************************************
C* AW_PMAP                                                              *
C*                                                                      *
C* This subroutine is the map background definition block.  It permits  *
C* the user to map the product pixel coordinates to Earth coordinates.  *
C* This block must preceed the vector, raster, or gridded blocks.	*
C* This subroutine represents mode 1, submode 10.                       *
C*								        *
C* Currently, mode 4, submode 21 is set to call this subroutine.        *
C*								        *
C* AW_PMAP ( BDATA, IREPT, IFF, IRET )                        	*
C*								        *
C* Input parameters: 						        *
C*      BDATA(*) BYTE		Byte data array                         *
C*      IREPT   INTEGER         Beginning byte position for the block   *
C*      IFF     INTEGER         Length and checksum indicator           *
C*								        *
C* Output parameters:						        *
C*	IRET	INTEGER		Return code                             *
C**								        *
C* Log:									*
C* A. Hardy/GSC          7/98                                           *
C* A. Hardy/GSC          9/98   Deleted check for N. Hem. map           *
C* T. Piper/GSC		11/98	Updated prolog				*
C* A. Hardy/GSC         11/98   Added do loop and parentheses for -1    *
C* A. Hardy/GSC          3/99   Updated prolog				*
C* A. Hardy/GSC         11/99   Added 9999 to IF condition for isslt    *
C* A. Hardy/GSC          2/00   Increase the file byte size 20k -> 80k  *
C* A. Hardy/GSC         10/00   Put file number and dbug in awcmn.cmn	*
C* T. Piper/SAIC	 2/03	Changed b to bdata and added INT	*
C************************************************************************
 	INCLUDE         'awcmn.cmn'
C*
	BYTE		bdata(*)
	CHARACTER*40    mapbkg   
        INTEGER         icnt, icsif, icorp 
	INTEGER         ullat, ullon, urlat, urlon, lrlat, lrlon, 
     +                  lllat, lllon, ivtmr, istnd, isslt, ixgrd, iygrd
	REAL            fstnd, fvtmr, fsslt, flllat, flllon, furlat, 
     +                  furlon, fullat, fullon, flrlat, flrlon 
        COMMON          ixgrd, iygrd
        INTEGER         ibte(80000)
C------------------------------------------------------------------------
        IF ( dbug .eq. 'y' ) THEN
            write(flun,*)'In PMAP'
	END IF 
 	iret = 0
	icnt = irept + 4
C
C*      Reassign byte array to an integer array.
C
        DO i = 1, 80000
            ibte(i) = bdata(i)
        END DO
C
C*      Get the coordinate system indicator flag and the count of
C*      reference points. 
C
        icsif = ibits ( ibte(icnt), 0, 8 )
	icorp = 2 * ( ibits ( ibte(icnt+1), 0, 8 ) )
	icnt = icnt + 2
        IF ( dbug .eq. 'y' ) THEN
	    write(flun,40)'COORDINATE FLAG','NUMBER OF REFERENCE POINTS'
 40         format(a16,5x,a26)
	    write(flun,41)icsif, icorp
 41         format(i2,25x,i2)
	END IF 
C
C*      Getting the coordinates for the corner points of the map 
C*      background. The sequence is UL, UR, LR, and LL. The values
C*      are hundreds of degrees north lat. or west lon.
C
	IF ( icorp .ge. 2 ) THEN
	    CALL AW_ADBT (bdata, icnt, ullat, iret )
	    fullat = ullat /100.0
	    icnt = icnt + 2
	    CALL AW_ADBT (bdata, icnt, ullon, iret )
	    fullon = ullon * (-1) / 100.0
	    icnt = icnt + 2
        END IF
	IF ( icorp .ge. 4 ) THEN
	    CALL AW_ADBT (bdata, icnt, urlat, iret )
	    furlat = urlat /100.0
	    icnt = icnt + 2
	    CALL AW_ADBT (bdata, icnt, urlon, iret )
	    furlon = urlon * (-1) / 100.0
	    icnt = icnt + 2
        END IF
	IF ( icorp .ge. 6 ) THEN
            CALL AW_ADBT (bdata, icnt, lrlat, iret )
	    flrlat = lrlat /100.0
	    icnt = icnt + 2
	    CALL AW_ADBT (bdata, icnt, lrlon, iret )
	    flrlon = lrlon * (-1) / 100.0
	    icnt = icnt + 2
        END IF
	IF ( icorp .eq. 8 ) THEN
	    CALL AW_ADBT (bdata, icnt, lllat, iret )
	    flllat = lllat / 100.0
	    icnt = icnt + 2
	    CALL AW_ADBT (bdata, icnt, lllon, iret )
	    flllon = lllon * (-1) / 100.0
	    icnt = icnt + 2
        END IF
        IF ( dbug .eq. 'y' ) THEN
	    write(flun,43)'ULLAT','ULLON','URLAT','URLON'
	    write(flun,44)fullat, fullon, furlat, furlon
	    write(flun,43)'LRLAT','LRLON','LLLAT','LLLON'
	    write(flun,44)flrlat, flrlon, flllat, flllon
 43         format(a5,7x,a5,7x,a5,7x,a5)
 44         format(f7.2,5x,f7.2,5x,f7.2,5x,f7.2)
	END IF 
C
C*      Get the vertical meridian, standard latitude of the projection
C*      and the second standard latitude.
C
	CALL AW_ADBT (bdata, icnt, ivtmr, iret )
	fvtmr = ivtmr * (-1) / 100.0
        icnt = icnt + 2
C
	CALL AW_ADBT (bdata, icnt, istnd, iret )
	IF ( istnd .gt. 0 ) fstnd =  90.
	IF ( istnd .lt. 0 ) fstnd = -90.
        icnt = icnt + 2
C
	CALL AW_ADBT (bdata, icnt, isslt, iret )
	IF ( ( isslt .eq. 9900 ) .or. ( isslt .eq. 9999 ) ) THEN
	    fsslt = 0
          ELSE
            fsslt = isslt / 100.0
        END IF
        icnt = icnt + 1
        IF ( dbug .eq. 'y' ) THEN
           write(flun,45)'VERT. MERIDIAN','STANDARD LAT.',
     +                   '2ND STND. LAT.'
	   write(flun,46)fvtmr, fstnd, fsslt
  45       format(a14,5x,a14,5x,a14)
  46       format(f7.2,15x,f7.2,12x,f7.2)
	END IF 
C
C*      Get the map background name.
C
        DO i = 1, 6
	    mapbkg( i:i ) = CHAR(INT(bdata(icnt+i)))
	END DO
	icnt = icnt + 7
        IF ( dbug .eq. 'y' ) THEN
	    write(flun,*)'MAP BACKGROUND NAME'
	    write(flun,47)mapbkg
 47         format(5x,a6)
	END IF 
C
C*          Set the projection for the product.
C
        CALL GSGPRJ ( 'STR', fstnd, fvtmr, fsslt, ixgrd, iygrd,
     +                flllat, flllon, furlat, furlon, ier )
C
C*      Check for checksum. 
C
        IF ( dbug .eq. 'y' )  THEN
           IF ( iff .eq. 0) write(flun,*)'Checksum found.'
        END IF 
C*
	RETURN
	END
