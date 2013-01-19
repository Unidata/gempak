	SUBROUTINE AW_PPLT ( bdata, irept, lenbyt, iff, iret )
C************************************************************************
C* AW_PPLT                                                              *
C*                                                                      *
C* This subroutine defines the plot parameters block.  It identifies    *
C* the settings of display paramerters.  The settings are zoom enabled/ *
C* disabled, zoom threshold, zoom factor, plot color, background color, *
C* line character, line width, line mnonics, logical fill flag, and     *
C* fill pattern number.  This subroutine represents mode 1, submode 4.  *
C* 								        *
C* AW_PPLT ( BDATA, IREPT, LENBYT, IFF, IRET )				*	
C*								        *
C* Input parameters: 						        *
C*      BDATA(*) BYTE		Byte data array                         *
C*      IREPT   INTEGER         Beginning byte position for the block   *
C*      LENBYT  INTEGER         Length of the block in bytes            *
C*      IFF     INTEGER         Length and checksum indicator           *
C*								        *
C* Output parameters:						        *
C*	IRET	INTEGER		Return code                             *
C**								        *
C* Log:									*
C* A. Hardy/GSC          7/98                                           *
C* T. Piper/GSC		11/98	Updated prolog				*
C* A. Hardy/GSC         11/98   Added do loop; fix formats              *
C* A. Hardy/GSC          3/99   Updated prolog				*
C* A. Hardy/GSC          2/00   Increase the file byte size 20k -> 80k  *
C* A. Hardy/GSC         10/00   Put file number and dbug in awcmn.cmn	*
C* T. Piper/SAIC	 2/03	Changed b to bdata and added INT	*
C************************************************************************
 	INCLUDE         'awcmn.cmn'
C*
	BYTE		bdata(*)
        CHARACTER*40    lnmnem
        INTEGER         icnt, zomdis, pltclr, bkgclr, lnchar, lnwdth
	INTEGER         logfil, filpat, itotln, zthrsh, zmfact
        INTEGER         ibte(80000)
C------------------------------------------------------------------------
        IF ( dbug .eq. 'y' ) THEN
            write(flun,*)'In PPLT'
	END IF 
 	iret = 0
	icnt = irept + 4
	itotln = irept + lenbyt
	IF ( iff .eq. 0 ) itotln = itotln - 1
C
C*      Reassign byte array to an integer array.
C
        DO i = 1, 80000
            ibte(i) = bdata(i)
        END DO
C
C*      Get the zoom disable, zoom threshold, and zoom factor. 
C*      Zoom diabled if Z=1.
C
        zomdis = ibits ( ibte(icnt), 7, 1 )
	zthrsh = ibits ( ibte(icnt), 0, 7 )
	zmfact = ibits ( ibte(icnt + 1), 0, 8)
	icnt = icnt + 2
        IF ( dbug .eq. 'y' ) THEN
	    write(flun,49)'ZOOM DISABLE','ZOOM THRESHOLD','FACTOR'
 49         format(a14,5x,a14,5x,a10)
	    write(flun,50)zomdis,  zthrsh, zmfact 
 50         format(i14,5x,i14,5x,i10)
	END IF 
	IF ( icnt .lt. itotln ) THEN
C
C*          Get the plot color and background color.
C
            pltclr = ibits ( ibte(icnt), 0, 8)
	    bkgclr = ibits ( ibte(icnt + 1), 0, 8)
            IF ( dbug .eq. 'y' ) THEN
	        write(flun,70)'PLOT COLOR','BACKGROUND COLOR'
	        write(flun,71)pltclr, bkgclr
 70             format(a10,5x,a16)
 71	        format(i2,13x,i2)
	    END IF 
C
C*          Get the line character, width and mnemonic.
C
	    lnchar = ibits ( ibte(icnt + 2), 0, 8)
	    lnwdth = ibits ( ibte(icnt + 3), 0, 8)
	    icnt = icnt + 3
	    IF ( icnt .lt. itotln ) THEN
	        DO i = 1, 4
		    lnmnem( i:i ) = CHAR(INT(bdata(icnt+i)))
	        END DO
		icnt = icnt + 5
                IF ( dbug .eq. 'y' ) THEN
	            write(flun,49)'LINE CHARACTER','WIDTH','MNEMONIC'
	            write(flun,72)lnchar, lnwdth, lnmnem
  72                format (i5,21x,i5,9x,a4)
		END IF 
C
C*              Get the logical fill and fill pattern.
C
                IF ( icnt .lt. itotln ) THEN
                    logfil = ibits ( ibte(icnt), 0, 8)
	            filpat = ibits ( ibte(icnt + 1), 0, 8)
                    IF ( dbug .eq. 'y' ) THEN
                        write(flun,*)'LOGICAL FILL AND FILL PATTERN'
                        write(flun,*)logfil, filpat
                    END IF
	            icnt = icnt + 1
		END IF 
             END IF
        END IF
C
C*      Check for checksum. 
C
        IF ( dbug .eq. 'y' ) THEN
           IF ( iff .eq. 0) write(flun,*)'Checksum found.'
        END IF 
C*
	RETURN
	END
