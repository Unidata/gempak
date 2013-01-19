	SUBROUTINE GPLUTF ( lutfil, icbank, iret )
C************************************************************************
C* GPLUTF								*
C*									*
C* This subroutine defines and applies the selected image LUT file.	*
C*									*
C* The image colors are defined and allocated using this routine.	*
C* Therefore, this routine must be called before calling IM_DROP.	*
C*									*
C* IM_LUTF ( LUTFIL, IRET )						*
C*									*
C* Input parameters:							*
C*	LUTFIL		CHAR*		User defined LUT file		*
C*	ICBANK		INTEGER		Color bank to set		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -8 = error opening LUT file	*
C**									*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	lutfil
	INTEGER		icbank
C*
	CHARACTER	ltfile*80, ltfl*80, cnam*16, cab*4, cxnam*24
	PARAMETER	(MAXCOL = 256)
	INTEGER		icolrs (MAXCOL)
	INTEGER		irgun (MAXCOL), iggun (MAXCOL), ibgun (MAXCOL),
     +			ir (MAXCOL), ig (MAXCOL), ib (MAXCOL)
C*
	CHARACTER	device*72
	LOGICAL		flag
C------------------------------------------------------------------------
	iret = 0
C
C*	Get the number of colors for this color bank
C
	CALL GQCLRS ( icbank, ncolr, ier )
	IF  ( ier .ne. 0 )  THEN
	    CALL ER_WMSG ( 'GEMPLT', ier, ' ', iier )
	    RETURN
	END IF
	IF  ( ncolr .eq. 1 )  RETURN
C
C*	Determine whether the user wants no LUT, a predefined LUT,
C*	the default LUT, or one of their own.
C
	CALL ST_LCUC ( lutfil, ltfile, ier )
C
	IF  ( ltfile .eq. 'NONE' )  THEN
	    RETURN
	  ELSE IF  ( ( ltfile .eq. 'GRAY' ) .or.
     +		     ( ltfile .eq. 'GREY' ) )  THEN
	    ltfl = 'GRAY'
	  ELSE IF  ( ltfile .eq. 'RADAR' )  THEN
	    ltfl = 'osf_ref16.tbl'
	  ELSE IF  ( ( ltfile .eq. 'DEFAULT' ) .or.
     +		     ( ltfile .eq. ' ' ) )  THEN
	    ltfl = 'upc_n1p.tbl'
	  ELSE
	    ltfl = lutfil
	END IF
C
C*	See if a gray-scale has been requested. If so, just make one.
C
	IF  ( ltfl .eq. 'GRAY' ) THEN
C*
	    DO ic = 1, ncolr
		icolrs (ic) = ic - 1
		ir (ic) = NINT ( ( FLOAT (ic-1) / (ncolr-1 ) ) * 255 )
		ig (ic) = ir (ic)
		ib (ic) = ir (ic)
	    END DO
C*
	  ELSE
C
C*	    User asked for LUT file. Open the table.
C
	    CALL FL_TBOP  ( ltfl, 'luts', lun, ier )
	    IF  ( ier .ne. 0 )  THEN
		iret = -8
		CALL ER_WMSG ( 'IM', iret, ltfl, ier )
		RETURN
	    END IF
C
C*	    Read until end of file.
C
	    iend = 0
	    numcol = 0
	    DO WHILE  ( iend .eq. 0 )
		READ  ( lun, 1000, IOSTAT = iend )  cnam, cab, irg,
     +						    igg, ibg, cxnam
1000		FORMAT ( A, A, 3 (1X, I6), A )
C
		IF ( ( cnam (1:1) .ne. '!' ) .and. ( iend .eq. 0 ) )
     +								THEN
		    numcol = numcol + 1
		    irgun (numcol) = irg
		    iggun (numcol) = igg
		    ibgun (numcol) = ibg
		END IF
C
C*		Check for too many colors.
C
		IF  ( numcol .eq. MAXCOL )  iend = -1
	    END DO
C*
	    CALL FL_CLOS  ( lun, ier )
C
C*	    Map table entries into full color bank
C
	    ratio = FLOAT (numcol - 1) / (ncolr - 1)
	    DO  i = 1, ncolr
		index = ((i-1) * ratio + .5) + 1
		icolrs (i) = i - 1
		ir (i) = irgun (index)
		ig (i) = iggun (index)
		ib (i) = ibgun (index)
	    END DO
C*
	END IF
C
C*	If the device is PS, reset black to white until the first
C*	non-black color is encountered. This has the effect of changing
C*	the "background" color of the image from black to white.
C
	CALL GQDEV ( device, i, j, ier )
	IF  ( device .eq. 'PS' )  THEN
	    flag = .true.
	    DO  ic = 1, ncolr
		IF  ( ( ir (ic) .eq. 0 ) .and.
     +		      ( ig (ic) .eq. 0 ) .and.
     +		      ( ib (ic) .eq. 0 ) .and. flag )  THEN
		    ir (ic) = 255
		    ig (ic) = 255
		    ib (ic) = 255
		  ELSE
		    flag = .false.
		END IF
	    END DO
	END IF
C
C*	Set the color components
C
	CALL GSBRGB ( icbank, ncolr, icolrs, ir, ig, ib, ier )
	IF  ( ier .ne. 0 )  THEN
	    CALL ER_WMSG ( 'GEMPLT', ier, ' ', iier )
	    RETURN
	END IF
C*
	RETURN
	END
