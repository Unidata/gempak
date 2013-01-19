	SUBROUTINE IM_LUTF ( lutfil, iret )
C************************************************************************
C* IM_LUTF								*
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
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -8 = error opening LUT file	*
C**									*
C* Log:									*
C* C. Lin/EAI		 6/95						*
C* S. Jacobs/NMC	 8/95	Added check for imftyp not set		*
C* S. Jacobs/NMC	 8/95	Added GRAY, RADAR, DEFAULT, NONE	*
C* J. Cowie/COMET	11/95	Remove call to GSLUTF, read lut file,	*
C*				call GSBRGB to set color components	*
C* G. Krueger/EAI	11/95	Changed from 0-1 to 0-255 for RGB comps	*
C* D. Keiser/GSC	12/95	Changed FL_TOPN to FL_TBOP		*
C* C. Lin/EAI		 1/96	Check ncolr to avoid divide by 0	*
C* S. Jacobs/NCEP	 7/96	Added IFINVD				*
C* S. Jacobs/NCEP	 1/97	Removed IFINVD and added check for	*
C*				icbank; Changed blank lutfil from NONE	*
C*				to GRAY					*
C* S. Jacobs/NCEP	 2/97	Moved call to GQCLRS to beginning	*
C* J. Cowie/COMET	 1/97	Changed IMGDEF common variable names	*
C* S. Jacobs/NCEP	 4/97	Changed blank lutfil from GRAY to DEF;	*
C*				Added check for PS and background color	*
C* S. Jacobs/NCEP	 4/97	Removed call to IM_RTBL			*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C* R. Tian/SAIC		04/02	Make imbank includes FAX product(3)	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'IMGDEF.CMN'
C*
	CHARACTER*(*)	lutfil
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
C*	Check if IM_SIMG was called for seting up the navigation.
C
	IF  ( ( imbank .lt. 1 ) .or. ( imbank .gt. 3 ) )  RETURN
C
C*	Get the number of colors for this color bank
C
	CALL GQCLRS ( imbank, ncolr, ier )
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
	    ltfl = cmlutf
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
	CALL GSBRGB ( imbank, ncolr, icolrs, ir, ig, ib, ier )
	IF  ( ier .ne. 0 )  THEN
	    CALL ER_WMSG ( 'GEMPLT', ier, ' ', iier )
	    RETURN
	END IF
C*
	RETURN
	END
