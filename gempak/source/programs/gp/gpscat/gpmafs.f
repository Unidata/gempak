	SUBROUTINE GPMAFS ( afosfl, icolor, itype, iwidth, iret )
C************************************************************************
C* GPMAFS								*
C*									*
C* This subroutine plots the contents of an AFOS graphics file to the	*
C* current device.							*
C*									*
C* GPMAFS ( AFOSFL, ICOLOR, ITYPE, IWIDTH, IRET )			*
C*									*
C* Input parameters:							*
C*	AFOSFL		CHAR*		AFOS file name			*
C*	ICOLOR		INTEGER		Color for the plot		*
C*	ITYPE		INTEGER		Line type for the plot		*
C*	IWIDTH		INTEGER		Line width for the plot		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	10/97	Created					*
C* S. Schotz/NCEP	10/97	Added error processing for invalid file *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	afosfl
C*
	CHARACTER	afsfln*72, carr(15)*72, pnam*25, parea*4,
     +			name*4, prjaf*3, namgd*4
	REAL		anggd(3), garaf(4)
	LOGICAL		cont, found
C-----------------------------------------------------------------------
C*	Save the current line attributes.
C
	CALL GQLINE ( jtyp, jlhw, jwid, jwhw, ier )
C
C*	If the color is not 0, set the color and continue processing.
C
	IF  ( icolor .gt. 0 )  THEN
	    cont = .true.
	    CALL GSCOLR ( icolor, ier )
	    CALL GSLINE ( itype, 0, iwidth, 0, ier )
	  ELSE
	    cont = .false.
	END IF
C
C*	Open the AFOS map table.
C
	IF  ( cont )  THEN
	    CALL FL_TBOP ( 'nafosmap.tbl', 'nafos', lunmap, ier )
	    IF  ( ier .ne. 0 )  THEN
		CALL ER_WMSG ( 'FL', ier, 'nafosmap.tbl', ier1 )
		cont = .false.
	    END IF
	END IF
C
C*	Find the requested product in the background map table.
C
	IF  ( cont )  THEN
	    CALL ST_CLST ( afosfl, '/', ' ', 15, carr, num, ier )
	    ier = 0
	    cont = .false.
	    name = ' '
	    DO WHILE  ( ( .not. cont ) .and. ( ier .eq. 0 ) )
		READ ( lunmap, 1000, IOSTAT = ier ) pnam, parea
1000		FORMAT ( A, 2X, A )
		ipos = INDEX ( carr(num), pnam(1:3) )
		IF  ( ipos .ne. 0 )  THEN
		    cont = .true.
		    name = parea
		END IF
	    END DO
	    CALL FL_CLOS ( lunmap, ier1 )
	    IF ( ier .ne. 0 ) THEN
		cont = .false.
		CALL ER_WMSG ( 'FL', -1, afosfl, ier1)
	    END IF
	END IF
C
C*	Open the grid navigation table.
C
	IF  ( cont )  THEN
	    CALL FL_TBOP ( 'grdnav.tbl', 'grid', lungrd, ier )
	    IF  ( ier .ne. 0 )  THEN
		CALL ER_WMSG ( 'FL', ier, 'grdnav.tbl', ier1 )
		cont = .false.
	    END IF
	END IF
C
C*	Find the grid navigation for this product.
C
	IF  ( cont )  THEN
	    found = .false.
	    DO WHILE  ( ( .not. found ) .and. ( ier .eq. 0 ) )
		CALL TB_GRNV  ( lungrd, namgd, numgd, prjaf, anggd,
     +				garaf, nxgd, nygd, deln, extnd, ier )
		IF  ( name .eq. namgd )  THEN
		    found = .true.
		END IF
	    END DO
	    CALL FL_CLOS ( lungrd, ier )
C
C*	    Set the projection for the product.
C
	    IF  ( found )  THEN
		CALL GSGPRJ ( prjaf, anggd(1), anggd(2), anggd(3),
     +			      nxgd, nygd, garaf(1), garaf(2),
     +			      garaf(3), garaf(4), ier )
		CALL ST_NULL ( afosfl, afsfln, lenf, ier )
		izm = 1
C
C*		Plot the graphics.
C
		CALL UTF_PLOT ( afsfln, izm, iret )
	    END IF
	END IF
C
C*	Reset the line attributes.
C
	CALL GSLINE ( jtyp, jlhw, jwid, jwhw, ier )
C*
	RETURN
	END
