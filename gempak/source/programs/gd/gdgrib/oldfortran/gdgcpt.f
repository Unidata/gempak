	SUBROUTINE GDGCPT  ( cpyfil, proj, nxgd, nygd, rnvblk, igpds,
     +			     iret )
C************************************************************************
C* GDGCPT								*
C*									*
C* This subroutine finds grid INNAME (a numerical or character          *
C* identifier prefaced by '#') in a grid navigation table, then makes 	*
C* the navigation block.						*
C*									*
C* GDGCPT  ( CPYFIL, PROJ, NXGD, NYGD, RNVBLK, IGPDS, IRET )		*
C*									*
C* Input parameters:							*
C*	CPYFIL		CHAR*		Input for CPYFIL		*
C*									*
C* Output parameters:							*
C*	PROJ		CHAR*		Grid projection			*
C*	NXGD		INTEGER		Number of points in x dir	*
C*	NYGD		INTEGER		Number of points in y dir	*
C*	RNVBLK (LLNNAV)	REAL		Grid navigation block		*
C*	IGPDS		INTEGER		Grid number			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-19 = CPYFIL entry is invalid	*
C**									*
C* Log:									*
C* K. Brill/HPC		 2/00	Adapted from GDCTBL			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	cpyfil, proj
	REAL		rnvblk (*)
C*
	CHARACTER	namgd*4, c2name*8, name*8
	REAL		garea (4), anggd (3)
	LOGICAL		found
C------------------------------------------------------------------------
	iret = 0
	name = ' '
C
C*	Get the grid number (INGRDN) out of NAME; a conversion error
C*	sets IERNUM .ne. 0 and it is assumed that NAME is a type.
C
	CALL ST_LCUC  ( cpyfil, c2name, ier )
	name = c2name ( 2: )
	CALL ST_NUMB  ( name, ingrdn, iernum )
C
C*	Open the table of valid grid types.
C
	CALL FL_TBOP  ( 'grdnav.tbl', 'grid', lungrd, ier )
	IF  ( ier .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'FL', ier, 'grdnav.tbl', ier1 )
	    iret = -19
	    RETURN
	END IF
C
C*	Read through the list of valid grid types/numbers to get 
C*	navigation/analysis information.
C
	found = .false.
	DO  WHILE  ( ( .not. found ) .and. ( ier .eq. 0 ) )
	    CALL TB_GRNV  ( lungrd, namgd, numgd, proj, anggd, garea,
     +			    nxgd, nygd, deln, extnd, ier )
	    IF  (( name .eq. namgd ) .or. ( ingrdn .eq. numgd ) ) THEN
		igpds = numgd
     		found = .true.
	    END IF
	END DO
C*
	CALL FL_CLOS  ( lungrd, ier )
C
C*	Bail out if NAME wasn't found in the table.
C
	IF  ( .not. found )  THEN
	    CALL ER_WMSG  ( 'TB', ier, ' ', ier1 )
	    iret = -19
	    RETURN
	END IF
C
C*	Fill navigation block.
C
	CALL GR_MNAV  ( proj, nxgd, nygd, garea (1), garea (2),
     +			garea (3), garea (4), anggd (1), anggd (2),
     +			anggd (3), .true., rnvblk, ier )
C*
	RETURN
	END
