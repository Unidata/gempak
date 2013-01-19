	SUBROUTINE ND_GTBL  ( cpyfil, name, proj, nxgd, nygd, garea,
     +			     rnvblk, anlblk, iret )
C************************************************************************
C* ND_GTBL								*
C*									*
C* This subroutine finds grid INNAME (a numerical or character          *
C* identifier prefaced by '#') in a grid navigation table, then makes 	*
C* the navigation and analysis blocks.  The grid navigation is set up	*
C* in GEMPLT in order to check its validity.				*
C*									*
C* ND_GTBL  ( CPYFIL, NAME, PROJ, NXGD, NYGD, GAREA, RNVBLK, ANLBLK,	*
C*           IRET )							*
C*									*
C* Input parameters:							*
C*	CPYFIL		CHAR*		Input for CPYFIL		*
C*									*
C* Output parameters:							*
C*	NAME		CHAR*           Name of selected grid		*
C*	PROJ		CHAR*		Grid projection			*
C*	NXGD		INTEGER		Number of points in x dir	*
C*	NYGD		INTEGER		Number of points in y dir	*
C*	GAREA   (4)	REAL		Grid corners			*
C*	RNVBLK (LLNNAV)	REAL		Grid navigation block		*
C*	ANLBLK (LLNANL)	REAL		Grid analysis block		*
C*	IRET		INTEGER		Return code			*
C*					 +1 = EXIT entered		*
C*					  0 = normal return		*
C*					 -4 = invalid navigation	*
C*					 -9 = grid not found in table	*
C**									*
C* Log:									*
C* S. Jacobs/EAI	 7/93		Copied from GDCTBL		*
C* D. Keiser/GSC	12/95		Changed FL_TOPN to FL_TBOP	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	cpyfil, proj, name
	REAL		garea (*), rnvblk (*), anlblk (*)
C*
	CHARACTER	gntrec*80, namgd*4, c2name*8
	REAL		anggd (3), dbnds (4)
	INTEGER		iebnds (4)
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
	    iret = -9
	    RETURN
	END IF
C
C*	List the table contents for the user, if requested.
C
	IF  ( name .eq. 'LIST' )  THEN
	    iostat = 0
	    DO WHILE  ( iostat .eq. 0 )
		READ  ( lungrd, 1000, iostat=iostat )  gntrec
1000		FORMAT ( A )
		WRITE  (6,*)  gntrec(1:79)
	    END DO
C
C*	    Rewind the table file.
C
	    CALL FL_TREW ( lungrd, ier )
C
C*	    Prompt user for grid choice.
C
	    CALL TM_CHAR  ( 'Enter grid id or number', .false.,
     +			     .false., 1, name, n, iret )
	    IF  ( iret .ne. 0 )  THEN
		iret = +1
		RETURN
	    END IF
	    CALL ST_LCUC  ( name, name, ier )
	    CALL ST_NUMB  ( name, ingrdn, iernum )
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
	    iret = -9
	    RETURN
	END IF
C
C*	Fill navigation block.
C
	CALL GR_MNAV  ( proj, nxgd, nygd, garea (1), garea (2),
     +			garea (3), garea (4), anggd (1), anggd (2),
     +			anggd (3), .true., rnvblk, ier )
C
C*	Set up navigation in GEMPLT to check validity.
C
	CALL GR_SNAV  ( 13, rnvblk, ier )
	IF  ( ier .ne. 0 )  THEN
	    iret = -4
	    RETURN
	END IF
C
C*	Make an analysis block.
C
	DO  i = 1, 4
	    iebnds (i) = extnd
	END DO
	DO  i = 1, 4
	    dbnds (i) = RMISSD
	END DO
	CALL GR_MBN2  ( deln, iebnds, dbnds, rnvblk, anlblk, ier )
C*
	RETURN
	END
