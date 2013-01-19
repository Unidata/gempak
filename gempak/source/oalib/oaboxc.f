	SUBROUTINE OA_BOXC  ( slat, slon, nstn, iextnd, srow, scol,
     +			      iret )
C************************************************************************
C* OA_BOXC								*
C*									*
C* This subroutine translates station latitude and longitude into	*
C* the row and column numbers in the extend grid.  The first two	*
C* values in IEXTND are used to translate grid coordinates to the	*
C* extend grid coordinates.						*
C*									*
C* OA_BOXC  ( SLAT, SLON, NSTN, IEXTND, SROW, SCOL, IRET )		*
C*									*
C* Input parameters:							*
C*	SLAT  (NSTN)	REAL		Station latitudes		*
C*	SLON  (NSTN)	REAL		Station longitudes		*
C*	NSTN		INTEGER		Number of stations		*
C*	IEXTND (4)	INTEGER		Extend grid points		*
C*									*
C* Output parameters:							*
C*	SROW  (NSTN)	REAL		Rows in extend grid		*
C*	SCOL  (NSTN)	REAL		Columns in extend grid		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -2 = invalid grid projection	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 7/86						*
C* M. desJardins/GSFC	11/88	GEMPAK 4.1				*
C* K. Brill/NMC         01/92   Replace GERROR with ER_WMSG             *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		slat (*), slon (*), srow (*), scol (*)
	INTEGER		iextnd (*)
C------------------------------------------------------------------------
	iret = 0
C
C*	Translate from lat/lon to grid row/col.
C
	CALL GTRANS  ( 'M', 'G', nstn, slat, slon, scol, srow, iret )
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG   ( 'GEMPLT', iret, ' ', ier )
	    iret = -2
	    CALL ER_WMSG  ( 'OA', iret, ' ', ier )
	END IF
C
C*	Translate from grid row/col to extend row/col.
C
	IF  ( ( iextnd (1) .ne. 0 ) .or. ( iextnd (2) .ne. 0 ) )  THEN
	    exrow = FLOAT ( iextnd (2) )
	    excol = FLOAT ( iextnd (1) )
	    DO  i = 1, nstn
		srow (i) = srow (i) + exrow
		scol (i) = scol (i) + excol
	    END DO
	END IF
C*
	RETURN
	END
