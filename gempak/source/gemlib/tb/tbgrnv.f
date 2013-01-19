	SUBROUTINE TB_GRNV  ( lun,  namgd, numgd, prjgd, anggd, gargd,
     +			      nxgd, nygd,  deln,  extnd, iret )
C************************************************************************
C* TB_GRNV								*
C*									*
C* This subroutine returns the contents of a line in a GEMPAK grid 	*
C* navigation table.  Table entries are free format, but no entry	*
C* may be blank.							*
C*									*
C* TB_GRNV  ( LUN,  NAMGD, NUMGD, PRJGD, ANGGD, GARGD, NXGD, NYGD,	*
C*            DELN, EXTND, IRET )					*
C*									*
C* Input parameters:							*
C*	LUN		INTEGER		Logical unit number 		*
C*									*
C* Output parameters:							*
C*	NAMGD		CHAR*4		Grid type name			*
C*	NUMGD		INTEGER		Grid type number		*
C*	PRJGD		CHAR*		Projection name			*
C*	ANGGD (3)	REAL		Grid projection angles		*
C*	GARGD (4)	REAL		Grid lat/lon corners		*
C*	NXGD		INTEGER		Number of grid pts in x dir	*
C*	NYGD		INTEGER		Number of grid pts in y dir	*
C*	DELN		REAL		DELTA N for Barnes analysis	*
C*	EXTND		REAL		Grid size increase, first pass	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = end of file reached	*
C*					 -2 = read error		*
C*					 -9 = decode error		*
C**									*
C* Log:									*
C* G. Huffman/GSC	 4/89						*
C* M. desJardins/GSFC	 5/89	Replace ST_INTG with ST_NUMB		*
C************************************************************************
	CHARACTER*(*)	namgd, prjgd
	REAL		anggd (3), gargd (4)
C
	CHARACTER	tbline*80, tbpars (14)*8
C------------------------------------------------------------------------
	iret = 0
C
C*	Read in next record.
C
	READ   ( lun, 10, IOSTAT = iostat )  tbline
10	FORMAT ( A )
	IF  ( iostat .ne. 0 )  THEN
	    iret = -1
	    RETURN
	END IF
C
C*	Parse the line, then convert each item, if needed.
C
	CALL ST_CLST  ( tbline, ' ', ' ', 14, tbpars, ntbp, ier )
	IF  ( ( ntbp .lt. 14 ) .or. ( ier .ne. 0 ) )  THEN
	    iret = -2
	    RETURN
	END IF
C
	namgd = tbpars (1)
	CALL ST_NUMB  ( tbpars (2), numgd, ier0 )
	prjgd = tbpars (3)
	CALL ST_CRNM  ( tbpars (4),  anggd (1), ier1 )
	CALL ST_CRNM  ( tbpars (5),  anggd (2), ier2 )
	CALL ST_CRNM  ( tbpars (6),  anggd (3), ier3 )
	CALL ST_CRNM  ( tbpars (7),  gargd (1), ier4 )
	CALL ST_CRNM  ( tbpars (8),  gargd (2), ier5 )
	CALL ST_CRNM  ( tbpars (9),  gargd (3), ier6 )
	CALL ST_CRNM  ( tbpars (10), gargd (4), ier7 )
	CALL ST_NUMB  ( tbpars (11), nxgd, ier8 )
	CALL ST_NUMB  ( tbpars (12), nygd, ier9 )
	CALL ST_CRNM  ( tbpars (13), deln,  ier10 )
	CALL ST_CRNM  ( tbpars (14), extnd, ier11 )
	IF  ( ( ier0 + ier1 + ier2 + ier3  + ier4 + ier5 + ier6 +
     +		ier7 + ier8 + ier9 + ier10 + ier11 ) .ne. 0 )
     +	    iret = -9
C*
	RETURN
	END
