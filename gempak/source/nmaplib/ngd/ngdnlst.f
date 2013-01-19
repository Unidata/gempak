	SUBROUTINE NGD_NLST ( alias, namstr, lenstr, iret )
C************************************************************************
C* NGD_NLST								*
C*									*
C* This routine returns a list of names given GRID information.  The 	*
C* alias is used to locate the grid data files on disk.  The times are	*
C* returned in a single string separated by a semi-colon (;).		*
C*									*
C* NGD_NLST ( ALIAS, NAMSTR, LENSTR, IRET )				*
C*									*
C* Input parameters:							*
C*	ALIAS		CHAR*		Alias for GRID data		*
C*									*
C* Output parameters:							*
C*	NAMSTR		CHAR*		String containing all names	*
C*	LENSTR		INTEGER		Length of string 		*
C*	IRET		INTEGER		Return code			*
C*					  -3 = no files found		*
C*					  -6 = invalid template		*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 3/01	Created					*
C* A. Hardy/SAIC         2/02   Changed call FL_SCND			*
C* T. Lee/SAIC		 9/04	Replaced FL_TMPL with CTB_DTGET		*
C* A. Hardy/NCEP	11/04	Added calls to ST_RNUL			*
C* m.gamazaychikov/SAIC 12/04   Added ion flag to CTB_DTGET CS          *
C* m.gamazaychikov/SAIC 04/06   Added idtmch flag to CTB_DTGET CS       *
C* F. J. Yen/NCEP        4/08   Added bin mins & mstrct to CTB_DTGET CSC*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	alias, namstr
C*
	CHARACTER	path*(MXFLSZ), tmplt*(MXFLSZ),
     +			names(MXNMFL)*(MXFLSZ),
     +			namarr(MXNMFL)*(MXFLSZ),
     +			fnull*(MXFLSZ)
C------------------------------------------------------------------------
	iret = 0
        nexp = MXNMFL
C
C*	Get the info for the given alias.
C
	CALL ST_NULL ( alias, fnull, na, ier )
	path  = ' '
	tmplt = ' '
	CALL CTB_DTGET ( fnull, path, tmplt, ic, is, if, ir, ii, ion,
     +          ihb, mnb, iha, mna, mstrct, idtmch, ier )
	CALL ST_RNUL ( path, path, lens, ier )
	CALL ST_RNUL ( tmplt, tmplt, len, ier )
C
C*	Find the "*" in the template. This is where the name starts.
C
	CALL ST_NOCC ( tmplt, '*', 1, ipos, ier )
	IF  ( ( ier .ne. 0 ) .or. ( ipos .eq. 0 ) )  THEN
	    iret = -6
	    RETURN
	END IF
C
C*	Get all of the file names. Return if there are none.
C
	CALL FL_SCND ( path, tmplt, 1, nexp, names, nn, ier )
	IF  ( nn .eq. 0 )  THEN
	    iret = -3
	    RETURN
	END IF
C
C*	Keep only the "name" part of the file name.
C
	DO  i = 1, nn
	    namarr(i) = names(i)(ipos:)
	END DO
C
C*	Sort and get only the unique names.
C
	CALL ST_SORT ( 2, nn, namarr, nout, namarr, ier )
C
C*	Construct a single string from the array of names.
C
	CALL ST_LSTC ( namarr, nout, ';', namstr, ier )
	CALL ST_LSTR ( namstr, lenstr, ier )
C*
	RETURN
	END
