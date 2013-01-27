	SUBROUTINE SNPPST  ( istcol, stparm, nstprm, data, first,
     +			     iwloc, stid, istnm, dattim, iret )
C************************************************************************
C* SNPPST								*
C*									*
C* This subroutine write the date, station information and stability	*
C* indicies for SNPLOT.							*
C*									*
C* SNPPST  ( ISTCOL, STPARM, NSTPRM, DATA, FIRST, IWLOC, STID, ISTNM,	*
C*           DATTIM, IRET )						*
C*									*
C* Input parameters:							*
C*	ISTCOL		INTEGER		Color for title			*
C*	STPARM (NSTPRM)	CHAR*		Stability indicies		*
C*	NSTPRM		INTEGER		Number of stability indicies	*
C*	DATA (*)	REAL		Station data			*
C*	FIRST		LOGICAL		Write name flag			*
C*	IWLOC		INTEGER		Location			*
C*	STID		CHAR*		Station identifier		*
C*	ISTNM		INTEGER		Station number			*
C*	DATTIM		CHAR*		Date/time			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/88	GEMPAK 4.1				*
C* K. Brill/NMC		 8/93	stn*4 -> stn*8				*
C* G. Krueger/EAI	 4/94	Allow station indices to plot on more 	*
C*				than one line, if necessary		*
C* G. Krueger/EAI	 8/94	Update to station index plotting format	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
C*	INNMWD = index name width; IDATWD = index data width;
C*	ISTNWD = station info. width
	PARAMETER	( IXNMWD = 5, IXDTWD = 6, ISTNWD = 28 )
C*
	CHARACTER*(*)	stparm (*), dattim, stid
	REAL		data   (*)
C*
	REAL		rdata (MMPARM)
	CHARACTER	cdata (MMPARM)*8
	LOGICAL		first
	CHARACTER	string*120, time*11, stn*8, cstnm*6, cdiag*6
C----------------------------------------------------------------------
	iret = 0
	icolwd = IXNMWD + IXDTWD + 1
	ipos = 2 * ( 5 - iwloc )
C
C*	Check that the color is not 0.
C
	IF  ( istcol .eq. 0 )  THEN
	    RETURN
	  ELSE
	    CALL GSCOLR  ( istcol, ier )
	END IF
C
C*	Compute the stability indicies.
C
	CALL PC_CMST  ( data, rdata, cdata, ier )
C
C*	Calculate number of columns that can fit in view region.
C
	CALL GQSYSZ ( rxszmk, ryszmk, rxsztx, rysztx, rxszwb, ryszwb,
     +		      iret )
C
C*	Get coordinates of view region.
C
	CALL GQBND ( 'P', x1, y1, x2, y2, ier )
	CALL GQBND ( 'V', vx1, vy1, vx2, vy2, ier )
	ncols = ( (vx2 - x1) / rxsztx - ISTNWD ) / icolwd
C
C*	Encode in buffer.
C
	time = dattim
	stn  = stid
	CALL ST_INCH  ( istnm, cstnm, ier )
	IF  ( ier .ne. 0 )  cstnm = ' '
	string = time // ' '  // cstnm // ' ' // stn
C
C*	Display stability indices
C
	IF ( ncols .gt. 0 .and. nstprm .gt. 0 ) THEN
C
C*	    Calculate the number of rows available for indices.
C
	    IF ( MOD ( nstprm, ncols ) .eq. 0 ) THEN
		nrows = nstprm / ncols
	    ELSE
		nrows = nstprm / ncols + 1
	    END IF
C
C*	    Add stability indices.
C
	    DO lstprm = 1, nstprm, ncols
		ipt = ISTNWD
		mstprm = MIN (lstprm + ncols - 1, nstprm)
		irow = lstprm / ncols
		DO  i = lstprm, mstprm
		    idata = NINT  ( rdata (i) )
		    CALL ST_INCH  ( idata, cdiag, ier )
		    CALL ST_LSTR  ( cdiag, lend, ier )
		    ipp = ipt + ( ICOLWD - lend - 1)
		    CALL ST_LSTR ( stparm (i), lenn, ier )
		    string ( ipt : ) = stparm (i)
		    string ( ipt+lenn : ipt+lenn ) = ':'
		    string ( ipp : ) = cdiag
		    ipt = ipt + icolwd
		END DO
C
C*		Write this text string.
C
		CALL GTEXT  ( 'V', x1, y2, string, 0., 0,
     +			      ipos -
     +			      2 * ((nrows - 1) * (iwloc - 1) + irow),
     +			      ier )
		string = ' '
	    END DO
	ELSE
C
C*	    Write this text string.
C
	    CALL GTEXT  ( 'V', x1, y2, string, 0., 0, ipos, ier )
	END IF
C*
	RETURN
	END
