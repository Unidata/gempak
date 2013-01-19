	SUBROUTINE SNHDTA  ( isnfln, vlevel, nlev, lvert, levtyp,
     +			     u, v,   npts,   utag, vtag,  tag,
     +			     ntag,   iret )
C************************************************************************
C* SNHDTA								*
C*									*
C* This subroutine gets data from the sounding file for SNHODO.		*
C*									*
C* SNHDTA  ( ISNFLN, VLEVEL, NLEV, LVERT, LEVTYP, U, V, NPTS,		*
C*            UTAG, VTAG,   TAG,    NTAG, IRET )				*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER		Sounding file number		*
C*	VLEVEL (NLEV)	REAL		Levels				*
C*	NLEV		INTEGER		Number of levels		*
C*	LVERT		INTEGER		Output vertical coordinate	*
C*	LEVTYP		INTEGER		Level type			*
C*					  1 = list			*
C*					  2 = range without increment	*
C*									*
C* Output parameters:							*
C*	U    (NPTS)	REAL		U for line			*
C*	V    (NPTS)	REAL		V for line			*
C*	NPTS		INTEGER		Number of line levels		*
C*	UTAG (NTAG)	REAL		U for tags			*
C*	VTAG (NTAG)	REAL		V for tags			*
C*	TAG  (NTAG)	REAL		Vertical coordinate tags	*
C*	NTAG		INTEGER		Number of tags			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -9 = no data listed		*
C**									*
C* Log:									*
C* G. Huffman/USRA	 8/89	Adapted from SNLDTA, SNLLVD		*
C* S. Schotz/GSC	 7/90	Plot only mandatory and sig winds	*
C* S. Schotz/GSC	 8/90	Corrected error getting levels		*
C* K. Brill/NMC		 8/93	Change for 8-char ID			*
C* A. Hardy/GSC		 3/99	Added priority parameter to PC_SSTN     *
C* T. Piper/SAIC	 4/02	Fixed UMR; initialized selv, slat, slon	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		vlevel (*), u (*), v (*), utag (*), vtag (*),
     +			tag (*)
	INTEGER		idtype (LLMXLV)
C*
	REAL		data ( LLMXDT ), rdata (3)
	CHARACTER	stntim*20, stid*8, cdata ( MMPARM )*8
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
	selv = 0.
	slat = 0.
	slon = 0.
C
C*	Get the data.
C
	CALL SN_RDAT  ( isnfln, ndlev, data, stntim, ier )
	IF  ( ier .ne. 0 )  THEN
	    iret = -9
	    CALL ER_WMSG  ( 'SN', ier, ' ', ier1 )
	    RETURN
	END IF
	CALL SN_RTYP ( isnfln, ndlev, idtype, iret )
C
	ispri = 0
	CALL PC_SSTN  ( stid,  istnm, slat, slon, selv, ispri, stntim, 
     +			ndlev, ier )
C
C*	Loop through the levels and retrieve the winds.  They are stored
C*	twice, once for the line (U, V), and once for the vertical
C*	coordinate "tags".
C
C*	A list of levels to tag; set the bounds on the line ('ALL' is
C*	0 to -1) and do a separate retrieval for the tags.
C
	IF  ( levtyp .eq. 1 )  THEN			
	    rstrt =  0.
	    rstop = -1.
	    ntag  = 0
	    DO  lev = 1, nlev
		CALL PC_CMVR  ( vlevel (lev), lvert, data, rdata,
     +				cdata, ier )
		IF  ( ( .not. ERMISS ( rdata (1) ) ) .and.
     +		      ( .not. ERMISS ( rdata (2) ) ) .and.
     +		      ( .not. ERMISS ( rdata (3) ) ) )  THEN
		    ntag       = ntag + 1
		    tag  (ntag) = rdata (1)
		    utag (ntag) = rdata (2)
		    vtag (ntag) = rdata (3)
		END IF
	    END DO
C
C*	  A range without increment of levels to tag; make the bounds
C*	  on the line search the same as the bounds on the tag search.
C
	  ELSE
	    rstrt = vlevel (1)
	    rstop = vlevel (2)
	END IF
C
C*	Find bottom and top levels for line (and tag range).
C
	CALL PC_FLVL  ( rstrt, lvert, data, rrr, lvnum1, lvnum2, itype, 
     +								   ier )
	IF  ( itype .eq. 3 )  THEN
	    ilev = 1
	  ELSE IF  ( itype .eq. 4 )  THEN
	    ilev = nlev + 1
	  ELSE IF  ( itype .eq. 2 )  THEN
	    ilev = lvnum2
	  ELSE
	    ilev = lvnum1
	END IF
C
	CALL PC_FLVL  ( rstop, lvert, data, end, lvnum1, lvnum2, itype, 
     +								   ier )
	IF  ( itype .eq. 3 )  THEN
	    jlev = 0
	  ELSE IF  ( itype .eq. 4 )  THEN
	    jlev = nlev
	  ELSE 
	    jlev = lvnum1
	END IF
C
C*	Compute data at the intervening levels just for the line.
C
	IF  ( levtyp .eq. 1 )  THEN
	    npts = 0
	    DO  lev = ilev, jlev
		lll = lev - ilev + 1
		CALL PC_CMLV  ( lev, data, rdata, cdata, ier )
		IF  ( ( .not. ERMISS ( rdata (1) ) ) .and.
     +		      ( .not. ERMISS ( rdata (2) ) ) .and.
     +		      ( .not. ERMISS ( rdata (3) ) ) .and. 
     +                ( idtype (lev) .ne. 2 ) )  THEN
		    npts     = npts + 1
		    u (npts) = rdata (2)
		    v (npts) = rdata (3)
		END IF
	    END DO
C
C*	  Compute data at the intervening levels for the line and
C*	  the tags.
C
	  ELSE
	    npts = 0
	    ntag = 0
	    DO  lev = ilev, jlev
		lll = lev - ilev + 1
		CALL PC_CMLV  ( lev, data, rdata, cdata, ier )
		IF  ( ( .not. ERMISS ( rdata (1) ) ) .and.
     +		      ( .not. ERMISS ( rdata (2) ) ) .and.
     +		      ( .not. ERMISS ( rdata (3) ) ) .and. 
     +                ( idtype (lev) .ne. 2 ) )  THEN
		    npts        = npts + 1
		    u    (npts) = rdata (2)
		    v    (npts) = rdata (3)
		    ntag        = ntag + 1
		    tag  (ntag) = rdata (1)
		    utag (ntag) = rdata (2)
		    vtag (ntag) = rdata (3)
		END IF
	    END DO
	END IF
C
C*	Check that there are points to plot.
C
	IF  ( ( npts + ntag ) .eq. 0 )  THEN
	    iret = -9
	    CALL ER_WMSG  ( 'SNHODO', iret, ' ', ier )
	END IF
C*
	RETURN
	END
