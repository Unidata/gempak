	SUBROUTINE GG_BND  ( bnd, iret )
C************************************************************************
C* GG_BND								*
C*									*
C* This subroutine parses the BND parameter to process GEMPAK bounds 	*
C* areas. A bound area may be filled, outlined or have a marker or	*
C* symbol plotted at its centroid. All three options may be specified.	*
C*									*
C* BND = type/color/fill size/fill pattern/filter/minpts ! 		*
C*       lincol, lintyp, linwid, !					*
C*       symtyp, symcol, symnum, symsiz, symwid				*
C*									*
C* The BND string may contain the bound type (state_bnds, 		*
C* cnty_bnds, etc.), fill color, pattern size, pattern type, point	*
C* filter, minpts. It may also contain the outline color, line type and	*
C* line width. It may also contain the marker/sumbol type, color, 	*
C* number, size and width. If BND is blank, no operation is performed.	*
C* If a color is zero, then that operation will not be performed.  	*
C* The bound point filter can be YES or NO.  If the filter is turned 	*
C* on, the number of polygon points will be reduced. This option may be *
C* used for either the fill or outline or both. (If needed for only the	*
C* outline, specify a fill color of zero.) Elimination of small islands *
C* can be accomplished using minpts, the minimum number of polygon 	*
C* points allowed to be filled.						*
C*									*
C* GG_BND  ( BND, IRET )						*
C*									*
C* Input parameters:							*
C*	BND		CHAR*		Bound type/color/ ... 		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				  	  0  = normal return		*
C*				  	 -19 = error from GG_FBND	*
C**									*
C* Log:									*
C* D.W.Plummer/NCEP	 4/01						*
C* D.W.Plummer/NCEP	 1/02	Remove call to ST_LCUC for bound type	*
C* D.W.Plummer/NCEP	 9/02	Expand to perform outline and symbols	*
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
C
	PARAMETER	( MXBD = 20 )
C*
	CHARACTER*(*)	bnd
C*
	CHARACTER	bdatt(MXBD)*72, group(6)*72, bndtyp*32
	CHARACTER	opts(3)*72
C*
	INTEGER		bndcol, filpat, icolor(2)
	REAL		bndflt, filsiz
	INTEGER		lincol, lintyp, linwid
	CHARACTER	symtyp*4
	INTEGER		symcol, symnum, symwid
	REAL		symsiz
C-----------------------------------------------------------------------
	iret = 0
C
C*	Parse the input for BND.
C
	CALL ST_CLST  ( bnd, '+', ' ', MXBD, bdatt, numa, ier )
C
C*	Loop over all the bound types.
C
	DO  i = 1, numa
C
C*	    Break the list into three sections using '!' as the sep.
C
	    CALL ST_CLST  ( bdatt(i), '!', ' ', 3, opts, no, ier )
C
C*	    Break the first bang into 6 items.
C
	    CALL ST_CLST  ( opts(1), '/', '0', 6, group, ng, ier )
C
C*	    Save bound type.
C
	    bndtyp = group(1)
C
C*	    Decode color, fill size, and fill pattern.
C
	    CALL IN_COLR ( group (2), 2, icolor, ier )
	    bndcol = icolor(1)
	    CALL ST_CRNM ( group (3), filsiz, ier )
	    CALL ST_NUMB ( group (4), filpat, ier )
	    IF ( filpat .le. 0 )  filpat = 1
C
C*	    Decode filter and minpts.
C
	    CALL ST_CRNM ( group (5), bndflt, ier )
	    CALL ST_NUMB ( group (6), minpts, ier )
C
C*	    Break the second bang into 3 items; decode outline 
C*	    color, line type and line width.
C
	    CALL ST_CLST  ( opts(2), '/', '0', 3, group, ng, ier )
C
            CALL IN_COLR ( group (1), 2, icolor, ier )
	    lincol = icolor(1)
	    CALL ST_NUMB ( group (2), lintyp, ier )
	    CALL ST_NUMB ( group (3), linwid, ier )
C
C*	    Break the third bang into 5 items; decode marker/symbol 
C*	    color, number, size and width.
C
	    CALL ST_CLST  ( opts(3), '/', '0', 5, group, ng, ier )
C
	    symtyp = group(1)
            CALL IN_COLR ( group (2), 2, icolor, ier )
	    symcol = icolor(1)
	    CALL ST_NUMB ( group (3), symnum, ier )
	    CALL ST_CRNM ( group (4), symsiz, ier )
	    CALL ST_NUMB ( group (5), symwid, ier )
C
C*	    Fill the bound area(s).
C
	    CALL GPLBND( bndtyp, bndcol, filsiz, filpat, bndflt, minpts,
     +                   lincol, lintyp, linwid,
     +                   symtyp, symcol, symnum, symsiz, symwid, iret )
C
	    IF ( iret .lt. 0 )  THEN
                CALL ER_WMSG  ( 'GG', -19, bndtyp, ier ) 
	    END IF
C
	END DO
C*
	RETURN
	END
