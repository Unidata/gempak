	SUBROUTINE GDTXLV ( line, crange, scale,  kx, ky, imin, jmin,
     +			    imax, jmax, grid, icolor, iline, linwid, 
     +			    ilabel, iscale, dmin, dmax, clvl, nlvl, 
     +			    clbl, scflag, iret )
C************************************************************************
C* GDTXLV								*
C*									*
C* This subroutine decides which contour levels to create and the	*
C* colors and line types to use.					*
C*									*
C* GDTXLV  ( LINE, CRANGE, SCALE, KX, KY, IMIN, JMIN, IMAX, JMAX, GRID,	*
C*	     ICOLOR, ILINE, LINWID, ILABEL, ISCALE, DMIN, DMAX, CLVL,	*
C*	     NLVL, CLBL, SCFLAG, IRET )					*
C*									*
C* Input parameters:							*
C*	LINE		CHAR*		Line  input string 		*
C*	CRANGE		CHAR*		Contour interval input 		*
C*	SCALE		CHAR*		Scaling factor			*
C*	KX		INTEGER		Number of grid points in x dir	*
C*	KY		INTEGER		Number of grid points in y dir	*
C*	IMIN		INTEGER		Minimum x grid point		*
C*	JMIN		INTEGER		Minimum y grid point		*
C*	IMAX		INTEGER		Maximum x grid point		*
C*	JMAX		INTEGER		Maximum y grid points		*
C*									*
C* Input and output parameters:						*
C*	GRID (KX,KY)	REAL		Scaled grid data		*
C*									*
C* Output parameters:							*
C*	ICOLOR (NLVL)	INTEGER		Contour colors			*
C*	ILINE  (NLVL)	INTEGER		Line types			*
C*	LINWID (NLVL)	INTEGER		Line widths			*
C*	ILABEL (NLVL)	INTEGER		Label type			*
C*	ISCALE		INTEGER		Scaling factor			*
C*	DMIN		REAL		Data minimum			*
C*	DMAX		REAL		Data maximum			*
C*	CLVL   (NLVL)	REAL		Contour levels			*
C*	NLVL		INTEGER		Number of contour levels	*
C*	CLBL   (NLVL)	CHAR*		Contour labels			*
C*	SCFLAG		LOGICAL		Suppress small contour flag	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*                                       +1 = no contour levels		*
C**									*
C* Log:									*
C* T.W.Barker/WR/SSD	 8/91	Created from gdxlev			*
C* M. desJardins/NMC	11/92	Eliminate ERMISS.FNC			*
C* M. Linda/GSC		 9/97	Changed a key word in the prologue	*
C* S. Jacobs/NCEP	 1/99	Changed call to IN_LINE			*
C* S. Jacobs/NCEP	 5/99	Changed call to IN_LINE			*
C* C. Bailey/HPC	 6/06	Added contour label array		*
C* C. Bailey/HPC	10/06	Added suppress small contour flag	*
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
C*
	CHARACTER*(*)	line, crange, scale, clbl (*)
	REAL		grid (*), clvl (*)
	INTEGER		icolor (*), iline (*), linwid (*), ilabel (*)
	LOGICAL		scflag
C------------------------------------------------------------------------
	iret = 0
C
C*	Do an automatic scaling.
C
	CALL GR_SCAL  ( scale,  kx, ky, imin, jmin, imax, jmax, grid,
     +			iscale, dmin, dmax, ier )
C
C*	Find the contour levels to use.
C
	kxky1 = kx * ky
	CALL IN_CINT ( crange, grid, kxky1, dmin, dmax, clvl, nlvl,
     +                 clbl, rint, ier )
	IF ( ier .ne. 0 ) iret = +1
C
C*	Get the colors, line widths,line types and labels to use.
C
	IF ( nlvl .gt. 0 )  THEN
	    CALL IN_LINE  ( line, clvl, nlvl, icolor, iline, linwid, 
     +                      ilabel, smth, fltr, scflag, ier )
C
C*	    Eliminate lines with no color.
C
	    ilvl = 0
	    DO i = 1, nlvl
		IF ( icolor (i) .ge. 1 ) THEN
		    ilvl = ilvl + 1
		    icolor ( ilvl ) = icolor ( i )
		    iline  ( ilvl ) = iline  ( i )
		    linwid ( ilvl ) = linwid ( i )
		    ilabel ( ilvl ) = ilabel ( i )
		    clvl   ( ilvl ) = clvl   ( i )
		    clbl   ( ilvl ) = clbl   ( i )
		END IF
	    END DO
	    nlvl = ilvl
	END IF
C*
	RETURN
	END
