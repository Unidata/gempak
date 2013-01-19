	SUBROUTINE GCLGRNF ( kx, ky, grid, ioffx, ioffy, iskip, nlvl, 
     +			     clvl, clbl, icolr, lintyp, linwid, linlbl, 
     +			     scflag, zgrid, jhline, jvline, iret )
C************************************************************************
C* GCLGRNF								*
C* 									*
C* This subroutine draws contours through a grid of data.  The 		*
C* algorithm used is based on a two-dimensional Lagrangian fitting of	*
C* the grid points.  It is the original GEMPAK contouring program.	*
C* The original subroutine, GCONTR, had a different calling sequence.	*
C* 									*
C* GCLGRNF ( KX, KY, GRID, IOFFX, IOFFY, ISKIP, NLVL, CLVL, CLBL, 	*
C*           ICOLR, LINTYP, LINWID, LINLBL, SCFLAG, ZGRID, JHLINE,      *
C*           JVLINE, IRET )						*
C*									*
C* Input parameters:							*
C*									*
C*	KX		INTEGER		Number of x grid points 	*
C*	KY		INTEGER		Number of y grid points		*
C*	GRID (KX,KY)	REAL		Grid data array			*
C*	IOFFX		INTEGER		X offset to first point		*
C*	IOFFY		INTEGER		Y offset to first point		*
C*	ISKIP		INTEGER		Skip factor in original grid	*
C*	NLVL		INTEGER		Number of contour levels	*
C*	CLVL   (NLVL)	REAL		Contour level values		*
C*	CLBL   (NLVL)	CHAR*		Contour labels			*
C*	ICOLR  (NLVL)	INTEGER		Contour color numbers		*
C*	LINTYP (NLVL)	INTEGER		Contour line types		*
C*	LINWID (NLVL)	INTEGER 	Contour line widths		*
C*	LINLBL (NLVL)	INTEGER		Contour label types		*
C*	SCFLAG		LOGICAL		Small contour suppress flag	*
C*									*
C* Work parameters:							*
C*      ZGRID (KX*KY)   REAL            Array for grid points           *
C*      JHLINE (KX*KY)  INTEGER         Array for Horiz crossings       *
C*      JVLINE (KX*KY)  INTEGER         Array for Vert crossings       *
C*									*
C* Output parameters:							*
C*									*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/NMC	11/91	From GCONTR				*
C* S. Jacobs/NCEP	 1/96	Added CADJST to adjust grid values	*
C* M. desJardins/NCEP	 9/98	Eliminate subboxing when CONTUR=1	*
C* C. Bailey/HPC	 6/06	Added contour label array		*
C* C. Bailey/HPC	10/06	Added small contour suppress flag	*
C* S. Gilbert/NCEP	07/07	Added Work arrays as input args		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE 	'ERROR.PRM'
	INCLUDE		'CONTUR.CMN'
C*
	REAL		grid (*), clvl (*), zgrid (*)
	INTEGER		icolr (*), lintyp (*), linwid (*), linlbl (*)
	INTEGER		jhline (*), jvline (*)
	CHARACTER*(*)	clbl (*)
	LOGICAL		scflag
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Read in the box subset factor.  Check that it is valid and set
C*	it in common for the rest of the original contouring code.
C
	IF  ( jbxsub .lt. 2 )  THEN
	    numsub = 2
	  ELSE IF  ( jbxsub .gt. 48 )  THEN
	    numsub = 50
	  ELSE
	    numsub = jbxsub + 2
	END IF
C
C*	Move offsets and skip factor into common.
C
	offx = FLOAT ( ioffx )
	offy = FLOAT ( ioffy )
	skip = FLOAT ( iskip )
C
C*	This program expects the data in the zgrid array 
C
	DO  i = 1, kx * ky
	    zgrid (i) = grid (i)
	END DO
C
C*	Adjust the grid values.
C
	CALL CADJST ( kx, ky, nlvl, clvl, zgrid, ier )
C
C*	Save grid size as ISIZE, JSIZE.
C
	isize = kx
	jsize = ky
C
C*	Compute distance along each subbox.
C
	fincxy = 1. / FLOAT ( numsub - 1 )
C
C*	Call the driver for the Lagrangian contouring.
C
	CALL CLDRIV  ( nlvl, clvl, clbl, icolr, lintyp, linwid, 
     +		       linlbl, scflag, zgrid, jhline, jvline, ier )
C*
	RETURN
	END
