	SUBROUTINE SNSLEV  ( cflag, line, cint, fflag, fline, fint, kxy,
     +			     grid, nclvl, clvl, clbl, iccolr, icltyp, 
     +			     iclwid, iclabl, nflvl, flvl, ifcolr, 
     +			     iflabl, ifltyp, scflag, iret )
C************************************************************************
C* SNSLEV								*
C*									*
C* This subroutine decides which contour levels to create and the	*
C* colors and line types to use.					*
C*									*
C* SNSLEV  ( CFLAG, LINE, CINT, FFLAG, FLINE, FINT, KXY, GRID, NCLVL, 	*
C*	     CLVL, CLBL, ICCOLR, ICLTYP, ICLWID, ICLABL, NFLVL, FLVL,	*
C*           IFCOLR, IFLABL, IFLTYP, SCFLAG, IRET )			*
C*									*
C* Input parameters:							*
C*	CFLAG		LOGICAL		Line contour flag		*
C*	LINE		CHAR*		Line input string 		*
C*	CINT		CHAR*		Contour interval input 		*
C*	FFLAG		LOGICAL		Fill contour flag		*
C*	FLINE		CHAR*		Fill line input string		*
C*	FINT		CHAR*		Fill contour interval input	*
C*	KXY		INTEGER		Number of grid points		*
C*									*
C* Input and output parameters:						*
C*	GRID (KXY)	REAL		Grid data			*
C*									*
C* Output parameters:							*
C*	NCLVL		INTEGER		Number of line contours		*
C*	CLVL   (NLVL)	REAL		Line contour levels		*
C*	CLBL   (NLVL)	CHAR*		Line contour labels		*
C*	ICCOLR (NLVL)	INTEGER		Line contour colors		*
C*	ICLTYP (NLVL)	INTEGER		Line types			*
C*	ICLWID (NLVL)	INTEGER		Line widths			*
C*	ICLABL (NLVL)	INTEGER		Label types			*
C*	NFLVL		INTEGER		Number of fill contours		*
C*	FLVL   (NLVL)	REAL		Fill contour levels		*
C*	IFCOLR (NLVL+1)	INTEGER		Fill contour colors		*
C*	IFLABL (NLVL+1)	INTEGER		Fill contour labels		*
C*	IFLTYP (NLVL+1)	INTEGER		Fill contour types		*
C*	SCFLAG		LOGICAL		Suppress small contour flag	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. Brill/NMC		01/92	Recreated from GDNLEV			*
C* G. Krueger/EAI	 8/93	Modified to get RINT from IN_CINT	*
C* M. Linda/GSC		 9/97	Changed a key word in the prologue	*
C* S. Jacobs/NCEP	 1/99	Changed call to IN_LINE			*
C* S. Jacobs/NCEP	 5/99	Changed call to IN_LINE			*
C* T. Lee/SAIC		10/01	Added contour fill types		*
C* C. Bailey/HPC	 6/06	Added contour labels			*
C* C. Bailey/HPC	10/06	Added suppress small contour flag	*
C* S. Jacobs/NCEP	12/08	Added calc of dmin/dmax for fill option	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	line, cint, fline, fint, clbl (*)
	REAL		grid (*), clvl (*), flvl (*)
	INTEGER		iccolr (*), icltyp (*), iclwid (*), iclabl (*),
     +			ifcolr (*), iflabl (*), ifltyp (*)
	LOGICAL		cflag, fflag, scflag
C*
	CHARACTER*24	tlbl
	LOGICAL		onelev
C------------------------------------------------------------------------
	iret  = 0
	nclvl = 0
	nflvl = 0
	dmin = RMISSD
	dmax = dmin
C
C*	Get the levels for the line contours first.
C
	IF  ( cflag )  THEN
C
C*	  Find the contour levels to use.
C
	  CALL IN_CINT  ( cint, grid, kxy, dmin, dmax, clvl, nclvl,
     +			  clbl, rint, iret )
	  IF  ( iret .ne. 0 )  THEN
	    nclvl = 0
	    iret = +1
	    RETURN
	  END IF
C
C*	  Eliminate duplicate levels; sort levels; assign line attributes.
C
	  IF  ( nclvl .gt. 0 )  THEN
C
C*	    Make sure there are no duplicate levels.
C
	    ilvl = 1
	    DO  i = 2, nclvl
		IF  ( clvl (i) .ne. clvl (i-1) )  THEN
		    ilvl = ilvl + 1
		    clvl (ilvl) = clvl (i)
		    clbl (ilvl) = clbl (i)
		END IF
	    END DO
	    nclvl = ilvl
C
C*	    Get the colors, line types, line widths and labels.
C
	    CALL IN_LINE ( line, clvl, nclvl, iccolr, icltyp, iclwid, 
     +			   iclabl, smth, fltr, scflag, iret )
C
C*	    Check that at least one line has a color.  
C
	    onelev = .false.
	    DO  i = 1, nclvl
		IF  ( iccolr (i) .gt. 0 )  onelev = .true.
	    END DO
	    IF  ( .not. onelev )  THEN
		nclvl = 0
	      ELSE
C
C*		Sort the levels from smallest to largest.
C
		DO  i = 1, nclvl - 1
		    DO  j = i+1, nclvl
			IF  ( clvl (i) .gt. clvl (j) )  THEN
			    jcol = iccolr (i)
			    jtyp = icltyp (i)
			    jwid = iclwid (i)
			    jlbl = iclabl (i)
			    csav = clvl (i)
			    tlbl = clbl (i)
			    iccolr (i) = iccolr (j)
			    icltyp (i) = icltyp (j)
			    iclwid (i) = iclwid (j)
			    iclabl (i) = iclabl (j)
			    clvl   (i) = clvl   (j)
			    clbl   (i) = clbl   (j)
			    iccolr (j) = jcol
			    icltyp (j) = jtyp
			    iclwid (j) = jwid
			    iclabl (j) = jlbl
			    clvl   (j) = csav
			    clbl   (j) = tlbl
			END IF
		    END DO
		END DO
	    END IF
	  END IF
	END IF
C
C*	Now, get the contour levels for the filled contours.
C
	IF  ( fflag )  THEN
C
C*	  Compute the data minimum and maximum values from the grid.
C
	  isc = 0
	  ix1 = 1
	  iy1 = 1
	  ix2 = 128
	  iy2 = 128
	  CALL GR_SSCL ( isc, LLMAXD, LLMAXD, ix1, iy1, ix2, iy2,
     +			grid, dmin, dmax, ier )
C
C*	  Process color fill contours.
C
	  CALL IN_FILL ( fint, fline, dmin, dmax, flvl, nflvl, rfint,
     +			 fmin, fmax, ifcolr, ifltyp, iflabl, iret )
	  IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG ( 'IN', iret, ' ', ier )
	    iret = +1
	    nflvl = 0
	  END IF
	END IF
C*
	RETURN
	END
