	SUBROUTINE GR_NLEV  ( nclvl, clvl, clbl, iccolr, icltyp, iclwid,
     +			     iclabl, iret )
C************************************************************************
C* GR_NLEV								*
C*									*
C* This subroutine is based on gdcntr/gdnlev.f 				*
C* This subroutine checks that no duplicate contour levels have been	*
C* input.  This subroutine also sorts the contour levels from smallest	*
C* to largest.   It call be called by any grid program, for contour	*
C* lines or color fill contours.					*
C*									*
C* GR_NLEV  ( NCLVL, CLVL, CLBL, ICCOLR, ICLTYP, ICLWID, ICLABL, IRET )	*
C*									*
C* Input and output parameters:						*
C*	NCLVL		INTEGER		Number of contours		*
C*	CLVL   (NLVL)	REAL		Contour levels			*
C*	CLBL   (NLVL)	CHAR*		Contour labels			*
C*	ICCOLR (NLVL)	INTEGER		Contour colors			*
C*	ICLTYP (NLVL)	INTEGER		Line types			*
C*	ICLWID (NLVL)	INTEGER		Line widths			*
C*	ICLABL (NLVL)	INTEGER		Label types			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* P. Bruehl/Unidata	 8/94	Based on gdcntr/gdnlev.f		*
C* M. Linda/GSC		 9/97	Changed a key word in the prologue	*
C* T. Lee/SAIC		 8/01	Fixed contour fill			*
C* C. Bailey/HPC	 6/06	Added contour labels			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		clvl (*)
	INTEGER		iccolr (*), icltyp (*), iclwid (*), iclabl (*)
	CHARACTER*(*)	clbl (*)
C*
	CHARACTER*24	tlbl
	LOGICAL		onelev
C------------------------------------------------------------------------
	iret  = 0
C
C*	  Eliminate duplicate levels
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
		    iccolr (ilvl) = iccolr (i)
		    iclwid (ilvl) = iclwid (i)
		    icltyp (ilvl) = icltyp (i)
		    iclabl (ilvl) = iclabl (i)
		END IF
	    END DO
	    nclvl = ilvl
	    IF  ( nclvl .eq. LLCLEV )  THEN
		nclvl = nclvl - 1
	    END IF
	    nclvl1 = nclvl + 1
C
C*	    Check that at least one line has a color.  
C
	    onelev = .false.
	    DO  i = 1, nclvl1
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
C*
	RETURN
	END
