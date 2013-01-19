	SUBROUTINE GR_SUB2 ( rnvblk, rgx2, igxold, iret )
C************************************************************************
C* GR_SUB2								*
C*									*
C* This subroutine resets the subset grid navigation when the subset	*
C* traverses the original grid boundary.				*
C*									*
C*									*
C* GR_SUB2 ( RNVBLK, RGX2, IGXOLD, IRET )				*
C*									*
C* Input parameters:							*
C*	RNVBLK(*)	REAL 		Grid navigation			*
C*									*
C* Input and Output parameters:						*
C*	RGX2		REAL		Number of input points in x dir	*
C*									*
C*									*
C* Output parameters:							*
C*	IGXOLD		INTEGER		Old number of input points in x	*
C* 	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-21 = cannot cross grid bounds	*
C**									*
C* Log:									*
C* K. Brill/NMC		08/95 						*
C* D.W.Plummer/NCEP	 3/00	Rename from NAGSS2 to GR_SUB2		*
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
C*
	REAL		rnvblk (*)
	REAL		rglt(2), rgln(2)
C*
	CHARACTER*4	gprj
	LOGICAL		compar
C*
	compar (xx, yy) = ( ABS ( xx - yy ) .lt. RDIFFD )
C------------------------------------------------------------------------
	iret = -21
C*
	igxold = int ( rnvblk (5) )
	CALL GQGPRJ ( gprj, ag1, ag2, ag3, mx, my, aglt1, agln1,
     +		      aglt2, agln2, ier ) 
	IF ( gprj .ne. 'MER' .and. gprj .ne. 'CED' ) RETURN
C
C*	Check to see if another grid column is needed.
C
	rglt (1) = 1.
	rglt (2) = FLOAT ( mx + 1 )
	rgln (1) = 1.
	rgln (2) = 1.
	CALL GTRANS ( 'G', 'M', 2, rglt, rgln, rglt, rgln, ier )
	IF ( compar ( rgln (1), rgln (2) ) ) THEN
C
C*	    An extra column is needed to achieve complete wrapping.
C
	    igxold = INT ( rnvblk (5) )
	ELSE
C
C*	    Check for complete wrap around.
C
	    rglt (1) = 1.
	    rglt (2) = FLOAT (mx)
	    rgln (1) = 1.
	    rgln (2) = 1.
	    CALL GTRANS ( 'G', 'M', 2, rglt, rgln, rglt, rgln, ier )
	    IF ( compar ( rgln (1), rgln (2) ) ) THEN
		igxold = INT ( rnvblk (5) ) - 1
	    ELSE
		RETURN
	    END IF
	END IF
	iret = 0
	rgx2 = rgx2 + float ( igxold )
C*
	RETURN
	END
