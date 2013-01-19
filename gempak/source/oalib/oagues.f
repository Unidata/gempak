	SUBROUTINE OA_GUES  ( gg, ix, iy, iextnd, ag, ing, 
     +                        ngrid, kex, key, iret )
C************************************************************************
C* OA_GUES								*
C*									*
C* This subroutine puts the guess grid into the analysis grid.  	*
C*									*
C* OA_GUES  ( GG, IX, IY, IEXTND, AG, ING, NGRID, KEX, KEY, IRET )	*
C*									*
C* Input parameters:							*
C*	GG  (IX, IY)	REAL		Guess grid			*
C*	IX		INTEGER		Guess grid x dimension		*
C*	IY		INTEGER		Guess grid y dimension		*
C*	IEXTND (4)	INTEGER		Grid extension specification	*
C*	ING		INTEGER		Position to load analysis grid	*
C*	NGRID		INTEGER		Total number of grid positions	*
C*	KEX		INTEGER		Analysis grid x dimension	*
C*	KEY		INTEGER		Analysis grid y dimension	*
C*									*
C* Output parameters:							*
C*	AG (NGRID, KEX, KEY)	REAL	Analysis grid (incl extension)	*
C*	IRET		INTEGER		Return code			*
C*				  	  0 = normal return		*
C*			                 -6 = grid alignment error	*
C**									*
C* Log:									*
C* K. Brill/GSC 	 4/90						*
C* J. Nielsen/SUNYA	10/90	Made AG 3-d, modified calling sequence	*
C************************************************************************
	REAL		gg (ix, iy), ag (ngrid, kex, key)
	INTEGER         iextnd ( * )
C------------------------------------------------------------------------
	iret = 0
C
C*	Compute start and stop indexes for grid area within the
C*      analysis grid.
C
	ix1 = iextnd (1) + 1
        ix2 = kex - iextnd (3)
	iy1 = iextnd (2) + 1
        iy2 = key - iextnd (4)
C*
	i=0
	DO iag = ix1, ix2
	  i = i + 1
	  IF ( i .gt. ix ) THEN
	    iret = -6
	    CALL ER_WMSG ( 'OA', iret, ' ', ier )
            RETURN
	  END IF
	  j = 0
	  DO jag = iy1, iy2
	    j = j + 1
	    IF ( j .gt. iy ) THEN
	      iret = -6
	      CALL ER_WMSG ( 'OA', iret, ' ', ier )
	      RETURN
	    END IF
	    ag ( ing, iag, jag ) = gg ( i, j )
	  END DO
	END DO
C*
	IF ( i .ne. ix .or. j .ne. iy ) THEN
	  iret = -6
	  CALL ER_WMSG ( 'OA', iret, ' ', ier )
	  RETURN
	END IF
C*
	RETURN
	END
