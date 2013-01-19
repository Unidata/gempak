	SUBROUTINE GD_GENI  ( gdfile, nlun, luns, iret )
C************************************************************************
C* GD_GENI								*
C*									*
C* This subroutine prints general grid information given a grid		*
C* filename.  The grid file is opened and closed within this function.	*
C*									*
C* GD_GENI  ( GDFILE, NLUN, LUNS, IRET )				*
C*									*
C* Input parameters:							*
C*	GDFILE		CHAR*		Grid file name			*
C*	NLUN		INTEGER		Number of output units		*
C*	LUNS (NLUN)	INTEGER		Logical unit numbers		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* D.W.Plummer/NCEP	 3/00	From GDIGEN				*
C* R. Tian/SAIC		 4/05	Changed GD_OPNF to GD_OPEN		*
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
C
	REAL		anl (LLNANL), rnav (LLNNAV)
	INTEGER		luns (*)
	CHARACTER	gdfile*(*)
C*
	CHARACTER	proj*20, firstm*20, lasttm*20
	REAL		gbnds (4), dbnds (4), ebnds (4)
	INTEGER		iextnd (4)
C------------------------------------------------------------------------
	iret = 0
C
	CALL GD_OPEN ( gdfile, .false., LLNANL, LLNNAV, igdfln,
     +                 anl, rnav, maxgrd, ier )
	CALL GD_NGRD ( igdfln, numgrd, firstm, lasttm, ier )
C
C*	Loop through each output unit.
C
	DO  i = 1, nlun
	    lunout = luns (i)
C
C*          Write output grid file name.
C
            WRITE  ( lunout, 1000 )  gdfile
 1000       FORMAT ( / ' GRID FILE: ', A )
C
C*	    Write grid navigation block.
C
	    kx = rnav (5)
	    ky = rnav (6)
	    in = rnav (1)
	    CALL ST_ITOC  ( rnav (2), 1, proj, ier )
	    WRITE  ( lunout, 2000 )
 2000	    FORMAT ( / ' GRID NAVIGATION: ' )
	    IF  ( ( in .eq. 1 ) .or. ( in .eq. 2 ) .or. 
     +		  ( in .eq. 3 ) )  THEN
		WRITE  ( lunout, 2001 )  proj
 2001		FORMAT ( 5X, 'PROJECTION:', T27, A )
		IF  ( in .eq. 2 )  THEN
		    WRITE  ( lunout, 2002 )  ( rnav (j), j = 11, 13 )
 2002		    FORMAT ( 5X, 'ANGLES:', T25, 3F8.1 )
		END IF
		WRITE  ( lunout, 2003 )  kx, ky, ( rnav (j), j = 7, 10 )
 2003		FORMAT ( 5X, 'GRID SIZE:', T25, 2I4 /
     +                   5X, 'LL CORNER:', T25, 2F10.2 /
     +                   5X, 'UR CORNER:', T25, 2F10.2 / )
	      ELSE
		WRITE  ( lunout, 2010 )
 2010		FORMAT ( 5X, ' UNKNOWN GRID NAVIGATION ' )
	    END IF
C
C*	    Get grid analysis variables.
C
	    CALL GR_RBAN  ( anl, deltan, deltax, deltay, gbnds,
     +			    ebnds, dbnds, iextnd, ier )
C
C*	    Write grid analysis block.
C
	    WRITE ( lunout, 3000 )
 3000	    FORMAT ( ' GRID ANALYSIS BLOCK: ' )
	    IF  ( ier .eq. 0 )  THEN
		WRITE  ( lunout, 3001 )  
 3001		FORMAT ( 5X,'ANALYSIS TYPE:', T28, 'BARNES' )
		WRITE  ( lunout, 3002 )  deltan, deltax, deltay
 3002		FORMAT ( 5X, 'DELTAN:', T24, F9.3 /
     +                   5X, 'DELTAX:', T24, F9.3 /
     +                   5X, 'DELTAY:', T24, F9.3 )
		WRITE  ( lunout, 3003 )  gbnds, ebnds, dbnds
 3003	        FORMAT ( 5X, 'GRID AREA:', T25, 4F8.2 /
     +                   5X, 'EXTEND AREA:', T25, 4F8.2 /
     +                   5X, 'DATA AREA:', T25, 4F8.2 )
	      ELSE
		WRITE  ( lunout, 3004 )
 3004		FORMAT ( 5x, ' UNKNOWN ANALYSIS TYPE ' )
	    END IF
C
C*	    Write out the number of grids.
C
	    WRITE  ( lunout, 4000 ) numgrd
4000	    FORMAT ( / ' Number of grids in file: ', I5 )
	    WRITE  ( lunout, 4010 ) maxgrd
4010	    FORMAT ( / ' Maximum number of grids in file: ', I6 )
C
C*	    Write out first and last times.
C
	    WRITE  ( lunout, 5000 ) firstm
 5000	    FORMAT ( / ' First time in file: ', A )
	    WRITE  ( lunout, 5010 ) lasttm 
 5010	    FORMAT ( ' Last  time in file: ', A, / )
C
	END DO
C
	CALL GD_CLOS ( igdfln, ier )
C*
	RETURN
	END
