	SUBROUTINE GDIGEN  ( gdfile, anl, rnav, numgrd, maxgrd, nlun, 
     +			     luns, iret )
C************************************************************************
C* GDIGEN								*
C*									*
C* This subroutine prints general information for GDINFO.		*
C*									*
C* GDIGEN  ( GDFILE, ANL, RNAV, NUMGRD, MAXGRD, NLUN, LUNS, IRET )	*
C*									*
C* Input parameters:							*
C* 	GDFILE		CHAR*		Grid file name			*
C*	ANL (*)		REAL		Analysis block			*
C*	RNAV (*)	REAL		Navigation block		*
C*	NUMGRD		INTEGER		Number of grids in file		*
C*	MAXGRD		INTEGER		Maximum number of grids		*
C*	NLUN		INTEGER		Number of output units		*
C*	LUNS (NLUN)	INTEGER		Logical unit numbers		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/85						*
C* M. desJardins/GSFC	 8/88	GEMPAK4 Version				*
C* M. desJardins/NMC	 4/91	Add call to GR_RBAN			*
C* K. Brill/NMC		01/93	Write blank line at end			*
C* K. Brill/HPC         02/13   Enhance precision for grd corner output;*
C*				allow more digits for KX & KY		*
C************************************************************************
	CHARACTER*(*)	gdfile
	REAL		anl (*), rnav (*)
	INTEGER		luns (*)
C*
	CHARACTER	proj*20
	REAL		gbnds (4), dbnds (4), ebnds (4)
	INTEGER		iextnd (4)
C------------------------------------------------------------------------
	iret = 0
C
C*	Loop through each output unit.
C
	DO  i = 1, nlun
	    lunout = luns (i)
C
C*	    Write output grid file name.
C
	    WRITE  ( lunout, 1000 )  gdfile
 1000	    FORMAT ( / ' GRID FILE: ', A )
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
 2003		FORMAT ( 5X, 'GRID SIZE:', T25, 2I8 /
     +                   5X, 'LL CORNER:', T25, 2F10.4 /
     +                   5X, 'UR CORNER:', T25, 2F10.4 / )
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
4010	    FORMAT ( / ' Maximum number of grids in file: ', I6, / )
	END DO
C*
	RETURN
	END
                           
