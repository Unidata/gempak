	SUBROUTINE VG_IGEN  ( gdf, rnav, nlun, luns, iret )
C************************************************************************
C* VG_IGEN								*
C*									*
C* This subroutine prints navigation information for a grid file.	*
C*									*
C* VG_IGEN  ( GDF, RNAV, NLUN, LUNS, IRET )				*
C*									*
C* Input parameters:							*
C* 	GDF   		CHAR*		Grid file name			*
C*	RNAV (*)	REAL		Navigation block		*
C*	NLUN		INTEGER		Number of output units		*
C*	LUNS (NLUN)	INTEGER		Logical unit numbers		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. Brill		09/92						*
C************************************************************************
	CHARACTER*(*)	gdf
	REAL		rnav (*)
	INTEGER		luns (*)
C*
	CHARACTER	proj*20
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
	    WRITE  ( lunout, 1000 )  gdf
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
 2003		FORMAT ( 5X, 'GRID SIZE:', T25, 2I4 /
     +                   5X, 'LL CORNER:', T25, 2F10.2 /
     +                   5X, 'UR CORNER:', T25, 2F10.2 / )
	      ELSE
		WRITE  ( lunout, 2010 )
 2010		FORMAT ( 5X, ' UNKNOWN GRID NAVIGATION ' )
	    END IF
	END DO
C*
	RETURN
	END
                           
