	SUBROUTINE GDNDSP ( map, gdfile, gdtime, level, ivcord, parm,
     +			    garea, iscale, dmin, dmax, cflag, nclvl, 
     +			    clvl, icolor, iline, ilwid, ilabel, fflag,
     +			    nflvl, flvl, ifcolr, ifltyp, contin, iret )
C************************************************************************
C* GDNDSP								*
C*									*
C* This subroutine allows the user to accept parameters for the GDCNTR	*
C* program.								*
C*									*
C* GDNDSP ( MAP, GDFILE, GDTIME, LEVEL, IVCORD, PARM, GAREA, ISCALE,	*
C*          DMIN, DMAX, CFLAG, NCLVL, CLVL, ICOLOR, ILINE, ILWID,	*
C*          ILABEL, FFLAG, NFLVL, FLVL, IFCOLR, IFLTYP, CONTIN, IRET )	*
C*									*
C* Input parameters:							*
C*	MAP		CHAR*		Map specified by user		*
C*	GDFILE		CHAR*		Grid file			*
C*	GDTIME(2)	CHAR*		Grid time			*
C*	LEVEL(2)	INTEGER		Grid level			*
C*	IVCORD		INTEGER		Grid vertical coordinate	*
C*	PARM		CHAR*		Grid parameter			*
C*	GAREA		CHAR*		Graphics area			*
C*	ISCALE		INTEGER		Scaling factor			*
C*	DMIN		REAL 		Minimum data value		*
C*	DMAX		REAL 		Maximum data value		*
C*	CFLAG		LOGICAL		Flag for line contours		*
C*	NCLVL		INTEGER		Number of contour levels	*
C*	CLVL   (NCLVL)	REAL		Contour levels			*
C*	ICOLOR (NCLVL)	INTEGER		Contour colors			*
C*	ILINE  (NCLVL)	INTEGER		Contour line type		*
C* 	ILWID  (NCLVL)	INTEGER		Contour line width		*
C*	ILABEL (NCLVL)	INTEGER		Label type			*
C*	FFLAG		LOGICAL		Flag for fill contours		*
C*	NFLVL		INTEGER		Number of fill contour levels	*
C*	FLVL   (NFLVL+1)REAL		Fill contour levels		*
C*	IFCOLR (NFLVL+1)INTEGER		Fill colors			*
C*	IFLTYP (NFLVL+1)INTEGER		Fill types			*
C*	CONTIN		LOGICAL		Flag for asking user to continue*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  3 = user entered "EXIT"	*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 2/85						*
C* I. Graffman/RDS	 3/86	Respond flag fix, added min,max values	*
C* I. Graffman/RDS	 6/86	Added line width			*
C* I. Graffman/RDS	 3/88	Standard respond flag handling		*
C* M. desJardins/GSFC	 6/88	Cleaned up				*
C* S. Schotz/GSC	 6/90	Get respnd locally from IP_RESP		*
C* M. desJardins/NMC	12/91	Added contour fill			*
C* K. Brill/NMC		01/92	Increment I1 & I2 in fill loop		*
C* S. Jacobs/NCEP	 8/98	Added contin; Removed some output	*
C* T. Lee/SAIC		11/01	Displayed contour fill types		*
C************************************************************************
	CHARACTER*(*)	map, gdfile, gdtime (2), parm, garea
	INTEGER		level (2), icolor (*), iline (*), ilwid (*),
     +			ilabel (*), ifcolr (*), ifltyp (*)
	REAL		clvl (*), flvl (*)
	LOGICAL		contin, respnd, cflag, fflag
C-----------------------------------------------------------------------
	iret = 0
	ier  = 1
C
C*	Write out the grid file name.
C
	WRITE  ( 6, 1000) gdfile
1000	FORMAT ( / ' Grid file: ', A )
C
C*	Write out the grid identifier.
C
	WRITE  ( 6, 2000 )
2000	FORMAT ( ' GRID IDENTIFIER: ' )
	CALL GR_WTRM  ( 6, .true., 0, gdtime, level, ivcord, parm, ier )
C
C*	Write out the graphics area and scaling factor.
C
	WRITE  ( 6, 3000 ) garea, iscale, dmin, dmax
3000	FORMAT ( / ' GAREA:     ', A, ' SCALE: ', I2 , //
     *             ' MINIMUM AND MAXIMUM VALUES', 2f9.2)
C
C*	Check for no data.
C
	IF  ( dmin .eq. 0.0 .and. dmax .eq. 0.0 )  THEN
	    WRITE  ( 6, 4000 )
4000	    FORMAT ( 'NO CONTOUR LEVELS COMPUTED' )
	  ELSE
C
C*	    Write out levels in groups of 7.
C
	    IF  ( cflag )  THEN
		WRITE (6,1015)
1015		FORMAT ( /,  ' LINE CONTOURS: ' )
		nblk = ( nclvl - 1 ) / 7 + 1
	  	i1 = 1
	  	i2 = 7
	  	DO  i = 1, nblk
	    	    IF  ( i2 .gt. nclvl )  i2 = nclvl
		    WRITE ( 6, 1010 ) ( clvl   (j), j = i1, i2 )
1010		    FORMAT ( /' LEVELS:    ',7F9.2 )
		    i1 = i1 + 7
		    i2 = i2 + 7
		END DO
	    END IF
C
C*	    Write out contour fill information.
C
	    IF  ( fflag )  THEN
		WRITE ( 6, 2015 ) 
2015		FORMAT ( /, ' FILLED CONTOURS: ' )
		nblk = ( nflvl - 1 ) / 7 + 1
		i1 = 1
		i2 = 7
		DO  i = 1, nblk
		    i3 = i2
		    IF  ( i2 .ge. nflvl )  THEN
			i2 = nflvl
			i3 = nflvl + 1
		    END IF
		    WRITE (6, 1010 ) ( flvl  (j), j = i1, i2 )
		    WRITE (6, 2011 ) ( ifcolr (j), j = i1, i3 )
2011		    FORMAT ( ' COLORS:', 8 ( I7, 2X ) )
		    WRITE (6, 2012 ) ( ifltyp (j), j = i1, i3 )
2012		    FORMAT ( '  TYPES:', 8 ( I7, 2X ) )
		    i1 = i1 + 7
		    i2 = i2 + 7
		END DO
	    END IF
	END IF
C
C*	If respond is set, wait for user to accept parameters.
C
	IF  ( contin )  THEN
	    CALL IP_RESP ( respnd, ier )
	    IF  ( respnd )  THEN
		CALL TM_ACCP  ( ier )
		IF  ( ier .eq. 2 )  iret = 3
	    END IF
	END IF
C*
	RETURN
	END
