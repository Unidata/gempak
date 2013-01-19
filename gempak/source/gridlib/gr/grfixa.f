	SUBROUTINE GR_FIXA  ( igdfln, area, proj, areout, prjout, iret )
C************************************************************************
C* GR_FIXA								*
C*									*
C* This subroutine takes AREA and replaces GRID or DSET with the grid   *
C* area, EXTEND with the extend area, and DATA with the data area.	*
C* GRID or DSET is obtained from the navigation block; EXTEND and	*
C* DATA are obtained from the analysis block.				*
C*									*
C* GR_FIXA  ( IGDFLN, AREA, PROJ, AREOUT, PRJOUT, IRET )		*
C*									*
C* Input parameters:							*
C*	IGDFLN		INTEGER		Grid file number		*
C*	AREA		CHAR*		Area				*
C*	PROJ		CHAR*		Projection			*
C*									*
C* Output parameters:							*
C*	AREOUT		CHAR*		New area			*
C*	PRJOUT		CHAR*		New projection			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/88						*
C* M. desJardins/GSFC	12/89	Fixed // for Apollo			*
C* K. Brill/GSC         03/90   Added DSET				*
C* K. Brill/NMC         10/90   Set full lon range when lons are bad	*
C* K. Brill/NMC		02/92	Use LLNNAV, LLNANL			*
C* I. Durham/GSC	12/97	Deleted 3 longitudinal corrections	*
C* T. Lee/GSC		11/00	Set projection if needed		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	area, areout, proj, prjout
C*
	REAL		gbnds (4), ebnds (4), dbnds (4), rnav (LLNNAV),
     +			ranl (LLNANL)
	INTEGER		iex (4)
	CHARACTER	str1*8, str2*8, str3*8, str4*8, aaa*72
	CHARACTER	str6*8, str7*8, str8*8, prj*20
C------------------------------------------------------------------------
	iret = 0
C
C*	Convert to upper case.
C
	CALL ST_LCUC  ( area, areout, ier )
	CALL ST_LCUC  ( proj, prjout, ier )
C
C*	Check for word GRID.
C
	ig = INDEX  ( areout, 'GRID' )
	ih = INDEX  ( areout, 'DSET' )
	IF  ( ig .ne. 0 .or. ih .ne. 0 )  THEN
            IF ( ig .eq. 0 ) ig = ih
C
C*	    Read in navigation block.
C
	    CALL GD_GNAV  ( igdfln, rnav, navlen, ier )
	    IF  ( navlen .ge. 13 )  THEN
		CALL ST_RLCH  ( rnav (7), 2, str1, ier )
		CALL ST_LSTR  ( str1, len1, ier )
		CALL ST_RLCH  ( rnav (8), 2, str2, ier )
		CALL ST_LSTR  ( str2, len2, ier )
		CALL ST_RLCH  ( rnav (9), 2, str3, ier )
		CALL ST_LSTR  ( str3, len3, ier )
		CALL ST_RLCH  ( rnav (10), 2, str4, ier )
		CALL ST_LSTR  ( str4, len4, ier )
		IF  ( ig .eq. 1 )  THEN
		    aaa    = areout
		    areout = str1 ( :len1 ) // ';' // str2 ( :len2) //
     +			     ';' // str3 ( :len3) // ';' // 
     +			     str4 ( :len4) // aaa ( ig+4 : )
		  ELSE
		    aaa    = areout
		    areout = aaa ( :ig-1 ) // str1 ( :len1 ) // ';' 
     +			     // str2 ( :len2) // ';' // str3 ( :len3) 
     +			     // ';' // str4 ( :len4) // aaa ( ig+4 : )
		END IF
C
C*		Set projection if PROJ is blank.
C
		IF  ( prjout .eq. ' ' )  THEN
		    CALL ST_ITOC  ( rnav (2), 1, prj, ier )
		    CALL ST_LSTR  ( prj, len5, ier )
		    CALL ST_RLCH  ( rnav (11), 2, str6, ier )
		    CALL ST_LSTR  ( str6, len6, ier )
		    CALL ST_RLCH  ( rnav (12), 2, str7, ier )
		    CALL ST_LSTR  ( str7, len7, ier )
		    CALL ST_RLCH  ( rnav (13), 2, str8, ier )
		    CALL ST_LSTR  ( str8, len8, ier )
		    prjout = prj ( : len5) // '/' // str6 ( : len6)
     +			    // ';' // str7 ( : len7) // ';'
     +			    // str8 ( : len8)
		END IF
	    END IF
	END IF
C
C*	Check for EXTEND.
C
	ie = INDEX  ( areout, 'EXTEND' )
	IF  ( ie .ne. 0 )  THEN
C
C*	    Read in analysis block.
C
	    CALL GD_GANL  ( igdfln, ranl, ianlsz, ier )
	    CALL GR_RBAN  ( ranl, dn, dx, dy, gbnds, ebnds,
     +			    dbnds, iex, ier )
	    IF  ( ier .eq. 0 )  THEN
		CALL ST_RLCH  ( ebnds (1), 2, str1, ier )
		CALL ST_LSTR  ( str1, len1, ier )
		CALL ST_RLCH  ( ebnds (2), 2, str2, ier )
		CALL ST_LSTR  ( str2, len2, ier )
		CALL ST_RLCH  ( ebnds (3), 2, str3, ier )
		CALL ST_LSTR  ( str3, len3, ier )
		CALL ST_RLCH  ( ebnds (4), 2, str4, ier )
		CALL ST_LSTR  ( str4, len4, ier )
		IF  ( ie .eq. 1 )  THEN
		    aaa    = areout
		    areout = str1 ( :len1 ) // ';' // str2 ( :len2) //
     +			     ';' // str3 ( :len3) // ';' // 
     +			     str4 ( :len4) // aaa ( ie+6 : )
		  ELSE
		    aaa    = areout
		    areout = aaa ( :ie-1 ) // str1 ( :len1 ) // ';' 
     +			     // str2 ( :len2) // ';' // str3 ( :len3) 
     +			     // ';' // str4 ( :len4) // 
     +			     aaa ( ie+6 : )
		END IF
	    END IF
	END IF
C
C*	Check for DATA.
C
	id = INDEX  ( areout, 'DATA' )
	IF  ( id .ne. 0 )  THEN
C
C*	    Read in analysis block.
C
	    CALL GD_GANL  ( igdfln, ranl, ianlsz, ier )
	    CALL GR_RBAN  ( ranl, dn, dx, dy, gbnds, ebnds, dbnds,
     +			    iex, ier )
	    IF  ( ier .eq. 0 )  THEN
		CALL ST_RLCH  ( dbnds (1), 2, str1, ier )
		CALL ST_LSTR  ( str1, len1, ier )
		CALL ST_RLCH  ( dbnds (2), 2, str2, ier )
		CALL ST_LSTR  ( str2, len2, ier )
		CALL ST_RLCH  ( dbnds (3), 2, str3, ier )
		CALL ST_LSTR  ( str3, len3, ier )
		CALL ST_RLCH  ( dbnds (4), 2, str4, ier )
		CALL ST_LSTR  ( str4, len4, ier )
		IF  ( id .eq. 1 )  THEN
		    aaa    = areout
		    areout = str1 ( :len1 ) // ';' // str2 ( :len2) //
     +			     ';' // str3 ( :len3) // ';' // 
     +			     str4 ( :len4) // aaa ( id+4 : )
		  ELSE
		    aaa    = areout
		    areout = aaa ( :id-1 ) // str1 ( :len1 ) // ';' 
     +			     // str2 ( :len2) // ';' // str3 ( :len3) 
     +			     // ';' // str4 ( :len4) // 
     +			     aaa ( id+4 : )
		END IF
	    END IF
	END IF
C*
	RETURN
	END
