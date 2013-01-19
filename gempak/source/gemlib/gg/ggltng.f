	SUBROUTINE GG_LTNG ( dattim, ktminc, kcolrs, numc, ppmark,
     +			     pnmark, iflgs, iret )
C************************************************************************
C* GG_LTNG								*
C*									*
C* This subroutine sets up the times and attributes for plotting        *
C* lightning data.                                                      *
C*									*
C* GG_LTNG ( DATTIM, KTMINC, KCOLRS, NUMC, PPMARK, PNMARK, IFLGS, IRET )*
C*                                                                      *
C* Input parameters:							*
C*	DATTIM		CHAR*		Ending time for lightning data  *
C*	KTMINC(*)	INTEGER 	Time increments in minutes      *
C*	KCOLRS(*)	INTEGER		Color for each time increment   *
C*	NUMC		INTEGER		Number of colors                *
C*	PPMARK(*)	REAL		Marker values for + strikes     *
C*	PNMARK(*)	REAL		Marker values for - strikes     *
C*	IFLGS(*)	INTEGER		Flag values for data 		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	12/99	                                        *
C* S. Jacobs/NCEP	12/99	Added color bar				*
C* D. Kidwell/NCEP	 2/00	Added multiple data sources;restructured*
C* S. Jacobs/NCEP	 3/00	Renamed KSORC to IFLGS; Changed call to	*
C*				GG_LTRP to add Pos and Neg flags	*
C* M. Li/GSC             5/00   Added MXNMFL and MXFLSZ                 *
C* J. Wu/GSC             7/00   Added checks for TI_STAN return status	*
C* S. Jacobs/NCEP	 3/01	Increased file template to 48 chars	*
C* S. Jacobs/NCEP	10/01	Removed loop for getting attributes	*
C* A. Hardy/SAIC         2/02   Changed call FL_SCND			*
C* B. Yin/SAIC           3/04   Changed SS_GTIM to CSS_GTIM             *
C* T. Lee/SAIC		 9/04	Replaced FL_TMPL with CTB_DTGET		*
C* T. Lee/SAIC		10/04	Increased ALIAS size to append NULL	*
C* A. Hardy/NCEP	11/04	Added calls to ST_RNUL			*
C* m.gamazaychikov/SAIC 12/04   Added ion flag to CTB_DTGET CS          *
C* m.gamazaychikov/SAIC 01/06   Changed templ string length to MXTMPL   *
C* m.gamazaychikov/SAIC 04/06   Added idtmch flag to CTB_DTGET CS       *
C* F. J. Yen/NCEP        4/08   Added bin mins & mstrct to CTB_DTGET CSC*
C* S. Jacobs/NCEP	 7/08	Fixed calc of time increments		*
C* L. Hinson/AWC         4/11   Add LTGA Alias for ATDNet Lightning     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	dattim
	INTEGER		ktminc (*), kcolrs (*), iflgs (*)
	REAL 		ppmark (*), pnmark (*)
C*
	PARAMETER	( NUMSRC = 4 )
C*
	CHARACTER	path*25, templ*(MXTMPL), cdttm*20, dattm2*20, 
     +         		tfile*128, stime*20, flstrt*160, datfil*20,
     +			datgrp (LLCLEV)*20
	CHARACTER*(MXFLSZ)      filnam, files (MXNMFL), alias (NUMSRC)
	INTEGER		itarr(5), jtarr(5), itminc (LLCLEV),
     +			icolrs (LLCLEV), mark (6), itype
	REAL		ftminc(LLCLEV), szmrkr (2)
	LOGICAL		done
C*
	DATA		alias / 'LTNG', 'LTGO', 'LTGT', 'LTGA' /
C-----------------------------------------------------------------------
	iret = 0
C
	numclr = numc
	IF ( numclr .gt. 0 ) THEN
 	    DO ii = 1, numclr
		itminc ( ii ) = ktminc ( ii )
		ftminc ( ii ) = ktminc ( ii )
		icolrs ( ii ) = kcolrs ( ii )
	    END DO
	  ELSE
C
C*	    No times or colors were specified.  Set defaults.
C
	    numclr = 1
	    itminc ( 1 ) = 5
	    ftminc ( 1 ) = 5.
	    icolrs ( 1 ) = 2
	END IF
	ftminc (numclr+1) = ftminc (numclr)
	icolrs (numclr+1) = 0
        nexp   = MXNMFL
	iorder = 1
C
C*	Check for the last file requested by the user.
C
	CALL ST_LCUC ( dattim, dattim, ier )
	itype = 1
	IF  ( dattim .eq. 'LAST' ) THEN
	    CALL CSS_GTIM ( itype, dattm2, ier )
	  ELSE IF ( dattim .eq. 'ALL' ) THEN
	    RETURN
	  ELSE
	    CALL CSS_GTIM ( itype, cdttm, ier )
	    CALL TI_STAN ( dattim, cdttm, dattm2, ier )
	    IF ( ier .ne. 0 ) THEN
	    	CALL ER_WMSG ( 'TI', ier, dattim, ierr )
	    	iret = ier
	    	return
	    ENDIF	    	
	END IF
C
C*	Calculate how far to go back by using the last time increment.
C
	minuts = itminc ( numclr )
	CALL TI_CTOI ( dattm2, itarr, ier )
	CALL TI_SUBM ( itarr, minuts, jtarr, ier )
	CALL TI_ITOC ( jtarr, stime, ier )
C
C*	Get the break times for changing the marker color.
C*	Subtract each increment from the frame time and add it to 
C*	the list with the earliest time first.
C
	ntimes = numclr + 1
	datgrp ( ntimes ) = dattm2
	CALL TI_CTOI ( dattm2, itarr, ier )
	indx = 1
	DO ii = numclr, 1, -1
	    CALL TI_SUBM ( itarr, itminc(ii), jtarr, ier )
	    CALL TI_ITOC ( jtarr, datgrp(indx), ier )
	    indx = indx + 1
	END DO
C
C*	Get the marker attributes.
C
	mark ( 1 )   = NINT ( ppmark ( 1 ) )
	mark ( 2 )   = 0
	mark ( 3 )   = NINT ( ppmark ( 3 ) )
	szmrkr ( 1 ) = ppmark (2)
C
	mark ( 4 )   = NINT ( pnmark ( 1 ) )
	mark ( 5 )   = 0
	mark ( 6 )   = NINT ( pnmark ( 3 ) )
	szmrkr ( 2 ) = pnmark ( 2 )
C
C*	Set the default values.
C
	IF ( mark ( 1 ) .eq. 0 )  mark ( 1 ) = 1
	IF ( mark ( 3 ) .eq. 0 )  mark ( 3 ) = 1
	IF ( mark ( 4 ) .eq. 0 )  mark ( 4 ) = 22
	IF ( mark ( 6 ) .eq. 0 )  mark ( 6 ) = 1
C
	IF ( szmrkr ( 1 ) .eq. 0. ) szmrkr ( 1 ) = 1.
	IF ( szmrkr ( 2 ) .eq. 0. ) szmrkr ( 2 ) = 1.
C
	CALL GQCOLR ( jcolr, ier )
	CALL GQMRKR ( jmark, jmkhw, szmark, jmkwid, ier )
C
C*	Loop on the data sources.
C
	DO ks = 1, NUMSRC
	    IF ( iflgs ( ks+2 ) .ne. 0 ) THEN
C
C*	        Scan the directory for all of the lightning data files.
C
		CALL ST_NULL ( alias ( ks ), alias ( ks ), na, ier )
		path  = ' '
		templ = ' '
		CALL CTB_DTGET ( alias ( ks ), path, templ,  
     +				 ic, is, if, ir, ii, ion, ihb, mnb,
     +				 iha, mna, mstrct, idtmch, ier )
		CALL ST_RNUL ( path, path, lens, ier )
		CALL ST_RNUL ( templ, templ, lens, ier )
	        CALL ST_LSTR ( path, lenp, ier )
	        CALL FL_SCND ( path, templ, iorder, nexp, files, nfile, 
     +                         ier )
		IF ( ier .eq. 0 ) THEN
C
C*		    This data source exists.  Make the file name for the
C*		    last file requested.
C
	            CALL FL_MNAM ( dattm2, templ, filnam, ier )
C
C*	            Find the earliest file to start searching.
C
	            CALL FL_MNAM ( stime, templ, flstrt, ier )
C
C*	            Decode each file until the end time is reached.
C
	            done = .false.
	            ifl  = 1
	            DO WHILE ( ( ifl .le. nfile ) .and. ( .not. done ) )
	                IF  ( files(ifl) .gt. filnam )  THEN
		            done = .true.
	                  ELSE
		            IF  ( files(ifl) .ge. flstrt )  THEN
		                tfile = path(:lenp) // '/' // files(ifl)
		                CALL FL_DOPN ( tfile, 1, .false., lunf, 
     +					       ier )
C
C*		                Get the time for this data from the file
C*				name.
C
		                CALL FL_MDAT ( files ( ifl ), templ, 
     +				  	       dattm2, datfil, ier )
C
C*				Read and plot the lightning data.
C
				CALL GG_LTRP ( lunf, dattm2, datfil,
     +					       datgrp, ntimes, icolrs,
     +					       mark, szmrkr, iflgs(1),
     +					       iflgs(2), ier )
		            END IF
	                END IF
	                ifl = ifl + 1
	            END DO
C
	        END IF
	    END IF
	END DO
C
C*	Draw the color bar.
C
	CALL GG_CBAR ( '1/V/LR/.995;.05/.5;.01/-1',
     +			numclr, ftminc, icolrs, ier )
C
C*	Reset the saved attributes.
C
	CALL GSCOLR ( jcolr, ier )
	CALL GSMRKR ( jmark, jmkhw, szmark, jmkwid, ier )
C*
	RETURN
	END
