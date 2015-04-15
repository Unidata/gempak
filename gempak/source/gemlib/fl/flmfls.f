	 SUBROUTINE FL_MFLS  (  filtyp, dattim, cycle, maxfls, 
     +				filnms, nfiles, templt, iret )
C************************************************************************
C* FL_MFLS								*
C* 									*
C* This subroutine builds and searches for a path and filename(s) from  *
C* a type of data file (filtyp) such as surface and upper air 		*
C* observations and numerical model grids.  If a single file is to be	*
C* found, the file name is queried in the following order:		*
C*									*
C*		1. Locally  for all files matching the pattern		*
C*		2. Remotely for all files matching the pattern		*
C*									*
C* If cycle is '*', then all files under the directory path for filtyp	*
C* will be returned in the filnms array.				*
C* 									*
C* If the type of data file is not found, the input for filtyp is 	*
C* assumed to be an actual file name which is returned in filnms(1) 	*
C* with nfiles = 1.							*
C*									*
C* FL_MFLS  ( FILTYP, DATTIM, CYCLE, MAXFLS, FILNMS, NFILES, TEMPLT,	*
C*	      IRET )							*
C* 									*
C* Input parameters:							*
C*	FILTYP		CHAR*		File name type			*
C*	DATTIM		CHAR*		Full GEMPAK date/time		*
C*	CYCLE		CHAR*		Data cycle			*
C*	MAXFLS		INTEGER		Maximum number of files allowed	*
C*									*
C* Output parameters:							*
C*	FILNMS(MAXFLS)	CHAR*		File name(s)			*
C*	NFILES		INTEGER		Number of files returned	*
C*	TEMPLT		CHAR*		File name template, if applcble	*
C*	IRET		INTEGER		Return code			*
C*					    0 = normal return		*
C*					  -11 = no file for time range	*
C*					  -13 = no file for given type	*
C**									*
C* Log:									*
C* D. Keiser/GSC	 8/96						*
C* K. Tyle/GSC		 9/96	Increased tstr to 40 chars		*
C* D.W.Plummer/NCEP	11/96	Started with FL_MFIL; Added directory 	*
C*				processing				*
C* S. Maxwell/GSC	 1/97	Changed labels in call to FL_TBOP	*
C* W. Li/EAi		11/97	Changed datchr.tbl to data.tbl		*
C* D.W.Plummer/NCEP	 1/98	Added processing for grid dattim and	*
C*				forecast hour processing		*
C* D.W.Plummer/NCEP	 8/98	Modified logic for dattim = ' ';	*
C*				now looks back 1 days worth of cycles	*
C* D.W.Plummer/NCEP	 9/98	Added check against maxfls             	*
C* S. Jacobs/NCEP	 9/98	Increased the size of the path variable	*
C* T. Piper/GSC         11/98   Updated prolog                          *
C* D.W.Plummer/NCEP	 1/99	Changes for multiple fcst hr files	*
C* T. Lee/GSC		 2/99	Fixed multiple fcst hr files bug	*
C* D.W.Plummer/NCEP	 2/99	Fixed multiple fcst hr files bug	*
C* D. Kidwell/NCEP	 2/99	Allow 2- or 4- digit year               *
C* T. Piper/GSC		 3/99	Corrected prolog			*
C* T. Lee/GSC		 7/99	Implemented cylce			*
C* T. Lee/GSC		 8/99	Removed extra spaces in template	*
C* S. Jacobs/NCEP	 8/99	Changed call to CFL_SCND for sort order	*
C* S. Jacobs/NCEP	 8/99	Changed call to FL_TMPL			*
C* S. Jacobs/NCEP	12/99	Removed extra spaces in template (again)*
C* D.W.Plummer/NCEP	 4/00	Changes for multiple fcst hr files	*
C* M. Li/GSC		 5/00	Expanded filcyc and filnam		*
C* S. Jacobs/NCEP	 3/01	Increased file template to 48 chars	*
C* S. Jacobs/NCEP	 7/01	Added check for minutes in the template	*
C* B. Yin/SAIC           3/04   Changed SS_GTIM to CSS_GTIM             *
C* T. Lee/SAIC		 9/04	Replaced FL_TMPL with CTB_DTGET		*
C* T. Lee/SAIC		10/04	Fixed return code			*
C* A. Hardy/NCEP	11/04   Added calls to ST_RNUL			*
C* m.gamazaychikov/SAIC 12/04   Added ion flag to CTB_DTGET CS          *
C* m.gamazaychikov/SAIC 01/06   Changed tmplt string length to MXTMPL   *
C* m.gamazaychikov/SAIC 04/06   Added idtmch flag to CTB_DTGET CS       *
C* F. J. Yen/NCEP	 4/08	Added bin mins & mstrct to CTB_DTGET CSC*
C* S. Jacobs/NCEP	 8/14	Added support for FFFFF template	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	filtyp, filnms(maxfls), dattim, templt, cycle
	CHARACTER       filnam*(MXNMFL*MXFLSZ), sep*1
	CHARACTER       rymdhn*10, rymdh*8, rymd*6
	CHARACTER	ryymdhn*12, ryymdh*10, ryymd*8
	CHARACTER       path*25, tmplt*(MXTMPL), gtime*20, filcyc*40, 
     +      		ndttm*20
	CHARACTER	cfhr5*5, cfhr3*3, cfhr2*2, cent*2,
     +			fnull*(MXFLSZ)
	INTEGER		rngtyp, itype
	LOGICAL		found, wild, ftmplt, timchr
C------------------------------------------------------------------------
	iret = 0
C
C*	Check filename length
C
	CALL ST_LSTR ( filtyp, lf, ier )
	IF ( lf .eq. 0 )  THEN
	    iret = -13
	    RETURN
	END IF
C
C*	Get template information.
C
	found = .true.
	CALL ST_NULL ( filtyp, fnull, nf, ier )
	path = ' '
	tmplt = ' '
	CALL CTB_DTGET ( fnull, path, tmplt,
     +			 ic, is, if, ir, ii, ion, ihb, mnb, iha, mna,
     +			 mstrct, idtmch,ier)
	IF ( ier .ne. 0 )  found = .false. 
	CALL ST_RNUL ( path, path, lens, ier )
	CALL ST_RNUL ( tmplt, tmplt, lens, ier )
C
	IF  ( .not. found )  THEN
C
C*	    If a match was not found, set input file type to filnms.
C
	    filnms(1) = filtyp
	    nfiles = 1
	    templt = ' '
C
	ELSE
C
	    templt = tmplt
	    filcyc = filtyp
	    wild   = .false.
C
C*	    Determine the type of template and DATTIM.
C
	    CALL ST_LCUC ( dattim, ndttm, ier )
	    CALL TG_QRNG ( ndttm, rngtyp, timchr, iret )
	    ftmplt = INDEX ( tmplt, 'fFF' ) .ne. 0 
	    wild   = cycle .eq. '*'
C
C	    IF ( cycle .eq. ' ' )  cycle = dattim(:11)
C
C*	    Single file retrieval.
C
	    IF  ( ( .not. wild ) .and. ( .not. ftmplt ) .and.
     +		  ( rngtyp .ne. 2 ) )   THEN
C
C*		Normal time or simple forecast sequence.
C
		itype = 1
		CALL CSS_GTIM ( itype, gtime, ier )
		IF  ( .not. timchr )  THEN
		    CALL TI_STAN ( ndttm, gtime, gtime, ier )
		END IF
C
		IF  ( INDEX ( tmplt, 'NN' ) .eq. 0 )  THEN
		    gtime (10:11) = '00'
		END IF
C
C*		Combine cycle information with file type.
C
		IF  ( cycle .ne. ' ' )  THEN
		    CALL ST_LSTR ( filtyp, ntype, ier )
		    filcyc = filtyp ( :ntype ) // '|' // cycle
		END IF
C
C*		Find the most recent cycle file.
C
		CALL FL_MFIL ( filcyc, gtime, filnms (1), iret )
C
		IF  ( iret .ne. 0 )  THEN
		    filnms (1) = ' '
		    nfiles = 0
		    iret   = -11
		  ELSE
		    nfiles  = 1
		END IF
C
	      ELSE
C
C*	        Make a C call to get all the file names in the path 
C*		directory. Don't bother to check locally first.
C
                rymdh   = '????????'
                rymdhn  = '??????????'
                rymd    = '??????'
                ryymdh  = '??????????'
                ryymdhn = '????????????'
                ryymd   = '????????'
                cfhr2   = '??'
                cfhr3   = '???'
                cfhr5   = '?????'
C
C*		Add cycle time to the file alias.  If range type is 2 
C*		or cycle is '*', do nothing.
C
		IF  ( wild .or. ( rngtyp .eq. 2 ) )  THEN
C
		  ELSE
C
		    indxf = INDEX ( ndttm, 'F' )
		    CALL ST_LSTR ( ndttm, lenn, iret )
		    lenf = lenn - indxf
		    IF ( indxf .ne. 0 )  THEN
			IF ( lenf .eq. 5 )  THEN
 		          cfhr5  = ndttm(indxf+1:indxf+5)
 		          cfhr3  = ndttm(indxf+1:indxf+3)
 		          IF ( ndttm(indxf+3:indxf+3) .eq. ' ' )  THEN
       			    cfhr2  = ndttm(indxf+1:indxf+2)
 		          ELSE
       			    cfhr2  = ndttm(indxf+2:indxf+3)
 		          END IF
			ELSE IF ( lenf .eq. 3 )  THEN
 		          cfhr3  = ndttm(indxf+1:indxf+3)
			  cfhr5  = cfhr3 // '00'
 		          IF ( ndttm(indxf+3:indxf+3) .eq. ' ' )  THEN
       			    cfhr2  = ndttm(indxf+1:indxf+2)
 		          ELSE
       			    cfhr2  = ndttm(indxf+2:indxf+3)
 		          END IF
			ELSE IF ( lenf .eq. 2 )  THEN
       			  cfhr2  = ndttm(indxf+1:indxf+2)
       			  cfhr3  = '0' // cfhr2
       			  cfhr5  = '0' // cfhr2 // '00'
			END IF
			IF ( INDEX ( ndttm, 'FALL' ) .ne. 0 )  THEN
       			  cfhr2  = '??'
       			  cfhr3  = '???'
       			  cfhr5  = '?????'
			END IF
 		    END IF
C
		    IF  ( timchr .or. ndttm .eq. ' ' ) 
     +			  ndttm (1:9) = '?????????'
		    CALL TI_STAN ( cycle, ndttm, ndttm, ier )
		    ndttm (10:11) = '00'
C
		    rymdh = ndttm(1:6) // ndttm(8:9)
		    rymd  = ndttm(1:6)
		    CALL TI_CCNT ( ndttm, cent, iret )
		    IF ( cent .eq. ' ' )  cent = '??'
		    ryymdh = cent // rymdh
		    ryymd  = cent // rymd
C
		END IF
C
		jpos = INDEX ( tmplt, 'YYYYMMDDHHNN' )
		IF  ( jpos .ne. 0 )  THEN
		    CALL ST_RPST ( tmplt, 'YYYYMMDDHHNN', ryymdhn,
     +				   ipos, tmplt, iret )
C
		ELSE IF  ( INDEX ( tmplt, 'YYYYMMDDHH' ) .ne. 0 )  THEN
		    CALL ST_RPST ( tmplt, 'YYYYMMDDHH', ryymdh, ipos, 
     +				   tmplt, iret )
C
		  ELSE IF ( INDEX ( tmplt, 'YYYYMMDD' ) .ne. 0 )  THEN
		    CALL ST_RPST  ( tmplt, 'YYYYMMDD', ryymd, ipos, 
     +				    tmplt, iret )
C
		  ELSE IF ( INDEX ( tmplt, 'YYMMDDHHNN' ) .ne. 0 ) THEN
		    CALL ST_RPST  ( tmplt, 'YYMMDDHHNN', rymdhn, ipos, 
     +				    tmplt, iret )
C
		  ELSE IF ( INDEX ( tmplt, 'YYMMDDHH' ) .ne. 0 )  THEN
		    CALL ST_RPST  ( tmplt, 'YYMMDDHH', rymdh, ipos, 
     +				    tmplt, iret )
C
		  ELSE IF ( INDEX ( tmplt, 'YYMMDD' ) .ne. 0 )  THEN
		    CALL ST_RPST  ( tmplt, 'YYMMDD', rymd, ipos, 
     +				    tmplt, iret )
		END IF
C
		kpos = INDEX ( tmplt, 'FFFFFF' )
		IF  ( kpos .ne. 0 )  THEN
		    CALL ST_RPST  ( tmplt, 'FFFFFF', 'F' // cfhr5,
     +				    ipos, tmplt, iret )
C
		  ELSE IF ( INDEX ( tmplt, 'fFFFFF' ) .ne. 0 )  THEN
		    CALL ST_RPST  ( tmplt, 'fFFFFF', 'f' // cfhr5,
     +				    ipos, tmplt, iret )
C
		  ELSE IF ( INDEX ( tmplt, 'FFFF' ) .ne. 0 )  THEN
		    CALL ST_RPST  ( tmplt, 'FFFF', 'F' // cfhr3,
     +				    ipos, tmplt, iret )
C
		  ELSE IF ( INDEX ( tmplt, 'fFFF' ) .ne. 0 )  THEN
		    CALL ST_RPST  ( tmplt, 'fFFF', 'f' // cfhr3,
     +				    ipos, tmplt, iret )
C
		  ELSE IF ( INDEX ( tmplt, 'FFF' ) .ne. 0 )  THEN 
		    CALL ST_RPST  ( tmplt, 'FFF', 'F' // cfhr2,
     +				    ipos, tmplt, iret )
C
		  ELSE IF ( INDEX ( tmplt, 'fFF' ) .ne. 0 )  THEN	
		    CALL ST_RPST  ( tmplt, 'fFF', 'f' // cfhr2,
     +				    ipos, tmplt, iret )
		END IF 
C
		CALL ST_LSTR ( path, ilenp, ier )
		CALL ST_LSTR ( tmplt, ilent, ier )
		ilenf = 0
		sep = ';'
		maxlen = MXNMFL * MXFLSZ 
		isort = 1
		CALL CFL_SCND ( path(:ilenp), ilenp, tmplt(:ilent), 
     +				ilent, sep, maxlen, isort, filnam,
     +				ilenf, nf, iret )
C
C*		Parse long string.
C
		nfiles = 0
		IF ( iret .eq. 0 .and. nf .gt. 0 )  THEN
		    filnam(ilenf+1:) = ' '
		    n = 1
		    DO WHILE ( n .le. nf .and. nfiles .lt. maxfls )
			CALL ST_CLST  ( filnam(:ilenf), sep, ' ', 1,
     +					filnms(n), nx, ier )
			CALL ST_LSTR  ( filnms(n), ilenx, ier )
			filnms(n) = path(:ilenp) // '/' // 
     +				    filnms(n)(:ilenx)
			filnam = filnam(ilenx+2:)
			CALL ST_LSTR ( filnam, ilenf, ier )
			nfiles = nfiles + 1
			n = n + 1
		    END DO
		ELSE
		    iret = -11
		END IF
C
	    END IF
C
	END IF
C*
	RETURN
	END
