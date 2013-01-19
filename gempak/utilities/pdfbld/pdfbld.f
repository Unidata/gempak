	PROGRAM PDFBLD
C************************************************************************
C* PDFBLD                                                               *
C*                                                                      *
C* This program makes PDF files from GEMPARM:*.PRM and			*
C* GEMHLP:*.HL1 files.							*
C**                                                                     *
C* Log:                                                                 *
C* I. Graffman/RDS       8/88                                           *
C* G. Huffman/GSC       12/88   Correct error message spacing           *
C* M. desJardins/GSFC    7/89   Set to run from command procedure       *
C* K. Brill/NMC         03/92   Unix version                            *
C* S. Jacobs/EAI         3/93   Changed location of PRM files;		*
C*				  Cleaned up				*
C* S. Jacobs/NCEP	 7/96	Changed environ vars to "$.../" format	*
C* S. Jacobs/NCEP	 1/98	Added check for local input files	*
C* T. Lee/GSC		11/98	Reformatted I/O				*
C* T. Piper/SAIC	02/04	Changed location of help files		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	loc*128, prg*20, pdf*152, prm*72, parm*8
	CHARACTER	uprg*12, tprog*12, newfil*80, ch1*72, help1*72
	LOGICAL		done, exist
C------------------------------------------------------------------------
C*	First, get the arguments from the command line and check that
C*	they are not missing.
C
	narg = iargc ()
	IF  ( narg .lt. 2 )  THEN
	    CALL GETARG  ( 0, tprog )
	    CALL ST_LSTR ( tprog, lentpg, ier )
	    WRITE  ( 6, 1000 ) tprog(:lentpg)
1000	    FORMAT ( ' Usage: ', A, ' output_dir program_name' ) 
	    STOP
	END IF
C
C*	Get PDF location.
C
	CALL GETARG  ( 1, loc )
	CALL ST_LSTR ( loc, lenloc, ier )
	loc (lenloc+1:lenloc+1) = '/'
	lenloc = lenloc + 1
C
C*	Get program name.
C
	CALL GETARG  ( 2, prg )
	CALL ST_LSTR ( prg, lenprg, ier )
	CALL ST_LCUC ( prg, uprg, ier )
C
C*	Make name of output .PDF file.
C
	pdf = loc ( 1:lenloc ) // prg ( 1:lenprg ) // '.pdf'
	WRITE (6,*) 'BUILDING PDF:  ', pdf
C
C*	Open the output file and check for errors.
C
	CALL FL_SUNK  ( pdf, lunpdf, ier )
	IF  ( ier .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'FL', ier, pdf, ier2 )
	    STOP
	END IF
C
C*	Open the input .PRM file.
C
	prm = prg ( 1:lenprg ) // '.prm'
	CALL FL_INQR  ( prm, exist, newfil, ier )
	IF  ( .not. exist )  THEN
	    prm = '$GEMPARM/' // prg ( 1:lenprg ) // '.prm'
	END IF
	CALL FL_SOPN  ( prm, lunprm, ier )
	IF  ( ier .ne. 0 )  THEN
	    WRITE (6,*) 'ERROR OPENING .PRM FILE'
	    STOP
	END IF
C
C*	Loop reading parameters from file.
C
	done = .false.
	DO WHILE  ( .not. done )
	    READ   ( lunprm, 10, IOSTAT = iostat )  parm
10	    FORMAT ( A )
C
C*	    If there is an error, assume this is the end of the file.
C
	    IF  ( iostat .ne. 0 )  THEN
		done = .true.
	      ELSE
		WRITE (6,*) '  PROCESSING PARAMETER: ', parm
C
C*		Read in level 1 help file.
C
		CALL ST_LSTR  ( parm, lenp, ier )
		help1 = parm ( 1:lenp ) // '.hl1'
		CALL FL_INQR  ( help1, exist, newfil, ier )
		IF  ( .not. exist )  THEN
		    help1 = '$GEMHLP/hlx/' // parm ( 1:lenp ) // '.hl1'
		END IF
		CALL FL_SOPN  ( help1, lhlp1, ier )
		IF  ( ier .ne. 0 )  THEN
		    WRITE (6,*) 'NO LEVEL1 HELP FOR: ', parm
		  ELSE
		    READ  ( lhlp1, 10, IOSTAT = iostat )  ch1
		    IF  ( iostat .eq. 0 )  THEN
			WRITE  ( lunpdf, 20 )  parm, ch1
20			FORMAT ( A, A )
		    END IF
		    CALL FL_CLOS  ( lhlp1, ier )
		END IF
	    END IF
	END DO
C*
	CALL FL_CLOS  ( lunprm, ier )
	CALL FL_CLOS  ( lunpdf, ier )
C*
	END
