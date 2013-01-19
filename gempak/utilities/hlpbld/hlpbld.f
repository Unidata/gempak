	PROGRAM HLPBLD
C************************************************************************
C* HLPBLD								*
C*									*
C* This program makes program help files from GEMPARM:*.PRM,		*
C* GEMPTXT:*.TXT, GEMHLP:*.HL1 and GEMERR:*.ERR files.			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 8/88						*
C* G. Huffman/GSC	12/88	Correct error message spacing		*
C* M. desJardins/GSFC	 7/89	Set to run from command procedure	*
C* K. Brill/NMC		03/92	Unix version				*
C* S. Jacobs/EAI	 3/93	Changed location of PRM and TXT files;	*
C*				  Cleaned up				*
C* S. Jacobs/NCEP	 7/96	Changed environ vars to "$.../" format	*
C* S. Jacobs/NCEP	 1/98	Added check for local input files	*
C* T. Lee/GSC		11/98	Changed input format for *.hl1		*
C* T. Piper/SAIC	02/04	Changed location of help files		*
C* T. Piper/SAIC	06/04	Initialized nb, closed files 		*
C************************************************************************
	CHARACTER	hlpfil*180, txtfil*72, loc*128, prog*48, record*80
	CHARACTER	prmfil*72, hl1fil*72, parm*10, out*80, errfil*72
	CHARACTER	rec*80, sp*2, rcrdlc*80, blank*1, proguc*80
	CHARACTER	tprog*12, newfil*80
	LOGICAL		exist
C*
	DATA		sp /'  '/
C------------------------------------------------------------------------
	nb = 0
5	FORMAT (A)
	blank = ' '
C
C*	Get location and program name.
C
	narg = iargc ()
    	IF ( narg .lt. 2 ) THEN
	    CALL GETARG  ( 0, tprog )
	    CALL ST_LSTR ( tprog, lentpg, ier )
	    WRITE  ( 6, 1000 ) tprog(:lentpg)
1000	    FORMAT ( ' Usage: ', A, ' output_dir program_name' )
	    STOP
	END IF
C*
	CALL GETARG ( 1, loc )
	CALL ST_LSTR ( loc, lenloc, ier )
	lenloc = lenloc + 1
	loc ( lenloc:lenloc ) = '/'
C*
	CALL GETARG ( 2, proguc )
	CALL ST_LCUC ( proguc, proguc, ier )
	CALL ST_LSTR ( proguc, isprguc, ier )
	CALL ST_UCLC ( proguc, prog, ier )
	CALL ST_LSTR ( prog, isprog, ier )
C
C*	Construct the help, text, parameter and error file names.
C
	hlpfil = loc (1:lenloc) // prog (1:isprog) // '.hlp'
C
C*	Open the text file.
C
	txtfil = prog (1:isprog) // '.txt'
	CALL FL_INQR  ( txtfil, exist, newfil, ier )
	IF  ( .not. exist )  THEN
	    txtfil = '$GEMPTXT/' // prog (1:isprog) // '.txt'
	END IF
	CALL FL_SOPN  ( txtfil, luntxt, iret )
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'FL', iret, txtfil, ier )
	    STOP
	END IF
C
C*	Create the help file.
C
	CALL FL_SWOP  ( hlpfil, lunhlp, iret )
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'FL', iret, hlpfil, ier )
	    STOP
	END IF
C
C*	Find the first non blank record.
C
	CALL NOBLNK  ( luntxt, record, ier )
	mode = 0
	CALL ST_LSTR ( record, lrecl, ier )
	WRITE  ( lunhlp, 5 )  record (1:lrecl)
C
C*	Begin main loop.
C
	DO WHILE  ( .true. )
	    READ  ( luntxt, 5, END = 100 )  record
C
C*	    Look for program description or examples.  Write 2 
C*	    blanks, heading and get first non blank record.
C
	    IF ((INDEX (record, 'PROGRAM DESCRIPTION') .gt. 0) .or.
     +          (INDEX (record, 'EXAMPLES') .gt. 0)) THEN
	        DO i = 2, nb+1, -1
	            WRITE (lunhlp, 5) blank
	        END DO
		CALL ST_LSTR ( record, lrecl, ier )
		WRITE (lunhlp, 5) record (1:lrecl)
		WRITE (lunhlp, 5) blank
		CALL NOBLNK (luntxt, record, ier)
		CALL ST_LSTR ( record, lrecl, ier )
		WRITE (lunhlp, 5) record (1:lrecl)
		mode = 2
		nb = 0
C
C*	        Error messages.
C
	      ELSE IF (INDEX (record, 'ERROR MESSAGES') .gt. 0) THEN
C
C*	        Open error file
C
		CALL ST_LSTR ( record, lrecl, ier )
		WRITE (lunhlp, 5) record (1:lrecl)
		WRITE (lunhlp, 5) blank
		errfil = prog (1:isprog) // '.err'
		CALL FL_INQR  ( errfil, exist, newfil, ier )
		IF  ( .not. exist )  THEN
		    errfil = '$GEMERR/' // prog (1:isprog) // '.err'
		END IF
	        CALL FL_SOPN (errfil, lunerr, iret)
C
C*	        Find first non comment.
C
	        record (1:1) = '!'
	        DO WHILE ((record (1:1) .eq. '!') .or.
     +	                  (record .eq. ' '))
	            READ (lunerr, 5) record
	        END DO
	        BACKSPACE (lunerr)
C
C*              Leading spaces squeezed out first so that the tab
C*		expansion will compensate, keeping the message text
C*              in the same place.
C
		DO WHILE (.true.)
		    READ (lunerr, 5, end = 35) record
		    CALL ST_LDSP (record, record, n, ier)
		    CALL ST_UTAB (record, 80, rec, ier)
		    isp = INDEX (rec, '!')
		    IF (isp .gt. 0) rec (isp:isp) = ' '
		    isp = INDEX (rec, '!AS')
		    IF (isp .gt. 0) rec (isp:isp+2) = '...'
		    isp = INDEX (rec, ' ')
		    CALL ST_NUMB (rec (1:isp-1), ierr, ier)
		    IF (ABS (ierr) .gt. 9) THEN
		        nsp = 1
		      ELSE
		        nsp = 2
		    END IF
		    out = '    [' // proguc (1:isprguc) // sp (1:nsp) //
     +                     rec (1:isp-1) // ']' // rec (isprguc+1:)
		    CALL ST_LSTR ( out, lrecl, ier )
		    WRITE (lunhlp, 5) out (1:lrecl)
		END DO
   35		CONTINUE
		CALL FL_CLOS(lunerr, ier)
C
C*		Input parameters.
C
	      ELSE IF (INDEX (record, 'INPUT PARAMETERS') .gt. 0) THEN
C
C*		Open parameter file.
C
		prmfil = prog (1:isprog) // '.prm'
		CALL FL_INQR  ( prmfil, exist, newfil, ier )
		IF  ( .not. exist )  THEN
		    prmfil = '$GEMPARM/' // prog (1:isprog) // '.prm'
		END IF
		CALL FL_SOPN (prmfil, lunprm, iret)
		IF (iret .ne. 0) THEN
		    WRITE (lunhlp, 30)
   30	            FORMAT ('Parameter file not found')
		     GO TO 100
		END IF
C
C*		Loop through parameters. Create help 1 file name.
C
		CALL ST_LSTR ( record, lrecl, ier )
	        WRITE (lunhlp, 5) record (1:lrecl)
	        WRITE (lunhlp, 5) blank
		nprm = 0
	        DO WHILE (.true.)
	            READ (lunprm, 5, end = 50) record
		    nprm = nprm + 1
	            CALL ST_LSTR (record, isiz, ier)
		    CALL ST_UCLC (record, rcrdlc, ier )
	            hl1fil = rcrdlc (1:isiz) // '.hl1'
		    CALL FL_INQR  ( hl1fil, exist, newfil, ier )
		    IF  ( .not. exist )  THEN
			hl1fil = '$GEMHLP/hlx/' // rcrdlc (1:isiz)//'.hl1'
		    END IF
	            CALL FL_SOPN (hl1fil, lunhl1, iret)
	            parm = record (1:isiz)	                
	            IF (iret .ne. 0) THEN
	                WRITE (lunhlp, 40) parm
   40	                FORMAT ('Parameter ', a, 'not found')
C
C*	                Get past first records to blank.
	              ELSE
C
C*	                Read the record.
C
			READ (lunhl1, 5, end = 45) record
			out = '    ' // parm // record
			CALL ST_LSTR ( out, lrecl, ier )
	                WRITE (lunhlp, 5) out (1:lrecl)
	            END IF
   45	            CONTINUE
	            CALL FL_CLOS (lunhl1, ier)
	        END DO
   50	        CONTINUE
		CALL FL_CLOS(lunprm, ier)
		IF  ( nprm .eq. 0 )  THEN
		    WRITE  ( lunhlp, 55 )
   55		    FORMAT ( '    None' )
		END IF
	        WRITE (lunhlp, 5) blank
	        WRITE (lunhlp, 5) blank
C
C*	   Else if already found, just write.
C
	     ELSE 
	       IF (record .eq. ' ') THEN
	           nb = nb + 1
	           IF (nb .gt. 2) THEN
	               nb = 2
	             ELSE
		       CALL ST_LSTR ( record, lrecl, ier )
	               WRITE (lunhlp, 5) record (1:lrecl)
	           END IF
	         ELSE
	           nb = 0
		   CALL ST_LSTR ( record, lrecl, ier )
	           WRITE (lunhlp, 5) record (1:lrecl)
	       END IF
	    END IF
	END DO
100	CONTINUE
	CALL FL_CLOS(lunhlp, ier)
	CALL FL_CLOS(luntxt, ier)
C
	END
C
C------------------------------------------------------------------------
C* This subroutine returns the first non blank record.
C
	SUBROUTINE NOBLNK (lun, record, ier)
	CHARACTER* (*)	record
C
	DO WHILE (.true.)
	    READ (lun, 5, end = 10) record
	    IF (record .ne. ' ') RETURN
	END DO
    5	FORMAT (a)
   10	CONTINUE
C
	RETURN
	END
