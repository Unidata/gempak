	SUBROUTINE MAP_GSFSSF ( infil, outfil, iret )
C************************************************************************
C* PROGRAM GSFSSF	                                                *
C*                       	                                        *
C* This program converts a map file in the GEMPAK Standard Format	*
C* (GSF) to the Seqential Standard Format (SSF), which is an ASCII	*
C* file.  The formats of these files are documented in the GEMPAK	*
C* map files document.							*
C**	                                                                *
C* Log:		                                                        *
C* I.Graffman/RDS	 9/85						*
C* M. desJardins/GSFC	 4/86	Corrected				*
C* G. Huffman/GSC	12/88	GEMPAK4.1 upgrade; FL, remove BYTE, doc	*
C* M. desJardins/GSFC	 3/89	Recoded and rewrote documentation	*
C* G. Krueger/EAI	 1/00	Implemented file byte swapping		*
C* S. Jacobs/NCEP	 2/01	Added machine type MTLNUX		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( MAPLEN = 128 )
C*
	CHARACTER*(*) 	infil, outfil
C*
	INTEGER		imapbf ( 256 )
	REAL		rmapbf ( 256 )
	INTEGER*2	imapb2 ( 512 )
C*
	EQUIVALENCE	( imapbf , rmapbf )
	EQUIVALENCE	( imapb2 , imapbf )
C----------------------------------------------------------------------
	iret = 0
C
C*      Open input GSF map file.
C
	CALL FL_DOPN  ( infil, MAPLEN, .false., lunin, ier )
	IF  ( ier .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'FL', ier, infil, ier1 )
	    STOP
	END IF
C
C*      Create output SSF file.
C
	CALL FL_SWOP  ( outfil, lunout, ier )
	IF  ( ier .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'FL', ier, outfil, ier1 )
	    CALL FL_CLOS  ( lunin, ier )
	    STOP
	END IF
C
C*	Read header block from input map file.  If necessary, swap
C*	bytes for the output file.
C
	CALL FL_READ  ( lunin, 1, MAPLEN, imapbf (  1), ier1 )
	CALL FL_READ  ( lunin, 2, MAPLEN, imapbf (MAPLEN+1), ier2 )
	IF ( MTMACH .eq. MTIGPH .or. MTMACH .eq. MTULTX .or.
     +	     MTMACH .eq. MTALPH .or. MTMACH .eq. MTLNUX )
     +		CALL MV_SWP2 ( 4 * MAPLEN, imapb2, imapb2 )
	IF  ( ( ier1 .ne. 0 ) .or. ( ier2 .ne. 0 ) )  THEN
 	    CALL FL_CLOS  ( lunin,  ier )
	    CALL FL_CLOS  ( lunout, ier )
	    CALL ER_WMSG  ( 'FL', ier1, infil, ier )
	    CALL ER_WMSG  ( 'FL', ier2, infil, ier )
	    STOP
	END IF
C
C*	Get number of blocks in files.  Note that this is an INTEGER*2
C*	word.
C
	nmblk = imapb2 ( 1 )
	WRITE  ( 6, 4000 )  nmblk
4000   FORMAT  ( ' NMBLK = ', I10 )
C
C*      Read entire input map file.
C
	irec = 2
	iblk = 2
	DO  WHILE ( iblk .le. nmblk )
C
C*	    Increment block counter for next test and read in next 
C*	    block of data.  If necessary, swap bytes for the output
C*	    file.
C
	    iblk = iblk + 1
	    irec = irec + 1   
	    CALL FL_READ  ( lunin, irec, MAPLEN, imapbf (  1), ier1 )
	    irec = irec + 1   
	    CALL FL_READ  ( lunin, irec, MAPLEN, imapbf (MAPLEN+1),
     +			    ier2 )
	    IF ( MTMACH .eq. MTIGPH .or. MTMACH .eq. MTULTX .or.
     +		 MTMACH .eq. MTALPH .or. MTMACH .eq. MTLNUX )
     +		CALL MV_SWP4 ( 2 * MAPLEN, imapbf, imapbf )
	    IF  ( ( ier1 .ne. 0 ) .or. ( ier2 .ne. 0 ) )  THEN
 	        CALL FL_CLOS  ( lunin,  ier )
	        CALL FL_CLOS  ( lunout, ier )
	        CALL ER_WMSG  ( 'FL', ier1, infil, ier )
	        CALL ER_WMSG  ( 'FL', ier2, infil, ier )
	        STOP
	    END IF
C
C*	    Pick up number of segments in current GSF block and write 
C*	    them out to SSF file.
C
	    ipt   = 1
	    nmseg = imapbf ( ipt )
	    ipt   = ipt + 2
C
	    DO  ir = 1, nmseg
	        np   = imapbf ( ipt )
C
C*		Check for the case (only in old files) where the I*2
C*		word following np has a value of 1.
C
		IF  ( np .gt. 16000 ) np = MOD ( np, 65536 )
	        np2  = 2 * np
		ipt  = ipt + 1
 	        WRITE  ( lunout, 1030 )  np2, rmapbf (ipt+2),
     +					 rmapbf (ipt), rmapbf (ipt+3),
     +					 rmapbf (ipt+1),
     +			( rmapbf (i), i = ipt + 5, ipt + 4 + np2 )
1030		FORMAT ( I4, 14X, 6F9.3, 8X / (8F9.3, 8X ) )
		ipt  = ipt + 5 + np2
	    END DO
	END DO
C
C*      Close input and output files.
C
	CALL FL_CLOS  ( lunin,  ier )
	CALL FL_CLOS  ( lunout, ier )
C*
        STOP
	END
