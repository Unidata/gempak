	SUBROUTINE SF_STNF  ( isffln, tbfile, iret )
C************************************************************************
C* SF_STNF								*
C*									*
C* This subroutine adds stations from a table file to a surface file.	*
C* This subroutine can only be used if the times and stations are	*
C* not mixed in row and column headers.					*
C*									*
C* SF_STNF  ( ISFFLN, TBFILE, IRET )					*
C*									*
C* Input parameters:							*
C*	ISFFLN		INTEGER		Surface file number		*
C*	TBFILE		CHAR*		Station table file name		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C*					  -3 = file not open		*
C*					  -5 = no more space 		*
C*					 -16 = station file not opened	*
C*					 -19 = non-standard file	*
C**									*
C* Log:									*
C* I. Graffman/RDS	 6/87						*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* K. Brill/NMC		 8/93	Added ISPRI			 	*
C* D. Keiser/GSC	12/95	Changed FL_TOPN to FL_TBOP		*
C* L. Sager/NCEP         6/96   Changed calling sequence for TB_RSTN    *
C*                              to add character string parameter       *
C* S. Jacobs/NCEP	12/99	Changed size of tbchrs 14->20		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
C*
	CHARACTER*(*)	tbfile
C*
	CHARACTER	cid (LLSTFL)*8, state (LLSTFL)*2
	CHARACTER	name*32, coun (LLSTFL)*2, tbchrs (LLSTFL)*20
	INTEGER		id (LLSTFL), ispri (LLSTFL)
	REAL		rlat (LLSTFL), rlon (LLSTFL), elev (LLSTFL)
C-----------------------------------------------------------------------
	CALL SF_CHKF  ( isffln, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Open the table file.
C
	CALL FL_TBOP  ( tbfile, 'stns', lunstn, ier )
	IF  ( ier .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'FL', ier, tbfile, iret )
	    iret = -16
	    RETURN
	END IF
C
C*	Read the stations.
C
	iout = 0
	ns   = 1
	DO WHILE  ( iout .eq. 0 )
	    CALL TB_RSTN  ( lunstn, cid (ns), name, id (ns), state (ns),
     +			    coun (ns), rlat (ns), rlon (ns), elev (ns),
     +			    ispri (ns), tbchrs (ns), iout )
	    ns = ns + 1
	    IF  ( ns .gt. LLSTFL )  THEN
		iout = 1
		ns   = ns + 1
	    END IF
	END DO
	ns = ns - 2
C
C*	Add stations to the file.
C
	CALL SF_ASTN  ( isffln, ns, cid, id, rlat, rlon, elev,
     +			state, coun, ispri, nadd, iret )
C*
	RETURN
	END
