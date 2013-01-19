	SUBROUTINE PDS_BYT9  ( parm, wmotb, nceptb, byte9, ibyt9,
     +                         idt, iret )
C************************************************************************
C* PDS_BYT9								*
C*									*
C* This subroutine uses the GEMPAK GRIB parameter lookup tables to	*
C* determine the value of PDS octet 9.					*
C*									*
C* PDS_BYT9  ( PARM, WMOTB, NCEPTB, BYTE9, IBYT9, IDT, IRET )		*
C*									*
C* Input parameters:							*
C*	PARM		CHAR*		GEMPAK parameter name string	*
C*	WMOTB		CHAR*		WMO GRIB parm LUT file name	*
C*	NCEPTB		CHAR*		NCEP GRIB parm LUT file name	*
C*									*
C* Output parameters:							*
C*	BYTE9		CHAR*1		Byte with GRIB parm # stored	*
C*	IBYT9		INTEGER		Integer value of byte 9		*
C*	IDT		INTEGER		Value of any imbedded integer	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-83 = parm not found		*
C*					-84 = parm # not valid in GRIB	*
C*					-94 = parm name is too long	*
C**									*
C* Log:									*
C* K. Brill/HPC		 8/99						*
C* K. Brill/HPC		 3/00	Avoid character assignment to itself	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	CHARACTER*(*)	parm, wmotb, nceptb
	CHARACTER*1	byte9
C*
	LOGICAL		found
	CHARACTER*256	filnam (2), record
	CHARACTER*16	prmnam, chkprm, cinprm, cnum
	CHARACTER*1	cdum
C------------------------------------------------------------------------
	iret = 0
	idt = -9999
	byte9 = CHAR ( 255 )
	ibyt9 = 255
	CALL ST_LSTR ( parm, ln2, ier )
	IF ( ln2 .gt. 16 ) THEN
	    iret = -94
	    RETURN
	END IF
	CALL ST_LCUC ( parm, cinprm, ier )
	filnam (1) = wmotb
	filnam (2) = nceptb
	found = .false.
	ifile = 0
	DO WHILE ( ifile .lt. 2 .and. .not. found )
	    ifile = ifile + 1
	    IF ( filnam (ifile) .ne. ' ' ) THEN
		CALL FL_TBOP ( filnam (ifile), 'grid', luntbl, ios )
	    ELSE
		ios = -99
	    END IF
	    DO WHILE ( ios .eq. 0 .and. .not. found )
		READ (luntbl,'(A)',IOSTAT=ios) record
		IF ( ios .eq. 0 ) THEN
		    IF ( record (1:1) .ne. '!' ) THEN
			READ (record,'(I3,56X,A12)',IOSTAT=ier)
     +			    iprm, prmnam
			IF ( ier .eq. 0 ) THEN
			    cnum = ' '
			    chkprm = cinprm
			    CALL ST_LSTR ( prmnam, ln1, ier )
			    ifnd1 = INDEX ( chkprm, prmnam (1:ln1) )
			    IF ( prmnam (1:ln1) .eq.
     + 				 chkprm (1:ln2) ) THEN
				found = .true.
				ibyt9 = iprm
			    ELSE IF ( ifnd1 .eq. 1 ) THEN
				cnum = chkprm (ln1+1:ln2)
				CALL ST_NUMB ( cnum, idt, ier )
				found = ( ier .eq. 0 )
				IF ( found ) ibyt9 = iprm
			    ELSE
				idash = INDEX ( prmnam, '-' )
				IF ( idash .ne. 0 ) THEN
				    nnums = 0
				    kchr = 0
				    DO ic = 1, ln2
					CALL ST_ALNM ( chkprm (ic:ic),
     +						       ityp, ier )
					IF ( ityp .eq. 1 ) THEN
					    nnums = nnums + 1
					    cnum (nnums:nnums) =
     +							chkprm (ic:ic)
					    IF ( nnums .le. 2 ) THEN
						kchr = kchr + 1
						chkprm (kchr:kchr) =
     +								'-'
					    END IF
					ELSE
					    kchr = kchr + 1
					    cdum =
     +						chkprm (ic:ic)
					    chkprm (kchr:kchr) =
     +						cdum
					END IF
				    END DO
				    IF ( nnums .gt. 0 ) THEN
					found = ( chkprm (1:kchr)
     +					          .eq. prmnam (1:ln1) )
					IF ( found ) THEN
					    CALL ST_NUMB ( cnum, idt,
     +							   ier )
					    ibyt9 = iprm
					END IF
				    END IF
				END IF
			    END IF
			END IF
		    END IF
		END IF
	    END DO
	    CALL FL_CLOS ( luntbl, ier )
	END DO
	IF ( .not. found ) THEN
	    iret = -83
	ELSE IF ( ibyt 9 .lt. 255 .and. ibyt9 .gt. 0 ) THEN
	    byte9 = CHAR ( ibyt9 )
	ELSE
	    iret = -84
	END IF
C*
	RETURN
	END
