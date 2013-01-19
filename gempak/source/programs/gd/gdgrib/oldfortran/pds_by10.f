	SUBROUTINE PDS_BY10  ( cvcrd, wmotb, nceptb,
     +				byte10, ibyt10, iscale, iret )
C************************************************************************
C* PDS_BY10								*
C*									*
C* This subroutine uses the GEMPAK GRIB vertical coordinate lookup	*
C* tables to determine the value of PDS octet 10.			*
C*									*
C* PDS_BY10  ( CVCRD, WMOTB, NCEPTB, BYTE10, IBYT10, ISCALE, IRET )	*
C*									*
C* Input parameters:							*
C*	CVCRD		CHAR*		GEMPAK VCORD name string	*
C*	WMOTB		CHAR*		WMO GRIB VCORD LUT file name	*
C*	NCEPTB		CHAR*		NCEP GRIB VCORD LUT file name	*
C*									*
C* Output parameters:							*
C*	BYTE10		CHAR*1		Byte with GRIB VCORD # stored	*
C*	IBYT10		INTEGER		Integer value of byte 10	*
C*	ISCALE		INTEGER		Power of 10 scaling in GEMPAK	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-85 = VCORD not found		*
C*					-86 = VCORD # is invalid	*
C**									*
C* Log:									*
C* K. Brill/HPC		 7/99						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	CHARACTER*(*)	cvcrd, wmotb, nceptb
	CHARACTER*1	byte10
C*
	LOGICAL		found
	CHARACTER*256	filnam (2), record
	CHARACTER*4	prmnam, chkprm
C------------------------------------------------------------------------
	iret = 0
	byte10 = CHAR ( 255 )
	ibyt10 = 255
	iscale = 0
C
C*	Check the tables.
C
	CALL ST_LCUC ( cvcrd, chkprm, ier )
	CALL ST_LSTR ( chkprm, ln2, ier )
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
			READ (record,'(I3,56X,A4,X,I5)',IOSTAT=ier)
     +			    iprm, prmnam, iscl
			IF ( ier .eq. 0 ) THEN
			    CALL ST_LSTR ( prmnam, ln1, ier )
			    IF ( prmnam (1:ln1) .eq.
     + 				 chkprm (1:ln2) ) THEN
				found = .true.
				ibyt10 = iprm
				iscale = iscl
			    END IF
			END IF
		    END IF
		END IF
	    END DO
	    CALL FL_CLOS ( luntbl, ier )
	END DO
	IF ( .not. found ) THEN
	    iret = -85
	ELSE IF ( ibyt10 .lt. 255 .and. ibyt10 .gt. 0 ) THEN
	    byte10 = CHAR ( ibyt10 )
	ELSE
	    iret = -86
	END IF
C*
	RETURN
	END
