	SUBROUTINE MN_DECD ( report, istart, iprms, rdata, iret ) 
C************************************************************************
C* MN_DECD								*
C*									*
C* This subroutine decodes the data contained in an NGM MOS report.	*
C*									*
C* MN_DECD ( REPORT, ISTART, IPRMS, RDATA, IRET )			*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		NGM MOS report			*
C*	ISTART (*)	INTEGER		Indicies of line beginnings	*
C*	IPRMS (*)	INTEGER		Position of parameter in list	*
C*									*
C* Output parameters:							*
C*	RDATA		REAL		Forecast data			*
C*	(FCSTTM,MMPARM)							*
C*	IRET		INTEGER		Return Code			*
C**									*
C* Log:									*
C* D. Keiser/GSC	 2/96						*
C* D. Keiser/GSC	 5/96	Declare FCSTTM an integer	 	*
C* F. J. Yen/NCEP	11/98	Converted for restructuring of DC_MNDC	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INTEGER		FCSTTM
	PARAMETER       ( FCSTTM = 19 )
C*
	CHARACTER*(*)	report
	INTEGER		istart(*), iprms(*)
	REAL		rdata(FCSTTM,MMPARM)
C------------------------------------------------------------------------
	iret = 0
C
C*	Initialize all forecast data to missing.
C
	DO i = 1, FCSTTM 
	    DO j = 1, MMPARM
		rdata (i,j) = RMISSD
	    END DO
	END DO
C
C*	Decode each line of the NGM MOS report. Before decoding the
C*	extrema, check to see which order they appear, according to
C*	what time the model was issued.
C 
C*	If istart(3) is not zero then, MN/MX (12z), otherwise
C*	MX/MN (00z)
C
	IF ( istart(3) .ne. 0 ) THEN
	    CALL MN_NTDY ( report, istart(3), iprms(1), iprms(2),
     +			   rdata, iret )
	ELSE
	    CALL MN_NTDY ( report, istart(4), iprms(2), iprms(1),
     +			   rdata, iret ) 
	END IF
	CALL MN_3CHR ( report, istart(5),  iprms(3),  rdata, iret )
	CALL MN_3CHR ( report, istart(6),  iprms(4),  rdata, iret )
	CALL MN_3CHC ( report, istart(7),  iprms(5),  rdata, iret )
	CALL MN_3CHR ( report, istart(9),  iprms(6),  rdata, iret )
	CALL MN_3CHR ( report, istart(8),  iprms(7),  rdata, iret )
	CALL MN_3CHR ( report, istart(10), iprms(8),  rdata, iret )
	CALL MN_3CHR ( report, istart(11), iprms(9),  rdata, iret )
	CALL MN_SLS  ( report, istart(12), iprms(10),  iprms(11), 2,
     +		       rdata, iret )
	CALL MN_SLS  ( report, istart(13), iprms(12), iprms(14), 5,
     +		       rdata, iret )
	CALL MN_SLS  ( report, istart(14), iprms(13), iprms(15), 5,
     +		       rdata, iret )
	CALL MN_3CHC ( report, istart(15), iprms(16), rdata, iret )
	CALL MN_3CHR ( report, istart(16), iprms(17), rdata, iret )
	CALL MN_3CHR ( report, istart(17), iprms(18), rdata, iret )
	CALL MN_SLS  ( report, istart(18), iprms(19), iprms(20), 2,
     +		       rdata, iret )
	CALL MN_3CHR ( report, istart(19), iprms(21), rdata, iret )
	CALL MN_3CHR ( report, istart(20), iprms(22), rdata, iret )
	CALL MN_3CHC ( report, istart(21), iprms(23), rdata, iret )
C*
	RETURN
	END
