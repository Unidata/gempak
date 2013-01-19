	SUBROUTINE SFL6HD  ( nlun, lun, iret )
C************************************************************************
C* SFL6HD								*
C*									*
C* This subroutine writes out the parameter name header.		*
C*									*
C* SFL6HD  ( NLUN, LUN, IRET )						*
C*									*
C* Input parameters:							*
C*	NLUN		INTEGER		Number of output devices 	*
C*	LUN  (NLUN)	INTEGER		LUNs 				*
C*									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C**									*
C* Log:									*
C* I. Graffman/RDS	 7/87	GEMPAK4					*
C* M. desJardins/GSFC	 6/88	Leave space for 6 digit station		*
C* J. Whistler/SSAI	 2/91   Changed the header to incorporate new	*
C*				parameters				*
C* S. Jacobs/EAI	10/92	Changed horizontal spacing		*
C* K. Tyle/GSC		 1/97	Add a blank line at top			*
C************************************************************************
	INTEGER		lun (*)
C*
	CHARACTER	prmbuf*79
C*
	DATA 		prmbuf ( 1 : 49 )
     +		/ 'HH   STN  CLDL  CLDM  CLDH VSBY  WTHR  PMSL  TMP ' /
	DATA 		prmbuf ( 50 :  )
     +		/ 'DWP DDSS GST ALT  PTND PCPN SN' /
C------------------------------------------------------------------------
C*	Write header to requested luns.
C
	DO  ilun = 1, nlun
	    WRITE   ( lun ( ilun ), * ) ' '
	    WRITE   ( lun ( ilun ), 5000 )  prmbuf
5000	    FORMAT  ( ' ', A )
	END DO
	iret = 0
C*
	RETURN
	END
