	SUBROUTINE DG_NDTM ( gdatm, iret )
C************************************************************************
C* DG_NDTM                                                              *
C*                                                                      *
C* This subroutine scans the user input for GDATTIM and creates an      *
C* internal list of times to process. DG_NFIL must be called first to 	*
C* set either a template for the first GDFILE entry or to open the file	*
C* associated with it. DG_NFIL also determines first and last times	*
C* associated with the first GDFILE entry. The information from DG_NFIL *
C* is in DGCMN.CMN							*
C*									*
C* All indeterminant time substitutions are based on the times		*
C* associated with the first GDFILE entry.				*
C*                                                                      *
C* DG_NDTM  ( GDATM, IRET )					        *
C*                                                                      *
C* Input parameters:                                                    *
C*      GDATM           CHAR*           User input for GDATTIM          *
C*                                                                      *
C* Output parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C*                                      -22 = invalid time              *
C*                                      -56 = grid file open failed     *
C**                                                                     *
C* Log:                                                                 *
C* R. Tian/SAIC		 3/06	Fortran wrapper of DGC_NDTM		*
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
C*
        CHARACTER*(*)  	gdatm
C*
	CHARACTER	tmpgda*(LLMXLN)
C------------------------------------------------------------------------
	CALL ST_NULL ( gdatm, tmpgda, nt, ier )
	CALL DGC_NDTM ( tmpgda, iret )
C*
	RETURN
	END
