        SUBROUTINE DG_QGRD ( time, level, ivcord, parm, exist, iret )
C************************************************************************
C* DG_QGRD                                                              *
C*                                                                      *
C* This subroutine determines whether a grid exists in the output file. *
C*                                                                      *
C* DG_QGRD  ( TIME, LEVEL, IVCORD, PARM, EXIST, IRET )    		*
C*                                                                      *
C* Input parameters:                                                    *
C*      TIME (2)        CHAR*           GEMPAK times                    *
C*      LEVEL (2)       INTEGER         GEMPAK grid levle               *
C*      IVCORD          INTEGER         Vertical coordinate             *
C*      PARM            CHAR*           GEMPAK parameter name           *
C*                                                                      *
C* Output parameters:                                                   *
C*	EXIST		LOGICAL		Existence of a grid		*
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C*                                      -60 = grid file open failed     *
C**                                                                     *
C* Log:                                                                 *
C* R. Tian/SAIC          3/06   Fortran wrapper of DGC_QGRD             *
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
C*
        CHARACTER*(*)   time (2), parm
	INTEGER         level (2)
	LOGICAL		exist
C*
	CHARACTER	time1*(LLMXLN), time2*(LLMXLN), tmppar*(LLMXLN)
C------------------------------------------------------------------------
	CALL ST_NULL ( time (1), time1, nt, ier )
	CALL ST_NULL ( time (2), time2, nt, ier )
	CALL ST_NULL ( parm, tmppar, nt, ier )
        CALL DGC_QGRD ( time1, time2, level (1), level (2), ivcord,
     +	               tmppar, exist, iret )
C*
	RETURN
	END
