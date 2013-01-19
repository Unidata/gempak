        SUBROUTINE DG_NWDT ( grid, time, level, ivcord, parm, igdhdr, 
     +	                     gpack, rplc, iret )
C************************************************************************
C* DG_NWDT                                                              *
C*                                                                      *
C* This subroutine writes a packed grid.				*
C*                                                                      *
C* DG_NWDT  ( GRID, TIME, LEVEL, IVCORD, PARM, IGDHDR, GPACK, 		*
C*            RPLC, IRET )						*
C*                                                                      *
C* Input parameters:                                                    *
C*	GRID (*)	REAL    	Grid to write out		*
C*      TIME (2)        CHAR*           GEMPAK times                    *
C*      LEVEL (2)       INTEGER         GEMPAK grid levle               *
C*      IVCORD          INTEGER         Vertical coordinate             *
C*      PARM            CHAR*           GEMPAK parameter name           *
C*	IGDHDR (*)	INTEGER		Grid header			*
C*	GPACK		CHAR*   	Grid packing from user input	*
C*	RPLC		LOGICAL		Flag to replace existing grid	*
C*                                                                      *
C* Output parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C*                                      -61 = grid file open failed     *
C**                                                                     *
C* Log:                                                                 *
C* R. Tian/SAIC          3/06   Fortran wrapper of DGC_NWDT             *
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
C*
	REAL		grid (*)
        CHARACTER*(*)   time (2), parm, gpack
	INTEGER         level (*), igdhdr (*)
	LOGICAL		rplc
C*
	CHARACTER	time1*(LLMXLN), time2*(LLMXLN),
     +	                tmppar*(LLMXLN), tmpgpa*(LLMXLN)
C------------------------------------------------------------------------
	CALL ST_NULL ( time (1), time1, nt, ier )
	CALL ST_NULL ( time (2), time2, nt, ier )
	CALL ST_NULL ( parm,  tmppar, nt, ier )
	CALL ST_NULL ( gpack, tmpgpa, nt, ier )
        CALL DGC_NWDT ( grid, time1, time2, level (1), level (2),
     +                 ivcord, tmppar, igdhdr, tmpgpa, rplc, iret )
C
	RETURN
	END
