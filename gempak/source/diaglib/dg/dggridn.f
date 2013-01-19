	SUBROUTINE DG_GRIDN ( gdattm, glevel, gvcord, gfunc, pfunc, grid,
     +			     igx, igy, time, level, ivcord, parm, iret )
C************************************************************************
C* DG_GRIDN								*
C*									*
C* This subroutine computes a grid diagnostic scalar quantity.  The	*
C* inputs for GDATTM, GLEVEL, GVCORD and GFUNC should be the values	*
C* input by the user.							*
C*									*
C* DG_GRIDN  ( GDATTM, GLEVEL, GVCORD, GFUNC, PFUNC, GRID, 		*
C*            IGX, IGY, TIME, LEVEL, IVCORD, PARM, IRET )		*
C*									*
C* Input parameters:							*
C*	GDATTM 		CHAR*		Input date/time			*
C*	GLEVEL		CHAR*		Input level			*
C*	GVCORD		CHAR*		Input vertical coordinate	*
C*	GFUNC		CHAR*		Diagnostic function		*
C*									*
C* Output parameters:							*
C*	PFUNC		CHAR*		Diagnostic error string		*
C*	GRID (IGX,IGY)	REAL		Output scalar grid		*
C*	IGX		INTEGER		Number of points in x dir	*
C*	IGY		INTEGER		Number of points in y dir	*
C*	TIME  (2)	CHAR*		Output date/time		*
C*	LEVEL (2)	INTEGER		Output level 			*
C*	IVCORD		INTEGER		Output vertical coordinate	*
C*	PARM		CHAR*		Output parameter name		*
C*	IRET		INTEGER		Return code			*
C*				  	  3 = user typed EXIT		*
C*					  0 = normal return		*
C*					 -3 = GFUNC is blank		*
C*					 -4 = output grid not a scalar	*
C*					 -6 = wrong number of operands	*
C*					 -7 = grid cannot be found	*
C*					 -8 = grid is the wrong size	*
C*					 -9 = incorrect operands	*
C*					-10 = internal grid list full	*
C*					-11 = operand must be a vector	*
C*					-12 = operand must be a scalar	*
C*					-13 = operand must be from grid	*
C*					-14 = DG_INIT not initialized	*
C*					-15 = polar center invalid	*
C*					-16 = map proj is invalid	*
C*					-17 = LEVEL must be a layer	*
C*					-18 = TIME must be a range	*
C*					-19 = invalid operator		*
C*					-20 = stack is full		*
C*					-21 = stack is empty		*
C*					-22 = TIME is invalid		*
C*					-23 = LEVEL is invalid		*
C*					-24 = IVCORD is invalid		*
C*					-26 = layer of layers invalid	*
C*					-27 = time range layer invalid	*
C*					-47 = internal grid is too big	*
C*					-70 = cannot computer ensemble	*
C*					-71 = cannot computer layer	*
C**									*
C* Log:									*
C* R. Tian/SAIC		 3/06	Fortran wrapper of DGC_GRID		*
C* S. Gilbert/NCEP	 7/07	Renamed from DG_GRID			*
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
C*
	CHARACTER*(*)	gdattm, glevel, gvcord, gfunc, pfunc
	CHARACTER*(*)	time  (2), parm
	REAL		grid  (*)
	INTEGER		level (*)
C*
	CHARACTER	tmpgda*(LLMXLN), tmpgle*(LLMXLN),
     +	                tmpgvc*(LLMXLN), tmpgfu*(LLMXLN)
C------------------------------------------------------------------------
	CALL ST_NULL ( gdattm, tmpgda, nt, ier )
	CALL ST_NULL ( glevel, tmpgle, nt, ier )
	CALL ST_NULL ( gvcord, tmpgvc, nt, ier )
	CALL ST_NULL ( gfunc,  tmpgfu, nt, ier )
	CALL DGC_GRID ( tmpgda, tmpgle, tmpgvc, tmpgfu, pfunc, grid,
     +		   	igx, igy, time (1), time (2), level (1), level (2),
     +                  ivcord, parm, iret )
	CALL ST_RNUL ( pfunc, pfunc, nt, ier )
	CALL ST_RNUL ( time (1), time (1), nt, ier )
	CALL ST_RNUL ( time (2), time (2), nt, ier )
	CALL ST_RNUL ( parm, parm, nt, ier )
C*
	RETURN
	END
