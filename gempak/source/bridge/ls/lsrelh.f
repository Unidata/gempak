        SUBROUTINE  LS_RELH  ( lsfrpt, ipt, iret )
C************************************************************************
C* LS_RELH                                                              *
C*                                                                      *
C* This subroutine decodes the relative humidity group 29UUU, where UUU *
C* is the relative humidity of the air in percentage.                   *
C*                                                                      *
C* LS_RELH  ( LSFRPT, IPT, IRET ) 	                                *
C*                                                                      *
C* Input parameters:                                                    *
C*      LSFRPT          CHAR*           Report array                    *
C*                                                                      *
C* Input and Output parameters:                                         *
C*      IPT             INTEGER         On input, points to first U in  *
C*					29UUU group; on output, points  *
C*					to last U			*
C*					                                *
C* Output parameters:                                                   *
C*      RIVALS(IRRELH)  REAL            Relative humidity in percent    *
C*	IRET            INTEGER         Return code                     *
C*				   	  0 = normal return 	        *
C*                                        1 = problems                  *
C*                                                                      *
C**								        *
C* Log:							                *
C* R. Hollern/NCEP      6/96                                            *
C* D. Kidwell/NCEP      4/97	Removed inteface calls, reorganized     *
C*                              header and comments                     *
C* R. Hollern/NCEP      1/98	Changes based on MA_RELH                *
C************************************************************************
        INCLUDE 	'GEMPRM.PRM'
	INCLUDE		'lscmn.cmn'
C*
        CHARACTER*(*)   lsfrpt
C*
        CHARACTER   	fld3*3
C------------------------------------------------------------------------
        iret = 0
C
        fld3 = lsfrpt (ipt:ipt+2)
        CALL  ST_CRNM ( fld3, rval, ier )
        ipt = ipt + 2
        IF ( ier .eq. 0 ) rivals ( irrelh ) = rval
C*
        RETURN
        END
