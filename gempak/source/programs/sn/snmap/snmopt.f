	SUBROUTINE SNMOPT  ( datfil, chtim, device, proj, area, garea,
     +			     nparm,  prcons, icolr,  map, title, 
     +			     clear,  filtfc, kprc, panel, rlevel,
     +			     vcoord, iret )
C************************************************************************
C* SNMOPT								*
C*									*
C* This subroutine displays a summary of the users options.		*
C*									*
C* SNMOPT  ( DATFIL, CHTIM, DEVICE, PROJ, AREA, GAREA, NPARM, PRCONS,	*
C*           ICOLR, MAP, TITLE, CLEAR, FILTFC, KPRC, PANEL, RLEVEL, 	*
C*	     VCOORD, IRET )						*
C*									*
C* Input parameters:							*
C*	DATFIL		CHAR*		Data file name			*
C*	CHTIM		CHAR*		Time 				*
C*	DEVICE		CHAR*		Device 				*
C*	PROJ		CHAR*		Projection 			*
C*	AREA		CHAR*		Area name			*
C*	GAREA		CHAR*		Graphics area name		*
C*	NPARM		INTEGER		Number of parameters 		*
C*	PRCONS(*)	CHAR*		Parameters-conditions		*
C*	ICOLR(*)	INTEGER		Colors 				*
C*	MAP		CHAR*		Map options			*
C*	TITLE		CHAR*		Title				*
C*	CLEAR		LOGICAL		Clear flag			*
C*	FILTFC		REAL		Filter factor			*
C*	KPRC		INTEGER		Repeat plot counter		*
C*	PANEL		CHAR*		Panel input			*
C*	RLEVEL		REAL		Vertical level			*
C*	VCOORD		CHAR*		Vertical coordinate		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = exit			*
C* Log:									*
C* I. Graffman/RDS	12/84						*
C* M. desJardins/GSFC	 6/88	Changed					*
C* S. Schotz/GSC	 5/90	Get respnd locally from IP_RESP		*
C* S. Schotz/GSC	 8/90	Removed scale, added conditions		*
C* J. Nielsen/TAMU	11/91	Added filter factor			*
C* K. Brill/NMC		12/91	Removed WIND; Do not skip BLNK first    *
C*				parameter				*
C* K. Brill/NMC		02/92	Put PRCONS into PRMO for printing	*
C* K. Brill/NMC		03/92	F -> F5.2				*
C* S. Jacobs/NMC	 6/94	Aligned the output for the filter factor*
C* S. Maxwell/GSC 	 3/97	Removed marker                          *
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
C*
	CHARACTER*(*)	datfil, chtim, proj, area, garea, device, vcoord
	CHARACTER*(*)	prcons (*), map, title, panel
	INTEGER		icolr (*)
	LOGICAL		respnd, clear
	REAL		filtfc
C*
     	CHARACTER	clr*3, filt*3
	CHARACTER	prmo (MMPARM)*4
C------------------------------------------------------------------------
	iret  = 0
	level = NINT ( rlevel )
	IF  ( nparm .eq. 0 )  THEN
	    np = 1
	    prcons (1) = ' '
	  ELSE 
	    np = nparm
	END IF
	IF  ( clear )  THEN
	    clr = 'YES'
	  ELSE
	    clr = 'NO'
	END IF
	IF  ( filtfc .ne. 0. )  THEN
	    filt = 'YES'
	  ELSE
	    filt = 'NO'
	END IF
C*
	IF  ( kprc .gt. 1 )  THEN
	    WRITE  ( 6, 3000 )  chtim, level, vcoord
3000	    FORMAT ( / 'Next plot -- Time:     ', A /
     +               ' Level:    ', I6, '   VCOORD:   ', A )
	  ELSE
	    WRITE ( 6, 5000 ) area, garea
            DO  i = 1, np
		prmo (i) = prcons (i)
            END DO
	    WRITE ( 6, 5010 ) ( prmo (i), i = 1, np )
	    WRITE ( 6, 5020 ) ( icolr (j), j = 1, np )
	    WRITE (6,  5030 ) chtim, level, vcoord,
     +			      datfil (1:60), map,
     +			      title, device, proj, clr, filt, 
     +			      filtfc, panel
	END IF
	CALL IP_RESP ( respnd, ier )
	IF  ( respnd )  THEN
	    CALL TM_ACCP  ( ier )
	    IF  ( ier .eq. 2 )  iret = -1
	END IF
C*
5000	FORMAT ( ' SNMAP PARAMETERS:',//
     +           ' Data area:          ', A, /
     +           ' Graphics area name: ', A )
5010	FORMAT ( ' Valid parameters:   ' , 10 ( 1X, A4 ) )
5020	FORMAT ( ' Parameter colors:  ' ,  10 ( I4, ' ' ) )
5030	FORMAT ( ' Time:              ', A,/
     +           ' Level:             ', I6 /
     +           ' Vert coord:          ', A /
     +           ' File:              ', A,/
     +           ' Map:               ', A,/
     +           ' Title:             ', A,/
     +           ' Device:            ', A,/ 
     +           ' Projection:        ', A,/
     +           ' Clear screen:      ', A,/
     +           ' Filter:            ', A,/
     +           ' Filter factor:     ', F5.2,/
     +           ' Panel:             ', A )
C*
	RETURN
	END
