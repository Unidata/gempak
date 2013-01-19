	SUBROUTINE	DCITOC ( gbftim, gdattim, iret )
C	dc_itoc ( gbftim, gdattim, &iret, sizeof(gdattim[0]) );

	INTEGER		gbftim(3,2)
	CHARACTER*(*)	gdattim(2)

	CHARACTER*20	dattim
	CHARACTER*132	errstr

	CALL TG_ITOC ( gbftim(1,1), dattim, ier)

	gdattim(1) = dattim

	CALL TG_ITOC ( gbftim(1,2), dattim, ier)

	gdattim(2) = dattim

	write(errstr,1001) gdattim(1), gdattim(2)
1001	FORMAT ( 1X, 'time(1) ', A, ' time(2) ',A)
	
	CALL DC_WLOG ( 4, 'DCITOC', 0, errstr, ier)

	iret = 0

	RETURN
	END

	
