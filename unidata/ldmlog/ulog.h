/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: ulog.h,v 1.79 1995/11/21 17:47:44 davis Exp $ */

#ifndef _ULOG_H_
#define _ULOG_H_
#include <syslog.h>

/*
 * options to openulog() which are not options
 * to openlog()
 * Make sure these dont collide with ones in <syslog.h>
 * or the 4.2 compatibility section below
 */
#define LOG_NOTIME 0x200		/* don't put on the timestamp */
#define LOG_LOCALTIME 0x100  /* use localtime. default is gmt */

/*
 * This set of #defines allows this to work even with a
 * 4.2 BSD style syslog.h, like on Ultrix 4.x
 */
#ifndef LOG_NFACILITIES
/* means this system doesn't have 4.3 BSD syslog */
#define LOG_NFACILITIES 0
#endif /* !LOG_NFACILITIES */

#ifndef LOG_MASK
#define	LOG_MASK(pri)	(1 << (pri))		/* mask for one priority */
#endif 

#ifndef LOG_PRIMASK
#define LOG_PRIMASK (LOG_MASK(LOG_EMERG) | LOG_MASK(LOG_ALERT) \
	| LOG_MASK(LOG_CRIT) | LOG_MASK(LOG_ERR) | LOG_MASK(LOG_WARNING) \
	| LOG_MASK(LOG_NOTICE) | LOG_MASK(LOG_INFO) | LOG_MASK(LOG_DEBUG))
#endif

#ifndef LOG_FACMASK
#define LOG_FACMASK (~LOG_PRIMASK)
#endif

#ifndef LOG_USER
#define LOG_USER 0
#endif

#ifndef LOG_LOCAL0
#define LOG_LOCAL0 0
#endif

#ifndef LOG_CONS
#define LOG_CONS 0x20
#endif

#ifndef LOG_NOWAIT
#define LOG_NOWAIT 0x40
#endif

#ifndef LOG_UPTO
#define	LOG_UPTO(pri)	((1 << ((pri)+1)) - 1)	/* all priorities through pri */
#endif 
/* End 4.2 compatiblity section */


/*
 * The "facility" used by ldm applications.
 * If you change this, change the script ../scour/scour as well.
 */
#ifndef LOG_LDM
#define LOG_LDM LOG_LOCAL0
#endif


/*
 * The 'ident' arg to openulog() and setulogident will be
 * copied into static space, truncatated to this length.
 */
#define LOG_IDENT_LEN 32

#ifdef __cplusplus
extern "C" int closeulog(void);
extern "C" int openulog(
	const char *ident ,
	int options ,
	int facility , 
	const char *logfilename);
extern "C" void ulog(int pri, const char *fmt, ...);
extern "C" int setulogmask(int pmask);
extern "C" int toggleulogpri(int pri);
extern "C" void rollulogpri(void);
extern "C" int getulogmask(void);
extern "C" int ulogIsVerbose(void);
extern "C" int ulogIsDebug(void);
extern "C" void serror(const char *fmt, ...);
extern "C" void uerror(const char *fmt, ...);
extern "C" void unotice(const char *fmt, ...);
extern "C" void uinfo(const char *fmt, ...);
extern "C" void udebug(const char *fmt, ...);
extern "C" const char *ubasename(const char *av0);
extern "C" void _uassert( const char *ex, const char *file, int line);
#elif defined(__STDC__)

extern int
closeulog(void);

extern int
openulog(
	const char *ident ,
	int options ,
	int facility , 
	const char *logfilename);

extern void
ulog(int pri, const char *fmt, ...);

extern void
setulogident(const char *ident);

extern int
setulogmask(int pmask);

extern int
toggleulogpri(int pri);

extern void
rollulogpri(void);

extern int
getulogmask(void);

extern int
ulogIsVerbose(void);

extern int
ulogIsDebug(void);

extern void
serror(const char *fmt, ...);

extern void
uerror(const char *fmt, ...);

extern void
unotice(const char *fmt, ...);

extern void
uinfo(const char *fmt, ...);

extern void
udebug(const char *fmt, ...);

extern const char *
ubasename(const char *av0);

extern void
_uassert( const char *ex, const char *file, int line);

#ifdef NO_STRERROR
extern char *
strerror(int);
#endif

#else /* Old Style C */
extern int closeulog();
extern int openulog();
extern void ulog();
extern int setulogmask();
extern int toggleulogpri();
extern void rollulogpri();
extern int getulogmask();
extern int ulogIsVerbose();
extern int ulogIsDebug();
extern void serror();
extern void uerror();
extern void unotice();
extern void uinfo();
extern void udebug();
extern const char *ubasename();
extern void _uassert();
#ifdef NO_STRERROR
extern char *
strerror(/* int */);
#endif
#endif

/*
 * When we are using ulog, we want assert() messages to go via
 * the logger.
 */
#if defined(assert) && !defined(NDEBUG)
#undef assert
#if defined(__STDC__) || defined(__cplusplus)
#       define assert(EX) \
            (((int) (EX)) ? (void)0 :  _uassert(#EX, __FILE__, __LINE__))
#else
#       define assert(EX) \
            (((int) (EX)) ? (void)0 :  _uassert("EX", __FILE__, __LINE__))
#endif
#endif


#endif /* !_ULOG_H_ */
