/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: inetutil.h,v 1.1 1995/11/03 16:04:11 russ Exp $ */

/* 
 * Miscellaneous functions to make dealing with internet addresses easier.
 */

#ifndef _INETUTIL_H_
#define _INETUTIL_H_

#ifdef IPPROTO_IP /* we included netinet/in.h, so struct sockaddr_in is */
extern char *hostbyaddr(struct sockaddr_in *paddr);
extern int addrbyhost(const char *hostname, struct sockaddr_in *paddr);
extern char *s_sockaddr_in(struct sockaddr_in *paddr);
extern int gethostaddr_in(struct sockaddr_in *paddr);
#endif
extern int getservport(const char *servicename, const char *proto);
extern char *ghostname(void);
extern int usopen(const char *name);
extern int udpopen(const char *hostname, const char *servicename);
extern int isMe(const char *remote);
extern int sockbind(const char *type, unsigned short port);

#endif /* !_INETUTIL_H_ */
