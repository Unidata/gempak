/*
 * Copyright(c) 1997, Space Science and Engineering Center, UW-Madison
 * Refer to "McIDAS Software Acquisition and Distribution Policies"
 * in the file  mcidas/data/license.txt
 */

/**** $Id: servacct.h,v 1.1 1999/11/01 16:08:54 russd Exp $ ****/

/*
    This defines the servacct data structure; that is, the record which
a server writes out describing its transaction


    j.benson  02/94

*/

#ifndef _SERVACCT_H
#define _SERVACCT_H

struct Servacct {
    unsigned server_address;
    int server_port;
    unsigned int client_address;
    char user[4];			/* mcidas logon initials */
    int project;
    char password[12];
    char transaction[4];
    int input_length;		/* data to server(real) */
    char text[120];

    int reply_length;		/* data from server(real) */
    int cpu;			/* 100 X cpu seconds */
    int returncode;
    char errormsg[72];
    int date;			/* yyddd */
    int start_time;		/* hhmmss */
    int end_time;			/* hhmmss */
} ;

typedef struct Servacct servacct;

#endif  /* _SERVACCT_H  */
