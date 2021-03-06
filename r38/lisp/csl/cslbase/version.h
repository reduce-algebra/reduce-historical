/* version.h                  Copyright (C) 1990-2007 Codemist Ltd */

#ifndef header_version_h
#define header_version_h 1

/*
 * This code may be used and modified, and redistributed in binary
 * or source form, subject to the "CCL Public License", which should
 * accompany it. This license is a variant on the BSD license, and thus
 * permits use of code derived from this in either open and commercial
 * projects: but it does require that updates to this code be made
 * available back to the originators of the package.
 * Before merging other code in with this or linking this code
 * with other packages or libraries please check that the license terms
 * of the other material are compatible with those of this.
 */

/* Signature: 66b51631 19-Jan-2007 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

/*
 * VERSION is used to control the version number displayed when CSL/CCL
 * is started up in verbose mode (command line option -v). Version numbers
 * are also recorded in image files. But NOTE NOTE NOTE that the macro
 * VERSION gets set in config.h based on the version number established in
 * "configure.ac" and so the value set here is merely a fall-back. Indeed
 * this whole file is a bit of a joke!
 */

#ifndef VERSION
#define VERSION     "6.03"
#endif

/*
 * The next two lines are processed by filesign.c on the basis of
 * of a private file register.key and a user name to give 64 bytes
 * of registration information, coded up as two strings of hex digits.
 */
#define REG1 "69d24b1690f1ca585906206121ad286e84ad98306b8273a4509249584fa048e9"
#define REG2 "ec223ed82d9a91419bbeba5df8dc624876b95a05bd1fa2c170a9b252a86383bc"

#endif /* header_version_h */

/* end of version.h */
