#!/bin/sh

MKFILE      = Makefile
DEPFILE     = ${MKFILE}.dep
NOINCL      = ci clean spotless
NEEDINCL    = ${filter ${NOINCL}, ${MAKECMDGOALS}}
GMAKE       = ${MAKE} --no-print-directory

SOURCE      = sbi.scm
EXEBIN      = ${SOURCE}
OTHERS      = README
ALLSOURCES  = ${SOURCE} ${OTHERS} ${MKFILE}
SUBMITDIR   = cmps112-wm.f15 asg1
LISTING     = Listing.ps

all: ${EXEBIN}
	- checksource ${ALLSOURCES}
	- testrun.sh

ci: ${ALLSOURCES}
	cid + ${ALLSOURCES}
	- checksource ${ALLSOURCES}

again:
	${GMAKE} ci all

submit: ${ALLSOURCES}
	submit ${SUBMITDIR} ${ALLSOURCES}