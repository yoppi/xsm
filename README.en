# -*- rd -*-

= README.en

$Id: README.en 367 2006-03-10 07:45:02Z kou $

((*I'm not good in English. Please fix.*))

== Author

Kouhei Sutou <kou@cozmixng.org>

== License

GPL or BSD License

== Mailing list

Send mail that To field is cozdev@cozmixng.quickml.com , Cc
field is kou@cozmixng.org and Subject and Body contain
somthig to
((<"cozdev@cozmixng.quickml.com"|URL:mailto:cozdev@cozmixng.quickml.com?cc=kou@cozmixng.org&subject=Join!>)).

Note that this mailing list's main language is Japanese. But
English mail is welcome too.

== What's this?

A XML-RPC library for Gauche.

This doesn't have the following features.

  * multiCall and so on.

== Get

((<URL:http://www.cozmixng.org/~kou/download/xsm.tar.gz>))

  % svn co http://www.cozmixng.org/repos/gauche/xsm/trunk xsm

== Install

  # gosh install/install.scm

== Usage

Sed sample/calc_{client,cgi,phttpd}.scm.

== Mapping

This maps type of XML-RPC to type (class) of Scheme like
bellow.

: int
   ((|<integer>|))

: boolean
   ((|<boolean>|))

: string
   ((|<string>|))

: double
   ((|<double>|))

: dateTime.iso8601
   ((|<date>|))

: base64
   ((|<string>|))

   If you want to map object of Scheme to object of XML-RPC
   that have type base64, convert strings to ((|<base64>|))
   class by procedure (({make-base64-encoded-string})).

: struct
   ((|<hash-table>|))

: array
   ((|<list>|))

== Thanks

  * yamada: He reports some bugs with patches to fix the bug.
