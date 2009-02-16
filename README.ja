# -*- rd -*-

= README.ja

$Id: README.ja 367 2006-03-10 07:45:02Z kou $

== 作者

Kouhei Sutou <kou@cozmixng.org>

== ライセンス

GPL or BSD License

== メーリングリスト

((<COZMIXNG RWiki - 連絡先
|URL:http://www.cozmixng.org/~rwiki/?cmd=view;name=%CF%A2%CD%ED%C0%E8>))
を御覧下さい．

== なにこれ？

GaucheのためのXML-RPCライブラリです．

以下のような機能はありません．

  * multiCallとかいろいろ

== 入手方法

((<URL:http://www.cozmixng.org/~kou/download/xsm.tar.gz>))

  % svn co http://www.cozmixng.org/repos/gauche/xsm/trunk xsm

== インストール

  # gosh install/install.scm

== 使い方

sample/calc_{client,cgi,phttpd}.scmあたりを見てください．

== マッピング

XML-RPCでの型と，Scheme上での型（クラス）を以下のようにマッ
ピングします．

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

   SchemeのオブジェクトをXML-RPCのbase64型とする場合は，
   (({make-base64-encoded-string}))手続きを使って，文字列を
   ((|<base64>|))型に変換してください．

: struct
   ((|<hash-table>|))

: array
   ((|<list>|))

== 感謝

  * 山田さん: バグ報告（修正パッチ付き）をしてくれました．
