.. ===========================================================
.. PARTIAL REPORT — BENCHMARK WAS INTERRUPTED BY THE USER
.. Interrupted during: Mode B warm-up
.. Current file: ~/my/dvo/seed7-repos/seed7/prg/fannkuch.sd7
.. Interrupted phase elapsed so far: 00:01:05.291
.. Results below include only completed phases.
.. ===========================================================

=======================================================
PARTIAL GC-Controlled Benchmark Report: Seed7 Mode — Four Modes
=======================================================

:Running with: seed7-mode 2026-06-21T21:16:24+0000 W25-7
:Seed7 dir   : /Users/roup/my/dvo/seed7-repos/seed7/
:Started at: 15:25:49 local time
:Generated on: 2026-06-22 19:28:04 UTC
:N Iterations: 5  (mean of N timed opens per file)
:GC @ testing: suppressed (gc-cons-threshold = most-positive-fixnum)
:Warm-up info: yes (1 untimed pass per mode + garbage-collect before timing)
:Modes planned: A B C D
:INTERRUPTED: yes — during Mode B warm-up

Mode Descriptions
=================

A:
   Major-mode activation only. No window → jit-lock never fires.
B:
   Mode activation + initial visible jit-lock pass.
C:
   Mode activation + full-buffer fontification (font-lock-ensure).
D:
   Mode activation + full incremental jit-lock (scroll top→bottom).


Mode A — Major-Mode Activation Only
-----------------------------------

No window created. Measures syntax-table setup and font-lock keyword compilation.

File Load Times — Mode A (mean, GC-free)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

============================ ================= ================
File Name                    Mean Time (s)     Line count
============================ ================= ================
seed7/prg/addup.sd7          0.034067            190
seed7/prg/bas7.sd7           0.034127          11459
seed7/prg/bifurk.sd7         0.032786             73
seed7/prg/bigfiles.sd7       0.033791            129
seed7/prg/brainf7.sd7        0.033279             86
seed7/prg/calc7.sd7          0.033702            128
seed7/prg/carddemo.sd7       0.032629            190
seed7/prg/castle.sd7         0.033709           3148
seed7/prg/cat.sd7            0.033640             82
seed7/prg/cellauto.sd7       0.033665             85
seed7/prg/celsius.sd7        0.035135             42
seed7/prg/chk_all.sd7        0.034425            843
seed7/prg/chkarr.sd7         0.034134           8367
seed7/prg/chkbig.sd7         0.038922          29026
seed7/prg/chkbin.sd7         0.034491           6469
seed7/prg/chkbitdata.sd7     0.034340           6624
seed7/prg/chkbool.sd7        0.033948           3157
seed7/prg/chkbst.sd7         0.033991            722
seed7/prg/chkchr.sd7         0.033965           2809
seed7/prg/chkcmd.sd7         0.033531           1205
seed7/prg/chkdb.sd7          0.034648           7454
seed7/prg/chkdecl.sd7        0.033432            448
seed7/prg/chkenum.sd7        0.034346           1230
seed7/prg/chkerr.sd7         0.036985           4663
seed7/prg/chkexc.sd7         0.034487           2627
seed7/prg/chkfil.sd7         0.033728           1615
seed7/prg/chkflt.sd7         0.036804          20620
seed7/prg/chkhent.sd7        0.033384             54
seed7/prg/chkhsh.sd7         0.033900           4548
seed7/prg/chkidx.sd7         0.037930          19567
seed7/prg/chkint.sd7         0.047933          38129
seed7/prg/chkjson.sd7        0.034835           1764
seed7/prg/chkovf.sd7         0.033412           8216
seed7/prg/chkprc.sd7         0.033370          10111
seed7/prg/chkscan.sd7        0.032877            714
seed7/prg/chkset.sd7         0.033799          11974
seed7/prg/chkstr.sd7         0.036573          26952
seed7/prg/chktime.sd7        0.034583           2025
seed7/prg/chktoml.sd7        0.037068           1656
seed7/prg/clock.sd7          0.034850             47
seed7/prg/clock2.sd7         0.033630             43
seed7/prg/clock3.sd7         0.033735             95
seed7/prg/cmpfil.sd7         0.035217             84
seed7/prg/comanche.sd7       0.035911            180
seed7/prg/confval.sd7        0.033499            175
seed7/prg/db7.sd7            0.033586            417
seed7/prg/diff7.sd7          0.033714            263
seed7/prg/dirtst.sd7         0.033462             42
seed7/prg/dirx.sd7           0.033635            152
seed7/prg/dnafight.sd7       0.033504           1381
seed7/prg/dragon.sd7         0.032957             73
seed7/prg/echo.sd7           0.033424             39
seed7/prg/eliza.sd7          0.033662            302
seed7/prg/err.sd7            0.033691             96
seed7/prg/fannkuch.sd7       0.033385            131
seed7/prg/fib.sd7            0.033653             47
seed7/prg/find7.sd7          0.033656            133
seed7/prg/findchar.sd7       0.032597            149
seed7/prg/fractree.sd7       0.033352             55
seed7/prg/ftp7.sd7           0.032603            296
seed7/prg/ftpserv.sd7        0.032509             74
seed7/prg/gcd.sd7            0.032805            109
seed7/prg/gkbd.sd7           0.032568            358
seed7/prg/gtksvtst.sd7       0.032542             94
seed7/prg/hal.sd7            0.032692            250
seed7/prg/hamu.sd7           0.032241            573
seed7/prg/hanoi.sd7          0.032739             55
seed7/prg/hd.sd7             0.033942             79
seed7/prg/hello.sd7          0.033551             32
seed7/prg/hilbert.sd7        0.033474            108
seed7/prg/ide7.sd7           0.032306            196
seed7/prg/kbd.sd7            0.032358             49
seed7/prg/klondike.sd7       0.032294            883
seed7/prg/lander.sd7         0.046304           1551
seed7/prg/lst80bas.sd7       0.036859            344
seed7/prg/lst99bas.sd7       0.034192            401
seed7/prg/lstgwbas.sd7       0.033441            577
seed7/prg/mahjong.sd7        0.033569           1943
seed7/prg/make7.sd7          0.033584            121
seed7/prg/mandelbr.sd7       0.034271            237
seed7/prg/mind.sd7           0.033988            443
seed7/prg/mirror.sd7         0.033639            131
seed7/prg/ms.sd7             0.033544            641
seed7/prg/nicoma.sd7         0.033839            135
seed7/prg/pac.sd7            0.033675            726
seed7/prg/pairs.sd7          0.033709           2025
seed7/prg/panic.sd7          0.035704           2634
seed7/prg/percolation.sd7    0.033592            330
seed7/prg/planets.sd7        0.033918           1486
seed7/prg/portfwd7.sd7       0.034434            139
seed7/prg/prime.sd7          0.033765             74
seed7/prg/printpi1.sd7       0.033629             56
seed7/prg/printpi2.sd7       0.033016             54
seed7/prg/printpi3.sd7       0.032531             60
seed7/prg/pv7.sd7            0.032496            337
seed7/prg/queen.sd7          0.032666            149
seed7/prg/rand.sd7           0.033874            121
seed7/prg/raytrace.sd7       0.033490            538
seed7/prg/rever.sd7          0.035393            816
seed7/prg/roman.sd7          0.033773             38
seed7/prg/s7c.sd7            0.034302           9060
seed7/prg/s7check.sd7        0.033574             68
seed7/prg/savehd7.sd7        0.034773           1110
seed7/prg/self.sd7           0.033711             49
seed7/prg/shisen.sd7         0.033528           1423
seed7/prg/sl.sd7             0.033937           1029
seed7/prg/snake.sd7          0.033703            615
seed7/prg/sokoban.sd7        0.034101            891
seed7/prg/spigotpi.sd7       0.033957             64
seed7/prg/sql7.sd7           0.033860            278
seed7/prg/startrek.sd7       0.033769            979
seed7/prg/sudoku7.sd7        0.033673           2657
seed7/prg/sydir7.sd7         0.033410            384
seed7/prg/syntaxhl.sd7       0.033650            177
seed7/prg/tak.sd7            0.033410             59
seed7/prg/tar7.sd7           0.033531            121
seed7/prg/tch.sd7            0.033372             55
seed7/prg/testfont.sd7       0.033392             95
seed7/prg/tet.sd7            0.033665            479
seed7/prg/tetg.sd7           0.033441            501
seed7/prg/toutf8.sd7         0.033644            240
seed7/prg/tst_cli.sd7        0.033438             40
seed7/prg/tst_srv.sd7        0.033318             47
seed7/prg/wator.sd7          0.033881            651
seed7/prg/which.sd7          0.032868             65
seed7/prg/wiz.sd7            0.032754           2833
seed7/prg/wordcnt.sd7        0.032757             54
seed7/prg/wrinum.sd7         0.032285             43
seed7/prg/wumpus.sd7         0.032512            372
seed7/lib/aes.s7i            0.033289           1144
seed7/lib/aes_gcm.s7i        0.034797            392
seed7/lib/ar.s7i             0.033686           1532
seed7/lib/arc4.s7i           0.033699            144
seed7/lib/archive.s7i        0.033766            143
seed7/lib/archive_base.s7i   0.033742            135
seed7/lib/array.s7i          0.033622            610
seed7/lib/asn1.s7i           0.033580            544
seed7/lib/asn1oid.s7i        0.033575            157
seed7/lib/basearray.s7i      0.033663            450
seed7/lib/bigfile.s7i        0.033545            136
seed7/lib/bigint.s7i         0.033752            824
seed7/lib/bigrat.s7i         0.033856            784
seed7/lib/bin16.s7i          0.033193            592
seed7/lib/bin32.s7i          0.033549            490
seed7/lib/bin64.s7i          0.033452            539
seed7/lib/bitdata.s7i        0.033719           1330
seed7/lib/bitmapfont.s7i     0.033667            215
seed7/lib/bitset.s7i         0.033634            593
seed7/lib/bitsetof.s7i       0.033686            431
seed7/lib/blowfish.s7i       0.033856            383
seed7/lib/bmp.s7i            0.033525            924
seed7/lib/boolean.s7i        0.033598            403
seed7/lib/browser.s7i        0.033693            280
seed7/lib/bstring.s7i        0.033522            227
seed7/lib/bytedata.s7i       0.033450            482
seed7/lib/bzip2.s7i          0.033482            887
seed7/lib/cards.s7i          0.032746           1342
seed7/lib/category.s7i       0.035996            209
seed7/lib/cc_conf.s7i        0.033100           1314
seed7/lib/ccittfax.s7i       0.032625           1022
seed7/lib/cgi.s7i            0.032942            109
seed7/lib/cgidialog.s7i      0.033179           1118
seed7/lib/char.s7i           0.033259            356
seed7/lib/charsets.s7i       0.033711           2024
seed7/lib/chartype.s7i       0.033563            121
seed7/lib/cipher.s7i         0.033672            146
seed7/lib/cli_cmds.s7i       0.033357           1360
seed7/lib/clib_file.s7i      0.033582            301
seed7/lib/color.s7i          0.033575            185
seed7/lib/complex.s7i        0.033151            464
seed7/lib/compress.s7i       0.032902            150
seed7/lib/console.s7i        0.033907            188
seed7/lib/cpio.s7i           0.033346           1708
seed7/lib/crc32.s7i          0.033855            193
seed7/lib/cronos16.s7i       0.033470           1173
seed7/lib/cronos27.s7i       0.033564           1464
seed7/lib/csv.s7i            0.033331            201
seed7/lib/db_prop.s7i        0.033615            991
seed7/lib/deflate.s7i        0.033345            740
seed7/lib/des.s7i            0.033673            444
seed7/lib/dialog.s7i         0.033576            311
seed7/lib/dir.s7i            0.033539            163
seed7/lib/draw.s7i           0.035291            854
seed7/lib/duration.s7i       0.033679           1038
seed7/lib/echo.s7i           0.033549            132
seed7/lib/editline.s7i       0.033367            398
seed7/lib/elf.s7i            0.033773           1560
seed7/lib/elliptic.s7i       0.033283            649
seed7/lib/enable_io.s7i      0.033159            312
seed7/lib/encoding.s7i       0.033144            931
seed7/lib/enumeration.s7i    0.033225            236
seed7/lib/environment.s7i    0.032757            175
seed7/lib/exif.s7i           0.032561            152
seed7/lib/external_file.s7i  0.033889            340
seed7/lib/field.s7i          0.033657            268
seed7/lib/file.s7i           0.033499            372
seed7/lib/filebits.s7i       0.033808             46
seed7/lib/filesys.s7i        0.033499            601
seed7/lib/fileutil.s7i       0.033513            144
seed7/lib/fixarray.s7i       0.033567            307
seed7/lib/float.s7i          0.033611            757
seed7/lib/font.s7i           0.033555            196
seed7/lib/font8x8.s7i        0.033751            998
seed7/lib/forloop.s7i        0.033141            449
seed7/lib/ftp.s7i            0.033478            969
seed7/lib/ftpserv.s7i        0.034021            631
seed7/lib/getf.s7i           0.033615            115
seed7/lib/gethttp.s7i        0.033537             41
seed7/lib/gethttps.s7i       0.033498             41
seed7/lib/gif.s7i            0.033753            561
seed7/lib/graph.s7i          0.033712            415
seed7/lib/graph_file.s7i     0.033498            399
seed7/lib/gtkserver.s7i      0.033705            161
seed7/lib/gzip.s7i           0.033432            573
seed7/lib/hash.s7i           0.033616            421
seed7/lib/hashsetof.s7i      0.033616            499
seed7/lib/hmac.s7i           0.033314            152
seed7/lib/html.s7i           0.033480             83
seed7/lib/html_ent.s7i       0.033569            476
seed7/lib/htmldom.s7i        0.033555            286
seed7/lib/http_request.s7i   0.032845            696
seed7/lib/http_srv_resp.s7i  0.032526            380
seed7/lib/https_request.s7i  0.032566            211
seed7/lib/httpserv.s7i       0.032477            345
seed7/lib/huffman.s7i        0.032213            644
seed7/lib/ico.s7i            0.033906            221
seed7/lib/idxarray.s7i       0.033676            232
seed7/lib/image.s7i          0.033357            156
seed7/lib/imagefile.s7i      0.033716            171
seed7/lib/inflate.s7i        0.032981            411
seed7/lib/inifile.s7i        0.033479            129
seed7/lib/integer.s7i        0.033594            663
seed7/lib/iobuffer.s7i       0.033483            289
seed7/lib/jpeg.s7i           0.033698           1761
seed7/lib/json.s7i           0.033733            891
seed7/lib/json_serde.s7i     0.033702            783
seed7/lib/keybd.s7i          0.033941            639
seed7/lib/keydescr.s7i       0.033848            192
seed7/lib/leb128.s7i         0.033601            218
seed7/lib/line.s7i           0.033537            164
seed7/lib/listener.s7i       0.033647            247
seed7/lib/logfile.s7i        0.033659             73
seed7/lib/lower.s7i          0.033813            142
seed7/lib/lzma.s7i           0.033710            934
seed7/lib/lzw.s7i            0.033570            861
seed7/lib/magic.s7i          0.033697            403
seed7/lib/mahjng32.s7i       0.033388           1500
seed7/lib/make.s7i           0.033414            544
seed7/lib/makedata.s7i       0.033654           1428
seed7/lib/math.s7i           0.033631            201
seed7/lib/mixarith.s7i       0.033544            249
seed7/lib/modern27.s7i       0.033041           1099
seed7/lib/more.s7i           0.032804            130
seed7/lib/msgdigest.s7i      0.033074           1222
seed7/lib/multiscr.s7i       0.032659             68
seed7/lib/null_file.s7i      0.032452            345
seed7/lib/osfiles.s7i        0.032677           1085
seed7/lib/pbm.s7i            0.034336            230
seed7/lib/pcx.s7i            0.033257            638
seed7/lib/pem.s7i            0.032610            185
seed7/lib/pgm.s7i            0.032452            238
seed7/lib/pic16.s7i          0.033360           1037
seed7/lib/pic32.s7i          0.033752           2060
seed7/lib/pic_util.s7i       0.033659            144
seed7/lib/pixelimage.s7i     0.033586            320
seed7/lib/pixmap_file.s7i    0.033512            459
seed7/lib/pixmapfont.s7i     0.033449            184
seed7/lib/pkcs1.s7i          0.033424            543
seed7/lib/png.s7i            0.033577           1064
seed7/lib/poll.s7i           0.033726            313
seed7/lib/ppm.s7i            0.034043            240
seed7/lib/process.s7i        0.033763            541
seed7/lib/progs.s7i          0.033539            789
seed7/lib/propertyfile.s7i   0.033481            155
seed7/lib/rational.s7i       0.033524            792
seed7/lib/ref_list.s7i       0.033837            252
seed7/lib/reference.s7i      0.033736            126
seed7/lib/reverse.s7i        0.033555             94
seed7/lib/rpm.s7i            0.034088           3487
seed7/lib/rpmext.s7i         0.033793            318
seed7/lib/scanfile.s7i       0.034510           1779
seed7/lib/scanjson.s7i       0.033647            413
seed7/lib/scanstri.s7i       0.033740           1814
seed7/lib/scantoml.s7i       0.033239           1603
seed7/lib/seed7_05.s7i       0.032758           1072
seed7/lib/set.s7i            0.032623             57
seed7/lib/shell.s7i          0.032501            615
seed7/lib/showtls.s7i        0.032676            678
seed7/lib/signature.s7i      0.033227            131
seed7/lib/smtp.s7i           0.033914            261
seed7/lib/sockbase.s7i       0.033646            217
seed7/lib/socket.s7i         0.033751            326
seed7/lib/sokoban1.s7i       0.033446           1519
seed7/lib/sql_base.s7i       0.033725           1000
seed7/lib/stars.s7i          0.033518           1705
seed7/lib/stdfont10.s7i      0.033665           3347
seed7/lib/stdfont12.s7i      0.033572           3928
seed7/lib/stdfont14.s7i      0.033830           4510
seed7/lib/stdfont16.s7i      0.033713           5092
seed7/lib/stdfont18.s7i      0.033433           5868
seed7/lib/stdfont20.s7i      0.034052           6449
seed7/lib/stdfont24.s7i      0.033918           7421
seed7/lib/stdfont8.s7i       0.033519           2960
seed7/lib/stdfont9.s7i       0.033880           3152
seed7/lib/stdio.s7i          0.033558            192
seed7/lib/strifile.s7i       0.033821            345
seed7/lib/string.s7i         0.033766            779
seed7/lib/stritext.s7i       0.033772            352
seed7/lib/struct.s7i         0.033455            266
seed7/lib/struct_elem.s7i    0.033529            129
seed7/lib/subfile.s7i        0.033689            174
seed7/lib/subrange.s7i       0.033740             78
seed7/lib/syntax.s7i         0.033601            294
seed7/lib/tar.s7i            0.033746           1880
seed7/lib/tar_cmds.s7i       0.033600            752
seed7/lib/tdes.s7i           0.032340            143
seed7/lib/tee.s7i            0.032449            143
seed7/lib/text.s7i           0.032706            135
seed7/lib/tga.s7i            0.032828            676
seed7/lib/tiff.s7i           0.033068           2771
seed7/lib/time.s7i           0.033372           1191
seed7/lib/tls.s7i            0.033568           2230
seed7/lib/unicode.s7i        0.033697            575
seed7/lib/unionfnd.s7i       0.033578            130
seed7/lib/upper.s7i          0.033416            142
seed7/lib/utf16.s7i          0.033608            540
seed7/lib/utf8.s7i           0.033578            234
seed7/lib/vecfont10.s7i      0.033900           1056
seed7/lib/vecfont18.s7i      0.033660           1119
seed7/lib/vector3d.s7i       0.033451            293
seed7/lib/vectorfont.s7i     0.033437            239
seed7/lib/wildcard.s7i       0.033577            140
seed7/lib/window.s7i         0.033522            455
seed7/lib/wrinum.s7i         0.033190            248
seed7/lib/x509cert.s7i       0.033686           1243
seed7/lib/xml_ent.s7i        0.033681             94
seed7/lib/xmldom.s7i         0.033423            303
seed7/lib/xz.s7i             0.033738            442
seed7/lib/zip.s7i            0.033682           2792
seed7/lib/zstd.s7i           0.033735           1263
============================ ================= ================

Statistical Summary — Mode A (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.033700        |
+-----------+-----------------+
| Minimum   | 0.032213        |
+-----------+-----------------+
| Maximum   | 0.047933        |
+-----------+-----------------+

Cross-Mode Comparison
=====================

+------+-----------------+-----------------+-----------------+
| Mode | Average (s)     | Minimum (s)     | Maximum (s)     |
+======+=================+=================+=================+
| A    | 0.033700        | 0.032213        | 0.047933        |
+------+-----------------+-----------------+-----------------+

Phase Timing Summary
====================

**Partial report: interrupted during Mode B warm-up.**

Wall-clock times include warm-up work and GC performed by the warm-up helper.

+----------+--------------+--------------+--------------+
| Phase    | Warm-up      | Timed pass   | Mode total   |
+==========+==============+==============+==============+
| Mode A   | 00:00:12.395 | 00:00:57.405 | 00:01:09.800 |
+----------+--------------+--------------+--------------+
| Total    |              |              | 00:02:15.094 |
+----------+--------------+--------------+--------------+


.. ---------------------------------------------------------------------------
