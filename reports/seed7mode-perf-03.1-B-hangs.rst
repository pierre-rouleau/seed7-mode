.. ===========================================================
.. PARTIAL REPORT — BENCHMARK WAS INTERRUPTED BY THE USER
.. Interrupted during: Mode B warm-up
.. Current file: ~/my/dvo/seed7-repos/seed7/prg/fannkuch.sd7
.. Interrupted phase elapsed so far: 00:13:58.582
.. Results below include only completed phases.
.. ===========================================================

===============================================================
PARTIAL GC-Controlled Benchmark Report: Seed7 Mode — Four Modes
===============================================================

:Running with: seed7-mode 2026-06-21T21:16:24+0000 W25-7
:Seed7 dir   : /Users/roup/my/dvo/seed7-repos/seed7/
:Started at: 15:03:00 local time
:Generated on: 2026-06-22 19:18:08 UTC
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
seed7/prg/addup.sd7          0.033194            190
seed7/prg/bas7.sd7           0.033290          11459
seed7/prg/bifurk.sd7         0.032601             73
seed7/prg/bigfiles.sd7       0.032584            129
seed7/prg/brainf7.sd7        0.033553             86
seed7/prg/calc7.sd7          0.034224            128
seed7/prg/carddemo.sd7       0.033798            190
seed7/prg/castle.sd7         0.033957           3148
seed7/prg/cat.sd7            0.033672             82
seed7/prg/cellauto.sd7       0.033905             85
seed7/prg/celsius.sd7        0.033500             42
seed7/prg/chk_all.sd7        0.033513            843
seed7/prg/chkarr.sd7         0.034154           8367
seed7/prg/chkbig.sd7         0.038085          29026
seed7/prg/chkbin.sd7         0.034332           6469
seed7/prg/chkbitdata.sd7     0.034359           6624
seed7/prg/chkbool.sd7        0.033760           3157
seed7/prg/chkbst.sd7         0.033588            722
seed7/prg/chkchr.sd7         0.034139           2809
seed7/prg/chkcmd.sd7         0.033661           1205
seed7/prg/chkdb.sd7          0.034590           7454
seed7/prg/chkdecl.sd7        0.033660            448
seed7/prg/chkenum.sd7        0.033488           1230
seed7/prg/chkerr.sd7         0.033993           4663
seed7/prg/chkexc.sd7         0.034023           2627
seed7/prg/chkfil.sd7         0.033577           1615
seed7/prg/chkflt.sd7         0.036867          20620
seed7/prg/chkhent.sd7        0.033653             54
seed7/prg/chkhsh.sd7         0.033914           4548
seed7/prg/chkidx.sd7         0.035295          19567
seed7/prg/chkint.sd7         0.038846          38129
seed7/prg/chkjson.sd7        0.033111           1764
seed7/prg/chkovf.sd7         0.033562           8216
seed7/prg/chkprc.sd7         0.033231          10111
seed7/prg/chkscan.sd7        0.033371            714
seed7/prg/chkset.sd7         0.034456          11974
seed7/prg/chkstr.sd7         0.036686          26952
seed7/prg/chktime.sd7        0.032791           2025
seed7/prg/chktoml.sd7        0.033032           1656
seed7/prg/clock.sd7          0.032709             47
seed7/prg/clock2.sd7         0.032619             43
seed7/prg/clock3.sd7         0.032947             95
seed7/prg/cmpfil.sd7         0.032374             84
seed7/prg/comanche.sd7       0.032535            180
seed7/prg/confval.sd7        0.033153            175
seed7/prg/db7.sd7            0.033487            417
seed7/prg/diff7.sd7          0.033705            263
seed7/prg/dirtst.sd7         0.033915             42
seed7/prg/dirx.sd7           0.033647            152
seed7/prg/dnafight.sd7       0.033903           1381
seed7/prg/dragon.sd7         0.033715             73
seed7/prg/echo.sd7           0.033506             39
seed7/prg/eliza.sd7          0.033550            302
seed7/prg/err.sd7            0.033601             96
seed7/prg/fannkuch.sd7       0.033345            131
seed7/prg/fib.sd7            0.033848             47
seed7/prg/find7.sd7          0.033455            133
seed7/prg/findchar.sd7       0.033640            149
seed7/prg/fractree.sd7       0.033598             55
seed7/prg/ftp7.sd7           0.033567            296
seed7/prg/ftpserv.sd7        0.033505             74
seed7/prg/gcd.sd7            0.032650            109
seed7/prg/gkbd.sd7           0.032695            358
seed7/prg/gtksvtst.sd7       0.032585             94
seed7/prg/hal.sd7            0.032765            250
seed7/prg/hamu.sd7           0.032622            573
seed7/prg/hanoi.sd7          0.035087             55
seed7/prg/hd.sd7             0.033401             79
seed7/prg/hello.sd7          0.033380             32
seed7/prg/hilbert.sd7        0.033059            108
seed7/prg/ide7.sd7           0.032700            196
seed7/prg/kbd.sd7            0.032364             49
seed7/prg/klondike.sd7       0.035366            883
seed7/prg/lander.sd7         0.038878           1551
seed7/prg/lst80bas.sd7       0.035102            344
seed7/prg/lst99bas.sd7       0.033897            401
seed7/prg/lstgwbas.sd7       0.034013            577
seed7/prg/mahjong.sd7        0.033989           1943
seed7/prg/make7.sd7          0.033793            121
seed7/prg/mandelbr.sd7       0.034155            237
seed7/prg/mind.sd7           0.033856            443
seed7/prg/mirror.sd7         0.033685            131
seed7/prg/ms.sd7             0.033733            641
seed7/prg/nicoma.sd7         0.034762            135
seed7/prg/pac.sd7            0.033992            726
seed7/prg/pairs.sd7          0.033919           2025
seed7/prg/panic.sd7          0.033804           2634
seed7/prg/percolation.sd7    0.033528            330
seed7/prg/planets.sd7        0.033730           1486
seed7/prg/portfwd7.sd7       0.033886            139
seed7/prg/prime.sd7          0.033394             74
seed7/prg/printpi1.sd7       0.033645             56
seed7/prg/printpi2.sd7       0.032937             54
seed7/prg/printpi3.sd7       0.032715             60
seed7/prg/pv7.sd7            0.032879            337
seed7/prg/queen.sd7          0.032348            149
seed7/prg/rand.sd7           0.032827            121
seed7/prg/raytrace.sd7       0.032718            538
seed7/prg/rever.sd7          0.035597            816
seed7/prg/roman.sd7          0.034002             38
seed7/prg/s7c.sd7            0.034496           9060
seed7/prg/s7check.sd7        0.033839             68
seed7/prg/savehd7.sd7        0.033661           1110
seed7/prg/self.sd7           0.033757             49
seed7/prg/shisen.sd7         0.033858           1423
seed7/prg/sl.sd7             0.033496           1029
seed7/prg/snake.sd7          0.033716            615
seed7/prg/sokoban.sd7        0.033769            891
seed7/prg/spigotpi.sd7       0.033659             64
seed7/prg/sql7.sd7           0.033524            278
seed7/prg/startrek.sd7       0.033830            979
seed7/prg/sudoku7.sd7        0.033640           2657
seed7/prg/sydir7.sd7         0.033647            384
seed7/prg/syntaxhl.sd7       0.034480            177
seed7/prg/tak.sd7            0.033659             59
seed7/prg/tar7.sd7           0.033770            121
seed7/prg/tch.sd7            0.033820             55
seed7/prg/testfont.sd7       0.033456             95
seed7/prg/tet.sd7            0.033578            479
seed7/prg/tetg.sd7           0.033736            501
seed7/prg/toutf8.sd7         0.034720            240
seed7/prg/tst_cli.sd7        0.034130             40
seed7/prg/tst_srv.sd7        0.033666             47
seed7/prg/wator.sd7          0.033709            651
seed7/prg/which.sd7          0.032783             65
seed7/prg/wiz.sd7            0.032716           2833
seed7/prg/wordcnt.sd7        0.032809             54
seed7/prg/wrinum.sd7         0.032584             43
seed7/prg/wumpus.sd7         0.032757            372
seed7/lib/aes.s7i            0.033678           1144
seed7/lib/aes_gcm.s7i        0.033877            392
seed7/lib/ar.s7i             0.033971           1532
seed7/lib/arc4.s7i           0.033509            144
seed7/lib/archive.s7i        0.033726            143
seed7/lib/archive_base.s7i   0.033797            135
seed7/lib/array.s7i          0.033892            610
seed7/lib/asn1.s7i           0.033637            544
seed7/lib/asn1oid.s7i        0.033624            157
seed7/lib/basearray.s7i      0.035372            450
seed7/lib/bigfile.s7i        0.033628            136
seed7/lib/bigint.s7i         0.033549            824
seed7/lib/bigrat.s7i         0.033797            784
seed7/lib/bin16.s7i          0.033696            592
seed7/lib/bin32.s7i          0.033581            490
seed7/lib/bin64.s7i          0.034930            539
seed7/lib/bitdata.s7i        0.034543           1330
seed7/lib/bitmapfont.s7i     0.033779            215
seed7/lib/bitset.s7i         0.034033            593
seed7/lib/bitsetof.s7i       0.033698            431
seed7/lib/blowfish.s7i       0.033703            383
seed7/lib/bmp.s7i            0.033734            924
seed7/lib/boolean.s7i        0.033758            403
seed7/lib/browser.s7i        0.033843            280
seed7/lib/bstring.s7i        0.033270            227
seed7/lib/bytedata.s7i       0.032945            482
seed7/lib/bzip2.s7i          0.033060            887
seed7/lib/cards.s7i          0.032891           1342
seed7/lib/category.s7i       0.033875            209
seed7/lib/cc_conf.s7i        0.032713           1314
seed7/lib/ccittfax.s7i       0.036492           1022
seed7/lib/cgi.s7i            0.037030            109
seed7/lib/cgidialog.s7i      0.033940           1118
seed7/lib/char.s7i           0.033988            356
seed7/lib/charsets.s7i       0.034034           2024
seed7/lib/chartype.s7i       0.033545            121
seed7/lib/cipher.s7i         0.033563            146
seed7/lib/cli_cmds.s7i       0.033480           1360
seed7/lib/clib_file.s7i      0.033289            301
seed7/lib/color.s7i          0.033585            185
seed7/lib/complex.s7i        0.033930            464
seed7/lib/compress.s7i       0.033912            150
seed7/lib/console.s7i        0.033699            188
seed7/lib/cpio.s7i           0.034081           1708
seed7/lib/crc32.s7i          0.033585            193
seed7/lib/cronos16.s7i       0.033638           1173
seed7/lib/cronos27.s7i       0.034259           1464
seed7/lib/csv.s7i            0.033733            201
seed7/lib/db_prop.s7i        0.034074            991
seed7/lib/deflate.s7i        0.033664            740
seed7/lib/des.s7i            0.033535            444
seed7/lib/dialog.s7i         0.033716            311
seed7/lib/dir.s7i            0.033616            163
seed7/lib/draw.s7i           0.033822            854
seed7/lib/duration.s7i       0.033527           1038
seed7/lib/echo.s7i           0.033773            132
seed7/lib/editline.s7i       0.032756            398
seed7/lib/elf.s7i            0.032740           1560
seed7/lib/elliptic.s7i       0.033510            649
seed7/lib/enable_io.s7i      0.032921            312
seed7/lib/encoding.s7i       0.032564            931
seed7/lib/enumeration.s7i    0.033920            236
seed7/lib/environment.s7i    0.034070            175
seed7/lib/exif.s7i           0.033507            152
seed7/lib/external_file.s7i  0.033975            340
seed7/lib/field.s7i          0.033838            268
seed7/lib/file.s7i           0.033480            372
seed7/lib/filebits.s7i       0.033741             46
seed7/lib/filesys.s7i        0.033464            601
seed7/lib/fileutil.s7i       0.033699            144
seed7/lib/fixarray.s7i       0.033517            307
seed7/lib/float.s7i          0.033634            757
seed7/lib/font.s7i           0.033445            196
seed7/lib/font8x8.s7i        0.033578            998
seed7/lib/forloop.s7i        0.033676            449
seed7/lib/ftp.s7i            0.033784            969
seed7/lib/ftpserv.s7i        0.033780            631
seed7/lib/getf.s7i           0.033474            115
seed7/lib/gethttp.s7i        0.033750             41
seed7/lib/gethttps.s7i       0.033748             41
seed7/lib/gif.s7i            0.033588            561
seed7/lib/graph.s7i          0.033704            415
seed7/lib/graph_file.s7i     0.033613            399
seed7/lib/gtkserver.s7i      0.033416            161
seed7/lib/gzip.s7i           0.033819            573
seed7/lib/hash.s7i           0.033661            421
seed7/lib/hashsetof.s7i      0.033602            499
seed7/lib/hmac.s7i           0.033782            152
seed7/lib/html.s7i           0.032614             83
seed7/lib/html_ent.s7i       0.032593            476
seed7/lib/htmldom.s7i        0.032585            286
seed7/lib/http_request.s7i   0.032695            696
seed7/lib/http_srv_resp.s7i  0.032850            380
seed7/lib/https_request.s7i  0.033732            211
seed7/lib/httpserv.s7i       0.033862            345
seed7/lib/huffman.s7i        0.033797            644
seed7/lib/ico.s7i            0.033816            221
seed7/lib/idxarray.s7i       0.033623            232
seed7/lib/image.s7i          0.033396            156
seed7/lib/imagefile.s7i      0.033713            171
seed7/lib/inflate.s7i        0.033492            411
seed7/lib/inifile.s7i        0.033783            129
seed7/lib/integer.s7i        0.033826            663
seed7/lib/iobuffer.s7i       0.033588            289
seed7/lib/jpeg.s7i           0.033797           1761
seed7/lib/json.s7i           0.033703            891
seed7/lib/json_serde.s7i     0.033907            783
seed7/lib/keybd.s7i          0.034052            639
seed7/lib/keydescr.s7i       0.033861            192
seed7/lib/leb128.s7i         0.033829            218
seed7/lib/line.s7i           0.033843            164
seed7/lib/listener.s7i       0.033647            247
seed7/lib/logfile.s7i        0.033469             73
seed7/lib/lower.s7i          0.033763            142
seed7/lib/lzma.s7i           0.033560            934
seed7/lib/lzw.s7i            0.034057            861
seed7/lib/magic.s7i          0.032812            403
seed7/lib/mahjng32.s7i       0.033819           1500
seed7/lib/make.s7i           0.033624            544
seed7/lib/makedata.s7i       0.033321           1428
seed7/lib/math.s7i           0.032710            201
seed7/lib/mixarith.s7i       0.032874            249
seed7/lib/modern27.s7i       0.032947           1099
seed7/lib/more.s7i           0.032181            130
seed7/lib/msgdigest.s7i      0.033116           1222
seed7/lib/multiscr.s7i       0.034511             68
seed7/lib/null_file.s7i      0.034259            345
seed7/lib/osfiles.s7i        0.035568           1085
seed7/lib/pbm.s7i            0.035842            230
seed7/lib/pcx.s7i            0.034244            638
seed7/lib/pem.s7i            0.034614            185
seed7/lib/pgm.s7i            0.033974            238
seed7/lib/pic16.s7i          0.033788           1037
seed7/lib/pic32.s7i          0.033750           2060
seed7/lib/pic_util.s7i       0.033744            144
seed7/lib/pixelimage.s7i     0.033631            320
seed7/lib/pixmap_file.s7i    0.033785            459
seed7/lib/pixmapfont.s7i     0.033564            184
seed7/lib/pkcs1.s7i          0.033765            543
seed7/lib/png.s7i            0.033544           1064
seed7/lib/poll.s7i           0.033741            313
seed7/lib/ppm.s7i            0.033716            240
seed7/lib/process.s7i        0.033743            541
seed7/lib/progs.s7i          0.033437            789
seed7/lib/propertyfile.s7i   0.033773            155
seed7/lib/rational.s7i       0.033884            792
seed7/lib/ref_list.s7i       0.033704            252
seed7/lib/reference.s7i      0.033703            126
seed7/lib/reverse.s7i        0.033884             94
seed7/lib/rpm.s7i            0.034598           3487
seed7/lib/rpmext.s7i         0.033734            318
seed7/lib/scanfile.s7i       0.033370           1779
seed7/lib/scanjson.s7i       0.035652            413
seed7/lib/scanstri.s7i       0.032873           1814
seed7/lib/scantoml.s7i       0.032705           1603
seed7/lib/seed7_05.s7i       0.032898           1072
seed7/lib/set.s7i            0.033552             57
seed7/lib/shell.s7i          0.033406            615
seed7/lib/showtls.s7i        0.033717            678
seed7/lib/signature.s7i      0.033905            131
seed7/lib/smtp.s7i           0.033590            261
seed7/lib/sockbase.s7i       0.033969            217
seed7/lib/socket.s7i         0.033716            326
seed7/lib/sokoban1.s7i       0.033541           1519
seed7/lib/sql_base.s7i       0.033848           1000
seed7/lib/stars.s7i          0.033808           1705
seed7/lib/stdfont10.s7i      0.033675           3347
seed7/lib/stdfont12.s7i      0.033916           3928
seed7/lib/stdfont14.s7i      0.033861           4510
seed7/lib/stdfont16.s7i      0.033894           5092
seed7/lib/stdfont18.s7i      0.034034           5868
seed7/lib/stdfont20.s7i      0.033876           6449
seed7/lib/stdfont24.s7i      0.033967           7421
seed7/lib/stdfont8.s7i       0.033864           2960
seed7/lib/stdfont9.s7i       0.033511           3152
seed7/lib/stdio.s7i          0.033627            192
seed7/lib/strifile.s7i       0.034297            345
seed7/lib/string.s7i         0.033958            779
seed7/lib/stritext.s7i       0.033976            352
seed7/lib/struct.s7i         0.033697            266
seed7/lib/struct_elem.s7i    0.033622            129
seed7/lib/subfile.s7i        0.033673            174
seed7/lib/subrange.s7i       0.033307             78
seed7/lib/syntax.s7i         0.032765            294
seed7/lib/tar.s7i            0.032756           1880
seed7/lib/tar_cmds.s7i       0.032820            752
seed7/lib/tdes.s7i           0.032785            143
seed7/lib/tee.s7i            0.034103            143
seed7/lib/text.s7i           0.033784            135
seed7/lib/tga.s7i            0.033822            676
seed7/lib/tiff.s7i           0.033865           2771
seed7/lib/time.s7i           0.033677           1191
seed7/lib/tls.s7i            0.033462           2230
seed7/lib/unicode.s7i        0.033823            575
seed7/lib/unionfnd.s7i       0.033680            130
seed7/lib/upper.s7i          0.033586            142
seed7/lib/utf16.s7i          0.033715            540
seed7/lib/utf8.s7i           0.033539            234
seed7/lib/vecfont10.s7i      0.033760           1056
seed7/lib/vecfont18.s7i      0.033704           1119
seed7/lib/vector3d.s7i       0.033676            293
seed7/lib/vectorfont.s7i     0.033930            239
seed7/lib/wildcard.s7i       0.033529            140
seed7/lib/window.s7i         0.033608            455
seed7/lib/wrinum.s7i         0.033729            248
seed7/lib/x509cert.s7i       0.034027           1243
seed7/lib/xml_ent.s7i        0.033904             94
seed7/lib/xmldom.s7i         0.033380            303
seed7/lib/xz.s7i             0.033557            442
seed7/lib/zip.s7i            0.033904           2792
seed7/lib/zstd.s7i           0.033401           1263
============================ ================= ================

Statistical Summary — Mode A (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.033719        |
+-----------+-----------------+
| Minimum   | 0.032181        |
+-----------+-----------------+
| Maximum   | 0.038878        |
+-----------+-----------------+

Cross-Mode Comparison
=====================

+------+-----------------+-----------------+-----------------+
| Mode | Average (s)     | Minimum (s)     | Maximum (s)     |
+======+=================+=================+=================+
| A    | 0.033719        | 0.032181        | 0.038878        |
+------+-----------------+-----------------+-----------------+

Phase Timing Summary
====================

**Partial report: interrupted during Mode B warm-up.**

Wall-clock times include warm-up work and GC performed by the warm-up helper.

+----------+--------------+--------------+--------------+
| Phase    | Warm-up      | Timed pass   | Mode total   |
+==========+==============+==============+==============+
| Mode A   | 00:00:12.191 | 00:00:57.438 | 00:01:09.630 |
+----------+--------------+--------------+--------------+
| Total    |              |              | 00:15:08.215 |
+----------+--------------+--------------+--------------+


.. ---------------------------------------------------------------------------
