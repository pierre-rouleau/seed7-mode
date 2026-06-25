=======================================================
GC-Controlled Benchmark Report: Seed7 Mode — Four Modes
=======================================================

:Running with: seed7-mode 2026-06-24T17:02:38+0000 W26-3
:Seed7 dir   : /Users/roup/my/dvo/seed7-repos/seed7/
:Started at: 13:10:54 local time
:Generated on: 2026-06-24 17:22:11 UTC
:N Iterations: 5  (mean of N timed opens per file)
:Window system: nil
:Display graphic: nil
:Frame size: 226x84 chars
:Window body: 226x82 chars
:GC @ testing: suppressed (gc-cons-threshold = most-positive-fixnum)
:Warm-up info: yes (1 untimed pass per mode + garbage-collect before timing)
:Modes planned: A B C D

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
seed7/prg/addup.sd7          0.038476            190
seed7/prg/bas7.sd7           0.037397          11459
seed7/prg/bifurk.sd7         0.035129             73
seed7/prg/bigfiles.sd7       0.037488            129
seed7/prg/brainf7.sd7        0.039315             86
seed7/prg/calc7.sd7          0.037047            128
seed7/prg/carddemo.sd7       0.038673            190
seed7/prg/castle.sd7         0.037708           3148
seed7/prg/cat.sd7            0.033860             82
seed7/prg/cellauto.sd7       0.035135             85
seed7/prg/celsius.sd7        0.034116             42
seed7/prg/chk_all.sd7        0.033522            843
seed7/prg/chkarr.sd7         0.034427           8367
seed7/prg/chkbig.sd7         0.037854          29026
seed7/prg/chkbin.sd7         0.034833           6469
seed7/prg/chkbitdata.sd7     0.035195           6624
seed7/prg/chkbool.sd7        0.034736           3157
seed7/prg/chkbst.sd7         0.033640            722
seed7/prg/chkchr.sd7         0.034786           2809
seed7/prg/chkcmd.sd7         0.038468           1205
seed7/prg/chkdb.sd7          0.036233           7454
seed7/prg/chkdecl.sd7        0.034669            448
seed7/prg/chkenum.sd7        0.033473           1230
seed7/prg/chkerr.sd7         0.033191           4663
seed7/prg/chkexc.sd7         0.033014           2627
seed7/prg/chkfil.sd7         0.032889           1615
seed7/prg/chkflt.sd7         0.037614          20620
seed7/prg/chkhent.sd7        0.033299             54
seed7/prg/chkhsh.sd7         0.037203           4548
seed7/prg/chkidx.sd7         0.039206          19567
seed7/prg/chkint.sd7         0.040483          38129
seed7/prg/chkjson.sd7        0.034065           1764
seed7/prg/chkovf.sd7         0.034823           8216
seed7/prg/chkprc.sd7         0.034055          10111
seed7/prg/chkscan.sd7        0.033387            714
seed7/prg/chkset.sd7         0.037497          11974
seed7/prg/chkstr.sd7         0.040727          26952
seed7/prg/chktime.sd7        0.034337           2025
seed7/prg/chktoml.sd7        0.033581           1656
seed7/prg/clock.sd7          0.033305             47
seed7/prg/clock2.sd7         0.033468             43
seed7/prg/clock3.sd7         0.034909             95
seed7/prg/cmpfil.sd7         0.040240             84
seed7/prg/comanche.sd7       0.041455            180
seed7/prg/confval.sd7        0.040830            175
seed7/prg/db7.sd7            0.036299            417
seed7/prg/diff7.sd7          0.033362            263
seed7/prg/dirtst.sd7         0.033497             42
seed7/prg/dirx.sd7           0.033470            152
seed7/prg/dnafight.sd7       0.033465           1381
seed7/prg/dragon.sd7         0.033386             73
seed7/prg/echo.sd7           0.032811             39
seed7/prg/eliza.sd7          0.032761            302
seed7/prg/err.sd7            0.033024             96
seed7/prg/fannkuch.sd7       0.035972            131
seed7/prg/fib.sd7            0.035568             47
seed7/prg/find7.sd7          0.032780            133
seed7/prg/findchar.sd7       0.035333            149
seed7/prg/fractree.sd7       0.033583             55
seed7/prg/ftp7.sd7           0.037172            296
seed7/prg/ftpserv.sd7        0.035058             74
seed7/prg/gcd.sd7            0.036854            109
seed7/prg/gkbd.sd7           0.035208            358
seed7/prg/gtksvtst.sd7       0.034745             94
seed7/prg/hal.sd7            0.036249            250
seed7/prg/hamu.sd7           0.034760            573
seed7/prg/hanoi.sd7          0.034631             55
seed7/prg/hd.sd7             0.033715             79
seed7/prg/hello.sd7          0.034005             32
seed7/prg/hilbert.sd7        0.034058            108
seed7/prg/ide7.sd7           0.034960            196
seed7/prg/kbd.sd7            0.033782             49
seed7/prg/klondike.sd7       0.033425            883
seed7/prg/lander.sd7         0.033664           1551
seed7/prg/lst80bas.sd7       0.033400            344
seed7/prg/lst99bas.sd7       0.033903            401
seed7/prg/lstgwbas.sd7       0.035291            577
seed7/prg/mahjong.sd7        0.036493           1943
seed7/prg/make7.sd7          0.035124            121
seed7/prg/mandelbr.sd7       0.035446            237
seed7/prg/mind.sd7           0.034222            443
seed7/prg/mirror.sd7         0.034656            131
seed7/prg/ms.sd7             0.033657            641
seed7/prg/nicoma.sd7         0.033414            135
seed7/prg/pac.sd7            0.032746            726
seed7/prg/pairs.sd7          0.032586           2025
seed7/prg/panic.sd7          0.032895           2634
seed7/prg/percolation.sd7    0.033377            330
seed7/prg/planets.sd7        0.036507           1486
seed7/prg/portfwd7.sd7       0.034190            139
seed7/prg/prime.sd7          0.033610             74
seed7/prg/printpi1.sd7       0.033622             56
seed7/prg/printpi2.sd7       0.034082             54
seed7/prg/printpi3.sd7       0.034604             60
seed7/prg/pv7.sd7            0.034793            337
seed7/prg/queen.sd7          0.034616            149
seed7/prg/rand.sd7           0.033529            121
seed7/prg/raytrace.sd7       0.033727            538
seed7/prg/rever.sd7          0.033723            816
seed7/prg/roman.sd7          0.033534             38
seed7/prg/s7c.sd7            0.034809           9060
seed7/prg/s7check.sd7        0.033840             68
seed7/prg/savehd7.sd7        0.033653           1110
seed7/prg/self.sd7           0.033481             49
seed7/prg/shisen.sd7         0.033906           1423
seed7/prg/sl.sd7             0.034474           1029
seed7/prg/snake.sd7          0.035385            615
seed7/prg/sokoban.sd7        0.034076            891
seed7/prg/spigotpi.sd7       0.033458             64
seed7/prg/sql7.sd7           0.033620            278
seed7/prg/startrek.sd7       0.033634            979
seed7/prg/sudoku7.sd7        0.033980           2657
seed7/prg/sydir7.sd7         0.034687            384
seed7/prg/syntaxhl.sd7       0.034866            177
seed7/prg/tak.sd7            0.032864             59
seed7/prg/tar7.sd7           0.032717            121
seed7/prg/tch.sd7            0.033117             55
seed7/prg/testfont.sd7       0.032524             95
seed7/prg/tet.sd7            0.032489            479
seed7/prg/tetg.sd7           0.035685            501
seed7/prg/toutf8.sd7         0.033921            240
seed7/prg/tst_cli.sd7        0.033590             40
seed7/prg/tst_srv.sd7        0.033435             47
seed7/prg/wator.sd7          0.034092            651
seed7/prg/which.sd7          0.033869             65
seed7/prg/wiz.sd7            0.040184           2833
seed7/prg/wordcnt.sd7        0.035929             54
seed7/prg/wrinum.sd7         0.034456             43
seed7/prg/wumpus.sd7         0.034509            372
seed7/lib/aes.s7i            0.033840           1144
seed7/lib/aes_gcm.s7i        0.033750            392
seed7/lib/ar.s7i             0.034819           1532
seed7/lib/arc4.s7i           0.034356            144
seed7/lib/archive.s7i        0.033800            143
seed7/lib/archive_base.s7i   0.033412            135
seed7/lib/array.s7i          0.034536            610
seed7/lib/asn1.s7i           0.033881            544
seed7/lib/asn1oid.s7i        0.033778            157
seed7/lib/basearray.s7i      0.034238            450
seed7/lib/bigfile.s7i        0.033712            136
seed7/lib/bigint.s7i         0.038036            824
seed7/lib/bigrat.s7i         0.038948            784
seed7/lib/bin16.s7i          0.034459            592
seed7/lib/bin32.s7i          0.034069            490
seed7/lib/bin64.s7i          0.033282            539
seed7/lib/bitdata.s7i        0.032695           1330
seed7/lib/bitmapfont.s7i     0.032682            215
seed7/lib/bitset.s7i         0.033105            593
seed7/lib/bitsetof.s7i       0.032580            431
seed7/lib/blowfish.s7i       0.033559            383
seed7/lib/bmp.s7i            0.035731            924
seed7/lib/boolean.s7i        0.033544            403
seed7/lib/browser.s7i        0.033883            280
seed7/lib/bstring.s7i        0.034155            227
seed7/lib/bytedata.s7i       0.033910            482
seed7/lib/bzip2.s7i          0.033570            887
seed7/lib/cards.s7i          0.034608           1342
seed7/lib/category.s7i       0.033675            209
seed7/lib/cc_conf.s7i        0.033702           1314
seed7/lib/ccittfax.s7i       0.035140           1022
seed7/lib/cgi.s7i            0.034591            109
seed7/lib/cgidialog.s7i      0.035944           1118
seed7/lib/char.s7i           0.033872            356
seed7/lib/charsets.s7i       0.035879           2024
seed7/lib/chartype.s7i       0.034193            121
seed7/lib/cipher.s7i         0.033657            146
seed7/lib/cli_cmds.s7i       0.034776           1360
seed7/lib/clib_file.s7i      0.039642            301
seed7/lib/color.s7i          0.034133            185
seed7/lib/complex.s7i        0.038660            464
seed7/lib/compress.s7i       0.035349            150
seed7/lib/console.s7i        0.034517            188
seed7/lib/cpio.s7i           0.033517           1708
seed7/lib/crc32.s7i          0.033485            193
seed7/lib/cronos16.s7i       0.033586           1173
seed7/lib/cronos27.s7i       0.034768           1464
seed7/lib/csv.s7i            0.033668            201
seed7/lib/db_prop.s7i        0.033447            991
seed7/lib/deflate.s7i        0.033378            740
seed7/lib/des.s7i            0.032729            444
seed7/lib/dialog.s7i         0.033820            311
seed7/lib/dir.s7i            0.035274            163
seed7/lib/draw.s7i           0.034068            854
seed7/lib/duration.s7i       0.034965           1038
seed7/lib/echo.s7i           0.035181            132
seed7/lib/editline.s7i       0.033663            398
seed7/lib/elf.s7i            0.033407           1560
seed7/lib/elliptic.s7i       0.034361            649
seed7/lib/enable_io.s7i      0.034119            312
seed7/lib/encoding.s7i       0.033604            931
seed7/lib/enumeration.s7i    0.035940            236
seed7/lib/environment.s7i    0.033363            175
seed7/lib/exif.s7i           0.033163            152
seed7/lib/external_file.s7i  0.033573            340
seed7/lib/field.s7i          0.034892            268
seed7/lib/file.s7i           0.033878            372
seed7/lib/filebits.s7i       0.035211             46
seed7/lib/filesys.s7i        0.033820            601
seed7/lib/fileutil.s7i       0.033802            144
seed7/lib/fixarray.s7i       0.033741            307
seed7/lib/float.s7i          0.035928            757
seed7/lib/font.s7i           0.033835            196
seed7/lib/font8x8.s7i        0.034232            998
seed7/lib/forloop.s7i        0.033701            449
seed7/lib/ftp.s7i            0.033554            969
seed7/lib/ftpserv.s7i        0.033569            631
seed7/lib/getf.s7i           0.035208            115
seed7/lib/gethttp.s7i        0.033327             41
seed7/lib/gethttps.s7i       0.034111             41
seed7/lib/gif.s7i            0.035114            561
seed7/lib/graph.s7i          0.032846            415
seed7/lib/graph_file.s7i     0.032542            399
seed7/lib/gtkserver.s7i      0.036615            161
seed7/lib/gzip.s7i           0.034504            573
seed7/lib/hash.s7i           0.034041            421
seed7/lib/hashsetof.s7i      0.033874            499
seed7/lib/hmac.s7i           0.033678            152
seed7/lib/html.s7i           0.033587             83
seed7/lib/html_ent.s7i       0.034933            476
seed7/lib/htmldom.s7i        0.033621            286
seed7/lib/http_request.s7i   0.035201            696
seed7/lib/http_srv_resp.s7i  0.034363            380
seed7/lib/https_request.s7i  0.033666            211
seed7/lib/httpserv.s7i       0.033785            345
seed7/lib/huffman.s7i        0.034174            644
seed7/lib/ico.s7i            0.034788            221
seed7/lib/idxarray.s7i       0.034615            232
seed7/lib/image.s7i          0.033923            156
seed7/lib/imagefile.s7i      0.033607            171
seed7/lib/inflate.s7i        0.033810            411
seed7/lib/inifile.s7i        0.034132            129
seed7/lib/integer.s7i        0.034160            663
seed7/lib/iobuffer.s7i       0.035391            289
seed7/lib/jpeg.s7i           0.034635           1761
seed7/lib/json.s7i           0.033581            891
seed7/lib/json_serde.s7i     0.033582            783
seed7/lib/keybd.s7i          0.034755            639
seed7/lib/keydescr.s7i       0.035050            192
seed7/lib/leb128.s7i         0.032729            218
seed7/lib/line.s7i           0.033680            164
seed7/lib/listener.s7i       0.033119            247
seed7/lib/logfile.s7i        0.033186             73
seed7/lib/lower.s7i          0.033060            142
seed7/lib/lzma.s7i           0.034952            934
seed7/lib/lzw.s7i            0.035146            861
seed7/lib/magic.s7i          0.035885            403
seed7/lib/mahjng32.s7i       0.033972           1500
seed7/lib/make.s7i           0.033651            544
seed7/lib/makedata.s7i       0.034766           1428
seed7/lib/math.s7i           0.035007            201
seed7/lib/mixarith.s7i       0.035254            249
seed7/lib/modern27.s7i       0.034823           1099
seed7/lib/more.s7i           0.035820            130
seed7/lib/msgdigest.s7i      0.033957           1222
seed7/lib/multiscr.s7i       0.033602             68
seed7/lib/null_file.s7i      0.035050            345
seed7/lib/osfiles.s7i        0.034132           1085
seed7/lib/pbm.s7i            0.033942            230
seed7/lib/pcx.s7i            0.033703            638
seed7/lib/pem.s7i            0.033699            185
seed7/lib/pgm.s7i            0.034249            238
seed7/lib/pic16.s7i          0.035136           1037
seed7/lib/pic32.s7i          0.034371           2060
seed7/lib/pic_util.s7i       0.033815            144
seed7/lib/pixelimage.s7i     0.033900            320
seed7/lib/pixmap_file.s7i    0.033489            459
seed7/lib/pixmapfont.s7i     0.033018            184
seed7/lib/pkcs1.s7i          0.033605            543
seed7/lib/png.s7i            0.033475           1064
seed7/lib/poll.s7i           0.034469            313
seed7/lib/ppm.s7i            0.036409            240
seed7/lib/process.s7i        0.033626            541
seed7/lib/progs.s7i          0.036544            789
seed7/lib/propertyfile.s7i   0.035751            155
seed7/lib/rational.s7i       0.033747            792
seed7/lib/ref_list.s7i       0.034102            252
seed7/lib/reference.s7i      0.034500            126
seed7/lib/reverse.s7i        0.033682             94
seed7/lib/rpm.s7i            0.033723           3487
seed7/lib/rpmext.s7i         0.035007            318
seed7/lib/scanfile.s7i       0.034205           1779
seed7/lib/scanjson.s7i       0.033941            413
seed7/lib/scanstri.s7i       0.034833           1814
seed7/lib/scantoml.s7i       0.035055           1603
seed7/lib/seed7_05.s7i       0.037228           1072
seed7/lib/set.s7i            0.035660             57
seed7/lib/shell.s7i          0.034832            615
seed7/lib/showtls.s7i        0.033893            678
seed7/lib/signature.s7i      0.037063            131
seed7/lib/smtp.s7i           0.034017            261
seed7/lib/sockbase.s7i       0.034516            217
seed7/lib/socket.s7i         0.034749            326
seed7/lib/sokoban1.s7i       0.033845           1519
seed7/lib/sql_base.s7i       0.033738           1000
seed7/lib/stars.s7i          0.034157           1705
seed7/lib/stdfont10.s7i      0.037065           3347
seed7/lib/stdfont12.s7i      0.040374           3928
seed7/lib/stdfont14.s7i      0.036963           4510
seed7/lib/stdfont16.s7i      0.034066           5092
seed7/lib/stdfont18.s7i      0.033395           5868
seed7/lib/stdfont20.s7i      0.033515           6449
seed7/lib/stdfont24.s7i      0.036935           7421
seed7/lib/stdfont8.s7i       0.033958           2960
seed7/lib/stdfont9.s7i       0.038783           3152
seed7/lib/stdio.s7i          0.034580            192
seed7/lib/strifile.s7i       0.034561            345
seed7/lib/string.s7i         0.034800            779
seed7/lib/stritext.s7i       0.033637            352
seed7/lib/struct.s7i         0.033589            266
seed7/lib/struct_elem.s7i    0.034394            129
seed7/lib/subfile.s7i        0.033653            174
seed7/lib/subrange.s7i       0.034498             78
seed7/lib/syntax.s7i         0.033850            294
seed7/lib/tar.s7i            0.033616           1880
seed7/lib/tar_cmds.s7i       0.033572            752
seed7/lib/tdes.s7i           0.035120            143
seed7/lib/tee.s7i            0.033835            143
seed7/lib/text.s7i           0.033819            135
seed7/lib/tga.s7i            0.034087            676
seed7/lib/tiff.s7i           0.034265           2771
seed7/lib/time.s7i           0.033914           1191
seed7/lib/tls.s7i            0.035265           2230
seed7/lib/unicode.s7i        0.033946            575
seed7/lib/unionfnd.s7i       0.033799            130
seed7/lib/upper.s7i          0.034114            142
seed7/lib/utf16.s7i          0.033571            540
seed7/lib/utf8.s7i           0.032624            234
seed7/lib/vecfont10.s7i      0.033767           1056
seed7/lib/vecfont18.s7i      0.033265           1119
seed7/lib/vector3d.s7i       0.032711            293
seed7/lib/vectorfont.s7i     0.033421            239
seed7/lib/wildcard.s7i       0.033790            140
seed7/lib/window.s7i         0.035860            455
seed7/lib/wrinum.s7i         0.035445            248
seed7/lib/x509cert.s7i       0.034028           1243
seed7/lib/xml_ent.s7i        0.033622             94
seed7/lib/xmldom.s7i         0.035751            303
seed7/lib/xz.s7i             0.034172            442
seed7/lib/zip.s7i            0.033954           2792
seed7/lib/zstd.s7i           0.035221           1263
============================ ================= ================

Statistical Summary — Mode A (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.034553        |
+-----------+-----------------+
| Minimum   | 0.032489        |
+-----------+-----------------+
| Maximum   | 0.041455        |
+-----------+-----------------+

Mode B — Mode Activation + Initial Visible jit-lock Pass
--------------------------------------------------------

The buffer is displayed in a window, then `jit-lock-fontify-now` is called
over `(window-start)` to `(window-end ... t)` to fontify the visible region.
This avoids `sit-for 0`, which can process queued terminal input events.

File Load Times — Mode B (mean, GC-free)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

============================ ================= ================
File Name                    Mean Time (s)     Line count
============================ ================= ================
seed7/prg/addup.sd7          0.048607            190
seed7/prg/bas7.sd7           0.041931          11459
seed7/prg/bifurk.sd7         0.037029             73
seed7/prg/bigfiles.sd7       0.038974            129
seed7/prg/brainf7.sd7        0.038170             86
seed7/prg/calc7.sd7          0.039103            128
seed7/prg/carddemo.sd7       0.038918            190
seed7/prg/castle.sd7         0.039299           3148
seed7/prg/cat.sd7            0.037317             82
seed7/prg/cellauto.sd7       0.037356             85
seed7/prg/celsius.sd7        0.035887             42
seed7/prg/chk_all.sd7        0.038382            843
seed7/prg/chkarr.sd7         0.038856           8367
seed7/prg/chkbig.sd7         0.041874          29026
seed7/prg/chkbin.sd7         0.037867           6469
seed7/prg/chkbitdata.sd7     0.041092           6624
seed7/prg/chkbool.sd7        0.038292           3157
seed7/prg/chkbst.sd7         0.038024            722
seed7/prg/chkchr.sd7         0.038690           2809
seed7/prg/chkcmd.sd7         0.036105           1205
seed7/prg/chkdb.sd7          0.038790           7454
seed7/prg/chkdecl.sd7        0.038019            448
seed7/prg/chkenum.sd7        0.036736           1230
seed7/prg/chkerr.sd7         0.038638           4663
seed7/prg/chkexc.sd7         0.037574           2627
seed7/prg/chkfil.sd7         0.039596           1615
seed7/prg/chkflt.sd7         0.041485          20620
seed7/prg/chkhent.sd7        0.036675             54
seed7/prg/chkhsh.sd7         0.039469           4548
seed7/prg/chkidx.sd7         0.041407          19567
seed7/prg/chkint.sd7         0.044618          38129
seed7/prg/chkjson.sd7        0.039152           1764
seed7/prg/chkovf.sd7         0.038767           8216
seed7/prg/chkprc.sd7         0.038925          10111
seed7/prg/chkscan.sd7        0.039304            714
seed7/prg/chkset.sd7         0.040678          11974
seed7/prg/chkstr.sd7         0.043067          26952
seed7/prg/chktime.sd7        0.047366           2025
seed7/prg/chktoml.sd7        0.043536           1656
seed7/prg/clock.sd7          0.037258             47
seed7/prg/clock2.sd7         0.036340             43
seed7/prg/clock3.sd7         0.038954             95
seed7/prg/cmpfil.sd7         0.038130             84
seed7/prg/comanche.sd7       0.039484            180
seed7/prg/confval.sd7        0.040360            175
seed7/prg/db7.sd7            0.039337            417
seed7/prg/diff7.sd7          0.038149            263
seed7/prg/dirtst.sd7         0.034768             42
seed7/prg/dirx.sd7           0.040015            152
seed7/prg/dnafight.sd7       0.038317           1381
seed7/prg/dragon.sd7         0.036837             73
seed7/prg/echo.sd7           0.036774             39
seed7/prg/eliza.sd7          0.038806            302
seed7/prg/err.sd7            0.043656             96
seed7/prg/fannkuch.sd7       0.041761            131
seed7/prg/fib.sd7            0.035278             47
seed7/prg/find7.sd7          0.038515            133
seed7/prg/findchar.sd7       0.038648            149
seed7/prg/fractree.sd7       0.036836             55
seed7/prg/ftp7.sd7           0.038715            296
seed7/prg/ftpserv.sd7        0.038166             74
seed7/prg/gcd.sd7            0.037591            109
seed7/prg/gkbd.sd7           0.039252            358
seed7/prg/gtksvtst.sd7       0.037774             94
seed7/prg/hal.sd7            0.037897            250
seed7/prg/hamu.sd7           0.038002            573
seed7/prg/hanoi.sd7          0.036180             55
seed7/prg/hd.sd7             0.037389             79
seed7/prg/hello.sd7          0.035695             32
seed7/prg/hilbert.sd7        0.038028            108
seed7/prg/ide7.sd7           0.038678            196
seed7/prg/kbd.sd7            0.036689             49
seed7/prg/klondike.sd7       0.039639            883
seed7/prg/lander.sd7         0.039577           1551
seed7/prg/lst80bas.sd7       0.036887            344
seed7/prg/lst99bas.sd7       0.036906            401
seed7/prg/lstgwbas.sd7       0.037783            577
seed7/prg/mahjong.sd7        0.037782           1943
seed7/prg/make7.sd7          0.037417            121
seed7/prg/mandelbr.sd7       0.039427            237
seed7/prg/mind.sd7           0.039598            443
seed7/prg/mirror.sd7         0.039025            131
seed7/prg/ms.sd7             0.038517            641
seed7/prg/nicoma.sd7         0.038920            135
seed7/prg/pac.sd7            0.038123            726
seed7/prg/pairs.sd7          0.039072           2025
seed7/prg/panic.sd7          0.039808           2634
seed7/prg/percolation.sd7    0.038865            330
seed7/prg/planets.sd7        0.039515           1486
seed7/prg/portfwd7.sd7       0.038341            139
seed7/prg/prime.sd7          0.037085             74
seed7/prg/printpi1.sd7       0.036873             56
seed7/prg/printpi2.sd7       0.036463             54
seed7/prg/printpi3.sd7       0.036879             60
seed7/prg/pv7.sd7            0.038560            337
seed7/prg/queen.sd7          0.037419            149
seed7/prg/rand.sd7           0.038568            121
seed7/prg/raytrace.sd7       0.039268            538
seed7/prg/rever.sd7          0.038857            816
seed7/prg/roman.sd7          0.035394             38
seed7/prg/s7c.sd7            0.038988           9060
seed7/prg/s7check.sd7        0.036944             68
seed7/prg/savehd7.sd7        0.038139           1110
seed7/prg/self.sd7           0.035633             49
seed7/prg/shisen.sd7         0.037897           1423
seed7/prg/sl.sd7             0.036415           1029
seed7/prg/snake.sd7          0.036624            615
seed7/prg/sokoban.sd7        0.039733            891
seed7/prg/spigotpi.sd7       0.037103             64
seed7/prg/sql7.sd7           0.039100            278
seed7/prg/startrek.sd7       0.038587            979
seed7/prg/sudoku7.sd7        0.041013           2657
seed7/prg/sydir7.sd7         0.047306            384
seed7/prg/syntaxhl.sd7       0.044959            177
seed7/prg/tak.sd7            0.038202             59
seed7/prg/tar7.sd7           0.039523            121
seed7/prg/tch.sd7            0.036896             55
seed7/prg/testfont.sd7       0.039220             95
seed7/prg/tet.sd7            0.041155            479
seed7/prg/tetg.sd7           0.039232            501
seed7/prg/toutf8.sd7         0.039291            240
seed7/prg/tst_cli.sd7        0.036284             40
seed7/prg/tst_srv.sd7        0.036512             47
seed7/prg/wator.sd7          0.038681            651
seed7/prg/which.sd7          0.037289             65
seed7/prg/wiz.sd7            0.038851           2833
seed7/prg/wordcnt.sd7        0.036657             54
seed7/prg/wrinum.sd7         0.036247             43
seed7/prg/wumpus.sd7         0.040622            372
seed7/lib/aes.s7i            0.040908           1144
seed7/lib/aes_gcm.s7i        0.039458            392
seed7/lib/ar.s7i             0.037988           1532
seed7/lib/arc4.s7i           0.037745            144
seed7/lib/archive.s7i        0.037444            143
seed7/lib/archive_base.s7i   0.039845            135
seed7/lib/array.s7i          0.039082            610
seed7/lib/asn1.s7i           0.037604            544
seed7/lib/asn1oid.s7i        0.041042            157
seed7/lib/basearray.s7i      0.039303            450
seed7/lib/bigfile.s7i        0.038531            136
seed7/lib/bigint.s7i         0.038722            824
seed7/lib/bigrat.s7i         0.038725            784
seed7/lib/bin16.s7i          0.039176            592
seed7/lib/bin32.s7i          0.039078            490
seed7/lib/bin64.s7i          0.039009            539
seed7/lib/bitdata.s7i        0.042890           1330
seed7/lib/bitmapfont.s7i     0.039525            215
seed7/lib/bitset.s7i         0.039212            593
seed7/lib/bitsetof.s7i       0.039301            431
seed7/lib/blowfish.s7i       0.042222            383
seed7/lib/bmp.s7i            0.039634            924
seed7/lib/boolean.s7i        0.041692            403
seed7/lib/browser.s7i        0.043885            280
seed7/lib/bstring.s7i        0.040949            227
seed7/lib/bytedata.s7i       0.038687            482
seed7/lib/bzip2.s7i          0.038454            887
seed7/lib/cards.s7i          0.036394           1342
seed7/lib/category.s7i       0.037530            209
seed7/lib/cc_conf.s7i        0.038146           1314
seed7/lib/ccittfax.s7i       0.040267           1022
seed7/lib/cgi.s7i            0.038500            109
seed7/lib/cgidialog.s7i      0.038972           1118
seed7/lib/char.s7i           0.039051            356
seed7/lib/charsets.s7i       0.039690           2024
seed7/lib/chartype.s7i       0.041832            121
seed7/lib/cipher.s7i         0.038670            146
seed7/lib/cli_cmds.s7i       0.038921           1360
seed7/lib/clib_file.s7i      0.038714            301
seed7/lib/color.s7i          0.040011            185
seed7/lib/complex.s7i        0.037617            464
seed7/lib/compress.s7i       0.039271            150
seed7/lib/console.s7i        0.038736            188
seed7/lib/cpio.s7i           0.038976           1708
seed7/lib/crc32.s7i          0.040566            193
seed7/lib/cronos16.s7i       0.042039           1173
seed7/lib/cronos27.s7i       0.040699           1464
seed7/lib/csv.s7i            0.038628            201
seed7/lib/db_prop.s7i        0.040200            991
seed7/lib/deflate.s7i        0.039843            740
seed7/lib/des.s7i            0.045705            444
seed7/lib/dialog.s7i         0.047338            311
seed7/lib/dir.s7i            0.038963            163
seed7/lib/draw.s7i           0.038329            854
seed7/lib/duration.s7i       0.041277           1038
seed7/lib/echo.s7i           0.045514            132
seed7/lib/editline.s7i       0.039712            398
seed7/lib/elf.s7i            0.039290           1560
seed7/lib/elliptic.s7i       0.037929            649
seed7/lib/enable_io.s7i      0.038834            312
seed7/lib/encoding.s7i       0.039795            931
seed7/lib/enumeration.s7i    0.039072            236
seed7/lib/environment.s7i    0.038726            175
seed7/lib/exif.s7i           0.039067            152
seed7/lib/external_file.s7i  0.039392            340
seed7/lib/field.s7i          0.039016            268
seed7/lib/file.s7i           0.038791            372
seed7/lib/filebits.s7i       0.037215             46
seed7/lib/filesys.s7i        0.043205            601
seed7/lib/fileutil.s7i       0.039172            144
seed7/lib/fixarray.s7i       0.041730            307
seed7/lib/float.s7i          0.042176            757
seed7/lib/font.s7i           0.038653            196
seed7/lib/font8x8.s7i        0.038064            998
seed7/lib/forloop.s7i        0.039196            449
seed7/lib/ftp.s7i            0.038152            969
seed7/lib/ftpserv.s7i        0.037889            631
seed7/lib/getf.s7i           0.037672            115
seed7/lib/gethttp.s7i        0.035636             41
seed7/lib/gethttps.s7i       0.035853             41
seed7/lib/gif.s7i            0.039743            561
seed7/lib/graph.s7i          0.041920            415
seed7/lib/graph_file.s7i     0.039553            399
seed7/lib/gtkserver.s7i      0.038872            161
seed7/lib/gzip.s7i           0.039106            573
seed7/lib/hash.s7i           0.040876            421
seed7/lib/hashsetof.s7i      0.042575            499
seed7/lib/hmac.s7i           0.038887            152
seed7/lib/html.s7i           0.037841             83
seed7/lib/html_ent.s7i       0.039472            476
seed7/lib/htmldom.s7i        0.038851            286
seed7/lib/http_request.s7i   0.039420            696
seed7/lib/http_srv_resp.s7i  0.039039            380
seed7/lib/https_request.s7i  0.038815            211
seed7/lib/httpserv.s7i       0.038821            345
seed7/lib/huffman.s7i        0.039259            644
seed7/lib/ico.s7i            0.041681            221
seed7/lib/idxarray.s7i       0.044626            232
seed7/lib/image.s7i          0.040124            156
seed7/lib/imagefile.s7i      0.039655            171
seed7/lib/inflate.s7i        0.039022            411
seed7/lib/inifile.s7i        0.038600            129
seed7/lib/integer.s7i        0.037962            663
seed7/lib/iobuffer.s7i       0.038071            289
seed7/lib/jpeg.s7i           0.038061           1761
seed7/lib/json.s7i           0.037545            891
seed7/lib/json_serde.s7i     0.041265            783
seed7/lib/keybd.s7i          0.038860            639
seed7/lib/keydescr.s7i       0.039128            192
seed7/lib/leb128.s7i         0.039146            218
seed7/lib/line.s7i           0.037455            164
seed7/lib/listener.s7i       0.039100            247
seed7/lib/logfile.s7i        0.039666             73
seed7/lib/lower.s7i          0.038536            142
seed7/lib/lzma.s7i           0.040030            934
seed7/lib/lzw.s7i            0.039261            861
seed7/lib/magic.s7i          0.039377            403
seed7/lib/mahjng32.s7i       0.039571           1500
seed7/lib/make.s7i           0.039032            544
seed7/lib/makedata.s7i       0.039374           1428
seed7/lib/math.s7i           0.039131            201
seed7/lib/mixarith.s7i       0.039129            249
seed7/lib/modern27.s7i       0.040227           1099
seed7/lib/more.s7i           0.038738            130
seed7/lib/msgdigest.s7i      0.040053           1222
seed7/lib/multiscr.s7i       0.037648             68
seed7/lib/null_file.s7i      0.038770            345
seed7/lib/osfiles.s7i        0.039834           1085
seed7/lib/pbm.s7i            0.038915            230
seed7/lib/pcx.s7i            0.038658            638
seed7/lib/pem.s7i            0.037569            185
seed7/lib/pgm.s7i            0.037845            238
seed7/lib/pic16.s7i          0.038219           1037
seed7/lib/pic32.s7i          0.039133           2060
seed7/lib/pic_util.s7i       0.039114            144
seed7/lib/pixelimage.s7i     0.039113            320
seed7/lib/pixmap_file.s7i    0.038919            459
seed7/lib/pixmapfont.s7i     0.039235            184
seed7/lib/pkcs1.s7i          0.044668            543
seed7/lib/png.s7i            0.039281           1064
seed7/lib/poll.s7i           0.039239            313
seed7/lib/ppm.s7i            0.038774            240
seed7/lib/process.s7i        0.039178            541
seed7/lib/progs.s7i          0.040028            789
seed7/lib/propertyfile.s7i   0.040742            155
seed7/lib/rational.s7i       0.039314            792
seed7/lib/ref_list.s7i       0.038680            252
seed7/lib/reference.s7i      0.038849            126
seed7/lib/reverse.s7i        0.038259             94
seed7/lib/rpm.s7i            0.039682           3487
seed7/lib/rpmext.s7i         0.040672            318
seed7/lib/scanfile.s7i       0.039670           1779
seed7/lib/scanjson.s7i       0.041180            413
seed7/lib/scanstri.s7i       0.040211           1814
seed7/lib/scantoml.s7i       0.037953           1603
seed7/lib/seed7_05.s7i       0.039728           1072
seed7/lib/set.s7i            0.036067             57
seed7/lib/shell.s7i          0.037589            615
seed7/lib/showtls.s7i        0.038238            678
seed7/lib/signature.s7i      0.040288            131
seed7/lib/smtp.s7i           0.039267            261
seed7/lib/sockbase.s7i       0.038988            217
seed7/lib/socket.s7i         0.039239            326
seed7/lib/sokoban1.s7i       0.037671           1519
seed7/lib/sql_base.s7i       0.039462           1000
seed7/lib/stars.s7i          0.039754           1705
seed7/lib/stdfont10.s7i      0.037668           3347
seed7/lib/stdfont12.s7i      0.037772           3928
seed7/lib/stdfont14.s7i      0.039318           4510
seed7/lib/stdfont16.s7i      0.038359           5092
seed7/lib/stdfont18.s7i      0.037926           5868
seed7/lib/stdfont20.s7i      0.038037           6449
seed7/lib/stdfont24.s7i      0.039029           7421
seed7/lib/stdfont8.s7i       0.038260           2960
seed7/lib/stdfont9.s7i       0.038215           3152
seed7/lib/stdio.s7i          0.039383            192
seed7/lib/strifile.s7i       0.039593            345
seed7/lib/string.s7i         0.039370            779
seed7/lib/stritext.s7i       0.039542            352
seed7/lib/struct.s7i         0.039219            266
seed7/lib/struct_elem.s7i    0.039381            129
seed7/lib/subfile.s7i        0.038352            174
seed7/lib/subrange.s7i       0.036800             78
seed7/lib/syntax.s7i         0.038464            294
seed7/lib/tar.s7i            0.039183           1880
seed7/lib/tar_cmds.s7i       0.039166            752
seed7/lib/tdes.s7i           0.038416            143
seed7/lib/tee.s7i            0.039357            143
seed7/lib/text.s7i           0.038745            135
seed7/lib/tga.s7i            0.041102            676
seed7/lib/tiff.s7i           0.040272           2771
seed7/lib/time.s7i           0.039431           1191
seed7/lib/tls.s7i            0.040795           2230
seed7/lib/unicode.s7i        0.042513            575
seed7/lib/unionfnd.s7i       0.040878            130
seed7/lib/upper.s7i          0.039047            142
seed7/lib/utf16.s7i          0.040177            540
seed7/lib/utf8.s7i           0.039054            234
seed7/lib/vecfont10.s7i      0.041343           1056
seed7/lib/vecfont18.s7i      0.041808           1119
seed7/lib/vector3d.s7i       0.040510            293
seed7/lib/vectorfont.s7i     0.046149            239
seed7/lib/wildcard.s7i       0.040246            140
seed7/lib/window.s7i         0.039233            455
seed7/lib/wrinum.s7i         0.038879            248
seed7/lib/x509cert.s7i       0.039262           1243
seed7/lib/xml_ent.s7i        0.039766             94
seed7/lib/xmldom.s7i         0.037493            303
seed7/lib/xz.s7i             0.040355            442
seed7/lib/zip.s7i            0.038464           2792
seed7/lib/zstd.s7i           0.038349           1263
============================ ================= ================

Statistical Summary — Mode B (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.039171        |
+-----------+-----------------+
| Minimum   | 0.034768        |
+-----------+-----------------+
| Maximum   | 0.048607        |
+-----------+-----------------+

Mode C — Mode Activation + Full-Buffer Fontification (font-lock-ensure)
-----------------------------------------------------------------------

Full-buffer fontification stress test.

File Load Times — Mode C (mean, GC-free)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

============================ ================= ================
File Name                    Mean Time (s)     Line count
============================ ================= ================
seed7/prg/addup.sd7          0.041202            190
seed7/prg/bas7.sd7           0.320383          11459
seed7/prg/bifurk.sd7         0.036868             73
seed7/prg/bigfiles.sd7       0.038366            129
seed7/prg/brainf7.sd7        0.036960             86
seed7/prg/calc7.sd7          0.038667            128
seed7/prg/carddemo.sd7       0.039378            190
seed7/prg/castle.sd7         0.107083           3148
seed7/prg/cat.sd7            0.036553             82
seed7/prg/cellauto.sd7       0.036452             85
seed7/prg/celsius.sd7        0.039544             42
seed7/prg/chk_all.sd7        0.060820            843
seed7/prg/chkarr.sd7         0.350835           8367
seed7/prg/chkbig.sd7         2.058439          29026
seed7/prg/chkbin.sd7         0.589870           6469
seed7/prg/chkbitdata.sd7     0.627145           6624
seed7/prg/chkbool.sd7        0.133666           3157
seed7/prg/chkbst.sd7         0.069906            722
seed7/prg/chkchr.sd7         0.214920           2809
seed7/prg/chkcmd.sd7         0.073425           1205
seed7/prg/chkdb.sd7          0.364949           7454
seed7/prg/chkdecl.sd7        0.062728            448
seed7/prg/chkenum.sd7        0.070471           1230
seed7/prg/chkerr.sd7         0.197261           4663
seed7/prg/chkexc.sd7         0.086169           2627
seed7/prg/chkfil.sd7         0.077761           1615
seed7/prg/chkflt.sd7         1.335764          20620
seed7/prg/chkhent.sd7        0.042144             54
seed7/prg/chkhsh.sd7         0.253184           4548
seed7/prg/chkidx.sd7         1.317565          19567
seed7/prg/chkint.sd7         2.528488          38129
seed7/prg/chkjson.sd7        0.107567           1764
seed7/prg/chkovf.sd7         0.554221           8216
seed7/prg/chkprc.sd7         0.322573          10111
seed7/prg/chkscan.sd7        0.060217            714
seed7/prg/chkset.sd7         0.682202          11974
seed7/prg/chkstr.sd7         1.465762          26952
seed7/prg/chktime.sd7        0.139117           2025
seed7/prg/chktoml.sd7        0.112861           1656
seed7/prg/clock.sd7          0.042745             47
seed7/prg/clock2.sd7         0.041995             43
seed7/prg/clock3.sd7         0.039851             95
seed7/prg/cmpfil.sd7         0.038207             84
seed7/prg/comanche.sd7       0.042267            180
seed7/prg/confval.sd7        0.046012            175
seed7/prg/db7.sd7            0.052057            417
seed7/prg/diff7.sd7          0.046262            263
seed7/prg/dirtst.sd7         0.038278             42
seed7/prg/dirx.sd7           0.041458            152
seed7/prg/dnafight.sd7       0.071743           1381
seed7/prg/dragon.sd7         0.039649             73
seed7/prg/echo.sd7           0.038641             39
seed7/prg/eliza.sd7          0.045765            302
seed7/prg/err.sd7            0.042609             96
seed7/prg/fannkuch.sd7       0.040177            131
seed7/prg/fib.sd7            0.037942             47
seed7/prg/find7.sd7          0.040442            133
seed7/prg/findchar.sd7       0.041227            149
seed7/prg/fractree.sd7       0.038847             55
seed7/prg/ftp7.sd7           0.045159            296
seed7/prg/ftpserv.sd7        0.040934             74
seed7/prg/gcd.sd7            0.040564            109
seed7/prg/gkbd.sd7           0.050338            358
seed7/prg/gtksvtst.sd7       0.038767             94
seed7/prg/hal.sd7            0.041848            250
seed7/prg/hamu.sd7           0.048573            573
seed7/prg/hanoi.sd7          0.035401             55
seed7/prg/hd.sd7             0.035936             79
seed7/prg/hello.sd7          0.038010             32
seed7/prg/hilbert.sd7        0.041551            108
seed7/prg/ide7.sd7           0.043704            196
seed7/prg/kbd.sd7            0.037930             49
seed7/prg/klondike.sd7       0.055810            883
seed7/prg/lander.sd7         0.072948           1551
seed7/prg/lst80bas.sd7       0.044208            344
seed7/prg/lst99bas.sd7       0.045020            401
seed7/prg/lstgwbas.sd7       0.050208            577
seed7/prg/mahjong.sd7        0.079882           1943
seed7/prg/make7.sd7          0.038811            121
seed7/prg/mandelbr.sd7       0.040643            237
seed7/prg/mind.sd7           0.044917            443
seed7/prg/mirror.sd7         0.038892            131
seed7/prg/ms.sd7             0.048129            641
seed7/prg/nicoma.sd7         0.037710            135
seed7/prg/pac.sd7            0.049410            726
seed7/prg/pairs.sd7          0.080240           2025
seed7/prg/panic.sd7          0.096279           2634
seed7/prg/percolation.sd7    0.041978            330
seed7/prg/planets.sd7        0.077846           1486
seed7/prg/portfwd7.sd7       0.038997            139
seed7/prg/prime.sd7          0.036345             74
seed7/prg/printpi1.sd7       0.036920             56
seed7/prg/printpi2.sd7       0.035840             54
seed7/prg/printpi3.sd7       0.036112             60
seed7/prg/pv7.sd7            0.044374            337
seed7/prg/queen.sd7          0.038188            149
seed7/prg/rand.sd7           0.037749            121
seed7/prg/raytrace.sd7       0.048306            538
seed7/prg/rever.sd7          0.053993            816
seed7/prg/roman.sd7          0.035892             38
seed7/prg/s7c.sd7            0.277893           9060
seed7/prg/s7check.sd7        0.036751             68
seed7/prg/savehd7.sd7        0.065008           1110
seed7/prg/self.sd7           0.035793             49
seed7/prg/shisen.sd7         0.069916           1423
seed7/prg/sl.sd7             0.058496           1029
seed7/prg/snake.sd7          0.047847            615
seed7/prg/sokoban.sd7        0.054642            891
seed7/prg/spigotpi.sd7       0.036671             64
seed7/prg/sql7.sd7           0.041928            278
seed7/prg/startrek.sd7       0.058852            979
seed7/prg/sudoku7.sd7        0.100516           2657
seed7/prg/sydir7.sd7         0.047604            384
seed7/prg/syntaxhl.sd7       0.047811            177
seed7/prg/tak.sd7            0.036921             59
seed7/prg/tar7.sd7           0.037578            121
seed7/prg/tch.sd7            0.035994             55
seed7/prg/testfont.sd7       0.037360             95
seed7/prg/tet.sd7            0.044426            479
seed7/prg/tetg.sd7           0.045663            501
seed7/prg/toutf8.sd7         0.042355            240
seed7/prg/tst_cli.sd7        0.035424             40
seed7/prg/tst_srv.sd7        0.035378             47
seed7/prg/wator.sd7          0.051631            651
seed7/prg/which.sd7          0.035677             65
seed7/prg/wiz.sd7            0.102836           2833
seed7/prg/wordcnt.sd7        0.034797             54
seed7/prg/wrinum.sd7         0.034144             43
seed7/prg/wumpus.sd7         0.044026            372
seed7/lib/aes.s7i            0.109156           1144
seed7/lib/aes_gcm.s7i        0.046550            392
seed7/lib/ar.s7i             0.073386           1532
seed7/lib/arc4.s7i           0.038510            144
seed7/lib/archive.s7i        0.040725            143
seed7/lib/archive_base.s7i   0.038545            135
seed7/lib/array.s7i          0.054332            610
seed7/lib/asn1.s7i           0.047278            544
seed7/lib/asn1oid.s7i        0.041504            157
seed7/lib/basearray.s7i      0.050250            450
seed7/lib/bigfile.s7i        0.040984            136
seed7/lib/bigint.s7i         0.055585            824
seed7/lib/bigrat.s7i         0.053523            784
seed7/lib/bin16.s7i          0.050571            592
seed7/lib/bin32.s7i          0.048416            490
seed7/lib/bin64.s7i          0.049179            539
seed7/lib/bitdata.s7i        0.074813           1330
seed7/lib/bitmapfont.s7i     0.038927            215
seed7/lib/bitset.s7i         0.047924            593
seed7/lib/bitsetof.s7i       0.047530            431
seed7/lib/blowfish.s7i       0.056229            383
seed7/lib/bmp.s7i            0.061019            924
seed7/lib/boolean.s7i        0.044633            403
seed7/lib/browser.s7i        0.042834            280
seed7/lib/bstring.s7i        0.041157            227
seed7/lib/bytedata.s7i       0.050267            482
seed7/lib/bzip2.s7i          0.059674            887
seed7/lib/cards.s7i          0.066744           1342
seed7/lib/category.s7i       0.042000            209
seed7/lib/cc_conf.s7i        0.078801           1314
seed7/lib/ccittfax.s7i       0.064963           1022
seed7/lib/cgi.s7i            0.037414            109
seed7/lib/cgidialog.s7i      0.060071           1118
seed7/lib/char.s7i           0.043578            356
seed7/lib/charsets.s7i       0.082123           2024
seed7/lib/chartype.s7i       0.039838            121
seed7/lib/cipher.s7i         0.036929            146
seed7/lib/cli_cmds.s7i       0.066637           1360
seed7/lib/clib_file.s7i      0.042352            301
seed7/lib/color.s7i          0.040564            185
seed7/lib/complex.s7i        0.045232            464
seed7/lib/compress.s7i       0.038360            150
seed7/lib/console.s7i        0.040825            188
seed7/lib/cpio.s7i           0.082969           1708
seed7/lib/crc32.s7i          0.043833            193
seed7/lib/cronos16.s7i       0.092190           1173
seed7/lib/cronos27.s7i       0.115617           1464
seed7/lib/csv.s7i            0.040552            201
seed7/lib/db_prop.s7i        0.063405            991
seed7/lib/deflate.s7i        0.055131            740
seed7/lib/des.s7i            0.055326            444
seed7/lib/dialog.s7i         0.046233            311
seed7/lib/dir.s7i            0.039161            163
seed7/lib/draw.s7i           0.055753            854
seed7/lib/duration.s7i       0.059846           1038
seed7/lib/echo.s7i           0.037191            132
seed7/lib/editline.s7i       0.045742            398
seed7/lib/elf.s7i            0.085662           1560
seed7/lib/elliptic.s7i       0.053613            649
seed7/lib/enable_io.s7i      0.043742            312
seed7/lib/encoding.s7i       0.061802            931
seed7/lib/enumeration.s7i    0.042285            236
seed7/lib/environment.s7i    0.040410            175
seed7/lib/exif.s7i           0.039977            152
seed7/lib/external_file.s7i  0.043804            340
seed7/lib/field.s7i          0.041658            268
seed7/lib/file.s7i           0.044560            372
seed7/lib/filebits.s7i       0.036077             46
seed7/lib/filesys.s7i        0.047960            601
seed7/lib/fileutil.s7i       0.038385            144
seed7/lib/fixarray.s7i       0.047910            307
seed7/lib/float.s7i          0.067264            757
seed7/lib/font.s7i           0.040901            196
seed7/lib/font8x8.s7i        0.048664            998
seed7/lib/forloop.s7i        0.056395            449
seed7/lib/ftp.s7i            0.057588            969
seed7/lib/ftpserv.s7i        0.050295            631
seed7/lib/getf.s7i           0.038031            115
seed7/lib/gethttp.s7i        0.034886             41
seed7/lib/gethttps.s7i       0.034777             41
seed7/lib/gif.s7i            0.048546            561
seed7/lib/graph.s7i          0.050121            415
seed7/lib/graph_file.s7i     0.044033            399
seed7/lib/gtkserver.s7i      0.038526            161
seed7/lib/gzip.s7i           0.048749            573
seed7/lib/hash.s7i           0.049809            421
seed7/lib/hashsetof.s7i      0.049063            499
seed7/lib/hmac.s7i           0.039305            152
seed7/lib/html.s7i           0.036761             83
seed7/lib/html_ent.s7i       0.046750            476
seed7/lib/htmldom.s7i        0.044926            286
seed7/lib/http_request.s7i   0.059755            696
seed7/lib/http_srv_resp.s7i  0.047034            380
seed7/lib/https_request.s7i  0.040690            211
seed7/lib/httpserv.s7i       0.043878            345
seed7/lib/huffman.s7i        0.052384            644
seed7/lib/ico.s7i            0.041218            221
seed7/lib/idxarray.s7i       0.042583            232
seed7/lib/image.s7i          0.037785            156
seed7/lib/imagefile.s7i      0.040125            171
seed7/lib/inflate.s7i        0.046204            411
seed7/lib/inifile.s7i        0.036720            129
seed7/lib/integer.s7i        0.051360            663
seed7/lib/iobuffer.s7i       0.041308            289
seed7/lib/jpeg.s7i           0.084840           1761
seed7/lib/json.s7i           0.056602            891
seed7/lib/json_serde.s7i     0.052872            783
seed7/lib/keybd.s7i          0.055419            639
seed7/lib/keydescr.s7i       0.042011            192
seed7/lib/leb128.s7i         0.040926            218
seed7/lib/line.s7i           0.039160            164
seed7/lib/listener.s7i       0.045273            247
seed7/lib/logfile.s7i        0.069936             73
seed7/lib/lower.s7i          0.052023            142
seed7/lib/lzma.s7i           0.074952            934
seed7/lib/lzw.s7i            0.064017            861
seed7/lib/magic.s7i          0.053257            403
seed7/lib/mahjng32.s7i       0.072653           1500
seed7/lib/make.s7i           0.059352            544
seed7/lib/makedata.s7i       0.076013           1428
seed7/lib/math.s7i           0.041151            201
seed7/lib/mixarith.s7i       0.042450            249
seed7/lib/modern27.s7i       0.084057           1099
seed7/lib/more.s7i           0.040706            130
seed7/lib/msgdigest.s7i      0.081202           1222
seed7/lib/multiscr.s7i       0.038001             68
seed7/lib/null_file.s7i      0.045330            345
seed7/lib/osfiles.s7i        0.067510           1085
seed7/lib/pbm.s7i            0.041882            230
seed7/lib/pcx.s7i            0.052872            638
seed7/lib/pem.s7i            0.040884            185
seed7/lib/pgm.s7i            0.041257            238
seed7/lib/pic16.s7i          0.049295           1037
seed7/lib/pic32.s7i          0.084953           2060
seed7/lib/pic_util.s7i       0.039105            144
seed7/lib/pixelimage.s7i     0.043044            320
seed7/lib/pixmap_file.s7i    0.045895            459
seed7/lib/pixmapfont.s7i     0.040852            184
seed7/lib/pkcs1.s7i          0.059594            543
seed7/lib/png.s7i            0.063984           1064
seed7/lib/poll.s7i           0.047528            313
seed7/lib/ppm.s7i            0.044003            240
seed7/lib/process.s7i        0.049452            541
seed7/lib/progs.s7i          0.056269            789
seed7/lib/propertyfile.s7i   0.038533            155
seed7/lib/rational.s7i       0.061763            792
seed7/lib/ref_list.s7i       0.046961            252
seed7/lib/reference.s7i      0.038728            126
seed7/lib/reverse.s7i        0.039671             94
seed7/lib/rpm.s7i            0.146435           3487
seed7/lib/rpmext.s7i         0.042390            318
seed7/lib/scanfile.s7i       0.081867           1779
seed7/lib/scanjson.s7i       0.047166            413
seed7/lib/scanstri.s7i       0.079737           1814
seed7/lib/scantoml.s7i       0.073479           1603
seed7/lib/seed7_05.s7i       0.069460           1072
seed7/lib/set.s7i            0.036522             57
seed7/lib/shell.s7i          0.052893            615
seed7/lib/showtls.s7i        0.054326            678
seed7/lib/signature.s7i      0.038417            131
seed7/lib/smtp.s7i           0.040501            261
seed7/lib/sockbase.s7i       0.043609            217
seed7/lib/socket.s7i         0.044544            326
seed7/lib/sokoban1.s7i       0.054870           1519
seed7/lib/sql_base.s7i       0.064330           1000
seed7/lib/stars.s7i          0.139439           1705
seed7/lib/stdfont10.s7i      0.082079           3347
seed7/lib/stdfont12.s7i      0.092737           3928
seed7/lib/stdfont14.s7i      0.103031           4510
seed7/lib/stdfont16.s7i      0.114627           5092
seed7/lib/stdfont18.s7i      0.130430           5868
seed7/lib/stdfont20.s7i      0.146374           6449
seed7/lib/stdfont24.s7i      0.178378           7421
seed7/lib/stdfont8.s7i       0.072047           2960
seed7/lib/stdfont9.s7i       0.084100           3152
seed7/lib/stdio.s7i          0.041120            192
seed7/lib/strifile.s7i       0.043729            345
seed7/lib/string.s7i         0.055488            779
seed7/lib/stritext.s7i       0.043378            352
seed7/lib/struct.s7i         0.044703            266
seed7/lib/struct_elem.s7i    0.038554            129
seed7/lib/subfile.s7i        0.039155            174
seed7/lib/subrange.s7i       0.038566             78
seed7/lib/syntax.s7i         0.046397            294
seed7/lib/tar.s7i            0.082574           1880
seed7/lib/tar_cmds.s7i       0.056283            752
seed7/lib/tdes.s7i           0.038865            143
seed7/lib/tee.s7i            0.038081            143
seed7/lib/text.s7i           0.038150            135
seed7/lib/tga.s7i            0.054121            676
seed7/lib/tiff.s7i           0.123917           2771
seed7/lib/time.s7i           0.063954           1191
seed7/lib/tls.s7i            0.105642           2230
seed7/lib/unicode.s7i        0.052877            575
seed7/lib/unionfnd.s7i       0.038881            130
seed7/lib/upper.s7i          0.039174            142
seed7/lib/utf16.s7i          0.050978            540
seed7/lib/utf8.s7i           0.041651            234
seed7/lib/vecfont10.s7i      0.080280           1056
seed7/lib/vecfont18.s7i      0.088391           1119
seed7/lib/vector3d.s7i       0.040806            293
seed7/lib/vectorfont.s7i     0.043188            239
seed7/lib/wildcard.s7i       0.038643            140
seed7/lib/window.s7i         0.046628            455
seed7/lib/wrinum.s7i         0.040680            248
seed7/lib/x509cert.s7i       0.071689           1243
seed7/lib/xml_ent.s7i        0.038163             94
seed7/lib/xmldom.s7i         0.041087            303
seed7/lib/xz.s7i             0.046991            442
seed7/lib/zip.s7i            0.119518           2792
seed7/lib/zstd.s7i           0.069662           1263
============================ ================= ================

Statistical Summary — Mode C (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.090868        |
+-----------+-----------------+
| Minimum   | 0.034144        |
+-----------+-----------------+
| Maximum   | 2.528488        |
+-----------+-----------------+

Mode D — Mode Activation + Full Incremental jit-lock (Scroll Pass)
------------------------------------------------------------------

The buffer is displayed and scrolled from top to bottom. After each screenful,
`jit-lock-fontify-now` is called over the currently visible window region.
This models incremental visible-region fontification without using `sit-for 0`.

File Load Times — Mode D (mean, GC-free)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

============================ ================= ================
File Name                    Mean Time (s)     Line count
============================ ================= ================
seed7/prg/addup.sd7          0.045739            190
seed7/prg/bas7.sd7           0.769191          11459
seed7/prg/bifurk.sd7         0.039581             73
seed7/prg/bigfiles.sd7       0.042495            129
seed7/prg/brainf7.sd7        0.039700             86
seed7/prg/calc7.sd7          0.043113            128
seed7/prg/carddemo.sd7       0.046248            190
seed7/prg/castle.sd7         0.215859           3148
seed7/prg/cat.sd7            0.039223             82
seed7/prg/cellauto.sd7       0.041305             85
seed7/prg/celsius.sd7        0.037419             42
seed7/prg/chk_all.sd7        0.083461            843
seed7/prg/chkarr.sd7         0.855962           8367
seed7/prg/chkbig.sd7         4.091371          29026
seed7/prg/chkbin.sd7         1.016466           6469
seed7/prg/chkbitdata.sd7     1.233249           6624
seed7/prg/chkbool.sd7        0.229919           3157
seed7/prg/chkbst.sd7         0.102153            722
seed7/prg/chkchr.sd7         0.475011           2809
seed7/prg/chkcmd.sd7         0.110632           1205
seed7/prg/chkdb.sd7          0.745011           7454
seed7/prg/chkdecl.sd7        0.094278            448
seed7/prg/chkenum.sd7        0.120089           1230
seed7/prg/chkerr.sd7         0.337179           4663
seed7/prg/chkexc.sd7         0.146785           2627
seed7/prg/chkfil.sd7         0.126691           1615
seed7/prg/chkflt.sd7         2.784296          20620
seed7/prg/chkhent.sd7        0.038491             54
seed7/prg/chkhsh.sd7         0.493381           4548
seed7/prg/chkidx.sd7         3.140209          19567
seed7/prg/chkint.sd7         5.516850          38129
seed7/prg/chkjson.sd7        0.183980           1764
seed7/prg/chkovf.sd7         1.195186           8216
seed7/prg/chkprc.sd7         0.696850          10111
seed7/prg/chkscan.sd7        0.088922            714
seed7/prg/chkset.sd7         1.736956          11974
seed7/prg/chkstr.sd7         3.408790          26952
seed7/prg/chktime.sd7        0.246344           2025
seed7/prg/chktoml.sd7        0.195300           1656
seed7/prg/clock.sd7          0.037469             47
seed7/prg/clock2.sd7         0.037275             43
seed7/prg/clock3.sd7         0.042991             95
seed7/prg/cmpfil.sd7         0.040022             84
seed7/prg/comanche.sd7       0.051861            180
seed7/prg/confval.sd7        0.053881            175
seed7/prg/db7.sd7            0.064839            417
seed7/prg/diff7.sd7          0.053082            263
seed7/prg/dirtst.sd7         0.037104             42
seed7/prg/dirx.sd7           0.042826            152
seed7/prg/dnafight.sd7       0.116433           1381
seed7/prg/dragon.sd7         0.039569             73
seed7/prg/echo.sd7           0.036517             39
seed7/prg/eliza.sd7          0.051773            302
seed7/prg/err.sd7            0.045248             96
seed7/prg/fannkuch.sd7       0.043247            131
seed7/prg/fib.sd7            0.038943             47
seed7/prg/find7.sd7          0.042078            133
seed7/prg/findchar.sd7       0.042736            149
seed7/prg/fractree.sd7       0.037289             55
seed7/prg/ftp7.sd7           0.052798            296
seed7/prg/ftpserv.sd7        0.040794             74
seed7/prg/gcd.sd7            0.040992            109
seed7/prg/gkbd.sd7           0.062141            358
seed7/prg/gtksvtst.sd7       0.040317             94
seed7/prg/hal.sd7            0.048176            250
seed7/prg/hamu.sd7           0.067241            573
seed7/prg/hanoi.sd7          0.042429             55
seed7/prg/hd.sd7             0.040991             79
seed7/prg/hello.sd7          0.039034             32
seed7/prg/hilbert.sd7        0.042308            108
seed7/prg/ide7.sd7           0.048514            196
seed7/prg/kbd.sd7            0.036733             49
seed7/prg/klondike.sd7       0.088892            883
seed7/prg/lander.sd7         0.132378           1551
seed7/prg/lst80bas.sd7       0.062143            344
seed7/prg/lst99bas.sd7       0.061760            401
seed7/prg/lstgwbas.sd7       0.073958            577
seed7/prg/mahjong.sd7        0.148125           1943
seed7/prg/make7.sd7          0.043174            121
seed7/prg/mandelbr.sd7       0.049677            237
seed7/prg/mind.sd7           0.063526            443
seed7/prg/mirror.sd7         0.044145            131
seed7/prg/ms.sd7             0.069432            641
seed7/prg/nicoma.sd7         0.042784            135
seed7/prg/pac.sd7            0.072814            726
seed7/prg/pairs.sd7          0.138858           2025
seed7/prg/panic.sd7          0.196288           2634
seed7/prg/percolation.sd7    0.057925            330
seed7/prg/planets.sd7        0.136521           1486
seed7/prg/portfwd7.sd7       0.044183            139
seed7/prg/prime.sd7          0.038563             74
seed7/prg/printpi1.sd7       0.037893             56
seed7/prg/printpi2.sd7       0.038391             54
seed7/prg/printpi3.sd7       0.039721             60
seed7/prg/pv7.sd7            0.058729            337
seed7/prg/queen.sd7          0.043485            149
seed7/prg/rand.sd7           0.041984            121
seed7/prg/raytrace.sd7       0.069671            538
seed7/prg/rever.sd7          0.084202            816
seed7/prg/roman.sd7          0.036540             38
seed7/prg/s7c.sd7            0.622911           9060
seed7/prg/s7check.sd7        0.038788             68
seed7/prg/savehd7.sd7        0.109286           1110
seed7/prg/self.sd7           0.036947             49
seed7/prg/shisen.sd7         0.117501           1423
seed7/prg/sl.sd7             0.095540           1029
seed7/prg/snake.sd7          0.065626            615
seed7/prg/sokoban.sd7        0.082225            891
seed7/prg/spigotpi.sd7       0.038236             64
seed7/prg/sql7.sd7           0.053315            278
seed7/prg/startrek.sd7       0.094979            979
seed7/prg/sudoku7.sd7        0.196954           2657
seed7/prg/sydir7.sd7         0.059983            384
seed7/prg/syntaxhl.sd7       0.049275            177
seed7/prg/tak.sd7            0.037711             59
seed7/prg/tar7.sd7           0.042338            121
seed7/prg/tch.sd7            0.038553             55
seed7/prg/testfont.sd7       0.042151             95
seed7/prg/tet.sd7            0.059259            479
seed7/prg/tetg.sd7           0.062268            501
seed7/prg/toutf8.sd7         0.051224            240
seed7/prg/tst_cli.sd7        0.036543             40
seed7/prg/tst_srv.sd7        0.037049             47
seed7/prg/wator.sd7          0.078242            651
seed7/prg/which.sd7          0.038161             65
seed7/prg/wiz.sd7            0.207762           2833
seed7/prg/wordcnt.sd7        0.039156             54
seed7/prg/wrinum.sd7         0.036130             43
seed7/prg/wumpus.sd7         0.053038            372
seed7/lib/aes.s7i            0.194900           1144
seed7/lib/aes_gcm.s7i        0.059108            392
seed7/lib/ar.s7i             0.124009           1532
seed7/lib/arc4.s7i           0.043126            144
seed7/lib/archive.s7i        0.043271            143
seed7/lib/archive_base.s7i   0.043885            135
seed7/lib/array.s7i          0.075707            610
seed7/lib/asn1.s7i           0.063293            544
seed7/lib/asn1oid.s7i        0.049174            157
seed7/lib/basearray.s7i      0.063157            450
seed7/lib/bigfile.s7i        0.041970            136
seed7/lib/bigint.s7i         0.079944            824
seed7/lib/bigrat.s7i         0.079401            784
seed7/lib/bin16.s7i          0.068332            592
seed7/lib/bin32.s7i          0.061477            490
seed7/lib/bin64.s7i          0.063656            539
seed7/lib/bitdata.s7i        0.123168           1330
seed7/lib/bitmapfont.s7i     0.047936            215
seed7/lib/bitset.s7i         0.064145            593
seed7/lib/bitsetof.s7i       0.061759            431
seed7/lib/blowfish.s7i       0.076485            383
seed7/lib/bmp.s7i            0.099742            924
seed7/lib/boolean.s7i        0.054628            403
seed7/lib/browser.s7i        0.052818            280
seed7/lib/bstring.s7i        0.046715            227
seed7/lib/bytedata.s7i       0.064749            482
seed7/lib/bzip2.s7i          0.092368            887
seed7/lib/cards.s7i          0.102677           1342
seed7/lib/category.s7i       0.048379            209
seed7/lib/cc_conf.s7i        0.118571           1314
seed7/lib/ccittfax.s7i       0.100993           1022
seed7/lib/cgi.s7i            0.040920            109
seed7/lib/cgidialog.s7i      0.096229           1118
seed7/lib/char.s7i           0.051377            356
seed7/lib/charsets.s7i       0.126606           2024
seed7/lib/chartype.s7i       0.047902            121
seed7/lib/cipher.s7i         0.044933            146
seed7/lib/cli_cmds.s7i       0.119180           1360
seed7/lib/clib_file.s7i      0.050770            301
seed7/lib/color.s7i          0.047382            185
seed7/lib/complex.s7i        0.060039            464
seed7/lib/compress.s7i       0.043899            150
seed7/lib/console.s7i        0.045420            188
seed7/lib/cpio.s7i           0.144807           1708
seed7/lib/crc32.s7i          0.054342            193
seed7/lib/cronos16.s7i       0.194459           1173
seed7/lib/cronos27.s7i       0.255776           1464
seed7/lib/csv.s7i            0.048142            201
seed7/lib/db_prop.s7i        0.100541            991
seed7/lib/deflate.s7i        0.085897            740
seed7/lib/des.s7i            0.080144            444
seed7/lib/dialog.s7i         0.057627            311
seed7/lib/dir.s7i            0.042848            163
seed7/lib/draw.s7i           0.084890            854
seed7/lib/duration.s7i       0.096071           1038
seed7/lib/echo.s7i           0.041486            132
seed7/lib/editline.s7i       0.059310            398
seed7/lib/elf.s7i            0.153873           1560
seed7/lib/elliptic.s7i       0.076132            649
seed7/lib/enable_io.s7i      0.051896            312
seed7/lib/encoding.s7i       0.097493            931
seed7/lib/enumeration.s7i    0.048982            236
seed7/lib/environment.s7i    0.044375            175
seed7/lib/exif.s7i           0.045864            152
seed7/lib/external_file.s7i  0.051761            340
seed7/lib/field.s7i          0.052092            268
seed7/lib/file.s7i           0.054247            372
seed7/lib/filebits.s7i       0.039011             46
seed7/lib/filesys.s7i        0.064697            601
seed7/lib/fileutil.s7i       0.043329            144
seed7/lib/fixarray.s7i       0.053760            307
seed7/lib/float.s7i          0.074580            757
seed7/lib/font.s7i           0.045204            196
seed7/lib/font8x8.s7i        0.067796            998
seed7/lib/forloop.s7i        0.060840            449
seed7/lib/ftp.s7i            0.088979            969
seed7/lib/ftpserv.s7i        0.075434            631
seed7/lib/getf.s7i           0.040953            115
seed7/lib/gethttp.s7i        0.036882             41
seed7/lib/gethttps.s7i       0.037210             41
seed7/lib/gif.s7i            0.070469            561
seed7/lib/graph.s7i          0.065378            415
seed7/lib/graph_file.s7i     0.057152            399
seed7/lib/gtkserver.s7i      0.041534            161
seed7/lib/gzip.s7i           0.070450            573
seed7/lib/hash.s7i           0.068922            421
seed7/lib/hashsetof.s7i      0.066381            499
seed7/lib/hmac.s7i           0.044492            152
seed7/lib/html.s7i           0.040058             83
seed7/lib/html_ent.s7i       0.062717            476
seed7/lib/htmldom.s7i        0.053047            286
seed7/lib/http_request.s7i   0.077289            696
seed7/lib/http_srv_resp.s7i  0.060024            380
seed7/lib/https_request.s7i  0.047580            211
seed7/lib/httpserv.s7i       0.057068            345
seed7/lib/huffman.s7i        0.075006            644
seed7/lib/ico.s7i            0.049066            221
seed7/lib/idxarray.s7i       0.052569            232
seed7/lib/image.s7i          0.044790            156
seed7/lib/imagefile.s7i      0.048132            171
seed7/lib/inflate.s7i        0.064368            411
seed7/lib/inifile.s7i        0.043182            129
seed7/lib/integer.s7i        0.068209            663
seed7/lib/iobuffer.s7i       0.050529            289
seed7/lib/jpeg.s7i           0.153309           1761
seed7/lib/json.s7i           0.080085            891
seed7/lib/json_serde.s7i     0.078583            783
seed7/lib/keybd.s7i          0.079500            639
seed7/lib/keydescr.s7i       0.049283            192
seed7/lib/leb128.s7i         0.046122            218
seed7/lib/line.s7i           0.044941            164
seed7/lib/listener.s7i       0.048950            247
seed7/lib/logfile.s7i        0.041747             73
seed7/lib/lower.s7i          0.044368            142
seed7/lib/lzma.s7i           0.103788            934
seed7/lib/lzw.s7i            0.092467            861
seed7/lib/magic.s7i          0.063226            403
seed7/lib/mahjng32.s7i       0.090832           1500
seed7/lib/make.s7i           0.070354            544
seed7/lib/makedata.s7i       0.119696           1428
seed7/lib/math.s7i           0.044801            201
seed7/lib/mixarith.s7i       0.046687            249
seed7/lib/modern27.s7i       0.166479           1099
seed7/lib/more.s7i           0.042083            130
seed7/lib/msgdigest.s7i      0.135728           1222
seed7/lib/multiscr.s7i       0.038111             68
seed7/lib/null_file.s7i      0.050990            345
seed7/lib/osfiles.s7i        0.094693           1085
seed7/lib/pbm.s7i            0.047664            230
seed7/lib/pcx.s7i            0.076147            638
seed7/lib/pem.s7i            0.046156            185
seed7/lib/pgm.s7i            0.048870            238
seed7/lib/pic16.s7i          0.065438           1037
seed7/lib/pic32.s7i          0.122312           2060
seed7/lib/pic_util.s7i       0.045533            144
seed7/lib/pixelimage.s7i     0.051880            320
seed7/lib/pixmap_file.s7i    0.061870            459
seed7/lib/pixmapfont.s7i     0.048149            184
seed7/lib/pkcs1.s7i          0.076950            543
seed7/lib/png.s7i            0.107656           1064
seed7/lib/poll.s7i           0.057371            313
seed7/lib/ppm.s7i            0.053437            240
seed7/lib/process.s7i        0.064290            541
seed7/lib/progs.s7i          0.079027            789
seed7/lib/propertyfile.s7i   0.043633            155
seed7/lib/rational.s7i       0.078603            792
seed7/lib/ref_list.s7i       0.049006            252
seed7/lib/reference.s7i      0.043488            126
seed7/lib/reverse.s7i        0.040264             94
seed7/lib/rpm.s7i            0.282731           3487
seed7/lib/rpmext.s7i         0.051445            318
seed7/lib/scanfile.s7i       0.131690           1779
seed7/lib/scanjson.s7i       0.060681            413
seed7/lib/scanstri.s7i       0.137956           1814
seed7/lib/scantoml.s7i       0.130593           1603
seed7/lib/seed7_05.s7i       0.111208           1072
seed7/lib/set.s7i            0.038624             57
seed7/lib/shell.s7i          0.068747            615
seed7/lib/showtls.s7i        0.086078            678
seed7/lib/signature.s7i      0.043432            131
seed7/lib/smtp.s7i           0.049197            261
seed7/lib/sockbase.s7i       0.051064            217
seed7/lib/socket.s7i         0.052624            326
seed7/lib/sokoban1.s7i       0.079800           1519
seed7/lib/sql_base.s7i       0.094667           1000
seed7/lib/stars.s7i          0.234783           1705
seed7/lib/stdfont10.s7i      0.143070           3347
seed7/lib/stdfont12.s7i      0.164147           3928
seed7/lib/stdfont14.s7i      0.190334           4510
seed7/lib/stdfont16.s7i      0.210348           5092
seed7/lib/stdfont18.s7i      0.243086           5868
seed7/lib/stdfont20.s7i      0.269706           6449
seed7/lib/stdfont24.s7i      0.328453           7421
seed7/lib/stdfont8.s7i       0.127434           2960
seed7/lib/stdfont9.s7i       0.132634           3152
seed7/lib/stdio.s7i          0.044332            192
seed7/lib/strifile.s7i       0.054401            345
seed7/lib/string.s7i         0.075992            779
seed7/lib/stritext.s7i       0.054097            352
seed7/lib/struct.s7i         0.055679            266
seed7/lib/struct_elem.s7i    0.042940            129
seed7/lib/subfile.s7i        0.044423            174
seed7/lib/subrange.s7i       0.038711             78
seed7/lib/syntax.s7i         0.059559            294
seed7/lib/tar.s7i            0.144150           1880
seed7/lib/tar_cmds.s7i       0.085810            752
seed7/lib/tdes.s7i           0.043657            143
seed7/lib/tee.s7i            0.041013            143
seed7/lib/text.s7i           0.041440            135
seed7/lib/tga.s7i            0.087471            676
seed7/lib/tiff.s7i           0.246402           2771
seed7/lib/time.s7i           0.101182           1191
seed7/lib/tls.s7i            0.194742           2230
seed7/lib/unicode.s7i        0.075772            575
seed7/lib/unionfnd.s7i       0.042102            130
seed7/lib/upper.s7i          0.041933            142
seed7/lib/utf16.s7i          0.066979            540
seed7/lib/utf8.s7i           0.048602            234
seed7/lib/vecfont10.s7i      0.162111           1056
seed7/lib/vecfont18.s7i      0.178030           1119
seed7/lib/vector3d.s7i       0.050351            293
seed7/lib/vectorfont.s7i     0.049122            239
seed7/lib/wildcard.s7i       0.042229            140
seed7/lib/window.s7i         0.060949            455
seed7/lib/wrinum.s7i         0.050578            248
seed7/lib/x509cert.s7i       0.117786           1243
seed7/lib/xml_ent.s7i        0.041472             94
seed7/lib/xmldom.s7i         0.050512            303
seed7/lib/xz.s7i             0.060643            442
seed7/lib/zip.s7i            0.231414           2792
seed7/lib/zstd.s7i           0.116255           1263
============================ ================= ================

Statistical Summary — Mode D (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.158102        |
+-----------+-----------------+
| Minimum   | 0.036130        |
+-----------+-----------------+
| Maximum   | 5.516850        |
+-----------+-----------------+

Cross-Mode Comparison
=====================

+------+-----------------+-----------------+-----------------+
| Mode | Average (s)     | Minimum (s)     | Maximum (s)     |
+======+=================+=================+=================+
| A    | 0.034553        | 0.032489        | 0.041455        |
+------+-----------------+-----------------+-----------------+
| B    | 0.039171        | 0.034768        | 0.048607        |
+------+-----------------+-----------------+-----------------+
| C    | 0.090868        | 0.034144        | 2.528488        |
+------+-----------------+-----------------+-----------------+
| D    | 0.158102        | 0.036130        | 5.516850        |
+------+-----------------+-----------------+-----------------+

Phase Timing Summary
====================

Wall-clock times include warm-up work and GC performed by the warm-up helper.

+----------+--------------+--------------+--------------+
| Phase    | Warm-up      | Timed pass   | Mode total   |
+==========+==============+==============+==============+
| Mode A   | 00:00:13.421 | 00:00:58.868 | 00:01:12.290 |
+----------+--------------+--------------+--------------+
| Mode B   | 00:00:15.162 | 00:01:06.790 | 00:01:21.952 |
+----------+--------------+--------------+--------------+
| Mode C   | 00:00:36.398 | 00:02:35.785 | 00:03:12.183 |
+----------+--------------+--------------+--------------+
| Mode D   | 00:01:01.199 | 00:04:30.256 | 00:05:31.456 |
+----------+--------------+--------------+--------------+
| Total    |              |              | 00:11:17.888 |
+----------+--------------+--------------+--------------+


.. ---------------------------------------------------------------------------
