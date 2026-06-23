.. ===========================================================
.. PARTIAL REPORT — BENCHMARK WAS INTERRUPTED BY THE USER
.. Interrupted during: Mode B warm-up
.. Current file: ~/my/dvo/seed7-repos/seed7/prg/fannkuch.sd7
.. Interrupted phase elapsed so far: 00:00:22.817
.. Results below include only completed phases.
.. ===========================================================

=======================================================
PARTIAL GC-Controlled Benchmark Report: Seed7 Mode — Four Modes
=======================================================

:Running with: seed7-mode 2026-06-21T21:16:24+0000 W25-7
:Seed7 dir   : /Users/roup/my/dvo/seed7-repos/seed7/
:Started at: 15:35:34 local time
:Generated on: 2026-06-22 19:37:07 UTC
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
seed7/prg/addup.sd7          0.034347            190
seed7/prg/bas7.sd7           0.034225          11459
seed7/prg/bifurk.sd7         0.033364             73
seed7/prg/bigfiles.sd7       0.033377            129
seed7/prg/brainf7.sd7        0.033571             86
seed7/prg/calc7.sd7          0.033335            128
seed7/prg/carddemo.sd7       0.033447            190
seed7/prg/castle.sd7         0.033775           3148
seed7/prg/cat.sd7            0.033513             82
seed7/prg/cellauto.sd7       0.034216             85
seed7/prg/celsius.sd7        0.032960             42
seed7/prg/chk_all.sd7        0.033168            843
seed7/prg/chkarr.sd7         0.033919           8367
seed7/prg/chkbig.sd7         0.037023          29026
seed7/prg/chkbin.sd7         0.033295           6469
seed7/prg/chkbitdata.sd7     0.033339           6624
seed7/prg/chkbool.sd7        0.034448           3157
seed7/prg/chkbst.sd7         0.033753            722
seed7/prg/chkchr.sd7         0.034136           2809
seed7/prg/chkcmd.sd7         0.033450           1205
seed7/prg/chkdb.sd7          0.034515           7454
seed7/prg/chkdecl.sd7        0.033304            448
seed7/prg/chkenum.sd7        0.033924           1230
seed7/prg/chkerr.sd7         0.034629           4663
seed7/prg/chkexc.sd7         0.033921           2627
seed7/prg/chkfil.sd7         0.033640           1615
seed7/prg/chkflt.sd7         0.036693          20620
seed7/prg/chkhent.sd7        0.033601             54
seed7/prg/chkhsh.sd7         0.033925           4548
seed7/prg/chkidx.sd7         0.035918          19567
seed7/prg/chkint.sd7         0.039861          38129
seed7/prg/chkjson.sd7        0.033940           1764
seed7/prg/chkovf.sd7         0.034164           8216
seed7/prg/chkprc.sd7         0.033933          10111
seed7/prg/chkscan.sd7        0.033434            714
seed7/prg/chkset.sd7         0.035022          11974
seed7/prg/chkstr.sd7         0.038054          26952
seed7/prg/chktime.sd7        0.033574           2025
seed7/prg/chktoml.sd7        0.033473           1656
seed7/prg/clock.sd7          0.033726             47
seed7/prg/clock2.sd7         0.033542             43
seed7/prg/clock3.sd7         0.033153             95
seed7/prg/cmpfil.sd7         0.032608             84
seed7/prg/comanche.sd7       0.032725            180
seed7/prg/confval.sd7        0.032799            175
seed7/prg/db7.sd7            0.032646            417
seed7/prg/diff7.sd7          0.032801            263
seed7/prg/dirtst.sd7         0.034358             42
seed7/prg/dirx.sd7           0.033706            152
seed7/prg/dnafight.sd7       0.033318           1381
seed7/prg/dragon.sd7         0.033738             73
seed7/prg/echo.sd7           0.033495             39
seed7/prg/eliza.sd7          0.033560            302
seed7/prg/err.sd7            0.033603             96
seed7/prg/fannkuch.sd7       0.033485            131
seed7/prg/fib.sd7            0.033377             47
seed7/prg/find7.sd7          0.033441            133
seed7/prg/findchar.sd7       0.033527            149
seed7/prg/fractree.sd7       0.033320             55
seed7/prg/ftp7.sd7           0.033439            296
seed7/prg/ftpserv.sd7        0.033567             74
seed7/prg/gcd.sd7            0.033145            109
seed7/prg/gkbd.sd7           0.033603            358
seed7/prg/gtksvtst.sd7       0.033489             94
seed7/prg/hal.sd7            0.033614            250
seed7/prg/hamu.sd7           0.033572            573
seed7/prg/hanoi.sd7          0.033595             55
seed7/prg/hd.sd7             0.033785             79
seed7/prg/hello.sd7          0.033559             32
seed7/prg/hilbert.sd7        0.033162            108
seed7/prg/ide7.sd7           0.033241            196
seed7/prg/kbd.sd7            0.033557             49
seed7/prg/klondike.sd7       0.034403            883
seed7/prg/lander.sd7         0.033119           1551
seed7/prg/lst80bas.sd7       0.032778            344
seed7/prg/lst99bas.sd7       0.033728            401
seed7/prg/lstgwbas.sd7       0.035156            577
seed7/prg/mahjong.sd7        0.032826           1943
seed7/prg/make7.sd7          0.032774            121
seed7/prg/mandelbr.sd7       0.033697            237
seed7/prg/mind.sd7           0.033696            443
seed7/prg/mirror.sd7         0.033344            131
seed7/prg/ms.sd7             0.033457            641
seed7/prg/nicoma.sd7         0.033552            135
seed7/prg/pac.sd7            0.033639            726
seed7/prg/pairs.sd7          0.033593           2025
seed7/prg/panic.sd7          0.033901           2634
seed7/prg/percolation.sd7    0.033248            330
seed7/prg/planets.sd7        0.033721           1486
seed7/prg/portfwd7.sd7       0.033577            139
seed7/prg/prime.sd7          0.033717             74
seed7/prg/printpi1.sd7       0.033706             56
seed7/prg/printpi2.sd7       0.036651             54
seed7/prg/printpi3.sd7       0.039456             60
seed7/prg/pv7.sd7            0.034217            337
seed7/prg/queen.sd7          0.033565            149
seed7/prg/rand.sd7           0.033483            121
seed7/prg/raytrace.sd7       0.034011            538
seed7/prg/rever.sd7          0.035140            816
seed7/prg/roman.sd7          0.035788             38
seed7/prg/s7c.sd7            0.034648           9060
seed7/prg/s7check.sd7        0.033698             68
seed7/prg/savehd7.sd7        0.033792           1110
seed7/prg/self.sd7           0.033321             49
seed7/prg/shisen.sd7         0.033142           1423
seed7/prg/sl.sd7             0.032506           1029
seed7/prg/snake.sd7          0.032307            615
seed7/prg/sokoban.sd7        0.034017            891
seed7/prg/spigotpi.sd7       0.032452             64
seed7/prg/sql7.sd7           0.032420            278
seed7/prg/startrek.sd7       0.034127            979
seed7/prg/sudoku7.sd7        0.033761           2657
seed7/prg/sydir7.sd7         0.033611            384
seed7/prg/syntaxhl.sd7       0.035182            177
seed7/prg/tak.sd7            0.035096             59
seed7/prg/tar7.sd7           0.033704            121
seed7/prg/tch.sd7            0.033636             55
seed7/prg/testfont.sd7       0.033518             95
seed7/prg/tet.sd7            0.033312            479
seed7/prg/tetg.sd7           0.033809            501
seed7/prg/toutf8.sd7         0.033781            240
seed7/prg/tst_cli.sd7        0.033460             40
seed7/prg/tst_srv.sd7        0.033210             47
seed7/prg/wator.sd7          0.033811            651
seed7/prg/which.sd7          0.033713             65
seed7/prg/wiz.sd7            0.033904           2833
seed7/prg/wordcnt.sd7        0.033649             54
seed7/prg/wrinum.sd7         0.033633             43
seed7/prg/wumpus.sd7         0.034158            372
seed7/lib/aes.s7i            0.033689           1144
seed7/lib/aes_gcm.s7i        0.034107            392
seed7/lib/ar.s7i             0.033894           1532
seed7/lib/arc4.s7i           0.033334            144
seed7/lib/archive.s7i        0.033797            143
seed7/lib/archive_base.s7i   0.033676            135
seed7/lib/array.s7i          0.033478            610
seed7/lib/asn1.s7i           0.032888            544
seed7/lib/asn1oid.s7i        0.032742            157
seed7/lib/basearray.s7i      0.032899            450
seed7/lib/bigfile.s7i        0.033734            136
seed7/lib/bigint.s7i         0.032672            824
seed7/lib/bigrat.s7i         0.032841            784
seed7/lib/bin16.s7i          0.033747            592
seed7/lib/bin32.s7i          0.033576            490
seed7/lib/bin64.s7i          0.033473            539
seed7/lib/bitdata.s7i        0.033789           1330
seed7/lib/bitmapfont.s7i     0.033511            215
seed7/lib/bitset.s7i         0.033530            593
seed7/lib/bitsetof.s7i       0.033426            431
seed7/lib/blowfish.s7i       0.033837            383
seed7/lib/bmp.s7i            0.033523            924
seed7/lib/boolean.s7i        0.033878            403
seed7/lib/browser.s7i        0.033913            280
seed7/lib/bstring.s7i        0.033618            227
seed7/lib/bytedata.s7i       0.034080            482
seed7/lib/bzip2.s7i          0.035466            887
seed7/lib/cards.s7i          0.039913           1342
seed7/lib/category.s7i       0.034571            209
seed7/lib/cc_conf.s7i        0.033711           1314
seed7/lib/ccittfax.s7i       0.034267           1022
seed7/lib/cgi.s7i            0.034142            109
seed7/lib/cgidialog.s7i      0.033430           1118
seed7/lib/char.s7i           0.033464            356
seed7/lib/charsets.s7i       0.033604           2024
seed7/lib/chartype.s7i       0.033599            121
seed7/lib/cipher.s7i         0.033554            146
seed7/lib/cli_cmds.s7i       0.033353           1360
seed7/lib/clib_file.s7i      0.033214            301
seed7/lib/color.s7i          0.032495            185
seed7/lib/complex.s7i        0.032423            464
seed7/lib/compress.s7i       0.032622            150
seed7/lib/console.s7i        0.032389            188
seed7/lib/cpio.s7i           0.032344           1708
seed7/lib/crc32.s7i          0.034369            193
seed7/lib/cronos16.s7i       0.034096           1173
seed7/lib/cronos27.s7i       0.033660           1464
seed7/lib/csv.s7i            0.033489            201
seed7/lib/db_prop.s7i        0.033770            991
seed7/lib/deflate.s7i        0.033618            740
seed7/lib/des.s7i            0.032942            444
seed7/lib/dialog.s7i         0.032495            311
seed7/lib/dir.s7i            0.032360            163
seed7/lib/draw.s7i           0.032848            854
seed7/lib/duration.s7i       0.032419           1038
seed7/lib/echo.s7i           0.032318            132
seed7/lib/editline.s7i       0.033005            398
seed7/lib/elf.s7i            0.032503           1560
seed7/lib/elliptic.s7i       0.032445            649
seed7/lib/enable_io.s7i      0.033412            312
seed7/lib/encoding.s7i       0.033801            931
seed7/lib/enumeration.s7i    0.033547            236
seed7/lib/environment.s7i    0.032963            175
seed7/lib/exif.s7i           0.032343            152
seed7/lib/external_file.s7i  0.032655            340
seed7/lib/field.s7i          0.032898            268
seed7/lib/file.s7i           0.032016            372
seed7/lib/filebits.s7i       0.032484             46
seed7/lib/filesys.s7i        0.032769            601
seed7/lib/fileutil.s7i       0.033378            144
seed7/lib/fixarray.s7i       0.033609            307
seed7/lib/float.s7i          0.032814            757
seed7/lib/font.s7i           0.032887            196
seed7/lib/font8x8.s7i        0.032803            998
seed7/lib/forloop.s7i        0.032786            449
seed7/lib/ftp.s7i            0.032696            969
seed7/lib/ftpserv.s7i        0.032405            631
seed7/lib/getf.s7i           0.034285            115
seed7/lib/gethttp.s7i        0.033753             41
seed7/lib/gethttps.s7i       0.033676             41
seed7/lib/gif.s7i            0.033742            561
seed7/lib/graph.s7i          0.033274            415
seed7/lib/graph_file.s7i     0.034161            399
seed7/lib/gtkserver.s7i      0.033338            161
seed7/lib/gzip.s7i           0.033628            573
seed7/lib/hash.s7i           0.033383            421
seed7/lib/hashsetof.s7i      0.033615            499
seed7/lib/hmac.s7i           0.033547            152
seed7/lib/html.s7i           0.033806             83
seed7/lib/html_ent.s7i       0.033610            476
seed7/lib/htmldom.s7i        0.033602            286
seed7/lib/http_request.s7i   0.034025            696
seed7/lib/http_srv_resp.s7i  0.033336            380
seed7/lib/https_request.s7i  0.033613            211
seed7/lib/httpserv.s7i       0.033595            345
seed7/lib/huffman.s7i        0.033295            644
seed7/lib/ico.s7i            0.033509            221
seed7/lib/idxarray.s7i       0.033497            232
seed7/lib/image.s7i          0.033490            156
seed7/lib/imagefile.s7i      0.033277            171
seed7/lib/inflate.s7i        0.033675            411
seed7/lib/inifile.s7i        0.033402            129
seed7/lib/integer.s7i        0.033589            663
seed7/lib/iobuffer.s7i       0.032455            289
seed7/lib/jpeg.s7i           0.033125           1761
seed7/lib/json.s7i           0.032649            891
seed7/lib/json_serde.s7i     0.032430            783
seed7/lib/keybd.s7i          0.032571            639
seed7/lib/keydescr.s7i       0.033809            192
seed7/lib/leb128.s7i         0.033662            218
seed7/lib/line.s7i           0.033580            164
seed7/lib/listener.s7i       0.033790            247
seed7/lib/logfile.s7i        0.033643             73
seed7/lib/lower.s7i          0.033790            142
seed7/lib/lzma.s7i           0.033824            934
seed7/lib/lzw.s7i            0.033869            861
seed7/lib/magic.s7i          0.033560            403
seed7/lib/mahjng32.s7i       0.033505           1500
seed7/lib/make.s7i           0.033682            544
seed7/lib/makedata.s7i       0.033190           1428
seed7/lib/math.s7i           0.033387            201
seed7/lib/mixarith.s7i       0.033994            249
seed7/lib/modern27.s7i       0.033578           1099
seed7/lib/more.s7i           0.033777            130
seed7/lib/msgdigest.s7i      0.033754           1222
seed7/lib/multiscr.s7i       0.033598             68
seed7/lib/null_file.s7i      0.033758            345
seed7/lib/osfiles.s7i        0.033542           1085
seed7/lib/pbm.s7i            0.033918            230
seed7/lib/pcx.s7i            0.033688            638
seed7/lib/pem.s7i            0.035237            185
seed7/lib/pgm.s7i            0.034532            238
seed7/lib/pic16.s7i          0.033965           1037
seed7/lib/pic32.s7i          0.033776           2060
seed7/lib/pic_util.s7i       0.032869            144
seed7/lib/pixelimage.s7i     0.032723            320
seed7/lib/pixmap_file.s7i    0.033070            459
seed7/lib/pixmapfont.s7i     0.032758            184
seed7/lib/pkcs1.s7i          0.032447            543
seed7/lib/png.s7i            0.033385           1064
seed7/lib/poll.s7i           0.034126            313
seed7/lib/ppm.s7i            0.034017            240
seed7/lib/process.s7i        0.034332            541
seed7/lib/progs.s7i          0.035227            789
seed7/lib/propertyfile.s7i   0.033471            155
seed7/lib/rational.s7i       0.033545            792
seed7/lib/ref_list.s7i       0.033443            252
seed7/lib/reference.s7i      0.033865            126
seed7/lib/reverse.s7i        0.033703             94
seed7/lib/rpm.s7i            0.034304           3487
seed7/lib/rpmext.s7i         0.033921            318
seed7/lib/scanfile.s7i       0.033744           1779
seed7/lib/scanjson.s7i       0.033614            413
seed7/lib/scanstri.s7i       0.033638           1814
seed7/lib/scantoml.s7i       0.033509           1603
seed7/lib/seed7_05.s7i       0.033579           1072
seed7/lib/set.s7i            0.033562             57
seed7/lib/shell.s7i          0.033525            615
seed7/lib/showtls.s7i        0.033743            678
seed7/lib/signature.s7i      0.033732            131
seed7/lib/smtp.s7i           0.033468            261
seed7/lib/sockbase.s7i       0.033646            217
seed7/lib/socket.s7i         0.033639            326
seed7/lib/sokoban1.s7i       0.033575           1519
seed7/lib/sql_base.s7i       0.032949           1000
seed7/lib/stars.s7i          0.032927           1705
seed7/lib/stdfont10.s7i      0.032955           3347
seed7/lib/stdfont12.s7i      0.032651           3928
seed7/lib/stdfont14.s7i      0.032901           4510
seed7/lib/stdfont16.s7i      0.033950           5092
seed7/lib/stdfont18.s7i      0.033986           5868
seed7/lib/stdfont20.s7i      0.033926           6449
seed7/lib/stdfont24.s7i      0.033951           7421
seed7/lib/stdfont8.s7i       0.033367           2960
seed7/lib/stdfont9.s7i       0.033876           3152
seed7/lib/stdio.s7i          0.033738            192
seed7/lib/strifile.s7i       0.033437            345
seed7/lib/string.s7i         0.033664            779
seed7/lib/stritext.s7i       0.034477            352
seed7/lib/struct.s7i         0.033627            266
seed7/lib/struct_elem.s7i    0.035711            129
seed7/lib/subfile.s7i        0.034236            174
seed7/lib/subrange.s7i       0.035905             78
seed7/lib/syntax.s7i         0.036301            294
seed7/lib/tar.s7i            0.034496           1880
seed7/lib/tar_cmds.s7i       0.033516            752
seed7/lib/tdes.s7i           0.033801            143
seed7/lib/tee.s7i            0.033694            143
seed7/lib/text.s7i           0.033606            135
seed7/lib/tga.s7i            0.033216            676
seed7/lib/tiff.s7i           0.033870           2771
seed7/lib/time.s7i           0.033496           1191
seed7/lib/tls.s7i            0.033556           2230
seed7/lib/unicode.s7i        0.032946            575
seed7/lib/unionfnd.s7i       0.032725            130
seed7/lib/upper.s7i          0.032547            142
seed7/lib/utf16.s7i          0.032539            540
seed7/lib/utf8.s7i           0.032322            234
seed7/lib/vecfont10.s7i      0.032417           1056
seed7/lib/vecfont18.s7i      0.034609           1119
seed7/lib/vector3d.s7i       0.033676            293
seed7/lib/vectorfont.s7i     0.032849            239
seed7/lib/wildcard.s7i       0.032487            140
seed7/lib/window.s7i         0.032409            455
seed7/lib/wrinum.s7i         0.032510            248
seed7/lib/x509cert.s7i       0.032527           1243
seed7/lib/xml_ent.s7i        0.032240             94
seed7/lib/xmldom.s7i         0.033070            303
seed7/lib/xz.s7i             0.032613            442
seed7/lib/zip.s7i            0.033316           2792
seed7/lib/zstd.s7i           0.033137           1263
============================ ================= ================

Statistical Summary — Mode A (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.033626        |
+-----------+-----------------+
| Minimum   | 0.032016        |
+-----------+-----------------+
| Maximum   | 0.039913        |
+-----------+-----------------+

Cross-Mode Comparison
=====================

+------+-----------------+-----------------+-----------------+
| Mode | Average (s)     | Minimum (s)     | Maximum (s)     |
+======+=================+=================+=================+
| A    | 0.033626        | 0.032016        | 0.039913        |
+------+-----------------+-----------------+-----------------+

Phase Timing Summary
====================

**Partial report: interrupted during Mode B warm-up.**

Wall-clock times include warm-up work and GC performed by the warm-up helper.

+----------+--------------+--------------+--------------+
| Phase    | Warm-up      | Timed pass   | Mode total   |
+==========+==============+==============+==============+
| Mode A   | 00:00:12.275 | 00:00:57.280 | 00:01:09.555 |
+----------+--------------+--------------+--------------+
| Total    |              |              | 00:01:32.375 |
+----------+--------------+--------------+--------------+


.. ---------------------------------------------------------------------------
