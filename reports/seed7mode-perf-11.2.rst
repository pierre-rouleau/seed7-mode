=======================================================
GC-Controlled Benchmark Report: Seed7 Mode — Four Modes
=======================================================

:Running with: seed7-mode 2026-06-25T14:41:17+0000 W26-4
:Seed7 dir   : /Users/roup/my/dvo/seed7-repos/seed7/
:Started at: 10:52:57 local time
:Generated on: 2026-06-25 15:04:30 UTC
:N Iterations: 5  (mean of N timed opens per file)
:Window system: nil
:Display graphic: nil
:Frame size: 253x95 chars
:Window body: 253x93 chars
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
seed7/prg/addup.sd7          0.034187            190
seed7/prg/bas7.sd7           0.034177          11459
seed7/prg/bifurk.sd7         0.034839             73
seed7/prg/bigfiles.sd7       0.034666            129
seed7/prg/brainf7.sd7        0.035568             86
seed7/prg/calc7.sd7          0.034224            128
seed7/prg/carddemo.sd7       0.033602            190
seed7/prg/castle.sd7         0.033618           3148
seed7/prg/cat.sd7            0.032914             82
seed7/prg/cellauto.sd7       0.034849             85
seed7/prg/celsius.sd7        0.032859             42
seed7/prg/chk_all.sd7        0.032745            843
seed7/prg/chkarr.sd7         0.032824           8367
seed7/prg/chkbig.sd7         0.037690          29026
seed7/prg/chkbin.sd7         0.034230           6469
seed7/prg/chkbitdata.sd7     0.036221           6624
seed7/prg/chkbool.sd7        0.037582           3157
seed7/prg/chkbst.sd7         0.034393            722
seed7/prg/chkchr.sd7         0.034039           2809
seed7/prg/chkcmd.sd7         0.033619           1205
seed7/prg/chkdb.sd7          0.034398           7454
seed7/prg/chkdecl.sd7        0.034638            448
seed7/prg/chkenum.sd7        0.036616           1230
seed7/prg/chkerr.sd7         0.034975           4663
seed7/prg/chkexc.sd7         0.033712           2627
seed7/prg/chkfil.sd7         0.034959           1615
seed7/prg/chkflt.sd7         0.044017          20620
seed7/prg/chkhent.sd7        0.037049             54
seed7/prg/chkhsh.sd7         0.040378           4548
seed7/prg/chkidx.sd7         0.035549          19567
seed7/prg/chkint.sd7         0.039219          38129
seed7/prg/chkjson.sd7        0.032710           1764
seed7/prg/chkovf.sd7         0.033443           8216
seed7/prg/chkprc.sd7         0.034465          10111
seed7/prg/chkscan.sd7        0.037278            714
seed7/prg/chkset.sd7         0.035192          11974
seed7/prg/chkstr.sd7         0.037366          26952
seed7/prg/chktime.sd7        0.032907           2025
seed7/prg/chktoml.sd7        0.033024           1656
seed7/prg/clock.sd7          0.032784             47
seed7/prg/clock2.sd7         0.032890             43
seed7/prg/clock3.sd7         0.034194             95
seed7/prg/cmpfil.sd7         0.034619             84
seed7/prg/comanche.sd7       0.033578            180
seed7/prg/confval.sd7        0.033391            175
seed7/prg/db7.sd7            0.033514            417
seed7/prg/diff7.sd7          0.033785            263
seed7/prg/dirtst.sd7         0.033680             42
seed7/prg/dirx.sd7           0.033493            152
seed7/prg/dnafight.sd7       0.034157           1381
seed7/prg/dragon.sd7         0.034738             73
seed7/prg/echo.sd7           0.033749             39
seed7/prg/eliza.sd7          0.033704            302
seed7/prg/err.sd7            0.033703             96
seed7/prg/fannkuch.sd7       0.033317            131
seed7/prg/fib.sd7            0.033352             47
seed7/prg/find7.sd7          0.033600            133
seed7/prg/findchar.sd7       0.033300            149
seed7/prg/fractree.sd7       0.033300             55
seed7/prg/ftp7.sd7           0.033535            296
seed7/prg/ftpserv.sd7        0.033398             74
seed7/prg/gcd.sd7            0.033443            109
seed7/prg/gkbd.sd7           0.033257            358
seed7/prg/gtksvtst.sd7       0.033687             94
seed7/prg/hal.sd7            0.033782            250
seed7/prg/hamu.sd7           0.033367            573
seed7/prg/hanoi.sd7          0.033143             55
seed7/prg/hd.sd7             0.034693             79
seed7/prg/hello.sd7          0.032539             32
seed7/prg/hilbert.sd7        0.032510            108
seed7/prg/ide7.sd7           0.038715            196
seed7/prg/kbd.sd7            0.037108             49
seed7/prg/klondike.sd7       0.035405            883
seed7/prg/lander.sd7         0.034185           1551
seed7/prg/lst80bas.sd7       0.036760            344
seed7/prg/lst99bas.sd7       0.035547            401
seed7/prg/lstgwbas.sd7       0.033789            577
seed7/prg/mahjong.sd7        0.033041           1943
seed7/prg/make7.sd7          0.032669            121
seed7/prg/mandelbr.sd7       0.033417            237
seed7/prg/mind.sd7           0.032960            443
seed7/prg/mirror.sd7         0.033621            131
seed7/prg/ms.sd7             0.032361            641
seed7/prg/nicoma.sd7         0.032308            135
seed7/prg/pac.sd7            0.033790            726
seed7/prg/pairs.sd7          0.033571           2025
seed7/prg/panic.sd7          0.033130           2634
seed7/prg/percolation.sd7    0.033521            330
seed7/prg/planets.sd7        0.033514           1486
seed7/prg/portfwd7.sd7       0.033293            139
seed7/prg/prime.sd7          0.033376             74
seed7/prg/printpi1.sd7       0.033404             56
seed7/prg/printpi2.sd7       0.033420             54
seed7/prg/printpi3.sd7       0.033434             60
seed7/prg/pv7.sd7            0.033989            337
seed7/prg/queen.sd7          0.032476            149
seed7/prg/rand.sd7           0.032698            121
seed7/prg/raytrace.sd7       0.033039            538
seed7/prg/rever.sd7          0.032378            816
seed7/prg/roman.sd7          0.032171             38
seed7/prg/s7c.sd7            0.033064           9060
seed7/prg/s7check.sd7        0.034377             68
seed7/prg/savehd7.sd7        0.034329           1110
seed7/prg/self.sd7           0.034092             49
seed7/prg/shisen.sd7         0.033192           1423
seed7/prg/sl.sd7             0.033996           1029
seed7/prg/snake.sd7          0.033403            615
seed7/prg/sokoban.sd7        0.033348            891
seed7/prg/spigotpi.sd7       0.033277             64
seed7/prg/sql7.sd7           0.033955            278
seed7/prg/startrek.sd7       0.033265            979
seed7/prg/sudoku7.sd7        0.033836           2657
seed7/prg/sydir7.sd7         0.038971            384
seed7/prg/syntaxhl.sd7       0.036541            177
seed7/prg/tak.sd7            0.033731             59
seed7/prg/tar7.sd7           0.033880            121
seed7/prg/tch.sd7            0.033897             55
seed7/prg/testfont.sd7       0.033516             95
seed7/prg/tet.sd7            0.033465            479
seed7/prg/tetg.sd7           0.033772            501
seed7/prg/toutf8.sd7         0.032731            240
seed7/prg/tst_cli.sd7        0.032677             40
seed7/prg/tst_srv.sd7        0.034826             47
seed7/prg/wator.sd7          0.036413            651
seed7/prg/which.sd7          0.033989             65
seed7/prg/wiz.sd7            0.035087           2833
seed7/prg/wordcnt.sd7        0.036671             54
seed7/prg/wrinum.sd7         0.034306             43
seed7/prg/wumpus.sd7         0.032667            372
seed7/lib/aes.s7i            0.033208           1144
seed7/lib/aes_gcm.s7i        0.034050            392
seed7/lib/ar.s7i             0.033764           1532
seed7/lib/arc4.s7i           0.033263            144
seed7/lib/archive.s7i        0.033589            143
seed7/lib/archive_base.s7i   0.034209            135
seed7/lib/array.s7i          0.034044            610
seed7/lib/asn1.s7i           0.033734            544
seed7/lib/asn1oid.s7i        0.033735            157
seed7/lib/basearray.s7i      0.033386            450
seed7/lib/bigfile.s7i        0.033361            136
seed7/lib/bigint.s7i         0.033779            824
seed7/lib/bigrat.s7i         0.033725            784
seed7/lib/bin16.s7i          0.035818            592
seed7/lib/bin32.s7i          0.036521            490
seed7/lib/bin64.s7i          0.033709            539
seed7/lib/bitdata.s7i        0.033780           1330
seed7/lib/bitmapfont.s7i     0.033281            215
seed7/lib/bitset.s7i         0.034881            593
seed7/lib/bitsetof.s7i       0.035703            431
seed7/lib/blowfish.s7i       0.033958            383
seed7/lib/bmp.s7i            0.033212            924
seed7/lib/boolean.s7i        0.034211            403
seed7/lib/browser.s7i        0.033986            280
seed7/lib/bstring.s7i        0.033345            227
seed7/lib/bytedata.s7i       0.033661            482
seed7/lib/bzip2.s7i          0.032329            887
seed7/lib/cards.s7i          0.034399           1342
seed7/lib/category.s7i       0.035361            209
seed7/lib/cc_conf.s7i        0.032758           1314
seed7/lib/ccittfax.s7i       0.032632           1022
seed7/lib/cgi.s7i            0.033492            109
seed7/lib/cgidialog.s7i      0.033985           1118
seed7/lib/char.s7i           0.033493            356
seed7/lib/charsets.s7i       0.034356           2024
seed7/lib/chartype.s7i       0.034754            121
seed7/lib/cipher.s7i         0.037376            146
seed7/lib/cli_cmds.s7i       0.032827           1360
seed7/lib/clib_file.s7i      0.032784            301
seed7/lib/color.s7i          0.032629            185
seed7/lib/complex.s7i        0.032602            464
seed7/lib/compress.s7i       0.032823            150
seed7/lib/console.s7i        0.032482            188
seed7/lib/cpio.s7i           0.032866           1708
seed7/lib/crc32.s7i          0.032579            193
seed7/lib/cronos16.s7i       0.033002           1173
seed7/lib/cronos27.s7i       0.033107           1464
seed7/lib/csv.s7i            0.034580            201
seed7/lib/db_prop.s7i        0.034025            991
seed7/lib/deflate.s7i        0.033761            740
seed7/lib/des.s7i            0.033861            444
seed7/lib/dialog.s7i         0.033689            311
seed7/lib/dir.s7i            0.033714            163
seed7/lib/draw.s7i           0.033961            854
seed7/lib/duration.s7i       0.034199           1038
seed7/lib/echo.s7i           0.035487            132
seed7/lib/editline.s7i       0.033764            398
seed7/lib/elf.s7i            0.035492           1560
seed7/lib/elliptic.s7i       0.035291            649
seed7/lib/enable_io.s7i      0.036586            312
seed7/lib/encoding.s7i       0.033527            931
seed7/lib/enumeration.s7i    0.032395            236
seed7/lib/environment.s7i    0.032424            175
seed7/lib/exif.s7i           0.034110            152
seed7/lib/external_file.s7i  0.033526            340
seed7/lib/field.s7i          0.033650            268
seed7/lib/file.s7i           0.033497            372
seed7/lib/filebits.s7i       0.033414             46
seed7/lib/filesys.s7i        0.033465            601
seed7/lib/fileutil.s7i       0.033437            144
seed7/lib/fixarray.s7i       0.033689            307
seed7/lib/float.s7i          0.033386            757
seed7/lib/font.s7i           0.033989            196
seed7/lib/font8x8.s7i        0.033736            998
seed7/lib/forloop.s7i        0.033367            449
seed7/lib/ftp.s7i            0.033632            969
seed7/lib/ftpserv.s7i        0.033493            631
seed7/lib/getf.s7i           0.033547            115
seed7/lib/gethttp.s7i        0.033664             41
seed7/lib/gethttps.s7i       0.033293             41
seed7/lib/gif.s7i            0.038822            561
seed7/lib/graph.s7i          0.038065            415
seed7/lib/graph_file.s7i     0.033538            399
seed7/lib/gtkserver.s7i      0.032916            161
seed7/lib/gzip.s7i           0.032653            573
seed7/lib/hash.s7i           0.032303            421
seed7/lib/hashsetof.s7i      0.032676            499
seed7/lib/hmac.s7i           0.033310            152
seed7/lib/html.s7i           0.032938             83
seed7/lib/html_ent.s7i       0.032716            476
seed7/lib/htmldom.s7i        0.033064            286
seed7/lib/http_request.s7i   0.032890            696
seed7/lib/http_srv_resp.s7i  0.032664            380
seed7/lib/https_request.s7i  0.032603            211
seed7/lib/httpserv.s7i       0.033187            345
seed7/lib/huffman.s7i        0.035333            644
seed7/lib/ico.s7i            0.035677            221
seed7/lib/idxarray.s7i       0.034110            232
seed7/lib/image.s7i          0.033528            156
seed7/lib/imagefile.s7i      0.033476            171
seed7/lib/inflate.s7i        0.033650            411
seed7/lib/inifile.s7i        0.033485            129
seed7/lib/integer.s7i        0.034026            663
seed7/lib/iobuffer.s7i       0.033934            289
seed7/lib/jpeg.s7i           0.033597           1761
seed7/lib/json.s7i           0.033602            891
seed7/lib/json_serde.s7i     0.033396            783
seed7/lib/keybd.s7i          0.034372            639
seed7/lib/keydescr.s7i       0.033957            192
seed7/lib/leb128.s7i         0.033686            218
seed7/lib/line.s7i           0.033800            164
seed7/lib/listener.s7i       0.033641            247
seed7/lib/logfile.s7i        0.034006             73
seed7/lib/lower.s7i          0.033586            142
seed7/lib/lzma.s7i           0.033691            934
seed7/lib/lzw.s7i            0.033707            861
seed7/lib/magic.s7i          0.033880            403
seed7/lib/mahjng32.s7i       0.034083           1500
seed7/lib/make.s7i           0.033600            544
seed7/lib/makedata.s7i       0.033540           1428
seed7/lib/math.s7i           0.033231            201
seed7/lib/mixarith.s7i       0.032461            249
seed7/lib/modern27.s7i       0.033771           1099
seed7/lib/more.s7i           0.032702            130
seed7/lib/msgdigest.s7i      0.032757           1222
seed7/lib/multiscr.s7i       0.032762             68
seed7/lib/null_file.s7i      0.034700            345
seed7/lib/osfiles.s7i        0.033196           1085
seed7/lib/pbm.s7i            0.033095            230
seed7/lib/pcx.s7i            0.032517            638
seed7/lib/pem.s7i            0.032964            185
seed7/lib/pgm.s7i            0.033124            238
seed7/lib/pic16.s7i          0.032412           1037
seed7/lib/pic32.s7i          0.032670           2060
seed7/lib/pic_util.s7i       0.033149            144
seed7/lib/pixelimage.s7i     0.032401            320
seed7/lib/pixmap_file.s7i    0.032834            459
seed7/lib/pixmapfont.s7i     0.033522            184
seed7/lib/pkcs1.s7i          0.033447            543
seed7/lib/png.s7i            0.033542           1064
seed7/lib/poll.s7i           0.033321            313
seed7/lib/ppm.s7i            0.033517            240
seed7/lib/process.s7i        0.033754            541
seed7/lib/progs.s7i          0.033590            789
seed7/lib/propertyfile.s7i   0.033362            155
seed7/lib/rational.s7i       0.033535            792
seed7/lib/ref_list.s7i       0.034032            252
seed7/lib/reference.s7i      0.033751            126
seed7/lib/reverse.s7i        0.033568             94
seed7/lib/rpm.s7i            0.034370           3487
seed7/lib/rpmext.s7i         0.033136            318
seed7/lib/scanfile.s7i       0.032530           1779
seed7/lib/scanjson.s7i       0.032899            413
seed7/lib/scanstri.s7i       0.032923           1814
seed7/lib/scantoml.s7i       0.032336           1603
seed7/lib/seed7_05.s7i       0.032405           1072
seed7/lib/set.s7i            0.034519             57
seed7/lib/shell.s7i          0.033611            615
seed7/lib/showtls.s7i        0.034261            678
seed7/lib/signature.s7i      0.033602            131
seed7/lib/smtp.s7i           0.034417            261
seed7/lib/sockbase.s7i       0.033372            217
seed7/lib/socket.s7i         0.037697            326
seed7/lib/sokoban1.s7i       0.037680           1519
seed7/lib/sql_base.s7i       0.037844           1000
seed7/lib/stars.s7i          0.034014           1705
seed7/lib/stdfont10.s7i      0.033806           3347
seed7/lib/stdfont12.s7i      0.033799           3928
seed7/lib/stdfont14.s7i      0.033777           4510
seed7/lib/stdfont16.s7i      0.033759           5092
seed7/lib/stdfont18.s7i      0.034373           5868
seed7/lib/stdfont20.s7i      0.033790           6449
seed7/lib/stdfont24.s7i      0.037282           7421
seed7/lib/stdfont8.s7i       0.040517           2960
seed7/lib/stdfont9.s7i       0.033979           3152
seed7/lib/stdio.s7i          0.032751            192
seed7/lib/strifile.s7i       0.032775            345
seed7/lib/string.s7i         0.032454            779
seed7/lib/stritext.s7i       0.032443            352
seed7/lib/struct.s7i         0.032973            266
seed7/lib/struct_elem.s7i    0.032663            129
seed7/lib/subfile.s7i        0.032505            174
seed7/lib/subrange.s7i       0.032837             78
seed7/lib/syntax.s7i         0.033134            294
seed7/lib/tar.s7i            0.033101           1880
seed7/lib/tar_cmds.s7i       0.032435            752
seed7/lib/tdes.s7i           0.032423            143
seed7/lib/tee.s7i            0.034283            143
seed7/lib/text.s7i           0.033914            135
seed7/lib/tga.s7i            0.034011            676
seed7/lib/tiff.s7i           0.033526           2771
seed7/lib/time.s7i           0.035596           1191
seed7/lib/tls.s7i            0.036232           2230
seed7/lib/unicode.s7i        0.033811            575
seed7/lib/unionfnd.s7i       0.033874            130
seed7/lib/upper.s7i          0.033464            142
seed7/lib/utf16.s7i          0.033830            540
seed7/lib/utf8.s7i           0.033848            234
seed7/lib/vecfont10.s7i      0.033813           1056
seed7/lib/vecfont18.s7i      0.033197           1119
seed7/lib/vector3d.s7i       0.033456            293
seed7/lib/vectorfont.s7i     0.033689            239
seed7/lib/wildcard.s7i       0.033793            140
seed7/lib/window.s7i         0.033683            455
seed7/lib/wrinum.s7i         0.033478            248
seed7/lib/x509cert.s7i       0.039437           1243
seed7/lib/xml_ent.s7i        0.037176             94
seed7/lib/xmldom.s7i         0.033471            303
seed7/lib/xz.s7i             0.033295            442
seed7/lib/zip.s7i            0.033668           2792
seed7/lib/zstd.s7i           0.033508           1263
============================ ================= ================

Statistical Summary — Mode A (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.033959        |
+-----------+-----------------+
| Minimum   | 0.032171        |
+-----------+-----------------+
| Maximum   | 0.044017        |
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
seed7/prg/addup.sd7          0.038871            190
seed7/prg/bas7.sd7           0.040081          11459
seed7/prg/bifurk.sd7         0.035555             73
seed7/prg/bigfiles.sd7       0.037347            129
seed7/prg/brainf7.sd7        0.036419             86
seed7/prg/calc7.sd7          0.037085            128
seed7/prg/carddemo.sd7       0.038067            190
seed7/prg/castle.sd7         0.038209           3148
seed7/prg/cat.sd7            0.036091             82
seed7/prg/cellauto.sd7       0.037108             85
seed7/prg/celsius.sd7        0.034928             42
seed7/prg/chk_all.sd7        0.040000            843
seed7/prg/chkarr.sd7         0.040272           8367
seed7/prg/chkbig.sd7         0.044016          29026
seed7/prg/chkbin.sd7         0.040211           6469
seed7/prg/chkbitdata.sd7     0.040281           6624
seed7/prg/chkbool.sd7        0.037784           3157
seed7/prg/chkbst.sd7         0.038411            722
seed7/prg/chkchr.sd7         0.039844           2809
seed7/prg/chkcmd.sd7         0.037698           1205
seed7/prg/chkdb.sd7          0.038734           7454
seed7/prg/chkdecl.sd7        0.037977            448
seed7/prg/chkenum.sd7        0.037771           1230
seed7/prg/chkerr.sd7         0.038489           4663
seed7/prg/chkexc.sd7         0.039054           2627
seed7/prg/chkfil.sd7         0.039056           1615
seed7/prg/chkflt.sd7         0.043095          20620
seed7/prg/chkhent.sd7        0.036861             54
seed7/prg/chkhsh.sd7         0.047065           4548
seed7/prg/chkidx.sd7         0.042864          19567
seed7/prg/chkint.sd7         0.047221          38129
seed7/prg/chkjson.sd7        0.038955           1764
seed7/prg/chkovf.sd7         0.041656           8216
seed7/prg/chkprc.sd7         0.041640          10111
seed7/prg/chkscan.sd7        0.038801            714
seed7/prg/chkset.sd7         0.039256          11974
seed7/prg/chkstr.sd7         0.042699          26952
seed7/prg/chktime.sd7        0.040888           2025
seed7/prg/chktoml.sd7        0.039218           1656
seed7/prg/clock.sd7          0.036263             47
seed7/prg/clock2.sd7         0.036022             43
seed7/prg/clock3.sd7         0.039755             95
seed7/prg/cmpfil.sd7         0.037884             84
seed7/prg/comanche.sd7       0.039056            180
seed7/prg/confval.sd7        0.039663            175
seed7/prg/db7.sd7            0.039197            417
seed7/prg/diff7.sd7          0.039661            263
seed7/prg/dirtst.sd7         0.035971             42
seed7/prg/dirx.sd7           0.038426            152
seed7/prg/dnafight.sd7       0.040298           1381
seed7/prg/dragon.sd7         0.037161             73
seed7/prg/echo.sd7           0.035508             39
seed7/prg/eliza.sd7          0.038528            302
seed7/prg/err.sd7            0.041394             96
seed7/prg/fannkuch.sd7       0.038254            131
seed7/prg/fib.sd7            0.034891             47
seed7/prg/find7.sd7          0.037682            133
seed7/prg/findchar.sd7       0.037863            149
seed7/prg/fractree.sd7       0.036568             55
seed7/prg/ftp7.sd7           0.038011            296
seed7/prg/ftpserv.sd7        0.036685             74
seed7/prg/gcd.sd7            0.036635            109
seed7/prg/gkbd.sd7           0.039702            358
seed7/prg/gtksvtst.sd7       0.036900             94
seed7/prg/hal.sd7            0.037400            250
seed7/prg/hamu.sd7           0.043061            573
seed7/prg/hanoi.sd7          0.040742             55
seed7/prg/hd.sd7             0.041748             79
seed7/prg/hello.sd7          0.035637             32
seed7/prg/hilbert.sd7        0.038426            108
seed7/prg/ide7.sd7           0.038872            196
seed7/prg/kbd.sd7            0.036040             49
seed7/prg/klondike.sd7       0.038503            883
seed7/prg/lander.sd7         0.039124           1551
seed7/prg/lst80bas.sd7       0.038093            344
seed7/prg/lst99bas.sd7       0.039960            401
seed7/prg/lstgwbas.sd7       0.039635            577
seed7/prg/mahjong.sd7        0.039307           1943
seed7/prg/make7.sd7          0.038738            121
seed7/prg/mandelbr.sd7       0.039123            237
seed7/prg/mind.sd7           0.042099            443
seed7/prg/mirror.sd7         0.046929            131
seed7/prg/ms.sd7             0.039551            641
seed7/prg/nicoma.sd7         0.039150            135
seed7/prg/pac.sd7            0.038712            726
seed7/prg/pairs.sd7          0.038826           2025
seed7/prg/panic.sd7          0.039790           2634
seed7/prg/percolation.sd7    0.038502            330
seed7/prg/planets.sd7        0.038518           1486
seed7/prg/portfwd7.sd7       0.038560            139
seed7/prg/prime.sd7          0.036292             74
seed7/prg/printpi1.sd7       0.035634             56
seed7/prg/printpi2.sd7       0.043093             54
seed7/prg/printpi3.sd7       0.043557             60
seed7/prg/pv7.sd7            0.042860            337
seed7/prg/queen.sd7          0.039699            149
seed7/prg/rand.sd7           0.037337            121
seed7/prg/raytrace.sd7       0.038389            538
seed7/prg/rever.sd7          0.037742            816
seed7/prg/roman.sd7          0.035017             38
seed7/prg/s7c.sd7            0.038334           9060
seed7/prg/s7check.sd7        0.036795             68
seed7/prg/savehd7.sd7        0.037940           1110
seed7/prg/self.sd7           0.035912             49
seed7/prg/shisen.sd7         0.038653           1423
seed7/prg/sl.sd7             0.038636           1029
seed7/prg/snake.sd7          0.040885            615
seed7/prg/sokoban.sd7        0.040843            891
seed7/prg/spigotpi.sd7       0.036465             64
seed7/prg/sql7.sd7           0.038789            278
seed7/prg/startrek.sd7       0.039048            979
seed7/prg/sudoku7.sd7        0.041071           2657
seed7/prg/sydir7.sd7         0.040699            384
seed7/prg/syntaxhl.sd7       0.041307            177
seed7/prg/tak.sd7            0.041336             59
seed7/prg/tar7.sd7           0.042709            121
seed7/prg/tch.sd7            0.037112             55
seed7/prg/testfont.sd7       0.040588             95
seed7/prg/tet.sd7            0.039884            479
seed7/prg/tetg.sd7           0.037789            501
seed7/prg/toutf8.sd7         0.041904            240
seed7/prg/tst_cli.sd7        0.035685             40
seed7/prg/tst_srv.sd7        0.036139             47
seed7/prg/wator.sd7          0.038927            651
seed7/prg/which.sd7          0.039754             65
seed7/prg/wiz.sd7            0.041526           2833
seed7/prg/wordcnt.sd7        0.036585             54
seed7/prg/wrinum.sd7         0.035921             43
seed7/prg/wumpus.sd7         0.038555            372
seed7/lib/aes.s7i            0.042719           1144
seed7/lib/aes_gcm.s7i        0.039952            392
seed7/lib/ar.s7i             0.039265           1532
seed7/lib/arc4.s7i           0.039530            144
seed7/lib/archive.s7i        0.039558            143
seed7/lib/archive_base.s7i   0.038893            135
seed7/lib/array.s7i          0.039082            610
seed7/lib/asn1.s7i           0.035985            544
seed7/lib/asn1oid.s7i        0.042136            157
seed7/lib/basearray.s7i      0.039454            450
seed7/lib/bigfile.s7i        0.037384            136
seed7/lib/bigint.s7i         0.038036            824
seed7/lib/bigrat.s7i         0.037829            784
seed7/lib/bin16.s7i          0.038024            592
seed7/lib/bin32.s7i          0.038213            490
seed7/lib/bin64.s7i          0.037827            539
seed7/lib/bitdata.s7i        0.043234           1330
seed7/lib/bitmapfont.s7i     0.037816            215
seed7/lib/bitset.s7i         0.040115            593
seed7/lib/bitsetof.s7i       0.040447            431
seed7/lib/blowfish.s7i       0.042148            383
seed7/lib/bmp.s7i            0.039348            924
seed7/lib/boolean.s7i        0.038869            403
seed7/lib/browser.s7i        0.039192            280
seed7/lib/bstring.s7i        0.041148            227
seed7/lib/bytedata.s7i       0.039553            482
seed7/lib/bzip2.s7i          0.039116            887
seed7/lib/cards.s7i          0.036964           1342
seed7/lib/category.s7i       0.039235            209
seed7/lib/cc_conf.s7i        0.038881           1314
seed7/lib/ccittfax.s7i       0.039417           1022
seed7/lib/cgi.s7i            0.039042            109
seed7/lib/cgidialog.s7i      0.039035           1118
seed7/lib/char.s7i           0.039173            356
seed7/lib/charsets.s7i       0.039646           2024
seed7/lib/chartype.s7i       0.042038            121
seed7/lib/cipher.s7i         0.038406            146
seed7/lib/cli_cmds.s7i       0.039271           1360
seed7/lib/clib_file.s7i      0.039122            301
seed7/lib/color.s7i          0.038817            185
seed7/lib/complex.s7i        0.038006            464
seed7/lib/compress.s7i       0.038191            150
seed7/lib/console.s7i        0.038012            188
seed7/lib/cpio.s7i           0.038088           1708
seed7/lib/crc32.s7i          0.038068            193
seed7/lib/cronos16.s7i       0.043468           1173
seed7/lib/cronos27.s7i       0.041969           1464
seed7/lib/csv.s7i            0.038406            201
seed7/lib/db_prop.s7i        0.038147            991
seed7/lib/deflate.s7i        0.038108            740
seed7/lib/des.s7i            0.038470            444
seed7/lib/dialog.s7i         0.037834            311
seed7/lib/dir.s7i            0.037791            163
seed7/lib/draw.s7i           0.037589            854
seed7/lib/duration.s7i       0.037498           1038
seed7/lib/echo.s7i           0.039189            132
seed7/lib/editline.s7i       0.038658            398
seed7/lib/elf.s7i            0.040996           1560
seed7/lib/elliptic.s7i       0.039081            649
seed7/lib/enable_io.s7i      0.039949            312
seed7/lib/encoding.s7i       0.039617            931
seed7/lib/enumeration.s7i    0.041881            236
seed7/lib/environment.s7i    0.038752            175
seed7/lib/exif.s7i           0.040621            152
seed7/lib/external_file.s7i  0.039095            340
seed7/lib/field.s7i          0.038850            268
seed7/lib/file.s7i           0.038686            372
seed7/lib/filebits.s7i       0.036159             46
seed7/lib/filesys.s7i        0.037639            601
seed7/lib/fileutil.s7i       0.039204            144
seed7/lib/fixarray.s7i       0.038621            307
seed7/lib/float.s7i          0.041224            757
seed7/lib/font.s7i           0.042000            196
seed7/lib/font8x8.s7i        0.040732            998
seed7/lib/forloop.s7i        0.042932            449
seed7/lib/ftp.s7i            0.041068            969
seed7/lib/ftpserv.s7i        0.041466            631
seed7/lib/getf.s7i           0.042367            115
seed7/lib/gethttp.s7i        0.036811             41
seed7/lib/gethttps.s7i       0.036529             41
seed7/lib/gif.s7i            0.039106            561
seed7/lib/graph.s7i          0.040358            415
seed7/lib/graph_file.s7i     0.039451            399
seed7/lib/gtkserver.s7i      0.037995            161
seed7/lib/gzip.s7i           0.038691            573
seed7/lib/hash.s7i           0.040879            421
seed7/lib/hashsetof.s7i      0.040402            499
seed7/lib/hmac.s7i           0.038500            152
seed7/lib/html.s7i           0.036434             83
seed7/lib/html_ent.s7i       0.037514            476
seed7/lib/htmldom.s7i        0.037928            286
seed7/lib/http_request.s7i   0.038158            696
seed7/lib/http_srv_resp.s7i  0.037852            380
seed7/lib/https_request.s7i  0.038061            211
seed7/lib/httpserv.s7i       0.037645            345
seed7/lib/huffman.s7i        0.038005            644
seed7/lib/ico.s7i            0.038169            221
seed7/lib/idxarray.s7i       0.037622            232
seed7/lib/image.s7i          0.037035            156
seed7/lib/imagefile.s7i      0.039175            171
seed7/lib/inflate.s7i        0.039516            411
seed7/lib/inifile.s7i        0.039255            129
seed7/lib/integer.s7i        0.038855            663
seed7/lib/iobuffer.s7i       0.039272            289
seed7/lib/jpeg.s7i           0.039403           1761
seed7/lib/json.s7i           0.038547            891
seed7/lib/json_serde.s7i     0.039833            783
seed7/lib/keybd.s7i          0.038783            639
seed7/lib/keydescr.s7i       0.040919            192
seed7/lib/leb128.s7i         0.039285            218
seed7/lib/line.s7i           0.039036            164
seed7/lib/listener.s7i       0.038800            247
seed7/lib/logfile.s7i        0.037635             73
seed7/lib/lower.s7i          0.038526            142
seed7/lib/lzma.s7i           0.039571            934
seed7/lib/lzw.s7i            0.040416            861
seed7/lib/magic.s7i          0.039949            403
seed7/lib/mahjng32.s7i       0.038224           1500
seed7/lib/make.s7i           0.039458            544
seed7/lib/makedata.s7i       0.039040           1428
seed7/lib/math.s7i           0.037971            201
seed7/lib/mixarith.s7i       0.038983            249
seed7/lib/modern27.s7i       0.040977           1099
seed7/lib/more.s7i           0.038088            130
seed7/lib/msgdigest.s7i      0.038775           1222
seed7/lib/multiscr.s7i       0.038777             68
seed7/lib/null_file.s7i      0.037415            345
seed7/lib/osfiles.s7i        0.040779           1085
seed7/lib/pbm.s7i            0.039486            230
seed7/lib/pcx.s7i            0.039329            638
seed7/lib/pem.s7i            0.038882            185
seed7/lib/pgm.s7i            0.039075            238
seed7/lib/pic16.s7i          0.038259           1037
seed7/lib/pic32.s7i          0.038688           2060
seed7/lib/pic_util.s7i       0.042969            144
seed7/lib/pixelimage.s7i     0.039271            320
seed7/lib/pixmap_file.s7i    0.039333            459
seed7/lib/pixmapfont.s7i     0.040544            184
seed7/lib/pkcs1.s7i          0.044211            543
seed7/lib/png.s7i            0.039666           1064
seed7/lib/poll.s7i           0.037556            313
seed7/lib/ppm.s7i            0.037641            240
seed7/lib/process.s7i        0.037746            541
seed7/lib/progs.s7i          0.042219            789
seed7/lib/propertyfile.s7i   0.046481            155
seed7/lib/rational.s7i       0.044054            792
seed7/lib/ref_list.s7i       0.038339            252
seed7/lib/reference.s7i      0.037620            126
seed7/lib/reverse.s7i        0.037369             94
seed7/lib/rpm.s7i            0.040192           3487
seed7/lib/rpmext.s7i         0.039691            318
seed7/lib/scanfile.s7i       0.040045           1779
seed7/lib/scanjson.s7i       0.039692            413
seed7/lib/scanstri.s7i       0.039536           1814
seed7/lib/scantoml.s7i       0.039744           1603
seed7/lib/seed7_05.s7i       0.041376           1072
seed7/lib/set.s7i            0.037091             57
seed7/lib/shell.s7i          0.039821            615
seed7/lib/showtls.s7i        0.039873            678
seed7/lib/signature.s7i      0.039255            131
seed7/lib/smtp.s7i           0.039562            261
seed7/lib/sockbase.s7i       0.040558            217
seed7/lib/socket.s7i         0.038940            326
seed7/lib/sokoban1.s7i       0.038967           1519
seed7/lib/sql_base.s7i       0.039090           1000
seed7/lib/stars.s7i          0.041128           1705
seed7/lib/stdfont10.s7i      0.037945           3347
seed7/lib/stdfont12.s7i      0.038520           3928
seed7/lib/stdfont14.s7i      0.039062           4510
seed7/lib/stdfont16.s7i      0.039016           5092
seed7/lib/stdfont18.s7i      0.037998           5868
seed7/lib/stdfont20.s7i      0.037788           6449
seed7/lib/stdfont24.s7i      0.037847           7421
seed7/lib/stdfont8.s7i       0.036370           2960
seed7/lib/stdfont9.s7i       0.036301           3152
seed7/lib/stdio.s7i          0.040825            192
seed7/lib/strifile.s7i       0.039394            345
seed7/lib/string.s7i         0.038633            779
seed7/lib/stritext.s7i       0.039345            352
seed7/lib/struct.s7i         0.040877            266
seed7/lib/struct_elem.s7i    0.038657            129
seed7/lib/subfile.s7i        0.041180            174
seed7/lib/subrange.s7i       0.037691             78
seed7/lib/syntax.s7i         0.038129            294
seed7/lib/tar.s7i            0.038551           1880
seed7/lib/tar_cmds.s7i       0.038190            752
seed7/lib/tdes.s7i           0.038142            143
seed7/lib/tee.s7i            0.037396            143
seed7/lib/text.s7i           0.037667            135
seed7/lib/tga.s7i            0.040040            676
seed7/lib/tiff.s7i           0.041033           2771
seed7/lib/time.s7i           0.039034           1191
seed7/lib/tls.s7i            0.039721           2230
seed7/lib/unicode.s7i        0.040915            575
seed7/lib/unionfnd.s7i       0.039152            130
seed7/lib/upper.s7i          0.038437            142
seed7/lib/utf16.s7i          0.038692            540
seed7/lib/utf8.s7i           0.039406            234
seed7/lib/vecfont10.s7i      0.040887           1056
seed7/lib/vecfont18.s7i      0.041327           1119
seed7/lib/vector3d.s7i       0.037913            293
seed7/lib/vectorfont.s7i     0.038325            239
seed7/lib/wildcard.s7i       0.038690            140
seed7/lib/window.s7i         0.039675            455
seed7/lib/wrinum.s7i         0.039683            248
seed7/lib/x509cert.s7i       0.039366           1243
seed7/lib/xml_ent.s7i        0.038956             94
seed7/lib/xmldom.s7i         0.039227            303
seed7/lib/xz.s7i             0.038900            442
seed7/lib/zip.s7i            0.041177           2792
seed7/lib/zstd.s7i           0.042343           1263
============================ ================= ================

Statistical Summary — Mode B (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.039160        |
+-----------+-----------------+
| Minimum   | 0.034891        |
+-----------+-----------------+
| Maximum   | 0.047221        |
+-----------+-----------------+

Mode C — Mode Activation + Full-Buffer Fontification (font-lock-ensure)
-----------------------------------------------------------------------

Full-buffer fontification stress test.

File Load Times — Mode C (mean, GC-free)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

============================ ================= ================
File Name                    Mean Time (s)     Line count
============================ ================= ================
seed7/prg/addup.sd7          0.042107            190
seed7/prg/bas7.sd7           0.337305          11459
seed7/prg/bifurk.sd7         0.037477             73
seed7/prg/bigfiles.sd7       0.039613            129
seed7/prg/brainf7.sd7        0.039934             86
seed7/prg/calc7.sd7          0.043078            128
seed7/prg/carddemo.sd7       0.043301            190
seed7/prg/castle.sd7         0.112137           3148
seed7/prg/cat.sd7            0.040278             82
seed7/prg/cellauto.sd7       0.037850             85
seed7/prg/celsius.sd7        0.037244             42
seed7/prg/chk_all.sd7        0.061746            843
seed7/prg/chkarr.sd7         0.367438           8367
seed7/prg/chkbig.sd7         2.121646          29026
seed7/prg/chkbin.sd7         0.526988           6469
seed7/prg/chkbitdata.sd7     0.635044           6624
seed7/prg/chkbool.sd7        0.118871           3157
seed7/prg/chkbst.sd7         0.066597            722
seed7/prg/chkchr.sd7         0.224355           2809
seed7/prg/chkcmd.sd7         0.068837           1205
seed7/prg/chkdb.sd7          0.364570           7454
seed7/prg/chkdecl.sd7        0.061961            448
seed7/prg/chkenum.sd7        0.071185           1230
seed7/prg/chkerr.sd7         0.201218           4663
seed7/prg/chkexc.sd7         0.084504           2627
seed7/prg/chkfil.sd7         0.076830           1615
seed7/prg/chkflt.sd7         1.381755          20620
seed7/prg/chkhent.sd7        0.038649             54
seed7/prg/chkhsh.sd7         0.256840           4548
seed7/prg/chkidx.sd7         1.362855          19567
seed7/prg/chkint.sd7         2.600730          38129
seed7/prg/chkjson.sd7        0.105256           1764
seed7/prg/chkovf.sd7         0.579615           8216
seed7/prg/chkprc.sd7         0.332545          10111
seed7/prg/chkscan.sd7        0.059807            714
seed7/prg/chkset.sd7         0.681373          11974
seed7/prg/chkstr.sd7         1.424580          26952
seed7/prg/chktime.sd7        0.136461           2025
seed7/prg/chktoml.sd7        0.112392           1656
seed7/prg/clock.sd7          0.038220             47
seed7/prg/clock2.sd7         0.037015             43
seed7/prg/clock3.sd7         0.039885             95
seed7/prg/cmpfil.sd7         0.038250             84
seed7/prg/comanche.sd7       0.041461            180
seed7/prg/confval.sd7        0.045484            175
seed7/prg/db7.sd7            0.049884            417
seed7/prg/diff7.sd7          0.045433            263
seed7/prg/dirtst.sd7         0.037719             42
seed7/prg/dirx.sd7           0.039441            152
seed7/prg/dnafight.sd7       0.068986           1381
seed7/prg/dragon.sd7         0.038703             73
seed7/prg/echo.sd7           0.045457             39
seed7/prg/eliza.sd7          0.045265            302
seed7/prg/err.sd7            0.042845             96
seed7/prg/fannkuch.sd7       0.046122            131
seed7/prg/fib.sd7            0.042892             47
seed7/prg/find7.sd7          0.042906            133
seed7/prg/findchar.sd7       0.042580            149
seed7/prg/fractree.sd7       0.039531             55
seed7/prg/ftp7.sd7           0.045596            296
seed7/prg/ftpserv.sd7        0.038887             74
seed7/prg/gcd.sd7            0.039306            109
seed7/prg/gkbd.sd7           0.047385            358
seed7/prg/gtksvtst.sd7       0.037702             94
seed7/prg/hal.sd7            0.041105            250
seed7/prg/hamu.sd7           0.050948            573
seed7/prg/hanoi.sd7          0.036727             55
seed7/prg/hd.sd7             0.036924             79
seed7/prg/hello.sd7          0.036558             32
seed7/prg/hilbert.sd7        0.040839            108
seed7/prg/ide7.sd7           0.041827            196
seed7/prg/kbd.sd7            0.036391             49
seed7/prg/klondike.sd7       0.058049            883
seed7/prg/lander.sd7         0.076088           1551
seed7/prg/lst80bas.sd7       0.049467            344
seed7/prg/lst99bas.sd7       0.051655            401
seed7/prg/lstgwbas.sd7       0.054382            577
seed7/prg/mahjong.sd7        0.083353           1943
seed7/prg/make7.sd7          0.040479            121
seed7/prg/mandelbr.sd7       0.043109            237
seed7/prg/mind.sd7           0.045769            443
seed7/prg/mirror.sd7         0.039137            131
seed7/prg/ms.sd7             0.049563            641
seed7/prg/nicoma.sd7         0.040341            135
seed7/prg/pac.sd7            0.051472            726
seed7/prg/pairs.sd7          0.086706           2025
seed7/prg/panic.sd7          0.100956           2634
seed7/prg/percolation.sd7    0.044336            330
seed7/prg/planets.sd7        0.077929           1486
seed7/prg/portfwd7.sd7       0.040271            139
seed7/prg/prime.sd7          0.038652             74
seed7/prg/printpi1.sd7       0.041167             56
seed7/prg/printpi2.sd7       0.038167             54
seed7/prg/printpi3.sd7       0.038738             60
seed7/prg/pv7.sd7            0.046039            337
seed7/prg/queen.sd7          0.039249            149
seed7/prg/rand.sd7           0.041145            121
seed7/prg/raytrace.sd7       0.054400            538
seed7/prg/rever.sd7          0.056264            816
seed7/prg/roman.sd7          0.036006             38
seed7/prg/s7c.sd7            0.290187           9060
seed7/prg/s7check.sd7        0.040803             68
seed7/prg/savehd7.sd7        0.071854           1110
seed7/prg/self.sd7           0.041042             49
seed7/prg/shisen.sd7         0.076099           1423
seed7/prg/sl.sd7             0.061756           1029
seed7/prg/snake.sd7          0.049010            615
seed7/prg/sokoban.sd7        0.057467            891
seed7/prg/spigotpi.sd7       0.038798             64
seed7/prg/sql7.sd7           0.043454            278
seed7/prg/startrek.sd7       0.061857            979
seed7/prg/sudoku7.sd7        0.106863           2657
seed7/prg/sydir7.sd7         0.048084            384
seed7/prg/syntaxhl.sd7       0.043091            177
seed7/prg/tak.sd7            0.037766             59
seed7/prg/tar7.sd7           0.040960            121
seed7/prg/tch.sd7            0.038657             55
seed7/prg/testfont.sd7       0.039519             95
seed7/prg/tet.sd7            0.046719            479
seed7/prg/tetg.sd7           0.047482            501
seed7/prg/toutf8.sd7         0.045835            240
seed7/prg/tst_cli.sd7        0.038063             40
seed7/prg/tst_srv.sd7        0.038115             47
seed7/prg/wator.sd7          0.055344            651
seed7/prg/which.sd7          0.038462             65
seed7/prg/wiz.sd7            0.109156           2833
seed7/prg/wordcnt.sd7        0.038183             54
seed7/prg/wrinum.sd7         0.040078             43
seed7/prg/wumpus.sd7         0.044070            372
seed7/lib/aes.s7i            0.115777           1144
seed7/lib/aes_gcm.s7i        0.048393            392
seed7/lib/ar.s7i             0.077036           1532
seed7/lib/arc4.s7i           0.039422            144
seed7/lib/archive.s7i        0.039455            143
seed7/lib/archive_base.s7i   0.039369            135
seed7/lib/array.s7i          0.055276            610
seed7/lib/asn1.s7i           0.047831            544
seed7/lib/asn1oid.s7i        0.044394            157
seed7/lib/basearray.s7i      0.050926            450
seed7/lib/bigfile.s7i        0.040770            136
seed7/lib/bigint.s7i         0.058550            824
seed7/lib/bigrat.s7i         0.056860            784
seed7/lib/bin16.s7i          0.053521            592
seed7/lib/bin32.s7i          0.050025            490
seed7/lib/bin64.s7i          0.052211            539
seed7/lib/bitdata.s7i        0.078459           1330
seed7/lib/bitmapfont.s7i     0.043032            215
seed7/lib/bitset.s7i         0.051126            593
seed7/lib/bitsetof.s7i       0.050858            431
seed7/lib/blowfish.s7i       0.060158            383
seed7/lib/bmp.s7i            0.067956            924
seed7/lib/boolean.s7i        0.047204            403
seed7/lib/browser.s7i        0.046167            280
seed7/lib/bstring.s7i        0.043577            227
seed7/lib/bytedata.s7i       0.052331            482
seed7/lib/bzip2.s7i          0.064131            887
seed7/lib/cards.s7i          0.069412           1342
seed7/lib/category.s7i       0.044208            209
seed7/lib/cc_conf.s7i        0.081898           1314
seed7/lib/ccittfax.s7i       0.068804           1022
seed7/lib/cgi.s7i            0.040105            109
seed7/lib/cgidialog.s7i      0.063806           1118
seed7/lib/char.s7i           0.043565            356
seed7/lib/charsets.s7i       0.084054           2024
seed7/lib/chartype.s7i       0.041667            121
seed7/lib/cipher.s7i         0.040787            146
seed7/lib/cli_cmds.s7i       0.070386           1360
seed7/lib/clib_file.s7i      0.046702            301
seed7/lib/color.s7i          0.047804            185
seed7/lib/complex.s7i        0.051133            464
seed7/lib/compress.s7i       0.041993            150
seed7/lib/console.s7i        0.042903            188
seed7/lib/cpio.s7i           0.090406           1708
seed7/lib/crc32.s7i          0.046784            193
seed7/lib/cronos16.s7i       0.095946           1173
seed7/lib/cronos27.s7i       0.121530           1464
seed7/lib/csv.s7i            0.042329            201
seed7/lib/db_prop.s7i        0.065212            991
seed7/lib/deflate.s7i        0.056963            740
seed7/lib/des.s7i            0.057947            444
seed7/lib/dialog.s7i         0.045126            311
seed7/lib/dir.s7i            0.041475            163
seed7/lib/draw.s7i           0.062164            854
seed7/lib/duration.s7i       0.062890           1038
seed7/lib/echo.s7i           0.038933            132
seed7/lib/editline.s7i       0.053745            398
seed7/lib/elf.s7i            0.090344           1560
seed7/lib/elliptic.s7i       0.056421            649
seed7/lib/enable_io.s7i      0.045813            312
seed7/lib/encoding.s7i       0.063534            931
seed7/lib/enumeration.s7i    0.042771            236
seed7/lib/environment.s7i    0.039899            175
seed7/lib/exif.s7i           0.041422            152
seed7/lib/external_file.s7i  0.056192            340
seed7/lib/field.s7i          0.046517            268
seed7/lib/file.s7i           0.046900            372
seed7/lib/filebits.s7i       0.038066             46
seed7/lib/filesys.s7i        0.052229            601
seed7/lib/fileutil.s7i       0.039961            144
seed7/lib/fixarray.s7i       0.045204            307
seed7/lib/float.s7i          0.057693            757
seed7/lib/font.s7i           0.041245            196
seed7/lib/font8x8.s7i        0.048935            998
seed7/lib/forloop.s7i        0.047609            449
seed7/lib/ftp.s7i            0.062514            969
seed7/lib/ftpserv.s7i        0.055419            631
seed7/lib/getf.s7i           0.040961            115
seed7/lib/gethttp.s7i        0.038890             41
seed7/lib/gethttps.s7i       0.039237             41
seed7/lib/gif.s7i            0.051550            561
seed7/lib/graph.s7i          0.050975            415
seed7/lib/graph_file.s7i     0.046563            399
seed7/lib/gtkserver.s7i      0.040355            161
seed7/lib/gzip.s7i           0.050890            573
seed7/lib/hash.s7i           0.051708            421
seed7/lib/hashsetof.s7i      0.055994            499
seed7/lib/hmac.s7i           0.042630            152
seed7/lib/html.s7i           0.039254             83
seed7/lib/html_ent.s7i       0.049542            476
seed7/lib/htmldom.s7i        0.044741            286
seed7/lib/http_request.s7i   0.053348            696
seed7/lib/http_srv_resp.s7i  0.048900            380
seed7/lib/https_request.s7i  0.041894            211
seed7/lib/httpserv.s7i       0.045561            345
seed7/lib/huffman.s7i        0.053934            644
seed7/lib/ico.s7i            0.041877            221
seed7/lib/idxarray.s7i       0.043096            232
seed7/lib/image.s7i          0.038220            156
seed7/lib/imagefile.s7i      0.042260            171
seed7/lib/inflate.s7i        0.048687            411
seed7/lib/inifile.s7i        0.038893            129
seed7/lib/integer.s7i        0.053008            663
seed7/lib/iobuffer.s7i       0.043796            289
seed7/lib/jpeg.s7i           0.086568           1761
seed7/lib/json.s7i           0.058128            891
seed7/lib/json_serde.s7i     0.054928            783
seed7/lib/keybd.s7i          0.057522            639
seed7/lib/keydescr.s7i       0.043000            192
seed7/lib/leb128.s7i         0.042164            218
seed7/lib/line.s7i           0.040227            164
seed7/lib/listener.s7i       0.043568            247
seed7/lib/logfile.s7i        0.039041             73
seed7/lib/lower.s7i          0.040656            142
seed7/lib/lzma.s7i           0.061950            934
seed7/lib/lzw.s7i            0.061152            861
seed7/lib/magic.s7i          0.050560            403
seed7/lib/mahjng32.s7i       0.064632           1500
seed7/lib/make.s7i           0.050214            544
seed7/lib/makedata.s7i       0.071471           1428
seed7/lib/math.s7i           0.042661            201
seed7/lib/mixarith.s7i       0.046152            249
seed7/lib/modern27.s7i       0.087732           1099
seed7/lib/more.s7i           0.040734            130
seed7/lib/msgdigest.s7i      0.081571           1222
seed7/lib/multiscr.s7i       0.038765             68
seed7/lib/null_file.s7i      0.043861            345
seed7/lib/osfiles.s7i        0.066765           1085
seed7/lib/pbm.s7i            0.040695            230
seed7/lib/pcx.s7i            0.052528            638
seed7/lib/pem.s7i            0.040093            185
seed7/lib/pgm.s7i            0.041383            238
seed7/lib/pic16.s7i          0.049184           1037
seed7/lib/pic32.s7i          0.081685           2060
seed7/lib/pic_util.s7i       0.040909            144
seed7/lib/pixelimage.s7i     0.043733            320
seed7/lib/pixmap_file.s7i    0.051087            459
seed7/lib/pixmapfont.s7i     0.047652            184
seed7/lib/pkcs1.s7i          0.061403            543
seed7/lib/png.s7i            0.068884           1064
seed7/lib/poll.s7i           0.049507            313
seed7/lib/ppm.s7i            0.042555            240
seed7/lib/process.s7i        0.051301            541
seed7/lib/progs.s7i          0.059029            789
seed7/lib/propertyfile.s7i   0.042759            155
seed7/lib/rational.s7i       0.055757            792
seed7/lib/ref_list.s7i       0.043990            252
seed7/lib/reference.s7i      0.040116            126
seed7/lib/reverse.s7i        0.041194             94
seed7/lib/rpm.s7i            0.149646           3487
seed7/lib/rpmext.s7i         0.044412            318
seed7/lib/scanfile.s7i       0.082860           1779
seed7/lib/scanjson.s7i       0.049011            413
seed7/lib/scanstri.s7i       0.083127           1814
seed7/lib/scantoml.s7i       0.073566           1603
seed7/lib/seed7_05.s7i       0.068823           1072
seed7/lib/set.s7i            0.036844             57
seed7/lib/shell.s7i          0.054417            615
seed7/lib/showtls.s7i        0.056204            678
seed7/lib/signature.s7i      0.040779            131
seed7/lib/smtp.s7i           0.042227            261
seed7/lib/sockbase.s7i       0.044718            217
seed7/lib/socket.s7i         0.044781            326
seed7/lib/sokoban1.s7i       0.055799           1519
seed7/lib/sql_base.s7i       0.066051           1000
seed7/lib/stars.s7i          0.138834           1705
seed7/lib/stdfont10.s7i      0.082366           3347
seed7/lib/stdfont12.s7i      0.095199           3928
seed7/lib/stdfont14.s7i      0.108064           4510
seed7/lib/stdfont16.s7i      0.119461           5092
seed7/lib/stdfont18.s7i      0.135180           5868
seed7/lib/stdfont20.s7i      0.153781           6449
seed7/lib/stdfont24.s7i      0.185455           7421
seed7/lib/stdfont8.s7i       0.081727           2960
seed7/lib/stdfont9.s7i       0.080367           3152
seed7/lib/stdio.s7i          0.044535            192
seed7/lib/strifile.s7i       0.045317            345
seed7/lib/string.s7i         0.056264            779
seed7/lib/stritext.s7i       0.043984            352
seed7/lib/struct.s7i         0.047774            266
seed7/lib/struct_elem.s7i    0.040537            129
seed7/lib/subfile.s7i        0.041152            174
seed7/lib/subrange.s7i       0.038299             78
seed7/lib/syntax.s7i         0.048750            294
seed7/lib/tar.s7i            0.084643           1880
seed7/lib/tar_cmds.s7i       0.058564            752
seed7/lib/tdes.s7i           0.040006            143
seed7/lib/tee.s7i            0.039351            143
seed7/lib/text.s7i           0.039891            135
seed7/lib/tga.s7i            0.056888            676
seed7/lib/tiff.s7i           0.127131           2771
seed7/lib/time.s7i           0.064826           1191
seed7/lib/tls.s7i            0.109207           2230
seed7/lib/unicode.s7i        0.053920            575
seed7/lib/unionfnd.s7i       0.038751            130
seed7/lib/upper.s7i          0.040660            142
seed7/lib/utf16.s7i          0.051393            540
seed7/lib/utf8.s7i           0.044172            234
seed7/lib/vecfont10.s7i      0.086555           1056
seed7/lib/vecfont18.s7i      0.093141           1119
seed7/lib/vector3d.s7i       0.041113            293
seed7/lib/vectorfont.s7i     0.042729            239
seed7/lib/wildcard.s7i       0.038934            140
seed7/lib/window.s7i         0.046069            455
seed7/lib/wrinum.s7i         0.042319            248
seed7/lib/x509cert.s7i       0.074910           1243
seed7/lib/xml_ent.s7i        0.038787             94
seed7/lib/xmldom.s7i         0.044440            303
seed7/lib/xz.s7i             0.050480            442
seed7/lib/zip.s7i            0.123723           2792
seed7/lib/zstd.s7i           0.071956           1263
============================ ================= ================

Statistical Summary — Mode C (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.092942        |
+-----------+-----------------+
| Minimum   | 0.036006        |
+-----------+-----------------+
| Maximum   | 2.600730        |
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
seed7/prg/addup.sd7          0.047384            190
seed7/prg/bas7.sd7           0.796590          11459
seed7/prg/bifurk.sd7         0.042697             73
seed7/prg/bigfiles.sd7       0.044803            129
seed7/prg/brainf7.sd7        0.042425             86
seed7/prg/calc7.sd7          0.044494            128
seed7/prg/carddemo.sd7       0.048895            190
seed7/prg/castle.sd7         0.229441           3148
seed7/prg/cat.sd7            0.043265             82
seed7/prg/cellauto.sd7       0.039956             85
seed7/prg/celsius.sd7        0.037882             42
seed7/prg/chk_all.sd7        0.084913            843
seed7/prg/chkarr.sd7         0.894452           8367
seed7/prg/chkbig.sd7         4.277617          29026
seed7/prg/chkbin.sd7         1.062097           6469
seed7/prg/chkbitdata.sd7     1.291751           6624
seed7/prg/chkbool.sd7        0.243501           3157
seed7/prg/chkbst.sd7         0.108879            722
seed7/prg/chkchr.sd7         0.494711           2809
seed7/prg/chkcmd.sd7         0.119050           1205
seed7/prg/chkdb.sd7          0.780321           7454
seed7/prg/chkdecl.sd7        0.102108            448
seed7/prg/chkenum.sd7        0.126132           1230
seed7/prg/chkerr.sd7         0.352665           4663
seed7/prg/chkexc.sd7         0.156637           2627
seed7/prg/chkfil.sd7         0.133094           1615
seed7/prg/chkflt.sd7         2.907869          20620
seed7/prg/chkhent.sd7        0.042157             54
seed7/prg/chkhsh.sd7         0.516126           4548
seed7/prg/chkidx.sd7         3.270432          19567
seed7/prg/chkint.sd7         5.720472          38129
seed7/prg/chkjson.sd7        0.195194           1764
seed7/prg/chkovf.sd7         1.242950           8216
seed7/prg/chkprc.sd7         0.722070          10111
seed7/prg/chkscan.sd7        0.095986            714
seed7/prg/chkset.sd7         1.799990          11974
seed7/prg/chkstr.sd7         3.522019          26952
seed7/prg/chktime.sd7        0.253122           2025
seed7/prg/chktoml.sd7        0.203574           1656
seed7/prg/clock.sd7          0.039328             47
seed7/prg/clock2.sd7         0.038066             43
seed7/prg/clock3.sd7         0.043745             95
seed7/prg/cmpfil.sd7         0.040120             84
seed7/prg/comanche.sd7       0.048333            180
seed7/prg/confval.sd7        0.052586            175
seed7/prg/db7.sd7            0.065949            417
seed7/prg/diff7.sd7          0.054449            263
seed7/prg/dirtst.sd7         0.038240             42
seed7/prg/dirx.sd7           0.044070            152
seed7/prg/dnafight.sd7       0.121559           1381
seed7/prg/dragon.sd7         0.039818             73
seed7/prg/echo.sd7           0.037055             39
seed7/prg/eliza.sd7          0.054237            302
seed7/prg/err.sd7            0.044916             96
seed7/prg/fannkuch.sd7       0.043466            131
seed7/prg/fib.sd7            0.038003             47
seed7/prg/find7.sd7          0.043893            133
seed7/prg/findchar.sd7       0.045779            149
seed7/prg/fractree.sd7       0.038981             55
seed7/prg/ftp7.sd7           0.055369            296
seed7/prg/ftpserv.sd7        0.039384             74
seed7/prg/gcd.sd7            0.040381            109
seed7/prg/gkbd.sd7           0.062575            358
seed7/prg/gtksvtst.sd7       0.038997             94
seed7/prg/hal.sd7            0.049004            250
seed7/prg/hamu.sd7           0.067705            573
seed7/prg/hanoi.sd7          0.037551             55
seed7/prg/hd.sd7             0.038730             79
seed7/prg/hello.sd7          0.037817             32
seed7/prg/hilbert.sd7        0.042281            108
seed7/prg/ide7.sd7           0.049769            196
seed7/prg/kbd.sd7            0.038134             49
seed7/prg/klondike.sd7       0.089832            883
seed7/prg/lander.sd7         0.135333           1551
seed7/prg/lst80bas.sd7       0.058780            344
seed7/prg/lst99bas.sd7       0.062465            401
seed7/prg/lstgwbas.sd7       0.074995            577
seed7/prg/mahjong.sd7        0.152862           1943
seed7/prg/make7.sd7          0.045975            121
seed7/prg/mandelbr.sd7       0.051819            237
seed7/prg/mind.sd7           0.061055            443
seed7/prg/mirror.sd7         0.046073            131
seed7/prg/ms.sd7             0.073045            641
seed7/prg/nicoma.sd7         0.044300            135
seed7/prg/pac.sd7            0.074300            726
seed7/prg/pairs.sd7          0.143398           2025
seed7/prg/panic.sd7          0.197501           2634
seed7/prg/percolation.sd7    0.057672            330
seed7/prg/planets.sd7        0.140127           1486
seed7/prg/portfwd7.sd7       0.047610            139
seed7/prg/prime.sd7          0.039609             74
seed7/prg/printpi1.sd7       0.039066             56
seed7/prg/printpi2.sd7       0.039576             54
seed7/prg/printpi3.sd7       0.039822             60
seed7/prg/pv7.sd7            0.061191            337
seed7/prg/queen.sd7          0.044142            149
seed7/prg/rand.sd7           0.043058            121
seed7/prg/raytrace.sd7       0.070790            538
seed7/prg/rever.sd7          0.084114            816
seed7/prg/roman.sd7          0.038372             38
seed7/prg/s7c.sd7            0.644513           9060
seed7/prg/s7check.sd7        0.040473             68
seed7/prg/savehd7.sd7        0.114481           1110
seed7/prg/self.sd7           0.037877             49
seed7/prg/shisen.sd7         0.122925           1423
seed7/prg/sl.sd7             0.099039           1029
seed7/prg/snake.sd7          0.068468            615
seed7/prg/sokoban.sd7        0.083735            891
seed7/prg/spigotpi.sd7       0.038978             64
seed7/prg/sql7.sd7           0.052182            278
seed7/prg/startrek.sd7       0.096463            979
seed7/prg/sudoku7.sd7        0.202970           2657
seed7/prg/sydir7.sd7         0.064911            384
seed7/prg/syntaxhl.sd7       0.050209            177
seed7/prg/tak.sd7            0.039914             59
seed7/prg/tar7.sd7           0.047389            121
seed7/prg/tch.sd7            0.039179             55
seed7/prg/testfont.sd7       0.043443             95
seed7/prg/tet.sd7            0.062317            479
seed7/prg/tetg.sd7           0.063873            501
seed7/prg/toutf8.sd7         0.053396            240
seed7/prg/tst_cli.sd7        0.038361             40
seed7/prg/tst_srv.sd7        0.039088             47
seed7/prg/wator.sd7          0.081367            651
seed7/prg/which.sd7          0.046718             65
seed7/prg/wiz.sd7            0.219166           2833
seed7/prg/wordcnt.sd7        0.040087             54
seed7/prg/wrinum.sd7         0.036925             43
seed7/prg/wumpus.sd7         0.055149            372
seed7/lib/aes.s7i            0.201910           1144
seed7/lib/aes_gcm.s7i        0.063944            392
seed7/lib/ar.s7i             0.128196           1532
seed7/lib/arc4.s7i           0.045955            144
seed7/lib/archive.s7i        0.046895            143
seed7/lib/archive_base.s7i   0.045777            135
seed7/lib/array.s7i          0.077926            610
seed7/lib/asn1.s7i           0.065332            544
seed7/lib/asn1oid.s7i        0.051499            157
seed7/lib/basearray.s7i      0.066871            450
seed7/lib/bigfile.s7i        0.042248            136
seed7/lib/bigint.s7i         0.082193            824
seed7/lib/bigrat.s7i         0.082390            784
seed7/lib/bin16.s7i          0.070151            592
seed7/lib/bin32.s7i          0.064714            490
seed7/lib/bin64.s7i          0.065525            539
seed7/lib/bitdata.s7i        0.127370           1330
seed7/lib/bitmapfont.s7i     0.049110            215
seed7/lib/bitset.s7i         0.065582            593
seed7/lib/bitsetof.s7i       0.062314            431
seed7/lib/blowfish.s7i       0.080480            383
seed7/lib/bmp.s7i            0.101958            924
seed7/lib/boolean.s7i        0.057434            403
seed7/lib/browser.s7i        0.054713            280
seed7/lib/bstring.s7i        0.047485            227
seed7/lib/bytedata.s7i       0.068130            482
seed7/lib/bzip2.s7i          0.093981            887
seed7/lib/cards.s7i          0.106182           1342
seed7/lib/category.s7i       0.050400            209
seed7/lib/cc_conf.s7i        0.124448           1314
seed7/lib/ccittfax.s7i       0.104466           1022
seed7/lib/cgi.s7i            0.042991            109
seed7/lib/cgidialog.s7i      0.101913           1118
seed7/lib/char.s7i           0.053395            356
seed7/lib/charsets.s7i       0.128978           2024
seed7/lib/chartype.s7i       0.049228            121
seed7/lib/cipher.s7i         0.043405            146
seed7/lib/cli_cmds.s7i       0.116153           1360
seed7/lib/clib_file.s7i      0.053554            301
seed7/lib/color.s7i          0.049596            185
seed7/lib/complex.s7i        0.061563            464
seed7/lib/compress.s7i       0.044890            150
seed7/lib/console.s7i        0.046547            188
seed7/lib/cpio.s7i           0.149703           1708
seed7/lib/crc32.s7i          0.056767            193
seed7/lib/cronos16.s7i       0.202914           1173
seed7/lib/cronos27.s7i       0.264287           1464
seed7/lib/csv.s7i            0.049168            201
seed7/lib/db_prop.s7i        0.103521            991
seed7/lib/deflate.s7i        0.089539            740
seed7/lib/des.s7i            0.084163            444
seed7/lib/dialog.s7i         0.060488            311
seed7/lib/dir.s7i            0.045111            163
seed7/lib/draw.s7i           0.089288            854
seed7/lib/duration.s7i       0.101575           1038
seed7/lib/echo.s7i           0.044260            132
seed7/lib/editline.s7i       0.062332            398
seed7/lib/elf.s7i            0.161355           1560
seed7/lib/elliptic.s7i       0.079047            649
seed7/lib/enable_io.s7i      0.054397            312
seed7/lib/encoding.s7i       0.099861            931
seed7/lib/enumeration.s7i    0.051123            236
seed7/lib/environment.s7i    0.044016            175
seed7/lib/exif.s7i           0.048782            152
seed7/lib/external_file.s7i  0.054934            340
seed7/lib/field.s7i          0.053245            268
seed7/lib/file.s7i           0.055673            372
seed7/lib/filebits.s7i       0.038834             46
seed7/lib/filesys.s7i        0.065886            601
seed7/lib/fileutil.s7i       0.045028            144
seed7/lib/fixarray.s7i       0.058232            307
seed7/lib/float.s7i          0.074921            757
seed7/lib/font.s7i           0.046019            196
seed7/lib/font8x8.s7i        0.068119            998
seed7/lib/forloop.s7i        0.061251            449
seed7/lib/ftp.s7i            0.089882            969
seed7/lib/ftpserv.s7i        0.078577            631
seed7/lib/getf.s7i           0.041467            115
seed7/lib/gethttp.s7i        0.037671             41
seed7/lib/gethttps.s7i       0.037631             41
seed7/lib/gif.s7i            0.072893            561
seed7/lib/graph.s7i          0.066570            415
seed7/lib/graph_file.s7i     0.060418            399
seed7/lib/gtkserver.s7i      0.043873            161
seed7/lib/gzip.s7i           0.070560            573
seed7/lib/hash.s7i           0.068610            421
seed7/lib/hashsetof.s7i      0.069373            499
seed7/lib/hmac.s7i           0.046285            152
seed7/lib/html.s7i           0.040478             83
seed7/lib/html_ent.s7i       0.066238            476
seed7/lib/htmldom.s7i        0.055076            286
seed7/lib/http_request.s7i   0.085550            696
seed7/lib/http_srv_resp.s7i  0.061887            380
seed7/lib/https_request.s7i  0.050227            211
seed7/lib/httpserv.s7i       0.060986            345
seed7/lib/huffman.s7i        0.079591            644
seed7/lib/ico.s7i            0.050389            221
seed7/lib/idxarray.s7i       0.051086            232
seed7/lib/image.s7i          0.042269            156
seed7/lib/imagefile.s7i      0.046633            171
seed7/lib/inflate.s7i        0.064989            411
seed7/lib/inifile.s7i        0.044328            129
seed7/lib/integer.s7i        0.070312            663
seed7/lib/iobuffer.s7i       0.054207            289
seed7/lib/jpeg.s7i           0.159837           1761
seed7/lib/json.s7i           0.082533            891
seed7/lib/json_serde.s7i     0.079889            783
seed7/lib/keybd.s7i          0.083432            639
seed7/lib/keydescr.s7i       0.055795            192
seed7/lib/leb128.s7i         0.048466            218
seed7/lib/line.s7i           0.046439            164
seed7/lib/listener.s7i       0.051053            247
seed7/lib/logfile.s7i        0.038884             73
seed7/lib/lower.s7i          0.042496            142
seed7/lib/lzma.s7i           0.098408            934
seed7/lib/lzw.s7i            0.091755            861
seed7/lib/magic.s7i          0.065992            403
seed7/lib/mahjng32.s7i       0.095771           1500
seed7/lib/make.s7i           0.071593            544
seed7/lib/makedata.s7i       0.124620           1428
seed7/lib/math.s7i           0.045644            201
seed7/lib/mixarith.s7i       0.047678            249
seed7/lib/modern27.s7i       0.174938           1099
seed7/lib/more.s7i           0.044125            130
seed7/lib/msgdigest.s7i      0.141369           1222
seed7/lib/multiscr.s7i       0.039121             68
seed7/lib/null_file.s7i      0.052279            345
seed7/lib/osfiles.s7i        0.098590           1085
seed7/lib/pbm.s7i            0.051118            230
seed7/lib/pcx.s7i            0.081078            638
seed7/lib/pem.s7i            0.046870            185
seed7/lib/pgm.s7i            0.050571            238
seed7/lib/pic16.s7i          0.069060           1037
seed7/lib/pic32.s7i          0.126389           2060
seed7/lib/pic_util.s7i       0.049802            144
seed7/lib/pixelimage.s7i     0.055180            320
seed7/lib/pixmap_file.s7i    0.065920            459
seed7/lib/pixmapfont.s7i     0.051806            184
seed7/lib/pkcs1.s7i          0.083138            543
seed7/lib/png.s7i            0.111277           1064
seed7/lib/poll.s7i           0.053242            313
seed7/lib/ppm.s7i            0.051584            240
seed7/lib/process.s7i        0.065374            541
seed7/lib/progs.s7i          0.082294            789
seed7/lib/propertyfile.s7i   0.045205            155
seed7/lib/rational.s7i       0.080856            792
seed7/lib/ref_list.s7i       0.053548            252
seed7/lib/reference.s7i      0.046053            126
seed7/lib/reverse.s7i        0.041358             94
seed7/lib/rpm.s7i            0.301291           3487
seed7/lib/rpmext.s7i         0.056368            318
seed7/lib/scanfile.s7i       0.137134           1779
seed7/lib/scanjson.s7i       0.061902            413
seed7/lib/scanstri.s7i       0.139927           1814
seed7/lib/scantoml.s7i       0.139133           1603
seed7/lib/seed7_05.s7i       0.122097           1072
seed7/lib/set.s7i            0.039711             57
seed7/lib/shell.s7i          0.072185            615
seed7/lib/showtls.s7i        0.087889            678
seed7/lib/signature.s7i      0.045088            131
seed7/lib/smtp.s7i           0.049901            261
seed7/lib/sockbase.s7i       0.052467            217
seed7/lib/socket.s7i         0.053857            326
seed7/lib/sokoban1.s7i       0.086545           1519
seed7/lib/sql_base.s7i       0.096633           1000
seed7/lib/stars.s7i          0.241627           1705
seed7/lib/stdfont10.s7i      0.150378           3347
seed7/lib/stdfont12.s7i      0.172897           3928
seed7/lib/stdfont14.s7i      0.195128           4510
seed7/lib/stdfont16.s7i      0.219333           5092
seed7/lib/stdfont18.s7i      0.254897           5868
seed7/lib/stdfont20.s7i      0.286859           6449
seed7/lib/stdfont24.s7i      0.340859           7421
seed7/lib/stdfont8.s7i       0.130052           2960
seed7/lib/stdfont9.s7i       0.137665           3152
seed7/lib/stdio.s7i          0.045456            192
seed7/lib/strifile.s7i       0.054806            345
seed7/lib/string.s7i         0.076985            779
seed7/lib/stritext.s7i       0.057552            352
seed7/lib/struct.s7i         0.058896            266
seed7/lib/struct_elem.s7i    0.043592            129
seed7/lib/subfile.s7i        0.045541            174
seed7/lib/subrange.s7i       0.041321             78
seed7/lib/syntax.s7i         0.062222            294
seed7/lib/tar.s7i            0.149539           1880
seed7/lib/tar_cmds.s7i       0.085855            752
seed7/lib/tdes.s7i           0.043680            143
seed7/lib/tee.s7i            0.041899            143
seed7/lib/text.s7i           0.043575            135
seed7/lib/tga.s7i            0.082358            676
seed7/lib/tiff.s7i           0.252612           2771
seed7/lib/time.s7i           0.104663           1191
seed7/lib/tls.s7i            0.205502           2230
seed7/lib/unicode.s7i        0.079867            575
seed7/lib/unionfnd.s7i       0.044134            130
seed7/lib/upper.s7i          0.044194            142
seed7/lib/utf16.s7i          0.067964            540
seed7/lib/utf8.s7i           0.049380            234
seed7/lib/vecfont10.s7i      0.164713           1056
seed7/lib/vecfont18.s7i      0.184650           1119
seed7/lib/vector3d.s7i       0.049831            293
seed7/lib/vectorfont.s7i     0.048995            239
seed7/lib/wildcard.s7i       0.042511            140
seed7/lib/window.s7i         0.062021            455
seed7/lib/wrinum.s7i         0.050678            248
seed7/lib/x509cert.s7i       0.122351           1243
seed7/lib/xml_ent.s7i        0.041882             94
seed7/lib/xmldom.s7i         0.052175            303
seed7/lib/xz.s7i             0.062330            442
seed7/lib/zip.s7i            0.241884           2792
seed7/lib/zstd.s7i           0.122201           1263
============================ ================= ================

Statistical Summary — Mode D (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.164089        |
+-----------+-----------------+
| Minimum   | 0.036925        |
+-----------+-----------------+
| Maximum   | 5.720472        |
+-----------+-----------------+

Cross-Mode Comparison
=====================

+------+-----------------+-----------------+-----------------+
| Mode | Average (s)     | Minimum (s)     | Maximum (s)     |
+======+=================+=================+=================+
| A    | 0.033959        | 0.032171        | 0.044017        |
+------+-----------------+-----------------+-----------------+
| B    | 0.039160        | 0.034891        | 0.047221        |
+------+-----------------+-----------------+-----------------+
| C    | 0.092942        | 0.036006        | 2.600730        |
+------+-----------------+-----------------+-----------------+
| D    | 0.164089        | 0.036925        | 5.720472        |
+------+-----------------+-----------------+-----------------+

Phase Timing Summary
====================

Wall-clock times include warm-up work and GC performed by the warm-up helper.

+----------+--------------+--------------+--------------+
| Phase    | Warm-up      | Timed pass   | Mode total   |
+==========+==============+==============+==============+
| Mode A   | 00:00:12.873 | 00:00:57.860 | 00:01:10.734 |
+----------+--------------+--------------+--------------+
| Mode B   | 00:00:14.852 | 00:01:06.787 | 00:01:21.639 |
+----------+--------------+--------------+--------------+
| Mode C   | 00:00:37.386 | 00:02:39.277 | 00:03:16.664 |
+----------+--------------+--------------+--------------+
| Mode D   | 00:01:03.689 | 00:04:40.440 | 00:05:44.129 |
+----------+--------------+--------------+--------------+
| Total    |              |              | 00:11:33.175 |
+----------+--------------+--------------+--------------+


.. ---------------------------------------------------------------------------
