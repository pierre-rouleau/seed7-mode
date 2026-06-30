=======================================================
GC-Controlled Benchmark Report: Seed7 Mode — Four Modes
=======================================================

:Running with: seed7-mode 2026-06-29T20:02:33+0000 W27-1
:Seed7 dir   : /Users/roup/my/dvo/seed7-repos/seed7/
:Started at: 16:13:36 local time
:Generated on: 2026-06-29 20:24:51 UTC
:N Iterations: 5  (mean of N timed opens per file)
:Window system: nil
:Display graphic: nil
:Frame size: 211x95 chars
:Window body: 211x93 chars
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
seed7/prg/addup.sd7          0.033556            190
seed7/prg/bas7.sd7           0.033565          11459
seed7/prg/bifurk.sd7         0.032907             73
seed7/prg/bigfiles.sd7       0.032698            129
seed7/prg/brainf7.sd7        0.033531             86
seed7/prg/calc7.sd7          0.032841            128
seed7/prg/carddemo.sd7       0.033102            190
seed7/prg/castle.sd7         0.035247           3148
seed7/prg/cat.sd7            0.039638             82
seed7/prg/cellauto.sd7       0.036653             85
seed7/prg/celsius.sd7        0.033253             42
seed7/prg/chk_all.sd7        0.033407            843
seed7/prg/chkarr.sd7         0.033703           8367
seed7/prg/chkbig.sd7         0.037345          29026
seed7/prg/chkbin.sd7         0.033936           6469
seed7/prg/chkbitdata.sd7     0.033600           6624
seed7/prg/chkbool.sd7        0.033177           3157
seed7/prg/chkbst.sd7         0.033389            722
seed7/prg/chkchr.sd7         0.033274           2809
seed7/prg/chkcmd.sd7         0.032831           1205
seed7/prg/chkdb.sd7          0.033907           7454
seed7/prg/chkdecl.sd7        0.032895            448
seed7/prg/chkenum.sd7        0.032996           1230
seed7/prg/chkerr.sd7         0.033245           4663
seed7/prg/chkexc.sd7         0.032685           2627
seed7/prg/chkfil.sd7         0.033227           1615
seed7/prg/chkflt.sd7         0.036961          20620
seed7/prg/chkhent.sd7        0.035521             54
seed7/prg/chkhsh.sd7         0.035750           4548
seed7/prg/chkidx.sd7         0.038878          19567
seed7/prg/chkint.sd7         0.043345          38129
seed7/prg/chkjson.sd7        0.035717           1764
seed7/prg/chkovf.sd7         0.034432           8216
seed7/prg/chkprc.sd7         0.033634          10111
seed7/prg/chkscan.sd7        0.032801            714
seed7/prg/chkset.sd7         0.034304          11974
seed7/prg/chkstr.sd7         0.037204          26952
seed7/prg/chktime.sd7        0.033633           2025
seed7/prg/chktoml.sd7        0.034968           1656
seed7/prg/clock.sd7          0.032603             47
seed7/prg/clock2.sd7         0.033120             43
seed7/prg/clock3.sd7         0.033302             95
seed7/prg/cmpfil.sd7         0.032940             84
seed7/prg/comanche.sd7       0.032921            180
seed7/prg/confval.sd7        0.032947            175
seed7/prg/db7.sd7            0.032950            417
seed7/prg/diff7.sd7          0.033381            263
seed7/prg/dirtst.sd7         0.033741             42
seed7/prg/dirx.sd7           0.032894            152
seed7/prg/dnafight.sd7       0.032966           1381
seed7/prg/dragon.sd7         0.033034             73
seed7/prg/echo.sd7           0.033102             39
seed7/prg/eliza.sd7          0.032952            302
seed7/prg/err.sd7            0.032867             96
seed7/prg/fannkuch.sd7       0.032722            131
seed7/prg/fib.sd7            0.032245             47
seed7/prg/find7.sd7          0.032030            133
seed7/prg/findchar.sd7       0.032283            149
seed7/prg/fractree.sd7       0.032964             55
seed7/prg/ftp7.sd7           0.032165            296
seed7/prg/ftpserv.sd7        0.032387             74
seed7/prg/gcd.sd7            0.033685            109
seed7/prg/gkbd.sd7           0.033194            358
seed7/prg/gtksvtst.sd7       0.033167             94
seed7/prg/hal.sd7            0.033443            250
seed7/prg/hamu.sd7           0.032852            573
seed7/prg/hanoi.sd7          0.033276             55
seed7/prg/hd.sd7             0.038877             79
seed7/prg/hello.sd7          0.034560             32
seed7/prg/hilbert.sd7        0.034197            108
seed7/prg/ide7.sd7           0.033148            196
seed7/prg/kbd.sd7            0.032883             49
seed7/prg/klondike.sd7       0.034889            883
seed7/prg/lander.sd7         0.033418           1551
seed7/prg/lst80bas.sd7       0.035040            344
seed7/prg/lst99bas.sd7       0.036568            401
seed7/prg/lstgwbas.sd7       0.036592            577
seed7/prg/mahjong.sd7        0.034221           1943
seed7/prg/make7.sd7          0.033170            121
seed7/prg/mandelbr.sd7       0.032923            237
seed7/prg/mind.sd7           0.033068            443
seed7/prg/mirror.sd7         0.033225            131
seed7/prg/ms.sd7             0.033307            641
seed7/prg/nicoma.sd7         0.032866            135
seed7/prg/pac.sd7            0.032845            726
seed7/prg/pairs.sd7          0.032992           2025
seed7/prg/panic.sd7          0.032708           2634
seed7/prg/percolation.sd7    0.033220            330
seed7/prg/planets.sd7        0.032544           1486
seed7/prg/portfwd7.sd7       0.033093            139
seed7/prg/prime.sd7          0.032752             74
seed7/prg/printpi1.sd7       0.036280             56
seed7/prg/printpi2.sd7       0.033914             54
seed7/prg/printpi3.sd7       0.033824             60
seed7/prg/pv7.sd7            0.032872            337
seed7/prg/queen.sd7          0.033149            149
seed7/prg/rand.sd7           0.033109            121
seed7/prg/raytrace.sd7       0.033259            538
seed7/prg/rever.sd7          0.032937            816
seed7/prg/roman.sd7          0.033120             38
seed7/prg/s7c.sd7            0.033123           9060
seed7/prg/s7check.sd7        0.033026             68
seed7/prg/savehd7.sd7        0.032885           1110
seed7/prg/self.sd7           0.033134             49
seed7/prg/shisen.sd7         0.033012           1423
seed7/prg/sl.sd7             0.033002           1029
seed7/prg/snake.sd7          0.032946            615
seed7/prg/sokoban.sd7        0.033373            891
seed7/prg/spigotpi.sd7       0.033023             64
seed7/prg/sql7.sd7           0.033552            278
seed7/prg/startrek.sd7       0.033226            979
seed7/prg/sudoku7.sd7        0.032923           2657
seed7/prg/sydir7.sd7         0.032754            384
seed7/prg/syntaxhl.sd7       0.033272            177
seed7/prg/tak.sd7            0.032715             59
seed7/prg/tar7.sd7           0.032767            121
seed7/prg/tch.sd7            0.032879             55
seed7/prg/testfont.sd7       0.032927             95
seed7/prg/tet.sd7            0.032901            479
seed7/prg/tetg.sd7           0.032237            501
seed7/prg/toutf8.sd7         0.032417            240
seed7/prg/tst_cli.sd7        0.033383             40
seed7/prg/tst_srv.sd7        0.032646             47
seed7/prg/wator.sd7          0.032486            651
seed7/prg/which.sd7          0.033555             65
seed7/prg/wiz.sd7            0.033244           2833
seed7/prg/wordcnt.sd7        0.032935             54
seed7/prg/wrinum.sd7         0.032905             43
seed7/prg/wumpus.sd7         0.033048            372
seed7/lib/aes.s7i            0.033362           1144
seed7/lib/aes_gcm.s7i        0.033457            392
seed7/lib/ar.s7i             0.032947           1532
seed7/lib/arc4.s7i           0.032389            144
seed7/lib/archive.s7i        0.033085            143
seed7/lib/archive_base.s7i   0.033754            135
seed7/lib/array.s7i          0.033065            610
seed7/lib/asn1.s7i           0.033295            544
seed7/lib/asn1oid.s7i        0.033067            157
seed7/lib/basearray.s7i      0.033196            450
seed7/lib/bigfile.s7i        0.032728            136
seed7/lib/bigint.s7i         0.032893            824
seed7/lib/bigrat.s7i         0.033005            784
seed7/lib/bin16.s7i          0.032752            592
seed7/lib/bin32.s7i          0.032900            490
seed7/lib/bin64.s7i          0.033022            539
seed7/lib/bitdata.s7i        0.033012           1330
seed7/lib/bitmapfont.s7i     0.032792            215
seed7/lib/bitset.s7i         0.032805            593
seed7/lib/bitsetof.s7i       0.033354            431
seed7/lib/blowfish.s7i       0.033006            383
seed7/lib/bmp.s7i            0.033283            924
seed7/lib/boolean.s7i        0.032859            403
seed7/lib/browser.s7i        0.033057            280
seed7/lib/bstring.s7i        0.032341            227
seed7/lib/bytedata.s7i       0.032716            482
seed7/lib/bzip2.s7i          0.032494            887
seed7/lib/cards.s7i          0.033014           1342
seed7/lib/category.s7i       0.035732            209
seed7/lib/cc_conf.s7i        0.032986           1314
seed7/lib/ccittfax.s7i       0.032964           1022
seed7/lib/cgi.s7i            0.033097            109
seed7/lib/cgidialog.s7i      0.033779           1118
seed7/lib/char.s7i           0.033188            356
seed7/lib/charsets.s7i       0.033607           2024
seed7/lib/chartype.s7i       0.033701            121
seed7/lib/cipher.s7i         0.033080            146
seed7/lib/cli_cmds.s7i       0.033176           1360
seed7/lib/clib_file.s7i      0.033280            301
seed7/lib/color.s7i          0.033074            185
seed7/lib/complex.s7i        0.032737            464
seed7/lib/compress.s7i       0.033040            150
seed7/lib/console.s7i        0.033048            188
seed7/lib/cpio.s7i           0.033069           1708
seed7/lib/crc32.s7i          0.033180            193
seed7/lib/cronos16.s7i       0.033444           1173
seed7/lib/cronos27.s7i       0.033506           1464
seed7/lib/csv.s7i            0.033049            201
seed7/lib/db_prop.s7i        0.033701            991
seed7/lib/deflate.s7i        0.033456            740
seed7/lib/des.s7i            0.033057            444
seed7/lib/dialog.s7i         0.033189            311
seed7/lib/dir.s7i            0.035684            163
seed7/lib/draw.s7i           0.041376            854
seed7/lib/duration.s7i       0.038299           1038
seed7/lib/echo.s7i           0.038388            132
seed7/lib/editline.s7i       0.036940            398
seed7/lib/elf.s7i            0.037191           1560
seed7/lib/elliptic.s7i       0.037702            649
seed7/lib/enable_io.s7i      0.042259            312
seed7/lib/encoding.s7i       0.037825            931
seed7/lib/enumeration.s7i    0.038998            236
seed7/lib/environment.s7i    0.037459            175
seed7/lib/exif.s7i           0.038399            152
seed7/lib/external_file.s7i  0.033223            340
seed7/lib/field.s7i          0.034880            268
seed7/lib/file.s7i           0.034597            372
seed7/lib/filebits.s7i       0.032931             46
seed7/lib/filesys.s7i        0.036363            601
seed7/lib/fileutil.s7i       0.034686            144
seed7/lib/fixarray.s7i       0.034599            307
seed7/lib/float.s7i          0.033658            757
seed7/lib/font.s7i           0.032838            196
seed7/lib/font8x8.s7i        0.038842            998
seed7/lib/forloop.s7i        0.039592            449
seed7/lib/ftp.s7i            0.038400            969
seed7/lib/ftpserv.s7i        0.036322            631
seed7/lib/getf.s7i           0.033836            115
seed7/lib/gethttp.s7i        0.036145             41
seed7/lib/gethttps.s7i       0.038576             41
seed7/lib/gif.s7i            0.038064            561
seed7/lib/graph.s7i          0.041124            415
seed7/lib/graph_file.s7i     0.043508            399
seed7/lib/gtkserver.s7i      0.040651            161
seed7/lib/gzip.s7i           0.039922            573
seed7/lib/hash.s7i           0.037487            421
seed7/lib/hashsetof.s7i      0.037789            499
seed7/lib/hmac.s7i           0.036342            152
seed7/lib/html.s7i           0.036590             83
seed7/lib/html_ent.s7i       0.036653            476
seed7/lib/htmldom.s7i        0.039326            286
seed7/lib/http_request.s7i   0.039746            696
seed7/lib/http_srv_resp.s7i  0.039972            380
seed7/lib/https_request.s7i  0.039977            211
seed7/lib/httpserv.s7i       0.036040            345
seed7/lib/huffman.s7i        0.034876            644
seed7/lib/ico.s7i            0.035978            221
seed7/lib/idxarray.s7i       0.037255            232
seed7/lib/image.s7i          0.039056            156
seed7/lib/imagefile.s7i      0.036910            171
seed7/lib/inflate.s7i        0.039605            411
seed7/lib/inifile.s7i        0.040247            129
seed7/lib/integer.s7i        0.039182            663
seed7/lib/iobuffer.s7i       0.035917            289
seed7/lib/jpeg.s7i           0.037371           1761
seed7/lib/json.s7i           0.037810            891
seed7/lib/json_serde.s7i     0.039943            783
seed7/lib/keybd.s7i          0.041017            639
seed7/lib/keydescr.s7i       0.035202            192
seed7/lib/leb128.s7i         0.036496            218
seed7/lib/line.s7i           0.036272            164
seed7/lib/listener.s7i       0.035280            247
seed7/lib/logfile.s7i        0.034634             73
seed7/lib/lower.s7i          0.033538            142
seed7/lib/lzma.s7i           0.033138            934
seed7/lib/lzw.s7i            0.032304            861
seed7/lib/magic.s7i          0.036297            403
seed7/lib/mahjng32.s7i       0.036287           1500
seed7/lib/make.s7i           0.034936            544
seed7/lib/makedata.s7i       0.035972           1428
seed7/lib/math.s7i           0.037438            201
seed7/lib/mixarith.s7i       0.040915            249
seed7/lib/modern27.s7i       0.040293           1099
seed7/lib/more.s7i           0.037177            130
seed7/lib/msgdigest.s7i      0.038220           1222
seed7/lib/multiscr.s7i       0.033874             68
seed7/lib/null_file.s7i      0.036120            345
seed7/lib/osfiles.s7i        0.035514           1085
seed7/lib/pbm.s7i            0.032988            230
seed7/lib/pcx.s7i            0.033945            638
seed7/lib/pem.s7i            0.033185            185
seed7/lib/pgm.s7i            0.033000            238
seed7/lib/pic16.s7i          0.034918           1037
seed7/lib/pic32.s7i          0.035460           2060
seed7/lib/pic_util.s7i       0.038118            144
seed7/lib/pixelimage.s7i     0.038288            320
seed7/lib/pixmap_file.s7i    0.033868            459
seed7/lib/pixmapfont.s7i     0.032808            184
seed7/lib/pkcs1.s7i          0.032746            543
seed7/lib/png.s7i            0.034797           1064
seed7/lib/poll.s7i           0.036984            313
seed7/lib/ppm.s7i            0.034411            240
seed7/lib/process.s7i        0.033157            541
seed7/lib/progs.s7i          0.037727            789
seed7/lib/propertyfile.s7i   0.039286            155
seed7/lib/rational.s7i       0.039056            792
seed7/lib/ref_list.s7i       0.039765            252
seed7/lib/reference.s7i      0.039572            126
seed7/lib/reverse.s7i        0.040640             94
seed7/lib/rpm.s7i            0.041080           3487
seed7/lib/rpmext.s7i         0.041114            318
seed7/lib/scanfile.s7i       0.040919           1779
seed7/lib/scanjson.s7i       0.040592            413
seed7/lib/scanstri.s7i       0.037722           1814
seed7/lib/scantoml.s7i       0.033015           1603
seed7/lib/seed7_05.s7i       0.034525           1072
seed7/lib/set.s7i            0.034420             57
seed7/lib/shell.s7i          0.035128            615
seed7/lib/showtls.s7i        0.033196            678
seed7/lib/signature.s7i      0.033003            131
seed7/lib/smtp.s7i           0.032866            261
seed7/lib/sockbase.s7i       0.032772            217
seed7/lib/socket.s7i         0.034927            326
seed7/lib/sokoban1.s7i       0.033149           1519
seed7/lib/sql_base.s7i       0.033013           1000
seed7/lib/stars.s7i          0.033045           1705
seed7/lib/stdfont10.s7i      0.032884           3347
seed7/lib/stdfont12.s7i      0.032956           3928
seed7/lib/stdfont14.s7i      0.032871           4510
seed7/lib/stdfont16.s7i      0.033748           5092
seed7/lib/stdfont18.s7i      0.033848           5868
seed7/lib/stdfont20.s7i      0.033029           6449
seed7/lib/stdfont24.s7i      0.033900           7421
seed7/lib/stdfont8.s7i       0.032446           2960
seed7/lib/stdfont9.s7i       0.032237           3152
seed7/lib/stdio.s7i          0.032780            192
seed7/lib/strifile.s7i       0.034861            345
seed7/lib/string.s7i         0.032941            779
seed7/lib/stritext.s7i       0.033059            352
seed7/lib/struct.s7i         0.033557            266
seed7/lib/struct_elem.s7i    0.039512            129
seed7/lib/subfile.s7i        0.034219            174
seed7/lib/subrange.s7i       0.034226             78
seed7/lib/syntax.s7i         0.035181            294
seed7/lib/tar.s7i            0.033849           1880
seed7/lib/tar_cmds.s7i       0.032954            752
seed7/lib/tdes.s7i           0.033375            143
seed7/lib/tee.s7i            0.033061            143
seed7/lib/text.s7i           0.032546            135
seed7/lib/tga.s7i            0.033353            676
seed7/lib/tiff.s7i           0.033286           2771
seed7/lib/time.s7i           0.033034           1191
seed7/lib/tls.s7i            0.033265           2230
seed7/lib/unicode.s7i        0.032953            575
seed7/lib/unionfnd.s7i       0.033027            130
seed7/lib/upper.s7i          0.036366            142
seed7/lib/utf16.s7i          0.038523            540
seed7/lib/utf8.s7i           0.034014            234
seed7/lib/vecfont10.s7i      0.036808           1056
seed7/lib/vecfont18.s7i      0.040326           1119
seed7/lib/vector3d.s7i       0.039168            293
seed7/lib/vectorfont.s7i     0.032954            239
seed7/lib/wildcard.s7i       0.032718            140
seed7/lib/window.s7i         0.032927            455
seed7/lib/wrinum.s7i         0.033848            248
seed7/lib/x509cert.s7i       0.035682           1243
seed7/lib/xml_ent.s7i        0.033531             94
seed7/lib/xmldom.s7i         0.033848            303
seed7/lib/xz.s7i             0.033002            442
seed7/lib/zip.s7i            0.035279           2792
seed7/lib/zstd.s7i           0.040179           1263
============================ ================= ================

Statistical Summary — Mode A (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.034692        |
+-----------+-----------------+
| Minimum   | 0.032030        |
+-----------+-----------------+
| Maximum   | 0.043508        |
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
seed7/prg/addup.sd7          0.038963            190
seed7/prg/bas7.sd7           0.040877          11459
seed7/prg/bifurk.sd7         0.036077             73
seed7/prg/bigfiles.sd7       0.037922            129
seed7/prg/brainf7.sd7        0.037430             86
seed7/prg/calc7.sd7          0.037518            128
seed7/prg/carddemo.sd7       0.038945            190
seed7/prg/castle.sd7         0.038674           3148
seed7/prg/cat.sd7            0.037375             82
seed7/prg/cellauto.sd7       0.036754             85
seed7/prg/celsius.sd7        0.035301             42
seed7/prg/chk_all.sd7        0.038713            843
seed7/prg/chkarr.sd7         0.039034           8367
seed7/prg/chkbig.sd7         0.044295          29026
seed7/prg/chkbin.sd7         0.039503           6469
seed7/prg/chkbitdata.sd7     0.039842           6624
seed7/prg/chkbool.sd7        0.038432           3157
seed7/prg/chkbst.sd7         0.038925            722
seed7/prg/chkchr.sd7         0.039891           2809
seed7/prg/chkcmd.sd7         0.038394           1205
seed7/prg/chkdb.sd7          0.039108           7454
seed7/prg/chkdecl.sd7        0.037843            448
seed7/prg/chkenum.sd7        0.037906           1230
seed7/prg/chkerr.sd7         0.038488           4663
seed7/prg/chkexc.sd7         0.037381           2627
seed7/prg/chkfil.sd7         0.040860           1615
seed7/prg/chkflt.sd7         0.044387          20620
seed7/prg/chkhent.sd7        0.035937             54
seed7/prg/chkhsh.sd7         0.040255           4548
seed7/prg/chkidx.sd7         0.041254          19567
seed7/prg/chkint.sd7         0.046037          38129
seed7/prg/chkjson.sd7        0.038659           1764
seed7/prg/chkovf.sd7         0.038920           8216
seed7/prg/chkprc.sd7         0.037541          10111
seed7/prg/chkscan.sd7        0.038893            714
seed7/prg/chkset.sd7         0.039874          11974
seed7/prg/chkstr.sd7         0.043193          26952
seed7/prg/chktime.sd7        0.039606           2025
seed7/prg/chktoml.sd7        0.038310           1656
seed7/prg/clock.sd7          0.035246             47
seed7/prg/clock2.sd7         0.035489             43
seed7/prg/clock3.sd7         0.038382             95
seed7/prg/cmpfil.sd7         0.036128             84
seed7/prg/comanche.sd7       0.038458            180
seed7/prg/confval.sd7        0.039817            175
seed7/prg/db7.sd7            0.038426            417
seed7/prg/diff7.sd7          0.041326            263
seed7/prg/dirtst.sd7         0.039986             42
seed7/prg/dirx.sd7           0.045232            152
seed7/prg/dnafight.sd7       0.045794           1381
seed7/prg/dragon.sd7         0.043775             73
seed7/prg/echo.sd7           0.043081             39
seed7/prg/eliza.sd7          0.045477            302
seed7/prg/err.sd7            0.041724             96
seed7/prg/fannkuch.sd7       0.038982            131
seed7/prg/fib.sd7            0.044230             47
seed7/prg/find7.sd7          0.045337            133
seed7/prg/findchar.sd7       0.046694            149
seed7/prg/fractree.sd7       0.041293             55
seed7/prg/ftp7.sd7           0.039645            296
seed7/prg/ftpserv.sd7        0.039271             74
seed7/prg/gcd.sd7            0.036877            109
seed7/prg/gkbd.sd7           0.039923            358
seed7/prg/gtksvtst.sd7       0.038878             94
seed7/prg/hal.sd7            0.039253            250
seed7/prg/hamu.sd7           0.042782            573
seed7/prg/hanoi.sd7          0.036754             55
seed7/prg/hd.sd7             0.042865             79
seed7/prg/hello.sd7          0.042789             32
seed7/prg/hilbert.sd7        0.040714            108
seed7/prg/ide7.sd7           0.038461            196
seed7/prg/kbd.sd7            0.035289             49
seed7/prg/klondike.sd7       0.038083            883
seed7/prg/lander.sd7         0.037929           1551
seed7/prg/lst80bas.sd7       0.036665            344
seed7/prg/lst99bas.sd7       0.038337            401
seed7/prg/lstgwbas.sd7       0.038635            577
seed7/prg/mahjong.sd7        0.039152           1943
seed7/prg/make7.sd7          0.039075            121
seed7/prg/mandelbr.sd7       0.038252            237
seed7/prg/mind.sd7           0.037963            443
seed7/prg/mirror.sd7         0.039577            131
seed7/prg/ms.sd7             0.040445            641
seed7/prg/nicoma.sd7         0.042567            135
seed7/prg/pac.sd7            0.043022            726
seed7/prg/pairs.sd7          0.042619           2025
seed7/prg/panic.sd7          0.043814           2634
seed7/prg/percolation.sd7    0.044086            330
seed7/prg/planets.sd7        0.045204           1486
seed7/prg/portfwd7.sd7       0.048937            139
seed7/prg/prime.sd7          0.037109             74
seed7/prg/printpi1.sd7       0.037512             56
seed7/prg/printpi2.sd7       0.040508             54
seed7/prg/printpi3.sd7       0.039895             60
seed7/prg/pv7.sd7            0.043324            337
seed7/prg/queen.sd7          0.043201            149
seed7/prg/rand.sd7           0.042331            121
seed7/prg/raytrace.sd7       0.043534            538
seed7/prg/rever.sd7          0.041505            816
seed7/prg/roman.sd7          0.036750             38
seed7/prg/s7c.sd7            0.038588           9060
seed7/prg/s7check.sd7        0.036931             68
seed7/prg/savehd7.sd7        0.040180           1110
seed7/prg/self.sd7           0.039797             49
seed7/prg/shisen.sd7         0.040399           1423
seed7/prg/sl.sd7             0.038705           1029
seed7/prg/snake.sd7          0.038084            615
seed7/prg/sokoban.sd7        0.038157            891
seed7/prg/spigotpi.sd7       0.039266             64
seed7/prg/sql7.sd7           0.042010            278
seed7/prg/startrek.sd7       0.045245            979
seed7/prg/sudoku7.sd7        0.039051           2657
seed7/prg/sydir7.sd7         0.038082            384
seed7/prg/syntaxhl.sd7       0.043100            177
seed7/prg/tak.sd7            0.037762             59
seed7/prg/tar7.sd7           0.043317            121
seed7/prg/tch.sd7            0.037811             55
seed7/prg/testfont.sd7       0.040062             95
seed7/prg/tet.sd7            0.038615            479
seed7/prg/tetg.sd7           0.038445            501
seed7/prg/toutf8.sd7         0.041074            240
seed7/prg/tst_cli.sd7        0.039406             40
seed7/prg/tst_srv.sd7        0.036074             47
seed7/prg/wator.sd7          0.038983            651
seed7/prg/which.sd7          0.036375             65
seed7/prg/wiz.sd7            0.037389           2833
seed7/prg/wordcnt.sd7        0.035147             54
seed7/prg/wrinum.sd7         0.034452             43
seed7/prg/wumpus.sd7         0.037443            372
seed7/lib/aes.s7i            0.040441           1144
seed7/lib/aes_gcm.s7i        0.040886            392
seed7/lib/ar.s7i             0.038965           1532
seed7/lib/arc4.s7i           0.038295            144
seed7/lib/archive.s7i        0.038036            143
seed7/lib/archive_base.s7i   0.038326            135
seed7/lib/array.s7i          0.038164            610
seed7/lib/asn1.s7i           0.036690            544
seed7/lib/asn1oid.s7i        0.042837            157
seed7/lib/basearray.s7i      0.039188            450
seed7/lib/bigfile.s7i        0.037991            136
seed7/lib/bigint.s7i         0.040567            824
seed7/lib/bigrat.s7i         0.041441            784
seed7/lib/bin16.s7i          0.038038            592
seed7/lib/bin32.s7i          0.038064            490
seed7/lib/bin64.s7i          0.038342            539
seed7/lib/bitdata.s7i        0.044587           1330
seed7/lib/bitmapfont.s7i     0.039051            215
seed7/lib/bitset.s7i         0.044393            593
seed7/lib/bitsetof.s7i       0.043851            431
seed7/lib/blowfish.s7i       0.044037            383
seed7/lib/bmp.s7i            0.041749            924
seed7/lib/boolean.s7i        0.039467            403
seed7/lib/browser.s7i        0.037799            280
seed7/lib/bstring.s7i        0.039866            227
seed7/lib/bytedata.s7i       0.041905            482
seed7/lib/bzip2.s7i          0.045192            887
seed7/lib/cards.s7i          0.041536           1342
seed7/lib/category.s7i       0.041604            209
seed7/lib/cc_conf.s7i        0.039041           1314
seed7/lib/ccittfax.s7i       0.038998           1022
seed7/lib/cgi.s7i            0.037684            109
seed7/lib/cgidialog.s7i      0.038461           1118
seed7/lib/char.s7i           0.038080            356
seed7/lib/charsets.s7i       0.038758           2024
seed7/lib/chartype.s7i       0.041223            121
seed7/lib/cipher.s7i         0.038088            146
seed7/lib/cli_cmds.s7i       0.038500           1360
seed7/lib/clib_file.s7i      0.037804            301
seed7/lib/color.s7i          0.038623            185
seed7/lib/complex.s7i        0.041180            464
seed7/lib/compress.s7i       0.038280            150
seed7/lib/console.s7i        0.037744            188
seed7/lib/cpio.s7i           0.038389           1708
seed7/lib/crc32.s7i          0.039081            193
seed7/lib/cronos16.s7i       0.041629           1173
seed7/lib/cronos27.s7i       0.045241           1464
seed7/lib/csv.s7i            0.046131            201
seed7/lib/db_prop.s7i        0.042907            991
seed7/lib/deflate.s7i        0.038744            740
seed7/lib/des.s7i            0.038046            444
seed7/lib/dialog.s7i         0.038897            311
seed7/lib/dir.s7i            0.038813            163
seed7/lib/draw.s7i           0.039206            854
seed7/lib/duration.s7i       0.044177           1038
seed7/lib/echo.s7i           0.040992            132
seed7/lib/editline.s7i       0.040755            398
seed7/lib/elf.s7i            0.046287           1560
seed7/lib/elliptic.s7i       0.038594            649
seed7/lib/enable_io.s7i      0.038890            312
seed7/lib/encoding.s7i       0.038425            931
seed7/lib/enumeration.s7i    0.038375            236
seed7/lib/environment.s7i    0.037396            175
seed7/lib/exif.s7i           0.039772            152
seed7/lib/external_file.s7i  0.037832            340
seed7/lib/field.s7i          0.038078            268
seed7/lib/file.s7i           0.038183            372
seed7/lib/filebits.s7i       0.036123             46
seed7/lib/filesys.s7i        0.037824            601
seed7/lib/fileutil.s7i       0.038032            144
seed7/lib/fixarray.s7i       0.039757            307
seed7/lib/float.s7i          0.038131            757
seed7/lib/font.s7i           0.037879            196
seed7/lib/font8x8.s7i        0.037312            998
seed7/lib/forloop.s7i        0.038802            449
seed7/lib/ftp.s7i            0.037862            969
seed7/lib/ftpserv.s7i        0.039349            631
seed7/lib/getf.s7i           0.041788            115
seed7/lib/gethttp.s7i        0.040696             41
seed7/lib/gethttps.s7i       0.037543             41
seed7/lib/gif.s7i            0.038908            561
seed7/lib/graph.s7i          0.039866            415
seed7/lib/graph_file.s7i     0.038437            399
seed7/lib/gtkserver.s7i      0.037619            161
seed7/lib/gzip.s7i           0.038494            573
seed7/lib/hash.s7i           0.039958            421
seed7/lib/hashsetof.s7i      0.040276            499
seed7/lib/hmac.s7i           0.038047            152
seed7/lib/html.s7i           0.037048             83
seed7/lib/html_ent.s7i       0.038435            476
seed7/lib/htmldom.s7i        0.037896            286
seed7/lib/http_request.s7i   0.038737            696
seed7/lib/http_srv_resp.s7i  0.039518            380
seed7/lib/https_request.s7i  0.038032            211
seed7/lib/httpserv.s7i       0.038019            345
seed7/lib/huffman.s7i        0.038071            644
seed7/lib/ico.s7i            0.038235            221
seed7/lib/idxarray.s7i       0.038210            232
seed7/lib/image.s7i          0.039797            156
seed7/lib/imagefile.s7i      0.038794            171
seed7/lib/inflate.s7i        0.039707            411
seed7/lib/inifile.s7i        0.038332            129
seed7/lib/integer.s7i        0.037358            663
seed7/lib/iobuffer.s7i       0.038704            289
seed7/lib/jpeg.s7i           0.038654           1761
seed7/lib/json.s7i           0.038728            891
seed7/lib/json_serde.s7i     0.039439            783
seed7/lib/keybd.s7i          0.038497            639
seed7/lib/keydescr.s7i       0.038633            192
seed7/lib/leb128.s7i         0.037512            218
seed7/lib/line.s7i           0.037546            164
seed7/lib/listener.s7i       0.037695            247
seed7/lib/logfile.s7i        0.036818             73
seed7/lib/lower.s7i          0.040255            142
seed7/lib/lzma.s7i           0.038718            934
seed7/lib/lzw.s7i            0.040956            861
seed7/lib/magic.s7i          0.046040            403
seed7/lib/mahjng32.s7i       0.041244           1500
seed7/lib/make.s7i           0.042695            544
seed7/lib/makedata.s7i       0.040030           1428
seed7/lib/math.s7i           0.040930            201
seed7/lib/mixarith.s7i       0.042132            249
seed7/lib/modern27.s7i       0.045107           1099
seed7/lib/more.s7i           0.041877            130
seed7/lib/msgdigest.s7i      0.039250           1222
seed7/lib/multiscr.s7i       0.036169             68
seed7/lib/null_file.s7i      0.037751            345
seed7/lib/osfiles.s7i        0.039263           1085
seed7/lib/pbm.s7i            0.038414            230
seed7/lib/pcx.s7i            0.037591            638
seed7/lib/pem.s7i            0.037342            185
seed7/lib/pgm.s7i            0.041845            238
seed7/lib/pic16.s7i          0.043980           1037
seed7/lib/pic32.s7i          0.038139           2060
seed7/lib/pic_util.s7i       0.040130            144
seed7/lib/pixelimage.s7i     0.038503            320
seed7/lib/pixmap_file.s7i    0.038105            459
seed7/lib/pixmapfont.s7i     0.040353            184
seed7/lib/pkcs1.s7i          0.043187            543
seed7/lib/png.s7i            0.038804           1064
seed7/lib/poll.s7i           0.038183            313
seed7/lib/ppm.s7i            0.038272            240
seed7/lib/process.s7i        0.038220            541
seed7/lib/progs.s7i          0.038055            789
seed7/lib/propertyfile.s7i   0.038297            155
seed7/lib/rational.s7i       0.038462            792
seed7/lib/ref_list.s7i       0.037952            252
seed7/lib/reference.s7i      0.038173            126
seed7/lib/reverse.s7i        0.037338             94
seed7/lib/rpm.s7i            0.041552           3487
seed7/lib/rpmext.s7i         0.043944            318
seed7/lib/scanfile.s7i       0.042550           1779
seed7/lib/scanjson.s7i       0.041742            413
seed7/lib/scanstri.s7i       0.041353           1814
seed7/lib/scantoml.s7i       0.040135           1603
seed7/lib/seed7_05.s7i       0.039702           1072
seed7/lib/set.s7i            0.039411             57
seed7/lib/shell.s7i          0.043241            615
seed7/lib/showtls.s7i        0.044382            678
seed7/lib/signature.s7i      0.046668            131
seed7/lib/smtp.s7i           0.042863            261
seed7/lib/sockbase.s7i       0.043746            217
seed7/lib/socket.s7i         0.042383            326
seed7/lib/sokoban1.s7i       0.042565           1519
seed7/lib/sql_base.s7i       0.042524           1000
seed7/lib/stars.s7i          0.040993           1705
seed7/lib/stdfont10.s7i      0.036925           3347
seed7/lib/stdfont12.s7i      0.038584           3928
seed7/lib/stdfont14.s7i      0.038038           4510
seed7/lib/stdfont16.s7i      0.038796           5092
seed7/lib/stdfont18.s7i      0.039915           5868
seed7/lib/stdfont20.s7i      0.039539           6449
seed7/lib/stdfont24.s7i      0.038200           7421
seed7/lib/stdfont8.s7i       0.036697           2960
seed7/lib/stdfont9.s7i       0.036869           3152
seed7/lib/stdio.s7i          0.038962            192
seed7/lib/strifile.s7i       0.039491            345
seed7/lib/string.s7i         0.037829            779
seed7/lib/stritext.s7i       0.039836            352
seed7/lib/struct.s7i         0.040345            266
seed7/lib/struct_elem.s7i    0.038230            129
seed7/lib/subfile.s7i        0.038490            174
seed7/lib/subrange.s7i       0.036411             78
seed7/lib/syntax.s7i         0.038013            294
seed7/lib/tar.s7i            0.038751           1880
seed7/lib/tar_cmds.s7i       0.039076            752
seed7/lib/tdes.s7i           0.038021            143
seed7/lib/tee.s7i            0.038131            143
seed7/lib/text.s7i           0.038128            135
seed7/lib/tga.s7i            0.040208            676
seed7/lib/tiff.s7i           0.040133           2771
seed7/lib/time.s7i           0.038291           1191
seed7/lib/tls.s7i            0.038293           2230
seed7/lib/unicode.s7i        0.040042            575
seed7/lib/unionfnd.s7i       0.037799            130
seed7/lib/upper.s7i          0.042615            142
seed7/lib/utf16.s7i          0.043276            540
seed7/lib/utf8.s7i           0.039445            234
seed7/lib/vecfont10.s7i      0.040847           1056
seed7/lib/vecfont18.s7i      0.041452           1119
seed7/lib/vector3d.s7i       0.038754            293
seed7/lib/vectorfont.s7i     0.038239            239
seed7/lib/wildcard.s7i       0.038261            140
seed7/lib/window.s7i         0.038140            455
seed7/lib/wrinum.s7i         0.038100            248
seed7/lib/x509cert.s7i       0.038539           1243
seed7/lib/xml_ent.s7i        0.037834             94
seed7/lib/xmldom.s7i         0.037309            303
seed7/lib/xz.s7i             0.038660            442
seed7/lib/zip.s7i            0.037525           2792
seed7/lib/zstd.s7i           0.037764           1263
============================ ================= ================

Statistical Summary — Mode B (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.039652        |
+-----------+-----------------+
| Minimum   | 0.034452        |
+-----------+-----------------+
| Maximum   | 0.048937        |
+-----------+-----------------+

Mode C — Mode Activation + Full-Buffer Fontification (font-lock-ensure)
-----------------------------------------------------------------------

Full-buffer fontification stress test.

File Load Times — Mode C (mean, GC-free)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

============================ ================= ================
File Name                    Mean Time (s)     Line count
============================ ================= ================
seed7/prg/addup.sd7          0.040322            190
seed7/prg/bas7.sd7           0.334226          11459
seed7/prg/bifurk.sd7         0.038471             73
seed7/prg/bigfiles.sd7       0.039997            129
seed7/prg/brainf7.sd7        0.037412             86
seed7/prg/calc7.sd7          0.039819            128
seed7/prg/carddemo.sd7       0.041123            190
seed7/prg/castle.sd7         0.110600           3148
seed7/prg/cat.sd7            0.037379             82
seed7/prg/cellauto.sd7       0.038581             85
seed7/prg/celsius.sd7        0.036415             42
seed7/prg/chk_all.sd7        0.060575            843
seed7/prg/chkarr.sd7         0.362974           8367
seed7/prg/chkbig.sd7         2.088469          29026
seed7/prg/chkbin.sd7         0.515330           6469
seed7/prg/chkbitdata.sd7     0.622542           6624
seed7/prg/chkbool.sd7        0.119621           3157
seed7/prg/chkbst.sd7         0.066438            722
seed7/prg/chkchr.sd7         0.226402           2809
seed7/prg/chkcmd.sd7         0.069558           1205
seed7/prg/chkdb.sd7          0.349846           7454
seed7/prg/chkdecl.sd7        0.058746            448
seed7/prg/chkenum.sd7        0.069841           1230
seed7/prg/chkerr.sd7         0.195375           4663
seed7/prg/chkexc.sd7         0.081779           2627
seed7/prg/chkfil.sd7         0.074925           1615
seed7/prg/chkflt.sd7         1.346800          20620
seed7/prg/chkhent.sd7        0.035420             54
seed7/prg/chkhsh.sd7         0.245926           4548
seed7/prg/chkidx.sd7         1.340781          19567
seed7/prg/chkint.sd7         2.573882          38129
seed7/prg/chkjson.sd7        0.109186           1764
seed7/prg/chkovf.sd7         0.566638           8216
seed7/prg/chkprc.sd7         0.323634          10111
seed7/prg/chkscan.sd7        0.061447            714
seed7/prg/chkset.sd7         0.662534          11974
seed7/prg/chkstr.sd7         1.441039          26952
seed7/prg/chktime.sd7        0.136205           2025
seed7/prg/chktoml.sd7        0.108730           1656
seed7/prg/clock.sd7          0.035603             47
seed7/prg/clock2.sd7         0.037470             43
seed7/prg/clock3.sd7         0.039041             95
seed7/prg/cmpfil.sd7         0.036244             84
seed7/prg/comanche.sd7       0.039527            180
seed7/prg/confval.sd7        0.041966            175
seed7/prg/db7.sd7            0.046039            417
seed7/prg/diff7.sd7          0.042509            263
seed7/prg/dirtst.sd7         0.036759             42
seed7/prg/dirx.sd7           0.037571            152
seed7/prg/dnafight.sd7       0.065734           1381
seed7/prg/dragon.sd7         0.035984             73
seed7/prg/echo.sd7           0.035107             39
seed7/prg/eliza.sd7          0.042152            302
seed7/prg/err.sd7            0.039003             96
seed7/prg/fannkuch.sd7       0.036831            131
seed7/prg/fib.sd7            0.035035             47
seed7/prg/find7.sd7          0.038011            133
seed7/prg/findchar.sd7       0.038200            149
seed7/prg/fractree.sd7       0.034891             55
seed7/prg/ftp7.sd7           0.040702            296
seed7/prg/ftpserv.sd7        0.036229             74
seed7/prg/gcd.sd7            0.035447            109
seed7/prg/gkbd.sd7           0.044155            358
seed7/prg/gtksvtst.sd7       0.036218             94
seed7/prg/hal.sd7            0.039291            250
seed7/prg/hamu.sd7           0.047404            573
seed7/prg/hanoi.sd7          0.035122             55
seed7/prg/hd.sd7             0.035759             79
seed7/prg/hello.sd7          0.034977             32
seed7/prg/hilbert.sd7        0.036192            108
seed7/prg/ide7.sd7           0.039324            196
seed7/prg/kbd.sd7            0.035270             49
seed7/prg/klondike.sd7       0.053700            883
seed7/prg/lander.sd7         0.070858           1551
seed7/prg/lst80bas.sd7       0.042908            344
seed7/prg/lst99bas.sd7       0.044102            401
seed7/prg/lstgwbas.sd7       0.049638            577
seed7/prg/mahjong.sd7        0.079759           1943
seed7/prg/make7.sd7          0.037400            121
seed7/prg/mandelbr.sd7       0.040211            237
seed7/prg/mind.sd7           0.044041            443
seed7/prg/mirror.sd7         0.038161            131
seed7/prg/ms.sd7             0.048270            641
seed7/prg/nicoma.sd7         0.037226            135
seed7/prg/pac.sd7            0.048201            726
seed7/prg/pairs.sd7          0.080055           2025
seed7/prg/panic.sd7          0.097547           2634
seed7/prg/percolation.sd7    0.043685            330
seed7/prg/planets.sd7        0.076328           1486
seed7/prg/portfwd7.sd7       0.037776            139
seed7/prg/prime.sd7          0.036570             74
seed7/prg/printpi1.sd7       0.035365             56
seed7/prg/printpi2.sd7       0.034970             54
seed7/prg/printpi3.sd7       0.035479             60
seed7/prg/pv7.sd7            0.043254            337
seed7/prg/queen.sd7          0.037690            149
seed7/prg/rand.sd7           0.036465            121
seed7/prg/raytrace.sd7       0.047289            538
seed7/prg/rever.sd7          0.053407            816
seed7/prg/roman.sd7          0.034399             38
seed7/prg/s7c.sd7            0.278603           9060
seed7/prg/s7check.sd7        0.035547             68
seed7/prg/savehd7.sd7        0.064447           1110
seed7/prg/self.sd7           0.034631             49
seed7/prg/shisen.sd7         0.069723           1423
seed7/prg/sl.sd7             0.060067           1029
seed7/prg/snake.sd7          0.045996            615
seed7/prg/sokoban.sd7        0.053871            891
seed7/prg/spigotpi.sd7       0.035253             64
seed7/prg/sql7.sd7           0.040603            278
seed7/prg/startrek.sd7       0.058230            979
seed7/prg/sudoku7.sd7        0.100607           2657
seed7/prg/sydir7.sd7         0.047478            384
seed7/prg/syntaxhl.sd7       0.040525            177
seed7/prg/tak.sd7            0.035107             59
seed7/prg/tar7.sd7           0.040284            121
seed7/prg/tch.sd7            0.042530             55
seed7/prg/testfont.sd7       0.037163             95
seed7/prg/tet.sd7            0.043794            479
seed7/prg/tetg.sd7           0.044616            501
seed7/prg/toutf8.sd7         0.041709            240
seed7/prg/tst_cli.sd7        0.034670             40
seed7/prg/tst_srv.sd7        0.039286             47
seed7/prg/wator.sd7          0.059665            651
seed7/prg/which.sd7          0.036619             65
seed7/prg/wiz.sd7            0.102163           2833
seed7/prg/wordcnt.sd7        0.034325             54
seed7/prg/wrinum.sd7         0.035598             43
seed7/prg/wumpus.sd7         0.041640            372
seed7/lib/aes.s7i            0.108682           1144
seed7/lib/aes_gcm.s7i        0.045177            392
seed7/lib/ar.s7i             0.072495           1532
seed7/lib/arc4.s7i           0.037303            144
seed7/lib/archive.s7i        0.037642            143
seed7/lib/archive_base.s7i   0.037838            135
seed7/lib/array.s7i          0.052767            610
seed7/lib/asn1.s7i           0.046604            544
seed7/lib/asn1oid.s7i        0.041013            157
seed7/lib/basearray.s7i      0.048437            450
seed7/lib/bigfile.s7i        0.037290            136
seed7/lib/bigint.s7i         0.054694            824
seed7/lib/bigrat.s7i         0.052784            784
seed7/lib/bin16.s7i          0.049739            592
seed7/lib/bin32.s7i          0.046878            490
seed7/lib/bin64.s7i          0.047991            539
seed7/lib/bitdata.s7i        0.074547           1330
seed7/lib/bitmapfont.s7i     0.038867            215
seed7/lib/bitset.s7i         0.047762            593
seed7/lib/bitsetof.s7i       0.047473            431
seed7/lib/blowfish.s7i       0.055250            383
seed7/lib/bmp.s7i            0.059416            924
seed7/lib/boolean.s7i        0.043801            403
seed7/lib/browser.s7i        0.041735            280
seed7/lib/bstring.s7i        0.040383            227
seed7/lib/bytedata.s7i       0.049260            482
seed7/lib/bzip2.s7i          0.058074            887
seed7/lib/cards.s7i          0.065717           1342
seed7/lib/category.s7i       0.040111            209
seed7/lib/cc_conf.s7i        0.077038           1314
seed7/lib/ccittfax.s7i       0.064463           1022
seed7/lib/cgi.s7i            0.036983            109
seed7/lib/cgidialog.s7i      0.059138           1118
seed7/lib/char.s7i           0.042366            356
seed7/lib/charsets.s7i       0.079574           2024
seed7/lib/chartype.s7i       0.038739            121
seed7/lib/cipher.s7i         0.037009            146
seed7/lib/cli_cmds.s7i       0.066295           1360
seed7/lib/clib_file.s7i      0.042008            301
seed7/lib/color.s7i          0.045586            185
seed7/lib/complex.s7i        0.052213            464
seed7/lib/compress.s7i       0.041353            150
seed7/lib/console.s7i        0.038753            188
seed7/lib/cpio.s7i           0.081761           1708
seed7/lib/crc32.s7i          0.043183            193
seed7/lib/cronos16.s7i       0.092038           1173
seed7/lib/cronos27.s7i       0.116551           1464
seed7/lib/csv.s7i            0.039716            201
seed7/lib/db_prop.s7i        0.062323            991
seed7/lib/deflate.s7i        0.054776            740
seed7/lib/des.s7i            0.054874            444
seed7/lib/dialog.s7i         0.042868            311
seed7/lib/dir.s7i            0.037598            163
seed7/lib/draw.s7i           0.055509            854
seed7/lib/duration.s7i       0.059289           1038
seed7/lib/echo.s7i           0.036962            132
seed7/lib/editline.s7i       0.044547            398
seed7/lib/elf.s7i            0.086276           1560
seed7/lib/elliptic.s7i       0.053881            649
seed7/lib/enable_io.s7i      0.045817            312
seed7/lib/encoding.s7i       0.061172            931
seed7/lib/enumeration.s7i    0.041642            236
seed7/lib/environment.s7i    0.038509            175
seed7/lib/exif.s7i           0.041622            152
seed7/lib/external_file.s7i  0.047269            340
seed7/lib/field.s7i          0.041234            268
seed7/lib/file.s7i           0.043859            372
seed7/lib/filebits.s7i       0.035778             46
seed7/lib/filesys.s7i        0.048058            601
seed7/lib/fileutil.s7i       0.037459            144
seed7/lib/fixarray.s7i       0.042563            307
seed7/lib/float.s7i          0.054658            757
seed7/lib/font.s7i           0.038857            196
seed7/lib/font8x8.s7i        0.047411            998
seed7/lib/forloop.s7i        0.044667            449
seed7/lib/ftp.s7i            0.056374            969
seed7/lib/ftpserv.s7i        0.050777            631
seed7/lib/getf.s7i           0.036485            115
seed7/lib/gethttp.s7i        0.034781             41
seed7/lib/gethttps.s7i       0.035605             41
seed7/lib/gif.s7i            0.049321            561
seed7/lib/graph.s7i          0.047992            415
seed7/lib/graph_file.s7i     0.043699            399
seed7/lib/gtkserver.s7i      0.037379            161
seed7/lib/gzip.s7i           0.048281            573
seed7/lib/hash.s7i           0.048745            421
seed7/lib/hashsetof.s7i      0.048282            499
seed7/lib/hmac.s7i           0.038372            152
seed7/lib/html.s7i           0.036651             83
seed7/lib/html_ent.s7i       0.046238            476
seed7/lib/htmldom.s7i        0.042541            286
seed7/lib/http_request.s7i   0.051303            696
seed7/lib/http_srv_resp.s7i  0.044541            380
seed7/lib/https_request.s7i  0.039420            211
seed7/lib/httpserv.s7i       0.049678            345
seed7/lib/huffman.s7i        0.054534            644
seed7/lib/ico.s7i            0.040521            221
seed7/lib/idxarray.s7i       0.041380            232
seed7/lib/image.s7i          0.036704            156
seed7/lib/imagefile.s7i      0.040160            171
seed7/lib/inflate.s7i        0.046701            411
seed7/lib/inifile.s7i        0.036695            129
seed7/lib/integer.s7i        0.051247            663
seed7/lib/iobuffer.s7i       0.043357            289
seed7/lib/jpeg.s7i           0.083482           1761
seed7/lib/json.s7i           0.055334            891
seed7/lib/json_serde.s7i     0.052232            783
seed7/lib/keybd.s7i          0.055129            639
seed7/lib/keydescr.s7i       0.041046            192
seed7/lib/leb128.s7i         0.039031            218
seed7/lib/line.s7i           0.037983            164
seed7/lib/listener.s7i       0.040992            247
seed7/lib/logfile.s7i        0.035909             73
seed7/lib/lower.s7i          0.037651            142
seed7/lib/lzma.s7i           0.059492            934
seed7/lib/lzw.s7i            0.058200            861
seed7/lib/magic.s7i          0.047165            403
seed7/lib/mahjng32.s7i       0.063887           1500
seed7/lib/make.s7i           0.049727            544
seed7/lib/makedata.s7i       0.068355           1428
seed7/lib/math.s7i           0.042745            201
seed7/lib/mixarith.s7i       0.038812            249
seed7/lib/modern27.s7i       0.081881           1099
seed7/lib/more.s7i           0.036658            130
seed7/lib/msgdigest.s7i      0.081008           1222
seed7/lib/multiscr.s7i       0.036686             68
seed7/lib/null_file.s7i      0.041539            345
seed7/lib/osfiles.s7i        0.067899           1085
seed7/lib/pbm.s7i            0.045867            230
seed7/lib/pcx.s7i            0.055015            638
seed7/lib/pem.s7i            0.039611            185
seed7/lib/pgm.s7i            0.040044            238
seed7/lib/pic16.s7i          0.050227           1037
seed7/lib/pic32.s7i          0.080631           2060
seed7/lib/pic_util.s7i       0.037789            144
seed7/lib/pixelimage.s7i     0.042006            320
seed7/lib/pixmap_file.s7i    0.047540            459
seed7/lib/pixmapfont.s7i     0.039862            184
seed7/lib/pkcs1.s7i          0.058939            543
seed7/lib/png.s7i            0.063557           1064
seed7/lib/poll.s7i           0.042823            313
seed7/lib/ppm.s7i            0.039117            240
seed7/lib/process.s7i        0.047422            541
seed7/lib/progs.s7i          0.055434            789
seed7/lib/propertyfile.s7i   0.037474            155
seed7/lib/rational.s7i       0.053567            792
seed7/lib/ref_list.s7i       0.040953            252
seed7/lib/reference.s7i      0.037768            126
seed7/lib/reverse.s7i        0.036075             94
seed7/lib/rpm.s7i            0.142877           3487
seed7/lib/rpmext.s7i         0.052467            318
seed7/lib/scanfile.s7i       0.079476           1779
seed7/lib/scanjson.s7i       0.046211            413
seed7/lib/scanstri.s7i       0.080071           1814
seed7/lib/scantoml.s7i       0.070753           1603
seed7/lib/seed7_05.s7i       0.065425           1072
seed7/lib/set.s7i            0.035650             57
seed7/lib/shell.s7i          0.052240            615
seed7/lib/showtls.s7i        0.055769            678
seed7/lib/signature.s7i      0.039400            131
seed7/lib/smtp.s7i           0.039541            261
seed7/lib/sockbase.s7i       0.041952            217
seed7/lib/socket.s7i         0.042246            326
seed7/lib/sokoban1.s7i       0.055563           1519
seed7/lib/sql_base.s7i       0.068254           1000
seed7/lib/stars.s7i          0.135421           1705
seed7/lib/stdfont10.s7i      0.080438           3347
seed7/lib/stdfont12.s7i      0.091522           3928
seed7/lib/stdfont14.s7i      0.101933           4510
seed7/lib/stdfont16.s7i      0.114551           5092
seed7/lib/stdfont18.s7i      0.130792           5868
seed7/lib/stdfont20.s7i      0.145012           6449
seed7/lib/stdfont24.s7i      0.176523           7421
seed7/lib/stdfont8.s7i       0.070728           2960
seed7/lib/stdfont9.s7i       0.076036           3152
seed7/lib/stdio.s7i          0.038608            192
seed7/lib/strifile.s7i       0.042571            345
seed7/lib/string.s7i         0.055069            779
seed7/lib/stritext.s7i       0.042325            352
seed7/lib/struct.s7i         0.043937            266
seed7/lib/struct_elem.s7i    0.037155            129
seed7/lib/subfile.s7i        0.038360            174
seed7/lib/subrange.s7i       0.035969             78
seed7/lib/syntax.s7i         0.045137            294
seed7/lib/tar.s7i            0.081262           1880
seed7/lib/tar_cmds.s7i       0.055510            752
seed7/lib/tdes.s7i           0.038154            143
seed7/lib/tee.s7i            0.037004            143
seed7/lib/text.s7i           0.037125            135
seed7/lib/tga.s7i            0.052728            676
seed7/lib/tiff.s7i           0.120242           2771
seed7/lib/time.s7i           0.060931           1191
seed7/lib/tls.s7i            0.102226           2230
seed7/lib/unicode.s7i        0.052986            575
seed7/lib/unionfnd.s7i       0.037881            130
seed7/lib/upper.s7i          0.037432            142
seed7/lib/utf16.s7i          0.048707            540
seed7/lib/utf8.s7i           0.040994            234
seed7/lib/vecfont10.s7i      0.077882           1056
seed7/lib/vecfont18.s7i      0.086500           1119
seed7/lib/vector3d.s7i       0.040145            293
seed7/lib/vectorfont.s7i     0.040395            239
seed7/lib/wildcard.s7i       0.037306            140
seed7/lib/window.s7i         0.044507            455
seed7/lib/wrinum.s7i         0.040721            248
seed7/lib/x509cert.s7i       0.070375           1243
seed7/lib/xml_ent.s7i        0.036732             94
seed7/lib/xmldom.s7i         0.040584            303
seed7/lib/xz.s7i             0.045030            442
seed7/lib/zip.s7i            0.126042           2792
seed7/lib/zstd.s7i           0.067771           1263
============================ ================= ================

Statistical Summary — Mode C (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.089476        |
+-----------+-----------------+
| Minimum   | 0.034325        |
+-----------+-----------------+
| Maximum   | 2.573882        |
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
seed7/prg/addup.sd7          0.044518            190
seed7/prg/bas7.sd7           0.756074          11459
seed7/prg/bifurk.sd7         0.037177             73
seed7/prg/bigfiles.sd7       0.040351            129
seed7/prg/brainf7.sd7        0.037314             86
seed7/prg/calc7.sd7          0.039649            128
seed7/prg/carddemo.sd7       0.045063            190
seed7/prg/castle.sd7         0.209755           3148
seed7/prg/cat.sd7            0.036877             82
seed7/prg/cellauto.sd7       0.037063             85
seed7/prg/celsius.sd7        0.036124             42
seed7/prg/chk_all.sd7        0.081597            843
seed7/prg/chkarr.sd7         0.859191           8367
seed7/prg/chkbig.sd7         4.102016          29026
seed7/prg/chkbin.sd7         1.029718           6469
seed7/prg/chkbitdata.sd7     1.254468           6624
seed7/prg/chkbool.sd7        0.229625           3157
seed7/prg/chkbst.sd7         0.101942            722
seed7/prg/chkchr.sd7         0.474652           2809
seed7/prg/chkcmd.sd7         0.109558           1205
seed7/prg/chkdb.sd7          0.758212           7454
seed7/prg/chkdecl.sd7        0.091327            448
seed7/prg/chkenum.sd7        0.120600           1230
seed7/prg/chkerr.sd7         0.341925           4663
seed7/prg/chkexc.sd7         0.147960           2627
seed7/prg/chkfil.sd7         0.127552           1615
seed7/prg/chkflt.sd7         2.818673          20620
seed7/prg/chkhent.sd7        0.037193             54
seed7/prg/chkhsh.sd7         0.502394           4548
seed7/prg/chkidx.sd7         3.184617          19567
seed7/prg/chkint.sd7         5.585167          38129
seed7/prg/chkjson.sd7        0.186489           1764
seed7/prg/chkovf.sd7         1.213033           8216
seed7/prg/chkprc.sd7         0.705026          10111
seed7/prg/chkscan.sd7        0.091685            714
seed7/prg/chkset.sd7         1.753908          11974
seed7/prg/chkstr.sd7         3.411852          26952
seed7/prg/chktime.sd7        0.247392           2025
seed7/prg/chktoml.sd7        0.199373           1656
seed7/prg/clock.sd7          0.036021             47
seed7/prg/clock2.sd7         0.035617             43
seed7/prg/clock3.sd7         0.042856             95
seed7/prg/cmpfil.sd7         0.041382             84
seed7/prg/comanche.sd7       0.046696            180
seed7/prg/confval.sd7        0.049997            175
seed7/prg/db7.sd7            0.061932            417
seed7/prg/diff7.sd7          0.050530            263
seed7/prg/dirtst.sd7         0.035563             42
seed7/prg/dirx.sd7           0.041560            152
seed7/prg/dnafight.sd7       0.117630           1381
seed7/prg/dragon.sd7         0.038430             73
seed7/prg/echo.sd7           0.035858             39
seed7/prg/eliza.sd7          0.056904            302
seed7/prg/err.sd7            0.044777             96
seed7/prg/fannkuch.sd7       0.040918            131
seed7/prg/fib.sd7            0.036494             47
seed7/prg/find7.sd7          0.041815            133
seed7/prg/findchar.sd7       0.043092            149
seed7/prg/fractree.sd7       0.036850             55
seed7/prg/ftp7.sd7           0.052435            296
seed7/prg/ftpserv.sd7        0.038395             74
seed7/prg/gcd.sd7            0.040139            109
seed7/prg/gkbd.sd7           0.064242            358
seed7/prg/gtksvtst.sd7       0.038188             94
seed7/prg/hal.sd7            0.047034            250
seed7/prg/hamu.sd7           0.066034            573
seed7/prg/hanoi.sd7          0.036673             55
seed7/prg/hd.sd7             0.038313             79
seed7/prg/hello.sd7          0.035671             32
seed7/prg/hilbert.sd7        0.039764            108
seed7/prg/ide7.sd7           0.046636            196
seed7/prg/kbd.sd7            0.036046             49
seed7/prg/klondike.sd7       0.086604            883
seed7/prg/lander.sd7         0.132028           1551
seed7/prg/lst80bas.sd7       0.054795            344
seed7/prg/lst99bas.sd7       0.059044            401
seed7/prg/lstgwbas.sd7       0.072642            577
seed7/prg/mahjong.sd7        0.147296           1943
seed7/prg/make7.sd7          0.041480            121
seed7/prg/mandelbr.sd7       0.048406            237
seed7/prg/mind.sd7           0.058776            443
seed7/prg/mirror.sd7         0.043218            131
seed7/prg/ms.sd7             0.070069            641
seed7/prg/nicoma.sd7         0.041680            135
seed7/prg/pac.sd7            0.069396            726
seed7/prg/pairs.sd7          0.137245           2025
seed7/prg/panic.sd7          0.191947           2634
seed7/prg/percolation.sd7    0.054312            330
seed7/prg/planets.sd7        0.138439           1486
seed7/prg/portfwd7.sd7       0.043436            139
seed7/prg/prime.sd7          0.037569             74
seed7/prg/printpi1.sd7       0.037293             56
seed7/prg/printpi2.sd7       0.036432             54
seed7/prg/printpi3.sd7       0.038435             60
seed7/prg/pv7.sd7            0.056904            337
seed7/prg/queen.sd7          0.042470            149
seed7/prg/rand.sd7           0.040549            121
seed7/prg/raytrace.sd7       0.067421            538
seed7/prg/rever.sd7          0.080337            816
seed7/prg/roman.sd7          0.035835             38
seed7/prg/s7c.sd7            0.626623           9060
seed7/prg/s7check.sd7        0.038022             68
seed7/prg/savehd7.sd7        0.111433           1110
seed7/prg/self.sd7           0.036703             49
seed7/prg/shisen.sd7         0.119856           1423
seed7/prg/sl.sd7             0.099506           1029
seed7/prg/snake.sd7          0.066818            615
seed7/prg/sokoban.sd7        0.079709            891
seed7/prg/spigotpi.sd7       0.038075             64
seed7/prg/sql7.sd7           0.051745            278
seed7/prg/startrek.sd7       0.092522            979
seed7/prg/sudoku7.sd7        0.196223           2657
seed7/prg/sydir7.sd7         0.059641            384
seed7/prg/syntaxhl.sd7       0.047402            177
seed7/prg/tak.sd7            0.037016             59
seed7/prg/tar7.sd7           0.042084            121
seed7/prg/tch.sd7            0.037149             55
seed7/prg/testfont.sd7       0.040855             95
seed7/prg/tet.sd7            0.059709            479
seed7/prg/tetg.sd7           0.061357            501
seed7/prg/toutf8.sd7         0.050403            240
seed7/prg/tst_cli.sd7        0.036327             40
seed7/prg/tst_srv.sd7        0.035855             47
seed7/prg/wator.sd7          0.078361            651
seed7/prg/which.sd7          0.037718             65
seed7/prg/wiz.sd7            0.207527           2833
seed7/prg/wordcnt.sd7        0.036503             54
seed7/prg/wrinum.sd7         0.035562             43
seed7/prg/wumpus.sd7         0.052978            372
seed7/lib/aes.s7i            0.199350           1144
seed7/lib/aes_gcm.s7i        0.060325            392
seed7/lib/ar.s7i             0.123498           1532
seed7/lib/arc4.s7i           0.042053            144
seed7/lib/archive.s7i        0.042801            143
seed7/lib/archive_base.s7i   0.042550            135
seed7/lib/array.s7i          0.074682            610
seed7/lib/asn1.s7i           0.062202            544
seed7/lib/asn1oid.s7i        0.048216            157
seed7/lib/basearray.s7i      0.061518            450
seed7/lib/bigfile.s7i        0.040628            136
seed7/lib/bigint.s7i         0.078581            824
seed7/lib/bigrat.s7i         0.078349            784
seed7/lib/bin16.s7i          0.066866            592
seed7/lib/bin32.s7i          0.060413            490
seed7/lib/bin64.s7i          0.062029            539
seed7/lib/bitdata.s7i        0.122907           1330
seed7/lib/bitmapfont.s7i     0.047119            215
seed7/lib/bitset.s7i         0.063076            593
seed7/lib/bitsetof.s7i       0.061372            431
seed7/lib/blowfish.s7i       0.076605            383
seed7/lib/bmp.s7i            0.100548            924
seed7/lib/boolean.s7i        0.054271            403
seed7/lib/browser.s7i        0.051548            280
seed7/lib/bstring.s7i        0.045837            227
seed7/lib/bytedata.s7i       0.064559            482
seed7/lib/bzip2.s7i          0.088238            887
seed7/lib/cards.s7i          0.101783           1342
seed7/lib/category.s7i       0.048086            209
seed7/lib/cc_conf.s7i        0.119596           1314
seed7/lib/ccittfax.s7i       0.101805           1022
seed7/lib/cgi.s7i            0.040058            109
seed7/lib/cgidialog.s7i      0.099095           1118
seed7/lib/char.s7i           0.050653            356
seed7/lib/charsets.s7i       0.124852           2024
seed7/lib/chartype.s7i       0.047237            121
seed7/lib/cipher.s7i         0.041171            146
seed7/lib/cli_cmds.s7i       0.110478           1360
seed7/lib/clib_file.s7i      0.050349            301
seed7/lib/color.s7i          0.044844            185
seed7/lib/complex.s7i        0.057753            464
seed7/lib/compress.s7i       0.044445            150
seed7/lib/console.s7i        0.044602            188
seed7/lib/cpio.s7i           0.147476           1708
seed7/lib/crc32.s7i          0.053032            193
seed7/lib/cronos16.s7i       0.193447           1173
seed7/lib/cronos27.s7i       0.255776           1464
seed7/lib/csv.s7i            0.047600            201
seed7/lib/db_prop.s7i        0.102808            991
seed7/lib/deflate.s7i        0.088781            740
seed7/lib/des.s7i            0.077854            444
seed7/lib/dialog.s7i         0.057169            311
seed7/lib/dir.s7i            0.042025            163
seed7/lib/draw.s7i           0.084454            854
seed7/lib/duration.s7i       0.103395           1038
seed7/lib/echo.s7i           0.041879            132
seed7/lib/editline.s7i       0.059275            398
seed7/lib/elf.s7i            0.159155           1560
seed7/lib/elliptic.s7i       0.075772            649
seed7/lib/enable_io.s7i      0.051693            312
seed7/lib/encoding.s7i       0.096251            931
seed7/lib/enumeration.s7i    0.048456            236
seed7/lib/environment.s7i    0.042294            175
seed7/lib/exif.s7i           0.044203            152
seed7/lib/external_file.s7i  0.050220            340
seed7/lib/field.s7i          0.049934            268
seed7/lib/file.s7i           0.052267            372
seed7/lib/filebits.s7i       0.037770             46
seed7/lib/filesys.s7i        0.063415            601
seed7/lib/fileutil.s7i       0.042341            144
seed7/lib/fixarray.s7i       0.052473            307
seed7/lib/float.s7i          0.072798            757
seed7/lib/font.s7i           0.044275            196
seed7/lib/font8x8.s7i        0.067331            998
seed7/lib/forloop.s7i        0.058118            449
seed7/lib/ftp.s7i            0.087586            969
seed7/lib/ftpserv.s7i        0.076319            631
seed7/lib/getf.s7i           0.040765            115
seed7/lib/gethttp.s7i        0.037913             41
seed7/lib/gethttps.s7i       0.036309             41
seed7/lib/gif.s7i            0.069973            561
seed7/lib/graph.s7i          0.063346            415
seed7/lib/graph_file.s7i     0.056401            399
seed7/lib/gtkserver.s7i      0.040084            161
seed7/lib/gzip.s7i           0.065794            573
seed7/lib/hash.s7i           0.064645            421
seed7/lib/hashsetof.s7i      0.066098            499
seed7/lib/hmac.s7i           0.043375            152
seed7/lib/html.s7i           0.038309             83
seed7/lib/html_ent.s7i       0.062230            476
seed7/lib/htmldom.s7i        0.052719            286
seed7/lib/http_request.s7i   0.076652            696
seed7/lib/http_srv_resp.s7i  0.058841            380
seed7/lib/https_request.s7i  0.047322            211
seed7/lib/httpserv.s7i       0.058763            345
seed7/lib/huffman.s7i        0.087858            644
seed7/lib/ico.s7i            0.048529            221
seed7/lib/idxarray.s7i       0.049849            232
seed7/lib/image.s7i          0.041155            156
seed7/lib/imagefile.s7i      0.044170            171
seed7/lib/inflate.s7i        0.062134            411
seed7/lib/inifile.s7i        0.040886            129
seed7/lib/integer.s7i        0.066375            663
seed7/lib/iobuffer.s7i       0.049247            289
seed7/lib/jpeg.s7i           0.155846           1761
seed7/lib/json.s7i           0.080282            891
seed7/lib/json_serde.s7i     0.078098            783
seed7/lib/keybd.s7i          0.079150            639
seed7/lib/keydescr.s7i       0.049591            192
seed7/lib/leb128.s7i         0.045931            218
seed7/lib/line.s7i           0.042340            164
seed7/lib/listener.s7i       0.048721            247
seed7/lib/logfile.s7i        0.037741             73
seed7/lib/lower.s7i          0.040985            142
seed7/lib/lzma.s7i           0.096791            934
seed7/lib/lzw.s7i            0.088648            861
seed7/lib/magic.s7i          0.062416            403
seed7/lib/mahjng32.s7i       0.090060           1500
seed7/lib/make.s7i           0.067244            544
seed7/lib/makedata.s7i       0.119761           1428
seed7/lib/math.s7i           0.044832            201
seed7/lib/mixarith.s7i       0.046041            249
seed7/lib/modern27.s7i       0.170245           1099
seed7/lib/more.s7i           0.041612            130
seed7/lib/msgdigest.s7i      0.136896           1222
seed7/lib/multiscr.s7i       0.037512             68
seed7/lib/null_file.s7i      0.050190            345
seed7/lib/osfiles.s7i        0.094195           1085
seed7/lib/pbm.s7i            0.047701            230
seed7/lib/pcx.s7i            0.076717            638
seed7/lib/pem.s7i            0.043246            185
seed7/lib/pgm.s7i            0.047174            238
seed7/lib/pic16.s7i          0.065038           1037
seed7/lib/pic32.s7i          0.119213           2060
seed7/lib/pic_util.s7i       0.044368            144
seed7/lib/pixelimage.s7i     0.052066            320
seed7/lib/pixmap_file.s7i    0.061620            459
seed7/lib/pixmapfont.s7i     0.046108            184
seed7/lib/pkcs1.s7i          0.077271            543
seed7/lib/png.s7i            0.109204           1064
seed7/lib/poll.s7i           0.052430            313
seed7/lib/ppm.s7i            0.048196            240
seed7/lib/process.s7i        0.063927            541
seed7/lib/progs.s7i          0.078153            789
seed7/lib/propertyfile.s7i   0.043190            155
seed7/lib/rational.s7i       0.078211            792
seed7/lib/ref_list.s7i       0.048124            252
seed7/lib/reference.s7i      0.040285            126
seed7/lib/reverse.s7i        0.037929             94
seed7/lib/rpm.s7i            0.288506           3487
seed7/lib/rpmext.s7i         0.053674            318
seed7/lib/scanfile.s7i       0.134893           1779
seed7/lib/scanjson.s7i       0.060096            413
seed7/lib/scanstri.s7i       0.142177           1814
seed7/lib/scantoml.s7i       0.129804           1603
seed7/lib/seed7_05.s7i       0.110089           1072
seed7/lib/set.s7i            0.037412             57
seed7/lib/shell.s7i          0.066920            615
seed7/lib/showtls.s7i        0.090209            678
seed7/lib/signature.s7i      0.041097            131
seed7/lib/smtp.s7i           0.049060            261
seed7/lib/sockbase.s7i       0.053112            217
seed7/lib/socket.s7i         0.050684            326
seed7/lib/sokoban1.s7i       0.079201           1519
seed7/lib/sql_base.s7i       0.091992           1000
seed7/lib/stars.s7i          0.230882           1705
seed7/lib/stdfont10.s7i      0.141402           3347
seed7/lib/stdfont12.s7i      0.165093           3928
seed7/lib/stdfont14.s7i      0.184533           4510
seed7/lib/stdfont16.s7i      0.212438           5092
seed7/lib/stdfont18.s7i      0.244398           5868
seed7/lib/stdfont20.s7i      0.272613           6449
seed7/lib/stdfont24.s7i      0.323703           7421
seed7/lib/stdfont8.s7i       0.126154           2960
seed7/lib/stdfont9.s7i       0.133250           3152
seed7/lib/stdio.s7i          0.043680            192
seed7/lib/strifile.s7i       0.053118            345
seed7/lib/string.s7i         0.074758            779
seed7/lib/stritext.s7i       0.053988            352
seed7/lib/struct.s7i         0.054549            266
seed7/lib/struct_elem.s7i    0.041437            129
seed7/lib/subfile.s7i        0.042486            174
seed7/lib/subrange.s7i       0.038282             78
seed7/lib/syntax.s7i         0.059495            294
seed7/lib/tar.s7i            0.142948           1880
seed7/lib/tar_cmds.s7i       0.082331            752
seed7/lib/tdes.s7i           0.049977            143
seed7/lib/tee.s7i            0.044080            143
seed7/lib/text.s7i           0.041467            135
seed7/lib/tga.s7i            0.078975            676
seed7/lib/tiff.s7i           0.242037           2771
seed7/lib/time.s7i           0.100769           1191
seed7/lib/tls.s7i            0.196292           2230
seed7/lib/unicode.s7i        0.072394            575
seed7/lib/unionfnd.s7i       0.041518            130
seed7/lib/upper.s7i          0.041440            142
seed7/lib/utf16.s7i          0.063254            540
seed7/lib/utf8.s7i           0.045882            234
seed7/lib/vecfont10.s7i      0.158292           1056
seed7/lib/vecfont18.s7i      0.178383           1119
seed7/lib/vector3d.s7i       0.048923            293
seed7/lib/vectorfont.s7i     0.047216            239
seed7/lib/wildcard.s7i       0.042019            140
seed7/lib/window.s7i         0.059826            455
seed7/lib/wrinum.s7i         0.048908            248
seed7/lib/x509cert.s7i       0.117082           1243
seed7/lib/xml_ent.s7i        0.039369             94
seed7/lib/xmldom.s7i         0.049724            303
seed7/lib/xz.s7i             0.059304            442
seed7/lib/zip.s7i            0.232942           2792
seed7/lib/zstd.s7i           0.114939           1263
============================ ================= ================

Statistical Summary — Mode D (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.158013        |
+-----------+-----------------+
| Minimum   | 0.035562        |
+-----------+-----------------+
| Maximum   | 5.585167        |
+-----------+-----------------+

Cross-Mode Comparison
=====================

+------+-----------------+-----------------+-----------------+
| Mode | Average (s)     | Minimum (s)     | Maximum (s)     |
+======+=================+=================+=================+
| A    | 0.034692        | 0.032030        | 0.043508        |
+------+-----------------+-----------------+-----------------+
| B    | 0.039652        | 0.034452        | 0.048937        |
+------+-----------------+-----------------+-----------------+
| C    | 0.089476        | 0.034325        | 2.573882        |
+------+-----------------+-----------------+-----------------+
| D    | 0.158013        | 0.035562        | 5.585167        |
+------+-----------------+-----------------+-----------------+

Phase Timing Summary
====================

Wall-clock times include warm-up work and GC performed by the warm-up helper.

+----------+--------------+--------------+--------------+
| Phase    | Warm-up      | Timed pass   | Mode total   |
+==========+==============+==============+==============+
| Mode A   | 00:00:12.148 | 00:00:59.110 | 00:01:11.259 |
+----------+--------------+--------------+--------------+
| Mode B   | 00:00:14.899 | 00:01:07.619 | 00:01:22.519 |
+----------+--------------+--------------+--------------+
| Mode C   | 00:00:36.981 | 00:02:33.298 | 00:03:10.280 |
+----------+--------------+--------------+--------------+
| Mode D   | 00:01:00.829 | 00:04:30.094 | 00:05:30.924 |
+----------+--------------+--------------+--------------+
| Total    |              |              | 00:11:14.989 |
+----------+--------------+--------------+--------------+


.. ---------------------------------------------------------------------------
