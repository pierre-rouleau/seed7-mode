=======================================================
GC-Controlled Benchmark Report: Seed7 Mode — Four Modes
=======================================================

:Running with: seed7-mode 2026-06-25T13:29:52+0000 W26-4
:Seed7 dir   : /Users/roup/my/dvo/seed7-repos/seed7/
:Started at: 09:36:48 local time
:Generated on: 2026-06-25 13:48:20 UTC
:N Iterations: 5  (mean of N timed opens per file)
:Window system: nil
:Display graphic: nil
:Frame size: 248x95 chars
:Window body: 248x93 chars
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
seed7/prg/addup.sd7          0.039502            190
seed7/prg/bas7.sd7           0.034090          11459
seed7/prg/bifurk.sd7         0.033128             73
seed7/prg/bigfiles.sd7       0.033384            129
seed7/prg/brainf7.sd7        0.033941             86
seed7/prg/calc7.sd7          0.034271            128
seed7/prg/carddemo.sd7       0.033795            190
seed7/prg/castle.sd7         0.033282           3148
seed7/prg/cat.sd7            0.032910             82
seed7/prg/cellauto.sd7       0.033164             85
seed7/prg/celsius.sd7        0.033267             42
seed7/prg/chk_all.sd7        0.035560            843
seed7/prg/chkarr.sd7         0.038474           8367
seed7/prg/chkbig.sd7         0.038647          29026
seed7/prg/chkbin.sd7         0.034350           6469
seed7/prg/chkbitdata.sd7     0.034336           6624
seed7/prg/chkbool.sd7        0.034029           3157
seed7/prg/chkbst.sd7         0.034801            722
seed7/prg/chkchr.sd7         0.037157           2809
seed7/prg/chkcmd.sd7         0.038700           1205
seed7/prg/chkdb.sd7          0.036729           7454
seed7/prg/chkdecl.sd7        0.033276            448
seed7/prg/chkenum.sd7        0.036382           1230
seed7/prg/chkerr.sd7         0.036687           4663
seed7/prg/chkexc.sd7         0.033909           2627
seed7/prg/chkfil.sd7         0.034727           1615
seed7/prg/chkflt.sd7         0.037391          20620
seed7/prg/chkhent.sd7        0.033572             54
seed7/prg/chkhsh.sd7         0.034863           4548
seed7/prg/chkidx.sd7         0.036403          19567
seed7/prg/chkint.sd7         0.040727          38129
seed7/prg/chkjson.sd7        0.035342           1764
seed7/prg/chkovf.sd7         0.034794           8216
seed7/prg/chkprc.sd7         0.034039          10111
seed7/prg/chkscan.sd7        0.034359            714
seed7/prg/chkset.sd7         0.034342          11974
seed7/prg/chkstr.sd7         0.037822          26952
seed7/prg/chktime.sd7        0.033121           2025
seed7/prg/chktoml.sd7        0.034768           1656
seed7/prg/clock.sd7          0.032549             47
seed7/prg/clock2.sd7         0.033752             43
seed7/prg/clock3.sd7         0.038107             95
seed7/prg/cmpfil.sd7         0.034683             84
seed7/prg/comanche.sd7       0.037380            180
seed7/prg/confval.sd7        0.035715            175
seed7/prg/db7.sd7            0.034162            417
seed7/prg/diff7.sd7          0.034531            263
seed7/prg/dirtst.sd7         0.035322             42
seed7/prg/dirx.sd7           0.034771            152
seed7/prg/dnafight.sd7       0.034023           1381
seed7/prg/dragon.sd7         0.033367             73
seed7/prg/echo.sd7           0.032948             39
seed7/prg/eliza.sd7          0.034543            302
seed7/prg/err.sd7            0.036719             96
seed7/prg/fannkuch.sd7       0.034083            131
seed7/prg/fib.sd7            0.035199             47
seed7/prg/find7.sd7          0.033440            133
seed7/prg/findchar.sd7       0.033276            149
seed7/prg/fractree.sd7       0.034043             55
seed7/prg/ftp7.sd7           0.035355            296
seed7/prg/ftpserv.sd7        0.037214             74
seed7/prg/gcd.sd7            0.034683            109
seed7/prg/gkbd.sd7           0.033494            358
seed7/prg/gtksvtst.sd7       0.033532             94
seed7/prg/hal.sd7            0.034577            250
seed7/prg/hamu.sd7           0.035478            573
seed7/prg/hanoi.sd7          0.035953             55
seed7/prg/hd.sd7             0.036545             79
seed7/prg/hello.sd7          0.037143             32
seed7/prg/hilbert.sd7        0.033262            108
seed7/prg/ide7.sd7           0.033550            196
seed7/prg/kbd.sd7            0.034432             49
seed7/prg/klondike.sd7       0.034791            883
seed7/prg/lander.sd7         0.035691           1551
seed7/prg/lst80bas.sd7       0.033486            344
seed7/prg/lst99bas.sd7       0.033254            401
seed7/prg/lstgwbas.sd7       0.033983            577
seed7/prg/mahjong.sd7        0.033417           1943
seed7/prg/make7.sd7          0.034978            121
seed7/prg/mandelbr.sd7       0.034847            237
seed7/prg/mind.sd7           0.033831            443
seed7/prg/mirror.sd7         0.032452            131
seed7/prg/ms.sd7             0.033372            641
seed7/prg/nicoma.sd7         0.033136            135
seed7/prg/pac.sd7            0.035726            726
seed7/prg/pairs.sd7          0.038198           2025
seed7/prg/panic.sd7          0.037392           2634
seed7/prg/percolation.sd7    0.035965            330
seed7/prg/planets.sd7        0.034714           1486
seed7/prg/portfwd7.sd7       0.036310            139
seed7/prg/prime.sd7          0.039715             74
seed7/prg/printpi1.sd7       0.037656             56
seed7/prg/printpi2.sd7       0.039316             54
seed7/prg/printpi3.sd7       0.036005             60
seed7/prg/pv7.sd7            0.036185            337
seed7/prg/queen.sd7          0.036881            149
seed7/prg/rand.sd7           0.033219            121
seed7/prg/raytrace.sd7       0.032782            538
seed7/prg/rever.sd7          0.033707            816
seed7/prg/roman.sd7          0.032473             38
seed7/prg/s7c.sd7            0.035781           9060
seed7/prg/s7check.sd7        0.036863             68
seed7/prg/savehd7.sd7        0.040604           1110
seed7/prg/self.sd7           0.040501             49
seed7/prg/shisen.sd7         0.038389           1423
seed7/prg/sl.sd7             0.033490           1029
seed7/prg/snake.sd7          0.034678            615
seed7/prg/sokoban.sd7        0.035058            891
seed7/prg/spigotpi.sd7       0.033515             64
seed7/prg/sql7.sd7           0.034473            278
seed7/prg/startrek.sd7       0.038141            979
seed7/prg/sudoku7.sd7        0.034722           2657
seed7/prg/sydir7.sd7         0.035073            384
seed7/prg/syntaxhl.sd7       0.038785            177
seed7/prg/tak.sd7            0.040598             59
seed7/prg/tar7.sd7           0.037187            121
seed7/prg/tch.sd7            0.034245             55
seed7/prg/testfont.sd7       0.034203             95
seed7/prg/tet.sd7            0.039590            479
seed7/prg/tetg.sd7           0.036673            501
seed7/prg/toutf8.sd7         0.034873            240
seed7/prg/tst_cli.sd7        0.036335             40
seed7/prg/tst_srv.sd7        0.037851             47
seed7/prg/wator.sd7          0.035158            651
seed7/prg/which.sd7          0.033995             65
seed7/prg/wiz.sd7            0.035315           2833
seed7/prg/wordcnt.sd7        0.033876             54
seed7/prg/wrinum.sd7         0.034606             43
seed7/prg/wumpus.sd7         0.033575            372
seed7/lib/aes.s7i            0.036657           1144
seed7/lib/aes_gcm.s7i        0.039171            392
seed7/lib/ar.s7i             0.036225           1532
seed7/lib/arc4.s7i           0.035712            144
seed7/lib/archive.s7i        0.035360            143
seed7/lib/archive_base.s7i   0.038611            135
seed7/lib/array.s7i          0.041604            610
seed7/lib/asn1.s7i           0.038961            544
seed7/lib/asn1oid.s7i        0.038026            157
seed7/lib/basearray.s7i      0.036056            450
seed7/lib/bigfile.s7i        0.035059            136
seed7/lib/bigint.s7i         0.033365            824
seed7/lib/bigrat.s7i         0.034644            784
seed7/lib/bin16.s7i          0.034899            592
seed7/lib/bin32.s7i          0.034871            490
seed7/lib/bin64.s7i          0.034757            539
seed7/lib/bitdata.s7i        0.035411           1330
seed7/lib/bitmapfont.s7i     0.034286            215
seed7/lib/bitset.s7i         0.035558            593
seed7/lib/bitsetof.s7i       0.036602            431
seed7/lib/blowfish.s7i       0.040823            383
seed7/lib/bmp.s7i            0.039482            924
seed7/lib/boolean.s7i        0.040652            403
seed7/lib/browser.s7i        0.039837            280
seed7/lib/bstring.s7i        0.035824            227
seed7/lib/bytedata.s7i       0.036747            482
seed7/lib/bzip2.s7i          0.035497            887
seed7/lib/cards.s7i          0.035853           1342
seed7/lib/category.s7i       0.037299            209
seed7/lib/cc_conf.s7i        0.039620           1314
seed7/lib/ccittfax.s7i       0.035186           1022
seed7/lib/cgi.s7i            0.033075            109
seed7/lib/cgidialog.s7i      0.034339           1118
seed7/lib/char.s7i           0.034591            356
seed7/lib/charsets.s7i       0.034398           2024
seed7/lib/chartype.s7i       0.034143            121
seed7/lib/cipher.s7i         0.033873            146
seed7/lib/cli_cmds.s7i       0.033747           1360
seed7/lib/clib_file.s7i      0.034415            301
seed7/lib/color.s7i          0.033542            185
seed7/lib/complex.s7i        0.033358            464
seed7/lib/compress.s7i       0.032948            150
seed7/lib/console.s7i        0.032825            188
seed7/lib/cpio.s7i           0.033788           1708
seed7/lib/crc32.s7i          0.033409            193
seed7/lib/cronos16.s7i       0.037402           1173
seed7/lib/cronos27.s7i       0.037499           1464
seed7/lib/csv.s7i            0.032783            201
seed7/lib/db_prop.s7i        0.034204            991
seed7/lib/deflate.s7i        0.036371            740
seed7/lib/des.s7i            0.035742            444
seed7/lib/dialog.s7i         0.035220            311
seed7/lib/dir.s7i            0.033857            163
seed7/lib/draw.s7i           0.033563            854
seed7/lib/duration.s7i       0.037681           1038
seed7/lib/echo.s7i           0.034555            132
seed7/lib/editline.s7i       0.033267            398
seed7/lib/elf.s7i            0.033337           1560
seed7/lib/elliptic.s7i       0.035633            649
seed7/lib/enable_io.s7i      0.035766            312
seed7/lib/encoding.s7i       0.036113            931
seed7/lib/enumeration.s7i    0.036656            236
seed7/lib/environment.s7i    0.037527            175
seed7/lib/exif.s7i           0.039526            152
seed7/lib/external_file.s7i  0.036855            340
seed7/lib/field.s7i          0.034872            268
seed7/lib/file.s7i           0.035089            372
seed7/lib/filebits.s7i       0.037677             46
seed7/lib/filesys.s7i        0.035720            601
seed7/lib/fileutil.s7i       0.038927            144
seed7/lib/fixarray.s7i       0.037986            307
seed7/lib/float.s7i          0.034288            757
seed7/lib/font.s7i           0.034981            196
seed7/lib/font8x8.s7i        0.034550            998
seed7/lib/forloop.s7i        0.033878            449
seed7/lib/ftp.s7i            0.035317            969
seed7/lib/ftpserv.s7i        0.034188            631
seed7/lib/getf.s7i           0.033605            115
seed7/lib/gethttp.s7i        0.034530             41
seed7/lib/gethttps.s7i       0.033440             41
seed7/lib/gif.s7i            0.033693            561
seed7/lib/graph.s7i          0.035063            415
seed7/lib/graph_file.s7i     0.034433            399
seed7/lib/gtkserver.s7i      0.033786            161
seed7/lib/gzip.s7i           0.034092            573
seed7/lib/hash.s7i           0.036406            421
seed7/lib/hashsetof.s7i      0.033626            499
seed7/lib/hmac.s7i           0.032834            152
seed7/lib/html.s7i           0.036663             83
seed7/lib/html_ent.s7i       0.032796            476
seed7/lib/htmldom.s7i        0.034075            286
seed7/lib/http_request.s7i   0.033536            696
seed7/lib/http_srv_resp.s7i  0.035446            380
seed7/lib/https_request.s7i  0.034571            211
seed7/lib/httpserv.s7i       0.036323            345
seed7/lib/huffman.s7i        0.038358            644
seed7/lib/ico.s7i            0.033992            221
seed7/lib/idxarray.s7i       0.034218            232
seed7/lib/image.s7i          0.033107            156
seed7/lib/imagefile.s7i      0.033302            171
seed7/lib/inflate.s7i        0.034615            411
seed7/lib/inifile.s7i        0.033449            129
seed7/lib/integer.s7i        0.033423            663
seed7/lib/iobuffer.s7i       0.034655            289
seed7/lib/jpeg.s7i           0.033376           1761
seed7/lib/json.s7i           0.033421            891
seed7/lib/json_serde.s7i     0.032908            783
seed7/lib/keybd.s7i          0.037708            639
seed7/lib/keydescr.s7i       0.034497            192
seed7/lib/leb128.s7i         0.035022            218
seed7/lib/line.s7i           0.033586            164
seed7/lib/listener.s7i       0.033160            247
seed7/lib/logfile.s7i        0.033793             73
seed7/lib/lower.s7i          0.033424            142
seed7/lib/lzma.s7i           0.033425            934
seed7/lib/lzw.s7i            0.033292            861
seed7/lib/magic.s7i          0.033798            403
seed7/lib/mahjng32.s7i       0.033579           1500
seed7/lib/make.s7i           0.032247            544
seed7/lib/makedata.s7i       0.032634           1428
seed7/lib/math.s7i           0.032347            201
seed7/lib/mixarith.s7i       0.035065            249
seed7/lib/modern27.s7i       0.036316           1099
seed7/lib/more.s7i           0.034593            130
seed7/lib/msgdigest.s7i      0.033134           1222
seed7/lib/multiscr.s7i       0.033017             68
seed7/lib/null_file.s7i      0.032479            345
seed7/lib/osfiles.s7i        0.032889           1085
seed7/lib/pbm.s7i            0.032237            230
seed7/lib/pcx.s7i            0.032246            638
seed7/lib/pem.s7i            0.032720            185
seed7/lib/pgm.s7i            0.032642            238
seed7/lib/pic16.s7i          0.032097           1037
seed7/lib/pic32.s7i          0.032847           2060
seed7/lib/pic_util.s7i       0.033395            144
seed7/lib/pixelimage.s7i     0.033528            320
seed7/lib/pixmap_file.s7i    0.033414            459
seed7/lib/pixmapfont.s7i     0.033211            184
seed7/lib/pkcs1.s7i          0.036427            543
seed7/lib/png.s7i            0.035957           1064
seed7/lib/poll.s7i           0.033643            313
seed7/lib/ppm.s7i            0.033825            240
seed7/lib/process.s7i        0.034221            541
seed7/lib/progs.s7i          0.033782            789
seed7/lib/propertyfile.s7i   0.033337            155
seed7/lib/rational.s7i       0.040035            792
seed7/lib/ref_list.s7i       0.034818            252
seed7/lib/reference.s7i      0.034080            126
seed7/lib/reverse.s7i        0.034710             94
seed7/lib/rpm.s7i            0.033406           3487
seed7/lib/rpmext.s7i         0.032931            318
seed7/lib/scanfile.s7i       0.037141           1779
seed7/lib/scanjson.s7i       0.036825            413
seed7/lib/scanstri.s7i       0.032733           1814
seed7/lib/scantoml.s7i       0.032659           1603
seed7/lib/seed7_05.s7i       0.035990           1072
seed7/lib/set.s7i            0.033875             57
seed7/lib/shell.s7i          0.034857            615
seed7/lib/showtls.s7i        0.034695            678
seed7/lib/signature.s7i      0.033230            131
seed7/lib/smtp.s7i           0.033442            261
seed7/lib/sockbase.s7i       0.033790            217
seed7/lib/socket.s7i         0.038929            326
seed7/lib/sokoban1.s7i       0.035586           1519
seed7/lib/sql_base.s7i       0.034009           1000
seed7/lib/stars.s7i          0.033422           1705
seed7/lib/stdfont10.s7i      0.033555           3347
seed7/lib/stdfont12.s7i      0.033520           3928
seed7/lib/stdfont14.s7i      0.033367           4510
seed7/lib/stdfont16.s7i      0.033520           5092
seed7/lib/stdfont18.s7i      0.034454           5868
seed7/lib/stdfont20.s7i      0.033690           6449
seed7/lib/stdfont24.s7i      0.033536           7421
seed7/lib/stdfont8.s7i       0.033469           2960
seed7/lib/stdfont9.s7i       0.035309           3152
seed7/lib/stdio.s7i          0.033943            192
seed7/lib/strifile.s7i       0.032628            345
seed7/lib/string.s7i         0.032609            779
seed7/lib/stritext.s7i       0.032859            352
seed7/lib/struct.s7i         0.032378            266
seed7/lib/struct_elem.s7i    0.032512            129
seed7/lib/subfile.s7i        0.032578            174
seed7/lib/subrange.s7i       0.032708             78
seed7/lib/syntax.s7i         0.036036            294
seed7/lib/tar.s7i            0.035596           1880
seed7/lib/tar_cmds.s7i       0.041448            752
seed7/lib/tdes.s7i           0.034242            143
seed7/lib/tee.s7i            0.034232            143
seed7/lib/text.s7i           0.033173            135
seed7/lib/tga.s7i            0.033282            676
seed7/lib/tiff.s7i           0.034150           2771
seed7/lib/time.s7i           0.034586           1191
seed7/lib/tls.s7i            0.036196           2230
seed7/lib/unicode.s7i        0.037269            575
seed7/lib/unionfnd.s7i       0.033829            130
seed7/lib/upper.s7i          0.033295            142
seed7/lib/utf16.s7i          0.033482            540
seed7/lib/utf8.s7i           0.033279            234
seed7/lib/vecfont10.s7i      0.033741           1056
seed7/lib/vecfont18.s7i      0.035318           1119
seed7/lib/vector3d.s7i       0.036811            293
seed7/lib/vectorfont.s7i     0.033774            239
seed7/lib/wildcard.s7i       0.033334            140
seed7/lib/window.s7i         0.035023            455
seed7/lib/wrinum.s7i         0.042321            248
seed7/lib/x509cert.s7i       0.038682           1243
seed7/lib/xml_ent.s7i        0.036565             94
seed7/lib/xmldom.s7i         0.037456            303
seed7/lib/xz.s7i             0.039454            442
seed7/lib/zip.s7i            0.034503           2792
seed7/lib/zstd.s7i           0.033913           1263
============================ ================= ================

Statistical Summary — Mode A (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.035029        |
+-----------+-----------------+
| Minimum   | 0.032097        |
+-----------+-----------------+
| Maximum   | 0.042321        |
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
seed7/prg/addup.sd7          0.039238            190
seed7/prg/bas7.sd7           0.041240          11459
seed7/prg/bifurk.sd7         0.035841             73
seed7/prg/bigfiles.sd7       0.037881            129
seed7/prg/brainf7.sd7        0.036853             86
seed7/prg/calc7.sd7          0.037161            128
seed7/prg/carddemo.sd7       0.037498            190
seed7/prg/castle.sd7         0.039290           3148
seed7/prg/cat.sd7            0.037702             82
seed7/prg/cellauto.sd7       0.037279             85
seed7/prg/celsius.sd7        0.035445             42
seed7/prg/chk_all.sd7        0.038343            843
seed7/prg/chkarr.sd7         0.038336           8367
seed7/prg/chkbig.sd7         0.042697          29026
seed7/prg/chkbin.sd7         0.042612           6469
seed7/prg/chkbitdata.sd7     0.041606           6624
seed7/prg/chkbool.sd7        0.038871           3157
seed7/prg/chkbst.sd7         0.038304            722
seed7/prg/chkchr.sd7         0.039726           2809
seed7/prg/chkcmd.sd7         0.040140           1205
seed7/prg/chkdb.sd7          0.040316           7454
seed7/prg/chkdecl.sd7        0.038235            448
seed7/prg/chkenum.sd7        0.038112           1230
seed7/prg/chkerr.sd7         0.038454           4663
seed7/prg/chkexc.sd7         0.037385           2627
seed7/prg/chkfil.sd7         0.037336           1615
seed7/prg/chkflt.sd7         0.042182          20620
seed7/prg/chkhent.sd7        0.035266             54
seed7/prg/chkhsh.sd7         0.039211           4548
seed7/prg/chkidx.sd7         0.041546          19567
seed7/prg/chkint.sd7         0.044669          38129
seed7/prg/chkjson.sd7        0.037966           1764
seed7/prg/chkovf.sd7         0.038991           8216
seed7/prg/chkprc.sd7         0.039176          10111
seed7/prg/chkscan.sd7        0.039096            714
seed7/prg/chkset.sd7         0.041103          11974
seed7/prg/chkstr.sd7         0.044079          26952
seed7/prg/chktime.sd7        0.039622           2025
seed7/prg/chktoml.sd7        0.039119           1656
seed7/prg/clock.sd7          0.036079             47
seed7/prg/clock2.sd7         0.036438             43
seed7/prg/clock3.sd7         0.040506             95
seed7/prg/cmpfil.sd7         0.037439             84
seed7/prg/comanche.sd7       0.038907            180
seed7/prg/confval.sd7        0.040090            175
seed7/prg/db7.sd7            0.039143            417
seed7/prg/diff7.sd7          0.039365            263
seed7/prg/dirtst.sd7         0.036302             42
seed7/prg/dirx.sd7           0.038863            152
seed7/prg/dnafight.sd7       0.039322           1381
seed7/prg/dragon.sd7         0.036691             73
seed7/prg/echo.sd7           0.035909             39
seed7/prg/eliza.sd7          0.037485            302
seed7/prg/err.sd7            0.040592             96
seed7/prg/fannkuch.sd7       0.037354            131
seed7/prg/fib.sd7            0.035110             47
seed7/prg/find7.sd7          0.037352            133
seed7/prg/findchar.sd7       0.037820            149
seed7/prg/fractree.sd7       0.036188             55
seed7/prg/ftp7.sd7           0.041586            296
seed7/prg/ftpserv.sd7        0.040233             74
seed7/prg/gcd.sd7            0.037981            109
seed7/prg/gkbd.sd7           0.040660            358
seed7/prg/gtksvtst.sd7       0.037926             94
seed7/prg/hal.sd7            0.038428            250
seed7/prg/hamu.sd7           0.038407            573
seed7/prg/hanoi.sd7          0.036616             55
seed7/prg/hd.sd7             0.038328             79
seed7/prg/hello.sd7          0.035991             32
seed7/prg/hilbert.sd7        0.039860            108
seed7/prg/ide7.sd7           0.041545            196
seed7/prg/kbd.sd7            0.036547             49
seed7/prg/klondike.sd7       0.038978            883
seed7/prg/lander.sd7         0.039298           1551
seed7/prg/lst80bas.sd7       0.038176            344
seed7/prg/lst99bas.sd7       0.039722            401
seed7/prg/lstgwbas.sd7       0.040098            577
seed7/prg/mahjong.sd7        0.039092           1943
seed7/prg/make7.sd7          0.038989            121
seed7/prg/mandelbr.sd7       0.038867            237
seed7/prg/mind.sd7           0.039114            443
seed7/prg/mirror.sd7         0.039066            131
seed7/prg/ms.sd7             0.037457            641
seed7/prg/nicoma.sd7         0.038009            135
seed7/prg/pac.sd7            0.039766            726
seed7/prg/pairs.sd7          0.037280           2025
seed7/prg/panic.sd7          0.041854           2634
seed7/prg/percolation.sd7    0.041415            330
seed7/prg/planets.sd7        0.039579           1486
seed7/prg/portfwd7.sd7       0.038909            139
seed7/prg/prime.sd7          0.037240             74
seed7/prg/printpi1.sd7       0.036717             56
seed7/prg/printpi2.sd7       0.036524             54
seed7/prg/printpi3.sd7       0.037003             60
seed7/prg/pv7.sd7            0.037821            337
seed7/prg/queen.sd7          0.037928            149
seed7/prg/rand.sd7           0.036881            121
seed7/prg/raytrace.sd7       0.037965            538
seed7/prg/rever.sd7          0.037528            816
seed7/prg/roman.sd7          0.035639             38
seed7/prg/s7c.sd7            0.039583           9060
seed7/prg/s7check.sd7        0.035732             68
seed7/prg/savehd7.sd7        0.038924           1110
seed7/prg/self.sd7           0.036490             49
seed7/prg/shisen.sd7         0.038773           1423
seed7/prg/sl.sd7             0.045018           1029
seed7/prg/snake.sd7          0.041331            615
seed7/prg/sokoban.sd7        0.039373            891
seed7/prg/spigotpi.sd7       0.040064             64
seed7/prg/sql7.sd7           0.037754            278
seed7/prg/startrek.sd7       0.037867            979
seed7/prg/sudoku7.sd7        0.037965           2657
seed7/prg/sydir7.sd7         0.038073            384
seed7/prg/syntaxhl.sd7       0.040418            177
seed7/prg/tak.sd7            0.037666             59
seed7/prg/tar7.sd7           0.039511            121
seed7/prg/tch.sd7            0.036633             55
seed7/prg/testfont.sd7       0.038765             95
seed7/prg/tet.sd7            0.039978            479
seed7/prg/tetg.sd7           0.039565            501
seed7/prg/toutf8.sd7         0.044931            240
seed7/prg/tst_cli.sd7        0.038351             40
seed7/prg/tst_srv.sd7        0.036208             47
seed7/prg/wator.sd7          0.039020            651
seed7/prg/which.sd7          0.036864             65
seed7/prg/wiz.sd7            0.039310           2833
seed7/prg/wordcnt.sd7        0.036920             54
seed7/prg/wrinum.sd7         0.035413             43
seed7/prg/wumpus.sd7         0.039291            372
seed7/lib/aes.s7i            0.043335           1144
seed7/lib/aes_gcm.s7i        0.039476            392
seed7/lib/ar.s7i             0.039522           1532
seed7/lib/arc4.s7i           0.039066            144
seed7/lib/archive.s7i        0.038380            143
seed7/lib/archive_base.s7i   0.037840            135
seed7/lib/array.s7i          0.038460            610
seed7/lib/asn1.s7i           0.036506            544
seed7/lib/asn1oid.s7i        0.041249            157
seed7/lib/basearray.s7i      0.038562            450
seed7/lib/bigfile.s7i        0.038140            136
seed7/lib/bigint.s7i         0.038001            824
seed7/lib/bigrat.s7i         0.040258            784
seed7/lib/bin16.s7i          0.038722            592
seed7/lib/bin32.s7i          0.043846            490
seed7/lib/bin64.s7i          0.040413            539
seed7/lib/bitdata.s7i        0.045253           1330
seed7/lib/bitmapfont.s7i     0.039206            215
seed7/lib/bitset.s7i         0.040480            593
seed7/lib/bitsetof.s7i       0.041331            431
seed7/lib/blowfish.s7i       0.042452            383
seed7/lib/bmp.s7i            0.039557            924
seed7/lib/boolean.s7i        0.038971            403
seed7/lib/browser.s7i        0.044330            280
seed7/lib/bstring.s7i        0.044044            227
seed7/lib/bytedata.s7i       0.039564            482
seed7/lib/bzip2.s7i          0.039122            887
seed7/lib/cards.s7i          0.037853           1342
seed7/lib/category.s7i       0.039249            209
seed7/lib/cc_conf.s7i        0.038642           1314
seed7/lib/ccittfax.s7i       0.039688           1022
seed7/lib/cgi.s7i            0.038577            109
seed7/lib/cgidialog.s7i      0.038189           1118
seed7/lib/char.s7i           0.037708            356
seed7/lib/charsets.s7i       0.038585           2024
seed7/lib/chartype.s7i       0.040690            121
seed7/lib/cipher.s7i         0.037189            146
seed7/lib/cli_cmds.s7i       0.038024           1360
seed7/lib/clib_file.s7i      0.040651            301
seed7/lib/color.s7i          0.039146            185
seed7/lib/complex.s7i        0.038927            464
seed7/lib/compress.s7i       0.039488            150
seed7/lib/console.s7i        0.038803            188
seed7/lib/cpio.s7i           0.040275           1708
seed7/lib/crc32.s7i          0.041150            193
seed7/lib/cronos16.s7i       0.042230           1173
seed7/lib/cronos27.s7i       0.043992           1464
seed7/lib/csv.s7i            0.037917            201
seed7/lib/db_prop.s7i        0.037682            991
seed7/lib/deflate.s7i        0.037748            740
seed7/lib/des.s7i            0.038537            444
seed7/lib/dialog.s7i         0.038479            311
seed7/lib/dir.s7i            0.037774            163
seed7/lib/draw.s7i           0.038337            854
seed7/lib/duration.s7i       0.039506           1038
seed7/lib/echo.s7i           0.039104            132
seed7/lib/editline.s7i       0.039022            398
seed7/lib/elf.s7i            0.040886           1560
seed7/lib/elliptic.s7i       0.037537            649
seed7/lib/enable_io.s7i      0.037550            312
seed7/lib/encoding.s7i       0.038267            931
seed7/lib/enumeration.s7i    0.037720            236
seed7/lib/environment.s7i    0.037441            175
seed7/lib/exif.s7i           0.040806            152
seed7/lib/external_file.s7i  0.038459            340
seed7/lib/field.s7i          0.039260            268
seed7/lib/file.s7i           0.039112            372
seed7/lib/filebits.s7i       0.036822             46
seed7/lib/filesys.s7i        0.038616            601
seed7/lib/fileutil.s7i       0.038944            144
seed7/lib/fixarray.s7i       0.040699            307
seed7/lib/float.s7i          0.039084            757
seed7/lib/font.s7i           0.039116            196
seed7/lib/font8x8.s7i        0.038562            998
seed7/lib/forloop.s7i        0.040823            449
seed7/lib/ftp.s7i            0.039221            969
seed7/lib/ftpserv.s7i        0.039257            631
seed7/lib/getf.s7i           0.038794            115
seed7/lib/gethttp.s7i        0.036077             41
seed7/lib/gethttps.s7i       0.036341             41
seed7/lib/gif.s7i            0.038798            561
seed7/lib/graph.s7i          0.040819            415
seed7/lib/graph_file.s7i     0.038990            399
seed7/lib/gtkserver.s7i      0.039788            161
seed7/lib/gzip.s7i           0.038540            573
seed7/lib/hash.s7i           0.039198            421
seed7/lib/hashsetof.s7i      0.039349            499
seed7/lib/hmac.s7i           0.037728            152
seed7/lib/html.s7i           0.036738             83
seed7/lib/html_ent.s7i       0.037715            476
seed7/lib/htmldom.s7i        0.038219            286
seed7/lib/http_request.s7i   0.039273            696
seed7/lib/http_srv_resp.s7i  0.038628            380
seed7/lib/https_request.s7i  0.038700            211
seed7/lib/httpserv.s7i       0.038800            345
seed7/lib/huffman.s7i        0.039563            644
seed7/lib/ico.s7i            0.042899            221
seed7/lib/idxarray.s7i       0.040206            232
seed7/lib/image.s7i          0.038998            156
seed7/lib/imagefile.s7i      0.038903            171
seed7/lib/inflate.s7i        0.039552            411
seed7/lib/inifile.s7i        0.038977            129
seed7/lib/integer.s7i        0.038955            663
seed7/lib/iobuffer.s7i       0.038785            289
seed7/lib/jpeg.s7i           0.040820           1761
seed7/lib/json.s7i           0.038640            891
seed7/lib/json_serde.s7i     0.038950            783
seed7/lib/keybd.s7i          0.038591            639
seed7/lib/keydescr.s7i       0.040435            192
seed7/lib/leb128.s7i         0.039462            218
seed7/lib/line.s7i           0.039042            164
seed7/lib/listener.s7i       0.039667            247
seed7/lib/logfile.s7i        0.037316             73
seed7/lib/lower.s7i          0.037557            142
seed7/lib/lzma.s7i           0.039232            934
seed7/lib/lzw.s7i            0.043782            861
seed7/lib/magic.s7i          0.040864            403
seed7/lib/mahjng32.s7i       0.038578           1500
seed7/lib/make.s7i           0.040338            544
seed7/lib/makedata.s7i       0.039894           1428
seed7/lib/math.s7i           0.039035            201
seed7/lib/mixarith.s7i       0.039106            249
seed7/lib/modern27.s7i       0.042561           1099
seed7/lib/more.s7i           0.039162            130
seed7/lib/msgdigest.s7i      0.039701           1222
seed7/lib/multiscr.s7i       0.037156             68
seed7/lib/null_file.s7i      0.037518            345
seed7/lib/osfiles.s7i        0.039489           1085
seed7/lib/pbm.s7i            0.037959            230
seed7/lib/pcx.s7i            0.038525            638
seed7/lib/pem.s7i            0.037437            185
seed7/lib/pgm.s7i            0.037898            238
seed7/lib/pic16.s7i          0.038322           1037
seed7/lib/pic32.s7i          0.036914           2060
seed7/lib/pic_util.s7i       0.037502            144
seed7/lib/pixelimage.s7i     0.039050            320
seed7/lib/pixmap_file.s7i    0.039109            459
seed7/lib/pixmapfont.s7i     0.040620            184
seed7/lib/pkcs1.s7i          0.044470            543
seed7/lib/png.s7i            0.039237           1064
seed7/lib/poll.s7i           0.037415            313
seed7/lib/ppm.s7i            0.037716            240
seed7/lib/process.s7i        0.037536            541
seed7/lib/progs.s7i          0.037957            789
seed7/lib/propertyfile.s7i   0.037663            155
seed7/lib/rational.s7i       0.039120            792
seed7/lib/ref_list.s7i       0.039017            252
seed7/lib/reference.s7i      0.038751            126
seed7/lib/reverse.s7i        0.038734             94
seed7/lib/rpm.s7i            0.039911           3487
seed7/lib/rpmext.s7i         0.038875            318
seed7/lib/scanfile.s7i       0.038238           1779
seed7/lib/scanjson.s7i       0.039557            413
seed7/lib/scanstri.s7i       0.039589           1814
seed7/lib/scantoml.s7i       0.039243           1603
seed7/lib/seed7_05.s7i       0.041010           1072
seed7/lib/set.s7i            0.037116             57
seed7/lib/shell.s7i          0.039637            615
seed7/lib/showtls.s7i        0.039157            678
seed7/lib/signature.s7i      0.039221            131
seed7/lib/smtp.s7i           0.038988            261
seed7/lib/sockbase.s7i       0.039987            217
seed7/lib/socket.s7i         0.038892            326
seed7/lib/sokoban1.s7i       0.038546           1519
seed7/lib/sql_base.s7i       0.039077           1000
seed7/lib/stars.s7i          0.041054           1705
seed7/lib/stdfont10.s7i      0.037733           3347
seed7/lib/stdfont12.s7i      0.037898           3928
seed7/lib/stdfont14.s7i      0.038268           4510
seed7/lib/stdfont16.s7i      0.038063           5092
seed7/lib/stdfont18.s7i      0.038007           5868
seed7/lib/stdfont20.s7i      0.037693           6449
seed7/lib/stdfont24.s7i      0.040017           7421
seed7/lib/stdfont8.s7i       0.038587           2960
seed7/lib/stdfont9.s7i       0.036355           3152
seed7/lib/stdio.s7i          0.044751            192
seed7/lib/strifile.s7i       0.050422            345
seed7/lib/string.s7i         0.044135            779
seed7/lib/stritext.s7i       0.039758            352
seed7/lib/struct.s7i         0.040026            266
seed7/lib/struct_elem.s7i    0.037275            129
seed7/lib/subfile.s7i        0.038228            174
seed7/lib/subrange.s7i       0.043224             78
seed7/lib/syntax.s7i         0.039235            294
seed7/lib/tar.s7i            0.040010           1880
seed7/lib/tar_cmds.s7i       0.039387            752
seed7/lib/tdes.s7i           0.038739            143
seed7/lib/tee.s7i            0.039046            143
seed7/lib/text.s7i           0.038563            135
seed7/lib/tga.s7i            0.040930            676
seed7/lib/tiff.s7i           0.040928           2771
seed7/lib/time.s7i           0.039167           1191
seed7/lib/tls.s7i            0.039080           2230
seed7/lib/unicode.s7i        0.041117            575
seed7/lib/unionfnd.s7i       0.037658            130
seed7/lib/upper.s7i          0.037310            142
seed7/lib/utf16.s7i          0.037289            540
seed7/lib/utf8.s7i           0.038772            234
seed7/lib/vecfont10.s7i      0.040413           1056
seed7/lib/vecfont18.s7i      0.042463           1119
seed7/lib/vector3d.s7i       0.039163            293
seed7/lib/vectorfont.s7i     0.040062            239
seed7/lib/wildcard.s7i       0.038903            140
seed7/lib/window.s7i         0.039048            455
seed7/lib/wrinum.s7i         0.039167            248
seed7/lib/x509cert.s7i       0.039242           1243
seed7/lib/xml_ent.s7i        0.038984             94
seed7/lib/xmldom.s7i         0.040145            303
seed7/lib/xz.s7i             0.037974            442
seed7/lib/zip.s7i            0.038127           2792
seed7/lib/zstd.s7i           0.037445           1263
============================ ================= ================

Statistical Summary — Mode B (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.039068        |
+-----------+-----------------+
| Minimum   | 0.035110        |
+-----------+-----------------+
| Maximum   | 0.050422        |
+-----------+-----------------+

Mode C — Mode Activation + Full-Buffer Fontification (font-lock-ensure)
-----------------------------------------------------------------------

Full-buffer fontification stress test.

File Load Times — Mode C (mean, GC-free)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

============================ ================= ================
File Name                    Mean Time (s)     Line count
============================ ================= ================
seed7/prg/addup.sd7          0.043563            190
seed7/prg/bas7.sd7           0.335241          11459
seed7/prg/bifurk.sd7         0.039558             73
seed7/prg/bigfiles.sd7       0.042088            129
seed7/prg/brainf7.sd7        0.039859             86
seed7/prg/calc7.sd7          0.041075            128
seed7/prg/carddemo.sd7       0.042287            190
seed7/prg/castle.sd7         0.113570           3148
seed7/prg/cat.sd7            0.039654             82
seed7/prg/cellauto.sd7       0.039709             85
seed7/prg/celsius.sd7        0.039292             42
seed7/prg/chk_all.sd7        0.064299            843
seed7/prg/chkarr.sd7         0.371218           8367
seed7/prg/chkbig.sd7         2.132564          29026
seed7/prg/chkbin.sd7         0.528717           6469
seed7/prg/chkbitdata.sd7     0.631322           6624
seed7/prg/chkbool.sd7        0.121650           3157
seed7/prg/chkbst.sd7         0.071568            722
seed7/prg/chkchr.sd7         0.227591           2809
seed7/prg/chkcmd.sd7         0.073145           1205
seed7/prg/chkdb.sd7          0.365639           7454
seed7/prg/chkdecl.sd7        0.062424            448
seed7/prg/chkenum.sd7        0.071994           1230
seed7/prg/chkerr.sd7         0.203665           4663
seed7/prg/chkexc.sd7         0.087023           2627
seed7/prg/chkfil.sd7         0.080717           1615
seed7/prg/chkflt.sd7         1.388200          20620
seed7/prg/chkhent.sd7        0.040388             54
seed7/prg/chkhsh.sd7         0.255544           4548
seed7/prg/chkidx.sd7         1.369854          19567
seed7/prg/chkint.sd7         2.613087          38129
seed7/prg/chkjson.sd7        0.106217           1764
seed7/prg/chkovf.sd7         0.578796           8216
seed7/prg/chkprc.sd7         0.332285          10111
seed7/prg/chkscan.sd7        0.061556            714
seed7/prg/chkset.sd7         0.688914          11974
seed7/prg/chkstr.sd7         1.427030          26952
seed7/prg/chktime.sd7        0.134833           2025
seed7/prg/chktoml.sd7        0.116077           1656
seed7/prg/clock.sd7          0.041554             47
seed7/prg/clock2.sd7         0.039293             43
seed7/prg/clock3.sd7         0.042752             95
seed7/prg/cmpfil.sd7         0.041430             84
seed7/prg/comanche.sd7       0.045112            180
seed7/prg/confval.sd7        0.047058            175
seed7/prg/db7.sd7            0.051663            417
seed7/prg/diff7.sd7          0.047363            263
seed7/prg/dirtst.sd7         0.040081             42
seed7/prg/dirx.sd7           0.042768            152
seed7/prg/dnafight.sd7       0.073469           1381
seed7/prg/dragon.sd7         0.041475             73
seed7/prg/echo.sd7           0.040161             39
seed7/prg/eliza.sd7          0.047677            302
seed7/prg/err.sd7            0.044679             96
seed7/prg/fannkuch.sd7       0.041702            131
seed7/prg/fib.sd7            0.038629             47
seed7/prg/find7.sd7          0.041414            133
seed7/prg/findchar.sd7       0.041737            149
seed7/prg/fractree.sd7       0.039385             55
seed7/prg/ftp7.sd7           0.046536            296
seed7/prg/ftpserv.sd7        0.041403             74
seed7/prg/gcd.sd7            0.041379            109
seed7/prg/gkbd.sd7           0.050858            358
seed7/prg/gtksvtst.sd7       0.041432             94
seed7/prg/hal.sd7            0.044126            250
seed7/prg/hamu.sd7           0.052426            573
seed7/prg/hanoi.sd7          0.039809             55
seed7/prg/hd.sd7             0.041418             79
seed7/prg/hello.sd7          0.039861             32
seed7/prg/hilbert.sd7        0.041429            108
seed7/prg/ide7.sd7           0.044869            196
seed7/prg/kbd.sd7            0.040327             49
seed7/prg/klondike.sd7       0.060438            883
seed7/prg/lander.sd7         0.077705           1551
seed7/prg/lst80bas.sd7       0.048445            344
seed7/prg/lst99bas.sd7       0.048807            401
seed7/prg/lstgwbas.sd7       0.054557            577
seed7/prg/mahjong.sd7        0.085665           1943
seed7/prg/make7.sd7          0.042106            121
seed7/prg/mandelbr.sd7       0.045071            237
seed7/prg/mind.sd7           0.048895            443
seed7/prg/mirror.sd7         0.042180            131
seed7/prg/ms.sd7             0.054376            641
seed7/prg/nicoma.sd7         0.042796            135
seed7/prg/pac.sd7            0.054521            726
seed7/prg/pairs.sd7          0.089274           2025
seed7/prg/panic.sd7          0.105252           2634
seed7/prg/percolation.sd7    0.048673            330
seed7/prg/planets.sd7        0.082700           1486
seed7/prg/portfwd7.sd7       0.043407            139
seed7/prg/prime.sd7          0.041167             74
seed7/prg/printpi1.sd7       0.040115             56
seed7/prg/printpi2.sd7       0.040142             54
seed7/prg/printpi3.sd7       0.039456             60
seed7/prg/pv7.sd7            0.048214            337
seed7/prg/queen.sd7          0.041003            149
seed7/prg/rand.sd7           0.040763            121
seed7/prg/raytrace.sd7       0.052579            538
seed7/prg/rever.sd7          0.057841            816
seed7/prg/roman.sd7          0.038919             38
seed7/prg/s7c.sd7            0.291422           9060
seed7/prg/s7check.sd7        0.040876             68
seed7/prg/savehd7.sd7        0.072934           1110
seed7/prg/self.sd7           0.040794             49
seed7/prg/shisen.sd7         0.077223           1423
seed7/prg/sl.sd7             0.065827           1029
seed7/prg/snake.sd7          0.052289            615
seed7/prg/sokoban.sd7        0.060824            891
seed7/prg/spigotpi.sd7       0.040979             64
seed7/prg/sql7.sd7           0.047854            278
seed7/prg/startrek.sd7       0.065676            979
seed7/prg/sudoku7.sd7        0.107006           2657
seed7/prg/sydir7.sd7         0.049333            384
seed7/prg/syntaxhl.sd7       0.044744            177
seed7/prg/tak.sd7            0.039826             59
seed7/prg/tar7.sd7           0.040927            121
seed7/prg/tch.sd7            0.040811             55
seed7/prg/testfont.sd7       0.042228             95
seed7/prg/tet.sd7            0.049368            479
seed7/prg/tetg.sd7           0.050612            501
seed7/prg/toutf8.sd7         0.047350            240
seed7/prg/tst_cli.sd7        0.040370             40
seed7/prg/tst_srv.sd7        0.040512             47
seed7/prg/wator.sd7          0.057798            651
seed7/prg/which.sd7          0.041007             65
seed7/prg/wiz.sd7            0.111937           2833
seed7/prg/wordcnt.sd7        0.040789             54
seed7/prg/wrinum.sd7         0.038852             43
seed7/prg/wumpus.sd7         0.046367            372
seed7/lib/aes.s7i            0.116099           1144
seed7/lib/aes_gcm.s7i        0.050324            392
seed7/lib/ar.s7i             0.078339           1532
seed7/lib/arc4.s7i           0.042003            144
seed7/lib/archive.s7i        0.042586            143
seed7/lib/archive_base.s7i   0.042591            135
seed7/lib/array.s7i          0.057497            610
seed7/lib/asn1.s7i           0.052409            544
seed7/lib/asn1oid.s7i        0.046616            157
seed7/lib/basearray.s7i      0.053915            450
seed7/lib/bigfile.s7i        0.041882            136
seed7/lib/bigint.s7i         0.060850            824
seed7/lib/bigrat.s7i         0.059781            784
seed7/lib/bin16.s7i          0.057533            592
seed7/lib/bin32.s7i          0.060289            490
seed7/lib/bin64.s7i          0.061388            539
seed7/lib/bitdata.s7i        0.086596           1330
seed7/lib/bitmapfont.s7i     0.044713            215
seed7/lib/bitset.s7i         0.053704            593
seed7/lib/bitsetof.s7i       0.052562            431
seed7/lib/blowfish.s7i       0.062228            383
seed7/lib/bmp.s7i            0.066446            924
seed7/lib/boolean.s7i        0.048581            403
seed7/lib/browser.s7i        0.046645            280
seed7/lib/bstring.s7i        0.044239            227
seed7/lib/bytedata.s7i       0.053518            482
seed7/lib/bzip2.s7i          0.063687            887
seed7/lib/cards.s7i          0.072014           1342
seed7/lib/category.s7i       0.045854            209
seed7/lib/cc_conf.s7i        0.083714           1314
seed7/lib/ccittfax.s7i       0.072805           1022
seed7/lib/cgi.s7i            0.041457            109
seed7/lib/cgidialog.s7i      0.064913           1118
seed7/lib/char.s7i           0.047480            356
seed7/lib/charsets.s7i       0.088254           2024
seed7/lib/chartype.s7i       0.044524            121
seed7/lib/cipher.s7i         0.042985            146
seed7/lib/cli_cmds.s7i       0.075824           1360
seed7/lib/clib_file.s7i      0.048730            301
seed7/lib/color.s7i          0.045682            185
seed7/lib/complex.s7i        0.049610            464
seed7/lib/compress.s7i       0.042033            150
seed7/lib/console.s7i        0.042926            188
seed7/lib/cpio.s7i           0.086591           1708
seed7/lib/crc32.s7i          0.048997            193
seed7/lib/cronos16.s7i       0.099828           1173
seed7/lib/cronos27.s7i       0.125232           1464
seed7/lib/csv.s7i            0.045203            201
seed7/lib/db_prop.s7i        0.069175            991
seed7/lib/deflate.s7i        0.061384            740
seed7/lib/des.s7i            0.061076            444
seed7/lib/dialog.s7i         0.048766            311
seed7/lib/dir.s7i            0.043057            163
seed7/lib/draw.s7i           0.061620            854
seed7/lib/duration.s7i       0.066834           1038
seed7/lib/echo.s7i           0.043143            132
seed7/lib/editline.s7i       0.049624            398
seed7/lib/elf.s7i            0.090809           1560
seed7/lib/elliptic.s7i       0.056767            649
seed7/lib/enable_io.s7i      0.048019            312
seed7/lib/encoding.s7i       0.067447            931
seed7/lib/enumeration.s7i    0.047074            236
seed7/lib/environment.s7i    0.044024            175
seed7/lib/exif.s7i           0.044843            152
seed7/lib/external_file.s7i  0.047849            340
seed7/lib/field.s7i          0.047262            268
seed7/lib/file.s7i           0.049666            372
seed7/lib/filebits.s7i       0.040969             46
seed7/lib/filesys.s7i        0.055828            601
seed7/lib/fileutil.s7i       0.045383            144
seed7/lib/fixarray.s7i       0.053324            307
seed7/lib/float.s7i          0.061250            757
seed7/lib/font.s7i           0.043941            196
seed7/lib/font8x8.s7i        0.053747            998
seed7/lib/forloop.s7i        0.050515            449
seed7/lib/ftp.s7i            0.062416            969
seed7/lib/ftpserv.s7i        0.057357            631
seed7/lib/getf.s7i           0.041519            115
seed7/lib/gethttp.s7i        0.039525             41
seed7/lib/gethttps.s7i       0.040030             41
seed7/lib/gif.s7i            0.056093            561
seed7/lib/graph.s7i          0.055857            415
seed7/lib/graph_file.s7i     0.051666            399
seed7/lib/gtkserver.s7i      0.043406            161
seed7/lib/gzip.s7i           0.055912            573
seed7/lib/hash.s7i           0.055911            421
seed7/lib/hashsetof.s7i      0.054826            499
seed7/lib/hmac.s7i           0.044208            152
seed7/lib/html.s7i           0.041329             83
seed7/lib/html_ent.s7i       0.052447            476
seed7/lib/htmldom.s7i        0.047957            286
seed7/lib/http_request.s7i   0.057103            696
seed7/lib/http_srv_resp.s7i  0.051191            380
seed7/lib/https_request.s7i  0.045016            211
seed7/lib/httpserv.s7i       0.048995            345
seed7/lib/huffman.s7i        0.058903            644
seed7/lib/ico.s7i            0.045317            221
seed7/lib/idxarray.s7i       0.046118            232
seed7/lib/image.s7i          0.041501            156
seed7/lib/imagefile.s7i      0.044369            171
seed7/lib/inflate.s7i        0.055496            411
seed7/lib/inifile.s7i        0.044422            129
seed7/lib/integer.s7i        0.058209            663
seed7/lib/iobuffer.s7i       0.047199            289
seed7/lib/jpeg.s7i           0.090623           1761
seed7/lib/json.s7i           0.061251            891
seed7/lib/json_serde.s7i     0.058472            783
seed7/lib/keybd.s7i          0.061015            639
seed7/lib/keydescr.s7i       0.045484            192
seed7/lib/leb128.s7i         0.044943            218
seed7/lib/line.s7i           0.041921            164
seed7/lib/listener.s7i       0.045136            247
seed7/lib/logfile.s7i        0.039422             73
seed7/lib/lower.s7i          0.040459            142
seed7/lib/lzma.s7i           0.064259            934
seed7/lib/lzw.s7i            0.064463            861
seed7/lib/magic.s7i          0.052221            403
seed7/lib/mahjng32.s7i       0.069631           1500
seed7/lib/make.s7i           0.053653            544
seed7/lib/makedata.s7i       0.074834           1428
seed7/lib/math.s7i           0.043795            201
seed7/lib/mixarith.s7i       0.044573            249
seed7/lib/modern27.s7i       0.090174           1099
seed7/lib/more.s7i           0.042898            130
seed7/lib/msgdigest.s7i      0.085067           1222
seed7/lib/multiscr.s7i       0.041415             68
seed7/lib/null_file.s7i      0.046433            345
seed7/lib/osfiles.s7i        0.071386           1085
seed7/lib/pbm.s7i            0.044848            230
seed7/lib/pcx.s7i            0.057685            638
seed7/lib/pem.s7i            0.044177            185
seed7/lib/pgm.s7i            0.045258            238
seed7/lib/pic16.s7i          0.053548           1037
seed7/lib/pic32.s7i          0.086336           2060
seed7/lib/pic_util.s7i       0.044067            144
seed7/lib/pixelimage.s7i     0.046045            320
seed7/lib/pixmap_file.s7i    0.049246            459
seed7/lib/pixmapfont.s7i     0.043265            184
seed7/lib/pkcs1.s7i          0.063014            543
seed7/lib/png.s7i            0.069093           1064
seed7/lib/poll.s7i           0.049491            313
seed7/lib/ppm.s7i            0.047515            240
seed7/lib/process.s7i        0.054312            541
seed7/lib/progs.s7i          0.060815            789
seed7/lib/propertyfile.s7i   0.042143            155
seed7/lib/rational.s7i       0.059740            792
seed7/lib/ref_list.s7i       0.045417            252
seed7/lib/reference.s7i      0.041367            126
seed7/lib/reverse.s7i        0.042098             94
seed7/lib/rpm.s7i            0.154600           3487
seed7/lib/rpmext.s7i         0.048017            318
seed7/lib/scanfile.s7i       0.088127           1779
seed7/lib/scanjson.s7i       0.051910            413
seed7/lib/scanstri.s7i       0.085490           1814
seed7/lib/scantoml.s7i       0.076483           1603
seed7/lib/seed7_05.s7i       0.071892           1072
seed7/lib/set.s7i            0.040573             57
seed7/lib/shell.s7i          0.058697            615
seed7/lib/showtls.s7i        0.060484            678
seed7/lib/signature.s7i      0.042625            131
seed7/lib/smtp.s7i           0.045169            261
seed7/lib/sockbase.s7i       0.046884            217
seed7/lib/socket.s7i         0.047120            326
seed7/lib/sokoban1.s7i       0.059360           1519
seed7/lib/sql_base.s7i       0.070157           1000
seed7/lib/stars.s7i          0.146279           1705
seed7/lib/stdfont10.s7i      0.087707           3347
seed7/lib/stdfont12.s7i      0.097692           3928
seed7/lib/stdfont14.s7i      0.109135           4510
seed7/lib/stdfont16.s7i      0.121682           5092
seed7/lib/stdfont18.s7i      0.141504           5868
seed7/lib/stdfont20.s7i      0.157736           6449
seed7/lib/stdfont24.s7i      0.191614           7421
seed7/lib/stdfont8.s7i       0.078443           2960
seed7/lib/stdfont9.s7i       0.083107           3152
seed7/lib/stdio.s7i          0.043964            192
seed7/lib/strifile.s7i       0.048013            345
seed7/lib/string.s7i         0.060575            779
seed7/lib/stritext.s7i       0.047023            352
seed7/lib/struct.s7i         0.047853            266
seed7/lib/struct_elem.s7i    0.045434            129
seed7/lib/subfile.s7i        0.044070            174
seed7/lib/subrange.s7i       0.040485             78
seed7/lib/syntax.s7i         0.050168            294
seed7/lib/tar.s7i            0.089300           1880
seed7/lib/tar_cmds.s7i       0.061624            752
seed7/lib/tdes.s7i           0.043114            143
seed7/lib/tee.s7i            0.041921            143
seed7/lib/text.s7i           0.041637            135
seed7/lib/tga.s7i            0.058060            676
seed7/lib/tiff.s7i           0.128945           2771
seed7/lib/time.s7i           0.069123           1191
seed7/lib/tls.s7i            0.110116           2230
seed7/lib/unicode.s7i        0.056990            575
seed7/lib/unionfnd.s7i       0.042105            130
seed7/lib/upper.s7i          0.042495            142
seed7/lib/utf16.s7i          0.054112            540
seed7/lib/utf8.s7i           0.047088            234
seed7/lib/vecfont10.s7i      0.085250           1056
seed7/lib/vecfont18.s7i      0.093905           1119
seed7/lib/vector3d.s7i       0.049631            293
seed7/lib/vectorfont.s7i     0.044727            239
seed7/lib/wildcard.s7i       0.042873            140
seed7/lib/window.s7i         0.049906            455
seed7/lib/wrinum.s7i         0.044969            248
seed7/lib/x509cert.s7i       0.077584           1243
seed7/lib/xml_ent.s7i        0.041551             94
seed7/lib/xmldom.s7i         0.045361            303
seed7/lib/xz.s7i             0.050654            442
seed7/lib/zip.s7i            0.127054           2792
seed7/lib/zstd.s7i           0.074948           1263
============================ ================= ================

Statistical Summary — Mode C (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.095313        |
+-----------+-----------------+
| Minimum   | 0.038629        |
+-----------+-----------------+
| Maximum   | 2.613087        |
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
seed7/prg/addup.sd7          0.045775            190
seed7/prg/bas7.sd7           0.773695          11459
seed7/prg/bifurk.sd7         0.039797             73
seed7/prg/bigfiles.sd7       0.042910            129
seed7/prg/brainf7.sd7        0.038717             86
seed7/prg/calc7.sd7          0.040954            128
seed7/prg/carddemo.sd7       0.045639            190
seed7/prg/castle.sd7         0.212839           3148
seed7/prg/cat.sd7            0.040003             82
seed7/prg/cellauto.sd7       0.040288             85
seed7/prg/celsius.sd7        0.036659             42
seed7/prg/chk_all.sd7        0.082019            843
seed7/prg/chkarr.sd7         0.854813           8367
seed7/prg/chkbig.sd7         4.116968          29026
seed7/prg/chkbin.sd7         1.026193           6469
seed7/prg/chkbitdata.sd7     1.247098           6624
seed7/prg/chkbool.sd7        0.229763           3157
seed7/prg/chkbst.sd7         0.103988            722
seed7/prg/chkchr.sd7         0.475445           2809
seed7/prg/chkcmd.sd7         0.112390           1205
seed7/prg/chkdb.sd7          0.757860           7454
seed7/prg/chkdecl.sd7        0.094618            448
seed7/prg/chkenum.sd7        0.122874           1230
seed7/prg/chkerr.sd7         0.338405           4663
seed7/prg/chkexc.sd7         0.149445           2627
seed7/prg/chkfil.sd7         0.127239           1615
seed7/prg/chkflt.sd7         2.823731          20620
seed7/prg/chkhent.sd7        0.038856             54
seed7/prg/chkhsh.sd7         0.505644           4548
seed7/prg/chkidx.sd7         3.162527          19567
seed7/prg/chkint.sd7         5.570794          38129
seed7/prg/chkjson.sd7        0.194530           1764
seed7/prg/chkovf.sd7         1.201189           8216
seed7/prg/chkprc.sd7         0.698575          10111
seed7/prg/chkscan.sd7        0.089580            714
seed7/prg/chkset.sd7         1.740321          11974
seed7/prg/chkstr.sd7         3.430423          26952
seed7/prg/chktime.sd7        0.247386           2025
seed7/prg/chktoml.sd7        0.197638           1656
seed7/prg/clock.sd7          0.038618             47
seed7/prg/clock2.sd7         0.036213             43
seed7/prg/clock3.sd7         0.040420             95
seed7/prg/cmpfil.sd7         0.038480             84
seed7/prg/comanche.sd7       0.046985            180
seed7/prg/confval.sd7        0.049877            175
seed7/prg/db7.sd7            0.063782            417
seed7/prg/diff7.sd7          0.053570            263
seed7/prg/dirtst.sd7         0.037301             42
seed7/prg/dirx.sd7           0.043734            152
seed7/prg/dnafight.sd7       0.120455           1381
seed7/prg/dragon.sd7         0.038337             73
seed7/prg/echo.sd7           0.036153             39
seed7/prg/eliza.sd7          0.052072            302
seed7/prg/err.sd7            0.044632             96
seed7/prg/fannkuch.sd7       0.042926            131
seed7/prg/fib.sd7            0.038698             47
seed7/prg/find7.sd7          0.042656            133
seed7/prg/findchar.sd7       0.043567            149
seed7/prg/fractree.sd7       0.036936             55
seed7/prg/ftp7.sd7           0.053566            296
seed7/prg/ftpserv.sd7        0.038895             74
seed7/prg/gcd.sd7            0.040400            109
seed7/prg/gkbd.sd7           0.062444            358
seed7/prg/gtksvtst.sd7       0.038846             94
seed7/prg/hal.sd7            0.047287            250
seed7/prg/hamu.sd7           0.070222            573
seed7/prg/hanoi.sd7          0.036655             55
seed7/prg/hd.sd7             0.037353             79
seed7/prg/hello.sd7          0.044636             32
seed7/prg/hilbert.sd7        0.045026            108
seed7/prg/ide7.sd7           0.050749            196
seed7/prg/kbd.sd7            0.037072             49
seed7/prg/klondike.sd7       0.088498            883
seed7/prg/lander.sd7         0.134835           1551
seed7/prg/lst80bas.sd7       0.055287            344
seed7/prg/lst99bas.sd7       0.066035            401
seed7/prg/lstgwbas.sd7       0.073924            577
seed7/prg/mahjong.sd7        0.147080           1943
seed7/prg/make7.sd7          0.042598            121
seed7/prg/mandelbr.sd7       0.049076            237
seed7/prg/mind.sd7           0.059239            443
seed7/prg/mirror.sd7         0.042804            131
seed7/prg/ms.sd7             0.068285            641
seed7/prg/nicoma.sd7         0.041274            135
seed7/prg/pac.sd7            0.071453            726
seed7/prg/pairs.sd7          0.138626           2025
seed7/prg/panic.sd7          0.192680           2634
seed7/prg/percolation.sd7    0.055252            330
seed7/prg/planets.sd7        0.135902           1486
seed7/prg/portfwd7.sd7       0.042877            139
seed7/prg/prime.sd7          0.037400             74
seed7/prg/printpi1.sd7       0.037019             56
seed7/prg/printpi2.sd7       0.036666             54
seed7/prg/printpi3.sd7       0.037218             60
seed7/prg/pv7.sd7            0.057226            337
seed7/prg/queen.sd7          0.042625            149
seed7/prg/rand.sd7           0.040629            121
seed7/prg/raytrace.sd7       0.065950            538
seed7/prg/rever.sd7          0.078766            816
seed7/prg/roman.sd7          0.038460             38
seed7/prg/s7c.sd7            0.622716           9060
seed7/prg/s7check.sd7        0.039297             68
seed7/prg/savehd7.sd7        0.116403           1110
seed7/prg/self.sd7           0.041508             49
seed7/prg/shisen.sd7         0.122610           1423
seed7/prg/sl.sd7             0.097551           1029
seed7/prg/snake.sd7          0.065440            615
seed7/prg/sokoban.sd7        0.083986            891
seed7/prg/spigotpi.sd7       0.037837             64
seed7/prg/sql7.sd7           0.053300            278
seed7/prg/startrek.sd7       0.094479            979
seed7/prg/sudoku7.sd7        0.201871           2657
seed7/prg/sydir7.sd7         0.061979            384
seed7/prg/syntaxhl.sd7       0.048615            177
seed7/prg/tak.sd7            0.037794             59
seed7/prg/tar7.sd7           0.043007            121
seed7/prg/tch.sd7            0.037088             55
seed7/prg/testfont.sd7       0.046242             95
seed7/prg/tet.sd7            0.062596            479
seed7/prg/tetg.sd7           0.065427            501
seed7/prg/toutf8.sd7         0.052604            240
seed7/prg/tst_cli.sd7        0.037686             40
seed7/prg/tst_srv.sd7        0.036760             47
seed7/prg/wator.sd7          0.078862            651
seed7/prg/which.sd7          0.038506             65
seed7/prg/wiz.sd7            0.207782           2833
seed7/prg/wordcnt.sd7        0.038046             54
seed7/prg/wrinum.sd7         0.036249             43
seed7/prg/wumpus.sd7         0.053489            372
seed7/lib/aes.s7i            0.197659           1144
seed7/lib/aes_gcm.s7i        0.062634            392
seed7/lib/ar.s7i             0.124887           1532
seed7/lib/arc4.s7i           0.042755            144
seed7/lib/archive.s7i        0.042675            143
seed7/lib/archive_base.s7i   0.041682            135
seed7/lib/array.s7i          0.078298            610
seed7/lib/asn1.s7i           0.062934            544
seed7/lib/asn1oid.s7i        0.053326            157
seed7/lib/basearray.s7i      0.063003            450
seed7/lib/bigfile.s7i        0.041586            136
seed7/lib/bigint.s7i         0.078350            824
seed7/lib/bigrat.s7i         0.084955            784
seed7/lib/bin16.s7i          0.072941            592
seed7/lib/bin32.s7i          0.064054            490
seed7/lib/bin64.s7i          0.062428            539
seed7/lib/bitdata.s7i        0.131996           1330
seed7/lib/bitmapfont.s7i     0.048505            215
seed7/lib/bitset.s7i         0.061935            593
seed7/lib/bitsetof.s7i       0.062076            431
seed7/lib/blowfish.s7i       0.078192            383
seed7/lib/bmp.s7i            0.097399            924
seed7/lib/boolean.s7i        0.054904            403
seed7/lib/browser.s7i        0.054187            280
seed7/lib/bstring.s7i        0.052458            227
seed7/lib/bytedata.s7i       0.068195            482
seed7/lib/bzip2.s7i          0.093274            887
seed7/lib/cards.s7i          0.107719           1342
seed7/lib/category.s7i       0.049308            209
seed7/lib/cc_conf.s7i        0.124567           1314
seed7/lib/ccittfax.s7i       0.102509           1022
seed7/lib/cgi.s7i            0.041830            109
seed7/lib/cgidialog.s7i      0.102349           1118
seed7/lib/char.s7i           0.050730            356
seed7/lib/charsets.s7i       0.121967           2024
seed7/lib/chartype.s7i       0.046126            121
seed7/lib/cipher.s7i         0.041168            146
seed7/lib/cli_cmds.s7i       0.111865           1360
seed7/lib/clib_file.s7i      0.053243            301
seed7/lib/color.s7i          0.046615            185
seed7/lib/complex.s7i        0.058754            464
seed7/lib/compress.s7i       0.045864            150
seed7/lib/console.s7i        0.046196            188
seed7/lib/cpio.s7i           0.147425           1708
seed7/lib/crc32.s7i          0.054960            193
seed7/lib/cronos16.s7i       0.195033           1173
seed7/lib/cronos27.s7i       0.252250           1464
seed7/lib/csv.s7i            0.046158            201
seed7/lib/db_prop.s7i        0.101327            991
seed7/lib/deflate.s7i        0.087195            740
seed7/lib/des.s7i            0.081545            444
seed7/lib/dialog.s7i         0.057923            311
seed7/lib/dir.s7i            0.042803            163
seed7/lib/draw.s7i           0.086973            854
seed7/lib/duration.s7i       0.102256           1038
seed7/lib/echo.s7i           0.042602            132
seed7/lib/editline.s7i       0.061361            398
seed7/lib/elf.s7i            0.156627           1560
seed7/lib/elliptic.s7i       0.075879            649
seed7/lib/enable_io.s7i      0.051497            312
seed7/lib/encoding.s7i       0.096877            931
seed7/lib/enumeration.s7i    0.051321            236
seed7/lib/environment.s7i    0.046394            175
seed7/lib/exif.s7i           0.047272            152
seed7/lib/external_file.s7i  0.052311            340
seed7/lib/field.s7i          0.055349            268
seed7/lib/file.s7i           0.057417            372
seed7/lib/filebits.s7i       0.038455             46
seed7/lib/filesys.s7i        0.065526            601
seed7/lib/fileutil.s7i       0.043620            144
seed7/lib/fixarray.s7i       0.054127            307
seed7/lib/float.s7i          0.074457            757
seed7/lib/font.s7i           0.045412            196
seed7/lib/font8x8.s7i        0.069384            998
seed7/lib/forloop.s7i        0.059734            449
seed7/lib/ftp.s7i            0.086300            969
seed7/lib/ftpserv.s7i        0.074980            631
seed7/lib/getf.s7i           0.040439            115
seed7/lib/gethttp.s7i        0.038635             41
seed7/lib/gethttps.s7i       0.036914             41
seed7/lib/gif.s7i            0.072811            561
seed7/lib/graph.s7i          0.065600            415
seed7/lib/graph_file.s7i     0.059513            399
seed7/lib/gtkserver.s7i      0.042454            161
seed7/lib/gzip.s7i           0.069319            573
seed7/lib/hash.s7i           0.066413            421
seed7/lib/hashsetof.s7i      0.068293            499
seed7/lib/hmac.s7i           0.044531            152
seed7/lib/html.s7i           0.039738             83
seed7/lib/html_ent.s7i       0.065338            476
seed7/lib/htmldom.s7i        0.053709            286
seed7/lib/http_request.s7i   0.078447            696
seed7/lib/http_srv_resp.s7i  0.060194            380
seed7/lib/https_request.s7i  0.048603            211
seed7/lib/httpserv.s7i       0.058447            345
seed7/lib/huffman.s7i        0.074098            644
seed7/lib/ico.s7i            0.049041            221
seed7/lib/idxarray.s7i       0.049368            232
seed7/lib/image.s7i          0.043943            156
seed7/lib/imagefile.s7i      0.044390            171
seed7/lib/inflate.s7i        0.063633            411
seed7/lib/inifile.s7i        0.040856            129
seed7/lib/integer.s7i        0.066818            663
seed7/lib/iobuffer.s7i       0.050064            289
seed7/lib/jpeg.s7i           0.153176           1761
seed7/lib/json.s7i           0.081315            891
seed7/lib/json_serde.s7i     0.078382            783
seed7/lib/keybd.s7i          0.079180            639
seed7/lib/keydescr.s7i       0.051042            192
seed7/lib/leb128.s7i         0.048141            218
seed7/lib/line.s7i           0.042270            164
seed7/lib/listener.s7i       0.046977            247
seed7/lib/logfile.s7i        0.039458             73
seed7/lib/lower.s7i          0.041945            142
seed7/lib/lzma.s7i           0.101601            934
seed7/lib/lzw.s7i            0.090021            861
seed7/lib/magic.s7i          0.064517            403
seed7/lib/mahjng32.s7i       0.095089           1500
seed7/lib/make.s7i           0.070460            544
seed7/lib/makedata.s7i       0.122506           1428
seed7/lib/math.s7i           0.044465            201
seed7/lib/mixarith.s7i       0.046950            249
seed7/lib/modern27.s7i       0.174733           1099
seed7/lib/more.s7i           0.043330            130
seed7/lib/msgdigest.s7i      0.134810           1222
seed7/lib/multiscr.s7i       0.039987             68
seed7/lib/null_file.s7i      0.050437            345
seed7/lib/osfiles.s7i        0.097394           1085
seed7/lib/pbm.s7i            0.048153            230
seed7/lib/pcx.s7i            0.078813            638
seed7/lib/pem.s7i            0.049189            185
seed7/lib/pgm.s7i            0.048913            238
seed7/lib/pic16.s7i          0.067044           1037
seed7/lib/pic32.s7i          0.124644           2060
seed7/lib/pic_util.s7i       0.045199            144
seed7/lib/pixelimage.s7i     0.053656            320
seed7/lib/pixmap_file.s7i    0.063377            459
seed7/lib/pixmapfont.s7i     0.047755            184
seed7/lib/pkcs1.s7i          0.078922            543
seed7/lib/png.s7i            0.107687           1064
seed7/lib/poll.s7i           0.050794            313
seed7/lib/ppm.s7i            0.048270            240
seed7/lib/process.s7i        0.070467            541
seed7/lib/progs.s7i          0.087507            789
seed7/lib/propertyfile.s7i   0.047103            155
seed7/lib/rational.s7i       0.083769            792
seed7/lib/ref_list.s7i       0.047468            252
seed7/lib/reference.s7i      0.040630            126
seed7/lib/reverse.s7i        0.039005             94
seed7/lib/rpm.s7i            0.285252           3487
seed7/lib/rpmext.s7i         0.054487            318
seed7/lib/scanfile.s7i       0.139395           1779
seed7/lib/scanjson.s7i       0.065311            413
seed7/lib/scanstri.s7i       0.134804           1814
seed7/lib/scantoml.s7i       0.134086           1603
seed7/lib/seed7_05.s7i       0.112066           1072
seed7/lib/set.s7i            0.043097             57
seed7/lib/shell.s7i          0.069802            615
seed7/lib/showtls.s7i        0.088850            678
seed7/lib/signature.s7i      0.045115            131
seed7/lib/smtp.s7i           0.049725            261
seed7/lib/sockbase.s7i       0.054548            217
seed7/lib/socket.s7i         0.052399            326
seed7/lib/sokoban1.s7i       0.084937           1519
seed7/lib/sql_base.s7i       0.094503           1000
seed7/lib/stars.s7i          0.238439           1705
seed7/lib/stdfont10.s7i      0.152002           3347
seed7/lib/stdfont12.s7i      0.168136           3928
seed7/lib/stdfont14.s7i      0.198650           4510
seed7/lib/stdfont16.s7i      0.215745           5092
seed7/lib/stdfont18.s7i      0.251831           5868
seed7/lib/stdfont20.s7i      0.284966           6449
seed7/lib/stdfont24.s7i      0.338735           7421
seed7/lib/stdfont8.s7i       0.136484           2960
seed7/lib/stdfont9.s7i       0.139787           3152
seed7/lib/stdio.s7i          0.044900            192
seed7/lib/strifile.s7i       0.055195            345
seed7/lib/string.s7i         0.076768            779
seed7/lib/stritext.s7i       0.053665            352
seed7/lib/struct.s7i         0.054388            266
seed7/lib/struct_elem.s7i    0.043562            129
seed7/lib/subfile.s7i        0.046572            174
seed7/lib/subrange.s7i       0.039102             78
seed7/lib/syntax.s7i         0.061445            294
seed7/lib/tar.s7i            0.155187           1880
seed7/lib/tar_cmds.s7i       0.087248            752
seed7/lib/tdes.s7i           0.048346            143
seed7/lib/tee.s7i            0.043578            143
seed7/lib/text.s7i           0.043947            135
seed7/lib/tga.s7i            0.082081            676
seed7/lib/tiff.s7i           0.247217           2771
seed7/lib/time.s7i           0.103795           1191
seed7/lib/tls.s7i            0.204144           2230
seed7/lib/unicode.s7i        0.075665            575
seed7/lib/unionfnd.s7i       0.044288            130
seed7/lib/upper.s7i          0.042630            142
seed7/lib/utf16.s7i          0.065863            540
seed7/lib/utf8.s7i           0.049394            234
seed7/lib/vecfont10.s7i      0.164033           1056
seed7/lib/vecfont18.s7i      0.186523           1119
seed7/lib/vector3d.s7i       0.053554            293
seed7/lib/vectorfont.s7i     0.049671            239
seed7/lib/wildcard.s7i       0.044430            140
seed7/lib/window.s7i         0.064293            455
seed7/lib/wrinum.s7i         0.052560            248
seed7/lib/x509cert.s7i       0.125881           1243
seed7/lib/xml_ent.s7i        0.041183             94
seed7/lib/xmldom.s7i         0.052029            303
seed7/lib/xz.s7i             0.065139            442
seed7/lib/zip.s7i            0.240010           2792
seed7/lib/zstd.s7i           0.124689           1263
============================ ================= ================

Statistical Summary — Mode D (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.159702        |
+-----------+-----------------+
| Minimum   | 0.036153        |
+-----------+-----------------+
| Maximum   | 5.570794        |
+-----------+-----------------+

Cross-Mode Comparison
=====================

+------+-----------------+-----------------+-----------------+
| Mode | Average (s)     | Minimum (s)     | Maximum (s)     |
+======+=================+=================+=================+
| A    | 0.035029        | 0.032097        | 0.042321        |
+------+-----------------+-----------------+-----------------+
| B    | 0.039068        | 0.035110        | 0.050422        |
+------+-----------------+-----------------+-----------------+
| C    | 0.095313        | 0.038629        | 2.613087        |
+------+-----------------+-----------------+-----------------+
| D    | 0.159702        | 0.036153        | 5.570794        |
+------+-----------------+-----------------+-----------------+

Phase Timing Summary
====================

Wall-clock times include warm-up work and GC performed by the warm-up helper.

+----------+--------------+--------------+--------------+
| Phase    | Warm-up      | Timed pass   | Mode total   |
+==========+==============+==============+==============+
| Mode A   | 00:00:13.062 | 00:00:59.681 | 00:01:12.744 |
+----------+--------------+--------------+--------------+
| Mode B   | 00:00:14.779 | 00:01:06.628 | 00:01:21.407 |
+----------+--------------+--------------+--------------+
| Mode C   | 00:00:38.182 | 00:02:43.328 | 00:03:21.511 |
+----------+--------------+--------------+--------------+
| Mode D   | 00:01:03.507 | 00:04:33.036 | 00:05:36.544 |
+----------+--------------+--------------+--------------+
| Total    |              |              | 00:11:32.214 |
+----------+--------------+--------------+--------------+


.. ---------------------------------------------------------------------------
