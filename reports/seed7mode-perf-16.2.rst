=======================================================
GC-Controlled Benchmark Report: Seed7 Mode — Four Modes
=======================================================

:Running with: seed7-mode 2026-06-30T03:26:24+0000 W27-2
:Seed7 dir   : /Users/roup/my/dvo/seed7-repos/seed7/
:Started at: 05:55:28 local time
:Generated on: 2026-06-30 10:06:44 UTC
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
seed7/prg/addup.sd7          0.034150            190
seed7/prg/bas7.sd7           0.033867          11459
seed7/prg/bifurk.sd7         0.033232             73
seed7/prg/bigfiles.sd7       0.032924            129
seed7/prg/brainf7.sd7        0.033095             86
seed7/prg/calc7.sd7          0.033233            128
seed7/prg/carddemo.sd7       0.033942            190
seed7/prg/castle.sd7         0.033295           3148
seed7/prg/cat.sd7            0.032717             82
seed7/prg/cellauto.sd7       0.032422             85
seed7/prg/celsius.sd7        0.032651             42
seed7/prg/chk_all.sd7        0.033590            843
seed7/prg/chkarr.sd7         0.033182           8367
seed7/prg/chkbig.sd7         0.041434          29026
seed7/prg/chkbin.sd7         0.034506           6469
seed7/prg/chkbitdata.sd7     0.034570           6624
seed7/prg/chkbool.sd7        0.034062           3157
seed7/prg/chkbst.sd7         0.033210            722
seed7/prg/chkchr.sd7         0.033748           2809
seed7/prg/chkcmd.sd7         0.035041           1205
seed7/prg/chkdb.sd7          0.034735           7454
seed7/prg/chkdecl.sd7        0.033127            448
seed7/prg/chkenum.sd7        0.033876           1230
seed7/prg/chkerr.sd7         0.033690           4663
seed7/prg/chkexc.sd7         0.033612           2627
seed7/prg/chkfil.sd7         0.033295           1615
seed7/prg/chkflt.sd7         0.036148          20620
seed7/prg/chkhent.sd7        0.033803             54
seed7/prg/chkhsh.sd7         0.033741           4548
seed7/prg/chkidx.sd7         0.036578          19567
seed7/prg/chkint.sd7         0.042053          38129
seed7/prg/chkjson.sd7        0.034006           1764
seed7/prg/chkovf.sd7         0.034129           8216
seed7/prg/chkprc.sd7         0.033815          10111
seed7/prg/chkscan.sd7        0.033350            714
seed7/prg/chkset.sd7         0.034209          11974
seed7/prg/chkstr.sd7         0.037977          26952
seed7/prg/chktime.sd7        0.033152           2025
seed7/prg/chktoml.sd7        0.033140           1656
seed7/prg/clock.sd7          0.032450             47
seed7/prg/clock2.sd7         0.033139             43
seed7/prg/clock3.sd7         0.032634             95
seed7/prg/cmpfil.sd7         0.032334             84
seed7/prg/comanche.sd7       0.034104            180
seed7/prg/confval.sd7        0.033200            175
seed7/prg/db7.sd7            0.034557            417
seed7/prg/diff7.sd7          0.033969            263
seed7/prg/dirtst.sd7         0.033809             42
seed7/prg/dirx.sd7           0.034082            152
seed7/prg/dnafight.sd7       0.033699           1381
seed7/prg/dragon.sd7         0.034066             73
seed7/prg/echo.sd7           0.033440             39
seed7/prg/eliza.sd7          0.033849            302
seed7/prg/err.sd7            0.033757             96
seed7/prg/fannkuch.sd7       0.035271            131
seed7/prg/fib.sd7            0.034025             47
seed7/prg/find7.sd7          0.033309            133
seed7/prg/findchar.sd7       0.033505            149
seed7/prg/fractree.sd7       0.033602             55
seed7/prg/ftp7.sd7           0.033546            296
seed7/prg/ftpserv.sd7        0.033506             74
seed7/prg/gcd.sd7            0.034025            109
seed7/prg/gkbd.sd7           0.034324            358
seed7/prg/gtksvtst.sd7       0.033449             94
seed7/prg/hal.sd7            0.033724            250
seed7/prg/hamu.sd7           0.033016            573
seed7/prg/hanoi.sd7          0.032947             55
seed7/prg/hd.sd7             0.032353             79
seed7/prg/hello.sd7          0.032821             32
seed7/prg/hilbert.sd7        0.032620            108
seed7/prg/ide7.sd7           0.032763            196
seed7/prg/kbd.sd7            0.033754             49
seed7/prg/klondike.sd7       0.032437            883
seed7/prg/lander.sd7         0.034455           1551
seed7/prg/lst80bas.sd7       0.033393            344
seed7/prg/lst99bas.sd7       0.033422            401
seed7/prg/lstgwbas.sd7       0.033984            577
seed7/prg/mahjong.sd7        0.033930           1943
seed7/prg/make7.sd7          0.033592            121
seed7/prg/mandelbr.sd7       0.033944            237
seed7/prg/mind.sd7           0.033262            443
seed7/prg/mirror.sd7         0.033432            131
seed7/prg/ms.sd7             0.033546            641
seed7/prg/nicoma.sd7         0.033162            135
seed7/prg/pac.sd7            0.033374            726
seed7/prg/pairs.sd7          0.033144           2025
seed7/prg/panic.sd7          0.033387           2634
seed7/prg/percolation.sd7    0.033924            330
seed7/prg/planets.sd7        0.033638           1486
seed7/prg/portfwd7.sd7       0.033788            139
seed7/prg/prime.sd7          0.034163             74
seed7/prg/printpi1.sd7       0.034605             56
seed7/prg/printpi2.sd7       0.033574             54
seed7/prg/printpi3.sd7       0.033653             60
seed7/prg/pv7.sd7            0.033733            337
seed7/prg/queen.sd7          0.033548            149
seed7/prg/rand.sd7           0.033383            121
seed7/prg/raytrace.sd7       0.032348            538
seed7/prg/rever.sd7          0.033396            816
seed7/prg/roman.sd7          0.032710             38
seed7/prg/s7c.sd7            0.033565           9060
seed7/prg/s7check.sd7        0.033276             68
seed7/prg/savehd7.sd7        0.033519           1110
seed7/prg/self.sd7           0.033522             49
seed7/prg/shisen.sd7         0.033963           1423
seed7/prg/sl.sd7             0.034091           1029
seed7/prg/snake.sd7          0.033269            615
seed7/prg/sokoban.sd7        0.033042            891
seed7/prg/spigotpi.sd7       0.034404             64
seed7/prg/sql7.sd7           0.033601            278
seed7/prg/startrek.sd7       0.038617            979
seed7/prg/sudoku7.sd7        0.040220           2657
seed7/prg/sydir7.sd7         0.033797            384
seed7/prg/syntaxhl.sd7       0.034578            177
seed7/prg/tak.sd7            0.033502             59
seed7/prg/tar7.sd7           0.033930            121
seed7/prg/tch.sd7            0.034152             55
seed7/prg/testfont.sd7       0.034267             95
seed7/prg/tet.sd7            0.033596            479
seed7/prg/tetg.sd7           0.034030            501
seed7/prg/toutf8.sd7         0.034412            240
seed7/prg/tst_cli.sd7        0.033825             40
seed7/prg/tst_srv.sd7        0.033931             47
seed7/prg/wator.sd7          0.033597            651
seed7/prg/which.sd7          0.034526             65
seed7/prg/wiz.sd7            0.034474           2833
seed7/prg/wordcnt.sd7        0.032957             54
seed7/prg/wrinum.sd7         0.032587             43
seed7/prg/wumpus.sd7         0.035007            372
seed7/lib/aes.s7i            0.032979           1144
seed7/lib/aes_gcm.s7i        0.033392            392
seed7/lib/ar.s7i             0.033035           1532
seed7/lib/arc4.s7i           0.033242            144
seed7/lib/archive.s7i        0.032881            143
seed7/lib/archive_base.s7i   0.034875            135
seed7/lib/array.s7i          0.034078            610
seed7/lib/asn1.s7i           0.033746            544
seed7/lib/asn1oid.s7i        0.033512            157
seed7/lib/basearray.s7i      0.033367            450
seed7/lib/bigfile.s7i        0.033435            136
seed7/lib/bigint.s7i         0.034580            824
seed7/lib/bigrat.s7i         0.033704            784
seed7/lib/bin16.s7i          0.033534            592
seed7/lib/bin32.s7i          0.033750            490
seed7/lib/bin64.s7i          0.033149            539
seed7/lib/bitdata.s7i        0.033543           1330
seed7/lib/bitmapfont.s7i     0.033547            215
seed7/lib/bitset.s7i         0.033490            593
seed7/lib/bitsetof.s7i       0.033373            431
seed7/lib/blowfish.s7i       0.033638            383
seed7/lib/bmp.s7i            0.033880            924
seed7/lib/boolean.s7i        0.033349            403
seed7/lib/browser.s7i        0.034938            280
seed7/lib/bstring.s7i        0.033383            227
seed7/lib/bytedata.s7i       0.033153            482
seed7/lib/bzip2.s7i          0.033318            887
seed7/lib/cards.s7i          0.033245           1342
seed7/lib/category.s7i       0.032992            209
seed7/lib/cc_conf.s7i        0.033410           1314
seed7/lib/ccittfax.s7i       0.033236           1022
seed7/lib/cgi.s7i            0.032281            109
seed7/lib/cgidialog.s7i      0.032480           1118
seed7/lib/char.s7i           0.033037            356
seed7/lib/charsets.s7i       0.032869           2024
seed7/lib/chartype.s7i       0.033678            121
seed7/lib/cipher.s7i         0.034550            146
seed7/lib/cli_cmds.s7i       0.034642           1360
seed7/lib/clib_file.s7i      0.033502            301
seed7/lib/color.s7i          0.033423            185
seed7/lib/complex.s7i        0.033200            464
seed7/lib/compress.s7i       0.032902            150
seed7/lib/console.s7i        0.032353            188
seed7/lib/cpio.s7i           0.032580           1708
seed7/lib/crc32.s7i          0.032568            193
seed7/lib/cronos16.s7i       0.032562           1173
seed7/lib/cronos27.s7i       0.032763           1464
seed7/lib/csv.s7i            0.032425            201
seed7/lib/db_prop.s7i        0.032217            991
seed7/lib/deflate.s7i        0.032282            740
seed7/lib/des.s7i            0.034510            444
seed7/lib/dialog.s7i         0.033595            311
seed7/lib/dir.s7i            0.033930            163
seed7/lib/draw.s7i           0.033319            854
seed7/lib/duration.s7i       0.033622           1038
seed7/lib/echo.s7i           0.033803            132
seed7/lib/editline.s7i       0.033414            398
seed7/lib/elf.s7i            0.034881           1560
seed7/lib/elliptic.s7i       0.033893            649
seed7/lib/enable_io.s7i      0.033222            312
seed7/lib/encoding.s7i       0.033270            931
seed7/lib/enumeration.s7i    0.033518            236
seed7/lib/environment.s7i    0.033816            175
seed7/lib/exif.s7i           0.032797            152
seed7/lib/external_file.s7i  0.033463            340
seed7/lib/field.s7i          0.033155            268
seed7/lib/file.s7i           0.034004            372
seed7/lib/filebits.s7i       0.033296             46
seed7/lib/filesys.s7i        0.033744            601
seed7/lib/fileutil.s7i       0.034821            144
seed7/lib/fixarray.s7i       0.033099            307
seed7/lib/float.s7i          0.034422            757
seed7/lib/font.s7i           0.033326            196
seed7/lib/font8x8.s7i        0.035512            998
seed7/lib/forloop.s7i        0.033677            449
seed7/lib/ftp.s7i            0.034391            969
seed7/lib/ftpserv.s7i        0.033258            631
seed7/lib/getf.s7i           0.034682            115
seed7/lib/gethttp.s7i        0.033789             41
seed7/lib/gethttps.s7i       0.033922             41
seed7/lib/gif.s7i            0.034261            561
seed7/lib/graph.s7i          0.033665            415
seed7/lib/graph_file.s7i     0.033037            399
seed7/lib/gtkserver.s7i      0.033616            161
seed7/lib/gzip.s7i           0.033631            573
seed7/lib/hash.s7i           0.033681            421
seed7/lib/hashsetof.s7i      0.033396            499
seed7/lib/hmac.s7i           0.033190            152
seed7/lib/html.s7i           0.033331             83
seed7/lib/html_ent.s7i       0.033269            476
seed7/lib/htmldom.s7i        0.033122            286
seed7/lib/http_request.s7i   0.033054            696
seed7/lib/http_srv_resp.s7i  0.033626            380
seed7/lib/https_request.s7i  0.032493            211
seed7/lib/httpserv.s7i       0.032454            345
seed7/lib/huffman.s7i        0.032665            644
seed7/lib/ico.s7i            0.033053            221
seed7/lib/idxarray.s7i       0.032781            232
seed7/lib/image.s7i          0.033264            156
seed7/lib/imagefile.s7i      0.034101            171
seed7/lib/inflate.s7i        0.033933            411
seed7/lib/inifile.s7i        0.034111            129
seed7/lib/integer.s7i        0.033858            663
seed7/lib/iobuffer.s7i       0.034010            289
seed7/lib/jpeg.s7i           0.034548           1761
seed7/lib/json.s7i           0.034754            891
seed7/lib/json_serde.s7i     0.034152            783
seed7/lib/keybd.s7i          0.033237            639
seed7/lib/keydescr.s7i       0.033731            192
seed7/lib/leb128.s7i         0.034372            218
seed7/lib/line.s7i           0.033757            164
seed7/lib/listener.s7i       0.034137            247
seed7/lib/logfile.s7i        0.033615             73
seed7/lib/lower.s7i          0.034075            142
seed7/lib/lzma.s7i           0.034255            934
seed7/lib/lzw.s7i            0.033520            861
seed7/lib/magic.s7i          0.033976            403
seed7/lib/mahjng32.s7i       0.034497           1500
seed7/lib/make.s7i           0.033526            544
seed7/lib/makedata.s7i       0.033249           1428
seed7/lib/math.s7i           0.033130            201
seed7/lib/mixarith.s7i       0.032944            249
seed7/lib/modern27.s7i       0.033305           1099
seed7/lib/more.s7i           0.032796            130
seed7/lib/msgdigest.s7i      0.032611           1222
seed7/lib/multiscr.s7i       0.032299             68
seed7/lib/null_file.s7i      0.033110            345
seed7/lib/osfiles.s7i        0.033148           1085
seed7/lib/pbm.s7i            0.032739            230
seed7/lib/pcx.s7i            0.035697            638
seed7/lib/pem.s7i            0.033532            185
seed7/lib/pgm.s7i            0.033833            238
seed7/lib/pic16.s7i          0.034132           1037
seed7/lib/pic32.s7i          0.033734           2060
seed7/lib/pic_util.s7i       0.033236            144
seed7/lib/pixelimage.s7i     0.033229            320
seed7/lib/pixmap_file.s7i    0.033749            459
seed7/lib/pixmapfont.s7i     0.033805            184
seed7/lib/pkcs1.s7i          0.033940            543
seed7/lib/png.s7i            0.034074           1064
seed7/lib/poll.s7i           0.033559            313
seed7/lib/ppm.s7i            0.034961            240
seed7/lib/process.s7i        0.033380            541
seed7/lib/progs.s7i          0.033759            789
seed7/lib/propertyfile.s7i   0.033295            155
seed7/lib/rational.s7i       0.034643            792
seed7/lib/ref_list.s7i       0.033640            252
seed7/lib/reference.s7i      0.034332            126
seed7/lib/reverse.s7i        0.033430             94
seed7/lib/rpm.s7i            0.033755           3487
seed7/lib/rpmext.s7i         0.032845            318
seed7/lib/scanfile.s7i       0.035114           1779
seed7/lib/scanjson.s7i       0.034711            413
seed7/lib/scanstri.s7i       0.033399           1814
seed7/lib/scantoml.s7i       0.034406           1603
seed7/lib/seed7_05.s7i       0.061562           1072
seed7/lib/set.s7i            0.034919             57
seed7/lib/shell.s7i          0.033528            615
seed7/lib/showtls.s7i        0.033044            678
seed7/lib/signature.s7i      0.032406            131
seed7/lib/smtp.s7i           0.034386            261
seed7/lib/sockbase.s7i       0.034090            217
seed7/lib/socket.s7i         0.034118            326
seed7/lib/sokoban1.s7i       0.033918           1519
seed7/lib/sql_base.s7i       0.033936           1000
seed7/lib/stars.s7i          0.033573           1705
seed7/lib/stdfont10.s7i      0.036147           3347
seed7/lib/stdfont12.s7i      0.034414           3928
seed7/lib/stdfont14.s7i      0.034515           4510
seed7/lib/stdfont16.s7i      0.037236           5092
seed7/lib/stdfont18.s7i      0.033997           5868
seed7/lib/stdfont20.s7i      0.034237           6449
seed7/lib/stdfont24.s7i      0.034207           7421
seed7/lib/stdfont8.s7i       0.034186           2960
seed7/lib/stdfont9.s7i       0.034654           3152
seed7/lib/stdio.s7i          0.034396            192
seed7/lib/strifile.s7i       0.034061            345
seed7/lib/string.s7i         0.034037            779
seed7/lib/stritext.s7i       0.033476            352
seed7/lib/struct.s7i         0.033575            266
seed7/lib/struct_elem.s7i    0.035026            129
seed7/lib/subfile.s7i        0.035616            174
seed7/lib/subrange.s7i       0.033975             78
seed7/lib/syntax.s7i         0.034145            294
seed7/lib/tar.s7i            0.034152           1880
seed7/lib/tar_cmds.s7i       0.033174            752
seed7/lib/tdes.s7i           0.032663            143
seed7/lib/tee.s7i            0.034645            143
seed7/lib/text.s7i           0.032392            135
seed7/lib/tga.s7i            0.032449            676
seed7/lib/tiff.s7i           0.034509           2771
seed7/lib/time.s7i           0.034054           1191
seed7/lib/tls.s7i            0.034314           2230
seed7/lib/unicode.s7i        0.033225            575
seed7/lib/unionfnd.s7i       0.033713            130
seed7/lib/upper.s7i          0.033213            142
seed7/lib/utf16.s7i          0.033278            540
seed7/lib/utf8.s7i           0.033534            234
seed7/lib/vecfont10.s7i      0.034241           1056
seed7/lib/vecfont18.s7i      0.033296           1119
seed7/lib/vector3d.s7i       0.033397            293
seed7/lib/vectorfont.s7i     0.033279            239
seed7/lib/wildcard.s7i       0.033072            140
seed7/lib/window.s7i         0.033249            455
seed7/lib/wrinum.s7i         0.032968            248
seed7/lib/x509cert.s7i       0.033184           1243
seed7/lib/xml_ent.s7i        0.034433             94
seed7/lib/xmldom.s7i         0.040294            303
seed7/lib/xz.s7i             0.036377            442
seed7/lib/zip.s7i            0.034716           2792
seed7/lib/zstd.s7i           0.033489           1263
============================ ================= ================

Statistical Summary — Mode A (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.033866        |
+-----------+-----------------+
| Minimum   | 0.032217        |
+-----------+-----------------+
| Maximum   | 0.061562        |
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
seed7/prg/addup.sd7          0.039316            190
seed7/prg/bas7.sd7           0.041519          11459
seed7/prg/bifurk.sd7         0.036591             73
seed7/prg/bigfiles.sd7       0.038154            129
seed7/prg/brainf7.sd7        0.037676             86
seed7/prg/calc7.sd7          0.038930            128
seed7/prg/carddemo.sd7       0.039365            190
seed7/prg/castle.sd7         0.039374           3148
seed7/prg/cat.sd7            0.038140             82
seed7/prg/cellauto.sd7       0.037280             85
seed7/prg/celsius.sd7        0.035248             42
seed7/prg/chk_all.sd7        0.037893            843
seed7/prg/chkarr.sd7         0.038083           8367
seed7/prg/chkbig.sd7         0.042736          29026
seed7/prg/chkbin.sd7         0.039668           6469
seed7/prg/chkbitdata.sd7     0.040373           6624
seed7/prg/chkbool.sd7        0.038704           3157
seed7/prg/chkbst.sd7         0.039611            722
seed7/prg/chkchr.sd7         0.040770           2809
seed7/prg/chkcmd.sd7         0.039698           1205
seed7/prg/chkdb.sd7          0.039752           7454
seed7/prg/chkdecl.sd7        0.039876            448
seed7/prg/chkenum.sd7        0.038814           1230
seed7/prg/chkerr.sd7         0.039919           4663
seed7/prg/chkexc.sd7         0.038943           2627
seed7/prg/chkfil.sd7         0.042821           1615
seed7/prg/chkflt.sd7         0.044244          20620
seed7/prg/chkhent.sd7        0.036610             54
seed7/prg/chkhsh.sd7         0.040293           4548
seed7/prg/chkidx.sd7         0.043402          19567
seed7/prg/chkint.sd7         0.048954          38129
seed7/prg/chkjson.sd7        0.042149           1764
seed7/prg/chkovf.sd7         0.039786           8216
seed7/prg/chkprc.sd7         0.038216          10111
seed7/prg/chkscan.sd7        0.038992            714
seed7/prg/chkset.sd7         0.040396          11974
seed7/prg/chkstr.sd7         0.042639          26952
seed7/prg/chktime.sd7        0.038584           2025
seed7/prg/chktoml.sd7        0.038300           1656
seed7/prg/clock.sd7          0.035012             47
seed7/prg/clock2.sd7         0.035781             43
seed7/prg/clock3.sd7         0.039795             95
seed7/prg/cmpfil.sd7         0.037263             84
seed7/prg/comanche.sd7       0.039890            180
seed7/prg/confval.sd7        0.039022            175
seed7/prg/db7.sd7            0.037393            417
seed7/prg/diff7.sd7          0.037844            263
seed7/prg/dirtst.sd7         0.035706             42
seed7/prg/dirx.sd7           0.037026            152
seed7/prg/dnafight.sd7       0.037960           1381
seed7/prg/dragon.sd7         0.035523             73
seed7/prg/echo.sd7           0.034564             39
seed7/prg/eliza.sd7          0.037584            302
seed7/prg/err.sd7            0.042996             96
seed7/prg/fannkuch.sd7       0.042092            131
seed7/prg/fib.sd7            0.038389             47
seed7/prg/find7.sd7          0.039415            133
seed7/prg/findchar.sd7       0.039623            149
seed7/prg/fractree.sd7       0.035920             55
seed7/prg/ftp7.sd7           0.039181            296
seed7/prg/ftpserv.sd7        0.037664             74
seed7/prg/gcd.sd7            0.038123            109
seed7/prg/gkbd.sd7           0.041224            358
seed7/prg/gtksvtst.sd7       0.036981             94
seed7/prg/hal.sd7            0.037363            250
seed7/prg/hamu.sd7           0.037860            573
seed7/prg/hanoi.sd7          0.036020             55
seed7/prg/hd.sd7             0.036039             79
seed7/prg/hello.sd7          0.035308             32
seed7/prg/hilbert.sd7        0.039877            108
seed7/prg/ide7.sd7           0.039703            196
seed7/prg/kbd.sd7            0.036772             49
seed7/prg/klondike.sd7       0.038846            883
seed7/prg/lander.sd7         0.039591           1551
seed7/prg/lst80bas.sd7       0.038185            344
seed7/prg/lst99bas.sd7       0.039857            401
seed7/prg/lstgwbas.sd7       0.039906            577
seed7/prg/mahjong.sd7        0.039491           1943
seed7/prg/make7.sd7          0.038918            121
seed7/prg/mandelbr.sd7       0.039493            237
seed7/prg/mind.sd7           0.040515            443
seed7/prg/mirror.sd7         0.040444            131
seed7/prg/ms.sd7             0.039366            641
seed7/prg/nicoma.sd7         0.038360            135
seed7/prg/pac.sd7            0.044095            726
seed7/prg/pairs.sd7          0.042179           2025
seed7/prg/panic.sd7          0.038368           2634
seed7/prg/percolation.sd7    0.039156            330
seed7/prg/planets.sd7        0.040140           1486
seed7/prg/portfwd7.sd7       0.039091            139
seed7/prg/prime.sd7          0.035704             74
seed7/prg/printpi1.sd7       0.035531             56
seed7/prg/printpi2.sd7       0.036756             54
seed7/prg/printpi3.sd7       0.035874             60
seed7/prg/pv7.sd7            0.038226            337
seed7/prg/queen.sd7          0.038972            149
seed7/prg/rand.sd7           0.038116            121
seed7/prg/raytrace.sd7       0.038990            538
seed7/prg/rever.sd7          0.039367            816
seed7/prg/roman.sd7          0.035609             38
seed7/prg/s7c.sd7            0.039308           9060
seed7/prg/s7check.sd7        0.036932             68
seed7/prg/savehd7.sd7        0.039499           1110
seed7/prg/self.sd7           0.036853             49
seed7/prg/shisen.sd7         0.038842           1423
seed7/prg/sl.sd7             0.038499           1029
seed7/prg/snake.sd7          0.038909            615
seed7/prg/sokoban.sd7        0.038613            891
seed7/prg/spigotpi.sd7       0.036779             64
seed7/prg/sql7.sd7           0.038500            278
seed7/prg/startrek.sd7       0.038666            979
seed7/prg/sudoku7.sd7        0.039095           2657
seed7/prg/sydir7.sd7         0.039083            384
seed7/prg/syntaxhl.sd7       0.041520            177
seed7/prg/tak.sd7            0.036523             59
seed7/prg/tar7.sd7           0.038695            121
seed7/prg/tch.sd7            0.035546             55
seed7/prg/testfont.sd7       0.038415             95
seed7/prg/tet.sd7            0.038714            479
seed7/prg/tetg.sd7           0.037694            501
seed7/prg/toutf8.sd7         0.039192            240
seed7/prg/tst_cli.sd7        0.036386             40
seed7/prg/tst_srv.sd7        0.036781             47
seed7/prg/wator.sd7          0.038577            651
seed7/prg/which.sd7          0.036637             65
seed7/prg/wiz.sd7            0.038525           2833
seed7/prg/wordcnt.sd7        0.038631             54
seed7/prg/wrinum.sd7         0.039205             43
seed7/prg/wumpus.sd7         0.041918            372
seed7/lib/aes.s7i            0.042240           1144
seed7/lib/aes_gcm.s7i        0.039956            392
seed7/lib/ar.s7i             0.042562           1532
seed7/lib/arc4.s7i           0.043424            144
seed7/lib/archive.s7i        0.042709            143
seed7/lib/archive_base.s7i   0.039073            135
seed7/lib/array.s7i          0.038789            610
seed7/lib/asn1.s7i           0.036947            544
seed7/lib/asn1oid.s7i        0.044786            157
seed7/lib/basearray.s7i      0.043638            450
seed7/lib/bigfile.s7i        0.042774            136
seed7/lib/bigint.s7i         0.043270            824
seed7/lib/bigrat.s7i         0.042559            784
seed7/lib/bin16.s7i          0.043061            592
seed7/lib/bin32.s7i          0.041817            490
seed7/lib/bin64.s7i          0.040219            539
seed7/lib/bitdata.s7i        0.046454           1330
seed7/lib/bitmapfont.s7i     0.042065            215
seed7/lib/bitset.s7i         0.045472            593
seed7/lib/bitsetof.s7i       0.045957            431
seed7/lib/blowfish.s7i       0.045148            383
seed7/lib/bmp.s7i            0.043564            924
seed7/lib/boolean.s7i        0.042222            403
seed7/lib/browser.s7i        0.043446            280
seed7/lib/bstring.s7i        0.042036            227
seed7/lib/bytedata.s7i       0.040567            482
seed7/lib/bzip2.s7i          0.042541            887
seed7/lib/cards.s7i          0.038400           1342
seed7/lib/category.s7i       0.038915            209
seed7/lib/cc_conf.s7i        0.038280           1314
seed7/lib/ccittfax.s7i       0.039140           1022
seed7/lib/cgi.s7i            0.038050            109
seed7/lib/cgidialog.s7i      0.038679           1118
seed7/lib/char.s7i           0.038679            356
seed7/lib/charsets.s7i       0.039450           2024
seed7/lib/chartype.s7i       0.045884            121
seed7/lib/cipher.s7i         0.043779            146
seed7/lib/cli_cmds.s7i       0.041514           1360
seed7/lib/clib_file.s7i      0.038906            301
seed7/lib/color.s7i          0.038272            185
seed7/lib/complex.s7i        0.038484            464
seed7/lib/compress.s7i       0.038285            150
seed7/lib/console.s7i        0.037876            188
seed7/lib/cpio.s7i           0.038679           1708
seed7/lib/crc32.s7i          0.039496            193
seed7/lib/cronos16.s7i       0.043867           1173
seed7/lib/cronos27.s7i       0.046112           1464
seed7/lib/csv.s7i            0.042542            201
seed7/lib/db_prop.s7i        0.043515            991
seed7/lib/deflate.s7i        0.043693            740
seed7/lib/des.s7i            0.043022            444
seed7/lib/dialog.s7i         0.042901            311
seed7/lib/dir.s7i            0.040428            163
seed7/lib/draw.s7i           0.041165            854
seed7/lib/duration.s7i       0.042549           1038
seed7/lib/echo.s7i           0.043772            132
seed7/lib/editline.s7i       0.038772            398
seed7/lib/elf.s7i            0.040486           1560
seed7/lib/elliptic.s7i       0.038625            649
seed7/lib/enable_io.s7i      0.038431            312
seed7/lib/encoding.s7i       0.042701            931
seed7/lib/enumeration.s7i    0.043752            236
seed7/lib/environment.s7i    0.042698            175
seed7/lib/exif.s7i           0.044405            152
seed7/lib/external_file.s7i  0.041940            340
seed7/lib/field.s7i          0.041894            268
seed7/lib/file.s7i           0.039719            372
seed7/lib/filebits.s7i       0.037722             46
seed7/lib/filesys.s7i        0.037722            601
seed7/lib/fileutil.s7i       0.037626            144
seed7/lib/fixarray.s7i       0.044691            307
seed7/lib/float.s7i          0.043632            757
seed7/lib/font.s7i           0.039915            196
seed7/lib/font8x8.s7i        0.040796            998
seed7/lib/forloop.s7i        0.042839            449
seed7/lib/ftp.s7i            0.039110            969
seed7/lib/ftpserv.s7i        0.039385            631
seed7/lib/getf.s7i           0.042995            115
seed7/lib/gethttp.s7i        0.037791             41
seed7/lib/gethttps.s7i       0.036798             41
seed7/lib/gif.s7i            0.038994            561
seed7/lib/graph.s7i          0.042204            415
seed7/lib/graph_file.s7i     0.039321            399
seed7/lib/gtkserver.s7i      0.039167            161
seed7/lib/gzip.s7i           0.040021            573
seed7/lib/hash.s7i           0.040650            421
seed7/lib/hashsetof.s7i      0.041161            499
seed7/lib/hmac.s7i           0.039435            152
seed7/lib/html.s7i           0.038237             83
seed7/lib/html_ent.s7i       0.039324            476
seed7/lib/htmldom.s7i        0.040054            286
seed7/lib/http_request.s7i   0.038483            696
seed7/lib/http_srv_resp.s7i  0.038637            380
seed7/lib/https_request.s7i  0.037709            211
seed7/lib/httpserv.s7i       0.037655            345
seed7/lib/huffman.s7i        0.039204            644
seed7/lib/ico.s7i            0.040287            221
seed7/lib/idxarray.s7i       0.039019            232
seed7/lib/image.s7i          0.038098            156
seed7/lib/imagefile.s7i      0.039147            171
seed7/lib/inflate.s7i        0.038991            411
seed7/lib/inifile.s7i        0.038933            129
seed7/lib/integer.s7i        0.038994            663
seed7/lib/iobuffer.s7i       0.039431            289
seed7/lib/jpeg.s7i           0.039563           1761
seed7/lib/json.s7i           0.039171            891
seed7/lib/json_serde.s7i     0.039456            783
seed7/lib/keybd.s7i          0.038274            639
seed7/lib/keydescr.s7i       0.042221            192
seed7/lib/leb128.s7i         0.039486            218
seed7/lib/line.s7i           0.038933            164
seed7/lib/listener.s7i       0.039549            247
seed7/lib/logfile.s7i        0.037168             73
seed7/lib/lower.s7i          0.038746            142
seed7/lib/lzma.s7i           0.039720            934
seed7/lib/lzw.s7i            0.039510            861
seed7/lib/magic.s7i          0.042500            403
seed7/lib/mahjng32.s7i       0.037498           1500
seed7/lib/make.s7i           0.038154            544
seed7/lib/makedata.s7i       0.037992           1428
seed7/lib/math.s7i           0.037519            201
seed7/lib/mixarith.s7i       0.039344            249
seed7/lib/modern27.s7i       0.043467           1099
seed7/lib/more.s7i           0.039292            130
seed7/lib/msgdigest.s7i      0.040047           1222
seed7/lib/multiscr.s7i       0.036950             68
seed7/lib/null_file.s7i      0.038600            345
seed7/lib/osfiles.s7i        0.040415           1085
seed7/lib/pbm.s7i            0.039226            230
seed7/lib/pcx.s7i            0.039271            638
seed7/lib/pem.s7i            0.038519            185
seed7/lib/pgm.s7i            0.038679            238
seed7/lib/pic16.s7i          0.039243           1037
seed7/lib/pic32.s7i          0.038129           2060
seed7/lib/pic_util.s7i       0.039148            144
seed7/lib/pixelimage.s7i     0.039285            320
seed7/lib/pixmap_file.s7i    0.039750            459
seed7/lib/pixmapfont.s7i     0.040235            184
seed7/lib/pkcs1.s7i          0.044047            543
seed7/lib/png.s7i            0.039775           1064
seed7/lib/poll.s7i           0.039107            313
seed7/lib/ppm.s7i            0.039673            240
seed7/lib/process.s7i        0.038449            541
seed7/lib/progs.s7i          0.037984            789
seed7/lib/propertyfile.s7i   0.037726            155
seed7/lib/rational.s7i       0.038406            792
seed7/lib/ref_list.s7i       0.038215            252
seed7/lib/reference.s7i      0.040472            126
seed7/lib/reverse.s7i        0.037827             94
seed7/lib/rpm.s7i            0.039399           3487
seed7/lib/rpmext.s7i         0.039899            318
seed7/lib/scanfile.s7i       0.039347           1779
seed7/lib/scanjson.s7i       0.039942            413
seed7/lib/scanstri.s7i       0.039159           1814
seed7/lib/scantoml.s7i       0.040412           1603
seed7/lib/seed7_05.s7i       0.041460           1072
seed7/lib/set.s7i            0.037507             57
seed7/lib/shell.s7i          0.040325            615
seed7/lib/showtls.s7i        0.039631            678
seed7/lib/signature.s7i      0.039432            131
seed7/lib/smtp.s7i           0.039508            261
seed7/lib/sockbase.s7i       0.041073            217
seed7/lib/socket.s7i         0.039405            326
seed7/lib/sokoban1.s7i       0.039469           1519
seed7/lib/sql_base.s7i       0.039679           1000
seed7/lib/stars.s7i          0.041672           1705
seed7/lib/stdfont10.s7i      0.037503           3347
seed7/lib/stdfont12.s7i      0.039281           3928
seed7/lib/stdfont14.s7i      0.040088           4510
seed7/lib/stdfont16.s7i      0.038008           5092
seed7/lib/stdfont18.s7i      0.039714           5868
seed7/lib/stdfont20.s7i      0.038071           6449
seed7/lib/stdfont24.s7i      0.038646           7421
seed7/lib/stdfont8.s7i       0.038427           2960
seed7/lib/stdfont9.s7i       0.037623           3152
seed7/lib/stdio.s7i          0.038594            192
seed7/lib/strifile.s7i       0.038987            345
seed7/lib/string.s7i         0.038643            779
seed7/lib/stritext.s7i       0.039217            352
seed7/lib/struct.s7i         0.040191            266
seed7/lib/struct_elem.s7i    0.038131            129
seed7/lib/subfile.s7i        0.038209            174
seed7/lib/subrange.s7i       0.037417             78
seed7/lib/syntax.s7i         0.039296            294
seed7/lib/tar.s7i            0.039263           1880
seed7/lib/tar_cmds.s7i       0.039226            752
seed7/lib/tdes.s7i           0.038497            143
seed7/lib/tee.s7i            0.038307            143
seed7/lib/text.s7i           0.038915            135
seed7/lib/tga.s7i            0.041043            676
seed7/lib/tiff.s7i           0.041033           2771
seed7/lib/time.s7i           0.039130           1191
seed7/lib/tls.s7i            0.039409           2230
seed7/lib/unicode.s7i        0.041442            575
seed7/lib/unionfnd.s7i       0.038741            130
seed7/lib/upper.s7i          0.038890            142
seed7/lib/utf16.s7i          0.038125            540
seed7/lib/utf8.s7i           0.039039            234
seed7/lib/vecfont10.s7i      0.041110           1056
seed7/lib/vecfont18.s7i      0.041134           1119
seed7/lib/vector3d.s7i       0.039249            293
seed7/lib/vectorfont.s7i     0.039206            239
seed7/lib/wildcard.s7i       0.040575            140
seed7/lib/window.s7i         0.038859            455
seed7/lib/wrinum.s7i         0.042339            248
seed7/lib/x509cert.s7i       0.040619           1243
seed7/lib/xml_ent.s7i        0.038251             94
seed7/lib/xmldom.s7i         0.038467            303
seed7/lib/xz.s7i             0.039615            442
seed7/lib/zip.s7i            0.039772           2792
seed7/lib/zstd.s7i           0.040829           1263
============================ ================= ================

Statistical Summary — Mode B (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.039602        |
+-----------+-----------------+
| Minimum   | 0.034564        |
+-----------+-----------------+
| Maximum   | 0.048954        |
+-----------+-----------------+

Mode C — Mode Activation + Full-Buffer Fontification (font-lock-ensure)
-----------------------------------------------------------------------

Full-buffer fontification stress test.

File Load Times — Mode C (mean, GC-free)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

============================ ================= ================
File Name                    Mean Time (s)     Line count
============================ ================= ================
seed7/prg/addup.sd7          0.041893            190
seed7/prg/bas7.sd7           0.328465          11459
seed7/prg/bifurk.sd7         0.037808             73
seed7/prg/bigfiles.sd7       0.039704            129
seed7/prg/brainf7.sd7        0.038391             86
seed7/prg/calc7.sd7          0.041786            128
seed7/prg/carddemo.sd7       0.041063            190
seed7/prg/castle.sd7         0.106669           3148
seed7/prg/cat.sd7            0.038702             82
seed7/prg/cellauto.sd7       0.049529             85
seed7/prg/celsius.sd7        0.036274             42
seed7/prg/chk_all.sd7        0.062137            843
seed7/prg/chkarr.sd7         0.357908           8367
seed7/prg/chkbig.sd7         2.059696          29026
seed7/prg/chkbin.sd7         0.505256           6469
seed7/prg/chkbitdata.sd7     0.605683           6624
seed7/prg/chkbool.sd7        0.119160           3157
seed7/prg/chkbst.sd7         0.062608            722
seed7/prg/chkchr.sd7         0.216743           2809
seed7/prg/chkcmd.sd7         0.064872           1205
seed7/prg/chkdb.sd7          0.345043           7454
seed7/prg/chkdecl.sd7        0.057459            448
seed7/prg/chkenum.sd7        0.066789           1230
seed7/prg/chkerr.sd7         0.191841           4663
seed7/prg/chkexc.sd7         0.082331           2627
seed7/prg/chkfil.sd7         0.075873           1615
seed7/prg/chkflt.sd7         1.331720          20620
seed7/prg/chkhent.sd7        0.037376             54
seed7/prg/chkhsh.sd7         0.244449           4548
seed7/prg/chkidx.sd7         1.304328          19567
seed7/prg/chkint.sd7         2.502191          38129
seed7/prg/chkjson.sd7        0.100377           1764
seed7/prg/chkovf.sd7         0.570728           8216
seed7/prg/chkprc.sd7         0.322333          10111
seed7/prg/chkscan.sd7        0.054571            714
seed7/prg/chkset.sd7         0.668381          11974
seed7/prg/chkstr.sd7         1.410674          26952
seed7/prg/chktime.sd7        0.134517           2025
seed7/prg/chktoml.sd7        0.109705           1656
seed7/prg/clock.sd7          0.035680             47
seed7/prg/clock2.sd7         0.034963             43
seed7/prg/clock3.sd7         0.037032             95
seed7/prg/cmpfil.sd7         0.041605             84
seed7/prg/comanche.sd7       0.044108            180
seed7/prg/confval.sd7        0.042509            175
seed7/prg/db7.sd7            0.046211            417
seed7/prg/diff7.sd7          0.042167            263
seed7/prg/dirtst.sd7         0.035212             42
seed7/prg/dirx.sd7           0.038043            152
seed7/prg/dnafight.sd7       0.069310           1381
seed7/prg/dragon.sd7         0.037544             73
seed7/prg/echo.sd7           0.035675             39
seed7/prg/eliza.sd7          0.042808            302
seed7/prg/err.sd7            0.042488             96
seed7/prg/fannkuch.sd7       0.037841            131
seed7/prg/fib.sd7            0.035599             47
seed7/prg/find7.sd7          0.036712            133
seed7/prg/findchar.sd7       0.037261            149
seed7/prg/fractree.sd7       0.037166             55
seed7/prg/ftp7.sd7           0.040970            296
seed7/prg/ftpserv.sd7        0.035397             74
seed7/prg/gcd.sd7            0.037904            109
seed7/prg/gkbd.sd7           0.047844            358
seed7/prg/gtksvtst.sd7       0.037719             94
seed7/prg/hal.sd7            0.039603            250
seed7/prg/hamu.sd7           0.048213            573
seed7/prg/hanoi.sd7          0.035899             55
seed7/prg/hd.sd7             0.036610             79
seed7/prg/hello.sd7          0.035309             32
seed7/prg/hilbert.sd7        0.038833            108
seed7/prg/ide7.sd7           0.039664            196
seed7/prg/kbd.sd7            0.035840             49
seed7/prg/klondike.sd7       0.054915            883
seed7/prg/lander.sd7         0.070843           1551
seed7/prg/lst80bas.sd7       0.043732            344
seed7/prg/lst99bas.sd7       0.044788            401
seed7/prg/lstgwbas.sd7       0.050268            577
seed7/prg/mahjong.sd7        0.081537           1943
seed7/prg/make7.sd7          0.037815            121
seed7/prg/mandelbr.sd7       0.039976            237
seed7/prg/mind.sd7           0.044051            443
seed7/prg/mirror.sd7         0.037719            131
seed7/prg/ms.sd7             0.047556            641
seed7/prg/nicoma.sd7         0.037044            135
seed7/prg/pac.sd7            0.048365            726
seed7/prg/pairs.sd7          0.086290           2025
seed7/prg/panic.sd7          0.097290           2634
seed7/prg/percolation.sd7    0.043044            330
seed7/prg/planets.sd7        0.076196           1486
seed7/prg/portfwd7.sd7       0.040517            139
seed7/prg/prime.sd7          0.037421             74
seed7/prg/printpi1.sd7       0.035810             56
seed7/prg/printpi2.sd7       0.035863             54
seed7/prg/printpi3.sd7       0.035704             60
seed7/prg/pv7.sd7            0.042574            337
seed7/prg/queen.sd7          0.036727            149
seed7/prg/rand.sd7           0.036044            121
seed7/prg/raytrace.sd7       0.046631            538
seed7/prg/rever.sd7          0.052881            816
seed7/prg/roman.sd7          0.035312             38
seed7/prg/s7c.sd7            0.275420           9060
seed7/prg/s7check.sd7        0.035739             68
seed7/prg/savehd7.sd7        0.065653           1110
seed7/prg/self.sd7           0.036995             49
seed7/prg/shisen.sd7         0.072471           1423
seed7/prg/sl.sd7             0.059561           1029
seed7/prg/snake.sd7          0.046654            615
seed7/prg/sokoban.sd7        0.053982            891
seed7/prg/spigotpi.sd7       0.035721             64
seed7/prg/sql7.sd7           0.040925            278
seed7/prg/startrek.sd7       0.058506            979
seed7/prg/sudoku7.sd7        0.099958           2657
seed7/prg/sydir7.sd7         0.045500            384
seed7/prg/syntaxhl.sd7       0.040635            177
seed7/prg/tak.sd7            0.035586             59
seed7/prg/tar7.sd7           0.037272            121
seed7/prg/tch.sd7            0.035672             55
seed7/prg/testfont.sd7       0.037193             95
seed7/prg/tet.sd7            0.044887            479
seed7/prg/tetg.sd7           0.045615            501
seed7/prg/toutf8.sd7         0.041256            240
seed7/prg/tst_cli.sd7        0.034137             40
seed7/prg/tst_srv.sd7        0.034105             47
seed7/prg/wator.sd7          0.050009            651
seed7/prg/which.sd7          0.035523             65
seed7/prg/wiz.sd7            0.106019           2833
seed7/prg/wordcnt.sd7        0.035684             54
seed7/prg/wrinum.sd7         0.035141             43
seed7/prg/wumpus.sd7         0.042357            372
seed7/lib/aes.s7i            0.108558           1144
seed7/lib/aes_gcm.s7i        0.045754            392
seed7/lib/ar.s7i             0.072401           1532
seed7/lib/arc4.s7i           0.037990            144
seed7/lib/archive.s7i        0.038297            143
seed7/lib/archive_base.s7i   0.038179            135
seed7/lib/array.s7i          0.053745            610
seed7/lib/asn1.s7i           0.046812            544
seed7/lib/asn1oid.s7i        0.041183            157
seed7/lib/basearray.s7i      0.048286            450
seed7/lib/bigfile.s7i        0.037911            136
seed7/lib/bigint.s7i         0.055580            824
seed7/lib/bigrat.s7i         0.052881            784
seed7/lib/bin16.s7i          0.049045            592
seed7/lib/bin32.s7i          0.046131            490
seed7/lib/bin64.s7i          0.048426            539
seed7/lib/bitdata.s7i        0.075585           1330
seed7/lib/bitmapfont.s7i     0.040919            215
seed7/lib/bitset.s7i         0.048587            593
seed7/lib/bitsetof.s7i       0.049077            431
seed7/lib/blowfish.s7i       0.055739            383
seed7/lib/bmp.s7i            0.064170            924
seed7/lib/boolean.s7i        0.044721            403
seed7/lib/browser.s7i        0.042591            280
seed7/lib/bstring.s7i        0.041427            227
seed7/lib/bytedata.s7i       0.049826            482
seed7/lib/bzip2.s7i          0.059068            887
seed7/lib/cards.s7i          0.066704           1342
seed7/lib/category.s7i       0.040872            209
seed7/lib/cc_conf.s7i        0.077453           1314
seed7/lib/ccittfax.s7i       0.064838           1022
seed7/lib/cgi.s7i            0.037276            109
seed7/lib/cgidialog.s7i      0.059431           1118
seed7/lib/char.s7i           0.041806            356
seed7/lib/charsets.s7i       0.079540           2024
seed7/lib/chartype.s7i       0.040356            121
seed7/lib/cipher.s7i         0.037310            146
seed7/lib/cli_cmds.s7i       0.069863           1360
seed7/lib/clib_file.s7i      0.043295            301
seed7/lib/color.s7i          0.040412            185
seed7/lib/complex.s7i        0.044993            464
seed7/lib/compress.s7i       0.037931            150
seed7/lib/console.s7i        0.039023            188
seed7/lib/cpio.s7i           0.081637           1708
seed7/lib/crc32.s7i          0.043533            193
seed7/lib/cronos16.s7i       0.092634           1173
seed7/lib/cronos27.s7i       0.116332           1464
seed7/lib/csv.s7i            0.040243            201
seed7/lib/db_prop.s7i        0.063362            991
seed7/lib/deflate.s7i        0.055023            740
seed7/lib/des.s7i            0.054605            444
seed7/lib/dialog.s7i         0.044343            311
seed7/lib/dir.s7i            0.037810            163
seed7/lib/draw.s7i           0.054688            854
seed7/lib/duration.s7i       0.058837           1038
seed7/lib/echo.s7i           0.036990            132
seed7/lib/editline.s7i       0.046273            398
seed7/lib/elf.s7i            0.084075           1560
seed7/lib/elliptic.s7i       0.051892            649
seed7/lib/enable_io.s7i      0.043596            312
seed7/lib/encoding.s7i       0.060755            931
seed7/lib/enumeration.s7i    0.041146            236
seed7/lib/environment.s7i    0.039149            175
seed7/lib/exif.s7i           0.039060            152
seed7/lib/external_file.s7i  0.044985            340
seed7/lib/field.s7i          0.042721            268
seed7/lib/file.s7i           0.043701            372
seed7/lib/filebits.s7i       0.035885             46
seed7/lib/filesys.s7i        0.048226            601
seed7/lib/fileutil.s7i       0.037863            144
seed7/lib/fixarray.s7i       0.043094            307
seed7/lib/float.s7i          0.054710            757
seed7/lib/font.s7i           0.039791            196
seed7/lib/font8x8.s7i        0.048166            998
seed7/lib/forloop.s7i        0.043956            449
seed7/lib/ftp.s7i            0.055677            969
seed7/lib/ftpserv.s7i        0.049701            631
seed7/lib/getf.s7i           0.035748            115
seed7/lib/gethttp.s7i        0.034559             41
seed7/lib/gethttps.s7i       0.036671             41
seed7/lib/gif.s7i            0.049707            561
seed7/lib/graph.s7i          0.048894            415
seed7/lib/graph_file.s7i     0.044215            399
seed7/lib/gtkserver.s7i      0.038118            161
seed7/lib/gzip.s7i           0.048532            573
seed7/lib/hash.s7i           0.049186            421
seed7/lib/hashsetof.s7i      0.048341            499
seed7/lib/hmac.s7i           0.038850            152
seed7/lib/html.s7i           0.036654             83
seed7/lib/html_ent.s7i       0.046910            476
seed7/lib/htmldom.s7i        0.043784            286
seed7/lib/http_request.s7i   0.052452            696
seed7/lib/http_srv_resp.s7i  0.046234            380
seed7/lib/https_request.s7i  0.040032            211
seed7/lib/httpserv.s7i       0.043500            345
seed7/lib/huffman.s7i        0.052349            644
seed7/lib/ico.s7i            0.041996            221
seed7/lib/idxarray.s7i       0.042108            232
seed7/lib/image.s7i          0.037173            156
seed7/lib/imagefile.s7i      0.038511            171
seed7/lib/inflate.s7i        0.045556            411
seed7/lib/inifile.s7i        0.036614            129
seed7/lib/integer.s7i        0.050541            663
seed7/lib/iobuffer.s7i       0.041583            289
seed7/lib/jpeg.s7i           0.083421           1761
seed7/lib/json.s7i           0.055267            891
seed7/lib/json_serde.s7i     0.052878            783
seed7/lib/keybd.s7i          0.054941            639
seed7/lib/keydescr.s7i       0.043036            192
seed7/lib/leb128.s7i         0.039614            218
seed7/lib/line.s7i           0.038723            164
seed7/lib/listener.s7i       0.041012            247
seed7/lib/logfile.s7i        0.036367             73
seed7/lib/lower.s7i          0.037435            142
seed7/lib/lzma.s7i           0.058508            934
seed7/lib/lzw.s7i            0.058174            861
seed7/lib/magic.s7i          0.047709            403
seed7/lib/mahjng32.s7i       0.064348           1500
seed7/lib/make.s7i           0.049932            544
seed7/lib/makedata.s7i       0.069471           1428
seed7/lib/math.s7i           0.038631            201
seed7/lib/mixarith.s7i       0.038365            249
seed7/lib/modern27.s7i       0.081727           1099
seed7/lib/more.s7i           0.037561            130
seed7/lib/msgdigest.s7i      0.078045           1222
seed7/lib/multiscr.s7i       0.036529             68
seed7/lib/null_file.s7i      0.042705            345
seed7/lib/osfiles.s7i        0.064727           1085
seed7/lib/pbm.s7i            0.040116            230
seed7/lib/pcx.s7i            0.051504            638
seed7/lib/pem.s7i            0.038211            185
seed7/lib/pgm.s7i            0.039349            238
seed7/lib/pic16.s7i          0.048034           1037
seed7/lib/pic32.s7i          0.079140           2060
seed7/lib/pic_util.s7i       0.037331            144
seed7/lib/pixelimage.s7i     0.040920            320
seed7/lib/pixmap_file.s7i    0.044133            459
seed7/lib/pixmapfont.s7i     0.039500            184
seed7/lib/pkcs1.s7i          0.063152            543
seed7/lib/png.s7i            0.063927           1064
seed7/lib/poll.s7i           0.042537            313
seed7/lib/ppm.s7i            0.040878            240
seed7/lib/process.s7i        0.049122            541
seed7/lib/progs.s7i          0.055337            789
seed7/lib/propertyfile.s7i   0.041477            155
seed7/lib/rational.s7i       0.053143            792
seed7/lib/ref_list.s7i       0.041860            252
seed7/lib/reference.s7i      0.038072            126
seed7/lib/reverse.s7i        0.036926             94
seed7/lib/rpm.s7i            0.143058           3487
seed7/lib/rpmext.s7i         0.044398            318
seed7/lib/scanfile.s7i       0.080093           1779
seed7/lib/scanjson.s7i       0.046791            413
seed7/lib/scanstri.s7i       0.080897           1814
seed7/lib/scantoml.s7i       0.071313           1603
seed7/lib/seed7_05.s7i       0.066487           1072
seed7/lib/set.s7i            0.035900             57
seed7/lib/shell.s7i          0.052116            615
seed7/lib/showtls.s7i        0.053870            678
seed7/lib/signature.s7i      0.037998            131
seed7/lib/smtp.s7i           0.039117            261
seed7/lib/sockbase.s7i       0.040729            217
seed7/lib/socket.s7i         0.044352            326
seed7/lib/sokoban1.s7i       0.053935           1519
seed7/lib/sql_base.s7i       0.063436           1000
seed7/lib/stars.s7i          0.133388           1705
seed7/lib/stdfont10.s7i      0.080646           3347
seed7/lib/stdfont12.s7i      0.092178           3928
seed7/lib/stdfont14.s7i      0.104897           4510
seed7/lib/stdfont16.s7i      0.114199           5092
seed7/lib/stdfont18.s7i      0.136016           5868
seed7/lib/stdfont20.s7i      0.147318           6449
seed7/lib/stdfont24.s7i      0.181782           7421
seed7/lib/stdfont8.s7i       0.073114           2960
seed7/lib/stdfont9.s7i       0.076043           3152
seed7/lib/stdio.s7i          0.040694            192
seed7/lib/strifile.s7i       0.043037            345
seed7/lib/string.s7i         0.061169            779
seed7/lib/stritext.s7i       0.050099            352
seed7/lib/struct.s7i         0.058996            266
seed7/lib/struct_elem.s7i    0.037863            129
seed7/lib/subfile.s7i        0.037572            174
seed7/lib/subrange.s7i       0.035857             78
seed7/lib/syntax.s7i         0.044446            294
seed7/lib/tar.s7i            0.079846           1880
seed7/lib/tar_cmds.s7i       0.056678            752
seed7/lib/tdes.s7i           0.037074            143
seed7/lib/tee.s7i            0.037752            143
seed7/lib/text.s7i           0.037561            135
seed7/lib/tga.s7i            0.052902            676
seed7/lib/tiff.s7i           0.119766           2771
seed7/lib/time.s7i           0.063974           1191
seed7/lib/tls.s7i            0.102760           2230
seed7/lib/unicode.s7i        0.052024            575
seed7/lib/unionfnd.s7i       0.037148            130
seed7/lib/upper.s7i          0.036914            142
seed7/lib/utf16.s7i          0.048463            540
seed7/lib/utf8.s7i           0.039971            234
seed7/lib/vecfont10.s7i      0.080460           1056
seed7/lib/vecfont18.s7i      0.087177           1119
seed7/lib/vector3d.s7i       0.040737            293
seed7/lib/vectorfont.s7i     0.040527            239
seed7/lib/wildcard.s7i       0.038924            140
seed7/lib/window.s7i         0.045021            455
seed7/lib/wrinum.s7i         0.040893            248
seed7/lib/x509cert.s7i       0.071705           1243
seed7/lib/xml_ent.s7i        0.036930             94
seed7/lib/xmldom.s7i         0.039987            303
seed7/lib/xz.s7i             0.044252            442
seed7/lib/zip.s7i            0.116569           2792
seed7/lib/zstd.s7i           0.069125           1263
============================ ================= ================

Statistical Summary — Mode C (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.088911        |
+-----------+-----------------+
| Minimum   | 0.034105        |
+-----------+-----------------+
| Maximum   | 2.502191        |
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
seed7/prg/addup.sd7          0.047043            190
seed7/prg/bas7.sd7           0.764153          11459
seed7/prg/bifurk.sd7         0.038449             73
seed7/prg/bigfiles.sd7       0.041350            129
seed7/prg/brainf7.sd7        0.037391             86
seed7/prg/calc7.sd7          0.047557            128
seed7/prg/carddemo.sd7       0.049231            190
seed7/prg/castle.sd7         0.219599           3148
seed7/prg/cat.sd7            0.041339             82
seed7/prg/cellauto.sd7       0.041174             85
seed7/prg/celsius.sd7        0.038395             42
seed7/prg/chk_all.sd7        0.085195            843
seed7/prg/chkarr.sd7         0.872240           8367
seed7/prg/chkbig.sd7         4.120094          29026
seed7/prg/chkbin.sd7         1.030891           6469
seed7/prg/chkbitdata.sd7     1.250768           6624
seed7/prg/chkbool.sd7        0.239652           3157
seed7/prg/chkbst.sd7         0.113847            722
seed7/prg/chkchr.sd7         0.485496           2809
seed7/prg/chkcmd.sd7         0.117475           1205
seed7/prg/chkdb.sd7          0.758078           7454
seed7/prg/chkdecl.sd7        0.093895            448
seed7/prg/chkenum.sd7        0.119820           1230
seed7/prg/chkerr.sd7         0.345272           4663
seed7/prg/chkexc.sd7         0.148529           2627
seed7/prg/chkfil.sd7         0.125234           1615
seed7/prg/chkflt.sd7         2.794039          20620
seed7/prg/chkhent.sd7        0.038098             54
seed7/prg/chkhsh.sd7         0.493997           4548
seed7/prg/chkidx.sd7         3.142457          19567
seed7/prg/chkint.sd7         5.499677          38129
seed7/prg/chkjson.sd7        0.186726           1764
seed7/prg/chkovf.sd7         1.189432           8216
seed7/prg/chkprc.sd7         0.695089          10111
seed7/prg/chkscan.sd7        0.089334            714
seed7/prg/chkset.sd7         1.724488          11974
seed7/prg/chkstr.sd7         3.449540          26952
seed7/prg/chktime.sd7        0.250791           2025
seed7/prg/chktoml.sd7        0.201709           1656
seed7/prg/clock.sd7          0.041845             47
seed7/prg/clock2.sd7         0.039697             43
seed7/prg/clock3.sd7         0.044926             95
seed7/prg/cmpfil.sd7         0.040014             84
seed7/prg/comanche.sd7       0.052159            180
seed7/prg/confval.sd7        0.053863            175
seed7/prg/db7.sd7            0.067289            417
seed7/prg/diff7.sd7          0.056471            263
seed7/prg/dirtst.sd7         0.039593             42
seed7/prg/dirx.sd7           0.048989            152
seed7/prg/dnafight.sd7       0.128308           1381
seed7/prg/dragon.sd7         0.043509             73
seed7/prg/echo.sd7           0.039681             39
seed7/prg/eliza.sd7          0.055250            302
seed7/prg/err.sd7            0.045689             96
seed7/prg/fannkuch.sd7       0.048565            131
seed7/prg/fib.sd7            0.042765             47
seed7/prg/find7.sd7          0.047779            133
seed7/prg/findchar.sd7       0.048914            149
seed7/prg/fractree.sd7       0.039455             55
seed7/prg/ftp7.sd7           0.056655            296
seed7/prg/ftpserv.sd7        0.042979             74
seed7/prg/gcd.sd7            0.044833            109
seed7/prg/gkbd.sd7           0.064237            358
seed7/prg/gtksvtst.sd7       0.042123             94
seed7/prg/hal.sd7            0.051798            250
seed7/prg/hamu.sd7           0.073659            573
seed7/prg/hanoi.sd7          0.040378             55
seed7/prg/hd.sd7             0.040851             79
seed7/prg/hello.sd7          0.037708             32
seed7/prg/hilbert.sd7        0.045908            108
seed7/prg/ide7.sd7           0.052829            196
seed7/prg/kbd.sd7            0.038688             49
seed7/prg/klondike.sd7       0.097400            883
seed7/prg/lander.sd7         0.142815           1551
seed7/prg/lst80bas.sd7       0.068811            344
seed7/prg/lst99bas.sd7       0.065201            401
seed7/prg/lstgwbas.sd7       0.081294            577
seed7/prg/mahjong.sd7        0.155152           1943
seed7/prg/make7.sd7          0.049778            121
seed7/prg/mandelbr.sd7       0.053198            237
seed7/prg/mind.sd7           0.064775            443
seed7/prg/mirror.sd7         0.051656            131
seed7/prg/ms.sd7             0.081112            641
seed7/prg/nicoma.sd7         0.057950            135
seed7/prg/pac.sd7            0.089812            726
seed7/prg/pairs.sd7          0.156255           2025
seed7/prg/panic.sd7          0.207569           2634
seed7/prg/percolation.sd7    0.060982            330
seed7/prg/planets.sd7        0.143512           1486
seed7/prg/portfwd7.sd7       0.048385            139
seed7/prg/prime.sd7          0.041685             74
seed7/prg/printpi1.sd7       0.044304             56
seed7/prg/printpi2.sd7       0.045638             54
seed7/prg/printpi3.sd7       0.045101             60
seed7/prg/pv7.sd7            0.068709            337
seed7/prg/queen.sd7          0.055762            149
seed7/prg/rand.sd7           0.049570            121
seed7/prg/raytrace.sd7       0.075675            538
seed7/prg/rever.sd7          0.084656            816
seed7/prg/roman.sd7          0.037707             38
seed7/prg/s7c.sd7            0.622837           9060
seed7/prg/s7check.sd7        0.043959             68
seed7/prg/savehd7.sd7        0.112341           1110
seed7/prg/self.sd7           0.042637             49
seed7/prg/shisen.sd7         0.123702           1423
seed7/prg/sl.sd7             0.104869           1029
seed7/prg/snake.sd7          0.071663            615
seed7/prg/sokoban.sd7        0.087120            891
seed7/prg/spigotpi.sd7       0.040997             64
seed7/prg/sql7.sd7           0.056869            278
seed7/prg/startrek.sd7       0.100274            979
seed7/prg/sudoku7.sd7        0.209327           2657
seed7/prg/sydir7.sd7         0.065811            384
seed7/prg/syntaxhl.sd7       0.053912            177
seed7/prg/tak.sd7            0.041921             59
seed7/prg/tar7.sd7           0.046420            121
seed7/prg/tch.sd7            0.040597             55
seed7/prg/testfont.sd7       0.046366             95
seed7/prg/tet.sd7            0.067998            479
seed7/prg/tetg.sd7           0.066574            501
seed7/prg/toutf8.sd7         0.055064            240
seed7/prg/tst_cli.sd7        0.045312             40
seed7/prg/tst_srv.sd7        0.042954             47
seed7/prg/wator.sd7          0.084577            651
seed7/prg/which.sd7          0.041858             65
seed7/prg/wiz.sd7            0.221157           2833
seed7/prg/wordcnt.sd7        0.041690             54
seed7/prg/wrinum.sd7         0.040682             43
seed7/prg/wumpus.sd7         0.061333            372
seed7/lib/aes.s7i            0.210803           1144
seed7/lib/aes_gcm.s7i        0.065724            392
seed7/lib/ar.s7i             0.134317           1532
seed7/lib/arc4.s7i           0.048289            144
seed7/lib/archive.s7i        0.046439            143
seed7/lib/archive_base.s7i   0.046304            135
seed7/lib/array.s7i          0.083466            610
seed7/lib/asn1.s7i           0.072678            544
seed7/lib/asn1oid.s7i        0.057608            157
seed7/lib/basearray.s7i      0.071171            450
seed7/lib/bigfile.s7i        0.047695            136
seed7/lib/bigint.s7i         0.084789            824
seed7/lib/bigrat.s7i         0.087342            784
seed7/lib/bin16.s7i          0.076315            592
seed7/lib/bin32.s7i          0.070444            490
seed7/lib/bin64.s7i          0.071496            539
seed7/lib/bitdata.s7i        0.131225           1330
seed7/lib/bitmapfont.s7i     0.049358            215
seed7/lib/bitset.s7i         0.067766            593
seed7/lib/bitsetof.s7i       0.065962            431
seed7/lib/blowfish.s7i       0.077036            383
seed7/lib/bmp.s7i            0.098725            924
seed7/lib/boolean.s7i        0.054670            403
seed7/lib/browser.s7i        0.052742            280
seed7/lib/bstring.s7i        0.046051            227
seed7/lib/bytedata.s7i       0.064490            482
seed7/lib/bzip2.s7i          0.086881            887
seed7/lib/cards.s7i          0.101896           1342
seed7/lib/category.s7i       0.048754            209
seed7/lib/cc_conf.s7i        0.120477           1314
seed7/lib/ccittfax.s7i       0.101400           1022
seed7/lib/cgi.s7i            0.040603            109
seed7/lib/cgidialog.s7i      0.096157           1118
seed7/lib/char.s7i           0.051463            356
seed7/lib/charsets.s7i       0.123001           2024
seed7/lib/chartype.s7i       0.047414            121
seed7/lib/cipher.s7i         0.042476            146
seed7/lib/cli_cmds.s7i       0.111906           1360
seed7/lib/clib_file.s7i      0.049767            301
seed7/lib/color.s7i          0.044815            185
seed7/lib/complex.s7i        0.057308            464
seed7/lib/compress.s7i       0.041893            150
seed7/lib/console.s7i        0.045850            188
seed7/lib/cpio.s7i           0.143090           1708
seed7/lib/crc32.s7i          0.053900            193
seed7/lib/cronos16.s7i       0.191172           1173
seed7/lib/cronos27.s7i       0.255816           1464
seed7/lib/csv.s7i            0.048144            201
seed7/lib/db_prop.s7i        0.100084            991
seed7/lib/deflate.s7i        0.086955            740
seed7/lib/des.s7i            0.077819            444
seed7/lib/dialog.s7i         0.055107            311
seed7/lib/dir.s7i            0.043725            163
seed7/lib/draw.s7i           0.085140            854
seed7/lib/duration.s7i       0.096273           1038
seed7/lib/echo.s7i           0.043209            132
seed7/lib/editline.s7i       0.059194            398
seed7/lib/elf.s7i            0.152261           1560
seed7/lib/elliptic.s7i       0.075393            649
seed7/lib/enable_io.s7i      0.051073            312
seed7/lib/encoding.s7i       0.096249            931
seed7/lib/enumeration.s7i    0.049805            236
seed7/lib/environment.s7i    0.043764            175
seed7/lib/exif.s7i           0.045746            152
seed7/lib/external_file.s7i  0.050397            340
seed7/lib/field.s7i          0.050012            268
seed7/lib/file.s7i           0.052278            372
seed7/lib/filebits.s7i       0.036300             46
seed7/lib/filesys.s7i        0.063428            601
seed7/lib/fileutil.s7i       0.041842            144
seed7/lib/fixarray.s7i       0.051776            307
seed7/lib/float.s7i          0.071595            757
seed7/lib/font.s7i           0.044077            196
seed7/lib/font8x8.s7i        0.065888            998
seed7/lib/forloop.s7i        0.058206            449
seed7/lib/ftp.s7i            0.086337            969
seed7/lib/ftpserv.s7i        0.075027            631
seed7/lib/getf.s7i           0.040912            115
seed7/lib/gethttp.s7i        0.038008             41
seed7/lib/gethttps.s7i       0.037159             41
seed7/lib/gif.s7i            0.071113            561
seed7/lib/graph.s7i          0.063809            415
seed7/lib/graph_file.s7i     0.057447            399
seed7/lib/gtkserver.s7i      0.041256            161
seed7/lib/gzip.s7i           0.065849            573
seed7/lib/hash.s7i           0.065901            421
seed7/lib/hashsetof.s7i      0.063921            499
seed7/lib/hmac.s7i           0.044123            152
seed7/lib/html.s7i           0.039182             83
seed7/lib/html_ent.s7i       0.063487            476
seed7/lib/htmldom.s7i        0.053072            286
seed7/lib/http_request.s7i   0.076346            696
seed7/lib/http_srv_resp.s7i  0.058471            380
seed7/lib/https_request.s7i  0.047596            211
seed7/lib/httpserv.s7i       0.054826            345
seed7/lib/huffman.s7i        0.073821            644
seed7/lib/ico.s7i            0.049116            221
seed7/lib/idxarray.s7i       0.050812            232
seed7/lib/image.s7i          0.041347            156
seed7/lib/imagefile.s7i      0.044769            171
seed7/lib/inflate.s7i        0.063394            411
seed7/lib/inifile.s7i        0.042109            129
seed7/lib/integer.s7i        0.067654            663
seed7/lib/iobuffer.s7i       0.049243            289
seed7/lib/jpeg.s7i           0.151885           1761
seed7/lib/json.s7i           0.079129            891
seed7/lib/json_serde.s7i     0.077625            783
seed7/lib/keybd.s7i          0.078591            639
seed7/lib/keydescr.s7i       0.049469            192
seed7/lib/leb128.s7i         0.046823            218
seed7/lib/line.s7i           0.042740            164
seed7/lib/listener.s7i       0.048256            247
seed7/lib/logfile.s7i        0.037914             73
seed7/lib/lower.s7i          0.041465            142
seed7/lib/lzma.s7i           0.097092            934
seed7/lib/lzw.s7i            0.092185            861
seed7/lib/magic.s7i          0.065970            403
seed7/lib/mahjng32.s7i       0.091085           1500
seed7/lib/make.s7i           0.066807            544
seed7/lib/makedata.s7i       0.117987           1428
seed7/lib/math.s7i           0.043571            201
seed7/lib/mixarith.s7i       0.046465            249
seed7/lib/modern27.s7i       0.168629           1099
seed7/lib/more.s7i           0.042142            130
seed7/lib/msgdigest.s7i      0.135399           1222
seed7/lib/multiscr.s7i       0.038277             68
seed7/lib/null_file.s7i      0.051034            345
seed7/lib/osfiles.s7i        0.097564           1085
seed7/lib/pbm.s7i            0.047807            230
seed7/lib/pcx.s7i            0.076816            638
seed7/lib/pem.s7i            0.045016            185
seed7/lib/pgm.s7i            0.048606            238
seed7/lib/pic16.s7i          0.065731           1037
seed7/lib/pic32.s7i          0.118088           2060
seed7/lib/pic_util.s7i       0.042143            144
seed7/lib/pixelimage.s7i     0.050706            320
seed7/lib/pixmap_file.s7i    0.061578            459
seed7/lib/pixmapfont.s7i     0.046396            184
seed7/lib/pkcs1.s7i          0.076855            543
seed7/lib/png.s7i            0.105903           1064
seed7/lib/poll.s7i           0.052351            313
seed7/lib/ppm.s7i            0.051077            240
seed7/lib/process.s7i        0.065991            541
seed7/lib/progs.s7i          0.078479            789
seed7/lib/propertyfile.s7i   0.044226            155
seed7/lib/rational.s7i       0.077950            792
seed7/lib/ref_list.s7i       0.047944            252
seed7/lib/reference.s7i      0.041889            126
seed7/lib/reverse.s7i        0.039223             94
seed7/lib/rpm.s7i            0.280001           3487
seed7/lib/rpmext.s7i         0.050240            318
seed7/lib/scanfile.s7i       0.131567           1779
seed7/lib/scanjson.s7i       0.060939            413
seed7/lib/scanstri.s7i       0.135562           1814
seed7/lib/scantoml.s7i       0.128478           1603
seed7/lib/seed7_05.s7i       0.109282           1072
seed7/lib/set.s7i            0.037733             57
seed7/lib/shell.s7i          0.068432            615
seed7/lib/showtls.s7i        0.083770            678
seed7/lib/signature.s7i      0.041352            131
seed7/lib/smtp.s7i           0.047363            261
seed7/lib/sockbase.s7i       0.048918            217
seed7/lib/socket.s7i         0.051318            326
seed7/lib/sokoban1.s7i       0.079923           1519
seed7/lib/sql_base.s7i       0.093062           1000
seed7/lib/stars.s7i          0.232921           1705
seed7/lib/stdfont10.s7i      0.141768           3347
seed7/lib/stdfont12.s7i      0.163967           3928
seed7/lib/stdfont14.s7i      0.185200           4510
seed7/lib/stdfont16.s7i      0.208043           5092
seed7/lib/stdfont18.s7i      0.242496           5868
seed7/lib/stdfont20.s7i      0.270016           6449
seed7/lib/stdfont24.s7i      0.324403           7421
seed7/lib/stdfont8.s7i       0.123779           2960
seed7/lib/stdfont9.s7i       0.131137           3152
seed7/lib/stdio.s7i          0.044420            192
seed7/lib/strifile.s7i       0.053299            345
seed7/lib/string.s7i         0.075139            779
seed7/lib/stritext.s7i       0.054432            352
seed7/lib/struct.s7i         0.054477            266
seed7/lib/struct_elem.s7i    0.041717            129
seed7/lib/subfile.s7i        0.043382            174
seed7/lib/subrange.s7i       0.038123             78
seed7/lib/syntax.s7i         0.060009            294
seed7/lib/tar.s7i            0.143046           1880
seed7/lib/tar_cmds.s7i       0.083542            752
seed7/lib/tdes.s7i           0.043203            143
seed7/lib/tee.s7i            0.040813            143
seed7/lib/text.s7i           0.040165            135
seed7/lib/tga.s7i            0.078541            676
seed7/lib/tiff.s7i           0.240355           2771
seed7/lib/time.s7i           0.101474           1191
seed7/lib/tls.s7i            0.193727           2230
seed7/lib/unicode.s7i        0.073696            575
seed7/lib/unionfnd.s7i       0.043028            130
seed7/lib/upper.s7i          0.042079            142
seed7/lib/utf16.s7i          0.064765            540
seed7/lib/utf8.s7i           0.047135            234
seed7/lib/vecfont10.s7i      0.156894           1056
seed7/lib/vecfont18.s7i      0.173614           1119
seed7/lib/vector3d.s7i       0.049023            293
seed7/lib/vectorfont.s7i     0.048101            239
seed7/lib/wildcard.s7i       0.042255            140
seed7/lib/window.s7i         0.060235            455
seed7/lib/wrinum.s7i         0.049296            248
seed7/lib/x509cert.s7i       0.119084           1243
seed7/lib/xml_ent.s7i        0.041384             94
seed7/lib/xmldom.s7i         0.050963            303
seed7/lib/xz.s7i             0.059750            442
seed7/lib/zip.s7i            0.236822           2792
seed7/lib/zstd.s7i           0.119275           1263
============================ ================= ================

Statistical Summary — Mode D (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.159773        |
+-----------+-----------------+
| Minimum   | 0.036300        |
+-----------+-----------------+
| Maximum   | 5.499677        |
+-----------+-----------------+

Cross-Mode Comparison
=====================

+------+-----------------+-----------------+-----------------+
| Mode | Average (s)     | Minimum (s)     | Maximum (s)     |
+======+=================+=================+=================+
| A    | 0.033866        | 0.032217        | 0.061562        |
+------+-----------------+-----------------+-----------------+
| B    | 0.039602        | 0.034564        | 0.048954        |
+------+-----------------+-----------------+-----------------+
| C    | 0.088911        | 0.034105        | 2.502191        |
+------+-----------------+-----------------+-----------------+
| D    | 0.159773        | 0.036300        | 5.499677        |
+------+-----------------+-----------------+-----------------+

Phase Timing Summary
====================

Wall-clock times include warm-up work and GC performed by the warm-up helper.

+----------+--------------+--------------+--------------+
| Phase    | Warm-up      | Timed pass   | Mode total   |
+==========+==============+==============+==============+
| Mode A   | 00:00:12.579 | 00:00:57.701 | 00:01:10.280 |
+----------+--------------+--------------+--------------+
| Mode B   | 00:00:14.986 | 00:01:07.536 | 00:01:22.523 |
+----------+--------------+--------------+--------------+
| Mode C   | 00:00:36.299 | 00:02:32.384 | 00:03:08.683 |
+----------+--------------+--------------+--------------+
| Mode D   | 00:01:01.475 | 00:04:33.108 | 00:05:34.583 |
+----------+--------------+--------------+--------------+
| Total    |              |              | 00:11:16.077 |
+----------+--------------+--------------+--------------+


.. ---------------------------------------------------------------------------
