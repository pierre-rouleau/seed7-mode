=======================================================
GC-Controlled Benchmark Report: Seed7 Mode — Four Modes
=======================================================

:Running with: seed7-mode 2026-06-25T03:05:30+0000 W26-4
:Seed7 dir   : /Users/roup/my/dvo/seed7-repos/seed7/
:Started at: 23:12:51 local time
:Generated on: 2026-06-25 03:24:28 UTC
:N Iterations: 5  (mean of N timed opens per file)
:Window system: nil
:Display graphic: nil
:Frame size: 230x95 chars
:Window body: 230x93 chars
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
seed7/prg/addup.sd7          0.034597            190
seed7/prg/bas7.sd7           0.034502          11459
seed7/prg/bifurk.sd7         0.034266             73
seed7/prg/bigfiles.sd7       0.033522            129
seed7/prg/brainf7.sd7        0.033608             86
seed7/prg/calc7.sd7          0.034264            128
seed7/prg/carddemo.sd7       0.035143            190
seed7/prg/castle.sd7         0.033730           3148
seed7/prg/cat.sd7            0.033302             82
seed7/prg/cellauto.sd7       0.032820             85
seed7/prg/celsius.sd7        0.032124             42
seed7/prg/chk_all.sd7        0.033063            843
seed7/prg/chkarr.sd7         0.033376           8367
seed7/prg/chkbig.sd7         0.037450          29026
seed7/prg/chkbin.sd7         0.033857           6469
seed7/prg/chkbitdata.sd7     0.034962           6624
seed7/prg/chkbool.sd7        0.033247           3157
seed7/prg/chkbst.sd7         0.032746            722
seed7/prg/chkchr.sd7         0.033464           2809
seed7/prg/chkcmd.sd7         0.032531           1205
seed7/prg/chkdb.sd7          0.033845           7454
seed7/prg/chkdecl.sd7        0.032223            448
seed7/prg/chkenum.sd7        0.033429           1230
seed7/prg/chkerr.sd7         0.036375           4663
seed7/prg/chkexc.sd7         0.038691           2627
seed7/prg/chkfil.sd7         0.033912           1615
seed7/prg/chkflt.sd7         0.038583          20620
seed7/prg/chkhent.sd7        0.033335             54
seed7/prg/chkhsh.sd7         0.033774           4548
seed7/prg/chkidx.sd7         0.035922          19567
seed7/prg/chkint.sd7         0.040152          38129
seed7/prg/chkjson.sd7        0.033247           1764
seed7/prg/chkovf.sd7         0.035443           8216
seed7/prg/chkprc.sd7         0.034165          10111
seed7/prg/chkscan.sd7        0.033392            714
seed7/prg/chkset.sd7         0.034531          11974
seed7/prg/chkstr.sd7         0.037595          26952
seed7/prg/chktime.sd7        0.033474           2025
seed7/prg/chktoml.sd7        0.033651           1656
seed7/prg/clock.sd7          0.032197             47
seed7/prg/clock2.sd7         0.032649             43
seed7/prg/clock3.sd7         0.033038             95
seed7/prg/cmpfil.sd7         0.032370             84
seed7/prg/comanche.sd7       0.032716            180
seed7/prg/confval.sd7        0.032732            175
seed7/prg/db7.sd7            0.032682            417
seed7/prg/diff7.sd7          0.036055            263
seed7/prg/dirtst.sd7         0.033555             42
seed7/prg/dirx.sd7           0.035349            152
seed7/prg/dnafight.sd7       0.036721           1381
seed7/prg/dragon.sd7         0.033503             73
seed7/prg/echo.sd7           0.033283             39
seed7/prg/eliza.sd7          0.037261            302
seed7/prg/err.sd7            0.037923             96
seed7/prg/fannkuch.sd7       0.036309            131
seed7/prg/fib.sd7            0.033390             47
seed7/prg/find7.sd7          0.032285            133
seed7/prg/findchar.sd7       0.032264            149
seed7/prg/fractree.sd7       0.036239             55
seed7/prg/ftp7.sd7           0.040213            296
seed7/prg/ftpserv.sd7        0.033002             74
seed7/prg/gcd.sd7            0.032644            109
seed7/prg/gkbd.sd7           0.032658            358
seed7/prg/gtksvtst.sd7       0.032371             94
seed7/prg/hal.sd7            0.033092            250
seed7/prg/hamu.sd7           0.032449            573
seed7/prg/hanoi.sd7          0.032082             55
seed7/prg/hd.sd7             0.032311             79
seed7/prg/hello.sd7          0.033853             32
seed7/prg/hilbert.sd7        0.033076            108
seed7/prg/ide7.sd7           0.033097            196
seed7/prg/kbd.sd7            0.033146             49
seed7/prg/klondike.sd7       0.033184            883
seed7/prg/lander.sd7         0.032600           1551
seed7/prg/lst80bas.sd7       0.032347            344
seed7/prg/lst99bas.sd7       0.032144            401
seed7/prg/lstgwbas.sd7       0.032416            577
seed7/prg/mahjong.sd7        0.032378           1943
seed7/prg/make7.sd7          0.035478            121
seed7/prg/mandelbr.sd7       0.034601            237
seed7/prg/mind.sd7           0.033709            443
seed7/prg/mirror.sd7         0.033490            131
seed7/prg/ms.sd7             0.033986            641
seed7/prg/nicoma.sd7         0.033725            135
seed7/prg/pac.sd7            0.033280            726
seed7/prg/pairs.sd7          0.033245           2025
seed7/prg/panic.sd7          0.033447           2634
seed7/prg/percolation.sd7    0.033198            330
seed7/prg/planets.sd7        0.033598           1486
seed7/prg/portfwd7.sd7       0.033597            139
seed7/prg/prime.sd7          0.033117             74
seed7/prg/printpi1.sd7       0.036226             56
seed7/prg/printpi2.sd7       0.034496             54
seed7/prg/printpi3.sd7       0.037173             60
seed7/prg/pv7.sd7            0.033633            337
seed7/prg/queen.sd7          0.033452            149
seed7/prg/rand.sd7           0.033224            121
seed7/prg/raytrace.sd7       0.033495            538
seed7/prg/rever.sd7          0.033157            816
seed7/prg/roman.sd7          0.038527             38
seed7/prg/s7c.sd7            0.037521           9060
seed7/prg/s7check.sd7        0.034552             68
seed7/prg/savehd7.sd7        0.034168           1110
seed7/prg/self.sd7           0.033322             49
seed7/prg/shisen.sd7         0.033214           1423
seed7/prg/sl.sd7             0.032750           1029
seed7/prg/snake.sd7          0.033334            615
seed7/prg/sokoban.sd7        0.033034            891
seed7/prg/spigotpi.sd7       0.033996             64
seed7/prg/sql7.sd7           0.037355            278
seed7/prg/startrek.sd7       0.039451            979
seed7/prg/sudoku7.sd7        0.034731           2657
seed7/prg/sydir7.sd7         0.033113            384
seed7/prg/syntaxhl.sd7       0.032674            177
seed7/prg/tak.sd7            0.033550             59
seed7/prg/tar7.sd7           0.033528            121
seed7/prg/tch.sd7            0.034723             55
seed7/prg/testfont.sd7       0.033903             95
seed7/prg/tet.sd7            0.033571            479
seed7/prg/tetg.sd7           0.033412            501
seed7/prg/toutf8.sd7         0.033694            240
seed7/prg/tst_cli.sd7        0.033435             40
seed7/prg/tst_srv.sd7        0.033176             47
seed7/prg/wator.sd7          0.033220            651
seed7/prg/which.sd7          0.033534             65
seed7/prg/wiz.sd7            0.033625           2833
seed7/prg/wordcnt.sd7        0.033022             54
seed7/prg/wrinum.sd7         0.033466             43
seed7/prg/wumpus.sd7         0.033477            372
seed7/lib/aes.s7i            0.033410           1144
seed7/lib/aes_gcm.s7i        0.033513            392
seed7/lib/ar.s7i             0.033397           1532
seed7/lib/arc4.s7i           0.032559            144
seed7/lib/archive.s7i        0.032813            143
seed7/lib/archive_base.s7i   0.032745            135
seed7/lib/array.s7i          0.032327            610
seed7/lib/asn1.s7i           0.033418            544
seed7/lib/asn1oid.s7i        0.032917            157
seed7/lib/basearray.s7i      0.034192            450
seed7/lib/bigfile.s7i        0.033460            136
seed7/lib/bigint.s7i         0.033311            824
seed7/lib/bigrat.s7i         0.033494            784
seed7/lib/bin16.s7i          0.033471            592
seed7/lib/bin32.s7i          0.033362            490
seed7/lib/bin64.s7i          0.033435            539
seed7/lib/bitdata.s7i        0.034111           1330
seed7/lib/bitmapfont.s7i     0.034095            215
seed7/lib/bitset.s7i         0.033532            593
seed7/lib/bitsetof.s7i       0.033957            431
seed7/lib/blowfish.s7i       0.033457            383
seed7/lib/bmp.s7i            0.033482            924
seed7/lib/boolean.s7i        0.033758            403
seed7/lib/browser.s7i        0.033635            280
seed7/lib/bstring.s7i        0.032951            227
seed7/lib/bytedata.s7i       0.033277            482
seed7/lib/bzip2.s7i          0.032705            887
seed7/lib/cards.s7i          0.032407           1342
seed7/lib/category.s7i       0.032688            209
seed7/lib/cc_conf.s7i        0.032360           1314
seed7/lib/ccittfax.s7i       0.032721           1022
seed7/lib/cgi.s7i            0.032609            109
seed7/lib/cgidialog.s7i      0.032991           1118
seed7/lib/char.s7i           0.032694            356
seed7/lib/charsets.s7i       0.032716           2024
seed7/lib/chartype.s7i       0.032654            121
seed7/lib/cipher.s7i         0.033039            146
seed7/lib/cli_cmds.s7i       0.032658           1360
seed7/lib/clib_file.s7i      0.033105            301
seed7/lib/color.s7i          0.035317            185
seed7/lib/complex.s7i        0.033722            464
seed7/lib/compress.s7i       0.033342            150
seed7/lib/console.s7i        0.033192            188
seed7/lib/cpio.s7i           0.033432           1708
seed7/lib/crc32.s7i          0.033091            193
seed7/lib/cronos16.s7i       0.033480           1173
seed7/lib/cronos27.s7i       0.033676           1464
seed7/lib/csv.s7i            0.033404            201
seed7/lib/db_prop.s7i        0.033336            991
seed7/lib/deflate.s7i        0.034156            740
seed7/lib/des.s7i            0.033284            444
seed7/lib/dialog.s7i         0.034070            311
seed7/lib/dir.s7i            0.033397            163
seed7/lib/draw.s7i           0.033500            854
seed7/lib/duration.s7i       0.033312           1038
seed7/lib/echo.s7i           0.033337            132
seed7/lib/editline.s7i       0.033429            398
seed7/lib/elf.s7i            0.033666           1560
seed7/lib/elliptic.s7i       0.033571            649
seed7/lib/enable_io.s7i      0.033023            312
seed7/lib/encoding.s7i       0.033162            931
seed7/lib/enumeration.s7i    0.033595            236
seed7/lib/environment.s7i    0.033231            175
seed7/lib/exif.s7i           0.035435            152
seed7/lib/external_file.s7i  0.036292            340
seed7/lib/field.s7i          0.032883            268
seed7/lib/file.s7i           0.032440            372
seed7/lib/filebits.s7i       0.033066             46
seed7/lib/filesys.s7i        0.037120            601
seed7/lib/fileutil.s7i       0.032765            144
seed7/lib/fixarray.s7i       0.033931            307
seed7/lib/float.s7i          0.033995            757
seed7/lib/font.s7i           0.032375            196
seed7/lib/font8x8.s7i        0.032717            998
seed7/lib/forloop.s7i        0.032480            449
seed7/lib/ftp.s7i            0.032651            969
seed7/lib/ftpserv.s7i        0.032623            631
seed7/lib/getf.s7i           0.032644            115
seed7/lib/gethttp.s7i        0.033375             41
seed7/lib/gethttps.s7i       0.032820             41
seed7/lib/gif.s7i            0.032412            561
seed7/lib/graph.s7i          0.033156            415
seed7/lib/graph_file.s7i     0.033412            399
seed7/lib/gtkserver.s7i      0.033148            161
seed7/lib/gzip.s7i           0.033631            573
seed7/lib/hash.s7i           0.033685            421
seed7/lib/hashsetof.s7i      0.033639            499
seed7/lib/hmac.s7i           0.033345            152
seed7/lib/html.s7i           0.032879             83
seed7/lib/html_ent.s7i       0.033033            476
seed7/lib/htmldom.s7i        0.033777            286
seed7/lib/http_request.s7i   0.033383            696
seed7/lib/http_srv_resp.s7i  0.033792            380
seed7/lib/https_request.s7i  0.033419            211
seed7/lib/httpserv.s7i       0.033786            345
seed7/lib/huffman.s7i        0.036458            644
seed7/lib/ico.s7i            0.033468            221
seed7/lib/idxarray.s7i       0.032366            232
seed7/lib/image.s7i          0.032528            156
seed7/lib/imagefile.s7i      0.032634            171
seed7/lib/inflate.s7i        0.032614            411
seed7/lib/inifile.s7i        0.032983            129
seed7/lib/integer.s7i        0.034544            663
seed7/lib/iobuffer.s7i       0.035662            289
seed7/lib/jpeg.s7i           0.033942           1761
seed7/lib/json.s7i           0.033666            891
seed7/lib/json_serde.s7i     0.033357            783
seed7/lib/keybd.s7i          0.033448            639
seed7/lib/keydescr.s7i       0.040685            192
seed7/lib/leb128.s7i         0.037196            218
seed7/lib/line.s7i           0.034770            164
seed7/lib/listener.s7i       0.033999            247
seed7/lib/logfile.s7i        0.033438             73
seed7/lib/lower.s7i          0.033618            142
seed7/lib/lzma.s7i           0.033598            934
seed7/lib/lzw.s7i            0.033698            861
seed7/lib/magic.s7i          0.033784            403
seed7/lib/mahjng32.s7i       0.033170           1500
seed7/lib/make.s7i           0.033168            544
seed7/lib/makedata.s7i       0.033504           1428
seed7/lib/math.s7i           0.033684            201
seed7/lib/mixarith.s7i       0.033326            249
seed7/lib/modern27.s7i       0.037915           1099
seed7/lib/more.s7i           0.038715            130
seed7/lib/msgdigest.s7i      0.033206           1222
seed7/lib/multiscr.s7i       0.032627             68
seed7/lib/null_file.s7i      0.032648            345
seed7/lib/osfiles.s7i        0.033290           1085
seed7/lib/pbm.s7i            0.033768            230
seed7/lib/pcx.s7i            0.032872            638
seed7/lib/pem.s7i            0.033035            185
seed7/lib/pgm.s7i            0.032628            238
seed7/lib/pic16.s7i          0.032671           1037
seed7/lib/pic32.s7i          0.034441           2060
seed7/lib/pic_util.s7i       0.035046            144
seed7/lib/pixelimage.s7i     0.033593            320
seed7/lib/pixmap_file.s7i    0.033237            459
seed7/lib/pixmapfont.s7i     0.033891            184
seed7/lib/pkcs1.s7i          0.033727            543
seed7/lib/png.s7i            0.033748           1064
seed7/lib/poll.s7i           0.033421            313
seed7/lib/ppm.s7i            0.033642            240
seed7/lib/process.s7i        0.033667            541
seed7/lib/progs.s7i          0.034251            789
seed7/lib/propertyfile.s7i   0.033528            155
seed7/lib/rational.s7i       0.033851            792
seed7/lib/ref_list.s7i       0.033449            252
seed7/lib/reference.s7i      0.033872            126
seed7/lib/reverse.s7i        0.033428             94
seed7/lib/rpm.s7i            0.033527           3487
seed7/lib/rpmext.s7i         0.033520            318
seed7/lib/scanfile.s7i       0.033820           1779
seed7/lib/scanjson.s7i       0.033427            413
seed7/lib/scanstri.s7i       0.033806           1814
seed7/lib/scantoml.s7i       0.033338           1603
seed7/lib/seed7_05.s7i       0.033416           1072
seed7/lib/set.s7i            0.033280             57
seed7/lib/shell.s7i          0.033522            615
seed7/lib/showtls.s7i        0.032556            678
seed7/lib/signature.s7i      0.032250            131
seed7/lib/smtp.s7i           0.032933            261
seed7/lib/sockbase.s7i       0.032845            217
seed7/lib/socket.s7i         0.032384            326
seed7/lib/sokoban1.s7i       0.032290           1519
seed7/lib/sql_base.s7i       0.034445           1000
seed7/lib/stars.s7i          0.033522           1705
seed7/lib/stdfont10.s7i      0.033330           3347
seed7/lib/stdfont12.s7i      0.033755           3928
seed7/lib/stdfont14.s7i      0.034111           4510
seed7/lib/stdfont16.s7i      0.032520           5092
seed7/lib/stdfont18.s7i      0.032751           5868
seed7/lib/stdfont20.s7i      0.032976           6449
seed7/lib/stdfont24.s7i      0.032991           7421
seed7/lib/stdfont8.s7i       0.032445           2960
seed7/lib/stdfont9.s7i       0.032332           3152
seed7/lib/stdio.s7i          0.032939            192
seed7/lib/strifile.s7i       0.032723            345
seed7/lib/string.s7i         0.032637            779
seed7/lib/stritext.s7i       0.032795            352
seed7/lib/struct.s7i         0.033327            266
seed7/lib/struct_elem.s7i    0.033247            129
seed7/lib/subfile.s7i        0.033564            174
seed7/lib/subrange.s7i       0.033489             78
seed7/lib/syntax.s7i         0.033601            294
seed7/lib/tar.s7i            0.033619           1880
seed7/lib/tar_cmds.s7i       0.033372            752
seed7/lib/tdes.s7i           0.033116            143
seed7/lib/tee.s7i            0.033560            143
seed7/lib/text.s7i           0.033648            135
seed7/lib/tga.s7i            0.033199            676
seed7/lib/tiff.s7i           0.032888           2771
seed7/lib/time.s7i           0.034340           1191
seed7/lib/tls.s7i            0.033007           2230
seed7/lib/unicode.s7i        0.034432            575
seed7/lib/unionfnd.s7i       0.034792            130
seed7/lib/upper.s7i          0.037061            142
seed7/lib/utf16.s7i          0.034271            540
seed7/lib/utf8.s7i           0.033541            234
seed7/lib/vecfont10.s7i      0.033846           1056
seed7/lib/vecfont18.s7i      0.034123           1119
seed7/lib/vector3d.s7i       0.034358            293
seed7/lib/vectorfont.s7i     0.033497            239
seed7/lib/wildcard.s7i       0.037190            140
seed7/lib/window.s7i         0.041644            455
seed7/lib/wrinum.s7i         0.033529            248
seed7/lib/x509cert.s7i       0.033572           1243
seed7/lib/xml_ent.s7i        0.033446             94
seed7/lib/xmldom.s7i         0.033797            303
seed7/lib/xz.s7i             0.033816            442
seed7/lib/zip.s7i            0.033836           2792
seed7/lib/zstd.s7i           0.033881           1263
============================ ================= ================

Statistical Summary — Mode A (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.033764        |
+-----------+-----------------+
| Minimum   | 0.032082        |
+-----------+-----------------+
| Maximum   | 0.041644        |
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
seed7/prg/addup.sd7          0.040882            190
seed7/prg/bas7.sd7           0.041404          11459
seed7/prg/bifurk.sd7         0.036913             73
seed7/prg/bigfiles.sd7       0.038948            129
seed7/prg/brainf7.sd7        0.037568             86
seed7/prg/calc7.sd7          0.039097            128
seed7/prg/carddemo.sd7       0.038330            190
seed7/prg/castle.sd7         0.039384           3148
seed7/prg/cat.sd7            0.035857             82
seed7/prg/cellauto.sd7       0.036360             85
seed7/prg/celsius.sd7        0.035447             42
seed7/prg/chk_all.sd7        0.038137            843
seed7/prg/chkarr.sd7         0.039353           8367
seed7/prg/chkbig.sd7         0.042586          29026
seed7/prg/chkbin.sd7         0.039203           6469
seed7/prg/chkbitdata.sd7     0.039025           6624
seed7/prg/chkbool.sd7        0.037372           3157
seed7/prg/chkbst.sd7         0.037874            722
seed7/prg/chkchr.sd7         0.041629           2809
seed7/prg/chkcmd.sd7         0.039162           1205
seed7/prg/chkdb.sd7          0.040080           7454
seed7/prg/chkdecl.sd7        0.039088            448
seed7/prg/chkenum.sd7        0.038762           1230
seed7/prg/chkerr.sd7         0.040138           4663
seed7/prg/chkexc.sd7         0.038816           2627
seed7/prg/chkfil.sd7         0.038662           1615
seed7/prg/chkflt.sd7         0.043141          20620
seed7/prg/chkhent.sd7        0.036365             54
seed7/prg/chkhsh.sd7         0.040524           4548
seed7/prg/chkidx.sd7         0.041683          19567
seed7/prg/chkint.sd7         0.046236          38129
seed7/prg/chkjson.sd7        0.040993           1764
seed7/prg/chkovf.sd7         0.045406           8216
seed7/prg/chkprc.sd7         0.040613          10111
seed7/prg/chkscan.sd7        0.046473            714
seed7/prg/chkset.sd7         0.041607          11974
seed7/prg/chkstr.sd7         0.044282          26952
seed7/prg/chktime.sd7        0.041046           2025
seed7/prg/chktoml.sd7        0.039644           1656
seed7/prg/clock.sd7          0.034834             47
seed7/prg/clock2.sd7         0.034666             43
seed7/prg/clock3.sd7         0.037544             95
seed7/prg/cmpfil.sd7         0.036105             84
seed7/prg/comanche.sd7       0.037540            180
seed7/prg/confval.sd7        0.040524            175
seed7/prg/db7.sd7            0.039800            417
seed7/prg/diff7.sd7          0.039865            263
seed7/prg/dirtst.sd7         0.035120             42
seed7/prg/dirx.sd7           0.038125            152
seed7/prg/dnafight.sd7       0.039106           1381
seed7/prg/dragon.sd7         0.036017             73
seed7/prg/echo.sd7           0.034326             39
seed7/prg/eliza.sd7          0.037736            302
seed7/prg/err.sd7            0.040153             96
seed7/prg/fannkuch.sd7       0.037449            131
seed7/prg/fib.sd7            0.035969             47
seed7/prg/find7.sd7          0.038843            133
seed7/prg/findchar.sd7       0.039191            149
seed7/prg/fractree.sd7       0.036578             55
seed7/prg/ftp7.sd7           0.038889            296
seed7/prg/ftpserv.sd7        0.037773             74
seed7/prg/gcd.sd7            0.038038            109
seed7/prg/gkbd.sd7           0.042282            358
seed7/prg/gtksvtst.sd7       0.038043             94
seed7/prg/hal.sd7            0.038494            250
seed7/prg/hamu.sd7           0.039408            573
seed7/prg/hanoi.sd7          0.036947             55
seed7/prg/hd.sd7             0.036797             79
seed7/prg/hello.sd7          0.034505             32
seed7/prg/hilbert.sd7        0.037281            108
seed7/prg/ide7.sd7           0.038183            196
seed7/prg/kbd.sd7            0.035192             49
seed7/prg/klondike.sd7       0.040018            883
seed7/prg/lander.sd7         0.039709           1551
seed7/prg/lst80bas.sd7       0.037966            344
seed7/prg/lst99bas.sd7       0.039309            401
seed7/prg/lstgwbas.sd7       0.040207            577
seed7/prg/mahjong.sd7        0.038970           1943
seed7/prg/make7.sd7          0.038906            121
seed7/prg/mandelbr.sd7       0.039144            237
seed7/prg/mind.sd7           0.038891            443
seed7/prg/mirror.sd7         0.039962            131
seed7/prg/ms.sd7             0.039575            641
seed7/prg/nicoma.sd7         0.038693            135
seed7/prg/pac.sd7            0.039049            726
seed7/prg/pairs.sd7          0.038778           2025
seed7/prg/panic.sd7          0.039497           2634
seed7/prg/percolation.sd7    0.039072            330
seed7/prg/planets.sd7        0.039850           1486
seed7/prg/portfwd7.sd7       0.039598            139
seed7/prg/prime.sd7          0.036794             74
seed7/prg/printpi1.sd7       0.037029             56
seed7/prg/printpi2.sd7       0.036339             54
seed7/prg/printpi3.sd7       0.036336             60
seed7/prg/pv7.sd7            0.037998            337
seed7/prg/queen.sd7          0.038076            149
seed7/prg/rand.sd7           0.036978            121
seed7/prg/raytrace.sd7       0.038241            538
seed7/prg/rever.sd7          0.037627            816
seed7/prg/roman.sd7          0.034425             38
seed7/prg/s7c.sd7            0.040186           9060
seed7/prg/s7check.sd7        0.036031             68
seed7/prg/savehd7.sd7        0.037717           1110
seed7/prg/self.sd7           0.035653             49
seed7/prg/shisen.sd7         0.038558           1423
seed7/prg/sl.sd7             0.037520           1029
seed7/prg/snake.sd7          0.038316            615
seed7/prg/sokoban.sd7        0.037268            891
seed7/prg/spigotpi.sd7       0.035821             64
seed7/prg/sql7.sd7           0.038062            278
seed7/prg/startrek.sd7       0.038671            979
seed7/prg/sudoku7.sd7        0.039066           2657
seed7/prg/sydir7.sd7         0.038995            384
seed7/prg/syntaxhl.sd7       0.041666            177
seed7/prg/tak.sd7            0.036660             59
seed7/prg/tar7.sd7           0.038719            121
seed7/prg/tch.sd7            0.036413             55
seed7/prg/testfont.sd7       0.038927             95
seed7/prg/tet.sd7            0.038596            479
seed7/prg/tetg.sd7           0.038986            501
seed7/prg/toutf8.sd7         0.040848            240
seed7/prg/tst_cli.sd7        0.036599             40
seed7/prg/tst_srv.sd7        0.035842             47
seed7/prg/wator.sd7          0.037699            651
seed7/prg/which.sd7          0.037646             65
seed7/prg/wiz.sd7            0.042034           2833
seed7/prg/wordcnt.sd7        0.036217             54
seed7/prg/wrinum.sd7         0.035483             43
seed7/prg/wumpus.sd7         0.039047            372
seed7/lib/aes.s7i            0.042074           1144
seed7/lib/aes_gcm.s7i        0.039740            392
seed7/lib/ar.s7i             0.039510           1532
seed7/lib/arc4.s7i           0.039504            144
seed7/lib/archive.s7i        0.038586            143
seed7/lib/archive_base.s7i   0.038923            135
seed7/lib/array.s7i          0.039060            610
seed7/lib/asn1.s7i           0.037927            544
seed7/lib/asn1oid.s7i        0.042898            157
seed7/lib/basearray.s7i      0.039970            450
seed7/lib/bigfile.s7i        0.038939            136
seed7/lib/bigint.s7i         0.038537            824
seed7/lib/bigrat.s7i         0.038461            784
seed7/lib/bin16.s7i          0.038687            592
seed7/lib/bin32.s7i          0.046037            490
seed7/lib/bin64.s7i          0.039742            539
seed7/lib/bitdata.s7i        0.043546           1330
seed7/lib/bitmapfont.s7i     0.037527            215
seed7/lib/bitset.s7i         0.037884            593
seed7/lib/bitsetof.s7i       0.039573            431
seed7/lib/blowfish.s7i       0.049414            383
seed7/lib/bmp.s7i            0.040876            924
seed7/lib/boolean.s7i        0.039574            403
seed7/lib/browser.s7i        0.039311            280
seed7/lib/bstring.s7i        0.038499            227
seed7/lib/bytedata.s7i       0.038383            482
seed7/lib/bzip2.s7i          0.040580            887
seed7/lib/cards.s7i          0.035805           1342
seed7/lib/category.s7i       0.037439            209
seed7/lib/cc_conf.s7i        0.038035           1314
seed7/lib/ccittfax.s7i       0.039504           1022
seed7/lib/cgi.s7i            0.041772            109
seed7/lib/cgidialog.s7i      0.040044           1118
seed7/lib/char.s7i           0.039492            356
seed7/lib/charsets.s7i       0.039786           2024
seed7/lib/chartype.s7i       0.041963            121
seed7/lib/cipher.s7i         0.038867            146
seed7/lib/cli_cmds.s7i       0.039272           1360
seed7/lib/clib_file.s7i      0.039601            301
seed7/lib/color.s7i          0.039516            185
seed7/lib/complex.s7i        0.042592            464
seed7/lib/compress.s7i       0.038522            150
seed7/lib/console.s7i        0.037945            188
seed7/lib/cpio.s7i           0.038691           1708
seed7/lib/crc32.s7i          0.039008            193
seed7/lib/cronos16.s7i       0.042132           1173
seed7/lib/cronos27.s7i       0.042255           1464
seed7/lib/csv.s7i            0.038199            201
seed7/lib/db_prop.s7i        0.038208            991
seed7/lib/deflate.s7i        0.045011            740
seed7/lib/des.s7i            0.040435            444
seed7/lib/dialog.s7i         0.039302            311
seed7/lib/dir.s7i            0.039316            163
seed7/lib/draw.s7i           0.039988            854
seed7/lib/duration.s7i       0.037878           1038
seed7/lib/echo.s7i           0.037747            132
seed7/lib/editline.s7i       0.037726            398
seed7/lib/elf.s7i            0.039181           1560
seed7/lib/elliptic.s7i       0.037395            649
seed7/lib/enable_io.s7i      0.037833            312
seed7/lib/encoding.s7i       0.037713            931
seed7/lib/enumeration.s7i    0.037950            236
seed7/lib/environment.s7i    0.037862            175
seed7/lib/exif.s7i           0.040182            152
seed7/lib/external_file.s7i  0.038674            340
seed7/lib/field.s7i          0.044679            268
seed7/lib/file.s7i           0.042860            372
seed7/lib/filebits.s7i       0.036344             46
seed7/lib/filesys.s7i        0.038890            601
seed7/lib/fileutil.s7i       0.038907            144
seed7/lib/fixarray.s7i       0.040324            307
seed7/lib/float.s7i          0.038690            757
seed7/lib/font.s7i           0.038296            196
seed7/lib/font8x8.s7i        0.038407            998
seed7/lib/forloop.s7i        0.039579            449
seed7/lib/ftp.s7i            0.038350            969
seed7/lib/ftpserv.s7i        0.040203            631
seed7/lib/getf.s7i           0.038038            115
seed7/lib/gethttp.s7i        0.035040             41
seed7/lib/gethttps.s7i       0.034847             41
seed7/lib/gif.s7i            0.039380            561
seed7/lib/graph.s7i          0.040927            415
seed7/lib/graph_file.s7i     0.038940            399
seed7/lib/gtkserver.s7i      0.038169            161
seed7/lib/gzip.s7i           0.038682            573
seed7/lib/hash.s7i           0.040402            421
seed7/lib/hashsetof.s7i      0.040993            499
seed7/lib/hmac.s7i           0.039233            152
seed7/lib/html.s7i           0.037815             83
seed7/lib/html_ent.s7i       0.038875            476
seed7/lib/htmldom.s7i        0.039066            286
seed7/lib/http_request.s7i   0.038631            696
seed7/lib/http_srv_resp.s7i  0.039772            380
seed7/lib/https_request.s7i  0.039439            211
seed7/lib/httpserv.s7i       0.045359            345
seed7/lib/huffman.s7i        0.038476            644
seed7/lib/ico.s7i            0.037760            221
seed7/lib/idxarray.s7i       0.038766            232
seed7/lib/image.s7i          0.037241            156
seed7/lib/imagefile.s7i      0.037881            171
seed7/lib/inflate.s7i        0.038752            411
seed7/lib/inifile.s7i        0.038087            129
seed7/lib/integer.s7i        0.037925            663
seed7/lib/iobuffer.s7i       0.038314            289
seed7/lib/jpeg.s7i           0.038238           1761
seed7/lib/json.s7i           0.037400            891
seed7/lib/json_serde.s7i     0.038142            783
seed7/lib/keybd.s7i          0.039839            639
seed7/lib/keydescr.s7i       0.040820            192
seed7/lib/leb128.s7i         0.039428            218
seed7/lib/line.s7i           0.039142            164
seed7/lib/listener.s7i       0.040847            247
seed7/lib/logfile.s7i        0.042637             73
seed7/lib/lower.s7i          0.039816            142
seed7/lib/lzma.s7i           0.039831            934
seed7/lib/lzw.s7i            0.039476            861
seed7/lib/magic.s7i          0.041015            403
seed7/lib/mahjng32.s7i       0.039111           1500
seed7/lib/make.s7i           0.038586            544
seed7/lib/makedata.s7i       0.039503           1428
seed7/lib/math.s7i           0.039341            201
seed7/lib/mixarith.s7i       0.039033            249
seed7/lib/modern27.s7i       0.042433           1099
seed7/lib/more.s7i           0.038932            130
seed7/lib/msgdigest.s7i      0.040871           1222
seed7/lib/multiscr.s7i       0.037478             68
seed7/lib/null_file.s7i      0.038480            345
seed7/lib/osfiles.s7i        0.041094           1085
seed7/lib/pbm.s7i            0.038448            230
seed7/lib/pcx.s7i            0.038272            638
seed7/lib/pem.s7i            0.037443            185
seed7/lib/pgm.s7i            0.038071            238
seed7/lib/pic16.s7i          0.036928           1037
seed7/lib/pic32.s7i          0.038082           2060
seed7/lib/pic_util.s7i       0.038371            144
seed7/lib/pixelimage.s7i     0.037816            320
seed7/lib/pixmap_file.s7i    0.038612            459
seed7/lib/pixmapfont.s7i     0.039044            184
seed7/lib/pkcs1.s7i          0.043121            543
seed7/lib/png.s7i            0.037853           1064
seed7/lib/poll.s7i           0.038282            313
seed7/lib/ppm.s7i            0.038372            240
seed7/lib/process.s7i        0.037972            541
seed7/lib/progs.s7i          0.040300            789
seed7/lib/propertyfile.s7i   0.040985            155
seed7/lib/rational.s7i       0.039489            792
seed7/lib/ref_list.s7i       0.038769            252
seed7/lib/reference.s7i      0.039142            126
seed7/lib/reverse.s7i        0.038679             94
seed7/lib/rpm.s7i            0.039076           3487
seed7/lib/rpmext.s7i         0.039415            318
seed7/lib/scanfile.s7i       0.039154           1779
seed7/lib/scanjson.s7i       0.039519            413
seed7/lib/scanstri.s7i       0.039141           1814
seed7/lib/scantoml.s7i       0.039349           1603
seed7/lib/seed7_05.s7i       0.041011           1072
seed7/lib/set.s7i            0.035747             57
seed7/lib/shell.s7i          0.038593            615
seed7/lib/showtls.s7i        0.038120            678
seed7/lib/signature.s7i      0.038054            131
seed7/lib/smtp.s7i           0.037543            261
seed7/lib/sockbase.s7i       0.041108            217
seed7/lib/socket.s7i         0.039119            326
seed7/lib/sokoban1.s7i       0.037941           1519
seed7/lib/sql_base.s7i       0.039357           1000
seed7/lib/stars.s7i          0.041406           1705
seed7/lib/stdfont10.s7i      0.039993           3347
seed7/lib/stdfont12.s7i      0.042173           3928
seed7/lib/stdfont14.s7i      0.041415           4510
seed7/lib/stdfont16.s7i      0.041270           5092
seed7/lib/stdfont18.s7i      0.041424           5868
seed7/lib/stdfont20.s7i      0.041328           6449
seed7/lib/stdfont24.s7i      0.041202           7421
seed7/lib/stdfont8.s7i       0.040650           2960
seed7/lib/stdfont9.s7i       0.040756           3152
seed7/lib/stdio.s7i          0.041753            192
seed7/lib/strifile.s7i       0.040434            345
seed7/lib/string.s7i         0.041244            779
seed7/lib/stritext.s7i       0.040560            352
seed7/lib/struct.s7i         0.041291            266
seed7/lib/struct_elem.s7i    0.039852            129
seed7/lib/subfile.s7i        0.040324            174
seed7/lib/subrange.s7i       0.038912             78
seed7/lib/syntax.s7i         0.040724            294
seed7/lib/tar.s7i            0.041207           1880
seed7/lib/tar_cmds.s7i       0.040579            752
seed7/lib/tdes.s7i           0.040057            143
seed7/lib/tee.s7i            0.040515            143
seed7/lib/text.s7i           0.040564            135
seed7/lib/tga.s7i            0.043037            676
seed7/lib/tiff.s7i           0.043157           2771
seed7/lib/time.s7i           0.042112           1191
seed7/lib/tls.s7i            0.041711           2230
seed7/lib/unicode.s7i        0.043102            575
seed7/lib/unionfnd.s7i       0.040953            130
seed7/lib/upper.s7i          0.040583            142
seed7/lib/utf16.s7i          0.040918            540
seed7/lib/utf8.s7i           0.042132            234
seed7/lib/vecfont10.s7i      0.044148           1056
seed7/lib/vecfont18.s7i      0.044284           1119
seed7/lib/vector3d.s7i       0.046615            293
seed7/lib/vectorfont.s7i     0.043546            239
seed7/lib/wildcard.s7i       0.040847            140
seed7/lib/window.s7i         0.040817            455
seed7/lib/wrinum.s7i         0.042137            248
seed7/lib/x509cert.s7i       0.041474           1243
seed7/lib/xml_ent.s7i        0.040671             94
seed7/lib/xmldom.s7i         0.040083            303
seed7/lib/xz.s7i             0.040008            442
seed7/lib/zip.s7i            0.041136           2792
seed7/lib/zstd.s7i           0.043073           1263
============================ ================= ================

Statistical Summary — Mode B (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.039331        |
+-----------+-----------------+
| Minimum   | 0.034326        |
+-----------+-----------------+
| Maximum   | 0.049414        |
+-----------+-----------------+

Mode C — Mode Activation + Full-Buffer Fontification (font-lock-ensure)
-----------------------------------------------------------------------

Full-buffer fontification stress test.

File Load Times — Mode C (mean, GC-free)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

============================ ================= ================
File Name                    Mean Time (s)     Line count
============================ ================= ================
seed7/prg/addup.sd7          0.044318            190
seed7/prg/bas7.sd7           0.335147          11459
seed7/prg/bifurk.sd7         0.040897             73
seed7/prg/bigfiles.sd7       0.040719            129
seed7/prg/brainf7.sd7        0.038646             86
seed7/prg/calc7.sd7          0.039257            128
seed7/prg/carddemo.sd7       0.040734            190
seed7/prg/castle.sd7         0.110211           3148
seed7/prg/cat.sd7            0.037811             82
seed7/prg/cellauto.sd7       0.036874             85
seed7/prg/celsius.sd7        0.036415             42
seed7/prg/chk_all.sd7        0.062023            843
seed7/prg/chkarr.sd7         0.370677           8367
seed7/prg/chkbig.sd7         2.115264          29026
seed7/prg/chkbin.sd7         0.528716           6469
seed7/prg/chkbitdata.sd7     0.631042           6624
seed7/prg/chkbool.sd7        0.120793           3157
seed7/prg/chkbst.sd7         0.066698            722
seed7/prg/chkchr.sd7         0.225455           2809
seed7/prg/chkcmd.sd7         0.069035           1205
seed7/prg/chkdb.sd7          0.363667           7454
seed7/prg/chkdecl.sd7        0.059836            448
seed7/prg/chkenum.sd7        0.070360           1230
seed7/prg/chkerr.sd7         0.198532           4663
seed7/prg/chkexc.sd7         0.084151           2627
seed7/prg/chkfil.sd7         0.077237           1615
seed7/prg/chkflt.sd7         1.377946          20620
seed7/prg/chkhent.sd7        0.037275             54
seed7/prg/chkhsh.sd7         0.252228           4548
seed7/prg/chkidx.sd7         1.349728          19567
seed7/prg/chkint.sd7         2.597008          38129
seed7/prg/chkjson.sd7        0.107299           1764
seed7/prg/chkovf.sd7         0.583550           8216
seed7/prg/chkprc.sd7         0.332219          10111
seed7/prg/chkscan.sd7        0.067686            714
seed7/prg/chkset.sd7         0.675382          11974
seed7/prg/chkstr.sd7         1.501723          26952
seed7/prg/chktime.sd7        0.134832           2025
seed7/prg/chktoml.sd7        0.111153           1656
seed7/prg/clock.sd7          0.044844             47
seed7/prg/clock2.sd7         0.043223             43
seed7/prg/clock3.sd7         0.044575             95
seed7/prg/cmpfil.sd7         0.042841             84
seed7/prg/comanche.sd7       0.045407            180
seed7/prg/confval.sd7        0.043065            175
seed7/prg/db7.sd7            0.049163            417
seed7/prg/diff7.sd7          0.046584            263
seed7/prg/dirtst.sd7         0.038665             42
seed7/prg/dirx.sd7           0.041952            152
seed7/prg/dnafight.sd7       0.071185           1381
seed7/prg/dragon.sd7         0.041744             73
seed7/prg/echo.sd7           0.041588             39
seed7/prg/eliza.sd7          0.050181            302
seed7/prg/err.sd7            0.045107             96
seed7/prg/fannkuch.sd7       0.042923            131
seed7/prg/fib.sd7            0.039760             47
seed7/prg/find7.sd7          0.041617            133
seed7/prg/findchar.sd7       0.044671            149
seed7/prg/fractree.sd7       0.039952             55
seed7/prg/ftp7.sd7           0.047958            296
seed7/prg/ftpserv.sd7        0.041455             74
seed7/prg/gcd.sd7            0.040458            109
seed7/prg/gkbd.sd7           0.051565            358
seed7/prg/gtksvtst.sd7       0.042721             94
seed7/prg/hal.sd7            0.042962            250
seed7/prg/hamu.sd7           0.053036            573
seed7/prg/hanoi.sd7          0.041252             55
seed7/prg/hd.sd7             0.042915             79
seed7/prg/hello.sd7          0.044921             32
seed7/prg/hilbert.sd7        0.043565            108
seed7/prg/ide7.sd7           0.046767            196
seed7/prg/kbd.sd7            0.042344             49
seed7/prg/klondike.sd7       0.062319            883
seed7/prg/lander.sd7         0.080794           1551
seed7/prg/lst80bas.sd7       0.049328            344
seed7/prg/lst99bas.sd7       0.051391            401
seed7/prg/lstgwbas.sd7       0.056203            577
seed7/prg/mahjong.sd7        0.088806           1943
seed7/prg/make7.sd7          0.043597            121
seed7/prg/mandelbr.sd7       0.045242            237
seed7/prg/mind.sd7           0.049754            443
seed7/prg/mirror.sd7         0.045340            131
seed7/prg/ms.sd7             0.052491            641
seed7/prg/nicoma.sd7         0.043865            135
seed7/prg/pac.sd7            0.056148            726
seed7/prg/pairs.sd7          0.090268           2025
seed7/prg/panic.sd7          0.104075           2634
seed7/prg/percolation.sd7    0.047397            330
seed7/prg/planets.sd7        0.080871           1486
seed7/prg/portfwd7.sd7       0.043899            139
seed7/prg/prime.sd7          0.041595             74
seed7/prg/printpi1.sd7       0.039291             56
seed7/prg/printpi2.sd7       0.038088             54
seed7/prg/printpi3.sd7       0.039690             60
seed7/prg/pv7.sd7            0.048157            337
seed7/prg/queen.sd7          0.041158            149
seed7/prg/rand.sd7           0.045638            121
seed7/prg/raytrace.sd7       0.053043            538
seed7/prg/rever.sd7          0.057602            816
seed7/prg/roman.sd7          0.041828             38
seed7/prg/s7c.sd7            0.294857           9060
seed7/prg/s7check.sd7        0.040055             68
seed7/prg/savehd7.sd7        0.074062           1110
seed7/prg/self.sd7           0.041221             49
seed7/prg/shisen.sd7         0.077371           1423
seed7/prg/sl.sd7             0.063331           1029
seed7/prg/snake.sd7          0.054389            615
seed7/prg/sokoban.sd7        0.060195            891
seed7/prg/spigotpi.sd7       0.041414             64
seed7/prg/sql7.sd7           0.045123            278
seed7/prg/startrek.sd7       0.067722            979
seed7/prg/sudoku7.sd7        0.106848           2657
seed7/prg/sydir7.sd7         0.054169            384
seed7/prg/syntaxhl.sd7       0.048635            177
seed7/prg/tak.sd7            0.042379             59
seed7/prg/tar7.sd7           0.046271            121
seed7/prg/tch.sd7            0.043619             55
seed7/prg/testfont.sd7       0.048987             95
seed7/prg/tet.sd7            0.066867            479
seed7/prg/tetg.sd7           0.052688            501
seed7/prg/toutf8.sd7         0.050372            240
seed7/prg/tst_cli.sd7        0.044780             40
seed7/prg/tst_srv.sd7        0.042429             47
seed7/prg/wator.sd7          0.060323            651
seed7/prg/which.sd7          0.043591             65
seed7/prg/wiz.sd7            0.119052           2833
seed7/prg/wordcnt.sd7        0.044341             54
seed7/prg/wrinum.sd7         0.040764             43
seed7/prg/wumpus.sd7         0.049618            372
seed7/lib/aes.s7i            0.118333           1144
seed7/lib/aes_gcm.s7i        0.052470            392
seed7/lib/ar.s7i             0.079701           1532
seed7/lib/arc4.s7i           0.043005            144
seed7/lib/archive.s7i        0.044683            143
seed7/lib/archive_base.s7i   0.042153            135
seed7/lib/array.s7i          0.059267            610
seed7/lib/asn1.s7i           0.050858            544
seed7/lib/asn1oid.s7i        0.044792            157
seed7/lib/basearray.s7i      0.050358            450
seed7/lib/bigfile.s7i        0.038567            136
seed7/lib/bigint.s7i         0.058678            824
seed7/lib/bigrat.s7i         0.055674            784
seed7/lib/bin16.s7i          0.053781            592
seed7/lib/bin32.s7i          0.052339            490
seed7/lib/bin64.s7i          0.060556            539
seed7/lib/bitdata.s7i        0.084559           1330
seed7/lib/bitmapfont.s7i     0.043958            215
seed7/lib/bitset.s7i         0.050794            593
seed7/lib/bitsetof.s7i       0.049724            431
seed7/lib/blowfish.s7i       0.060909            383
seed7/lib/bmp.s7i            0.063915            924
seed7/lib/boolean.s7i        0.046698            403
seed7/lib/browser.s7i        0.045750            280
seed7/lib/bstring.s7i        0.044349            227
seed7/lib/bytedata.s7i       0.053705            482
seed7/lib/bzip2.s7i          0.064113            887
seed7/lib/cards.s7i          0.071084           1342
seed7/lib/category.s7i       0.043769            209
seed7/lib/cc_conf.s7i        0.082889           1314
seed7/lib/ccittfax.s7i       0.072083           1022
seed7/lib/cgi.s7i            0.041014            109
seed7/lib/cgidialog.s7i      0.065138           1118
seed7/lib/char.s7i           0.048305            356
seed7/lib/charsets.s7i       0.088816           2024
seed7/lib/chartype.s7i       0.043134            121
seed7/lib/cipher.s7i         0.041152            146
seed7/lib/cli_cmds.s7i       0.073579           1360
seed7/lib/clib_file.s7i      0.046114            301
seed7/lib/color.s7i          0.042724            185
seed7/lib/complex.s7i        0.048846            464
seed7/lib/compress.s7i       0.042392            150
seed7/lib/console.s7i        0.042425            188
seed7/lib/cpio.s7i           0.086584           1708
seed7/lib/crc32.s7i          0.046378            193
seed7/lib/cronos16.s7i       0.098119           1173
seed7/lib/cronos27.s7i       0.124241           1464
seed7/lib/csv.s7i            0.043899            201
seed7/lib/db_prop.s7i        0.067728            991
seed7/lib/deflate.s7i        0.059649            740
seed7/lib/des.s7i            0.060591            444
seed7/lib/dialog.s7i         0.048331            311
seed7/lib/dir.s7i            0.041828            163
seed7/lib/draw.s7i           0.060405            854
seed7/lib/duration.s7i       0.065080           1038
seed7/lib/echo.s7i           0.040834            132
seed7/lib/editline.s7i       0.049460            398
seed7/lib/elf.s7i            0.090845           1560
seed7/lib/elliptic.s7i       0.056107            649
seed7/lib/enable_io.s7i      0.047327            312
seed7/lib/encoding.s7i       0.065772            931
seed7/lib/enumeration.s7i    0.045014            236
seed7/lib/environment.s7i    0.043006            175
seed7/lib/exif.s7i           0.043494            152
seed7/lib/external_file.s7i  0.055223            340
seed7/lib/field.s7i          0.051688            268
seed7/lib/file.s7i           0.048471            372
seed7/lib/filebits.s7i       0.038731             46
seed7/lib/filesys.s7i        0.050755            601
seed7/lib/fileutil.s7i       0.041283            144
seed7/lib/fixarray.s7i       0.050586            307
seed7/lib/float.s7i          0.063396            757
seed7/lib/font.s7i           0.042291            196
seed7/lib/font8x8.s7i        0.051473            998
seed7/lib/forloop.s7i        0.049292            449
seed7/lib/ftp.s7i            0.061527            969
seed7/lib/ftpserv.s7i        0.055647            631
seed7/lib/getf.s7i           0.040891            115
seed7/lib/gethttp.s7i        0.039303             41
seed7/lib/gethttps.s7i       0.042442             41
seed7/lib/gif.s7i            0.055469            561
seed7/lib/graph.s7i          0.052750            415
seed7/lib/graph_file.s7i     0.048477            399
seed7/lib/gtkserver.s7i      0.041590            161
seed7/lib/gzip.s7i           0.051176            573
seed7/lib/hash.s7i           0.051596            421
seed7/lib/hashsetof.s7i      0.051323            499
seed7/lib/hmac.s7i           0.040169            152
seed7/lib/html.s7i           0.038313             83
seed7/lib/html_ent.s7i       0.047995            476
seed7/lib/htmldom.s7i        0.044696            286
seed7/lib/http_request.s7i   0.055694            696
seed7/lib/http_srv_resp.s7i  0.049866            380
seed7/lib/https_request.s7i  0.043474            211
seed7/lib/httpserv.s7i       0.045507            345
seed7/lib/huffman.s7i        0.054718            644
seed7/lib/ico.s7i            0.046110            221
seed7/lib/idxarray.s7i       0.049358            232
seed7/lib/image.s7i          0.040271            156
seed7/lib/imagefile.s7i      0.041076            171
seed7/lib/inflate.s7i        0.048503            411
seed7/lib/inifile.s7i        0.039932            129
seed7/lib/integer.s7i        0.053928            663
seed7/lib/iobuffer.s7i       0.043309            289
seed7/lib/jpeg.s7i           0.085225           1761
seed7/lib/json.s7i           0.057677            891
seed7/lib/json_serde.s7i     0.055242            783
seed7/lib/keybd.s7i          0.056595            639
seed7/lib/keydescr.s7i       0.045039            192
seed7/lib/leb128.s7i         0.041224            218
seed7/lib/line.s7i           0.039812            164
seed7/lib/listener.s7i       0.044011            247
seed7/lib/logfile.s7i        0.039848             73
seed7/lib/lower.s7i          0.041939            142
seed7/lib/lzma.s7i           0.062838            934
seed7/lib/lzw.s7i            0.066445            861
seed7/lib/magic.s7i          0.050882            403
seed7/lib/mahjng32.s7i       0.068025           1500
seed7/lib/make.s7i           0.056332            544
seed7/lib/makedata.s7i       0.078680           1428
seed7/lib/math.s7i           0.041377            201
seed7/lib/mixarith.s7i       0.043786            249
seed7/lib/modern27.s7i       0.086908           1099
seed7/lib/more.s7i           0.039309            130
seed7/lib/msgdigest.s7i      0.080843           1222
seed7/lib/multiscr.s7i       0.038141             68
seed7/lib/null_file.s7i      0.044400            345
seed7/lib/osfiles.s7i        0.067219           1085
seed7/lib/pbm.s7i            0.041818            230
seed7/lib/pcx.s7i            0.056067            638
seed7/lib/pem.s7i            0.042302            185
seed7/lib/pgm.s7i            0.044181            238
seed7/lib/pic16.s7i          0.051301           1037
seed7/lib/pic32.s7i          0.081539           2060
seed7/lib/pic_util.s7i       0.038977            144
seed7/lib/pixelimage.s7i     0.042656            320
seed7/lib/pixmap_file.s7i    0.046848            459
seed7/lib/pixmapfont.s7i     0.041873            184
seed7/lib/pkcs1.s7i          0.062125            543
seed7/lib/png.s7i            0.065812           1064
seed7/lib/poll.s7i           0.045849            313
seed7/lib/ppm.s7i            0.043011            240
seed7/lib/process.s7i        0.050429            541
seed7/lib/progs.s7i          0.057776            789
seed7/lib/propertyfile.s7i   0.040483            155
seed7/lib/rational.s7i       0.055445            792
seed7/lib/ref_list.s7i       0.043210            252
seed7/lib/reference.s7i      0.039679            126
seed7/lib/reverse.s7i        0.038140             94
seed7/lib/rpm.s7i            0.152451           3487
seed7/lib/rpmext.s7i         0.045356            318
seed7/lib/scanfile.s7i       0.083191           1779
seed7/lib/scanjson.s7i       0.048864            413
seed7/lib/scanstri.s7i       0.082299           1814
seed7/lib/scantoml.s7i       0.074055           1603
seed7/lib/seed7_05.s7i       0.068662           1072
seed7/lib/set.s7i            0.038277             57
seed7/lib/shell.s7i          0.055673            615
seed7/lib/showtls.s7i        0.056995            678
seed7/lib/signature.s7i      0.039764            131
seed7/lib/smtp.s7i           0.042839            261
seed7/lib/sockbase.s7i       0.043602            217
seed7/lib/socket.s7i         0.044885            326
seed7/lib/sokoban1.s7i       0.057412           1519
seed7/lib/sql_base.s7i       0.066262           1000
seed7/lib/stars.s7i          0.141531           1705
seed7/lib/stdfont10.s7i      0.086549           3347
seed7/lib/stdfont12.s7i      0.094613           3928
seed7/lib/stdfont14.s7i      0.105710           4510
seed7/lib/stdfont16.s7i      0.118117           5092
seed7/lib/stdfont18.s7i      0.136242           5868
seed7/lib/stdfont20.s7i      0.151562           6449
seed7/lib/stdfont24.s7i      0.184486           7421
seed7/lib/stdfont8.s7i       0.073515           2960
seed7/lib/stdfont9.s7i       0.077950           3152
seed7/lib/stdio.s7i          0.042283            192
seed7/lib/strifile.s7i       0.047299            345
seed7/lib/string.s7i         0.057048            779
seed7/lib/stritext.s7i       0.043751            352
seed7/lib/struct.s7i         0.046140            266
seed7/lib/struct_elem.s7i    0.039849            129
seed7/lib/subfile.s7i        0.041099            174
seed7/lib/subrange.s7i       0.038211             78
seed7/lib/syntax.s7i         0.047197            294
seed7/lib/tar.s7i            0.084250           1880
seed7/lib/tar_cmds.s7i       0.058504            752
seed7/lib/tdes.s7i           0.039686            143
seed7/lib/tee.s7i            0.038970            143
seed7/lib/text.s7i           0.039245            135
seed7/lib/tga.s7i            0.055385            676
seed7/lib/tiff.s7i           0.125569           2771
seed7/lib/time.s7i           0.065194           1191
seed7/lib/tls.s7i            0.106485           2230
seed7/lib/unicode.s7i        0.054975            575
seed7/lib/unionfnd.s7i       0.042572            130
seed7/lib/upper.s7i          0.043287            142
seed7/lib/utf16.s7i          0.053174            540
seed7/lib/utf8.s7i           0.042866            234
seed7/lib/vecfont10.s7i      0.080836           1056
seed7/lib/vecfont18.s7i      0.089734           1119
seed7/lib/vector3d.s7i       0.042764            293
seed7/lib/vectorfont.s7i     0.046204            239
seed7/lib/wildcard.s7i       0.039295            140
seed7/lib/window.s7i         0.045508            455
seed7/lib/wrinum.s7i         0.041102            248
seed7/lib/x509cert.s7i       0.072753           1243
seed7/lib/xml_ent.s7i        0.038428             94
seed7/lib/xmldom.s7i         0.041744            303
seed7/lib/xz.s7i             0.046196            442
seed7/lib/zip.s7i            0.123267           2792
seed7/lib/zstd.s7i           0.070863           1263
============================ ================= ================

Statistical Summary — Mode C (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.094209        |
+-----------+-----------------+
| Minimum   | 0.036415        |
+-----------+-----------------+
| Maximum   | 2.597008        |
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
seed7/prg/addup.sd7          0.047193            190
seed7/prg/bas7.sd7           0.787898          11459
seed7/prg/bifurk.sd7         0.039441             73
seed7/prg/bigfiles.sd7       0.044809            129
seed7/prg/brainf7.sd7        0.041974             86
seed7/prg/calc7.sd7          0.045075            128
seed7/prg/carddemo.sd7       0.048842            190
seed7/prg/castle.sd7         0.221276           3148
seed7/prg/cat.sd7            0.040174             82
seed7/prg/cellauto.sd7       0.040245             85
seed7/prg/celsius.sd7        0.039080             42
seed7/prg/chk_all.sd7        0.084582            843
seed7/prg/chkarr.sd7         0.889562           8367
seed7/prg/chkbig.sd7         4.257712          29026
seed7/prg/chkbin.sd7         1.058765           6469
seed7/prg/chkbitdata.sd7     1.283392           6624
seed7/prg/chkbool.sd7        0.240251           3157
seed7/prg/chkbst.sd7         0.108627            722
seed7/prg/chkchr.sd7         0.496045           2809
seed7/prg/chkcmd.sd7         0.114794           1205
seed7/prg/chkdb.sd7          0.777904           7454
seed7/prg/chkdecl.sd7        0.097295            448
seed7/prg/chkenum.sd7        0.124129           1230
seed7/prg/chkerr.sd7         0.348452           4663
seed7/prg/chkexc.sd7         0.157716           2627
seed7/prg/chkfil.sd7         0.132294           1615
seed7/prg/chkflt.sd7         2.908201          20620
seed7/prg/chkhent.sd7        0.041767             54
seed7/prg/chkhsh.sd7         0.519959           4548
seed7/prg/chkidx.sd7         3.279382          19567
seed7/prg/chkint.sd7         5.724086          38129
seed7/prg/chkjson.sd7        0.192756           1764
seed7/prg/chkovf.sd7         1.246020           8216
seed7/prg/chkprc.sd7         0.724023          10111
seed7/prg/chkscan.sd7        0.094194            714
seed7/prg/chkset.sd7         1.807403          11974
seed7/prg/chkstr.sd7         3.532060          26952
seed7/prg/chktime.sd7        0.254845           2025
seed7/prg/chktoml.sd7        0.203086           1656
seed7/prg/clock.sd7          0.038577             47
seed7/prg/clock2.sd7         0.038085             43
seed7/prg/clock3.sd7         0.043353             95
seed7/prg/cmpfil.sd7         0.039672             84
seed7/prg/comanche.sd7       0.048285            180
seed7/prg/confval.sd7        0.052691            175
seed7/prg/db7.sd7            0.065276            417
seed7/prg/diff7.sd7          0.056117            263
seed7/prg/dirtst.sd7         0.040362             42
seed7/prg/dirx.sd7           0.046068            152
seed7/prg/dnafight.sd7       0.125420           1381
seed7/prg/dragon.sd7         0.040538             73
seed7/prg/echo.sd7           0.038009             39
seed7/prg/eliza.sd7          0.053398            302
seed7/prg/err.sd7            0.045853             96
seed7/prg/fannkuch.sd7       0.043239            131
seed7/prg/fib.sd7            0.038303             47
seed7/prg/find7.sd7          0.043461            133
seed7/prg/findchar.sd7       0.045000            149
seed7/prg/fractree.sd7       0.038916             55
seed7/prg/ftp7.sd7           0.054906            296
seed7/prg/ftpserv.sd7        0.039017             74
seed7/prg/gcd.sd7            0.040310            109
seed7/prg/gkbd.sd7           0.063384            358
seed7/prg/gtksvtst.sd7       0.040973             94
seed7/prg/hal.sd7            0.049600            250
seed7/prg/hamu.sd7           0.068911            573
seed7/prg/hanoi.sd7          0.038029             55
seed7/prg/hd.sd7             0.041823             79
seed7/prg/hello.sd7          0.039579             32
seed7/prg/hilbert.sd7        0.045072            108
seed7/prg/ide7.sd7           0.050328            196
seed7/prg/kbd.sd7            0.038458             49
seed7/prg/klondike.sd7       0.089719            883
seed7/prg/lander.sd7         0.135708           1551
seed7/prg/lst80bas.sd7       0.057110            344
seed7/prg/lst99bas.sd7       0.061274            401
seed7/prg/lstgwbas.sd7       0.074347            577
seed7/prg/mahjong.sd7        0.151155           1943
seed7/prg/make7.sd7          0.044901            121
seed7/prg/mandelbr.sd7       0.087832            237
seed7/prg/mind.sd7           0.072176            443
seed7/prg/mirror.sd7         0.048404            131
seed7/prg/ms.sd7             0.075455            641
seed7/prg/nicoma.sd7         0.047575            135
seed7/prg/pac.sd7            0.074805            726
seed7/prg/pairs.sd7          0.145267           2025
seed7/prg/panic.sd7          0.198152           2634
seed7/prg/percolation.sd7    0.057287            330
seed7/prg/planets.sd7        0.141050           1486
seed7/prg/portfwd7.sd7       0.045310            139
seed7/prg/prime.sd7          0.040638             74
seed7/prg/printpi1.sd7       0.039502             56
seed7/prg/printpi2.sd7       0.039275             54
seed7/prg/printpi3.sd7       0.040136             60
seed7/prg/pv7.sd7            0.060657            337
seed7/prg/queen.sd7          0.044621            149
seed7/prg/rand.sd7           0.043236            121
seed7/prg/raytrace.sd7       0.075189            538
seed7/prg/rever.sd7          0.087296            816
seed7/prg/roman.sd7          0.038630             38
seed7/prg/s7c.sd7            0.642631           9060
seed7/prg/s7check.sd7        0.040735             68
seed7/prg/savehd7.sd7        0.114453           1110
seed7/prg/self.sd7           0.038909             49
seed7/prg/shisen.sd7         0.127802           1423
seed7/prg/sl.sd7             0.099586           1029
seed7/prg/snake.sd7          0.068195            615
seed7/prg/sokoban.sd7        0.085054            891
seed7/prg/spigotpi.sd7       0.039821             64
seed7/prg/sql7.sd7           0.053822            278
seed7/prg/startrek.sd7       0.096797            979
seed7/prg/sudoku7.sd7        0.203652           2657
seed7/prg/sydir7.sd7         0.062984            384
seed7/prg/syntaxhl.sd7       0.050805            177
seed7/prg/tak.sd7            0.042432             59
seed7/prg/tar7.sd7           0.046270            121
seed7/prg/tch.sd7            0.039464             55
seed7/prg/testfont.sd7       0.043366             95
seed7/prg/tet.sd7            0.064099            479
seed7/prg/tetg.sd7           0.067421            501
seed7/prg/toutf8.sd7         0.053962            240
seed7/prg/tst_cli.sd7        0.039017             40
seed7/prg/tst_srv.sd7        0.038721             47
seed7/prg/wator.sd7          0.081133            651
seed7/prg/which.sd7          0.040196             65
seed7/prg/wiz.sd7            0.216715           2833
seed7/prg/wordcnt.sd7        0.039011             54
seed7/prg/wrinum.sd7         0.037411             43
seed7/prg/wumpus.sd7         0.055244            372
seed7/lib/aes.s7i            0.204260           1144
seed7/lib/aes_gcm.s7i        0.070772            392
seed7/lib/ar.s7i             0.131414           1532
seed7/lib/arc4.s7i           0.047937            144
seed7/lib/archive.s7i        0.046331            143
seed7/lib/archive_base.s7i   0.045320            135
seed7/lib/array.s7i          0.078173            610
seed7/lib/asn1.s7i           0.066517            544
seed7/lib/asn1oid.s7i        0.051507            157
seed7/lib/basearray.s7i      0.066711            450
seed7/lib/bigfile.s7i        0.043508            136
seed7/lib/bigint.s7i         0.080139            824
seed7/lib/bigrat.s7i         0.085716            784
seed7/lib/bin16.s7i          0.072520            592
seed7/lib/bin32.s7i          0.064249            490
seed7/lib/bin64.s7i          0.066543            539
seed7/lib/bitdata.s7i        0.127899           1330
seed7/lib/bitmapfont.s7i     0.050739            215
seed7/lib/bitset.s7i         0.073357            593
seed7/lib/bitsetof.s7i       0.069647            431
seed7/lib/blowfish.s7i       0.080642            383
seed7/lib/bmp.s7i            0.103669            924
seed7/lib/boolean.s7i        0.058837            403
seed7/lib/browser.s7i        0.059494            280
seed7/lib/bstring.s7i        0.049183            227
seed7/lib/bytedata.s7i       0.068117            482
seed7/lib/bzip2.s7i          0.091255            887
seed7/lib/cards.s7i          0.105133           1342
seed7/lib/category.s7i       0.053813            209
seed7/lib/cc_conf.s7i        0.123010           1314
seed7/lib/ccittfax.s7i       0.103546           1022
seed7/lib/cgi.s7i            0.045295            109
seed7/lib/cgidialog.s7i      0.104730           1118
seed7/lib/char.s7i           0.054092            356
seed7/lib/charsets.s7i       0.132559           2024
seed7/lib/chartype.s7i       0.050132            121
seed7/lib/cipher.s7i         0.044066            146
seed7/lib/cli_cmds.s7i       0.117215           1360
seed7/lib/clib_file.s7i      0.054341            301
seed7/lib/color.s7i          0.048939            185
seed7/lib/complex.s7i        0.061663            464
seed7/lib/compress.s7i       0.045477            150
seed7/lib/console.s7i        0.049927            188
seed7/lib/cpio.s7i           0.149538           1708
seed7/lib/crc32.s7i          0.056405            193
seed7/lib/cronos16.s7i       0.203629           1173
seed7/lib/cronos27.s7i       0.262788           1464
seed7/lib/csv.s7i            0.052613            201
seed7/lib/db_prop.s7i        0.105392            991
seed7/lib/deflate.s7i        0.089467            740
seed7/lib/des.s7i            0.083576            444
seed7/lib/dialog.s7i         0.058923            311
seed7/lib/dir.s7i            0.045230            163
seed7/lib/draw.s7i           0.088614            854
seed7/lib/duration.s7i       0.104176           1038
seed7/lib/echo.s7i           0.046945            132
seed7/lib/editline.s7i       0.061908            398
seed7/lib/elf.s7i            0.159925           1560
seed7/lib/elliptic.s7i       0.078656            649
seed7/lib/enable_io.s7i      0.054322            312
seed7/lib/encoding.s7i       0.100466            931
seed7/lib/enumeration.s7i    0.051431            236
seed7/lib/environment.s7i    0.045734            175
seed7/lib/exif.s7i           0.047873            152
seed7/lib/external_file.s7i  0.053736            340
seed7/lib/field.s7i          0.053877            268
seed7/lib/file.s7i           0.056245            372
seed7/lib/filebits.s7i       0.039862             46
seed7/lib/filesys.s7i        0.066897            601
seed7/lib/fileutil.s7i       0.045071            144
seed7/lib/fixarray.s7i       0.054888            307
seed7/lib/float.s7i          0.076427            757
seed7/lib/font.s7i           0.045797            196
seed7/lib/font8x8.s7i        0.069196            998
seed7/lib/forloop.s7i        0.061556            449
seed7/lib/ftp.s7i            0.089081            969
seed7/lib/ftpserv.s7i        0.078292            631
seed7/lib/getf.s7i           0.042970            115
seed7/lib/gethttp.s7i        0.038715             41
seed7/lib/gethttps.s7i       0.039594             41
seed7/lib/gif.s7i            0.073283            561
seed7/lib/graph.s7i          0.066370            415
seed7/lib/graph_file.s7i     0.059528            399
seed7/lib/gtkserver.s7i      0.043615            161
seed7/lib/gzip.s7i           0.070200            573
seed7/lib/hash.s7i           0.068449            421
seed7/lib/hashsetof.s7i      0.067971            499
seed7/lib/hmac.s7i           0.046052            152
seed7/lib/html.s7i           0.041980             83
seed7/lib/html_ent.s7i       0.065503            476
seed7/lib/htmldom.s7i        0.056103            286
seed7/lib/http_request.s7i   0.082945            696
seed7/lib/http_srv_resp.s7i  0.062644            380
seed7/lib/https_request.s7i  0.050773            211
seed7/lib/httpserv.s7i       0.059920            345
seed7/lib/huffman.s7i        0.078791            644
seed7/lib/ico.s7i            0.051664            221
seed7/lib/idxarray.s7i       0.052733            232
seed7/lib/image.s7i          0.042641            156
seed7/lib/imagefile.s7i      0.046229            171
seed7/lib/inflate.s7i        0.066572            411
seed7/lib/inifile.s7i        0.043919            129
seed7/lib/integer.s7i        0.069756            663
seed7/lib/iobuffer.s7i       0.052977            289
seed7/lib/jpeg.s7i           0.159923           1761
seed7/lib/json.s7i           0.083478            891
seed7/lib/json_serde.s7i     0.081195            783
seed7/lib/keybd.s7i          0.081803            639
seed7/lib/keydescr.s7i       0.051821            192
seed7/lib/leb128.s7i         0.047811            218
seed7/lib/line.s7i           0.045113            164
seed7/lib/listener.s7i       0.050191            247
seed7/lib/logfile.s7i        0.041420             73
seed7/lib/lower.s7i          0.043664            142
seed7/lib/lzma.s7i           0.101917            934
seed7/lib/lzw.s7i            0.091627            861
seed7/lib/magic.s7i          0.065127            403
seed7/lib/mahjng32.s7i       0.094421           1500
seed7/lib/make.s7i           0.070513            544
seed7/lib/makedata.s7i       0.124303           1428
seed7/lib/math.s7i           0.047064            201
seed7/lib/mixarith.s7i       0.048573            249
seed7/lib/modern27.s7i       0.176122           1099
seed7/lib/more.s7i           0.044338            130
seed7/lib/msgdigest.s7i      0.142170           1222
seed7/lib/multiscr.s7i       0.039919             68
seed7/lib/null_file.s7i      0.052895            345
seed7/lib/osfiles.s7i        0.097374           1085
seed7/lib/pbm.s7i            0.049918            230
seed7/lib/pcx.s7i            0.080072            638
seed7/lib/pem.s7i            0.045796            185
seed7/lib/pgm.s7i            0.050576            238
seed7/lib/pic16.s7i          0.069715           1037
seed7/lib/pic32.s7i          0.124984           2060
seed7/lib/pic_util.s7i       0.045311            144
seed7/lib/pixelimage.s7i     0.054871            320
seed7/lib/pixmap_file.s7i    0.065499            459
seed7/lib/pixmapfont.s7i     0.049164            184
seed7/lib/pkcs1.s7i          0.079134            543
seed7/lib/png.s7i            0.111013           1064
seed7/lib/poll.s7i           0.054490            313
seed7/lib/ppm.s7i            0.049956            240
seed7/lib/process.s7i        0.065620            541
seed7/lib/progs.s7i          0.082045            789
seed7/lib/propertyfile.s7i   0.045022            155
seed7/lib/rational.s7i       0.080127            792
seed7/lib/ref_list.s7i       0.049788            252
seed7/lib/reference.s7i      0.043651            126
seed7/lib/reverse.s7i        0.041934             94
seed7/lib/rpm.s7i            0.294845           3487
seed7/lib/rpmext.s7i         0.054959            318
seed7/lib/scanfile.s7i       0.136142           1779
seed7/lib/scanjson.s7i       0.063127            413
seed7/lib/scanstri.s7i       0.140384           1814
seed7/lib/scantoml.s7i       0.135382           1603
seed7/lib/seed7_05.s7i       0.113406           1072
seed7/lib/set.s7i            0.039595             57
seed7/lib/shell.s7i          0.070982            615
seed7/lib/showtls.s7i        0.087747            678
seed7/lib/signature.s7i      0.044405            131
seed7/lib/smtp.s7i           0.051137            261
seed7/lib/sockbase.s7i       0.052228            217
seed7/lib/socket.s7i         0.056213            326
seed7/lib/sokoban1.s7i       0.084412           1519
seed7/lib/sql_base.s7i       0.102495           1000
seed7/lib/stars.s7i          0.243425           1705
seed7/lib/stdfont10.s7i      0.148347           3347
seed7/lib/stdfont12.s7i      0.170354           3928
seed7/lib/stdfont14.s7i      0.196206           4510
seed7/lib/stdfont16.s7i      0.222559           5092
seed7/lib/stdfont18.s7i      0.255333           5868
seed7/lib/stdfont20.s7i      0.281887           6449
seed7/lib/stdfont24.s7i      0.338947           7421
seed7/lib/stdfont8.s7i       0.132782           2960
seed7/lib/stdfont9.s7i       0.143055           3152
seed7/lib/stdio.s7i          0.045835            192
seed7/lib/strifile.s7i       0.055393            345
seed7/lib/string.s7i         0.077217            779
seed7/lib/stritext.s7i       0.056298            352
seed7/lib/struct.s7i         0.057729            266
seed7/lib/struct_elem.s7i    0.047468            129
seed7/lib/subfile.s7i        0.047405            174
seed7/lib/subrange.s7i       0.040659             78
seed7/lib/syntax.s7i         0.063142            294
seed7/lib/tar.s7i            0.150501           1880
seed7/lib/tar_cmds.s7i       0.087308            752
seed7/lib/tdes.s7i           0.045998            143
seed7/lib/tee.s7i            0.044361            143
seed7/lib/text.s7i           0.044665            135
seed7/lib/tga.s7i            0.082708            676
seed7/lib/tiff.s7i           0.250859           2771
seed7/lib/time.s7i           0.105024           1191
seed7/lib/tls.s7i            0.202900           2230
seed7/lib/unicode.s7i        0.077754            575
seed7/lib/unionfnd.s7i       0.045860            130
seed7/lib/upper.s7i          0.044187            142
seed7/lib/utf16.s7i          0.068398            540
seed7/lib/utf8.s7i           0.050525            234
seed7/lib/vecfont10.s7i      0.165607           1056
seed7/lib/vecfont18.s7i      0.184383           1119
seed7/lib/vector3d.s7i       0.052171            293
seed7/lib/vectorfont.s7i     0.049805            239
seed7/lib/wildcard.s7i       0.044412            140
seed7/lib/window.s7i         0.063764            455
seed7/lib/wrinum.s7i         0.053151            248
seed7/lib/x509cert.s7i       0.122771           1243
seed7/lib/xml_ent.s7i        0.041728             94
seed7/lib/xmldom.s7i         0.054790            303
seed7/lib/xz.s7i             0.062829            442
seed7/lib/zip.s7i            0.241086           2792
seed7/lib/zstd.s7i           0.121809           1263
============================ ================= ================

Statistical Summary — Mode D (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.164394        |
+-----------+-----------------+
| Minimum   | 0.037411        |
+-----------+-----------------+
| Maximum   | 5.724086        |
+-----------+-----------------+

Cross-Mode Comparison
=====================

+------+-----------------+-----------------+-----------------+
| Mode | Average (s)     | Minimum (s)     | Maximum (s)     |
+======+=================+=================+=================+
| A    | 0.033764        | 0.032082        | 0.041644        |
+------+-----------------+-----------------+-----------------+
| B    | 0.039331        | 0.034326        | 0.049414        |
+------+-----------------+-----------------+-----------------+
| C    | 0.094209        | 0.036415        | 2.597008        |
+------+-----------------+-----------------+-----------------+
| D    | 0.164394        | 0.037411        | 5.724086        |
+------+-----------------+-----------------+-----------------+

Phase Timing Summary
====================

Wall-clock times include warm-up work and GC performed by the warm-up helper.

+----------+--------------+--------------+--------------+
| Phase    | Warm-up      | Timed pass   | Mode total   |
+==========+==============+==============+==============+
| Mode A   | 00:00:12.464 | 00:00:57.528 | 00:01:09.992 |
+----------+--------------+--------------+--------------+
| Mode B   | 00:00:14.965 | 00:01:07.078 | 00:01:22.044 |
+----------+--------------+--------------+--------------+
| Mode C   | 00:00:38.492 | 00:02:41.483 | 00:03:19.976 |
+----------+--------------+--------------+--------------+
| Mode D   | 00:01:03.282 | 00:04:41.239 | 00:05:44.521 |
+----------+--------------+--------------+--------------+
| Total    |              |              | 00:11:36.542 |
+----------+--------------+--------------+--------------+


.. ---------------------------------------------------------------------------
