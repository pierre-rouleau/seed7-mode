=======================================================
GC-Controlled Benchmark Report: Seed7 Mode — Four Modes
=======================================================

:Running with: seed7-mode 2026-07-01T17:00:53+0000 W27-3
:Seed7 dir   : /Users/roup/my/dvo/seed7-repos/seed7/
:Started at: 13:13:40 local time
:Generated on: 2026-07-01 17:25:01 UTC
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
seed7/prg/addup.sd7          0.033892            190
seed7/prg/bas7.sd7           0.033469          11459
seed7/prg/bifurk.sd7         0.032570             73
seed7/prg/bigfiles.sd7       0.032997            129
seed7/prg/brainf7.sd7        0.032749             86
seed7/prg/calc7.sd7          0.032418            128
seed7/prg/carddemo.sd7       0.034410            190
seed7/prg/castle.sd7         0.034285           3148
seed7/prg/cat.sd7            0.033757             82
seed7/prg/cellauto.sd7       0.033833             85
seed7/prg/celsius.sd7        0.033659             42
seed7/prg/chk_all.sd7        0.034697            843
seed7/prg/chkarr.sd7         0.059686           8367
seed7/prg/chkbig.sd7         0.037558          29026
seed7/prg/chkbin.sd7         0.035734           6469
seed7/prg/chkbitdata.sd7     0.036934           6624
seed7/prg/chkbool.sd7        0.034981           3157
seed7/prg/chkbst.sd7         0.034892            722
seed7/prg/chkchr.sd7         0.038837           2809
seed7/prg/chkcmd.sd7         0.041501           1205
seed7/prg/chkdb.sd7          0.035495           7454
seed7/prg/chkdecl.sd7        0.033375            448
seed7/prg/chkenum.sd7        0.033239           1230
seed7/prg/chkerr.sd7         0.033126           4663
seed7/prg/chkexc.sd7         0.033076           2627
seed7/prg/chkfil.sd7         0.032885           1615
seed7/prg/chkflt.sd7         0.040523          20620
seed7/prg/chkhent.sd7        0.034708             54
seed7/prg/chkhsh.sd7         0.050848           4548
seed7/prg/chkidx.sd7         0.040859          19567
seed7/prg/chkint.sd7         0.042585          38129
seed7/prg/chkjson.sd7        0.034714           1764
seed7/prg/chkovf.sd7         0.035679           8216
seed7/prg/chkprc.sd7         0.035298          10111
seed7/prg/chkscan.sd7        0.034813            714
seed7/prg/chkset.sd7         0.037156          11974
seed7/prg/chkstr.sd7         0.039948          26952
seed7/prg/chktime.sd7        0.035221           2025
seed7/prg/chktoml.sd7        0.033999           1656
seed7/prg/clock.sd7          0.032852             47
seed7/prg/clock2.sd7         0.040076             43
seed7/prg/clock3.sd7         0.039394             95
seed7/prg/cmpfil.sd7         0.033149             84
seed7/prg/comanche.sd7       0.035249            180
seed7/prg/confval.sd7        0.034099            175
seed7/prg/db7.sd7            0.035108            417
seed7/prg/diff7.sd7          0.034634            263
seed7/prg/dirtst.sd7         0.034324             42
seed7/prg/dirx.sd7           0.033805            152
seed7/prg/dnafight.sd7       0.034295           1381
seed7/prg/dragon.sd7         0.034095             73
seed7/prg/echo.sd7           0.036106             39
seed7/prg/eliza.sd7          0.034675            302
seed7/prg/err.sd7            0.033819             96
seed7/prg/fannkuch.sd7       0.033004            131
seed7/prg/fib.sd7            0.033012             47
seed7/prg/find7.sd7          0.033146            133
seed7/prg/findchar.sd7       0.033192            149
seed7/prg/fractree.sd7       0.033023             55
seed7/prg/ftp7.sd7           0.034047            296
seed7/prg/ftpserv.sd7        0.033520             74
seed7/prg/gcd.sd7            0.032719            109
seed7/prg/gkbd.sd7           0.033104            358
seed7/prg/gtksvtst.sd7       0.034027             94
seed7/prg/hal.sd7            0.033934            250
seed7/prg/hamu.sd7           0.033749            573
seed7/prg/hanoi.sd7          0.034733             55
seed7/prg/hd.sd7             0.033640             79
seed7/prg/hello.sd7          0.033729             32
seed7/prg/hilbert.sd7        0.033556            108
seed7/prg/ide7.sd7           0.033826            196
seed7/prg/kbd.sd7            0.034062             49
seed7/prg/klondike.sd7       0.034085            883
seed7/prg/lander.sd7         0.034093           1551
seed7/prg/lst80bas.sd7       0.033972            344
seed7/prg/lst99bas.sd7       0.033666            401
seed7/prg/lstgwbas.sd7       0.033932            577
seed7/prg/mahjong.sd7        0.033880           1943
seed7/prg/make7.sd7          0.033704            121
seed7/prg/mandelbr.sd7       0.033955            237
seed7/prg/mind.sd7           0.033803            443
seed7/prg/mirror.sd7         0.033643            131
seed7/prg/ms.sd7             0.033680            641
seed7/prg/nicoma.sd7         0.033894            135
seed7/prg/pac.sd7            0.033655            726
seed7/prg/pairs.sd7          0.034034           2025
seed7/prg/panic.sd7          0.033857           2634
seed7/prg/percolation.sd7    0.033995            330
seed7/prg/planets.sd7        0.034019           1486
seed7/prg/portfwd7.sd7       0.032486            139
seed7/prg/prime.sd7          0.033018             74
seed7/prg/printpi1.sd7       0.032717             56
seed7/prg/printpi2.sd7       0.032756             54
seed7/prg/printpi3.sd7       0.033034             60
seed7/prg/pv7.sd7            0.033164            337
seed7/prg/queen.sd7          0.034799            149
seed7/prg/rand.sd7           0.033876            121
seed7/prg/raytrace.sd7       0.033774            538
seed7/prg/rever.sd7          0.033985            816
seed7/prg/roman.sd7          0.033409             38
seed7/prg/s7c.sd7            0.034509           9060
seed7/prg/s7check.sd7        0.033501             68
seed7/prg/savehd7.sd7        0.033779           1110
seed7/prg/self.sd7           0.033709             49
seed7/prg/shisen.sd7         0.033517           1423
seed7/prg/sl.sd7             0.033169           1029
seed7/prg/snake.sd7          0.033823            615
seed7/prg/sokoban.sd7        0.034055            891
seed7/prg/spigotpi.sd7       0.034068             64
seed7/prg/sql7.sd7           0.033761            278
seed7/prg/startrek.sd7       0.033564            979
seed7/prg/sudoku7.sd7        0.033700           2657
seed7/prg/sydir7.sd7         0.033794            384
seed7/prg/syntaxhl.sd7       0.033708            177
seed7/prg/tak.sd7            0.033441             59
seed7/prg/tar7.sd7           0.034154            121
seed7/prg/tch.sd7            0.034263             55
seed7/prg/testfont.sd7       0.034312             95
seed7/prg/tet.sd7            0.034013            479
seed7/prg/tetg.sd7           0.034206            501
seed7/prg/toutf8.sd7         0.034171            240
seed7/prg/tst_cli.sd7        0.033393             40
seed7/prg/tst_srv.sd7        0.032957             47
seed7/prg/wator.sd7          0.032964            651
seed7/prg/which.sd7          0.032910             65
seed7/prg/wiz.sd7            0.033632           2833
seed7/prg/wordcnt.sd7        0.035112             54
seed7/prg/wrinum.sd7         0.034018             43
seed7/prg/wumpus.sd7         0.033762            372
seed7/lib/aes.s7i            0.033633           1144
seed7/lib/aes_gcm.s7i        0.033523            392
seed7/lib/ar.s7i             0.034737           1532
seed7/lib/arc4.s7i           0.033951            144
seed7/lib/archive.s7i        0.033850            143
seed7/lib/archive_base.s7i   0.033776            135
seed7/lib/array.s7i          0.033534            610
seed7/lib/asn1.s7i           0.034780            544
seed7/lib/asn1oid.s7i        0.033663            157
seed7/lib/basearray.s7i      0.034124            450
seed7/lib/bigfile.s7i        0.033850            136
seed7/lib/bigint.s7i         0.033917            824
seed7/lib/bigrat.s7i         0.034053            784
seed7/lib/bin16.s7i          0.033938            592
seed7/lib/bin32.s7i          0.033409            490
seed7/lib/bin64.s7i          0.034750            539
seed7/lib/bitdata.s7i        0.037583           1330
seed7/lib/bitmapfont.s7i     0.034945            215
seed7/lib/bitset.s7i         0.034198            593
seed7/lib/bitsetof.s7i       0.034143            431
seed7/lib/blowfish.s7i       0.033802            383
seed7/lib/bmp.s7i            0.033426            924
seed7/lib/boolean.s7i        0.032693            403
seed7/lib/browser.s7i        0.033057            280
seed7/lib/bstring.s7i        0.032772            227
seed7/lib/bytedata.s7i       0.033209            482
seed7/lib/bzip2.s7i          0.032742            887
seed7/lib/cards.s7i          0.033855           1342
seed7/lib/category.s7i       0.034664            209
seed7/lib/cc_conf.s7i        0.034016           1314
seed7/lib/ccittfax.s7i       0.033920           1022
seed7/lib/cgi.s7i            0.034666            109
seed7/lib/cgidialog.s7i      0.033909           1118
seed7/lib/char.s7i           0.033888            356
seed7/lib/charsets.s7i       0.033934           2024
seed7/lib/chartype.s7i       0.033523            121
seed7/lib/cipher.s7i         0.034302            146
seed7/lib/cli_cmds.s7i       0.034209           1360
seed7/lib/clib_file.s7i      0.033781            301
seed7/lib/color.s7i          0.033790            185
seed7/lib/complex.s7i        0.034124            464
seed7/lib/compress.s7i       0.034017            150
seed7/lib/console.s7i        0.033695            188
seed7/lib/cpio.s7i           0.033521           1708
seed7/lib/crc32.s7i          0.033920            193
seed7/lib/cronos16.s7i       0.033800           1173
seed7/lib/cronos27.s7i       0.033791           1464
seed7/lib/csv.s7i            0.033519            201
seed7/lib/db_prop.s7i        0.033734            991
seed7/lib/deflate.s7i        0.033790            740
seed7/lib/des.s7i            0.033994            444
seed7/lib/dialog.s7i         0.034206            311
seed7/lib/dir.s7i            0.033687            163
seed7/lib/draw.s7i           0.033132            854
seed7/lib/duration.s7i       0.032756           1038
seed7/lib/echo.s7i           0.034183            132
seed7/lib/editline.s7i       0.032510            398
seed7/lib/elf.s7i            0.032760           1560
seed7/lib/elliptic.s7i       0.032835            649
seed7/lib/enable_io.s7i      0.035603            312
seed7/lib/encoding.s7i       0.034102            931
seed7/lib/enumeration.s7i    0.033670            236
seed7/lib/environment.s7i    0.033790            175
seed7/lib/exif.s7i           0.033777            152
seed7/lib/external_file.s7i  0.033964            340
seed7/lib/field.s7i          0.034060            268
seed7/lib/file.s7i           0.033727            372
seed7/lib/filebits.s7i       0.033615             46
seed7/lib/filesys.s7i        0.034288            601
seed7/lib/fileutil.s7i       0.033819            144
seed7/lib/fixarray.s7i       0.033749            307
seed7/lib/float.s7i          0.033668            757
seed7/lib/font.s7i           0.033657            196
seed7/lib/font8x8.s7i        0.033919            998
seed7/lib/forloop.s7i        0.033866            449
seed7/lib/ftp.s7i            0.033834            969
seed7/lib/ftpserv.s7i        0.033782            631
seed7/lib/getf.s7i           0.035296            115
seed7/lib/gethttp.s7i        0.036874             41
seed7/lib/gethttps.s7i       0.034359             41
seed7/lib/gif.s7i            0.033961            561
seed7/lib/graph.s7i          0.033537            415
seed7/lib/graph_file.s7i     0.033665            399
seed7/lib/gtkserver.s7i      0.033787            161
seed7/lib/gzip.s7i           0.033620            573
seed7/lib/hash.s7i           0.033096            421
seed7/lib/hashsetof.s7i      0.032998            499
seed7/lib/hmac.s7i           0.032558            152
seed7/lib/html.s7i           0.032508             83
seed7/lib/html_ent.s7i       0.032930            476
seed7/lib/htmldom.s7i        0.034435            286
seed7/lib/http_request.s7i   0.034558            696
seed7/lib/http_srv_resp.s7i  0.033962            380
seed7/lib/https_request.s7i  0.033824            211
seed7/lib/httpserv.s7i       0.034332            345
seed7/lib/huffman.s7i        0.033888            644
seed7/lib/ico.s7i            0.033828            221
seed7/lib/idxarray.s7i       0.033867            232
seed7/lib/image.s7i          0.033877            156
seed7/lib/imagefile.s7i      0.033692            171
seed7/lib/inflate.s7i        0.034271            411
seed7/lib/inifile.s7i        0.034089            129
seed7/lib/integer.s7i        0.033190            663
seed7/lib/iobuffer.s7i       0.033950            289
seed7/lib/jpeg.s7i           0.033990           1761
seed7/lib/json.s7i           0.033759            891
seed7/lib/json_serde.s7i     0.033713            783
seed7/lib/keybd.s7i          0.034654            639
seed7/lib/keydescr.s7i       0.033708            192
seed7/lib/leb128.s7i         0.033818            218
seed7/lib/line.s7i           0.033799            164
seed7/lib/listener.s7i       0.033542            247
seed7/lib/logfile.s7i        0.033785             73
seed7/lib/lower.s7i          0.033870            142
seed7/lib/lzma.s7i           0.033709            934
seed7/lib/lzw.s7i            0.034483            861
seed7/lib/magic.s7i          0.033039            403
seed7/lib/mahjng32.s7i       0.032825           1500
seed7/lib/make.s7i           0.032612            544
seed7/lib/makedata.s7i       0.033313           1428
seed7/lib/math.s7i           0.032697            201
seed7/lib/mixarith.s7i       0.032454            249
seed7/lib/modern27.s7i       0.035202           1099
seed7/lib/more.s7i           0.033968            130
seed7/lib/msgdigest.s7i      0.033878           1222
seed7/lib/multiscr.s7i       0.033756             68
seed7/lib/null_file.s7i      0.033566            345
seed7/lib/osfiles.s7i        0.033789           1085
seed7/lib/pbm.s7i            0.033668            230
seed7/lib/pcx.s7i            0.033601            638
seed7/lib/pem.s7i            0.034199            185
seed7/lib/pgm.s7i            0.033810            238
seed7/lib/pic16.s7i          0.033662           1037
seed7/lib/pic32.s7i          0.034034           2060
seed7/lib/pic_util.s7i       0.033849            144
seed7/lib/pixelimage.s7i     0.033626            320
seed7/lib/pixmap_file.s7i    0.033945            459
seed7/lib/pixmapfont.s7i     0.033857            184
seed7/lib/pkcs1.s7i          0.033623            543
seed7/lib/png.s7i            0.033784           1064
seed7/lib/poll.s7i           0.033652            313
seed7/lib/ppm.s7i            0.033613            240
seed7/lib/process.s7i        0.033436            541
seed7/lib/progs.s7i          0.033398            789
seed7/lib/propertyfile.s7i   0.034256            155
seed7/lib/rational.s7i       0.033679            792
seed7/lib/ref_list.s7i       0.033922            252
seed7/lib/reference.s7i      0.033413            126
seed7/lib/reverse.s7i        0.032737             94
seed7/lib/rpm.s7i            0.033246           3487
seed7/lib/rpmext.s7i         0.032776            318
seed7/lib/scanfile.s7i       0.032987           1779
seed7/lib/scanjson.s7i       0.032916            413
seed7/lib/scanstri.s7i       0.033537           1814
seed7/lib/scantoml.s7i       0.034432           1603
seed7/lib/seed7_05.s7i       0.034145           1072
seed7/lib/set.s7i            0.033897             57
seed7/lib/shell.s7i          0.033831            615
seed7/lib/showtls.s7i        0.034088            678
seed7/lib/signature.s7i      0.033623            131
seed7/lib/smtp.s7i           0.034889            261
seed7/lib/sockbase.s7i       0.034367            217
seed7/lib/socket.s7i         0.033654            326
seed7/lib/sokoban1.s7i       0.033875           1519
seed7/lib/sql_base.s7i       0.033961           1000
seed7/lib/stars.s7i          0.033768           1705
seed7/lib/stdfont10.s7i      0.033916           3347
seed7/lib/stdfont12.s7i      0.033944           3928
seed7/lib/stdfont14.s7i      0.033826           4510
seed7/lib/stdfont16.s7i      0.034140           5092
seed7/lib/stdfont18.s7i      0.034013           5868
seed7/lib/stdfont20.s7i      0.034011           6449
seed7/lib/stdfont24.s7i      0.034046           7421
seed7/lib/stdfont8.s7i       0.034811           2960
seed7/lib/stdfont9.s7i       0.033842           3152
seed7/lib/stdio.s7i          0.033684            192
seed7/lib/strifile.s7i       0.034395            345
seed7/lib/string.s7i         0.036864            779
seed7/lib/stritext.s7i       0.034331            352
seed7/lib/struct.s7i         0.034352            266
seed7/lib/struct_elem.s7i    0.036610            129
seed7/lib/subfile.s7i        0.032956            174
seed7/lib/subrange.s7i       0.032747             78
seed7/lib/syntax.s7i         0.033007            294
seed7/lib/tar.s7i            0.032768           1880
seed7/lib/tar_cmds.s7i       0.034302            752
seed7/lib/tdes.s7i           0.035919            143
seed7/lib/tee.s7i            0.037330            143
seed7/lib/text.s7i           0.033934            135
seed7/lib/tga.s7i            0.033777            676
seed7/lib/tiff.s7i           0.034203           2771
seed7/lib/time.s7i           0.033694           1191
seed7/lib/tls.s7i            0.033851           2230
seed7/lib/unicode.s7i        0.034288            575
seed7/lib/unionfnd.s7i       0.033718            130
seed7/lib/upper.s7i          0.034058            142
seed7/lib/utf16.s7i          0.036231            540
seed7/lib/utf8.s7i           0.034603            234
seed7/lib/vecfont10.s7i      0.033892           1056
seed7/lib/vecfont18.s7i      0.034704           1119
seed7/lib/vector3d.s7i       0.033947            293
seed7/lib/vectorfont.s7i     0.033778            239
seed7/lib/wildcard.s7i       0.033989            140
seed7/lib/window.s7i         0.033895            455
seed7/lib/wrinum.s7i         0.034019            248
seed7/lib/x509cert.s7i       0.033767           1243
seed7/lib/xml_ent.s7i        0.033971             94
seed7/lib/xmldom.s7i         0.033806            303
seed7/lib/xz.s7i             0.033806            442
seed7/lib/zip.s7i            0.033872           2792
seed7/lib/zstd.s7i           0.033035           1263
============================ ================= ================

Statistical Summary — Mode A (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.034187        |
+-----------+-----------------+
| Minimum   | 0.032418        |
+-----------+-----------------+
| Maximum   | 0.059686        |
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
seed7/prg/addup.sd7          0.039369            190
seed7/prg/bas7.sd7           0.040793          11459
seed7/prg/bifurk.sd7         0.038442             73
seed7/prg/bigfiles.sd7       0.038739            129
seed7/prg/brainf7.sd7        0.037247             86
seed7/prg/calc7.sd7          0.037315            128
seed7/prg/carddemo.sd7       0.038327            190
seed7/prg/castle.sd7         0.037921           3148
seed7/prg/cat.sd7            0.036402             82
seed7/prg/cellauto.sd7       0.037875             85
seed7/prg/celsius.sd7        0.036197             42
seed7/prg/chk_all.sd7        0.039315            843
seed7/prg/chkarr.sd7         0.040107           8367
seed7/prg/chkbig.sd7         0.044474          29026
seed7/prg/chkbin.sd7         0.040435           6469
seed7/prg/chkbitdata.sd7     0.040694           6624
seed7/prg/chkbool.sd7        0.038744           3157
seed7/prg/chkbst.sd7         0.039832            722
seed7/prg/chkchr.sd7         0.041056           2809
seed7/prg/chkcmd.sd7         0.039011           1205
seed7/prg/chkdb.sd7          0.040294           7454
seed7/prg/chkdecl.sd7        0.039174            448
seed7/prg/chkenum.sd7        0.038748           1230
seed7/prg/chkerr.sd7         0.043214           4663
seed7/prg/chkexc.sd7         0.047807           2627
seed7/prg/chkfil.sd7         0.043671           1615
seed7/prg/chkflt.sd7         0.043253          20620
seed7/prg/chkhent.sd7        0.036543             54
seed7/prg/chkhsh.sd7         0.041000           4548
seed7/prg/chkidx.sd7         0.040169          19567
seed7/prg/chkint.sd7         0.044606          38129
seed7/prg/chkjson.sd7        0.038128           1764
seed7/prg/chkovf.sd7         0.038318           8216
seed7/prg/chkprc.sd7         0.037127          10111
seed7/prg/chkscan.sd7        0.039985            714
seed7/prg/chkset.sd7         0.041284          11974
seed7/prg/chkstr.sd7         0.043889          26952
seed7/prg/chktime.sd7        0.039911           2025
seed7/prg/chktoml.sd7        0.041103           1656
seed7/prg/clock.sd7          0.036300             47
seed7/prg/clock2.sd7         0.036469             43
seed7/prg/clock3.sd7         0.039101             95
seed7/prg/cmpfil.sd7         0.037599             84
seed7/prg/comanche.sd7       0.039411            180
seed7/prg/confval.sd7        0.040342            175
seed7/prg/db7.sd7            0.038992            417
seed7/prg/diff7.sd7          0.039028            263
seed7/prg/dirtst.sd7         0.036276             42
seed7/prg/dirx.sd7           0.038615            152
seed7/prg/dnafight.sd7       0.039634           1381
seed7/prg/dragon.sd7         0.037333             73
seed7/prg/echo.sd7           0.036000             39
seed7/prg/eliza.sd7          0.039000            302
seed7/prg/err.sd7            0.042101             96
seed7/prg/fannkuch.sd7       0.039187            131
seed7/prg/fib.sd7            0.036431             47
seed7/prg/find7.sd7          0.038157            133
seed7/prg/findchar.sd7       0.037340            149
seed7/prg/fractree.sd7       0.035739             55
seed7/prg/ftp7.sd7           0.038691            296
seed7/prg/ftpserv.sd7        0.036459             74
seed7/prg/gcd.sd7            0.037528            109
seed7/prg/gkbd.sd7           0.041067            358
seed7/prg/gtksvtst.sd7       0.037833             94
seed7/prg/hal.sd7            0.038438            250
seed7/prg/hamu.sd7           0.038995            573
seed7/prg/hanoi.sd7          0.036324             55
seed7/prg/hd.sd7             0.037469             79
seed7/prg/hello.sd7          0.035506             32
seed7/prg/hilbert.sd7        0.038734            108
seed7/prg/ide7.sd7           0.039269            196
seed7/prg/kbd.sd7            0.036694             49
seed7/prg/klondike.sd7       0.038812            883
seed7/prg/lander.sd7         0.039412           1551
seed7/prg/lst80bas.sd7       0.038279            344
seed7/prg/lst99bas.sd7       0.040090            401
seed7/prg/lstgwbas.sd7       0.040332            577
seed7/prg/mahjong.sd7        0.039164           1943
seed7/prg/make7.sd7          0.039307            121
seed7/prg/mandelbr.sd7       0.041087            237
seed7/prg/mind.sd7           0.042268            443
seed7/prg/mirror.sd7         0.044294            131
seed7/prg/ms.sd7             0.039496            641
seed7/prg/nicoma.sd7         0.038604            135
seed7/prg/pac.sd7            0.037710            726
seed7/prg/pairs.sd7          0.037760           2025
seed7/prg/panic.sd7          0.039156           2634
seed7/prg/percolation.sd7    0.038011            330
seed7/prg/planets.sd7        0.038435           1486
seed7/prg/portfwd7.sd7       0.039654            139
seed7/prg/prime.sd7          0.037136             74
seed7/prg/printpi1.sd7       0.037168             56
seed7/prg/printpi2.sd7       0.037058             54
seed7/prg/printpi3.sd7       0.036615             60
seed7/prg/pv7.sd7            0.039615            337
seed7/prg/queen.sd7          0.039272            149
seed7/prg/rand.sd7           0.038973            121
seed7/prg/raytrace.sd7       0.040085            538
seed7/prg/rever.sd7          0.039094            816
seed7/prg/roman.sd7          0.036177             38
seed7/prg/s7c.sd7            0.039176           9060
seed7/prg/s7check.sd7        0.037344             68
seed7/prg/savehd7.sd7        0.038906           1110
seed7/prg/self.sd7           0.036472             49
seed7/prg/shisen.sd7         0.039047           1423
seed7/prg/sl.sd7             0.038630           1029
seed7/prg/snake.sd7          0.038770            615
seed7/prg/sokoban.sd7        0.039442            891
seed7/prg/spigotpi.sd7       0.039099             64
seed7/prg/sql7.sd7           0.046188            278
seed7/prg/startrek.sd7       0.040320            979
seed7/prg/sudoku7.sd7        0.038352           2657
seed7/prg/sydir7.sd7         0.037752            384
seed7/prg/syntaxhl.sd7       0.040189            177
seed7/prg/tak.sd7            0.035293             59
seed7/prg/tar7.sd7           0.037727            121
seed7/prg/tch.sd7            0.037624             55
seed7/prg/testfont.sd7       0.039897             95
seed7/prg/tet.sd7            0.039198            479
seed7/prg/tetg.sd7           0.039693            501
seed7/prg/toutf8.sd7         0.041311            240
seed7/prg/tst_cli.sd7        0.036218             40
seed7/prg/tst_srv.sd7        0.036500             47
seed7/prg/wator.sd7          0.039186            651
seed7/prg/which.sd7          0.037323             65
seed7/prg/wiz.sd7            0.038897           2833
seed7/prg/wordcnt.sd7        0.036824             54
seed7/prg/wrinum.sd7         0.035930             43
seed7/prg/wumpus.sd7         0.038953            372
seed7/lib/aes.s7i            0.042155           1144
seed7/lib/aes_gcm.s7i        0.039947            392
seed7/lib/ar.s7i             0.039547           1532
seed7/lib/arc4.s7i           0.039827            144
seed7/lib/archive.s7i        0.039272            143
seed7/lib/archive_base.s7i   0.040346            135
seed7/lib/array.s7i          0.039700            610
seed7/lib/asn1.s7i           0.037443            544
seed7/lib/asn1oid.s7i        0.042166            157
seed7/lib/basearray.s7i      0.039080            450
seed7/lib/bigfile.s7i        0.037338            136
seed7/lib/bigint.s7i         0.037520            824
seed7/lib/bigrat.s7i         0.037622            784
seed7/lib/bin16.s7i          0.037747            592
seed7/lib/bin32.s7i          0.040373            490
seed7/lib/bin64.s7i          0.039885            539
seed7/lib/bitdata.s7i        0.044696           1330
seed7/lib/bitmapfont.s7i     0.039227            215
seed7/lib/bitset.s7i         0.039153            593
seed7/lib/bitsetof.s7i       0.041519            431
seed7/lib/blowfish.s7i       0.042177            383
seed7/lib/bmp.s7i            0.039648            924
seed7/lib/boolean.s7i        0.038762            403
seed7/lib/browser.s7i        0.039283            280
seed7/lib/bstring.s7i        0.039379            227
seed7/lib/bytedata.s7i       0.039433            482
seed7/lib/bzip2.s7i          0.039586            887
seed7/lib/cards.s7i          0.037483           1342
seed7/lib/category.s7i       0.039239            209
seed7/lib/cc_conf.s7i        0.038955           1314
seed7/lib/ccittfax.s7i       0.039150           1022
seed7/lib/cgi.s7i            0.038559            109
seed7/lib/cgidialog.s7i      0.038787           1118
seed7/lib/char.s7i           0.040161            356
seed7/lib/charsets.s7i       0.038687           2024
seed7/lib/chartype.s7i       0.040856            121
seed7/lib/cipher.s7i         0.037696            146
seed7/lib/cli_cmds.s7i       0.038133           1360
seed7/lib/clib_file.s7i      0.037410            301
seed7/lib/color.s7i          0.040394            185
seed7/lib/complex.s7i        0.039627            464
seed7/lib/compress.s7i       0.039355            150
seed7/lib/console.s7i        0.039162            188
seed7/lib/cpio.s7i           0.039334           1708
seed7/lib/crc32.s7i          0.040704            193
seed7/lib/cronos16.s7i       0.042579           1173
seed7/lib/cronos27.s7i       0.042990           1464
seed7/lib/csv.s7i            0.039613            201
seed7/lib/db_prop.s7i        0.039187            991
seed7/lib/deflate.s7i        0.039559            740
seed7/lib/des.s7i            0.038744            444
seed7/lib/dialog.s7i         0.039265            311
seed7/lib/dir.s7i            0.038815            163
seed7/lib/draw.s7i           0.039163            854
seed7/lib/duration.s7i       0.039164           1038
seed7/lib/echo.s7i           0.038847            132
seed7/lib/editline.s7i       0.039259            398
seed7/lib/elf.s7i            0.041007           1560
seed7/lib/elliptic.s7i       0.039249            649
seed7/lib/enable_io.s7i      0.039203            312
seed7/lib/encoding.s7i       0.041020            931
seed7/lib/enumeration.s7i    0.041850            236
seed7/lib/environment.s7i    0.038068            175
seed7/lib/exif.s7i           0.039241            152
seed7/lib/external_file.s7i  0.037325            340
seed7/lib/field.s7i          0.037711            268
seed7/lib/file.s7i           0.041568            372
seed7/lib/filebits.s7i       0.037528             46
seed7/lib/filesys.s7i        0.039815            601
seed7/lib/fileutil.s7i       0.039562            144
seed7/lib/fixarray.s7i       0.040557            307
seed7/lib/float.s7i          0.040562            757
seed7/lib/font.s7i           0.039609            196
seed7/lib/font8x8.s7i        0.038902            998
seed7/lib/forloop.s7i        0.040442            449
seed7/lib/ftp.s7i            0.039132            969
seed7/lib/ftpserv.s7i        0.039007            631
seed7/lib/getf.s7i           0.038763            115
seed7/lib/gethttp.s7i        0.036497             41
seed7/lib/gethttps.s7i       0.036482             41
seed7/lib/gif.s7i            0.039570            561
seed7/lib/graph.s7i          0.041842            415
seed7/lib/graph_file.s7i     0.039116            399
seed7/lib/gtkserver.s7i      0.038692            161
seed7/lib/gzip.s7i           0.040063            573
seed7/lib/hash.s7i           0.040851            421
seed7/lib/hashsetof.s7i      0.040580            499
seed7/lib/hmac.s7i           0.039184            152
seed7/lib/html.s7i           0.036676             83
seed7/lib/html_ent.s7i       0.037635            476
seed7/lib/htmldom.s7i        0.037802            286
seed7/lib/http_request.s7i   0.037878            696
seed7/lib/http_srv_resp.s7i  0.037992            380
seed7/lib/https_request.s7i  0.041092            211
seed7/lib/httpserv.s7i       0.039396            345
seed7/lib/huffman.s7i        0.039119            644
seed7/lib/ico.s7i            0.039445            221
seed7/lib/idxarray.s7i       0.039740            232
seed7/lib/image.s7i          0.038787            156
seed7/lib/imagefile.s7i      0.039983            171
seed7/lib/inflate.s7i        0.039229            411
seed7/lib/inifile.s7i        0.038773            129
seed7/lib/integer.s7i        0.039292            663
seed7/lib/iobuffer.s7i       0.039308            289
seed7/lib/jpeg.s7i           0.040041           1761
seed7/lib/json.s7i           0.038916            891
seed7/lib/json_serde.s7i     0.039050            783
seed7/lib/keybd.s7i          0.040850            639
seed7/lib/keydescr.s7i       0.041709            192
seed7/lib/leb128.s7i         0.039918            218
seed7/lib/line.s7i           0.039497            164
seed7/lib/listener.s7i       0.039536            247
seed7/lib/logfile.s7i        0.038199             73
seed7/lib/lower.s7i          0.039611            142
seed7/lib/lzma.s7i           0.039708            934
seed7/lib/lzw.s7i            0.038115            861
seed7/lib/magic.s7i          0.039504            403
seed7/lib/mahjng32.s7i       0.037781           1500
seed7/lib/make.s7i           0.038167            544
seed7/lib/makedata.s7i       0.038394           1428
seed7/lib/math.s7i           0.040396            201
seed7/lib/mixarith.s7i       0.039860            249
seed7/lib/modern27.s7i       0.042964           1099
seed7/lib/more.s7i           0.039610            130
seed7/lib/msgdigest.s7i      0.040218           1222
seed7/lib/multiscr.s7i       0.038228             68
seed7/lib/null_file.s7i      0.038945            345
seed7/lib/osfiles.s7i        0.040220           1085
seed7/lib/pbm.s7i            0.039682            230
seed7/lib/pcx.s7i            0.039350            638
seed7/lib/pem.s7i            0.039432            185
seed7/lib/pgm.s7i            0.039526            238
seed7/lib/pic16.s7i          0.038450           1037
seed7/lib/pic32.s7i          0.038995           2060
seed7/lib/pic_util.s7i       0.040753            144
seed7/lib/pixelimage.s7i     0.038766            320
seed7/lib/pixmap_file.s7i    0.038271            459
seed7/lib/pixmapfont.s7i     0.039922            184
seed7/lib/pkcs1.s7i          0.044629            543
seed7/lib/png.s7i            0.039259           1064
seed7/lib/poll.s7i           0.038945            313
seed7/lib/ppm.s7i            0.039751            240
seed7/lib/process.s7i        0.037902            541
seed7/lib/progs.s7i          0.039417            789
seed7/lib/propertyfile.s7i   0.040292            155
seed7/lib/rational.s7i       0.048808            792
seed7/lib/ref_list.s7i       0.042883            252
seed7/lib/reference.s7i      0.048443            126
seed7/lib/reverse.s7i        0.042045             94
seed7/lib/rpm.s7i            0.043547           3487
seed7/lib/rpmext.s7i         0.042949            318
seed7/lib/scanfile.s7i       0.128467           1779
seed7/lib/scanjson.s7i       0.047989            413
seed7/lib/scanstri.s7i       0.046809           1814
seed7/lib/scantoml.s7i       0.046891           1603
seed7/lib/seed7_05.s7i       0.050455           1072
seed7/lib/set.s7i            0.048353             57
seed7/lib/shell.s7i          0.048236            615
seed7/lib/showtls.s7i        0.043519            678
seed7/lib/signature.s7i      0.039922            131
seed7/lib/smtp.s7i           0.039271            261
seed7/lib/sockbase.s7i       0.041887            217
seed7/lib/socket.s7i         0.043275            326
seed7/lib/sokoban1.s7i       0.039723           1519
seed7/lib/sql_base.s7i       0.039243           1000
seed7/lib/stars.s7i          0.041010           1705
seed7/lib/stdfont10.s7i      0.040756           3347
seed7/lib/stdfont12.s7i      0.040573           3928
seed7/lib/stdfont14.s7i      0.049349           4510
seed7/lib/stdfont16.s7i      0.046746           5092
seed7/lib/stdfont18.s7i      0.047279           5868
seed7/lib/stdfont20.s7i      0.046435           6449
seed7/lib/stdfont24.s7i      0.044812           7421
seed7/lib/stdfont8.s7i       0.037314           2960
seed7/lib/stdfont9.s7i       0.038464           3152
seed7/lib/stdio.s7i          0.040748            192
seed7/lib/strifile.s7i       0.041749            345
seed7/lib/string.s7i         0.042716            779
seed7/lib/stritext.s7i       0.042499            352
seed7/lib/struct.s7i         0.041115            266
seed7/lib/struct_elem.s7i    0.044061            129
seed7/lib/subfile.s7i        0.042649            174
seed7/lib/subrange.s7i       0.044099             78
seed7/lib/syntax.s7i         0.045150            294
seed7/lib/tar.s7i            0.044826           1880
seed7/lib/tar_cmds.s7i       0.044787            752
seed7/lib/tdes.s7i           0.041218            143
seed7/lib/tee.s7i            0.041762            143
seed7/lib/text.s7i           0.043945            135
seed7/lib/tga.s7i            0.043733            676
seed7/lib/tiff.s7i           0.044785           2771
seed7/lib/time.s7i           0.042840           1191
seed7/lib/tls.s7i            0.043698           2230
seed7/lib/unicode.s7i        0.044549            575
seed7/lib/unionfnd.s7i       0.044547            130
seed7/lib/upper.s7i          0.042649            142
seed7/lib/utf16.s7i          0.045205            540
seed7/lib/utf8.s7i           0.045868            234
seed7/lib/vecfont10.s7i      0.050486           1056
seed7/lib/vecfont18.s7i      0.048290           1119
seed7/lib/vector3d.s7i       0.045634            293
seed7/lib/vectorfont.s7i     0.044467            239
seed7/lib/wildcard.s7i       0.041061            140
seed7/lib/window.s7i         0.042717            455
seed7/lib/wrinum.s7i         0.053020            248
seed7/lib/x509cert.s7i       0.045860           1243
seed7/lib/xml_ent.s7i        0.043044             94
seed7/lib/xmldom.s7i         0.046876            303
seed7/lib/xz.s7i             0.044391            442
seed7/lib/zip.s7i            0.044900           2792
seed7/lib/zstd.s7i           0.044748           1263
============================ ================= ================

Statistical Summary — Mode B (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.040446        |
+-----------+-----------------+
| Minimum   | 0.035293        |
+-----------+-----------------+
| Maximum   | 0.128467        |
+-----------+-----------------+

Mode C — Mode Activation + Full-Buffer Fontification (font-lock-ensure)
-----------------------------------------------------------------------

Full-buffer fontification stress test.

File Load Times — Mode C (mean, GC-free)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

============================ ================= ================
File Name                    Mean Time (s)     Line count
============================ ================= ================
seed7/prg/addup.sd7          0.040733            190
seed7/prg/bas7.sd7           0.342456          11459
seed7/prg/bifurk.sd7         0.039274             73
seed7/prg/bigfiles.sd7       0.040941            129
seed7/prg/brainf7.sd7        0.040154             86
seed7/prg/calc7.sd7          0.041161            128
seed7/prg/carddemo.sd7       0.042313            190
seed7/prg/castle.sd7         0.115087           3148
seed7/prg/cat.sd7            0.039570             82
seed7/prg/cellauto.sd7       0.039374             85
seed7/prg/celsius.sd7        0.038253             42
seed7/prg/chk_all.sd7        0.063133            843
seed7/prg/chkarr.sd7         0.362603           8367
seed7/prg/chkbig.sd7         2.054953          29026
seed7/prg/chkbin.sd7         0.512460           6469
seed7/prg/chkbitdata.sd7     0.610192           6624
seed7/prg/chkbool.sd7        0.115479           3157
seed7/prg/chkbst.sd7         0.062711            722
seed7/prg/chkchr.sd7         0.216673           2809
seed7/prg/chkcmd.sd7         0.066502           1205
seed7/prg/chkdb.sd7          0.345160           7454
seed7/prg/chkdecl.sd7        0.057290            448
seed7/prg/chkenum.sd7        0.067527           1230
seed7/prg/chkerr.sd7         0.193258           4663
seed7/prg/chkexc.sd7         0.082031           2627
seed7/prg/chkfil.sd7         0.075151           1615
seed7/prg/chkflt.sd7         1.333721          20620
seed7/prg/chkhent.sd7        0.036965             54
seed7/prg/chkhsh.sd7         0.242723           4548
seed7/prg/chkidx.sd7         1.318080          19567
seed7/prg/chkint.sd7         2.512780          38129
seed7/prg/chkjson.sd7        0.098233           1764
seed7/prg/chkovf.sd7         0.558519           8216
seed7/prg/chkprc.sd7         0.320006          10111
seed7/prg/chkscan.sd7        0.056423            714
seed7/prg/chkset.sd7         0.654269          11974
seed7/prg/chkstr.sd7         1.377395          26952
seed7/prg/chktime.sd7        0.127176           2025
seed7/prg/chktoml.sd7        0.103440           1656
seed7/prg/clock.sd7          0.035070             47
seed7/prg/clock2.sd7         0.034595             43
seed7/prg/clock3.sd7         0.036884             95
seed7/prg/cmpfil.sd7         0.037412             84
seed7/prg/comanche.sd7       0.040765            180
seed7/prg/confval.sd7        0.042904            175
seed7/prg/db7.sd7            0.047027            417
seed7/prg/diff7.sd7          0.042421            263
seed7/prg/dirtst.sd7         0.035505             42
seed7/prg/dirx.sd7           0.038323            152
seed7/prg/dnafight.sd7       0.066776           1381
seed7/prg/dragon.sd7         0.036416             73
seed7/prg/echo.sd7           0.035372             39
seed7/prg/eliza.sd7          0.042580            302
seed7/prg/err.sd7            0.039635             96
seed7/prg/fannkuch.sd7       0.037419            131
seed7/prg/fib.sd7            0.035518             47
seed7/prg/find7.sd7          0.038290            133
seed7/prg/findchar.sd7       0.039633            149
seed7/prg/fractree.sd7       0.037026             55
seed7/prg/ftp7.sd7           0.044485            296
seed7/prg/ftpserv.sd7        0.036113             74
seed7/prg/gcd.sd7            0.035970            109
seed7/prg/gkbd.sd7           0.044576            358
seed7/prg/gtksvtst.sd7       0.036712             94
seed7/prg/hal.sd7            0.038770            250
seed7/prg/hamu.sd7           0.048068            573
seed7/prg/hanoi.sd7          0.036212             55
seed7/prg/hd.sd7             0.036837             79
seed7/prg/hello.sd7          0.035510             32
seed7/prg/hilbert.sd7        0.036921            108
seed7/prg/ide7.sd7           0.040223            196
seed7/prg/kbd.sd7            0.035152             49
seed7/prg/klondike.sd7       0.054129            883
seed7/prg/lander.sd7         0.071905           1551
seed7/prg/lst80bas.sd7       0.043835            344
seed7/prg/lst99bas.sd7       0.045375            401
seed7/prg/lstgwbas.sd7       0.050557            577
seed7/prg/mahjong.sd7        0.080203           1943
seed7/prg/make7.sd7          0.038276            121
seed7/prg/mandelbr.sd7       0.040946            237
seed7/prg/mind.sd7           0.045174            443
seed7/prg/mirror.sd7         0.038904            131
seed7/prg/ms.sd7             0.048787            641
seed7/prg/nicoma.sd7         0.038038            135
seed7/prg/pac.sd7            0.052298            726
seed7/prg/pairs.sd7          0.082844           2025
seed7/prg/panic.sd7          0.095697           2634
seed7/prg/percolation.sd7    0.042837            330
seed7/prg/planets.sd7        0.078216           1486
seed7/prg/portfwd7.sd7       0.038826            139
seed7/prg/prime.sd7          0.036403             74
seed7/prg/printpi1.sd7       0.036022             56
seed7/prg/printpi2.sd7       0.035824             54
seed7/prg/printpi3.sd7       0.037170             60
seed7/prg/pv7.sd7            0.044203            337
seed7/prg/queen.sd7          0.038049            149
seed7/prg/rand.sd7           0.037441            121
seed7/prg/raytrace.sd7       0.048517            538
seed7/prg/rever.sd7          0.054148            816
seed7/prg/roman.sd7          0.035309             38
seed7/prg/s7c.sd7            0.279093           9060
seed7/prg/s7check.sd7        0.036381             68
seed7/prg/savehd7.sd7        0.064764           1110
seed7/prg/self.sd7           0.035465             49
seed7/prg/shisen.sd7         0.069427           1423
seed7/prg/sl.sd7             0.058948           1029
seed7/prg/snake.sd7          0.046840            615
seed7/prg/sokoban.sd7        0.054377            891
seed7/prg/spigotpi.sd7       0.036275             64
seed7/prg/sql7.sd7           0.041594            278
seed7/prg/startrek.sd7       0.059103            979
seed7/prg/sudoku7.sd7        0.100226           2657
seed7/prg/sydir7.sd7         0.045831            384
seed7/prg/syntaxhl.sd7       0.041330            177
seed7/prg/tak.sd7            0.035797             59
seed7/prg/tar7.sd7           0.038430            121
seed7/prg/tch.sd7            0.036199             55
seed7/prg/testfont.sd7       0.037422             95
seed7/prg/tet.sd7            0.044651            479
seed7/prg/tetg.sd7           0.045594            501
seed7/prg/toutf8.sd7         0.042614            240
seed7/prg/tst_cli.sd7        0.035255             40
seed7/prg/tst_srv.sd7        0.036117             47
seed7/prg/wator.sd7          0.051628            651
seed7/prg/which.sd7          0.035698             65
seed7/prg/wiz.sd7            0.102612           2833
seed7/prg/wordcnt.sd7        0.035489             54
seed7/prg/wrinum.sd7         0.034525             43
seed7/prg/wumpus.sd7         0.043088            372
seed7/lib/aes.s7i            0.110108           1144
seed7/lib/aes_gcm.s7i        0.046679            392
seed7/lib/ar.s7i             0.073250           1532
seed7/lib/arc4.s7i           0.038401            144
seed7/lib/archive.s7i        0.038902            143
seed7/lib/archive_base.s7i   0.038840            135
seed7/lib/array.s7i          0.054040            610
seed7/lib/asn1.s7i           0.048084            544
seed7/lib/asn1oid.s7i        0.042073            157
seed7/lib/basearray.s7i      0.048723            450
seed7/lib/bigfile.s7i        0.038202            136
seed7/lib/bigint.s7i         0.055861            824
seed7/lib/bigrat.s7i         0.054569            784
seed7/lib/bin16.s7i          0.050866            592
seed7/lib/bin32.s7i          0.048444            490
seed7/lib/bin64.s7i          0.048746            539
seed7/lib/bitdata.s7i        0.078625           1330
seed7/lib/bitmapfont.s7i     0.040798            215
seed7/lib/bitset.s7i         0.047776            593
seed7/lib/bitsetof.s7i       0.048327            431
seed7/lib/blowfish.s7i       0.056485            383
seed7/lib/bmp.s7i            0.064476            924
seed7/lib/boolean.s7i        0.045722            403
seed7/lib/browser.s7i        0.043257            280
seed7/lib/bstring.s7i        0.040993            227
seed7/lib/bytedata.s7i       0.050516            482
seed7/lib/bzip2.s7i          0.058861            887
seed7/lib/cards.s7i          0.066286           1342
seed7/lib/category.s7i       0.041582            209
seed7/lib/cc_conf.s7i        0.078281           1314
seed7/lib/ccittfax.s7i       0.072329           1022
seed7/lib/cgi.s7i            0.038961            109
seed7/lib/cgidialog.s7i      0.059605           1118
seed7/lib/char.s7i           0.043848            356
seed7/lib/charsets.s7i       0.081583           2024
seed7/lib/chartype.s7i       0.039466            121
seed7/lib/cipher.s7i         0.037571            146
seed7/lib/cli_cmds.s7i       0.066924           1360
seed7/lib/clib_file.s7i      0.042618            301
seed7/lib/color.s7i          0.042436            185
seed7/lib/complex.s7i        0.045460            464
seed7/lib/compress.s7i       0.038668            150
seed7/lib/console.s7i        0.039917            188
seed7/lib/cpio.s7i           0.081885           1708
seed7/lib/crc32.s7i          0.043899            193
seed7/lib/cronos16.s7i       0.092547           1173
seed7/lib/cronos27.s7i       0.116261           1464
seed7/lib/csv.s7i            0.040920            201
seed7/lib/db_prop.s7i        0.063399            991
seed7/lib/deflate.s7i        0.055493            740
seed7/lib/des.s7i            0.055663            444
seed7/lib/dialog.s7i         0.044147            311
seed7/lib/dir.s7i            0.045120            163
seed7/lib/draw.s7i           0.077327            854
seed7/lib/duration.s7i       0.098943           1038
seed7/lib/echo.s7i           0.041039            132
seed7/lib/editline.s7i       0.046860            398
seed7/lib/elf.s7i            0.102004           1560
seed7/lib/elliptic.s7i       0.060508            649
seed7/lib/enable_io.s7i      0.046247            312
seed7/lib/encoding.s7i       0.062719            931
seed7/lib/enumeration.s7i    0.047828            236
seed7/lib/environment.s7i    0.042286            175
seed7/lib/exif.s7i           0.040984            152
seed7/lib/external_file.s7i  0.044134            340
seed7/lib/field.s7i          0.042473            268
seed7/lib/file.s7i           0.045274            372
seed7/lib/filebits.s7i       0.036754             46
seed7/lib/filesys.s7i        0.049254            601
seed7/lib/fileutil.s7i       0.038581            144
seed7/lib/fixarray.s7i       0.043971            307
seed7/lib/float.s7i          0.062922            757
seed7/lib/font.s7i           0.048380            196
seed7/lib/font8x8.s7i        0.050096            998
seed7/lib/forloop.s7i        0.045194            449
seed7/lib/ftp.s7i            0.057355            969
seed7/lib/ftpserv.s7i        0.050820            631
seed7/lib/getf.s7i           0.036919            115
seed7/lib/gethttp.s7i        0.035411             41
seed7/lib/gethttps.s7i       0.038198             41
seed7/lib/gif.s7i            0.050640            561
seed7/lib/graph.s7i          0.050054            415
seed7/lib/graph_file.s7i     0.045388            399
seed7/lib/gtkserver.s7i      0.039042            161
seed7/lib/gzip.s7i           0.049035            573
seed7/lib/hash.s7i           0.050221            421
seed7/lib/hashsetof.s7i      0.049608            499
seed7/lib/hmac.s7i           0.039829            152
seed7/lib/html.s7i           0.037958             83
seed7/lib/html_ent.s7i       0.047512            476
seed7/lib/htmldom.s7i        0.043714            286
seed7/lib/http_request.s7i   0.052505            696
seed7/lib/http_srv_resp.s7i  0.045690            380
seed7/lib/https_request.s7i  0.040719            211
seed7/lib/httpserv.s7i       0.044377            345
seed7/lib/huffman.s7i        0.053295            644
seed7/lib/ico.s7i            0.042225            221
seed7/lib/idxarray.s7i       0.042162            232
seed7/lib/image.s7i          0.038031            156
seed7/lib/imagefile.s7i      0.039224            171
seed7/lib/inflate.s7i        0.046228            411
seed7/lib/inifile.s7i        0.037802            129
seed7/lib/integer.s7i        0.061860            663
seed7/lib/iobuffer.s7i       0.045770            289
seed7/lib/jpeg.s7i           0.091656           1761
seed7/lib/json.s7i           0.055919            891
seed7/lib/json_serde.s7i     0.052798            783
seed7/lib/keybd.s7i          0.058827            639
seed7/lib/keydescr.s7i       0.049236            192
seed7/lib/leb128.s7i         0.045519            218
seed7/lib/line.s7i           0.039033            164
seed7/lib/listener.s7i       0.044667            247
seed7/lib/logfile.s7i        0.043212             73
seed7/lib/lower.s7i          0.038560            142
seed7/lib/lzma.s7i           0.065032            934
seed7/lib/lzw.s7i            0.070670            861
seed7/lib/magic.s7i          0.059625            403
seed7/lib/mahjng32.s7i       0.074397           1500
seed7/lib/make.s7i           0.059014            544
seed7/lib/makedata.s7i       0.079400           1428
seed7/lib/math.s7i           0.049229            201
seed7/lib/mixarith.s7i       0.051409            249
seed7/lib/modern27.s7i       0.098782           1099
seed7/lib/more.s7i           0.050409            130
seed7/lib/msgdigest.s7i      0.089673           1222
seed7/lib/multiscr.s7i       0.045310             68
seed7/lib/null_file.s7i      0.052186            345
seed7/lib/osfiles.s7i        0.074436           1085
seed7/lib/pbm.s7i            0.053095            230
seed7/lib/pcx.s7i            0.064525            638
seed7/lib/pem.s7i            0.048307            185
seed7/lib/pgm.s7i            0.047060            238
seed7/lib/pic16.s7i          0.053527           1037
seed7/lib/pic32.s7i          0.081953           2060
seed7/lib/pic_util.s7i       0.042058            144
seed7/lib/pixelimage.s7i     0.048678            320
seed7/lib/pixmap_file.s7i    0.048470            459
seed7/lib/pixmapfont.s7i     0.040544            184
seed7/lib/pkcs1.s7i          0.064733            543
seed7/lib/png.s7i            0.074779           1064
seed7/lib/poll.s7i           0.051117            313
seed7/lib/ppm.s7i            0.046625            240
seed7/lib/process.s7i        0.054711            541
seed7/lib/progs.s7i          0.061774            789
seed7/lib/propertyfile.s7i   0.047711            155
seed7/lib/rational.s7i       0.061758            792
seed7/lib/ref_list.s7i       0.047866            252
seed7/lib/reference.s7i      0.047042            126
seed7/lib/reverse.s7i        0.038330             94
seed7/lib/rpm.s7i            0.144709           3487
seed7/lib/rpmext.s7i         0.043907            318
seed7/lib/scanfile.s7i       0.080923           1779
seed7/lib/scanjson.s7i       0.047077            413
seed7/lib/scanstri.s7i       0.079713           1814
seed7/lib/scantoml.s7i       0.069846           1603
seed7/lib/seed7_05.s7i       0.067231           1072
seed7/lib/set.s7i            0.038372             57
seed7/lib/shell.s7i          0.053000            615
seed7/lib/showtls.s7i        0.061945            678
seed7/lib/signature.s7i      0.045628            131
seed7/lib/smtp.s7i           0.045463            261
seed7/lib/sockbase.s7i       0.046149            217
seed7/lib/socket.s7i         0.048029            326
seed7/lib/sokoban1.s7i       0.055122           1519
seed7/lib/sql_base.s7i       0.064451           1000
seed7/lib/stars.s7i          0.137901           1705
seed7/lib/stdfont10.s7i      0.081943           3347
seed7/lib/stdfont12.s7i      0.096705           3928
seed7/lib/stdfont14.s7i      0.113875           4510
seed7/lib/stdfont16.s7i      0.122483           5092
seed7/lib/stdfont18.s7i      0.143716           5868
seed7/lib/stdfont20.s7i      0.154168           6449
seed7/lib/stdfont24.s7i      0.181806           7421
seed7/lib/stdfont8.s7i       0.078601           2960
seed7/lib/stdfont9.s7i       0.083663           3152
seed7/lib/stdio.s7i          0.040238            192
seed7/lib/strifile.s7i       0.045654            345
seed7/lib/string.s7i         0.060928            779
seed7/lib/stritext.s7i       0.048119            352
seed7/lib/struct.s7i         0.050093            266
seed7/lib/struct_elem.s7i    0.040876            129
seed7/lib/subfile.s7i        0.040427            174
seed7/lib/subrange.s7i       0.036906             78
seed7/lib/syntax.s7i         0.050879            294
seed7/lib/tar.s7i            0.089942           1880
seed7/lib/tar_cmds.s7i       0.061801            752
seed7/lib/tdes.s7i           0.041532            143
seed7/lib/tee.s7i            0.042994            143
seed7/lib/text.s7i           0.049017            135
seed7/lib/tga.s7i            0.060819            676
seed7/lib/tiff.s7i           0.128870           2771
seed7/lib/time.s7i           0.068033           1191
seed7/lib/tls.s7i            0.110066           2230
seed7/lib/unicode.s7i        0.061407            575
seed7/lib/unionfnd.s7i       0.047425            130
seed7/lib/upper.s7i          0.047458            142
seed7/lib/utf16.s7i          0.053126            540
seed7/lib/utf8.s7i           0.049323            234
seed7/lib/vecfont10.s7i      0.085605           1056
seed7/lib/vecfont18.s7i      0.095566           1119
seed7/lib/vector3d.s7i       0.043409            293
seed7/lib/vectorfont.s7i     0.042813            239
seed7/lib/wildcard.s7i       0.043412            140
seed7/lib/window.s7i         0.050009            455
seed7/lib/wrinum.s7i         0.045207            248
seed7/lib/x509cert.s7i       0.075632           1243
seed7/lib/xml_ent.s7i        0.041331             94
seed7/lib/xmldom.s7i         0.046085            303
seed7/lib/xz.s7i             0.051294            442
seed7/lib/zip.s7i            0.127876           2792
seed7/lib/zstd.s7i           0.075358           1263
============================ ================= ================

Statistical Summary — Mode C (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.091281        |
+-----------+-----------------+
| Minimum   | 0.034525        |
+-----------+-----------------+
| Maximum   | 2.512780        |
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
seed7/prg/addup.sd7          0.050958            190
seed7/prg/bas7.sd7           0.777310          11459
seed7/prg/bifurk.sd7         0.039297             73
seed7/prg/bigfiles.sd7       0.042075            129
seed7/prg/brainf7.sd7        0.039020             86
seed7/prg/calc7.sd7          0.041068            128
seed7/prg/carddemo.sd7       0.045173            190
seed7/prg/castle.sd7         0.218710           3148
seed7/prg/cat.sd7            0.038868             82
seed7/prg/cellauto.sd7       0.038555             85
seed7/prg/celsius.sd7        0.036789             42
seed7/prg/chk_all.sd7        0.082407            843
seed7/prg/chkarr.sd7         0.861889           8367
seed7/prg/chkbig.sd7         4.121425          29026
seed7/prg/chkbin.sd7         1.019831           6469
seed7/prg/chkbitdata.sd7     1.236058           6624
seed7/prg/chkbool.sd7        0.229604           3157
seed7/prg/chkbst.sd7         0.100358            722
seed7/prg/chkchr.sd7         0.477052           2809
seed7/prg/chkcmd.sd7         0.118043           1205
seed7/prg/chkdb.sd7          0.750244           7454
seed7/prg/chkdecl.sd7        0.090486            448
seed7/prg/chkenum.sd7        0.118834           1230
seed7/prg/chkerr.sd7         0.338472           4663
seed7/prg/chkexc.sd7         0.147715           2627
seed7/prg/chkfil.sd7         0.128784           1615
seed7/prg/chkflt.sd7         2.808089          20620
seed7/prg/chkhent.sd7        0.038167             54
seed7/prg/chkhsh.sd7         0.499808           4548
seed7/prg/chkidx.sd7         3.163047          19567
seed7/prg/chkint.sd7         5.515460          38129
seed7/prg/chkjson.sd7        0.186803           1764
seed7/prg/chkovf.sd7         1.203513           8216
seed7/prg/chkprc.sd7         0.690846          10111
seed7/prg/chkscan.sd7        0.090053            714
seed7/prg/chkset.sd7         1.741249          11974
seed7/prg/chkstr.sd7         3.406322          26952
seed7/prg/chktime.sd7        0.251378           2025
seed7/prg/chktoml.sd7        0.201659           1656
seed7/prg/clock.sd7          0.038542             47
seed7/prg/clock2.sd7         0.037855             43
seed7/prg/clock3.sd7         0.042242             95
seed7/prg/cmpfil.sd7         0.038961             84
seed7/prg/comanche.sd7       0.046326            180
seed7/prg/confval.sd7        0.049733            175
seed7/prg/db7.sd7            0.061713            417
seed7/prg/diff7.sd7          0.050088            263
seed7/prg/dirtst.sd7         0.035629             42
seed7/prg/dirx.sd7           0.043690            152
seed7/prg/dnafight.sd7       0.118488           1381
seed7/prg/dragon.sd7         0.038517             73
seed7/prg/echo.sd7           0.036264             39
seed7/prg/eliza.sd7          0.052169            302
seed7/prg/err.sd7            0.044889             96
seed7/prg/fannkuch.sd7       0.041647            131
seed7/prg/fib.sd7            0.036661             47
seed7/prg/find7.sd7          0.041527            133
seed7/prg/findchar.sd7       0.043742            149
seed7/prg/fractree.sd7       0.037763             55
seed7/prg/ftp7.sd7           0.053494            296
seed7/prg/ftpserv.sd7        0.039115             74
seed7/prg/gcd.sd7            0.040178            109
seed7/prg/gkbd.sd7           0.060931            358
seed7/prg/gtksvtst.sd7       0.038953             94
seed7/prg/hal.sd7            0.046988            250
seed7/prg/hamu.sd7           0.064968            573
seed7/prg/hanoi.sd7          0.035758             55
seed7/prg/hd.sd7             0.037172             79
seed7/prg/hello.sd7          0.036285             32
seed7/prg/hilbert.sd7        0.039843            108
seed7/prg/ide7.sd7           0.048175            196
seed7/prg/kbd.sd7            0.036859             49
seed7/prg/klondike.sd7       0.087080            883
seed7/prg/lander.sd7         0.131689           1551
seed7/prg/lst80bas.sd7       0.055504            344
seed7/prg/lst99bas.sd7       0.059537            401
seed7/prg/lstgwbas.sd7       0.072579            577
seed7/prg/mahjong.sd7        0.146713           1943
seed7/prg/make7.sd7          0.042308            121
seed7/prg/mandelbr.sd7       0.048855            237
seed7/prg/mind.sd7           0.060691            443
seed7/prg/mirror.sd7         0.046958            131
seed7/prg/ms.sd7             0.069313            641
seed7/prg/nicoma.sd7         0.041091            135
seed7/prg/pac.sd7            0.069367            726
seed7/prg/pairs.sd7          0.138330           2025
seed7/prg/panic.sd7          0.192919           2634
seed7/prg/percolation.sd7    0.055779            330
seed7/prg/planets.sd7        0.136163           1486
seed7/prg/portfwd7.sd7       0.043203            139
seed7/prg/prime.sd7          0.037783             74
seed7/prg/printpi1.sd7       0.037468             56
seed7/prg/printpi2.sd7       0.037489             54
seed7/prg/printpi3.sd7       0.038042             60
seed7/prg/pv7.sd7            0.060954            337
seed7/prg/queen.sd7          0.043623            149
seed7/prg/rand.sd7           0.041108            121
seed7/prg/raytrace.sd7       0.067802            538
seed7/prg/rever.sd7          0.080455            816
seed7/prg/roman.sd7          0.035536             38
seed7/prg/s7c.sd7            0.620969           9060
seed7/prg/s7check.sd7        0.038969             68
seed7/prg/savehd7.sd7        0.110974           1110
seed7/prg/self.sd7           0.037399             49
seed7/prg/shisen.sd7         0.119146           1423
seed7/prg/sl.sd7             0.094692           1029
seed7/prg/snake.sd7          0.066567            615
seed7/prg/sokoban.sd7        0.080292            891
seed7/prg/spigotpi.sd7       0.037298             64
seed7/prg/sql7.sd7           0.052823            278
seed7/prg/startrek.sd7       0.090898            979
seed7/prg/sudoku7.sd7        0.192168           2657
seed7/prg/sydir7.sd7         0.060734            384
seed7/prg/syntaxhl.sd7       0.048750            177
seed7/prg/tak.sd7            0.037087             59
seed7/prg/tar7.sd7           0.042715            121
seed7/prg/tch.sd7            0.037400             55
seed7/prg/testfont.sd7       0.041334             95
seed7/prg/tet.sd7            0.059973            479
seed7/prg/tetg.sd7           0.061218            501
seed7/prg/toutf8.sd7         0.051749            240
seed7/prg/tst_cli.sd7        0.035302             40
seed7/prg/tst_srv.sd7        0.036436             47
seed7/prg/wator.sd7          0.078413            651
seed7/prg/which.sd7          0.038517             65
seed7/prg/wiz.sd7            0.208439           2833
seed7/prg/wordcnt.sd7        0.037855             54
seed7/prg/wrinum.sd7         0.036706             43
seed7/prg/wumpus.sd7         0.054679            372
seed7/lib/aes.s7i            0.194699           1144
seed7/lib/aes_gcm.s7i        0.060171            392
seed7/lib/ar.s7i             0.123669           1532
seed7/lib/arc4.s7i           0.042965            144
seed7/lib/archive.s7i        0.042994            143
seed7/lib/archive_base.s7i   0.042800            135
seed7/lib/array.s7i          0.073893            610
seed7/lib/asn1.s7i           0.062209            544
seed7/lib/asn1oid.s7i        0.047998            157
seed7/lib/basearray.s7i      0.061799            450
seed7/lib/bigfile.s7i        0.042150            136
seed7/lib/bigint.s7i         0.078223            824
seed7/lib/bigrat.s7i         0.078870            784
seed7/lib/bin16.s7i          0.067154            592
seed7/lib/bin32.s7i          0.062348            490
seed7/lib/bin64.s7i          0.063668            539
seed7/lib/bitdata.s7i        0.122405           1330
seed7/lib/bitmapfont.s7i     0.047009            215
seed7/lib/bitset.s7i         0.063592            593
seed7/lib/bitsetof.s7i       0.060740            431
seed7/lib/blowfish.s7i       0.076850            383
seed7/lib/bmp.s7i            0.098766            924
seed7/lib/boolean.s7i        0.053003            403
seed7/lib/browser.s7i        0.051604            280
seed7/lib/bstring.s7i        0.045867            227
seed7/lib/bytedata.s7i       0.065228            482
seed7/lib/bzip2.s7i          0.088487            887
seed7/lib/cards.s7i          0.106967           1342
seed7/lib/category.s7i       0.050377            209
seed7/lib/cc_conf.s7i        0.120030           1314
seed7/lib/ccittfax.s7i       0.100646           1022
seed7/lib/cgi.s7i            0.041147            109
seed7/lib/cgidialog.s7i      0.096886           1118
seed7/lib/char.s7i           0.051575            356
seed7/lib/charsets.s7i       0.124153           2024
seed7/lib/chartype.s7i       0.047511            121
seed7/lib/cipher.s7i         0.041434            146
seed7/lib/cli_cmds.s7i       0.110865           1360
seed7/lib/clib_file.s7i      0.049881            301
seed7/lib/color.s7i          0.046298            185
seed7/lib/complex.s7i        0.059149            464
seed7/lib/compress.s7i       0.045162            150
seed7/lib/console.s7i        0.048030            188
seed7/lib/cpio.s7i           0.143251           1708
seed7/lib/crc32.s7i          0.053575            193
seed7/lib/cronos16.s7i       0.192437           1173
seed7/lib/cronos27.s7i       0.254108           1464
seed7/lib/csv.s7i            0.046928            201
seed7/lib/db_prop.s7i        0.099390            991
seed7/lib/deflate.s7i        0.084451            740
seed7/lib/des.s7i            0.079418            444
seed7/lib/dialog.s7i         0.056096            311
seed7/lib/dir.s7i            0.042929            163
seed7/lib/draw.s7i           0.084811            854
seed7/lib/duration.s7i       0.096638           1038
seed7/lib/echo.s7i           0.041580            132
seed7/lib/editline.s7i       0.059432            398
seed7/lib/elf.s7i            0.154200           1560
seed7/lib/elliptic.s7i       0.074189            649
seed7/lib/enable_io.s7i      0.052177            312
seed7/lib/encoding.s7i       0.099652            931
seed7/lib/enumeration.s7i    0.047806            236
seed7/lib/environment.s7i    0.042209            175
seed7/lib/exif.s7i           0.044163            152
seed7/lib/external_file.s7i  0.050756            340
seed7/lib/field.s7i          0.059925            268
seed7/lib/file.s7i           0.057809            372
seed7/lib/filebits.s7i       0.037910             46
seed7/lib/filesys.s7i        0.063938            601
seed7/lib/fileutil.s7i       0.043202            144
seed7/lib/fixarray.s7i       0.053111            307
seed7/lib/float.s7i          0.073531            757
seed7/lib/font.s7i           0.044768            196
seed7/lib/font8x8.s7i        0.067764            998
seed7/lib/forloop.s7i        0.058634            449
seed7/lib/ftp.s7i            0.087266            969
seed7/lib/ftpserv.s7i        0.075459            631
seed7/lib/getf.s7i           0.041298            115
seed7/lib/gethttp.s7i        0.036715             41
seed7/lib/gethttps.s7i       0.036998             41
seed7/lib/gif.s7i            0.069634            561
seed7/lib/graph.s7i          0.061700            415
seed7/lib/graph_file.s7i     0.055946            399
seed7/lib/gtkserver.s7i      0.040266            161
seed7/lib/gzip.s7i           0.067333            573
seed7/lib/hash.s7i           0.065730            421
seed7/lib/hashsetof.s7i      0.065124            499
seed7/lib/hmac.s7i           0.044000            152
seed7/lib/html.s7i           0.039127             83
seed7/lib/html_ent.s7i       0.063267            476
seed7/lib/htmldom.s7i        0.053800            286
seed7/lib/http_request.s7i   0.077180            696
seed7/lib/http_srv_resp.s7i  0.059692            380
seed7/lib/https_request.s7i  0.048001            211
seed7/lib/httpserv.s7i       0.055100            345
seed7/lib/huffman.s7i        0.074398            644
seed7/lib/ico.s7i            0.049181            221
seed7/lib/idxarray.s7i       0.050475            232
seed7/lib/image.s7i          0.040460            156
seed7/lib/imagefile.s7i      0.042994            171
seed7/lib/inflate.s7i        0.061609            411
seed7/lib/inifile.s7i        0.041402            129
seed7/lib/integer.s7i        0.065843            663
seed7/lib/iobuffer.s7i       0.050824            289
seed7/lib/jpeg.s7i           0.154876           1761
seed7/lib/json.s7i           0.079476            891
seed7/lib/json_serde.s7i     0.077968            783
seed7/lib/keybd.s7i          0.078733            639
seed7/lib/keydescr.s7i       0.049706            192
seed7/lib/leb128.s7i         0.046542            218
seed7/lib/line.s7i           0.042706            164
seed7/lib/listener.s7i       0.048307            247
seed7/lib/logfile.s7i        0.038514             73
seed7/lib/lower.s7i          0.041534            142
seed7/lib/lzma.s7i           0.096304            934
seed7/lib/lzw.s7i            0.087747            861
seed7/lib/magic.s7i          0.061794            403
seed7/lib/mahjng32.s7i       0.089686           1500
seed7/lib/make.s7i           0.066774            544
seed7/lib/makedata.s7i       0.121597           1428
seed7/lib/math.s7i           0.044930            201
seed7/lib/mixarith.s7i       0.046790            249
seed7/lib/modern27.s7i       0.169693           1099
seed7/lib/more.s7i           0.041918            130
seed7/lib/msgdigest.s7i      0.136731           1222
seed7/lib/multiscr.s7i       0.038025             68
seed7/lib/null_file.s7i      0.050584            345
seed7/lib/osfiles.s7i        0.095558           1085
seed7/lib/pbm.s7i            0.047261            230
seed7/lib/pcx.s7i            0.075081            638
seed7/lib/pem.s7i            0.043465            185
seed7/lib/pgm.s7i            0.047300            238
seed7/lib/pic16.s7i          0.068466           1037
seed7/lib/pic32.s7i          0.122099           2060
seed7/lib/pic_util.s7i       0.043659            144
seed7/lib/pixelimage.s7i     0.052433            320
seed7/lib/pixmap_file.s7i    0.061875            459
seed7/lib/pixmapfont.s7i     0.047410            184
seed7/lib/pkcs1.s7i          0.077435            543
seed7/lib/png.s7i            0.108230           1064
seed7/lib/poll.s7i           0.052330            313
seed7/lib/ppm.s7i            0.048504            240
seed7/lib/process.s7i        0.063840            541
seed7/lib/progs.s7i          0.078400            789
seed7/lib/propertyfile.s7i   0.042863            155
seed7/lib/rational.s7i       0.076256            792
seed7/lib/ref_list.s7i       0.048453            252
seed7/lib/reference.s7i      0.040678            126
seed7/lib/reverse.s7i        0.040936             94
seed7/lib/rpm.s7i            0.283845           3487
seed7/lib/rpmext.s7i         0.051857            318
seed7/lib/scanfile.s7i       0.132344           1779
seed7/lib/scanjson.s7i       0.061171            413
seed7/lib/scanstri.s7i       0.135712           1814
seed7/lib/scantoml.s7i       0.129303           1603
seed7/lib/seed7_05.s7i       0.108159           1072
seed7/lib/set.s7i            0.036746             57
seed7/lib/shell.s7i          0.070894            615
seed7/lib/showtls.s7i        0.085497            678
seed7/lib/signature.s7i      0.042936            131
seed7/lib/smtp.s7i           0.048936            261
seed7/lib/sockbase.s7i       0.050985            217
seed7/lib/socket.s7i         0.061819            326
seed7/lib/sokoban1.s7i       0.082542           1519
seed7/lib/sql_base.s7i       0.094205           1000
seed7/lib/stars.s7i          0.236849           1705
seed7/lib/stdfont10.s7i      0.151681           3347
seed7/lib/stdfont12.s7i      0.171461           3928
seed7/lib/stdfont14.s7i      0.195211           4510
seed7/lib/stdfont16.s7i      0.211956           5092
seed7/lib/stdfont18.s7i      0.244857           5868
seed7/lib/stdfont20.s7i      0.271224           6449
seed7/lib/stdfont24.s7i      0.325130           7421
seed7/lib/stdfont8.s7i       0.127287           2960
seed7/lib/stdfont9.s7i       0.133260           3152
seed7/lib/stdio.s7i          0.044246            192
seed7/lib/strifile.s7i       0.053723            345
seed7/lib/string.s7i         0.075297            779
seed7/lib/stritext.s7i       0.054387            352
seed7/lib/struct.s7i         0.054558            266
seed7/lib/struct_elem.s7i    0.041812            129
seed7/lib/subfile.s7i        0.043360            174
seed7/lib/subrange.s7i       0.038972             78
seed7/lib/syntax.s7i         0.058812            294
seed7/lib/tar.s7i            0.142623           1880
seed7/lib/tar_cmds.s7i       0.084048            752
seed7/lib/tdes.s7i           0.044081            143
seed7/lib/tee.s7i            0.041774            143
seed7/lib/text.s7i           0.042247            135
seed7/lib/tga.s7i            0.079532            676
seed7/lib/tiff.s7i           0.242150           2771
seed7/lib/time.s7i           0.101856           1191
seed7/lib/tls.s7i            0.196020           2230
seed7/lib/unicode.s7i        0.073399            575
seed7/lib/unionfnd.s7i       0.040978            130
seed7/lib/upper.s7i          0.039971            142
seed7/lib/utf16.s7i          0.064482            540
seed7/lib/utf8.s7i           0.047922            234
seed7/lib/vecfont10.s7i      0.159158           1056
seed7/lib/vecfont18.s7i      0.179356           1119
seed7/lib/vector3d.s7i       0.050624            293
seed7/lib/vectorfont.s7i     0.047997            239
seed7/lib/wildcard.s7i       0.042776            140
seed7/lib/window.s7i         0.060250            455
seed7/lib/wrinum.s7i         0.048957            248
seed7/lib/x509cert.s7i       0.117610           1243
seed7/lib/xml_ent.s7i        0.039139             94
seed7/lib/xmldom.s7i         0.049148            303
seed7/lib/xz.s7i             0.057980            442
seed7/lib/zip.s7i            0.235659           2792
seed7/lib/zstd.s7i           0.120770           1263
============================ ================= ================

Statistical Summary — Mode D (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.157895        |
+-----------+-----------------+
| Minimum   | 0.035302        |
+-----------+-----------------+
| Maximum   | 5.515460        |
+-----------+-----------------+

Cross-Mode Comparison
=====================

+------+-----------------+-----------------+-----------------+
| Mode | Average (s)     | Minimum (s)     | Maximum (s)     |
+======+=================+=================+=================+
| A    | 0.034187        | 0.032418        | 0.059686        |
+------+-----------------+-----------------+-----------------+
| B    | 0.040446        | 0.035293        | 0.128467        |
+------+-----------------+-----------------+-----------------+
| C    | 0.091281        | 0.034525        | 2.512780        |
+------+-----------------+-----------------+-----------------+
| D    | 0.157895        | 0.035302        | 5.515460        |
+------+-----------------+-----------------+-----------------+

Phase Timing Summary
====================

Wall-clock times include warm-up work and GC performed by the warm-up helper.

+----------+--------------+--------------+--------------+
| Phase    | Warm-up      | Timed pass   | Mode total   |
+==========+==============+==============+==============+
| Mode A   | 00:00:12.283 | 00:00:58.234 | 00:01:10.518 |
+----------+--------------+--------------+--------------+
| Mode B   | 00:00:14.971 | 00:01:08.965 | 00:01:23.937 |
+----------+--------------+--------------+--------------+
| Mode C   | 00:00:37.428 | 00:02:36.427 | 00:03:13.856 |
+----------+--------------+--------------+--------------+
| Mode D   | 00:01:03.117 | 00:04:29.848 | 00:05:32.966 |
+----------+--------------+--------------+--------------+
| Total    |              |              | 00:11:21.284 |
+----------+--------------+--------------+--------------+


.. ---------------------------------------------------------------------------
