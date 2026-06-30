=======================================================
GC-Controlled Benchmark Report: Seed7 Mode — Four Modes
=======================================================

:Running with: seed7-mode 2026-06-26T02:09:26+0000 W26-5
:Seed7 dir   : /Users/roup/my/dvo/seed7-repos/seed7/
:Started at: 07:51:41 local time
:Generated on: 2026-06-26 12:02:51 UTC
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
seed7/prg/addup.sd7          0.034750            190
seed7/prg/bas7.sd7           0.033432          11459
seed7/prg/bifurk.sd7         0.032463             73
seed7/prg/bigfiles.sd7       0.032571            129
seed7/prg/brainf7.sd7        0.032651             86
seed7/prg/calc7.sd7          0.033284            128
seed7/prg/carddemo.sd7       0.033112            190
seed7/prg/castle.sd7         0.034655           3148
seed7/prg/cat.sd7            0.033679             82
seed7/prg/cellauto.sd7       0.033358             85
seed7/prg/celsius.sd7        0.033470             42
seed7/prg/chk_all.sd7        0.033758            843
seed7/prg/chkarr.sd7         0.034293           8367
seed7/prg/chkbig.sd7         0.037962          29026
seed7/prg/chkbin.sd7         0.034597           6469
seed7/prg/chkbitdata.sd7     0.034452           6624
seed7/prg/chkbool.sd7        0.033817           3157
seed7/prg/chkbst.sd7         0.033723            722
seed7/prg/chkchr.sd7         0.034932           2809
seed7/prg/chkcmd.sd7         0.033693           1205
seed7/prg/chkdb.sd7          0.035037           7454
seed7/prg/chkdecl.sd7        0.033827            448
seed7/prg/chkenum.sd7        0.033505           1230
seed7/prg/chkerr.sd7         0.034238           4663
seed7/prg/chkexc.sd7         0.033889           2627
seed7/prg/chkfil.sd7         0.033567           1615
seed7/prg/chkflt.sd7         0.036546          20620
seed7/prg/chkhent.sd7        0.033907             54
seed7/prg/chkhsh.sd7         0.033943           4548
seed7/prg/chkidx.sd7         0.036050          19567
seed7/prg/chkint.sd7         0.039571          38129
seed7/prg/chkjson.sd7        0.032892           1764
seed7/prg/chkovf.sd7         0.033316           8216
seed7/prg/chkprc.sd7         0.035171          10111
seed7/prg/chkscan.sd7        0.036247            714
seed7/prg/chkset.sd7         0.035238          11974
seed7/prg/chkstr.sd7         0.039972          26952
seed7/prg/chktime.sd7        0.034250           2025
seed7/prg/chktoml.sd7        0.037616           1656
seed7/prg/clock.sd7          0.036085             47
seed7/prg/clock2.sd7         0.033564             43
seed7/prg/clock3.sd7         0.033566             95
seed7/prg/cmpfil.sd7         0.033630             84
seed7/prg/comanche.sd7       0.033556            180
seed7/prg/confval.sd7        0.033691            175
seed7/prg/db7.sd7            0.033771            417
seed7/prg/diff7.sd7          0.033513            263
seed7/prg/dirtst.sd7         0.034032             42
seed7/prg/dirx.sd7           0.033578            152
seed7/prg/dnafight.sd7       0.033829           1381
seed7/prg/dragon.sd7         0.033439             73
seed7/prg/echo.sd7           0.033536             39
seed7/prg/eliza.sd7          0.033595            302
seed7/prg/err.sd7            0.033764             96
seed7/prg/fannkuch.sd7       0.033471            131
seed7/prg/fib.sd7            0.033609             47
seed7/prg/find7.sd7          0.033549            133
seed7/prg/findchar.sd7       0.033407            149
seed7/prg/fractree.sd7       0.033777             55
seed7/prg/ftp7.sd7           0.033392            296
seed7/prg/ftpserv.sd7        0.040001             74
seed7/prg/gcd.sd7            0.037155            109
seed7/prg/gkbd.sd7           0.034517            358
seed7/prg/gtksvtst.sd7       0.033741             94
seed7/prg/hal.sd7            0.032922            250
seed7/prg/hamu.sd7           0.032791            573
seed7/prg/hanoi.sd7          0.032755             55
seed7/prg/hd.sd7             0.034857             79
seed7/prg/hello.sd7          0.033560             32
seed7/prg/hilbert.sd7        0.033660            108
seed7/prg/ide7.sd7           0.034188            196
seed7/prg/kbd.sd7            0.034031             49
seed7/prg/klondike.sd7       0.033905            883
seed7/prg/lander.sd7         0.033764           1551
seed7/prg/lst80bas.sd7       0.033537            344
seed7/prg/lst99bas.sd7       0.033656            401
seed7/prg/lstgwbas.sd7       0.033462            577
seed7/prg/mahjong.sd7        0.033491           1943
seed7/prg/make7.sd7          0.033685            121
seed7/prg/mandelbr.sd7       0.033543            237
seed7/prg/mind.sd7           0.033672            443
seed7/prg/mirror.sd7         0.033366            131
seed7/prg/ms.sd7             0.033701            641
seed7/prg/nicoma.sd7         0.033187            135
seed7/prg/pac.sd7            0.033755            726
seed7/prg/pairs.sd7          0.033886           2025
seed7/prg/panic.sd7          0.033648           2634
seed7/prg/percolation.sd7    0.033626            330
seed7/prg/planets.sd7        0.033663           1486
seed7/prg/portfwd7.sd7       0.033676            139
seed7/prg/prime.sd7          0.033558             74
seed7/prg/printpi1.sd7       0.034065             56
seed7/prg/printpi2.sd7       0.033099             54
seed7/prg/printpi3.sd7       0.032863             60
seed7/prg/pv7.sd7            0.032983            337
seed7/prg/queen.sd7          0.033305            149
seed7/prg/rand.sd7           0.032610            121
seed7/prg/raytrace.sd7       0.032413            538
seed7/prg/rever.sd7          0.033405            816
seed7/prg/roman.sd7          0.033650             38
seed7/prg/s7c.sd7            0.033057           9060
seed7/prg/s7check.sd7        0.032497             68
seed7/prg/savehd7.sd7        0.033661           1110
seed7/prg/self.sd7           0.033600             49
seed7/prg/shisen.sd7         0.033641           1423
seed7/prg/sl.sd7             0.033702           1029
seed7/prg/snake.sd7          0.033684            615
seed7/prg/sokoban.sd7        0.033639            891
seed7/prg/spigotpi.sd7       0.033444             64
seed7/prg/sql7.sd7           0.033557            278
seed7/prg/startrek.sd7       0.033666            979
seed7/prg/sudoku7.sd7        0.033849           2657
seed7/prg/sydir7.sd7         0.033415            384
seed7/prg/syntaxhl.sd7       0.033830            177
seed7/prg/tak.sd7            0.033378             59
seed7/prg/tar7.sd7           0.033724            121
seed7/prg/tch.sd7            0.033548             55
seed7/prg/testfont.sd7       0.033737             95
seed7/prg/tet.sd7            0.033647            479
seed7/prg/tetg.sd7           0.033291            501
seed7/prg/toutf8.sd7         0.033483            240
seed7/prg/tst_cli.sd7        0.033602             40
seed7/prg/tst_srv.sd7        0.033669             47
seed7/prg/wator.sd7          0.033896            651
seed7/prg/which.sd7          0.033727             65
seed7/prg/wiz.sd7            0.033422           2833
seed7/prg/wordcnt.sd7        0.032795             54
seed7/prg/wrinum.sd7         0.032988             43
seed7/prg/wumpus.sd7         0.032732            372
seed7/lib/aes.s7i            0.032894           1144
seed7/lib/aes_gcm.s7i        0.033918            392
seed7/lib/ar.s7i             0.033902           1532
seed7/lib/arc4.s7i           0.033211            144
seed7/lib/archive.s7i        0.033960            143
seed7/lib/archive_base.s7i   0.033537            135
seed7/lib/array.s7i          0.033617            610
seed7/lib/asn1.s7i           0.033467            544
seed7/lib/asn1oid.s7i        0.033268            157
seed7/lib/basearray.s7i      0.033651            450
seed7/lib/bigfile.s7i        0.033816            136
seed7/lib/bigint.s7i         0.033497            824
seed7/lib/bigrat.s7i         0.033617            784
seed7/lib/bin16.s7i          0.033497            592
seed7/lib/bin32.s7i          0.033415            490
seed7/lib/bin64.s7i          0.033592            539
seed7/lib/bitdata.s7i        0.033706           1330
seed7/lib/bitmapfont.s7i     0.033607            215
seed7/lib/bitset.s7i         0.033729            593
seed7/lib/bitsetof.s7i       0.033771            431
seed7/lib/blowfish.s7i       0.033678            383
seed7/lib/bmp.s7i            0.034142            924
seed7/lib/boolean.s7i        0.033774            403
seed7/lib/browser.s7i        0.033824            280
seed7/lib/bstring.s7i        0.033664            227
seed7/lib/bytedata.s7i       0.033555            482
seed7/lib/bzip2.s7i          0.033883            887
seed7/lib/cards.s7i          0.033096           1342
seed7/lib/category.s7i       0.032576            209
seed7/lib/cc_conf.s7i        0.032611           1314
seed7/lib/ccittfax.s7i       0.032831           1022
seed7/lib/cgi.s7i            0.034409            109
seed7/lib/cgidialog.s7i      0.036556           1118
seed7/lib/char.s7i           0.034877            356
seed7/lib/charsets.s7i       0.034355           2024
seed7/lib/chartype.s7i       0.033773            121
seed7/lib/cipher.s7i         0.033658            146
seed7/lib/cli_cmds.s7i       0.033780           1360
seed7/lib/clib_file.s7i      0.033778            301
seed7/lib/color.s7i          0.033841            185
seed7/lib/complex.s7i        0.034060            464
seed7/lib/compress.s7i       0.033645            150
seed7/lib/console.s7i        0.033797            188
seed7/lib/cpio.s7i           0.033915           1708
seed7/lib/crc32.s7i          0.033588            193
seed7/lib/cronos16.s7i       0.033837           1173
seed7/lib/cronos27.s7i       0.034062           1464
seed7/lib/csv.s7i            0.033737            201
seed7/lib/db_prop.s7i        0.033693            991
seed7/lib/deflate.s7i        0.033668            740
seed7/lib/des.s7i            0.033767            444
seed7/lib/dialog.s7i         0.033591            311
seed7/lib/dir.s7i            0.033520            163
seed7/lib/draw.s7i           0.033958            854
seed7/lib/duration.s7i       0.033744           1038
seed7/lib/echo.s7i           0.033508            132
seed7/lib/editline.s7i       0.034143            398
seed7/lib/elf.s7i            0.033877           1560
seed7/lib/elliptic.s7i       0.033630            649
seed7/lib/enable_io.s7i      0.032618            312
seed7/lib/encoding.s7i       0.032598            931
seed7/lib/enumeration.s7i    0.032771            236
seed7/lib/environment.s7i    0.032501            175
seed7/lib/exif.s7i           0.032762            152
seed7/lib/external_file.s7i  0.033111            340
seed7/lib/field.s7i          0.034281            268
seed7/lib/file.s7i           0.033678            372
seed7/lib/filebits.s7i       0.033451             46
seed7/lib/filesys.s7i        0.033506            601
seed7/lib/fileutil.s7i       0.033890            144
seed7/lib/fixarray.s7i       0.033457            307
seed7/lib/float.s7i          0.033667            757
seed7/lib/font.s7i           0.033585            196
seed7/lib/font8x8.s7i        0.033487            998
seed7/lib/forloop.s7i        0.033705            449
seed7/lib/ftp.s7i            0.033359            969
seed7/lib/ftpserv.s7i        0.033631            631
seed7/lib/getf.s7i           0.033769            115
seed7/lib/gethttp.s7i        0.033698             41
seed7/lib/gethttps.s7i       0.033522             41
seed7/lib/gif.s7i            0.033820            561
seed7/lib/graph.s7i          0.034507            415
seed7/lib/graph_file.s7i     0.033767            399
seed7/lib/gtkserver.s7i      0.033915            161
seed7/lib/gzip.s7i           0.033784            573
seed7/lib/hash.s7i           0.033667            421
seed7/lib/hashsetof.s7i      0.033903            499
seed7/lib/hmac.s7i           0.033942            152
seed7/lib/html.s7i           0.033527             83
seed7/lib/html_ent.s7i       0.033842            476
seed7/lib/htmldom.s7i        0.033582            286
seed7/lib/http_request.s7i   0.032711            696
seed7/lib/http_srv_resp.s7i  0.033965            380
seed7/lib/https_request.s7i  0.033082            211
seed7/lib/httpserv.s7i       0.032839            345
seed7/lib/huffman.s7i        0.032883            644
seed7/lib/ico.s7i            0.033869            221
seed7/lib/idxarray.s7i       0.033659            232
seed7/lib/image.s7i          0.033639            156
seed7/lib/imagefile.s7i      0.033934            171
seed7/lib/inflate.s7i        0.033784            411
seed7/lib/inifile.s7i        0.033701            129
seed7/lib/integer.s7i        0.033705            663
seed7/lib/iobuffer.s7i       0.033821            289
seed7/lib/jpeg.s7i           0.033881           1761
seed7/lib/json.s7i           0.033899            891
seed7/lib/json_serde.s7i     0.033672            783
seed7/lib/keybd.s7i          0.034071            639
seed7/lib/keydescr.s7i       0.033844            192
seed7/lib/leb128.s7i         0.033827            218
seed7/lib/line.s7i           0.033536            164
seed7/lib/listener.s7i       0.034283            247
seed7/lib/logfile.s7i        0.034124             73
seed7/lib/lower.s7i          0.033823            142
seed7/lib/lzma.s7i           0.033898            934
seed7/lib/lzw.s7i            0.033707            861
seed7/lib/magic.s7i          0.033891            403
seed7/lib/mahjng32.s7i       0.033804           1500
seed7/lib/make.s7i           0.034119            544
seed7/lib/makedata.s7i       0.033949           1428
seed7/lib/math.s7i           0.033768            201
seed7/lib/mixarith.s7i       0.033882            249
seed7/lib/modern27.s7i       0.033131           1099
seed7/lib/more.s7i           0.032575            130
seed7/lib/msgdigest.s7i      0.033488           1222
seed7/lib/multiscr.s7i       0.032999             68
seed7/lib/null_file.s7i      0.032880            345
seed7/lib/osfiles.s7i        0.032698           1085
seed7/lib/pbm.s7i            0.034806            230
seed7/lib/pcx.s7i            0.034209            638
seed7/lib/pem.s7i            0.034056            185
seed7/lib/pgm.s7i            0.033866            238
seed7/lib/pic16.s7i          0.033695           1037
seed7/lib/pic32.s7i          0.033902           2060
seed7/lib/pic_util.s7i       0.033895            144
seed7/lib/pixelimage.s7i     0.033682            320
seed7/lib/pixmap_file.s7i    0.033788            459
seed7/lib/pixmapfont.s7i     0.033973            184
seed7/lib/pkcs1.s7i          0.033779            543
seed7/lib/png.s7i            0.033696           1064
seed7/lib/poll.s7i           0.034114            313
seed7/lib/ppm.s7i            0.034223            240
seed7/lib/process.s7i        0.033382            541
seed7/lib/progs.s7i          0.033836            789
seed7/lib/propertyfile.s7i   0.033960            155
seed7/lib/rational.s7i       0.033816            792
seed7/lib/ref_list.s7i       0.033422            252
seed7/lib/reference.s7i      0.033973            126
seed7/lib/reverse.s7i        0.033990             94
seed7/lib/rpm.s7i            0.034326           3487
seed7/lib/rpmext.s7i         0.033513            318
seed7/lib/scanfile.s7i       0.032914           1779
seed7/lib/scanjson.s7i       0.034104            413
seed7/lib/scanstri.s7i       0.033702           1814
seed7/lib/scantoml.s7i       0.032508           1603
seed7/lib/seed7_05.s7i       0.032992           1072
seed7/lib/set.s7i            0.032789             57
seed7/lib/shell.s7i          0.032709            615
seed7/lib/showtls.s7i        0.032678            678
seed7/lib/signature.s7i      0.033597            131
seed7/lib/smtp.s7i           0.034199            261
seed7/lib/sockbase.s7i       0.033716            217
seed7/lib/socket.s7i         0.033764            326
seed7/lib/sokoban1.s7i       0.033830           1519
seed7/lib/sql_base.s7i       0.033878           1000
seed7/lib/stars.s7i          0.033738           1705
seed7/lib/stdfont10.s7i      0.033907           3347
seed7/lib/stdfont12.s7i      0.033701           3928
seed7/lib/stdfont14.s7i      0.033828           4510
seed7/lib/stdfont16.s7i      0.033948           5092
seed7/lib/stdfont18.s7i      0.034216           5868
seed7/lib/stdfont20.s7i      0.034145           6449
seed7/lib/stdfont24.s7i      0.034188           7421
seed7/lib/stdfont8.s7i       0.033860           2960
seed7/lib/stdfont9.s7i       0.033808           3152
seed7/lib/stdio.s7i          0.033793            192
seed7/lib/strifile.s7i       0.033878            345
seed7/lib/string.s7i         0.033707            779
seed7/lib/stritext.s7i       0.033856            352
seed7/lib/struct.s7i         0.033774            266
seed7/lib/struct_elem.s7i    0.033797            129
seed7/lib/subfile.s7i        0.033779            174
seed7/lib/subrange.s7i       0.033719             78
seed7/lib/syntax.s7i         0.033800            294
seed7/lib/tar.s7i            0.033786           1880
seed7/lib/tar_cmds.s7i       0.034776            752
seed7/lib/tdes.s7i           0.035977            143
seed7/lib/tee.s7i            0.034059            143
seed7/lib/text.s7i           0.032832            135
seed7/lib/tga.s7i            0.032783            676
seed7/lib/tiff.s7i           0.033790           2771
seed7/lib/time.s7i           0.034411           1191
seed7/lib/tls.s7i            0.034079           2230
seed7/lib/unicode.s7i        0.035167            575
seed7/lib/unionfnd.s7i       0.040304            130
seed7/lib/upper.s7i          0.034753            142
seed7/lib/utf16.s7i          0.034148            540
seed7/lib/utf8.s7i           0.034109            234
seed7/lib/vecfont10.s7i      0.034115           1056
seed7/lib/vecfont18.s7i      0.033795           1119
seed7/lib/vector3d.s7i       0.033674            293
seed7/lib/vectorfont.s7i     0.034263            239
seed7/lib/wildcard.s7i       0.033959            140
seed7/lib/window.s7i         0.033921            455
seed7/lib/wrinum.s7i         0.033849            248
seed7/lib/x509cert.s7i       0.034393           1243
seed7/lib/xml_ent.s7i        0.034037             94
seed7/lib/xmldom.s7i         0.034798            303
seed7/lib/xz.s7i             0.034445            442
seed7/lib/zip.s7i            0.033999           2792
seed7/lib/zstd.s7i           0.033723           1263
============================ ================= ================

Statistical Summary — Mode A (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.033834        |
+-----------+-----------------+
| Minimum   | 0.032413        |
+-----------+-----------------+
| Maximum   | 0.040304        |
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
seed7/prg/addup.sd7          0.039504            190
seed7/prg/bas7.sd7           0.041365          11459
seed7/prg/bifurk.sd7         0.036988             73
seed7/prg/bigfiles.sd7       0.038882            129
seed7/prg/brainf7.sd7        0.036939             86
seed7/prg/calc7.sd7          0.037251            128
seed7/prg/carddemo.sd7       0.037790            190
seed7/prg/castle.sd7         0.037970           3148
seed7/prg/cat.sd7            0.035930             82
seed7/prg/cellauto.sd7       0.037907             85
seed7/prg/celsius.sd7        0.037388             42
seed7/prg/chk_all.sd7        0.039602            843
seed7/prg/chkarr.sd7         0.039507           8367
seed7/prg/chkbig.sd7         0.044307          29026
seed7/prg/chkbin.sd7         0.040495           6469
seed7/prg/chkbitdata.sd7     0.040712           6624
seed7/prg/chkbool.sd7        0.039445           3157
seed7/prg/chkbst.sd7         0.039615            722
seed7/prg/chkchr.sd7         0.040549           2809
seed7/prg/chkcmd.sd7         0.039615           1205
seed7/prg/chkdb.sd7          0.040272           7454
seed7/prg/chkdecl.sd7        0.039827            448
seed7/prg/chkenum.sd7        0.039039           1230
seed7/prg/chkerr.sd7         0.039865           4663
seed7/prg/chkexc.sd7         0.038015           2627
seed7/prg/chkfil.sd7         0.038969           1615
seed7/prg/chkflt.sd7         0.043539          20620
seed7/prg/chkhent.sd7        0.036290             54
seed7/prg/chkhsh.sd7         0.040309           4548
seed7/prg/chkidx.sd7         0.041353          19567
seed7/prg/chkint.sd7         0.046597          38129
seed7/prg/chkjson.sd7        0.038203           1764
seed7/prg/chkovf.sd7         0.038803           8216
seed7/prg/chkprc.sd7         0.037360          10111
seed7/prg/chkscan.sd7        0.037768            714
seed7/prg/chkset.sd7         0.039717          11974
seed7/prg/chkstr.sd7         0.043684          26952
seed7/prg/chktime.sd7        0.040285           2025
seed7/prg/chktoml.sd7        0.039379           1656
seed7/prg/clock.sd7          0.036369             47
seed7/prg/clock2.sd7         0.036076             43
seed7/prg/clock3.sd7         0.039481             95
seed7/prg/cmpfil.sd7         0.037916             84
seed7/prg/comanche.sd7       0.039207            180
seed7/prg/confval.sd7        0.039915            175
seed7/prg/db7.sd7            0.039167            417
seed7/prg/diff7.sd7          0.039424            263
seed7/prg/dirtst.sd7         0.036269             42
seed7/prg/dirx.sd7           0.039139            152
seed7/prg/dnafight.sd7       0.038944           1381
seed7/prg/dragon.sd7         0.037042             73
seed7/prg/echo.sd7           0.035944             39
seed7/prg/eliza.sd7          0.039201            302
seed7/prg/err.sd7            0.041915             96
seed7/prg/fannkuch.sd7       0.038928            131
seed7/prg/fib.sd7            0.036169             47
seed7/prg/find7.sd7          0.038724            133
seed7/prg/findchar.sd7       0.038870            149
seed7/prg/fractree.sd7       0.036310             55
seed7/prg/ftp7.sd7           0.037770            296
seed7/prg/ftpserv.sd7        0.036377             74
seed7/prg/gcd.sd7            0.037006            109
seed7/prg/gkbd.sd7           0.039152            358
seed7/prg/gtksvtst.sd7       0.041561             94
seed7/prg/hal.sd7            0.040063            250
seed7/prg/hamu.sd7           0.039682            573
seed7/prg/hanoi.sd7          0.036655             55
seed7/prg/hd.sd7             0.037475             79
seed7/prg/hello.sd7          0.035937             32
seed7/prg/hilbert.sd7        0.038356            108
seed7/prg/ide7.sd7           0.038934            196
seed7/prg/kbd.sd7            0.036476             49
seed7/prg/klondike.sd7       0.039511            883
seed7/prg/lander.sd7         0.039501           1551
seed7/prg/lst80bas.sd7       0.038437            344
seed7/prg/lst99bas.sd7       0.040147            401
seed7/prg/lstgwbas.sd7       0.039903            577
seed7/prg/mahjong.sd7        0.039398           1943
seed7/prg/make7.sd7          0.038722            121
seed7/prg/mandelbr.sd7       0.038814            237
seed7/prg/mind.sd7           0.039440            443
seed7/prg/mirror.sd7         0.040096            131
seed7/prg/ms.sd7             0.039086            641
seed7/prg/nicoma.sd7         0.038666            135
seed7/prg/pac.sd7            0.038863            726
seed7/prg/pairs.sd7          0.037560           2025
seed7/prg/panic.sd7          0.038533           2634
seed7/prg/percolation.sd7    0.037873            330
seed7/prg/planets.sd7        0.038426           1486
seed7/prg/portfwd7.sd7       0.037250            139
seed7/prg/prime.sd7          0.038280             74
seed7/prg/printpi1.sd7       0.037189             56
seed7/prg/printpi2.sd7       0.036890             54
seed7/prg/printpi3.sd7       0.037230             60
seed7/prg/pv7.sd7            0.039078            337
seed7/prg/queen.sd7          0.041099            149
seed7/prg/rand.sd7           0.041895            121
seed7/prg/raytrace.sd7       0.039400            538
seed7/prg/rever.sd7          0.039329            816
seed7/prg/roman.sd7          0.036425             38
seed7/prg/s7c.sd7            0.039311           9060
seed7/prg/s7check.sd7        0.037039             68
seed7/prg/savehd7.sd7        0.039460           1110
seed7/prg/self.sd7           0.036770             49
seed7/prg/shisen.sd7         0.038733           1423
seed7/prg/sl.sd7             0.039075           1029
seed7/prg/snake.sd7          0.039385            615
seed7/prg/sokoban.sd7        0.039532            891
seed7/prg/spigotpi.sd7       0.036988             64
seed7/prg/sql7.sd7           0.038994            278
seed7/prg/startrek.sd7       0.039429            979
seed7/prg/sudoku7.sd7        0.038548           2657
seed7/prg/sydir7.sd7         0.038118            384
seed7/prg/syntaxhl.sd7       0.040328            177
seed7/prg/tak.sd7            0.035564             59
seed7/prg/tar7.sd7           0.039651            121
seed7/prg/tch.sd7            0.036859             55
seed7/prg/testfont.sd7       0.039249             95
seed7/prg/tet.sd7            0.039238            479
seed7/prg/tetg.sd7           0.039395            501
seed7/prg/toutf8.sd7         0.040476            240
seed7/prg/tst_cli.sd7        0.036109             40
seed7/prg/tst_srv.sd7        0.036450             47
seed7/prg/wator.sd7          0.039008            651
seed7/prg/which.sd7          0.037079             65
seed7/prg/wiz.sd7            0.039239           2833
seed7/prg/wordcnt.sd7        0.036437             54
seed7/prg/wrinum.sd7         0.035994             43
seed7/prg/wumpus.sd7         0.038827            372
seed7/lib/aes.s7i            0.042689           1144
seed7/lib/aes_gcm.s7i        0.040004            392
seed7/lib/ar.s7i             0.039395           1532
seed7/lib/arc4.s7i           0.039232            144
seed7/lib/archive.s7i        0.038951            143
seed7/lib/archive_base.s7i   0.039195            135
seed7/lib/array.s7i          0.039712            610
seed7/lib/asn1.s7i           0.037621            544
seed7/lib/asn1oid.s7i        0.043125            157
seed7/lib/basearray.s7i      0.040247            450
seed7/lib/bigfile.s7i        0.037134            136
seed7/lib/bigint.s7i         0.037168            824
seed7/lib/bigrat.s7i         0.037742            784
seed7/lib/bin16.s7i          0.037548            592
seed7/lib/bin32.s7i          0.037407            490
seed7/lib/bin64.s7i          0.040269            539
seed7/lib/bitdata.s7i        0.044954           1330
seed7/lib/bitmapfont.s7i     0.039095            215
seed7/lib/bitset.s7i         0.038771            593
seed7/lib/bitsetof.s7i       0.040287            431
seed7/lib/blowfish.s7i       0.042158            383
seed7/lib/bmp.s7i            0.039458            924
seed7/lib/boolean.s7i        0.039040            403
seed7/lib/browser.s7i        0.039361            280
seed7/lib/bstring.s7i        0.038748            227
seed7/lib/bytedata.s7i       0.039377            482
seed7/lib/bzip2.s7i          0.039300            887
seed7/lib/cards.s7i          0.037244           1342
seed7/lib/category.s7i       0.038844            209
seed7/lib/cc_conf.s7i        0.038198           1314
seed7/lib/ccittfax.s7i       0.039502           1022
seed7/lib/cgi.s7i            0.038383            109
seed7/lib/cgidialog.s7i      0.038842           1118
seed7/lib/char.s7i           0.038696            356
seed7/lib/charsets.s7i       0.039481           2024
seed7/lib/chartype.s7i       0.043175            121
seed7/lib/cipher.s7i         0.040347            146
seed7/lib/cli_cmds.s7i       0.040275           1360
seed7/lib/clib_file.s7i      0.039305            301
seed7/lib/color.s7i          0.038616            185
seed7/lib/complex.s7i        0.038627            464
seed7/lib/compress.s7i       0.039000            150
seed7/lib/console.s7i        0.039108            188
seed7/lib/cpio.s7i           0.039514           1708
seed7/lib/crc32.s7i          0.039728            193
seed7/lib/cronos16.s7i       0.042456           1173
seed7/lib/cronos27.s7i       0.043420           1464
seed7/lib/csv.s7i            0.039246            201
seed7/lib/db_prop.s7i        0.039510            991
seed7/lib/deflate.s7i        0.039093            740
seed7/lib/des.s7i            0.039711            444
seed7/lib/dialog.s7i         0.039267            311
seed7/lib/dir.s7i            0.038762            163
seed7/lib/draw.s7i           0.039366            854
seed7/lib/duration.s7i       0.039125           1038
seed7/lib/echo.s7i           0.039130            132
seed7/lib/editline.s7i       0.038882            398
seed7/lib/elf.s7i            0.040774           1560
seed7/lib/elliptic.s7i       0.038831            649
seed7/lib/enable_io.s7i      0.039091            312
seed7/lib/encoding.s7i       0.039549            931
seed7/lib/enumeration.s7i    0.039245            236
seed7/lib/environment.s7i    0.038406            175
seed7/lib/exif.s7i           0.040049            152
seed7/lib/external_file.s7i  0.037741            340
seed7/lib/field.s7i          0.037488            268
seed7/lib/file.s7i           0.037723            372
seed7/lib/filebits.s7i       0.035854             46
seed7/lib/filesys.s7i        0.038192            601
seed7/lib/fileutil.s7i       0.039884            144
seed7/lib/fixarray.s7i       0.039918            307
seed7/lib/float.s7i          0.038962            757
seed7/lib/font.s7i           0.038690            196
seed7/lib/font8x8.s7i        0.039372            998
seed7/lib/forloop.s7i        0.040041            449
seed7/lib/ftp.s7i            0.038724            969
seed7/lib/ftpserv.s7i        0.039106            631
seed7/lib/getf.s7i           0.038618            115
seed7/lib/gethttp.s7i        0.036420             41
seed7/lib/gethttps.s7i       0.036089             41
seed7/lib/gif.s7i            0.039028            561
seed7/lib/graph.s7i          0.041190            415
seed7/lib/graph_file.s7i     0.038989            399
seed7/lib/gtkserver.s7i      0.038295            161
seed7/lib/gzip.s7i           0.039011            573
seed7/lib/hash.s7i           0.040607            421
seed7/lib/hashsetof.s7i      0.040896            499
seed7/lib/hmac.s7i           0.038666            152
seed7/lib/html.s7i           0.037449             83
seed7/lib/html_ent.s7i       0.039299            476
seed7/lib/htmldom.s7i        0.039287            286
seed7/lib/http_request.s7i   0.038396            696
seed7/lib/http_srv_resp.s7i  0.037971            380
seed7/lib/https_request.s7i  0.037696            211
seed7/lib/httpserv.s7i       0.037623            345
seed7/lib/huffman.s7i        0.037943            644
seed7/lib/ico.s7i            0.039971            221
seed7/lib/idxarray.s7i       0.039350            232
seed7/lib/image.s7i          0.038404            156
seed7/lib/imagefile.s7i      0.039045            171
seed7/lib/inflate.s7i        0.039430            411
seed7/lib/inifile.s7i        0.038744            129
seed7/lib/integer.s7i        0.038944            663
seed7/lib/iobuffer.s7i       0.038615            289
seed7/lib/jpeg.s7i           0.039100           1761
seed7/lib/json.s7i           0.039093            891
seed7/lib/json_serde.s7i     0.044919            783
seed7/lib/keybd.s7i          0.042180            639
seed7/lib/keydescr.s7i       0.040467            192
seed7/lib/leb128.s7i         0.039315            218
seed7/lib/line.s7i           0.038998            164
seed7/lib/listener.s7i       0.038967            247
seed7/lib/logfile.s7i        0.037640             73
seed7/lib/lower.s7i          0.038350            142
seed7/lib/lzma.s7i           0.039814            934
seed7/lib/lzw.s7i            0.040207            861
seed7/lib/magic.s7i          0.040464            403
seed7/lib/mahjng32.s7i       0.038705           1500
seed7/lib/make.s7i           0.038038            544
seed7/lib/makedata.s7i       0.037703           1428
seed7/lib/math.s7i           0.037657            201
seed7/lib/mixarith.s7i       0.037353            249
seed7/lib/modern27.s7i       0.040917           1099
seed7/lib/more.s7i           0.039620            130
seed7/lib/msgdigest.s7i      0.040491           1222
seed7/lib/multiscr.s7i       0.037292             68
seed7/lib/null_file.s7i      0.038969            345
seed7/lib/osfiles.s7i        0.040554           1085
seed7/lib/pbm.s7i            0.039249            230
seed7/lib/pcx.s7i            0.038724            638
seed7/lib/pem.s7i            0.038614            185
seed7/lib/pgm.s7i            0.039193            238
seed7/lib/pic16.s7i          0.038551           1037
seed7/lib/pic32.s7i          0.038532           2060
seed7/lib/pic_util.s7i       0.038953            144
seed7/lib/pixelimage.s7i     0.039152            320
seed7/lib/pixmap_file.s7i    0.039180            459
seed7/lib/pixmapfont.s7i     0.040474            184
seed7/lib/pkcs1.s7i          0.044329            543
seed7/lib/png.s7i            0.038879           1064
seed7/lib/poll.s7i           0.038491            313
seed7/lib/ppm.s7i            0.039096            240
seed7/lib/process.s7i        0.039209            541
seed7/lib/progs.s7i          0.039422            789
seed7/lib/propertyfile.s7i   0.039114            155
seed7/lib/rational.s7i       0.038345            792
seed7/lib/ref_list.s7i       0.038174            252
seed7/lib/reference.s7i      0.041366            126
seed7/lib/reverse.s7i        0.040582             94
seed7/lib/rpm.s7i            0.038742           3487
seed7/lib/rpmext.s7i         0.039595            318
seed7/lib/scanfile.s7i       0.039262           1779
seed7/lib/scanjson.s7i       0.039495            413
seed7/lib/scanstri.s7i       0.039398           1814
seed7/lib/scantoml.s7i       0.039232           1603
seed7/lib/seed7_05.s7i       0.041013           1072
seed7/lib/set.s7i            0.037023             57
seed7/lib/shell.s7i          0.039669            615
seed7/lib/showtls.s7i        0.039110            678
seed7/lib/signature.s7i      0.038666            131
seed7/lib/smtp.s7i           0.039245            261
seed7/lib/sockbase.s7i       0.040120            217
seed7/lib/socket.s7i         0.038971            326
seed7/lib/sokoban1.s7i       0.038395           1519
seed7/lib/sql_base.s7i       0.039058           1000
seed7/lib/stars.s7i          0.041286           1705
seed7/lib/stdfont10.s7i      0.037640           3347
seed7/lib/stdfont12.s7i      0.038622           3928
seed7/lib/stdfont14.s7i      0.038720           4510
seed7/lib/stdfont16.s7i      0.038968           5092
seed7/lib/stdfont18.s7i      0.039063           5868
seed7/lib/stdfont20.s7i      0.038644           6449
seed7/lib/stdfont24.s7i      0.038233           7421
seed7/lib/stdfont8.s7i       0.036158           2960
seed7/lib/stdfont9.s7i       0.036241           3152
seed7/lib/stdio.s7i          0.037297            192
seed7/lib/strifile.s7i       0.037497            345
seed7/lib/string.s7i         0.037861            779
seed7/lib/stritext.s7i       0.039131            352
seed7/lib/struct.s7i         0.040698            266
seed7/lib/struct_elem.s7i    0.039007            129
seed7/lib/subfile.s7i        0.038638            174
seed7/lib/subrange.s7i       0.037600             78
seed7/lib/syntax.s7i         0.039330            294
seed7/lib/tar.s7i            0.039605           1880
seed7/lib/tar_cmds.s7i       0.039312            752
seed7/lib/tdes.s7i           0.038822            143
seed7/lib/tee.s7i            0.038848            143
seed7/lib/text.s7i           0.038638            135
seed7/lib/tga.s7i            0.041382            676
seed7/lib/tiff.s7i           0.040992           2771
seed7/lib/time.s7i           0.037744           1191
seed7/lib/tls.s7i            0.037085           2230
seed7/lib/unicode.s7i        0.039596            575
seed7/lib/unionfnd.s7i       0.038146            130
seed7/lib/upper.s7i          0.037812            142
seed7/lib/utf16.s7i          0.038877            540
seed7/lib/utf8.s7i           0.040053            234
seed7/lib/vecfont10.s7i      0.041923           1056
seed7/lib/vecfont18.s7i      0.042335           1119
seed7/lib/vector3d.s7i       0.037958            293
seed7/lib/vectorfont.s7i     0.037482            239
seed7/lib/wildcard.s7i       0.037884            140
seed7/lib/window.s7i         0.037461            455
seed7/lib/wrinum.s7i         0.037818            248
seed7/lib/x509cert.s7i       0.041107           1243
seed7/lib/xml_ent.s7i        0.038743             94
seed7/lib/xmldom.s7i         0.038813            303
seed7/lib/xz.s7i             0.038818            442
seed7/lib/zip.s7i            0.039388           2792
seed7/lib/zstd.s7i           0.038859           1263
============================ ================= ================

Statistical Summary — Mode B (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.039011        |
+-----------+-----------------+
| Minimum   | 0.035564        |
+-----------+-----------------+
| Maximum   | 0.046597        |
+-----------+-----------------+

Mode C — Mode Activation + Full-Buffer Fontification (font-lock-ensure)
-----------------------------------------------------------------------

Full-buffer fontification stress test.

File Load Times — Mode C (mean, GC-free)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

============================ ================= ================
File Name                    Mean Time (s)     Line count
============================ ================= ================
seed7/prg/addup.sd7          0.039764            190
seed7/prg/bas7.sd7           0.321936          11459
seed7/prg/bifurk.sd7         0.036554             73
seed7/prg/bigfiles.sd7       0.037739            129
seed7/prg/brainf7.sd7        0.036525             86
seed7/prg/calc7.sd7          0.037998            128
seed7/prg/carddemo.sd7       0.038748            190
seed7/prg/castle.sd7         0.107160           3148
seed7/prg/cat.sd7            0.037145             82
seed7/prg/cellauto.sd7       0.037129             85
seed7/prg/celsius.sd7        0.035233             42
seed7/prg/chk_all.sd7        0.056597            843
seed7/prg/chkarr.sd7         0.349444           8367
seed7/prg/chkbig.sd7         2.042448          29026
seed7/prg/chkbin.sd7         0.508849           6469
seed7/prg/chkbitdata.sd7     0.605344           6624
seed7/prg/chkbool.sd7        0.114544           3157
seed7/prg/chkbst.sd7         0.061603            722
seed7/prg/chkchr.sd7         0.214072           2809
seed7/prg/chkcmd.sd7         0.066553           1205
seed7/prg/chkdb.sd7          0.344592           7454
seed7/prg/chkdecl.sd7        0.056097            448
seed7/prg/chkenum.sd7        0.067946           1230
seed7/prg/chkerr.sd7         0.191770           4663
seed7/prg/chkexc.sd7         0.081648           2627
seed7/prg/chkfil.sd7         0.074780           1615
seed7/prg/chkflt.sd7         1.325862          20620
seed7/prg/chkhent.sd7        0.036707             54
seed7/prg/chkhsh.sd7         0.242468           4548
seed7/prg/chkidx.sd7         1.309349          19567
seed7/prg/chkint.sd7         2.502998          38129
seed7/prg/chkjson.sd7        0.099954           1764
seed7/prg/chkovf.sd7         0.553705           8216
seed7/prg/chkprc.sd7         0.318710          10111
seed7/prg/chkscan.sd7        0.056328            714
seed7/prg/chkset.sd7         0.651022          11974
seed7/prg/chkstr.sd7         1.372398          26952
seed7/prg/chktime.sd7        0.129079           2025
seed7/prg/chktoml.sd7        0.106405           1656
seed7/prg/clock.sd7          0.048014             47
seed7/prg/clock2.sd7         0.039865             43
seed7/prg/clock3.sd7         0.037416             95
seed7/prg/cmpfil.sd7         0.036478             84
seed7/prg/comanche.sd7       0.039895            180
seed7/prg/confval.sd7        0.041056            175
seed7/prg/db7.sd7            0.045597            417
seed7/prg/diff7.sd7          0.040968            263
seed7/prg/dirtst.sd7         0.034377             42
seed7/prg/dirx.sd7           0.036912            152
seed7/prg/dnafight.sd7       0.067213           1381
seed7/prg/dragon.sd7         0.036207             73
seed7/prg/echo.sd7           0.035409             39
seed7/prg/eliza.sd7          0.042390            302
seed7/prg/err.sd7            0.039730             96
seed7/prg/fannkuch.sd7       0.037331            131
seed7/prg/fib.sd7            0.035254             47
seed7/prg/find7.sd7          0.037935            133
seed7/prg/findchar.sd7       0.038442            149
seed7/prg/fractree.sd7       0.034892             55
seed7/prg/ftp7.sd7           0.042008            296
seed7/prg/ftpserv.sd7        0.036757             74
seed7/prg/gcd.sd7            0.036597            109
seed7/prg/gkbd.sd7           0.045662            358
seed7/prg/gtksvtst.sd7       0.037172             94
seed7/prg/hal.sd7            0.040437            250
seed7/prg/hamu.sd7           0.049206            573
seed7/prg/hanoi.sd7          0.036956             55
seed7/prg/hd.sd7             0.037407             79
seed7/prg/hello.sd7          0.035841             32
seed7/prg/hilbert.sd7        0.037923            108
seed7/prg/ide7.sd7           0.039847            196
seed7/prg/kbd.sd7            0.034671             49
seed7/prg/klondike.sd7       0.052717            883
seed7/prg/lander.sd7         0.068888           1551
seed7/prg/lst80bas.sd7       0.042117            344
seed7/prg/lst99bas.sd7       0.046364            401
seed7/prg/lstgwbas.sd7       0.050419            577
seed7/prg/mahjong.sd7        0.079995           1943
seed7/prg/make7.sd7          0.037934            121
seed7/prg/mandelbr.sd7       0.040810            237
seed7/prg/mind.sd7           0.044832            443
seed7/prg/mirror.sd7         0.038895            131
seed7/prg/ms.sd7             0.048589            641
seed7/prg/nicoma.sd7         0.037815            135
seed7/prg/pac.sd7            0.049050            726
seed7/prg/pairs.sd7          0.082373           2025
seed7/prg/panic.sd7          0.096532           2634
seed7/prg/percolation.sd7    0.042509            330
seed7/prg/planets.sd7        0.075876           1486
seed7/prg/portfwd7.sd7       0.038672            139
seed7/prg/prime.sd7          0.035484             74
seed7/prg/printpi1.sd7       0.034732             56
seed7/prg/printpi2.sd7       0.034403             54
seed7/prg/printpi3.sd7       0.034771             60
seed7/prg/pv7.sd7            0.042633            337
seed7/prg/queen.sd7          0.036509            149
seed7/prg/rand.sd7           0.037803            121
seed7/prg/raytrace.sd7       0.048471            538
seed7/prg/rever.sd7          0.053835            816
seed7/prg/roman.sd7          0.036246             38
seed7/prg/s7c.sd7            0.277804           9060
seed7/prg/s7check.sd7        0.036598             68
seed7/prg/savehd7.sd7        0.066581           1110
seed7/prg/self.sd7           0.035810             49
seed7/prg/shisen.sd7         0.071797           1423
seed7/prg/sl.sd7             0.059165           1029
seed7/prg/snake.sd7          0.046763            615
seed7/prg/sokoban.sd7        0.056095            891
seed7/prg/spigotpi.sd7       0.034918             64
seed7/prg/sql7.sd7           0.040039            278
seed7/prg/startrek.sd7       0.057051            979
seed7/prg/sudoku7.sd7        0.101480           2657
seed7/prg/sydir7.sd7         0.046150            384
seed7/prg/syntaxhl.sd7       0.041272            177
seed7/prg/tak.sd7            0.036430             59
seed7/prg/tar7.sd7           0.037912            121
seed7/prg/tch.sd7            0.035794             55
seed7/prg/testfont.sd7       0.037157             95
seed7/prg/tet.sd7            0.044369            479
seed7/prg/tetg.sd7           0.045751            501
seed7/prg/toutf8.sd7         0.042777            240
seed7/prg/tst_cli.sd7        0.035578             40
seed7/prg/tst_srv.sd7        0.035559             47
seed7/prg/wator.sd7          0.052077            651
seed7/prg/which.sd7          0.036038             65
seed7/prg/wiz.sd7            0.104594           2833
seed7/prg/wordcnt.sd7        0.036056             54
seed7/prg/wrinum.sd7         0.035145             43
seed7/prg/wumpus.sd7         0.042452            372
seed7/lib/aes.s7i            0.108449           1144
seed7/lib/aes_gcm.s7i        0.045274            392
seed7/lib/ar.s7i             0.071795           1532
seed7/lib/arc4.s7i           0.036792            144
seed7/lib/archive.s7i        0.038422            143
seed7/lib/archive_base.s7i   0.039082            135
seed7/lib/array.s7i          0.053669            610
seed7/lib/asn1.s7i           0.047142            544
seed7/lib/asn1oid.s7i        0.041537            157
seed7/lib/basearray.s7i      0.048699            450
seed7/lib/bigfile.s7i        0.038537            136
seed7/lib/bigint.s7i         0.055426            824
seed7/lib/bigrat.s7i         0.053865            784
seed7/lib/bin16.s7i          0.050650            592
seed7/lib/bin32.s7i          0.048007            490
seed7/lib/bin64.s7i          0.049431            539
seed7/lib/bitdata.s7i        0.076040           1330
seed7/lib/bitmapfont.s7i     0.040194            215
seed7/lib/bitset.s7i         0.049333            593
seed7/lib/bitsetof.s7i       0.047233            431
seed7/lib/blowfish.s7i       0.056228            383
seed7/lib/bmp.s7i            0.059436            924
seed7/lib/boolean.s7i        0.043084            403
seed7/lib/browser.s7i        0.041603            280
seed7/lib/bstring.s7i        0.039642            227
seed7/lib/bytedata.s7i       0.048167            482
seed7/lib/bzip2.s7i          0.059041            887
seed7/lib/cards.s7i          0.065975           1342
seed7/lib/category.s7i       0.041159            209
seed7/lib/cc_conf.s7i        0.081754           1314
seed7/lib/ccittfax.s7i       0.065940           1022
seed7/lib/cgi.s7i            0.037827            109
seed7/lib/cgidialog.s7i      0.060230           1118
seed7/lib/char.s7i           0.043245            356
seed7/lib/charsets.s7i       0.082033           2024
seed7/lib/chartype.s7i       0.039951            121
seed7/lib/cipher.s7i         0.038216            146
seed7/lib/cli_cmds.s7i       0.068397           1360
seed7/lib/clib_file.s7i      0.043337            301
seed7/lib/color.s7i          0.040925            185
seed7/lib/complex.s7i        0.047470            464
seed7/lib/compress.s7i       0.038595            150
seed7/lib/console.s7i        0.038338            188
seed7/lib/cpio.s7i           0.079475           1708
seed7/lib/crc32.s7i          0.042348            193
seed7/lib/cronos16.s7i       0.090160           1173
seed7/lib/cronos27.s7i       0.117156           1464
seed7/lib/csv.s7i            0.041102            201
seed7/lib/db_prop.s7i        0.065525            991
seed7/lib/deflate.s7i        0.057176            740
seed7/lib/des.s7i            0.055425            444
seed7/lib/dialog.s7i         0.044366            311
seed7/lib/dir.s7i            0.038092            163
seed7/lib/draw.s7i           0.055738            854
seed7/lib/duration.s7i       0.061095           1038
seed7/lib/echo.s7i           0.038098            132
seed7/lib/editline.s7i       0.045685            398
seed7/lib/elf.s7i            0.085390           1560
seed7/lib/elliptic.s7i       0.052484            649
seed7/lib/enable_io.s7i      0.043087            312
seed7/lib/encoding.s7i       0.059305            931
seed7/lib/enumeration.s7i    0.040700            236
seed7/lib/environment.s7i    0.037946            175
seed7/lib/exif.s7i           0.038286            152
seed7/lib/external_file.s7i  0.043957            340
seed7/lib/field.s7i          0.042478            268
seed7/lib/file.s7i           0.044508            372
seed7/lib/filebits.s7i       0.036510             46
seed7/lib/filesys.s7i        0.048614            601
seed7/lib/fileutil.s7i       0.038340            144
seed7/lib/fixarray.s7i       0.043323            307
seed7/lib/float.s7i          0.055247            757
seed7/lib/font.s7i           0.039398            196
seed7/lib/font8x8.s7i        0.048323            998
seed7/lib/forloop.s7i        0.045595            449
seed7/lib/ftp.s7i            0.104439            969
seed7/lib/ftpserv.s7i        0.065253            631
seed7/lib/getf.s7i           0.043046            115
seed7/lib/gethttp.s7i        0.036955             41
seed7/lib/gethttps.s7i       0.035746             41
seed7/lib/gif.s7i            0.049734            561
seed7/lib/graph.s7i          0.048600            415
seed7/lib/graph_file.s7i     0.044870            399
seed7/lib/gtkserver.s7i      0.039093            161
seed7/lib/gzip.s7i           0.049345            573
seed7/lib/hash.s7i           0.050564            421
seed7/lib/hashsetof.s7i      0.049260            499
seed7/lib/hmac.s7i           0.040857            152
seed7/lib/html.s7i           0.036629             83
seed7/lib/html_ent.s7i       0.046337            476
seed7/lib/htmldom.s7i        0.042722            286
seed7/lib/http_request.s7i   0.051432            696
seed7/lib/http_srv_resp.s7i  0.046060            380
seed7/lib/https_request.s7i  0.040159            211
seed7/lib/httpserv.s7i       0.045784            345
seed7/lib/huffman.s7i        0.054878            644
seed7/lib/ico.s7i            0.041714            221
seed7/lib/idxarray.s7i       0.042624            232
seed7/lib/image.s7i          0.038591            156
seed7/lib/imagefile.s7i      0.040254            171
seed7/lib/inflate.s7i        0.047569            411
seed7/lib/inifile.s7i        0.038900            129
seed7/lib/integer.s7i        0.052677            663
seed7/lib/iobuffer.s7i       0.042698            289
seed7/lib/jpeg.s7i           0.082889           1761
seed7/lib/json.s7i           0.054908            891
seed7/lib/json_serde.s7i     0.054791            783
seed7/lib/keybd.s7i          0.056901            639
seed7/lib/keydescr.s7i       0.044017            192
seed7/lib/leb128.s7i         0.040612            218
seed7/lib/line.s7i           0.039780            164
seed7/lib/listener.s7i       0.041918            247
seed7/lib/logfile.s7i        0.038771             73
seed7/lib/lower.s7i          0.039407            142
seed7/lib/lzma.s7i           0.059873            934
seed7/lib/lzw.s7i            0.059240            861
seed7/lib/magic.s7i          0.048941            403
seed7/lib/mahjng32.s7i       0.067525           1500
seed7/lib/make.s7i           0.050853            544
seed7/lib/makedata.s7i       0.070051           1428
seed7/lib/math.s7i           0.039739            201
seed7/lib/mixarith.s7i       0.039934            249
seed7/lib/modern27.s7i       0.083331           1099
seed7/lib/more.s7i           0.037815            130
seed7/lib/msgdigest.s7i      0.078120           1222
seed7/lib/multiscr.s7i       0.037036             68
seed7/lib/null_file.s7i      0.042307            345
seed7/lib/osfiles.s7i        0.064216           1085
seed7/lib/pbm.s7i            0.040382            230
seed7/lib/pcx.s7i            0.053733            638
seed7/lib/pem.s7i            0.040306            185
seed7/lib/pgm.s7i            0.040730            238
seed7/lib/pic16.s7i          0.049312           1037
seed7/lib/pic32.s7i          0.080315           2060
seed7/lib/pic_util.s7i       0.039138            144
seed7/lib/pixelimage.s7i     0.042815            320
seed7/lib/pixmap_file.s7i    0.046569            459
seed7/lib/pixmapfont.s7i     0.040177            184
seed7/lib/pkcs1.s7i          0.059480            543
seed7/lib/png.s7i            0.066368           1064
seed7/lib/poll.s7i           0.044810            313
seed7/lib/ppm.s7i            0.041224            240
seed7/lib/process.s7i        0.049096            541
seed7/lib/progs.s7i          0.057124            789
seed7/lib/propertyfile.s7i   0.038885            155
seed7/lib/rational.s7i       0.053076            792
seed7/lib/ref_list.s7i       0.041251            252
seed7/lib/reference.s7i      0.037685            126
seed7/lib/reverse.s7i        0.036993             94
seed7/lib/rpm.s7i            0.143279           3487
seed7/lib/rpmext.s7i         0.048900            318
seed7/lib/scanfile.s7i       0.081651           1779
seed7/lib/scanjson.s7i       0.046655            413
seed7/lib/scanstri.s7i       0.079743           1814
seed7/lib/scantoml.s7i       0.075361           1603
seed7/lib/seed7_05.s7i       0.066537           1072
seed7/lib/set.s7i            0.037489             57
seed7/lib/shell.s7i          0.052914            615
seed7/lib/showtls.s7i        0.054753            678
seed7/lib/signature.s7i      0.038890            131
seed7/lib/smtp.s7i           0.041594            261
seed7/lib/sockbase.s7i       0.043149            217
seed7/lib/socket.s7i         0.043510            326
seed7/lib/sokoban1.s7i       0.053642           1519
seed7/lib/sql_base.s7i       0.064131           1000
seed7/lib/stars.s7i          0.134601           1705
seed7/lib/stdfont10.s7i      0.082480           3347
seed7/lib/stdfont12.s7i      0.092180           3928
seed7/lib/stdfont14.s7i      0.103090           4510
seed7/lib/stdfont16.s7i      0.114946           5092
seed7/lib/stdfont18.s7i      0.131378           5868
seed7/lib/stdfont20.s7i      0.146978           6449
seed7/lib/stdfont24.s7i      0.177940           7421
seed7/lib/stdfont8.s7i       0.072888           2960
seed7/lib/stdfont9.s7i       0.075238           3152
seed7/lib/stdio.s7i          0.039093            192
seed7/lib/strifile.s7i       0.044037            345
seed7/lib/string.s7i         0.057085            779
seed7/lib/stritext.s7i       0.043498            352
seed7/lib/struct.s7i         0.045642            266
seed7/lib/struct_elem.s7i    0.039301            129
seed7/lib/subfile.s7i        0.039349            174
seed7/lib/subrange.s7i       0.037231             78
seed7/lib/syntax.s7i         0.047014            294
seed7/lib/tar.s7i            0.082214           1880
seed7/lib/tar_cmds.s7i       0.055377            752
seed7/lib/tdes.s7i           0.038718            143
seed7/lib/tee.s7i            0.037149            143
seed7/lib/text.s7i           0.037755            135
seed7/lib/tga.s7i            0.053507            676
seed7/lib/tiff.s7i           0.120268           2771
seed7/lib/time.s7i           0.062407           1191
seed7/lib/tls.s7i            0.104867           2230
seed7/lib/unicode.s7i        0.051637            575
seed7/lib/unionfnd.s7i       0.037909            130
seed7/lib/upper.s7i          0.038645            142
seed7/lib/utf16.s7i          0.049307            540
seed7/lib/utf8.s7i           0.046557            234
seed7/lib/vecfont10.s7i      0.079925           1056
seed7/lib/vecfont18.s7i      0.087911           1119
seed7/lib/vector3d.s7i       0.041615            293
seed7/lib/vectorfont.s7i     0.041328            239
seed7/lib/wildcard.s7i       0.039033            140
seed7/lib/window.s7i         0.046156            455
seed7/lib/wrinum.s7i         0.041817            248
seed7/lib/x509cert.s7i       0.072236           1243
seed7/lib/xml_ent.s7i        0.038849             94
seed7/lib/xmldom.s7i         0.041993            303
seed7/lib/xz.s7i             0.046382            442
seed7/lib/zip.s7i            0.118847           2792
seed7/lib/zstd.s7i           0.070525           1263
============================ ================= ================

Statistical Summary — Mode C (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.088926        |
+-----------+-----------------+
| Minimum   | 0.034377        |
+-----------+-----------------+
| Maximum   | 2.502998        |
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
seed7/prg/addup.sd7          0.045143            190
seed7/prg/bas7.sd7           0.756285          11459
seed7/prg/bifurk.sd7         0.038129             73
seed7/prg/bigfiles.sd7       0.042612            129
seed7/prg/brainf7.sd7        0.039191             86
seed7/prg/calc7.sd7          0.042081            128
seed7/prg/carddemo.sd7       0.047280            190
seed7/prg/castle.sd7         0.211426           3148
seed7/prg/cat.sd7            0.038902             82
seed7/prg/cellauto.sd7       0.039598             85
seed7/prg/celsius.sd7        0.036850             42
seed7/prg/chk_all.sd7        0.081114            843
seed7/prg/chkarr.sd7         0.849173           8367
seed7/prg/chkbig.sd7         4.076068          29026
seed7/prg/chkbin.sd7         1.014353           6469
seed7/prg/chkbitdata.sd7     1.232161           6624
seed7/prg/chkbool.sd7        0.227118           3157
seed7/prg/chkbst.sd7         0.099885            722
seed7/prg/chkchr.sd7         0.472826           2809
seed7/prg/chkcmd.sd7         0.109480           1205
seed7/prg/chkdb.sd7          0.744541           7454
seed7/prg/chkdecl.sd7        0.091884            448
seed7/prg/chkenum.sd7        0.121114           1230
seed7/prg/chkerr.sd7         0.335979           4663
seed7/prg/chkexc.sd7         0.146589           2627
seed7/prg/chkfil.sd7         0.126968           1615
seed7/prg/chkflt.sd7         2.788370          20620
seed7/prg/chkhent.sd7        0.039077             54
seed7/prg/chkhsh.sd7         0.494080           4548
seed7/prg/chkidx.sd7         3.271977          19567
seed7/prg/chkint.sd7         5.534053          38129
seed7/prg/chkjson.sd7        0.185405           1764
seed7/prg/chkovf.sd7         1.192760           8216
seed7/prg/chkprc.sd7         0.691074          10111
seed7/prg/chkscan.sd7        0.089620            714
seed7/prg/chkset.sd7         1.723667          11974
seed7/prg/chkstr.sd7         3.383229          26952
seed7/prg/chktime.sd7        0.243340           2025
seed7/prg/chktoml.sd7        0.193388           1656
seed7/prg/clock.sd7          0.037057             47
seed7/prg/clock2.sd7         0.036482             43
seed7/prg/clock3.sd7         0.041677             95
seed7/prg/cmpfil.sd7         0.038725             84
seed7/prg/comanche.sd7       0.046605            180
seed7/prg/confval.sd7        0.052119            175
seed7/prg/db7.sd7            0.062537            417
seed7/prg/diff7.sd7          0.051474            263
seed7/prg/dirtst.sd7         0.036907             42
seed7/prg/dirx.sd7           0.042936            152
seed7/prg/dnafight.sd7       0.116939           1381
seed7/prg/dragon.sd7         0.038223             73
seed7/prg/echo.sd7           0.036875             39
seed7/prg/eliza.sd7          0.052023            302
seed7/prg/err.sd7            0.045168             96
seed7/prg/fannkuch.sd7       0.045289            131
seed7/prg/fib.sd7            0.044387             47
seed7/prg/find7.sd7          0.042466            133
seed7/prg/findchar.sd7       0.044233            149
seed7/prg/fractree.sd7       0.037733             55
seed7/prg/ftp7.sd7           0.054035            296
seed7/prg/ftpserv.sd7        0.039749             74
seed7/prg/gcd.sd7            0.041220            109
seed7/prg/gkbd.sd7           0.061043            358
seed7/prg/gtksvtst.sd7       0.039189             94
seed7/prg/hal.sd7            0.046616            250
seed7/prg/hamu.sd7           0.066398            573
seed7/prg/hanoi.sd7          0.039101             55
seed7/prg/hd.sd7             0.039544             79
seed7/prg/hello.sd7          0.036521             32
seed7/prg/hilbert.sd7        0.041215            108
seed7/prg/ide7.sd7           0.048051            196
seed7/prg/kbd.sd7            0.037312             49
seed7/prg/klondike.sd7       0.087184            883
seed7/prg/lander.sd7         0.131605           1551
seed7/prg/lst80bas.sd7       0.055280            344
seed7/prg/lst99bas.sd7       0.058888            401
seed7/prg/lstgwbas.sd7       0.072729            577
seed7/prg/mahjong.sd7        0.147289           1943
seed7/prg/make7.sd7          0.042041            121
seed7/prg/mandelbr.sd7       0.048935            237
seed7/prg/mind.sd7           0.058489            443
seed7/prg/mirror.sd7         0.045946            131
seed7/prg/ms.sd7             0.073537            641
seed7/prg/nicoma.sd7         0.044644            135
seed7/prg/pac.sd7            0.071628            726
seed7/prg/pairs.sd7          0.137794           2025
seed7/prg/panic.sd7          0.190260           2634
seed7/prg/percolation.sd7    0.056363            330
seed7/prg/planets.sd7        0.137123           1486
seed7/prg/portfwd7.sd7       0.042735            139
seed7/prg/prime.sd7          0.037724             74
seed7/prg/printpi1.sd7       0.036951             56
seed7/prg/printpi2.sd7       0.036788             54
seed7/prg/printpi3.sd7       0.037436             60
seed7/prg/pv7.sd7            0.057162            337
seed7/prg/queen.sd7          0.042115            149
seed7/prg/rand.sd7           0.040729            121
seed7/prg/raytrace.sd7       0.069131            538
seed7/prg/rever.sd7          0.080565            816
seed7/prg/roman.sd7          0.036393             38
seed7/prg/s7c.sd7            0.613974           9060
seed7/prg/s7check.sd7        0.038617             68
seed7/prg/savehd7.sd7        0.110814           1110
seed7/prg/self.sd7           0.037935             49
seed7/prg/shisen.sd7         0.119765           1423
seed7/prg/sl.sd7             0.096091           1029
seed7/prg/snake.sd7          0.065571            615
seed7/prg/sokoban.sd7        0.081878            891
seed7/prg/spigotpi.sd7       0.038281             64
seed7/prg/sql7.sd7           0.052618            278
seed7/prg/startrek.sd7       0.092559            979
seed7/prg/sudoku7.sd7        0.193323           2657
seed7/prg/sydir7.sd7         0.062320            384
seed7/prg/syntaxhl.sd7       0.047816            177
seed7/prg/tak.sd7            0.037897             59
seed7/prg/tar7.sd7           0.042920            121
seed7/prg/tch.sd7            0.037964             55
seed7/prg/testfont.sd7       0.041443             95
seed7/prg/tet.sd7            0.059702            479
seed7/prg/tetg.sd7           0.062297            501
seed7/prg/toutf8.sd7         0.051063            240
seed7/prg/tst_cli.sd7        0.036010             40
seed7/prg/tst_srv.sd7        0.039560             47
seed7/prg/wator.sd7          0.078546            651
seed7/prg/which.sd7          0.037858             65
seed7/prg/wiz.sd7            0.204926           2833
seed7/prg/wordcnt.sd7        0.036725             54
seed7/prg/wrinum.sd7         0.036441             43
seed7/prg/wumpus.sd7         0.055414            372
seed7/lib/aes.s7i            0.194765           1144
seed7/lib/aes_gcm.s7i        0.061034            392
seed7/lib/ar.s7i             0.125575           1532
seed7/lib/arc4.s7i           0.043005            144
seed7/lib/archive.s7i        0.045350            143
seed7/lib/archive_base.s7i   0.047175            135
seed7/lib/array.s7i          0.074623            610
seed7/lib/asn1.s7i           0.063453            544
seed7/lib/asn1oid.s7i        0.049486            157
seed7/lib/basearray.s7i      0.062790            450
seed7/lib/bigfile.s7i        0.041220            136
seed7/lib/bigint.s7i         0.076882            824
seed7/lib/bigrat.s7i         0.078862            784
seed7/lib/bin16.s7i          0.066651            592
seed7/lib/bin32.s7i          0.060182            490
seed7/lib/bin64.s7i          0.061924            539
seed7/lib/bitdata.s7i        0.121633           1330
seed7/lib/bitmapfont.s7i     0.046958            215
seed7/lib/bitset.s7i         0.063054            593
seed7/lib/bitsetof.s7i       0.060382            431
seed7/lib/blowfish.s7i       0.077545            383
seed7/lib/bmp.s7i            0.098855            924
seed7/lib/boolean.s7i        0.054513            403
seed7/lib/browser.s7i        0.052416            280
seed7/lib/bstring.s7i        0.048244            227
seed7/lib/bytedata.s7i       0.065534            482
seed7/lib/bzip2.s7i          0.087375            887
seed7/lib/cards.s7i          0.101083           1342
seed7/lib/category.s7i       0.048543            209
seed7/lib/cc_conf.s7i        0.117400           1314
seed7/lib/ccittfax.s7i       0.099260           1022
seed7/lib/cgi.s7i            0.041108            109
seed7/lib/cgidialog.s7i      0.095458           1118
seed7/lib/char.s7i           0.050371            356
seed7/lib/charsets.s7i       0.123951           2024
seed7/lib/chartype.s7i       0.047754            121
seed7/lib/cipher.s7i         0.041256            146
seed7/lib/cli_cmds.s7i       0.111095           1360
seed7/lib/clib_file.s7i      0.050827            301
seed7/lib/color.s7i          0.045961            185
seed7/lib/complex.s7i        0.058776            464
seed7/lib/compress.s7i       0.042711            150
seed7/lib/console.s7i        0.044680            188
seed7/lib/cpio.s7i           0.141742           1708
seed7/lib/crc32.s7i          0.052645            193
seed7/lib/cronos16.s7i       0.190471           1173
seed7/lib/cronos27.s7i       0.254153           1464
seed7/lib/csv.s7i            0.047503            201
seed7/lib/db_prop.s7i        0.099513            991
seed7/lib/deflate.s7i        0.085213            740
seed7/lib/des.s7i            0.079415            444
seed7/lib/dialog.s7i         0.056829            311
seed7/lib/dir.s7i            0.043008            163
seed7/lib/draw.s7i           0.083215            854
seed7/lib/duration.s7i       0.095440           1038
seed7/lib/echo.s7i           0.042056            132
seed7/lib/editline.s7i       0.059100            398
seed7/lib/elf.s7i            0.151560           1560
seed7/lib/elliptic.s7i       0.075563            649
seed7/lib/enable_io.s7i      0.052713            312
seed7/lib/encoding.s7i       0.096619            931
seed7/lib/enumeration.s7i    0.049233            236
seed7/lib/environment.s7i    0.043515            175
seed7/lib/exif.s7i           0.045009            152
seed7/lib/external_file.s7i  0.050999            340
seed7/lib/field.s7i          0.050750            268
seed7/lib/file.s7i           0.054964            372
seed7/lib/filebits.s7i       0.037939             46
seed7/lib/filesys.s7i        0.064352            601
seed7/lib/fileutil.s7i       0.042701            144
seed7/lib/fixarray.s7i       0.052889            307
seed7/lib/float.s7i          0.072896            757
seed7/lib/font.s7i           0.044060            196
seed7/lib/font8x8.s7i        0.065847            998
seed7/lib/forloop.s7i        0.062727            449
seed7/lib/ftp.s7i            0.087640            969
seed7/lib/ftpserv.s7i        0.076497            631
seed7/lib/getf.s7i           0.042699            115
seed7/lib/gethttp.s7i        0.037562             41
seed7/lib/gethttps.s7i       0.036489             41
seed7/lib/gif.s7i            0.069380            561
seed7/lib/graph.s7i          0.063937            415
seed7/lib/graph_file.s7i     0.059169            399
seed7/lib/gtkserver.s7i      0.041190            161
seed7/lib/gzip.s7i           0.067471            573
seed7/lib/hash.s7i           0.064103            421
seed7/lib/hashsetof.s7i      0.064130            499
seed7/lib/hmac.s7i           0.043510            152
seed7/lib/html.s7i           0.037969             83
seed7/lib/html_ent.s7i       0.062570            476
seed7/lib/htmldom.s7i        0.051590            286
seed7/lib/http_request.s7i   0.076140            696
seed7/lib/http_srv_resp.s7i  0.058214            380
seed7/lib/https_request.s7i  0.047522            211
seed7/lib/httpserv.s7i       0.054840            345
seed7/lib/huffman.s7i        0.074562            644
seed7/lib/ico.s7i            0.057581            221
seed7/lib/idxarray.s7i       0.059197            232
seed7/lib/image.s7i          0.042162            156
seed7/lib/imagefile.s7i      0.044012            171
seed7/lib/inflate.s7i        0.062592            411
seed7/lib/inifile.s7i        0.042011            129
seed7/lib/integer.s7i        0.067093            663
seed7/lib/iobuffer.s7i       0.049993            289
seed7/lib/jpeg.s7i           0.153435           1761
seed7/lib/json.s7i           0.079251            891
seed7/lib/json_serde.s7i     0.077662            783
seed7/lib/keybd.s7i          0.078258            639
seed7/lib/keydescr.s7i       0.049240            192
seed7/lib/leb128.s7i         0.065746            218
seed7/lib/line.s7i           0.050535            164
seed7/lib/listener.s7i       0.050210            247
seed7/lib/logfile.s7i        0.039620             73
seed7/lib/lower.s7i          0.042177            142
seed7/lib/lzma.s7i           0.106792            934
seed7/lib/lzw.s7i            0.088251            861
seed7/lib/magic.s7i          0.063678            403
seed7/lib/mahjng32.s7i       0.092432           1500
seed7/lib/make.s7i           0.068938            544
seed7/lib/makedata.s7i       0.120896           1428
seed7/lib/math.s7i           0.045164            201
seed7/lib/mixarith.s7i       0.046599            249
seed7/lib/modern27.s7i       0.168401           1099
seed7/lib/more.s7i           0.041915            130
seed7/lib/msgdigest.s7i      0.136405           1222
seed7/lib/multiscr.s7i       0.039325             68
seed7/lib/null_file.s7i      0.051238            345
seed7/lib/osfiles.s7i        0.094658           1085
seed7/lib/pbm.s7i            0.048478            230
seed7/lib/pcx.s7i            0.076947            638
seed7/lib/pem.s7i            0.044900            185
seed7/lib/pgm.s7i            0.048688            238
seed7/lib/pic16.s7i          0.066669           1037
seed7/lib/pic32.s7i          0.121261           2060
seed7/lib/pic_util.s7i       0.043914            144
seed7/lib/pixelimage.s7i     0.052745            320
seed7/lib/pixmap_file.s7i    0.062157            459
seed7/lib/pixmapfont.s7i     0.046750            184
seed7/lib/pkcs1.s7i          0.075587            543
seed7/lib/png.s7i            0.105412           1064
seed7/lib/poll.s7i           0.050894            313
seed7/lib/ppm.s7i            0.049881            240
seed7/lib/process.s7i        0.064272            541
seed7/lib/progs.s7i          0.079283            789
seed7/lib/propertyfile.s7i   0.044501            155
seed7/lib/rational.s7i       0.078746            792
seed7/lib/ref_list.s7i       0.048813            252
seed7/lib/reference.s7i      0.042176            126
seed7/lib/reverse.s7i        0.039444             94
seed7/lib/rpm.s7i            0.284843           3487
seed7/lib/rpmext.s7i         0.052847            318
seed7/lib/scanfile.s7i       0.131237           1779
seed7/lib/scanjson.s7i       0.059470            413
seed7/lib/scanstri.s7i       0.135627           1814
seed7/lib/scantoml.s7i       0.130556           1603
seed7/lib/seed7_05.s7i       0.110852           1072
seed7/lib/set.s7i            0.038363             57
seed7/lib/shell.s7i          0.068785            615
seed7/lib/showtls.s7i        0.085161            678
seed7/lib/signature.s7i      0.043009            131
seed7/lib/smtp.s7i           0.048649            261
seed7/lib/sockbase.s7i       0.050258            217
seed7/lib/socket.s7i         0.052701            326
seed7/lib/sokoban1.s7i       0.080815           1519
seed7/lib/sql_base.s7i       0.093206           1000
seed7/lib/stars.s7i          0.231514           1705
seed7/lib/stdfont10.s7i      0.143193           3347
seed7/lib/stdfont12.s7i      0.164822           3928
seed7/lib/stdfont14.s7i      0.187831           4510
seed7/lib/stdfont16.s7i      0.211072           5092
seed7/lib/stdfont18.s7i      0.242551           5868
seed7/lib/stdfont20.s7i      0.270513           6449
seed7/lib/stdfont24.s7i      0.329902           7421
seed7/lib/stdfont8.s7i       0.126798           2960
seed7/lib/stdfont9.s7i       0.133192           3152
seed7/lib/stdio.s7i          0.044616            192
seed7/lib/strifile.s7i       0.052832            345
seed7/lib/string.s7i         0.075104            779
seed7/lib/stritext.s7i       0.053078            352
seed7/lib/struct.s7i         0.053289            266
seed7/lib/struct_elem.s7i    0.042227            129
seed7/lib/subfile.s7i        0.043928            174
seed7/lib/subrange.s7i       0.039424             78
seed7/lib/syntax.s7i         0.060524            294
seed7/lib/tar.s7i            0.145160           1880
seed7/lib/tar_cmds.s7i       0.085043            752
seed7/lib/tdes.s7i           0.044124            143
seed7/lib/tee.s7i            0.042451            143
seed7/lib/text.s7i           0.042778            135
seed7/lib/tga.s7i            0.080166            676
seed7/lib/tiff.s7i           0.242590           2771
seed7/lib/time.s7i           0.099873           1191
seed7/lib/tls.s7i            0.193961           2230
seed7/lib/unicode.s7i        0.073624            575
seed7/lib/unionfnd.s7i       0.043079            130
seed7/lib/upper.s7i          0.042040            142
seed7/lib/utf16.s7i          0.065467            540
seed7/lib/utf8.s7i           0.047773            234
seed7/lib/vecfont10.s7i      0.159408           1056
seed7/lib/vecfont18.s7i      0.177591           1119
seed7/lib/vector3d.s7i       0.050203            293
seed7/lib/vectorfont.s7i     0.048896            239
seed7/lib/wildcard.s7i       0.043074            140
seed7/lib/window.s7i         0.058933            455
seed7/lib/wrinum.s7i         0.047984            248
seed7/lib/x509cert.s7i       0.115833           1243
seed7/lib/xml_ent.s7i        0.040008             94
seed7/lib/xmldom.s7i         0.052229            303
seed7/lib/xz.s7i             0.062992            442
seed7/lib/zip.s7i            0.230941           2792
seed7/lib/zstd.s7i           0.117486           1263
============================ ================= ================

Statistical Summary — Mode D (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.157734        |
+-----------+-----------------+
| Minimum   | 0.036010        |
+-----------+-----------------+
| Maximum   | 5.534053        |
+-----------+-----------------+

Cross-Mode Comparison
=====================

+------+-----------------+-----------------+-----------------+
| Mode | Average (s)     | Minimum (s)     | Maximum (s)     |
+======+=================+=================+=================+
| A    | 0.033834        | 0.032413        | 0.040304        |
+------+-----------------+-----------------+-----------------+
| B    | 0.039011        | 0.035564        | 0.046597        |
+------+-----------------+-----------------+-----------------+
| C    | 0.088926        | 0.034377        | 2.502998        |
+------+-----------------+-----------------+-----------------+
| D    | 0.157734        | 0.036010        | 5.534053        |
+------+-----------------+-----------------+-----------------+

Phase Timing Summary
====================

Wall-clock times include warm-up work and GC performed by the warm-up helper.

+----------+--------------+--------------+--------------+
| Phase    | Warm-up      | Timed pass   | Mode total   |
+==========+==============+==============+==============+
| Mode A   | 00:00:12.291 | 00:00:57.635 | 00:01:09.927 |
+----------+--------------+--------------+--------------+
| Mode B   | 00:00:14.987 | 00:01:06.523 | 00:01:21.510 |
+----------+--------------+--------------+--------------+
| Mode C   | 00:00:35.859 | 00:02:32.383 | 00:03:08.243 |
+----------+--------------+--------------+--------------+
| Mode D   | 00:01:00.881 | 00:04:29.602 | 00:05:30.483 |
+----------+--------------+--------------+--------------+
| Total    |              |              | 00:11:10.171 |
+----------+--------------+--------------+--------------+


.. ---------------------------------------------------------------------------
