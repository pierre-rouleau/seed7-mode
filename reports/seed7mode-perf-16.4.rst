=======================================================
GC-Controlled Benchmark Report: Seed7 Mode — Four Modes
=======================================================

:Running with: seed7-mode 2026-06-30T15:25:53+0000 W27-2
:Seed7 dir   : /Users/roup/my/dvo/seed7-repos/seed7/
:Started at: 11:27:47 local time
:Generated on: 2026-06-30 15:39:14 UTC
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
seed7/prg/addup.sd7          0.037354            190
seed7/prg/bas7.sd7           0.037081          11459
seed7/prg/bifurk.sd7         0.034604             73
seed7/prg/bigfiles.sd7       0.033830            129
seed7/prg/brainf7.sd7        0.033348             86
seed7/prg/calc7.sd7          0.033274            128
seed7/prg/carddemo.sd7       0.033294            190
seed7/prg/castle.sd7         0.033296           3148
seed7/prg/cat.sd7            0.033745             82
seed7/prg/cellauto.sd7       0.033576             85
seed7/prg/celsius.sd7        0.033280             42
seed7/prg/chk_all.sd7        0.033644            843
seed7/prg/chkarr.sd7         0.034205           8367
seed7/prg/chkbig.sd7         0.037696          29026
seed7/prg/chkbin.sd7         0.034424           6469
seed7/prg/chkbitdata.sd7     0.034262           6624
seed7/prg/chkbool.sd7        0.033609           3157
seed7/prg/chkbst.sd7         0.033462            722
seed7/prg/chkchr.sd7         0.033989           2809
seed7/prg/chkcmd.sd7         0.033787           1205
seed7/prg/chkdb.sd7          0.034504           7454
seed7/prg/chkdecl.sd7        0.033390            448
seed7/prg/chkenum.sd7        0.033594           1230
seed7/prg/chkerr.sd7         0.034435           4663
seed7/prg/chkexc.sd7         0.033205           2627
seed7/prg/chkfil.sd7         0.034497           1615
seed7/prg/chkflt.sd7         0.036662          20620
seed7/prg/chkhent.sd7        0.032648             54
seed7/prg/chkhsh.sd7         0.032674           4548
seed7/prg/chkidx.sd7         0.035104          19567
seed7/prg/chkint.sd7         0.039313          38129
seed7/prg/chkjson.sd7        0.033199           1764
seed7/prg/chkovf.sd7         0.034033           8216
seed7/prg/chkprc.sd7         0.035937          10111
seed7/prg/chkscan.sd7        0.034243            714
seed7/prg/chkset.sd7         0.034879          11974
seed7/prg/chkstr.sd7         0.037597          26952
seed7/prg/chktime.sd7        0.033752           2025
seed7/prg/chktoml.sd7        0.033747           1656
seed7/prg/clock.sd7          0.033312             47
seed7/prg/clock2.sd7         0.033349             43
seed7/prg/clock3.sd7         0.033382             95
seed7/prg/cmpfil.sd7         0.033554             84
seed7/prg/comanche.sd7       0.033247            180
seed7/prg/confval.sd7        0.033211            175
seed7/prg/db7.sd7            0.033260            417
seed7/prg/diff7.sd7          0.033310            263
seed7/prg/dirtst.sd7         0.033416             42
seed7/prg/dirx.sd7           0.033291            152
seed7/prg/dnafight.sd7       0.034037           1381
seed7/prg/dragon.sd7         0.033551             73
seed7/prg/echo.sd7           0.033814             39
seed7/prg/eliza.sd7          0.033548            302
seed7/prg/err.sd7            0.033341             96
seed7/prg/fannkuch.sd7       0.033120            131
seed7/prg/fib.sd7            0.033224             47
seed7/prg/find7.sd7          0.033418            133
seed7/prg/findchar.sd7       0.033515            149
seed7/prg/fractree.sd7       0.032862             55
seed7/prg/ftp7.sd7           0.032588            296
seed7/prg/ftpserv.sd7        0.032446             74
seed7/prg/gcd.sd7            0.033239            109
seed7/prg/gkbd.sd7           0.032610            358
seed7/prg/gtksvtst.sd7       0.032777             94
seed7/prg/hal.sd7            0.033763            250
seed7/prg/hamu.sd7           0.034529            573
seed7/prg/hanoi.sd7          0.033300             55
seed7/prg/hd.sd7             0.032954             79
seed7/prg/hello.sd7          0.032838             32
seed7/prg/hilbert.sd7        0.032721            108
seed7/prg/ide7.sd7           0.032611            196
seed7/prg/kbd.sd7            0.032418             49
seed7/prg/klondike.sd7       0.032551            883
seed7/prg/lander.sd7         0.032658           1551
seed7/prg/lst80bas.sd7       0.032557            344
seed7/prg/lst99bas.sd7       0.032638            401
seed7/prg/lstgwbas.sd7       0.032947            577
seed7/prg/mahjong.sd7        0.033589           1943
seed7/prg/make7.sd7          0.033460            121
seed7/prg/mandelbr.sd7       0.033547            237
seed7/prg/mind.sd7           0.033360            443
seed7/prg/mirror.sd7         0.033912            131
seed7/prg/ms.sd7             0.033774            641
seed7/prg/nicoma.sd7         0.033441            135
seed7/prg/pac.sd7            0.035572            726
seed7/prg/pairs.sd7          0.037250           2025
seed7/prg/panic.sd7          0.033725           2634
seed7/prg/percolation.sd7    0.033855            330
seed7/prg/planets.sd7        0.033705           1486
seed7/prg/portfwd7.sd7       0.033643            139
seed7/prg/prime.sd7          0.032905             74
seed7/prg/printpi1.sd7       0.032628             56
seed7/prg/printpi2.sd7       0.032373             54
seed7/prg/printpi3.sd7       0.032526             60
seed7/prg/pv7.sd7            0.032668            337
seed7/prg/queen.sd7          0.032426            149
seed7/prg/rand.sd7           0.033450            121
seed7/prg/raytrace.sd7       0.034290            538
seed7/prg/rever.sd7          0.034286            816
seed7/prg/roman.sd7          0.032867             38
seed7/prg/s7c.sd7            0.033337           9060
seed7/prg/s7check.sd7        0.032699             68
seed7/prg/savehd7.sd7        0.032412           1110
seed7/prg/self.sd7           0.032197             49
seed7/prg/shisen.sd7         0.033442           1423
seed7/prg/sl.sd7             0.033806           1029
seed7/prg/snake.sd7          0.033430            615
seed7/prg/sokoban.sd7        0.033463            891
seed7/prg/spigotpi.sd7       0.033391             64
seed7/prg/sql7.sd7           0.033302            278
seed7/prg/startrek.sd7       0.033438            979
seed7/prg/sudoku7.sd7        0.033676           2657
seed7/prg/sydir7.sd7         0.033249            384
seed7/prg/syntaxhl.sd7       0.034121            177
seed7/prg/tak.sd7            0.033926             59
seed7/prg/tar7.sd7           0.033713            121
seed7/prg/tch.sd7            0.037301             55
seed7/prg/testfont.sd7       0.039572             95
seed7/prg/tet.sd7            0.035808            479
seed7/prg/tetg.sd7           0.035461            501
seed7/prg/toutf8.sd7         0.035262            240
seed7/prg/tst_cli.sd7        0.034240             40
seed7/prg/tst_srv.sd7        0.032988             47
seed7/prg/wator.sd7          0.032482            651
seed7/prg/which.sd7          0.033216             65
seed7/prg/wiz.sd7            0.032815           2833
seed7/prg/wordcnt.sd7        0.032379             54
seed7/prg/wrinum.sd7         0.033883             43
seed7/prg/wumpus.sd7         0.034158            372
seed7/lib/aes.s7i            0.037850           1144
seed7/lib/aes_gcm.s7i        0.037326            392
seed7/lib/ar.s7i             0.034992           1532
seed7/lib/arc4.s7i           0.033633            144
seed7/lib/archive.s7i        0.033724            143
seed7/lib/archive_base.s7i   0.033477            135
seed7/lib/array.s7i          0.033942            610
seed7/lib/asn1.s7i           0.034360            544
seed7/lib/asn1oid.s7i        0.033405            157
seed7/lib/basearray.s7i      0.033554            450
seed7/lib/bigfile.s7i        0.033558            136
seed7/lib/bigint.s7i         0.033543            824
seed7/lib/bigrat.s7i         0.033251            784
seed7/lib/bin16.s7i          0.033688            592
seed7/lib/bin32.s7i          0.033745            490
seed7/lib/bin64.s7i          0.033524            539
seed7/lib/bitdata.s7i        0.033620           1330
seed7/lib/bitmapfont.s7i     0.034349            215
seed7/lib/bitset.s7i         0.033685            593
seed7/lib/bitsetof.s7i       0.034237            431
seed7/lib/blowfish.s7i       0.033560            383
seed7/lib/bmp.s7i            0.033571            924
seed7/lib/boolean.s7i        0.033488            403
seed7/lib/browser.s7i        0.033975            280
seed7/lib/bstring.s7i        0.033504            227
seed7/lib/bytedata.s7i       0.032695            482
seed7/lib/bzip2.s7i          0.032718            887
seed7/lib/cards.s7i          0.032818           1342
seed7/lib/category.s7i       0.033412            209
seed7/lib/cc_conf.s7i        0.036591           1314
seed7/lib/ccittfax.s7i       0.038271           1022
seed7/lib/cgi.s7i            0.036475            109
seed7/lib/cgidialog.s7i      0.034829           1118
seed7/lib/char.s7i           0.033818            356
seed7/lib/charsets.s7i       0.033703           2024
seed7/lib/chartype.s7i       0.033566            121
seed7/lib/cipher.s7i         0.033943            146
seed7/lib/cli_cmds.s7i       0.033408           1360
seed7/lib/clib_file.s7i      0.033481            301
seed7/lib/color.s7i          0.033690            185
seed7/lib/complex.s7i        0.033394            464
seed7/lib/compress.s7i       0.033728            150
seed7/lib/console.s7i        0.033627            188
seed7/lib/cpio.s7i           0.033794           1708
seed7/lib/crc32.s7i          0.033556            193
seed7/lib/cronos16.s7i       0.033594           1173
seed7/lib/cronos27.s7i       0.033601           1464
seed7/lib/csv.s7i            0.034287            201
seed7/lib/db_prop.s7i        0.035806            991
seed7/lib/deflate.s7i        0.039198            740
seed7/lib/des.s7i            0.034246            444
seed7/lib/dialog.s7i         0.033699            311
seed7/lib/dir.s7i            0.033902            163
seed7/lib/draw.s7i           0.037368            854
seed7/lib/duration.s7i       0.038507           1038
seed7/lib/echo.s7i           0.033470            132
seed7/lib/editline.s7i       0.032978            398
seed7/lib/elf.s7i            0.032803           1560
seed7/lib/elliptic.s7i       0.032706            649
seed7/lib/enable_io.s7i      0.032679            312
seed7/lib/encoding.s7i       0.032485            931
seed7/lib/enumeration.s7i    0.034770            236
seed7/lib/environment.s7i    0.034437            175
seed7/lib/exif.s7i           0.033518            152
seed7/lib/external_file.s7i  0.033651            340
seed7/lib/field.s7i          0.033488            268
seed7/lib/file.s7i           0.033564            372
seed7/lib/filebits.s7i       0.034012             46
seed7/lib/filesys.s7i        0.033452            601
seed7/lib/fileutil.s7i       0.033631            144
seed7/lib/fixarray.s7i       0.033620            307
seed7/lib/float.s7i          0.033684            757
seed7/lib/font.s7i           0.033632            196
seed7/lib/font8x8.s7i        0.033827            998
seed7/lib/forloop.s7i        0.033563            449
seed7/lib/ftp.s7i            0.033350            969
seed7/lib/ftpserv.s7i        0.033736            631
seed7/lib/getf.s7i           0.033684            115
seed7/lib/gethttp.s7i        0.033554             41
seed7/lib/gethttps.s7i       0.033167             41
seed7/lib/gif.s7i            0.033486            561
seed7/lib/graph.s7i          0.033588            415
seed7/lib/graph_file.s7i     0.033791            399
seed7/lib/gtkserver.s7i      0.033254            161
seed7/lib/gzip.s7i           0.033321            573
seed7/lib/hash.s7i           0.033808            421
seed7/lib/hashsetof.s7i      0.033058            499
seed7/lib/hmac.s7i           0.032507            152
seed7/lib/html.s7i           0.032608             83
seed7/lib/html_ent.s7i       0.032754            476
seed7/lib/htmldom.s7i        0.032588            286
seed7/lib/http_request.s7i   0.032646            696
seed7/lib/http_srv_resp.s7i  0.034369            380
seed7/lib/https_request.s7i  0.033676            211
seed7/lib/httpserv.s7i       0.033575            345
seed7/lib/huffman.s7i        0.033797            644
seed7/lib/ico.s7i            0.033516            221
seed7/lib/idxarray.s7i       0.033414            232
seed7/lib/image.s7i          0.033493            156
seed7/lib/imagefile.s7i      0.033992            171
seed7/lib/inflate.s7i        0.034053            411
seed7/lib/inifile.s7i        0.033562            129
seed7/lib/integer.s7i        0.033533            663
seed7/lib/iobuffer.s7i       0.033552            289
seed7/lib/jpeg.s7i           0.033776           1761
seed7/lib/json.s7i           0.033178            891
seed7/lib/json_serde.s7i     0.033544            783
seed7/lib/keybd.s7i          0.033384            639
seed7/lib/keydescr.s7i       0.033484            192
seed7/lib/leb128.s7i         0.033961            218
seed7/lib/line.s7i           0.033606            164
seed7/lib/listener.s7i       0.033351            247
seed7/lib/logfile.s7i        0.032827             73
seed7/lib/lower.s7i          0.033968            142
seed7/lib/lzma.s7i           0.033422            934
seed7/lib/lzw.s7i            0.033814            861
seed7/lib/magic.s7i          0.032739            403
seed7/lib/mahjng32.s7i       0.033113           1500
seed7/lib/make.s7i           0.032623            544
seed7/lib/makedata.s7i       0.032549           1428
seed7/lib/math.s7i           0.032612            201
seed7/lib/mixarith.s7i       0.032284            249
seed7/lib/modern27.s7i       0.033976           1099
seed7/lib/more.s7i           0.033381            130
seed7/lib/msgdigest.s7i      0.033364           1222
seed7/lib/multiscr.s7i       0.033449             68
seed7/lib/null_file.s7i      0.033497            345
seed7/lib/osfiles.s7i        0.033465           1085
seed7/lib/pbm.s7i            0.034209            230
seed7/lib/pcx.s7i            0.033386            638
seed7/lib/pem.s7i            0.033366            185
seed7/lib/pgm.s7i            0.033601            238
seed7/lib/pic16.s7i          0.033701           1037
seed7/lib/pic32.s7i          0.033872           2060
seed7/lib/pic_util.s7i       0.033647            144
seed7/lib/pixelimage.s7i     0.033694            320
seed7/lib/pixmap_file.s7i    0.033707            459
seed7/lib/pixmapfont.s7i     0.033089            184
seed7/lib/pkcs1.s7i          0.032825            543
seed7/lib/png.s7i            0.032647           1064
seed7/lib/poll.s7i           0.033161            313
seed7/lib/ppm.s7i            0.032717            240
seed7/lib/process.s7i        0.032335            541
seed7/lib/progs.s7i          0.032935            789
seed7/lib/propertyfile.s7i   0.032699            155
seed7/lib/rational.s7i       0.032459            792
seed7/lib/ref_list.s7i       0.033280            252
seed7/lib/reference.s7i      0.032389            126
seed7/lib/reverse.s7i        0.032701             94
seed7/lib/rpm.s7i            0.033138           3487
seed7/lib/rpmext.s7i         0.032350            318
seed7/lib/scanfile.s7i       0.032771           1779
seed7/lib/scanjson.s7i       0.034032            413
seed7/lib/scanstri.s7i       0.034492           1814
seed7/lib/scantoml.s7i       0.033499           1603
seed7/lib/seed7_05.s7i       0.033389           1072
seed7/lib/set.s7i            0.033532             57
seed7/lib/shell.s7i          0.033464            615
seed7/lib/showtls.s7i        0.033519            678
seed7/lib/signature.s7i      0.033443            131
seed7/lib/smtp.s7i           0.033295            261
seed7/lib/sockbase.s7i       0.033508            217
seed7/lib/socket.s7i         0.033833            326
seed7/lib/sokoban1.s7i       0.033670           1519
seed7/lib/sql_base.s7i       0.033791           1000
seed7/lib/stars.s7i          0.034344           1705
seed7/lib/stdfont10.s7i      0.033831           3347
seed7/lib/stdfont12.s7i      0.033158           3928
seed7/lib/stdfont14.s7i      0.034521           4510
seed7/lib/stdfont16.s7i      0.033793           5092
seed7/lib/stdfont18.s7i      0.034258           5868
seed7/lib/stdfont20.s7i      0.034217           6449
seed7/lib/stdfont24.s7i      0.034098           7421
seed7/lib/stdfont8.s7i       0.034234           2960
seed7/lib/stdfont9.s7i       0.033718           3152
seed7/lib/stdio.s7i          0.033465            192
seed7/lib/strifile.s7i       0.033683            345
seed7/lib/string.s7i         0.033688            779
seed7/lib/stritext.s7i       0.032623            352
seed7/lib/struct.s7i         0.032915            266
seed7/lib/struct_elem.s7i    0.032527            129
seed7/lib/subfile.s7i        0.032557            174
seed7/lib/subrange.s7i       0.032837             78
seed7/lib/syntax.s7i         0.033535            294
seed7/lib/tar.s7i            0.034406           1880
seed7/lib/tar_cmds.s7i       0.033660            752
seed7/lib/tdes.s7i           0.033513            143
seed7/lib/tee.s7i            0.033633            143
seed7/lib/text.s7i           0.033487            135
seed7/lib/tga.s7i            0.033898            676
seed7/lib/tiff.s7i           0.033756           2771
seed7/lib/time.s7i           0.033405           1191
seed7/lib/tls.s7i            0.033806           2230
seed7/lib/unicode.s7i        0.033553            575
seed7/lib/unionfnd.s7i       0.033596            130
seed7/lib/upper.s7i          0.033450            142
seed7/lib/utf16.s7i          0.033527            540
seed7/lib/utf8.s7i           0.033553            234
seed7/lib/vecfont10.s7i      0.033341           1056
seed7/lib/vecfont18.s7i      0.033436           1119
seed7/lib/vector3d.s7i       0.033505            293
seed7/lib/vectorfont.s7i     0.033833            239
seed7/lib/wildcard.s7i       0.032908            140
seed7/lib/window.s7i         0.033448            455
seed7/lib/wrinum.s7i         0.033300            248
seed7/lib/x509cert.s7i       0.033748           1243
seed7/lib/xml_ent.s7i        0.033608             94
seed7/lib/xmldom.s7i         0.033512            303
seed7/lib/xz.s7i             0.033636            442
seed7/lib/zip.s7i            0.033438           2792
seed7/lib/zstd.s7i           0.032813           1263
============================ ================= ================

Statistical Summary — Mode A (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.033710        |
+-----------+-----------------+
| Minimum   | 0.032197        |
+-----------+-----------------+
| Maximum   | 0.039572        |
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
seed7/prg/addup.sd7          0.039235            190
seed7/prg/bas7.sd7           0.039988          11459
seed7/prg/bifurk.sd7         0.036120             73
seed7/prg/bigfiles.sd7       0.037583            129
seed7/prg/brainf7.sd7        0.037286             86
seed7/prg/calc7.sd7          0.037202            128
seed7/prg/carddemo.sd7       0.039313            190
seed7/prg/castle.sd7         0.039809           3148
seed7/prg/cat.sd7            0.037116             82
seed7/prg/cellauto.sd7       0.036976             85
seed7/prg/celsius.sd7        0.036579             42
seed7/prg/chk_all.sd7        0.039247            843
seed7/prg/chkarr.sd7         0.039374           8367
seed7/prg/chkbig.sd7         0.044373          29026
seed7/prg/chkbin.sd7         0.040942           6469
seed7/prg/chkbitdata.sd7     0.040353           6624
seed7/prg/chkbool.sd7        0.039160           3157
seed7/prg/chkbst.sd7         0.039132            722
seed7/prg/chkchr.sd7         0.040544           2809
seed7/prg/chkcmd.sd7         0.038681           1205
seed7/prg/chkdb.sd7          0.039848           7454
seed7/prg/chkdecl.sd7        0.040797            448
seed7/prg/chkenum.sd7        0.043655           1230
seed7/prg/chkerr.sd7         0.045713           4663
seed7/prg/chkexc.sd7         0.042197           2627
seed7/prg/chkfil.sd7         0.039770           1615
seed7/prg/chkflt.sd7         0.043506          20620
seed7/prg/chkhent.sd7        0.036487             54
seed7/prg/chkhsh.sd7         0.039679           4548
seed7/prg/chkidx.sd7         0.046867          19567
seed7/prg/chkint.sd7         0.048396          38129
seed7/prg/chkjson.sd7        0.038315           1764
seed7/prg/chkovf.sd7         0.040833           8216
seed7/prg/chkprc.sd7         0.042741          10111
seed7/prg/chkscan.sd7        0.039572            714
seed7/prg/chkset.sd7         0.040740          11974
seed7/prg/chkstr.sd7         0.043878          26952
seed7/prg/chktime.sd7        0.040314           2025
seed7/prg/chktoml.sd7        0.039826           1656
seed7/prg/clock.sd7          0.037297             47
seed7/prg/clock2.sd7         0.035888             43
seed7/prg/clock3.sd7         0.039239             95
seed7/prg/cmpfil.sd7         0.037595             84
seed7/prg/comanche.sd7       0.039119            180
seed7/prg/confval.sd7        0.040230            175
seed7/prg/db7.sd7            0.039120            417
seed7/prg/diff7.sd7          0.039207            263
seed7/prg/dirtst.sd7         0.035950             42
seed7/prg/dirx.sd7           0.039560            152
seed7/prg/dnafight.sd7       0.039993           1381
seed7/prg/dragon.sd7         0.036938             73
seed7/prg/echo.sd7           0.036481             39
seed7/prg/eliza.sd7          0.038645            302
seed7/prg/err.sd7            0.041748             96
seed7/prg/fannkuch.sd7       0.038697            131
seed7/prg/fib.sd7            0.035342             47
seed7/prg/find7.sd7          0.037554            133
seed7/prg/findchar.sd7       0.037649            149
seed7/prg/fractree.sd7       0.035435             55
seed7/prg/ftp7.sd7           0.037880            296
seed7/prg/ftpserv.sd7        0.038542             74
seed7/prg/gcd.sd7            0.037503            109
seed7/prg/gkbd.sd7           0.040748            358
seed7/prg/gtksvtst.sd7       0.037451             94
seed7/prg/hal.sd7            0.038601            250
seed7/prg/hamu.sd7           0.038547            573
seed7/prg/hanoi.sd7          0.036070             55
seed7/prg/hd.sd7             0.037535             79
seed7/prg/hello.sd7          0.035610             32
seed7/prg/hilbert.sd7        0.037930            108
seed7/prg/ide7.sd7           0.040505            196
seed7/prg/kbd.sd7            0.037120             49
seed7/prg/klondike.sd7       0.038612            883
seed7/prg/lander.sd7         0.039084           1551
seed7/prg/lst80bas.sd7       0.037990            344
seed7/prg/lst99bas.sd7       0.039698            401
seed7/prg/lstgwbas.sd7       0.040379            577
seed7/prg/mahjong.sd7        0.039291           1943
seed7/prg/make7.sd7          0.039249            121
seed7/prg/mandelbr.sd7       0.038955            237
seed7/prg/mind.sd7           0.038967            443
seed7/prg/mirror.sd7         0.041959            131
seed7/prg/ms.sd7             0.042140            641
seed7/prg/nicoma.sd7         0.041621            135
seed7/prg/pac.sd7            0.040943            726
seed7/prg/pairs.sd7          0.041414           2025
seed7/prg/panic.sd7          0.041411           2634
seed7/prg/percolation.sd7    0.041073            330
seed7/prg/planets.sd7        0.039957           1486
seed7/prg/portfwd7.sd7       0.040827            139
seed7/prg/prime.sd7          0.037964             74
seed7/prg/printpi1.sd7       0.036796             56
seed7/prg/printpi2.sd7       0.036398             54
seed7/prg/printpi3.sd7       0.036612             60
seed7/prg/pv7.sd7            0.040522            337
seed7/prg/queen.sd7          0.040176            149
seed7/prg/rand.sd7           0.039450            121
seed7/prg/raytrace.sd7       0.039461            538
seed7/prg/rever.sd7          0.039035            816
seed7/prg/roman.sd7          0.037153             38
seed7/prg/s7c.sd7            0.038957           9060
seed7/prg/s7check.sd7        0.036879             68
seed7/prg/savehd7.sd7        0.042324           1110
seed7/prg/self.sd7           0.039101             49
seed7/prg/shisen.sd7         0.039788           1423
seed7/prg/sl.sd7             0.038845           1029
seed7/prg/snake.sd7          0.039059            615
seed7/prg/sokoban.sd7        0.038619            891
seed7/prg/spigotpi.sd7       0.036882             64
seed7/prg/sql7.sd7           0.038385            278
seed7/prg/startrek.sd7       0.038652            979
seed7/prg/sudoku7.sd7        0.038594           2657
seed7/prg/sydir7.sd7         0.038010            384
seed7/prg/syntaxhl.sd7       0.040244            177
seed7/prg/tak.sd7            0.037170             59
seed7/prg/tar7.sd7           0.039478            121
seed7/prg/tch.sd7            0.036933             55
seed7/prg/testfont.sd7       0.038869             95
seed7/prg/tet.sd7            0.039121            479
seed7/prg/tetg.sd7           0.038738            501
seed7/prg/toutf8.sd7         0.040598            240
seed7/prg/tst_cli.sd7        0.036208             40
seed7/prg/tst_srv.sd7        0.036213             47
seed7/prg/wator.sd7          0.039095            651
seed7/prg/which.sd7          0.036637             65
seed7/prg/wiz.sd7            0.038959           2833
seed7/prg/wordcnt.sd7        0.036670             54
seed7/prg/wrinum.sd7         0.036030             43
seed7/prg/wumpus.sd7         0.038757            372
seed7/lib/aes.s7i            0.042366           1144
seed7/lib/aes_gcm.s7i        0.039158            392
seed7/lib/ar.s7i             0.039145           1532
seed7/lib/arc4.s7i           0.039129            144
seed7/lib/archive.s7i        0.038806            143
seed7/lib/archive_base.s7i   0.039042            135
seed7/lib/array.s7i          0.038829            610
seed7/lib/asn1.s7i           0.037569            544
seed7/lib/asn1oid.s7i        0.041998            157
seed7/lib/basearray.s7i      0.039048            450
seed7/lib/bigfile.s7i        0.037665            136
seed7/lib/bigint.s7i         0.037877            824
seed7/lib/bigrat.s7i         0.037960            784
seed7/lib/bin16.s7i          0.041202            592
seed7/lib/bin32.s7i          0.039721            490
seed7/lib/bin64.s7i          0.038638            539
seed7/lib/bitdata.s7i        0.043861           1330
seed7/lib/bitmapfont.s7i     0.039220            215
seed7/lib/bitset.s7i         0.039034            593
seed7/lib/bitsetof.s7i       0.040363            431
seed7/lib/blowfish.s7i       0.043726            383
seed7/lib/bmp.s7i            0.039730            924
seed7/lib/boolean.s7i        0.038960            403
seed7/lib/browser.s7i        0.039340            280
seed7/lib/bstring.s7i        0.039108            227
seed7/lib/bytedata.s7i       0.039275            482
seed7/lib/bzip2.s7i          0.039358            887
seed7/lib/cards.s7i          0.037273           1342
seed7/lib/category.s7i       0.039011            209
seed7/lib/cc_conf.s7i        0.038552           1314
seed7/lib/ccittfax.s7i       0.039488           1022
seed7/lib/cgi.s7i            0.038775            109
seed7/lib/cgidialog.s7i      0.039216           1118
seed7/lib/char.s7i           0.039560            356
seed7/lib/charsets.s7i       0.039416           2024
seed7/lib/chartype.s7i       0.040803            121
seed7/lib/cipher.s7i         0.037732            146
seed7/lib/cli_cmds.s7i       0.037886           1360
seed7/lib/clib_file.s7i      0.037855            301
seed7/lib/color.s7i          0.038270            185
seed7/lib/complex.s7i        0.040314            464
seed7/lib/compress.s7i       0.039374            150
seed7/lib/console.s7i        0.038549            188
seed7/lib/cpio.s7i           0.039474           1708
seed7/lib/crc32.s7i          0.039321            193
seed7/lib/cronos16.s7i       0.042424           1173
seed7/lib/cronos27.s7i       0.042690           1464
seed7/lib/csv.s7i            0.039199            201
seed7/lib/db_prop.s7i        0.039195            991
seed7/lib/deflate.s7i        0.039774            740
seed7/lib/des.s7i            0.039898            444
seed7/lib/dialog.s7i         0.039180            311
seed7/lib/dir.s7i            0.039124            163
seed7/lib/draw.s7i           0.038753            854
seed7/lib/duration.s7i       0.038764           1038
seed7/lib/echo.s7i           0.038630            132
seed7/lib/editline.s7i       0.039597            398
seed7/lib/elf.s7i            0.044204           1560
seed7/lib/elliptic.s7i       0.039433            649
seed7/lib/enable_io.s7i      0.038699            312
seed7/lib/encoding.s7i       0.039342            931
seed7/lib/enumeration.s7i    0.039027            236
seed7/lib/environment.s7i    0.038079            175
seed7/lib/exif.s7i           0.039214            152
seed7/lib/external_file.s7i  0.037398            340
seed7/lib/field.s7i          0.037488            268
seed7/lib/file.s7i           0.038189            372
seed7/lib/filebits.s7i       0.037198             46
seed7/lib/filesys.s7i        0.038823            601
seed7/lib/fileutil.s7i       0.037873            144
seed7/lib/fixarray.s7i       0.039122            307
seed7/lib/float.s7i          0.037681            757
seed7/lib/font.s7i           0.037512            196
seed7/lib/font8x8.s7i        0.037295            998
seed7/lib/forloop.s7i        0.040009            449
seed7/lib/ftp.s7i            0.038286            969
seed7/lib/ftpserv.s7i        0.038344            631
seed7/lib/getf.s7i           0.037687            115
seed7/lib/gethttp.s7i        0.035902             41
seed7/lib/gethttps.s7i       0.036151             41
seed7/lib/gif.s7i            0.038795            561
seed7/lib/graph.s7i          0.040501            415
seed7/lib/graph_file.s7i     0.038891            399
seed7/lib/gtkserver.s7i      0.038743            161
seed7/lib/gzip.s7i           0.039263            573
seed7/lib/hash.s7i           0.040806            421
seed7/lib/hashsetof.s7i      0.040571            499
seed7/lib/hmac.s7i           0.041152            152
seed7/lib/html.s7i           0.043526             83
seed7/lib/html_ent.s7i       0.043060            476
seed7/lib/htmldom.s7i        0.038027            286
seed7/lib/http_request.s7i   0.038065            696
seed7/lib/http_srv_resp.s7i  0.038150            380
seed7/lib/https_request.s7i  0.037925            211
seed7/lib/httpserv.s7i       0.038104            345
seed7/lib/huffman.s7i        0.039618            644
seed7/lib/ico.s7i            0.039454            221
seed7/lib/idxarray.s7i       0.038918            232
seed7/lib/image.s7i          0.038421            156
seed7/lib/imagefile.s7i      0.038875            171
seed7/lib/inflate.s7i        0.039446            411
seed7/lib/inifile.s7i        0.038958            129
seed7/lib/integer.s7i        0.039070            663
seed7/lib/iobuffer.s7i       0.039379            289
seed7/lib/jpeg.s7i           0.039585           1761
seed7/lib/json.s7i           0.038560            891
seed7/lib/json_serde.s7i     0.038977            783
seed7/lib/keybd.s7i          0.038600            639
seed7/lib/keydescr.s7i       0.040210            192
seed7/lib/leb128.s7i         0.039212            218
seed7/lib/line.s7i           0.038879            164
seed7/lib/listener.s7i       0.038083            247
seed7/lib/logfile.s7i        0.037391             73
seed7/lib/lower.s7i          0.037617            142
seed7/lib/lzma.s7i           0.044040            934
seed7/lib/lzw.s7i            0.038659            861
seed7/lib/magic.s7i          0.039312            403
seed7/lib/mahjng32.s7i       0.037329           1500
seed7/lib/make.s7i           0.038137            544
seed7/lib/makedata.s7i       0.038383           1428
seed7/lib/math.s7i           0.038198            201
seed7/lib/mixarith.s7i       0.038074            249
seed7/lib/modern27.s7i       0.042478           1099
seed7/lib/more.s7i           0.039114            130
seed7/lib/msgdigest.s7i      0.039930           1222
seed7/lib/multiscr.s7i       0.037561             68
seed7/lib/null_file.s7i      0.038650            345
seed7/lib/osfiles.s7i        0.039868           1085
seed7/lib/pbm.s7i            0.039380            230
seed7/lib/pcx.s7i            0.039654            638
seed7/lib/pem.s7i            0.038859            185
seed7/lib/pgm.s7i            0.039550            238
seed7/lib/pic16.s7i          0.038749           1037
seed7/lib/pic32.s7i          0.038416           2060
seed7/lib/pic_util.s7i       0.039201            144
seed7/lib/pixelimage.s7i     0.038940            320
seed7/lib/pixmap_file.s7i    0.039154            459
seed7/lib/pixmapfont.s7i     0.040653            184
seed7/lib/pkcs1.s7i          0.044676            543
seed7/lib/png.s7i            0.039961           1064
seed7/lib/poll.s7i           0.038821            313
seed7/lib/ppm.s7i            0.039391            240
seed7/lib/process.s7i        0.039244            541
seed7/lib/progs.s7i          0.039574            789
seed7/lib/propertyfile.s7i   0.038304            155
seed7/lib/rational.s7i       0.038058            792
seed7/lib/ref_list.s7i       0.037840            252
seed7/lib/reference.s7i      0.037987            126
seed7/lib/reverse.s7i        0.036934             94
seed7/lib/rpm.s7i            0.040491           3487
seed7/lib/rpmext.s7i         0.039489            318
seed7/lib/scanfile.s7i       0.039350           1779
seed7/lib/scanjson.s7i       0.039383            413
seed7/lib/scanstri.s7i       0.039154           1814
seed7/lib/scantoml.s7i       0.039274           1603
seed7/lib/seed7_05.s7i       0.041226           1072
seed7/lib/set.s7i            0.037493             57
seed7/lib/shell.s7i          0.039609            615
seed7/lib/showtls.s7i        0.039068            678
seed7/lib/signature.s7i      0.038598            131
seed7/lib/smtp.s7i           0.038765            261
seed7/lib/sockbase.s7i       0.039756            217
seed7/lib/socket.s7i         0.039299            326
seed7/lib/sokoban1.s7i       0.038729           1519
seed7/lib/sql_base.s7i       0.038930           1000
seed7/lib/stars.s7i          0.041692           1705
seed7/lib/stdfont10.s7i      0.037632           3347
seed7/lib/stdfont12.s7i      0.039257           3928
seed7/lib/stdfont14.s7i      0.038941           4510
seed7/lib/stdfont16.s7i      0.038561           5092
seed7/lib/stdfont18.s7i      0.038742           5868
seed7/lib/stdfont20.s7i      0.038785           6449
seed7/lib/stdfont24.s7i      0.037940           7421
seed7/lib/stdfont8.s7i       0.036397           2960
seed7/lib/stdfont9.s7i       0.036344           3152
seed7/lib/stdio.s7i          0.037335            192
seed7/lib/strifile.s7i       0.038595            345
seed7/lib/string.s7i         0.039673            779
seed7/lib/stritext.s7i       0.039007            352
seed7/lib/struct.s7i         0.040325            266
seed7/lib/struct_elem.s7i    0.038502            129
seed7/lib/subfile.s7i        0.039020            174
seed7/lib/subrange.s7i       0.038174             78
seed7/lib/syntax.s7i         0.039199            294
seed7/lib/tar.s7i            0.039790           1880
seed7/lib/tar_cmds.s7i       0.039141            752
seed7/lib/tdes.s7i           0.038510            143
seed7/lib/tee.s7i            0.039348            143
seed7/lib/text.s7i           0.038880            135
seed7/lib/tga.s7i            0.041089            676
seed7/lib/tiff.s7i           0.041556           2771
seed7/lib/time.s7i           0.039564           1191
seed7/lib/tls.s7i            0.038857           2230
seed7/lib/unicode.s7i        0.041715            575
seed7/lib/unionfnd.s7i       0.039323            130
seed7/lib/upper.s7i          0.038633            142
seed7/lib/utf16.s7i          0.038838            540
seed7/lib/utf8.s7i           0.039811            234
seed7/lib/vecfont10.s7i      0.040977           1056
seed7/lib/vecfont18.s7i      0.041117           1119
seed7/lib/vector3d.s7i       0.038393            293
seed7/lib/vectorfont.s7i     0.037534            239
seed7/lib/wildcard.s7i       0.037481            140
seed7/lib/window.s7i         0.039075            455
seed7/lib/wrinum.s7i         0.039486            248
seed7/lib/x509cert.s7i       0.039153           1243
seed7/lib/xml_ent.s7i        0.038720             94
seed7/lib/xmldom.s7i         0.038298            303
seed7/lib/xz.s7i             0.038927            442
seed7/lib/zip.s7i            0.039069           2792
seed7/lib/zstd.s7i           0.039814           1263
============================ ================= ================

Statistical Summary — Mode B (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.039158        |
+-----------+-----------------+
| Minimum   | 0.035342        |
+-----------+-----------------+
| Maximum   | 0.048396        |
+-----------+-----------------+

Mode C — Mode Activation + Full-Buffer Fontification (font-lock-ensure)
-----------------------------------------------------------------------

Full-buffer fontification stress test.

File Load Times — Mode C (mean, GC-free)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

============================ ================= ================
File Name                    Mean Time (s)     Line count
============================ ================= ================
seed7/prg/addup.sd7          0.041430            190
seed7/prg/bas7.sd7           0.333643          11459
seed7/prg/bifurk.sd7         0.038406             73
seed7/prg/bigfiles.sd7       0.040161            129
seed7/prg/brainf7.sd7        0.039217             86
seed7/prg/calc7.sd7          0.040181            128
seed7/prg/carddemo.sd7       0.041130            190
seed7/prg/castle.sd7         0.112699           3148
seed7/prg/cat.sd7            0.037965             82
seed7/prg/cellauto.sd7       0.037464             85
seed7/prg/celsius.sd7        0.038348             42
seed7/prg/chk_all.sd7        0.064850            843
seed7/prg/chkarr.sd7         0.369161           8367
seed7/prg/chkbig.sd7         2.112991          29026
seed7/prg/chkbin.sd7         0.529009           6469
seed7/prg/chkbitdata.sd7     0.629600           6624
seed7/prg/chkbool.sd7        0.119415           3157
seed7/prg/chkbst.sd7         0.065248            722
seed7/prg/chkchr.sd7         0.222829           2809
seed7/prg/chkcmd.sd7         0.068212           1205
seed7/prg/chkdb.sd7          0.358636           7454
seed7/prg/chkdecl.sd7        0.058805            448
seed7/prg/chkenum.sd7        0.070156           1230
seed7/prg/chkerr.sd7         0.200294           4663
seed7/prg/chkexc.sd7         0.085168           2627
seed7/prg/chkfil.sd7         0.077486           1615
seed7/prg/chkflt.sd7         1.378060          20620
seed7/prg/chkhent.sd7        0.037519             54
seed7/prg/chkhsh.sd7         0.253926           4548
seed7/prg/chkidx.sd7         1.349764          19567
seed7/prg/chkint.sd7         2.588956          38129
seed7/prg/chkjson.sd7        0.101577           1764
seed7/prg/chkovf.sd7         0.579146           8216
seed7/prg/chkprc.sd7         0.327640          10111
seed7/prg/chkscan.sd7        0.057591            714
seed7/prg/chkset.sd7         0.680270          11974
seed7/prg/chkstr.sd7         1.421703          26952
seed7/prg/chktime.sd7        0.133340           2025
seed7/prg/chktoml.sd7        0.110725           1656
seed7/prg/clock.sd7          0.037529             47
seed7/prg/clock2.sd7         0.037491             43
seed7/prg/clock3.sd7         0.039607             95
seed7/prg/cmpfil.sd7         0.037892             84
seed7/prg/comanche.sd7       0.042725            180
seed7/prg/confval.sd7        0.044808            175
seed7/prg/db7.sd7            0.048988            417
seed7/prg/diff7.sd7          0.044497            263
seed7/prg/dirtst.sd7         0.037671             42
seed7/prg/dirx.sd7           0.040261            152
seed7/prg/dnafight.sd7       0.069929           1381
seed7/prg/dragon.sd7         0.038326             73
seed7/prg/echo.sd7           0.038250             39
seed7/prg/eliza.sd7          0.044032            302
seed7/prg/err.sd7            0.040223             96
seed7/prg/fannkuch.sd7       0.038181            131
seed7/prg/fib.sd7            0.035802             47
seed7/prg/find7.sd7          0.038939            133
seed7/prg/findchar.sd7       0.040635            149
seed7/prg/fractree.sd7       0.038082             55
seed7/prg/ftp7.sd7           0.043624            296
seed7/prg/ftpserv.sd7        0.038382             74
seed7/prg/gcd.sd7            0.038098            109
seed7/prg/gkbd.sd7           0.047620            358
seed7/prg/gtksvtst.sd7       0.038132             94
seed7/prg/hal.sd7            0.041611            250
seed7/prg/hamu.sd7           0.050435            573
seed7/prg/hanoi.sd7          0.037426             55
seed7/prg/hd.sd7             0.038433             79
seed7/prg/hello.sd7          0.037455             32
seed7/prg/hilbert.sd7        0.038653            108
seed7/prg/ide7.sd7           0.042289            196
seed7/prg/kbd.sd7            0.036898             49
seed7/prg/klondike.sd7       0.056890            883
seed7/prg/lander.sd7         0.074495           1551
seed7/prg/lst80bas.sd7       0.045920            344
seed7/prg/lst99bas.sd7       0.047210            401
seed7/prg/lstgwbas.sd7       0.052763            577
seed7/prg/mahjong.sd7        0.081340           1943
seed7/prg/make7.sd7          0.038672            121
seed7/prg/mandelbr.sd7       0.040702            237
seed7/prg/mind.sd7           0.045292            443
seed7/prg/mirror.sd7         0.040567            131
seed7/prg/ms.sd7             0.050297            641
seed7/prg/nicoma.sd7         0.039469            135
seed7/prg/pac.sd7            0.051186            726
seed7/prg/pairs.sd7          0.085074           2025
seed7/prg/panic.sd7          0.101655           2634
seed7/prg/percolation.sd7    0.045107            330
seed7/prg/planets.sd7        0.079623           1486
seed7/prg/portfwd7.sd7       0.040658            139
seed7/prg/prime.sd7          0.037794             74
seed7/prg/printpi1.sd7       0.037792             56
seed7/prg/printpi2.sd7       0.037570             54
seed7/prg/printpi3.sd7       0.037503             60
seed7/prg/pv7.sd7            0.046064            337
seed7/prg/queen.sd7          0.039594            149
seed7/prg/rand.sd7           0.037809            121
seed7/prg/raytrace.sd7       0.048843            538
seed7/prg/rever.sd7          0.055204            816
seed7/prg/roman.sd7          0.035842             38
seed7/prg/s7c.sd7            0.285924           9060
seed7/prg/s7check.sd7        0.038342             68
seed7/prg/savehd7.sd7        0.068857           1110
seed7/prg/self.sd7           0.038124             49
seed7/prg/shisen.sd7         0.074702           1423
seed7/prg/sl.sd7             0.061746           1029
seed7/prg/snake.sd7          0.048706            615
seed7/prg/sokoban.sd7        0.056681            891
seed7/prg/spigotpi.sd7       0.038431             64
seed7/prg/sql7.sd7           0.043297            278
seed7/prg/startrek.sd7       0.061327            979
seed7/prg/sudoku7.sd7        0.102484           2657
seed7/prg/sydir7.sd7         0.045951            384
seed7/prg/syntaxhl.sd7       0.041540            177
seed7/prg/tak.sd7            0.036264             59
seed7/prg/tar7.sd7           0.038828            121
seed7/prg/tch.sd7            0.037508             55
seed7/prg/testfont.sd7       0.039112             95
seed7/prg/tet.sd7            0.046172            479
seed7/prg/tetg.sd7           0.047537            501
seed7/prg/toutf8.sd7         0.044096            240
seed7/prg/tst_cli.sd7        0.036981             40
seed7/prg/tst_srv.sd7        0.037636             47
seed7/prg/wator.sd7          0.054992            651
seed7/prg/which.sd7          0.037881             65
seed7/prg/wiz.sd7            0.108257           2833
seed7/prg/wordcnt.sd7        0.038130             54
seed7/prg/wrinum.sd7         0.037308             43
seed7/prg/wumpus.sd7         0.044369            372
seed7/lib/aes.s7i            0.114361           1144
seed7/lib/aes_gcm.s7i        0.048213            392
seed7/lib/ar.s7i             0.075758           1532
seed7/lib/arc4.s7i           0.041827            144
seed7/lib/archive.s7i        0.040388            143
seed7/lib/archive_base.s7i   0.039031            135
seed7/lib/array.s7i          0.054583            610
seed7/lib/asn1.s7i           0.049041            544
seed7/lib/asn1oid.s7i        0.043645            157
seed7/lib/basearray.s7i      0.050703            450
seed7/lib/bigfile.s7i        0.040202            136
seed7/lib/bigint.s7i         0.057535            824
seed7/lib/bigrat.s7i         0.056037            784
seed7/lib/bin16.s7i          0.052897            592
seed7/lib/bin32.s7i          0.050201            490
seed7/lib/bin64.s7i          0.051544            539
seed7/lib/bitdata.s7i        0.079468           1330
seed7/lib/bitmapfont.s7i     0.043229            215
seed7/lib/bitset.s7i         0.050573            593
seed7/lib/bitsetof.s7i       0.050177            431
seed7/lib/blowfish.s7i       0.058167            383
seed7/lib/bmp.s7i            0.062497            924
seed7/lib/boolean.s7i        0.045852            403
seed7/lib/browser.s7i        0.043394            280
seed7/lib/bstring.s7i        0.041345            227
seed7/lib/bytedata.s7i       0.051142            482
seed7/lib/bzip2.s7i          0.060054            887
seed7/lib/cards.s7i          0.067676           1342
seed7/lib/category.s7i       0.043139            209
seed7/lib/cc_conf.s7i        0.080281           1314
seed7/lib/ccittfax.s7i       0.068316           1022
seed7/lib/cgi.s7i            0.039752            109
seed7/lib/cgidialog.s7i      0.061795           1118
seed7/lib/char.s7i           0.044723            356
seed7/lib/charsets.s7i       0.084987           2024
seed7/lib/chartype.s7i       0.041875            121
seed7/lib/cipher.s7i         0.039850            146
seed7/lib/cli_cmds.s7i       0.071323           1360
seed7/lib/clib_file.s7i      0.045243            301
seed7/lib/color.s7i          0.041830            185
seed7/lib/complex.s7i        0.047224            464
seed7/lib/compress.s7i       0.040294            150
seed7/lib/console.s7i        0.041048            188
seed7/lib/cpio.s7i           0.083768           1708
seed7/lib/crc32.s7i          0.044835            193
seed7/lib/cronos16.s7i       0.094695           1173
seed7/lib/cronos27.s7i       0.119487           1464
seed7/lib/csv.s7i            0.042958            201
seed7/lib/db_prop.s7i        0.065775            991
seed7/lib/deflate.s7i        0.057578            740
seed7/lib/des.s7i            0.057813            444
seed7/lib/dialog.s7i         0.045261            311
seed7/lib/dir.s7i            0.039714            163
seed7/lib/draw.s7i           0.058119            854
seed7/lib/duration.s7i       0.063030           1038
seed7/lib/echo.s7i           0.040274            132
seed7/lib/editline.s7i       0.046969            398
seed7/lib/elf.s7i            0.088278           1560
seed7/lib/elliptic.s7i       0.054622            649
seed7/lib/enable_io.s7i      0.045419            312
seed7/lib/encoding.s7i       0.062165            931
seed7/lib/enumeration.s7i    0.041643            236
seed7/lib/environment.s7i    0.040993            175
seed7/lib/exif.s7i           0.040366            152
seed7/lib/external_file.s7i  0.043778            340
seed7/lib/field.s7i          0.045132            268
seed7/lib/file.s7i           0.045939            372
seed7/lib/filebits.s7i       0.037847             46
seed7/lib/filesys.s7i        0.050721            601
seed7/lib/fileutil.s7i       0.040288            144
seed7/lib/fixarray.s7i       0.045599            307
seed7/lib/float.s7i          0.057290            757
seed7/lib/font.s7i           0.042081            196
seed7/lib/font8x8.s7i        0.050798            998
seed7/lib/forloop.s7i        0.048130            449
seed7/lib/ftp.s7i            0.059366            969
seed7/lib/ftpserv.s7i        0.052701            631
seed7/lib/getf.s7i           0.040447            115
seed7/lib/gethttp.s7i        0.037266             41
seed7/lib/gethttps.s7i       0.038039             41
seed7/lib/gif.s7i            0.052060            561
seed7/lib/graph.s7i          0.050824            415
seed7/lib/graph_file.s7i     0.045744            399
seed7/lib/gtkserver.s7i      0.039612            161
seed7/lib/gzip.s7i           0.050642            573
seed7/lib/hash.s7i           0.049989            421
seed7/lib/hashsetof.s7i      0.050291            499
seed7/lib/hmac.s7i           0.041193            152
seed7/lib/html.s7i           0.038754             83
seed7/lib/html_ent.s7i       0.049276            476
seed7/lib/htmldom.s7i        0.044691            286
seed7/lib/http_request.s7i   0.053954            696
seed7/lib/http_srv_resp.s7i  0.046936            380
seed7/lib/https_request.s7i  0.041786            211
seed7/lib/httpserv.s7i       0.046872            345
seed7/lib/huffman.s7i        0.054639            644
seed7/lib/ico.s7i            0.043479            221
seed7/lib/idxarray.s7i       0.043979            232
seed7/lib/image.s7i          0.039469            156
seed7/lib/imagefile.s7i      0.041127            171
seed7/lib/inflate.s7i        0.048443            411
seed7/lib/inifile.s7i        0.039388            129
seed7/lib/integer.s7i        0.054104            663
seed7/lib/iobuffer.s7i       0.043865            289
seed7/lib/jpeg.s7i           0.085901           1761
seed7/lib/json.s7i           0.056180            891
seed7/lib/json_serde.s7i     0.055052            783
seed7/lib/keybd.s7i          0.056668            639
seed7/lib/keydescr.s7i       0.048648            192
seed7/lib/leb128.s7i         0.041757            218
seed7/lib/line.s7i           0.040001            164
seed7/lib/listener.s7i       0.042938            247
seed7/lib/logfile.s7i        0.038254             73
seed7/lib/lower.s7i          0.039957            142
seed7/lib/lzma.s7i           0.061420            934
seed7/lib/lzw.s7i            0.061112            861
seed7/lib/magic.s7i          0.050140            403
seed7/lib/mahjng32.s7i       0.066308           1500
seed7/lib/make.s7i           0.051966            544
seed7/lib/makedata.s7i       0.073024           1428
seed7/lib/math.s7i           0.041693            201
seed7/lib/mixarith.s7i       0.042439            249
seed7/lib/modern27.s7i       0.088738           1099
seed7/lib/more.s7i           0.040336            130
seed7/lib/msgdigest.s7i      0.081133           1222
seed7/lib/multiscr.s7i       0.038109             68
seed7/lib/null_file.s7i      0.044356            345
seed7/lib/osfiles.s7i        0.066026           1085
seed7/lib/pbm.s7i            0.040762            230
seed7/lib/pcx.s7i            0.054397            638
seed7/lib/pem.s7i            0.041283            185
seed7/lib/pgm.s7i            0.043089            238
seed7/lib/pic16.s7i          0.051640           1037
seed7/lib/pic32.s7i          0.083061           2060
seed7/lib/pic_util.s7i       0.040933            144
seed7/lib/pixelimage.s7i     0.044187            320
seed7/lib/pixmap_file.s7i    0.047773            459
seed7/lib/pixmapfont.s7i     0.041999            184
seed7/lib/pkcs1.s7i          0.061709            543
seed7/lib/png.s7i            0.066466           1064
seed7/lib/poll.s7i           0.046018            313
seed7/lib/ppm.s7i            0.042824            240
seed7/lib/process.s7i        0.050710            541
seed7/lib/progs.s7i          0.058308            789
seed7/lib/propertyfile.s7i   0.040972            155
seed7/lib/rational.s7i       0.054925            792
seed7/lib/ref_list.s7i       0.042223            252
seed7/lib/reference.s7i      0.038563            126
seed7/lib/reverse.s7i        0.037082             94
seed7/lib/rpm.s7i            0.148871           3487
seed7/lib/rpmext.s7i         0.044568            318
seed7/lib/scanfile.s7i       0.083412           1779
seed7/lib/scanjson.s7i       0.048838            413
seed7/lib/scanstri.s7i       0.082671           1814
seed7/lib/scantoml.s7i       0.074497           1603
seed7/lib/seed7_05.s7i       0.069129           1072
seed7/lib/set.s7i            0.038125             57
seed7/lib/shell.s7i          0.055764            615
seed7/lib/showtls.s7i        0.056944            678
seed7/lib/signature.s7i      0.040344            131
seed7/lib/smtp.s7i           0.042805            261
seed7/lib/sockbase.s7i       0.043959            217
seed7/lib/socket.s7i         0.044733            326
seed7/lib/sokoban1.s7i       0.055078           1519
seed7/lib/sql_base.s7i       0.065049           1000
seed7/lib/stars.s7i          0.139396           1705
seed7/lib/stdfont10.s7i      0.083644           3347
seed7/lib/stdfont12.s7i      0.094030           3928
seed7/lib/stdfont14.s7i      0.106409           4510
seed7/lib/stdfont16.s7i      0.119142           5092
seed7/lib/stdfont18.s7i      0.136640           5868
seed7/lib/stdfont20.s7i      0.155818           6449
seed7/lib/stdfont24.s7i      0.184909           7421
seed7/lib/stdfont8.s7i       0.074396           2960
seed7/lib/stdfont9.s7i       0.077300           3152
seed7/lib/stdio.s7i          0.039448            192
seed7/lib/strifile.s7i       0.045677            345
seed7/lib/string.s7i         0.057622            779
seed7/lib/stritext.s7i       0.045559            352
seed7/lib/struct.s7i         0.046626            266
seed7/lib/struct_elem.s7i    0.040392            129
seed7/lib/subfile.s7i        0.040082            174
seed7/lib/subrange.s7i       0.041392             78
seed7/lib/syntax.s7i         0.049197            294
seed7/lib/tar.s7i            0.084072           1880
seed7/lib/tar_cmds.s7i       0.057995            752
seed7/lib/tdes.s7i           0.039899            143
seed7/lib/tee.s7i            0.039569            143
seed7/lib/text.s7i           0.038845            135
seed7/lib/tga.s7i            0.055382            676
seed7/lib/tiff.s7i           0.127652           2771
seed7/lib/time.s7i           0.065041           1191
seed7/lib/tls.s7i            0.106466           2230
seed7/lib/unicode.s7i        0.053230            575
seed7/lib/unionfnd.s7i       0.038303            130
seed7/lib/upper.s7i          0.039941            142
seed7/lib/utf16.s7i          0.051061            540
seed7/lib/utf8.s7i           0.043369            234
seed7/lib/vecfont10.s7i      0.081955           1056
seed7/lib/vecfont18.s7i      0.091176           1119
seed7/lib/vector3d.s7i       0.043121            293
seed7/lib/vectorfont.s7i     0.042248            239
seed7/lib/wildcard.s7i       0.040585            140
seed7/lib/window.s7i         0.047876            455
seed7/lib/wrinum.s7i         0.042286            248
seed7/lib/x509cert.s7i       0.072694           1243
seed7/lib/xml_ent.s7i        0.038138             94
seed7/lib/xmldom.s7i         0.042634            303
seed7/lib/xz.s7i             0.046614            442
seed7/lib/zip.s7i            0.121657           2792
seed7/lib/zstd.s7i           0.070217           1263
============================ ================= ================

Statistical Summary — Mode C (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.091955        |
+-----------+-----------------+
| Minimum   | 0.035802        |
+-----------+-----------------+
| Maximum   | 2.588956        |
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
seed7/prg/addup.sd7          0.048830            190
seed7/prg/bas7.sd7           0.788240          11459
seed7/prg/bifurk.sd7         0.039507             73
seed7/prg/bigfiles.sd7       0.042821            129
seed7/prg/brainf7.sd7        0.040841             86
seed7/prg/calc7.sd7          0.043419            128
seed7/prg/carddemo.sd7       0.048498            190
seed7/prg/castle.sd7         0.223079           3148
seed7/prg/cat.sd7            0.039452             82
seed7/prg/cellauto.sd7       0.039264             85
seed7/prg/celsius.sd7        0.037647             42
seed7/prg/chk_all.sd7        0.084248            843
seed7/prg/chkarr.sd7         0.883027           8367
seed7/prg/chkbig.sd7         4.232041          29026
seed7/prg/chkbin.sd7         1.051963           6469
seed7/prg/chkbitdata.sd7     1.277722           6624
seed7/prg/chkbool.sd7        0.237807           3157
seed7/prg/chkbst.sd7         0.103026            722
seed7/prg/chkchr.sd7         0.496527           2809
seed7/prg/chkcmd.sd7         0.113785           1205
seed7/prg/chkdb.sd7          0.773719           7454
seed7/prg/chkdecl.sd7        0.096442            448
seed7/prg/chkenum.sd7        0.125030           1230
seed7/prg/chkerr.sd7         0.348226           4663
seed7/prg/chkexc.sd7         0.151700           2627
seed7/prg/chkfil.sd7         0.130596           1615
seed7/prg/chkflt.sd7         2.898160          20620
seed7/prg/chkhent.sd7        0.039141             54
seed7/prg/chkhsh.sd7         0.514200           4548
seed7/prg/chkidx.sd7         3.265719          19567
seed7/prg/chkint.sd7         5.699982          38129
seed7/prg/chkjson.sd7        0.194125           1764
seed7/prg/chkovf.sd7         1.238205           8216
seed7/prg/chkprc.sd7         0.718989          10111
seed7/prg/chkscan.sd7        0.092294            714
seed7/prg/chkset.sd7         1.794186          11974
seed7/prg/chkstr.sd7         3.510478          26952
seed7/prg/chktime.sd7        0.254363           2025
seed7/prg/chktoml.sd7        0.202626           1656
seed7/prg/clock.sd7          0.039069             47
seed7/prg/clock2.sd7         0.038067             43
seed7/prg/clock3.sd7         0.043564             95
seed7/prg/cmpfil.sd7         0.039971             84
seed7/prg/comanche.sd7       0.054574            180
seed7/prg/confval.sd7        0.054575            175
seed7/prg/db7.sd7            0.063645            417
seed7/prg/diff7.sd7          0.052644            263
seed7/prg/dirtst.sd7         0.036808             42
seed7/prg/dirx.sd7           0.043304            152
seed7/prg/dnafight.sd7       0.123208           1381
seed7/prg/dragon.sd7         0.039792             73
seed7/prg/echo.sd7           0.037948             39
seed7/prg/eliza.sd7          0.053504            302
seed7/prg/err.sd7            0.046145             96
seed7/prg/fannkuch.sd7       0.043903            131
seed7/prg/fib.sd7            0.038635             47
seed7/prg/find7.sd7          0.043741            133
seed7/prg/findchar.sd7       0.045604            149
seed7/prg/fractree.sd7       0.039186             55
seed7/prg/ftp7.sd7           0.053799            296
seed7/prg/ftpserv.sd7        0.039103             74
seed7/prg/gcd.sd7            0.040818            109
seed7/prg/gkbd.sd7           0.062604            358
seed7/prg/gtksvtst.sd7       0.039777             94
seed7/prg/hal.sd7            0.047377            250
seed7/prg/hamu.sd7           0.067492            573
seed7/prg/hanoi.sd7          0.038533             55
seed7/prg/hd.sd7             0.039692             79
seed7/prg/hello.sd7          0.037241             32
seed7/prg/hilbert.sd7        0.042526            108
seed7/prg/ide7.sd7           0.050216            196
seed7/prg/kbd.sd7            0.038472             49
seed7/prg/klondike.sd7       0.091483            883
seed7/prg/lander.sd7         0.135813           1551
seed7/prg/lst80bas.sd7       0.058353            344
seed7/prg/lst99bas.sd7       0.061910            401
seed7/prg/lstgwbas.sd7       0.076206            577
seed7/prg/mahjong.sd7        0.152596           1943
seed7/prg/make7.sd7          0.044057            121
seed7/prg/mandelbr.sd7       0.051405            237
seed7/prg/mind.sd7           0.060076            443
seed7/prg/mirror.sd7         0.044349            131
seed7/prg/ms.sd7             0.071519            641
seed7/prg/nicoma.sd7         0.042731            135
seed7/prg/pac.sd7            0.073850            726
seed7/prg/pairs.sd7          0.144744           2025
seed7/prg/panic.sd7          0.198533           2634
seed7/prg/percolation.sd7    0.057106            330
seed7/prg/planets.sd7        0.142237           1486
seed7/prg/portfwd7.sd7       0.044450            139
seed7/prg/prime.sd7          0.039192             74
seed7/prg/printpi1.sd7       0.039732             56
seed7/prg/printpi2.sd7       0.038874             54
seed7/prg/printpi3.sd7       0.038943             60
seed7/prg/pv7.sd7            0.058794            337
seed7/prg/queen.sd7          0.042994            149
seed7/prg/rand.sd7           0.041941            121
seed7/prg/raytrace.sd7       0.069187            538
seed7/prg/rever.sd7          0.083561            816
seed7/prg/roman.sd7          0.038738             38
seed7/prg/s7c.sd7            0.641590           9060
seed7/prg/s7check.sd7        0.040742             68
seed7/prg/savehd7.sd7        0.115065           1110
seed7/prg/self.sd7           0.037667             49
seed7/prg/shisen.sd7         0.121831           1423
seed7/prg/sl.sd7             0.099281           1029
seed7/prg/snake.sd7          0.070023            615
seed7/prg/sokoban.sd7        0.085511            891
seed7/prg/spigotpi.sd7       0.039303             64
seed7/prg/sql7.sd7           0.053947            278
seed7/prg/startrek.sd7       0.096197            979
seed7/prg/sudoku7.sd7        0.202718           2657
seed7/prg/sydir7.sd7         0.062420            384
seed7/prg/syntaxhl.sd7       0.049280            177
seed7/prg/tak.sd7            0.038915             59
seed7/prg/tar7.sd7           0.044326            121
seed7/prg/tch.sd7            0.039699             55
seed7/prg/testfont.sd7       0.041915             95
seed7/prg/tet.sd7            0.060461            479
seed7/prg/tetg.sd7           0.062047            501
seed7/prg/toutf8.sd7         0.052072            240
seed7/prg/tst_cli.sd7        0.038637             40
seed7/prg/tst_srv.sd7        0.038119             47
seed7/prg/wator.sd7          0.081017            651
seed7/prg/which.sd7          0.039974             65
seed7/prg/wiz.sd7            0.215393           2833
seed7/prg/wordcnt.sd7        0.039377             54
seed7/prg/wrinum.sd7         0.037843             43
seed7/prg/wumpus.sd7         0.055945            372
seed7/lib/aes.s7i            0.202732           1144
seed7/lib/aes_gcm.s7i        0.062945            392
seed7/lib/ar.s7i             0.126371           1532
seed7/lib/arc4.s7i           0.043509            144
seed7/lib/archive.s7i        0.042923            143
seed7/lib/archive_base.s7i   0.044194            135
seed7/lib/array.s7i          0.078109            610
seed7/lib/asn1.s7i           0.066608            544
seed7/lib/asn1oid.s7i        0.051193            157
seed7/lib/basearray.s7i      0.065103            450
seed7/lib/bigfile.s7i        0.043365            136
seed7/lib/bigint.s7i         0.080516            824
seed7/lib/bigrat.s7i         0.081025            784
seed7/lib/bin16.s7i          0.069321            592
seed7/lib/bin32.s7i          0.064093            490
seed7/lib/bin64.s7i          0.064873            539
seed7/lib/bitdata.s7i        0.127127           1330
seed7/lib/bitmapfont.s7i     0.047426            215
seed7/lib/bitset.s7i         0.064481            593
seed7/lib/bitsetof.s7i       0.062163            431
seed7/lib/blowfish.s7i       0.078234            383
seed7/lib/bmp.s7i            0.104902            924
seed7/lib/boolean.s7i        0.057533            403
seed7/lib/browser.s7i        0.055094            280
seed7/lib/bstring.s7i        0.048687            227
seed7/lib/bytedata.s7i       0.068887            482
seed7/lib/bzip2.s7i          0.091185            887
seed7/lib/cards.s7i          0.105819           1342
seed7/lib/category.s7i       0.050430            209
seed7/lib/cc_conf.s7i        0.124295           1314
seed7/lib/ccittfax.s7i       0.107298           1022
seed7/lib/cgi.s7i            0.041667            109
seed7/lib/cgidialog.s7i      0.098561           1118
seed7/lib/char.s7i           0.052991            356
seed7/lib/charsets.s7i       0.129867           2024
seed7/lib/chartype.s7i       0.055391            121
seed7/lib/cipher.s7i         0.045445            146
seed7/lib/cli_cmds.s7i       0.117141           1360
seed7/lib/clib_file.s7i      0.060498            301
seed7/lib/color.s7i          0.049347            185
seed7/lib/complex.s7i        0.061094            464
seed7/lib/compress.s7i       0.045973            150
seed7/lib/console.s7i        0.047720            188
seed7/lib/cpio.s7i           0.148614           1708
seed7/lib/crc32.s7i          0.056299            193
seed7/lib/cronos16.s7i       0.198931           1173
seed7/lib/cronos27.s7i       0.262380           1464
seed7/lib/csv.s7i            0.049572            201
seed7/lib/db_prop.s7i        0.104530            991
seed7/lib/deflate.s7i        0.088280            740
seed7/lib/des.s7i            0.080431            444
seed7/lib/dialog.s7i         0.057694            311
seed7/lib/dir.s7i            0.043803            163
seed7/lib/draw.s7i           0.086573            854
seed7/lib/duration.s7i       0.100454           1038
seed7/lib/echo.s7i           0.042493            132
seed7/lib/editline.s7i       0.060090            398
seed7/lib/elf.s7i            0.158336           1560
seed7/lib/elliptic.s7i       0.079248            649
seed7/lib/enable_io.s7i      0.055102            312
seed7/lib/encoding.s7i       0.099844            931
seed7/lib/enumeration.s7i    0.050852            236
seed7/lib/environment.s7i    0.045763            175
seed7/lib/exif.s7i           0.047796            152
seed7/lib/external_file.s7i  0.053584            340
seed7/lib/field.s7i          0.053467            268
seed7/lib/file.s7i           0.056372            372
seed7/lib/filebits.s7i       0.039619             46
seed7/lib/filesys.s7i        0.066556            601
seed7/lib/fileutil.s7i       0.044882            144
seed7/lib/fixarray.s7i       0.054989            307
seed7/lib/float.s7i          0.074362            757
seed7/lib/font.s7i           0.046084            196
seed7/lib/font8x8.s7i        0.068110            998
seed7/lib/forloop.s7i        0.060778            449
seed7/lib/ftp.s7i            0.089823            969
seed7/lib/ftpserv.s7i        0.079069            631
seed7/lib/getf.s7i           0.042388            115
seed7/lib/gethttp.s7i        0.038435             41
seed7/lib/gethttps.s7i       0.039216             41
seed7/lib/gif.s7i            0.073191            561
seed7/lib/graph.s7i          0.065988            415
seed7/lib/graph_file.s7i     0.059558            399
seed7/lib/gtkserver.s7i      0.043922            161
seed7/lib/gzip.s7i           0.069767            573
seed7/lib/hash.s7i           0.068083            421
seed7/lib/hashsetof.s7i      0.068942            499
seed7/lib/hmac.s7i           0.046113            152
seed7/lib/html.s7i           0.039832             83
seed7/lib/html_ent.s7i       0.065049            476
seed7/lib/htmldom.s7i        0.054202            286
seed7/lib/http_request.s7i   0.113096            696
seed7/lib/http_srv_resp.s7i  0.074122            380
seed7/lib/https_request.s7i  0.052009            211
seed7/lib/httpserv.s7i       0.057863            345
seed7/lib/huffman.s7i        0.078042            644
seed7/lib/ico.s7i            0.052797            221
seed7/lib/idxarray.s7i       0.052498            232
seed7/lib/image.s7i          0.042696            156
seed7/lib/imagefile.s7i      0.047307            171
seed7/lib/inflate.s7i        0.066929            411
seed7/lib/inifile.s7i        0.045721            129
seed7/lib/integer.s7i        0.070628            663
seed7/lib/iobuffer.s7i       0.053471            289
seed7/lib/jpeg.s7i           0.160107           1761
seed7/lib/json.s7i           0.083826            891
seed7/lib/json_serde.s7i     0.081025            783
seed7/lib/keybd.s7i          0.082100            639
seed7/lib/keydescr.s7i       0.053538            192
seed7/lib/leb128.s7i         0.048188            218
seed7/lib/line.s7i           0.045739            164
seed7/lib/listener.s7i       0.050680            247
seed7/lib/logfile.s7i        0.040735             73
seed7/lib/lower.s7i          0.043046            142
seed7/lib/lzma.s7i           0.099131            934
seed7/lib/lzw.s7i            0.092233            861
seed7/lib/magic.s7i          0.065952            403
seed7/lib/mahjng32.s7i       0.094660           1500
seed7/lib/make.s7i           0.072238            544
seed7/lib/makedata.s7i       0.126288           1428
seed7/lib/math.s7i           0.047277            201
seed7/lib/mixarith.s7i       0.049802            249
seed7/lib/modern27.s7i       0.176854           1099
seed7/lib/more.s7i           0.044699            130
seed7/lib/msgdigest.s7i      0.141096           1222
seed7/lib/multiscr.s7i       0.040988             68
seed7/lib/null_file.s7i      0.053045            345
seed7/lib/osfiles.s7i        0.098432           1085
seed7/lib/pbm.s7i            0.050302            230
seed7/lib/pcx.s7i            0.081294            638
seed7/lib/pem.s7i            0.046954            185
seed7/lib/pgm.s7i            0.051461            238
seed7/lib/pic16.s7i          0.069998           1037
seed7/lib/pic32.s7i          0.125508           2060
seed7/lib/pic_util.s7i       0.045542            144
seed7/lib/pixelimage.s7i     0.054567            320
seed7/lib/pixmap_file.s7i    0.064356            459
seed7/lib/pixmapfont.s7i     0.049736            184
seed7/lib/pkcs1.s7i          0.081806            543
seed7/lib/png.s7i            0.111793           1064
seed7/lib/poll.s7i           0.054708            313
seed7/lib/ppm.s7i            0.050644            240
seed7/lib/process.s7i        0.066432            541
seed7/lib/progs.s7i          0.082264            789
seed7/lib/propertyfile.s7i   0.046265            155
seed7/lib/rational.s7i       0.081969            792
seed7/lib/ref_list.s7i       0.051852            252
seed7/lib/reference.s7i      0.044113            126
seed7/lib/reverse.s7i        0.041140             94
seed7/lib/rpm.s7i            0.293445           3487
seed7/lib/rpmext.s7i         0.054891            318
seed7/lib/scanfile.s7i       0.138961           1779
seed7/lib/scanjson.s7i       0.063761            413
seed7/lib/scanstri.s7i       0.140783           1814
seed7/lib/scantoml.s7i       0.134579           1603
seed7/lib/seed7_05.s7i       0.115049           1072
seed7/lib/set.s7i            0.039888             57
seed7/lib/shell.s7i          0.071756            615
seed7/lib/showtls.s7i        0.088414            678
seed7/lib/signature.s7i      0.044744            131
seed7/lib/smtp.s7i           0.051028            261
seed7/lib/sockbase.s7i       0.052723            217
seed7/lib/socket.s7i         0.059928            326
seed7/lib/sokoban1.s7i       0.085371           1519
seed7/lib/sql_base.s7i       0.097329           1000
seed7/lib/stars.s7i          0.243433           1705
seed7/lib/stdfont10.s7i      0.148543           3347
seed7/lib/stdfont12.s7i      0.170748           3928
seed7/lib/stdfont14.s7i      0.193951           4510
seed7/lib/stdfont16.s7i      0.218637           5092
seed7/lib/stdfont18.s7i      0.254266           5868
seed7/lib/stdfont20.s7i      0.281342           6449
seed7/lib/stdfont24.s7i      0.339683           7421
seed7/lib/stdfont8.s7i       0.131382           2960
seed7/lib/stdfont9.s7i       0.139078           3152
seed7/lib/stdio.s7i          0.046442            192
seed7/lib/strifile.s7i       0.060289            345
seed7/lib/string.s7i         0.079589            779
seed7/lib/stritext.s7i       0.057522            352
seed7/lib/struct.s7i         0.058456            266
seed7/lib/struct_elem.s7i    0.043539            129
seed7/lib/subfile.s7i        0.052547            174
seed7/lib/subrange.s7i       0.040636             78
seed7/lib/syntax.s7i         0.064264            294
seed7/lib/tar.s7i            0.149941           1880
seed7/lib/tar_cmds.s7i       0.086931            752
seed7/lib/tdes.s7i           0.045854            143
seed7/lib/tee.s7i            0.043497            143
seed7/lib/text.s7i           0.043483            135
seed7/lib/tga.s7i            0.084068            676
seed7/lib/tiff.s7i           0.251326           2771
seed7/lib/time.s7i           0.105632           1191
seed7/lib/tls.s7i            0.202223           2230
seed7/lib/unicode.s7i        0.076169            575
seed7/lib/unionfnd.s7i       0.044422            130
seed7/lib/upper.s7i          0.043677            142
seed7/lib/utf16.s7i          0.067374            540
seed7/lib/utf8.s7i           0.050155            234
seed7/lib/vecfont10.s7i      0.164457           1056
seed7/lib/vecfont18.s7i      0.184176           1119
seed7/lib/vector3d.s7i       0.052570            293
seed7/lib/vectorfont.s7i     0.051178            239
seed7/lib/wildcard.s7i       0.044330            140
seed7/lib/window.s7i         0.062819            455
seed7/lib/wrinum.s7i         0.050824            248
seed7/lib/x509cert.s7i       0.121569           1243
seed7/lib/xml_ent.s7i        0.044450             94
seed7/lib/xmldom.s7i         0.052733            303
seed7/lib/xz.s7i             0.062804            442
seed7/lib/zip.s7i            0.237878           2792
seed7/lib/zstd.s7i           0.121729           1263
============================ ================= ================

Statistical Summary — Mode D (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.163413        |
+-----------+-----------------+
| Minimum   | 0.036808        |
+-----------+-----------------+
| Maximum   | 5.699982        |
+-----------+-----------------+

Cross-Mode Comparison
=====================

+------+-----------------+-----------------+-----------------+
| Mode | Average (s)     | Minimum (s)     | Maximum (s)     |
+======+=================+=================+=================+
| A    | 0.033710        | 0.032197        | 0.039572        |
+------+-----------------+-----------------+-----------------+
| B    | 0.039158        | 0.035342        | 0.048396        |
+------+-----------------+-----------------+-----------------+
| C    | 0.091955        | 0.035802        | 2.588956        |
+------+-----------------+-----------------+-----------------+
| D    | 0.163413        | 0.036808        | 5.699982        |
+------+-----------------+-----------------+-----------------+

Phase Timing Summary
====================

Wall-clock times include warm-up work and GC performed by the warm-up helper.

+----------+--------------+--------------+--------------+
| Phase    | Warm-up      | Timed pass   | Mode total   |
+==========+==============+==============+==============+
| Mode A   | 00:00:12.347 | 00:00:57.435 | 00:01:09.782 |
+----------+--------------+--------------+--------------+
| Mode B   | 00:00:14.970 | 00:01:06.778 | 00:01:21.748 |
+----------+--------------+--------------+--------------+
| Mode C   | 00:00:35.888 | 00:02:37.514 | 00:03:13.403 |
+----------+--------------+--------------+--------------+
| Mode D   | 00:01:03.091 | 00:04:39.301 | 00:05:42.392 |
+----------+--------------+--------------+--------------+
| Total    |              |              | 00:11:27.335 |
+----------+--------------+--------------+--------------+


.. ---------------------------------------------------------------------------
