=======================================================
GC-Controlled Benchmark Report: Seed7 Mode — Four Modes
=======================================================

:Running with: seed7-mode 2026-06-23T14:10:42+0000 W26-2
:Seed7 dir   : /Users/roup/my/dvo/seed7-repos/seed7/
:Started at: 10:15:22 local time
:Generated on: 2026-06-23 14:26:31 UTC
:N Iterations: 5  (mean of N timed opens per file)
:Window system: nil
:Display graphic: nil
:Frame size: 261x90 chars
:Window body: 261x88 chars
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
seed7/prg/addup.sd7          0.034796            190
seed7/prg/bas7.sd7           0.034174          11459
seed7/prg/bifurk.sd7         0.033880             73
seed7/prg/bigfiles.sd7       0.033509            129
seed7/prg/brainf7.sd7        0.033691             86
seed7/prg/calc7.sd7          0.033752            128
seed7/prg/carddemo.sd7       0.033476            190
seed7/prg/castle.sd7         0.033681           3148
seed7/prg/cat.sd7            0.033787             82
seed7/prg/cellauto.sd7       0.033511             85
seed7/prg/celsius.sd7        0.033444             42
seed7/prg/chk_all.sd7        0.033352            843
seed7/prg/chkarr.sd7         0.033984           8367
seed7/prg/chkbig.sd7         0.037948          29026
seed7/prg/chkbin.sd7         0.034323           6469
seed7/prg/chkbitdata.sd7     0.033666           6624
seed7/prg/chkbool.sd7        0.032841           3157
seed7/prg/chkbst.sd7         0.032806            722
seed7/prg/chkchr.sd7         0.033139           2809
seed7/prg/chkcmd.sd7         0.032508           1205
seed7/prg/chkdb.sd7          0.033211           7454
seed7/prg/chkdecl.sd7        0.034496            448
seed7/prg/chkenum.sd7        0.033569           1230
seed7/prg/chkerr.sd7         0.033429           4663
seed7/prg/chkexc.sd7         0.033022           2627
seed7/prg/chkfil.sd7         0.033077           1615
seed7/prg/chkflt.sd7         0.036914          20620
seed7/prg/chkhent.sd7        0.032417             54
seed7/prg/chkhsh.sd7         0.033075           4548
seed7/prg/chkidx.sd7         0.035016          19567
seed7/prg/chkint.sd7         0.039114          38129
seed7/prg/chkjson.sd7        0.032740           1764
seed7/prg/chkovf.sd7         0.034049           8216
seed7/prg/chkprc.sd7         0.032877          10111
seed7/prg/chkscan.sd7        0.033346            714
seed7/prg/chkset.sd7         0.034030          11974
seed7/prg/chkstr.sd7         0.038152          26952
seed7/prg/chktime.sd7        0.034085           2025
seed7/prg/chktoml.sd7        0.033652           1656
seed7/prg/clock.sd7          0.032698             47
seed7/prg/clock2.sd7         0.033458             43
seed7/prg/clock3.sd7         0.033704             95
seed7/prg/cmpfil.sd7         0.033595             84
seed7/prg/comanche.sd7       0.033667            180
seed7/prg/confval.sd7        0.033844            175
seed7/prg/db7.sd7            0.033076            417
seed7/prg/diff7.sd7          0.032741            263
seed7/prg/dirtst.sd7         0.032631             42
seed7/prg/dirx.sd7           0.032511            152
seed7/prg/dnafight.sd7       0.032577           1381
seed7/prg/dragon.sd7         0.032487             73
seed7/prg/echo.sd7           0.033905             39
seed7/prg/eliza.sd7          0.033449            302
seed7/prg/err.sd7            0.033961             96
seed7/prg/fannkuch.sd7       0.034980            131
seed7/prg/fib.sd7            0.034005             47
seed7/prg/find7.sd7          0.033679            133
seed7/prg/findchar.sd7       0.033437            149
seed7/prg/fractree.sd7       0.033776             55
seed7/prg/ftp7.sd7           0.033489            296
seed7/prg/ftpserv.sd7        0.033538             74
seed7/prg/gcd.sd7            0.033605            109
seed7/prg/gkbd.sd7           0.033747            358
seed7/prg/gtksvtst.sd7       0.035122             94
seed7/prg/hal.sd7            0.033752            250
seed7/prg/hamu.sd7           0.033674            573
seed7/prg/hanoi.sd7          0.033599             55
seed7/prg/hd.sd7             0.033778             79
seed7/prg/hello.sd7          0.033613             32
seed7/prg/hilbert.sd7        0.033496            108
seed7/prg/ide7.sd7           0.035847            196
seed7/prg/kbd.sd7            0.035248             49
seed7/prg/klondike.sd7       0.033904            883
seed7/prg/lander.sd7         0.033715           1551
seed7/prg/lst80bas.sd7       0.033638            344
seed7/prg/lst99bas.sd7       0.033251            401
seed7/prg/lstgwbas.sd7       0.032668            577
seed7/prg/mahjong.sd7        0.032881           1943
seed7/prg/make7.sd7          0.033153            121
seed7/prg/mandelbr.sd7       0.032445            237
seed7/prg/mind.sd7           0.032368            443
seed7/prg/mirror.sd7         0.034342            131
seed7/prg/ms.sd7             0.033931            641
seed7/prg/nicoma.sd7         0.033624            135
seed7/prg/pac.sd7            0.033602            726
seed7/prg/pairs.sd7          0.034120           2025
seed7/prg/panic.sd7          0.033894           2634
seed7/prg/percolation.sd7    0.033308            330
seed7/prg/planets.sd7        0.033722           1486
seed7/prg/portfwd7.sd7       0.033676            139
seed7/prg/prime.sd7          0.033828             74
seed7/prg/printpi1.sd7       0.033576             56
seed7/prg/printpi2.sd7       0.033634             54
seed7/prg/printpi3.sd7       0.033477             60
seed7/prg/pv7.sd7            0.033779            337
seed7/prg/queen.sd7          0.033260            149
seed7/prg/rand.sd7           0.033804            121
seed7/prg/raytrace.sd7       0.033545            538
seed7/prg/rever.sd7          0.033531            816
seed7/prg/roman.sd7          0.033518             38
seed7/prg/s7c.sd7            0.034290           9060
seed7/prg/s7check.sd7        0.033440             68
seed7/prg/savehd7.sd7        0.033611           1110
seed7/prg/self.sd7           0.033187             49
seed7/prg/shisen.sd7         0.033699           1423
seed7/prg/sl.sd7             0.033598           1029
seed7/prg/snake.sd7          0.033345            615
seed7/prg/sokoban.sd7        0.033026            891
seed7/prg/spigotpi.sd7       0.032637             64
seed7/prg/sql7.sd7           0.032235            278
seed7/prg/startrek.sd7       0.032396            979
seed7/prg/sudoku7.sd7        0.032639           2657
seed7/prg/sydir7.sd7         0.032759            384
seed7/prg/syntaxhl.sd7       0.034048            177
seed7/prg/tak.sd7            0.032393             59
seed7/prg/tar7.sd7           0.032783            121
seed7/prg/tch.sd7            0.032813             55
seed7/prg/testfont.sd7       0.032821             95
seed7/prg/tet.sd7            0.032641            479
seed7/prg/tetg.sd7           0.032275            501
seed7/prg/toutf8.sd7         0.032460            240
seed7/prg/tst_cli.sd7        0.033250             40
seed7/prg/tst_srv.sd7        0.033530             47
seed7/prg/wator.sd7          0.033300            651
seed7/prg/which.sd7          0.033638             65
seed7/prg/wiz.sd7            0.033398           2833
seed7/prg/wordcnt.sd7        0.033471             54
seed7/prg/wrinum.sd7         0.033655             43
seed7/prg/wumpus.sd7         0.033425            372
seed7/lib/aes.s7i            0.033656           1144
seed7/lib/aes_gcm.s7i        0.033617            392
seed7/lib/ar.s7i             0.033553           1532
seed7/lib/arc4.s7i           0.033270            144
seed7/lib/archive.s7i        0.033333            143
seed7/lib/archive_base.s7i   0.033468            135
seed7/lib/array.s7i          0.033583            610
seed7/lib/asn1.s7i           0.033678            544
seed7/lib/asn1oid.s7i        0.034220            157
seed7/lib/basearray.s7i      0.033602            450
seed7/lib/bigfile.s7i        0.033149            136
seed7/lib/bigint.s7i         0.032450            824
seed7/lib/bigrat.s7i         0.033007            784
seed7/lib/bin16.s7i          0.032577            592
seed7/lib/bin32.s7i          0.032867            490
seed7/lib/bin64.s7i          0.033387            539
seed7/lib/bitdata.s7i        0.035173           1330
seed7/lib/bitmapfont.s7i     0.035052            215
seed7/lib/bitset.s7i         0.033532            593
seed7/lib/bitsetof.s7i       0.034038            431
seed7/lib/blowfish.s7i       0.035319            383
seed7/lib/bmp.s7i            0.035131            924
seed7/lib/boolean.s7i        0.033776            403
seed7/lib/browser.s7i        0.033906            280
seed7/lib/bstring.s7i        0.033423            227
seed7/lib/bytedata.s7i       0.033972            482
seed7/lib/bzip2.s7i          0.033997            887
seed7/lib/cards.s7i          0.033549           1342
seed7/lib/category.s7i       0.034003            209
seed7/lib/cc_conf.s7i        0.033599           1314
seed7/lib/ccittfax.s7i       0.032759           1022
seed7/lib/cgi.s7i            0.032480            109
seed7/lib/cgidialog.s7i      0.032991           1118
seed7/lib/char.s7i           0.032496            356
seed7/lib/charsets.s7i       0.032765           2024
seed7/lib/chartype.s7i       0.032706            121
seed7/lib/cipher.s7i         0.033602            146
seed7/lib/cli_cmds.s7i       0.032633           1360
seed7/lib/clib_file.s7i      0.032675            301
seed7/lib/color.s7i          0.033444            185
seed7/lib/complex.s7i        0.035342            464
seed7/lib/compress.s7i       0.039587            150
seed7/lib/console.s7i        0.034477            188
seed7/lib/cpio.s7i           0.032601           1708
seed7/lib/crc32.s7i          0.032793            193
seed7/lib/cronos16.s7i       0.033141           1173
seed7/lib/cronos27.s7i       0.032748           1464
seed7/lib/csv.s7i            0.034384            201
seed7/lib/db_prop.s7i        0.033599            991
seed7/lib/deflate.s7i        0.033434            740
seed7/lib/des.s7i            0.033452            444
seed7/lib/dialog.s7i         0.033670            311
seed7/lib/dir.s7i            0.033843            163
seed7/lib/draw.s7i           0.033589            854
seed7/lib/duration.s7i       0.033629           1038
seed7/lib/echo.s7i           0.033809            132
seed7/lib/editline.s7i       0.033587            398
seed7/lib/elf.s7i            0.033636           1560
seed7/lib/elliptic.s7i       0.033922            649
seed7/lib/enable_io.s7i      0.033416            312
seed7/lib/encoding.s7i       0.034019            931
seed7/lib/enumeration.s7i    0.034023            236
seed7/lib/environment.s7i    0.033660            175
seed7/lib/exif.s7i           0.033606            152
seed7/lib/external_file.s7i  0.033734            340
seed7/lib/field.s7i          0.033356            268
seed7/lib/file.s7i           0.033731            372
seed7/lib/filebits.s7i       0.033144             46
seed7/lib/filesys.s7i        0.033441            601
seed7/lib/fileutil.s7i       0.033510            144
seed7/lib/fixarray.s7i       0.033922            307
seed7/lib/float.s7i          0.033193            757
seed7/lib/font.s7i           0.032749            196
seed7/lib/font8x8.s7i        0.033163            998
seed7/lib/forloop.s7i        0.032477            449
seed7/lib/ftp.s7i            0.032887            969
seed7/lib/ftpserv.s7i        0.032762            631
seed7/lib/getf.s7i           0.034538            115
seed7/lib/gethttp.s7i        0.033687             41
seed7/lib/gethttps.s7i       0.033669             41
seed7/lib/gif.s7i            0.033465            561
seed7/lib/graph.s7i          0.033650            415
seed7/lib/graph_file.s7i     0.033741            399
seed7/lib/gtkserver.s7i      0.033285            161
seed7/lib/gzip.s7i           0.033201            573
seed7/lib/hash.s7i           0.033073            421
seed7/lib/hashsetof.s7i      0.033522            499
seed7/lib/hmac.s7i           0.033884            152
seed7/lib/html.s7i           0.033568             83
seed7/lib/html_ent.s7i       0.033708            476
seed7/lib/htmldom.s7i        0.033298            286
seed7/lib/http_request.s7i   0.032744            696
seed7/lib/http_srv_resp.s7i  0.032511            380
seed7/lib/https_request.s7i  0.032112            211
seed7/lib/httpserv.s7i       0.032131            345
seed7/lib/huffman.s7i        0.032680            644
seed7/lib/ico.s7i            0.032731            221
seed7/lib/idxarray.s7i       0.033593            232
seed7/lib/image.s7i          0.033311            156
seed7/lib/imagefile.s7i      0.033611            171
seed7/lib/inflate.s7i        0.033696            411
seed7/lib/inifile.s7i        0.033213            129
seed7/lib/integer.s7i        0.033711            663
seed7/lib/iobuffer.s7i       0.033307            289
seed7/lib/jpeg.s7i           0.033367           1761
seed7/lib/json.s7i           0.033161            891
seed7/lib/json_serde.s7i     0.032683            783
seed7/lib/keybd.s7i          0.032566            639
seed7/lib/keydescr.s7i       0.032528            192
seed7/lib/leb128.s7i         0.033836            218
seed7/lib/line.s7i           0.032529            164
seed7/lib/listener.s7i       0.033035            247
seed7/lib/logfile.s7i        0.032239             73
seed7/lib/lower.s7i          0.034259            142
seed7/lib/lzma.s7i           0.033926            934
seed7/lib/lzw.s7i            0.033523            861
seed7/lib/magic.s7i          0.033619            403
seed7/lib/mahjng32.s7i       0.033683           1500
seed7/lib/make.s7i           0.033627            544
seed7/lib/makedata.s7i       0.034931           1428
seed7/lib/math.s7i           0.035446            201
seed7/lib/mixarith.s7i       0.033477            249
seed7/lib/modern27.s7i       0.034158           1099
seed7/lib/more.s7i           0.033663            130
seed7/lib/msgdigest.s7i      0.033709           1222
seed7/lib/multiscr.s7i       0.033610             68
seed7/lib/null_file.s7i      0.034369            345
seed7/lib/osfiles.s7i        0.034217           1085
seed7/lib/pbm.s7i            0.033722            230
seed7/lib/pcx.s7i            0.033673            638
seed7/lib/pem.s7i            0.033517            185
seed7/lib/pgm.s7i            0.033626            238
seed7/lib/pic16.s7i          0.033613           1037
seed7/lib/pic32.s7i          0.033421           2060
seed7/lib/pic_util.s7i       0.033272            144
seed7/lib/pixelimage.s7i     0.033786            320
seed7/lib/pixmap_file.s7i    0.032816            459
seed7/lib/pixmapfont.s7i     0.032550            184
seed7/lib/pkcs1.s7i          0.033123            543
seed7/lib/png.s7i            0.032649           1064
seed7/lib/poll.s7i           0.032754            313
seed7/lib/ppm.s7i            0.033908            240
seed7/lib/process.s7i        0.033854            541
seed7/lib/progs.s7i          0.033872            789
seed7/lib/propertyfile.s7i   0.034201            155
seed7/lib/rational.s7i       0.033942            792
seed7/lib/ref_list.s7i       0.033643            252
seed7/lib/reference.s7i      0.033782            126
seed7/lib/reverse.s7i        0.033525             94
seed7/lib/rpm.s7i            0.033944           3487
seed7/lib/rpmext.s7i         0.033590            318
seed7/lib/scanfile.s7i       0.036263           1779
seed7/lib/scanjson.s7i       0.041882            413
seed7/lib/scanstri.s7i       0.035374           1814
seed7/lib/scantoml.s7i       0.033136           1603
seed7/lib/seed7_05.s7i       0.032811           1072
seed7/lib/set.s7i            0.032618             57
seed7/lib/shell.s7i          0.033595            615
seed7/lib/showtls.s7i        0.033526            678
seed7/lib/signature.s7i      0.032557            131
seed7/lib/smtp.s7i           0.033966            261
seed7/lib/sockbase.s7i       0.033621            217
seed7/lib/socket.s7i         0.033524            326
seed7/lib/sokoban1.s7i       0.033687           1519
seed7/lib/sql_base.s7i       0.034034           1000
seed7/lib/stars.s7i          0.033658           1705
seed7/lib/stdfont10.s7i      0.033170           3347
seed7/lib/stdfont12.s7i      0.032594           3928
seed7/lib/stdfont14.s7i      0.032622           4510
seed7/lib/stdfont16.s7i      0.033854           5092
seed7/lib/stdfont18.s7i      0.033512           5868
seed7/lib/stdfont20.s7i      0.032910           6449
seed7/lib/stdfont24.s7i      0.034738           7421
seed7/lib/stdfont8.s7i       0.041809           2960
seed7/lib/stdfont9.s7i       0.036319           3152
seed7/lib/stdio.s7i          0.033310            192
seed7/lib/strifile.s7i       0.033190            345
seed7/lib/string.s7i         0.033302            779
seed7/lib/stritext.s7i       0.033245            352
seed7/lib/struct.s7i         0.033511            266
seed7/lib/struct_elem.s7i    0.033296            129
seed7/lib/subfile.s7i        0.033590            174
seed7/lib/subrange.s7i       0.033289             78
seed7/lib/syntax.s7i         0.034372            294
seed7/lib/tar.s7i            0.034790           1880
seed7/lib/tar_cmds.s7i       0.033747            752
seed7/lib/tdes.s7i           0.035002            143
seed7/lib/tee.s7i            0.034242            143
seed7/lib/text.s7i           0.033898            135
seed7/lib/tga.s7i            0.035152            676
seed7/lib/tiff.s7i           0.033503           2771
seed7/lib/time.s7i           0.033433           1191
seed7/lib/tls.s7i            0.034799           2230
seed7/lib/unicode.s7i        0.034323            575
seed7/lib/unionfnd.s7i       0.034569            130
seed7/lib/upper.s7i          0.034422            142
seed7/lib/utf16.s7i          0.033130            540
seed7/lib/utf8.s7i           0.033679            234
seed7/lib/vecfont10.s7i      0.032744           1056
seed7/lib/vecfont18.s7i      0.032699           1119
seed7/lib/vector3d.s7i       0.032616            293
seed7/lib/vectorfont.s7i     0.033125            239
seed7/lib/wildcard.s7i       0.032417            140
seed7/lib/window.s7i         0.032938            455
seed7/lib/wrinum.s7i         0.035344            248
seed7/lib/x509cert.s7i       0.032578           1243
seed7/lib/xml_ent.s7i        0.032608             94
seed7/lib/xmldom.s7i         0.033463            303
seed7/lib/xz.s7i             0.033214            442
seed7/lib/zip.s7i            0.033248           2792
seed7/lib/zstd.s7i           0.033585           1263
============================ ================= ================

Statistical Summary — Mode A (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.033634        |
+-----------+-----------------+
| Minimum   | 0.032112        |
+-----------+-----------------+
| Maximum   | 0.041882        |
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
seed7/prg/addup.sd7          0.040150            190
seed7/prg/bas7.sd7           0.039666          11459
seed7/prg/bifurk.sd7         0.036629             73
seed7/prg/bigfiles.sd7       0.038249            129
seed7/prg/brainf7.sd7        0.037301             86
seed7/prg/calc7.sd7          0.037861            128
seed7/prg/carddemo.sd7       0.038517            190
seed7/prg/castle.sd7         0.039330           3148
seed7/prg/cat.sd7            0.037022             82
seed7/prg/cellauto.sd7       0.037723             85
seed7/prg/celsius.sd7        0.036118             42
seed7/prg/chk_all.sd7        0.039264            843
seed7/prg/chkarr.sd7         0.039719           8367
seed7/prg/chkbig.sd7         0.042809          29026
seed7/prg/chkbin.sd7         0.041644           6469
seed7/prg/chkbitdata.sd7     0.040797           6624
seed7/prg/chkbool.sd7        0.038644           3157
seed7/prg/chkbst.sd7         0.039085            722
seed7/prg/chkchr.sd7         0.040657           2809
seed7/prg/chkcmd.sd7         0.037235           1205
seed7/prg/chkdb.sd7          0.040413           7454
seed7/prg/chkdecl.sd7        0.039411            448
seed7/prg/chkenum.sd7        0.037510           1230
seed7/prg/chkerr.sd7         0.038413           4663
seed7/prg/chkexc.sd7         0.036261           2627
seed7/prg/chkfil.sd7         0.037666           1615
seed7/prg/chkflt.sd7         0.041313          20620
seed7/prg/chkhent.sd7        0.036742             54
seed7/prg/chkhsh.sd7         0.040308           4548
seed7/prg/chkidx.sd7         0.041373          19567
seed7/prg/chkint.sd7         0.045739          38129
seed7/prg/chkjson.sd7        0.039013           1764
seed7/prg/chkovf.sd7         0.039786           8216
seed7/prg/chkprc.sd7         0.038370          10111
seed7/prg/chkscan.sd7        0.039014            714
seed7/prg/chkset.sd7         0.040724          11974
seed7/prg/chkstr.sd7         0.043327          26952
seed7/prg/chktime.sd7        0.040018           2025
seed7/prg/chktoml.sd7        0.039047           1656
seed7/prg/clock.sd7          0.036023             47
seed7/prg/clock2.sd7         0.035795             43
seed7/prg/clock3.sd7         0.038875             95
seed7/prg/cmpfil.sd7         0.036452             84
seed7/prg/comanche.sd7       0.037622            180
seed7/prg/confval.sd7        0.038707            175
seed7/prg/db7.sd7            0.038058            417
seed7/prg/diff7.sd7          0.037806            263
seed7/prg/dirtst.sd7         0.035079             42
seed7/prg/dirx.sd7           0.037295            152
seed7/prg/dnafight.sd7       0.037920           1381
seed7/prg/dragon.sd7         0.035971             73
seed7/prg/echo.sd7           0.034540             39
seed7/prg/eliza.sd7          0.037254            302
seed7/prg/err.sd7            0.039810             96
seed7/prg/fannkuch.sd7       0.038760            131
seed7/prg/fib.sd7            0.036104             47
seed7/prg/find7.sd7          0.038784            133
seed7/prg/findchar.sd7       0.038867            149
seed7/prg/fractree.sd7       0.036663             55
seed7/prg/ftp7.sd7           0.038367            296
seed7/prg/ftpserv.sd7        0.037173             74
seed7/prg/gcd.sd7            0.037138            109
seed7/prg/gkbd.sd7           0.039011            358
seed7/prg/gtksvtst.sd7       0.039098             94
seed7/prg/hal.sd7            0.037360            250
seed7/prg/hamu.sd7           0.038566            573
seed7/prg/hanoi.sd7          0.036127             55
seed7/prg/hd.sd7             0.037910             79
seed7/prg/hello.sd7          0.038399             32
seed7/prg/hilbert.sd7        0.039162            108
seed7/prg/ide7.sd7           0.038865            196
seed7/prg/kbd.sd7            0.035993             49
seed7/prg/klondike.sd7       0.038683            883
seed7/prg/lander.sd7         0.039040           1551
seed7/prg/lst80bas.sd7       0.039104            344
seed7/prg/lst99bas.sd7       0.037465            401
seed7/prg/lstgwbas.sd7       0.036462            577
seed7/prg/mahjong.sd7        0.037727           1943
seed7/prg/make7.sd7          0.038643            121
seed7/prg/mandelbr.sd7       0.037839            237
seed7/prg/mind.sd7           0.039497            443
seed7/prg/mirror.sd7         0.038724            131
seed7/prg/ms.sd7             0.038558            641
seed7/prg/nicoma.sd7         0.038923            135
seed7/prg/pac.sd7            0.038727            726
seed7/prg/pairs.sd7          0.039076           2025
seed7/prg/panic.sd7          0.039606           2634
seed7/prg/percolation.sd7    0.038833            330
seed7/prg/planets.sd7        0.039674           1486
seed7/prg/portfwd7.sd7       0.038714            139
seed7/prg/prime.sd7          0.037110             74
seed7/prg/printpi1.sd7       0.036625             56
seed7/prg/printpi2.sd7       0.036325             54
seed7/prg/printpi3.sd7       0.036192             60
seed7/prg/pv7.sd7            0.038394            337
seed7/prg/queen.sd7          0.039050            149
seed7/prg/rand.sd7           0.038453            121
seed7/prg/raytrace.sd7       0.038785            538
seed7/prg/rever.sd7          0.038842            816
seed7/prg/roman.sd7          0.035834             38
seed7/prg/s7c.sd7            0.038496           9060
seed7/prg/s7check.sd7        0.036449             68
seed7/prg/savehd7.sd7        0.037649           1110
seed7/prg/self.sd7           0.035146             49
seed7/prg/shisen.sd7         0.037571           1423
seed7/prg/sl.sd7             0.037195           1029
seed7/prg/snake.sd7          0.037750            615
seed7/prg/sokoban.sd7        0.039539            891
seed7/prg/spigotpi.sd7       0.036679             64
seed7/prg/sql7.sd7           0.038496            278
seed7/prg/startrek.sd7       0.038969            979
seed7/prg/sudoku7.sd7        0.039065           2657
seed7/prg/sydir7.sd7         0.039076            384
seed7/prg/syntaxhl.sd7       0.041083            177
seed7/prg/tak.sd7            0.036373             59
seed7/prg/tar7.sd7           0.038899            121
seed7/prg/tch.sd7            0.036918             55
seed7/prg/testfont.sd7       0.039007             95
seed7/prg/tet.sd7            0.038650            479
seed7/prg/tetg.sd7           0.038724            501
seed7/prg/toutf8.sd7         0.039019            240
seed7/prg/tst_cli.sd7        0.035628             40
seed7/prg/tst_srv.sd7        0.035739             47
seed7/prg/wator.sd7          0.038218            651
seed7/prg/which.sd7          0.035831             65
seed7/prg/wiz.sd7            0.038487           2833
seed7/prg/wordcnt.sd7        0.036544             54
seed7/prg/wrinum.sd7         0.036215             43
seed7/prg/wumpus.sd7         0.038682            372
seed7/lib/aes.s7i            0.040624           1144
seed7/lib/aes_gcm.s7i        0.038222            392
seed7/lib/ar.s7i             0.037985           1532
seed7/lib/arc4.s7i           0.037681            144
seed7/lib/archive.s7i        0.038013            143
seed7/lib/archive_base.s7i   0.038774            135
seed7/lib/array.s7i          0.037493            610
seed7/lib/asn1.s7i           0.036471            544
seed7/lib/asn1oid.s7i        0.040859            157
seed7/lib/basearray.s7i      0.038035            450
seed7/lib/bigfile.s7i        0.038812            136
seed7/lib/bigint.s7i         0.038401            824
seed7/lib/bigrat.s7i         0.038958            784
seed7/lib/bin16.s7i          0.039168            592
seed7/lib/bin32.s7i          0.038683            490
seed7/lib/bin64.s7i          0.038635            539
seed7/lib/bitdata.s7i        0.044644           1330
seed7/lib/bitmapfont.s7i     0.038896            215
seed7/lib/bitset.s7i         0.039192            593
seed7/lib/bitsetof.s7i       0.039338            431
seed7/lib/blowfish.s7i       0.042049            383
seed7/lib/bmp.s7i            0.039265            924
seed7/lib/boolean.s7i        0.039099            403
seed7/lib/browser.s7i        0.039591            280
seed7/lib/bstring.s7i        0.038852            227
seed7/lib/bytedata.s7i       0.038901            482
seed7/lib/bzip2.s7i          0.038771            887
seed7/lib/cards.s7i          0.035905           1342
seed7/lib/category.s7i       0.037183            209
seed7/lib/cc_conf.s7i        0.037319           1314
seed7/lib/ccittfax.s7i       0.037612           1022
seed7/lib/cgi.s7i            0.037074            109
seed7/lib/cgidialog.s7i      0.039810           1118
seed7/lib/char.s7i           0.039088            356
seed7/lib/charsets.s7i       0.039992           2024
seed7/lib/chartype.s7i       0.041518            121
seed7/lib/cipher.s7i         0.038383            146
seed7/lib/cli_cmds.s7i       0.039021           1360
seed7/lib/clib_file.s7i      0.038824            301
seed7/lib/color.s7i          0.039411            185
seed7/lib/complex.s7i        0.040351            464
seed7/lib/compress.s7i       0.041065            150
seed7/lib/console.s7i        0.039206            188
seed7/lib/cpio.s7i           0.039428           1708
seed7/lib/crc32.s7i          0.044089            193
seed7/lib/cronos16.s7i       0.046278           1173
seed7/lib/cronos27.s7i       0.043925           1464
seed7/lib/csv.s7i            0.039527            201
seed7/lib/db_prop.s7i        0.038882            991
seed7/lib/deflate.s7i        0.039610            740
seed7/lib/des.s7i            0.039488            444
seed7/lib/dialog.s7i         0.038960            311
seed7/lib/dir.s7i            0.038570            163
seed7/lib/draw.s7i           0.042766            854
seed7/lib/duration.s7i       0.044088           1038
seed7/lib/echo.s7i           0.037830            132
seed7/lib/editline.s7i       0.037330            398
seed7/lib/elf.s7i            0.039030           1560
seed7/lib/elliptic.s7i       0.038258            649
seed7/lib/enable_io.s7i      0.038392            312
seed7/lib/encoding.s7i       0.039138            931
seed7/lib/enumeration.s7i    0.038894            236
seed7/lib/environment.s7i    0.038550            175
seed7/lib/exif.s7i           0.038648            152
seed7/lib/external_file.s7i  0.038408            340
seed7/lib/field.s7i          0.038665            268
seed7/lib/file.s7i           0.038536            372
seed7/lib/filebits.s7i       0.036345             46
seed7/lib/filesys.s7i        0.038383            601
seed7/lib/fileutil.s7i       0.038709            144
seed7/lib/fixarray.s7i       0.039957            307
seed7/lib/float.s7i          0.038745            757
seed7/lib/font.s7i           0.038348            196
seed7/lib/font8x8.s7i        0.037477            998
seed7/lib/forloop.s7i        0.039402            449
seed7/lib/ftp.s7i            0.038592            969
seed7/lib/ftpserv.s7i        0.037705            631
seed7/lib/getf.s7i           0.037061            115
seed7/lib/gethttp.s7i        0.035007             41
seed7/lib/gethttps.s7i       0.035369             41
seed7/lib/gif.s7i            0.037824            561
seed7/lib/graph.s7i          0.039826            415
seed7/lib/graph_file.s7i     0.037511            399
seed7/lib/gtkserver.s7i      0.037081            161
seed7/lib/gzip.s7i           0.037661            573
seed7/lib/hash.s7i           0.038995            421
seed7/lib/hashsetof.s7i      0.040800            499
seed7/lib/hmac.s7i           0.038627            152
seed7/lib/html.s7i           0.037277             83
seed7/lib/html_ent.s7i       0.038800            476
seed7/lib/htmldom.s7i        0.039160            286
seed7/lib/http_request.s7i   0.038923            696
seed7/lib/http_srv_resp.s7i  0.038523            380
seed7/lib/https_request.s7i  0.038586            211
seed7/lib/httpserv.s7i       0.038534            345
seed7/lib/huffman.s7i        0.038491            644
seed7/lib/ico.s7i            0.038741            221
seed7/lib/idxarray.s7i       0.037965            232
seed7/lib/image.s7i          0.039327            156
seed7/lib/imagefile.s7i      0.038585            171
seed7/lib/inflate.s7i        0.038903            411
seed7/lib/inifile.s7i        0.038893            129
seed7/lib/integer.s7i        0.038575            663
seed7/lib/iobuffer.s7i       0.038341            289
seed7/lib/jpeg.s7i           0.039180           1761
seed7/lib/json.s7i           0.038397            891
seed7/lib/json_serde.s7i     0.038646            783
seed7/lib/keybd.s7i          0.037359            639
seed7/lib/keydescr.s7i       0.038647            192
seed7/lib/leb128.s7i         0.038105            218
seed7/lib/line.s7i           0.037132            164
seed7/lib/listener.s7i       0.037152            247
seed7/lib/logfile.s7i        0.037562             73
seed7/lib/lower.s7i          0.038326            142
seed7/lib/lzma.s7i           0.039014            934
seed7/lib/lzw.s7i            0.038638            861
seed7/lib/magic.s7i          0.040630            403
seed7/lib/mahjng32.s7i       0.037940           1500
seed7/lib/make.s7i           0.038473            544
seed7/lib/makedata.s7i       0.038675           1428
seed7/lib/math.s7i           0.039575            201
seed7/lib/mixarith.s7i       0.038885            249
seed7/lib/modern27.s7i       0.041006           1099
seed7/lib/more.s7i           0.038282            130
seed7/lib/msgdigest.s7i      0.039519           1222
seed7/lib/multiscr.s7i       0.036685             68
seed7/lib/null_file.s7i      0.038024            345
seed7/lib/osfiles.s7i        0.040141           1085
seed7/lib/pbm.s7i            0.038857            230
seed7/lib/pcx.s7i            0.039133            638
seed7/lib/pem.s7i            0.038913            185
seed7/lib/pgm.s7i            0.039127            238
seed7/lib/pic16.s7i          0.037693           1037
seed7/lib/pic32.s7i          0.038601           2060
seed7/lib/pic_util.s7i       0.038604            144
seed7/lib/pixelimage.s7i     0.037324            320
seed7/lib/pixmap_file.s7i    0.037589            459
seed7/lib/pixmapfont.s7i     0.039074            184
seed7/lib/pkcs1.s7i          0.042745            543
seed7/lib/png.s7i            0.038801           1064
seed7/lib/poll.s7i           0.038838            313
seed7/lib/ppm.s7i            0.039702            240
seed7/lib/process.s7i        0.038934            541
seed7/lib/progs.s7i          0.038771            789
seed7/lib/propertyfile.s7i   0.039254            155
seed7/lib/rational.s7i       0.039245            792
seed7/lib/ref_list.s7i       0.038630            252
seed7/lib/reference.s7i      0.038674            126
seed7/lib/reverse.s7i        0.037700             94
seed7/lib/rpm.s7i            0.039213           3487
seed7/lib/rpmext.s7i         0.039671            318
seed7/lib/scanfile.s7i       0.043335           1779
seed7/lib/scanjson.s7i       0.045342            413
seed7/lib/scanstri.s7i       0.038265           1814
seed7/lib/scantoml.s7i       0.038358           1603
seed7/lib/seed7_05.s7i       0.040195           1072
seed7/lib/set.s7i            0.036252             57
seed7/lib/shell.s7i          0.039264            615
seed7/lib/showtls.s7i        0.038419            678
seed7/lib/signature.s7i      0.038096            131
seed7/lib/smtp.s7i           0.038956            261
seed7/lib/sockbase.s7i       0.039302            217
seed7/lib/socket.s7i         0.037581            326
seed7/lib/sokoban1.s7i       0.036039           1519
seed7/lib/sql_base.s7i       0.038124           1000
seed7/lib/stars.s7i          0.039094           1705
seed7/lib/stdfont10.s7i      0.041278           3347
seed7/lib/stdfont12.s7i      0.039416           3928
seed7/lib/stdfont14.s7i      0.037303           4510
seed7/lib/stdfont16.s7i      0.037701           5092
seed7/lib/stdfont18.s7i      0.039616           5868
seed7/lib/stdfont20.s7i      0.039475           6449
seed7/lib/stdfont24.s7i      0.039114           7421
seed7/lib/stdfont8.s7i       0.037404           2960
seed7/lib/stdfont9.s7i       0.037225           3152
seed7/lib/stdio.s7i          0.038541            192
seed7/lib/strifile.s7i       0.038969            345
seed7/lib/string.s7i         0.038739            779
seed7/lib/stritext.s7i       0.039108            352
seed7/lib/struct.s7i         0.040320            266
seed7/lib/struct_elem.s7i    0.038806            129
seed7/lib/subfile.s7i        0.038825            174
seed7/lib/subrange.s7i       0.037487             78
seed7/lib/syntax.s7i         0.039714            294
seed7/lib/tar.s7i            0.039922           1880
seed7/lib/tar_cmds.s7i       0.038916            752
seed7/lib/tdes.s7i           0.038887            143
seed7/lib/tee.s7i            0.038897            143
seed7/lib/text.s7i           0.038298            135
seed7/lib/tga.s7i            0.040165            676
seed7/lib/tiff.s7i           0.039895           2771
seed7/lib/time.s7i           0.038274           1191
seed7/lib/tls.s7i            0.038275           2230
seed7/lib/unicode.s7i        0.038366            575
seed7/lib/unionfnd.s7i       0.039491            130
seed7/lib/upper.s7i          0.037257            142
seed7/lib/utf16.s7i          0.038504            540
seed7/lib/utf8.s7i           0.039379            234
seed7/lib/vecfont10.s7i      0.038997           1056
seed7/lib/vecfont18.s7i      0.041069           1119
seed7/lib/vector3d.s7i       0.039812            293
seed7/lib/vectorfont.s7i     0.038813            239
seed7/lib/wildcard.s7i       0.038890            140
seed7/lib/window.s7i         0.038799            455
seed7/lib/wrinum.s7i         0.039628            248
seed7/lib/x509cert.s7i       0.039254           1243
seed7/lib/xml_ent.s7i        0.038700             94
seed7/lib/xmldom.s7i         0.038573            303
seed7/lib/xz.s7i             0.039119            442
seed7/lib/zip.s7i            0.039436           2792
seed7/lib/zstd.s7i           0.039296           1263
============================ ================= ================

Statistical Summary — Mode B (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.038684        |
+-----------+-----------------+
| Minimum   | 0.034540        |
+-----------+-----------------+
| Maximum   | 0.046278        |
+-----------+-----------------+

Mode C — Mode Activation + Full-Buffer Fontification (font-lock-ensure)
-----------------------------------------------------------------------

Full-buffer fontification stress test.

File Load Times — Mode C (mean, GC-free)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

============================ ================= ================
File Name                    Mean Time (s)     Line count
============================ ================= ================
seed7/prg/addup.sd7          0.040771            190
seed7/prg/bas7.sd7           0.322865          11459
seed7/prg/bifurk.sd7         0.041548             73
seed7/prg/bigfiles.sd7       0.036863            129
seed7/prg/brainf7.sd7        0.035188             86
seed7/prg/calc7.sd7          0.036405            128
seed7/prg/carddemo.sd7       0.040221            190
seed7/prg/castle.sd7         0.106493           3148
seed7/prg/cat.sd7            0.036092             82
seed7/prg/cellauto.sd7       0.036018             85
seed7/prg/celsius.sd7        0.035053             42
seed7/prg/chk_all.sd7        0.058109            843
seed7/prg/chkarr.sd7         0.348747           8367
seed7/prg/chkbig.sd7         2.034186          29026
seed7/prg/chkbin.sd7         0.504262           6469
seed7/prg/chkbitdata.sd7     0.602278           6624
seed7/prg/chkbool.sd7        0.114817           3157
seed7/prg/chkbst.sd7         0.063042            722
seed7/prg/chkchr.sd7         0.210928           2809
seed7/prg/chkcmd.sd7         0.064303           1205
seed7/prg/chkdb.sd7          0.346647           7454
seed7/prg/chkdecl.sd7        0.057570            448
seed7/prg/chkenum.sd7        0.067242           1230
seed7/prg/chkerr.sd7         0.190542           4663
seed7/prg/chkexc.sd7         0.081970           2627
seed7/prg/chkfil.sd7         0.076357           1615
seed7/prg/chkflt.sd7         1.326595          20620
seed7/prg/chkhent.sd7        0.037135             54
seed7/prg/chkhsh.sd7         0.245033           4548
seed7/prg/chkidx.sd7         1.302946          19567
seed7/prg/chkint.sd7         2.498480          38129
seed7/prg/chkjson.sd7        0.101593           1764
seed7/prg/chkovf.sd7         0.554021           8216
seed7/prg/chkprc.sd7         0.314604          10111
seed7/prg/chkscan.sd7        0.056182            714
seed7/prg/chkset.sd7         0.651066          11974
seed7/prg/chkstr.sd7         1.366141          26952
seed7/prg/chktime.sd7        0.127654           2025
seed7/prg/chktoml.sd7        0.104115           1656
seed7/prg/clock.sd7          0.035836             47
seed7/prg/clock2.sd7         0.035327             43
seed7/prg/clock3.sd7         0.037492             95
seed7/prg/cmpfil.sd7         0.035227             84
seed7/prg/comanche.sd7       0.039898            180
seed7/prg/confval.sd7        0.042595            175
seed7/prg/db7.sd7            0.046220            417
seed7/prg/diff7.sd7          0.041927            263
seed7/prg/dirtst.sd7         0.035186             42
seed7/prg/dirx.sd7           0.038146            152
seed7/prg/dnafight.sd7       0.066091           1381
seed7/prg/dragon.sd7         0.036109             73
seed7/prg/echo.sd7           0.035490             39
seed7/prg/eliza.sd7          0.042457            302
seed7/prg/err.sd7            0.039566             96
seed7/prg/fannkuch.sd7       0.036259            131
seed7/prg/fib.sd7            0.034153             47
seed7/prg/find7.sd7          0.036451            133
seed7/prg/findchar.sd7       0.036929            149
seed7/prg/fractree.sd7       0.034503             55
seed7/prg/ftp7.sd7           0.045675            296
seed7/prg/ftpserv.sd7        0.037786             74
seed7/prg/gcd.sd7            0.036704            109
seed7/prg/gkbd.sd7           0.045947            358
seed7/prg/gtksvtst.sd7       0.036490             94
seed7/prg/hal.sd7            0.039707            250
seed7/prg/hamu.sd7           0.047559            573
seed7/prg/hanoi.sd7          0.035959             55
seed7/prg/hd.sd7             0.036149             79
seed7/prg/hello.sd7          0.035153             32
seed7/prg/hilbert.sd7        0.036769            108
seed7/prg/ide7.sd7           0.039637            196
seed7/prg/kbd.sd7            0.035213             49
seed7/prg/klondike.sd7       0.054150            883
seed7/prg/lander.sd7         0.071631           1551
seed7/prg/lst80bas.sd7       0.043212            344
seed7/prg/lst99bas.sd7       0.044847            401
seed7/prg/lstgwbas.sd7       0.050183            577
seed7/prg/mahjong.sd7        0.079679           1943
seed7/prg/make7.sd7          0.037832            121
seed7/prg/mandelbr.sd7       0.041110            237
seed7/prg/mind.sd7           0.043681            443
seed7/prg/mirror.sd7         0.037420            131
seed7/prg/ms.sd7             0.047801            641
seed7/prg/nicoma.sd7         0.037636            135
seed7/prg/pac.sd7            0.048606            726
seed7/prg/pairs.sd7          0.080254           2025
seed7/prg/panic.sd7          0.096935           2634
seed7/prg/percolation.sd7    0.042739            330
seed7/prg/planets.sd7        0.075914           1486
seed7/prg/portfwd7.sd7       0.038447            139
seed7/prg/prime.sd7          0.036617             74
seed7/prg/printpi1.sd7       0.036383             56
seed7/prg/printpi2.sd7       0.035529             54
seed7/prg/printpi3.sd7       0.035648             60
seed7/prg/pv7.sd7            0.043573            337
seed7/prg/queen.sd7          0.037734            149
seed7/prg/rand.sd7           0.036926            121
seed7/prg/raytrace.sd7       0.047793            538
seed7/prg/rever.sd7          0.053596            816
seed7/prg/roman.sd7          0.035009             38
seed7/prg/s7c.sd7            0.275692           9060
seed7/prg/s7check.sd7        0.036005             68
seed7/prg/savehd7.sd7        0.066593           1110
seed7/prg/self.sd7           0.036458             49
seed7/prg/shisen.sd7         0.071455           1423
seed7/prg/sl.sd7             0.059345           1029
seed7/prg/snake.sd7          0.046778            615
seed7/prg/sokoban.sd7        0.055669            891
seed7/prg/spigotpi.sd7       0.035327             64
seed7/prg/sql7.sd7           0.040459            278
seed7/prg/startrek.sd7       0.057453            979
seed7/prg/sudoku7.sd7        0.097936           2657
seed7/prg/sydir7.sd7         0.044161            384
seed7/prg/syntaxhl.sd7       0.039456            177
seed7/prg/tak.sd7            0.035597             59
seed7/prg/tar7.sd7           0.037515            121
seed7/prg/tch.sd7            0.035615             55
seed7/prg/testfont.sd7       0.037086             95
seed7/prg/tet.sd7            0.044888            479
seed7/prg/tetg.sd7           0.043995            501
seed7/prg/toutf8.sd7         0.045531            240
seed7/prg/tst_cli.sd7        0.035990             40
seed7/prg/tst_srv.sd7        0.040472             47
seed7/prg/wator.sd7          0.052031            651
seed7/prg/which.sd7          0.035578             65
seed7/prg/wiz.sd7            0.103346           2833
seed7/prg/wordcnt.sd7        0.035915             54
seed7/prg/wrinum.sd7         0.034952             43
seed7/prg/wumpus.sd7         0.042159            372
seed7/lib/aes.s7i            0.109853           1144
seed7/lib/aes_gcm.s7i        0.046051            392
seed7/lib/ar.s7i             0.072361           1532
seed7/lib/arc4.s7i           0.038020            144
seed7/lib/archive.s7i        0.038648            143
seed7/lib/archive_base.s7i   0.038244            135
seed7/lib/array.s7i          0.053485            610
seed7/lib/asn1.s7i           0.047651            544
seed7/lib/asn1oid.s7i        0.041537            157
seed7/lib/basearray.s7i      0.048769            450
seed7/lib/bigfile.s7i        0.037916            136
seed7/lib/bigint.s7i         0.053552            824
seed7/lib/bigrat.s7i         0.052009            784
seed7/lib/bin16.s7i          0.049022            592
seed7/lib/bin32.s7i          0.046009            490
seed7/lib/bin64.s7i          0.050601            539
seed7/lib/bitdata.s7i        0.076446           1330
seed7/lib/bitmapfont.s7i     0.040510            215
seed7/lib/bitset.s7i         0.048530            593
seed7/lib/bitsetof.s7i       0.047293            431
seed7/lib/blowfish.s7i       0.055454            383
seed7/lib/bmp.s7i            0.058208            924
seed7/lib/boolean.s7i        0.043366            403
seed7/lib/browser.s7i        0.041356            280
seed7/lib/bstring.s7i        0.039787            227
seed7/lib/bytedata.s7i       0.049608            482
seed7/lib/bzip2.s7i          0.056751            887
seed7/lib/cards.s7i          0.068447           1342
seed7/lib/category.s7i       0.040783            209
seed7/lib/cc_conf.s7i        0.077496           1314
seed7/lib/ccittfax.s7i       0.064959           1022
seed7/lib/cgi.s7i            0.036470            109
seed7/lib/cgidialog.s7i      0.057996           1118
seed7/lib/char.s7i           0.041817            356
seed7/lib/charsets.s7i       0.080481           2024
seed7/lib/chartype.s7i       0.040461            121
seed7/lib/cipher.s7i         0.038528            146
seed7/lib/cli_cmds.s7i       0.067925           1360
seed7/lib/clib_file.s7i      0.043500            301
seed7/lib/color.s7i          0.040848            185
seed7/lib/complex.s7i        0.044825            464
seed7/lib/compress.s7i       0.038302            150
seed7/lib/console.s7i        0.038499            188
seed7/lib/cpio.s7i           0.080979           1708
seed7/lib/crc32.s7i          0.043534            193
seed7/lib/cronos16.s7i       0.091981           1173
seed7/lib/cronos27.s7i       0.119183           1464
seed7/lib/csv.s7i            0.041472            201
seed7/lib/db_prop.s7i        0.062270            991
seed7/lib/deflate.s7i        0.054841            740
seed7/lib/des.s7i            0.053740            444
seed7/lib/dialog.s7i         0.042213            311
seed7/lib/dir.s7i            0.036910            163
seed7/lib/draw.s7i           0.054193            854
seed7/lib/duration.s7i       0.062809           1038
seed7/lib/echo.s7i           0.038039            132
seed7/lib/editline.s7i       0.045425            398
seed7/lib/elf.s7i            0.084749           1560
seed7/lib/elliptic.s7i       0.052379            649
seed7/lib/enable_io.s7i      0.043511            312
seed7/lib/encoding.s7i       0.059413            931
seed7/lib/enumeration.s7i    0.042799            236
seed7/lib/environment.s7i    0.045310            175
seed7/lib/exif.s7i           0.039590            152
seed7/lib/external_file.s7i  0.042028            340
seed7/lib/field.s7i          0.040854            268
seed7/lib/file.s7i           0.043533            372
seed7/lib/filebits.s7i       0.035765             46
seed7/lib/filesys.s7i        0.047992            601
seed7/lib/fileutil.s7i       0.037974            144
seed7/lib/fixarray.s7i       0.043473            307
seed7/lib/float.s7i          0.054789            757
seed7/lib/font.s7i           0.037827            196
seed7/lib/font8x8.s7i        0.047027            998
seed7/lib/forloop.s7i        0.043877            449
seed7/lib/ftp.s7i            0.055484            969
seed7/lib/ftpserv.s7i        0.050982            631
seed7/lib/getf.s7i           0.037349            115
seed7/lib/gethttp.s7i        0.035481             41
seed7/lib/gethttps.s7i       0.035274             41
seed7/lib/gif.s7i            0.049470            561
seed7/lib/graph.s7i          0.048674            415
seed7/lib/graph_file.s7i     0.043847            399
seed7/lib/gtkserver.s7i      0.037912            161
seed7/lib/gzip.s7i           0.048313            573
seed7/lib/hash.s7i           0.049426            421
seed7/lib/hashsetof.s7i      0.048388            499
seed7/lib/hmac.s7i           0.038765            152
seed7/lib/html.s7i           0.036646             83
seed7/lib/html_ent.s7i       0.046901            476
seed7/lib/htmldom.s7i        0.043396            286
seed7/lib/http_request.s7i   0.051366            696
seed7/lib/http_srv_resp.s7i  0.045077            380
seed7/lib/https_request.s7i  0.039840            211
seed7/lib/httpserv.s7i       0.043501            345
seed7/lib/huffman.s7i        0.052138            644
seed7/lib/ico.s7i            0.039467            221
seed7/lib/idxarray.s7i       0.040589            232
seed7/lib/image.s7i          0.036167            156
seed7/lib/imagefile.s7i      0.037903            171
seed7/lib/inflate.s7i        0.046178            411
seed7/lib/inifile.s7i        0.038183            129
seed7/lib/integer.s7i        0.052067            663
seed7/lib/iobuffer.s7i       0.041430            289
seed7/lib/jpeg.s7i           0.085712           1761
seed7/lib/json.s7i           0.054146            891
seed7/lib/json_serde.s7i     0.051246            783
seed7/lib/keybd.s7i          0.053961            639
seed7/lib/keydescr.s7i       0.039786            192
seed7/lib/leb128.s7i         0.038562            218
seed7/lib/line.s7i           0.038337            164
seed7/lib/listener.s7i       0.040434            247
seed7/lib/logfile.s7i        0.035140             73
seed7/lib/lower.s7i          0.038194            142
seed7/lib/lzma.s7i           0.058675            934
seed7/lib/lzw.s7i            0.058618            861
seed7/lib/magic.s7i          0.047855            403
seed7/lib/mahjng32.s7i       0.063689           1500
seed7/lib/make.s7i           0.047705            544
seed7/lib/makedata.s7i       0.067985           1428
seed7/lib/math.s7i           0.038651            201
seed7/lib/mixarith.s7i       0.038821            249
seed7/lib/modern27.s7i       0.084379           1099
seed7/lib/more.s7i           0.038084            130
seed7/lib/msgdigest.s7i      0.078495           1222
seed7/lib/multiscr.s7i       0.036483             68
seed7/lib/null_file.s7i      0.042420            345
seed7/lib/osfiles.s7i        0.064776           1085
seed7/lib/pbm.s7i            0.041102            230
seed7/lib/pcx.s7i            0.051788            638
seed7/lib/pem.s7i            0.039903            185
seed7/lib/pgm.s7i            0.040375            238
seed7/lib/pic16.s7i          0.048607           1037
seed7/lib/pic32.s7i          0.079574           2060
seed7/lib/pic_util.s7i       0.038633            144
seed7/lib/pixelimage.s7i     0.042288            320
seed7/lib/pixmap_file.s7i    0.044906            459
seed7/lib/pixmapfont.s7i     0.038982            184
seed7/lib/pkcs1.s7i          0.057922            543
seed7/lib/png.s7i            0.062269           1064
seed7/lib/poll.s7i           0.042483            313
seed7/lib/ppm.s7i            0.040759            240
seed7/lib/process.s7i        0.047828            541
seed7/lib/progs.s7i          0.056061            789
seed7/lib/propertyfile.s7i   0.039112            155
seed7/lib/rational.s7i       0.053560            792
seed7/lib/ref_list.s7i       0.041489            252
seed7/lib/reference.s7i      0.038069            126
seed7/lib/reverse.s7i        0.036617             94
seed7/lib/rpm.s7i            0.142321           3487
seed7/lib/rpmext.s7i         0.042383            318
seed7/lib/scanfile.s7i       0.079680           1779
seed7/lib/scanjson.s7i       0.047062            413
seed7/lib/scanstri.s7i       0.080171           1814
seed7/lib/scantoml.s7i       0.070384           1603
seed7/lib/seed7_05.s7i       0.066724           1072
seed7/lib/set.s7i            0.036251             57
seed7/lib/shell.s7i          0.051851            615
seed7/lib/showtls.s7i        0.053340            678
seed7/lib/signature.s7i      0.037431            131
seed7/lib/smtp.s7i           0.039184            261
seed7/lib/sockbase.s7i       0.041706            217
seed7/lib/socket.s7i         0.043920            326
seed7/lib/sokoban1.s7i       0.053639           1519
seed7/lib/sql_base.s7i       0.064820           1000
seed7/lib/stars.s7i          0.133927           1705
seed7/lib/stdfont10.s7i      0.079742           3347
seed7/lib/stdfont12.s7i      0.090068           3928
seed7/lib/stdfont14.s7i      0.105504           4510
seed7/lib/stdfont16.s7i      0.114428           5092
seed7/lib/stdfont18.s7i      0.129987           5868
seed7/lib/stdfont20.s7i      0.143999           6449
seed7/lib/stdfont24.s7i      0.176167           7421
seed7/lib/stdfont8.s7i       0.072092           2960
seed7/lib/stdfont9.s7i       0.075682           3152
seed7/lib/stdio.s7i          0.039544            192
seed7/lib/strifile.s7i       0.043876            345
seed7/lib/string.s7i         0.054948            779
seed7/lib/stritext.s7i       0.042531            352
seed7/lib/struct.s7i         0.043688            266
seed7/lib/struct_elem.s7i    0.039039            129
seed7/lib/subfile.s7i        0.038724            174
seed7/lib/subrange.s7i       0.036502             78
seed7/lib/syntax.s7i         0.045414            294
seed7/lib/tar.s7i            0.081755           1880
seed7/lib/tar_cmds.s7i       0.057618            752
seed7/lib/tdes.s7i           0.038746            143
seed7/lib/tee.s7i            0.036501            143
seed7/lib/text.s7i           0.036316            135
seed7/lib/tga.s7i            0.051923            676
seed7/lib/tiff.s7i           0.119531           2771
seed7/lib/time.s7i           0.062809           1191
seed7/lib/tls.s7i            0.102898           2230
seed7/lib/unicode.s7i        0.051413            575
seed7/lib/unionfnd.s7i       0.037400            130
seed7/lib/upper.s7i          0.037653            142
seed7/lib/utf16.s7i          0.049541            540
seed7/lib/utf8.s7i           0.041045            234
seed7/lib/vecfont10.s7i      0.078477           1056
seed7/lib/vecfont18.s7i      0.086701           1119
seed7/lib/vector3d.s7i       0.040221            293
seed7/lib/vectorfont.s7i     0.040581            239
seed7/lib/wildcard.s7i       0.038101            140
seed7/lib/window.s7i         0.045205            455
seed7/lib/wrinum.s7i         0.040804            248
seed7/lib/x509cert.s7i       0.071113           1243
seed7/lib/xml_ent.s7i        0.036547             94
seed7/lib/xmldom.s7i         0.039677            303
seed7/lib/xz.s7i             0.044334            442
seed7/lib/zip.s7i            0.116948           2792
seed7/lib/zstd.s7i           0.068669           1263
============================ ================= ================

Statistical Summary — Mode C (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.088058        |
+-----------+-----------------+
| Minimum   | 0.034153        |
+-----------+-----------------+
| Maximum   | 2.498480        |
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
seed7/prg/addup.sd7          0.045498            190
seed7/prg/bas7.sd7           0.771059          11459
seed7/prg/bifurk.sd7         0.044660             73
seed7/prg/bigfiles.sd7       0.048264            129
seed7/prg/brainf7.sd7        0.039868             86
seed7/prg/calc7.sd7          0.040892            128
seed7/prg/carddemo.sd7       0.046091            190
seed7/prg/castle.sd7         0.212203           3148
seed7/prg/cat.sd7            0.039230             82
seed7/prg/cellauto.sd7       0.038985             85
seed7/prg/celsius.sd7        0.037136             42
seed7/prg/chk_all.sd7        0.080742            843
seed7/prg/chkarr.sd7         0.851674           8367
seed7/prg/chkbig.sd7         4.109469          29026
seed7/prg/chkbin.sd7         1.023526           6469
seed7/prg/chkbitdata.sd7     1.236330           6624
seed7/prg/chkbool.sd7        0.231980           3157
seed7/prg/chkbst.sd7         0.102467            722
seed7/prg/chkchr.sd7         0.479880           2809
seed7/prg/chkcmd.sd7         0.111156           1205
seed7/prg/chkdb.sd7          0.744954           7454
seed7/prg/chkdecl.sd7        0.092879            448
seed7/prg/chkenum.sd7        0.119330           1230
seed7/prg/chkerr.sd7         0.339261           4663
seed7/prg/chkexc.sd7         0.148969           2627
seed7/prg/chkfil.sd7         0.127993           1615
seed7/prg/chkflt.sd7         2.813090          20620
seed7/prg/chkhent.sd7        0.050937             54
seed7/prg/chkhsh.sd7         0.493350           4548
seed7/prg/chkidx.sd7         3.161414          19567
seed7/prg/chkint.sd7         5.534495          38129
seed7/prg/chkjson.sd7        0.189491           1764
seed7/prg/chkovf.sd7         1.206114           8216
seed7/prg/chkprc.sd7         0.691546          10111
seed7/prg/chkscan.sd7        0.089912            714
seed7/prg/chkset.sd7         1.732310          11974
seed7/prg/chkstr.sd7         3.386721          26952
seed7/prg/chktime.sd7        0.247271           2025
seed7/prg/chktoml.sd7        0.193338           1656
seed7/prg/clock.sd7          0.037499             47
seed7/prg/clock2.sd7         0.036833             43
seed7/prg/clock3.sd7         0.042286             95
seed7/prg/cmpfil.sd7         0.038923             84
seed7/prg/comanche.sd7       0.047535            180
seed7/prg/confval.sd7        0.050298            175
seed7/prg/db7.sd7            0.062251            417
seed7/prg/diff7.sd7          0.053418            263
seed7/prg/dirtst.sd7         0.036721             42
seed7/prg/dirx.sd7           0.042914            152
seed7/prg/dnafight.sd7       0.117227           1381
seed7/prg/dragon.sd7         0.038496             73
seed7/prg/echo.sd7           0.036173             39
seed7/prg/eliza.sd7          0.052417            302
seed7/prg/err.sd7            0.044999             96
seed7/prg/fannkuch.sd7       0.042027            131
seed7/prg/fib.sd7            0.036662             47
seed7/prg/find7.sd7          0.041710            133
seed7/prg/findchar.sd7       0.043344            149
seed7/prg/fractree.sd7       0.037438             55
seed7/prg/ftp7.sd7           0.053437            296
seed7/prg/ftpserv.sd7        0.039240             74
seed7/prg/gcd.sd7            0.041032            109
seed7/prg/gkbd.sd7           0.061759            358
seed7/prg/gtksvtst.sd7       0.039920             94
seed7/prg/hal.sd7            0.047623            250
seed7/prg/hamu.sd7           0.067099            573
seed7/prg/hanoi.sd7          0.037382             55
seed7/prg/hd.sd7             0.038711             79
seed7/prg/hello.sd7          0.036268             32
seed7/prg/hilbert.sd7        0.041602            108
seed7/prg/ide7.sd7           0.047741            196
seed7/prg/kbd.sd7            0.037227             49
seed7/prg/klondike.sd7       0.087411            883
seed7/prg/lander.sd7         0.130935           1551
seed7/prg/lst80bas.sd7       0.056848            344
seed7/prg/lst99bas.sd7       0.059748            401
seed7/prg/lstgwbas.sd7       0.073000            577
seed7/prg/mahjong.sd7        0.145416           1943
seed7/prg/make7.sd7          0.044516            121
seed7/prg/mandelbr.sd7       0.048812            237
seed7/prg/mind.sd7           0.061546            443
seed7/prg/mirror.sd7         0.044098            131
seed7/prg/ms.sd7             0.070366            641
seed7/prg/nicoma.sd7         0.043609            135
seed7/prg/pac.sd7            0.072273            726
seed7/prg/pairs.sd7          0.137698           2025
seed7/prg/panic.sd7          0.191954           2634
seed7/prg/percolation.sd7    0.056424            330
seed7/prg/planets.sd7        0.136752           1486
seed7/prg/portfwd7.sd7       0.044010            139
seed7/prg/prime.sd7          0.046022             74
seed7/prg/printpi1.sd7       0.042387             56
seed7/prg/printpi2.sd7       0.038351             54
seed7/prg/printpi3.sd7       0.037744             60
seed7/prg/pv7.sd7            0.057640            337
seed7/prg/queen.sd7          0.042973            149
seed7/prg/rand.sd7           0.041641            121
seed7/prg/raytrace.sd7       0.069046            538
seed7/prg/rever.sd7          0.082670            816
seed7/prg/roman.sd7          0.036734             38
seed7/prg/s7c.sd7            0.617174           9060
seed7/prg/s7check.sd7        0.039476             68
seed7/prg/savehd7.sd7        0.109798           1110
seed7/prg/self.sd7           0.037863             49
seed7/prg/shisen.sd7         0.119485           1423
seed7/prg/sl.sd7             0.096003           1029
seed7/prg/snake.sd7          0.066306            615
seed7/prg/sokoban.sd7        0.082048            891
seed7/prg/spigotpi.sd7       0.040047             64
seed7/prg/sql7.sd7           0.051490            278
seed7/prg/startrek.sd7       0.093520            979
seed7/prg/sudoku7.sd7        0.197550           2657
seed7/prg/sydir7.sd7         0.061209            384
seed7/prg/syntaxhl.sd7       0.049884            177
seed7/prg/tak.sd7            0.037582             59
seed7/prg/tar7.sd7           0.043418            121
seed7/prg/tch.sd7            0.040094             55
seed7/prg/testfont.sd7       0.046584             95
seed7/prg/tet.sd7            0.059821            479
seed7/prg/tetg.sd7           0.061474            501
seed7/prg/toutf8.sd7         0.050833            240
seed7/prg/tst_cli.sd7        0.036828             40
seed7/prg/tst_srv.sd7        0.036664             47
seed7/prg/wator.sd7          0.078392            651
seed7/prg/which.sd7          0.038104             65
seed7/prg/wiz.sd7            0.205680           2833
seed7/prg/wordcnt.sd7        0.038055             54
seed7/prg/wrinum.sd7         0.036727             43
seed7/prg/wumpus.sd7         0.054646            372
seed7/lib/aes.s7i            0.196291           1144
seed7/lib/aes_gcm.s7i        0.060452            392
seed7/lib/ar.s7i             0.122182           1532
seed7/lib/arc4.s7i           0.043124            144
seed7/lib/archive.s7i        0.045589            143
seed7/lib/archive_base.s7i   0.043130            135
seed7/lib/array.s7i          0.075847            610
seed7/lib/asn1.s7i           0.064584            544
seed7/lib/asn1oid.s7i        0.050068            157
seed7/lib/basearray.s7i      0.064138            450
seed7/lib/bigfile.s7i        0.042250            136
seed7/lib/bigint.s7i         0.077063            824
seed7/lib/bigrat.s7i         0.078765            784
seed7/lib/bin16.s7i          0.067506            592
seed7/lib/bin32.s7i          0.061688            490
seed7/lib/bin64.s7i          0.063589            539
seed7/lib/bitdata.s7i        0.122143           1330
seed7/lib/bitmapfont.s7i     0.047501            215
seed7/lib/bitset.s7i         0.063415            593
seed7/lib/bitsetof.s7i       0.060583            431
seed7/lib/blowfish.s7i       0.076219            383
seed7/lib/bmp.s7i            0.097657            924
seed7/lib/boolean.s7i        0.054929            403
seed7/lib/browser.s7i        0.053276            280
seed7/lib/bstring.s7i        0.047727            227
seed7/lib/bytedata.s7i       0.065290            482
seed7/lib/bzip2.s7i          0.088895            887
seed7/lib/cards.s7i          0.101628           1342
seed7/lib/category.s7i       0.048481            209
seed7/lib/cc_conf.s7i        0.124387           1314
seed7/lib/ccittfax.s7i       0.100150           1022
seed7/lib/cgi.s7i            0.040844            109
seed7/lib/cgidialog.s7i      0.095646           1118
seed7/lib/char.s7i           0.051481            356
seed7/lib/charsets.s7i       0.122641           2024
seed7/lib/chartype.s7i       0.047943            121
seed7/lib/cipher.s7i         0.043247            146
seed7/lib/cli_cmds.s7i       0.112003           1360
seed7/lib/clib_file.s7i      0.051879            301
seed7/lib/color.s7i          0.047654            185
seed7/lib/complex.s7i        0.059062            464
seed7/lib/compress.s7i       0.043327            150
seed7/lib/console.s7i        0.045660            188
seed7/lib/cpio.s7i           0.143601           1708
seed7/lib/crc32.s7i          0.054286            193
seed7/lib/cronos16.s7i       0.192437           1173
seed7/lib/cronos27.s7i       0.249252           1464
seed7/lib/csv.s7i            0.048058            201
seed7/lib/db_prop.s7i        0.101000            991
seed7/lib/deflate.s7i        0.086333            740
seed7/lib/des.s7i            0.084926            444
seed7/lib/dialog.s7i         0.057228            311
seed7/lib/dir.s7i            0.043165            163
seed7/lib/draw.s7i           0.084894            854
seed7/lib/duration.s7i       0.098807           1038
seed7/lib/echo.s7i           0.042464            132
seed7/lib/editline.s7i       0.060249            398
seed7/lib/elf.s7i            0.152326           1560
seed7/lib/elliptic.s7i       0.074935            649
seed7/lib/enable_io.s7i      0.052416            312
seed7/lib/encoding.s7i       0.095640            931
seed7/lib/enumeration.s7i    0.049304            236
seed7/lib/environment.s7i    0.045159            175
seed7/lib/exif.s7i           0.046196            152
seed7/lib/external_file.s7i  0.051439            340
seed7/lib/field.s7i          0.052543            268
seed7/lib/file.s7i           0.053639            372
seed7/lib/filebits.s7i       0.038523             46
seed7/lib/filesys.s7i        0.064518            601
seed7/lib/fileutil.s7i       0.043828            144
seed7/lib/fixarray.s7i       0.052695            307
seed7/lib/float.s7i          0.073593            757
seed7/lib/font.s7i           0.044889            196
seed7/lib/font8x8.s7i        0.068866            998
seed7/lib/forloop.s7i        0.058636            449
seed7/lib/ftp.s7i            0.086904            969
seed7/lib/ftpserv.s7i        0.078040            631
seed7/lib/getf.s7i           0.042054            115
seed7/lib/gethttp.s7i        0.037256             41
seed7/lib/gethttps.s7i       0.036858             41
seed7/lib/gif.s7i            0.076523            561
seed7/lib/graph.s7i          0.063633            415
seed7/lib/graph_file.s7i     0.058548            399
seed7/lib/gtkserver.s7i      0.042218            161
seed7/lib/gzip.s7i           0.066798            573
seed7/lib/hash.s7i           0.065999            421
seed7/lib/hashsetof.s7i      0.065286            499
seed7/lib/hmac.s7i           0.043771            152
seed7/lib/html.s7i           0.039774             83
seed7/lib/html_ent.s7i       0.065347            476
seed7/lib/htmldom.s7i        0.053995            286
seed7/lib/http_request.s7i   0.078383            696
seed7/lib/http_srv_resp.s7i  0.058684            380
seed7/lib/https_request.s7i  0.048172            211
seed7/lib/httpserv.s7i       0.057415            345
seed7/lib/huffman.s7i        0.073373            644
seed7/lib/ico.s7i            0.049222            221
seed7/lib/idxarray.s7i       0.051296            232
seed7/lib/image.s7i          0.041543            156
seed7/lib/imagefile.s7i      0.045469            171
seed7/lib/inflate.s7i        0.065262            411
seed7/lib/inifile.s7i        0.042664            129
seed7/lib/integer.s7i        0.067299            663
seed7/lib/iobuffer.s7i       0.051376            289
seed7/lib/jpeg.s7i           0.154092           1761
seed7/lib/json.s7i           0.081376            891
seed7/lib/json_serde.s7i     0.079375            783
seed7/lib/keybd.s7i          0.079867            639
seed7/lib/keydescr.s7i       0.049516            192
seed7/lib/leb128.s7i         0.047400            218
seed7/lib/line.s7i           0.042854            164
seed7/lib/listener.s7i       0.048541            247
seed7/lib/logfile.s7i        0.040862             73
seed7/lib/lower.s7i          0.042257            142
seed7/lib/lzma.s7i           0.096669            934
seed7/lib/lzw.s7i            0.087873            861
seed7/lib/magic.s7i          0.063955            403
seed7/lib/mahjng32.s7i       0.092423           1500
seed7/lib/make.s7i           0.070509            544
seed7/lib/makedata.s7i       0.123931           1428
seed7/lib/math.s7i           0.045999            201
seed7/lib/mixarith.s7i       0.046701            249
seed7/lib/modern27.s7i       0.166811           1099
seed7/lib/more.s7i           0.042818            130
seed7/lib/msgdigest.s7i      0.137454           1222
seed7/lib/multiscr.s7i       0.037736             68
seed7/lib/null_file.s7i      0.050367            345
seed7/lib/osfiles.s7i        0.094825           1085
seed7/lib/pbm.s7i            0.048011            230
seed7/lib/pcx.s7i            0.079822            638
seed7/lib/pem.s7i            0.047931            185
seed7/lib/pgm.s7i            0.051860            238
seed7/lib/pic16.s7i          0.068086           1037
seed7/lib/pic32.s7i          0.121348           2060
seed7/lib/pic_util.s7i       0.045298            144
seed7/lib/pixelimage.s7i     0.055818            320
seed7/lib/pixmap_file.s7i    0.065909            459
seed7/lib/pixmapfont.s7i     0.050955            184
seed7/lib/pkcs1.s7i          0.079850            543
seed7/lib/png.s7i            0.106808           1064
seed7/lib/poll.s7i           0.053717            313
seed7/lib/ppm.s7i            0.048364            240
seed7/lib/process.s7i        0.065570            541
seed7/lib/progs.s7i          0.084140            789
seed7/lib/propertyfile.s7i   0.045420            155
seed7/lib/rational.s7i       0.078623            792
seed7/lib/ref_list.s7i       0.048721            252
seed7/lib/reference.s7i      0.042312            126
seed7/lib/reverse.s7i        0.040395             94
seed7/lib/rpm.s7i            0.285174           3487
seed7/lib/rpmext.s7i         0.055358            318
seed7/lib/scanfile.s7i       0.131422           1779
seed7/lib/scanjson.s7i       0.069272            413
seed7/lib/scanstri.s7i       0.136133           1814
seed7/lib/scantoml.s7i       0.128861           1603
seed7/lib/seed7_05.s7i       0.110123           1072
seed7/lib/set.s7i            0.037736             57
seed7/lib/shell.s7i          0.069572            615
seed7/lib/showtls.s7i        0.085863            678
seed7/lib/signature.s7i      0.042359            131
seed7/lib/smtp.s7i           0.048707            261
seed7/lib/sockbase.s7i       0.050294            217
seed7/lib/socket.s7i         0.051687            326
seed7/lib/sokoban1.s7i       0.080065           1519
seed7/lib/sql_base.s7i       0.095065           1000
seed7/lib/stars.s7i          0.233436           1705
seed7/lib/stdfont10.s7i      0.142638           3347
seed7/lib/stdfont12.s7i      0.163158           3928
seed7/lib/stdfont14.s7i      0.187045           4510
seed7/lib/stdfont16.s7i      0.212002           5092
seed7/lib/stdfont18.s7i      0.243189           5868
seed7/lib/stdfont20.s7i      0.271382           6449
seed7/lib/stdfont24.s7i      0.325723           7421
seed7/lib/stdfont8.s7i       0.125878           2960
seed7/lib/stdfont9.s7i       0.131372           3152
seed7/lib/stdio.s7i          0.043512            192
seed7/lib/strifile.s7i       0.053527            345
seed7/lib/string.s7i         0.076320            779
seed7/lib/stritext.s7i       0.055212            352
seed7/lib/struct.s7i         0.056229            266
seed7/lib/struct_elem.s7i    0.041749            129
seed7/lib/subfile.s7i        0.044083            174
seed7/lib/subrange.s7i       0.038315             78
seed7/lib/syntax.s7i         0.059773            294
seed7/lib/tar.s7i            0.143840           1880
seed7/lib/tar_cmds.s7i       0.085272            752
seed7/lib/tdes.s7i           0.047650            143
seed7/lib/tee.s7i            0.042671            143
seed7/lib/text.s7i           0.042836            135
seed7/lib/tga.s7i            0.079731            676
seed7/lib/tiff.s7i           0.243382           2771
seed7/lib/time.s7i           0.101405           1191
seed7/lib/tls.s7i            0.198582           2230
seed7/lib/unicode.s7i        0.072531            575
seed7/lib/unionfnd.s7i       0.043609            130
seed7/lib/upper.s7i          0.042029            142
seed7/lib/utf16.s7i          0.066533            540
seed7/lib/utf8.s7i           0.048303            234
seed7/lib/vecfont10.s7i      0.161885           1056
seed7/lib/vecfont18.s7i      0.178422           1119
seed7/lib/vector3d.s7i       0.050256            293
seed7/lib/vectorfont.s7i     0.047784            239
seed7/lib/wildcard.s7i       0.043874            140
seed7/lib/window.s7i         0.061678            455
seed7/lib/wrinum.s7i         0.049120            248
seed7/lib/x509cert.s7i       0.117711           1243
seed7/lib/xml_ent.s7i        0.042309             94
seed7/lib/xmldom.s7i         0.050221            303
seed7/lib/xz.s7i             0.063562            442
seed7/lib/zip.s7i            0.237428           2792
seed7/lib/zstd.s7i           0.116872           1263
============================ ================= ================

Statistical Summary — Mode D (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.158276        |
+-----------+-----------------+
| Minimum   | 0.036173        |
+-----------+-----------------+
| Maximum   | 5.534495        |
+-----------+-----------------+

Cross-Mode Comparison
=====================

+------+-----------------+-----------------+-----------------+
| Mode | Average (s)     | Minimum (s)     | Maximum (s)     |
+======+=================+=================+=================+
| A    | 0.033634        | 0.032112        | 0.041882        |
+------+-----------------+-----------------+-----------------+
| B    | 0.038684        | 0.034540        | 0.046278        |
+------+-----------------+-----------------+-----------------+
| C    | 0.088058        | 0.034153        | 2.498480        |
+------+-----------------+-----------------+-----------------+
| D    | 0.158276        | 0.036173        | 5.534495        |
+------+-----------------+-----------------+-----------------+

Phase Timing Summary
====================

Wall-clock times include warm-up work and GC performed by the warm-up helper.

+----------+--------------+--------------+--------------+
| Phase    | Warm-up      | Timed pass   | Mode total   |
+==========+==============+==============+==============+
| Mode A   | 00:00:12.295 | 00:00:57.297 | 00:01:09.592 |
+----------+--------------+--------------+--------------+
| Mode B   | 00:00:14.796 | 00:01:05.969 | 00:01:20.766 |
+----------+--------------+--------------+--------------+
| Mode C   | 00:00:35.949 | 00:02:30.896 | 00:03:06.845 |
+----------+--------------+--------------+--------------+
| Mode D   | 00:01:01.030 | 00:04:30.560 | 00:05:31.591 |
+----------+--------------+--------------+--------------+
| Total    |              |              | 00:11:08.802 |
+----------+--------------+--------------+--------------+


.. ---------------------------------------------------------------------------
