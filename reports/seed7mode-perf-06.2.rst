=======================================================
GC-Controlled Benchmark Report: Seed7 Mode — Four Modes
=======================================================

:Running with: seed7-mode 2026-06-24T14:48:55+0000 W26-3
:Seed7 dir   : /Users/roup/my/dvo/seed7-repos/seed7/
:Started at: 10:50:47 local time
:Generated on: 2026-06-24 15:02:10 UTC
:N Iterations: 5  (mean of N timed opens per file)
:Window system: nil
:Display graphic: nil
:Frame size: 226x84 chars
:Window body: 226x82 chars
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
seed7/prg/addup.sd7          0.034276            190
seed7/prg/bas7.sd7           0.034296          11459
seed7/prg/bifurk.sd7         0.033697             73
seed7/prg/bigfiles.sd7       0.033487            129
seed7/prg/brainf7.sd7        0.034276             86
seed7/prg/calc7.sd7          0.035170            128
seed7/prg/carddemo.sd7       0.035486            190
seed7/prg/castle.sd7         0.033998           3148
seed7/prg/cat.sd7            0.033508             82
seed7/prg/cellauto.sd7       0.033736             85
seed7/prg/celsius.sd7        0.033553             42
seed7/prg/chk_all.sd7        0.033627            843
seed7/prg/chkarr.sd7         0.034105           8367
seed7/prg/chkbig.sd7         0.037687          29026
seed7/prg/chkbin.sd7         0.034508           6469
seed7/prg/chkbitdata.sd7     0.036918           6624
seed7/prg/chkbool.sd7        0.034160           3157
seed7/prg/chkbst.sd7         0.033134            722
seed7/prg/chkchr.sd7         0.033121           2809
seed7/prg/chkcmd.sd7         0.036349           1205
seed7/prg/chkdb.sd7          0.034388           7454
seed7/prg/chkdecl.sd7        0.032888            448
seed7/prg/chkenum.sd7        0.033329           1230
seed7/prg/chkerr.sd7         0.034682           4663
seed7/prg/chkexc.sd7         0.033868           2627
seed7/prg/chkfil.sd7         0.033313           1615
seed7/prg/chkflt.sd7         0.035611          20620
seed7/prg/chkhent.sd7        0.033480             54
seed7/prg/chkhsh.sd7         0.038841           4548
seed7/prg/chkidx.sd7         0.043743          19567
seed7/prg/chkint.sd7         0.044258          38129
seed7/prg/chkjson.sd7        0.037593           1764
seed7/prg/chkovf.sd7         0.037578           8216
seed7/prg/chkprc.sd7         0.035059          10111
seed7/prg/chkscan.sd7        0.033481            714
seed7/prg/chkset.sd7         0.034363          11974
seed7/prg/chkstr.sd7         0.037451          26952
seed7/prg/chktime.sd7        0.033559           2025
seed7/prg/chktoml.sd7        0.033522           1656
seed7/prg/clock.sd7          0.033549             47
seed7/prg/clock2.sd7         0.035696             43
seed7/prg/clock3.sd7         0.034488             95
seed7/prg/cmpfil.sd7         0.034398             84
seed7/prg/comanche.sd7       0.036595            180
seed7/prg/confval.sd7        0.039368            175
seed7/prg/db7.sd7            0.033859            417
seed7/prg/diff7.sd7          0.032590            263
seed7/prg/dirtst.sd7         0.034896             42
seed7/prg/dirx.sd7           0.037544            152
seed7/prg/dnafight.sd7       0.039294           1381
seed7/prg/dragon.sd7         0.034338             73
seed7/prg/echo.sd7           0.034650             39
seed7/prg/eliza.sd7          0.033731            302
seed7/prg/err.sd7            0.033473             96
seed7/prg/fannkuch.sd7       0.033630            131
seed7/prg/fib.sd7            0.034048             47
seed7/prg/find7.sd7          0.033266            133
seed7/prg/findchar.sd7       0.034574            149
seed7/prg/fractree.sd7       0.033897             55
seed7/prg/ftp7.sd7           0.033674            296
seed7/prg/ftpserv.sd7        0.033128             74
seed7/prg/gcd.sd7            0.033685            109
seed7/prg/gkbd.sd7           0.033404            358
seed7/prg/gtksvtst.sd7       0.033942             94
seed7/prg/hal.sd7            0.034947            250
seed7/prg/hamu.sd7           0.033572            573
seed7/prg/hanoi.sd7          0.034842             55
seed7/prg/hd.sd7             0.036127             79
seed7/prg/hello.sd7          0.033495             32
seed7/prg/hilbert.sd7        0.033356            108
seed7/prg/ide7.sd7           0.034286            196
seed7/prg/kbd.sd7            0.033691             49
seed7/prg/klondike.sd7       0.034916            883
seed7/prg/lander.sd7         0.033564           1551
seed7/prg/lst80bas.sd7       0.034764            344
seed7/prg/lst99bas.sd7       0.035412            401
seed7/prg/lstgwbas.sd7       0.033596            577
seed7/prg/mahjong.sd7        0.035653           1943
seed7/prg/make7.sd7          0.033037            121
seed7/prg/mandelbr.sd7       0.032439            237
seed7/prg/mind.sd7           0.036203            443
seed7/prg/mirror.sd7         0.033478            131
seed7/prg/ms.sd7             0.032762            641
seed7/prg/nicoma.sd7         0.032731            135
seed7/prg/pac.sd7            0.032710            726
seed7/prg/pairs.sd7          0.033398           2025
seed7/prg/panic.sd7          0.033038           2634
seed7/prg/percolation.sd7    0.033587            330
seed7/prg/planets.sd7        0.033559           1486
seed7/prg/portfwd7.sd7       0.033670            139
seed7/prg/prime.sd7          0.033216             74
seed7/prg/printpi1.sd7       0.033352             56
seed7/prg/printpi2.sd7       0.033542             54
seed7/prg/printpi3.sd7       0.033512             60
seed7/prg/pv7.sd7            0.033236            337
seed7/prg/queen.sd7          0.033912            149
seed7/prg/rand.sd7           0.033498            121
seed7/prg/raytrace.sd7       0.033638            538
seed7/prg/rever.sd7          0.033778            816
seed7/prg/roman.sd7          0.033502             38
seed7/prg/s7c.sd7            0.034464           9060
seed7/prg/s7check.sd7        0.033789             68
seed7/prg/savehd7.sd7        0.034302           1110
seed7/prg/self.sd7           0.033390             49
seed7/prg/shisen.sd7         0.033660           1423
seed7/prg/sl.sd7             0.033531           1029
seed7/prg/snake.sd7          0.032955            615
seed7/prg/sokoban.sd7        0.033869            891
seed7/prg/spigotpi.sd7       0.032780             64
seed7/prg/sql7.sd7           0.033158            278
seed7/prg/startrek.sd7       0.033192            979
seed7/prg/sudoku7.sd7        0.033617           2657
seed7/prg/sydir7.sd7         0.033732            384
seed7/prg/syntaxhl.sd7       0.034697            177
seed7/prg/tak.sd7            0.034577             59
seed7/prg/tar7.sd7           0.033615            121
seed7/prg/tch.sd7            0.034464             55
seed7/prg/testfont.sd7       0.034155             95
seed7/prg/tet.sd7            0.033236            479
seed7/prg/tetg.sd7           0.033489            501
seed7/prg/toutf8.sd7         0.033826            240
seed7/prg/tst_cli.sd7        0.033839             40
seed7/prg/tst_srv.sd7        0.033946             47
seed7/prg/wator.sd7          0.033581            651
seed7/prg/which.sd7          0.033133             65
seed7/prg/wiz.sd7            0.033331           2833
seed7/prg/wordcnt.sd7        0.032933             54
seed7/prg/wrinum.sd7         0.032441             43
seed7/prg/wumpus.sd7         0.032743            372
seed7/lib/aes.s7i            0.033838           1144
seed7/lib/aes_gcm.s7i        0.032944            392
seed7/lib/ar.s7i             0.033042           1532
seed7/lib/arc4.s7i           0.032495            144
seed7/lib/archive.s7i        0.033560            143
seed7/lib/archive_base.s7i   0.033544            135
seed7/lib/array.s7i          0.033918            610
seed7/lib/asn1.s7i           0.034169            544
seed7/lib/asn1oid.s7i        0.032934            157
seed7/lib/basearray.s7i      0.033732            450
seed7/lib/bigfile.s7i        0.032691            136
seed7/lib/bigint.s7i         0.033447            824
seed7/lib/bigrat.s7i         0.032462            784
seed7/lib/bin16.s7i          0.034064            592
seed7/lib/bin32.s7i          0.033098            490
seed7/lib/bin64.s7i          0.032581            539
seed7/lib/bitdata.s7i        0.033004           1330
seed7/lib/bitmapfont.s7i     0.033603            215
seed7/lib/bitset.s7i         0.033617            593
seed7/lib/bitsetof.s7i       0.033411            431
seed7/lib/blowfish.s7i       0.033443            383
seed7/lib/bmp.s7i            0.033845            924
seed7/lib/boolean.s7i        0.033498            403
seed7/lib/browser.s7i        0.034470            280
seed7/lib/bstring.s7i        0.033685            227
seed7/lib/bytedata.s7i       0.033646            482
seed7/lib/bzip2.s7i          0.033575            887
seed7/lib/cards.s7i          0.033837           1342
seed7/lib/category.s7i       0.033390            209
seed7/lib/cc_conf.s7i        0.033614           1314
seed7/lib/ccittfax.s7i       0.033602           1022
seed7/lib/cgi.s7i            0.033499            109
seed7/lib/cgidialog.s7i      0.033612           1118
seed7/lib/char.s7i           0.034489            356
seed7/lib/charsets.s7i       0.035926           2024
seed7/lib/chartype.s7i       0.035407            121
seed7/lib/cipher.s7i         0.036522            146
seed7/lib/cli_cmds.s7i       0.034981           1360
seed7/lib/clib_file.s7i      0.037166            301
seed7/lib/color.s7i          0.035569            185
seed7/lib/complex.s7i        0.033276            464
seed7/lib/compress.s7i       0.032746            150
seed7/lib/console.s7i        0.032522            188
seed7/lib/cpio.s7i           0.033203           1708
seed7/lib/crc32.s7i          0.034266            193
seed7/lib/cronos16.s7i       0.035335           1173
seed7/lib/cronos27.s7i       0.033189           1464
seed7/lib/csv.s7i            0.032452            201
seed7/lib/db_prop.s7i        0.033945            991
seed7/lib/deflate.s7i        0.033743            740
seed7/lib/des.s7i            0.032676            444
seed7/lib/dialog.s7i         0.033514            311
seed7/lib/dir.s7i            0.033517            163
seed7/lib/draw.s7i           0.033398            854
seed7/lib/duration.s7i       0.035411           1038
seed7/lib/echo.s7i           0.045546            132
seed7/lib/editline.s7i       0.037873            398
seed7/lib/elf.s7i            0.035399           1560
seed7/lib/elliptic.s7i       0.034722            649
seed7/lib/enable_io.s7i      0.034292            312
seed7/lib/encoding.s7i       0.034714            931
seed7/lib/enumeration.s7i    0.036474            236
seed7/lib/environment.s7i    0.033830            175
seed7/lib/exif.s7i           0.034344            152
seed7/lib/external_file.s7i  0.033386            340
seed7/lib/field.s7i          0.033585            268
seed7/lib/file.s7i           0.033605            372
seed7/lib/filebits.s7i       0.033454             46
seed7/lib/filesys.s7i        0.033960            601
seed7/lib/fileutil.s7i       0.039283            144
seed7/lib/fixarray.s7i       0.037258            307
seed7/lib/float.s7i          0.034088            757
seed7/lib/font.s7i           0.033034            196
seed7/lib/font8x8.s7i        0.033460            998
seed7/lib/forloop.s7i        0.032962            449
seed7/lib/ftp.s7i            0.035248            969
seed7/lib/ftpserv.s7i        0.033697            631
seed7/lib/getf.s7i           0.033528            115
seed7/lib/gethttp.s7i        0.033614             41
seed7/lib/gethttps.s7i       0.033476             41
seed7/lib/gif.s7i            0.033904            561
seed7/lib/graph.s7i          0.033723            415
seed7/lib/graph_file.s7i     0.033536            399
seed7/lib/gtkserver.s7i      0.033500            161
seed7/lib/gzip.s7i           0.033542            573
seed7/lib/hash.s7i           0.033786            421
seed7/lib/hashsetof.s7i      0.033521            499
seed7/lib/hmac.s7i           0.033694            152
seed7/lib/html.s7i           0.033304             83
seed7/lib/html_ent.s7i       0.033170            476
seed7/lib/htmldom.s7i        0.032376            286
seed7/lib/http_request.s7i   0.032725            696
seed7/lib/http_srv_resp.s7i  0.033481            380
seed7/lib/https_request.s7i  0.037343            211
seed7/lib/httpserv.s7i       0.034530            345
seed7/lib/huffman.s7i        0.033919            644
seed7/lib/ico.s7i            0.035252            221
seed7/lib/idxarray.s7i       0.033944            232
seed7/lib/image.s7i          0.035210            156
seed7/lib/imagefile.s7i      0.034022            171
seed7/lib/inflate.s7i        0.035100            411
seed7/lib/inifile.s7i        0.039319            129
seed7/lib/integer.s7i        0.033532            663
seed7/lib/iobuffer.s7i       0.032581            289
seed7/lib/jpeg.s7i           0.032926           1761
seed7/lib/json.s7i           0.034140            891
seed7/lib/json_serde.s7i     0.037681            783
seed7/lib/keybd.s7i          0.035362            639
seed7/lib/keydescr.s7i       0.033516            192
seed7/lib/leb128.s7i         0.034009            218
seed7/lib/line.s7i           0.033807            164
seed7/lib/listener.s7i       0.033632            247
seed7/lib/logfile.s7i        0.033443             73
seed7/lib/lower.s7i          0.033751            142
seed7/lib/lzma.s7i           0.034426            934
seed7/lib/lzw.s7i            0.034660            861
seed7/lib/magic.s7i          0.035108            403
seed7/lib/mahjng32.s7i       0.034896           1500
seed7/lib/make.s7i           0.034906            544
seed7/lib/makedata.s7i       0.034686           1428
seed7/lib/math.s7i           0.033658            201
seed7/lib/mixarith.s7i       0.035690            249
seed7/lib/modern27.s7i       0.034150           1099
seed7/lib/more.s7i           0.034595            130
seed7/lib/msgdigest.s7i      0.038315           1222
seed7/lib/multiscr.s7i       0.041026             68
seed7/lib/null_file.s7i      0.037638            345
seed7/lib/osfiles.s7i        0.035218           1085
seed7/lib/pbm.s7i            0.033879            230
seed7/lib/pcx.s7i            0.033991            638
seed7/lib/pem.s7i            0.033935            185
seed7/lib/pgm.s7i            0.033305            238
seed7/lib/pic16.s7i          0.033905           1037
seed7/lib/pic32.s7i          0.034182           2060
seed7/lib/pic_util.s7i       0.032493            144
seed7/lib/pixelimage.s7i     0.033129            320
seed7/lib/pixmap_file.s7i    0.033516            459
seed7/lib/pixmapfont.s7i     0.034220            184
seed7/lib/pkcs1.s7i          0.033111            543
seed7/lib/png.s7i            0.032804           1064
seed7/lib/poll.s7i           0.033228            313
seed7/lib/ppm.s7i            0.034623            240
seed7/lib/process.s7i        0.036616            541
seed7/lib/progs.s7i          0.036164            789
seed7/lib/propertyfile.s7i   0.034898            155
seed7/lib/rational.s7i       0.035623            792
seed7/lib/ref_list.s7i       0.038197            252
seed7/lib/reference.s7i      0.035630            126
seed7/lib/reverse.s7i        0.037057             94
seed7/lib/rpm.s7i            0.040155           3487
seed7/lib/rpmext.s7i         0.036495            318
seed7/lib/scanfile.s7i       0.037106           1779
seed7/lib/scanjson.s7i       0.036729            413
seed7/lib/scanstri.s7i       0.034093           1814
seed7/lib/scantoml.s7i       0.035379           1603
seed7/lib/seed7_05.s7i       0.036828           1072
seed7/lib/set.s7i            0.035976             57
seed7/lib/shell.s7i          0.034069            615
seed7/lib/showtls.s7i        0.036714            678
seed7/lib/signature.s7i      0.036130            131
seed7/lib/smtp.s7i           0.035497            261
seed7/lib/sockbase.s7i       0.033067            217
seed7/lib/socket.s7i         0.032533            326
seed7/lib/sokoban1.s7i       0.032979           1519
seed7/lib/sql_base.s7i       0.033241           1000
seed7/lib/stars.s7i          0.035252           1705
seed7/lib/stdfont10.s7i      0.033407           3347
seed7/lib/stdfont12.s7i      0.036120           3928
seed7/lib/stdfont14.s7i      0.034217           4510
seed7/lib/stdfont16.s7i      0.033812           5092
seed7/lib/stdfont18.s7i      0.034259           5868
seed7/lib/stdfont20.s7i      0.036803           6449
seed7/lib/stdfont24.s7i      0.034827           7421
seed7/lib/stdfont8.s7i       0.037034           2960
seed7/lib/stdfont9.s7i       0.035588           3152
seed7/lib/stdio.s7i          0.033934            192
seed7/lib/strifile.s7i       0.033849            345
seed7/lib/string.s7i         0.034145            779
seed7/lib/stritext.s7i       0.037024            352
seed7/lib/struct.s7i         0.036100            266
seed7/lib/struct_elem.s7i    0.035731            129
seed7/lib/subfile.s7i        0.033950            174
seed7/lib/subrange.s7i       0.034175             78
seed7/lib/syntax.s7i         0.037087            294
seed7/lib/tar.s7i            0.036461           1880
seed7/lib/tar_cmds.s7i       0.035881            752
seed7/lib/tdes.s7i           0.036063            143
seed7/lib/tee.s7i            0.033662            143
seed7/lib/text.s7i           0.033496            135
seed7/lib/tga.s7i            0.033223            676
seed7/lib/tiff.s7i           0.033557           2771
seed7/lib/time.s7i           0.032670           1191
seed7/lib/tls.s7i            0.032796           2230
seed7/lib/unicode.s7i        0.032917            575
seed7/lib/unionfnd.s7i       0.033056            130
seed7/lib/upper.s7i          0.032941            142
seed7/lib/utf16.s7i          0.032535            540
seed7/lib/utf8.s7i           0.034810            234
seed7/lib/vecfont10.s7i      0.037612           1056
seed7/lib/vecfont18.s7i      0.037345           1119
seed7/lib/vector3d.s7i       0.036516            293
seed7/lib/vectorfont.s7i     0.036402            239
seed7/lib/wildcard.s7i       0.034313            140
seed7/lib/window.s7i         0.035926            455
seed7/lib/wrinum.s7i         0.034362            248
seed7/lib/x509cert.s7i       0.033984           1243
seed7/lib/xml_ent.s7i        0.036182             94
seed7/lib/xmldom.s7i         0.037736            303
seed7/lib/xz.s7i             0.035145            442
seed7/lib/zip.s7i            0.036679           2792
seed7/lib/zstd.s7i           0.037386           1263
============================ ================= ================

Statistical Summary — Mode A (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.034479        |
+-----------+-----------------+
| Minimum   | 0.032376        |
+-----------+-----------------+
| Maximum   | 0.045546        |
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
seed7/prg/addup.sd7          0.040224            190
seed7/prg/bas7.sd7           0.040085          11459
seed7/prg/bifurk.sd7         0.037520             73
seed7/prg/bigfiles.sd7       0.042046            129
seed7/prg/brainf7.sd7        0.040648             86
seed7/prg/calc7.sd7          0.042183            128
seed7/prg/carddemo.sd7       0.042473            190
seed7/prg/castle.sd7         0.039925           3148
seed7/prg/cat.sd7            0.040211             82
seed7/prg/cellauto.sd7       0.040609             85
seed7/prg/celsius.sd7        0.036754             42
seed7/prg/chk_all.sd7        0.040357            843
seed7/prg/chkarr.sd7         0.039863           8367
seed7/prg/chkbig.sd7         0.041368          29026
seed7/prg/chkbin.sd7         0.041584           6469
seed7/prg/chkbitdata.sd7     0.046058           6624
seed7/prg/chkbool.sd7        0.039855           3157
seed7/prg/chkbst.sd7         0.038661            722
seed7/prg/chkchr.sd7         0.038235           2809
seed7/prg/chkcmd.sd7         0.036248           1205
seed7/prg/chkdb.sd7          0.038833           7454
seed7/prg/chkdecl.sd7        0.038990            448
seed7/prg/chkenum.sd7        0.036233           1230
seed7/prg/chkerr.sd7         0.038966           4663
seed7/prg/chkexc.sd7         0.036431           2627
seed7/prg/chkfil.sd7         0.037649           1615
seed7/prg/chkflt.sd7         0.039888          20620
seed7/prg/chkhent.sd7        0.035197             54
seed7/prg/chkhsh.sd7         0.037900           4548
seed7/prg/chkidx.sd7         0.041076          19567
seed7/prg/chkint.sd7         0.043609          38129
seed7/prg/chkjson.sd7        0.038932           1764
seed7/prg/chkovf.sd7         0.038158           8216
seed7/prg/chkprc.sd7         0.039055          10111
seed7/prg/chkscan.sd7        0.038708            714
seed7/prg/chkset.sd7         0.039237          11974
seed7/prg/chkstr.sd7         0.042307          26952
seed7/prg/chktime.sd7        0.039479           2025
seed7/prg/chktoml.sd7        0.044670           1656
seed7/prg/clock.sd7          0.040175             47
seed7/prg/clock2.sd7         0.039461             43
seed7/prg/clock3.sd7         0.039301             95
seed7/prg/cmpfil.sd7         0.038144             84
seed7/prg/comanche.sd7       0.040377            180
seed7/prg/confval.sd7        0.042243            175
seed7/prg/db7.sd7            0.040636            417
seed7/prg/diff7.sd7          0.041418            263
seed7/prg/dirtst.sd7         0.037228             42
seed7/prg/dirx.sd7           0.042973            152
seed7/prg/dnafight.sd7       0.041706           1381
seed7/prg/dragon.sd7         0.038881             73
seed7/prg/echo.sd7           0.035561             39
seed7/prg/eliza.sd7          0.037763            302
seed7/prg/err.sd7            0.040250             96
seed7/prg/fannkuch.sd7       0.041135            131
seed7/prg/fib.sd7            0.037155             47
seed7/prg/find7.sd7          0.041350            133
seed7/prg/findchar.sd7       0.041467            149
seed7/prg/fractree.sd7       0.037075             55
seed7/prg/ftp7.sd7           0.038700            296
seed7/prg/ftpserv.sd7        0.037243             74
seed7/prg/gcd.sd7            0.040090            109
seed7/prg/gkbd.sd7           0.040934            358
seed7/prg/gtksvtst.sd7       0.036963             94
seed7/prg/hal.sd7            0.038551            250
seed7/prg/hamu.sd7           0.039144            573
seed7/prg/hanoi.sd7          0.035585             55
seed7/prg/hd.sd7             0.041785             79
seed7/prg/hello.sd7          0.039873             32
seed7/prg/hilbert.sd7        0.042852            108
seed7/prg/ide7.sd7           0.043033            196
seed7/prg/kbd.sd7            0.037271             49
seed7/prg/klondike.sd7       0.038838            883
seed7/prg/lander.sd7         0.039148           1551
seed7/prg/lst80bas.sd7       0.038602            344
seed7/prg/lst99bas.sd7       0.038330            401
seed7/prg/lstgwbas.sd7       0.038318            577
seed7/prg/mahjong.sd7        0.038946           1943
seed7/prg/make7.sd7          0.039171            121
seed7/prg/mandelbr.sd7       0.039215            237
seed7/prg/mind.sd7           0.037791            443
seed7/prg/mirror.sd7         0.041126            131
seed7/prg/ms.sd7             0.041379            641
seed7/prg/nicoma.sd7         0.042677            135
seed7/prg/pac.sd7            0.041004            726
seed7/prg/pairs.sd7          0.040805           2025
seed7/prg/panic.sd7          0.041267           2634
seed7/prg/percolation.sd7    0.038133            330
seed7/prg/planets.sd7        0.040922           1486
seed7/prg/portfwd7.sd7       0.040908            139
seed7/prg/prime.sd7          0.041503             74
seed7/prg/printpi1.sd7       0.039531             56
seed7/prg/printpi2.sd7       0.038977             54
seed7/prg/printpi3.sd7       0.037665             60
seed7/prg/pv7.sd7            0.040971            337
seed7/prg/queen.sd7          0.040822            149
seed7/prg/rand.sd7           0.041268            121
seed7/prg/raytrace.sd7       0.040835            538
seed7/prg/rever.sd7          0.042743            816
seed7/prg/roman.sd7          0.037390             38
seed7/prg/s7c.sd7            0.040441           9060
seed7/prg/s7check.sd7        0.039614             68
seed7/prg/savehd7.sd7        0.039921           1110
seed7/prg/self.sd7           0.038913             49
seed7/prg/shisen.sd7         0.040977           1423
seed7/prg/sl.sd7             0.037599           1029
seed7/prg/snake.sd7          0.036586            615
seed7/prg/sokoban.sd7        0.037430            891
seed7/prg/spigotpi.sd7       0.036491             64
seed7/prg/sql7.sd7           0.038662            278
seed7/prg/startrek.sd7       0.041205            979
seed7/prg/sudoku7.sd7        0.041313           2657
seed7/prg/sydir7.sd7         0.040866            384
seed7/prg/syntaxhl.sd7       0.040892            177
seed7/prg/tak.sd7            0.035779             59
seed7/prg/tar7.sd7           0.038027            121
seed7/prg/tch.sd7            0.038441             55
seed7/prg/testfont.sd7       0.043716             95
seed7/prg/tet.sd7            0.039881            479
seed7/prg/tetg.sd7           0.039577            501
seed7/prg/toutf8.sd7         0.042036            240
seed7/prg/tst_cli.sd7        0.038462             40
seed7/prg/tst_srv.sd7        0.039356             47
seed7/prg/wator.sd7          0.040014            651
seed7/prg/which.sd7          0.039667             65
seed7/prg/wiz.sd7            0.041404           2833
seed7/prg/wordcnt.sd7        0.038437             54
seed7/prg/wrinum.sd7         0.043277             43
seed7/prg/wumpus.sd7         0.041960            372
seed7/lib/aes.s7i            0.045700           1144
seed7/lib/aes_gcm.s7i        0.043021            392
seed7/lib/ar.s7i             0.041500           1532
seed7/lib/arc4.s7i           0.042113            144
seed7/lib/archive.s7i        0.042828            143
seed7/lib/archive_base.s7i   0.041578            135
seed7/lib/array.s7i          0.041636            610
seed7/lib/asn1.s7i           0.039908            544
seed7/lib/asn1oid.s7i        0.045043            157
seed7/lib/basearray.s7i      0.040343            450
seed7/lib/bigfile.s7i        0.042353            136
seed7/lib/bigint.s7i         0.041480            824
seed7/lib/bigrat.s7i         0.038325            784
seed7/lib/bin16.s7i          0.038491            592
seed7/lib/bin32.s7i          0.039460            490
seed7/lib/bin64.s7i          0.038427            539
seed7/lib/bitdata.s7i        0.045545           1330
seed7/lib/bitmapfont.s7i     0.041218            215
seed7/lib/bitset.s7i         0.044649            593
seed7/lib/bitsetof.s7i       0.042122            431
seed7/lib/blowfish.s7i       0.046031            383
seed7/lib/bmp.s7i            0.043149            924
seed7/lib/boolean.s7i        0.038277            403
seed7/lib/browser.s7i        0.039800            280
seed7/lib/bstring.s7i        0.039125            227
seed7/lib/bytedata.s7i       0.039679            482
seed7/lib/bzip2.s7i          0.041624            887
seed7/lib/cards.s7i          0.037816           1342
seed7/lib/category.s7i       0.041697            209
seed7/lib/cc_conf.s7i        0.046663           1314
seed7/lib/ccittfax.s7i       0.043466           1022
seed7/lib/cgi.s7i            0.044603            109
seed7/lib/cgidialog.s7i      0.046618           1118
seed7/lib/char.s7i           0.047892            356
seed7/lib/charsets.s7i       0.044910           2024
seed7/lib/chartype.s7i       0.049282            121
seed7/lib/cipher.s7i         0.041719            146
seed7/lib/cli_cmds.s7i       0.038933           1360
seed7/lib/clib_file.s7i      0.042557            301
seed7/lib/color.s7i          0.049120            185
seed7/lib/complex.s7i        0.044605            464
seed7/lib/compress.s7i       0.044964            150
seed7/lib/console.s7i        0.041548            188
seed7/lib/cpio.s7i           0.040889           1708
seed7/lib/crc32.s7i          0.041267            193
seed7/lib/cronos16.s7i       0.041415           1173
seed7/lib/cronos27.s7i       0.045511           1464
seed7/lib/csv.s7i            0.045815            201
seed7/lib/db_prop.s7i        0.045959            991
seed7/lib/deflate.s7i        0.042665            740
seed7/lib/des.s7i            0.042191            444
seed7/lib/dialog.s7i         0.038993            311
seed7/lib/dir.s7i            0.037925            163
seed7/lib/draw.s7i           0.037856            854
seed7/lib/duration.s7i       0.038533           1038
seed7/lib/echo.s7i           0.038759            132
seed7/lib/editline.s7i       0.038711            398
seed7/lib/elf.s7i            0.038830           1560
seed7/lib/elliptic.s7i       0.036920            649
seed7/lib/enable_io.s7i      0.037711            312
seed7/lib/encoding.s7i       0.038294            931
seed7/lib/enumeration.s7i    0.037871            236
seed7/lib/environment.s7i    0.037996            175
seed7/lib/exif.s7i           0.039282            152
seed7/lib/external_file.s7i  0.039470            340
seed7/lib/field.s7i          0.039053            268
seed7/lib/file.s7i           0.038605            372
seed7/lib/filebits.s7i       0.037180             46
seed7/lib/filesys.s7i        0.039295            601
seed7/lib/fileutil.s7i       0.045228            144
seed7/lib/fixarray.s7i       0.045174            307
seed7/lib/float.s7i          0.040652            757
seed7/lib/font.s7i           0.040463            196
seed7/lib/font8x8.s7i        0.037834            998
seed7/lib/forloop.s7i        0.038821            449
seed7/lib/ftp.s7i            0.039059            969
seed7/lib/ftpserv.s7i        0.040257            631
seed7/lib/getf.s7i           0.039612            115
seed7/lib/gethttp.s7i        0.036495             41
seed7/lib/gethttps.s7i       0.037815             41
seed7/lib/gif.s7i            0.039306            561
seed7/lib/graph.s7i          0.040916            415
seed7/lib/graph_file.s7i     0.038911            399
seed7/lib/gtkserver.s7i      0.039788            161
seed7/lib/gzip.s7i           0.039238            573
seed7/lib/hash.s7i           0.040051            421
seed7/lib/hashsetof.s7i      0.039330            499
seed7/lib/hmac.s7i           0.039997            152
seed7/lib/html.s7i           0.036768             83
seed7/lib/html_ent.s7i       0.037800            476
seed7/lib/htmldom.s7i        0.041563            286
seed7/lib/http_request.s7i   0.047115            696
seed7/lib/http_srv_resp.s7i  0.048300            380
seed7/lib/https_request.s7i  0.046163            211
seed7/lib/httpserv.s7i       0.046255            345
seed7/lib/huffman.s7i        0.047296            644
seed7/lib/ico.s7i            0.039701            221
seed7/lib/idxarray.s7i       0.039371            232
seed7/lib/image.s7i          0.037989            156
seed7/lib/imagefile.s7i      0.039634            171
seed7/lib/inflate.s7i        0.040640            411
seed7/lib/inifile.s7i        0.038819            129
seed7/lib/integer.s7i        0.038837            663
seed7/lib/iobuffer.s7i       0.038867            289
seed7/lib/jpeg.s7i           0.039334           1761
seed7/lib/json.s7i           0.038732            891
seed7/lib/json_serde.s7i     0.038950            783
seed7/lib/keybd.s7i          0.042022            639
seed7/lib/keydescr.s7i       0.040753            192
seed7/lib/leb128.s7i         0.040950            218
seed7/lib/line.s7i           0.038411            164
seed7/lib/listener.s7i       0.038628            247
seed7/lib/logfile.s7i        0.036556             73
seed7/lib/lower.s7i          0.037186            142
seed7/lib/lzma.s7i           0.038348            934
seed7/lib/lzw.s7i            0.037749            861
seed7/lib/magic.s7i          0.039432            403
seed7/lib/mahjng32.s7i       0.039936           1500
seed7/lib/make.s7i           0.038836            544
seed7/lib/makedata.s7i       0.039551           1428
seed7/lib/math.s7i           0.039231            201
seed7/lib/mixarith.s7i       0.038696            249
seed7/lib/modern27.s7i       0.041814           1099
seed7/lib/more.s7i           0.047002            130
seed7/lib/msgdigest.s7i      0.042960           1222
seed7/lib/multiscr.s7i       0.043069             68
seed7/lib/null_file.s7i      0.042792            345
seed7/lib/osfiles.s7i        0.041334           1085
seed7/lib/pbm.s7i            0.041704            230
seed7/lib/pcx.s7i            0.038130            638
seed7/lib/pem.s7i            0.037901            185
seed7/lib/pgm.s7i            0.038126            238
seed7/lib/pic16.s7i          0.035828           1037
seed7/lib/pic32.s7i          0.037719           2060
seed7/lib/pic_util.s7i       0.037662            144
seed7/lib/pixelimage.s7i     0.037314            320
seed7/lib/pixmap_file.s7i    0.038859            459
seed7/lib/pixmapfont.s7i     0.039236            184
seed7/lib/pkcs1.s7i          0.043769            543
seed7/lib/png.s7i            0.039091           1064
seed7/lib/poll.s7i           0.037445            313
seed7/lib/ppm.s7i            0.039092            240
seed7/lib/process.s7i        0.037611            541
seed7/lib/progs.s7i          0.038634            789
seed7/lib/propertyfile.s7i   0.040116            155
seed7/lib/rational.s7i       0.039078            792
seed7/lib/ref_list.s7i       0.039184            252
seed7/lib/reference.s7i      0.040082            126
seed7/lib/reverse.s7i        0.038112             94
seed7/lib/rpm.s7i            0.039995           3487
seed7/lib/rpmext.s7i         0.038725            318
seed7/lib/scanfile.s7i       0.039364           1779
seed7/lib/scanjson.s7i       0.042996            413
seed7/lib/scanstri.s7i       0.040399           1814
seed7/lib/scantoml.s7i       0.040065           1603
seed7/lib/seed7_05.s7i       0.041218           1072
seed7/lib/set.s7i            0.037306             57
seed7/lib/shell.s7i          0.038780            615
seed7/lib/showtls.s7i        0.039181            678
seed7/lib/signature.s7i      0.039620            131
seed7/lib/smtp.s7i           0.038776            261
seed7/lib/sockbase.s7i       0.038945            217
seed7/lib/socket.s7i         0.039413            326
seed7/lib/sokoban1.s7i       0.037334           1519
seed7/lib/sql_base.s7i       0.038733           1000
seed7/lib/stars.s7i          0.038998           1705
seed7/lib/stdfont10.s7i      0.036332           3347
seed7/lib/stdfont12.s7i      0.037154           3928
seed7/lib/stdfont14.s7i      0.036461           4510
seed7/lib/stdfont16.s7i      0.036452           5092
seed7/lib/stdfont18.s7i      0.036614           5868
seed7/lib/stdfont20.s7i      0.039623           6449
seed7/lib/stdfont24.s7i      0.039558           7421
seed7/lib/stdfont8.s7i       0.037590           2960
seed7/lib/stdfont9.s7i       0.037517           3152
seed7/lib/stdio.s7i          0.038443            192
seed7/lib/strifile.s7i       0.038804            345
seed7/lib/string.s7i         0.039498            779
seed7/lib/stritext.s7i       0.041875            352
seed7/lib/struct.s7i         0.042319            266
seed7/lib/struct_elem.s7i    0.038067            129
seed7/lib/subfile.s7i        0.039097            174
seed7/lib/subrange.s7i       0.037904             78
seed7/lib/syntax.s7i         0.040634            294
seed7/lib/tar.s7i            0.044318           1880
seed7/lib/tar_cmds.s7i       0.044963            752
seed7/lib/tdes.s7i           0.041836            143
seed7/lib/tee.s7i            0.040446            143
seed7/lib/text.s7i           0.042649            135
seed7/lib/tga.s7i            0.044176            676
seed7/lib/tiff.s7i           0.039510           2771
seed7/lib/time.s7i           0.039706           1191
seed7/lib/tls.s7i            0.043038           2230
seed7/lib/unicode.s7i        0.048589            575
seed7/lib/unionfnd.s7i       0.042313            130
seed7/lib/upper.s7i          0.037524            142
seed7/lib/utf16.s7i          0.038770            540
seed7/lib/utf8.s7i           0.040136            234
seed7/lib/vecfont10.s7i      0.039253           1056
seed7/lib/vecfont18.s7i      0.040446           1119
seed7/lib/vector3d.s7i       0.037300            293
seed7/lib/vectorfont.s7i     0.039096            239
seed7/lib/wildcard.s7i       0.040381            140
seed7/lib/window.s7i         0.041589            455
seed7/lib/wrinum.s7i         0.039350            248
seed7/lib/x509cert.s7i       0.038766           1243
seed7/lib/xml_ent.s7i        0.040736             94
seed7/lib/xmldom.s7i         0.038647            303
seed7/lib/xz.s7i             0.039546            442
seed7/lib/zip.s7i            0.039379           2792
seed7/lib/zstd.s7i           0.039399           1263
============================ ================= ================

Statistical Summary — Mode B (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.040174        |
+-----------+-----------------+
| Minimum   | 0.035197        |
+-----------+-----------------+
| Maximum   | 0.049282        |
+-----------+-----------------+

Mode C — Mode Activation + Full-Buffer Fontification (font-lock-ensure)
-----------------------------------------------------------------------

Full-buffer fontification stress test.

File Load Times — Mode C (mean, GC-free)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

============================ ================= ================
File Name                    Mean Time (s)     Line count
============================ ================= ================
seed7/prg/addup.sd7          0.039589            190
seed7/prg/bas7.sd7           0.322439          11459
seed7/prg/bifurk.sd7         0.035897             73
seed7/prg/bigfiles.sd7       0.036494            129
seed7/prg/brainf7.sd7        0.036122             86
seed7/prg/calc7.sd7          0.037410            128
seed7/prg/carddemo.sd7       0.038996            190
seed7/prg/castle.sd7         0.112756           3148
seed7/prg/cat.sd7            0.037286             82
seed7/prg/cellauto.sd7       0.036792             85
seed7/prg/celsius.sd7        0.035577             42
seed7/prg/chk_all.sd7        0.059473            843
seed7/prg/chkarr.sd7         0.359256           8367
seed7/prg/chkbig.sd7         2.056914          29026
seed7/prg/chkbin.sd7         0.510258           6469
seed7/prg/chkbitdata.sd7     0.615628           6624
seed7/prg/chkbool.sd7        0.123608           3157
seed7/prg/chkbst.sd7         0.069098            722
seed7/prg/chkchr.sd7         0.214328           2809
seed7/prg/chkcmd.sd7         0.069609           1205
seed7/prg/chkdb.sd7          0.350256           7454
seed7/prg/chkdecl.sd7        0.058512            448
seed7/prg/chkenum.sd7        0.067545           1230
seed7/prg/chkerr.sd7         0.195035           4663
seed7/prg/chkexc.sd7         0.082477           2627
seed7/prg/chkfil.sd7         0.073460           1615
seed7/prg/chkflt.sd7         1.337076          20620
seed7/prg/chkhent.sd7        0.038506             54
seed7/prg/chkhsh.sd7         0.249794           4548
seed7/prg/chkidx.sd7         1.336020          19567
seed7/prg/chkint.sd7         2.519594          38129
seed7/prg/chkjson.sd7        0.100987           1764
seed7/prg/chkovf.sd7         0.565448           8216
seed7/prg/chkprc.sd7         0.328563          10111
seed7/prg/chkscan.sd7        0.055464            714
seed7/prg/chkset.sd7         0.669403          11974
seed7/prg/chkstr.sd7         1.386190          26952
seed7/prg/chktime.sd7        0.134414           2025
seed7/prg/chktoml.sd7        0.107542           1656
seed7/prg/clock.sd7          0.043536             47
seed7/prg/clock2.sd7         0.038803             43
seed7/prg/clock3.sd7         0.038039             95
seed7/prg/cmpfil.sd7         0.036538             84
seed7/prg/comanche.sd7       0.041278            180
seed7/prg/confval.sd7        0.043363            175
seed7/prg/db7.sd7            0.049715            417
seed7/prg/diff7.sd7          0.042367            263
seed7/prg/dirtst.sd7         0.037053             42
seed7/prg/dirx.sd7           0.041102            152
seed7/prg/dnafight.sd7       0.067242           1381
seed7/prg/dragon.sd7         0.037068             73
seed7/prg/echo.sd7           0.036531             39
seed7/prg/eliza.sd7          0.043017            302
seed7/prg/err.sd7            0.041396             96
seed7/prg/fannkuch.sd7       0.037271            131
seed7/prg/fib.sd7            0.034924             47
seed7/prg/find7.sd7          0.036992            133
seed7/prg/findchar.sd7       0.037289            149
seed7/prg/fractree.sd7       0.034586             55
seed7/prg/ftp7.sd7           0.042585            296
seed7/prg/ftpserv.sd7        0.038138             74
seed7/prg/gcd.sd7            0.036757            109
seed7/prg/gkbd.sd7           0.045893            358
seed7/prg/gtksvtst.sd7       0.036291             94
seed7/prg/hal.sd7            0.040080            250
seed7/prg/hamu.sd7           0.048119            573
seed7/prg/hanoi.sd7          0.037061             55
seed7/prg/hd.sd7             0.037231             79
seed7/prg/hello.sd7          0.036317             32
seed7/prg/hilbert.sd7        0.038141            108
seed7/prg/ide7.sd7           0.042631            196
seed7/prg/kbd.sd7            0.038128             49
seed7/prg/klondike.sd7       0.057840            883
seed7/prg/lander.sd7         0.071607           1551
seed7/prg/lst80bas.sd7       0.044495            344
seed7/prg/lst99bas.sd7       0.051973            401
seed7/prg/lstgwbas.sd7       0.055294            577
seed7/prg/mahjong.sd7        0.081862           1943
seed7/prg/make7.sd7          0.037899            121
seed7/prg/mandelbr.sd7       0.040391            237
seed7/prg/mind.sd7           0.044116            443
seed7/prg/mirror.sd7         0.038337            131
seed7/prg/ms.sd7             0.047690            641
seed7/prg/nicoma.sd7         0.039967            135
seed7/prg/pac.sd7            0.051355            726
seed7/prg/pairs.sd7          0.085965           2025
seed7/prg/panic.sd7          0.097724           2634
seed7/prg/percolation.sd7    0.043543            330
seed7/prg/planets.sd7        0.077282           1486
seed7/prg/portfwd7.sd7       0.039311            139
seed7/prg/prime.sd7          0.036412             74
seed7/prg/printpi1.sd7       0.036797             56
seed7/prg/printpi2.sd7       0.036692             54
seed7/prg/printpi3.sd7       0.036833             60
seed7/prg/pv7.sd7            0.044819            337
seed7/prg/queen.sd7          0.041491            149
seed7/prg/rand.sd7           0.039880            121
seed7/prg/raytrace.sd7       0.050009            538
seed7/prg/rever.sd7          0.054423            816
seed7/prg/roman.sd7          0.036551             38
seed7/prg/s7c.sd7            0.279231           9060
seed7/prg/s7check.sd7        0.038719             68
seed7/prg/savehd7.sd7        0.067672           1110
seed7/prg/self.sd7           0.036607             49
seed7/prg/shisen.sd7         0.072002           1423
seed7/prg/sl.sd7             0.059782           1029
seed7/prg/snake.sd7          0.046643            615
seed7/prg/sokoban.sd7        0.054361            891
seed7/prg/spigotpi.sd7       0.036662             64
seed7/prg/sql7.sd7           0.041694            278
seed7/prg/startrek.sd7       0.060353            979
seed7/prg/sudoku7.sd7        0.102559           2657
seed7/prg/sydir7.sd7         0.045644            384
seed7/prg/syntaxhl.sd7       0.040925            177
seed7/prg/tak.sd7            0.036151             59
seed7/prg/tar7.sd7           0.041498            121
seed7/prg/tch.sd7            0.037688             55
seed7/prg/testfont.sd7       0.039861             95
seed7/prg/tet.sd7            0.045831            479
seed7/prg/tetg.sd7           0.045280            501
seed7/prg/toutf8.sd7         0.041889            240
seed7/prg/tst_cli.sd7        0.040191             40
seed7/prg/tst_srv.sd7        0.037519             47
seed7/prg/wator.sd7          0.050860            651
seed7/prg/which.sd7          0.035643             65
seed7/prg/wiz.sd7            0.103978           2833
seed7/prg/wordcnt.sd7        0.034766             54
seed7/prg/wrinum.sd7         0.035065             43
seed7/prg/wumpus.sd7         0.042682            372
seed7/lib/aes.s7i            0.110170           1144
seed7/lib/aes_gcm.s7i        0.048165            392
seed7/lib/ar.s7i             0.075113           1532
seed7/lib/arc4.s7i           0.038901            144
seed7/lib/archive.s7i        0.038707            143
seed7/lib/archive_base.s7i   0.038695            135
seed7/lib/array.s7i          0.054356            610
seed7/lib/asn1.s7i           0.046306            544
seed7/lib/asn1oid.s7i        0.040643            157
seed7/lib/basearray.s7i      0.047301            450
seed7/lib/bigfile.s7i        0.037178            136
seed7/lib/bigint.s7i         0.053560            824
seed7/lib/bigrat.s7i         0.055378            784
seed7/lib/bin16.s7i          0.054702            592
seed7/lib/bin32.s7i          0.050500            490
seed7/lib/bin64.s7i          0.050817            539
seed7/lib/bitdata.s7i        0.079632           1330
seed7/lib/bitmapfont.s7i     0.041853            215
seed7/lib/bitset.s7i         0.050331            593
seed7/lib/bitsetof.s7i       0.048443            431
seed7/lib/blowfish.s7i       0.055521            383
seed7/lib/bmp.s7i            0.059776            924
seed7/lib/boolean.s7i        0.043139            403
seed7/lib/browser.s7i        0.041364            280
seed7/lib/bstring.s7i        0.040487            227
seed7/lib/bytedata.s7i       0.049998            482
seed7/lib/bzip2.s7i          0.060722            887
seed7/lib/cards.s7i          0.066951           1342
seed7/lib/category.s7i       0.041474            209
seed7/lib/cc_conf.s7i        0.076530           1314
seed7/lib/ccittfax.s7i       0.065139           1022
seed7/lib/cgi.s7i            0.038931            109
seed7/lib/cgidialog.s7i      0.069346           1118
seed7/lib/char.s7i           0.050348            356
seed7/lib/charsets.s7i       0.084218           2024
seed7/lib/chartype.s7i       0.040845            121
seed7/lib/cipher.s7i         0.041861            146
seed7/lib/cli_cmds.s7i       0.073663           1360
seed7/lib/clib_file.s7i      0.046086            301
seed7/lib/color.s7i          0.040789            185
seed7/lib/complex.s7i        0.045742            464
seed7/lib/compress.s7i       0.039285            150
seed7/lib/console.s7i        0.040976            188
seed7/lib/cpio.s7i           0.082000           1708
seed7/lib/crc32.s7i          0.045303            193
seed7/lib/cronos16.s7i       0.092220           1173
seed7/lib/cronos27.s7i       0.114407           1464
seed7/lib/csv.s7i            0.040532            201
seed7/lib/db_prop.s7i        0.063386            991
seed7/lib/deflate.s7i        0.056123            740
seed7/lib/des.s7i            0.055481            444
seed7/lib/dialog.s7i         0.044368            311
seed7/lib/dir.s7i            0.038519            163
seed7/lib/draw.s7i           0.056335            854
seed7/lib/duration.s7i       0.061439           1038
seed7/lib/echo.s7i           0.039292            132
seed7/lib/editline.s7i       0.045088            398
seed7/lib/elf.s7i            0.086778           1560
seed7/lib/elliptic.s7i       0.054747            649
seed7/lib/enable_io.s7i      0.046174            312
seed7/lib/encoding.s7i       0.063466            931
seed7/lib/enumeration.s7i    0.042279            236
seed7/lib/environment.s7i    0.039277            175
seed7/lib/exif.s7i           0.040046            152
seed7/lib/external_file.s7i  0.044002            340
seed7/lib/field.s7i          0.041864            268
seed7/lib/file.s7i           0.043856            372
seed7/lib/filebits.s7i       0.037305             46
seed7/lib/filesys.s7i        0.050079            601
seed7/lib/fileutil.s7i       0.040412            144
seed7/lib/fixarray.s7i       0.043999            307
seed7/lib/float.s7i          0.056097            757
seed7/lib/font.s7i           0.040342            196
seed7/lib/font8x8.s7i        0.049519            998
seed7/lib/forloop.s7i        0.046424            449
seed7/lib/ftp.s7i            0.057814            969
seed7/lib/ftpserv.s7i        0.051732            631
seed7/lib/getf.s7i           0.037769            115
seed7/lib/gethttp.s7i        0.036082             41
seed7/lib/gethttps.s7i       0.036945             41
seed7/lib/gif.s7i            0.049718            561
seed7/lib/graph.s7i          0.049018            415
seed7/lib/graph_file.s7i     0.045580            399
seed7/lib/gtkserver.s7i      0.039845            161
seed7/lib/gzip.s7i           0.052390            573
seed7/lib/hash.s7i           0.050521            421
seed7/lib/hashsetof.s7i      0.053664            499
seed7/lib/hmac.s7i           0.042406            152
seed7/lib/html.s7i           0.037459             83
seed7/lib/html_ent.s7i       0.049145            476
seed7/lib/htmldom.s7i        0.043136            286
seed7/lib/http_request.s7i   0.051844            696
seed7/lib/http_srv_resp.s7i  0.048440            380
seed7/lib/https_request.s7i  0.045257            211
seed7/lib/httpserv.s7i       0.047258            345
seed7/lib/huffman.s7i        0.054609            644
seed7/lib/ico.s7i            0.046106            221
seed7/lib/idxarray.s7i       0.049598            232
seed7/lib/image.s7i          0.039848            156
seed7/lib/imagefile.s7i      0.044909            171
seed7/lib/inflate.s7i        0.051746            411
seed7/lib/inifile.s7i        0.038747            129
seed7/lib/integer.s7i        0.052921            663
seed7/lib/iobuffer.s7i       0.041597            289
seed7/lib/jpeg.s7i           0.082982           1761
seed7/lib/json.s7i           0.055078            891
seed7/lib/json_serde.s7i     0.053146            783
seed7/lib/keybd.s7i          0.067263            639
seed7/lib/keydescr.s7i       0.046591            192
seed7/lib/leb128.s7i         0.040143            218
seed7/lib/line.s7i           0.039691            164
seed7/lib/listener.s7i       0.041953            247
seed7/lib/logfile.s7i        0.037402             73
seed7/lib/lower.s7i          0.041307            142
seed7/lib/lzma.s7i           0.062867            934
seed7/lib/lzw.s7i            0.060474            861
seed7/lib/magic.s7i          0.058065            403
seed7/lib/mahjng32.s7i       0.071484           1500
seed7/lib/make.s7i           0.053929            544
seed7/lib/makedata.s7i       0.072476           1428
seed7/lib/math.s7i           0.041649            201
seed7/lib/mixarith.s7i       0.040422            249
seed7/lib/modern27.s7i       0.084737           1099
seed7/lib/more.s7i           0.038702            130
seed7/lib/msgdigest.s7i      0.079722           1222
seed7/lib/multiscr.s7i       0.037409             68
seed7/lib/null_file.s7i      0.043652            345
seed7/lib/osfiles.s7i        0.067824           1085
seed7/lib/pbm.s7i            0.040477            230
seed7/lib/pcx.s7i            0.051085            638
seed7/lib/pem.s7i            0.039043            185
seed7/lib/pgm.s7i            0.040225            238
seed7/lib/pic16.s7i          0.049005           1037
seed7/lib/pic32.s7i          0.080617           2060
seed7/lib/pic_util.s7i       0.039422            144
seed7/lib/pixelimage.s7i     0.046285            320
seed7/lib/pixmap_file.s7i    0.054759            459
seed7/lib/pixmapfont.s7i     0.048499            184
seed7/lib/pkcs1.s7i          0.062114            543
seed7/lib/png.s7i            0.064090           1064
seed7/lib/poll.s7i           0.044933            313
seed7/lib/ppm.s7i            0.040011            240
seed7/lib/process.s7i        0.050385            541
seed7/lib/progs.s7i          0.056363            789
seed7/lib/propertyfile.s7i   0.041868            155
seed7/lib/rational.s7i       0.053583            792
seed7/lib/ref_list.s7i       0.041486            252
seed7/lib/reference.s7i      0.039407            126
seed7/lib/reverse.s7i        0.037590             94
seed7/lib/rpm.s7i            0.142008           3487
seed7/lib/rpmext.s7i         0.042038            318
seed7/lib/scanfile.s7i       0.080818           1779
seed7/lib/scanjson.s7i       0.048518            413
seed7/lib/scanstri.s7i       0.079671           1814
seed7/lib/scantoml.s7i       0.073096           1603
seed7/lib/seed7_05.s7i       0.068541           1072
seed7/lib/set.s7i            0.037354             57
seed7/lib/shell.s7i          0.055630            615
seed7/lib/showtls.s7i        0.063857            678
seed7/lib/signature.s7i      0.041023            131
seed7/lib/smtp.s7i           0.043156            261
seed7/lib/sockbase.s7i       0.045288            217
seed7/lib/socket.s7i         0.045170            326
seed7/lib/sokoban1.s7i       0.055937           1519
seed7/lib/sql_base.s7i       0.064351           1000
seed7/lib/stars.s7i          0.136342           1705
seed7/lib/stdfont10.s7i      0.080937           3347
seed7/lib/stdfont12.s7i      0.090951           3928
seed7/lib/stdfont14.s7i      0.108754           4510
seed7/lib/stdfont16.s7i      0.118032           5092
seed7/lib/stdfont18.s7i      0.133994           5868
seed7/lib/stdfont20.s7i      0.150654           6449
seed7/lib/stdfont24.s7i      0.181111           7421
seed7/lib/stdfont8.s7i       0.075182           2960
seed7/lib/stdfont9.s7i       0.074868           3152
seed7/lib/stdio.s7i          0.039073            192
seed7/lib/strifile.s7i       0.042728            345
seed7/lib/string.s7i         0.054230            779
seed7/lib/stritext.s7i       0.042999            352
seed7/lib/struct.s7i         0.042787            266
seed7/lib/struct_elem.s7i    0.038890            129
seed7/lib/subfile.s7i        0.038776            174
seed7/lib/subrange.s7i       0.036418             78
seed7/lib/syntax.s7i         0.045776            294
seed7/lib/tar.s7i            0.081742           1880
seed7/lib/tar_cmds.s7i       0.058215            752
seed7/lib/tdes.s7i           0.039321            143
seed7/lib/tee.s7i            0.038877            143
seed7/lib/text.s7i           0.038511            135
seed7/lib/tga.s7i            0.055026            676
seed7/lib/tiff.s7i           0.123744           2771
seed7/lib/time.s7i           0.063753           1191
seed7/lib/tls.s7i            0.105132           2230
seed7/lib/unicode.s7i        0.053077            575
seed7/lib/unionfnd.s7i       0.038860            130
seed7/lib/upper.s7i          0.039005            142
seed7/lib/utf16.s7i          0.050746            540
seed7/lib/utf8.s7i           0.041650            234
seed7/lib/vecfont10.s7i      0.080593           1056
seed7/lib/vecfont18.s7i      0.087404           1119
seed7/lib/vector3d.s7i       0.040964            293
seed7/lib/vectorfont.s7i     0.040958            239
seed7/lib/wildcard.s7i       0.037023            140
seed7/lib/window.s7i         0.044605            455
seed7/lib/wrinum.s7i         0.039726            248
seed7/lib/x509cert.s7i       0.070736           1243
seed7/lib/xml_ent.s7i        0.037564             94
seed7/lib/xmldom.s7i         0.041709            303
seed7/lib/xz.s7i             0.046758            442
seed7/lib/zip.s7i            0.121963           2792
seed7/lib/zstd.s7i           0.069665           1263
============================ ================= ================

Statistical Summary — Mode C (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.090233        |
+-----------+-----------------+
| Minimum   | 0.034586        |
+-----------+-----------------+
| Maximum   | 2.519594        |
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
seed7/prg/addup.sd7          0.043925            190
seed7/prg/bas7.sd7           0.776081          11459
seed7/prg/bifurk.sd7         0.038322             73
seed7/prg/bigfiles.sd7       0.041047            129
seed7/prg/brainf7.sd7        0.041280             86
seed7/prg/calc7.sd7          0.041155            128
seed7/prg/carddemo.sd7       0.047451            190
seed7/prg/castle.sd7         0.220016           3148
seed7/prg/cat.sd7            0.038476             82
seed7/prg/cellauto.sd7       0.042480             85
seed7/prg/celsius.sd7        0.042537             42
seed7/prg/chk_all.sd7        0.089442            843
seed7/prg/chkarr.sd7         0.878199           8367
seed7/prg/chkbig.sd7         4.162132          29026
seed7/prg/chkbin.sd7         1.040329           6469
seed7/prg/chkbitdata.sd7     1.250073           6624
seed7/prg/chkbool.sd7        0.230234           3157
seed7/prg/chkbst.sd7         0.101835            722
seed7/prg/chkchr.sd7         0.478209           2809
seed7/prg/chkcmd.sd7         0.110205           1205
seed7/prg/chkdb.sd7          0.758433           7454
seed7/prg/chkdecl.sd7        0.094968            448
seed7/prg/chkenum.sd7        0.128171           1230
seed7/prg/chkerr.sd7         0.344143           4663
seed7/prg/chkexc.sd7         0.147319           2627
seed7/prg/chkfil.sd7         0.127951           1615
seed7/prg/chkflt.sd7         2.831005          20620
seed7/prg/chkhent.sd7        0.041763             54
seed7/prg/chkhsh.sd7         0.508937           4548
seed7/prg/chkidx.sd7         3.202210          19567
seed7/prg/chkint.sd7         5.598503          38129
seed7/prg/chkjson.sd7        0.196772           1764
seed7/prg/chkovf.sd7         1.212565           8216
seed7/prg/chkprc.sd7         0.697018          10111
seed7/prg/chkscan.sd7        0.090185            714
seed7/prg/chkset.sd7         1.750567          11974
seed7/prg/chkstr.sd7         3.460525          26952
seed7/prg/chktime.sd7        0.248998           2025
seed7/prg/chktoml.sd7        0.193584           1656
seed7/prg/clock.sd7          0.043275             47
seed7/prg/clock2.sd7         0.038869             43
seed7/prg/clock3.sd7         0.044063             95
seed7/prg/cmpfil.sd7         0.041253             84
seed7/prg/comanche.sd7       0.048363            180
seed7/prg/confval.sd7        0.052060            175
seed7/prg/db7.sd7            0.063141            417
seed7/prg/diff7.sd7          0.052073            263
seed7/prg/dirtst.sd7         0.039196             42
seed7/prg/dirx.sd7           0.047443            152
seed7/prg/dnafight.sd7       0.122515           1381
seed7/prg/dragon.sd7         0.040818             73
seed7/prg/echo.sd7           0.040924             39
seed7/prg/eliza.sd7          0.051778            302
seed7/prg/err.sd7            0.044163             96
seed7/prg/fannkuch.sd7       0.046641            131
seed7/prg/fib.sd7            0.044875             47
seed7/prg/find7.sd7          0.046966            133
seed7/prg/findchar.sd7       0.046977            149
seed7/prg/fractree.sd7       0.041144             55
seed7/prg/ftp7.sd7           0.055580            296
seed7/prg/ftpserv.sd7        0.040524             74
seed7/prg/gcd.sd7            0.039861            109
seed7/prg/gkbd.sd7           0.060037            358
seed7/prg/gtksvtst.sd7       0.038404             94
seed7/prg/hal.sd7            0.052161            250
seed7/prg/hamu.sd7           0.072559            573
seed7/prg/hanoi.sd7          0.039598             55
seed7/prg/hd.sd7             0.038371             79
seed7/prg/hello.sd7          0.037938             32
seed7/prg/hilbert.sd7        0.041577            108
seed7/prg/ide7.sd7           0.047336            196
seed7/prg/kbd.sd7            0.037085             49
seed7/prg/klondike.sd7       0.086829            883
seed7/prg/lander.sd7         0.131706           1551
seed7/prg/lst80bas.sd7       0.061199            344
seed7/prg/lst99bas.sd7       0.062641            401
seed7/prg/lstgwbas.sd7       0.073870            577
seed7/prg/mahjong.sd7        0.154398           1943
seed7/prg/make7.sd7          0.042153            121
seed7/prg/mandelbr.sd7       0.048077            237
seed7/prg/mind.sd7           0.057849            443
seed7/prg/mirror.sd7         0.042226            131
seed7/prg/ms.sd7             0.070089            641
seed7/prg/nicoma.sd7         0.042739            135
seed7/prg/pac.sd7            0.074242            726
seed7/prg/pairs.sd7          0.139648           2025
seed7/prg/panic.sd7          0.196563           2634
seed7/prg/percolation.sd7    0.057363            330
seed7/prg/planets.sd7        0.136809           1486
seed7/prg/portfwd7.sd7       0.044922            139
seed7/prg/prime.sd7          0.038907             74
seed7/prg/printpi1.sd7       0.037691             56
seed7/prg/printpi2.sd7       0.037353             54
seed7/prg/printpi3.sd7       0.036692             60
seed7/prg/pv7.sd7            0.056372            337
seed7/prg/queen.sd7          0.049838            149
seed7/prg/rand.sd7           0.048522            121
seed7/prg/raytrace.sd7       0.074041            538
seed7/prg/rever.sd7          0.085204            816
seed7/prg/roman.sd7          0.045123             38
seed7/prg/s7c.sd7            0.638732           9060
seed7/prg/s7check.sd7        0.038444             68
seed7/prg/savehd7.sd7        0.109604           1110
seed7/prg/self.sd7           0.038493             49
seed7/prg/shisen.sd7         0.125085           1423
seed7/prg/sl.sd7             0.102017           1029
seed7/prg/snake.sd7          0.071659            615
seed7/prg/sokoban.sd7        0.083501            891
seed7/prg/spigotpi.sd7       0.038714             64
seed7/prg/sql7.sd7           0.053923            278
seed7/prg/startrek.sd7       0.094661            979
seed7/prg/sudoku7.sd7        0.203914           2657
seed7/prg/sydir7.sd7         0.065003            384
seed7/prg/syntaxhl.sd7       0.055544            177
seed7/prg/tak.sd7            0.043146             59
seed7/prg/tar7.sd7           0.044589            121
seed7/prg/tch.sd7            0.040506             55
seed7/prg/testfont.sd7       0.044205             95
seed7/prg/tet.sd7            0.059355            479
seed7/prg/tetg.sd7           0.064139            501
seed7/prg/toutf8.sd7         0.050270            240
seed7/prg/tst_cli.sd7        0.035964             40
seed7/prg/tst_srv.sd7        0.037694             47
seed7/prg/wator.sd7          0.078834            651
seed7/prg/which.sd7          0.038635             65
seed7/prg/wiz.sd7            0.208004           2833
seed7/prg/wordcnt.sd7        0.038277             54
seed7/prg/wrinum.sd7         0.037181             43
seed7/prg/wumpus.sd7         0.054665            372
seed7/lib/aes.s7i            0.205505           1144
seed7/lib/aes_gcm.s7i        0.059950            392
seed7/lib/ar.s7i             0.124510           1532
seed7/lib/arc4.s7i           0.041851            144
seed7/lib/archive.s7i        0.045867            143
seed7/lib/archive_base.s7i   0.043801            135
seed7/lib/array.s7i          0.074185            610
seed7/lib/asn1.s7i           0.063619            544
seed7/lib/asn1oid.s7i        0.049385            157
seed7/lib/basearray.s7i      0.064518            450
seed7/lib/bigfile.s7i        0.041958            136
seed7/lib/bigint.s7i         0.077250            824
seed7/lib/bigrat.s7i         0.078904            784
seed7/lib/bin16.s7i          0.068920            592
seed7/lib/bin32.s7i          0.062294            490
seed7/lib/bin64.s7i          0.063344            539
seed7/lib/bitdata.s7i        0.122815           1330
seed7/lib/bitmapfont.s7i     0.047476            215
seed7/lib/bitset.s7i         0.062655            593
seed7/lib/bitsetof.s7i       0.060096            431
seed7/lib/blowfish.s7i       0.075955            383
seed7/lib/bmp.s7i            0.099218            924
seed7/lib/boolean.s7i        0.054223            403
seed7/lib/browser.s7i        0.053365            280
seed7/lib/bstring.s7i        0.047150            227
seed7/lib/bytedata.s7i       0.065356            482
seed7/lib/bzip2.s7i          0.089599            887
seed7/lib/cards.s7i          0.101896           1342
seed7/lib/category.s7i       0.046931            209
seed7/lib/cc_conf.s7i        0.117205           1314
seed7/lib/ccittfax.s7i       0.099678           1022
seed7/lib/cgi.s7i            0.041380            109
seed7/lib/cgidialog.s7i      0.097636           1118
seed7/lib/char.s7i           0.054485            356
seed7/lib/charsets.s7i       0.126148           2024
seed7/lib/chartype.s7i       0.048001            121
seed7/lib/cipher.s7i         0.042147            146
seed7/lib/cli_cmds.s7i       0.117422           1360
seed7/lib/clib_file.s7i      0.051709            301
seed7/lib/color.s7i          0.049446            185
seed7/lib/complex.s7i        0.061341            464
seed7/lib/compress.s7i       0.045900            150
seed7/lib/console.s7i        0.052545            188
seed7/lib/cpio.s7i           0.151496           1708
seed7/lib/crc32.s7i          0.055283            193
seed7/lib/cronos16.s7i       0.193216           1173
seed7/lib/cronos27.s7i       0.253490           1464
seed7/lib/csv.s7i            0.047819            201
seed7/lib/db_prop.s7i        0.100076            991
seed7/lib/deflate.s7i        0.085123            740
seed7/lib/des.s7i            0.081324            444
seed7/lib/dialog.s7i         0.056807            311
seed7/lib/dir.s7i            0.042773            163
seed7/lib/draw.s7i           0.085140            854
seed7/lib/duration.s7i       0.096535           1038
seed7/lib/echo.s7i           0.041002            132
seed7/lib/editline.s7i       0.057579            398
seed7/lib/elf.s7i            0.151968           1560
seed7/lib/elliptic.s7i       0.076760            649
seed7/lib/enable_io.s7i      0.051783            312
seed7/lib/encoding.s7i       0.096993            931
seed7/lib/enumeration.s7i    0.048772            236
seed7/lib/environment.s7i    0.044650            175
seed7/lib/exif.s7i           0.045158            152
seed7/lib/external_file.s7i  0.052856            340
seed7/lib/field.s7i          0.052420            268
seed7/lib/file.s7i           0.053689            372
seed7/lib/filebits.s7i       0.037958             46
seed7/lib/filesys.s7i        0.066127            601
seed7/lib/fileutil.s7i       0.044084            144
seed7/lib/fixarray.s7i       0.052058            307
seed7/lib/float.s7i          0.072026            757
seed7/lib/font.s7i           0.045439            196
seed7/lib/font8x8.s7i        0.067417            998
seed7/lib/forloop.s7i        0.059335            449
seed7/lib/ftp.s7i            0.088525            969
seed7/lib/ftpserv.s7i        0.076434            631
seed7/lib/getf.s7i           0.042890            115
seed7/lib/gethttp.s7i        0.037623             41
seed7/lib/gethttps.s7i       0.037166             41
seed7/lib/gif.s7i            0.071082            561
seed7/lib/graph.s7i          0.064183            415
seed7/lib/graph_file.s7i     0.058157            399
seed7/lib/gtkserver.s7i      0.041520            161
seed7/lib/gzip.s7i           0.068104            573
seed7/lib/hash.s7i           0.066623            421
seed7/lib/hashsetof.s7i      0.067091            499
seed7/lib/hmac.s7i           0.045007            152
seed7/lib/html.s7i           0.040328             83
seed7/lib/html_ent.s7i       0.061970            476
seed7/lib/htmldom.s7i        0.051977            286
seed7/lib/http_request.s7i   0.075241            696
seed7/lib/http_srv_resp.s7i  0.057412            380
seed7/lib/https_request.s7i  0.048613            211
seed7/lib/httpserv.s7i       0.055875            345
seed7/lib/huffman.s7i        0.075321            644
seed7/lib/ico.s7i            0.050663            221
seed7/lib/idxarray.s7i       0.051302            232
seed7/lib/image.s7i          0.041373            156
seed7/lib/imagefile.s7i      0.045482            171
seed7/lib/inflate.s7i        0.065669            411
seed7/lib/inifile.s7i        0.042574            129
seed7/lib/integer.s7i        0.068018            663
seed7/lib/iobuffer.s7i       0.051304            289
seed7/lib/jpeg.s7i           0.154239           1761
seed7/lib/json.s7i           0.080308            891
seed7/lib/json_serde.s7i     0.077305            783
seed7/lib/keybd.s7i          0.077622            639
seed7/lib/keydescr.s7i       0.048016            192
seed7/lib/leb128.s7i         0.051530            218
seed7/lib/line.s7i           0.046978            164
seed7/lib/listener.s7i       0.051683            247
seed7/lib/logfile.s7i        0.040479             73
seed7/lib/lower.s7i          0.043671            142
seed7/lib/lzma.s7i           0.097236            934
seed7/lib/lzw.s7i            0.092432            861
seed7/lib/magic.s7i          0.065982            403
seed7/lib/mahjng32.s7i       0.092089           1500
seed7/lib/make.s7i           0.070765            544
seed7/lib/makedata.s7i       0.119742           1428
seed7/lib/math.s7i           0.045951            201
seed7/lib/mixarith.s7i       0.047194            249
seed7/lib/modern27.s7i       0.166366           1099
seed7/lib/more.s7i           0.042349            130
seed7/lib/msgdigest.s7i      0.137774           1222
seed7/lib/multiscr.s7i       0.040494             68
seed7/lib/null_file.s7i      0.051670            345
seed7/lib/osfiles.s7i        0.094946           1085
seed7/lib/pbm.s7i            0.047653            230
seed7/lib/pcx.s7i            0.077165            638
seed7/lib/pem.s7i            0.045553            185
seed7/lib/pgm.s7i            0.048127            238
seed7/lib/pic16.s7i          0.066462           1037
seed7/lib/pic32.s7i          0.120485           2060
seed7/lib/pic_util.s7i       0.042303            144
seed7/lib/pixelimage.s7i     0.050839            320
seed7/lib/pixmap_file.s7i    0.060086            459
seed7/lib/pixmapfont.s7i     0.047657            184
seed7/lib/pkcs1.s7i          0.076508            543
seed7/lib/png.s7i            0.109681           1064
seed7/lib/poll.s7i           0.067546            313
seed7/lib/ppm.s7i            0.059130            240
seed7/lib/process.s7i        0.070145            541
seed7/lib/progs.s7i          0.090712            789
seed7/lib/propertyfile.s7i   0.046443            155
seed7/lib/rational.s7i       0.080296            792
seed7/lib/ref_list.s7i       0.050583            252
seed7/lib/reference.s7i      0.044561            126
seed7/lib/reverse.s7i        0.040613             94
seed7/lib/rpm.s7i            0.283201           3487
seed7/lib/rpmext.s7i         0.050751            318
seed7/lib/scanfile.s7i       0.131453           1779
seed7/lib/scanjson.s7i       0.061601            413
seed7/lib/scanstri.s7i       0.136127           1814
seed7/lib/scantoml.s7i       0.131560           1603
seed7/lib/seed7_05.s7i       0.111854           1072
seed7/lib/set.s7i            0.038412             57
seed7/lib/shell.s7i          0.069093            615
seed7/lib/showtls.s7i        0.084061            678
seed7/lib/signature.s7i      0.041508            131
seed7/lib/smtp.s7i           0.047664            261
seed7/lib/sockbase.s7i       0.048506            217
seed7/lib/socket.s7i         0.053147            326
seed7/lib/sokoban1.s7i       0.082198           1519
seed7/lib/sql_base.s7i       0.097949           1000
seed7/lib/stars.s7i          0.239553           1705
seed7/lib/stdfont10.s7i      0.152090           3347
seed7/lib/stdfont12.s7i      0.171668           3928
seed7/lib/stdfont14.s7i      0.190284           4510
seed7/lib/stdfont16.s7i      0.208493           5092
seed7/lib/stdfont18.s7i      0.248535           5868
seed7/lib/stdfont20.s7i      0.272550           6449
seed7/lib/stdfont24.s7i      0.329836           7421
seed7/lib/stdfont8.s7i       0.125942           2960
seed7/lib/stdfont9.s7i       0.134537           3152
seed7/lib/stdio.s7i          0.047038            192
seed7/lib/strifile.s7i       0.060564            345
seed7/lib/string.s7i         0.078481            779
seed7/lib/stritext.s7i       0.054687            352
seed7/lib/struct.s7i         0.055315            266
seed7/lib/struct_elem.s7i    0.041860            129
seed7/lib/subfile.s7i        0.042850            174
seed7/lib/subrange.s7i       0.038697             78
seed7/lib/syntax.s7i         0.058706            294
seed7/lib/tar.s7i            0.146596           1880
seed7/lib/tar_cmds.s7i       0.082884            752
seed7/lib/tdes.s7i           0.048767            143
seed7/lib/tee.s7i            0.042477            143
seed7/lib/text.s7i           0.041325            135
seed7/lib/tga.s7i            0.080996            676
seed7/lib/tiff.s7i           0.256643           2771
seed7/lib/time.s7i           0.103197           1191
seed7/lib/tls.s7i            0.195574           2230
seed7/lib/unicode.s7i        0.072995            575
seed7/lib/unionfnd.s7i       0.042449            130
seed7/lib/upper.s7i          0.041687            142
seed7/lib/utf16.s7i          0.065297            540
seed7/lib/utf8.s7i           0.047955            234
seed7/lib/vecfont10.s7i      0.158958           1056
seed7/lib/vecfont18.s7i      0.176027           1119
seed7/lib/vector3d.s7i       0.048541            293
seed7/lib/vectorfont.s7i     0.046442            239
seed7/lib/wildcard.s7i       0.044058            140
seed7/lib/window.s7i         0.060569            455
seed7/lib/wrinum.s7i         0.049726            248
seed7/lib/x509cert.s7i       0.118490           1243
seed7/lib/xml_ent.s7i        0.041020             94
seed7/lib/xmldom.s7i         0.050514            303
seed7/lib/xz.s7i             0.060112            442
seed7/lib/zip.s7i            0.229966           2792
seed7/lib/zstd.s7i           0.117448           1263
============================ ================= ================

Statistical Summary — Mode D (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.160164        |
+-----------+-----------------+
| Minimum   | 0.035964        |
+-----------+-----------------+
| Maximum   | 5.598503        |
+-----------+-----------------+

Cross-Mode Comparison
=====================

+------+-----------------+-----------------+-----------------+
| Mode | Average (s)     | Minimum (s)     | Maximum (s)     |
+======+=================+=================+=================+
| A    | 0.034479        | 0.032376        | 0.045546        |
+------+-----------------+-----------------+-----------------+
| B    | 0.040174        | 0.035197        | 0.049282        |
+------+-----------------+-----------------+-----------------+
| C    | 0.090233        | 0.034586        | 2.519594        |
+------+-----------------+-----------------+-----------------+
| D    | 0.160164        | 0.035964        | 5.598503        |
+------+-----------------+-----------------+-----------------+

Phase Timing Summary
====================

Wall-clock times include warm-up work and GC performed by the warm-up helper.

+----------+--------------+--------------+--------------+
| Phase    | Warm-up      | Timed pass   | Mode total   |
+==========+==============+==============+==============+
| Mode A   | 00:00:12.657 | 00:00:58.742 | 00:01:11.400 |
+----------+--------------+--------------+--------------+
| Mode B   | 00:00:15.630 | 00:01:08.503 | 00:01:24.133 |
+----------+--------------+--------------+--------------+
| Mode C   | 00:00:36.669 | 00:02:34.602 | 00:03:11.271 |
+----------+--------------+--------------+--------------+
| Mode D   | 00:01:02.532 | 00:04:33.779 | 00:05:36.312 |
+----------+--------------+--------------+--------------+
| Total    |              |              | 00:11:23.124 |
+----------+--------------+--------------+--------------+


.. ---------------------------------------------------------------------------
