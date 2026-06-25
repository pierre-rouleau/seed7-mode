=======================================================
GC-Controlled Benchmark Report: Seed7 Mode — Four Modes
=======================================================

:Running with: seed7-mode 2026-06-24T13:12:22+0000 W26-3
:Seed7 dir   : /Users/roup/my/dvo/seed7-repos/seed7/
:Started at: 09:19:36 local time
:Generated on: 2026-06-24 13:30:48 UTC
:N Iterations: 5  (mean of N timed opens per file)
:Window system: nil
:Display graphic: nil
:Frame size: 254x84 chars
:Window body: 254x82 chars
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
seed7/prg/addup.sd7          0.035315            190
seed7/prg/bas7.sd7           0.036636          11459
seed7/prg/bifurk.sd7         0.035617             73
seed7/prg/bigfiles.sd7       0.036439            129
seed7/prg/brainf7.sd7        0.034041             86
seed7/prg/calc7.sd7          0.033726            128
seed7/prg/carddemo.sd7       0.034747            190
seed7/prg/castle.sd7         0.034243           3148
seed7/prg/cat.sd7            0.033565             82
seed7/prg/cellauto.sd7       0.034045             85
seed7/prg/celsius.sd7        0.032914             42
seed7/prg/chk_all.sd7        0.033155            843
seed7/prg/chkarr.sd7         0.033603           8367
seed7/prg/chkbig.sd7         0.037540          29026
seed7/prg/chkbin.sd7         0.035856           6469
seed7/prg/chkbitdata.sd7     0.035078           6624
seed7/prg/chkbool.sd7        0.033757           3157
seed7/prg/chkbst.sd7         0.033445            722
seed7/prg/chkchr.sd7         0.034260           2809
seed7/prg/chkcmd.sd7         0.033427           1205
seed7/prg/chkdb.sd7          0.034438           7454
seed7/prg/chkdecl.sd7        0.034089            448
seed7/prg/chkenum.sd7        0.033772           1230
seed7/prg/chkerr.sd7         0.034276           4663
seed7/prg/chkexc.sd7         0.033990           2627
seed7/prg/chkfil.sd7         0.033275           1615
seed7/prg/chkflt.sd7         0.035898          20620
seed7/prg/chkhent.sd7        0.032976             54
seed7/prg/chkhsh.sd7         0.033104           4548
seed7/prg/chkidx.sd7         0.035624          19567
seed7/prg/chkint.sd7         0.040137          38129
seed7/prg/chkjson.sd7        0.034048           1764
seed7/prg/chkovf.sd7         0.034911           8216
seed7/prg/chkprc.sd7         0.033376          10111
seed7/prg/chkscan.sd7        0.032939            714
seed7/prg/chkset.sd7         0.034372          11974
seed7/prg/chkstr.sd7         0.038254          26952
seed7/prg/chktime.sd7        0.034469           2025
seed7/prg/chktoml.sd7        0.033652           1656
seed7/prg/clock.sd7          0.033458             47
seed7/prg/clock2.sd7         0.032761             43
seed7/prg/clock3.sd7         0.032776             95
seed7/prg/cmpfil.sd7         0.032946             84
seed7/prg/comanche.sd7       0.032574            180
seed7/prg/confval.sd7        0.032733            175
seed7/prg/db7.sd7            0.033987            417
seed7/prg/diff7.sd7          0.034060            263
seed7/prg/dirtst.sd7         0.033339             42
seed7/prg/dirx.sd7           0.033202            152
seed7/prg/dnafight.sd7       0.033441           1381
seed7/prg/dragon.sd7         0.033869             73
seed7/prg/echo.sd7           0.033351             39
seed7/prg/eliza.sd7          0.037201            302
seed7/prg/err.sd7            0.039528             96
seed7/prg/fannkuch.sd7       0.034173            131
seed7/prg/fib.sd7            0.033443             47
seed7/prg/find7.sd7          0.033840            133
seed7/prg/findchar.sd7       0.033380            149
seed7/prg/fractree.sd7       0.033747             55
seed7/prg/ftp7.sd7           0.034121            296
seed7/prg/ftpserv.sd7        0.033701             74
seed7/prg/gcd.sd7            0.033442            109
seed7/prg/gkbd.sd7           0.033327            358
seed7/prg/gtksvtst.sd7       0.042646             94
seed7/prg/hal.sd7            0.036107            250
seed7/prg/hamu.sd7           0.033640            573
seed7/prg/hanoi.sd7          0.033887             55
seed7/prg/hd.sd7             0.033396             79
seed7/prg/hello.sd7          0.033272             32
seed7/prg/hilbert.sd7        0.033645            108
seed7/prg/ide7.sd7           0.036236            196
seed7/prg/kbd.sd7            0.040184             49
seed7/prg/klondike.sd7       0.032865            883
seed7/prg/lander.sd7         0.032999           1551
seed7/prg/lst80bas.sd7       0.032902            344
seed7/prg/lst99bas.sd7       0.033162            401
seed7/prg/lstgwbas.sd7       0.033937            577
seed7/prg/mahjong.sd7        0.033329           1943
seed7/prg/make7.sd7          0.033386            121
seed7/prg/mandelbr.sd7       0.032905            237
seed7/prg/mind.sd7           0.032719            443
seed7/prg/mirror.sd7         0.032560            131
seed7/prg/ms.sd7             0.032817            641
seed7/prg/nicoma.sd7         0.033797            135
seed7/prg/pac.sd7            0.033752            726
seed7/prg/pairs.sd7          0.033842           2025
seed7/prg/panic.sd7          0.034015           2634
seed7/prg/percolation.sd7    0.033693            330
seed7/prg/planets.sd7        0.034380           1486
seed7/prg/portfwd7.sd7       0.036825            139
seed7/prg/prime.sd7          0.039368             74
seed7/prg/printpi1.sd7       0.034587             56
seed7/prg/printpi2.sd7       0.033869             54
seed7/prg/printpi3.sd7       0.033639             60
seed7/prg/pv7.sd7            0.033422            337
seed7/prg/queen.sd7          0.033829            149
seed7/prg/rand.sd7           0.033531            121
seed7/prg/raytrace.sd7       0.034019            538
seed7/prg/rever.sd7          0.033666            816
seed7/prg/roman.sd7          0.033529             38
seed7/prg/s7c.sd7            0.034444           9060
seed7/prg/s7check.sd7        0.032747             68
seed7/prg/savehd7.sd7        0.033159           1110
seed7/prg/self.sd7           0.032903             49
seed7/prg/shisen.sd7         0.032731           1423
seed7/prg/sl.sd7             0.032750           1029
seed7/prg/snake.sd7          0.032525            615
seed7/prg/sokoban.sd7        0.034670            891
seed7/prg/spigotpi.sd7       0.033553             64
seed7/prg/sql7.sd7           0.033789            278
seed7/prg/startrek.sd7       0.033425            979
seed7/prg/sudoku7.sd7        0.034137           2657
seed7/prg/sydir7.sd7         0.033559            384
seed7/prg/syntaxhl.sd7       0.033603            177
seed7/prg/tak.sd7            0.033887             59
seed7/prg/tar7.sd7           0.033547            121
seed7/prg/tch.sd7            0.033687             55
seed7/prg/testfont.sd7       0.033797             95
seed7/prg/tet.sd7            0.033805            479
seed7/prg/tetg.sd7           0.033825            501
seed7/prg/toutf8.sd7         0.033406            240
seed7/prg/tst_cli.sd7        0.033857             40
seed7/prg/tst_srv.sd7        0.032891             47
seed7/prg/wator.sd7          0.032559            651
seed7/prg/which.sd7          0.032698             65
seed7/prg/wiz.sd7            0.032622           2833
seed7/prg/wordcnt.sd7        0.032978             54
seed7/prg/wrinum.sd7         0.032757             43
seed7/prg/wumpus.sd7         0.033007            372
seed7/lib/aes.s7i            0.033036           1144
seed7/lib/aes_gcm.s7i        0.032407            392
seed7/lib/ar.s7i             0.033471           1532
seed7/lib/arc4.s7i           0.032860            144
seed7/lib/archive.s7i        0.033098            143
seed7/lib/archive_base.s7i   0.032668            135
seed7/lib/array.s7i          0.032745            610
seed7/lib/asn1.s7i           0.032540            544
seed7/lib/asn1oid.s7i        0.035235            157
seed7/lib/basearray.s7i      0.033871            450
seed7/lib/bigfile.s7i        0.033856            136
seed7/lib/bigint.s7i         0.033636            824
seed7/lib/bigrat.s7i         0.033793            784
seed7/lib/bin16.s7i          0.033864            592
seed7/lib/bin32.s7i          0.034188            490
seed7/lib/bin64.s7i          0.033764            539
seed7/lib/bitdata.s7i        0.033975           1330
seed7/lib/bitmapfont.s7i     0.033418            215
seed7/lib/bitset.s7i         0.033885            593
seed7/lib/bitsetof.s7i       0.034044            431
seed7/lib/blowfish.s7i       0.033838            383
seed7/lib/bmp.s7i            0.033957            924
seed7/lib/boolean.s7i        0.033406            403
seed7/lib/browser.s7i        0.033872            280
seed7/lib/bstring.s7i        0.033485            227
seed7/lib/bytedata.s7i       0.033514            482
seed7/lib/bzip2.s7i          0.034006            887
seed7/lib/cards.s7i          0.033684           1342
seed7/lib/category.s7i       0.033557            209
seed7/lib/cc_conf.s7i        0.033916           1314
seed7/lib/ccittfax.s7i       0.033653           1022
seed7/lib/cgi.s7i            0.033391            109
seed7/lib/cgidialog.s7i      0.032812           1118
seed7/lib/char.s7i           0.032786            356
seed7/lib/charsets.s7i       0.033025           2024
seed7/lib/chartype.s7i       0.032684            121
seed7/lib/cipher.s7i         0.032475            146
seed7/lib/cli_cmds.s7i       0.033579           1360
seed7/lib/clib_file.s7i      0.034348            301
seed7/lib/color.s7i          0.033527            185
seed7/lib/complex.s7i        0.033116            464
seed7/lib/compress.s7i       0.032772            150
seed7/lib/console.s7i        0.033437            188
seed7/lib/cpio.s7i           0.033032           1708
seed7/lib/crc32.s7i          0.032506            193
seed7/lib/cronos16.s7i       0.032726           1173
seed7/lib/cronos27.s7i       0.032847           1464
seed7/lib/csv.s7i            0.032708            201
seed7/lib/db_prop.s7i        0.033156            991
seed7/lib/deflate.s7i        0.032742            740
seed7/lib/des.s7i            0.033619            444
seed7/lib/dialog.s7i         0.033590            311
seed7/lib/dir.s7i            0.033654            163
seed7/lib/draw.s7i           0.034054            854
seed7/lib/duration.s7i       0.033658           1038
seed7/lib/echo.s7i           0.033673            132
seed7/lib/editline.s7i       0.036415            398
seed7/lib/elf.s7i            0.036588           1560
seed7/lib/elliptic.s7i       0.033592            649
seed7/lib/enable_io.s7i      0.033895            312
seed7/lib/encoding.s7i       0.033992            931
seed7/lib/enumeration.s7i    0.033560            236
seed7/lib/environment.s7i    0.033574            175
seed7/lib/exif.s7i           0.032911            152
seed7/lib/external_file.s7i  0.033002            340
seed7/lib/field.s7i          0.032659            268
seed7/lib/file.s7i           0.032608            372
seed7/lib/filebits.s7i       0.032415             46
seed7/lib/filesys.s7i        0.035452            601
seed7/lib/fileutil.s7i       0.034125            144
seed7/lib/fixarray.s7i       0.033540            307
seed7/lib/float.s7i          0.033846            757
seed7/lib/font.s7i           0.033510            196
seed7/lib/font8x8.s7i        0.038280            998
seed7/lib/forloop.s7i        0.037143            449
seed7/lib/ftp.s7i            0.033824            969
seed7/lib/ftpserv.s7i        0.033984            631
seed7/lib/getf.s7i           0.034121            115
seed7/lib/gethttp.s7i        0.034118             41
seed7/lib/gethttps.s7i       0.034198             41
seed7/lib/gif.s7i            0.033628            561
seed7/lib/graph.s7i          0.033801            415
seed7/lib/graph_file.s7i     0.034157            399
seed7/lib/gtkserver.s7i      0.034063            161
seed7/lib/gzip.s7i           0.033598            573
seed7/lib/hash.s7i           0.033535            421
seed7/lib/hashsetof.s7i      0.038064            499
seed7/lib/hmac.s7i           0.037972            152
seed7/lib/html.s7i           0.034044             83
seed7/lib/html_ent.s7i       0.033694            476
seed7/lib/htmldom.s7i        0.034115            286
seed7/lib/http_request.s7i   0.034294            696
seed7/lib/http_srv_resp.s7i  0.033894            380
seed7/lib/https_request.s7i  0.033170            211
seed7/lib/httpserv.s7i       0.033752            345
seed7/lib/huffman.s7i        0.032916            644
seed7/lib/ico.s7i            0.033150            221
seed7/lib/idxarray.s7i       0.033574            232
seed7/lib/image.s7i          0.033119            156
seed7/lib/imagefile.s7i      0.035774            171
seed7/lib/inflate.s7i        0.033768            411
seed7/lib/inifile.s7i        0.033170            129
seed7/lib/integer.s7i        0.032819            663
seed7/lib/iobuffer.s7i       0.033099            289
seed7/lib/jpeg.s7i           0.033121           1761
seed7/lib/json.s7i           0.033055            891
seed7/lib/json_serde.s7i     0.033348            783
seed7/lib/keybd.s7i          0.033795            639
seed7/lib/keydescr.s7i       0.032585            192
seed7/lib/leb128.s7i         0.032667            218
seed7/lib/line.s7i           0.032897            164
seed7/lib/listener.s7i       0.033715            247
seed7/lib/logfile.s7i        0.034042             73
seed7/lib/lower.s7i          0.033525            142
seed7/lib/lzma.s7i           0.033691            934
seed7/lib/lzw.s7i            0.033740            861
seed7/lib/magic.s7i          0.033470            403
seed7/lib/mahjng32.s7i       0.033646           1500
seed7/lib/make.s7i           0.033590            544
seed7/lib/makedata.s7i       0.033432           1428
seed7/lib/math.s7i           0.033420            201
seed7/lib/mixarith.s7i       0.033579            249
seed7/lib/modern27.s7i       0.033737           1099
seed7/lib/more.s7i           0.034145            130
seed7/lib/msgdigest.s7i      0.033542           1222
seed7/lib/multiscr.s7i       0.033030             68
seed7/lib/null_file.s7i      0.033002            345
seed7/lib/osfiles.s7i        0.033073           1085
seed7/lib/pbm.s7i            0.032963            230
seed7/lib/pcx.s7i            0.033602            638
seed7/lib/pem.s7i            0.032701            185
seed7/lib/pgm.s7i            0.035222            238
seed7/lib/pic16.s7i          0.033620           1037
seed7/lib/pic32.s7i          0.033947           2060
seed7/lib/pic_util.s7i       0.033470            144
seed7/lib/pixelimage.s7i     0.034022            320
seed7/lib/pixmap_file.s7i    0.040932            459
seed7/lib/pixmapfont.s7i     0.035507            184
seed7/lib/pkcs1.s7i          0.033564            543
seed7/lib/png.s7i            0.035994           1064
seed7/lib/poll.s7i           0.034955            313
seed7/lib/ppm.s7i            0.033700            240
seed7/lib/process.s7i        0.033754            541
seed7/lib/progs.s7i          0.034871            789
seed7/lib/propertyfile.s7i   0.034064            155
seed7/lib/rational.s7i       0.033937            792
seed7/lib/ref_list.s7i       0.034213            252
seed7/lib/reference.s7i      0.033370            126
seed7/lib/reverse.s7i        0.033053             94
seed7/lib/rpm.s7i            0.033822           3487
seed7/lib/rpmext.s7i         0.032481            318
seed7/lib/scanfile.s7i       0.033102           1779
seed7/lib/scanjson.s7i       0.033024            413
seed7/lib/scanstri.s7i       0.032864           1814
seed7/lib/scantoml.s7i       0.032707           1603
seed7/lib/seed7_05.s7i       0.032683           1072
seed7/lib/set.s7i            0.032808             57
seed7/lib/shell.s7i          0.032842            615
seed7/lib/showtls.s7i        0.032718            678
seed7/lib/signature.s7i      0.032779            131
seed7/lib/smtp.s7i           0.032716            261
seed7/lib/sockbase.s7i       0.032515            217
seed7/lib/socket.s7i         0.034070            326
seed7/lib/sokoban1.s7i       0.033674           1519
seed7/lib/sql_base.s7i       0.033825           1000
seed7/lib/stars.s7i          0.033983           1705
seed7/lib/stdfont10.s7i      0.033872           3347
seed7/lib/stdfont12.s7i      0.034290           3928
seed7/lib/stdfont14.s7i      0.033673           4510
seed7/lib/stdfont16.s7i      0.034326           5092
seed7/lib/stdfont18.s7i      0.034825           5868
seed7/lib/stdfont20.s7i      0.034200           6449
seed7/lib/stdfont24.s7i      0.034491           7421
seed7/lib/stdfont8.s7i       0.038326           2960
seed7/lib/stdfont9.s7i       0.038275           3152
seed7/lib/stdio.s7i          0.033931            192
seed7/lib/strifile.s7i       0.033920            345
seed7/lib/string.s7i         0.034014            779
seed7/lib/stritext.s7i       0.033839            352
seed7/lib/struct.s7i         0.034139            266
seed7/lib/struct_elem.s7i    0.035276            129
seed7/lib/subfile.s7i        0.034149            174
seed7/lib/subrange.s7i       0.033820             78
seed7/lib/syntax.s7i         0.034147            294
seed7/lib/tar.s7i            0.033796           1880
seed7/lib/tar_cmds.s7i       0.033704            752
seed7/lib/tdes.s7i           0.032741            143
seed7/lib/tee.s7i            0.033079            143
seed7/lib/text.s7i           0.033018            135
seed7/lib/tga.s7i            0.033369            676
seed7/lib/tiff.s7i           0.032983           2771
seed7/lib/time.s7i           0.033034           1191
seed7/lib/tls.s7i            0.034635           2230
seed7/lib/unicode.s7i        0.032925            575
seed7/lib/unionfnd.s7i       0.032811            130
seed7/lib/upper.s7i          0.033231            142
seed7/lib/utf16.s7i          0.033080            540
seed7/lib/utf8.s7i           0.032792            234
seed7/lib/vecfont10.s7i      0.033368           1056
seed7/lib/vecfont18.s7i      0.033773           1119
seed7/lib/vector3d.s7i       0.033094            293
seed7/lib/vectorfont.s7i     0.033229            239
seed7/lib/wildcard.s7i       0.033062            140
seed7/lib/window.s7i         0.033427            455
seed7/lib/wrinum.s7i         0.034018            248
seed7/lib/x509cert.s7i       0.033834           1243
seed7/lib/xml_ent.s7i        0.033529             94
seed7/lib/xmldom.s7i         0.033647            303
seed7/lib/xz.s7i             0.033616            442
seed7/lib/zip.s7i            0.034044           2792
seed7/lib/zstd.s7i           0.034999           1263
============================ ================= ================

Statistical Summary — Mode A (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.033885        |
+-----------+-----------------+
| Minimum   | 0.032407        |
+-----------+-----------------+
| Maximum   | 0.042646        |
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
seed7/prg/addup.sd7          0.047622            190
seed7/prg/bas7.sd7           0.041024          11459
seed7/prg/bifurk.sd7         0.036888             73
seed7/prg/bigfiles.sd7       0.038623            129
seed7/prg/brainf7.sd7        0.038260             86
seed7/prg/calc7.sd7          0.039541            128
seed7/prg/carddemo.sd7       0.038779            190
seed7/prg/castle.sd7         0.041416           3148
seed7/prg/cat.sd7            0.036576             82
seed7/prg/cellauto.sd7       0.039455             85
seed7/prg/celsius.sd7        0.037497             42
seed7/prg/chk_all.sd7        0.039340            843
seed7/prg/chkarr.sd7         0.038544           8367
seed7/prg/chkbig.sd7         0.042497          29026
seed7/prg/chkbin.sd7         0.038346           6469
seed7/prg/chkbitdata.sd7     0.040734           6624
seed7/prg/chkbool.sd7        0.038718           3157
seed7/prg/chkbst.sd7         0.039465            722
seed7/prg/chkchr.sd7         0.038915           2809
seed7/prg/chkcmd.sd7         0.037716           1205
seed7/prg/chkdb.sd7          0.041163           7454
seed7/prg/chkdecl.sd7        0.038995            448
seed7/prg/chkenum.sd7        0.037881           1230
seed7/prg/chkerr.sd7         0.039856           4663
seed7/prg/chkexc.sd7         0.038156           2627
seed7/prg/chkfil.sd7         0.038840           1615
seed7/prg/chkflt.sd7         0.040530          20620
seed7/prg/chkhent.sd7        0.036819             54
seed7/prg/chkhsh.sd7         0.039291           4548
seed7/prg/chkidx.sd7         0.041426          19567
seed7/prg/chkint.sd7         0.046295          38129
seed7/prg/chkjson.sd7        0.038823           1764
seed7/prg/chkovf.sd7         0.038340           8216
seed7/prg/chkprc.sd7         0.040345          10111
seed7/prg/chkscan.sd7        0.039592            714
seed7/prg/chkset.sd7         0.040006          11974
seed7/prg/chkstr.sd7         0.042362          26952
seed7/prg/chktime.sd7        0.038984           2025
seed7/prg/chktoml.sd7        0.041159           1656
seed7/prg/clock.sd7          0.036230             47
seed7/prg/clock2.sd7         0.036344             43
seed7/prg/clock3.sd7         0.039285             95
seed7/prg/cmpfil.sd7         0.037815             84
seed7/prg/comanche.sd7       0.039509            180
seed7/prg/confval.sd7        0.040039            175
seed7/prg/db7.sd7            0.040477            417
seed7/prg/diff7.sd7          0.038950            263
seed7/prg/dirtst.sd7         0.036348             42
seed7/prg/dirx.sd7           0.038169            152
seed7/prg/dnafight.sd7       0.039657           1381
seed7/prg/dragon.sd7         0.036811             73
seed7/prg/echo.sd7           0.036137             39
seed7/prg/eliza.sd7          0.038600            302
seed7/prg/err.sd7            0.040865             96
seed7/prg/fannkuch.sd7       0.039045            131
seed7/prg/fib.sd7            0.038767             47
seed7/prg/find7.sd7          0.039298            133
seed7/prg/findchar.sd7       0.039191            149
seed7/prg/fractree.sd7       0.036751             55
seed7/prg/ftp7.sd7           0.039416            296
seed7/prg/ftpserv.sd7        0.037774             74
seed7/prg/gcd.sd7            0.037859            109
seed7/prg/gkbd.sd7           0.037898            358
seed7/prg/gtksvtst.sd7       0.036406             94
seed7/prg/hal.sd7            0.036792            250
seed7/prg/hamu.sd7           0.036801            573
seed7/prg/hanoi.sd7          0.037234             55
seed7/prg/hd.sd7             0.037510             79
seed7/prg/hello.sd7          0.036004             32
seed7/prg/hilbert.sd7        0.038209            108
seed7/prg/ide7.sd7           0.039190            196
seed7/prg/kbd.sd7            0.038576             49
seed7/prg/klondike.sd7       0.040296            883
seed7/prg/lander.sd7         0.041463           1551
seed7/prg/lst80bas.sd7       0.038257            344
seed7/prg/lst99bas.sd7       0.038001            401
seed7/prg/lstgwbas.sd7       0.037564            577
seed7/prg/mahjong.sd7        0.041238           1943
seed7/prg/make7.sd7          0.039021            121
seed7/prg/mandelbr.sd7       0.038597            237
seed7/prg/mind.sd7           0.037448            443
seed7/prg/mirror.sd7         0.038391            131
seed7/prg/ms.sd7             0.039961            641
seed7/prg/nicoma.sd7         0.039299            135
seed7/prg/pac.sd7            0.035959            726
seed7/prg/pairs.sd7          0.037662           2025
seed7/prg/panic.sd7          0.038673           2634
seed7/prg/percolation.sd7    0.038013            330
seed7/prg/planets.sd7        0.040314           1486
seed7/prg/portfwd7.sd7       0.038221            139
seed7/prg/prime.sd7          0.036696             74
seed7/prg/printpi1.sd7       0.035541             56
seed7/prg/printpi2.sd7       0.035969             54
seed7/prg/printpi3.sd7       0.037672             60
seed7/prg/pv7.sd7            0.041338            337
seed7/prg/queen.sd7          0.039074            149
seed7/prg/rand.sd7           0.038358            121
seed7/prg/raytrace.sd7       0.039513            538
seed7/prg/rever.sd7          0.039176            816
seed7/prg/roman.sd7          0.038295             38
seed7/prg/s7c.sd7            0.039908           9060
seed7/prg/s7check.sd7        0.036997             68
seed7/prg/savehd7.sd7        0.038860           1110
seed7/prg/self.sd7           0.036716             49
seed7/prg/shisen.sd7         0.039090           1423
seed7/prg/sl.sd7             0.039046           1029
seed7/prg/snake.sd7          0.038368            615
seed7/prg/sokoban.sd7        0.038753            891
seed7/prg/spigotpi.sd7       0.037393             64
seed7/prg/sql7.sd7           0.038485            278
seed7/prg/startrek.sd7       0.039206            979
seed7/prg/sudoku7.sd7        0.039961           2657
seed7/prg/sydir7.sd7         0.040656            384
seed7/prg/syntaxhl.sd7       0.041111            177
seed7/prg/tak.sd7            0.037292             59
seed7/prg/tar7.sd7           0.046299            121
seed7/prg/tch.sd7            0.037630             55
seed7/prg/testfont.sd7       0.041580             95
seed7/prg/tet.sd7            0.040902            479
seed7/prg/tetg.sd7           0.037621            501
seed7/prg/toutf8.sd7         0.038430            240
seed7/prg/tst_cli.sd7        0.037827             40
seed7/prg/tst_srv.sd7        0.036816             47
seed7/prg/wator.sd7          0.037190            651
seed7/prg/which.sd7          0.035901             65
seed7/prg/wiz.sd7            0.038435           2833
seed7/prg/wordcnt.sd7        0.035935             54
seed7/prg/wrinum.sd7         0.036516             43
seed7/prg/wumpus.sd7         0.037572            372
seed7/lib/aes.s7i            0.041270           1144
seed7/lib/aes_gcm.s7i        0.038729            392
seed7/lib/ar.s7i             0.038280           1532
seed7/lib/arc4.s7i           0.039574            144
seed7/lib/archive.s7i        0.039368            143
seed7/lib/archive_base.s7i   0.038812            135
seed7/lib/array.s7i          0.039305            610
seed7/lib/asn1.s7i           0.037292            544
seed7/lib/asn1oid.s7i        0.041456            157
seed7/lib/basearray.s7i      0.039596            450
seed7/lib/bigfile.s7i        0.038650            136
seed7/lib/bigint.s7i         0.039358            824
seed7/lib/bigrat.s7i         0.039116            784
seed7/lib/bin16.s7i          0.039223            592
seed7/lib/bin32.s7i          0.039488            490
seed7/lib/bin64.s7i          0.038056            539
seed7/lib/bitdata.s7i        0.041557           1330
seed7/lib/bitmapfont.s7i     0.037708            215
seed7/lib/bitset.s7i         0.040084            593
seed7/lib/bitsetof.s7i       0.040113            431
seed7/lib/blowfish.s7i       0.042550            383
seed7/lib/bmp.s7i            0.039516            924
seed7/lib/boolean.s7i        0.038150            403
seed7/lib/browser.s7i        0.039180            280
seed7/lib/bstring.s7i        0.039192            227
seed7/lib/bytedata.s7i       0.041623            482
seed7/lib/bzip2.s7i          0.040589            887
seed7/lib/cards.s7i          0.037538           1342
seed7/lib/category.s7i       0.039317            209
seed7/lib/cc_conf.s7i        0.038658           1314
seed7/lib/ccittfax.s7i       0.039659           1022
seed7/lib/cgi.s7i            0.039249            109
seed7/lib/cgidialog.s7i      0.039055           1118
seed7/lib/char.s7i           0.038717            356
seed7/lib/charsets.s7i       0.039889           2024
seed7/lib/chartype.s7i       0.041587            121
seed7/lib/cipher.s7i         0.040476            146
seed7/lib/cli_cmds.s7i       0.039923           1360
seed7/lib/clib_file.s7i      0.038763            301
seed7/lib/color.s7i          0.039464            185
seed7/lib/complex.s7i        0.037328            464
seed7/lib/compress.s7i       0.039100            150
seed7/lib/console.s7i        0.037450            188
seed7/lib/cpio.s7i           0.038069           1708
seed7/lib/crc32.s7i          0.038671            193
seed7/lib/cronos16.s7i       0.039328           1173
seed7/lib/cronos27.s7i       0.041310           1464
seed7/lib/csv.s7i            0.039258            201
seed7/lib/db_prop.s7i        0.037520            991
seed7/lib/deflate.s7i        0.039719            740
seed7/lib/des.s7i            0.038249            444
seed7/lib/dialog.s7i         0.038506            311
seed7/lib/dir.s7i            0.038446            163
seed7/lib/draw.s7i           0.037967            854
seed7/lib/duration.s7i       0.037412           1038
seed7/lib/echo.s7i           0.038797            132
seed7/lib/editline.s7i       0.038418            398
seed7/lib/elf.s7i            0.039085           1560
seed7/lib/elliptic.s7i       0.039562            649
seed7/lib/enable_io.s7i      0.039069            312
seed7/lib/encoding.s7i       0.039928            931
seed7/lib/enumeration.s7i    0.039157            236
seed7/lib/environment.s7i    0.038783            175
seed7/lib/exif.s7i           0.040163            152
seed7/lib/external_file.s7i  0.040740            340
seed7/lib/field.s7i          0.040789            268
seed7/lib/file.s7i           0.038791            372
seed7/lib/filebits.s7i       0.036477             46
seed7/lib/filesys.s7i        0.039451            601
seed7/lib/fileutil.s7i       0.039113            144
seed7/lib/fixarray.s7i       0.040093            307
seed7/lib/float.s7i          0.041295            757
seed7/lib/font.s7i           0.043888            196
seed7/lib/font8x8.s7i        0.037340            998
seed7/lib/forloop.s7i        0.040516            449
seed7/lib/ftp.s7i            0.041954            969
seed7/lib/ftpserv.s7i        0.039217            631
seed7/lib/getf.s7i           0.039926            115
seed7/lib/gethttp.s7i        0.036416             41
seed7/lib/gethttps.s7i       0.036504             41
seed7/lib/gif.s7i            0.040064            561
seed7/lib/graph.s7i          0.040981            415
seed7/lib/graph_file.s7i     0.038764            399
seed7/lib/gtkserver.s7i      0.038355            161
seed7/lib/gzip.s7i           0.038813            573
seed7/lib/hash.s7i           0.041456            421
seed7/lib/hashsetof.s7i      0.040942            499
seed7/lib/hmac.s7i           0.038032            152
seed7/lib/html.s7i           0.038168             83
seed7/lib/html_ent.s7i       0.038375            476
seed7/lib/htmldom.s7i        0.038671            286
seed7/lib/http_request.s7i   0.039302            696
seed7/lib/http_srv_resp.s7i  0.038791            380
seed7/lib/https_request.s7i  0.038474            211
seed7/lib/httpserv.s7i       0.037627            345
seed7/lib/huffman.s7i        0.038366            644
seed7/lib/ico.s7i            0.038123            221
seed7/lib/idxarray.s7i       0.038178            232
seed7/lib/image.s7i          0.036437            156
seed7/lib/imagefile.s7i      0.037229            171
seed7/lib/inflate.s7i        0.038400            411
seed7/lib/inifile.s7i        0.039162            129
seed7/lib/integer.s7i        0.040063            663
seed7/lib/iobuffer.s7i       0.040455            289
seed7/lib/jpeg.s7i           0.039129           1761
seed7/lib/json.s7i           0.038214            891
seed7/lib/json_serde.s7i     0.039201            783
seed7/lib/keybd.s7i          0.039126            639
seed7/lib/keydescr.s7i       0.040433            192
seed7/lib/leb128.s7i         0.039887            218
seed7/lib/line.s7i           0.037730            164
seed7/lib/listener.s7i       0.038537            247
seed7/lib/logfile.s7i        0.037981             73
seed7/lib/lower.s7i          0.041139            142
seed7/lib/lzma.s7i           0.040993            934
seed7/lib/lzw.s7i            0.038916            861
seed7/lib/magic.s7i          0.039279            403
seed7/lib/mahjng32.s7i       0.038771           1500
seed7/lib/make.s7i           0.039167            544
seed7/lib/makedata.s7i       0.039760           1428
seed7/lib/math.s7i           0.038758            201
seed7/lib/mixarith.s7i       0.038580            249
seed7/lib/modern27.s7i       0.040726           1099
seed7/lib/more.s7i           0.037600            130
seed7/lib/msgdigest.s7i      0.040458           1222
seed7/lib/multiscr.s7i       0.038553             68
seed7/lib/null_file.s7i      0.037485            345
seed7/lib/osfiles.s7i        0.039306           1085
seed7/lib/pbm.s7i            0.039730            230
seed7/lib/pcx.s7i            0.039399            638
seed7/lib/pem.s7i            0.038436            185
seed7/lib/pgm.s7i            0.037884            238
seed7/lib/pic16.s7i          0.036140           1037
seed7/lib/pic32.s7i          0.037182           2060
seed7/lib/pic_util.s7i       0.037608            144
seed7/lib/pixelimage.s7i     0.040045            320
seed7/lib/pixmap_file.s7i    0.038141            459
seed7/lib/pixmapfont.s7i     0.037863            184
seed7/lib/pkcs1.s7i          0.043794            543
seed7/lib/png.s7i            0.039127           1064
seed7/lib/poll.s7i           0.038845            313
seed7/lib/ppm.s7i            0.043975            240
seed7/lib/process.s7i        0.044052            541
seed7/lib/progs.s7i          0.040016            789
seed7/lib/propertyfile.s7i   0.039736            155
seed7/lib/rational.s7i       0.040463            792
seed7/lib/ref_list.s7i       0.040736            252
seed7/lib/reference.s7i      0.041790            126
seed7/lib/reverse.s7i        0.040755             94
seed7/lib/rpm.s7i            0.040017           3487
seed7/lib/rpmext.s7i         0.038455            318
seed7/lib/scanfile.s7i       0.038905           1779
seed7/lib/scanjson.s7i       0.038572            413
seed7/lib/scanstri.s7i       0.038353           1814
seed7/lib/scantoml.s7i       0.037779           1603
seed7/lib/seed7_05.s7i       0.042089           1072
seed7/lib/set.s7i            0.037767             57
seed7/lib/shell.s7i          0.039344            615
seed7/lib/showtls.s7i        0.039475            678
seed7/lib/signature.s7i      0.039672            131
seed7/lib/smtp.s7i           0.038957            261
seed7/lib/sockbase.s7i       0.038636            217
seed7/lib/socket.s7i         0.041633            326
seed7/lib/sokoban1.s7i       0.038782           1519
seed7/lib/sql_base.s7i       0.043575           1000
seed7/lib/stars.s7i          0.039891           1705
seed7/lib/stdfont10.s7i      0.038708           3347
seed7/lib/stdfont12.s7i      0.038692           3928
seed7/lib/stdfont14.s7i      0.038638           4510
seed7/lib/stdfont16.s7i      0.048630           5092
seed7/lib/stdfont18.s7i      0.036651           5868
seed7/lib/stdfont20.s7i      0.037607           6449
seed7/lib/stdfont24.s7i      0.038067           7421
seed7/lib/stdfont8.s7i       0.040114           2960
seed7/lib/stdfont9.s7i       0.037296           3152
seed7/lib/stdio.s7i          0.037587            192
seed7/lib/strifile.s7i       0.042192            345
seed7/lib/string.s7i         0.043508            779
seed7/lib/stritext.s7i       0.039303            352
seed7/lib/struct.s7i         0.038261            266
seed7/lib/struct_elem.s7i    0.037875            129
seed7/lib/subfile.s7i        0.038945            174
seed7/lib/subrange.s7i       0.038268             78
seed7/lib/syntax.s7i         0.039741            294
seed7/lib/tar.s7i            0.040672           1880
seed7/lib/tar_cmds.s7i       0.039609            752
seed7/lib/tdes.s7i           0.039016            143
seed7/lib/tee.s7i            0.038922            143
seed7/lib/text.s7i           0.038591            135
seed7/lib/tga.s7i            0.041434            676
seed7/lib/tiff.s7i           0.040306           2771
seed7/lib/time.s7i           0.039306           1191
seed7/lib/tls.s7i            0.039417           2230
seed7/lib/unicode.s7i        0.039980            575
seed7/lib/unionfnd.s7i       0.037959            130
seed7/lib/upper.s7i          0.038708            142
seed7/lib/utf16.s7i          0.038483            540
seed7/lib/utf8.s7i           0.038897            234
seed7/lib/vecfont10.s7i      0.040313           1056
seed7/lib/vecfont18.s7i      0.040881           1119
seed7/lib/vector3d.s7i       0.038017            293
seed7/lib/vectorfont.s7i     0.040121            239
seed7/lib/wildcard.s7i       0.039739            140
seed7/lib/window.s7i         0.038764            455
seed7/lib/wrinum.s7i         0.038114            248
seed7/lib/x509cert.s7i       0.038119           1243
seed7/lib/xml_ent.s7i        0.037592             94
seed7/lib/xmldom.s7i         0.036343            303
seed7/lib/xz.s7i             0.039598            442
seed7/lib/zip.s7i            0.042261           2792
seed7/lib/zstd.s7i           0.039003           1263
============================ ================= ================

Statistical Summary — Mode B (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.039124        |
+-----------+-----------------+
| Minimum   | 0.035541        |
+-----------+-----------------+
| Maximum   | 0.048630        |
+-----------+-----------------+

Mode C — Mode Activation + Full-Buffer Fontification (font-lock-ensure)
-----------------------------------------------------------------------

Full-buffer fontification stress test.

File Load Times — Mode C (mean, GC-free)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

============================ ================= ================
File Name                    Mean Time (s)     Line count
============================ ================= ================
seed7/prg/addup.sd7          0.038216            190
seed7/prg/bas7.sd7           0.321793          11459
seed7/prg/bifurk.sd7         0.036597             73
seed7/prg/bigfiles.sd7       0.037677            129
seed7/prg/brainf7.sd7        0.036741             86
seed7/prg/calc7.sd7          0.038012            128
seed7/prg/carddemo.sd7       0.039286            190
seed7/prg/castle.sd7         0.110907           3148
seed7/prg/cat.sd7            0.038534             82
seed7/prg/cellauto.sd7       0.036560             85
seed7/prg/celsius.sd7        0.035952             42
seed7/prg/chk_all.sd7        0.058897            843
seed7/prg/chkarr.sd7         0.348368           8367
seed7/prg/chkbig.sd7         2.042482          29026
seed7/prg/chkbin.sd7         0.508079           6469
seed7/prg/chkbitdata.sd7     0.608467           6624
seed7/prg/chkbool.sd7        0.114303           3157
seed7/prg/chkbst.sd7         0.061707            722
seed7/prg/chkchr.sd7         0.212260           2809
seed7/prg/chkcmd.sd7         0.066129           1205
seed7/prg/chkdb.sd7          0.345779           7454
seed7/prg/chkdecl.sd7        0.056542            448
seed7/prg/chkenum.sd7        0.068733           1230
seed7/prg/chkerr.sd7         0.193666           4663
seed7/prg/chkexc.sd7         0.081808           2627
seed7/prg/chkfil.sd7         0.075304           1615
seed7/prg/chkflt.sd7         1.329557          20620
seed7/prg/chkhent.sd7        0.041495             54
seed7/prg/chkhsh.sd7         0.244181           4548
seed7/prg/chkidx.sd7         1.298338          19567
seed7/prg/chkint.sd7         2.496066          38129
seed7/prg/chkjson.sd7        0.099809           1764
seed7/prg/chkovf.sd7         0.551905           8216
seed7/prg/chkprc.sd7         0.317258          10111
seed7/prg/chkscan.sd7        0.056527            714
seed7/prg/chkset.sd7         0.654224          11974
seed7/prg/chkstr.sd7         1.362559          26952
seed7/prg/chktime.sd7        0.128463           2025
seed7/prg/chktoml.sd7        0.105146           1656
seed7/prg/clock.sd7          0.036132             47
seed7/prg/clock2.sd7         0.035330             43
seed7/prg/clock3.sd7         0.037284             95
seed7/prg/cmpfil.sd7         0.036290             84
seed7/prg/comanche.sd7       0.040176            180
seed7/prg/confval.sd7        0.042613            175
seed7/prg/db7.sd7            0.046063            417
seed7/prg/diff7.sd7          0.042231            263
seed7/prg/dirtst.sd7         0.034852             42
seed7/prg/dirx.sd7           0.036692            152
seed7/prg/dnafight.sd7       0.064727           1381
seed7/prg/dragon.sd7         0.035616             73
seed7/prg/echo.sd7           0.034208             39
seed7/prg/eliza.sd7          0.043085            302
seed7/prg/err.sd7            0.039890             96
seed7/prg/fannkuch.sd7       0.037224            131
seed7/prg/fib.sd7            0.035761             47
seed7/prg/find7.sd7          0.037577            133
seed7/prg/findchar.sd7       0.038393            149
seed7/prg/fractree.sd7       0.035975             55
seed7/prg/ftp7.sd7           0.048754            296
seed7/prg/ftpserv.sd7        0.041068             74
seed7/prg/gcd.sd7            0.036692            109
seed7/prg/gkbd.sd7           0.045500            358
seed7/prg/gtksvtst.sd7       0.036710             94
seed7/prg/hal.sd7            0.039307            250
seed7/prg/hamu.sd7           0.050282            573
seed7/prg/hanoi.sd7          0.035917             55
seed7/prg/hd.sd7             0.036368             79
seed7/prg/hello.sd7          0.035473             32
seed7/prg/hilbert.sd7        0.037047            108
seed7/prg/ide7.sd7           0.039997            196
seed7/prg/kbd.sd7            0.036174             49
seed7/prg/klondike.sd7       0.054376            883
seed7/prg/lander.sd7         0.070180           1551
seed7/prg/lst80bas.sd7       0.043847            344
seed7/prg/lst99bas.sd7       0.044190            401
seed7/prg/lstgwbas.sd7       0.050536            577
seed7/prg/mahjong.sd7        0.081087           1943
seed7/prg/make7.sd7          0.038435            121
seed7/prg/mandelbr.sd7       0.040789            237
seed7/prg/mind.sd7           0.044996            443
seed7/prg/mirror.sd7         0.039389            131
seed7/prg/ms.sd7             0.048488            641
seed7/prg/nicoma.sd7         0.038587            135
seed7/prg/pac.sd7            0.048955            726
seed7/prg/pairs.sd7          0.080319           2025
seed7/prg/panic.sd7          0.095515           2634
seed7/prg/percolation.sd7    0.041723            330
seed7/prg/planets.sd7        0.074366           1486
seed7/prg/portfwd7.sd7       0.037311            139
seed7/prg/prime.sd7          0.035321             74
seed7/prg/printpi1.sd7       0.035348             56
seed7/prg/printpi2.sd7       0.034721             54
seed7/prg/printpi3.sd7       0.035183             60
seed7/prg/pv7.sd7            0.042405            337
seed7/prg/queen.sd7          0.036889            149
seed7/prg/rand.sd7           0.036035            121
seed7/prg/raytrace.sd7       0.049796            538
seed7/prg/rever.sd7          0.053930            816
seed7/prg/roman.sd7          0.035834             38
seed7/prg/s7c.sd7            0.277098           9060
seed7/prg/s7check.sd7        0.036415             68
seed7/prg/savehd7.sd7        0.066189           1110
seed7/prg/self.sd7           0.036242             49
seed7/prg/shisen.sd7         0.071587           1423
seed7/prg/sl.sd7             0.061683           1029
seed7/prg/snake.sd7          0.049254            615
seed7/prg/sokoban.sd7        0.054436            891
seed7/prg/spigotpi.sd7       0.036367             64
seed7/prg/sql7.sd7           0.041237            278
seed7/prg/startrek.sd7       0.060511            979
seed7/prg/sudoku7.sd7        0.100624           2657
seed7/prg/sydir7.sd7         0.044954            384
seed7/prg/syntaxhl.sd7       0.041703            177
seed7/prg/tak.sd7            0.038789             59
seed7/prg/tar7.sd7           0.038233            121
seed7/prg/tch.sd7            0.036004             55
seed7/prg/testfont.sd7       0.036414             95
seed7/prg/tet.sd7            0.043833            479
seed7/prg/tetg.sd7           0.044076            501
seed7/prg/toutf8.sd7         0.041075            240
seed7/prg/tst_cli.sd7        0.035382             40
seed7/prg/tst_srv.sd7        0.035373             47
seed7/prg/wator.sd7          0.052347            651
seed7/prg/which.sd7          0.036804             65
seed7/prg/wiz.sd7            0.104459           2833
seed7/prg/wordcnt.sd7        0.035976             54
seed7/prg/wrinum.sd7         0.035522             43
seed7/prg/wumpus.sd7         0.042696            372
seed7/lib/aes.s7i            0.109084           1144
seed7/lib/aes_gcm.s7i        0.046306            392
seed7/lib/ar.s7i             0.071114           1532
seed7/lib/arc4.s7i           0.037283            144
seed7/lib/archive.s7i        0.037436            143
seed7/lib/archive_base.s7i   0.037358            135
seed7/lib/array.s7i          0.053864            610
seed7/lib/asn1.s7i           0.047147            544
seed7/lib/asn1oid.s7i        0.041868            157
seed7/lib/basearray.s7i      0.048824            450
seed7/lib/bigfile.s7i        0.038269            136
seed7/lib/bigint.s7i         0.055417            824
seed7/lib/bigrat.s7i         0.053912            784
seed7/lib/bin16.s7i          0.050674            592
seed7/lib/bin32.s7i          0.047970            490
seed7/lib/bin64.s7i          0.049531            539
seed7/lib/bitdata.s7i        0.076044           1330
seed7/lib/bitmapfont.s7i     0.038961            215
seed7/lib/bitset.s7i         0.047132            593
seed7/lib/bitsetof.s7i       0.046065            431
seed7/lib/blowfish.s7i       0.054929            383
seed7/lib/bmp.s7i            0.059096            924
seed7/lib/boolean.s7i        0.043314            403
seed7/lib/browser.s7i        0.041658            280
seed7/lib/bstring.s7i        0.040033            227
seed7/lib/bytedata.s7i       0.048576            482
seed7/lib/bzip2.s7i          0.057391            887
seed7/lib/cards.s7i          0.065483           1342
seed7/lib/category.s7i       0.042097            209
seed7/lib/cc_conf.s7i        0.078009           1314
seed7/lib/ccittfax.s7i       0.065228           1022
seed7/lib/cgi.s7i            0.038263            109
seed7/lib/cgidialog.s7i      0.060182           1118
seed7/lib/char.s7i           0.043562            356
seed7/lib/charsets.s7i       0.081033           2024
seed7/lib/chartype.s7i       0.039955            121
seed7/lib/cipher.s7i         0.038178            146
seed7/lib/cli_cmds.s7i       0.067819           1360
seed7/lib/clib_file.s7i      0.044232            301
seed7/lib/color.s7i          0.048023            185
seed7/lib/complex.s7i        0.046774            464
seed7/lib/compress.s7i       0.038591            150
seed7/lib/console.s7i        0.039532            188
seed7/lib/cpio.s7i           0.079390           1708
seed7/lib/crc32.s7i          0.042638            193
seed7/lib/cronos16.s7i       0.091091           1173
seed7/lib/cronos27.s7i       0.116239           1464
seed7/lib/csv.s7i            0.039845            201
seed7/lib/db_prop.s7i        0.061013            991
seed7/lib/deflate.s7i        0.054276            740
seed7/lib/des.s7i            0.053917            444
seed7/lib/dialog.s7i         0.042713            311
seed7/lib/dir.s7i            0.037044            163
seed7/lib/draw.s7i           0.055744            854
seed7/lib/duration.s7i       0.060648           1038
seed7/lib/echo.s7i           0.038078            132
seed7/lib/editline.s7i       0.045282            398
seed7/lib/elf.s7i            0.085642           1560
seed7/lib/elliptic.s7i       0.052426            649
seed7/lib/enable_io.s7i      0.043790            312
seed7/lib/encoding.s7i       0.060213            931
seed7/lib/enumeration.s7i    0.040337            236
seed7/lib/environment.s7i    0.038551            175
seed7/lib/exif.s7i           0.039964            152
seed7/lib/external_file.s7i  0.044560            340
seed7/lib/field.s7i          0.041877            268
seed7/lib/file.s7i           0.044787            372
seed7/lib/filebits.s7i       0.039864             46
seed7/lib/filesys.s7i        0.055519            601
seed7/lib/fileutil.s7i       0.041788            144
seed7/lib/fixarray.s7i       0.043976            307
seed7/lib/float.s7i          0.055253            757
seed7/lib/font.s7i           0.039820            196
seed7/lib/font8x8.s7i        0.048603            998
seed7/lib/forloop.s7i        0.045780            449
seed7/lib/ftp.s7i            0.057613            969
seed7/lib/ftpserv.s7i        0.051144            631
seed7/lib/getf.s7i           0.037352            115
seed7/lib/gethttp.s7i        0.035611             41
seed7/lib/gethttps.s7i       0.035922             41
seed7/lib/gif.s7i            0.056697            561
seed7/lib/graph.s7i          0.050767            415
seed7/lib/graph_file.s7i     0.044288            399
seed7/lib/gtkserver.s7i      0.038391            161
seed7/lib/gzip.s7i           0.048371            573
seed7/lib/hash.s7i           0.048211            421
seed7/lib/hashsetof.s7i      0.047492            499
seed7/lib/hmac.s7i           0.041392            152
seed7/lib/html.s7i           0.041950             83
seed7/lib/html_ent.s7i       0.049718            476
seed7/lib/htmldom.s7i        0.043427            286
seed7/lib/http_request.s7i   0.051452            696
seed7/lib/http_srv_resp.s7i  0.045325            380
seed7/lib/https_request.s7i  0.040430            211
seed7/lib/httpserv.s7i       0.043729            345
seed7/lib/huffman.s7i        0.052698            644
seed7/lib/ico.s7i            0.041239            221
seed7/lib/idxarray.s7i       0.041838            232
seed7/lib/image.s7i          0.037728            156
seed7/lib/imagefile.s7i      0.039766            171
seed7/lib/inflate.s7i        0.046668            411
seed7/lib/inifile.s7i        0.038084            129
seed7/lib/integer.s7i        0.052308            663
seed7/lib/iobuffer.s7i       0.041660            289
seed7/lib/jpeg.s7i           0.084129           1761
seed7/lib/json.s7i           0.055318            891
seed7/lib/json_serde.s7i     0.053186            783
seed7/lib/keybd.s7i          0.053708            639
seed7/lib/keydescr.s7i       0.039993            192
seed7/lib/leb128.s7i         0.038678            218
seed7/lib/line.s7i           0.036878            164
seed7/lib/listener.s7i       0.041202            247
seed7/lib/logfile.s7i        0.036933             73
seed7/lib/lower.s7i          0.038089            142
seed7/lib/lzma.s7i           0.059278            934
seed7/lib/lzw.s7i            0.059450            861
seed7/lib/magic.s7i          0.048113            403
seed7/lib/mahjng32.s7i       0.063725           1500
seed7/lib/make.s7i           0.049575            544
seed7/lib/makedata.s7i       0.070050           1428
seed7/lib/math.s7i           0.040148            201
seed7/lib/mixarith.s7i       0.040199            249
seed7/lib/modern27.s7i       0.084043           1099
seed7/lib/more.s7i           0.043196            130
seed7/lib/msgdigest.s7i      0.081018           1222
seed7/lib/multiscr.s7i       0.037184             68
seed7/lib/null_file.s7i      0.041408            345
seed7/lib/osfiles.s7i        0.063603           1085
seed7/lib/pbm.s7i            0.039204            230
seed7/lib/pcx.s7i            0.051116            638
seed7/lib/pem.s7i            0.038058            185
seed7/lib/pgm.s7i            0.039293            238
seed7/lib/pic16.s7i          0.049263           1037
seed7/lib/pic32.s7i          0.080362           2060
seed7/lib/pic_util.s7i       0.038820            144
seed7/lib/pixelimage.s7i     0.042353            320
seed7/lib/pixmap_file.s7i    0.045551            459
seed7/lib/pixmapfont.s7i     0.039553            184
seed7/lib/pkcs1.s7i          0.058959            543
seed7/lib/png.s7i            0.064052           1064
seed7/lib/poll.s7i           0.044287            313
seed7/lib/ppm.s7i            0.040988            240
seed7/lib/process.s7i        0.048774            541
seed7/lib/progs.s7i          0.055966            789
seed7/lib/propertyfile.s7i   0.038983            155
seed7/lib/rational.s7i       0.053187            792
seed7/lib/ref_list.s7i       0.041979            252
seed7/lib/reference.s7i      0.038217            126
seed7/lib/reverse.s7i        0.036629             94
seed7/lib/rpm.s7i            0.141224           3487
seed7/lib/rpmext.s7i         0.043984            318
seed7/lib/scanfile.s7i       0.080045           1779
seed7/lib/scanjson.s7i       0.046939            413
seed7/lib/scanstri.s7i       0.081600           1814
seed7/lib/scantoml.s7i       0.070562           1603
seed7/lib/seed7_05.s7i       0.065922           1072
seed7/lib/set.s7i            0.034902             57
seed7/lib/shell.s7i          0.052670            615
seed7/lib/showtls.s7i        0.053861            678
seed7/lib/signature.s7i      0.037216            131
seed7/lib/smtp.s7i           0.039514            261
seed7/lib/sockbase.s7i       0.042205            217
seed7/lib/socket.s7i         0.043527            326
seed7/lib/sokoban1.s7i       0.054097           1519
seed7/lib/sql_base.s7i       0.064246           1000
seed7/lib/stars.s7i          0.140615           1705
seed7/lib/stdfont10.s7i      0.080475           3347
seed7/lib/stdfont12.s7i      0.089533           3928
seed7/lib/stdfont14.s7i      0.100540           4510
seed7/lib/stdfont16.s7i      0.118437           5092
seed7/lib/stdfont18.s7i      0.134462           5868
seed7/lib/stdfont20.s7i      0.151222           6449
seed7/lib/stdfont24.s7i      0.177707           7421
seed7/lib/stdfont8.s7i       0.071669           2960
seed7/lib/stdfont9.s7i       0.075443           3152
seed7/lib/stdio.s7i          0.038548            192
seed7/lib/strifile.s7i       0.042383            345
seed7/lib/string.s7i         0.053819            779
seed7/lib/stritext.s7i       0.041823            352
seed7/lib/struct.s7i         0.043123            266
seed7/lib/struct_elem.s7i    0.037761            129
seed7/lib/subfile.s7i        0.037433            174
seed7/lib/subrange.s7i       0.037307             78
seed7/lib/syntax.s7i         0.045920            294
seed7/lib/tar.s7i            0.081859           1880
seed7/lib/tar_cmds.s7i       0.056041            752
seed7/lib/tdes.s7i           0.038815            143
seed7/lib/tee.s7i            0.037825            143
seed7/lib/text.s7i           0.037946            135
seed7/lib/tga.s7i            0.053782            676
seed7/lib/tiff.s7i           0.121850           2771
seed7/lib/time.s7i           0.069546           1191
seed7/lib/tls.s7i            0.104767           2230
seed7/lib/unicode.s7i        0.051789            575
seed7/lib/unionfnd.s7i       0.037668            130
seed7/lib/upper.s7i          0.037764            142
seed7/lib/utf16.s7i          0.048996            540
seed7/lib/utf8.s7i           0.040390            234
seed7/lib/vecfont10.s7i      0.077197           1056
seed7/lib/vecfont18.s7i      0.085142           1119
seed7/lib/vector3d.s7i       0.040614            293
seed7/lib/vectorfont.s7i     0.041244            239
seed7/lib/wildcard.s7i       0.038426            140
seed7/lib/window.s7i         0.045312            455
seed7/lib/wrinum.s7i         0.041523            248
seed7/lib/x509cert.s7i       0.070663           1243
seed7/lib/xml_ent.s7i        0.036483             94
seed7/lib/xmldom.s7i         0.039937            303
seed7/lib/xz.s7i             0.044546            442
seed7/lib/zip.s7i            0.117584           2792
seed7/lib/zstd.s7i           0.067016           1263
============================ ================= ================

Statistical Summary — Mode C (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.088500        |
+-----------+-----------------+
| Minimum   | 0.034208        |
+-----------+-----------------+
| Maximum   | 2.496066        |
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
seed7/prg/addup.sd7          0.045814            190
seed7/prg/bas7.sd7           0.759259          11459
seed7/prg/bifurk.sd7         0.039933             73
seed7/prg/bigfiles.sd7       0.043066            129
seed7/prg/brainf7.sd7        0.040727             86
seed7/prg/calc7.sd7          0.042053            128
seed7/prg/carddemo.sd7       0.048778            190
seed7/prg/castle.sd7         0.215410           3148
seed7/prg/cat.sd7            0.039979             82
seed7/prg/cellauto.sd7       0.040543             85
seed7/prg/celsius.sd7        0.037663             42
seed7/prg/chk_all.sd7        0.083144            843
seed7/prg/chkarr.sd7         0.855990           8367
seed7/prg/chkbig.sd7         4.108866          29026
seed7/prg/chkbin.sd7         1.022505           6469
seed7/prg/chkbitdata.sd7     1.239625           6624
seed7/prg/chkbool.sd7        0.231546           3157
seed7/prg/chkbst.sd7         0.102663            722
seed7/prg/chkchr.sd7         0.481528           2809
seed7/prg/chkcmd.sd7         0.111798           1205
seed7/prg/chkdb.sd7          0.743925           7454
seed7/prg/chkdecl.sd7        0.097516            448
seed7/prg/chkenum.sd7        0.122124           1230
seed7/prg/chkerr.sd7         0.338353           4663
seed7/prg/chkexc.sd7         0.150023           2627
seed7/prg/chkfil.sd7         0.130306           1615
seed7/prg/chkflt.sd7         2.805699          20620
seed7/prg/chkhent.sd7        0.038861             54
seed7/prg/chkhsh.sd7         0.503419           4548
seed7/prg/chkidx.sd7         3.177717          19567
seed7/prg/chkint.sd7         5.546317          38129
seed7/prg/chkjson.sd7        0.189614           1764
seed7/prg/chkovf.sd7         1.198371           8216
seed7/prg/chkprc.sd7         0.692983          10111
seed7/prg/chkscan.sd7        0.089599            714
seed7/prg/chkset.sd7         1.726069          11974
seed7/prg/chkstr.sd7         3.393365          26952
seed7/prg/chktime.sd7        0.249399           2025
seed7/prg/chktoml.sd7        0.197684           1656
seed7/prg/clock.sd7          0.038864             47
seed7/prg/clock2.sd7         0.036620             43
seed7/prg/clock3.sd7         0.042432             95
seed7/prg/cmpfil.sd7         0.043468             84
seed7/prg/comanche.sd7       0.047897            180
seed7/prg/confval.sd7        0.052438            175
seed7/prg/db7.sd7            0.064295            417
seed7/prg/diff7.sd7          0.054121            263
seed7/prg/dirtst.sd7         0.037606             42
seed7/prg/dirx.sd7           0.042608            152
seed7/prg/dnafight.sd7       0.116810           1381
seed7/prg/dragon.sd7         0.039375             73
seed7/prg/echo.sd7           0.036609             39
seed7/prg/eliza.sd7          0.052279            302
seed7/prg/err.sd7            0.044273             96
seed7/prg/fannkuch.sd7       0.041666            131
seed7/prg/fib.sd7            0.037275             47
seed7/prg/find7.sd7          0.042410            133
seed7/prg/findchar.sd7       0.043985            149
seed7/prg/fractree.sd7       0.037552             55
seed7/prg/ftp7.sd7           0.052228            296
seed7/prg/ftpserv.sd7        0.039164             74
seed7/prg/gcd.sd7            0.040793            109
seed7/prg/gkbd.sd7           0.060741            358
seed7/prg/gtksvtst.sd7       0.040637             94
seed7/prg/hal.sd7            0.046741            250
seed7/prg/hamu.sd7           0.067413            573
seed7/prg/hanoi.sd7          0.037676             55
seed7/prg/hd.sd7             0.039039             79
seed7/prg/hello.sd7          0.036272             32
seed7/prg/hilbert.sd7        0.040591            108
seed7/prg/ide7.sd7           0.047431            196
seed7/prg/kbd.sd7            0.038295             49
seed7/prg/klondike.sd7       0.106642            883
seed7/prg/lander.sd7         0.131374           1551
seed7/prg/lst80bas.sd7       0.056691            344
seed7/prg/lst99bas.sd7       0.059149            401
seed7/prg/lstgwbas.sd7       0.072982            577
seed7/prg/mahjong.sd7        0.145226           1943
seed7/prg/make7.sd7          0.043416            121
seed7/prg/mandelbr.sd7       0.049489            237
seed7/prg/mind.sd7           0.061606            443
seed7/prg/mirror.sd7         0.043907            131
seed7/prg/ms.sd7             0.068926            641
seed7/prg/nicoma.sd7         0.042754            135
seed7/prg/pac.sd7            0.073582            726
seed7/prg/pairs.sd7          0.139498           2025
seed7/prg/panic.sd7          0.192024           2634
seed7/prg/percolation.sd7    0.056474            330
seed7/prg/planets.sd7        0.139684           1486
seed7/prg/portfwd7.sd7       0.043590            139
seed7/prg/prime.sd7          0.038970             74
seed7/prg/printpi1.sd7       0.037982             56
seed7/prg/printpi2.sd7       0.037461             54
seed7/prg/printpi3.sd7       0.037233             60
seed7/prg/pv7.sd7            0.058780            337
seed7/prg/queen.sd7          0.043084            149
seed7/prg/rand.sd7           0.041503            121
seed7/prg/raytrace.sd7       0.069140            538
seed7/prg/rever.sd7          0.084159            816
seed7/prg/roman.sd7          0.037921             38
seed7/prg/s7c.sd7            0.619302           9060
seed7/prg/s7check.sd7        0.040384             68
seed7/prg/savehd7.sd7        0.111154           1110
seed7/prg/self.sd7           0.037921             49
seed7/prg/shisen.sd7         0.119297           1423
seed7/prg/sl.sd7             0.095115           1029
seed7/prg/snake.sd7          0.068825            615
seed7/prg/sokoban.sd7        0.084804            891
seed7/prg/spigotpi.sd7       0.038812             64
seed7/prg/sql7.sd7           0.052247            278
seed7/prg/startrek.sd7       0.094669            979
seed7/prg/sudoku7.sd7        0.193302           2657
seed7/prg/sydir7.sd7         0.059955            384
seed7/prg/syntaxhl.sd7       0.048581            177
seed7/prg/tak.sd7            0.037509             59
seed7/prg/tar7.sd7           0.042635            121
seed7/prg/tch.sd7            0.038096             55
seed7/prg/testfont.sd7       0.042051             95
seed7/prg/tet.sd7            0.059448            479
seed7/prg/tetg.sd7           0.061102            501
seed7/prg/toutf8.sd7         0.050485            240
seed7/prg/tst_cli.sd7        0.036686             40
seed7/prg/tst_srv.sd7        0.037607             47
seed7/prg/wator.sd7          0.079876            651
seed7/prg/which.sd7          0.038874             65
seed7/prg/wiz.sd7            0.209156           2833
seed7/prg/wordcnt.sd7        0.037807             54
seed7/prg/wrinum.sd7         0.037559             43
seed7/prg/wumpus.sd7         0.053773            372
seed7/lib/aes.s7i            0.195772           1144
seed7/lib/aes_gcm.s7i        0.059945            392
seed7/lib/ar.s7i             0.125013           1532
seed7/lib/arc4.s7i           0.043004            144
seed7/lib/archive.s7i        0.043402            143
seed7/lib/archive_base.s7i   0.043668            135
seed7/lib/array.s7i          0.075755            610
seed7/lib/asn1.s7i           0.065927            544
seed7/lib/asn1oid.s7i        0.050067            157
seed7/lib/basearray.s7i      0.064246            450
seed7/lib/bigfile.s7i        0.042182            136
seed7/lib/bigint.s7i         0.078091            824
seed7/lib/bigrat.s7i         0.079111            784
seed7/lib/bin16.s7i          0.068648            592
seed7/lib/bin32.s7i          0.061408            490
seed7/lib/bin64.s7i          0.063982            539
seed7/lib/bitdata.s7i        0.123491           1330
seed7/lib/bitmapfont.s7i     0.047487            215
seed7/lib/bitset.s7i         0.064157            593
seed7/lib/bitsetof.s7i       0.060712            431
seed7/lib/blowfish.s7i       0.075737            383
seed7/lib/bmp.s7i            0.097782            924
seed7/lib/boolean.s7i        0.054025            403
seed7/lib/browser.s7i        0.054128            280
seed7/lib/bstring.s7i        0.047305            227
seed7/lib/bytedata.s7i       0.064862            482
seed7/lib/bzip2.s7i          0.089668            887
seed7/lib/cards.s7i          0.102965           1342
seed7/lib/category.s7i       0.048478            209
seed7/lib/cc_conf.s7i        0.119976           1314
seed7/lib/ccittfax.s7i       0.101393           1022
seed7/lib/cgi.s7i            0.041715            109
seed7/lib/cgidialog.s7i      0.096427           1118
seed7/lib/char.s7i           0.052247            356
seed7/lib/charsets.s7i       0.123808           2024
seed7/lib/chartype.s7i       0.047154            121
seed7/lib/cipher.s7i         0.051172            146
seed7/lib/cli_cmds.s7i       0.117781           1360
seed7/lib/clib_file.s7i      0.052397            301
seed7/lib/color.s7i          0.048424            185
seed7/lib/complex.s7i        0.059886            464
seed7/lib/compress.s7i       0.044278            150
seed7/lib/console.s7i        0.045315            188
seed7/lib/cpio.s7i           0.144288           1708
seed7/lib/crc32.s7i          0.053440            193
seed7/lib/cronos16.s7i       0.193054           1173
seed7/lib/cronos27.s7i       0.251176           1464
seed7/lib/csv.s7i            0.047752            201
seed7/lib/db_prop.s7i        0.101959            991
seed7/lib/deflate.s7i        0.087686            740
seed7/lib/des.s7i            0.081238            444
seed7/lib/dialog.s7i         0.056525            311
seed7/lib/dir.s7i            0.043828            163
seed7/lib/draw.s7i           0.086149            854
seed7/lib/duration.s7i       0.098740           1038
seed7/lib/echo.s7i           0.043635            132
seed7/lib/editline.s7i       0.059537            398
seed7/lib/elf.s7i            0.154752           1560
seed7/lib/elliptic.s7i       0.076245            649
seed7/lib/enable_io.s7i      0.051214            312
seed7/lib/encoding.s7i       0.095838            931
seed7/lib/enumeration.s7i    0.049113            236
seed7/lib/environment.s7i    0.045731            175
seed7/lib/exif.s7i           0.047266            152
seed7/lib/external_file.s7i  0.052056            340
seed7/lib/field.s7i          0.052473            268
seed7/lib/file.s7i           0.052946            372
seed7/lib/filebits.s7i       0.037753             46
seed7/lib/filesys.s7i        0.063596            601
seed7/lib/fileutil.s7i       0.043636            144
seed7/lib/fixarray.s7i       0.053301            307
seed7/lib/float.s7i          0.074086            757
seed7/lib/font.s7i           0.045213            196
seed7/lib/font8x8.s7i        0.068747            998
seed7/lib/forloop.s7i        0.059475            449
seed7/lib/ftp.s7i            0.087489            969
seed7/lib/ftpserv.s7i        0.076338            631
seed7/lib/getf.s7i           0.041746            115
seed7/lib/gethttp.s7i        0.036980             41
seed7/lib/gethttps.s7i       0.036848             41
seed7/lib/gif.s7i            0.069878            561
seed7/lib/graph.s7i          0.064903            415
seed7/lib/graph_file.s7i     0.058235            399
seed7/lib/gtkserver.s7i      0.042935            161
seed7/lib/gzip.s7i           0.069235            573
seed7/lib/hash.s7i           0.067162            421
seed7/lib/hashsetof.s7i      0.066033            499
seed7/lib/hmac.s7i           0.046342            152
seed7/lib/html.s7i           0.042934             83
seed7/lib/html_ent.s7i       0.071002            476
seed7/lib/htmldom.s7i        0.057980            286
seed7/lib/http_request.s7i   0.078174            696
seed7/lib/http_srv_resp.s7i  0.058395            380
seed7/lib/https_request.s7i  0.047636            211
seed7/lib/httpserv.s7i       0.056122            345
seed7/lib/huffman.s7i        0.074132            644
seed7/lib/ico.s7i            0.048488            221
seed7/lib/idxarray.s7i       0.050233            232
seed7/lib/image.s7i          0.041161            156
seed7/lib/imagefile.s7i      0.045605            171
seed7/lib/inflate.s7i        0.064990            411
seed7/lib/inifile.s7i        0.042897            129
seed7/lib/integer.s7i        0.069428            663
seed7/lib/iobuffer.s7i       0.051088            289
seed7/lib/jpeg.s7i           0.154613           1761
seed7/lib/json.s7i           0.083392            891
seed7/lib/json_serde.s7i     0.082766            783
seed7/lib/keybd.s7i          0.080360            639
seed7/lib/keydescr.s7i       0.049511            192
seed7/lib/leb128.s7i         0.046724            218
seed7/lib/line.s7i           0.044144            164
seed7/lib/listener.s7i       0.048920            247
seed7/lib/logfile.s7i        0.038726             73
seed7/lib/lower.s7i          0.042376            142
seed7/lib/lzma.s7i           0.095488            934
seed7/lib/lzw.s7i            0.092473            861
seed7/lib/magic.s7i          0.066010            403
seed7/lib/mahjng32.s7i       0.095342           1500
seed7/lib/make.s7i           0.071058            544
seed7/lib/makedata.s7i       0.119167           1428
seed7/lib/math.s7i           0.045070            201
seed7/lib/mixarith.s7i       0.047896            249
seed7/lib/modern27.s7i       0.167266           1099
seed7/lib/more.s7i           0.042423            130
seed7/lib/msgdigest.s7i      0.136030           1222
seed7/lib/multiscr.s7i       0.038176             68
seed7/lib/null_file.s7i      0.051001            345
seed7/lib/osfiles.s7i        0.097247           1085
seed7/lib/pbm.s7i            0.048832            230
seed7/lib/pcx.s7i            0.077864            638
seed7/lib/pem.s7i            0.045927            185
seed7/lib/pgm.s7i            0.049766            238
seed7/lib/pic16.s7i          0.068420           1037
seed7/lib/pic32.s7i          0.122665           2060
seed7/lib/pic_util.s7i       0.044314            144
seed7/lib/pixelimage.s7i     0.053030            320
seed7/lib/pixmap_file.s7i    0.061917            459
seed7/lib/pixmapfont.s7i     0.048766            184
seed7/lib/pkcs1.s7i          0.078307            543
seed7/lib/png.s7i            0.107736           1064
seed7/lib/poll.s7i           0.051935            313
seed7/lib/ppm.s7i            0.047984            240
seed7/lib/process.s7i        0.065388            541
seed7/lib/progs.s7i          0.078421            789
seed7/lib/propertyfile.s7i   0.044464            155
seed7/lib/rational.s7i       0.078339            792
seed7/lib/ref_list.s7i       0.049630            252
seed7/lib/reference.s7i      0.043337            126
seed7/lib/reverse.s7i        0.040625             94
seed7/lib/rpm.s7i            0.283252           3487
seed7/lib/rpmext.s7i         0.052436            318
seed7/lib/scanfile.s7i       0.132060           1779
seed7/lib/scanjson.s7i       0.060585            413
seed7/lib/scanstri.s7i       0.136536           1814
seed7/lib/scantoml.s7i       0.131370           1603
seed7/lib/seed7_05.s7i       0.113293           1072
seed7/lib/set.s7i            0.039175             57
seed7/lib/shell.s7i          0.070077            615
seed7/lib/showtls.s7i        0.085968            678
seed7/lib/signature.s7i      0.045104            131
seed7/lib/smtp.s7i           0.050905            261
seed7/lib/sockbase.s7i       0.051938            217
seed7/lib/socket.s7i         0.052956            326
seed7/lib/sokoban1.s7i       0.081290           1519
seed7/lib/sql_base.s7i       0.094498           1000
seed7/lib/stars.s7i          0.233408           1705
seed7/lib/stdfont10.s7i      0.144548           3347
seed7/lib/stdfont12.s7i      0.165737           3928
seed7/lib/stdfont14.s7i      0.188360           4510
seed7/lib/stdfont16.s7i      0.212618           5092
seed7/lib/stdfont18.s7i      0.244615           5868
seed7/lib/stdfont20.s7i      0.273818           6449
seed7/lib/stdfont24.s7i      0.330685           7421
seed7/lib/stdfont8.s7i       0.127027           2960
seed7/lib/stdfont9.s7i       0.135061           3152
seed7/lib/stdio.s7i          0.052380            192
seed7/lib/strifile.s7i       0.055311            345
seed7/lib/string.s7i         0.075010            779
seed7/lib/stritext.s7i       0.054796            352
seed7/lib/struct.s7i         0.054970            266
seed7/lib/struct_elem.s7i    0.044107            129
seed7/lib/subfile.s7i        0.044912            174
seed7/lib/subrange.s7i       0.041715             78
seed7/lib/syntax.s7i         0.060688            294
seed7/lib/tar.s7i            0.145752           1880
seed7/lib/tar_cmds.s7i       0.084504            752
seed7/lib/tdes.s7i           0.044467            143
seed7/lib/tee.s7i            0.042181            143
seed7/lib/text.s7i           0.050834            135
seed7/lib/tga.s7i            0.081335            676
seed7/lib/tiff.s7i           0.242330           2771
seed7/lib/time.s7i           0.100608           1191
seed7/lib/tls.s7i            0.196730           2230
seed7/lib/unicode.s7i        0.073272            575
seed7/lib/unionfnd.s7i       0.042424            130
seed7/lib/upper.s7i          0.041216            142
seed7/lib/utf16.s7i          0.065471            540
seed7/lib/utf8.s7i           0.049029            234
seed7/lib/vecfont10.s7i      0.160663           1056
seed7/lib/vecfont18.s7i      0.175884           1119
seed7/lib/vector3d.s7i       0.049496            293
seed7/lib/vectorfont.s7i     0.047971            239
seed7/lib/wildcard.s7i       0.042499            140
seed7/lib/window.s7i         0.060809            455
seed7/lib/wrinum.s7i         0.051826            248
seed7/lib/x509cert.s7i       0.119283           1243
seed7/lib/xml_ent.s7i        0.042128             94
seed7/lib/xmldom.s7i         0.050536            303
seed7/lib/xz.s7i             0.061465            442
seed7/lib/zip.s7i            0.231692           2792
seed7/lib/zstd.s7i           0.119430           1263
============================ ================= ================

Statistical Summary — Mode D (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.158620        |
+-----------+-----------------+
| Minimum   | 0.036272        |
+-----------+-----------------+
| Maximum   | 5.546317        |
+-----------+-----------------+

Cross-Mode Comparison
=====================

+------+-----------------+-----------------+-----------------+
| Mode | Average (s)     | Minimum (s)     | Maximum (s)     |
+======+=================+=================+=================+
| A    | 0.033885        | 0.032407        | 0.042646        |
+------+-----------------+-----------------+-----------------+
| B    | 0.039124        | 0.035541        | 0.048630        |
+------+-----------------+-----------------+-----------------+
| C    | 0.088500        | 0.034208        | 2.496066        |
+------+-----------------+-----------------+-----------------+
| D    | 0.158620        | 0.036272        | 5.546317        |
+------+-----------------+-----------------+-----------------+

Phase Timing Summary
====================

Wall-clock times include warm-up work and GC performed by the warm-up helper.

+----------+--------------+--------------+--------------+
| Phase    | Warm-up      | Timed pass   | Mode total   |
+==========+==============+==============+==============+
| Mode A   | 00:00:12.478 | 00:00:57.730 | 00:01:10.209 |
+----------+--------------+--------------+--------------+
| Mode B   | 00:00:14.846 | 00:01:06.722 | 00:01:21.569 |
+----------+--------------+--------------+--------------+
| Mode C   | 00:00:36.022 | 00:02:31.633 | 00:03:07.655 |
+----------+--------------+--------------+--------------+
| Mode D   | 00:01:01.324 | 00:04:31.131 | 00:05:32.455 |
+----------+--------------+--------------+--------------+
| Total    |              |              | 00:11:11.896 |
+----------+--------------+--------------+--------------+


.. ---------------------------------------------------------------------------
