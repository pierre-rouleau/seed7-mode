=======================================================
GC-Controlled Benchmark Report: Seed7 Mode — Four Modes
=======================================================

:Running with: seed7-mode 2026-06-24T18:30:15+0000 W26-3
:Seed7 dir   : /Users/roup/my/dvo/seed7-repos/seed7/
:Started at: 14:45:50 local time
:Generated on: 2026-06-24 18:57:05 UTC
:N Iterations: 5  (mean of N timed opens per file)
:Window system: nil
:Display graphic: nil
:Frame size: 300x84 chars
:Window body: 300x82 chars
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
seed7/prg/addup.sd7          0.043033            190
seed7/prg/bas7.sd7           0.041685          11459
seed7/prg/bifurk.sd7         0.035718             73
seed7/prg/bigfiles.sd7       0.034653            129
seed7/prg/brainf7.sd7        0.034271             86
seed7/prg/calc7.sd7          0.033698            128
seed7/prg/carddemo.sd7       0.033657            190
seed7/prg/castle.sd7         0.033807           3148
seed7/prg/cat.sd7            0.032493             82
seed7/prg/cellauto.sd7       0.036500             85
seed7/prg/celsius.sd7        0.034512             42
seed7/prg/chk_all.sd7        0.034140            843
seed7/prg/chkarr.sd7         0.038329           8367
seed7/prg/chkbig.sd7         0.039335          29026
seed7/prg/chkbin.sd7         0.040990           6469
seed7/prg/chkbitdata.sd7     0.039314           6624
seed7/prg/chkbool.sd7        0.041714           3157
seed7/prg/chkbst.sd7         0.039285            722
seed7/prg/chkchr.sd7         0.042684           2809
seed7/prg/chkcmd.sd7         0.040771           1205
seed7/prg/chkdb.sd7          0.037836           7454
seed7/prg/chkdecl.sd7        0.036387            448
seed7/prg/chkenum.sd7        0.037775           1230
seed7/prg/chkerr.sd7         0.046100           4663
seed7/prg/chkexc.sd7         0.034645           2627
seed7/prg/chkfil.sd7         0.033924           1615
seed7/prg/chkflt.sd7         0.036091          20620
seed7/prg/chkhent.sd7        0.033001             54
seed7/prg/chkhsh.sd7         0.033428           4548
seed7/prg/chkidx.sd7         0.038379          19567
seed7/prg/chkint.sd7         0.045446          38129
seed7/prg/chkjson.sd7        0.034328           1764
seed7/prg/chkovf.sd7         0.034182           8216
seed7/prg/chkprc.sd7         0.033888          10111
seed7/prg/chkscan.sd7        0.033368            714
seed7/prg/chkset.sd7         0.041861          11974
seed7/prg/chkstr.sd7         0.046170          26952
seed7/prg/chktime.sd7        0.034422           2025
seed7/prg/chktoml.sd7        0.034257           1656
seed7/prg/clock.sd7          0.033572             47
seed7/prg/clock2.sd7         0.033715             43
seed7/prg/clock3.sd7         0.034371             95
seed7/prg/cmpfil.sd7         0.033685             84
seed7/prg/comanche.sd7       0.033630            180
seed7/prg/confval.sd7        0.033482            175
seed7/prg/db7.sd7            0.033861            417
seed7/prg/diff7.sd7          0.033981            263
seed7/prg/dirtst.sd7         0.034526             42
seed7/prg/dirx.sd7           0.033495            152
seed7/prg/dnafight.sd7       0.032875           1381
seed7/prg/dragon.sd7         0.033013             73
seed7/prg/echo.sd7           0.032424             39
seed7/prg/eliza.sd7          0.037365            302
seed7/prg/err.sd7            0.040722             96
seed7/prg/fannkuch.sd7       0.036664            131
seed7/prg/fib.sd7            0.033589             47
seed7/prg/find7.sd7          0.033463            133
seed7/prg/findchar.sd7       0.033430            149
seed7/prg/fractree.sd7       0.033630             55
seed7/prg/ftp7.sd7           0.033782            296
seed7/prg/ftpserv.sd7        0.034042             74
seed7/prg/gcd.sd7            0.033443            109
seed7/prg/gkbd.sd7           0.038747            358
seed7/prg/gtksvtst.sd7       0.034398             94
seed7/prg/hal.sd7            0.033511            250
seed7/prg/hamu.sd7           0.033798            573
seed7/prg/hanoi.sd7          0.033575             55
seed7/prg/hd.sd7             0.033519             79
seed7/prg/hello.sd7          0.033739             32
seed7/prg/hilbert.sd7        0.033484            108
seed7/prg/ide7.sd7           0.033838            196
seed7/prg/kbd.sd7            0.033380             49
seed7/prg/klondike.sd7       0.033788            883
seed7/prg/lander.sd7         0.033876           1551
seed7/prg/lst80bas.sd7       0.033711            344
seed7/prg/lst99bas.sd7       0.033780            401
seed7/prg/lstgwbas.sd7       0.033440            577
seed7/prg/mahjong.sd7        0.033799           1943
seed7/prg/make7.sd7          0.033584            121
seed7/prg/mandelbr.sd7       0.033377            237
seed7/prg/mind.sd7           0.032697            443
seed7/prg/mirror.sd7         0.032696            131
seed7/prg/ms.sd7             0.033473            641
seed7/prg/nicoma.sd7         0.032566            135
seed7/prg/pac.sd7            0.032719            726
seed7/prg/pairs.sd7          0.034221           2025
seed7/prg/panic.sd7          0.034008           2634
seed7/prg/percolation.sd7    0.033879            330
seed7/prg/planets.sd7        0.033821           1486
seed7/prg/portfwd7.sd7       0.034805            139
seed7/prg/prime.sd7          0.035095             74
seed7/prg/printpi1.sd7       0.033931             56
seed7/prg/printpi2.sd7       0.033592             54
seed7/prg/printpi3.sd7       0.034320             60
seed7/prg/pv7.sd7            0.033565            337
seed7/prg/queen.sd7          0.034015            149
seed7/prg/rand.sd7           0.033650            121
seed7/prg/raytrace.sd7       0.033538            538
seed7/prg/rever.sd7          0.033290            816
seed7/prg/roman.sd7          0.033635             38
seed7/prg/s7c.sd7            0.034648           9060
seed7/prg/s7check.sd7        0.033762             68
seed7/prg/savehd7.sd7        0.034349           1110
seed7/prg/self.sd7           0.040301             49
seed7/prg/shisen.sd7         0.036612           1423
seed7/prg/sl.sd7             0.034353           1029
seed7/prg/snake.sd7          0.033606            615
seed7/prg/sokoban.sd7        0.033693            891
seed7/prg/spigotpi.sd7       0.033756             64
seed7/prg/sql7.sd7           0.033298            278
seed7/prg/startrek.sd7       0.033095            979
seed7/prg/sudoku7.sd7        0.033401           2657
seed7/prg/sydir7.sd7         0.033061            384
seed7/prg/syntaxhl.sd7       0.032853            177
seed7/prg/tak.sd7            0.033065             59
seed7/prg/tar7.sd7           0.033653            121
seed7/prg/tch.sd7            0.034096             55
seed7/prg/testfont.sd7       0.033381             95
seed7/prg/tet.sd7            0.032555            479
seed7/prg/tetg.sd7           0.033435            501
seed7/prg/toutf8.sd7         0.033691            240
seed7/prg/tst_cli.sd7        0.033622             40
seed7/prg/tst_srv.sd7        0.033896             47
seed7/prg/wator.sd7          0.033614            651
seed7/prg/which.sd7          0.033739             65
seed7/prg/wiz.sd7            0.033811           2833
seed7/prg/wordcnt.sd7        0.033708             54
seed7/prg/wrinum.sd7         0.033386             43
seed7/prg/wumpus.sd7         0.033822            372
seed7/lib/aes.s7i            0.033815           1144
seed7/lib/aes_gcm.s7i        0.033847            392
seed7/lib/ar.s7i             0.034211           1532
seed7/lib/arc4.s7i           0.034262            144
seed7/lib/archive.s7i        0.033708            143
seed7/lib/archive_base.s7i   0.033836            135
seed7/lib/array.s7i          0.033451            610
seed7/lib/asn1.s7i           0.033423            544
seed7/lib/asn1oid.s7i        0.033580            157
seed7/lib/basearray.s7i      0.033616            450
seed7/lib/bigfile.s7i        0.032915            136
seed7/lib/bigint.s7i         0.032590            824
seed7/lib/bigrat.s7i         0.033214            784
seed7/lib/bin16.s7i          0.032784            592
seed7/lib/bin32.s7i          0.032988            490
seed7/lib/bin64.s7i          0.033196            539
seed7/lib/bitdata.s7i        0.034498           1330
seed7/lib/bitmapfont.s7i     0.033660            215
seed7/lib/bitset.s7i         0.033695            593
seed7/lib/bitsetof.s7i       0.033589            431
seed7/lib/blowfish.s7i       0.034506            383
seed7/lib/bmp.s7i            0.034021            924
seed7/lib/boolean.s7i        0.033941            403
seed7/lib/browser.s7i        0.034050            280
seed7/lib/bstring.s7i        0.033796            227
seed7/lib/bytedata.s7i       0.033661            482
seed7/lib/bzip2.s7i          0.033967            887
seed7/lib/cards.s7i          0.033757           1342
seed7/lib/category.s7i       0.033933            209
seed7/lib/cc_conf.s7i        0.033504           1314
seed7/lib/ccittfax.s7i       0.033457           1022
seed7/lib/cgi.s7i            0.037011            109
seed7/lib/cgidialog.s7i      0.039962           1118
seed7/lib/char.s7i           0.034610            356
seed7/lib/charsets.s7i       0.034107           2024
seed7/lib/chartype.s7i       0.033861            121
seed7/lib/cipher.s7i         0.034070            146
seed7/lib/cli_cmds.s7i       0.033753           1360
seed7/lib/clib_file.s7i      0.033860            301
seed7/lib/color.s7i          0.033928            185
seed7/lib/complex.s7i        0.034064            464
seed7/lib/compress.s7i       0.033740            150
seed7/lib/console.s7i        0.033498            188
seed7/lib/cpio.s7i           0.033208           1708
seed7/lib/crc32.s7i          0.033160            193
seed7/lib/cronos16.s7i       0.032720           1173
seed7/lib/cronos27.s7i       0.032881           1464
seed7/lib/csv.s7i            0.034329            201
seed7/lib/db_prop.s7i        0.034023            991
seed7/lib/deflate.s7i        0.034306            740
seed7/lib/des.s7i            0.034703            444
seed7/lib/dialog.s7i         0.034509            311
seed7/lib/dir.s7i            0.033914            163
seed7/lib/draw.s7i           0.034024            854
seed7/lib/duration.s7i       0.033684           1038
seed7/lib/echo.s7i           0.033369            132
seed7/lib/editline.s7i       0.033948            398
seed7/lib/elf.s7i            0.034568           1560
seed7/lib/elliptic.s7i       0.034636            649
seed7/lib/enable_io.s7i      0.033829            312
seed7/lib/encoding.s7i       0.033868            931
seed7/lib/enumeration.s7i    0.033795            236
seed7/lib/environment.s7i    0.033604            175
seed7/lib/exif.s7i           0.034344            152
seed7/lib/external_file.s7i  0.034234            340
seed7/lib/field.s7i          0.034289            268
seed7/lib/file.s7i           0.033609            372
seed7/lib/filebits.s7i       0.033789             46
seed7/lib/filesys.s7i        0.033745            601
seed7/lib/fileutil.s7i       0.033441            144
seed7/lib/fixarray.s7i       0.033868            307
seed7/lib/float.s7i          0.033328            757
seed7/lib/font.s7i           0.033634            196
seed7/lib/font8x8.s7i        0.034143            998
seed7/lib/forloop.s7i        0.033076            449
seed7/lib/ftp.s7i            0.033419            969
seed7/lib/ftpserv.s7i        0.034715            631
seed7/lib/getf.s7i           0.033924            115
seed7/lib/gethttp.s7i        0.034663             41
seed7/lib/gethttps.s7i       0.038350             41
seed7/lib/gif.s7i            0.033316            561
seed7/lib/graph.s7i          0.034075            415
seed7/lib/graph_file.s7i     0.033681            399
seed7/lib/gtkserver.s7i      0.033457            161
seed7/lib/gzip.s7i           0.033327            573
seed7/lib/hash.s7i           0.034079            421
seed7/lib/hashsetof.s7i      0.034424            499
seed7/lib/hmac.s7i           0.034678            152
seed7/lib/html.s7i           0.034561             83
seed7/lib/html_ent.s7i       0.033963            476
seed7/lib/htmldom.s7i        0.033673            286
seed7/lib/http_request.s7i   0.033613            696
seed7/lib/http_srv_resp.s7i  0.033521            380
seed7/lib/https_request.s7i  0.033784            211
seed7/lib/httpserv.s7i       0.033696            345
seed7/lib/huffman.s7i        0.034592            644
seed7/lib/ico.s7i            0.034070            221
seed7/lib/idxarray.s7i       0.033677            232
seed7/lib/image.s7i          0.033792            156
seed7/lib/imagefile.s7i      0.033862            171
seed7/lib/inflate.s7i        0.033576            411
seed7/lib/inifile.s7i        0.033875            129
seed7/lib/integer.s7i        0.033739            663
seed7/lib/iobuffer.s7i       0.033855            289
seed7/lib/jpeg.s7i           0.033259           1761
seed7/lib/json.s7i           0.032729            891
seed7/lib/json_serde.s7i     0.033379            783
seed7/lib/keybd.s7i          0.033869            639
seed7/lib/keydescr.s7i       0.033695            192
seed7/lib/leb128.s7i         0.033225            218
seed7/lib/line.s7i           0.033918            164
seed7/lib/listener.s7i       0.034103            247
seed7/lib/logfile.s7i        0.033697             73
seed7/lib/lower.s7i          0.033735            142
seed7/lib/lzma.s7i           0.033661            934
seed7/lib/lzw.s7i            0.033915            861
seed7/lib/magic.s7i          0.033720            403
seed7/lib/mahjng32.s7i       0.033684           1500
seed7/lib/make.s7i           0.033417            544
seed7/lib/makedata.s7i       0.033946           1428
seed7/lib/math.s7i           0.033229            201
seed7/lib/mixarith.s7i       0.033014            249
seed7/lib/modern27.s7i       0.033055           1099
seed7/lib/more.s7i           0.032709            130
seed7/lib/msgdigest.s7i      0.033277           1222
seed7/lib/multiscr.s7i       0.033234             68
seed7/lib/null_file.s7i      0.033124            345
seed7/lib/osfiles.s7i        0.033226           1085
seed7/lib/pbm.s7i            0.033272            230
seed7/lib/pcx.s7i            0.033351            638
seed7/lib/pem.s7i            0.033612            185
seed7/lib/pgm.s7i            0.033487            238
seed7/lib/pic16.s7i          0.034093           1037
seed7/lib/pic32.s7i          0.034229           2060
seed7/lib/pic_util.s7i       0.033952            144
seed7/lib/pixelimage.s7i     0.033881            320
seed7/lib/pixmap_file.s7i    0.032858            459
seed7/lib/pixmapfont.s7i     0.037275            184
seed7/lib/pkcs1.s7i          0.035086            543
seed7/lib/png.s7i            0.033586           1064
seed7/lib/poll.s7i           0.033366            313
seed7/lib/ppm.s7i            0.033863            240
seed7/lib/process.s7i        0.034830            541
seed7/lib/progs.s7i          0.035491            789
seed7/lib/propertyfile.s7i   0.033817            155
seed7/lib/rational.s7i       0.033695            792
seed7/lib/ref_list.s7i       0.033923            252
seed7/lib/reference.s7i      0.033828            126
seed7/lib/reverse.s7i        0.034064             94
seed7/lib/rpm.s7i            0.034294           3487
seed7/lib/rpmext.s7i         0.033804            318
seed7/lib/scanfile.s7i       0.033726           1779
seed7/lib/scanjson.s7i       0.033559            413
seed7/lib/scanstri.s7i       0.033635           1814
seed7/lib/scantoml.s7i       0.033433           1603
seed7/lib/seed7_05.s7i       0.033684           1072
seed7/lib/set.s7i            0.033622             57
seed7/lib/shell.s7i          0.033890            615
seed7/lib/showtls.s7i        0.033666            678
seed7/lib/signature.s7i      0.033564            131
seed7/lib/smtp.s7i           0.033493            261
seed7/lib/sockbase.s7i       0.033699            217
seed7/lib/socket.s7i         0.033578            326
seed7/lib/sokoban1.s7i       0.033773           1519
seed7/lib/sql_base.s7i       0.033774           1000
seed7/lib/stars.s7i          0.033514           1705
seed7/lib/stdfont10.s7i      0.033039           3347
seed7/lib/stdfont12.s7i      0.033224           3928
seed7/lib/stdfont14.s7i      0.033145           4510
seed7/lib/stdfont16.s7i      0.033098           5092
seed7/lib/stdfont18.s7i      0.033265           5868
seed7/lib/stdfont20.s7i      0.033775           6449
seed7/lib/stdfont24.s7i      0.033744           7421
seed7/lib/stdfont8.s7i       0.034723           2960
seed7/lib/stdfont9.s7i       0.035340           3152
seed7/lib/stdio.s7i          0.033550            192
seed7/lib/strifile.s7i       0.033959            345
seed7/lib/string.s7i         0.033787            779
seed7/lib/stritext.s7i       0.033984            352
seed7/lib/struct.s7i         0.033931            266
seed7/lib/struct_elem.s7i    0.033898            129
seed7/lib/subfile.s7i        0.033780            174
seed7/lib/subrange.s7i       0.033865             78
seed7/lib/syntax.s7i         0.034012            294
seed7/lib/tar.s7i            0.033877           1880
seed7/lib/tar_cmds.s7i       0.033674            752
seed7/lib/tdes.s7i           0.033732            143
seed7/lib/tee.s7i            0.033755            143
seed7/lib/text.s7i           0.033991            135
seed7/lib/tga.s7i            0.033760            676
seed7/lib/tiff.s7i           0.033879           2771
seed7/lib/time.s7i           0.033947           1191
seed7/lib/tls.s7i            0.034006           2230
seed7/lib/unicode.s7i        0.033588            575
seed7/lib/unionfnd.s7i       0.034082            130
seed7/lib/upper.s7i          0.033868            142
seed7/lib/utf16.s7i          0.035523            540
seed7/lib/utf8.s7i           0.034289            234
seed7/lib/vecfont10.s7i      0.034019           1056
seed7/lib/vecfont18.s7i      0.032853           1119
seed7/lib/vector3d.s7i       0.032764            293
seed7/lib/vectorfont.s7i     0.036920            239
seed7/lib/wildcard.s7i       0.035958            140
seed7/lib/window.s7i         0.033199            455
seed7/lib/wrinum.s7i         0.033708            248
seed7/lib/x509cert.s7i       0.034932           1243
seed7/lib/xml_ent.s7i        0.033599             94
seed7/lib/xmldom.s7i         0.033467            303
seed7/lib/xz.s7i             0.033777            442
seed7/lib/zip.s7i            0.033879           2792
seed7/lib/zstd.s7i           0.033879           1263
============================ ================= ================

Statistical Summary — Mode A (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.034296        |
+-----------+-----------------+
| Minimum   | 0.032424        |
+-----------+-----------------+
| Maximum   | 0.046170        |
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
seed7/prg/addup.sd7          0.039753            190
seed7/prg/bas7.sd7           0.041555          11459
seed7/prg/bifurk.sd7         0.037630             73
seed7/prg/bigfiles.sd7       0.039280            129
seed7/prg/brainf7.sd7        0.037919             86
seed7/prg/calc7.sd7          0.038822            128
seed7/prg/carddemo.sd7       0.040122            190
seed7/prg/castle.sd7         0.040005           3148
seed7/prg/cat.sd7            0.037603             82
seed7/prg/cellauto.sd7       0.037800             85
seed7/prg/celsius.sd7        0.036886             42
seed7/prg/chk_all.sd7        0.038547            843
seed7/prg/chkarr.sd7         0.038187           8367
seed7/prg/chkbig.sd7         0.042727          29026
seed7/prg/chkbin.sd7         0.039329           6469
seed7/prg/chkbitdata.sd7     0.040425           6624
seed7/prg/chkbool.sd7        0.037792           3157
seed7/prg/chkbst.sd7         0.037857            722
seed7/prg/chkchr.sd7         0.038338           2809
seed7/prg/chkcmd.sd7         0.036893           1205
seed7/prg/chkdb.sd7          0.039567           7454
seed7/prg/chkdecl.sd7        0.037718            448
seed7/prg/chkenum.sd7        0.036349           1230
seed7/prg/chkerr.sd7         0.038713           4663
seed7/prg/chkexc.sd7         0.036921           2627
seed7/prg/chkfil.sd7         0.038687           1615
seed7/prg/chkflt.sd7         0.040259          20620
seed7/prg/chkhent.sd7        0.035317             54
seed7/prg/chkhsh.sd7         0.038369           4548
seed7/prg/chkidx.sd7         0.042367          19567
seed7/prg/chkint.sd7         0.044500          38129
seed7/prg/chkjson.sd7        0.038542           1764
seed7/prg/chkovf.sd7         0.038112           8216
seed7/prg/chkprc.sd7         0.038355          10111
seed7/prg/chkscan.sd7        0.039365            714
seed7/prg/chkset.sd7         0.040721          11974
seed7/prg/chkstr.sd7         0.042164          26952
seed7/prg/chktime.sd7        0.039844           2025
seed7/prg/chktoml.sd7        0.039202           1656
seed7/prg/clock.sd7          0.036305             47
seed7/prg/clock2.sd7         0.036880             43
seed7/prg/clock3.sd7         0.039264             95
seed7/prg/cmpfil.sd7         0.037448             84
seed7/prg/comanche.sd7       0.038430            180
seed7/prg/confval.sd7        0.039148            175
seed7/prg/db7.sd7            0.038841            417
seed7/prg/diff7.sd7          0.039008            263
seed7/prg/dirtst.sd7         0.035753             42
seed7/prg/dirx.sd7           0.038433            152
seed7/prg/dnafight.sd7       0.038498           1381
seed7/prg/dragon.sd7         0.036623             73
seed7/prg/echo.sd7           0.035306             39
seed7/prg/eliza.sd7          0.037566            302
seed7/prg/err.sd7            0.039888             96
seed7/prg/fannkuch.sd7       0.036677            131
seed7/prg/fib.sd7            0.034987             47
seed7/prg/find7.sd7          0.038578            133
seed7/prg/findchar.sd7       0.038846            149
seed7/prg/fractree.sd7       0.036946             55
seed7/prg/ftp7.sd7           0.039114            296
seed7/prg/ftpserv.sd7        0.037670             74
seed7/prg/gcd.sd7            0.037230            109
seed7/prg/gkbd.sd7           0.039190            358
seed7/prg/gtksvtst.sd7       0.037485             94
seed7/prg/hal.sd7            0.037009            250
seed7/prg/hamu.sd7           0.037331            573
seed7/prg/hanoi.sd7          0.036639             55
seed7/prg/hd.sd7             0.038020             79
seed7/prg/hello.sd7          0.037128             32
seed7/prg/hilbert.sd7        0.038242            108
seed7/prg/ide7.sd7           0.038524            196
seed7/prg/kbd.sd7            0.035953             49
seed7/prg/klondike.sd7       0.038835            883
seed7/prg/lander.sd7         0.039649           1551
seed7/prg/lst80bas.sd7       0.037135            344
seed7/prg/lst99bas.sd7       0.036951            401
seed7/prg/lstgwbas.sd7       0.036726            577
seed7/prg/mahjong.sd7        0.037720           1943
seed7/prg/make7.sd7          0.038285            121
seed7/prg/mandelbr.sd7       0.038460            237
seed7/prg/mind.sd7           0.036472            443
seed7/prg/mirror.sd7         0.037507            131
seed7/prg/ms.sd7             0.037992            641
seed7/prg/nicoma.sd7         0.037620            135
seed7/prg/pac.sd7            0.038804            726
seed7/prg/pairs.sd7          0.039020           2025
seed7/prg/panic.sd7          0.039242           2634
seed7/prg/percolation.sd7    0.039381            330
seed7/prg/planets.sd7        0.039637           1486
seed7/prg/portfwd7.sd7       0.038729            139
seed7/prg/prime.sd7          0.037949             74
seed7/prg/printpi1.sd7       0.036943             56
seed7/prg/printpi2.sd7       0.036771             54
seed7/prg/printpi3.sd7       0.036814             60
seed7/prg/pv7.sd7            0.038508            337
seed7/prg/queen.sd7          0.037172            149
seed7/prg/rand.sd7           0.037442            121
seed7/prg/raytrace.sd7       0.038936            538
seed7/prg/rever.sd7          0.038656            816
seed7/prg/roman.sd7          0.035956             38
seed7/prg/s7c.sd7            0.038831           9060
seed7/prg/s7check.sd7        0.036760             68
seed7/prg/savehd7.sd7        0.038874           1110
seed7/prg/self.sd7           0.036285             49
seed7/prg/shisen.sd7         0.038276           1423
seed7/prg/sl.sd7             0.036470           1029
seed7/prg/snake.sd7          0.036778            615
seed7/prg/sokoban.sd7        0.037678            891
seed7/prg/spigotpi.sd7       0.035394             64
seed7/prg/sql7.sd7           0.037061            278
seed7/prg/startrek.sd7       0.038785            979
seed7/prg/sudoku7.sd7        0.039105           2657
seed7/prg/sydir7.sd7         0.038876            384
seed7/prg/syntaxhl.sd7       0.039633            177
seed7/prg/tak.sd7            0.036387             59
seed7/prg/tar7.sd7           0.038528            121
seed7/prg/tch.sd7            0.036090             55
seed7/prg/testfont.sd7       0.040160             95
seed7/prg/tet.sd7            0.040756            479
seed7/prg/tetg.sd7           0.039084            501
seed7/prg/toutf8.sd7         0.038646            240
seed7/prg/tst_cli.sd7        0.035571             40
seed7/prg/tst_srv.sd7        0.035862             47
seed7/prg/wator.sd7          0.036965            651
seed7/prg/which.sd7          0.037012             65
seed7/prg/wiz.sd7            0.038711           2833
seed7/prg/wordcnt.sd7        0.037477             54
seed7/prg/wrinum.sd7         0.035530             43
seed7/prg/wumpus.sd7         0.038473            372
seed7/lib/aes.s7i            0.041854           1144
seed7/lib/aes_gcm.s7i        0.039766            392
seed7/lib/ar.s7i             0.039185           1532
seed7/lib/arc4.s7i           0.037980            144
seed7/lib/archive.s7i        0.037639            143
seed7/lib/archive_base.s7i   0.037667            135
seed7/lib/array.s7i          0.037593            610
seed7/lib/asn1.s7i           0.036148            544
seed7/lib/asn1oid.s7i        0.041671            157
seed7/lib/basearray.s7i      0.044325            450
seed7/lib/bigfile.s7i        0.042385            136
seed7/lib/bigint.s7i         0.039508            824
seed7/lib/bigrat.s7i         0.039300            784
seed7/lib/bin16.s7i          0.039115            592
seed7/lib/bin32.s7i          0.038727            490
seed7/lib/bin64.s7i          0.039159            539
seed7/lib/bitdata.s7i        0.042865           1330
seed7/lib/bitmapfont.s7i     0.038918            215
seed7/lib/bitset.s7i         0.038644            593
seed7/lib/bitsetof.s7i       0.039379            431
seed7/lib/blowfish.s7i       0.042697            383
seed7/lib/bmp.s7i            0.038905            924
seed7/lib/boolean.s7i        0.037447            403
seed7/lib/browser.s7i        0.039010            280
seed7/lib/bstring.s7i        0.038651            227
seed7/lib/bytedata.s7i       0.040102            482
seed7/lib/bzip2.s7i          0.039067            887
seed7/lib/cards.s7i          0.036923           1342
seed7/lib/category.s7i       0.038963            209
seed7/lib/cc_conf.s7i        0.038469           1314
seed7/lib/ccittfax.s7i       0.038251           1022
seed7/lib/cgi.s7i            0.037287            109
seed7/lib/cgidialog.s7i      0.037597           1118
seed7/lib/char.s7i           0.037601            356
seed7/lib/charsets.s7i       0.038172           2024
seed7/lib/chartype.s7i       0.042617            121
seed7/lib/cipher.s7i         0.038485            146
seed7/lib/cli_cmds.s7i       0.037998           1360
seed7/lib/clib_file.s7i      0.038063            301
seed7/lib/color.s7i          0.038870            185
seed7/lib/complex.s7i        0.037622            464
seed7/lib/compress.s7i       0.039586            150
seed7/lib/console.s7i        0.038909            188
seed7/lib/cpio.s7i           0.038868           1708
seed7/lib/crc32.s7i          0.040081            193
seed7/lib/cronos16.s7i       0.040699           1173
seed7/lib/cronos27.s7i       0.048164           1464
seed7/lib/csv.s7i            0.041386            201
seed7/lib/db_prop.s7i        0.039258            991
seed7/lib/deflate.s7i        0.038818            740
seed7/lib/des.s7i            0.039484            444
seed7/lib/dialog.s7i         0.038853            311
seed7/lib/dir.s7i            0.038760            163
seed7/lib/draw.s7i           0.039271            854
seed7/lib/duration.s7i       0.038938           1038
seed7/lib/echo.s7i           0.038734            132
seed7/lib/editline.s7i       0.038382            398
seed7/lib/elf.s7i            0.038140           1560
seed7/lib/elliptic.s7i       0.036360            649
seed7/lib/enable_io.s7i      0.037583            312
seed7/lib/encoding.s7i       0.037955            931
seed7/lib/enumeration.s7i    0.037983            236
seed7/lib/environment.s7i    0.038024            175
seed7/lib/exif.s7i           0.039070            152
seed7/lib/external_file.s7i  0.038704            340
seed7/lib/field.s7i          0.038714            268
seed7/lib/file.s7i           0.038786            372
seed7/lib/filebits.s7i       0.039450             46
seed7/lib/filesys.s7i        0.041036            601
seed7/lib/fileutil.s7i       0.039278            144
seed7/lib/fixarray.s7i       0.038955            307
seed7/lib/float.s7i          0.039285            757
seed7/lib/font.s7i           0.039595            196
seed7/lib/font8x8.s7i        0.038364            998
seed7/lib/forloop.s7i        0.039217            449
seed7/lib/ftp.s7i            0.039234            969
seed7/lib/ftpserv.s7i        0.039182            631
seed7/lib/getf.s7i           0.039949            115
seed7/lib/gethttp.s7i        0.036855             41
seed7/lib/gethttps.s7i       0.036521             41
seed7/lib/gif.s7i            0.038939            561
seed7/lib/graph.s7i          0.040994            415
seed7/lib/graph_file.s7i     0.037842            399
seed7/lib/gtkserver.s7i      0.037533            161
seed7/lib/gzip.s7i           0.037640            573
seed7/lib/hash.s7i           0.039237            421
seed7/lib/hashsetof.s7i      0.039623            499
seed7/lib/hmac.s7i           0.037620            152
seed7/lib/html.s7i           0.036974             83
seed7/lib/html_ent.s7i       0.038403            476
seed7/lib/htmldom.s7i        0.039003            286
seed7/lib/http_request.s7i   0.039353            696
seed7/lib/http_srv_resp.s7i  0.040192            380
seed7/lib/https_request.s7i  0.038919            211
seed7/lib/httpserv.s7i       0.038636            345
seed7/lib/huffman.s7i        0.038464            644
seed7/lib/ico.s7i            0.039330            221
seed7/lib/idxarray.s7i       0.042599            232
seed7/lib/image.s7i          0.037206            156
seed7/lib/imagefile.s7i      0.038581            171
seed7/lib/inflate.s7i        0.039003            411
seed7/lib/inifile.s7i        0.038505            129
seed7/lib/integer.s7i        0.038492            663
seed7/lib/iobuffer.s7i       0.041205            289
seed7/lib/jpeg.s7i           0.040801           1761
seed7/lib/json.s7i           0.039662            891
seed7/lib/json_serde.s7i     0.040577            783
seed7/lib/keybd.s7i          0.040039            639
seed7/lib/keydescr.s7i       0.040451            192
seed7/lib/leb128.s7i         0.039415            218
seed7/lib/line.s7i           0.036190            164
seed7/lib/listener.s7i       0.037307            247
seed7/lib/logfile.s7i        0.036789             73
seed7/lib/lower.s7i          0.037142            142
seed7/lib/lzma.s7i           0.037909            934
seed7/lib/lzw.s7i            0.041378            861
seed7/lib/magic.s7i          0.040429            403
seed7/lib/mahjng32.s7i       0.038869           1500
seed7/lib/make.s7i           0.040349            544
seed7/lib/makedata.s7i       0.039176           1428
seed7/lib/math.s7i           0.038531            201
seed7/lib/mixarith.s7i       0.038893            249
seed7/lib/modern27.s7i       0.040737           1099
seed7/lib/more.s7i           0.038839            130
seed7/lib/msgdigest.s7i      0.038705           1222
seed7/lib/multiscr.s7i       0.036366             68
seed7/lib/null_file.s7i      0.037529            345
seed7/lib/osfiles.s7i        0.041197           1085
seed7/lib/pbm.s7i            0.041685            230
seed7/lib/pcx.s7i            0.042431            638
seed7/lib/pem.s7i            0.042551            185
seed7/lib/pgm.s7i            0.041731            238
seed7/lib/pic16.s7i          0.039777           1037
seed7/lib/pic32.s7i          0.039096           2060
seed7/lib/pic_util.s7i       0.039483            144
seed7/lib/pixelimage.s7i     0.041963            320
seed7/lib/pixmap_file.s7i    0.039909            459
seed7/lib/pixmapfont.s7i     0.038367            184
seed7/lib/pkcs1.s7i          0.045543            543
seed7/lib/png.s7i            0.037950           1064
seed7/lib/poll.s7i           0.038906            313
seed7/lib/ppm.s7i            0.038001            240
seed7/lib/process.s7i        0.039250            541
seed7/lib/progs.s7i          0.039604            789
seed7/lib/propertyfile.s7i   0.039109            155
seed7/lib/rational.s7i       0.039092            792
seed7/lib/ref_list.s7i       0.038720            252
seed7/lib/reference.s7i      0.038735            126
seed7/lib/reverse.s7i        0.038520             94
seed7/lib/rpm.s7i            0.038305           3487
seed7/lib/rpmext.s7i         0.038044            318
seed7/lib/scanfile.s7i       0.038576           1779
seed7/lib/scanjson.s7i       0.038196            413
seed7/lib/scanstri.s7i       0.038326           1814
seed7/lib/scantoml.s7i       0.037807           1603
seed7/lib/seed7_05.s7i       0.039927           1072
seed7/lib/set.s7i            0.036599             57
seed7/lib/shell.s7i          0.038251            615
seed7/lib/showtls.s7i        0.040036            678
seed7/lib/signature.s7i      0.038874            131
seed7/lib/smtp.s7i           0.039363            261
seed7/lib/sockbase.s7i       0.038822            217
seed7/lib/socket.s7i         0.038951            326
seed7/lib/sokoban1.s7i       0.037727           1519
seed7/lib/sql_base.s7i       0.037993           1000
seed7/lib/stars.s7i          0.044302           1705
seed7/lib/stdfont10.s7i      0.041817           3347
seed7/lib/stdfont12.s7i      0.037486           3928
seed7/lib/stdfont14.s7i      0.036656           4510
seed7/lib/stdfont16.s7i      0.039157           5092
seed7/lib/stdfont18.s7i      0.037853           5868
seed7/lib/stdfont20.s7i      0.037775           6449
seed7/lib/stdfont24.s7i      0.040054           7421
seed7/lib/stdfont8.s7i       0.037754           2960
seed7/lib/stdfont9.s7i       0.037522           3152
seed7/lib/stdio.s7i          0.038571            192
seed7/lib/strifile.s7i       0.038754            345
seed7/lib/string.s7i         0.038992            779
seed7/lib/stritext.s7i       0.039248            352
seed7/lib/struct.s7i         0.039589            266
seed7/lib/struct_elem.s7i    0.038639            129
seed7/lib/subfile.s7i        0.038730            174
seed7/lib/subrange.s7i       0.037917             78
seed7/lib/syntax.s7i         0.039794            294
seed7/lib/tar.s7i            0.039294           1880
seed7/lib/tar_cmds.s7i       0.039125            752
seed7/lib/tdes.s7i           0.039130            143
seed7/lib/tee.s7i            0.038802            143
seed7/lib/text.s7i           0.038743            135
seed7/lib/tga.s7i            0.041054            676
seed7/lib/tiff.s7i           0.039945           2771
seed7/lib/time.s7i           0.038752           1191
seed7/lib/tls.s7i            0.037861           2230
seed7/lib/unicode.s7i        0.038555            575
seed7/lib/unionfnd.s7i       0.036656            130
seed7/lib/upper.s7i          0.038736            142
seed7/lib/utf16.s7i          0.037634            540
seed7/lib/utf8.s7i           0.039190            234
seed7/lib/vecfont10.s7i      0.039294           1056
seed7/lib/vecfont18.s7i      0.039543           1119
seed7/lib/vector3d.s7i       0.037734            293
seed7/lib/vectorfont.s7i     0.038953            239
seed7/lib/wildcard.s7i       0.038359            140
seed7/lib/window.s7i         0.038754            455
seed7/lib/wrinum.s7i         0.038809            248
seed7/lib/x509cert.s7i       0.040093           1243
seed7/lib/xml_ent.s7i        0.038877             94
seed7/lib/xmldom.s7i         0.037670            303
seed7/lib/xz.s7i             0.039098            442
seed7/lib/zip.s7i            0.039275           2792
seed7/lib/zstd.s7i           0.039208           1263
============================ ================= ================

Statistical Summary — Mode B (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.038740        |
+-----------+-----------------+
| Minimum   | 0.034987        |
+-----------+-----------------+
| Maximum   | 0.048164        |
+-----------+-----------------+

Mode C — Mode Activation + Full-Buffer Fontification (font-lock-ensure)
-----------------------------------------------------------------------

Full-buffer fontification stress test.

File Load Times — Mode C (mean, GC-free)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

============================ ================= ================
File Name                    Mean Time (s)     Line count
============================ ================= ================
seed7/prg/addup.sd7          0.039510            190
seed7/prg/bas7.sd7           0.324084          11459
seed7/prg/bifurk.sd7         0.036801             73
seed7/prg/bigfiles.sd7       0.036685            129
seed7/prg/brainf7.sd7        0.036176             86
seed7/prg/calc7.sd7          0.037237            128
seed7/prg/carddemo.sd7       0.038004            190
seed7/prg/castle.sd7         0.109823           3148
seed7/prg/cat.sd7            0.039826             82
seed7/prg/cellauto.sd7       0.041919             85
seed7/prg/celsius.sd7        0.041773             42
seed7/prg/chk_all.sd7        0.065823            843
seed7/prg/chkarr.sd7         0.366574           8367
seed7/prg/chkbig.sd7         2.170022          29026
seed7/prg/chkbin.sd7         0.519614           6469
seed7/prg/chkbitdata.sd7     0.607730           6624
seed7/prg/chkbool.sd7        0.115121           3157
seed7/prg/chkbst.sd7         0.061152            722
seed7/prg/chkchr.sd7         0.211585           2809
seed7/prg/chkcmd.sd7         0.067677           1205
seed7/prg/chkdb.sd7          0.348250           7454
seed7/prg/chkdecl.sd7        0.057952            448
seed7/prg/chkenum.sd7        0.067576           1230
seed7/prg/chkerr.sd7         0.192183           4663
seed7/prg/chkexc.sd7         0.083943           2627
seed7/prg/chkfil.sd7         0.073946           1615
seed7/prg/chkflt.sd7         1.327770          20620
seed7/prg/chkhent.sd7        0.037734             54
seed7/prg/chkhsh.sd7         0.244878           4548
seed7/prg/chkidx.sd7         1.303036          19567
seed7/prg/chkint.sd7         2.496171          38129
seed7/prg/chkjson.sd7        0.104529           1764
seed7/prg/chkovf.sd7         0.557039           8216
seed7/prg/chkprc.sd7         0.317987          10111
seed7/prg/chkscan.sd7        0.058005            714
seed7/prg/chkset.sd7         0.659361          11974
seed7/prg/chkstr.sd7         1.364678          26952
seed7/prg/chktime.sd7        0.128089           2025
seed7/prg/chktoml.sd7        0.105794           1656
seed7/prg/clock.sd7          0.038507             47
seed7/prg/clock2.sd7         0.042753             43
seed7/prg/clock3.sd7         0.037545             95
seed7/prg/cmpfil.sd7         0.035606             84
seed7/prg/comanche.sd7       0.040775            180
seed7/prg/confval.sd7        0.041659            175
seed7/prg/db7.sd7            0.045616            417
seed7/prg/diff7.sd7          0.042534            263
seed7/prg/dirtst.sd7         0.035942             42
seed7/prg/dirx.sd7           0.037310            152
seed7/prg/dnafight.sd7       0.064727           1381
seed7/prg/dragon.sd7         0.035454             73
seed7/prg/echo.sd7           0.034406             39
seed7/prg/eliza.sd7          0.042352            302
seed7/prg/err.sd7            0.040617             96
seed7/prg/fannkuch.sd7       0.037488            131
seed7/prg/fib.sd7            0.035832             47
seed7/prg/find7.sd7          0.038059            133
seed7/prg/findchar.sd7       0.038738            149
seed7/prg/fractree.sd7       0.036105             55
seed7/prg/ftp7.sd7           0.042360            296
seed7/prg/ftpserv.sd7        0.037034             74
seed7/prg/gcd.sd7            0.036826            109
seed7/prg/gkbd.sd7           0.049407            358
seed7/prg/gtksvtst.sd7       0.037426             94
seed7/prg/hal.sd7            0.039782            250
seed7/prg/hamu.sd7           0.048688            573
seed7/prg/hanoi.sd7          0.036244             55
seed7/prg/hd.sd7             0.036450             79
seed7/prg/hello.sd7          0.035494             32
seed7/prg/hilbert.sd7        0.037175            108
seed7/prg/ide7.sd7           0.039859            196
seed7/prg/kbd.sd7            0.035653             49
seed7/prg/klondike.sd7       0.055306            883
seed7/prg/lander.sd7         0.071911           1551
seed7/prg/lst80bas.sd7       0.042621            344
seed7/prg/lst99bas.sd7       0.043981            401
seed7/prg/lstgwbas.sd7       0.049187            577
seed7/prg/mahjong.sd7        0.078274           1943
seed7/prg/make7.sd7          0.039404            121
seed7/prg/mandelbr.sd7       0.039345            237
seed7/prg/mind.sd7           0.043819            443
seed7/prg/mirror.sd7         0.037800            131
seed7/prg/ms.sd7             0.047901            641
seed7/prg/nicoma.sd7         0.038871            135
seed7/prg/pac.sd7            0.049507            726
seed7/prg/pairs.sd7          0.082312           2025
seed7/prg/panic.sd7          0.097169           2634
seed7/prg/percolation.sd7    0.043424            330
seed7/prg/planets.sd7        0.076163           1486
seed7/prg/portfwd7.sd7       0.038706            139
seed7/prg/prime.sd7          0.036139             74
seed7/prg/printpi1.sd7       0.040481             56
seed7/prg/printpi2.sd7       0.040092             54
seed7/prg/printpi3.sd7       0.039605             60
seed7/prg/pv7.sd7            0.045407            337
seed7/prg/queen.sd7          0.037908            149
seed7/prg/rand.sd7           0.036655            121
seed7/prg/raytrace.sd7       0.047225            538
seed7/prg/rever.sd7          0.052517            816
seed7/prg/roman.sd7          0.035603             38
seed7/prg/s7c.sd7            0.279481           9060
seed7/prg/s7check.sd7        0.036879             68
seed7/prg/savehd7.sd7        0.069273           1110
seed7/prg/self.sd7           0.037886             49
seed7/prg/shisen.sd7         0.071535           1423
seed7/prg/sl.sd7             0.059962           1029
seed7/prg/snake.sd7          0.046736            615
seed7/prg/sokoban.sd7        0.054822            891
seed7/prg/spigotpi.sd7       0.036108             64
seed7/prg/sql7.sd7           0.041887            278
seed7/prg/startrek.sd7       0.058688            979
seed7/prg/sudoku7.sd7        0.098125           2657
seed7/prg/sydir7.sd7         0.044339            384
seed7/prg/syntaxhl.sd7       0.041072            177
seed7/prg/tak.sd7            0.035492             59
seed7/prg/tar7.sd7           0.038005            121
seed7/prg/tch.sd7            0.037016             55
seed7/prg/testfont.sd7       0.036629             95
seed7/prg/tet.sd7            0.044924            479
seed7/prg/tetg.sd7           0.045809            501
seed7/prg/toutf8.sd7         0.043010            240
seed7/prg/tst_cli.sd7        0.035760             40
seed7/prg/tst_srv.sd7        0.035419             47
seed7/prg/wator.sd7          0.052473            651
seed7/prg/which.sd7          0.036337             65
seed7/prg/wiz.sd7            0.104269           2833
seed7/prg/wordcnt.sd7        0.036399             54
seed7/prg/wrinum.sd7         0.035339             43
seed7/prg/wumpus.sd7         0.042575            372
seed7/lib/aes.s7i            0.109494           1144
seed7/lib/aes_gcm.s7i        0.046527            392
seed7/lib/ar.s7i             0.073270           1532
seed7/lib/arc4.s7i           0.038166            144
seed7/lib/archive.s7i        0.037774            143
seed7/lib/archive_base.s7i   0.038116            135
seed7/lib/array.s7i          0.053386            610
seed7/lib/asn1.s7i           0.046800            544
seed7/lib/asn1oid.s7i        0.042060            157
seed7/lib/basearray.s7i      0.048698            450
seed7/lib/bigfile.s7i        0.038633            136
seed7/lib/bigint.s7i         0.055971            824
seed7/lib/bigrat.s7i         0.054123            784
seed7/lib/bin16.s7i          0.052170            592
seed7/lib/bin32.s7i          0.048652            490
seed7/lib/bin64.s7i          0.050486            539
seed7/lib/bitdata.s7i        0.076364           1330
seed7/lib/bitmapfont.s7i     0.041023            215
seed7/lib/bitset.s7i         0.049424            593
seed7/lib/bitsetof.s7i       0.047919            431
seed7/lib/blowfish.s7i       0.056990            383
seed7/lib/bmp.s7i            0.060244            924
seed7/lib/boolean.s7i        0.045697            403
seed7/lib/browser.s7i        0.043108            280
seed7/lib/bstring.s7i        0.041199            227
seed7/lib/bytedata.s7i       0.056549            482
seed7/lib/bzip2.s7i          0.060584            887
seed7/lib/cards.s7i          0.064838           1342
seed7/lib/category.s7i       0.040060            209
seed7/lib/cc_conf.s7i        0.078277           1314
seed7/lib/ccittfax.s7i       0.066704           1022
seed7/lib/cgi.s7i            0.038293            109
seed7/lib/cgidialog.s7i      0.059559           1118
seed7/lib/char.s7i           0.043720            356
seed7/lib/charsets.s7i       0.081608           2024
seed7/lib/chartype.s7i       0.040284            121
seed7/lib/cipher.s7i         0.038727            146
seed7/lib/cli_cmds.s7i       0.069219           1360
seed7/lib/clib_file.s7i      0.043813            301
seed7/lib/color.s7i          0.040774            185
seed7/lib/complex.s7i        0.045220            464
seed7/lib/compress.s7i       0.038646            150
seed7/lib/console.s7i        0.039630            188
seed7/lib/cpio.s7i           0.081765           1708
seed7/lib/crc32.s7i          0.045779            193
seed7/lib/cronos16.s7i       0.093175           1173
seed7/lib/cronos27.s7i       0.114209           1464
seed7/lib/csv.s7i            0.041726            201
seed7/lib/db_prop.s7i        0.063037            991
seed7/lib/deflate.s7i        0.056044            740
seed7/lib/des.s7i            0.055564            444
seed7/lib/dialog.s7i         0.043690            311
seed7/lib/dir.s7i            0.039385            163
seed7/lib/draw.s7i           0.057367            854
seed7/lib/duration.s7i       0.060964           1038
seed7/lib/echo.s7i           0.037689            132
seed7/lib/editline.s7i       0.044639            398
seed7/lib/elf.s7i            0.085438           1560
seed7/lib/elliptic.s7i       0.051987            649
seed7/lib/enable_io.s7i      0.043313            312
seed7/lib/encoding.s7i       0.059457            931
seed7/lib/enumeration.s7i    0.041822            236
seed7/lib/environment.s7i    0.040007            175
seed7/lib/exif.s7i           0.040886            152
seed7/lib/external_file.s7i  0.044214            340
seed7/lib/field.s7i          0.041440            268
seed7/lib/file.s7i           0.043078            372
seed7/lib/filebits.s7i       0.035275             46
seed7/lib/filesys.s7i        0.047118            601
seed7/lib/fileutil.s7i       0.039214            144
seed7/lib/fixarray.s7i       0.043883            307
seed7/lib/float.s7i          0.055109            757
seed7/lib/font.s7i           0.039499            196
seed7/lib/font8x8.s7i        0.048153            998
seed7/lib/forloop.s7i        0.045862            449
seed7/lib/ftp.s7i            0.058117            969
seed7/lib/ftpserv.s7i        0.051477            631
seed7/lib/getf.s7i           0.038182            115
seed7/lib/gethttp.s7i        0.036014             41
seed7/lib/gethttps.s7i       0.035870             41
seed7/lib/gif.s7i            0.049928            561
seed7/lib/graph.s7i          0.049139            415
seed7/lib/graph_file.s7i     0.044258            399
seed7/lib/gtkserver.s7i      0.038470            161
seed7/lib/gzip.s7i           0.048869            573
seed7/lib/hash.s7i           0.049862            421
seed7/lib/hashsetof.s7i      0.048633            499
seed7/lib/hmac.s7i           0.039043            152
seed7/lib/html.s7i           0.035660             83
seed7/lib/html_ent.s7i       0.046984            476
seed7/lib/htmldom.s7i        0.041681            286
seed7/lib/http_request.s7i   0.050496            696
seed7/lib/http_srv_resp.s7i  0.045311            380
seed7/lib/https_request.s7i  0.044570            211
seed7/lib/httpserv.s7i       0.044552            345
seed7/lib/huffman.s7i        0.053092            644
seed7/lib/ico.s7i            0.041329            221
seed7/lib/idxarray.s7i       0.042528            232
seed7/lib/image.s7i          0.037772            156
seed7/lib/imagefile.s7i      0.039131            171
seed7/lib/inflate.s7i        0.046395            411
seed7/lib/inifile.s7i        0.038969            129
seed7/lib/integer.s7i        0.052813            663
seed7/lib/iobuffer.s7i       0.043900            289
seed7/lib/jpeg.s7i           0.082995           1761
seed7/lib/json.s7i           0.058265            891
seed7/lib/json_serde.s7i     0.052029            783
seed7/lib/keybd.s7i          0.055291            639
seed7/lib/keydescr.s7i       0.056097            192
seed7/lib/leb128.s7i         0.040648            218
seed7/lib/line.s7i           0.038338            164
seed7/lib/listener.s7i       0.040361            247
seed7/lib/logfile.s7i        0.035610             73
seed7/lib/lower.s7i          0.037140            142
seed7/lib/lzma.s7i           0.058298            934
seed7/lib/lzw.s7i            0.059093            861
seed7/lib/magic.s7i          0.048402            403
seed7/lib/mahjng32.s7i       0.063829           1500
seed7/lib/make.s7i           0.049282            544
seed7/lib/makedata.s7i       0.070591           1428
seed7/lib/math.s7i           0.040492            201
seed7/lib/mixarith.s7i       0.040617            249
seed7/lib/modern27.s7i       0.085425           1099
seed7/lib/more.s7i           0.038313            130
seed7/lib/msgdigest.s7i      0.079045           1222
seed7/lib/multiscr.s7i       0.036785             68
seed7/lib/null_file.s7i      0.043915            345
seed7/lib/osfiles.s7i        0.065068           1085
seed7/lib/pbm.s7i            0.040481            230
seed7/lib/pcx.s7i            0.052245            638
seed7/lib/pem.s7i            0.039688            185
seed7/lib/pgm.s7i            0.040283            238
seed7/lib/pic16.s7i          0.047465           1037
seed7/lib/pic32.s7i          0.078138           2060
seed7/lib/pic_util.s7i       0.037638            144
seed7/lib/pixelimage.s7i     0.041235            320
seed7/lib/pixmap_file.s7i    0.046328            459
seed7/lib/pixmapfont.s7i     0.040310            184
seed7/lib/pkcs1.s7i          0.059609            543
seed7/lib/png.s7i            0.064221           1064
seed7/lib/poll.s7i           0.044113            313
seed7/lib/ppm.s7i            0.040455            240
seed7/lib/process.s7i        0.049164            541
seed7/lib/progs.s7i          0.056109            789
seed7/lib/propertyfile.s7i   0.038584            155
seed7/lib/rational.s7i       0.054055            792
seed7/lib/ref_list.s7i       0.041781            252
seed7/lib/reference.s7i      0.038120            126
seed7/lib/reverse.s7i        0.036521             94
seed7/lib/rpm.s7i            0.141779           3487
seed7/lib/rpmext.s7i         0.041589            318
seed7/lib/scanfile.s7i       0.080508           1779
seed7/lib/scanjson.s7i       0.046875            413
seed7/lib/scanstri.s7i       0.080889           1814
seed7/lib/scantoml.s7i       0.069726           1603
seed7/lib/seed7_05.s7i       0.066666           1072
seed7/lib/set.s7i            0.040107             57
seed7/lib/shell.s7i          0.053247            615
seed7/lib/showtls.s7i        0.055448            678
seed7/lib/signature.s7i      0.038409            131
seed7/lib/smtp.s7i           0.041372            261
seed7/lib/sockbase.s7i       0.045582            217
seed7/lib/socket.s7i         0.043808            326
seed7/lib/sokoban1.s7i       0.060174           1519
seed7/lib/sql_base.s7i       0.073708           1000
seed7/lib/stars.s7i          0.135234           1705
seed7/lib/stdfont10.s7i      0.081112           3347
seed7/lib/stdfont12.s7i      0.091408           3928
seed7/lib/stdfont14.s7i      0.103348           4510
seed7/lib/stdfont16.s7i      0.113775           5092
seed7/lib/stdfont18.s7i      0.132990           5868
seed7/lib/stdfont20.s7i      0.149299           6449
seed7/lib/stdfont24.s7i      0.176378           7421
seed7/lib/stdfont8.s7i       0.071841           2960
seed7/lib/stdfont9.s7i       0.075336           3152
seed7/lib/stdio.s7i          0.039194            192
seed7/lib/strifile.s7i       0.043203            345
seed7/lib/string.s7i         0.055536            779
seed7/lib/stritext.s7i       0.042782            352
seed7/lib/struct.s7i         0.044153            266
seed7/lib/struct_elem.s7i    0.037716            129
seed7/lib/subfile.s7i        0.037787            174
seed7/lib/subrange.s7i       0.035745             78
seed7/lib/syntax.s7i         0.043984            294
seed7/lib/tar.s7i            0.080044           1880
seed7/lib/tar_cmds.s7i       0.057586            752
seed7/lib/tdes.s7i           0.039015            143
seed7/lib/tee.s7i            0.037460            143
seed7/lib/text.s7i           0.037603            135
seed7/lib/tga.s7i            0.053236            676
seed7/lib/tiff.s7i           0.134740           2771
seed7/lib/time.s7i           0.072735           1191
seed7/lib/tls.s7i            0.113704           2230
seed7/lib/unicode.s7i        0.055039            575
seed7/lib/unionfnd.s7i       0.038785            130
seed7/lib/upper.s7i          0.038191            142
seed7/lib/utf16.s7i          0.049407            540
seed7/lib/utf8.s7i           0.040309            234
seed7/lib/vecfont10.s7i      0.076328           1056
seed7/lib/vecfont18.s7i      0.084755           1119
seed7/lib/vector3d.s7i       0.040299            293
seed7/lib/vectorfont.s7i     0.039101            239
seed7/lib/wildcard.s7i       0.036769            140
seed7/lib/window.s7i         0.045329            455
seed7/lib/wrinum.s7i         0.041868            248
seed7/lib/x509cert.s7i       0.071450           1243
seed7/lib/xml_ent.s7i        0.037724             94
seed7/lib/xmldom.s7i         0.041574            303
seed7/lib/xz.s7i             0.045782            442
seed7/lib/zip.s7i            0.119727           2792
seed7/lib/zstd.s7i           0.073320           1263
============================ ================= ================

Statistical Summary — Mode C (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.089429        |
+-----------+-----------------+
| Minimum   | 0.034406        |
+-----------+-----------------+
| Maximum   | 2.496171        |
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
seed7/prg/addup.sd7          0.044043            190
seed7/prg/bas7.sd7           0.761290          11459
seed7/prg/bifurk.sd7         0.037806             73
seed7/prg/bigfiles.sd7       0.040720            129
seed7/prg/brainf7.sd7        0.038809             86
seed7/prg/calc7.sd7          0.041359            128
seed7/prg/carddemo.sd7       0.046302            190
seed7/prg/castle.sd7         0.216848           3148
seed7/prg/cat.sd7            0.040526             82
seed7/prg/cellauto.sd7       0.039652             85
seed7/prg/celsius.sd7        0.036889             42
seed7/prg/chk_all.sd7        0.081366            843
seed7/prg/chkarr.sd7         0.851535           8367
seed7/prg/chkbig.sd7         4.084797          29026
seed7/prg/chkbin.sd7         1.018055           6469
seed7/prg/chkbitdata.sd7     1.233478           6624
seed7/prg/chkbool.sd7        0.228829           3157
seed7/prg/chkbst.sd7         0.099843            722
seed7/prg/chkchr.sd7         0.474373           2809
seed7/prg/chkcmd.sd7         0.115977           1205
seed7/prg/chkdb.sd7          0.744669           7454
seed7/prg/chkdecl.sd7        0.093474            448
seed7/prg/chkenum.sd7        0.123918           1230
seed7/prg/chkerr.sd7         0.338435           4663
seed7/prg/chkexc.sd7         0.147061           2627
seed7/prg/chkfil.sd7         0.127299           1615
seed7/prg/chkflt.sd7         2.801449          20620
seed7/prg/chkhent.sd7        0.038329             54
seed7/prg/chkhsh.sd7         0.505229           4548
seed7/prg/chkidx.sd7         3.228355          19567
seed7/prg/chkint.sd7         5.568652          38129
seed7/prg/chkjson.sd7        0.192956           1764
seed7/prg/chkovf.sd7         1.200365           8216
seed7/prg/chkprc.sd7         0.693852          10111
seed7/prg/chkscan.sd7        0.092691            714
seed7/prg/chkset.sd7         1.728276          11974
seed7/prg/chkstr.sd7         3.432734          26952
seed7/prg/chktime.sd7        0.260754           2025
seed7/prg/chktoml.sd7        0.214876           1656
seed7/prg/clock.sd7          0.044160             47
seed7/prg/clock2.sd7         0.044262             43
seed7/prg/clock3.sd7         0.050889             95
seed7/prg/cmpfil.sd7         0.042903             84
seed7/prg/comanche.sd7       0.048117            180
seed7/prg/confval.sd7        0.052390            175
seed7/prg/db7.sd7            0.064471            417
seed7/prg/diff7.sd7          0.055104            263
seed7/prg/dirtst.sd7         0.041120             42
seed7/prg/dirx.sd7           0.049810            152
seed7/prg/dnafight.sd7       0.126617           1381
seed7/prg/dragon.sd7         0.039337             73
seed7/prg/echo.sd7           0.039400             39
seed7/prg/eliza.sd7          0.052310            302
seed7/prg/err.sd7            0.044755             96
seed7/prg/fannkuch.sd7       0.043158            131
seed7/prg/fib.sd7            0.037299             47
seed7/prg/find7.sd7          0.042370            133
seed7/prg/findchar.sd7       0.044761            149
seed7/prg/fractree.sd7       0.039631             55
seed7/prg/ftp7.sd7           0.054607            296
seed7/prg/ftpserv.sd7        0.039435             74
seed7/prg/gcd.sd7            0.040399            109
seed7/prg/gkbd.sd7           0.061395            358
seed7/prg/gtksvtst.sd7       0.039491             94
seed7/prg/hal.sd7            0.048104            250
seed7/prg/hamu.sd7           0.065818            573
seed7/prg/hanoi.sd7          0.036324             55
seed7/prg/hd.sd7             0.038379             79
seed7/prg/hello.sd7          0.036382             32
seed7/prg/hilbert.sd7        0.040083            108
seed7/prg/ide7.sd7           0.046804            196
seed7/prg/kbd.sd7            0.036049             49
seed7/prg/klondike.sd7       0.086149            883
seed7/prg/lander.sd7         0.130705           1551
seed7/prg/lst80bas.sd7       0.056570            344
seed7/prg/lst99bas.sd7       0.059755            401
seed7/prg/lstgwbas.sd7       0.073587            577
seed7/prg/mahjong.sd7        0.146112           1943
seed7/prg/make7.sd7          0.042296            121
seed7/prg/mandelbr.sd7       0.047678            237
seed7/prg/mind.sd7           0.059419            443
seed7/prg/mirror.sd7         0.042765            131
seed7/prg/ms.sd7             0.067610            641
seed7/prg/nicoma.sd7         0.040922            135
seed7/prg/pac.sd7            0.072125            726
seed7/prg/pairs.sd7          0.138775           2025
seed7/prg/panic.sd7          0.191222           2634
seed7/prg/percolation.sd7    0.055884            330
seed7/prg/planets.sd7        0.135523           1486
seed7/prg/portfwd7.sd7       0.042853            139
seed7/prg/prime.sd7          0.038143             74
seed7/prg/printpi1.sd7       0.037563             56
seed7/prg/printpi2.sd7       0.037249             54
seed7/prg/printpi3.sd7       0.037699             60
seed7/prg/pv7.sd7            0.059087            337
seed7/prg/queen.sd7          0.042949            149
seed7/prg/rand.sd7           0.039565            121
seed7/prg/raytrace.sd7       0.067138            538
seed7/prg/rever.sd7          0.080913            816
seed7/prg/roman.sd7          0.035310             38
seed7/prg/s7c.sd7            0.625425           9060
seed7/prg/s7check.sd7        0.040886             68
seed7/prg/savehd7.sd7        0.110824           1110
seed7/prg/self.sd7           0.037695             49
seed7/prg/shisen.sd7         0.124520           1423
seed7/prg/sl.sd7             0.095575           1029
seed7/prg/snake.sd7          0.070423            615
seed7/prg/sokoban.sd7        0.088764            891
seed7/prg/spigotpi.sd7       0.037769             64
seed7/prg/sql7.sd7           0.051405            278
seed7/prg/startrek.sd7       0.094862            979
seed7/prg/sudoku7.sd7        0.199585           2657
seed7/prg/sydir7.sd7         0.064351            384
seed7/prg/syntaxhl.sd7       0.051085            177
seed7/prg/tak.sd7            0.039519             59
seed7/prg/tar7.sd7           0.043091            121
seed7/prg/tch.sd7            0.037877             55
seed7/prg/testfont.sd7       0.041535             95
seed7/prg/tet.sd7            0.060080            479
seed7/prg/tetg.sd7           0.062963            501
seed7/prg/toutf8.sd7         0.051079            240
seed7/prg/tst_cli.sd7        0.036252             40
seed7/prg/tst_srv.sd7        0.038750             47
seed7/prg/wator.sd7          0.081767            651
seed7/prg/which.sd7          0.036830             65
seed7/prg/wiz.sd7            0.205001           2833
seed7/prg/wordcnt.sd7        0.036438             54
seed7/prg/wrinum.sd7         0.039229             43
seed7/prg/wumpus.sd7         0.056501            372
seed7/lib/aes.s7i            0.196393           1144
seed7/lib/aes_gcm.s7i        0.059567            392
seed7/lib/ar.s7i             0.124379           1532
seed7/lib/arc4.s7i           0.045942            144
seed7/lib/archive.s7i        0.045434            143
seed7/lib/archive_base.s7i   0.044834            135
seed7/lib/array.s7i          0.076218            610
seed7/lib/asn1.s7i           0.066125            544
seed7/lib/asn1oid.s7i        0.049419            157
seed7/lib/basearray.s7i      0.063225            450
seed7/lib/bigfile.s7i        0.041269            136
seed7/lib/bigint.s7i         0.078021            824
seed7/lib/bigrat.s7i         0.077024            784
seed7/lib/bin16.s7i          0.066102            592
seed7/lib/bin32.s7i          0.062429            490
seed7/lib/bin64.s7i          0.065742            539
seed7/lib/bitdata.s7i        0.122960           1330
seed7/lib/bitmapfont.s7i     0.046797            215
seed7/lib/bitset.s7i         0.065207            593
seed7/lib/bitsetof.s7i       0.061556            431
seed7/lib/blowfish.s7i       0.075349            383
seed7/lib/bmp.s7i            0.099732            924
seed7/lib/boolean.s7i        0.053085            403
seed7/lib/browser.s7i        0.054674            280
seed7/lib/bstring.s7i        0.048790            227
seed7/lib/bytedata.s7i       0.065982            482
seed7/lib/bzip2.s7i          0.088645            887
seed7/lib/cards.s7i          0.103983           1342
seed7/lib/category.s7i       0.048680            209
seed7/lib/cc_conf.s7i        0.121028           1314
seed7/lib/ccittfax.s7i       0.100516           1022
seed7/lib/cgi.s7i            0.040969            109
seed7/lib/cgidialog.s7i      0.102216           1118
seed7/lib/char.s7i           0.052243            356
seed7/lib/charsets.s7i       0.125377           2024
seed7/lib/chartype.s7i       0.049462            121
seed7/lib/cipher.s7i         0.042020            146
seed7/lib/cli_cmds.s7i       0.119669           1360
seed7/lib/clib_file.s7i      0.052086            301
seed7/lib/color.s7i          0.048158            185
seed7/lib/complex.s7i        0.058405            464
seed7/lib/compress.s7i       0.044190            150
seed7/lib/console.s7i        0.046481            188
seed7/lib/cpio.s7i           0.147173           1708
seed7/lib/crc32.s7i          0.057249            193
seed7/lib/cronos16.s7i       0.196019           1173
seed7/lib/cronos27.s7i       0.257794           1464
seed7/lib/csv.s7i            0.047730            201
seed7/lib/db_prop.s7i        0.104501            991
seed7/lib/deflate.s7i        0.089899            740
seed7/lib/des.s7i            0.080493            444
seed7/lib/dialog.s7i         0.056441            311
seed7/lib/dir.s7i            0.042988            163
seed7/lib/draw.s7i           0.085588            854
seed7/lib/duration.s7i       0.110294           1038
seed7/lib/echo.s7i           0.041104            132
seed7/lib/editline.s7i       0.058000            398
seed7/lib/elf.s7i            0.151105           1560
seed7/lib/elliptic.s7i       0.074565            649
seed7/lib/enable_io.s7i      0.050040            312
seed7/lib/encoding.s7i       0.094896            931
seed7/lib/enumeration.s7i    0.049185            236
seed7/lib/environment.s7i    0.044357            175
seed7/lib/exif.s7i           0.045313            152
seed7/lib/external_file.s7i  0.052170            340
seed7/lib/field.s7i          0.052184            268
seed7/lib/file.s7i           0.053542            372
seed7/lib/filebits.s7i       0.038446             46
seed7/lib/filesys.s7i        0.064451            601
seed7/lib/fileutil.s7i       0.049495            144
seed7/lib/fixarray.s7i       0.057296            307
seed7/lib/float.s7i          0.074604            757
seed7/lib/font.s7i           0.045066            196
seed7/lib/font8x8.s7i        0.071063            998
seed7/lib/forloop.s7i        0.059519            449
seed7/lib/ftp.s7i            0.086566            969
seed7/lib/ftpserv.s7i        0.074657            631
seed7/lib/getf.s7i           0.039761            115
seed7/lib/gethttp.s7i        0.035736             41
seed7/lib/gethttps.s7i       0.035994             41
seed7/lib/gif.s7i            0.070212            561
seed7/lib/graph.s7i          0.062322            415
seed7/lib/graph_file.s7i     0.055790            399
seed7/lib/gtkserver.s7i      0.040408            161
seed7/lib/gzip.s7i           0.068386            573
seed7/lib/hash.s7i           0.064031            421
seed7/lib/hashsetof.s7i      0.065339            499
seed7/lib/hmac.s7i           0.044045            152
seed7/lib/html.s7i           0.038831             83
seed7/lib/html_ent.s7i       0.062614            476
seed7/lib/htmldom.s7i        0.051804            286
seed7/lib/http_request.s7i   0.077055            696
seed7/lib/http_srv_resp.s7i  0.058525            380
seed7/lib/https_request.s7i  0.047858            211
seed7/lib/httpserv.s7i       0.055744            345
seed7/lib/huffman.s7i        0.075352            644
seed7/lib/ico.s7i            0.049664            221
seed7/lib/idxarray.s7i       0.051990            232
seed7/lib/image.s7i          0.045121            156
seed7/lib/imagefile.s7i      0.046944            171
seed7/lib/inflate.s7i        0.064192            411
seed7/lib/inifile.s7i        0.044656            129
seed7/lib/integer.s7i        0.071362            663
seed7/lib/iobuffer.s7i       0.051453            289
seed7/lib/jpeg.s7i           0.159663           1761
seed7/lib/json.s7i           0.084975            891
seed7/lib/json_serde.s7i     0.078812            783
seed7/lib/keybd.s7i          0.078735            639
seed7/lib/keydescr.s7i       0.048812            192
seed7/lib/leb128.s7i         0.046395            218
seed7/lib/line.s7i           0.044031            164
seed7/lib/listener.s7i       0.049332            247
seed7/lib/logfile.s7i        0.039351             73
seed7/lib/lower.s7i          0.047881            142
seed7/lib/lzma.s7i           0.098320            934
seed7/lib/lzw.s7i            0.097149            861
seed7/lib/magic.s7i          0.066686            403
seed7/lib/mahjng32.s7i       0.100803           1500
seed7/lib/make.s7i           0.081508            544
seed7/lib/makedata.s7i       0.129069           1428
seed7/lib/math.s7i           0.050899            201
seed7/lib/mixarith.s7i       0.053948            249
seed7/lib/modern27.s7i       0.170187           1099
seed7/lib/more.s7i           0.043061            130
seed7/lib/msgdigest.s7i      0.140929           1222
seed7/lib/multiscr.s7i       0.039469             68
seed7/lib/null_file.s7i      0.051229            345
seed7/lib/osfiles.s7i        0.096713           1085
seed7/lib/pbm.s7i            0.053124            230
seed7/lib/pcx.s7i            0.077503            638
seed7/lib/pem.s7i            0.046304            185
seed7/lib/pgm.s7i            0.048565            238
seed7/lib/pic16.s7i          0.066738           1037
seed7/lib/pic32.s7i          0.121116           2060
seed7/lib/pic_util.s7i       0.043748            144
seed7/lib/pixelimage.s7i     0.052286            320
seed7/lib/pixmap_file.s7i    0.061152            459
seed7/lib/pixmapfont.s7i     0.047814            184
seed7/lib/pkcs1.s7i          0.077292            543
seed7/lib/png.s7i            0.106897           1064
seed7/lib/poll.s7i           0.050749            313
seed7/lib/ppm.s7i            0.046732            240
seed7/lib/process.s7i        0.064112            541
seed7/lib/progs.s7i          0.079950            789
seed7/lib/propertyfile.s7i   0.043563            155
seed7/lib/rational.s7i       0.076479            792
seed7/lib/ref_list.s7i       0.047926            252
seed7/lib/reference.s7i      0.040281            126
seed7/lib/reverse.s7i        0.039306             94
seed7/lib/rpm.s7i            0.283729           3487
seed7/lib/rpmext.s7i         0.051510            318
seed7/lib/scanfile.s7i       0.132139           1779
seed7/lib/scanjson.s7i       0.059605            413
seed7/lib/scanstri.s7i       0.135661           1814
seed7/lib/scantoml.s7i       0.138296           1603
seed7/lib/seed7_05.s7i       0.124148           1072
seed7/lib/set.s7i            0.044556             57
seed7/lib/shell.s7i          0.079783            615
seed7/lib/showtls.s7i        0.089101            678
seed7/lib/signature.s7i      0.045465            131
seed7/lib/smtp.s7i           0.053324            261
seed7/lib/sockbase.s7i       0.050600            217
seed7/lib/socket.s7i         0.052448            326
seed7/lib/sokoban1.s7i       0.080155           1519
seed7/lib/sql_base.s7i       0.093349           1000
seed7/lib/stars.s7i          0.232391           1705
seed7/lib/stdfont10.s7i      0.144228           3347
seed7/lib/stdfont12.s7i      0.167185           3928
seed7/lib/stdfont14.s7i      0.188558           4510
seed7/lib/stdfont16.s7i      0.222554           5092
seed7/lib/stdfont18.s7i      0.251189           5868
seed7/lib/stdfont20.s7i      0.281307           6449
seed7/lib/stdfont24.s7i      0.339444           7421
seed7/lib/stdfont8.s7i       0.130051           2960
seed7/lib/stdfont9.s7i       0.136156           3152
seed7/lib/stdio.s7i          0.046398            192
seed7/lib/strifile.s7i       0.055347            345
seed7/lib/string.s7i         0.079022            779
seed7/lib/stritext.s7i       0.060105            352
seed7/lib/struct.s7i         0.058091            266
seed7/lib/struct_elem.s7i    0.042407            129
seed7/lib/subfile.s7i        0.045478            174
seed7/lib/subrange.s7i       0.040622             78
seed7/lib/syntax.s7i         0.061688            294
seed7/lib/tar.s7i            0.147378           1880
seed7/lib/tar_cmds.s7i       0.085014            752
seed7/lib/tdes.s7i           0.044914            143
seed7/lib/tee.s7i            0.041910            143
seed7/lib/text.s7i           0.041526            135
seed7/lib/tga.s7i            0.081030            676
seed7/lib/tiff.s7i           0.243364           2771
seed7/lib/time.s7i           0.100108           1191
seed7/lib/tls.s7i            0.195651           2230
seed7/lib/unicode.s7i        0.071961            575
seed7/lib/unionfnd.s7i       0.042839            130
seed7/lib/upper.s7i          0.041995            142
seed7/lib/utf16.s7i          0.065447            540
seed7/lib/utf8.s7i           0.047258            234
seed7/lib/vecfont10.s7i      0.159098           1056
seed7/lib/vecfont18.s7i      0.182680           1119
seed7/lib/vector3d.s7i       0.051794            293
seed7/lib/vectorfont.s7i     0.047042            239
seed7/lib/wildcard.s7i       0.042381            140
seed7/lib/window.s7i         0.079210            455
seed7/lib/wrinum.s7i         0.050407            248
seed7/lib/x509cert.s7i       0.118430           1243
seed7/lib/xml_ent.s7i        0.041746             94
seed7/lib/xmldom.s7i         0.050891            303
seed7/lib/xz.s7i             0.060305            442
seed7/lib/zip.s7i            0.229137           2792
seed7/lib/zstd.s7i           0.115733           1263
============================ ================= ================

Statistical Summary — Mode D (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.159359        |
+-----------+-----------------+
| Minimum   | 0.035310        |
+-----------+-----------------+
| Maximum   | 5.568652        |
+-----------+-----------------+

Cross-Mode Comparison
=====================

+------+-----------------+-----------------+-----------------+
| Mode | Average (s)     | Minimum (s)     | Maximum (s)     |
+======+=================+=================+=================+
| A    | 0.034296        | 0.032424        | 0.046170        |
+------+-----------------+-----------------+-----------------+
| B    | 0.038740        | 0.034987        | 0.048164        |
+------+-----------------+-----------------+-----------------+
| C    | 0.089429        | 0.034406        | 2.496171        |
+------+-----------------+-----------------+-----------------+
| D    | 0.159359        | 0.035310        | 5.568652        |
+------+-----------------+-----------------+-----------------+

Phase Timing Summary
====================

Wall-clock times include warm-up work and GC performed by the warm-up helper.

+----------+--------------+--------------+--------------+
| Phase    | Warm-up      | Timed pass   | Mode total   |
+==========+==============+==============+==============+
| Mode A   | 00:00:12.843 | 00:00:58.432 | 00:01:11.276 |
+----------+--------------+--------------+--------------+
| Mode B   | 00:00:14.727 | 00:01:06.059 | 00:01:20.787 |
+----------+--------------+--------------+--------------+
| Mode C   | 00:00:35.941 | 00:02:33.310 | 00:03:09.251 |
+----------+--------------+--------------+--------------+
| Mode D   | 00:01:00.844 | 00:04:32.461 | 00:05:33.306 |
+----------+--------------+--------------+--------------+
| Total    |              |              | 00:11:14.628 |
+----------+--------------+--------------+--------------+


.. ---------------------------------------------------------------------------
