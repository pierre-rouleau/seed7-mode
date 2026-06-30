=======================================================
GC-Controlled Benchmark Report: Seed7 Mode — Four Modes
=======================================================

:Running with: seed7-mode 2026-06-30T14:06:30+0000 W27-2
:Seed7 dir   : /Users/roup/my/dvo/seed7-repos/seed7/
:Started at: 10:41:43 local time
:Generated on: 2026-06-30 14:53:26 UTC
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
seed7/prg/addup.sd7          0.040885            190
seed7/prg/bas7.sd7           0.041112          11459
seed7/prg/bifurk.sd7         0.041173             73
seed7/prg/bigfiles.sd7       0.040544            129
seed7/prg/brainf7.sd7        0.039259             86
seed7/prg/calc7.sd7          0.040114            128
seed7/prg/carddemo.sd7       0.042236            190
seed7/prg/castle.sd7         0.044821           3148
seed7/prg/cat.sd7            0.044119             82
seed7/prg/cellauto.sd7       0.038589             85
seed7/prg/celsius.sd7        0.038676             42
seed7/prg/chk_all.sd7        0.042777            843
seed7/prg/chkarr.sd7         0.040937           8367
seed7/prg/chkbig.sd7         0.046908          29026
seed7/prg/chkbin.sd7         0.042976           6469
seed7/prg/chkbitdata.sd7     0.043417           6624
seed7/prg/chkbool.sd7        0.046039           3157
seed7/prg/chkbst.sd7         0.041990            722
seed7/prg/chkchr.sd7         0.044622           2809
seed7/prg/chkcmd.sd7         0.042982           1205
seed7/prg/chkdb.sd7          0.044422           7454
seed7/prg/chkdecl.sd7        0.043182            448
seed7/prg/chkenum.sd7        0.044246           1230
seed7/prg/chkerr.sd7         0.044301           4663
seed7/prg/chkexc.sd7         0.039144           2627
seed7/prg/chkfil.sd7         0.039453           1615
seed7/prg/chkflt.sd7         0.044043          20620
seed7/prg/chkhent.sd7        0.042377             54
seed7/prg/chkhsh.sd7         0.042580           4548
seed7/prg/chkidx.sd7         0.045347          19567
seed7/prg/chkint.sd7         0.050347          38129
seed7/prg/chkjson.sd7        0.043324           1764
seed7/prg/chkovf.sd7         0.043080           8216
seed7/prg/chkprc.sd7         0.042131          10111
seed7/prg/chkscan.sd7        0.039645            714
seed7/prg/chkset.sd7         0.038332          11974
seed7/prg/chkstr.sd7         0.042002          26952
seed7/prg/chktime.sd7        0.034852           2025
seed7/prg/chktoml.sd7        0.035774           1656
seed7/prg/clock.sd7          0.034729             47
seed7/prg/clock2.sd7         0.035971             43
seed7/prg/clock3.sd7         0.034873             95
seed7/prg/cmpfil.sd7         0.035627             84
seed7/prg/comanche.sd7       0.035481            180
seed7/prg/confval.sd7        0.035491            175
seed7/prg/db7.sd7            0.034945            417
seed7/prg/diff7.sd7          0.034710            263
seed7/prg/dirtst.sd7         0.034844             42
seed7/prg/dirx.sd7           0.035056            152
seed7/prg/dnafight.sd7       0.036501           1381
seed7/prg/dragon.sd7         0.036502             73
seed7/prg/echo.sd7           0.036550             39
seed7/prg/eliza.sd7          0.035170            302
seed7/prg/err.sd7            0.037819             96
seed7/prg/fannkuch.sd7       0.037908            131
seed7/prg/fib.sd7            0.038590             47
seed7/prg/find7.sd7          0.039796            133
seed7/prg/findchar.sd7       0.040654            149
seed7/prg/fractree.sd7       0.040943             55
seed7/prg/ftp7.sd7           0.039715            296
seed7/prg/ftpserv.sd7        0.038130             74
seed7/prg/gcd.sd7            0.035630            109
seed7/prg/gkbd.sd7           0.036425            358
seed7/prg/gtksvtst.sd7       0.034049             94
seed7/prg/hal.sd7            0.035123            250
seed7/prg/hamu.sd7           0.035498            573
seed7/prg/hanoi.sd7          0.035460             55
seed7/prg/hd.sd7             0.034556             79
seed7/prg/hello.sd7          0.034644             32
seed7/prg/hilbert.sd7        0.034740            108
seed7/prg/ide7.sd7           0.040102            196
seed7/prg/kbd.sd7            0.036917             49
seed7/prg/klondike.sd7       0.036452            883
seed7/prg/lander.sd7         0.039779           1551
seed7/prg/lst80bas.sd7       0.044000            344
seed7/prg/lst99bas.sd7       0.043670            401
seed7/prg/lstgwbas.sd7       0.041452            577
seed7/prg/mahjong.sd7        0.036288           1943
seed7/prg/make7.sd7          0.034621            121
seed7/prg/mandelbr.sd7       0.038833            237
seed7/prg/mind.sd7           0.037109            443
seed7/prg/mirror.sd7         0.039538            131
seed7/prg/ms.sd7             0.038694            641
seed7/prg/nicoma.sd7         0.041883            135
seed7/prg/pac.sd7            0.042526            726
seed7/prg/pairs.sd7          0.040225           2025
seed7/prg/panic.sd7          0.034778           2634
seed7/prg/percolation.sd7    0.034197            330
seed7/prg/planets.sd7        0.035200           1486
seed7/prg/portfwd7.sd7       0.033993            139
seed7/prg/prime.sd7          0.033599             74
seed7/prg/printpi1.sd7       0.036833             56
seed7/prg/printpi2.sd7       0.034974             54
seed7/prg/printpi3.sd7       0.034748             60
seed7/prg/pv7.sd7            0.035386            337
seed7/prg/queen.sd7          0.034935            149
seed7/prg/rand.sd7           0.036258            121
seed7/prg/raytrace.sd7       0.040232            538
seed7/prg/rever.sd7          0.035946            816
seed7/prg/roman.sd7          0.035778             38
seed7/prg/s7c.sd7            0.035635           9060
seed7/prg/s7check.sd7        0.035286             68
seed7/prg/savehd7.sd7        0.034668           1110
seed7/prg/self.sd7           0.034917             49
seed7/prg/shisen.sd7         0.036364           1423
seed7/prg/sl.sd7             0.038104           1029
seed7/prg/snake.sd7          0.039723            615
seed7/prg/sokoban.sd7        0.039838            891
seed7/prg/spigotpi.sd7       0.040415             64
seed7/prg/sql7.sd7           0.038350            278
seed7/prg/startrek.sd7       0.036405            979
seed7/prg/sudoku7.sd7        0.036095           2657
seed7/prg/sydir7.sd7         0.035561            384
seed7/prg/syntaxhl.sd7       0.034148            177
seed7/prg/tak.sd7            0.033884             59
seed7/prg/tar7.sd7           0.034154            121
seed7/prg/tch.sd7            0.035476             55
seed7/prg/testfont.sd7       0.034084             95
seed7/prg/tet.sd7            0.034165            479
seed7/prg/tetg.sd7           0.034195            501
seed7/prg/toutf8.sd7         0.034526            240
seed7/prg/tst_cli.sd7        0.034731             40
seed7/prg/tst_srv.sd7        0.034962             47
seed7/prg/wator.sd7          0.034817            651
seed7/prg/which.sd7          0.035021             65
seed7/prg/wiz.sd7            0.035217           2833
seed7/prg/wordcnt.sd7        0.034590             54
seed7/prg/wrinum.sd7         0.034985             43
seed7/prg/wumpus.sd7         0.035380            372
seed7/lib/aes.s7i            0.037838           1144
seed7/lib/aes_gcm.s7i        0.039555            392
seed7/lib/ar.s7i             0.038245           1532
seed7/lib/arc4.s7i           0.037143            144
seed7/lib/archive.s7i        0.037259            143
seed7/lib/archive_base.s7i   0.037361            135
seed7/lib/array.s7i          0.034845            610
seed7/lib/asn1.s7i           0.037987            544
seed7/lib/asn1oid.s7i        0.038262            157
seed7/lib/basearray.s7i      0.037093            450
seed7/lib/bigfile.s7i        0.036596            136
seed7/lib/bigint.s7i         0.035004            824
seed7/lib/bigrat.s7i         0.035065            784
seed7/lib/bin16.s7i          0.040240            592
seed7/lib/bin32.s7i          0.042444            490
seed7/lib/bin64.s7i          0.042314            539
seed7/lib/bitdata.s7i        0.042524           1330
seed7/lib/bitmapfont.s7i     0.041290            215
seed7/lib/bitset.s7i         0.039766            593
seed7/lib/bitsetof.s7i       0.038621            431
seed7/lib/blowfish.s7i       0.040920            383
seed7/lib/bmp.s7i            0.045399            924
seed7/lib/boolean.s7i        0.038478            403
seed7/lib/browser.s7i        0.043082            280
seed7/lib/bstring.s7i        0.043061            227
seed7/lib/bytedata.s7i       0.041335            482
seed7/lib/bzip2.s7i          0.042467            887
seed7/lib/cards.s7i          0.040986           1342
seed7/lib/category.s7i       0.040515            209
seed7/lib/cc_conf.s7i        0.037735           1314
seed7/lib/ccittfax.s7i       0.038982           1022
seed7/lib/cgi.s7i            0.036972            109
seed7/lib/cgidialog.s7i      0.035890           1118
seed7/lib/char.s7i           0.035449            356
seed7/lib/charsets.s7i       0.040190           2024
seed7/lib/chartype.s7i       0.041136            121
seed7/lib/cipher.s7i         0.041763            146
seed7/lib/cli_cmds.s7i       0.041604           1360
seed7/lib/clib_file.s7i      0.036973            301
seed7/lib/color.s7i          0.040244            185
seed7/lib/complex.s7i        0.040763            464
seed7/lib/compress.s7i       0.041384            150
seed7/lib/console.s7i        0.040586            188
seed7/lib/cpio.s7i           0.041378           1708
seed7/lib/crc32.s7i          0.042797            193
seed7/lib/cronos16.s7i       0.042113           1173
seed7/lib/cronos27.s7i       0.040396           1464
seed7/lib/csv.s7i            0.042561            201
seed7/lib/db_prop.s7i        0.042428            991
seed7/lib/deflate.s7i        0.041474            740
seed7/lib/des.s7i            0.040920            444
seed7/lib/dialog.s7i         0.043016            311
seed7/lib/dir.s7i            0.041725            163
seed7/lib/draw.s7i           0.039122            854
seed7/lib/duration.s7i       0.039488           1038
seed7/lib/echo.s7i           0.038622            132
seed7/lib/editline.s7i       0.036259            398
seed7/lib/elf.s7i            0.037583           1560
seed7/lib/elliptic.s7i       0.039658            649
seed7/lib/enable_io.s7i      0.040740            312
seed7/lib/encoding.s7i       0.041878            931
seed7/lib/enumeration.s7i    0.038387            236
seed7/lib/environment.s7i    0.039587            175
seed7/lib/exif.s7i           0.040499            152
seed7/lib/external_file.s7i  0.042573            340
seed7/lib/field.s7i          0.037160            268
seed7/lib/file.s7i           0.040004            372
seed7/lib/filebits.s7i       0.039781             46
seed7/lib/filesys.s7i        0.034329            601
seed7/lib/fileutil.s7i       0.034643            144
seed7/lib/fixarray.s7i       0.034076            307
seed7/lib/float.s7i          0.034863            757
seed7/lib/font.s7i           0.035885            196
seed7/lib/font8x8.s7i        0.034931            998
seed7/lib/forloop.s7i        0.034953            449
seed7/lib/ftp.s7i            0.035841            969
seed7/lib/ftpserv.s7i        0.035855            631
seed7/lib/getf.s7i           0.036242            115
seed7/lib/gethttp.s7i        0.034631             41
seed7/lib/gethttps.s7i       0.035126             41
seed7/lib/gif.s7i            0.035248            561
seed7/lib/graph.s7i          0.034976            415
seed7/lib/graph_file.s7i     0.035089            399
seed7/lib/gtkserver.s7i      0.035839            161
seed7/lib/gzip.s7i           0.036817            573
seed7/lib/hash.s7i           0.038482            421
seed7/lib/hashsetof.s7i      0.041446            499
seed7/lib/hmac.s7i           0.039350            152
seed7/lib/html.s7i           0.036134             83
seed7/lib/html_ent.s7i       0.035056            476
seed7/lib/htmldom.s7i        0.034965            286
seed7/lib/http_request.s7i   0.035187            696
seed7/lib/http_srv_resp.s7i  0.034760            380
seed7/lib/https_request.s7i  0.034949            211
seed7/lib/httpserv.s7i       0.035384            345
seed7/lib/huffman.s7i        0.035125            644
seed7/lib/ico.s7i            0.033913            221
seed7/lib/idxarray.s7i       0.033832            232
seed7/lib/image.s7i          0.034089            156
seed7/lib/imagefile.s7i      0.034544            171
seed7/lib/inflate.s7i        0.036818            411
seed7/lib/inifile.s7i        0.042578            129
seed7/lib/integer.s7i        0.042551            663
seed7/lib/iobuffer.s7i       0.040179            289
seed7/lib/jpeg.s7i           0.037017           1761
seed7/lib/json.s7i           0.036828            891
seed7/lib/json_serde.s7i     0.040908            783
seed7/lib/keybd.s7i          0.038604            639
seed7/lib/keydescr.s7i       0.036004            192
seed7/lib/leb128.s7i         0.035018            218
seed7/lib/line.s7i           0.035895            164
seed7/lib/listener.s7i       0.036756            247
seed7/lib/logfile.s7i        0.035734             73
seed7/lib/lower.s7i          0.035399            142
seed7/lib/lzma.s7i           0.034932            934
seed7/lib/lzw.s7i            0.035230            861
seed7/lib/magic.s7i          0.035193            403
seed7/lib/mahjng32.s7i       0.035278           1500
seed7/lib/make.s7i           0.034738            544
seed7/lib/makedata.s7i       0.035065           1428
seed7/lib/math.s7i           0.035957            201
seed7/lib/mixarith.s7i       0.042377            249
seed7/lib/modern27.s7i       0.044305           1099
seed7/lib/more.s7i           0.036656            130
seed7/lib/msgdigest.s7i      0.035348           1222
seed7/lib/multiscr.s7i       0.034934             68
seed7/lib/null_file.s7i      0.034362            345
seed7/lib/osfiles.s7i        0.033940           1085
seed7/lib/pbm.s7i            0.033834            230
seed7/lib/pcx.s7i            0.034174            638
seed7/lib/pem.s7i            0.034982            185
seed7/lib/pgm.s7i            0.035416            238
seed7/lib/pic16.s7i          0.034751           1037
seed7/lib/pic32.s7i          0.034719           2060
seed7/lib/pic_util.s7i       0.035497            144
seed7/lib/pixelimage.s7i     0.034815            320
seed7/lib/pixmap_file.s7i    0.034892            459
seed7/lib/pixmapfont.s7i     0.035239            184
seed7/lib/pkcs1.s7i          0.034882            543
seed7/lib/png.s7i            0.034832           1064
seed7/lib/poll.s7i           0.035296            313
seed7/lib/ppm.s7i            0.035092            240
seed7/lib/process.s7i        0.034957            541
seed7/lib/progs.s7i          0.034933            789
seed7/lib/propertyfile.s7i   0.034901            155
seed7/lib/rational.s7i       0.035027            792
seed7/lib/ref_list.s7i       0.035452            252
seed7/lib/reference.s7i      0.035136            126
seed7/lib/reverse.s7i        0.035093             94
seed7/lib/rpm.s7i            0.035323           3487
seed7/lib/rpmext.s7i         0.034686            318
seed7/lib/scanfile.s7i       0.036719           1779
seed7/lib/scanjson.s7i       0.038030            413
seed7/lib/scanstri.s7i       0.034863           1814
seed7/lib/scantoml.s7i       0.034709           1603
seed7/lib/seed7_05.s7i       0.034006           1072
seed7/lib/set.s7i            0.033748             57
seed7/lib/shell.s7i          0.034099            615
seed7/lib/showtls.s7i        0.042979            678
seed7/lib/signature.s7i      0.043985            131
seed7/lib/smtp.s7i           0.042912            261
seed7/lib/sockbase.s7i       0.040486            217
seed7/lib/socket.s7i         0.036847            326
seed7/lib/sokoban1.s7i       0.035330           1519
seed7/lib/sql_base.s7i       0.036058           1000
seed7/lib/stars.s7i          0.036018           1705
seed7/lib/stdfont10.s7i      0.036788           3347
seed7/lib/stdfont12.s7i      0.041240           3928
seed7/lib/stdfont14.s7i      0.036903           4510
seed7/lib/stdfont16.s7i      0.038278           5092
seed7/lib/stdfont18.s7i      0.038802           5868
seed7/lib/stdfont20.s7i      0.035754           6449
seed7/lib/stdfont24.s7i      0.038139           7421
seed7/lib/stdfont8.s7i       0.039619           2960
seed7/lib/stdfont9.s7i       0.042495           3152
seed7/lib/stdio.s7i          0.039060            192
seed7/lib/strifile.s7i       0.038841            345
seed7/lib/string.s7i         0.034873            779
seed7/lib/stritext.s7i       0.036942            352
seed7/lib/struct.s7i         0.036240            266
seed7/lib/struct_elem.s7i    0.034651            129
seed7/lib/subfile.s7i        0.034897            174
seed7/lib/subrange.s7i       0.035734             78
seed7/lib/syntax.s7i         0.035848            294
seed7/lib/tar.s7i            0.037893           1880
seed7/lib/tar_cmds.s7i       0.037747            752
seed7/lib/tdes.s7i           0.034160            143
seed7/lib/tee.s7i            0.034127            143
seed7/lib/text.s7i           0.035948            135
seed7/lib/tga.s7i            0.034526            676
seed7/lib/tiff.s7i           0.035589           2771
seed7/lib/time.s7i           0.035031           1191
seed7/lib/tls.s7i            0.035415           2230
seed7/lib/unicode.s7i        0.036499            575
seed7/lib/unionfnd.s7i       0.040679            130
seed7/lib/upper.s7i          0.042418            142
seed7/lib/utf16.s7i          0.041029            540
seed7/lib/utf8.s7i           0.042036            234
seed7/lib/vecfont10.s7i      0.041889           1056
seed7/lib/vecfont18.s7i      0.036614           1119
seed7/lib/vector3d.s7i       0.040013            293
seed7/lib/vectorfont.s7i     0.038221            239
seed7/lib/wildcard.s7i       0.036918            140
seed7/lib/window.s7i         0.039944            455
seed7/lib/wrinum.s7i         0.039310            248
seed7/lib/x509cert.s7i       0.039951           1243
seed7/lib/xml_ent.s7i        0.035735             94
seed7/lib/xmldom.s7i         0.036904            303
seed7/lib/xz.s7i             0.036482            442
seed7/lib/zip.s7i            0.035828           2792
seed7/lib/zstd.s7i           0.034963           1263
============================ ================= ================

Statistical Summary — Mode A (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.037853        |
+-----------+-----------------+
| Minimum   | 0.033599        |
+-----------+-----------------+
| Maximum   | 0.050347        |
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
seed7/prg/addup.sd7          0.039712            190
seed7/prg/bas7.sd7           0.042305          11459
seed7/prg/bifurk.sd7         0.038437             73
seed7/prg/bigfiles.sd7       0.044993            129
seed7/prg/brainf7.sd7        0.044726             86
seed7/prg/calc7.sd7          0.044658            128
seed7/prg/carddemo.sd7       0.044959            190
seed7/prg/castle.sd7         0.046801           3148
seed7/prg/cat.sd7            0.040995             82
seed7/prg/cellauto.sd7       0.039842             85
seed7/prg/celsius.sd7        0.045414             42
seed7/prg/chk_all.sd7        0.050084            843
seed7/prg/chkarr.sd7         0.050251           8367
seed7/prg/chkbig.sd7         0.051906          29026
seed7/prg/chkbin.sd7         0.046452           6469
seed7/prg/chkbitdata.sd7     0.042709           6624
seed7/prg/chkbool.sd7        0.041844           3157
seed7/prg/chkbst.sd7         0.040942            722
seed7/prg/chkchr.sd7         0.044902           2809
seed7/prg/chkcmd.sd7         0.044782           1205
seed7/prg/chkdb.sd7          0.047413           7454
seed7/prg/chkdecl.sd7        0.046754            448
seed7/prg/chkenum.sd7        0.045107           1230
seed7/prg/chkerr.sd7         0.047061           4663
seed7/prg/chkexc.sd7         0.039987           2627
seed7/prg/chkfil.sd7         0.042134           1615
seed7/prg/chkflt.sd7         0.046303          20620
seed7/prg/chkhent.sd7        0.044665             54
seed7/prg/chkhsh.sd7         0.049183           4548
seed7/prg/chkidx.sd7         0.046347          19567
seed7/prg/chkint.sd7         0.048823          38129
seed7/prg/chkjson.sd7        0.043314           1764
seed7/prg/chkovf.sd7         0.042667           8216
seed7/prg/chkprc.sd7         0.040957          10111
seed7/prg/chkscan.sd7        0.042140            714
seed7/prg/chkset.sd7         0.045057          11974
seed7/prg/chkstr.sd7         0.048446          26952
seed7/prg/chktime.sd7        0.041301           2025
seed7/prg/chktoml.sd7        0.040929           1656
seed7/prg/clock.sd7          0.037331             47
seed7/prg/clock2.sd7         0.037455             43
seed7/prg/clock3.sd7         0.040811             95
seed7/prg/cmpfil.sd7         0.039783             84
seed7/prg/comanche.sd7       0.042185            180
seed7/prg/confval.sd7        0.045447            175
seed7/prg/db7.sd7            0.044154            417
seed7/prg/diff7.sd7          0.042533            263
seed7/prg/dirtst.sd7         0.037210             42
seed7/prg/dirx.sd7           0.039329            152
seed7/prg/dnafight.sd7       0.040235           1381
seed7/prg/dragon.sd7         0.038784             73
seed7/prg/echo.sd7           0.037542             39
seed7/prg/eliza.sd7          0.040163            302
seed7/prg/err.sd7            0.047805             96
seed7/prg/fannkuch.sd7       0.047998            131
seed7/prg/fib.sd7            0.045710             47
seed7/prg/find7.sd7          0.046622            133
seed7/prg/findchar.sd7       0.040486            149
seed7/prg/fractree.sd7       0.042581             55
seed7/prg/ftp7.sd7           0.041309            296
seed7/prg/ftpserv.sd7        0.040954             74
seed7/prg/gcd.sd7            0.045707            109
seed7/prg/gkbd.sd7           0.046639            358
seed7/prg/gtksvtst.sd7       0.044364             94
seed7/prg/hal.sd7            0.045894            250
seed7/prg/hamu.sd7           0.045540            573
seed7/prg/hanoi.sd7          0.042792             55
seed7/prg/hd.sd7             0.039855             79
seed7/prg/hello.sd7          0.037722             32
seed7/prg/hilbert.sd7        0.041673            108
seed7/prg/ide7.sd7           0.041663            196
seed7/prg/kbd.sd7            0.036902             49
seed7/prg/klondike.sd7       0.039486            883
seed7/prg/lander.sd7         0.040538           1551
seed7/prg/lst80bas.sd7       0.038573            344
seed7/prg/lst99bas.sd7       0.043303            401
seed7/prg/lstgwbas.sd7       0.046190            577
seed7/prg/mahjong.sd7        0.046433           1943
seed7/prg/make7.sd7          0.045340            121
seed7/prg/mandelbr.sd7       0.041843            237
seed7/prg/mind.sd7           0.040935            443
seed7/prg/mirror.sd7         0.044127            131
seed7/prg/ms.sd7             0.041814            641
seed7/prg/nicoma.sd7         0.040616            135
seed7/prg/pac.sd7            0.040560            726
seed7/prg/pairs.sd7          0.039970           2025
seed7/prg/panic.sd7          0.042444           2634
seed7/prg/percolation.sd7    0.042039            330
seed7/prg/planets.sd7        0.041727           1486
seed7/prg/portfwd7.sd7       0.040107            139
seed7/prg/prime.sd7          0.038148             74
seed7/prg/printpi1.sd7       0.038142             56
seed7/prg/printpi2.sd7       0.037590             54
seed7/prg/printpi3.sd7       0.038399             60
seed7/prg/pv7.sd7            0.040643            337
seed7/prg/queen.sd7          0.040344            149
seed7/prg/rand.sd7           0.039632            121
seed7/prg/raytrace.sd7       0.039757            538
seed7/prg/rever.sd7          0.041625            816
seed7/prg/roman.sd7          0.037784             38
seed7/prg/s7c.sd7            0.039966           9060
seed7/prg/s7check.sd7        0.039376             68
seed7/prg/savehd7.sd7        0.040836           1110
seed7/prg/self.sd7           0.038511             49
seed7/prg/shisen.sd7         0.045213           1423
seed7/prg/sl.sd7             0.045680           1029
seed7/prg/snake.sd7          0.044158            615
seed7/prg/sokoban.sd7        0.043595            891
seed7/prg/spigotpi.sd7       0.038676             64
seed7/prg/sql7.sd7           0.042211            278
seed7/prg/startrek.sd7       0.041131            979
seed7/prg/sudoku7.sd7        0.041034           2657
seed7/prg/sydir7.sd7         0.040086            384
seed7/prg/syntaxhl.sd7       0.043630            177
seed7/prg/tak.sd7            0.043316             59
seed7/prg/tar7.sd7           0.044874            121
seed7/prg/tch.sd7            0.042374             55
seed7/prg/testfont.sd7       0.044530             95
seed7/prg/tet.sd7            0.045788            479
seed7/prg/tetg.sd7           0.046215            501
seed7/prg/toutf8.sd7         0.045036            240
seed7/prg/tst_cli.sd7        0.038684             40
seed7/prg/tst_srv.sd7        0.040127             47
seed7/prg/wator.sd7          0.047772            651
seed7/prg/which.sd7          0.040977             65
seed7/prg/wiz.sd7            0.048826           2833
seed7/prg/wordcnt.sd7        0.043389             54
seed7/prg/wrinum.sd7         0.041872             43
seed7/prg/wumpus.sd7         0.044580            372
seed7/lib/aes.s7i            0.048151           1144
seed7/lib/aes_gcm.s7i        0.041393            392
seed7/lib/ar.s7i             0.040500           1532
seed7/lib/arc4.s7i           0.041121            144
seed7/lib/archive.s7i        0.040369            143
seed7/lib/archive_base.s7i   0.041268            135
seed7/lib/array.s7i          0.040240            610
seed7/lib/asn1.s7i           0.039474            544
seed7/lib/asn1oid.s7i        0.044635            157
seed7/lib/basearray.s7i      0.041668            450
seed7/lib/bigfile.s7i        0.041137            136
seed7/lib/bigint.s7i         0.040295            824
seed7/lib/bigrat.s7i         0.040500            784
seed7/lib/bin16.s7i          0.040993            592
seed7/lib/bin32.s7i          0.041107            490
seed7/lib/bin64.s7i          0.039971            539
seed7/lib/bitdata.s7i        0.045677           1330
seed7/lib/bitmapfont.s7i     0.039828            215
seed7/lib/bitset.s7i         0.044585            593
seed7/lib/bitsetof.s7i       0.044903            431
seed7/lib/blowfish.s7i       0.046663            383
seed7/lib/bmp.s7i            0.043628            924
seed7/lib/boolean.s7i        0.040717            403
seed7/lib/browser.s7i        0.041761            280
seed7/lib/bstring.s7i        0.041652            227
seed7/lib/bytedata.s7i       0.041032            482
seed7/lib/bzip2.s7i          0.040684            887
seed7/lib/cards.s7i          0.038764           1342
seed7/lib/category.s7i       0.040523            209
seed7/lib/cc_conf.s7i        0.040205           1314
seed7/lib/ccittfax.s7i       0.040531           1022
seed7/lib/cgi.s7i            0.039776            109
seed7/lib/cgidialog.s7i      0.040895           1118
seed7/lib/char.s7i           0.040368            356
seed7/lib/charsets.s7i       0.041470           2024
seed7/lib/chartype.s7i       0.043441            121
seed7/lib/cipher.s7i         0.040179            146
seed7/lib/cli_cmds.s7i       0.041837           1360
seed7/lib/clib_file.s7i      0.040502            301
seed7/lib/color.s7i          0.040252            185
seed7/lib/complex.s7i        0.041040            464
seed7/lib/compress.s7i       0.039496            150
seed7/lib/console.s7i        0.038849            188
seed7/lib/cpio.s7i           0.039992           1708
seed7/lib/crc32.s7i          0.040083            193
seed7/lib/cronos16.s7i       0.042860           1173
seed7/lib/cronos27.s7i       0.045277           1464
seed7/lib/csv.s7i            0.040361            201
seed7/lib/db_prop.s7i        0.040854            991
seed7/lib/deflate.s7i        0.041365            740
seed7/lib/des.s7i            0.041370            444
seed7/lib/dialog.s7i         0.042573            311
seed7/lib/dir.s7i            0.043697            163
seed7/lib/draw.s7i           0.041428            854
seed7/lib/duration.s7i       0.042120           1038
seed7/lib/echo.s7i           0.041818            132
seed7/lib/editline.s7i       0.041412            398
seed7/lib/elf.s7i            0.042779           1560
seed7/lib/elliptic.s7i       0.040509            649
seed7/lib/enable_io.s7i      0.040257            312
seed7/lib/encoding.s7i       0.045921            931
seed7/lib/enumeration.s7i    0.045253            236
seed7/lib/environment.s7i    0.042419            175
seed7/lib/exif.s7i           0.044657            152
seed7/lib/external_file.s7i  0.043948            340
seed7/lib/field.s7i          0.045939            268
seed7/lib/file.s7i           0.043753            372
seed7/lib/filebits.s7i       0.038574             46
seed7/lib/filesys.s7i        0.041497            601
seed7/lib/fileutil.s7i       0.044296            144
seed7/lib/fixarray.s7i       0.046948            307
seed7/lib/float.s7i          0.043941            757
seed7/lib/font.s7i           0.040198            196
seed7/lib/font8x8.s7i        0.043078            998
seed7/lib/forloop.s7i        0.050571            449
seed7/lib/ftp.s7i            0.045722            969
seed7/lib/ftpserv.s7i        0.045637            631
seed7/lib/getf.s7i           0.042052            115
seed7/lib/gethttp.s7i        0.042278             41
seed7/lib/gethttps.s7i       0.042382             41
seed7/lib/gif.s7i            0.045960            561
seed7/lib/graph.s7i          0.045842            415
seed7/lib/graph_file.s7i     0.042249            399
seed7/lib/gtkserver.s7i      0.043832            161
seed7/lib/gzip.s7i           0.042833            573
seed7/lib/hash.s7i           0.043063            421
seed7/lib/hashsetof.s7i      0.043154            499
seed7/lib/hmac.s7i           0.042460            152
seed7/lib/html.s7i           0.044639             83
seed7/lib/html_ent.s7i       0.045332            476
seed7/lib/htmldom.s7i        0.044869            286
seed7/lib/http_request.s7i   0.043819            696
seed7/lib/http_srv_resp.s7i  0.043390            380
seed7/lib/https_request.s7i  0.044676            211
seed7/lib/httpserv.s7i       0.046046            345
seed7/lib/huffman.s7i        0.047270            644
seed7/lib/ico.s7i            0.046509            221
seed7/lib/idxarray.s7i       0.045691            232
seed7/lib/image.s7i          0.045930            156
seed7/lib/imagefile.s7i      0.040779            171
seed7/lib/inflate.s7i        0.040743            411
seed7/lib/inifile.s7i        0.040588            129
seed7/lib/integer.s7i        0.041150            663
seed7/lib/iobuffer.s7i       0.041760            289
seed7/lib/jpeg.s7i           0.041038           1761
seed7/lib/json.s7i           0.040365            891
seed7/lib/json_serde.s7i     0.044478            783
seed7/lib/keybd.s7i          0.042376            639
seed7/lib/keydescr.s7i       0.045310            192
seed7/lib/leb128.s7i         0.043715            218
seed7/lib/line.s7i           0.041063            164
seed7/lib/listener.s7i       0.040429            247
seed7/lib/logfile.s7i        0.043513             73
seed7/lib/lower.s7i          0.044477            142
seed7/lib/lzma.s7i           0.044153            934
seed7/lib/lzw.s7i            0.046431            861
seed7/lib/magic.s7i          0.046486            403
seed7/lib/mahjng32.s7i       0.044289           1500
seed7/lib/make.s7i           0.045907            544
seed7/lib/makedata.s7i       0.044811           1428
seed7/lib/math.s7i           0.043750            201
seed7/lib/mixarith.s7i       0.041040            249
seed7/lib/modern27.s7i       0.045035           1099
seed7/lib/more.s7i           0.045825            130
seed7/lib/msgdigest.s7i      0.045041           1222
seed7/lib/multiscr.s7i       0.040106             68
seed7/lib/null_file.s7i      0.045622            345
seed7/lib/osfiles.s7i        0.048176           1085
seed7/lib/pbm.s7i            0.044201            230
seed7/lib/pcx.s7i            0.042567            638
seed7/lib/pem.s7i            0.041347            185
seed7/lib/pgm.s7i            0.047346            238
seed7/lib/pic16.s7i          0.048796           1037
seed7/lib/pic32.s7i          0.048095           2060
seed7/lib/pic_util.s7i       0.048215            144
seed7/lib/pixelimage.s7i     0.047677            320
seed7/lib/pixmap_file.s7i    0.043342            459
seed7/lib/pixmapfont.s7i     0.045753            184
seed7/lib/pkcs1.s7i          0.052185            543
seed7/lib/png.s7i            0.043657           1064
seed7/lib/poll.s7i           0.041387            313
seed7/lib/ppm.s7i            0.043080            240
seed7/lib/process.s7i        0.047626            541
seed7/lib/progs.s7i          0.048215            789
seed7/lib/propertyfile.s7i   0.045734            155
seed7/lib/rational.s7i       0.045304            792
seed7/lib/ref_list.s7i       0.044602            252
seed7/lib/reference.s7i      0.043649            126
seed7/lib/reverse.s7i        0.042203             94
seed7/lib/rpm.s7i            0.041243           3487
seed7/lib/rpmext.s7i         0.040605            318
seed7/lib/scanfile.s7i       0.042844           1779
seed7/lib/scanjson.s7i       0.050898            413
seed7/lib/scanstri.s7i       0.050028           1814
seed7/lib/scantoml.s7i       0.050589           1603
seed7/lib/seed7_05.s7i       0.045635           1072
seed7/lib/set.s7i            0.038329             57
seed7/lib/shell.s7i          0.040125            615
seed7/lib/showtls.s7i        0.039596            678
seed7/lib/signature.s7i      0.039479            131
seed7/lib/smtp.s7i           0.040752            261
seed7/lib/sockbase.s7i       0.040481            217
seed7/lib/socket.s7i         0.042937            326
seed7/lib/sokoban1.s7i       0.043800           1519
seed7/lib/sql_base.s7i       0.045564           1000
seed7/lib/stars.s7i          0.046374           1705
seed7/lib/stdfont10.s7i      0.041693           3347
seed7/lib/stdfont12.s7i      0.046616           3928
seed7/lib/stdfont14.s7i      0.041455           4510
seed7/lib/stdfont16.s7i      0.042024           5092
seed7/lib/stdfont18.s7i      0.041913           5868
seed7/lib/stdfont20.s7i      0.040441           6449
seed7/lib/stdfont24.s7i      0.040390           7421
seed7/lib/stdfont8.s7i       0.039169           2960
seed7/lib/stdfont9.s7i       0.039320           3152
seed7/lib/stdio.s7i          0.040978            192
seed7/lib/strifile.s7i       0.039783            345
seed7/lib/string.s7i         0.040287            779
seed7/lib/stritext.s7i       0.040555            352
seed7/lib/struct.s7i         0.041014            266
seed7/lib/struct_elem.s7i    0.039556            129
seed7/lib/subfile.s7i        0.039729            174
seed7/lib/subrange.s7i       0.038425             78
seed7/lib/syntax.s7i         0.039854            294
seed7/lib/tar.s7i            0.039545           1880
seed7/lib/tar_cmds.s7i       0.040014            752
seed7/lib/tdes.s7i           0.042854            143
seed7/lib/tee.s7i            0.040639            143
seed7/lib/text.s7i           0.039123            135
seed7/lib/tga.s7i            0.041910            676
seed7/lib/tiff.s7i           0.041559           2771
seed7/lib/time.s7i           0.041030           1191
seed7/lib/tls.s7i            0.043262           2230
seed7/lib/unicode.s7i        0.042998            575
seed7/lib/unionfnd.s7i       0.040499            130
seed7/lib/upper.s7i          0.040179            142
seed7/lib/utf16.s7i          0.040711            540
seed7/lib/utf8.s7i           0.041256            234
seed7/lib/vecfont10.s7i      0.043108           1056
seed7/lib/vecfont18.s7i      0.043873           1119
seed7/lib/vector3d.s7i       0.040436            293
seed7/lib/vectorfont.s7i     0.040544            239
seed7/lib/wildcard.s7i       0.040313            140
seed7/lib/window.s7i         0.040314            455
seed7/lib/wrinum.s7i         0.040511            248
seed7/lib/x509cert.s7i       0.040398           1243
seed7/lib/xml_ent.s7i        0.040182             94
seed7/lib/xmldom.s7i         0.040341            303
seed7/lib/xz.s7i             0.040633            442
seed7/lib/zip.s7i            0.040784           2792
seed7/lib/zstd.s7i           0.042313           1263
============================ ================= ================

Statistical Summary — Mode B (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.042717        |
+-----------+-----------------+
| Minimum   | 0.036902        |
+-----------+-----------------+
| Maximum   | 0.052185        |
+-----------+-----------------+

Mode C — Mode Activation + Full-Buffer Fontification (font-lock-ensure)
-----------------------------------------------------------------------

Full-buffer fontification stress test.

File Load Times — Mode C (mean, GC-free)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

============================ ================= ================
File Name                    Mean Time (s)     Line count
============================ ================= ================
seed7/prg/addup.sd7          0.043823            190
seed7/prg/bas7.sd7           0.334746          11459
seed7/prg/bifurk.sd7         0.037437             73
seed7/prg/bigfiles.sd7       0.040565            129
seed7/prg/brainf7.sd7        0.038971             86
seed7/prg/calc7.sd7          0.039394            128
seed7/prg/carddemo.sd7       0.040146            190
seed7/prg/castle.sd7         0.111031           3148
seed7/prg/cat.sd7            0.037621             82
seed7/prg/cellauto.sd7       0.037896             85
seed7/prg/celsius.sd7        0.036324             42
seed7/prg/chk_all.sd7        0.060120            843
seed7/prg/chkarr.sd7         0.365279           8367
seed7/prg/chkbig.sd7         2.134157          29026
seed7/prg/chkbin.sd7         0.530784           6469
seed7/prg/chkbitdata.sd7     0.633632           6624
seed7/prg/chkbool.sd7        0.119848           3157
seed7/prg/chkbst.sd7         0.064416            722
seed7/prg/chkchr.sd7         0.223741           2809
seed7/prg/chkcmd.sd7         0.068776           1205
seed7/prg/chkdb.sd7          0.361034           7454
seed7/prg/chkdecl.sd7        0.063280            448
seed7/prg/chkenum.sd7        0.070964           1230
seed7/prg/chkerr.sd7         0.199000           4663
seed7/prg/chkexc.sd7         0.084305           2627
seed7/prg/chkfil.sd7         0.076335           1615
seed7/prg/chkflt.sd7         1.378257          20620
seed7/prg/chkhent.sd7        0.038506             54
seed7/prg/chkhsh.sd7         0.256377           4548
seed7/prg/chkidx.sd7         1.361515          19567
seed7/prg/chkint.sd7         2.589570          38129
seed7/prg/chkjson.sd7        0.103520           1764
seed7/prg/chkovf.sd7         0.575172           8216
seed7/prg/chkprc.sd7         0.328037          10111
seed7/prg/chkscan.sd7        0.058515            714
seed7/prg/chkset.sd7         0.677117          11974
seed7/prg/chkstr.sd7         1.413889          26952
seed7/prg/chktime.sd7        0.132004           2025
seed7/prg/chktoml.sd7        0.109929           1656
seed7/prg/clock.sd7          0.035866             47
seed7/prg/clock2.sd7         0.035477             43
seed7/prg/clock3.sd7         0.037905             95
seed7/prg/cmpfil.sd7         0.036258             84
seed7/prg/comanche.sd7       0.040343            180
seed7/prg/confval.sd7        0.043073            175
seed7/prg/db7.sd7            0.047910            417
seed7/prg/diff7.sd7          0.043512            263
seed7/prg/dirtst.sd7         0.036768             42
seed7/prg/dirx.sd7           0.039834            152
seed7/prg/dnafight.sd7       0.068582           1381
seed7/prg/dragon.sd7         0.037640             73
seed7/prg/echo.sd7           0.036288             39
seed7/prg/eliza.sd7          0.043733            302
seed7/prg/err.sd7            0.041068             96
seed7/prg/fannkuch.sd7       0.038855            131
seed7/prg/fib.sd7            0.036728             47
seed7/prg/find7.sd7          0.039397            133
seed7/prg/findchar.sd7       0.039749            149
seed7/prg/fractree.sd7       0.037272             55
seed7/prg/ftp7.sd7           0.043787            296
seed7/prg/ftpserv.sd7        0.038624             74
seed7/prg/gcd.sd7            0.037998            109
seed7/prg/gkbd.sd7           0.046884            358
seed7/prg/gtksvtst.sd7       0.037966             94
seed7/prg/hal.sd7            0.040252            250
seed7/prg/hamu.sd7           0.048826            573
seed7/prg/hanoi.sd7          0.036422             55
seed7/prg/hd.sd7             0.036685             79
seed7/prg/hello.sd7          0.035298             32
seed7/prg/hilbert.sd7        0.037178            108
seed7/prg/ide7.sd7           0.041574            196
seed7/prg/kbd.sd7            0.037033             49
seed7/prg/klondike.sd7       0.055955            883
seed7/prg/lander.sd7         0.073274           1551
seed7/prg/lst80bas.sd7       0.044966            344
seed7/prg/lst99bas.sd7       0.046139            401
seed7/prg/lstgwbas.sd7       0.051929            577
seed7/prg/mahjong.sd7        0.082188           1943
seed7/prg/make7.sd7          0.039094            121
seed7/prg/mandelbr.sd7       0.048331            237
seed7/prg/mind.sd7           0.047494            443
seed7/prg/mirror.sd7         0.040659            131
seed7/prg/ms.sd7             0.050126            641
seed7/prg/nicoma.sd7         0.039671            135
seed7/prg/pac.sd7            0.050547            726
seed7/prg/pairs.sd7          0.083483           2025
seed7/prg/panic.sd7          0.098649           2634
seed7/prg/percolation.sd7    0.042898            330
seed7/prg/planets.sd7        0.077587           1486
seed7/prg/portfwd7.sd7       0.039689            139
seed7/prg/prime.sd7          0.037474             74
seed7/prg/printpi1.sd7       0.037237             56
seed7/prg/printpi2.sd7       0.037306             54
seed7/prg/printpi3.sd7       0.040243             60
seed7/prg/pv7.sd7            0.046613            337
seed7/prg/queen.sd7          0.039325            149
seed7/prg/rand.sd7           0.038727            121
seed7/prg/raytrace.sd7       0.049654            538
seed7/prg/rever.sd7          0.055043            816
seed7/prg/roman.sd7          0.036502             38
seed7/prg/s7c.sd7            0.286943           9060
seed7/prg/s7check.sd7        0.036739             68
seed7/prg/savehd7.sd7        0.067601           1110
seed7/prg/self.sd7           0.036467             49
seed7/prg/shisen.sd7         0.072005           1423
seed7/prg/sl.sd7             0.061015           1029
seed7/prg/snake.sd7          0.048233            615
seed7/prg/sokoban.sd7        0.056424            891
seed7/prg/spigotpi.sd7       0.037708             64
seed7/prg/sql7.sd7           0.043064            278
seed7/prg/startrek.sd7       0.060625            979
seed7/prg/sudoku7.sd7        0.102504           2657
seed7/prg/sydir7.sd7         0.046797            384
seed7/prg/syntaxhl.sd7       0.042030            177
seed7/prg/tak.sd7            0.037270             59
seed7/prg/tar7.sd7           0.039276            121
seed7/prg/tch.sd7            0.037186             55
seed7/prg/testfont.sd7       0.038889             95
seed7/prg/tet.sd7            0.045335            479
seed7/prg/tetg.sd7           0.045558            501
seed7/prg/toutf8.sd7         0.042933            240
seed7/prg/tst_cli.sd7        0.035642             40
seed7/prg/tst_srv.sd7        0.035796             47
seed7/prg/wator.sd7          0.052247            651
seed7/prg/which.sd7          0.036927             65
seed7/prg/wiz.sd7            0.106648           2833
seed7/prg/wordcnt.sd7        0.035976             54
seed7/prg/wrinum.sd7         0.037248             43
seed7/prg/wumpus.sd7         0.044510            372
seed7/lib/aes.s7i            0.112506           1144
seed7/lib/aes_gcm.s7i        0.048059            392
seed7/lib/ar.s7i             0.075800           1532
seed7/lib/arc4.s7i           0.039787            144
seed7/lib/archive.s7i        0.039816            143
seed7/lib/archive_base.s7i   0.039558            135
seed7/lib/array.s7i          0.055381            610
seed7/lib/asn1.s7i           0.048567            544
seed7/lib/asn1oid.s7i        0.042887            157
seed7/lib/basearray.s7i      0.049644            450
seed7/lib/bigfile.s7i        0.039156            136
seed7/lib/bigint.s7i         0.056708            824
seed7/lib/bigrat.s7i         0.055286            784
seed7/lib/bin16.s7i          0.051677            592
seed7/lib/bin32.s7i          0.048392            490
seed7/lib/bin64.s7i          0.049613            539
seed7/lib/bitdata.s7i        0.077446           1330
seed7/lib/bitmapfont.s7i     0.040235            215
seed7/lib/bitset.s7i         0.050149            593
seed7/lib/bitsetof.s7i       0.049001            431
seed7/lib/blowfish.s7i       0.058044            383
seed7/lib/bmp.s7i            0.063091            924
seed7/lib/boolean.s7i        0.046075            403
seed7/lib/browser.s7i        0.044048            280
seed7/lib/bstring.s7i        0.042210            227
seed7/lib/bytedata.s7i       0.051689            482
seed7/lib/bzip2.s7i          0.060540            887
seed7/lib/cards.s7i          0.067897           1342
seed7/lib/category.s7i       0.042305            209
seed7/lib/cc_conf.s7i        0.079695           1314
seed7/lib/ccittfax.s7i       0.067861           1022
seed7/lib/cgi.s7i            0.038706            109
seed7/lib/cgidialog.s7i      0.061957           1118
seed7/lib/char.s7i           0.043711            356
seed7/lib/charsets.s7i       0.082317           2024
seed7/lib/chartype.s7i       0.039920            121
seed7/lib/cipher.s7i         0.037933            146
seed7/lib/cli_cmds.s7i       0.070536           1360
seed7/lib/clib_file.s7i      0.045010            301
seed7/lib/color.s7i          0.041644            185
seed7/lib/complex.s7i        0.047664            464
seed7/lib/compress.s7i       0.047386            150
seed7/lib/console.s7i        0.041702            188
seed7/lib/cpio.s7i           0.087232           1708
seed7/lib/crc32.s7i          0.044465            193
seed7/lib/cronos16.s7i       0.098876           1173
seed7/lib/cronos27.s7i       0.120374           1464
seed7/lib/csv.s7i            0.045023            201
seed7/lib/db_prop.s7i        0.068105            991
seed7/lib/deflate.s7i        0.060379            740
seed7/lib/des.s7i            0.056398            444
seed7/lib/dialog.s7i         0.045486            311
seed7/lib/dir.s7i            0.040999            163
seed7/lib/draw.s7i           0.057909            854
seed7/lib/duration.s7i       0.063891           1038
seed7/lib/echo.s7i           0.041297            132
seed7/lib/editline.s7i       0.048045            398
seed7/lib/elf.s7i            0.090084           1560
seed7/lib/elliptic.s7i       0.056462            649
seed7/lib/enable_io.s7i      0.050287            312
seed7/lib/encoding.s7i       0.065965            931
seed7/lib/enumeration.s7i    0.044499            236
seed7/lib/environment.s7i    0.040754            175
seed7/lib/exif.s7i           0.040922            152
seed7/lib/external_file.s7i  0.045596            340
seed7/lib/field.s7i          0.044339            268
seed7/lib/file.s7i           0.047585            372
seed7/lib/filebits.s7i       0.038529             46
seed7/lib/filesys.s7i        0.051480            601
seed7/lib/fileutil.s7i       0.043479            144
seed7/lib/fixarray.s7i       0.047538            307
seed7/lib/float.s7i          0.057600            757
seed7/lib/font.s7i           0.042641            196
seed7/lib/font8x8.s7i        0.051122            998
seed7/lib/forloop.s7i        0.047300            449
seed7/lib/ftp.s7i            0.062291            969
seed7/lib/ftpserv.s7i        0.054885            631
seed7/lib/getf.s7i           0.039568            115
seed7/lib/gethttp.s7i        0.039059             41
seed7/lib/gethttps.s7i       0.038602             41
seed7/lib/gif.s7i            0.052823            561
seed7/lib/graph.s7i          0.052887            415
seed7/lib/graph_file.s7i     0.047331            399
seed7/lib/gtkserver.s7i      0.041156            161
seed7/lib/gzip.s7i           0.050794            573
seed7/lib/hash.s7i           0.051016            421
seed7/lib/hashsetof.s7i      0.049690            499
seed7/lib/hmac.s7i           0.040825            152
seed7/lib/html.s7i           0.037341             83
seed7/lib/html_ent.s7i       0.048423            476
seed7/lib/htmldom.s7i        0.044709            286
seed7/lib/http_request.s7i   0.053525            696
seed7/lib/http_srv_resp.s7i  0.047675            380
seed7/lib/https_request.s7i  0.040863            211
seed7/lib/httpserv.s7i       0.044590            345
seed7/lib/huffman.s7i        0.053181            644
seed7/lib/ico.s7i            0.041664            221
seed7/lib/idxarray.s7i       0.042388            232
seed7/lib/image.s7i          0.038956            156
seed7/lib/imagefile.s7i      0.040438            171
seed7/lib/inflate.s7i        0.048377            411
seed7/lib/inifile.s7i        0.039547            129
seed7/lib/integer.s7i        0.054091            663
seed7/lib/iobuffer.s7i       0.043039            289
seed7/lib/jpeg.s7i           0.085624           1761
seed7/lib/json.s7i           0.056929            891
seed7/lib/json_serde.s7i     0.054340            783
seed7/lib/keybd.s7i          0.056749            639
seed7/lib/keydescr.s7i       0.041955            192
seed7/lib/leb128.s7i         0.039877            218
seed7/lib/line.s7i           0.038128            164
seed7/lib/listener.s7i       0.041362            247
seed7/lib/logfile.s7i        0.037169             73
seed7/lib/lower.s7i          0.037994            142
seed7/lib/lzma.s7i           0.060190            934
seed7/lib/lzw.s7i            0.059681            861
seed7/lib/magic.s7i          0.048711            403
seed7/lib/mahjng32.s7i       0.064857           1500
seed7/lib/make.s7i           0.050265            544
seed7/lib/makedata.s7i       0.071192           1428
seed7/lib/math.s7i           0.041684            201
seed7/lib/mixarith.s7i       0.041633            249
seed7/lib/modern27.s7i       0.087034           1099
seed7/lib/more.s7i           0.039069            130
seed7/lib/msgdigest.s7i      0.080535           1222
seed7/lib/multiscr.s7i       0.037922             68
seed7/lib/null_file.s7i      0.044425            345
seed7/lib/osfiles.s7i        0.066842           1085
seed7/lib/pbm.s7i            0.042206            230
seed7/lib/pcx.s7i            0.053651            638
seed7/lib/pem.s7i            0.040750            185
seed7/lib/pgm.s7i            0.042311            238
seed7/lib/pic16.s7i          0.049976           1037
seed7/lib/pic32.s7i          0.082007           2060
seed7/lib/pic_util.s7i       0.039152            144
seed7/lib/pixelimage.s7i     0.042562            320
seed7/lib/pixmap_file.s7i    0.046108            459
seed7/lib/pixmapfont.s7i     0.040779            184
seed7/lib/pkcs1.s7i          0.059653            543
seed7/lib/png.s7i            0.066528           1064
seed7/lib/poll.s7i           0.045382            313
seed7/lib/ppm.s7i            0.041958            240
seed7/lib/process.s7i        0.050226            541
seed7/lib/progs.s7i          0.057624            789
seed7/lib/propertyfile.s7i   0.039890            155
seed7/lib/rational.s7i       0.055628            792
seed7/lib/ref_list.s7i       0.042848            252
seed7/lib/reference.s7i      0.039206            126
seed7/lib/reverse.s7i        0.037502             94
seed7/lib/rpm.s7i            0.147646           3487
seed7/lib/rpmext.s7i         0.043838            318
seed7/lib/scanfile.s7i       0.082973           1779
seed7/lib/scanjson.s7i       0.048048            413
seed7/lib/scanstri.s7i       0.082426           1814
seed7/lib/scantoml.s7i       0.072745           1603
seed7/lib/seed7_05.s7i       0.068169           1072
seed7/lib/set.s7i            0.038413             57
seed7/lib/shell.s7i          0.055451            615
seed7/lib/showtls.s7i        0.055263            678
seed7/lib/signature.s7i      0.038606            131
seed7/lib/smtp.s7i           0.040951            261
seed7/lib/sockbase.s7i       0.042374            217
seed7/lib/socket.s7i         0.043343            326
seed7/lib/sokoban1.s7i       0.054671           1519
seed7/lib/sql_base.s7i       0.065304           1000
seed7/lib/stars.s7i          0.139992           1705
seed7/lib/stdfont10.s7i      0.087861           3347
seed7/lib/stdfont12.s7i      0.099817           3928
seed7/lib/stdfont14.s7i      0.106540           4510
seed7/lib/stdfont16.s7i      0.122302           5092
seed7/lib/stdfont18.s7i      0.136316           5868
seed7/lib/stdfont20.s7i      0.151270           6449
seed7/lib/stdfont24.s7i      0.185465           7421
seed7/lib/stdfont8.s7i       0.074293           2960
seed7/lib/stdfont9.s7i       0.079083           3152
seed7/lib/stdio.s7i          0.040453            192
seed7/lib/strifile.s7i       0.044555            345
seed7/lib/string.s7i         0.056962            779
seed7/lib/stritext.s7i       0.044498            352
seed7/lib/struct.s7i         0.047057            266
seed7/lib/struct_elem.s7i    0.039657            129
seed7/lib/subfile.s7i        0.043020            174
seed7/lib/subrange.s7i       0.039527             78
seed7/lib/syntax.s7i         0.046989            294
seed7/lib/tar.s7i            0.083361           1880
seed7/lib/tar_cmds.s7i       0.056988            752
seed7/lib/tdes.s7i           0.040671            143
seed7/lib/tee.s7i            0.039003            143
seed7/lib/text.s7i           0.039765            135
seed7/lib/tga.s7i            0.055778            676
seed7/lib/tiff.s7i           0.126984           2771
seed7/lib/time.s7i           0.066042           1191
seed7/lib/tls.s7i            0.110687           2230
seed7/lib/unicode.s7i        0.057822            575
seed7/lib/unionfnd.s7i       0.038912            130
seed7/lib/upper.s7i          0.039024            142
seed7/lib/utf16.s7i          0.050922            540
seed7/lib/utf8.s7i           0.043333            234
seed7/lib/vecfont10.s7i      0.082579           1056
seed7/lib/vecfont18.s7i      0.090117           1119
seed7/lib/vector3d.s7i       0.042174            293
seed7/lib/vectorfont.s7i     0.040997            239
seed7/lib/wildcard.s7i       0.038388            140
seed7/lib/window.s7i         0.045897            455
seed7/lib/wrinum.s7i         0.042889            248
seed7/lib/x509cert.s7i       0.073143           1243
seed7/lib/xml_ent.s7i        0.038722             94
seed7/lib/xmldom.s7i         0.042575            303
seed7/lib/xz.s7i             0.047140            442
seed7/lib/zip.s7i            0.121658           2792
seed7/lib/zstd.s7i           0.070952           1263
============================ ================= ================

Statistical Summary — Mode C (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.091823        |
+-----------+-----------------+
| Minimum   | 0.035298        |
+-----------+-----------------+
| Maximum   | 2.589570        |
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
seed7/prg/addup.sd7          0.047025            190
seed7/prg/bas7.sd7           0.786250          11459
seed7/prg/bifurk.sd7         0.040518             73
seed7/prg/bigfiles.sd7       0.044398            129
seed7/prg/brainf7.sd7        0.041136             86
seed7/prg/calc7.sd7          0.043279            128
seed7/prg/carddemo.sd7       0.048482            190
seed7/prg/castle.sd7         0.223259           3148
seed7/prg/cat.sd7            0.039781             82
seed7/prg/cellauto.sd7       0.039193             85
seed7/prg/celsius.sd7        0.037113             42
seed7/prg/chk_all.sd7        0.083560            843
seed7/prg/chkarr.sd7         0.883872           8367
seed7/prg/chkbig.sd7         4.239324          29026
seed7/prg/chkbin.sd7         1.056140           6469
seed7/prg/chkbitdata.sd7     1.282232           6624
seed7/prg/chkbool.sd7        0.237671           3157
seed7/prg/chkbst.sd7         0.104197            722
seed7/prg/chkchr.sd7         0.493074           2809
seed7/prg/chkcmd.sd7         0.113198           1205
seed7/prg/chkdb.sd7          0.776126           7454
seed7/prg/chkdecl.sd7        0.095062            448
seed7/prg/chkenum.sd7        0.122685           1230
seed7/prg/chkerr.sd7         0.347950           4663
seed7/prg/chkexc.sd7         0.152197           2627
seed7/prg/chkfil.sd7         0.132141           1615
seed7/prg/chkflt.sd7         2.901358          20620
seed7/prg/chkhent.sd7        0.039654             54
seed7/prg/chkhsh.sd7         0.517281           4548
seed7/prg/chkidx.sd7         3.265565          19567
seed7/prg/chkint.sd7         5.721937          38129
seed7/prg/chkjson.sd7        0.192809           1764
seed7/prg/chkovf.sd7         1.240535           8216
seed7/prg/chkprc.sd7         0.719138          10111
seed7/prg/chkscan.sd7        0.092987            714
seed7/prg/chkset.sd7         1.797017          11974
seed7/prg/chkstr.sd7         3.511704          26952
seed7/prg/chktime.sd7        0.252245           2025
seed7/prg/chktoml.sd7        0.200505           1656
seed7/prg/clock.sd7          0.037215             47
seed7/prg/clock2.sd7         0.037140             43
seed7/prg/clock3.sd7         0.043356             95
seed7/prg/cmpfil.sd7         0.039916             84
seed7/prg/comanche.sd7       0.048370            180
seed7/prg/confval.sd7        0.052336            175
seed7/prg/db7.sd7            0.064956            417
seed7/prg/diff7.sd7          0.053470            263
seed7/prg/dirtst.sd7         0.038044             42
seed7/prg/dirx.sd7           0.044734            152
seed7/prg/dnafight.sd7       0.121369           1381
seed7/prg/dragon.sd7         0.039231             73
seed7/prg/echo.sd7           0.038361             39
seed7/prg/eliza.sd7          0.053742            302
seed7/prg/err.sd7            0.046024             96
seed7/prg/fannkuch.sd7       0.043128            131
seed7/prg/fib.sd7            0.038039             47
seed7/prg/find7.sd7          0.042866            133
seed7/prg/findchar.sd7       0.045145            149
seed7/prg/fractree.sd7       0.044103             55
seed7/prg/ftp7.sd7           0.054485            296
seed7/prg/ftpserv.sd7        0.040329             74
seed7/prg/gcd.sd7            0.041730            109
seed7/prg/gkbd.sd7           0.063671            358
seed7/prg/gtksvtst.sd7       0.040292             94
seed7/prg/hal.sd7            0.048150            250
seed7/prg/hamu.sd7           0.068522            573
seed7/prg/hanoi.sd7          0.038867             55
seed7/prg/hd.sd7             0.040115             79
seed7/prg/hello.sd7          0.037972             32
seed7/prg/hilbert.sd7        0.042521            108
seed7/prg/ide7.sd7           0.049424            196
seed7/prg/kbd.sd7            0.037948             49
seed7/prg/klondike.sd7       0.089517            883
seed7/prg/lander.sd7         0.135595           1551
seed7/prg/lst80bas.sd7       0.056523            344
seed7/prg/lst99bas.sd7       0.060569            401
seed7/prg/lstgwbas.sd7       0.073719            577
seed7/prg/mahjong.sd7        0.153781           1943
seed7/prg/make7.sd7          0.045002            121
seed7/prg/mandelbr.sd7       0.052925            237
seed7/prg/mind.sd7           0.064315            443
seed7/prg/mirror.sd7         0.047588            131
seed7/prg/ms.sd7             0.075117            641
seed7/prg/nicoma.sd7         0.044589            135
seed7/prg/pac.sd7            0.074523            726
seed7/prg/pairs.sd7          0.150414           2025
seed7/prg/panic.sd7          0.202525           2634
seed7/prg/percolation.sd7    0.056839            330
seed7/prg/planets.sd7        0.141729           1486
seed7/prg/portfwd7.sd7       0.044420            139
seed7/prg/prime.sd7          0.039509             74
seed7/prg/printpi1.sd7       0.038929             56
seed7/prg/printpi2.sd7       0.039340             54
seed7/prg/printpi3.sd7       0.040355             60
seed7/prg/pv7.sd7            0.059889            337
seed7/prg/queen.sd7          0.044439            149
seed7/prg/rand.sd7           0.042552            121
seed7/prg/raytrace.sd7       0.071130            538
seed7/prg/rever.sd7          0.084819            816
seed7/prg/roman.sd7          0.038069             38
seed7/prg/s7c.sd7            0.645648           9060
seed7/prg/s7check.sd7        0.040163             68
seed7/prg/savehd7.sd7        0.114457           1110
seed7/prg/self.sd7           0.038972             49
seed7/prg/shisen.sd7         0.123825           1423
seed7/prg/sl.sd7             0.100077           1029
seed7/prg/snake.sd7          0.067861            615
seed7/prg/sokoban.sd7        0.086533            891
seed7/prg/spigotpi.sd7       0.040291             64
seed7/prg/sql7.sd7           0.053517            278
seed7/prg/startrek.sd7       0.095634            979
seed7/prg/sudoku7.sd7        0.204534           2657
seed7/prg/sydir7.sd7         0.064890            384
seed7/prg/syntaxhl.sd7       0.049317            177
seed7/prg/tak.sd7            0.038714             59
seed7/prg/tar7.sd7           0.043391            121
seed7/prg/tch.sd7            0.038342             55
seed7/prg/testfont.sd7       0.043220             95
seed7/prg/tet.sd7            0.061927            479
seed7/prg/tetg.sd7           0.063528            501
seed7/prg/toutf8.sd7         0.052865            240
seed7/prg/tst_cli.sd7        0.037732             40
seed7/prg/tst_srv.sd7        0.038041             47
seed7/prg/wator.sd7          0.080622            651
seed7/prg/which.sd7          0.039099             65
seed7/prg/wiz.sd7            0.215380           2833
seed7/prg/wordcnt.sd7        0.038963             54
seed7/prg/wrinum.sd7         0.037320             43
seed7/prg/wumpus.sd7         0.054987            372
seed7/lib/aes.s7i            0.204665           1144
seed7/lib/aes_gcm.s7i        0.064346            392
seed7/lib/ar.s7i             0.133214           1532
seed7/lib/arc4.s7i           0.045098            144
seed7/lib/archive.s7i        0.044834            143
seed7/lib/archive_base.s7i   0.044526            135
seed7/lib/array.s7i          0.077380            610
seed7/lib/asn1.s7i           0.067133            544
seed7/lib/asn1oid.s7i        0.052158            157
seed7/lib/basearray.s7i      0.066139            450
seed7/lib/bigfile.s7i        0.044143            136
seed7/lib/bigint.s7i         0.078827            824
seed7/lib/bigrat.s7i         0.080627            784
seed7/lib/bin16.s7i          0.068341            592
seed7/lib/bin32.s7i          0.063541            490
seed7/lib/bin64.s7i          0.066497            539
seed7/lib/bitdata.s7i        0.126849           1330
seed7/lib/bitmapfont.s7i     0.048509            215
seed7/lib/bitset.s7i         0.066201            593
seed7/lib/bitsetof.s7i       0.062912            431
seed7/lib/blowfish.s7i       0.079562            383
seed7/lib/bmp.s7i            0.103453            924
seed7/lib/boolean.s7i        0.056706            403
seed7/lib/browser.s7i        0.055015            280
seed7/lib/bstring.s7i        0.048886            227
seed7/lib/bytedata.s7i       0.067170            482
seed7/lib/bzip2.s7i          0.092031            887
seed7/lib/cards.s7i          0.106826           1342
seed7/lib/category.s7i       0.053330            209
seed7/lib/cc_conf.s7i        0.127399           1314
seed7/lib/ccittfax.s7i       0.108425           1022
seed7/lib/cgi.s7i            0.046961            109
seed7/lib/cgidialog.s7i      0.100034           1118
seed7/lib/char.s7i           0.051412            356
seed7/lib/charsets.s7i       0.127544           2024
seed7/lib/chartype.s7i       0.048469            121
seed7/lib/cipher.s7i         0.043788            146
seed7/lib/cli_cmds.s7i       0.116560           1360
seed7/lib/clib_file.s7i      0.052327            301
seed7/lib/color.s7i          0.047502            185
seed7/lib/complex.s7i        0.060620            464
seed7/lib/compress.s7i       0.043882            150
seed7/lib/console.s7i        0.047654            188
seed7/lib/cpio.s7i           0.148865           1708
seed7/lib/crc32.s7i          0.057183            193
seed7/lib/cronos16.s7i       0.200691           1173
seed7/lib/cronos27.s7i       0.265158           1464
seed7/lib/csv.s7i            0.050075            201
seed7/lib/db_prop.s7i        0.104546            991
seed7/lib/deflate.s7i        0.087995            740
seed7/lib/des.s7i            0.082161            444
seed7/lib/dialog.s7i         0.057160            311
seed7/lib/dir.s7i            0.044157            163
seed7/lib/draw.s7i           0.086134            854
seed7/lib/duration.s7i       0.100306           1038
seed7/lib/echo.s7i           0.043363            132
seed7/lib/editline.s7i       0.061252            398
seed7/lib/elf.s7i            0.160571           1560
seed7/lib/elliptic.s7i       0.079519            649
seed7/lib/enable_io.s7i      0.054442            312
seed7/lib/encoding.s7i       0.100560            931
seed7/lib/enumeration.s7i    0.051558            236
seed7/lib/environment.s7i    0.045584            175
seed7/lib/exif.s7i           0.047831            152
seed7/lib/external_file.s7i  0.052771            340
seed7/lib/field.s7i          0.052281            268
seed7/lib/file.s7i           0.054664            372
seed7/lib/filebits.s7i       0.039071             46
seed7/lib/filesys.s7i        0.066813            601
seed7/lib/fileutil.s7i       0.045294            144
seed7/lib/fixarray.s7i       0.056471            307
seed7/lib/float.s7i          0.075749            757
seed7/lib/font.s7i           0.047204            196
seed7/lib/font8x8.s7i        0.069846            998
seed7/lib/forloop.s7i        0.061681            449
seed7/lib/ftp.s7i            0.090137            969
seed7/lib/ftpserv.s7i        0.077283            631
seed7/lib/getf.s7i           0.042642            115
seed7/lib/gethttp.s7i        0.038633             41
seed7/lib/gethttps.s7i       0.038353             41
seed7/lib/gif.s7i            0.072722            561
seed7/lib/graph.s7i          0.064649            415
seed7/lib/graph_file.s7i     0.058637            399
seed7/lib/gtkserver.s7i      0.041864            161
seed7/lib/gzip.s7i           0.068619            573
seed7/lib/hash.s7i           0.067559            421
seed7/lib/hashsetof.s7i      0.067553            499
seed7/lib/hmac.s7i           0.045648            152
seed7/lib/html.s7i           0.040490             83
seed7/lib/html_ent.s7i       0.065955            476
seed7/lib/htmldom.s7i        0.055332            286
seed7/lib/http_request.s7i   0.078163            696
seed7/lib/http_srv_resp.s7i  0.059548            380
seed7/lib/https_request.s7i  0.048141            211
seed7/lib/httpserv.s7i       0.056116            345
seed7/lib/huffman.s7i        0.077157            644
seed7/lib/ico.s7i            0.050213            221
seed7/lib/idxarray.s7i       0.057354            232
seed7/lib/image.s7i          0.044269            156
seed7/lib/imagefile.s7i      0.044876            171
seed7/lib/inflate.s7i        0.064745            411
seed7/lib/inifile.s7i        0.042520            129
seed7/lib/integer.s7i        0.071161            663
seed7/lib/iobuffer.s7i       0.052711            289
seed7/lib/jpeg.s7i           0.159155           1761
seed7/lib/json.s7i           0.083398            891
seed7/lib/json_serde.s7i     0.080990            783
seed7/lib/keybd.s7i          0.081569            639
seed7/lib/keydescr.s7i       0.050915            192
seed7/lib/leb128.s7i         0.047873            218
seed7/lib/line.s7i           0.044517            164
seed7/lib/listener.s7i       0.049785            247
seed7/lib/logfile.s7i        0.039908             73
seed7/lib/lower.s7i          0.043146            142
seed7/lib/lzma.s7i           0.099061            934
seed7/lib/lzw.s7i            0.090678            861
seed7/lib/magic.s7i          0.064557            403
seed7/lib/mahjng32.s7i       0.094577           1500
seed7/lib/make.s7i           0.071052            544
seed7/lib/makedata.s7i       0.124565           1428
seed7/lib/math.s7i           0.046991            201
seed7/lib/mixarith.s7i       0.048603            249
seed7/lib/modern27.s7i       0.175596           1099
seed7/lib/more.s7i           0.044407            130
seed7/lib/msgdigest.s7i      0.141890           1222
seed7/lib/multiscr.s7i       0.040375             68
seed7/lib/null_file.s7i      0.053719            345
seed7/lib/osfiles.s7i        0.096878           1085
seed7/lib/pbm.s7i            0.048829            230
seed7/lib/pcx.s7i            0.078473            638
seed7/lib/pem.s7i            0.045408            185
seed7/lib/pgm.s7i            0.052044            238
seed7/lib/pic16.s7i          0.068859           1037
seed7/lib/pic32.s7i          0.127328           2060
seed7/lib/pic_util.s7i       0.045232            144
seed7/lib/pixelimage.s7i     0.054613            320
seed7/lib/pixmap_file.s7i    0.064772            459
seed7/lib/pixmapfont.s7i     0.048791            184
seed7/lib/pkcs1.s7i          0.080929            543
seed7/lib/png.s7i            0.112081           1064
seed7/lib/poll.s7i           0.054401            313
seed7/lib/ppm.s7i            0.051398            240
seed7/lib/process.s7i        0.065595            541
seed7/lib/progs.s7i          0.081720            789
seed7/lib/propertyfile.s7i   0.044468            155
seed7/lib/rational.s7i       0.080048            792
seed7/lib/ref_list.s7i       0.050268            252
seed7/lib/reference.s7i      0.043476            126
seed7/lib/reverse.s7i        0.041255             94
seed7/lib/rpm.s7i            0.296422           3487
seed7/lib/rpmext.s7i         0.054108            318
seed7/lib/scanfile.s7i       0.137632           1779
seed7/lib/scanjson.s7i       0.063462            413
seed7/lib/scanstri.s7i       0.140766           1814
seed7/lib/scantoml.s7i       0.134292           1603
seed7/lib/seed7_05.s7i       0.113500           1072
seed7/lib/set.s7i            0.039026             57
seed7/lib/shell.s7i          0.073370            615
seed7/lib/showtls.s7i        0.088824            678
seed7/lib/signature.s7i      0.044029            131
seed7/lib/smtp.s7i           0.050362            261
seed7/lib/sockbase.s7i       0.052231            217
seed7/lib/socket.s7i         0.055103            326
seed7/lib/sokoban1.s7i       0.082623           1519
seed7/lib/sql_base.s7i       0.097105           1000
seed7/lib/stars.s7i          0.248115           1705
seed7/lib/stdfont10.s7i      0.147369           3347
seed7/lib/stdfont12.s7i      0.171003           3928
seed7/lib/stdfont14.s7i      0.193706           4510
seed7/lib/stdfont16.s7i      0.219174           5092
seed7/lib/stdfont18.s7i      0.253837           5868
seed7/lib/stdfont20.s7i      0.281272           6449
seed7/lib/stdfont24.s7i      0.343009           7421
seed7/lib/stdfont8.s7i       0.132220           2960
seed7/lib/stdfont9.s7i       0.139834           3152
seed7/lib/stdio.s7i          0.046061            192
seed7/lib/strifile.s7i       0.057080            345
seed7/lib/string.s7i         0.081203            779
seed7/lib/stritext.s7i       0.057830            352
seed7/lib/struct.s7i         0.055541            266
seed7/lib/struct_elem.s7i    0.042435            129
seed7/lib/subfile.s7i        0.044107            174
seed7/lib/subrange.s7i       0.039587             78
seed7/lib/syntax.s7i         0.064484            294
seed7/lib/tar.s7i            0.151851           1880
seed7/lib/tar_cmds.s7i       0.086797            752
seed7/lib/tdes.s7i           0.044914            143
seed7/lib/tee.s7i            0.043514            143
seed7/lib/text.s7i           0.043809            135
seed7/lib/tga.s7i            0.082903            676
seed7/lib/tiff.s7i           0.250186           2771
seed7/lib/time.s7i           0.105094           1191
seed7/lib/tls.s7i            0.201578           2230
seed7/lib/unicode.s7i        0.074458            575
seed7/lib/unionfnd.s7i       0.043846            130
seed7/lib/upper.s7i          0.042464            142
seed7/lib/utf16.s7i          0.066934            540
seed7/lib/utf8.s7i           0.048933            234
seed7/lib/vecfont10.s7i      0.164659           1056
seed7/lib/vecfont18.s7i      0.183824           1119
seed7/lib/vector3d.s7i       0.050717            293
seed7/lib/vectorfont.s7i     0.048469            239
seed7/lib/wildcard.s7i       0.042878            140
seed7/lib/window.s7i         0.062178            455
seed7/lib/wrinum.s7i         0.049816            248
seed7/lib/x509cert.s7i       0.120152           1243
seed7/lib/xml_ent.s7i        0.040284             94
seed7/lib/xmldom.s7i         0.051149            303
seed7/lib/xz.s7i             0.062428            442
seed7/lib/zip.s7i            0.240056           2792
seed7/lib/zstd.s7i           0.120874           1263
============================ ================= ================

Statistical Summary — Mode D (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.163300        |
+-----------+-----------------+
| Minimum   | 0.037113        |
+-----------+-----------------+
| Maximum   | 5.721937        |
+-----------+-----------------+

Cross-Mode Comparison
=====================

+------+-----------------+-----------------+-----------------+
| Mode | Average (s)     | Minimum (s)     | Maximum (s)     |
+======+=================+=================+=================+
| A    | 0.037853        | 0.033599        | 0.050347        |
+------+-----------------+-----------------+-----------------+
| B    | 0.042717        | 0.036902        | 0.052185        |
+------+-----------------+-----------------+-----------------+
| C    | 0.091823        | 0.035298        | 2.589570        |
+------+-----------------+-----------------+-----------------+
| D    | 0.163300        | 0.037113        | 5.721937        |
+------+-----------------+-----------------+-----------------+

Phase Timing Summary
====================

Wall-clock times include warm-up work and GC performed by the warm-up helper.

+----------+--------------+--------------+--------------+
| Phase    | Warm-up      | Timed pass   | Mode total   |
+==========+==============+==============+==============+
| Mode A   | 00:00:13.583 | 00:01:04.489 | 00:01:18.072 |
+----------+--------------+--------------+--------------+
| Mode B   | 00:00:15.669 | 00:01:12.845 | 00:01:28.514 |
+----------+--------------+--------------+--------------+
| Mode C   | 00:00:37.182 | 00:02:37.298 | 00:03:14.480 |
+----------+--------------+--------------+--------------+
| Mode D   | 00:01:02.911 | 00:04:39.056 | 00:05:41.968 |
+----------+--------------+--------------+--------------+
| Total    |              |              | 00:11:43.041 |
+----------+--------------+--------------+--------------+


.. ---------------------------------------------------------------------------
