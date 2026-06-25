=======================================================
GC-Controlled Benchmark Report: Seed7 Mode — Four Modes
=======================================================

:Running with: seed7-mode 2026-06-24T20:06:11+0000 W26-3
:Seed7 dir   : /Users/roup/my/dvo/seed7-repos/seed7/
:Started at: 16:16:15 local time
:Generated on: 2026-06-24 20:27:37 UTC
:N Iterations: 5  (mean of N timed opens per file)
:Window system: nil
:Display graphic: nil
:Frame size: 230x84 chars
:Window body: 230x82 chars
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
seed7/prg/addup.sd7          0.035241            190
seed7/prg/bas7.sd7           0.038376          11459
seed7/prg/bifurk.sd7         0.037395             73
seed7/prg/bigfiles.sd7       0.039219            129
seed7/prg/brainf7.sd7        0.037962             86
seed7/prg/calc7.sd7          0.038670            128
seed7/prg/carddemo.sd7       0.039275            190
seed7/prg/castle.sd7         0.037289           3148
seed7/prg/cat.sd7            0.036480             82
seed7/prg/cellauto.sd7       0.038929             85
seed7/prg/celsius.sd7        0.037035             42
seed7/prg/chk_all.sd7        0.034584            843
seed7/prg/chkarr.sd7         0.038375           8367
seed7/prg/chkbig.sd7         0.042510          29026
seed7/prg/chkbin.sd7         0.037422           6469
seed7/prg/chkbitdata.sd7     0.036582           6624
seed7/prg/chkbool.sd7        0.036234           3157
seed7/prg/chkbst.sd7         0.035026            722
seed7/prg/chkchr.sd7         0.035250           2809
seed7/prg/chkcmd.sd7         0.036530           1205
seed7/prg/chkdb.sd7          0.038499           7454
seed7/prg/chkdecl.sd7        0.039028            448
seed7/prg/chkenum.sd7        0.036263           1230
seed7/prg/chkerr.sd7         0.037605           4663
seed7/prg/chkexc.sd7         0.034370           2627
seed7/prg/chkfil.sd7         0.034859           1615
seed7/prg/chkflt.sd7         0.037251          20620
seed7/prg/chkhent.sd7        0.034141             54
seed7/prg/chkhsh.sd7         0.034528           4548
seed7/prg/chkidx.sd7         0.037448          19567
seed7/prg/chkint.sd7         0.042845          38129
seed7/prg/chkjson.sd7        0.036518           1764
seed7/prg/chkovf.sd7         0.035646           8216
seed7/prg/chkprc.sd7         0.038502          10111
seed7/prg/chkscan.sd7        0.037379            714
seed7/prg/chkset.sd7         0.039252          11974
seed7/prg/chkstr.sd7         0.042594          26952
seed7/prg/chktime.sd7        0.036842           2025
seed7/prg/chktoml.sd7        0.038461           1656
seed7/prg/clock.sd7          0.039058             47
seed7/prg/clock2.sd7         0.036827             43
seed7/prg/clock3.sd7         0.038126             95
seed7/prg/cmpfil.sd7         0.039354             84
seed7/prg/comanche.sd7       0.037296            180
seed7/prg/confval.sd7        0.037637            175
seed7/prg/db7.sd7            0.039785            417
seed7/prg/diff7.sd7          0.043105            263
seed7/prg/dirtst.sd7         0.035440             42
seed7/prg/dirx.sd7           0.035194            152
seed7/prg/dnafight.sd7       0.033811           1381
seed7/prg/dragon.sd7         0.037449             73
seed7/prg/echo.sd7           0.039672             39
seed7/prg/eliza.sd7          0.041850            302
seed7/prg/err.sd7            0.040254             96
seed7/prg/fannkuch.sd7       0.036702            131
seed7/prg/fib.sd7            0.036128             47
seed7/prg/find7.sd7          0.036158            133
seed7/prg/findchar.sd7       0.035612            149
seed7/prg/fractree.sd7       0.034141             55
seed7/prg/ftp7.sd7           0.036745            296
seed7/prg/ftpserv.sd7        0.039154             74
seed7/prg/gcd.sd7            0.038448            109
seed7/prg/gkbd.sd7           0.033284            358
seed7/prg/gtksvtst.sd7       0.032541             94
seed7/prg/hal.sd7            0.032904            250
seed7/prg/hamu.sd7           0.032855            573
seed7/prg/hanoi.sd7          0.033611             55
seed7/prg/hd.sd7             0.033659             79
seed7/prg/hello.sd7          0.033550             32
seed7/prg/hilbert.sd7        0.039784            108
seed7/prg/ide7.sd7           0.038864            196
seed7/prg/kbd.sd7            0.036629             49
seed7/prg/klondike.sd7       0.038635            883
seed7/prg/lander.sd7         0.036310           1551
seed7/prg/lst80bas.sd7       0.033830            344
seed7/prg/lst99bas.sd7       0.033506            401
seed7/prg/lstgwbas.sd7       0.033321            577
seed7/prg/mahjong.sd7        0.037900           1943
seed7/prg/make7.sd7          0.037232            121
seed7/prg/mandelbr.sd7       0.037995            237
seed7/prg/mind.sd7           0.036958            443
seed7/prg/mirror.sd7         0.033868            131
seed7/prg/ms.sd7             0.036356            641
seed7/prg/nicoma.sd7         0.036693            135
seed7/prg/pac.sd7            0.036025            726
seed7/prg/pairs.sd7          0.037780           2025
seed7/prg/panic.sd7          0.038203           2634
seed7/prg/percolation.sd7    0.039838            330
seed7/prg/planets.sd7        0.036715           1486
seed7/prg/portfwd7.sd7       0.037517            139
seed7/prg/prime.sd7          0.036263             74
seed7/prg/printpi1.sd7       0.039065             56
seed7/prg/printpi2.sd7       0.039221             54
seed7/prg/printpi3.sd7       0.037425             60
seed7/prg/pv7.sd7            0.038217            337
seed7/prg/queen.sd7          0.037499            149
seed7/prg/rand.sd7           0.036803            121
seed7/prg/raytrace.sd7       0.037882            538
seed7/prg/rever.sd7          0.039364            816
seed7/prg/roman.sd7          0.036049             38
seed7/prg/s7c.sd7            0.036163           9060
seed7/prg/s7check.sd7        0.035309             68
seed7/prg/savehd7.sd7        0.035994           1110
seed7/prg/self.sd7           0.035792             49
seed7/prg/shisen.sd7         0.035134           1423
seed7/prg/sl.sd7             0.035013           1029
seed7/prg/snake.sd7          0.035422            615
seed7/prg/sokoban.sd7        0.035848            891
seed7/prg/spigotpi.sd7       0.035297             64
seed7/prg/sql7.sd7           0.035384            278
seed7/prg/startrek.sd7       0.035636            979
seed7/prg/sudoku7.sd7        0.035731           2657
seed7/prg/sydir7.sd7         0.035634            384
seed7/prg/syntaxhl.sd7       0.035416            177
seed7/prg/tak.sd7            0.036308             59
seed7/prg/tar7.sd7           0.036031            121
seed7/prg/tch.sd7            0.035544             55
seed7/prg/testfont.sd7       0.035349             95
seed7/prg/tet.sd7            0.035913            479
seed7/prg/tetg.sd7           0.035778            501
seed7/prg/toutf8.sd7         0.035574            240
seed7/prg/tst_cli.sd7        0.035636             40
seed7/prg/tst_srv.sd7        0.035361             47
seed7/prg/wator.sd7          0.035606            651
seed7/prg/which.sd7          0.035293             65
seed7/prg/wiz.sd7            0.035968           2833
seed7/prg/wordcnt.sd7        0.035715             54
seed7/prg/wrinum.sd7         0.036109             43
seed7/prg/wumpus.sd7         0.035628            372
seed7/lib/aes.s7i            0.036033           1144
seed7/lib/aes_gcm.s7i        0.035703            392
seed7/lib/ar.s7i             0.035649           1532
seed7/lib/arc4.s7i           0.035693            144
seed7/lib/archive.s7i        0.038287            143
seed7/lib/archive_base.s7i   0.037539            135
seed7/lib/array.s7i          0.035550            610
seed7/lib/asn1.s7i           0.035646            544
seed7/lib/asn1oid.s7i        0.035394            157
seed7/lib/basearray.s7i      0.035490            450
seed7/lib/bigfile.s7i        0.036883            136
seed7/lib/bigint.s7i         0.037139            824
seed7/lib/bigrat.s7i         0.037357            784
seed7/lib/bin16.s7i          0.037513            592
seed7/lib/bin32.s7i          0.036003            490
seed7/lib/bin64.s7i          0.035610            539
seed7/lib/bitdata.s7i        0.037295           1330
seed7/lib/bitmapfont.s7i     0.038779            215
seed7/lib/bitset.s7i         0.036625            593
seed7/lib/bitsetof.s7i       0.036407            431
seed7/lib/blowfish.s7i       0.036031            383
seed7/lib/bmp.s7i            0.035643            924
seed7/lib/boolean.s7i        0.035627            403
seed7/lib/browser.s7i        0.035893            280
seed7/lib/bstring.s7i        0.036215            227
seed7/lib/bytedata.s7i       0.036793            482
seed7/lib/bzip2.s7i          0.035493            887
seed7/lib/cards.s7i          0.035425           1342
seed7/lib/category.s7i       0.037443            209
seed7/lib/cc_conf.s7i        0.037080           1314
seed7/lib/ccittfax.s7i       0.036521           1022
seed7/lib/cgi.s7i            0.036073            109
seed7/lib/cgidialog.s7i      0.036848           1118
seed7/lib/char.s7i           0.036905            356
seed7/lib/charsets.s7i       0.036903           2024
seed7/lib/chartype.s7i       0.037469            121
seed7/lib/cipher.s7i         0.037203            146
seed7/lib/cli_cmds.s7i       0.040110           1360
seed7/lib/clib_file.s7i      0.035552            301
seed7/lib/color.s7i          0.035836            185
seed7/lib/complex.s7i        0.036764            464
seed7/lib/compress.s7i       0.035957            150
seed7/lib/console.s7i        0.035643            188
seed7/lib/cpio.s7i           0.036135           1708
seed7/lib/crc32.s7i          0.035914            193
seed7/lib/cronos16.s7i       0.037425           1173
seed7/lib/cronos27.s7i       0.036352           1464
seed7/lib/csv.s7i            0.035733            201
seed7/lib/db_prop.s7i        0.035746            991
seed7/lib/deflate.s7i        0.035288            740
seed7/lib/des.s7i            0.035835            444
seed7/lib/dialog.s7i         0.035153            311
seed7/lib/dir.s7i            0.035147            163
seed7/lib/draw.s7i           0.035446            854
seed7/lib/duration.s7i       0.035423           1038
seed7/lib/echo.s7i           0.035629            132
seed7/lib/editline.s7i       0.035664            398
seed7/lib/elf.s7i            0.036652           1560
seed7/lib/elliptic.s7i       0.035544            649
seed7/lib/enable_io.s7i      0.035351            312
seed7/lib/encoding.s7i       0.035690            931
seed7/lib/enumeration.s7i    0.035376            236
seed7/lib/environment.s7i    0.037663            175
seed7/lib/exif.s7i           0.037860            152
seed7/lib/external_file.s7i  0.037727            340
seed7/lib/field.s7i          0.035791            268
seed7/lib/file.s7i           0.036006            372
seed7/lib/filebits.s7i       0.038272             46
seed7/lib/filesys.s7i        0.035751            601
seed7/lib/fileutil.s7i       0.036457            144
seed7/lib/fixarray.s7i       0.039769            307
seed7/lib/float.s7i          0.037586            757
seed7/lib/font.s7i           0.037277            196
seed7/lib/font8x8.s7i        0.035864            998
seed7/lib/forloop.s7i        0.035946            449
seed7/lib/ftp.s7i            0.035666            969
seed7/lib/ftpserv.s7i        0.035711            631
seed7/lib/getf.s7i           0.035762            115
seed7/lib/gethttp.s7i        0.036198             41
seed7/lib/gethttps.s7i       0.035677             41
seed7/lib/gif.s7i            0.035749            561
seed7/lib/graph.s7i          0.035662            415
seed7/lib/graph_file.s7i     0.036092            399
seed7/lib/gtkserver.s7i      0.035921            161
seed7/lib/gzip.s7i           0.036002            573
seed7/lib/hash.s7i           0.035552            421
seed7/lib/hashsetof.s7i      0.035393            499
seed7/lib/hmac.s7i           0.035142            152
seed7/lib/html.s7i           0.035536             83
seed7/lib/html_ent.s7i       0.035958            476
seed7/lib/htmldom.s7i        0.036176            286
seed7/lib/http_request.s7i   0.035601            696
seed7/lib/http_srv_resp.s7i  0.035657            380
seed7/lib/https_request.s7i  0.035416            211
seed7/lib/httpserv.s7i       0.035430            345
seed7/lib/huffman.s7i        0.035340            644
seed7/lib/ico.s7i            0.035543            221
seed7/lib/idxarray.s7i       0.038135            232
seed7/lib/image.s7i          0.035204            156
seed7/lib/imagefile.s7i      0.035563            171
seed7/lib/inflate.s7i        0.035814            411
seed7/lib/inifile.s7i        0.035506            129
seed7/lib/integer.s7i        0.035448            663
seed7/lib/iobuffer.s7i       0.035896            289
seed7/lib/jpeg.s7i           0.037571           1761
seed7/lib/json.s7i           0.035496            891
seed7/lib/json_serde.s7i     0.035641            783
seed7/lib/keybd.s7i          0.036139            639
seed7/lib/keydescr.s7i       0.035831            192
seed7/lib/leb128.s7i         0.035587            218
seed7/lib/line.s7i           0.036402            164
seed7/lib/listener.s7i       0.035910            247
seed7/lib/logfile.s7i        0.035962             73
seed7/lib/lower.s7i          0.036275            142
seed7/lib/lzma.s7i           0.035664            934
seed7/lib/lzw.s7i            0.035757            861
seed7/lib/magic.s7i          0.035778            403
seed7/lib/mahjng32.s7i       0.035215           1500
seed7/lib/make.s7i           0.034988            544
seed7/lib/makedata.s7i       0.035254           1428
seed7/lib/math.s7i           0.037285            201
seed7/lib/mixarith.s7i       0.037893            249
seed7/lib/modern27.s7i       0.038636           1099
seed7/lib/more.s7i           0.036715            130
seed7/lib/msgdigest.s7i      0.036945           1222
seed7/lib/multiscr.s7i       0.035789             68
seed7/lib/null_file.s7i      0.035936            345
seed7/lib/osfiles.s7i        0.039235           1085
seed7/lib/pbm.s7i            0.039897            230
seed7/lib/pcx.s7i            0.037920            638
seed7/lib/pem.s7i            0.037015            185
seed7/lib/pgm.s7i            0.037123            238
seed7/lib/pic16.s7i          0.036079           1037
seed7/lib/pic32.s7i          0.035968           2060
seed7/lib/pic_util.s7i       0.036327            144
seed7/lib/pixelimage.s7i     0.035905            320
seed7/lib/pixmap_file.s7i    0.035878            459
seed7/lib/pixmapfont.s7i     0.037201            184
seed7/lib/pkcs1.s7i          0.039783            543
seed7/lib/png.s7i            0.036355           1064
seed7/lib/poll.s7i           0.035588            313
seed7/lib/ppm.s7i            0.034980            240
seed7/lib/process.s7i        0.035203            541
seed7/lib/progs.s7i          0.036530            789
seed7/lib/propertyfile.s7i   0.036523            155
seed7/lib/rational.s7i       0.035060            792
seed7/lib/ref_list.s7i       0.035152            252
seed7/lib/reference.s7i      0.035064            126
seed7/lib/reverse.s7i        0.034842             94
seed7/lib/rpm.s7i            0.035189           3487
seed7/lib/rpmext.s7i         0.035424            318
seed7/lib/scanfile.s7i       0.036550           1779
seed7/lib/scanjson.s7i       0.036392            413
seed7/lib/scanstri.s7i       0.036820           1814
seed7/lib/scantoml.s7i       0.036669           1603
seed7/lib/seed7_05.s7i       0.040249           1072
seed7/lib/set.s7i            0.039580             57
seed7/lib/shell.s7i          0.039032            615
seed7/lib/showtls.s7i        0.037479            678
seed7/lib/signature.s7i      0.036567            131
seed7/lib/smtp.s7i           0.036383            261
seed7/lib/sockbase.s7i       0.035454            217
seed7/lib/socket.s7i         0.036428            326
seed7/lib/sokoban1.s7i       0.036092           1519
seed7/lib/sql_base.s7i       0.036217           1000
seed7/lib/stars.s7i          0.036120           1705
seed7/lib/stdfont10.s7i      0.036160           3347
seed7/lib/stdfont12.s7i      0.036114           3928
seed7/lib/stdfont14.s7i      0.035943           4510
seed7/lib/stdfont16.s7i      0.036022           5092
seed7/lib/stdfont18.s7i      0.036456           5868
seed7/lib/stdfont20.s7i      0.035846           6449
seed7/lib/stdfont24.s7i      0.036209           7421
seed7/lib/stdfont8.s7i       0.036149           2960
seed7/lib/stdfont9.s7i       0.035321           3152
seed7/lib/stdio.s7i          0.035271            192
seed7/lib/strifile.s7i       0.034956            345
seed7/lib/string.s7i         0.035063            779
seed7/lib/stritext.s7i       0.035674            352
seed7/lib/struct.s7i         0.035318            266
seed7/lib/struct_elem.s7i    0.035692            129
seed7/lib/subfile.s7i        0.035769            174
seed7/lib/subrange.s7i       0.035633             78
seed7/lib/syntax.s7i         0.035433            294
seed7/lib/tar.s7i            0.035694           1880
seed7/lib/tar_cmds.s7i       0.036081            752
seed7/lib/tdes.s7i           0.035157            143
seed7/lib/tee.s7i            0.035306            143
seed7/lib/text.s7i           0.035446            135
seed7/lib/tga.s7i            0.035370            676
seed7/lib/tiff.s7i           0.036628           2771
seed7/lib/time.s7i           0.037589           1191
seed7/lib/tls.s7i            0.037775           2230
seed7/lib/unicode.s7i        0.036390            575
seed7/lib/unionfnd.s7i       0.036111            130
seed7/lib/upper.s7i          0.035707            142
seed7/lib/utf16.s7i          0.035827            540
seed7/lib/utf8.s7i           0.035978            234
seed7/lib/vecfont10.s7i      0.036500           1056
seed7/lib/vecfont18.s7i      0.035930           1119
seed7/lib/vector3d.s7i       0.035870            293
seed7/lib/vectorfont.s7i     0.035897            239
seed7/lib/wildcard.s7i       0.035845            140
seed7/lib/window.s7i         0.036225            455
seed7/lib/wrinum.s7i         0.035189            248
seed7/lib/x509cert.s7i       0.035056           1243
seed7/lib/xml_ent.s7i        0.035079             94
seed7/lib/xmldom.s7i         0.034926            303
seed7/lib/xz.s7i             0.035630            442
seed7/lib/zip.s7i            0.036707           2792
seed7/lib/zstd.s7i           0.036665           1263
============================ ================= ================

Statistical Summary — Mode A (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.036444        |
+-----------+-----------------+
| Minimum   | 0.032541        |
+-----------+-----------------+
| Maximum   | 0.043105        |
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
seed7/prg/addup.sd7          0.038394            190
seed7/prg/bas7.sd7           0.038858          11459
seed7/prg/bifurk.sd7         0.035943             73
seed7/prg/bigfiles.sd7       0.037267            129
seed7/prg/brainf7.sd7        0.036704             86
seed7/prg/calc7.sd7          0.039838            128
seed7/prg/carddemo.sd7       0.038538            190
seed7/prg/castle.sd7         0.038664           3148
seed7/prg/cat.sd7            0.036910             82
seed7/prg/cellauto.sd7       0.037501             85
seed7/prg/celsius.sd7        0.035849             42
seed7/prg/chk_all.sd7        0.037987            843
seed7/prg/chkarr.sd7         0.037726           8367
seed7/prg/chkbig.sd7         0.042527          29026
seed7/prg/chkbin.sd7         0.041072           6469
seed7/prg/chkbitdata.sd7     0.039808           6624
seed7/prg/chkbool.sd7        0.037567           3157
seed7/prg/chkbst.sd7         0.037668            722
seed7/prg/chkchr.sd7         0.038527           2809
seed7/prg/chkcmd.sd7         0.036932           1205
seed7/prg/chkdb.sd7          0.039722           7454
seed7/prg/chkdecl.sd7        0.038431            448
seed7/prg/chkenum.sd7        0.036446           1230
seed7/prg/chkerr.sd7         0.039015           4663
seed7/prg/chkexc.sd7         0.036492           2627
seed7/prg/chkfil.sd7         0.037908           1615
seed7/prg/chkflt.sd7         0.041925          20620
seed7/prg/chkhent.sd7        0.036057             54
seed7/prg/chkhsh.sd7         0.038527           4548
seed7/prg/chkidx.sd7         0.040494          19567
seed7/prg/chkint.sd7         0.043550          38129
seed7/prg/chkjson.sd7        0.037838           1764
seed7/prg/chkovf.sd7         0.037969           8216
seed7/prg/chkprc.sd7         0.038020          10111
seed7/prg/chkscan.sd7        0.037955            714
seed7/prg/chkset.sd7         0.042209          11974
seed7/prg/chkstr.sd7         0.045322          26952
seed7/prg/chktime.sd7        0.043952           2025
seed7/prg/chktoml.sd7        0.042023           1656
seed7/prg/clock.sd7          0.035801             47
seed7/prg/clock2.sd7         0.035980             43
seed7/prg/clock3.sd7         0.038437             95
seed7/prg/cmpfil.sd7         0.037448             84
seed7/prg/comanche.sd7       0.038681            180
seed7/prg/confval.sd7        0.039366            175
seed7/prg/db7.sd7            0.041593            417
seed7/prg/diff7.sd7          0.039179            263
seed7/prg/dirtst.sd7         0.035639             42
seed7/prg/dirx.sd7           0.038312            152
seed7/prg/dnafight.sd7       0.039592           1381
seed7/prg/dragon.sd7         0.036882             73
seed7/prg/echo.sd7           0.035706             39
seed7/prg/eliza.sd7          0.039023            302
seed7/prg/err.sd7            0.042526             96
seed7/prg/fannkuch.sd7       0.038194            131
seed7/prg/fib.sd7            0.035894             47
seed7/prg/find7.sd7          0.037526            133
seed7/prg/findchar.sd7       0.038353            149
seed7/prg/fractree.sd7       0.037320             55
seed7/prg/ftp7.sd7           0.038275            296
seed7/prg/ftpserv.sd7        0.037534             74
seed7/prg/gcd.sd7            0.037274            109
seed7/prg/gkbd.sd7           0.038713            358
seed7/prg/gtksvtst.sd7       0.037291             94
seed7/prg/hal.sd7            0.037116            250
seed7/prg/hamu.sd7           0.036365            573
seed7/prg/hanoi.sd7          0.035441             55
seed7/prg/hd.sd7             0.036448             79
seed7/prg/hello.sd7          0.035614             32
seed7/prg/hilbert.sd7        0.037678            108
seed7/prg/ide7.sd7           0.040691            196
seed7/prg/kbd.sd7            0.040348             49
seed7/prg/klondike.sd7       0.037397            883
seed7/prg/lander.sd7         0.038151           1551
seed7/prg/lst80bas.sd7       0.037726            344
seed7/prg/lst99bas.sd7       0.037695            401
seed7/prg/lstgwbas.sd7       0.037695            577
seed7/prg/mahjong.sd7        0.038640           1943
seed7/prg/make7.sd7          0.038629            121
seed7/prg/mandelbr.sd7       0.038128            237
seed7/prg/mind.sd7           0.036194            443
seed7/prg/mirror.sd7         0.038313            131
seed7/prg/ms.sd7             0.037844            641
seed7/prg/nicoma.sd7         0.038460            135
seed7/prg/pac.sd7            0.036934            726
seed7/prg/pairs.sd7          0.039735           2025
seed7/prg/panic.sd7          0.039497           2634
seed7/prg/percolation.sd7    0.038549            330
seed7/prg/planets.sd7        0.039177           1486
seed7/prg/portfwd7.sd7       0.038375            139
seed7/prg/prime.sd7          0.036633             74
seed7/prg/printpi1.sd7       0.036342             56
seed7/prg/printpi2.sd7       0.036551             54
seed7/prg/printpi3.sd7       0.036880             60
seed7/prg/pv7.sd7            0.038108            337
seed7/prg/queen.sd7          0.037845            149
seed7/prg/rand.sd7           0.038395            121
seed7/prg/raytrace.sd7       0.039030            538
seed7/prg/rever.sd7          0.038608            816
seed7/prg/roman.sd7          0.036189             38
seed7/prg/s7c.sd7            0.042230           9060
seed7/prg/s7check.sd7        0.037611             68
seed7/prg/savehd7.sd7        0.040484           1110
seed7/prg/self.sd7           0.036561             49
seed7/prg/shisen.sd7         0.038602           1423
seed7/prg/sl.sd7             0.037331           1029
seed7/prg/snake.sd7          0.037528            615
seed7/prg/sokoban.sd7        0.037729            891
seed7/prg/spigotpi.sd7       0.035992             64
seed7/prg/sql7.sd7           0.037494            278
seed7/prg/startrek.sd7       0.037587            979
seed7/prg/sudoku7.sd7        0.041399           2657
seed7/prg/sydir7.sd7         0.039199            384
seed7/prg/syntaxhl.sd7       0.043273            177
seed7/prg/tak.sd7            0.036217             59
seed7/prg/tar7.sd7           0.040904            121
seed7/prg/tch.sd7            0.040191             55
seed7/prg/testfont.sd7       0.040620             95
seed7/prg/tet.sd7            0.039420            479
seed7/prg/tetg.sd7           0.038182            501
seed7/prg/toutf8.sd7         0.038821            240
seed7/prg/tst_cli.sd7        0.035252             40
seed7/prg/tst_srv.sd7        0.036691             47
seed7/prg/wator.sd7          0.038018            651
seed7/prg/which.sd7          0.037129             65
seed7/prg/wiz.sd7            0.039121           2833
seed7/prg/wordcnt.sd7        0.038026             54
seed7/prg/wrinum.sd7         0.038139             43
seed7/prg/wumpus.sd7         0.039189            372
seed7/lib/aes.s7i            0.043179           1144
seed7/lib/aes_gcm.s7i        0.040275            392
seed7/lib/ar.s7i             0.039869           1532
seed7/lib/arc4.s7i           0.039632            144
seed7/lib/archive.s7i        0.041084            143
seed7/lib/archive_base.s7i   0.040992            135
seed7/lib/array.s7i          0.038931            610
seed7/lib/asn1.s7i           0.038350            544
seed7/lib/asn1oid.s7i        0.041536            157
seed7/lib/basearray.s7i      0.039030            450
seed7/lib/bigfile.s7i        0.038453            136
seed7/lib/bigint.s7i         0.040999            824
seed7/lib/bigrat.s7i         0.039593            784
seed7/lib/bin16.s7i          0.039873            592
seed7/lib/bin32.s7i          0.038860            490
seed7/lib/bin64.s7i          0.039269            539
seed7/lib/bitdata.s7i        0.042206           1330
seed7/lib/bitmapfont.s7i     0.038095            215
seed7/lib/bitset.s7i         0.038470            593
seed7/lib/bitsetof.s7i       0.038984            431
seed7/lib/blowfish.s7i       0.040743            383
seed7/lib/bmp.s7i            0.038050            924
seed7/lib/boolean.s7i        0.036903            403
seed7/lib/browser.s7i        0.037790            280
seed7/lib/bstring.s7i        0.037603            227
seed7/lib/bytedata.s7i       0.037991            482
seed7/lib/bzip2.s7i          0.038740            887
seed7/lib/cards.s7i          0.036037           1342
seed7/lib/category.s7i       0.037508            209
seed7/lib/cc_conf.s7i        0.037659           1314
seed7/lib/ccittfax.s7i       0.038637           1022
seed7/lib/cgi.s7i            0.038436            109
seed7/lib/cgidialog.s7i      0.038851           1118
seed7/lib/char.s7i           0.037780            356
seed7/lib/charsets.s7i       0.039324           2024
seed7/lib/chartype.s7i       0.040500            121
seed7/lib/cipher.s7i         0.037396            146
seed7/lib/cli_cmds.s7i       0.037722           1360
seed7/lib/clib_file.s7i      0.039201            301
seed7/lib/color.s7i          0.038603            185
seed7/lib/complex.s7i        0.037229            464
seed7/lib/compress.s7i       0.041894            150
seed7/lib/console.s7i        0.044060            188
seed7/lib/cpio.s7i           0.038503           1708
seed7/lib/crc32.s7i          0.039809            193
seed7/lib/cronos16.s7i       0.040205           1173
seed7/lib/cronos27.s7i       0.040186           1464
seed7/lib/csv.s7i            0.038546            201
seed7/lib/db_prop.s7i        0.038540            991
seed7/lib/deflate.s7i        0.039002            740
seed7/lib/des.s7i            0.039178            444
seed7/lib/dialog.s7i         0.038522            311
seed7/lib/dir.s7i            0.038795            163
seed7/lib/draw.s7i           0.039543            854
seed7/lib/duration.s7i       0.039885           1038
seed7/lib/echo.s7i           0.040332            132
seed7/lib/editline.s7i       0.038370            398
seed7/lib/elf.s7i            0.039408           1560
seed7/lib/elliptic.s7i       0.036848            649
seed7/lib/enable_io.s7i      0.037888            312
seed7/lib/encoding.s7i       0.038181            931
seed7/lib/enumeration.s7i    0.037689            236
seed7/lib/environment.s7i    0.037699            175
seed7/lib/exif.s7i           0.037755            152
seed7/lib/external_file.s7i  0.037410            340
seed7/lib/field.s7i          0.040000            268
seed7/lib/file.s7i           0.037805            372
seed7/lib/filebits.s7i       0.036069             46
seed7/lib/filesys.s7i        0.038735            601
seed7/lib/fileutil.s7i       0.038453            144
seed7/lib/fixarray.s7i       0.037651            307
seed7/lib/float.s7i          0.037530            757
seed7/lib/font.s7i           0.038240            196
seed7/lib/font8x8.s7i        0.036734            998
seed7/lib/forloop.s7i        0.038319            449
seed7/lib/ftp.s7i            0.038363            969
seed7/lib/ftpserv.s7i        0.038477            631
seed7/lib/getf.s7i           0.038276            115
seed7/lib/gethttp.s7i        0.035855             41
seed7/lib/gethttps.s7i       0.036009             41
seed7/lib/gif.s7i            0.039113            561
seed7/lib/graph.s7i          0.040662            415
seed7/lib/graph_file.s7i     0.038287            399
seed7/lib/gtkserver.s7i      0.038601            161
seed7/lib/gzip.s7i           0.038573            573
seed7/lib/hash.s7i           0.040089            421
seed7/lib/hashsetof.s7i      0.039881            499
seed7/lib/hmac.s7i           0.038453            152
seed7/lib/html.s7i           0.042512             83
seed7/lib/html_ent.s7i       0.038183            476
seed7/lib/htmldom.s7i        0.037649            286
seed7/lib/http_request.s7i   0.037898            696
seed7/lib/http_srv_resp.s7i  0.037867            380
seed7/lib/https_request.s7i  0.039886            211
seed7/lib/httpserv.s7i       0.038277            345
seed7/lib/huffman.s7i        0.038209            644
seed7/lib/ico.s7i            0.038389            221
seed7/lib/idxarray.s7i       0.038352            232
seed7/lib/image.s7i          0.036771            156
seed7/lib/imagefile.s7i      0.038153            171
seed7/lib/inflate.s7i        0.039279            411
seed7/lib/inifile.s7i        0.038190            129
seed7/lib/integer.s7i        0.040411            663
seed7/lib/iobuffer.s7i       0.038995            289
seed7/lib/jpeg.s7i           0.040780           1761
seed7/lib/json.s7i           0.038774            891
seed7/lib/json_serde.s7i     0.038772            783
seed7/lib/keybd.s7i          0.037902            639
seed7/lib/keydescr.s7i       0.038669            192
seed7/lib/leb128.s7i         0.039892            218
seed7/lib/line.s7i           0.038541            164
seed7/lib/listener.s7i       0.038294            247
seed7/lib/logfile.s7i        0.036238             73
seed7/lib/lower.s7i          0.036830            142
seed7/lib/lzma.s7i           0.041196            934
seed7/lib/lzw.s7i            0.040460            861
seed7/lib/magic.s7i          0.038817            403
seed7/lib/mahjng32.s7i       0.038811           1500
seed7/lib/make.s7i           0.038793            544
seed7/lib/makedata.s7i       0.040216           1428
seed7/lib/math.s7i           0.044040            201
seed7/lib/mixarith.s7i       0.040367            249
seed7/lib/modern27.s7i       0.040855           1099
seed7/lib/more.s7i           0.039160            130
seed7/lib/msgdigest.s7i      0.040947           1222
seed7/lib/multiscr.s7i       0.039332             68
seed7/lib/null_file.s7i      0.039008            345
seed7/lib/osfiles.s7i        0.040287           1085
seed7/lib/pbm.s7i            0.039146            230
seed7/lib/pcx.s7i            0.038701            638
seed7/lib/pem.s7i            0.038247            185
seed7/lib/pgm.s7i            0.038569            238
seed7/lib/pic16.s7i          0.037321           1037
seed7/lib/pic32.s7i          0.038443           2060
seed7/lib/pic_util.s7i       0.040754            144
seed7/lib/pixelimage.s7i     0.040882            320
seed7/lib/pixmap_file.s7i    0.040210            459
seed7/lib/pixmapfont.s7i     0.039949            184
seed7/lib/pkcs1.s7i          0.044661            543
seed7/lib/png.s7i            0.039738           1064
seed7/lib/poll.s7i           0.039123            313
seed7/lib/ppm.s7i            0.040986            240
seed7/lib/process.s7i        0.041048            541
seed7/lib/progs.s7i          0.041177            789
seed7/lib/propertyfile.s7i   0.040322            155
seed7/lib/rational.s7i       0.041596            792
seed7/lib/ref_list.s7i       0.040661            252
seed7/lib/reference.s7i      0.038272            126
seed7/lib/reverse.s7i        0.037441             94
seed7/lib/rpm.s7i            0.040129           3487
seed7/lib/rpmext.s7i         0.039528            318
seed7/lib/scanfile.s7i       0.041366           1779
seed7/lib/scanjson.s7i       0.039244            413
seed7/lib/scanstri.s7i       0.039871           1814
seed7/lib/scantoml.s7i       0.041092           1603
seed7/lib/seed7_05.s7i       0.041824           1072
seed7/lib/set.s7i            0.037562             57
seed7/lib/shell.s7i          0.038815            615
seed7/lib/showtls.s7i        0.038839            678
seed7/lib/signature.s7i      0.038323            131
seed7/lib/smtp.s7i           0.038354            261
seed7/lib/sockbase.s7i       0.038406            217
seed7/lib/socket.s7i         0.038196            326
seed7/lib/sokoban1.s7i       0.036791           1519
seed7/lib/sql_base.s7i       0.038399           1000
seed7/lib/stars.s7i          0.039475           1705
seed7/lib/stdfont10.s7i      0.037401           3347
seed7/lib/stdfont12.s7i      0.038459           3928
seed7/lib/stdfont14.s7i      0.037722           4510
seed7/lib/stdfont16.s7i      0.046121           5092
seed7/lib/stdfont18.s7i      0.036614           5868
seed7/lib/stdfont20.s7i      0.036596           6449
seed7/lib/stdfont24.s7i      0.037484           7421
seed7/lib/stdfont8.s7i       0.038175           2960
seed7/lib/stdfont9.s7i       0.037047           3152
seed7/lib/stdio.s7i          0.037958            192
seed7/lib/strifile.s7i       0.038317            345
seed7/lib/string.s7i         0.038459            779
seed7/lib/stritext.s7i       0.038690            352
seed7/lib/struct.s7i         0.038775            266
seed7/lib/struct_elem.s7i    0.038839            129
seed7/lib/subfile.s7i        0.038256            174
seed7/lib/subrange.s7i       0.037374             78
seed7/lib/syntax.s7i         0.038569            294
seed7/lib/tar.s7i            0.038898           1880
seed7/lib/tar_cmds.s7i       0.038724            752
seed7/lib/tdes.s7i           0.038903            143
seed7/lib/tee.s7i            0.038467            143
seed7/lib/text.s7i           0.038581            135
seed7/lib/tga.s7i            0.040556            676
seed7/lib/tiff.s7i           0.039567           2771
seed7/lib/time.s7i           0.038573           1191
seed7/lib/tls.s7i            0.038484           2230
seed7/lib/unicode.s7i        0.038780            575
seed7/lib/unionfnd.s7i       0.037305            130
seed7/lib/upper.s7i          0.037982            142
seed7/lib/utf16.s7i          0.037597            540
seed7/lib/utf8.s7i           0.037560            234
seed7/lib/vecfont10.s7i      0.039272           1056
seed7/lib/vecfont18.s7i      0.039802           1119
seed7/lib/vector3d.s7i       0.037595            293
seed7/lib/vectorfont.s7i     0.037882            239
seed7/lib/wildcard.s7i       0.037352            140
seed7/lib/window.s7i         0.037686            455
seed7/lib/wrinum.s7i         0.038214            248
seed7/lib/x509cert.s7i       0.038586           1243
seed7/lib/xml_ent.s7i        0.037763             94
seed7/lib/xmldom.s7i         0.036498            303
seed7/lib/xz.s7i             0.037571            442
seed7/lib/zip.s7i            0.038759           2792
seed7/lib/zstd.s7i           0.038800           1263
============================ ================= ================

Statistical Summary — Mode B (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.038717        |
+-----------+-----------------+
| Minimum   | 0.035252        |
+-----------+-----------------+
| Maximum   | 0.046121        |
+-----------+-----------------+

Mode C — Mode Activation + Full-Buffer Fontification (font-lock-ensure)
-----------------------------------------------------------------------

Full-buffer fontification stress test.

File Load Times — Mode C (mean, GC-free)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

============================ ================= ================
File Name                    Mean Time (s)     Line count
============================ ================= ================
seed7/prg/addup.sd7          0.037724            190
seed7/prg/bas7.sd7           0.322031          11459
seed7/prg/bifurk.sd7         0.036572             73
seed7/prg/bigfiles.sd7       0.037061            129
seed7/prg/brainf7.sd7        0.036262             86
seed7/prg/calc7.sd7          0.037511            128
seed7/prg/carddemo.sd7       0.039049            190
seed7/prg/castle.sd7         0.110786           3148
seed7/prg/cat.sd7            0.035457             82
seed7/prg/cellauto.sd7       0.035705             85
seed7/prg/celsius.sd7        0.034887             42
seed7/prg/chk_all.sd7        0.057494            843
seed7/prg/chkarr.sd7         0.350554           8367
seed7/prg/chkbig.sd7         2.043048          29026
seed7/prg/chkbin.sd7         0.513003           6469
seed7/prg/chkbitdata.sd7     0.617016           6624
seed7/prg/chkbool.sd7        0.116304           3157
seed7/prg/chkbst.sd7         0.063191            722
seed7/prg/chkchr.sd7         0.216953           2809
seed7/prg/chkcmd.sd7         0.069236           1205
seed7/prg/chkdb.sd7          0.353278           7454
seed7/prg/chkdecl.sd7        0.058211            448
seed7/prg/chkenum.sd7        0.067065           1230
seed7/prg/chkerr.sd7         0.192605           4663
seed7/prg/chkexc.sd7         0.084014           2627
seed7/prg/chkfil.sd7         0.075686           1615
seed7/prg/chkflt.sd7         1.343658          20620
seed7/prg/chkhent.sd7        0.036390             54
seed7/prg/chkhsh.sd7         0.258517           4548
seed7/prg/chkidx.sd7         1.313739          19567
seed7/prg/chkint.sd7         2.510868          38129
seed7/prg/chkjson.sd7        0.109532           1764
seed7/prg/chkovf.sd7         0.563700           8216
seed7/prg/chkprc.sd7         0.321492          10111
seed7/prg/chkscan.sd7        0.055579            714
seed7/prg/chkset.sd7         0.667282          11974
seed7/prg/chkstr.sd7         1.388681          26952
seed7/prg/chktime.sd7        0.128233           2025
seed7/prg/chktoml.sd7        0.106162           1656
seed7/prg/clock.sd7          0.034457             47
seed7/prg/clock2.sd7         0.034498             43
seed7/prg/clock3.sd7         0.036602             95
seed7/prg/cmpfil.sd7         0.035223             84
seed7/prg/comanche.sd7       0.040101            180
seed7/prg/confval.sd7        0.041893            175
seed7/prg/db7.sd7            0.045936            417
seed7/prg/diff7.sd7          0.041832            263
seed7/prg/dirtst.sd7         0.034962             42
seed7/prg/dirx.sd7           0.038012            152
seed7/prg/dnafight.sd7       0.067758           1381
seed7/prg/dragon.sd7         0.039021             73
seed7/prg/echo.sd7           0.035386             39
seed7/prg/eliza.sd7          0.042171            302
seed7/prg/err.sd7            0.039318             96
seed7/prg/fannkuch.sd7       0.036818            131
seed7/prg/fib.sd7            0.035326             47
seed7/prg/find7.sd7          0.037378            133
seed7/prg/findchar.sd7       0.038686            149
seed7/prg/fractree.sd7       0.035267             55
seed7/prg/ftp7.sd7           0.041028            296
seed7/prg/ftpserv.sd7        0.037841             74
seed7/prg/gcd.sd7            0.035668            109
seed7/prg/gkbd.sd7           0.044730            358
seed7/prg/gtksvtst.sd7       0.042586             94
seed7/prg/hal.sd7            0.048879            250
seed7/prg/hamu.sd7           0.054321            573
seed7/prg/hanoi.sd7          0.036786             55
seed7/prg/hd.sd7             0.035800             79
seed7/prg/hello.sd7          0.035206             32
seed7/prg/hilbert.sd7        0.037554            108
seed7/prg/ide7.sd7           0.039850            196
seed7/prg/kbd.sd7            0.035160             49
seed7/prg/klondike.sd7       0.054321            883
seed7/prg/lander.sd7         0.084596           1551
seed7/prg/lst80bas.sd7       0.047309            344
seed7/prg/lst99bas.sd7       0.055351            401
seed7/prg/lstgwbas.sd7       0.052707            577
seed7/prg/mahjong.sd7        0.089274           1943
seed7/prg/make7.sd7          0.039311            121
seed7/prg/mandelbr.sd7       0.040401            237
seed7/prg/mind.sd7           0.046744            443
seed7/prg/mirror.sd7         0.041690            131
seed7/prg/ms.sd7             0.049723            641
seed7/prg/nicoma.sd7         0.044839            135
seed7/prg/pac.sd7            0.048700            726
seed7/prg/pairs.sd7          0.081298           2025
seed7/prg/panic.sd7          0.105050           2634
seed7/prg/percolation.sd7    0.047821            330
seed7/prg/planets.sd7        0.080973           1486
seed7/prg/portfwd7.sd7       0.037652            139
seed7/prg/prime.sd7          0.035278             74
seed7/prg/printpi1.sd7       0.035216             56
seed7/prg/printpi2.sd7       0.034703             54
seed7/prg/printpi3.sd7       0.035099             60
seed7/prg/pv7.sd7            0.042535            337
seed7/prg/queen.sd7          0.036997            149
seed7/prg/rand.sd7           0.036362            121
seed7/prg/raytrace.sd7       0.047546            538
seed7/prg/rever.sd7          0.054213            816
seed7/prg/roman.sd7          0.035919             38
seed7/prg/s7c.sd7            0.283150           9060
seed7/prg/s7check.sd7        0.036962             68
seed7/prg/savehd7.sd7        0.066200           1110
seed7/prg/self.sd7           0.036405             49
seed7/prg/shisen.sd7         0.069368           1423
seed7/prg/sl.sd7             0.057495           1029
seed7/prg/snake.sd7          0.045457            615
seed7/prg/sokoban.sd7        0.054707            891
seed7/prg/spigotpi.sd7       0.035788             64
seed7/prg/sql7.sd7           0.041258            278
seed7/prg/startrek.sd7       0.058574            979
seed7/prg/sudoku7.sd7        0.099521           2657
seed7/prg/sydir7.sd7         0.044946            384
seed7/prg/syntaxhl.sd7       0.040545            177
seed7/prg/tak.sd7            0.035971             59
seed7/prg/tar7.sd7           0.037371            121
seed7/prg/tch.sd7            0.035786             55
seed7/prg/testfont.sd7       0.037560             95
seed7/prg/tet.sd7            0.054189            479
seed7/prg/tetg.sd7           0.045763            501
seed7/prg/toutf8.sd7         0.041444            240
seed7/prg/tst_cli.sd7        0.034260             40
seed7/prg/tst_srv.sd7        0.034564             47
seed7/prg/wator.sd7          0.050269            651
seed7/prg/which.sd7          0.035231             65
seed7/prg/wiz.sd7            0.102972           2833
seed7/prg/wordcnt.sd7        0.036040             54
seed7/prg/wrinum.sd7         0.034610             43
seed7/prg/wumpus.sd7         0.041223            372
seed7/lib/aes.s7i            0.111555           1144
seed7/lib/aes_gcm.s7i        0.046945            392
seed7/lib/ar.s7i             0.075626           1532
seed7/lib/arc4.s7i           0.038081            144
seed7/lib/archive.s7i        0.037899            143
seed7/lib/archive_base.s7i   0.038889            135
seed7/lib/array.s7i          0.053648            610
seed7/lib/asn1.s7i           0.046756            544
seed7/lib/asn1oid.s7i        0.041686            157
seed7/lib/basearray.s7i      0.048104            450
seed7/lib/bigfile.s7i        0.038921            136
seed7/lib/bigint.s7i         0.058337            824
seed7/lib/bigrat.s7i         0.060482            784
seed7/lib/bin16.s7i          0.056665            592
seed7/lib/bin32.s7i          0.050182            490
seed7/lib/bin64.s7i          0.054146            539
seed7/lib/bitdata.s7i        0.080697           1330
seed7/lib/bitmapfont.s7i     0.047035            215
seed7/lib/bitset.s7i         0.050683            593
seed7/lib/bitsetof.s7i       0.051041            431
seed7/lib/blowfish.s7i       0.062818            383
seed7/lib/bmp.s7i            0.062961            924
seed7/lib/boolean.s7i        0.047381            403
seed7/lib/browser.s7i        0.046956            280
seed7/lib/bstring.s7i        0.044400            227
seed7/lib/bytedata.s7i       0.052765            482
seed7/lib/bzip2.s7i          0.062341            887
seed7/lib/cards.s7i          0.073718           1342
seed7/lib/category.s7i       0.045257            209
seed7/lib/cc_conf.s7i        0.080993           1314
seed7/lib/ccittfax.s7i       0.072088           1022
seed7/lib/cgi.s7i            0.040121            109
seed7/lib/cgidialog.s7i      0.066405           1118
seed7/lib/char.s7i           0.050230            356
seed7/lib/charsets.s7i       0.082052           2024
seed7/lib/chartype.s7i       0.038690            121
seed7/lib/cipher.s7i         0.038244            146
seed7/lib/cli_cmds.s7i       0.066954           1360
seed7/lib/clib_file.s7i      0.042988            301
seed7/lib/color.s7i          0.041033            185
seed7/lib/complex.s7i        0.044874            464
seed7/lib/compress.s7i       0.038079            150
seed7/lib/console.s7i        0.039343            188
seed7/lib/cpio.s7i           0.084935           1708
seed7/lib/crc32.s7i          0.043968            193
seed7/lib/cronos16.s7i       0.097485           1173
seed7/lib/cronos27.s7i       0.115856           1464
seed7/lib/csv.s7i            0.039352            201
seed7/lib/db_prop.s7i        0.061308            991
seed7/lib/deflate.s7i        0.053966            740
seed7/lib/des.s7i            0.054188            444
seed7/lib/dialog.s7i         0.042416            311
seed7/lib/dir.s7i            0.037679            163
seed7/lib/draw.s7i           0.055211            854
seed7/lib/duration.s7i       0.059320           1038
seed7/lib/echo.s7i           0.037297            132
seed7/lib/editline.s7i       0.043937            398
seed7/lib/elf.s7i            0.083117           1560
seed7/lib/elliptic.s7i       0.053129            649
seed7/lib/enable_io.s7i      0.043255            312
seed7/lib/encoding.s7i       0.060736            931
seed7/lib/enumeration.s7i    0.041529            236
seed7/lib/environment.s7i    0.039137            175
seed7/lib/exif.s7i           0.039505            152
seed7/lib/external_file.s7i  0.043193            340
seed7/lib/field.s7i          0.041324            268
seed7/lib/file.s7i           0.044057            372
seed7/lib/filebits.s7i       0.035745             46
seed7/lib/filesys.s7i        0.048213            601
seed7/lib/fileutil.s7i       0.039510            144
seed7/lib/fixarray.s7i       0.045707            307
seed7/lib/float.s7i          0.055001            757
seed7/lib/font.s7i           0.039156            196
seed7/lib/font8x8.s7i        0.048527            998
seed7/lib/forloop.s7i        0.044960            449
seed7/lib/ftp.s7i            0.057714            969
seed7/lib/ftpserv.s7i        0.050394            631
seed7/lib/getf.s7i           0.036232            115
seed7/lib/gethttp.s7i        0.034729             41
seed7/lib/gethttps.s7i       0.034807             41
seed7/lib/gif.s7i            0.049811            561
seed7/lib/graph.s7i          0.048123            415
seed7/lib/graph_file.s7i     0.043523            399
seed7/lib/gtkserver.s7i      0.037441            161
seed7/lib/gzip.s7i           0.047300            573
seed7/lib/hash.s7i           0.047937            421
seed7/lib/hashsetof.s7i      0.048370            499
seed7/lib/hmac.s7i           0.037438            152
seed7/lib/html.s7i           0.035747             83
seed7/lib/html_ent.s7i       0.046477            476
seed7/lib/htmldom.s7i        0.042499            286
seed7/lib/http_request.s7i   0.051503            696
seed7/lib/http_srv_resp.s7i  0.045268            380
seed7/lib/https_request.s7i  0.039929            211
seed7/lib/httpserv.s7i       0.043515            345
seed7/lib/huffman.s7i        0.052484            644
seed7/lib/ico.s7i            0.040526            221
seed7/lib/idxarray.s7i       0.041598            232
seed7/lib/image.s7i          0.036670            156
seed7/lib/imagefile.s7i      0.038130            171
seed7/lib/inflate.s7i        0.045691            411
seed7/lib/inifile.s7i        0.036493            129
seed7/lib/integer.s7i        0.051436            663
seed7/lib/iobuffer.s7i       0.041915            289
seed7/lib/jpeg.s7i           0.083326           1761
seed7/lib/json.s7i           0.055587            891
seed7/lib/json_serde.s7i     0.052394            783
seed7/lib/keybd.s7i          0.055150            639
seed7/lib/keydescr.s7i       0.040790            192
seed7/lib/leb128.s7i         0.039456            218
seed7/lib/line.s7i           0.037915            164
seed7/lib/listener.s7i       0.040898            247
seed7/lib/logfile.s7i        0.036006             73
seed7/lib/lower.s7i          0.038122            142
seed7/lib/lzma.s7i           0.058969            934
seed7/lib/lzw.s7i            0.058047            861
seed7/lib/magic.s7i          0.046714            403
seed7/lib/mahjng32.s7i       0.063107           1500
seed7/lib/make.s7i           0.047685            544
seed7/lib/makedata.s7i       0.068020           1428
seed7/lib/math.s7i           0.038750            201
seed7/lib/mixarith.s7i       0.044709            249
seed7/lib/modern27.s7i       0.085834           1099
seed7/lib/more.s7i           0.038270            130
seed7/lib/msgdigest.s7i      0.078410           1222
seed7/lib/multiscr.s7i       0.040602             68
seed7/lib/null_file.s7i      0.042825            345
seed7/lib/osfiles.s7i        0.064773           1085
seed7/lib/pbm.s7i            0.039578            230
seed7/lib/pcx.s7i            0.051776            638
seed7/lib/pem.s7i            0.039294            185
seed7/lib/pgm.s7i            0.039941            238
seed7/lib/pic16.s7i          0.048546           1037
seed7/lib/pic32.s7i          0.079104           2060
seed7/lib/pic_util.s7i       0.038036            144
seed7/lib/pixelimage.s7i     0.041768            320
seed7/lib/pixmap_file.s7i    0.045170            459
seed7/lib/pixmapfont.s7i     0.039594            184
seed7/lib/pkcs1.s7i          0.058807            543
seed7/lib/png.s7i            0.063283           1064
seed7/lib/poll.s7i           0.042739            313
seed7/lib/ppm.s7i            0.039558            240
seed7/lib/process.s7i        0.047677            541
seed7/lib/progs.s7i          0.056174            789
seed7/lib/propertyfile.s7i   0.038534            155
seed7/lib/rational.s7i       0.053514            792
seed7/lib/ref_list.s7i       0.041066            252
seed7/lib/reference.s7i      0.037696            126
seed7/lib/reverse.s7i        0.036728             94
seed7/lib/rpm.s7i            0.140293           3487
seed7/lib/rpmext.s7i         0.041755            318
seed7/lib/scanfile.s7i       0.079963           1779
seed7/lib/scanjson.s7i       0.046001            413
seed7/lib/scanstri.s7i       0.079077           1814
seed7/lib/scantoml.s7i       0.070967           1603
seed7/lib/seed7_05.s7i       0.066190           1072
seed7/lib/set.s7i            0.036175             57
seed7/lib/shell.s7i          0.052920            615
seed7/lib/showtls.s7i        0.053569            678
seed7/lib/signature.s7i      0.037162            131
seed7/lib/smtp.s7i           0.039846            261
seed7/lib/sockbase.s7i       0.041217            217
seed7/lib/socket.s7i         0.046197            326
seed7/lib/sokoban1.s7i       0.059677           1519
seed7/lib/sql_base.s7i       0.067532           1000
seed7/lib/stars.s7i          0.139587           1705
seed7/lib/stdfont10.s7i      0.086914           3347
seed7/lib/stdfont12.s7i      0.095629           3928
seed7/lib/stdfont14.s7i      0.110036           4510
seed7/lib/stdfont16.s7i      0.122630           5092
seed7/lib/stdfont18.s7i      0.137523           5868
seed7/lib/stdfont20.s7i      0.148575           6449
seed7/lib/stdfont24.s7i      0.176702           7421
seed7/lib/stdfont8.s7i       0.072197           2960
seed7/lib/stdfont9.s7i       0.074477           3152
seed7/lib/stdio.s7i          0.038148            192
seed7/lib/strifile.s7i       0.042215            345
seed7/lib/string.s7i         0.054031            779
seed7/lib/stritext.s7i       0.042772            352
seed7/lib/struct.s7i         0.042350            266
seed7/lib/struct_elem.s7i    0.036740            129
seed7/lib/subfile.s7i        0.038108            174
seed7/lib/subrange.s7i       0.036609             78
seed7/lib/syntax.s7i         0.045052            294
seed7/lib/tar.s7i            0.081687           1880
seed7/lib/tar_cmds.s7i       0.055764            752
seed7/lib/tdes.s7i           0.038177            143
seed7/lib/tee.s7i            0.037293            143
seed7/lib/text.s7i           0.036546            135
seed7/lib/tga.s7i            0.052221            676
seed7/lib/tiff.s7i           0.123769           2771
seed7/lib/time.s7i           0.061959           1191
seed7/lib/tls.s7i            0.103457           2230
seed7/lib/unicode.s7i        0.052165            575
seed7/lib/unionfnd.s7i       0.036840            130
seed7/lib/upper.s7i          0.037409            142
seed7/lib/utf16.s7i          0.049385            540
seed7/lib/utf8.s7i           0.040746            234
seed7/lib/vecfont10.s7i      0.078378           1056
seed7/lib/vecfont18.s7i      0.086754           1119
seed7/lib/vector3d.s7i       0.040931            293
seed7/lib/vectorfont.s7i     0.040457            239
seed7/lib/wildcard.s7i       0.037993            140
seed7/lib/window.s7i         0.045088            455
seed7/lib/wrinum.s7i         0.039874            248
seed7/lib/x509cert.s7i       0.070032           1243
seed7/lib/xml_ent.s7i        0.036168             94
seed7/lib/xmldom.s7i         0.040041            303
seed7/lib/xz.s7i             0.044976            442
seed7/lib/zip.s7i            0.118034           2792
seed7/lib/zstd.s7i           0.081392           1263
============================ ================= ================

Statistical Summary — Mode C (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.089399        |
+-----------+-----------------+
| Minimum   | 0.034260        |
+-----------+-----------------+
| Maximum   | 2.510868        |
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
seed7/prg/addup.sd7          0.050192            190
seed7/prg/bas7.sd7           0.792509          11459
seed7/prg/bifurk.sd7         0.044658             73
seed7/prg/bigfiles.sd7       0.048537            129
seed7/prg/brainf7.sd7        0.044687             86
seed7/prg/calc7.sd7          0.048385            128
seed7/prg/carddemo.sd7       0.052854            190
seed7/prg/castle.sd7         0.222013           3148
seed7/prg/cat.sd7            0.040558             82
seed7/prg/cellauto.sd7       0.039885             85
seed7/prg/celsius.sd7        0.039989             42
seed7/prg/chk_all.sd7        0.086621            843
seed7/prg/chkarr.sd7         0.881348           8367
seed7/prg/chkbig.sd7         4.170704          29026
seed7/prg/chkbin.sd7         1.028347           6469
seed7/prg/chkbitdata.sd7     1.269761           6624
seed7/prg/chkbool.sd7        0.232274           3157
seed7/prg/chkbst.sd7         0.105386            722
seed7/prg/chkchr.sd7         0.482467           2809
seed7/prg/chkcmd.sd7         0.109907           1205
seed7/prg/chkdb.sd7          0.768569           7454
seed7/prg/chkdecl.sd7        0.093845            448
seed7/prg/chkenum.sd7        0.121771           1230
seed7/prg/chkerr.sd7         0.339230           4663
seed7/prg/chkexc.sd7         0.148882           2627
seed7/prg/chkfil.sd7         0.127918           1615
seed7/prg/chkflt.sd7         2.823792          20620
seed7/prg/chkhent.sd7        0.037182             54
seed7/prg/chkhsh.sd7         0.504200           4548
seed7/prg/chkidx.sd7         3.163368          19567
seed7/prg/chkint.sd7         5.635506          38129
seed7/prg/chkjson.sd7        0.185724           1764
seed7/prg/chkovf.sd7         1.207329           8216
seed7/prg/chkprc.sd7         0.704401          10111
seed7/prg/chkscan.sd7        0.089687            714
seed7/prg/chkset.sd7         1.752859          11974
seed7/prg/chkstr.sd7         3.415173          26952
seed7/prg/chktime.sd7        0.250800           2025
seed7/prg/chktoml.sd7        0.201630           1656
seed7/prg/clock.sd7          0.038773             47
seed7/prg/clock2.sd7         0.038149             43
seed7/prg/clock3.sd7         0.042360             95
seed7/prg/cmpfil.sd7         0.040440             84
seed7/prg/comanche.sd7       0.048575            180
seed7/prg/confval.sd7        0.053711            175
seed7/prg/db7.sd7            0.065883            417
seed7/prg/diff7.sd7          0.058202            263
seed7/prg/dirtst.sd7         0.040606             42
seed7/prg/dirx.sd7           0.042970            152
seed7/prg/dnafight.sd7       0.115146           1381
seed7/prg/dragon.sd7         0.038189             73
seed7/prg/echo.sd7           0.035269             39
seed7/prg/eliza.sd7          0.050248            302
seed7/prg/err.sd7            0.045555             96
seed7/prg/fannkuch.sd7       0.041141            131
seed7/prg/fib.sd7            0.036668             47
seed7/prg/find7.sd7          0.042228            133
seed7/prg/findchar.sd7       0.044843            149
seed7/prg/fractree.sd7       0.037708             55
seed7/prg/ftp7.sd7           0.051819            296
seed7/prg/ftpserv.sd7        0.038352             74
seed7/prg/gcd.sd7            0.038812            109
seed7/prg/gkbd.sd7           0.062985            358
seed7/prg/gtksvtst.sd7       0.039969             94
seed7/prg/hal.sd7            0.046438            250
seed7/prg/hamu.sd7           0.068791            573
seed7/prg/hanoi.sd7          0.036158             55
seed7/prg/hd.sd7             0.037869             79
seed7/prg/hello.sd7          0.035982             32
seed7/prg/hilbert.sd7        0.040477            108
seed7/prg/ide7.sd7           0.046698            196
seed7/prg/kbd.sd7            0.036340             49
seed7/prg/klondike.sd7       0.087287            883
seed7/prg/lander.sd7         0.129443           1551
seed7/prg/lst80bas.sd7       0.054862            344
seed7/prg/lst99bas.sd7       0.060256            401
seed7/prg/lstgwbas.sd7       0.077119            577
seed7/prg/mahjong.sd7        0.150748           1943
seed7/prg/make7.sd7          0.042660            121
seed7/prg/mandelbr.sd7       0.049308            237
seed7/prg/mind.sd7           0.060838            443
seed7/prg/mirror.sd7         0.044872            131
seed7/prg/ms.sd7             0.068544            641
seed7/prg/nicoma.sd7         0.041576            135
seed7/prg/pac.sd7            0.072216            726
seed7/prg/pairs.sd7          0.140374           2025
seed7/prg/panic.sd7          0.194940           2634
seed7/prg/percolation.sd7    0.060765            330
seed7/prg/planets.sd7        0.138966           1486
seed7/prg/portfwd7.sd7       0.043349            139
seed7/prg/prime.sd7          0.036934             74
seed7/prg/printpi1.sd7       0.036772             56
seed7/prg/printpi2.sd7       0.037000             54
seed7/prg/printpi3.sd7       0.037608             60
seed7/prg/pv7.sd7            0.057783            337
seed7/prg/queen.sd7          0.042300            149
seed7/prg/rand.sd7           0.040524            121
seed7/prg/raytrace.sd7       0.068341            538
seed7/prg/rever.sd7          0.082512            816
seed7/prg/roman.sd7          0.035762             38
seed7/prg/s7c.sd7            0.629999           9060
seed7/prg/s7check.sd7        0.040017             68
seed7/prg/savehd7.sd7        0.115914           1110
seed7/prg/self.sd7           0.037881             49
seed7/prg/shisen.sd7         0.122049           1423
seed7/prg/sl.sd7             0.096102           1029
seed7/prg/snake.sd7          0.066031            615
seed7/prg/sokoban.sd7        0.084362            891
seed7/prg/spigotpi.sd7       0.038855             64
seed7/prg/sql7.sd7           0.050220            278
seed7/prg/startrek.sd7       0.093750            979
seed7/prg/sudoku7.sd7        0.202489           2657
seed7/prg/sydir7.sd7         0.063900            384
seed7/prg/syntaxhl.sd7       0.048730            177
seed7/prg/tak.sd7            0.037927             59
seed7/prg/tar7.sd7           0.041705            121
seed7/prg/tch.sd7            0.036845             55
seed7/prg/testfont.sd7       0.040695             95
seed7/prg/tet.sd7            0.059967            479
seed7/prg/tetg.sd7           0.061420            501
seed7/prg/toutf8.sd7         0.050201            240
seed7/prg/tst_cli.sd7        0.036258             40
seed7/prg/tst_srv.sd7        0.039181             47
seed7/prg/wator.sd7          0.078321            651
seed7/prg/which.sd7          0.037166             65
seed7/prg/wiz.sd7            0.206552           2833
seed7/prg/wordcnt.sd7        0.037848             54
seed7/prg/wrinum.sd7         0.042431             43
seed7/prg/wumpus.sd7         0.056856            372
seed7/lib/aes.s7i            0.201927           1144
seed7/lib/aes_gcm.s7i        0.062172            392
seed7/lib/ar.s7i             0.123543           1532
seed7/lib/arc4.s7i           0.042641            144
seed7/lib/archive.s7i        0.043529            143
seed7/lib/archive_base.s7i   0.043717            135
seed7/lib/array.s7i          0.073828            610
seed7/lib/asn1.s7i           0.062768            544
seed7/lib/asn1oid.s7i        0.048543            157
seed7/lib/basearray.s7i      0.064287            450
seed7/lib/bigfile.s7i        0.040616            136
seed7/lib/bigint.s7i         0.077533            824
seed7/lib/bigrat.s7i         0.081056            784
seed7/lib/bin16.s7i          0.073432            592
seed7/lib/bin32.s7i          0.068787            490
seed7/lib/bin64.s7i          0.067608            539
seed7/lib/bitdata.s7i        0.127017           1330
seed7/lib/bitmapfont.s7i     0.048767            215
seed7/lib/bitset.s7i         0.066296            593
seed7/lib/bitsetof.s7i       0.063420            431
seed7/lib/blowfish.s7i       0.079335            383
seed7/lib/bmp.s7i            0.102486            924
seed7/lib/boolean.s7i        0.057666            403
seed7/lib/browser.s7i        0.054377            280
seed7/lib/bstring.s7i        0.047613            227
seed7/lib/bytedata.s7i       0.064021            482
seed7/lib/bzip2.s7i          0.089526            887
seed7/lib/cards.s7i          0.102359           1342
seed7/lib/category.s7i       0.051137            209
seed7/lib/cc_conf.s7i        0.123161           1314
seed7/lib/ccittfax.s7i       0.105233           1022
seed7/lib/cgi.s7i            0.040615            109
seed7/lib/cgidialog.s7i      0.096412           1118
seed7/lib/char.s7i           0.053639            356
seed7/lib/charsets.s7i       0.123446           2024
seed7/lib/chartype.s7i       0.046436            121
seed7/lib/cipher.s7i         0.041425            146
seed7/lib/cli_cmds.s7i       0.114424           1360
seed7/lib/clib_file.s7i      0.051959            301
seed7/lib/color.s7i          0.049866            185
seed7/lib/complex.s7i        0.058204            464
seed7/lib/compress.s7i       0.041902            150
seed7/lib/console.s7i        0.043735            188
seed7/lib/cpio.s7i           0.149780           1708
seed7/lib/crc32.s7i          0.054059            193
seed7/lib/cronos16.s7i       0.195162           1173
seed7/lib/cronos27.s7i       0.256364           1464
seed7/lib/csv.s7i            0.048150            201
seed7/lib/db_prop.s7i        0.104531            991
seed7/lib/deflate.s7i        0.085679            740
seed7/lib/des.s7i            0.078989            444
seed7/lib/dialog.s7i         0.055007            311
seed7/lib/dir.s7i            0.042832            163
seed7/lib/draw.s7i           0.085176            854
seed7/lib/duration.s7i       0.097846           1038
seed7/lib/echo.s7i           0.041695            132
seed7/lib/editline.s7i       0.058356            398
seed7/lib/elf.s7i            0.156028           1560
seed7/lib/elliptic.s7i       0.075546            649
seed7/lib/enable_io.s7i      0.051665            312
seed7/lib/encoding.s7i       0.096050            931
seed7/lib/enumeration.s7i    0.048694            236
seed7/lib/environment.s7i    0.043546            175
seed7/lib/exif.s7i           0.044328            152
seed7/lib/external_file.s7i  0.050723            340
seed7/lib/field.s7i          0.051110            268
seed7/lib/file.s7i           0.052265            372
seed7/lib/filebits.s7i       0.037044             46
seed7/lib/filesys.s7i        0.067932            601
seed7/lib/fileutil.s7i       0.043849            144
seed7/lib/fixarray.s7i       0.054222            307
seed7/lib/float.s7i          0.074575            757
seed7/lib/font.s7i           0.043069            196
seed7/lib/font8x8.s7i        0.071331            998
seed7/lib/forloop.s7i        0.065048            449
seed7/lib/ftp.s7i            0.087005            969
seed7/lib/ftpserv.s7i        0.073960            631
seed7/lib/getf.s7i           0.040952            115
seed7/lib/gethttp.s7i        0.037607             41
seed7/lib/gethttps.s7i       0.036502             41
seed7/lib/gif.s7i            0.070284            561
seed7/lib/graph.s7i          0.063777            415
seed7/lib/graph_file.s7i     0.055509            399
seed7/lib/gtkserver.s7i      0.040414            161
seed7/lib/gzip.s7i           0.066108            573
seed7/lib/hash.s7i           0.064023            421
seed7/lib/hashsetof.s7i      0.065370            499
seed7/lib/hmac.s7i           0.043380            152
seed7/lib/html.s7i           0.038699             83
seed7/lib/html_ent.s7i       0.062343            476
seed7/lib/htmldom.s7i        0.053422            286
seed7/lib/http_request.s7i   0.077178            696
seed7/lib/http_srv_resp.s7i  0.057759            380
seed7/lib/https_request.s7i  0.046694            211
seed7/lib/httpserv.s7i       0.055005            345
seed7/lib/huffman.s7i        0.074083            644
seed7/lib/ico.s7i            0.055341            221
seed7/lib/idxarray.s7i       0.055128            232
seed7/lib/image.s7i          0.041142            156
seed7/lib/imagefile.s7i      0.045005            171
seed7/lib/inflate.s7i        0.062823            411
seed7/lib/inifile.s7i        0.040973            129
seed7/lib/integer.s7i        0.069622            663
seed7/lib/iobuffer.s7i       0.051912            289
seed7/lib/jpeg.s7i           0.158740           1761
seed7/lib/json.s7i           0.080528            891
seed7/lib/json_serde.s7i     0.081372            783
seed7/lib/keybd.s7i          0.082112            639
seed7/lib/keydescr.s7i       0.051088            192
seed7/lib/leb128.s7i         0.047336            218
seed7/lib/line.s7i           0.043356            164
seed7/lib/listener.s7i       0.050010            247
seed7/lib/logfile.s7i        0.039553             73
seed7/lib/lower.s7i          0.042187            142
seed7/lib/lzma.s7i           0.096844            934
seed7/lib/lzw.s7i            0.091438            861
seed7/lib/magic.s7i          0.062540            403
seed7/lib/mahjng32.s7i       0.090356           1500
seed7/lib/make.s7i           0.070799            544
seed7/lib/makedata.s7i       0.122015           1428
seed7/lib/math.s7i           0.044170            201
seed7/lib/mixarith.s7i       0.047026            249
seed7/lib/modern27.s7i       0.166965           1099
seed7/lib/more.s7i           0.040848            130
seed7/lib/msgdigest.s7i      0.134994           1222
seed7/lib/multiscr.s7i       0.037681             68
seed7/lib/null_file.s7i      0.050241            345
seed7/lib/osfiles.s7i        0.096529           1085
seed7/lib/pbm.s7i            0.047471            230
seed7/lib/pcx.s7i            0.076428            638
seed7/lib/pem.s7i            0.044374            185
seed7/lib/pgm.s7i            0.047467            238
seed7/lib/pic16.s7i          0.064744           1037
seed7/lib/pic32.s7i          0.126549           2060
seed7/lib/pic_util.s7i       0.043642            144
seed7/lib/pixelimage.s7i     0.051974            320
seed7/lib/pixmap_file.s7i    0.061552            459
seed7/lib/pixmapfont.s7i     0.056089            184
seed7/lib/pkcs1.s7i          0.077656            543
seed7/lib/png.s7i            0.108485           1064
seed7/lib/poll.s7i           0.051472            313
seed7/lib/ppm.s7i            0.048199            240
seed7/lib/process.s7i        0.063521            541
seed7/lib/progs.s7i          0.078907            789
seed7/lib/propertyfile.s7i   0.043273            155
seed7/lib/rational.s7i       0.078529            792
seed7/lib/ref_list.s7i       0.048182            252
seed7/lib/reference.s7i      0.040936            126
seed7/lib/reverse.s7i        0.039411             94
seed7/lib/rpm.s7i            0.282949           3487
seed7/lib/rpmext.s7i         0.051243            318
seed7/lib/scanfile.s7i       0.134147           1779
seed7/lib/scanjson.s7i       0.060449            413
seed7/lib/scanstri.s7i       0.136529           1814
seed7/lib/scantoml.s7i       0.136413           1603
seed7/lib/seed7_05.s7i       0.110932           1072
seed7/lib/set.s7i            0.037070             57
seed7/lib/shell.s7i          0.067423            615
seed7/lib/showtls.s7i        0.083257            678
seed7/lib/signature.s7i      0.042092            131
seed7/lib/smtp.s7i           0.051722            261
seed7/lib/sockbase.s7i       0.050062            217
seed7/lib/socket.s7i         0.051977            326
seed7/lib/sokoban1.s7i       0.080358           1519
seed7/lib/sql_base.s7i       0.093985           1000
seed7/lib/stars.s7i          0.236932           1705
seed7/lib/stdfont10.s7i      0.141472           3347
seed7/lib/stdfont12.s7i      0.168521           3928
seed7/lib/stdfont14.s7i      0.186269           4510
seed7/lib/stdfont16.s7i      0.211324           5092
seed7/lib/stdfont18.s7i      0.245015           5868
seed7/lib/stdfont20.s7i      0.274614           6449
seed7/lib/stdfont24.s7i      0.326728           7421
seed7/lib/stdfont8.s7i       0.124571           2960
seed7/lib/stdfont9.s7i       0.131817           3152
seed7/lib/stdio.s7i          0.042560            192
seed7/lib/strifile.s7i       0.052985            345
seed7/lib/string.s7i         0.074823            779
seed7/lib/stritext.s7i       0.054293            352
seed7/lib/struct.s7i         0.055096            266
seed7/lib/struct_elem.s7i    0.041503            129
seed7/lib/subfile.s7i        0.051193            174
seed7/lib/subrange.s7i       0.039475             78
seed7/lib/syntax.s7i         0.059705            294
seed7/lib/tar.s7i            0.150091           1880
seed7/lib/tar_cmds.s7i       0.082743            752
seed7/lib/tdes.s7i           0.042340            143
seed7/lib/tee.s7i            0.040354            143
seed7/lib/text.s7i           0.040211            135
seed7/lib/tga.s7i            0.079057            676
seed7/lib/tiff.s7i           0.242867           2771
seed7/lib/time.s7i           0.101474           1191
seed7/lib/tls.s7i            0.193854           2230
seed7/lib/unicode.s7i        0.072445            575
seed7/lib/unionfnd.s7i       0.041192            130
seed7/lib/upper.s7i          0.041270            142
seed7/lib/utf16.s7i          0.064347            540
seed7/lib/utf8.s7i           0.046026            234
seed7/lib/vecfont10.s7i      0.160469           1056
seed7/lib/vecfont18.s7i      0.181491           1119
seed7/lib/vector3d.s7i       0.048865            293
seed7/lib/vectorfont.s7i     0.047251            239
seed7/lib/wildcard.s7i       0.041976            140
seed7/lib/window.s7i         0.060370            455
seed7/lib/wrinum.s7i         0.049251            248
seed7/lib/x509cert.s7i       0.117956           1243
seed7/lib/xml_ent.s7i        0.040517             94
seed7/lib/xmldom.s7i         0.049654            303
seed7/lib/xz.s7i             0.060368            442
seed7/lib/zip.s7i            0.232999           2792
seed7/lib/zstd.s7i           0.115612           1263
============================ ================= ================

Statistical Summary — Mode D (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.159538        |
+-----------+-----------------+
| Minimum   | 0.035269        |
+-----------+-----------------+
| Maximum   | 5.635506        |
+-----------+-----------------+

Cross-Mode Comparison
=====================

+------+-----------------+-----------------+-----------------+
| Mode | Average (s)     | Minimum (s)     | Maximum (s)     |
+======+=================+=================+=================+
| A    | 0.036444        | 0.032541        | 0.043105        |
+------+-----------------+-----------------+-----------------+
| B    | 0.038717        | 0.035252        | 0.046121        |
+------+-----------------+-----------------+-----------------+
| C    | 0.089399        | 0.034260        | 2.510868        |
+------+-----------------+-----------------+-----------------+
| D    | 0.159538        | 0.035269        | 5.635506        |
+------+-----------------+-----------------+-----------------+

Phase Timing Summary
====================

Wall-clock times include warm-up work and GC performed by the warm-up helper.

+----------+--------------+--------------+--------------+
| Phase    | Warm-up      | Timed pass   | Mode total   |
+==========+==============+==============+==============+
| Mode A   | 00:00:14.034 | 00:01:02.084 | 00:01:16.118 |
+----------+--------------+--------------+--------------+
| Mode B   | 00:00:14.878 | 00:01:06.012 | 00:01:20.891 |
+----------+--------------+--------------+--------------+
| Mode C   | 00:00:35.879 | 00:02:33.286 | 00:03:09.165 |
+----------+--------------+--------------+--------------+
| Mode D   | 00:01:02.905 | 00:04:32.838 | 00:05:35.744 |
+----------+--------------+--------------+--------------+
| Total    |              |              | 00:11:21.926 |
+----------+--------------+--------------+--------------+


.. ---------------------------------------------------------------------------
