=======================================================
GC-Controlled Benchmark Report: Seed7 Mode — Four Modes
=======================================================

:Running with: seed7-mode 2026-06-24T14:48:55+0000 W26-3
:Seed7 dir   : /Users/roup/my/dvo/seed7-repos/seed7/
:Started at: 11:07:20 local time
:Generated on: 2026-06-24 15:18:41 UTC
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
seed7/prg/addup.sd7          0.035594            190
seed7/prg/bas7.sd7           0.035403          11459
seed7/prg/bifurk.sd7         0.034456             73
seed7/prg/bigfiles.sd7       0.036377            129
seed7/prg/brainf7.sd7        0.035754             86
seed7/prg/calc7.sd7          0.036259            128
seed7/prg/carddemo.sd7       0.039538            190
seed7/prg/castle.sd7         0.037112           3148
seed7/prg/cat.sd7            0.036318             82
seed7/prg/cellauto.sd7       0.035969             85
seed7/prg/celsius.sd7        0.032922             42
seed7/prg/chk_all.sd7        0.033062            843
seed7/prg/chkarr.sd7         0.033552           8367
seed7/prg/chkbig.sd7         0.036702          29026
seed7/prg/chkbin.sd7         0.035476           6469
seed7/prg/chkbitdata.sd7     0.035640           6624
seed7/prg/chkbool.sd7        0.032949           3157
seed7/prg/chkbst.sd7         0.033629            722
seed7/prg/chkchr.sd7         0.034002           2809
seed7/prg/chkcmd.sd7         0.034565           1205
seed7/prg/chkdb.sd7          0.035034           7454
seed7/prg/chkdecl.sd7        0.034366            448
seed7/prg/chkenum.sd7        0.034944           1230
seed7/prg/chkerr.sd7         0.034395           4663
seed7/prg/chkexc.sd7         0.033862           2627
seed7/prg/chkfil.sd7         0.033501           1615
seed7/prg/chkflt.sd7         0.040666          20620
seed7/prg/chkhent.sd7        0.041429             54
seed7/prg/chkhsh.sd7         0.035995           4548
seed7/prg/chkidx.sd7         0.036853          19567
seed7/prg/chkint.sd7         0.040707          38129
seed7/prg/chkjson.sd7        0.034532           1764
seed7/prg/chkovf.sd7         0.036238           8216
seed7/prg/chkprc.sd7         0.034320          10111
seed7/prg/chkscan.sd7        0.033989            714
seed7/prg/chkset.sd7         0.035156          11974
seed7/prg/chkstr.sd7         0.038518          26952
seed7/prg/chktime.sd7        0.033828           2025
seed7/prg/chktoml.sd7        0.035550           1656
seed7/prg/clock.sd7          0.034677             47
seed7/prg/clock2.sd7         0.032500             43
seed7/prg/clock3.sd7         0.036676             95
seed7/prg/cmpfil.sd7         0.040462             84
seed7/prg/comanche.sd7       0.035482            180
seed7/prg/confval.sd7        0.036312            175
seed7/prg/db7.sd7            0.035637            417
seed7/prg/diff7.sd7          0.034713            263
seed7/prg/dirtst.sd7         0.033501             42
seed7/prg/dirx.sd7           0.033640            152
seed7/prg/dnafight.sd7       0.034632           1381
seed7/prg/dragon.sd7         0.035173             73
seed7/prg/echo.sd7           0.034884             39
seed7/prg/eliza.sd7          0.033964            302
seed7/prg/err.sd7            0.034315             96
seed7/prg/fannkuch.sd7       0.033475            131
seed7/prg/fib.sd7            0.035026             47
seed7/prg/find7.sd7          0.033275            133
seed7/prg/findchar.sd7       0.033152            149
seed7/prg/fractree.sd7       0.032793             55
seed7/prg/ftp7.sd7           0.033190            296
seed7/prg/ftpserv.sd7        0.034053             74
seed7/prg/gcd.sd7            0.033381            109
seed7/prg/gkbd.sd7           0.034184            358
seed7/prg/gtksvtst.sd7       0.033022             94
seed7/prg/hal.sd7            0.033945            250
seed7/prg/hamu.sd7           0.034552            573
seed7/prg/hanoi.sd7          0.034638             55
seed7/prg/hd.sd7             0.036516             79
seed7/prg/hello.sd7          0.036363             32
seed7/prg/hilbert.sd7        0.034876            108
seed7/prg/ide7.sd7           0.034423            196
seed7/prg/kbd.sd7            0.032291             49
seed7/prg/klondike.sd7       0.033526            883
seed7/prg/lander.sd7         0.033135           1551
seed7/prg/lst80bas.sd7       0.035038            344
seed7/prg/lst99bas.sd7       0.034032            401
seed7/prg/lstgwbas.sd7       0.034654            577
seed7/prg/mahjong.sd7        0.034173           1943
seed7/prg/make7.sd7          0.037993            121
seed7/prg/mandelbr.sd7       0.040190            237
seed7/prg/mind.sd7           0.034984            443
seed7/prg/mirror.sd7         0.036126            131
seed7/prg/ms.sd7             0.034260            641
seed7/prg/nicoma.sd7         0.034052            135
seed7/prg/pac.sd7            0.034582            726
seed7/prg/pairs.sd7          0.034947           2025
seed7/prg/panic.sd7          0.035784           2634
seed7/prg/percolation.sd7    0.034689            330
seed7/prg/planets.sd7        0.033240           1486
seed7/prg/portfwd7.sd7       0.034332            139
seed7/prg/prime.sd7          0.033859             74
seed7/prg/printpi1.sd7       0.034142             56
seed7/prg/printpi2.sd7       0.036631             54
seed7/prg/printpi3.sd7       0.034861             60
seed7/prg/pv7.sd7            0.033688            337
seed7/prg/queen.sd7          0.033401            149
seed7/prg/rand.sd7           0.034373            121
seed7/prg/raytrace.sd7       0.035161            538
seed7/prg/rever.sd7          0.034376            816
seed7/prg/roman.sd7          0.033484             38
seed7/prg/s7c.sd7            0.033217           9060
seed7/prg/s7check.sd7        0.032484             68
seed7/prg/savehd7.sd7        0.033228           1110
seed7/prg/self.sd7           0.033363             49
seed7/prg/shisen.sd7         0.034469           1423
seed7/prg/sl.sd7             0.032440           1029
seed7/prg/snake.sd7          0.033188            615
seed7/prg/sokoban.sd7        0.037307            891
seed7/prg/spigotpi.sd7       0.036058             64
seed7/prg/sql7.sd7           0.033680            278
seed7/prg/startrek.sd7       0.035365            979
seed7/prg/sudoku7.sd7        0.034025           2657
seed7/prg/sydir7.sd7         0.033587            384
seed7/prg/syntaxhl.sd7       0.034551            177
seed7/prg/tak.sd7            0.035962             59
seed7/prg/tar7.sd7           0.039568            121
seed7/prg/tch.sd7            0.040085             55
seed7/prg/testfont.sd7       0.035289             95
seed7/prg/tet.sd7            0.034531            479
seed7/prg/tetg.sd7           0.035140            501
seed7/prg/toutf8.sd7         0.038924            240
seed7/prg/tst_cli.sd7        0.043373             40
seed7/prg/tst_srv.sd7        0.041734             47
seed7/prg/wator.sd7          0.040171            651
seed7/prg/which.sd7          0.039395             65
seed7/prg/wiz.sd7            0.038358           2833
seed7/prg/wordcnt.sd7        0.036961             54
seed7/prg/wrinum.sd7         0.035687             43
seed7/prg/wumpus.sd7         0.033694            372
seed7/lib/aes.s7i            0.034676           1144
seed7/lib/aes_gcm.s7i        0.035696            392
seed7/lib/ar.s7i             0.038349           1532
seed7/lib/arc4.s7i           0.037152            144
seed7/lib/archive.s7i        0.038020            143
seed7/lib/archive_base.s7i   0.038168            135
seed7/lib/array.s7i          0.037025            610
seed7/lib/asn1.s7i           0.042344            544
seed7/lib/asn1oid.s7i        0.038700            157
seed7/lib/basearray.s7i      0.038570            450
seed7/lib/bigfile.s7i        0.035807            136
seed7/lib/bigint.s7i         0.035898            824
seed7/lib/bigrat.s7i         0.039025            784
seed7/lib/bin16.s7i          0.035619            592
seed7/lib/bin32.s7i          0.036374            490
seed7/lib/bin64.s7i          0.038664            539
seed7/lib/bitdata.s7i        0.035999           1330
seed7/lib/bitmapfont.s7i     0.035299            215
seed7/lib/bitset.s7i         0.036993            593
seed7/lib/bitsetof.s7i       0.037818            431
seed7/lib/blowfish.s7i       0.039544            383
seed7/lib/bmp.s7i            0.042315            924
seed7/lib/boolean.s7i        0.040404            403
seed7/lib/browser.s7i        0.042269            280
seed7/lib/bstring.s7i        0.035950            227
seed7/lib/bytedata.s7i       0.036934            482
seed7/lib/bzip2.s7i          0.036558            887
seed7/lib/cards.s7i          0.035430           1342
seed7/lib/category.s7i       0.037921            209
seed7/lib/cc_conf.s7i        0.037462           1314
seed7/lib/ccittfax.s7i       0.037991           1022
seed7/lib/cgi.s7i            0.037240            109
seed7/lib/cgidialog.s7i      0.036963           1118
seed7/lib/char.s7i           0.036611            356
seed7/lib/charsets.s7i       0.033612           2024
seed7/lib/chartype.s7i       0.034032            121
seed7/lib/cipher.s7i         0.033591            146
seed7/lib/cli_cmds.s7i       0.034817           1360
seed7/lib/clib_file.s7i      0.034230            301
seed7/lib/color.s7i          0.033180            185
seed7/lib/complex.s7i        0.033383            464
seed7/lib/compress.s7i       0.033300            150
seed7/lib/console.s7i        0.033323            188
seed7/lib/cpio.s7i           0.036147           1708
seed7/lib/crc32.s7i          0.034604            193
seed7/lib/cronos16.s7i       0.033937           1173
seed7/lib/cronos27.s7i       0.034038           1464
seed7/lib/csv.s7i            0.034012            201
seed7/lib/db_prop.s7i        0.033424            991
seed7/lib/deflate.s7i        0.035719            740
seed7/lib/des.s7i            0.040578            444
seed7/lib/dialog.s7i         0.041708            311
seed7/lib/dir.s7i            0.041339            163
seed7/lib/draw.s7i           0.035989            854
seed7/lib/duration.s7i       0.036544           1038
seed7/lib/echo.s7i           0.036667            132
seed7/lib/editline.s7i       0.035114            398
seed7/lib/elf.s7i            0.033422           1560
seed7/lib/elliptic.s7i       0.032821            649
seed7/lib/enable_io.s7i      0.033509            312
seed7/lib/encoding.s7i       0.034666            931
seed7/lib/enumeration.s7i    0.035095            236
seed7/lib/environment.s7i    0.033511            175
seed7/lib/exif.s7i           0.033158            152
seed7/lib/external_file.s7i  0.032832            340
seed7/lib/field.s7i          0.034145            268
seed7/lib/file.s7i           0.035796            372
seed7/lib/filebits.s7i       0.036405             46
seed7/lib/filesys.s7i        0.034739            601
seed7/lib/fileutil.s7i       0.034024            144
seed7/lib/fixarray.s7i       0.033474            307
seed7/lib/float.s7i          0.033668            757
seed7/lib/font.s7i           0.034483            196
seed7/lib/font8x8.s7i        0.035746            998
seed7/lib/forloop.s7i        0.040899            449
seed7/lib/ftp.s7i            0.042195            969
seed7/lib/ftpserv.s7i        0.039491            631
seed7/lib/getf.s7i           0.035464            115
seed7/lib/gethttp.s7i        0.034818             41
seed7/lib/gethttps.s7i       0.034106             41
seed7/lib/gif.s7i            0.035735            561
seed7/lib/graph.s7i          0.033825            415
seed7/lib/graph_file.s7i     0.033352            399
seed7/lib/gtkserver.s7i      0.033708            161
seed7/lib/gzip.s7i           0.033638            573
seed7/lib/hash.s7i           0.033619            421
seed7/lib/hashsetof.s7i      0.034278            499
seed7/lib/hmac.s7i           0.035803            152
seed7/lib/html.s7i           0.035488             83
seed7/lib/html_ent.s7i       0.033522            476
seed7/lib/htmldom.s7i        0.035136            286
seed7/lib/http_request.s7i   0.036068            696
seed7/lib/http_srv_resp.s7i  0.032791            380
seed7/lib/https_request.s7i  0.032535            211
seed7/lib/httpserv.s7i       0.039107            345
seed7/lib/huffman.s7i        0.039923            644
seed7/lib/ico.s7i            0.036201            221
seed7/lib/idxarray.s7i       0.033521            232
seed7/lib/image.s7i          0.033703            156
seed7/lib/imagefile.s7i      0.035195            171
seed7/lib/inflate.s7i        0.034495            411
seed7/lib/inifile.s7i        0.039846            129
seed7/lib/integer.s7i        0.043167            663
seed7/lib/iobuffer.s7i       0.038160            289
seed7/lib/jpeg.s7i           0.036293           1761
seed7/lib/json.s7i           0.035234            891
seed7/lib/json_serde.s7i     0.034460            783
seed7/lib/keybd.s7i          0.037701            639
seed7/lib/keydescr.s7i       0.035584            192
seed7/lib/leb128.s7i         0.037039            218
seed7/lib/line.s7i           0.041223            164
seed7/lib/listener.s7i       0.041742            247
seed7/lib/logfile.s7i        0.038508             73
seed7/lib/lower.s7i          0.034371            142
seed7/lib/lzma.s7i           0.033585            934
seed7/lib/lzw.s7i            0.034455            861
seed7/lib/magic.s7i          0.033326            403
seed7/lib/mahjng32.s7i       0.032873           1500
seed7/lib/make.s7i           0.033003            544
seed7/lib/makedata.s7i       0.032887           1428
seed7/lib/math.s7i           0.032688            201
seed7/lib/mixarith.s7i       0.034784            249
seed7/lib/modern27.s7i       0.034977           1099
seed7/lib/more.s7i           0.037249            130
seed7/lib/msgdigest.s7i      0.034621           1222
seed7/lib/multiscr.s7i       0.033247             68
seed7/lib/null_file.s7i      0.033519            345
seed7/lib/osfiles.s7i        0.035381           1085
seed7/lib/pbm.s7i            0.035676            230
seed7/lib/pcx.s7i            0.035324            638
seed7/lib/pem.s7i            0.036401            185
seed7/lib/pgm.s7i            0.036210            238
seed7/lib/pic16.s7i          0.034446           1037
seed7/lib/pic32.s7i          0.034524           2060
seed7/lib/pic_util.s7i       0.034113            144
seed7/lib/pixelimage.s7i     0.033795            320
seed7/lib/pixmap_file.s7i    0.033302            459
seed7/lib/pixmapfont.s7i     0.033725            184
seed7/lib/pkcs1.s7i          0.035753            543
seed7/lib/png.s7i            0.033549           1064
seed7/lib/poll.s7i           0.033223            313
seed7/lib/ppm.s7i            0.033388            240
seed7/lib/process.s7i        0.036031            541
seed7/lib/progs.s7i          0.034058            789
seed7/lib/propertyfile.s7i   0.034624            155
seed7/lib/rational.s7i       0.033623            792
seed7/lib/ref_list.s7i       0.034320            252
seed7/lib/reference.s7i      0.036162            126
seed7/lib/reverse.s7i        0.040570             94
seed7/lib/rpm.s7i            0.037480           3487
seed7/lib/rpmext.s7i         0.034997            318
seed7/lib/scanfile.s7i       0.033835           1779
seed7/lib/scanjson.s7i       0.033373            413
seed7/lib/scanstri.s7i       0.033893           1814
seed7/lib/scantoml.s7i       0.033879           1603
seed7/lib/seed7_05.s7i       0.034062           1072
seed7/lib/set.s7i            0.034624             57
seed7/lib/shell.s7i          0.033147            615
seed7/lib/showtls.s7i        0.033272            678
seed7/lib/signature.s7i      0.033094            131
seed7/lib/smtp.s7i           0.033910            261
seed7/lib/sockbase.s7i       0.033883            217
seed7/lib/socket.s7i         0.033931            326
seed7/lib/sokoban1.s7i       0.034827           1519
seed7/lib/sql_base.s7i       0.034080           1000
seed7/lib/stars.s7i          0.033790           1705
seed7/lib/stdfont10.s7i      0.035165           3347
seed7/lib/stdfont12.s7i      0.034993           3928
seed7/lib/stdfont14.s7i      0.034137           4510
seed7/lib/stdfont16.s7i      0.034392           5092
seed7/lib/stdfont18.s7i      0.033824           5868
seed7/lib/stdfont20.s7i      0.035067           6449
seed7/lib/stdfont24.s7i      0.034733           7421
seed7/lib/stdfont8.s7i       0.033640           2960
seed7/lib/stdfont9.s7i       0.034808           3152
seed7/lib/stdio.s7i          0.035463            192
seed7/lib/strifile.s7i       0.033848            345
seed7/lib/string.s7i         0.034254            779
seed7/lib/stritext.s7i       0.035085            352
seed7/lib/struct.s7i         0.034239            266
seed7/lib/struct_elem.s7i    0.033680            129
seed7/lib/subfile.s7i        0.032922            174
seed7/lib/subrange.s7i       0.032998             78
seed7/lib/syntax.s7i         0.032428            294
seed7/lib/tar.s7i            0.035191           1880
seed7/lib/tar_cmds.s7i       0.040420            752
seed7/lib/tdes.s7i           0.037652            143
seed7/lib/tee.s7i            0.034918            143
seed7/lib/text.s7i           0.034152            135
seed7/lib/tga.s7i            0.035391            676
seed7/lib/tiff.s7i           0.035671           2771
seed7/lib/time.s7i           0.034523           1191
seed7/lib/tls.s7i            0.034476           2230
seed7/lib/unicode.s7i        0.033469            575
seed7/lib/unionfnd.s7i       0.033530            130
seed7/lib/upper.s7i          0.033236            142
seed7/lib/utf16.s7i          0.033656            540
seed7/lib/utf8.s7i           0.033717            234
seed7/lib/vecfont10.s7i      0.034546           1056
seed7/lib/vecfont18.s7i      0.034405           1119
seed7/lib/vector3d.s7i       0.033278            293
seed7/lib/vectorfont.s7i     0.033457            239
seed7/lib/wildcard.s7i       0.035019            140
seed7/lib/window.s7i         0.034623            455
seed7/lib/wrinum.s7i         0.033750            248
seed7/lib/x509cert.s7i       0.033343           1243
seed7/lib/xml_ent.s7i        0.033122             94
seed7/lib/xmldom.s7i         0.034095            303
seed7/lib/xz.s7i             0.033480            442
seed7/lib/zip.s7i            0.034244           2792
seed7/lib/zstd.s7i           0.032437           1263
============================ ================= ================

Statistical Summary — Mode A (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.035323        |
+-----------+-----------------+
| Minimum   | 0.032291        |
+-----------+-----------------+
| Maximum   | 0.043373        |
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
seed7/prg/addup.sd7          0.039020            190
seed7/prg/bas7.sd7           0.040676          11459
seed7/prg/bifurk.sd7         0.038119             73
seed7/prg/bigfiles.sd7       0.041402            129
seed7/prg/brainf7.sd7        0.041350             86
seed7/prg/calc7.sd7          0.040805            128
seed7/prg/carddemo.sd7       0.040808            190
seed7/prg/castle.sd7         0.040219           3148
seed7/prg/cat.sd7            0.037602             82
seed7/prg/cellauto.sd7       0.038103             85
seed7/prg/celsius.sd7        0.036373             42
seed7/prg/chk_all.sd7        0.038899            843
seed7/prg/chkarr.sd7         0.040206           8367
seed7/prg/chkbig.sd7         0.043480          29026
seed7/prg/chkbin.sd7         0.038881           6469
seed7/prg/chkbitdata.sd7     0.040442           6624
seed7/prg/chkbool.sd7        0.037625           3157
seed7/prg/chkbst.sd7         0.038789            722
seed7/prg/chkchr.sd7         0.040344           2809
seed7/prg/chkcmd.sd7         0.038924           1205
seed7/prg/chkdb.sd7          0.040545           7454
seed7/prg/chkdecl.sd7        0.038888            448
seed7/prg/chkenum.sd7        0.036778           1230
seed7/prg/chkerr.sd7         0.039011           4663
seed7/prg/chkexc.sd7         0.036578           2627
seed7/prg/chkfil.sd7         0.038629           1615
seed7/prg/chkflt.sd7         0.040331          20620
seed7/prg/chkhent.sd7        0.037163             54
seed7/prg/chkhsh.sd7         0.045504           4548
seed7/prg/chkidx.sd7         0.048085          19567
seed7/prg/chkint.sd7         0.046597          38129
seed7/prg/chkjson.sd7        0.039626           1764
seed7/prg/chkovf.sd7         0.039135           8216
seed7/prg/chkprc.sd7         0.038884          10111
seed7/prg/chkscan.sd7        0.040093            714
seed7/prg/chkset.sd7         0.042556          11974
seed7/prg/chkstr.sd7         0.043063          26952
seed7/prg/chktime.sd7        0.044123           2025
seed7/prg/chktoml.sd7        0.040176           1656
seed7/prg/clock.sd7          0.035884             47
seed7/prg/clock2.sd7         0.035958             43
seed7/prg/clock3.sd7         0.038626             95
seed7/prg/cmpfil.sd7         0.038231             84
seed7/prg/comanche.sd7       0.039138            180
seed7/prg/confval.sd7        0.039751            175
seed7/prg/db7.sd7            0.038140            417
seed7/prg/diff7.sd7          0.039606            263
seed7/prg/dirtst.sd7         0.036135             42
seed7/prg/dirx.sd7           0.039986            152
seed7/prg/dnafight.sd7       0.040553           1381
seed7/prg/dragon.sd7         0.036725             73
seed7/prg/echo.sd7           0.034920             39
seed7/prg/eliza.sd7          0.037825            302
seed7/prg/err.sd7            0.040724             96
seed7/prg/fannkuch.sd7       0.038222            131
seed7/prg/fib.sd7            0.035491             47
seed7/prg/find7.sd7          0.042042            133
seed7/prg/findchar.sd7       0.039845            149
seed7/prg/fractree.sd7       0.040380             55
seed7/prg/ftp7.sd7           0.042248            296
seed7/prg/ftpserv.sd7        0.045178             74
seed7/prg/gcd.sd7            0.041874            109
seed7/prg/gkbd.sd7           0.041353            358
seed7/prg/gtksvtst.sd7       0.038659             94
seed7/prg/hal.sd7            0.036252            250
seed7/prg/hamu.sd7           0.038410            573
seed7/prg/hanoi.sd7          0.036520             55
seed7/prg/hd.sd7             0.036255             79
seed7/prg/hello.sd7          0.035245             32
seed7/prg/hilbert.sd7        0.038345            108
seed7/prg/ide7.sd7           0.038505            196
seed7/prg/kbd.sd7            0.035624             49
seed7/prg/klondike.sd7       0.038489            883
seed7/prg/lander.sd7         0.039270           1551
seed7/prg/lst80bas.sd7       0.037894            344
seed7/prg/lst99bas.sd7       0.038957            401
seed7/prg/lstgwbas.sd7       0.037793            577
seed7/prg/mahjong.sd7        0.038667           1943
seed7/prg/make7.sd7          0.047618            121
seed7/prg/mandelbr.sd7       0.046283            237
seed7/prg/mind.sd7           0.043009            443
seed7/prg/mirror.sd7         0.040447            131
seed7/prg/ms.sd7             0.039583            641
seed7/prg/nicoma.sd7         0.046028            135
seed7/prg/pac.sd7            0.042906            726
seed7/prg/pairs.sd7          0.047167           2025
seed7/prg/panic.sd7          0.048456           2634
seed7/prg/percolation.sd7    0.047218            330
seed7/prg/planets.sd7        0.044452           1486
seed7/prg/portfwd7.sd7       0.041850            139
seed7/prg/prime.sd7          0.040629             74
seed7/prg/printpi1.sd7       0.037209             56
seed7/prg/printpi2.sd7       0.036057             54
seed7/prg/printpi3.sd7       0.036373             60
seed7/prg/pv7.sd7            0.043664            337
seed7/prg/queen.sd7          0.038547            149
seed7/prg/rand.sd7           0.040069            121
seed7/prg/raytrace.sd7       0.044979            538
seed7/prg/rever.sd7          0.045898            816
seed7/prg/roman.sd7          0.039740             38
seed7/prg/s7c.sd7            0.040210           9060
seed7/prg/s7check.sd7        0.042154             68
seed7/prg/savehd7.sd7        0.042582           1110
seed7/prg/self.sd7           0.037328             49
seed7/prg/shisen.sd7         0.039906           1423
seed7/prg/sl.sd7             0.041114           1029
seed7/prg/snake.sd7          0.045005            615
seed7/prg/sokoban.sd7        0.046616            891
seed7/prg/spigotpi.sd7       0.039896             64
seed7/prg/sql7.sd7           0.039360            278
seed7/prg/startrek.sd7       0.037182            979
seed7/prg/sudoku7.sd7        0.040123           2657
seed7/prg/sydir7.sd7         0.039409            384
seed7/prg/syntaxhl.sd7       0.043729            177
seed7/prg/tak.sd7            0.037636             59
seed7/prg/tar7.sd7           0.038580            121
seed7/prg/tch.sd7            0.036191             55
seed7/prg/testfont.sd7       0.038270             95
seed7/prg/tet.sd7            0.038609            479
seed7/prg/tetg.sd7           0.046769            501
seed7/prg/toutf8.sd7         0.046775            240
seed7/prg/tst_cli.sd7        0.037456             40
seed7/prg/tst_srv.sd7        0.037281             47
seed7/prg/wator.sd7          0.037714            651
seed7/prg/which.sd7          0.036690             65
seed7/prg/wiz.sd7            0.040361           2833
seed7/prg/wordcnt.sd7        0.046913             54
seed7/prg/wrinum.sd7         0.044092             43
seed7/prg/wumpus.sd7         0.048046            372
seed7/lib/aes.s7i            0.044960           1144
seed7/lib/aes_gcm.s7i        0.043274            392
seed7/lib/ar.s7i             0.045073           1532
seed7/lib/arc4.s7i           0.042343            144
seed7/lib/archive.s7i        0.040173            143
seed7/lib/archive_base.s7i   0.039744            135
seed7/lib/array.s7i          0.039374            610
seed7/lib/asn1.s7i           0.037686            544
seed7/lib/asn1oid.s7i        0.041300            157
seed7/lib/basearray.s7i      0.038885            450
seed7/lib/bigfile.s7i        0.039304            136
seed7/lib/bigint.s7i         0.038702            824
seed7/lib/bigrat.s7i         0.041735            784
seed7/lib/bin16.s7i          0.041570            592
seed7/lib/bin32.s7i          0.042761            490
seed7/lib/bin64.s7i          0.039386            539
seed7/lib/bitdata.s7i        0.042295           1330
seed7/lib/bitmapfont.s7i     0.038814            215
seed7/lib/bitset.s7i         0.038260            593
seed7/lib/bitsetof.s7i       0.038152            431
seed7/lib/blowfish.s7i       0.040960            383
seed7/lib/bmp.s7i            0.043392            924
seed7/lib/boolean.s7i        0.040307            403
seed7/lib/browser.s7i        0.042201            280
seed7/lib/bstring.s7i        0.040463            227
seed7/lib/bytedata.s7i       0.039180            482
seed7/lib/bzip2.s7i          0.039718            887
seed7/lib/cards.s7i          0.037314           1342
seed7/lib/category.s7i       0.038068            209
seed7/lib/cc_conf.s7i        0.044731           1314
seed7/lib/ccittfax.s7i       0.043315           1022
seed7/lib/cgi.s7i            0.038790            109
seed7/lib/cgidialog.s7i      0.044242           1118
seed7/lib/char.s7i           0.042941            356
seed7/lib/charsets.s7i       0.041020           2024
seed7/lib/chartype.s7i       0.043300            121
seed7/lib/cipher.s7i         0.039377            146
seed7/lib/cli_cmds.s7i       0.040940           1360
seed7/lib/clib_file.s7i      0.040483            301
seed7/lib/color.s7i          0.039726            185
seed7/lib/complex.s7i        0.037519            464
seed7/lib/compress.s7i       0.038968            150
seed7/lib/console.s7i        0.043449            188
seed7/lib/cpio.s7i           0.043691           1708
seed7/lib/crc32.s7i          0.048360            193
seed7/lib/cronos16.s7i       0.040947           1173
seed7/lib/cronos27.s7i       0.039592           1464
seed7/lib/csv.s7i            0.038228            201
seed7/lib/db_prop.s7i        0.038987            991
seed7/lib/deflate.s7i        0.038037            740
seed7/lib/des.s7i            0.040848            444
seed7/lib/dialog.s7i         0.038716            311
seed7/lib/dir.s7i            0.038729            163
seed7/lib/draw.s7i           0.038658            854
seed7/lib/duration.s7i       0.037448           1038
seed7/lib/echo.s7i           0.037236            132
seed7/lib/editline.s7i       0.037154            398
seed7/lib/elf.s7i            0.040987           1560
seed7/lib/elliptic.s7i       0.042075            649
seed7/lib/enable_io.s7i      0.043595            312
seed7/lib/encoding.s7i       0.041398            931
seed7/lib/enumeration.s7i    0.040548            236
seed7/lib/environment.s7i    0.040948            175
seed7/lib/exif.s7i           0.043349            152
seed7/lib/external_file.s7i  0.040221            340
seed7/lib/field.s7i          0.038530            268
seed7/lib/file.s7i           0.040735            372
seed7/lib/filebits.s7i       0.036783             46
seed7/lib/filesys.s7i        0.037796            601
seed7/lib/fileutil.s7i       0.038289            144
seed7/lib/fixarray.s7i       0.039350            307
seed7/lib/float.s7i          0.039143            757
seed7/lib/font.s7i           0.039351            196
seed7/lib/font8x8.s7i        0.038505            998
seed7/lib/forloop.s7i        0.038971            449
seed7/lib/ftp.s7i            0.038715            969
seed7/lib/ftpserv.s7i        0.038084            631
seed7/lib/getf.s7i           0.039611            115
seed7/lib/gethttp.s7i        0.038531             41
seed7/lib/gethttps.s7i       0.038047             41
seed7/lib/gif.s7i            0.042464            561
seed7/lib/graph.s7i          0.042273            415
seed7/lib/graph_file.s7i     0.039791            399
seed7/lib/gtkserver.s7i      0.038961            161
seed7/lib/gzip.s7i           0.039041            573
seed7/lib/hash.s7i           0.043196            421
seed7/lib/hashsetof.s7i      0.041041            499
seed7/lib/hmac.s7i           0.039251            152
seed7/lib/html.s7i           0.037885             83
seed7/lib/html_ent.s7i       0.039427            476
seed7/lib/htmldom.s7i        0.038990            286
seed7/lib/http_request.s7i   0.039230            696
seed7/lib/http_srv_resp.s7i  0.038647            380
seed7/lib/https_request.s7i  0.040999            211
seed7/lib/httpserv.s7i       0.039247            345
seed7/lib/huffman.s7i        0.038982            644
seed7/lib/ico.s7i            0.038653            221
seed7/lib/idxarray.s7i       0.038948            232
seed7/lib/image.s7i          0.037864            156
seed7/lib/imagefile.s7i      0.038930            171
seed7/lib/inflate.s7i        0.038827            411
seed7/lib/inifile.s7i        0.038810            129
seed7/lib/integer.s7i        0.039030            663
seed7/lib/iobuffer.s7i       0.038609            289
seed7/lib/jpeg.s7i           0.041159           1761
seed7/lib/json.s7i           0.038894            891
seed7/lib/json_serde.s7i     0.039059            783
seed7/lib/keybd.s7i          0.037928            639
seed7/lib/keydescr.s7i       0.038694            192
seed7/lib/leb128.s7i         0.038469            218
seed7/lib/line.s7i           0.036824            164
seed7/lib/listener.s7i       0.037532            247
seed7/lib/logfile.s7i        0.037416             73
seed7/lib/lower.s7i          0.038799            142
seed7/lib/lzma.s7i           0.042361            934
seed7/lib/lzw.s7i            0.039497            861
seed7/lib/magic.s7i          0.039018            403
seed7/lib/mahjng32.s7i       0.038539           1500
seed7/lib/make.s7i           0.039115            544
seed7/lib/makedata.s7i       0.039274           1428
seed7/lib/math.s7i           0.039112            201
seed7/lib/mixarith.s7i       0.038983            249
seed7/lib/modern27.s7i       0.040621           1099
seed7/lib/more.s7i           0.038131            130
seed7/lib/msgdigest.s7i      0.041588           1222
seed7/lib/multiscr.s7i       0.037899             68
seed7/lib/null_file.s7i      0.038527            345
seed7/lib/osfiles.s7i        0.040385           1085
seed7/lib/pbm.s7i            0.038933            230
seed7/lib/pcx.s7i            0.039062            638
seed7/lib/pem.s7i            0.038504            185
seed7/lib/pgm.s7i            0.039578            238
seed7/lib/pic16.s7i          0.037282           1037
seed7/lib/pic32.s7i          0.038909           2060
seed7/lib/pic_util.s7i       0.038865            144
seed7/lib/pixelimage.s7i     0.038097            320
seed7/lib/pixmap_file.s7i    0.038068            459
seed7/lib/pixmapfont.s7i     0.037858            184
seed7/lib/pkcs1.s7i          0.043151            543
seed7/lib/png.s7i            0.039001           1064
seed7/lib/poll.s7i           0.038651            313
seed7/lib/ppm.s7i            0.039241            240
seed7/lib/process.s7i        0.039988            541
seed7/lib/progs.s7i          0.039383            789
seed7/lib/propertyfile.s7i   0.038653            155
seed7/lib/rational.s7i       0.038619            792
seed7/lib/ref_list.s7i       0.039194            252
seed7/lib/reference.s7i      0.038363            126
seed7/lib/reverse.s7i        0.037906             94
seed7/lib/rpm.s7i            0.039787           3487
seed7/lib/rpmext.s7i         0.038565            318
seed7/lib/scanfile.s7i       0.038283           1779
seed7/lib/scanjson.s7i       0.040312            413
seed7/lib/scanstri.s7i       0.040334           1814
seed7/lib/scantoml.s7i       0.039858           1603
seed7/lib/seed7_05.s7i       0.043672           1072
seed7/lib/set.s7i            0.038228             57
seed7/lib/shell.s7i          0.039467            615
seed7/lib/showtls.s7i        0.041200            678
seed7/lib/signature.s7i      0.039499            131
seed7/lib/smtp.s7i           0.040155            261
seed7/lib/sockbase.s7i       0.040726            217
seed7/lib/socket.s7i         0.038708            326
seed7/lib/sokoban1.s7i       0.036889           1519
seed7/lib/sql_base.s7i       0.039094           1000
seed7/lib/stars.s7i          0.038492           1705
seed7/lib/stdfont10.s7i      0.039212           3347
seed7/lib/stdfont12.s7i      0.037509           3928
seed7/lib/stdfont14.s7i      0.038562           4510
seed7/lib/stdfont16.s7i      0.039188           5092
seed7/lib/stdfont18.s7i      0.038574           5868
seed7/lib/stdfont20.s7i      0.038365           6449
seed7/lib/stdfont24.s7i      0.038796           7421
seed7/lib/stdfont8.s7i       0.037546           2960
seed7/lib/stdfont9.s7i       0.037603           3152
seed7/lib/stdio.s7i          0.040003            192
seed7/lib/strifile.s7i       0.039971            345
seed7/lib/string.s7i         0.040708            779
seed7/lib/stritext.s7i       0.041028            352
seed7/lib/struct.s7i         0.042067            266
seed7/lib/struct_elem.s7i    0.038516            129
seed7/lib/subfile.s7i        0.038471            174
seed7/lib/subrange.s7i       0.037414             78
seed7/lib/syntax.s7i         0.039449            294
seed7/lib/tar.s7i            0.043578           1880
seed7/lib/tar_cmds.s7i       0.038988            752
seed7/lib/tdes.s7i           0.040280            143
seed7/lib/tee.s7i            0.038771            143
seed7/lib/text.s7i           0.037836            135
seed7/lib/tga.s7i            0.039842            676
seed7/lib/tiff.s7i           0.038526           2771
seed7/lib/time.s7i           0.037663           1191
seed7/lib/tls.s7i            0.037787           2230
seed7/lib/unicode.s7i        0.039706            575
seed7/lib/unionfnd.s7i       0.036792            130
seed7/lib/upper.s7i          0.037061            142
seed7/lib/utf16.s7i          0.037886            540
seed7/lib/utf8.s7i           0.037783            234
seed7/lib/vecfont10.s7i      0.038734           1056
seed7/lib/vecfont18.s7i      0.039884           1119
seed7/lib/vector3d.s7i       0.038378            293
seed7/lib/vectorfont.s7i     0.038524            239
seed7/lib/wildcard.s7i       0.038670            140
seed7/lib/window.s7i         0.039080            455
seed7/lib/wrinum.s7i         0.040675            248
seed7/lib/x509cert.s7i       0.039681           1243
seed7/lib/xml_ent.s7i        0.038506             94
seed7/lib/xmldom.s7i         0.037147            303
seed7/lib/xz.s7i             0.040634            442
seed7/lib/zip.s7i            0.038791           2792
seed7/lib/zstd.s7i           0.039465           1263
============================ ================= ================

Statistical Summary — Mode B (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.039857        |
+-----------+-----------------+
| Minimum   | 0.034920        |
+-----------+-----------------+
| Maximum   | 0.048456        |
+-----------+-----------------+

Mode C — Mode Activation + Full-Buffer Fontification (font-lock-ensure)
-----------------------------------------------------------------------

Full-buffer fontification stress test.

File Load Times — Mode C (mean, GC-free)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

============================ ================= ================
File Name                    Mean Time (s)     Line count
============================ ================= ================
seed7/prg/addup.sd7          0.038999            190
seed7/prg/bas7.sd7           0.321413          11459
seed7/prg/bifurk.sd7         0.035316             73
seed7/prg/bigfiles.sd7       0.037754            129
seed7/prg/brainf7.sd7        0.037171             86
seed7/prg/calc7.sd7          0.037919            128
seed7/prg/carddemo.sd7       0.039270            190
seed7/prg/castle.sd7         0.107272           3148
seed7/prg/cat.sd7            0.036809             82
seed7/prg/cellauto.sd7       0.038913             85
seed7/prg/celsius.sd7        0.035936             42
seed7/prg/chk_all.sd7        0.058206            843
seed7/prg/chkarr.sd7         0.349531           8367
seed7/prg/chkbig.sd7         2.051240          29026
seed7/prg/chkbin.sd7         0.515162           6469
seed7/prg/chkbitdata.sd7     0.610977           6624
seed7/prg/chkbool.sd7        0.116065           3157
seed7/prg/chkbst.sd7         0.063002            722
seed7/prg/chkchr.sd7         0.229591           2809
seed7/prg/chkcmd.sd7         0.073440           1205
seed7/prg/chkdb.sd7          0.346644           7454
seed7/prg/chkdecl.sd7        0.062265            448
seed7/prg/chkenum.sd7        0.069345           1230
seed7/prg/chkerr.sd7         0.195841           4663
seed7/prg/chkexc.sd7         0.080299           2627
seed7/prg/chkfil.sd7         0.075236           1615
seed7/prg/chkflt.sd7         1.339872          20620
seed7/prg/chkhent.sd7        0.036485             54
seed7/prg/chkhsh.sd7         0.245091           4548
seed7/prg/chkidx.sd7         1.297783          19567
seed7/prg/chkint.sd7         2.488218          38129
seed7/prg/chkjson.sd7        0.099763           1764
seed7/prg/chkovf.sd7         0.553486           8216
seed7/prg/chkprc.sd7         0.313401          10111
seed7/prg/chkscan.sd7        0.055978            714
seed7/prg/chkset.sd7         0.650508          11974
seed7/prg/chkstr.sd7         1.686104          26952
seed7/prg/chktime.sd7        0.195035           2025
seed7/prg/chktoml.sd7        0.149317           1656
seed7/prg/clock.sd7          0.082980             47
seed7/prg/clock2.sd7         0.063590             43
seed7/prg/clock3.sd7         0.063801             95
seed7/prg/cmpfil.sd7         0.064934             84
seed7/prg/comanche.sd7       0.071372            180
seed7/prg/confval.sd7        0.070027            175
seed7/prg/db7.sd7            0.072481            417
seed7/prg/diff7.sd7          0.080326            263
seed7/prg/dirtst.sd7         0.058542             42
seed7/prg/dirx.sd7           0.068718            152
seed7/prg/dnafight.sd7       0.069792           1381
seed7/prg/dragon.sd7         0.036326             73
seed7/prg/echo.sd7           0.035253             39
seed7/prg/eliza.sd7          0.042298            302
seed7/prg/err.sd7            0.039871             96
seed7/prg/fannkuch.sd7       0.037053            131
seed7/prg/fib.sd7            0.035270             47
seed7/prg/find7.sd7          0.038299            133
seed7/prg/findchar.sd7       0.038403            149
seed7/prg/fractree.sd7       0.035831             55
seed7/prg/ftp7.sd7           0.042404            296
seed7/prg/ftpserv.sd7        0.037089             74
seed7/prg/gcd.sd7            0.036060            109
seed7/prg/gkbd.sd7           0.044713            358
seed7/prg/gtksvtst.sd7       0.035560             94
seed7/prg/hal.sd7            0.038258            250
seed7/prg/hamu.sd7           0.046794            573
seed7/prg/hanoi.sd7          0.036032             55
seed7/prg/hd.sd7             0.037142             79
seed7/prg/hello.sd7          0.035155             32
seed7/prg/hilbert.sd7        0.036979            108
seed7/prg/ide7.sd7           0.040599            196
seed7/prg/kbd.sd7            0.035968             49
seed7/prg/klondike.sd7       0.054785            883
seed7/prg/lander.sd7         0.071741           1551
seed7/prg/lst80bas.sd7       0.043751            344
seed7/prg/lst99bas.sd7       0.044927            401
seed7/prg/lstgwbas.sd7       0.051213            577
seed7/prg/mahjong.sd7        0.079532           1943
seed7/prg/make7.sd7          0.038197            121
seed7/prg/mandelbr.sd7       0.040102            237
seed7/prg/mind.sd7           0.043474            443
seed7/prg/mirror.sd7         0.040278            131
seed7/prg/ms.sd7             0.047442            641
seed7/prg/nicoma.sd7         0.037757            135
seed7/prg/pac.sd7            0.047742            726
seed7/prg/pairs.sd7          0.080611           2025
seed7/prg/panic.sd7          0.094793           2634
seed7/prg/percolation.sd7    0.043558            330
seed7/prg/planets.sd7        0.076777           1486
seed7/prg/portfwd7.sd7       0.038556            139
seed7/prg/prime.sd7          0.036339             74
seed7/prg/printpi1.sd7       0.037395             56
seed7/prg/printpi2.sd7       0.038859             54
seed7/prg/printpi3.sd7       0.037364             60
seed7/prg/pv7.sd7            0.044200            337
seed7/prg/queen.sd7          0.038531            149
seed7/prg/rand.sd7           0.037024            121
seed7/prg/raytrace.sd7       0.048346            538
seed7/prg/rever.sd7          0.054644            816
seed7/prg/roman.sd7          0.035768             38
seed7/prg/s7c.sd7            0.276417           9060
seed7/prg/s7check.sd7        0.036052             68
seed7/prg/savehd7.sd7        0.065490           1110
seed7/prg/self.sd7           0.035660             49
seed7/prg/shisen.sd7         0.069551           1423
seed7/prg/sl.sd7             0.059241           1029
seed7/prg/snake.sd7          0.046601            615
seed7/prg/sokoban.sd7        0.054561            891
seed7/prg/spigotpi.sd7       0.035950             64
seed7/prg/sql7.sd7           0.040280            278
seed7/prg/startrek.sd7       0.057656            979
seed7/prg/sudoku7.sd7        0.097671           2657
seed7/prg/sydir7.sd7         0.044181            384
seed7/prg/syntaxhl.sd7       0.040240            177
seed7/prg/tak.sd7            0.035780             59
seed7/prg/tar7.sd7           0.038636            121
seed7/prg/tch.sd7            0.036289             55
seed7/prg/testfont.sd7       0.037522             95
seed7/prg/tet.sd7            0.044585            479
seed7/prg/tetg.sd7           0.045576            501
seed7/prg/toutf8.sd7         0.042869            240
seed7/prg/tst_cli.sd7        0.035240             40
seed7/prg/tst_srv.sd7        0.036160             47
seed7/prg/wator.sd7          0.051066            651
seed7/prg/which.sd7          0.035462             65
seed7/prg/wiz.sd7            0.104857           2833
seed7/prg/wordcnt.sd7        0.035860             54
seed7/prg/wrinum.sd7         0.036907             43
seed7/prg/wumpus.sd7         0.047048            372
seed7/lib/aes.s7i            0.110929           1144
seed7/lib/aes_gcm.s7i        0.046629            392
seed7/lib/ar.s7i             0.079025           1532
seed7/lib/arc4.s7i           0.040192            144
seed7/lib/archive.s7i        0.038112            143
seed7/lib/archive_base.s7i   0.038143            135
seed7/lib/array.s7i          0.053688            610
seed7/lib/asn1.s7i           0.047475            544
seed7/lib/asn1oid.s7i        0.041964            157
seed7/lib/basearray.s7i      0.048821            450
seed7/lib/bigfile.s7i        0.038364            136
seed7/lib/bigint.s7i         0.055357            824
seed7/lib/bigrat.s7i         0.053578            784
seed7/lib/bin16.s7i          0.050678            592
seed7/lib/bin32.s7i          0.047921            490
seed7/lib/bin64.s7i          0.049056            539
seed7/lib/bitdata.s7i        0.074786           1330
seed7/lib/bitmapfont.s7i     0.039125            215
seed7/lib/bitset.s7i         0.049027            593
seed7/lib/bitsetof.s7i       0.047591            431
seed7/lib/blowfish.s7i       0.054613            383
seed7/lib/bmp.s7i            0.060381            924
seed7/lib/boolean.s7i        0.043300            403
seed7/lib/browser.s7i        0.041350            280
seed7/lib/bstring.s7i        0.040065            227
seed7/lib/bytedata.s7i       0.049100            482
seed7/lib/bzip2.s7i          0.057860            887
seed7/lib/cards.s7i          0.066279           1342
seed7/lib/category.s7i       0.041348            209
seed7/lib/cc_conf.s7i        0.077186           1314
seed7/lib/ccittfax.s7i       0.066326           1022
seed7/lib/cgi.s7i            0.037793            109
seed7/lib/cgidialog.s7i      0.060212           1118
seed7/lib/char.s7i           0.043219            356
seed7/lib/charsets.s7i       0.080344           2024
seed7/lib/chartype.s7i       0.039481            121
seed7/lib/cipher.s7i         0.037112            146
seed7/lib/cli_cmds.s7i       0.068457           1360
seed7/lib/clib_file.s7i      0.044500            301
seed7/lib/color.s7i          0.040448            185
seed7/lib/complex.s7i        0.045141            464
seed7/lib/compress.s7i       0.038953            150
seed7/lib/console.s7i        0.039635            188
seed7/lib/cpio.s7i           0.080834           1708
seed7/lib/crc32.s7i          0.044598            193
seed7/lib/cronos16.s7i       0.092907           1173
seed7/lib/cronos27.s7i       0.115630           1464
seed7/lib/csv.s7i            0.040671            201
seed7/lib/db_prop.s7i        0.062759            991
seed7/lib/deflate.s7i        0.055787            740
seed7/lib/des.s7i            0.055567            444
seed7/lib/dialog.s7i         0.044189            311
seed7/lib/dir.s7i            0.038247            163
seed7/lib/draw.s7i           0.054903            854
seed7/lib/duration.s7i       0.059485           1038
seed7/lib/echo.s7i           0.037520            132
seed7/lib/editline.s7i       0.044435            398
seed7/lib/elf.s7i            0.085209           1560
seed7/lib/elliptic.s7i       0.052794            649
seed7/lib/enable_io.s7i      0.045961            312
seed7/lib/encoding.s7i       0.063662            931
seed7/lib/enumeration.s7i    0.042090            236
seed7/lib/environment.s7i    0.039381            175
seed7/lib/exif.s7i           0.039402            152
seed7/lib/external_file.s7i  0.046221            340
seed7/lib/field.s7i          0.046528            268
seed7/lib/file.s7i           0.044525            372
seed7/lib/filebits.s7i       0.041165             46
seed7/lib/filesys.s7i        0.048511            601
seed7/lib/fileutil.s7i       0.037213            144
seed7/lib/fixarray.s7i       0.042307            307
seed7/lib/float.s7i          0.054946            757
seed7/lib/font.s7i           0.042590            196
seed7/lib/font8x8.s7i        0.049857            998
seed7/lib/forloop.s7i        0.046427            449
seed7/lib/ftp.s7i            0.060252            969
seed7/lib/ftpserv.s7i        0.051590            631
seed7/lib/getf.s7i           0.037427            115
seed7/lib/gethttp.s7i        0.035265             41
seed7/lib/gethttps.s7i       0.037631             41
seed7/lib/gif.s7i            0.049555            561
seed7/lib/graph.s7i          0.048951            415
seed7/lib/graph_file.s7i     0.044474            399
seed7/lib/gtkserver.s7i      0.038695            161
seed7/lib/gzip.s7i           0.052315            573
seed7/lib/hash.s7i           0.054756            421
seed7/lib/hashsetof.s7i      0.050858            499
seed7/lib/hmac.s7i           0.044686            152
seed7/lib/html.s7i           0.037058             83
seed7/lib/html_ent.s7i       0.047048            476
seed7/lib/htmldom.s7i        0.043100            286
seed7/lib/http_request.s7i   0.051279            696
seed7/lib/http_srv_resp.s7i  0.045652            380
seed7/lib/https_request.s7i  0.040324            211
seed7/lib/httpserv.s7i       0.044113            345
seed7/lib/huffman.s7i        0.053011            644
seed7/lib/ico.s7i            0.041681            221
seed7/lib/idxarray.s7i       0.042455            232
seed7/lib/image.s7i          0.036644            156
seed7/lib/imagefile.s7i      0.038493            171
seed7/lib/inflate.s7i        0.045883            411
seed7/lib/inifile.s7i        0.036830            129
seed7/lib/integer.s7i        0.052540            663
seed7/lib/iobuffer.s7i       0.042727            289
seed7/lib/jpeg.s7i           0.084016           1761
seed7/lib/json.s7i           0.055828            891
seed7/lib/json_serde.s7i     0.053155            783
seed7/lib/keybd.s7i          0.055260            639
seed7/lib/keydescr.s7i       0.041385            192
seed7/lib/leb128.s7i         0.040003            218
seed7/lib/line.s7i           0.037987            164
seed7/lib/listener.s7i       0.040896            247
seed7/lib/logfile.s7i        0.036577             73
seed7/lib/lower.s7i          0.038356            142
seed7/lib/lzma.s7i           0.059714            934
seed7/lib/lzw.s7i            0.059117            861
seed7/lib/magic.s7i          0.048118            403
seed7/lib/mahjng32.s7i       0.065437           1500
seed7/lib/make.s7i           0.049400            544
seed7/lib/makedata.s7i       0.069295           1428
seed7/lib/math.s7i           0.043108            201
seed7/lib/mixarith.s7i       0.039412            249
seed7/lib/modern27.s7i       0.082820           1099
seed7/lib/more.s7i           0.038225            130
seed7/lib/msgdigest.s7i      0.083962           1222
seed7/lib/multiscr.s7i       0.040582             68
seed7/lib/null_file.s7i      0.041015            345
seed7/lib/osfiles.s7i        0.063503           1085
seed7/lib/pbm.s7i            0.041450            230
seed7/lib/pcx.s7i            0.053857            638
seed7/lib/pem.s7i            0.039643            185
seed7/lib/pgm.s7i            0.041349            238
seed7/lib/pic16.s7i          0.058236           1037
seed7/lib/pic32.s7i          0.091860           2060
seed7/lib/pic_util.s7i       0.043155            144
seed7/lib/pixelimage.s7i     0.044579            320
seed7/lib/pixmap_file.s7i    0.046926            459
seed7/lib/pixmapfont.s7i     0.039793            184
seed7/lib/pkcs1.s7i          0.060956            543
seed7/lib/png.s7i            0.065812           1064
seed7/lib/poll.s7i           0.048524            313
seed7/lib/ppm.s7i            0.042282            240
seed7/lib/process.s7i        0.049701            541
seed7/lib/progs.s7i          0.057181            789
seed7/lib/propertyfile.s7i   0.041873            155
seed7/lib/rational.s7i       0.053520            792
seed7/lib/ref_list.s7i       0.042119            252
seed7/lib/reference.s7i      0.037818            126
seed7/lib/reverse.s7i        0.042229             94
seed7/lib/rpm.s7i            0.142203           3487
seed7/lib/rpmext.s7i         0.042807            318
seed7/lib/scanfile.s7i       0.079420           1779
seed7/lib/scanjson.s7i       0.046817            413
seed7/lib/scanstri.s7i       0.079622           1814
seed7/lib/scantoml.s7i       0.070934           1603
seed7/lib/seed7_05.s7i       0.066070           1072
seed7/lib/set.s7i            0.035747             57
seed7/lib/shell.s7i          0.051466            615
seed7/lib/showtls.s7i        0.053336            678
seed7/lib/signature.s7i      0.037287            131
seed7/lib/smtp.s7i           0.039987            261
seed7/lib/sockbase.s7i       0.043024            217
seed7/lib/socket.s7i         0.045045            326
seed7/lib/sokoban1.s7i       0.055407           1519
seed7/lib/sql_base.s7i       0.064130           1000
seed7/lib/stars.s7i          0.133787           1705
seed7/lib/stdfont10.s7i      0.080749           3347
seed7/lib/stdfont12.s7i      0.090935           3928
seed7/lib/stdfont14.s7i      0.102267           4510
seed7/lib/stdfont16.s7i      0.115979           5092
seed7/lib/stdfont18.s7i      0.131071           5868
seed7/lib/stdfont20.s7i      0.147057           6449
seed7/lib/stdfont24.s7i      0.175359           7421
seed7/lib/stdfont8.s7i       0.071708           2960
seed7/lib/stdfont9.s7i       0.075333           3152
seed7/lib/stdio.s7i          0.038501            192
seed7/lib/strifile.s7i       0.042673            345
seed7/lib/string.s7i         0.054042            779
seed7/lib/stritext.s7i       0.042522            352
seed7/lib/struct.s7i         0.046330            266
seed7/lib/struct_elem.s7i    0.042090            129
seed7/lib/subfile.s7i        0.043782            174
seed7/lib/subrange.s7i       0.041096             78
seed7/lib/syntax.s7i         0.051763            294
seed7/lib/tar.s7i            0.090573           1880
seed7/lib/tar_cmds.s7i       0.060450            752
seed7/lib/tdes.s7i           0.039397            143
seed7/lib/tee.s7i            0.037778            143
seed7/lib/text.s7i           0.037727            135
seed7/lib/tga.s7i            0.052460            676
seed7/lib/tiff.s7i           0.121820           2771
seed7/lib/time.s7i           0.063515           1191
seed7/lib/tls.s7i            0.110476           2230
seed7/lib/unicode.s7i        0.055329            575
seed7/lib/unionfnd.s7i       0.043899            130
seed7/lib/upper.s7i          0.048857            142
seed7/lib/utf16.s7i          0.055501            540
seed7/lib/utf8.s7i           0.047357            234
seed7/lib/vecfont10.s7i      0.084116           1056
seed7/lib/vecfont18.s7i      0.092679           1119
seed7/lib/vector3d.s7i       0.045644            293
seed7/lib/vectorfont.s7i     0.043825            239
seed7/lib/wildcard.s7i       0.040037            140
seed7/lib/window.s7i         0.047691            455
seed7/lib/wrinum.s7i         0.040976            248
seed7/lib/x509cert.s7i       0.069880           1243
seed7/lib/xml_ent.s7i        0.036620             94
seed7/lib/xmldom.s7i         0.040008            303
seed7/lib/xz.s7i             0.045234            442
seed7/lib/zip.s7i            0.126749           2792
seed7/lib/zstd.s7i           0.072699           1263
============================ ================= ================

Statistical Summary — Mode C (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.091343        |
+-----------+-----------------+
| Minimum   | 0.035155        |
+-----------+-----------------+
| Maximum   | 2.488218        |
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
seed7/prg/addup.sd7          0.054674            190
seed7/prg/bas7.sd7           0.771152          11459
seed7/prg/bifurk.sd7         0.046046             73
seed7/prg/bigfiles.sd7       0.044948            129
seed7/prg/brainf7.sd7        0.041694             86
seed7/prg/calc7.sd7          0.044447            128
seed7/prg/carddemo.sd7       0.050845            190
seed7/prg/castle.sd7         0.223607           3148
seed7/prg/cat.sd7            0.045036             82
seed7/prg/cellauto.sd7       0.047860             85
seed7/prg/celsius.sd7        0.043438             42
seed7/prg/chk_all.sd7        0.084570            843
seed7/prg/chkarr.sd7         0.863140           8367
seed7/prg/chkbig.sd7         4.111285          29026
seed7/prg/chkbin.sd7         1.077292           6469
seed7/prg/chkbitdata.sd7     1.320369           6624
seed7/prg/chkbool.sd7        0.246169           3157
seed7/prg/chkbst.sd7         0.106771            722
seed7/prg/chkchr.sd7         0.511049           2809
seed7/prg/chkcmd.sd7         0.122366           1205
seed7/prg/chkdb.sd7          0.748435           7454
seed7/prg/chkdecl.sd7        0.094481            448
seed7/prg/chkenum.sd7        0.121293           1230
seed7/prg/chkerr.sd7         0.334644           4663
seed7/prg/chkexc.sd7         0.145321           2627
seed7/prg/chkfil.sd7         0.126770           1615
seed7/prg/chkflt.sd7         2.792998          20620
seed7/prg/chkhent.sd7        0.037206             54
seed7/prg/chkhsh.sd7         0.502534           4548
seed7/prg/chkidx.sd7         3.144744          19567
seed7/prg/chkint.sd7         5.501658          38129
seed7/prg/chkjson.sd7        0.186151           1764
seed7/prg/chkovf.sd7         1.193264           8216
seed7/prg/chkprc.sd7         0.694862          10111
seed7/prg/chkscan.sd7        0.089474            714
seed7/prg/chkset.sd7         1.734940          11974
seed7/prg/chkstr.sd7         3.382754          26952
seed7/prg/chktime.sd7        0.245544           2025
seed7/prg/chktoml.sd7        0.192865           1656
seed7/prg/clock.sd7          0.035975             47
seed7/prg/clock2.sd7         0.035495             43
seed7/prg/clock3.sd7         0.040783             95
seed7/prg/cmpfil.sd7         0.039654             84
seed7/prg/comanche.sd7       0.046883            180
seed7/prg/confval.sd7        0.051099            175
seed7/prg/db7.sd7            0.061851            417
seed7/prg/diff7.sd7          0.050519            263
seed7/prg/dirtst.sd7         0.035801             42
seed7/prg/dirx.sd7           0.041331            152
seed7/prg/dnafight.sd7       0.116419           1381
seed7/prg/dragon.sd7         0.038118             73
seed7/prg/echo.sd7           0.036187             39
seed7/prg/eliza.sd7          0.051485            302
seed7/prg/err.sd7            0.044208             96
seed7/prg/fannkuch.sd7       0.041430            131
seed7/prg/fib.sd7            0.036566             47
seed7/prg/find7.sd7          0.041888            133
seed7/prg/findchar.sd7       0.043540            149
seed7/prg/fractree.sd7       0.037118             55
seed7/prg/ftp7.sd7           0.051171            296
seed7/prg/ftpserv.sd7        0.037540             74
seed7/prg/gcd.sd7            0.039124            109
seed7/prg/gkbd.sd7           0.059843            358
seed7/prg/gtksvtst.sd7       0.040431             94
seed7/prg/hal.sd7            0.047383            250
seed7/prg/hamu.sd7           0.066876            573
seed7/prg/hanoi.sd7          0.037438             55
seed7/prg/hd.sd7             0.038469             79
seed7/prg/hello.sd7          0.036209             32
seed7/prg/hilbert.sd7        0.040896            108
seed7/prg/ide7.sd7           0.047670            196
seed7/prg/kbd.sd7            0.036700             49
seed7/prg/klondike.sd7       0.086238            883
seed7/prg/lander.sd7         0.130539           1551
seed7/prg/lst80bas.sd7       0.054778            344
seed7/prg/lst99bas.sd7       0.057539            401
seed7/prg/lstgwbas.sd7       0.071257            577
seed7/prg/mahjong.sd7        0.145116           1943
seed7/prg/make7.sd7          0.040841            121
seed7/prg/mandelbr.sd7       0.046961            237
seed7/prg/mind.sd7           0.059210            443
seed7/prg/mirror.sd7         0.043856            131
seed7/prg/ms.sd7             0.068693            641
seed7/prg/nicoma.sd7         0.041902            135
seed7/prg/pac.sd7            0.071876            726
seed7/prg/pairs.sd7          0.139138           2025
seed7/prg/panic.sd7          0.194876           2634
seed7/prg/percolation.sd7    0.056163            330
seed7/prg/planets.sd7        0.135284           1486
seed7/prg/portfwd7.sd7       0.041526            139
seed7/prg/prime.sd7          0.038019             74
seed7/prg/printpi1.sd7       0.036253             56
seed7/prg/printpi2.sd7       0.036362             54
seed7/prg/printpi3.sd7       0.037229             60
seed7/prg/pv7.sd7            0.058111            337
seed7/prg/queen.sd7          0.042922            149
seed7/prg/rand.sd7           0.041101            121
seed7/prg/raytrace.sd7       0.067920            538
seed7/prg/rever.sd7          0.080943            816
seed7/prg/roman.sd7          0.036217             38
seed7/prg/s7c.sd7            0.615778           9060
seed7/prg/s7check.sd7        0.037945             68
seed7/prg/savehd7.sd7        0.109018           1110
seed7/prg/self.sd7           0.038160             49
seed7/prg/shisen.sd7         0.117799           1423
seed7/prg/sl.sd7             0.096958           1029
seed7/prg/snake.sd7          0.065703            615
seed7/prg/sokoban.sd7        0.082197            891
seed7/prg/spigotpi.sd7       0.037815             64
seed7/prg/sql7.sd7           0.051683            278
seed7/prg/startrek.sd7       0.095368            979
seed7/prg/sudoku7.sd7        0.194684           2657
seed7/prg/sydir7.sd7         0.058257            384
seed7/prg/syntaxhl.sd7       0.047901            177
seed7/prg/tak.sd7            0.036287             59
seed7/prg/tar7.sd7           0.040901            121
seed7/prg/tch.sd7            0.037780             55
seed7/prg/testfont.sd7       0.040301             95
seed7/prg/tet.sd7            0.057438            479
seed7/prg/tetg.sd7           0.060191            501
seed7/prg/toutf8.sd7         0.049741            240
seed7/prg/tst_cli.sd7        0.037223             40
seed7/prg/tst_srv.sd7        0.036772             47
seed7/prg/wator.sd7          0.077812            651
seed7/prg/which.sd7          0.038056             65
seed7/prg/wiz.sd7            0.206772           2833
seed7/prg/wordcnt.sd7        0.037668             54
seed7/prg/wrinum.sd7         0.036486             43
seed7/prg/wumpus.sd7         0.052913            372
seed7/lib/aes.s7i            0.200207           1144
seed7/lib/aes_gcm.s7i        0.059120            392
seed7/lib/ar.s7i             0.122547           1532
seed7/lib/arc4.s7i           0.042764            144
seed7/lib/archive.s7i        0.042571            143
seed7/lib/archive_base.s7i   0.043301            135
seed7/lib/array.s7i          0.074912            610
seed7/lib/asn1.s7i           0.064395            544
seed7/lib/asn1oid.s7i        0.049386            157
seed7/lib/basearray.s7i      0.062721            450
seed7/lib/bigfile.s7i        0.040275            136
seed7/lib/bigint.s7i         0.079169            824
seed7/lib/bigrat.s7i         0.078395            784
seed7/lib/bin16.s7i          0.066534            592
seed7/lib/bin32.s7i          0.059732            490
seed7/lib/bin64.s7i          0.061565            539
seed7/lib/bitdata.s7i        0.120772           1330
seed7/lib/bitmapfont.s7i     0.045259            215
seed7/lib/bitset.s7i         0.062140            593
seed7/lib/bitsetof.s7i       0.062400            431
seed7/lib/blowfish.s7i       0.076942            383
seed7/lib/bmp.s7i            0.098533            924
seed7/lib/boolean.s7i        0.054172            403
seed7/lib/browser.s7i        0.055974            280
seed7/lib/bstring.s7i        0.048724            227
seed7/lib/bytedata.s7i       0.065268            482
seed7/lib/bzip2.s7i          0.090478            887
seed7/lib/cards.s7i          0.101763           1342
seed7/lib/category.s7i       0.048289            209
seed7/lib/cc_conf.s7i        0.118342           1314
seed7/lib/ccittfax.s7i       0.098301           1022
seed7/lib/cgi.s7i            0.040154            109
seed7/lib/cgidialog.s7i      0.094835           1118
seed7/lib/char.s7i           0.051176            356
seed7/lib/charsets.s7i       0.125613           2024
seed7/lib/chartype.s7i       0.047318            121
seed7/lib/cipher.s7i         0.040798            146
seed7/lib/cli_cmds.s7i       0.111383           1360
seed7/lib/clib_file.s7i      0.051292            301
seed7/lib/color.s7i          0.048009            185
seed7/lib/complex.s7i        0.058403            464
seed7/lib/compress.s7i       0.043109            150
seed7/lib/console.s7i        0.045010            188
seed7/lib/cpio.s7i           0.143635           1708
seed7/lib/crc32.s7i          0.053196            193
seed7/lib/cronos16.s7i       0.190615           1173
seed7/lib/cronos27.s7i       0.252658           1464
seed7/lib/csv.s7i            0.048093            201
seed7/lib/db_prop.s7i        0.101614            991
seed7/lib/deflate.s7i        0.085877            740
seed7/lib/des.s7i            0.078687            444
seed7/lib/dialog.s7i         0.055957            311
seed7/lib/dir.s7i            0.041634            163
seed7/lib/draw.s7i           0.083811            854
seed7/lib/duration.s7i       0.096610           1038
seed7/lib/echo.s7i           0.041246            132
seed7/lib/editline.s7i       0.057513            398
seed7/lib/elf.s7i            0.154945           1560
seed7/lib/elliptic.s7i       0.077437            649
seed7/lib/enable_io.s7i      0.052137            312
seed7/lib/encoding.s7i       0.095921            931
seed7/lib/enumeration.s7i    0.048455            236
seed7/lib/environment.s7i    0.043719            175
seed7/lib/exif.s7i           0.045746            152
seed7/lib/external_file.s7i  0.052416            340
seed7/lib/field.s7i          0.052659            268
seed7/lib/file.s7i           0.053586            372
seed7/lib/filebits.s7i       0.037698             46
seed7/lib/filesys.s7i        0.063753            601
seed7/lib/fileutil.s7i       0.043076            144
seed7/lib/fixarray.s7i       0.052959            307
seed7/lib/float.s7i          0.073428            757
seed7/lib/font.s7i           0.043896            196
seed7/lib/font8x8.s7i        0.065614            998
seed7/lib/forloop.s7i        0.057697            449
seed7/lib/ftp.s7i            0.085878            969
seed7/lib/ftpserv.s7i        0.075318            631
seed7/lib/getf.s7i           0.039730            115
seed7/lib/gethttp.s7i        0.035936             41
seed7/lib/gethttps.s7i       0.035803             41
seed7/lib/gif.s7i            0.068292            561
seed7/lib/graph.s7i          0.062908            415
seed7/lib/graph_file.s7i     0.055513            399
seed7/lib/gtkserver.s7i      0.040326            161
seed7/lib/gzip.s7i           0.067178            573
seed7/lib/hash.s7i           0.065740            421
seed7/lib/hashsetof.s7i      0.065512            499
seed7/lib/hmac.s7i           0.044076            152
seed7/lib/html.s7i           0.039392             83
seed7/lib/html_ent.s7i       0.062341            476
seed7/lib/htmldom.s7i        0.051449            286
seed7/lib/http_request.s7i   0.075119            696
seed7/lib/http_srv_resp.s7i  0.057255            380
seed7/lib/https_request.s7i  0.045931            211
seed7/lib/httpserv.s7i       0.055066            345
seed7/lib/huffman.s7i        0.074019            644
seed7/lib/ico.s7i            0.049041            221
seed7/lib/idxarray.s7i       0.050650            232
seed7/lib/image.s7i          0.041412            156
seed7/lib/imagefile.s7i      0.045707            171
seed7/lib/inflate.s7i        0.063614            411
seed7/lib/inifile.s7i        0.041897            129
seed7/lib/integer.s7i        0.067740            663
seed7/lib/iobuffer.s7i       0.050547            289
seed7/lib/jpeg.s7i           0.152310           1761
seed7/lib/json.s7i           0.079122            891
seed7/lib/json_serde.s7i     0.076460            783
seed7/lib/keybd.s7i          0.078092            639
seed7/lib/keydescr.s7i       0.049371            192
seed7/lib/leb128.s7i         0.045043            218
seed7/lib/line.s7i           0.050565            164
seed7/lib/listener.s7i       0.050312            247
seed7/lib/logfile.s7i        0.040035             73
seed7/lib/lower.s7i          0.041860            142
seed7/lib/lzma.s7i           0.095794            934
seed7/lib/lzw.s7i            0.087937            861
seed7/lib/magic.s7i          0.063020            403
seed7/lib/mahjng32.s7i       0.091460           1500
seed7/lib/make.s7i           0.069472            544
seed7/lib/makedata.s7i       0.120252           1428
seed7/lib/math.s7i           0.044728            201
seed7/lib/mixarith.s7i       0.047051            249
seed7/lib/modern27.s7i       0.166813           1099
seed7/lib/more.s7i           0.043035            130
seed7/lib/msgdigest.s7i      0.139411           1222
seed7/lib/multiscr.s7i       0.038738             68
seed7/lib/null_file.s7i      0.051708            345
seed7/lib/osfiles.s7i        0.095552           1085
seed7/lib/pbm.s7i            0.046444            230
seed7/lib/pcx.s7i            0.075900            638
seed7/lib/pem.s7i            0.044324            185
seed7/lib/pgm.s7i            0.047752            238
seed7/lib/pic16.s7i          0.065553           1037
seed7/lib/pic32.s7i          0.119730           2060
seed7/lib/pic_util.s7i       0.043184            144
seed7/lib/pixelimage.s7i     0.052165            320
seed7/lib/pixmap_file.s7i    0.060826            459
seed7/lib/pixmapfont.s7i     0.047823            184
seed7/lib/pkcs1.s7i          0.076254            543
seed7/lib/png.s7i            0.106137           1064
seed7/lib/poll.s7i           0.050723            313
seed7/lib/ppm.s7i            0.047856            240
seed7/lib/process.s7i        0.063959            541
seed7/lib/progs.s7i          0.078305            789
seed7/lib/propertyfile.s7i   0.043649            155
seed7/lib/rational.s7i       0.078187            792
seed7/lib/ref_list.s7i       0.049003            252
seed7/lib/reference.s7i      0.041842            126
seed7/lib/reverse.s7i        0.040020             94
seed7/lib/rpm.s7i            0.283106           3487
seed7/lib/rpmext.s7i         0.056112            318
seed7/lib/scanfile.s7i       0.134620           1779
seed7/lib/scanjson.s7i       0.059752            413
seed7/lib/scanstri.s7i       0.137130           1814
seed7/lib/scantoml.s7i       0.130133           1603
seed7/lib/seed7_05.s7i       0.111397           1072
seed7/lib/set.s7i            0.037816             57
seed7/lib/shell.s7i          0.068534            615
seed7/lib/showtls.s7i        0.084440            678
seed7/lib/signature.s7i      0.042663            131
seed7/lib/smtp.s7i           0.049460            261
seed7/lib/sockbase.s7i       0.050137            217
seed7/lib/socket.s7i         0.052812            326
seed7/lib/sokoban1.s7i       0.080320           1519
seed7/lib/sql_base.s7i       0.094058           1000
seed7/lib/stars.s7i          0.232607           1705
seed7/lib/stdfont10.s7i      0.142922           3347
seed7/lib/stdfont12.s7i      0.163757           3928
seed7/lib/stdfont14.s7i      0.187813           4510
seed7/lib/stdfont16.s7i      0.211224           5092
seed7/lib/stdfont18.s7i      0.242302           5868
seed7/lib/stdfont20.s7i      0.270682           6449
seed7/lib/stdfont24.s7i      0.326395           7421
seed7/lib/stdfont8.s7i       0.124936           2960
seed7/lib/stdfont9.s7i       0.131249           3152
seed7/lib/stdio.s7i          0.042704            192
seed7/lib/strifile.s7i       0.052637            345
seed7/lib/string.s7i         0.073714            779
seed7/lib/stritext.s7i       0.053716            352
seed7/lib/struct.s7i         0.054300            266
seed7/lib/struct_elem.s7i    0.040955            129
seed7/lib/subfile.s7i        0.044301            174
seed7/lib/subrange.s7i       0.038786             78
seed7/lib/syntax.s7i         0.059386            294
seed7/lib/tar.s7i            0.148219           1880
seed7/lib/tar_cmds.s7i       0.084377            752
seed7/lib/tdes.s7i           0.043431            143
seed7/lib/tee.s7i            0.041989            143
seed7/lib/text.s7i           0.042177            135
seed7/lib/tga.s7i            0.079409            676
seed7/lib/tiff.s7i           0.241703           2771
seed7/lib/time.s7i           0.099443           1191
seed7/lib/tls.s7i            0.192275           2230
seed7/lib/unicode.s7i        0.074339            575
seed7/lib/unionfnd.s7i       0.042285            130
seed7/lib/upper.s7i          0.040699            142
seed7/lib/utf16.s7i          0.064206            540
seed7/lib/utf8.s7i           0.045911            234
seed7/lib/vecfont10.s7i      0.161657           1056
seed7/lib/vecfont18.s7i      0.174390           1119
seed7/lib/vector3d.s7i       0.049289            293
seed7/lib/vectorfont.s7i     0.047734            239
seed7/lib/wildcard.s7i       0.042635            140
seed7/lib/window.s7i         0.059966            455
seed7/lib/wrinum.s7i         0.048457            248
seed7/lib/x509cert.s7i       0.115982           1243
seed7/lib/xml_ent.s7i        0.039816             94
seed7/lib/xmldom.s7i         0.051142            303
seed7/lib/xz.s7i             0.060483            442
seed7/lib/zip.s7i            0.230054           2792
seed7/lib/zstd.s7i           0.117273           1263
============================ ================= ================

Statistical Summary — Mode D (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.157956        |
+-----------+-----------------+
| Minimum   | 0.035495        |
+-----------+-----------------+
| Maximum   | 5.501658        |
+-----------+-----------------+

Cross-Mode Comparison
=====================

+------+-----------------+-----------------+-----------------+
| Mode | Average (s)     | Minimum (s)     | Maximum (s)     |
+======+=================+=================+=================+
| A    | 0.035323        | 0.032291        | 0.043373        |
+------+-----------------+-----------------+-----------------+
| B    | 0.039857        | 0.034920        | 0.048456        |
+------+-----------------+-----------------+-----------------+
| C    | 0.091343        | 0.035155        | 2.488218        |
+------+-----------------+-----------------+-----------------+
| D    | 0.157956        | 0.035495        | 5.501658        |
+------+-----------------+-----------------+-----------------+

Phase Timing Summary
====================

Wall-clock times include warm-up work and GC performed by the warm-up helper.

+----------+--------------+--------------+--------------+
| Phase    | Warm-up      | Timed pass   | Mode total   |
+==========+==============+==============+==============+
| Mode A   | 00:00:12.809 | 00:01:00.177 | 00:01:12.986 |
+----------+--------------+--------------+--------------+
| Mode B   | 00:00:15.460 | 00:01:07.952 | 00:01:23.413 |
+----------+--------------+--------------+--------------+
| Mode C   | 00:00:36.353 | 00:02:36.464 | 00:03:12.817 |
+----------+--------------+--------------+--------------+
| Mode D   | 00:01:02.083 | 00:04:29.932 | 00:05:32.016 |
+----------+--------------+--------------+--------------+
| Total    |              |              | 00:11:21.240 |
+----------+--------------+--------------+--------------+


.. ---------------------------------------------------------------------------
