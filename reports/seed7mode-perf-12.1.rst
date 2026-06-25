=======================================================
GC-Controlled Benchmark Report: Seed7 Mode — Four Modes
=======================================================

:Running with: seed7-mode 2026-06-25T19:49:14+0000 W26-4
:Seed7 dir   : /Users/roup/my/dvo/seed7-repos/seed7/
:Started at: 15:57:59 local time
:Generated on: 2026-06-25 20:09:27 UTC
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
seed7/prg/.#bas7.sd7         0.018352              1
seed7/prg/addup.sd7          0.032678            190
seed7/prg/bas7.sd7           0.034712          11459
seed7/prg/bifurk.sd7         0.032618             73
seed7/prg/bigfiles.sd7       0.034745            129
seed7/prg/brainf7.sd7        0.033476             86
seed7/prg/calc7.sd7          0.034240            128
seed7/prg/carddemo.sd7       0.033582            190
seed7/prg/castle.sd7         0.033903           3148
seed7/prg/cat.sd7            0.033300             82
seed7/prg/cellauto.sd7       0.033383             85
seed7/prg/celsius.sd7        0.033228             42
seed7/prg/chk_all.sd7        0.033159            843
seed7/prg/chkarr.sd7         0.033265           8367
seed7/prg/chkbig.sd7         0.037074          29026
seed7/prg/chkbin.sd7         0.033688           6469
seed7/prg/chkbitdata.sd7     0.033438           6624
seed7/prg/chkbool.sd7        0.032785           3157
seed7/prg/chkbst.sd7         0.033837            722
seed7/prg/chkchr.sd7         0.034319           2809
seed7/prg/chkcmd.sd7         0.032722           1205
seed7/prg/chkdb.sd7          0.034048           7454
seed7/prg/chkdecl.sd7        0.033459            448
seed7/prg/chkenum.sd7        0.033730           1230
seed7/prg/chkerr.sd7         0.033660           4663
seed7/prg/chkexc.sd7         0.033970           2627
seed7/prg/chkfil.sd7         0.033818           1615
seed7/prg/chkflt.sd7         0.037818          20620
seed7/prg/chkhent.sd7        0.033782             54
seed7/prg/chkhsh.sd7         0.033451           4548
seed7/prg/chkidx.sd7         0.035133          19567
seed7/prg/chkint.sd7         0.039725          38129
seed7/prg/chkjson.sd7        0.032898           1764
seed7/prg/chkovf.sd7         0.033230           8216
seed7/prg/chkprc.sd7         0.033040          10111
seed7/prg/chkscan.sd7        0.034540            714
seed7/prg/chkset.sd7         0.035601          11974
seed7/prg/chkstr.sd7         0.037544          26952
seed7/prg/chktime.sd7        0.033550           2025
seed7/prg/chktoml.sd7        0.035590           1656
seed7/prg/clock.sd7          0.033759             47
seed7/prg/clock2.sd7         0.034991             43
seed7/prg/clock3.sd7         0.035856             95
seed7/prg/cmpfil.sd7         0.034649             84
seed7/prg/comanche.sd7       0.033730            180
seed7/prg/confval.sd7        0.033721            175
seed7/prg/db7.sd7            0.033501            417
seed7/prg/diff7.sd7          0.033657            263
seed7/prg/dirtst.sd7         0.034404             42
seed7/prg/dirx.sd7           0.033319            152
seed7/prg/dnafight.sd7       0.033726           1381
seed7/prg/dragon.sd7         0.033397             73
seed7/prg/echo.sd7           0.033116             39
seed7/prg/eliza.sd7          0.033478            302
seed7/prg/err.sd7            0.033480             96
seed7/prg/fannkuch.sd7       0.033775            131
seed7/prg/fib.sd7            0.034041             47
seed7/prg/find7.sd7          0.032800            133
seed7/prg/findchar.sd7       0.032467            149
seed7/prg/fractree.sd7       0.032515             55
seed7/prg/ftp7.sd7           0.033284            296
seed7/prg/ftpserv.sd7        0.033541             74
seed7/prg/gcd.sd7            0.032373            109
seed7/prg/gkbd.sd7           0.032721            358
seed7/prg/gtksvtst.sd7       0.032789             94
seed7/prg/hal.sd7            0.032557            250
seed7/prg/hamu.sd7           0.032495            573
seed7/prg/hanoi.sd7          0.034044             55
seed7/prg/hd.sd7             0.033572             79
seed7/prg/hello.sd7          0.035773             32
seed7/prg/hilbert.sd7        0.040733            108
seed7/prg/ide7.sd7           0.034871            196
seed7/prg/kbd.sd7            0.033403             49
seed7/prg/klondike.sd7       0.033491            883
seed7/prg/lander.sd7         0.033910           1551
seed7/prg/lst80bas.sd7       0.033416            344
seed7/prg/lst99bas.sd7       0.033272            401
seed7/prg/lstgwbas.sd7       0.036019            577
seed7/prg/mahjong.sd7        0.041161           1943
seed7/prg/make7.sd7          0.041597            121
seed7/prg/mandelbr.sd7       0.044306            237
seed7/prg/mind.sd7           0.038890            443
seed7/prg/mirror.sd7         0.035208            131
seed7/prg/ms.sd7             0.033989            641
seed7/prg/nicoma.sd7         0.036796            135
seed7/prg/pac.sd7            0.036288            726
seed7/prg/pairs.sd7          0.038957           2025
seed7/prg/panic.sd7          0.045271           2634
seed7/prg/percolation.sd7    0.048612            330
seed7/prg/planets.sd7        0.037543           1486
seed7/prg/portfwd7.sd7       0.049664            139
seed7/prg/prime.sd7          0.042829             74
seed7/prg/printpi1.sd7       0.041220             56
seed7/prg/printpi2.sd7       0.039441             54
seed7/prg/printpi3.sd7       0.033496             60
seed7/prg/pv7.sd7            0.033393            337
seed7/prg/queen.sd7          0.034114            149
seed7/prg/rand.sd7           0.033808            121
seed7/prg/raytrace.sd7       0.034004            538
seed7/prg/rever.sd7          0.038210            816
seed7/prg/roman.sd7          0.040868             38
seed7/prg/s7c.sd7            0.081123           9060
seed7/prg/s7check.sd7        0.040133             68
seed7/prg/savehd7.sd7        0.042546           1110
seed7/prg/self.sd7           0.075342             49
seed7/prg/shisen.sd7         0.055452           1423
seed7/prg/sl.sd7             0.045531           1029
seed7/prg/snake.sd7          0.040335            615
seed7/prg/sokoban.sd7        0.037949            891
seed7/prg/spigotpi.sd7       0.038245             64
seed7/prg/sql7.sd7           0.035250            278
seed7/prg/startrek.sd7       0.036614            979
seed7/prg/sudoku7.sd7        0.034631           2657
seed7/prg/sydir7.sd7         0.035382            384
seed7/prg/syntaxhl.sd7       0.033451            177
seed7/prg/tak.sd7            0.033438             59
seed7/prg/tar7.sd7           0.035541            121
seed7/prg/tch.sd7            0.039283             55
seed7/prg/testfont.sd7       0.043352             95
seed7/prg/tet.sd7            0.037417            479
seed7/prg/tetg.sd7           0.034415            501
seed7/prg/toutf8.sd7         0.034525            240
seed7/prg/tst_cli.sd7        0.036779             40
seed7/prg/tst_srv.sd7        0.034245             47
seed7/prg/wator.sd7          0.034109            651
seed7/prg/which.sd7          0.033969             65
seed7/prg/wiz.sd7            0.034068           2833
seed7/prg/wordcnt.sd7        0.034184             54
seed7/prg/wrinum.sd7         0.033882             43
seed7/prg/wumpus.sd7         0.033742            372
seed7/lib/aes.s7i            0.033900           1144
seed7/lib/aes_gcm.s7i        0.034129            392
seed7/lib/ar.s7i             0.034047           1532
seed7/lib/arc4.s7i           0.033950            144
seed7/lib/archive.s7i        0.033952            143
seed7/lib/archive_base.s7i   0.033782            135
seed7/lib/array.s7i          0.033889            610
seed7/lib/asn1.s7i           0.034124            544
seed7/lib/asn1oid.s7i        0.034385            157
seed7/lib/basearray.s7i      0.033895            450
seed7/lib/bigfile.s7i        0.034266            136
seed7/lib/bigint.s7i         0.034038            824
seed7/lib/bigrat.s7i         0.034326            784
seed7/lib/bin16.s7i          0.034189            592
seed7/lib/bin32.s7i          0.033342            490
seed7/lib/bin64.s7i          0.034039            539
seed7/lib/bitdata.s7i        0.033700           1330
seed7/lib/bitmapfont.s7i     0.033423            215
seed7/lib/bitset.s7i         0.034084            593
seed7/lib/bitsetof.s7i       0.034689            431
seed7/lib/blowfish.s7i       0.033757            383
seed7/lib/bmp.s7i            0.033881            924
seed7/lib/boolean.s7i        0.034085            403
seed7/lib/browser.s7i        0.035806            280
seed7/lib/bstring.s7i        0.040155            227
seed7/lib/bytedata.s7i       0.035417            482
seed7/lib/bzip2.s7i          0.033773            887
seed7/lib/cards.s7i          0.033869           1342
seed7/lib/category.s7i       0.034261            209
seed7/lib/cc_conf.s7i        0.045193           1314
seed7/lib/ccittfax.s7i       0.052019           1022
seed7/lib/cgi.s7i            0.035129            109
seed7/lib/cgidialog.s7i      0.034571           1118
seed7/lib/char.s7i           0.034070            356
seed7/lib/charsets.s7i       0.037257           2024
seed7/lib/chartype.s7i       0.048052            121
seed7/lib/cipher.s7i         0.035359            146
seed7/lib/cli_cmds.s7i       0.035523           1360
seed7/lib/clib_file.s7i      0.034905            301
seed7/lib/color.s7i          0.035421            185
seed7/lib/complex.s7i        0.057693            464
seed7/lib/compress.s7i       0.038789            150
seed7/lib/console.s7i        0.035789            188
seed7/lib/cpio.s7i           0.034346           1708
seed7/lib/crc32.s7i          0.033452            193
seed7/lib/cronos16.s7i       0.033263           1173
seed7/lib/cronos27.s7i       0.033075           1464
seed7/lib/csv.s7i            0.033806            201
seed7/lib/db_prop.s7i        0.033971            991
seed7/lib/deflate.s7i        0.035931            740
seed7/lib/des.s7i            0.040513            444
seed7/lib/dialog.s7i         0.044642            311
seed7/lib/dir.s7i            0.036986            163
seed7/lib/draw.s7i           0.034625            854
seed7/lib/duration.s7i       0.033887           1038
seed7/lib/echo.s7i           0.033471            132
seed7/lib/editline.s7i       0.038837            398
seed7/lib/elf.s7i            0.035244           1560
seed7/lib/elliptic.s7i       0.034092            649
seed7/lib/enable_io.s7i      0.038467            312
seed7/lib/encoding.s7i       0.039506            931
seed7/lib/enumeration.s7i    0.041678            236
seed7/lib/environment.s7i    0.041523            175
seed7/lib/exif.s7i           0.045418            152
seed7/lib/external_file.s7i  0.039815            340
seed7/lib/field.s7i          0.044288            268
seed7/lib/file.s7i           0.038330            372
seed7/lib/filebits.s7i       0.042049             46
seed7/lib/filesys.s7i        0.039977            601
seed7/lib/fileutil.s7i       0.034444            144
seed7/lib/fixarray.s7i       0.033903            307
seed7/lib/float.s7i          0.033423            757
seed7/lib/font.s7i           0.033359            196
seed7/lib/font8x8.s7i        0.035862            998
seed7/lib/forloop.s7i        0.037287            449
seed7/lib/ftp.s7i            0.036596            969
seed7/lib/ftpserv.s7i        0.034498            631
seed7/lib/getf.s7i           0.036732            115
seed7/lib/gethttp.s7i        0.033646             41
seed7/lib/gethttps.s7i       0.033992             41
seed7/lib/gif.s7i            0.034135            561
seed7/lib/graph.s7i          0.033868            415
seed7/lib/graph_file.s7i     0.033855            399
seed7/lib/gtkserver.s7i      0.033429            161
seed7/lib/gzip.s7i           0.033739            573
seed7/lib/hash.s7i           0.036181            421
seed7/lib/hashsetof.s7i      0.041096            499
seed7/lib/hmac.s7i           0.034919            152
seed7/lib/html.s7i           0.033900             83
seed7/lib/html_ent.s7i       0.033932            476
seed7/lib/htmldom.s7i        0.033513            286
seed7/lib/http_request.s7i   0.033736            696
seed7/lib/http_srv_resp.s7i  0.034287            380
seed7/lib/https_request.s7i  0.033687            211
seed7/lib/httpserv.s7i       0.035050            345
seed7/lib/huffman.s7i        0.035942            644
seed7/lib/ico.s7i            0.035609            221
seed7/lib/idxarray.s7i       0.040963            232
seed7/lib/image.s7i          0.034307            156
seed7/lib/imagefile.s7i      0.033773            171
seed7/lib/inflate.s7i        0.035462            411
seed7/lib/inifile.s7i        0.033623            129
seed7/lib/integer.s7i        0.033683            663
seed7/lib/iobuffer.s7i       0.033292            289
seed7/lib/jpeg.s7i           0.033352           1761
seed7/lib/json.s7i           0.032569            891
seed7/lib/json_serde.s7i     0.032905            783
seed7/lib/keybd.s7i          0.033037            639
seed7/lib/keydescr.s7i       0.032597            192
seed7/lib/leb128.s7i         0.033644            218
seed7/lib/line.s7i           0.033582            164
seed7/lib/listener.s7i       0.032722            247
seed7/lib/logfile.s7i        0.032880             73
seed7/lib/lower.s7i          0.032761            142
seed7/lib/lzma.s7i           0.033523            934
seed7/lib/lzw.s7i            0.034045            861
seed7/lib/magic.s7i          0.033560            403
seed7/lib/mahjng32.s7i       0.034020           1500
seed7/lib/make.s7i           0.033492            544
seed7/lib/makedata.s7i       0.033447           1428
seed7/lib/math.s7i           0.033861            201
seed7/lib/mixarith.s7i       0.034183            249
seed7/lib/modern27.s7i       0.033651           1099
seed7/lib/more.s7i           0.033943            130
seed7/lib/msgdigest.s7i      0.034729           1222
seed7/lib/multiscr.s7i       0.034651             68
seed7/lib/null_file.s7i      0.044675            345
seed7/lib/osfiles.s7i        0.033987           1085
seed7/lib/pbm.s7i            0.033778            230
seed7/lib/pcx.s7i            0.033672            638
seed7/lib/pem.s7i            0.033056            185
seed7/lib/pgm.s7i            0.033918            238
seed7/lib/pic16.s7i          0.033708           1037
seed7/lib/pic32.s7i          0.033646           2060
seed7/lib/pic_util.s7i       0.033281            144
seed7/lib/pixelimage.s7i     0.037913            320
seed7/lib/pixmap_file.s7i    0.037243            459
seed7/lib/pixmapfont.s7i     0.034019            184
seed7/lib/pkcs1.s7i          0.032925            543
seed7/lib/png.s7i            0.032995           1064
seed7/lib/poll.s7i           0.034065            313
seed7/lib/ppm.s7i            0.033949            240
seed7/lib/process.s7i        0.033643            541
seed7/lib/progs.s7i          0.033602            789
seed7/lib/propertyfile.s7i   0.033679            155
seed7/lib/rational.s7i       0.033636            792
seed7/lib/ref_list.s7i       0.033533            252
seed7/lib/reference.s7i      0.033791            126
seed7/lib/reverse.s7i        0.033603             94
seed7/lib/rpm.s7i            0.033815           3487
seed7/lib/rpmext.s7i         0.033792            318
seed7/lib/scanfile.s7i       0.034085           1779
seed7/lib/scanjson.s7i       0.033486            413
seed7/lib/scanstri.s7i       0.033626           1814
seed7/lib/scantoml.s7i       0.032846           1603
seed7/lib/seed7_05.s7i       0.032792           1072
seed7/lib/set.s7i            0.032730             57
seed7/lib/shell.s7i          0.032776            615
seed7/lib/showtls.s7i        0.032524            678
seed7/lib/signature.s7i      0.032731            131
seed7/lib/smtp.s7i           0.033111            261
seed7/lib/sockbase.s7i       0.033278            217
seed7/lib/socket.s7i         0.032710            326
seed7/lib/sokoban1.s7i       0.032933           1519
seed7/lib/sql_base.s7i       0.032936           1000
seed7/lib/stars.s7i          0.033157           1705
seed7/lib/stdfont10.s7i      0.033067           3347
seed7/lib/stdfont12.s7i      0.033216           3928
seed7/lib/stdfont14.s7i      0.033062           4510
seed7/lib/stdfont16.s7i      0.032800           5092
seed7/lib/stdfont18.s7i      0.034508           5868
seed7/lib/stdfont20.s7i      0.034120           6449
seed7/lib/stdfont24.s7i      0.034034           7421
seed7/lib/stdfont8.s7i       0.033314           2960
seed7/lib/stdfont9.s7i       0.034394           3152
seed7/lib/stdio.s7i          0.034935            192
seed7/lib/strifile.s7i       0.033573            345
seed7/lib/string.s7i         0.034223            779
seed7/lib/stritext.s7i       0.033460            352
seed7/lib/struct.s7i         0.033795            266
seed7/lib/struct_elem.s7i    0.033650            129
seed7/lib/subfile.s7i        0.033494            174
seed7/lib/subrange.s7i       0.033454             78
seed7/lib/syntax.s7i         0.033531            294
seed7/lib/tar.s7i            0.033712           1880
seed7/lib/tar_cmds.s7i       0.033889            752
seed7/lib/tdes.s7i           0.033492            143
seed7/lib/tee.s7i            0.033322            143
seed7/lib/text.s7i           0.033201            135
seed7/lib/tga.s7i            0.033421            676
seed7/lib/tiff.s7i           0.033829           2771
seed7/lib/time.s7i           0.034243           1191
seed7/lib/tls.s7i            0.033739           2230
seed7/lib/unicode.s7i        0.033273            575
seed7/lib/unionfnd.s7i       0.033331            130
seed7/lib/upper.s7i          0.033261            142
seed7/lib/utf16.s7i          0.032931            540
seed7/lib/utf8.s7i           0.032761            234
seed7/lib/vecfont10.s7i      0.033088           1056
seed7/lib/vecfont18.s7i      0.033060           1119
seed7/lib/vector3d.s7i       0.032815            293
seed7/lib/vectorfont.s7i     0.035517            239
seed7/lib/wildcard.s7i       0.033040            140
seed7/lib/window.s7i         0.032955            455
seed7/lib/wrinum.s7i         0.033086            248
seed7/lib/x509cert.s7i       0.033011           1243
seed7/lib/xml_ent.s7i        0.032945             94
seed7/lib/xmldom.s7i         0.032796            303
seed7/lib/xz.s7i             0.032752            442
seed7/lib/zip.s7i            0.033393           2792
seed7/lib/zstd.s7i           0.033457           1263
============================ ================= ================

Statistical Summary — Mode A (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.035360        |
+-----------+-----------------+
| Minimum   | 0.018352        |
+-----------+-----------------+
| Maximum   | 0.081123        |
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
seed7/prg/.#bas7.sd7         0.018322              1
seed7/prg/addup.sd7          0.037926            190
seed7/prg/bas7.sd7           0.040281          11459
seed7/prg/bifurk.sd7         0.036297             73
seed7/prg/bigfiles.sd7       0.039150            129
seed7/prg/brainf7.sd7        0.038041             86
seed7/prg/calc7.sd7          0.039891            128
seed7/prg/carddemo.sd7       0.038612            190
seed7/prg/castle.sd7         0.039161           3148
seed7/prg/cat.sd7            0.037320             82
seed7/prg/cellauto.sd7       0.037300             85
seed7/prg/celsius.sd7        0.035596             42
seed7/prg/chk_all.sd7        0.041009            843
seed7/prg/chkarr.sd7         0.039549           8367
seed7/prg/chkbig.sd7         0.044401          29026
seed7/prg/chkbin.sd7         0.040676           6469
seed7/prg/chkbitdata.sd7     0.040173           6624
seed7/prg/chkbool.sd7        0.037940           3157
seed7/prg/chkbst.sd7         0.038381            722
seed7/prg/chkchr.sd7         0.039459           2809
seed7/prg/chkcmd.sd7         0.037935           1205
seed7/prg/chkdb.sd7          0.039678           7454
seed7/prg/chkdecl.sd7        0.039270            448
seed7/prg/chkenum.sd7        0.038677           1230
seed7/prg/chkerr.sd7         0.040034           4663
seed7/prg/chkexc.sd7         0.039910           2627
seed7/prg/chkfil.sd7         0.038700           1615
seed7/prg/chkflt.sd7         0.043206          20620
seed7/prg/chkhent.sd7        0.036685             54
seed7/prg/chkhsh.sd7         0.040358           4548
seed7/prg/chkidx.sd7         0.042696          19567
seed7/prg/chkint.sd7         0.046102          38129
seed7/prg/chkjson.sd7        0.039538           1764
seed7/prg/chkovf.sd7         0.039967           8216
seed7/prg/chkprc.sd7         0.038183          10111
seed7/prg/chkscan.sd7        0.039430            714
seed7/prg/chkset.sd7         0.041552          11974
seed7/prg/chkstr.sd7         0.044389          26952
seed7/prg/chktime.sd7        0.042979           2025
seed7/prg/chktoml.sd7        0.040051           1656
seed7/prg/clock.sd7          0.036394             47
seed7/prg/clock2.sd7         0.035506             43
seed7/prg/clock3.sd7         0.037991             95
seed7/prg/cmpfil.sd7         0.036849             84
seed7/prg/comanche.sd7       0.037730            180
seed7/prg/confval.sd7        0.040311            175
seed7/prg/db7.sd7            0.038905            417
seed7/prg/diff7.sd7          0.041570            263
seed7/prg/dirtst.sd7         0.038226             42
seed7/prg/dirx.sd7           0.040677            152
seed7/prg/dnafight.sd7       0.038497           1381
seed7/prg/dragon.sd7         0.036404             73
seed7/prg/echo.sd7           0.035197             39
seed7/prg/eliza.sd7          0.039988            302
seed7/prg/err.sd7            0.041878             96
seed7/prg/fannkuch.sd7       0.037784            131
seed7/prg/fib.sd7            0.035592             47
seed7/prg/find7.sd7          0.038923            133
seed7/prg/findchar.sd7       0.038986            149
seed7/prg/fractree.sd7       0.037023             55
seed7/prg/ftp7.sd7           0.039257            296
seed7/prg/ftpserv.sd7        0.037828             74
seed7/prg/gcd.sd7            0.037777            109
seed7/prg/gkbd.sd7           0.040884            358
seed7/prg/gtksvtst.sd7       0.037971             94
seed7/prg/hal.sd7            0.039042            250
seed7/prg/hamu.sd7           0.039280            573
seed7/prg/hanoi.sd7          0.035893             55
seed7/prg/hd.sd7             0.037205             79
seed7/prg/hello.sd7          0.035008             32
seed7/prg/hilbert.sd7        0.039157            108
seed7/prg/ide7.sd7           0.038967            196
seed7/prg/kbd.sd7            0.035892             49
seed7/prg/klondike.sd7       0.040825            883
seed7/prg/lander.sd7         0.039216           1551
seed7/prg/lst80bas.sd7       0.038623            344
seed7/prg/lst99bas.sd7       0.040412            401
seed7/prg/lstgwbas.sd7       0.040986            577
seed7/prg/mahjong.sd7        0.039440           1943
seed7/prg/make7.sd7          0.039854            121
seed7/prg/mandelbr.sd7       0.039385            237
seed7/prg/mind.sd7           0.039088            443
seed7/prg/mirror.sd7         0.039777            131
seed7/prg/ms.sd7             0.039008            641
seed7/prg/nicoma.sd7         0.039083            135
seed7/prg/pac.sd7            0.038873            726
seed7/prg/pairs.sd7          0.039128           2025
seed7/prg/panic.sd7          0.039803           2634
seed7/prg/percolation.sd7    0.038686            330
seed7/prg/planets.sd7        0.040025           1486
seed7/prg/portfwd7.sd7       0.037968            139
seed7/prg/prime.sd7          0.035964             74
seed7/prg/printpi1.sd7       0.035496             56
seed7/prg/printpi2.sd7       0.035772             54
seed7/prg/printpi3.sd7       0.041207             60
seed7/prg/pv7.sd7            0.039644            337
seed7/prg/queen.sd7          0.038004            149
seed7/prg/rand.sd7           0.037358            121
seed7/prg/raytrace.sd7       0.038810            538
seed7/prg/rever.sd7          0.038114            816
seed7/prg/roman.sd7          0.037756             38
seed7/prg/s7c.sd7            0.039927           9060
seed7/prg/s7check.sd7        0.038080             68
seed7/prg/savehd7.sd7        0.041237           1110
seed7/prg/self.sd7           0.036881             49
seed7/prg/shisen.sd7         0.038856           1423
seed7/prg/sl.sd7             0.039132           1029
seed7/prg/snake.sd7          0.039042            615
seed7/prg/sokoban.sd7        0.038837            891
seed7/prg/spigotpi.sd7       0.037695             64
seed7/prg/sql7.sd7           0.038962            278
seed7/prg/startrek.sd7       0.039666            979
seed7/prg/sudoku7.sd7        0.039354           2657
seed7/prg/sydir7.sd7         0.039107            384
seed7/prg/syntaxhl.sd7       0.041267            177
seed7/prg/tak.sd7            0.036616             59
seed7/prg/tar7.sd7           0.039133            121
seed7/prg/tch.sd7            0.036748             55
seed7/prg/testfont.sd7       0.038823             95
seed7/prg/tet.sd7            0.038416            479
seed7/prg/tetg.sd7           0.039456            501
seed7/prg/toutf8.sd7         0.040773            240
seed7/prg/tst_cli.sd7        0.035106             40
seed7/prg/tst_srv.sd7        0.035114             47
seed7/prg/wator.sd7          0.037891            651
seed7/prg/which.sd7          0.035454             65
seed7/prg/wiz.sd7            0.038521           2833
seed7/prg/wordcnt.sd7        0.036626             54
seed7/prg/wrinum.sd7         0.036522             43
seed7/prg/wumpus.sd7         0.038971            372
seed7/lib/aes.s7i            0.042137           1144
seed7/lib/aes_gcm.s7i        0.039714            392
seed7/lib/ar.s7i             0.039344           1532
seed7/lib/arc4.s7i           0.039171            144
seed7/lib/archive.s7i        0.038563            143
seed7/lib/archive_base.s7i   0.040816            135
seed7/lib/array.s7i          0.046270            610
seed7/lib/asn1.s7i           0.038500            544
seed7/lib/asn1oid.s7i        0.043317            157
seed7/lib/basearray.s7i      0.040337            450
seed7/lib/bigfile.s7i        0.039848            136
seed7/lib/bigint.s7i         0.037673            824
seed7/lib/bigrat.s7i         0.037859            784
seed7/lib/bin16.s7i          0.037980            592
seed7/lib/bin32.s7i          0.038135            490
seed7/lib/bin64.s7i          0.038280            539
seed7/lib/bitdata.s7i        0.044399           1330
seed7/lib/bitmapfont.s7i     0.038063            215
seed7/lib/bitset.s7i         0.037692            593
seed7/lib/bitsetof.s7i       0.040106            431
seed7/lib/blowfish.s7i       0.040818            383
seed7/lib/bmp.s7i            0.043126            924
seed7/lib/boolean.s7i        0.044065            403
seed7/lib/browser.s7i        0.038201            280
seed7/lib/bstring.s7i        0.038901            227
seed7/lib/bytedata.s7i       0.040102            482
seed7/lib/bzip2.s7i          0.045339            887
seed7/lib/cards.s7i          0.038648           1342
seed7/lib/category.s7i       0.039464            209
seed7/lib/cc_conf.s7i        0.038903           1314
seed7/lib/ccittfax.s7i       0.039632           1022
seed7/lib/cgi.s7i            0.038474            109
seed7/lib/cgidialog.s7i      0.040206           1118
seed7/lib/char.s7i           0.039076            356
seed7/lib/charsets.s7i       0.039881           2024
seed7/lib/chartype.s7i       0.042445            121
seed7/lib/cipher.s7i         0.040432            146
seed7/lib/cli_cmds.s7i       0.041509           1360
seed7/lib/clib_file.s7i      0.039312            301
seed7/lib/color.s7i          0.039745            185
seed7/lib/complex.s7i        0.039609            464
seed7/lib/compress.s7i       0.039645            150
seed7/lib/console.s7i        0.040363            188
seed7/lib/cpio.s7i           0.039664           1708
seed7/lib/crc32.s7i          0.039009            193
seed7/lib/cronos16.s7i       0.041574           1173
seed7/lib/cronos27.s7i       0.041752           1464
seed7/lib/csv.s7i            0.038887            201
seed7/lib/db_prop.s7i        0.038611            991
seed7/lib/deflate.s7i        0.039864            740
seed7/lib/des.s7i            0.040135            444
seed7/lib/dialog.s7i         0.038717            311
seed7/lib/dir.s7i            0.038186            163
seed7/lib/draw.s7i           0.038190            854
seed7/lib/duration.s7i       0.037827           1038
seed7/lib/echo.s7i           0.037975            132
seed7/lib/editline.s7i       0.045793            398
seed7/lib/elf.s7i            0.043559           1560
seed7/lib/elliptic.s7i       0.038509            649
seed7/lib/enable_io.s7i      0.039398            312
seed7/lib/encoding.s7i       0.039677            931
seed7/lib/enumeration.s7i    0.038885            236
seed7/lib/environment.s7i    0.039244            175
seed7/lib/exif.s7i           0.041461            152
seed7/lib/external_file.s7i  0.038822            340
seed7/lib/field.s7i          0.038985            268
seed7/lib/file.s7i           0.039677            372
seed7/lib/filebits.s7i       0.037305             46
seed7/lib/filesys.s7i        0.041202            601
seed7/lib/fileutil.s7i       0.039278            144
seed7/lib/fixarray.s7i       0.039109            307
seed7/lib/float.s7i          0.038258            757
seed7/lib/font.s7i           0.038293            196
seed7/lib/font8x8.s7i        0.037516            998
seed7/lib/forloop.s7i        0.039314            449
seed7/lib/ftp.s7i            0.039574            969
seed7/lib/ftpserv.s7i        0.039304            631
seed7/lib/getf.s7i           0.039633            115
seed7/lib/gethttp.s7i        0.036885             41
seed7/lib/gethttps.s7i       0.036559             41
seed7/lib/gif.s7i            0.039258            561
seed7/lib/graph.s7i          0.040555            415
seed7/lib/graph_file.s7i     0.038988            399
seed7/lib/gtkserver.s7i      0.038763            161
seed7/lib/gzip.s7i           0.039720            573
seed7/lib/hash.s7i           0.041120            421
seed7/lib/hashsetof.s7i      0.040961            499
seed7/lib/hmac.s7i           0.039779            152
seed7/lib/html.s7i           0.038626             83
seed7/lib/html_ent.s7i       0.039199            476
seed7/lib/htmldom.s7i        0.039188            286
seed7/lib/http_request.s7i   0.038401            696
seed7/lib/http_srv_resp.s7i  0.038205            380
seed7/lib/https_request.s7i  0.037631            211
seed7/lib/httpserv.s7i       0.038149            345
seed7/lib/huffman.s7i        0.038218            644
seed7/lib/ico.s7i            0.038219            221
seed7/lib/idxarray.s7i       0.038164            232
seed7/lib/image.s7i          0.038559            156
seed7/lib/imagefile.s7i      0.037722            171
seed7/lib/inflate.s7i        0.040332            411
seed7/lib/inifile.s7i        0.037995            129
seed7/lib/integer.s7i        0.039562            663
seed7/lib/iobuffer.s7i       0.039235            289
seed7/lib/jpeg.s7i           0.039832           1761
seed7/lib/json.s7i           0.038768            891
seed7/lib/json_serde.s7i     0.039235            783
seed7/lib/keybd.s7i          0.038681            639
seed7/lib/keydescr.s7i       0.040994            192
seed7/lib/leb128.s7i         0.039423            218
seed7/lib/line.s7i           0.038814            164
seed7/lib/listener.s7i       0.040030            247
seed7/lib/logfile.s7i        0.037668             73
seed7/lib/lower.s7i          0.038566            142
seed7/lib/lzma.s7i           0.040190            934
seed7/lib/lzw.s7i            0.039329            861
seed7/lib/magic.s7i          0.040901            403
seed7/lib/mahjng32.s7i       0.039042           1500
seed7/lib/make.s7i           0.039106            544
seed7/lib/makedata.s7i       0.039967           1428
seed7/lib/math.s7i           0.039130            201
seed7/lib/mixarith.s7i       0.039184            249
seed7/lib/modern27.s7i       0.042537           1099
seed7/lib/more.s7i           0.038900            130
seed7/lib/msgdigest.s7i      0.040040           1222
seed7/lib/multiscr.s7i       0.038992             68
seed7/lib/null_file.s7i      0.041513            345
seed7/lib/osfiles.s7i        0.039797           1085
seed7/lib/pbm.s7i            0.039831            230
seed7/lib/pcx.s7i            0.041304            638
seed7/lib/pem.s7i            0.037939            185
seed7/lib/pgm.s7i            0.039538            238
seed7/lib/pic16.s7i          0.038945           1037
seed7/lib/pic32.s7i          0.037740           2060
seed7/lib/pic_util.s7i       0.038932            144
seed7/lib/pixelimage.s7i     0.038980            320
seed7/lib/pixmap_file.s7i    0.039398            459
seed7/lib/pixmapfont.s7i     0.041046            184
seed7/lib/pkcs1.s7i          0.044354            543
seed7/lib/png.s7i            0.040433           1064
seed7/lib/poll.s7i           0.038958            313
seed7/lib/ppm.s7i            0.038309            240
seed7/lib/process.s7i        0.038127            541
seed7/lib/progs.s7i          0.038301            789
seed7/lib/propertyfile.s7i   0.038090            155
seed7/lib/rational.s7i       0.038077            792
seed7/lib/ref_list.s7i       0.038252            252
seed7/lib/reference.s7i      0.037843            126
seed7/lib/reverse.s7i        0.037063             94
seed7/lib/rpm.s7i            0.039614           3487
seed7/lib/rpmext.s7i         0.040738            318
seed7/lib/scanfile.s7i       0.038557           1779
seed7/lib/scanjson.s7i       0.038371            413
seed7/lib/scanstri.s7i       0.038399           1814
seed7/lib/scantoml.s7i       0.037978           1603
seed7/lib/seed7_05.s7i       0.040275           1072
seed7/lib/set.s7i            0.037646             57
seed7/lib/shell.s7i          0.040118            615
seed7/lib/showtls.s7i        0.039425            678
seed7/lib/signature.s7i      0.039273            131
seed7/lib/smtp.s7i           0.040270            261
seed7/lib/sockbase.s7i       0.041883            217
seed7/lib/socket.s7i         0.040254            326
seed7/lib/sokoban1.s7i       0.038775           1519
seed7/lib/sql_base.s7i       0.041166           1000
seed7/lib/stars.s7i          0.042049           1705
seed7/lib/stdfont10.s7i      0.038807           3347
seed7/lib/stdfont12.s7i      0.042043           3928
seed7/lib/stdfont14.s7i      0.039489           4510
seed7/lib/stdfont16.s7i      0.039271           5092
seed7/lib/stdfont18.s7i      0.039548           5868
seed7/lib/stdfont20.s7i      0.039384           6449
seed7/lib/stdfont24.s7i      0.039701           7421
seed7/lib/stdfont8.s7i       0.037868           2960
seed7/lib/stdfont9.s7i       0.037843           3152
seed7/lib/stdio.s7i          0.038946            192
seed7/lib/strifile.s7i       0.039214            345
seed7/lib/string.s7i         0.039218            779
seed7/lib/stritext.s7i       0.038146            352
seed7/lib/struct.s7i         0.039262            266
seed7/lib/struct_elem.s7i    0.038034            129
seed7/lib/subfile.s7i        0.038378            174
seed7/lib/subrange.s7i       0.036731             78
seed7/lib/syntax.s7i         0.040928            294
seed7/lib/tar.s7i            0.038653           1880
seed7/lib/tar_cmds.s7i       0.038823            752
seed7/lib/tdes.s7i           0.039082            143
seed7/lib/tee.s7i            0.038265            143
seed7/lib/text.s7i           0.038120            135
seed7/lib/tga.s7i            0.040300            676
seed7/lib/tiff.s7i           0.039763           2771
seed7/lib/time.s7i           0.039135           1191
seed7/lib/tls.s7i            0.039652           2230
seed7/lib/unicode.s7i        0.045021            575
seed7/lib/unionfnd.s7i       0.040293            130
seed7/lib/upper.s7i          0.039782            142
seed7/lib/utf16.s7i          0.042297            540
seed7/lib/utf8.s7i           0.041195            234
seed7/lib/vecfont10.s7i      0.044885           1056
seed7/lib/vecfont18.s7i      0.043036           1119
seed7/lib/vector3d.s7i       0.039620            293
seed7/lib/vectorfont.s7i     0.040877            239
seed7/lib/wildcard.s7i       0.039030            140
seed7/lib/window.s7i         0.038974            455
seed7/lib/wrinum.s7i         0.038815            248
seed7/lib/x509cert.s7i       0.039066           1243
seed7/lib/xml_ent.s7i        0.040927             94
seed7/lib/xmldom.s7i         0.037877            303
seed7/lib/xz.s7i             0.037616            442
seed7/lib/zip.s7i            0.038179           2792
seed7/lib/zstd.s7i           0.040732           1263
============================ ================= ================

Statistical Summary — Mode B (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.039229        |
+-----------+-----------------+
| Minimum   | 0.018322        |
+-----------+-----------------+
| Maximum   | 0.046270        |
+-----------+-----------------+

Mode C — Mode Activation + Full-Buffer Fontification (font-lock-ensure)
-----------------------------------------------------------------------

Full-buffer fontification stress test.

File Load Times — Mode C (mean, GC-free)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

============================ ================= ================
File Name                    Mean Time (s)     Line count
============================ ================= ================
seed7/prg/.#bas7.sd7         0.018444              1
seed7/prg/addup.sd7          0.039427            190
seed7/prg/bas7.sd7           0.318640          11459
seed7/prg/bifurk.sd7         0.036253             73
seed7/prg/bigfiles.sd7       0.037496            129
seed7/prg/brainf7.sd7        0.036519             86
seed7/prg/calc7.sd7          0.038701            128
seed7/prg/carddemo.sd7       0.039243            190
seed7/prg/castle.sd7         0.107050           3148
seed7/prg/cat.sd7            0.036208             82
seed7/prg/cellauto.sd7       0.037044             85
seed7/prg/celsius.sd7        0.035455             42
seed7/prg/chk_all.sd7        0.058339            843
seed7/prg/chkarr.sd7         0.351806           8367
seed7/prg/chkbig.sd7         2.032677          29026
seed7/prg/chkbin.sd7         0.504548           6469
seed7/prg/chkbitdata.sd7     0.603663           6624
seed7/prg/chkbool.sd7        0.115410           3157
seed7/prg/chkbst.sd7         0.062167            722
seed7/prg/chkchr.sd7         0.214545           2809
seed7/prg/chkcmd.sd7         0.067353           1205
seed7/prg/chkdb.sd7          0.352983           7454
seed7/prg/chkdecl.sd7        0.056913            448
seed7/prg/chkenum.sd7        0.065475           1230
seed7/prg/chkerr.sd7         0.191810           4663
seed7/prg/chkexc.sd7         0.082328           2627
seed7/prg/chkfil.sd7         0.075763           1615
seed7/prg/chkflt.sd7         1.329503          20620
seed7/prg/chkhent.sd7        0.039161             54
seed7/prg/chkhsh.sd7         0.249382           4548
seed7/prg/chkidx.sd7         1.301670          19567
seed7/prg/chkint.sd7         2.499137          38129
seed7/prg/chkjson.sd7        0.101020           1764
seed7/prg/chkovf.sd7         0.553746           8216
seed7/prg/chkprc.sd7         0.316338          10111
seed7/prg/chkscan.sd7        0.055649            714
seed7/prg/chkset.sd7         0.645734          11974
seed7/prg/chkstr.sd7         1.360632          26952
seed7/prg/chktime.sd7        0.126165           2025
seed7/prg/chktoml.sd7        0.106018           1656
seed7/prg/clock.sd7          0.035989             47
seed7/prg/clock2.sd7         0.035692             43
seed7/prg/clock3.sd7         0.037357             95
seed7/prg/cmpfil.sd7         0.036163             84
seed7/prg/comanche.sd7       0.040240            180
seed7/prg/confval.sd7        0.043027            175
seed7/prg/db7.sd7            0.049302            417
seed7/prg/diff7.sd7          0.045098            263
seed7/prg/dirtst.sd7         0.036238             42
seed7/prg/dirx.sd7           0.038524            152
seed7/prg/dnafight.sd7       0.065090           1381
seed7/prg/dragon.sd7         0.037590             73
seed7/prg/echo.sd7           0.034428             39
seed7/prg/eliza.sd7          0.041237            302
seed7/prg/err.sd7            0.040360             96
seed7/prg/fannkuch.sd7       0.043446            131
seed7/prg/fib.sd7            0.036258             47
seed7/prg/find7.sd7          0.037784            133
seed7/prg/findchar.sd7       0.041870            149
seed7/prg/fractree.sd7       0.036257             55
seed7/prg/ftp7.sd7           0.042047            296
seed7/prg/ftpserv.sd7        0.036881             74
seed7/prg/gcd.sd7            0.037069            109
seed7/prg/gkbd.sd7           0.045676            358
seed7/prg/gtksvtst.sd7       0.036914             94
seed7/prg/hal.sd7            0.039776            250
seed7/prg/hamu.sd7           0.047341            573
seed7/prg/hanoi.sd7          0.034670             55
seed7/prg/hd.sd7             0.035861             79
seed7/prg/hello.sd7          0.033997             32
seed7/prg/hilbert.sd7        0.035758            108
seed7/prg/ide7.sd7           0.040037            196
seed7/prg/kbd.sd7            0.034985             49
seed7/prg/klondike.sd7       0.053086            883
seed7/prg/lander.sd7         0.069694           1551
seed7/prg/lst80bas.sd7       0.042487            344
seed7/prg/lst99bas.sd7       0.044418            401
seed7/prg/lstgwbas.sd7       0.049504            577
seed7/prg/mahjong.sd7        0.078187           1943
seed7/prg/make7.sd7          0.038145            121
seed7/prg/mandelbr.sd7       0.041911            237
seed7/prg/mind.sd7           0.046219            443
seed7/prg/mirror.sd7         0.039090            131
seed7/prg/ms.sd7             0.048242            641
seed7/prg/nicoma.sd7         0.038167            135
seed7/prg/pac.sd7            0.049010            726
seed7/prg/pairs.sd7          0.082877           2025
seed7/prg/panic.sd7          0.097044           2634
seed7/prg/percolation.sd7    0.042906            330
seed7/prg/planets.sd7        0.077033           1486
seed7/prg/portfwd7.sd7       0.038703            139
seed7/prg/prime.sd7          0.037314             74
seed7/prg/printpi1.sd7       0.036186             56
seed7/prg/printpi2.sd7       0.035749             54
seed7/prg/printpi3.sd7       0.036586             60
seed7/prg/pv7.sd7            0.043565            337
seed7/prg/queen.sd7          0.037752            149
seed7/prg/rand.sd7           0.036169            121
seed7/prg/raytrace.sd7       0.047070            538
seed7/prg/rever.sd7          0.057017            816
seed7/prg/roman.sd7          0.040461             38
seed7/prg/s7c.sd7            0.275680           9060
seed7/prg/s7check.sd7        0.036449             68
seed7/prg/savehd7.sd7        0.068487           1110
seed7/prg/self.sd7           0.036806             49
seed7/prg/shisen.sd7         0.072435           1423
seed7/prg/sl.sd7             0.058989           1029
seed7/prg/snake.sd7          0.046920            615
seed7/prg/sokoban.sd7        0.053790            891
seed7/prg/spigotpi.sd7       0.035848             64
seed7/prg/sql7.sd7           0.041244            278
seed7/prg/startrek.sd7       0.058791            979
seed7/prg/sudoku7.sd7        0.098155           2657
seed7/prg/sydir7.sd7         0.044532            384
seed7/prg/syntaxhl.sd7       0.039445            177
seed7/prg/tak.sd7            0.035298             59
seed7/prg/tar7.sd7           0.036913            121
seed7/prg/tch.sd7            0.037254             55
seed7/prg/testfont.sd7       0.037199             95
seed7/prg/tet.sd7            0.044530            479
seed7/prg/tetg.sd7           0.045531            501
seed7/prg/toutf8.sd7         0.042341            240
seed7/prg/tst_cli.sd7        0.035362             40
seed7/prg/tst_srv.sd7        0.035683             47
seed7/prg/wator.sd7          0.052749            651
seed7/prg/which.sd7          0.037569             65
seed7/prg/wiz.sd7            0.103775           2833
seed7/prg/wordcnt.sd7        0.038400             54
seed7/prg/wrinum.sd7         0.042730             43
seed7/prg/wumpus.sd7         0.043361            372
seed7/lib/aes.s7i            0.112181           1144
seed7/lib/aes_gcm.s7i        0.049922            392
seed7/lib/ar.s7i             0.072231           1532
seed7/lib/arc4.s7i           0.037690            144
seed7/lib/archive.s7i        0.047491            143
seed7/lib/archive_base.s7i   0.040981            135
seed7/lib/array.s7i          0.054277            610
seed7/lib/asn1.s7i           0.045850            544
seed7/lib/asn1oid.s7i        0.042644            157
seed7/lib/basearray.s7i      0.048933            450
seed7/lib/bigfile.s7i        0.037863            136
seed7/lib/bigint.s7i         0.054944            824
seed7/lib/bigrat.s7i         0.054017            784
seed7/lib/bin16.s7i          0.050476            592
seed7/lib/bin32.s7i          0.049110            490
seed7/lib/bin64.s7i          0.050476            539
seed7/lib/bitdata.s7i        0.077267           1330
seed7/lib/bitmapfont.s7i     0.040278            215
seed7/lib/bitset.s7i         0.048864            593
seed7/lib/bitsetof.s7i       0.047619            431
seed7/lib/blowfish.s7i       0.055643            383
seed7/lib/bmp.s7i            0.060753            924
seed7/lib/boolean.s7i        0.045424            403
seed7/lib/browser.s7i        0.043243            280
seed7/lib/bstring.s7i        0.041037            227
seed7/lib/bytedata.s7i       0.049022            482
seed7/lib/bzip2.s7i          0.057166            887
seed7/lib/cards.s7i          0.064635           1342
seed7/lib/category.s7i       0.039844            209
seed7/lib/cc_conf.s7i        0.077475           1314
seed7/lib/ccittfax.s7i       0.064639           1022
seed7/lib/cgi.s7i            0.037073            109
seed7/lib/cgidialog.s7i      0.058315           1118
seed7/lib/char.s7i           0.042066            356
seed7/lib/charsets.s7i       0.082274           2024
seed7/lib/chartype.s7i       0.038352            121
seed7/lib/cipher.s7i         0.037558            146
seed7/lib/cli_cmds.s7i       0.068630           1360
seed7/lib/clib_file.s7i      0.043526            301
seed7/lib/color.s7i          0.040147            185
seed7/lib/complex.s7i        0.044942            464
seed7/lib/compress.s7i       0.039180            150
seed7/lib/console.s7i        0.039660            188
seed7/lib/cpio.s7i           0.081745           1708
seed7/lib/crc32.s7i          0.042899            193
seed7/lib/cronos16.s7i       0.097094           1173
seed7/lib/cronos27.s7i       0.117744           1464
seed7/lib/csv.s7i            0.041251            201
seed7/lib/db_prop.s7i        0.062969            991
seed7/lib/deflate.s7i        0.056901            740
seed7/lib/des.s7i            0.057830            444
seed7/lib/dialog.s7i         0.052570            311
seed7/lib/dir.s7i            0.038556            163
seed7/lib/draw.s7i           0.056374            854
seed7/lib/duration.s7i       0.060573           1038
seed7/lib/echo.s7i           0.038466            132
seed7/lib/editline.s7i       0.045346            398
seed7/lib/elf.s7i            0.085742           1560
seed7/lib/elliptic.s7i       0.052779            649
seed7/lib/enable_io.s7i      0.043986            312
seed7/lib/encoding.s7i       0.060934            931
seed7/lib/enumeration.s7i    0.040792            236
seed7/lib/environment.s7i    0.038137            175
seed7/lib/exif.s7i           0.038658            152
seed7/lib/external_file.s7i  0.042156            340
seed7/lib/field.s7i          0.040529            268
seed7/lib/file.s7i           0.045611            372
seed7/lib/filebits.s7i       0.036911             46
seed7/lib/filesys.s7i        0.048798            601
seed7/lib/fileutil.s7i       0.038271            144
seed7/lib/fixarray.s7i       0.045843            307
seed7/lib/float.s7i          0.058924            757
seed7/lib/font.s7i           0.042994            196
seed7/lib/font8x8.s7i        0.053227            998
seed7/lib/forloop.s7i        0.047978            449
seed7/lib/ftp.s7i            0.055620            969
seed7/lib/ftpserv.s7i        0.050798            631
seed7/lib/getf.s7i           0.037550            115
seed7/lib/gethttp.s7i        0.035592             41
seed7/lib/gethttps.s7i       0.036995             41
seed7/lib/gif.s7i            0.050772            561
seed7/lib/graph.s7i          0.049194            415
seed7/lib/graph_file.s7i     0.044727            399
seed7/lib/gtkserver.s7i      0.038607            161
seed7/lib/gzip.s7i           0.048927            573
seed7/lib/hash.s7i           0.048509            421
seed7/lib/hashsetof.s7i      0.047635            499
seed7/lib/hmac.s7i           0.037386            152
seed7/lib/html.s7i           0.035472             83
seed7/lib/html_ent.s7i       0.047188            476
seed7/lib/htmldom.s7i        0.044258            286
seed7/lib/http_request.s7i   0.051843            696
seed7/lib/http_srv_resp.s7i  0.045366            380
seed7/lib/https_request.s7i  0.040112            211
seed7/lib/httpserv.s7i       0.043875            345
seed7/lib/huffman.s7i        0.052981            644
seed7/lib/ico.s7i            0.040961            221
seed7/lib/idxarray.s7i       0.041468            232
seed7/lib/image.s7i          0.037004            156
seed7/lib/imagefile.s7i      0.038820            171
seed7/lib/inflate.s7i        0.046167            411
seed7/lib/inifile.s7i        0.037796            129
seed7/lib/integer.s7i        0.052331            663
seed7/lib/iobuffer.s7i       0.044225            289
seed7/lib/jpeg.s7i           0.082758           1761
seed7/lib/json.s7i           0.054646            891
seed7/lib/json_serde.s7i     0.051830            783
seed7/lib/keybd.s7i          0.054742            639
seed7/lib/keydescr.s7i       0.039943            192
seed7/lib/leb128.s7i         0.038829            218
seed7/lib/line.s7i           0.038060            164
seed7/lib/listener.s7i       0.040038            247
seed7/lib/logfile.s7i        0.036012             73
seed7/lib/lower.s7i          0.038004            142
seed7/lib/lzma.s7i           0.059671            934
seed7/lib/lzw.s7i            0.058979            861
seed7/lib/magic.s7i          0.048301            403
seed7/lib/mahjng32.s7i       0.063877           1500
seed7/lib/make.s7i           0.049398            544
seed7/lib/makedata.s7i       0.071465           1428
seed7/lib/math.s7i           0.041664            201
seed7/lib/mixarith.s7i       0.040109            249
seed7/lib/modern27.s7i       0.083863           1099
seed7/lib/more.s7i           0.038212            130
seed7/lib/msgdigest.s7i      0.078645           1222
seed7/lib/multiscr.s7i       0.036603             68
seed7/lib/null_file.s7i      0.042479            345
seed7/lib/osfiles.s7i        0.065290           1085
seed7/lib/pbm.s7i            0.040597            230
seed7/lib/pcx.s7i            0.050384            638
seed7/lib/pem.s7i            0.038262            185
seed7/lib/pgm.s7i            0.039435            238
seed7/lib/pic16.s7i          0.048088           1037
seed7/lib/pic32.s7i          0.079470           2060
seed7/lib/pic_util.s7i       0.038885            144
seed7/lib/pixelimage.s7i     0.042861            320
seed7/lib/pixmap_file.s7i    0.046062            459
seed7/lib/pixmapfont.s7i     0.041925            184
seed7/lib/pkcs1.s7i          0.058536            543
seed7/lib/png.s7i            0.063093           1064
seed7/lib/poll.s7i           0.043907            313
seed7/lib/ppm.s7i            0.039591            240
seed7/lib/process.s7i        0.048024            541
seed7/lib/progs.s7i          0.054949            789
seed7/lib/propertyfile.s7i   0.037614            155
seed7/lib/rational.s7i       0.052989            792
seed7/lib/ref_list.s7i       0.041258            252
seed7/lib/reference.s7i      0.038237            126
seed7/lib/reverse.s7i        0.036739             94
seed7/lib/rpm.s7i            0.142695           3487
seed7/lib/rpmext.s7i         0.041432            318
seed7/lib/scanfile.s7i       0.079286           1779
seed7/lib/scanjson.s7i       0.045550            413
seed7/lib/scanstri.s7i       0.080312           1814
seed7/lib/scantoml.s7i       0.071453           1603
seed7/lib/seed7_05.s7i       0.066427           1072
seed7/lib/set.s7i            0.036281             57
seed7/lib/shell.s7i          0.053146            615
seed7/lib/showtls.s7i        0.054920            678
seed7/lib/signature.s7i      0.037953            131
seed7/lib/smtp.s7i           0.040560            261
seed7/lib/sockbase.s7i       0.042098            217
seed7/lib/socket.s7i         0.043159            326
seed7/lib/sokoban1.s7i       0.054264           1519
seed7/lib/sql_base.s7i       0.070327           1000
seed7/lib/stars.s7i          0.134555           1705
seed7/lib/stdfont10.s7i      0.081001           3347
seed7/lib/stdfont12.s7i      0.092269           3928
seed7/lib/stdfont14.s7i      0.107766           4510
seed7/lib/stdfont16.s7i      0.114010           5092
seed7/lib/stdfont18.s7i      0.134331           5868
seed7/lib/stdfont20.s7i      0.147698           6449
seed7/lib/stdfont24.s7i      0.178325           7421
seed7/lib/stdfont8.s7i       0.074892           2960
seed7/lib/stdfont9.s7i       0.076384           3152
seed7/lib/stdio.s7i          0.038678            192
seed7/lib/strifile.s7i       0.042152            345
seed7/lib/string.s7i         0.054311            779
seed7/lib/stritext.s7i       0.041658            352
seed7/lib/struct.s7i         0.042770            266
seed7/lib/struct_elem.s7i    0.042231            129
seed7/lib/subfile.s7i        0.043402            174
seed7/lib/subrange.s7i       0.037673             78
seed7/lib/syntax.s7i         0.049742            294
seed7/lib/tar.s7i            0.084348           1880
seed7/lib/tar_cmds.s7i       0.056327            752
seed7/lib/tdes.s7i           0.038739            143
seed7/lib/tee.s7i            0.038931            143
seed7/lib/text.s7i           0.037600            135
seed7/lib/tga.s7i            0.053364            676
seed7/lib/tiff.s7i           0.122262           2771
seed7/lib/time.s7i           0.063058           1191
seed7/lib/tls.s7i            0.107130           2230
seed7/lib/unicode.s7i        0.051978            575
seed7/lib/unionfnd.s7i       0.037037            130
seed7/lib/upper.s7i          0.037497            142
seed7/lib/utf16.s7i          0.048609            540
seed7/lib/utf8.s7i           0.040105            234
seed7/lib/vecfont10.s7i      0.077543           1056
seed7/lib/vecfont18.s7i      0.089442           1119
seed7/lib/vector3d.s7i       0.044045            293
seed7/lib/vectorfont.s7i     0.041822            239
seed7/lib/wildcard.s7i       0.039104            140
seed7/lib/window.s7i         0.046318            455
seed7/lib/wrinum.s7i         0.041427            248
seed7/lib/x509cert.s7i       0.071298           1243
seed7/lib/xml_ent.s7i        0.038104             94
seed7/lib/xmldom.s7i         0.041618            303
seed7/lib/xz.s7i             0.045837            442
seed7/lib/zip.s7i            0.118783           2792
seed7/lib/zstd.s7i           0.068849           1263
============================ ================= ================

Statistical Summary — Mode C (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.088481        |
+-----------+-----------------+
| Minimum   | 0.018444        |
+-----------+-----------------+
| Maximum   | 2.499137        |
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
seed7/prg/.#bas7.sd7         0.018224              1
seed7/prg/addup.sd7          0.049985            190
seed7/prg/bas7.sd7           0.763757          11459
seed7/prg/bifurk.sd7         0.038332             73
seed7/prg/bigfiles.sd7       0.041621            129
seed7/prg/brainf7.sd7        0.037394             86
seed7/prg/calc7.sd7          0.040279            128
seed7/prg/carddemo.sd7       0.045135            190
seed7/prg/castle.sd7         0.214052           3148
seed7/prg/cat.sd7            0.038181             82
seed7/prg/cellauto.sd7       0.038393             85
seed7/prg/celsius.sd7        0.036724             42
seed7/prg/chk_all.sd7        0.082977            843
seed7/prg/chkarr.sd7         0.874102           8367
seed7/prg/chkbig.sd7         4.238042          29026
seed7/prg/chkbin.sd7         1.053360           6469
seed7/prg/chkbitdata.sd7     1.282404           6624
seed7/prg/chkbool.sd7        0.236312           3157
seed7/prg/chkbst.sd7         0.105941            722
seed7/prg/chkchr.sd7         0.496806           2809
seed7/prg/chkcmd.sd7         0.116113           1205
seed7/prg/chkdb.sd7          0.775085           7454
seed7/prg/chkdecl.sd7        0.097502            448
seed7/prg/chkenum.sd7        0.125203           1230
seed7/prg/chkerr.sd7         0.353229           4663
seed7/prg/chkexc.sd7         0.154681           2627
seed7/prg/chkfil.sd7         0.135034           1615
seed7/prg/chkflt.sd7         2.927997          20620
seed7/prg/chkhent.sd7        0.043614             54
seed7/prg/chkhsh.sd7         0.523669           4548
seed7/prg/chkidx.sd7         3.289965          19567
seed7/prg/chkint.sd7         5.752917          38129
seed7/prg/chkjson.sd7        0.193184           1764
seed7/prg/chkovf.sd7         1.246388           8216
seed7/prg/chkprc.sd7         0.723060          10111
seed7/prg/chkscan.sd7        0.095687            714
seed7/prg/chkset.sd7         1.804793          11974
seed7/prg/chkstr.sd7         3.518682          26952
seed7/prg/chktime.sd7        0.254326           2025
seed7/prg/chktoml.sd7        0.203720           1656
seed7/prg/clock.sd7          0.038938             47
seed7/prg/clock2.sd7         0.038347             43
seed7/prg/clock3.sd7         0.043073             95
seed7/prg/cmpfil.sd7         0.039257             84
seed7/prg/comanche.sd7       0.046651            180
seed7/prg/confval.sd7        0.051149            175
seed7/prg/db7.sd7            0.063776            417
seed7/prg/diff7.sd7          0.054112            263
seed7/prg/dirtst.sd7         0.038972             42
seed7/prg/dirx.sd7           0.044747            152
seed7/prg/dnafight.sd7       0.123597           1381
seed7/prg/dragon.sd7         0.039970             73
seed7/prg/echo.sd7           0.037804             39
seed7/prg/eliza.sd7          0.054020            302
seed7/prg/err.sd7            0.047643             96
seed7/prg/fannkuch.sd7       0.050051            131
seed7/prg/fib.sd7            0.039338             47
seed7/prg/find7.sd7          0.043323            133
seed7/prg/findchar.sd7       0.044564            149
seed7/prg/fractree.sd7       0.039732             55
seed7/prg/ftp7.sd7           0.055652            296
seed7/prg/ftpserv.sd7        0.041357             74
seed7/prg/gcd.sd7            0.042346            109
seed7/prg/gkbd.sd7           0.064636            358
seed7/prg/gtksvtst.sd7       0.041174             94
seed7/prg/hal.sd7            0.049484            250
seed7/prg/hamu.sd7           0.069819            573
seed7/prg/hanoi.sd7          0.039147             55
seed7/prg/hd.sd7             0.041419             79
seed7/prg/hello.sd7          0.039680             32
seed7/prg/hilbert.sd7        0.044163            108
seed7/prg/ide7.sd7           0.050948            196
seed7/prg/kbd.sd7            0.039827             49
seed7/prg/klondike.sd7       0.092085            883
seed7/prg/lander.sd7         0.137633           1551
seed7/prg/lst80bas.sd7       0.058023            344
seed7/prg/lst99bas.sd7       0.062308            401
seed7/prg/lstgwbas.sd7       0.075910            577
seed7/prg/mahjong.sd7        0.152127           1943
seed7/prg/make7.sd7          0.044497            121
seed7/prg/mandelbr.sd7       0.049972            237
seed7/prg/mind.sd7           0.060014            443
seed7/prg/mirror.sd7         0.044809            131
seed7/prg/ms.sd7             0.070803            641
seed7/prg/nicoma.sd7         0.043872            135
seed7/prg/pac.sd7            0.073820            726
seed7/prg/pairs.sd7          0.142710           2025
seed7/prg/panic.sd7          0.198408           2634
seed7/prg/percolation.sd7    0.057734            330
seed7/prg/planets.sd7        0.141259           1486
seed7/prg/portfwd7.sd7       0.045071            139
seed7/prg/prime.sd7          0.040612             74
seed7/prg/printpi1.sd7       0.042624             56
seed7/prg/printpi2.sd7       0.041732             54
seed7/prg/printpi3.sd7       0.038996             60
seed7/prg/pv7.sd7            0.059322            337
seed7/prg/queen.sd7          0.043587            149
seed7/prg/rand.sd7           0.041513            121
seed7/prg/raytrace.sd7       0.073593            538
seed7/prg/rever.sd7          0.086077            816
seed7/prg/roman.sd7          0.038707             38
seed7/prg/s7c.sd7            0.644268           9060
seed7/prg/s7check.sd7        0.039051             68
seed7/prg/savehd7.sd7        0.115965           1110
seed7/prg/self.sd7           0.039299             49
seed7/prg/shisen.sd7         0.125078           1423
seed7/prg/sl.sd7             0.101093           1029
seed7/prg/snake.sd7          0.068012            615
seed7/prg/sokoban.sd7        0.085794            891
seed7/prg/spigotpi.sd7       0.039343             64
seed7/prg/sql7.sd7           0.053919            278
seed7/prg/startrek.sd7       0.096327            979
seed7/prg/sudoku7.sd7        0.207310           2657
seed7/prg/sydir7.sd7         0.067411            384
seed7/prg/syntaxhl.sd7       0.050287            177
seed7/prg/tak.sd7            0.038929             59
seed7/prg/tar7.sd7           0.042787            121
seed7/prg/tch.sd7            0.037807             55
seed7/prg/testfont.sd7       0.041777             95
seed7/prg/tet.sd7            0.062587            479
seed7/prg/tetg.sd7           0.066767            501
seed7/prg/toutf8.sd7         0.054490            240
seed7/prg/tst_cli.sd7        0.037584             40
seed7/prg/tst_srv.sd7        0.039103             47
seed7/prg/wator.sd7          0.081345            651
seed7/prg/which.sd7          0.040182             65
seed7/prg/wiz.sd7            0.217310           2833
seed7/prg/wordcnt.sd7        0.040544             54
seed7/prg/wrinum.sd7         0.037202             43
seed7/prg/wumpus.sd7         0.054710            372
seed7/lib/aes.s7i            0.201839           1144
seed7/lib/aes_gcm.s7i        0.063112            392
seed7/lib/ar.s7i             0.126886           1532
seed7/lib/arc4.s7i           0.046260            144
seed7/lib/archive.s7i        0.054157            143
seed7/lib/archive_base.s7i   0.047520            135
seed7/lib/array.s7i          0.078520            610
seed7/lib/asn1.s7i           0.066092            544
seed7/lib/asn1oid.s7i        0.051952            157
seed7/lib/basearray.s7i      0.066358            450
seed7/lib/bigfile.s7i        0.046650            136
seed7/lib/bigint.s7i         0.080704            824
seed7/lib/bigrat.s7i         0.081417            784
seed7/lib/bin16.s7i          0.069976            592
seed7/lib/bin32.s7i          0.070163            490
seed7/lib/bin64.s7i          0.069739            539
seed7/lib/bitdata.s7i        0.128558           1330
seed7/lib/bitmapfont.s7i     0.049132            215
seed7/lib/bitset.s7i         0.068161            593
seed7/lib/bitsetof.s7i       0.063268            431
seed7/lib/blowfish.s7i       0.080773            383
seed7/lib/bmp.s7i            0.101651            924
seed7/lib/boolean.s7i        0.056618            403
seed7/lib/browser.s7i        0.056269            280
seed7/lib/bstring.s7i        0.059003            227
seed7/lib/bytedata.s7i       0.074069            482
seed7/lib/bzip2.s7i          0.093179            887
seed7/lib/cards.s7i          0.110459           1342
seed7/lib/category.s7i       0.053208            209
seed7/lib/cc_conf.s7i        0.124850           1314
seed7/lib/ccittfax.s7i       0.104319           1022
seed7/lib/cgi.s7i            0.043551            109
seed7/lib/cgidialog.s7i      0.102509           1118
seed7/lib/char.s7i           0.056985            356
seed7/lib/charsets.s7i       0.137349           2024
seed7/lib/chartype.s7i       0.058704            121
seed7/lib/cipher.s7i         0.049671            146
seed7/lib/cli_cmds.s7i       0.125518           1360
seed7/lib/clib_file.s7i      0.059665            301
seed7/lib/color.s7i          0.054778            185
seed7/lib/complex.s7i        0.066540            464
seed7/lib/compress.s7i       0.050950            150
seed7/lib/console.s7i        0.053371            188
seed7/lib/cpio.s7i           0.154722           1708
seed7/lib/crc32.s7i          0.056069            193
seed7/lib/cronos16.s7i       0.204038           1173
seed7/lib/cronos27.s7i       0.267047           1464
seed7/lib/csv.s7i            0.052247            201
seed7/lib/db_prop.s7i        0.113238            991
seed7/lib/deflate.s7i        0.094099            740
seed7/lib/des.s7i            0.095234            444
seed7/lib/dialog.s7i         0.068601            311
seed7/lib/dir.s7i            0.056202            163
seed7/lib/draw.s7i           0.094504            854
seed7/lib/duration.s7i       0.101555           1038
seed7/lib/echo.s7i           0.044891            132
seed7/lib/editline.s7i       0.063169            398
seed7/lib/elf.s7i            0.166235           1560
seed7/lib/elliptic.s7i       0.083095            649
seed7/lib/enable_io.s7i      0.053809            312
seed7/lib/encoding.s7i       0.102567            931
seed7/lib/enumeration.s7i    0.052499            236
seed7/lib/environment.s7i    0.044058            175
seed7/lib/exif.s7i           0.047132            152
seed7/lib/external_file.s7i  0.058980            340
seed7/lib/field.s7i          0.053002            268
seed7/lib/file.s7i           0.057613            372
seed7/lib/filebits.s7i       0.040655             46
seed7/lib/filesys.s7i        0.068459            601
seed7/lib/fileutil.s7i       0.050969            144
seed7/lib/fixarray.s7i       0.060880            307
seed7/lib/float.s7i          0.078373            757
seed7/lib/font.s7i           0.046303            196
seed7/lib/font8x8.s7i        0.070599            998
seed7/lib/forloop.s7i        0.060566            449
seed7/lib/ftp.s7i            0.090697            969
seed7/lib/ftpserv.s7i        0.078463            631
seed7/lib/getf.s7i           0.042330            115
seed7/lib/gethttp.s7i        0.038615             41
seed7/lib/gethttps.s7i       0.040153             41
seed7/lib/gif.s7i            0.074963            561
seed7/lib/graph.s7i          0.065392            415
seed7/lib/graph_file.s7i     0.058419            399
seed7/lib/gtkserver.s7i      0.042062            161
seed7/lib/gzip.s7i           0.069296            573
seed7/lib/hash.s7i           0.075308            421
seed7/lib/hashsetof.s7i      0.069807            499
seed7/lib/hmac.s7i           0.052039            152
seed7/lib/html.s7i           0.043630             83
seed7/lib/html_ent.s7i       0.067489            476
seed7/lib/htmldom.s7i        0.069698            286
seed7/lib/http_request.s7i   0.090737            696
seed7/lib/http_srv_resp.s7i  0.071751            380
seed7/lib/https_request.s7i  0.052414            211
seed7/lib/httpserv.s7i       0.060783            345
seed7/lib/huffman.s7i        0.079139            644
seed7/lib/ico.s7i            0.054307            221
seed7/lib/idxarray.s7i       0.057252            232
seed7/lib/image.s7i          0.045832            156
seed7/lib/imagefile.s7i      0.045168            171
seed7/lib/inflate.s7i        0.068367            411
seed7/lib/inifile.s7i        0.045532            129
seed7/lib/integer.s7i        0.073753            663
seed7/lib/iobuffer.s7i       0.053550            289
seed7/lib/jpeg.s7i           0.162319           1761
seed7/lib/json.s7i           0.089264            891
seed7/lib/json_serde.s7i     0.086903            783
seed7/lib/keybd.s7i          0.084496            639
seed7/lib/keydescr.s7i       0.055931            192
seed7/lib/leb128.s7i         0.051067            218
seed7/lib/line.s7i           0.046206            164
seed7/lib/listener.s7i       0.052694            247
seed7/lib/logfile.s7i        0.040389             73
seed7/lib/lower.s7i          0.042656            142
seed7/lib/lzma.s7i           0.099278            934
seed7/lib/lzw.s7i            0.094001            861
seed7/lib/magic.s7i          0.065483            403
seed7/lib/mahjng32.s7i       0.097639           1500
seed7/lib/make.s7i           0.077934            544
seed7/lib/makedata.s7i       0.127309           1428
seed7/lib/math.s7i           0.046655            201
seed7/lib/mixarith.s7i       0.053202            249
seed7/lib/modern27.s7i       0.178353           1099
seed7/lib/more.s7i           0.044332            130
seed7/lib/msgdigest.s7i      0.142289           1222
seed7/lib/multiscr.s7i       0.040009             68
seed7/lib/null_file.s7i      0.053074            345
seed7/lib/osfiles.s7i        0.099312           1085
seed7/lib/pbm.s7i            0.048756            230
seed7/lib/pcx.s7i            0.078218            638
seed7/lib/pem.s7i            0.046352            185
seed7/lib/pgm.s7i            0.051215            238
seed7/lib/pic16.s7i          0.069116           1037
seed7/lib/pic32.s7i          0.125624           2060
seed7/lib/pic_util.s7i       0.044042            144
seed7/lib/pixelimage.s7i     0.053569            320
seed7/lib/pixmap_file.s7i    0.063444            459
seed7/lib/pixmapfont.s7i     0.047902            184
seed7/lib/pkcs1.s7i          0.079610            543
seed7/lib/png.s7i            0.110410           1064
seed7/lib/poll.s7i           0.054625            313
seed7/lib/ppm.s7i            0.050774            240
seed7/lib/process.s7i        0.066474            541
seed7/lib/progs.s7i          0.081212            789
seed7/lib/propertyfile.s7i   0.045042            155
seed7/lib/rational.s7i       0.079673            792
seed7/lib/ref_list.s7i       0.050728            252
seed7/lib/reference.s7i      0.044013            126
seed7/lib/reverse.s7i        0.041497             94
seed7/lib/rpm.s7i            0.295474           3487
seed7/lib/rpmext.s7i         0.054249            318
seed7/lib/scanfile.s7i       0.138357           1779
seed7/lib/scanjson.s7i       0.063130            413
seed7/lib/scanstri.s7i       0.142239           1814
seed7/lib/scantoml.s7i       0.134093           1603
seed7/lib/seed7_05.s7i       0.113710           1072
seed7/lib/set.s7i            0.038807             57
seed7/lib/shell.s7i          0.071383            615
seed7/lib/showtls.s7i        0.087836            678
seed7/lib/signature.s7i      0.044458            131
seed7/lib/smtp.s7i           0.050502            261
seed7/lib/sockbase.s7i       0.052028            217
seed7/lib/socket.s7i         0.054527            326
seed7/lib/sokoban1.s7i       0.084819           1519
seed7/lib/sql_base.s7i       0.111786           1000
seed7/lib/stars.s7i          0.245407           1705
seed7/lib/stdfont10.s7i      0.146846           3347
seed7/lib/stdfont12.s7i      0.175563           3928
seed7/lib/stdfont14.s7i      0.194005           4510
seed7/lib/stdfont16.s7i      0.218201           5092
seed7/lib/stdfont18.s7i      0.251531           5868
seed7/lib/stdfont20.s7i      0.279401           6449
seed7/lib/stdfont24.s7i      0.341210           7421
seed7/lib/stdfont8.s7i       0.131837           2960
seed7/lib/stdfont9.s7i       0.138361           3152
seed7/lib/stdio.s7i          0.046226            192
seed7/lib/strifile.s7i       0.056170            345
seed7/lib/string.s7i         0.077920            779
seed7/lib/stritext.s7i       0.055582            352
seed7/lib/struct.s7i         0.055479            266
seed7/lib/struct_elem.s7i    0.042346            129
seed7/lib/subfile.s7i        0.045092            174
seed7/lib/subrange.s7i       0.040094             78
seed7/lib/syntax.s7i         0.061689            294
seed7/lib/tar.s7i            0.149956           1880
seed7/lib/tar_cmds.s7i       0.087594            752
seed7/lib/tdes.s7i           0.045218            143
seed7/lib/tee.s7i            0.043648            143
seed7/lib/text.s7i           0.044192            135
seed7/lib/tga.s7i            0.083488            676
seed7/lib/tiff.s7i           0.260865           2771
seed7/lib/time.s7i           0.105274           1191
seed7/lib/tls.s7i            0.200714           2230
seed7/lib/unicode.s7i        0.076228            575
seed7/lib/unionfnd.s7i       0.044239            130
seed7/lib/upper.s7i          0.043739            142
seed7/lib/utf16.s7i          0.068451            540
seed7/lib/utf8.s7i           0.049222            234
seed7/lib/vecfont10.s7i      0.165778           1056
seed7/lib/vecfont18.s7i      0.183715           1119
seed7/lib/vector3d.s7i       0.051070            293
seed7/lib/vectorfont.s7i     0.049020            239
seed7/lib/wildcard.s7i       0.043603            140
seed7/lib/window.s7i         0.062114            455
seed7/lib/wrinum.s7i         0.050764            248
seed7/lib/x509cert.s7i       0.120232           1243
seed7/lib/xml_ent.s7i        0.040991             94
seed7/lib/xmldom.s7i         0.053475            303
seed7/lib/xz.s7i             0.063127            442
seed7/lib/zip.s7i            0.243454           2792
seed7/lib/zstd.s7i           0.122399           1263
============================ ================= ================

Statistical Summary — Mode D (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.164365        |
+-----------+-----------------+
| Minimum   | 0.018224        |
+-----------+-----------------+
| Maximum   | 5.752917        |
+-----------+-----------------+

Cross-Mode Comparison
=====================

+------+-----------------+-----------------+-----------------+
| Mode | Average (s)     | Minimum (s)     | Maximum (s)     |
+======+=================+=================+=================+
| A    | 0.035360        | 0.018352        | 0.081123        |
+------+-----------------+-----------------+-----------------+
| B    | 0.039229        | 0.018322        | 0.046270        |
+------+-----------------+-----------------+-----------------+
| C    | 0.088481        | 0.018444        | 2.499137        |
+------+-----------------+-----------------+-----------------+
| D    | 0.164365        | 0.018224        | 5.752917        |
+------+-----------------+-----------------+-----------------+

Phase Timing Summary
====================

Wall-clock times include warm-up work and GC performed by the warm-up helper.

+----------+--------------+--------------+--------------+
| Phase    | Warm-up      | Timed pass   | Mode total   |
+==========+==============+==============+==============+
| Mode A   | 00:00:14.762 | 00:01:00.424 | 00:01:15.186 |
+----------+--------------+--------------+--------------+
| Mode B   | 00:00:15.104 | 00:01:07.104 | 00:01:22.208 |
+----------+--------------+--------------+--------------+
| Mode C   | 00:00:36.130 | 00:02:32.059 | 00:03:08.190 |
+----------+--------------+--------------+--------------+
| Mode D   | 00:01:01.072 | 00:04:41.798 | 00:05:42.871 |
+----------+--------------+--------------+--------------+
| Total    |              |              | 00:11:28.463 |
+----------+--------------+--------------+--------------+


.. ---------------------------------------------------------------------------
