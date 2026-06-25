=======================================================
GC-Controlled Benchmark Report: Seed7 Mode — Four Modes
=======================================================

:Running with: seed7-mode 2026-06-25T15:38:52+0000 W26-4
:Seed7 dir   : /Users/roup/my/dvo/seed7-repos/seed7/
:Started at: 11:43:11 local time
:Generated on: 2026-06-25 15:54:46 UTC
:N Iterations: 5  (mean of N timed opens per file)
:Window system: nil
:Display graphic: nil
:Frame size: 183x95 chars
:Window body: 183x93 chars
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
seed7/prg/addup.sd7          0.033857            190
seed7/prg/bas7.sd7           0.033562          11459
seed7/prg/bifurk.sd7         0.032619             73
seed7/prg/bigfiles.sd7       0.032688            129
seed7/prg/brainf7.sd7        0.039621             86
seed7/prg/calc7.sd7          0.037112            128
seed7/prg/carddemo.sd7       0.034076            190
seed7/prg/castle.sd7         0.034179           3148
seed7/prg/cat.sd7            0.033296             82
seed7/prg/cellauto.sd7       0.033621             85
seed7/prg/celsius.sd7        0.033239             42
seed7/prg/chk_all.sd7        0.033519            843
seed7/prg/chkarr.sd7         0.034050           8367
seed7/prg/chkbig.sd7         0.038142          29026
seed7/prg/chkbin.sd7         0.034350           6469
seed7/prg/chkbitdata.sd7     0.034008           6624
seed7/prg/chkbool.sd7        0.033487           3157
seed7/prg/chkbst.sd7         0.033589            722
seed7/prg/chkchr.sd7         0.034005           2809
seed7/prg/chkcmd.sd7         0.033445           1205
seed7/prg/chkdb.sd7          0.034435           7454
seed7/prg/chkdecl.sd7        0.033453            448
seed7/prg/chkenum.sd7        0.033337           1230
seed7/prg/chkerr.sd7         0.033887           4663
seed7/prg/chkexc.sd7         0.033614           2627
seed7/prg/chkfil.sd7         0.033353           1615
seed7/prg/chkflt.sd7         0.036446          20620
seed7/prg/chkhent.sd7        0.033130             54
seed7/prg/chkhsh.sd7         0.033864           4548
seed7/prg/chkidx.sd7         0.036386          19567
seed7/prg/chkint.sd7         0.039901          38129
seed7/prg/chkjson.sd7        0.033431           1764
seed7/prg/chkovf.sd7         0.033402           8216
seed7/prg/chkprc.sd7         0.033019          10111
seed7/prg/chkscan.sd7        0.032782            714
seed7/prg/chkset.sd7         0.033567          11974
seed7/prg/chkstr.sd7         0.036648          26952
seed7/prg/chktime.sd7        0.033390           2025
seed7/prg/chktoml.sd7        0.034554           1656
seed7/prg/clock.sd7          0.033468             47
seed7/prg/clock2.sd7         0.033902             43
seed7/prg/clock3.sd7         0.033622             95
seed7/prg/cmpfil.sd7         0.034037             84
seed7/prg/comanche.sd7       0.033403            180
seed7/prg/confval.sd7        0.033498            175
seed7/prg/db7.sd7            0.033199            417
seed7/prg/diff7.sd7          0.032456            263
seed7/prg/dirtst.sd7         0.032516             42
seed7/prg/dirx.sd7           0.032571            152
seed7/prg/dnafight.sd7       0.032777           1381
seed7/prg/dragon.sd7         0.032665             73
seed7/prg/echo.sd7           0.033532             39
seed7/prg/eliza.sd7          0.032512            302
seed7/prg/err.sd7            0.032941             96
seed7/prg/fannkuch.sd7       0.032832            131
seed7/prg/fib.sd7            0.033691             47
seed7/prg/find7.sd7          0.033254            133
seed7/prg/findchar.sd7       0.038559            149
seed7/prg/fractree.sd7       0.039916             55
seed7/prg/ftp7.sd7           0.036396            296
seed7/prg/ftpserv.sd7        0.034031             74
seed7/prg/gcd.sd7            0.033837            109
seed7/prg/gkbd.sd7           0.033837            358
seed7/prg/gtksvtst.sd7       0.032983             94
seed7/prg/hal.sd7            0.032510            250
seed7/prg/hamu.sd7           0.033532            573
seed7/prg/hanoi.sd7          0.037831             55
seed7/prg/hd.sd7             0.037068             79
seed7/prg/hello.sd7          0.034692             32
seed7/prg/hilbert.sd7        0.036216            108
seed7/prg/ide7.sd7           0.034123            196
seed7/prg/kbd.sd7            0.033466             49
seed7/prg/klondike.sd7       0.033410            883
seed7/prg/lander.sd7         0.033432           1551
seed7/prg/lst80bas.sd7       0.033304            344
seed7/prg/lst99bas.sd7       0.033493            401
seed7/prg/lstgwbas.sd7       0.033801            577
seed7/prg/mahjong.sd7        0.033507           1943
seed7/prg/make7.sd7          0.033799            121
seed7/prg/mandelbr.sd7       0.033144            237
seed7/prg/mind.sd7           0.033201            443
seed7/prg/mirror.sd7         0.033564            131
seed7/prg/ms.sd7             0.033352            641
seed7/prg/nicoma.sd7         0.033218            135
seed7/prg/pac.sd7            0.033363            726
seed7/prg/pairs.sd7          0.033438           2025
seed7/prg/panic.sd7          0.033142           2634
seed7/prg/percolation.sd7    0.033449            330
seed7/prg/planets.sd7        0.033559           1486
seed7/prg/portfwd7.sd7       0.033101            139
seed7/prg/prime.sd7          0.032967             74
seed7/prg/printpi1.sd7       0.033360             56
seed7/prg/printpi2.sd7       0.033053             54
seed7/prg/printpi3.sd7       0.033377             60
seed7/prg/pv7.sd7            0.033114            337
seed7/prg/queen.sd7          0.032402            149
seed7/prg/rand.sd7           0.032594            121
seed7/prg/raytrace.sd7       0.032828            538
seed7/prg/rever.sd7          0.032342            816
seed7/prg/roman.sd7          0.033422             38
seed7/prg/s7c.sd7            0.034070           9060
seed7/prg/s7check.sd7        0.033496             68
seed7/prg/savehd7.sd7        0.033551           1110
seed7/prg/self.sd7           0.033350             49
seed7/prg/shisen.sd7         0.033273           1423
seed7/prg/sl.sd7             0.037693           1029
seed7/prg/snake.sd7          0.037962            615
seed7/prg/sokoban.sd7        0.032528            891
seed7/prg/spigotpi.sd7       0.032289             64
seed7/prg/sql7.sd7           0.032239            278
seed7/prg/startrek.sd7       0.032414            979
seed7/prg/sudoku7.sd7        0.033235           2657
seed7/prg/sydir7.sd7         0.033201            384
seed7/prg/syntaxhl.sd7       0.032438            177
seed7/prg/tak.sd7            0.032231             59
seed7/prg/tar7.sd7           0.031949            121
seed7/prg/tch.sd7            0.033077             55
seed7/prg/testfont.sd7       0.033304             95
seed7/prg/tet.sd7            0.033665            479
seed7/prg/tetg.sd7           0.033222            501
seed7/prg/toutf8.sd7         0.033568            240
seed7/prg/tst_cli.sd7        0.033419             40
seed7/prg/tst_srv.sd7        0.033226             47
seed7/prg/wator.sd7          0.034167            651
seed7/prg/which.sd7          0.033740             65
seed7/prg/wiz.sd7            0.034789           2833
seed7/prg/wordcnt.sd7        0.033427             54
seed7/prg/wrinum.sd7         0.032730             43
seed7/prg/wumpus.sd7         0.032694            372
seed7/lib/aes.s7i            0.033113           1144
seed7/lib/aes_gcm.s7i        0.032874            392
seed7/lib/ar.s7i             0.033420           1532
seed7/lib/arc4.s7i           0.033581            144
seed7/lib/archive.s7i        0.033944            143
seed7/lib/archive_base.s7i   0.033222            135
seed7/lib/array.s7i          0.033632            610
seed7/lib/asn1.s7i           0.033806            544
seed7/lib/asn1oid.s7i        0.033506            157
seed7/lib/basearray.s7i      0.033408            450
seed7/lib/bigfile.s7i        0.033602            136
seed7/lib/bigint.s7i         0.033456            824
seed7/lib/bigrat.s7i         0.033551            784
seed7/lib/bin16.s7i          0.033434            592
seed7/lib/bin32.s7i          0.033313            490
seed7/lib/bin64.s7i          0.037957            539
seed7/lib/bitdata.s7i        0.035902           1330
seed7/lib/bitmapfont.s7i     0.033689            215
seed7/lib/bitset.s7i         0.033657            593
seed7/lib/bitsetof.s7i       0.034052            431
seed7/lib/blowfish.s7i       0.033465            383
seed7/lib/bmp.s7i            0.033489            924
seed7/lib/boolean.s7i        0.033736            403
seed7/lib/browser.s7i        0.033794            280
seed7/lib/bstring.s7i        0.033352            227
seed7/lib/bytedata.s7i       0.033635            482
seed7/lib/bzip2.s7i          0.033205            887
seed7/lib/cards.s7i          0.033082           1342
seed7/lib/category.s7i       0.032737            209
seed7/lib/cc_conf.s7i        0.033136           1314
seed7/lib/ccittfax.s7i       0.032596           1022
seed7/lib/cgi.s7i            0.032902            109
seed7/lib/cgidialog.s7i      0.033288           1118
seed7/lib/char.s7i           0.032617            356
seed7/lib/charsets.s7i       0.032639           2024
seed7/lib/chartype.s7i       0.034062            121
seed7/lib/cipher.s7i         0.033061            146
seed7/lib/cli_cmds.s7i       0.034616           1360
seed7/lib/clib_file.s7i      0.036398            301
seed7/lib/color.s7i          0.035840            185
seed7/lib/complex.s7i        0.035934            464
seed7/lib/compress.s7i       0.036111            150
seed7/lib/console.s7i        0.036219            188
seed7/lib/cpio.s7i           0.035564           1708
seed7/lib/crc32.s7i          0.034818            193
seed7/lib/cronos16.s7i       0.033676           1173
seed7/lib/cronos27.s7i       0.033482           1464
seed7/lib/csv.s7i            0.033542            201
seed7/lib/db_prop.s7i        0.033966            991
seed7/lib/deflate.s7i        0.035534            740
seed7/lib/des.s7i            0.037060            444
seed7/lib/dialog.s7i         0.033940            311
seed7/lib/dir.s7i            0.033844            163
seed7/lib/draw.s7i           0.033618            854
seed7/lib/duration.s7i       0.033487           1038
seed7/lib/echo.s7i           0.033522            132
seed7/lib/editline.s7i       0.033338            398
seed7/lib/elf.s7i            0.033608           1560
seed7/lib/elliptic.s7i       0.033483            649
seed7/lib/enable_io.s7i      0.033650            312
seed7/lib/encoding.s7i       0.033148            931
seed7/lib/enumeration.s7i    0.033359            236
seed7/lib/environment.s7i    0.032435            175
seed7/lib/exif.s7i           0.032891            152
seed7/lib/external_file.s7i  0.032663            340
seed7/lib/field.s7i          0.032309            268
seed7/lib/file.s7i           0.033306            372
seed7/lib/filebits.s7i       0.033302             46
seed7/lib/filesys.s7i        0.033613            601
seed7/lib/fileutil.s7i       0.033330            144
seed7/lib/fixarray.s7i       0.033495            307
seed7/lib/float.s7i          0.033666            757
seed7/lib/font.s7i           0.033483            196
seed7/lib/font8x8.s7i        0.033906            998
seed7/lib/forloop.s7i        0.033007            449
seed7/lib/ftp.s7i            0.032443            969
seed7/lib/ftpserv.s7i        0.032956            631
seed7/lib/getf.s7i           0.033433            115
seed7/lib/gethttp.s7i        0.033023             41
seed7/lib/gethttps.s7i       0.033699             41
seed7/lib/gif.s7i            0.032728            561
seed7/lib/graph.s7i          0.032594            415
seed7/lib/graph_file.s7i     0.033761            399
seed7/lib/gtkserver.s7i      0.036458            161
seed7/lib/gzip.s7i           0.032836            573
seed7/lib/hash.s7i           0.033958            421
seed7/lib/hashsetof.s7i      0.033992            499
seed7/lib/hmac.s7i           0.033445            152
seed7/lib/html.s7i           0.033330             83
seed7/lib/html_ent.s7i       0.033287            476
seed7/lib/htmldom.s7i        0.033575            286
seed7/lib/http_request.s7i   0.033012            696
seed7/lib/http_srv_resp.s7i  0.033023            380
seed7/lib/https_request.s7i  0.032576            211
seed7/lib/httpserv.s7i       0.032570            345
seed7/lib/huffman.s7i        0.032601            644
seed7/lib/ico.s7i            0.032647            221
seed7/lib/idxarray.s7i       0.038184            232
seed7/lib/image.s7i          0.036895            156
seed7/lib/imagefile.s7i      0.036718            171
seed7/lib/inflate.s7i        0.036396            411
seed7/lib/inifile.s7i        0.033777            129
seed7/lib/integer.s7i        0.033743            663
seed7/lib/iobuffer.s7i       0.034828            289
seed7/lib/jpeg.s7i           0.041438           1761
seed7/lib/json.s7i           0.036343            891
seed7/lib/json_serde.s7i     0.033709            783
seed7/lib/keybd.s7i          0.038597            639
seed7/lib/keydescr.s7i       0.037274            192
seed7/lib/leb128.s7i         0.034525            218
seed7/lib/line.s7i           0.034283            164
seed7/lib/listener.s7i       0.033995            247
seed7/lib/logfile.s7i        0.032660             73
seed7/lib/lower.s7i          0.032842            142
seed7/lib/lzma.s7i           0.032559            934
seed7/lib/lzw.s7i            0.032767            861
seed7/lib/magic.s7i          0.032644            403
seed7/lib/mahjng32.s7i       0.032508           1500
seed7/lib/make.s7i           0.033569            544
seed7/lib/makedata.s7i       0.033156           1428
seed7/lib/math.s7i           0.033048            201
seed7/lib/mixarith.s7i       0.032458            249
seed7/lib/modern27.s7i       0.033361           1099
seed7/lib/more.s7i           0.033083            130
seed7/lib/msgdigest.s7i      0.032608           1222
seed7/lib/multiscr.s7i       0.032620             68
seed7/lib/null_file.s7i      0.033095            345
seed7/lib/osfiles.s7i        0.035244           1085
seed7/lib/pbm.s7i            0.033120            230
seed7/lib/pcx.s7i            0.032813            638
seed7/lib/pem.s7i            0.032423            185
seed7/lib/pgm.s7i            0.033864            238
seed7/lib/pic16.s7i          0.033505           1037
seed7/lib/pic32.s7i          0.034430           2060
seed7/lib/pic_util.s7i       0.034327            144
seed7/lib/pixelimage.s7i     0.034060            320
seed7/lib/pixmap_file.s7i    0.033426            459
seed7/lib/pixmapfont.s7i     0.033895            184
seed7/lib/pkcs1.s7i          0.034016            543
seed7/lib/png.s7i            0.033356           1064
seed7/lib/poll.s7i           0.033347            313
seed7/lib/ppm.s7i            0.033443            240
seed7/lib/process.s7i        0.034007            541
seed7/lib/progs.s7i          0.036564            789
seed7/lib/propertyfile.s7i   0.035514            155
seed7/lib/rational.s7i       0.036404            792
seed7/lib/ref_list.s7i       0.032722            252
seed7/lib/reference.s7i      0.033897            126
seed7/lib/reverse.s7i        0.034117             94
seed7/lib/rpm.s7i            0.033860           3487
seed7/lib/rpmext.s7i         0.032704            318
seed7/lib/scanfile.s7i       0.032757           1779
seed7/lib/scanjson.s7i       0.032795            413
seed7/lib/scanstri.s7i       0.032575           1814
seed7/lib/scantoml.s7i       0.032345           1603
seed7/lib/seed7_05.s7i       0.032308           1072
seed7/lib/set.s7i            0.033791             57
seed7/lib/shell.s7i          0.033040            615
seed7/lib/showtls.s7i        0.033674            678
seed7/lib/signature.s7i      0.033625            131
seed7/lib/smtp.s7i           0.037365            261
seed7/lib/sockbase.s7i       0.034682            217
seed7/lib/socket.s7i         0.033982            326
seed7/lib/sokoban1.s7i       0.033620           1519
seed7/lib/sql_base.s7i       0.033441           1000
seed7/lib/stars.s7i          0.038702           1705
seed7/lib/stdfont10.s7i      0.036791           3347
seed7/lib/stdfont12.s7i      0.034403           3928
seed7/lib/stdfont14.s7i      0.033826           4510
seed7/lib/stdfont16.s7i      0.033673           5092
seed7/lib/stdfont18.s7i      0.032559           5868
seed7/lib/stdfont20.s7i      0.033673           6449
seed7/lib/stdfont24.s7i      0.033002           7421
seed7/lib/stdfont8.s7i       0.032292           2960
seed7/lib/stdfont9.s7i       0.032346           3152
seed7/lib/stdio.s7i          0.032212            192
seed7/lib/strifile.s7i       0.032640            345
seed7/lib/string.s7i         0.032664            779
seed7/lib/stritext.s7i       0.032537            352
seed7/lib/struct.s7i         0.032591            266
seed7/lib/struct_elem.s7i    0.033195            129
seed7/lib/subfile.s7i        0.033040            174
seed7/lib/subrange.s7i       0.032383             78
seed7/lib/syntax.s7i         0.032599            294
seed7/lib/tar.s7i            0.032697           1880
seed7/lib/tar_cmds.s7i       0.032903            752
seed7/lib/tdes.s7i           0.032409            143
seed7/lib/tee.s7i            0.032840            143
seed7/lib/text.s7i           0.034667            135
seed7/lib/tga.s7i            0.033910            676
seed7/lib/tiff.s7i           0.033670           2771
seed7/lib/time.s7i           0.033483           1191
seed7/lib/tls.s7i            0.033362           2230
seed7/lib/unicode.s7i        0.033522            575
seed7/lib/unionfnd.s7i       0.033360            130
seed7/lib/upper.s7i          0.033622            142
seed7/lib/utf16.s7i          0.033965            540
seed7/lib/utf8.s7i           0.033587            234
seed7/lib/vecfont10.s7i      0.033623           1056
seed7/lib/vecfont18.s7i      0.033352           1119
seed7/lib/vector3d.s7i       0.033639            293
seed7/lib/vectorfont.s7i     0.033459            239
seed7/lib/wildcard.s7i       0.033559            140
seed7/lib/window.s7i         0.033498            455
seed7/lib/wrinum.s7i         0.033361            248
seed7/lib/x509cert.s7i       0.033573           1243
seed7/lib/xml_ent.s7i        0.033285             94
seed7/lib/xmldom.s7i         0.033638            303
seed7/lib/xz.s7i             0.033627            442
seed7/lib/zip.s7i            0.033676           2792
seed7/lib/zstd.s7i           0.034137           1263
============================ ================= ================

Statistical Summary — Mode A (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.033800        |
+-----------+-----------------+
| Minimum   | 0.031949        |
+-----------+-----------------+
| Maximum   | 0.041438        |
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
seed7/prg/addup.sd7          0.039256            190
seed7/prg/bas7.sd7           0.041511          11459
seed7/prg/bifurk.sd7         0.037158             73
seed7/prg/bigfiles.sd7       0.038655            129
seed7/prg/brainf7.sd7        0.038247             86
seed7/prg/calc7.sd7          0.038218            128
seed7/prg/carddemo.sd7       0.038708            190
seed7/prg/castle.sd7         0.038106           3148
seed7/prg/cat.sd7            0.035960             82
seed7/prg/cellauto.sd7       0.036911             85
seed7/prg/celsius.sd7        0.035282             42
seed7/prg/chk_all.sd7        0.038642            843
seed7/prg/chkarr.sd7         0.040482           8367
seed7/prg/chkbig.sd7         0.044445          29026
seed7/prg/chkbin.sd7         0.039072           6469
seed7/prg/chkbitdata.sd7     0.040234           6624
seed7/prg/chkbool.sd7        0.038119           3157
seed7/prg/chkbst.sd7         0.038096            722
seed7/prg/chkchr.sd7         0.039029           2809
seed7/prg/chkcmd.sd7         0.039552           1205
seed7/prg/chkdb.sd7          0.040377           7454
seed7/prg/chkdecl.sd7        0.038465            448
seed7/prg/chkenum.sd7        0.038634           1230
seed7/prg/chkerr.sd7         0.039908           4663
seed7/prg/chkexc.sd7         0.039246           2627
seed7/prg/chkfil.sd7         0.040417           1615
seed7/prg/chkflt.sd7         0.044083          20620
seed7/prg/chkhent.sd7        0.036607             54
seed7/prg/chkhsh.sd7         0.040548           4548
seed7/prg/chkidx.sd7         0.041719          19567
seed7/prg/chkint.sd7         0.046477          38129
seed7/prg/chkjson.sd7        0.039522           1764
seed7/prg/chkovf.sd7         0.040032           8216
seed7/prg/chkprc.sd7         0.037960          10111
seed7/prg/chkscan.sd7        0.038495            714
seed7/prg/chkset.sd7         0.039889          11974
seed7/prg/chkstr.sd7         0.042438          26952
seed7/prg/chktime.sd7        0.039072           2025
seed7/prg/chktoml.sd7        0.039167           1656
seed7/prg/clock.sd7          0.038011             47
seed7/prg/clock2.sd7         0.036328             43
seed7/prg/clock3.sd7         0.039538             95
seed7/prg/cmpfil.sd7         0.037627             84
seed7/prg/comanche.sd7       0.038942            180
seed7/prg/confval.sd7        0.040303            175
seed7/prg/db7.sd7            0.039553            417
seed7/prg/diff7.sd7          0.039240            263
seed7/prg/dirtst.sd7         0.036168             42
seed7/prg/dirx.sd7           0.038891            152
seed7/prg/dnafight.sd7       0.039190           1381
seed7/prg/dragon.sd7         0.036249             73
seed7/prg/echo.sd7           0.035116             39
seed7/prg/eliza.sd7          0.037591            302
seed7/prg/err.sd7            0.040295             96
seed7/prg/fannkuch.sd7       0.037173            131
seed7/prg/fib.sd7            0.035214             47
seed7/prg/find7.sd7          0.037588            133
seed7/prg/findchar.sd7       0.037436            149
seed7/prg/fractree.sd7       0.035778             55
seed7/prg/ftp7.sd7           0.039217            296
seed7/prg/ftpserv.sd7        0.037847             74
seed7/prg/gcd.sd7            0.036630            109
seed7/prg/gkbd.sd7           0.039422            358
seed7/prg/gtksvtst.sd7       0.039177             94
seed7/prg/hal.sd7            0.039804            250
seed7/prg/hamu.sd7           0.041370            573
seed7/prg/hanoi.sd7          0.036918             55
seed7/prg/hd.sd7             0.038722             79
seed7/prg/hello.sd7          0.035952             32
seed7/prg/hilbert.sd7        0.038264            108
seed7/prg/ide7.sd7           0.039101            196
seed7/prg/kbd.sd7            0.035865             49
seed7/prg/klondike.sd7       0.038213            883
seed7/prg/lander.sd7         0.039313           1551
seed7/prg/lst80bas.sd7       0.038347            344
seed7/prg/lst99bas.sd7       0.039898            401
seed7/prg/lstgwbas.sd7       0.040687            577
seed7/prg/mahjong.sd7        0.039489           1943
seed7/prg/make7.sd7          0.039014            121
seed7/prg/mandelbr.sd7       0.038993            237
seed7/prg/mind.sd7           0.039110            443
seed7/prg/mirror.sd7         0.040175            131
seed7/prg/ms.sd7             0.041236            641
seed7/prg/nicoma.sd7         0.041050            135
seed7/prg/pac.sd7            0.038832            726
seed7/prg/pairs.sd7          0.039056           2025
seed7/prg/panic.sd7          0.040385           2634
seed7/prg/percolation.sd7    0.038307            330
seed7/prg/planets.sd7        0.038282           1486
seed7/prg/portfwd7.sd7       0.038123            139
seed7/prg/prime.sd7          0.036212             74
seed7/prg/printpi1.sd7       0.035274             56
seed7/prg/printpi2.sd7       0.037311             54
seed7/prg/printpi3.sd7       0.036864             60
seed7/prg/pv7.sd7            0.039942            337
seed7/prg/queen.sd7          0.039272            149
seed7/prg/rand.sd7           0.038298            121
seed7/prg/raytrace.sd7       0.038937            538
seed7/prg/rever.sd7          0.039060            816
seed7/prg/roman.sd7          0.036321             38
seed7/prg/s7c.sd7            0.039834           9060
seed7/prg/s7check.sd7        0.037753             68
seed7/prg/savehd7.sd7        0.039691           1110
seed7/prg/self.sd7           0.037368             49
seed7/prg/shisen.sd7         0.038684           1423
seed7/prg/sl.sd7             0.040238           1029
seed7/prg/snake.sd7          0.039447            615
seed7/prg/sokoban.sd7        0.037617            891
seed7/prg/spigotpi.sd7       0.035885             64
seed7/prg/sql7.sd7           0.037918            278
seed7/prg/startrek.sd7       0.038228            979
seed7/prg/sudoku7.sd7        0.038051           2657
seed7/prg/sydir7.sd7         0.038041            384
seed7/prg/syntaxhl.sd7       0.040666            177
seed7/prg/tak.sd7            0.036754             59
seed7/prg/tar7.sd7           0.037980            121
seed7/prg/tch.sd7            0.036219             55
seed7/prg/testfont.sd7       0.041017             95
seed7/prg/tet.sd7            0.038918            479
seed7/prg/tetg.sd7           0.037619            501
seed7/prg/toutf8.sd7         0.044191            240
seed7/prg/tst_cli.sd7        0.037452             40
seed7/prg/tst_srv.sd7        0.036091             47
seed7/prg/wator.sd7          0.039058            651
seed7/prg/which.sd7          0.037070             65
seed7/prg/wiz.sd7            0.039227           2833
seed7/prg/wordcnt.sd7        0.036531             54
seed7/prg/wrinum.sd7         0.035766             43
seed7/prg/wumpus.sd7         0.039253            372
seed7/lib/aes.s7i            0.042482           1144
seed7/lib/aes_gcm.s7i        0.039904            392
seed7/lib/ar.s7i             0.039335           1532
seed7/lib/arc4.s7i           0.039241            144
seed7/lib/archive.s7i        0.046019            143
seed7/lib/archive_base.s7i   0.040907            135
seed7/lib/array.s7i          0.041846            610
seed7/lib/asn1.s7i           0.037855            544
seed7/lib/asn1oid.s7i        0.043118            157
seed7/lib/basearray.s7i      0.040457            450
seed7/lib/bigfile.s7i        0.038528            136
seed7/lib/bigint.s7i         0.039201            824
seed7/lib/bigrat.s7i         0.039788            784
seed7/lib/bin16.s7i          0.038997            592
seed7/lib/bin32.s7i          0.039002            490
seed7/lib/bin64.s7i          0.038307            539
seed7/lib/bitdata.s7i        0.043705           1330
seed7/lib/bitmapfont.s7i     0.037808            215
seed7/lib/bitset.s7i         0.038811            593
seed7/lib/bitsetof.s7i       0.041078            431
seed7/lib/blowfish.s7i       0.042689            383
seed7/lib/bmp.s7i            0.038549            924
seed7/lib/boolean.s7i        0.037826            403
seed7/lib/browser.s7i        0.038172            280
seed7/lib/bstring.s7i        0.038505            227
seed7/lib/bytedata.s7i       0.038324            482
seed7/lib/bzip2.s7i          0.037904            887
seed7/lib/cards.s7i          0.037031           1342
seed7/lib/category.s7i       0.039284            209
seed7/lib/cc_conf.s7i        0.038995           1314
seed7/lib/ccittfax.s7i       0.039148           1022
seed7/lib/cgi.s7i            0.038636            109
seed7/lib/cgidialog.s7i      0.038992           1118
seed7/lib/char.s7i           0.038910            356
seed7/lib/charsets.s7i       0.040314           2024
seed7/lib/chartype.s7i       0.042303            121
seed7/lib/cipher.s7i         0.039306            146
seed7/lib/cli_cmds.s7i       0.042105           1360
seed7/lib/clib_file.s7i      0.040081            301
seed7/lib/color.s7i          0.041318            185
seed7/lib/complex.s7i        0.037944            464
seed7/lib/compress.s7i       0.037834            150
seed7/lib/console.s7i        0.037693            188
seed7/lib/cpio.s7i           0.038138           1708
seed7/lib/crc32.s7i          0.038243            193
seed7/lib/cronos16.s7i       0.042718           1173
seed7/lib/cronos27.s7i       0.045466           1464
seed7/lib/csv.s7i            0.038974            201
seed7/lib/db_prop.s7i        0.039118            991
seed7/lib/deflate.s7i        0.040022            740
seed7/lib/des.s7i            0.039942            444
seed7/lib/dialog.s7i         0.038856            311
seed7/lib/dir.s7i            0.039270            163
seed7/lib/draw.s7i           0.039331            854
seed7/lib/duration.s7i       0.039262           1038
seed7/lib/echo.s7i           0.039034            132
seed7/lib/editline.s7i       0.038914            398
seed7/lib/elf.s7i            0.040674           1560
seed7/lib/elliptic.s7i       0.037733            649
seed7/lib/enable_io.s7i      0.037432            312
seed7/lib/encoding.s7i       0.038403            931
seed7/lib/enumeration.s7i    0.037886            236
seed7/lib/environment.s7i    0.037492            175
seed7/lib/exif.s7i           0.040038            152
seed7/lib/external_file.s7i  0.037934            340
seed7/lib/field.s7i          0.037237            268
seed7/lib/file.s7i           0.038090            372
seed7/lib/filebits.s7i       0.036181             46
seed7/lib/filesys.s7i        0.037230            601
seed7/lib/fileutil.s7i       0.037545            144
seed7/lib/fixarray.s7i       0.038800            307
seed7/lib/float.s7i          0.038055            757
seed7/lib/font.s7i           0.037363            196
seed7/lib/font8x8.s7i        0.039924            998
seed7/lib/forloop.s7i        0.040392            449
seed7/lib/ftp.s7i            0.038749            969
seed7/lib/ftpserv.s7i        0.038840            631
seed7/lib/getf.s7i           0.038499            115
seed7/lib/gethttp.s7i        0.036277             41
seed7/lib/gethttps.s7i       0.036272             41
seed7/lib/gif.s7i            0.038803            561
seed7/lib/graph.s7i          0.040565            415
seed7/lib/graph_file.s7i     0.039292            399
seed7/lib/gtkserver.s7i      0.038650            161
seed7/lib/gzip.s7i           0.039227            573
seed7/lib/hash.s7i           0.040745            421
seed7/lib/hashsetof.s7i      0.041412            499
seed7/lib/hmac.s7i           0.039080            152
seed7/lib/html.s7i           0.038169             83
seed7/lib/html_ent.s7i       0.039324            476
seed7/lib/htmldom.s7i        0.039300            286
seed7/lib/http_request.s7i   0.039265            696
seed7/lib/http_srv_resp.s7i  0.039522            380
seed7/lib/https_request.s7i  0.038602            211
seed7/lib/httpserv.s7i       0.038620            345
seed7/lib/huffman.s7i        0.038386            644
seed7/lib/ico.s7i            0.038394            221
seed7/lib/idxarray.s7i       0.038041            232
seed7/lib/image.s7i          0.037421            156
seed7/lib/imagefile.s7i      0.037232            171
seed7/lib/inflate.s7i        0.039677            411
seed7/lib/inifile.s7i        0.038041            129
seed7/lib/integer.s7i        0.037537            663
seed7/lib/iobuffer.s7i       0.037341            289
seed7/lib/jpeg.s7i           0.038653           1761
seed7/lib/json.s7i           0.037858            891
seed7/lib/json_serde.s7i     0.037785            783
seed7/lib/keybd.s7i          0.037181            639
seed7/lib/keydescr.s7i       0.038820            192
seed7/lib/leb128.s7i         0.039417            218
seed7/lib/line.s7i           0.038987            164
seed7/lib/listener.s7i       0.038932            247
seed7/lib/logfile.s7i        0.037629             73
seed7/lib/lower.s7i          0.038323            142
seed7/lib/lzma.s7i           0.039757            934
seed7/lib/lzw.s7i            0.039201            861
seed7/lib/magic.s7i          0.040245            403
seed7/lib/mahjng32.s7i       0.038522           1500
seed7/lib/make.s7i           0.038946            544
seed7/lib/makedata.s7i       0.039000           1428
seed7/lib/math.s7i           0.040110            201
seed7/lib/mixarith.s7i       0.039954            249
seed7/lib/modern27.s7i       0.042257           1099
seed7/lib/more.s7i           0.037623            130
seed7/lib/msgdigest.s7i      0.038852           1222
seed7/lib/multiscr.s7i       0.036048             68
seed7/lib/null_file.s7i      0.037512            345
seed7/lib/osfiles.s7i        0.041316           1085
seed7/lib/pbm.s7i            0.039516            230
seed7/lib/pcx.s7i            0.039029            638
seed7/lib/pem.s7i            0.039462            185
seed7/lib/pgm.s7i            0.039359            238
seed7/lib/pic16.s7i          0.038734           1037
seed7/lib/pic32.s7i          0.038162           2060
seed7/lib/pic_util.s7i       0.038735            144
seed7/lib/pixelimage.s7i     0.038593            320
seed7/lib/pixmap_file.s7i    0.038585            459
seed7/lib/pixmapfont.s7i     0.040631            184
seed7/lib/pkcs1.s7i          0.044425            543
seed7/lib/png.s7i            0.039397           1064
seed7/lib/poll.s7i           0.038880            313
seed7/lib/ppm.s7i            0.039538            240
seed7/lib/process.s7i        0.037799            541
seed7/lib/progs.s7i          0.037742            789
seed7/lib/propertyfile.s7i   0.037779            155
seed7/lib/rational.s7i       0.037660            792
seed7/lib/ref_list.s7i       0.037837            252
seed7/lib/reference.s7i      0.037554            126
seed7/lib/reverse.s7i        0.037225             94
seed7/lib/rpm.s7i            0.038103           3487
seed7/lib/rpmext.s7i         0.038120            318
seed7/lib/scanfile.s7i       0.037852           1779
seed7/lib/scanjson.s7i       0.038509            413
seed7/lib/scanstri.s7i       0.037673           1814
seed7/lib/scantoml.s7i       0.038082           1603
seed7/lib/seed7_05.s7i       0.041829           1072
seed7/lib/set.s7i            0.037013             57
seed7/lib/shell.s7i          0.039553            615
seed7/lib/showtls.s7i        0.039037            678
seed7/lib/signature.s7i      0.039509            131
seed7/lib/smtp.s7i           0.039380            261
seed7/lib/sockbase.s7i       0.040048            217
seed7/lib/socket.s7i         0.039198            326
seed7/lib/sokoban1.s7i       0.038551           1519
seed7/lib/sql_base.s7i       0.039301           1000
seed7/lib/stars.s7i          0.041828           1705
seed7/lib/stdfont10.s7i      0.037807           3347
seed7/lib/stdfont12.s7i      0.039253           3928
seed7/lib/stdfont14.s7i      0.038875           4510
seed7/lib/stdfont16.s7i      0.038674           5092
seed7/lib/stdfont18.s7i      0.038873           5868
seed7/lib/stdfont20.s7i      0.038943           6449
seed7/lib/stdfont24.s7i      0.039155           7421
seed7/lib/stdfont8.s7i       0.037726           2960
seed7/lib/stdfont9.s7i       0.037606           3152
seed7/lib/stdio.s7i          0.038425            192
seed7/lib/strifile.s7i       0.040371            345
seed7/lib/string.s7i         0.037708            779
seed7/lib/stritext.s7i       0.038533            352
seed7/lib/struct.s7i         0.039835            266
seed7/lib/struct_elem.s7i    0.037508            129
seed7/lib/subfile.s7i        0.037797            174
seed7/lib/subrange.s7i       0.037238             78
seed7/lib/syntax.s7i         0.039499            294
seed7/lib/tar.s7i            0.038969           1880
seed7/lib/tar_cmds.s7i       0.038517            752
seed7/lib/tdes.s7i           0.037211            143
seed7/lib/tee.s7i            0.037340            143
seed7/lib/text.s7i           0.037676            135
seed7/lib/tga.s7i            0.040553            676
seed7/lib/tiff.s7i           0.039790           2771
seed7/lib/time.s7i           0.037993           1191
seed7/lib/tls.s7i            0.038738           2230
seed7/lib/unicode.s7i        0.041036            575
seed7/lib/unionfnd.s7i       0.038900            130
seed7/lib/upper.s7i          0.038533            142
seed7/lib/utf16.s7i          0.038829            540
seed7/lib/utf8.s7i           0.040183            234
seed7/lib/vecfont10.s7i      0.042401           1056
seed7/lib/vecfont18.s7i      0.042687           1119
seed7/lib/vector3d.s7i       0.038984            293
seed7/lib/vectorfont.s7i     0.039294            239
seed7/lib/wildcard.s7i       0.038828            140
seed7/lib/window.s7i         0.038038            455
seed7/lib/wrinum.s7i         0.038242            248
seed7/lib/x509cert.s7i       0.038114           1243
seed7/lib/xml_ent.s7i        0.037640             94
seed7/lib/xmldom.s7i         0.037612            303
seed7/lib/xz.s7i             0.041049            442
seed7/lib/zip.s7i            0.047138           2792
seed7/lib/zstd.s7i           0.039860           1263
============================ ================= ================

Statistical Summary — Mode B (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.038936        |
+-----------+-----------------+
| Minimum   | 0.035116        |
+-----------+-----------------+
| Maximum   | 0.047138        |
+-----------+-----------------+

Mode C — Mode Activation + Full-Buffer Fontification (font-lock-ensure)
-----------------------------------------------------------------------

Full-buffer fontification stress test.

File Load Times — Mode C (mean, GC-free)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

============================ ================= ================
File Name                    Mean Time (s)     Line count
============================ ================= ================
seed7/prg/addup.sd7          0.040192            190
seed7/prg/bas7.sd7           0.335194          11459
seed7/prg/bifurk.sd7         0.040943             73
seed7/prg/bigfiles.sd7       0.042553            129
seed7/prg/brainf7.sd7        0.039200             86
seed7/prg/calc7.sd7          0.040194            128
seed7/prg/carddemo.sd7       0.040823            190
seed7/prg/castle.sd7         0.112424           3148
seed7/prg/cat.sd7            0.037911             82
seed7/prg/cellauto.sd7       0.036838             85
seed7/prg/celsius.sd7        0.036533             42
seed7/prg/chk_all.sd7        0.062533            843
seed7/prg/chkarr.sd7         0.371103           8367
seed7/prg/chkbig.sd7         2.122358          29026
seed7/prg/chkbin.sd7         0.526938           6469
seed7/prg/chkbitdata.sd7     0.630248           6624
seed7/prg/chkbool.sd7        0.119981           3157
seed7/prg/chkbst.sd7         0.065010            722
seed7/prg/chkchr.sd7         0.222899           2809
seed7/prg/chkcmd.sd7         0.068686           1205
seed7/prg/chkdb.sd7          0.360052           7454
seed7/prg/chkdecl.sd7        0.059764            448
seed7/prg/chkenum.sd7        0.070659           1230
seed7/prg/chkerr.sd7         0.199838           4663
seed7/prg/chkexc.sd7         0.085741           2627
seed7/prg/chkfil.sd7         0.078611           1615
seed7/prg/chkflt.sd7         1.375985          20620
seed7/prg/chkhent.sd7        0.037629             54
seed7/prg/chkhsh.sd7         0.255197           4548
seed7/prg/chkidx.sd7         1.367318          19567
seed7/prg/chkint.sd7         2.597001          38129
seed7/prg/chkjson.sd7        0.103502           1764
seed7/prg/chkovf.sd7         0.579505           8216
seed7/prg/chkprc.sd7         0.329303          10111
seed7/prg/chkscan.sd7        0.058260            714
seed7/prg/chkset.sd7         0.678180          11974
seed7/prg/chkstr.sd7         1.419730          26952
seed7/prg/chktime.sd7        0.133369           2025
seed7/prg/chktoml.sd7        0.109948           1656
seed7/prg/clock.sd7          0.038069             47
seed7/prg/clock2.sd7         0.037350             43
seed7/prg/clock3.sd7         0.039864             95
seed7/prg/cmpfil.sd7         0.037826             84
seed7/prg/comanche.sd7       0.041542            180
seed7/prg/confval.sd7        0.045873            175
seed7/prg/db7.sd7            0.049089            417
seed7/prg/diff7.sd7          0.044594            263
seed7/prg/dirtst.sd7         0.037257             42
seed7/prg/dirx.sd7           0.040208            152
seed7/prg/dnafight.sd7       0.074260           1381
seed7/prg/dragon.sd7         0.038534             73
seed7/prg/echo.sd7           0.036415             39
seed7/prg/eliza.sd7          0.043795            302
seed7/prg/err.sd7            0.040232             96
seed7/prg/fannkuch.sd7       0.038484            131
seed7/prg/fib.sd7            0.037827             47
seed7/prg/find7.sd7          0.039749            133
seed7/prg/findchar.sd7       0.040346            149
seed7/prg/fractree.sd7       0.038348             55
seed7/prg/ftp7.sd7           0.043785            296
seed7/prg/ftpserv.sd7        0.038599             74
seed7/prg/gcd.sd7            0.038911            109
seed7/prg/gkbd.sd7           0.046558            358
seed7/prg/gtksvtst.sd7       0.037502             94
seed7/prg/hal.sd7            0.041408            250
seed7/prg/hamu.sd7           0.049718            573
seed7/prg/hanoi.sd7          0.036255             55
seed7/prg/hd.sd7             0.036844             79
seed7/prg/hello.sd7          0.036504             32
seed7/prg/hilbert.sd7        0.037678            108
seed7/prg/ide7.sd7           0.041360            196
seed7/prg/kbd.sd7            0.037449             49
seed7/prg/klondike.sd7       0.057189            883
seed7/prg/lander.sd7         0.074518           1551
seed7/prg/lst80bas.sd7       0.046835            344
seed7/prg/lst99bas.sd7       0.045438            401
seed7/prg/lstgwbas.sd7       0.052155            577
seed7/prg/mahjong.sd7        0.087500           1943
seed7/prg/make7.sd7          0.042521            121
seed7/prg/mandelbr.sd7       0.043393            237
seed7/prg/mind.sd7           0.047711            443
seed7/prg/mirror.sd7         0.041348            131
seed7/prg/ms.sd7             0.050682            641
seed7/prg/nicoma.sd7         0.041148            135
seed7/prg/pac.sd7            0.051075            726
seed7/prg/pairs.sd7          0.085212           2025
seed7/prg/panic.sd7          0.101254           2634
seed7/prg/percolation.sd7    0.044612            330
seed7/prg/planets.sd7        0.079606           1486
seed7/prg/portfwd7.sd7       0.040176            139
seed7/prg/prime.sd7          0.037820             74
seed7/prg/printpi1.sd7       0.037874             56
seed7/prg/printpi2.sd7       0.037619             54
seed7/prg/printpi3.sd7       0.037442             60
seed7/prg/pv7.sd7            0.044990            337
seed7/prg/queen.sd7          0.038824            149
seed7/prg/rand.sd7           0.038100            121
seed7/prg/raytrace.sd7       0.049596            538
seed7/prg/rever.sd7          0.054624            816
seed7/prg/roman.sd7          0.036688             38
seed7/prg/s7c.sd7            0.289365           9060
seed7/prg/s7check.sd7        0.039822             68
seed7/prg/savehd7.sd7        0.069838           1110
seed7/prg/self.sd7           0.037984             49
seed7/prg/shisen.sd7         0.074067           1423
seed7/prg/sl.sd7             0.062392           1029
seed7/prg/snake.sd7          0.048701            615
seed7/prg/sokoban.sd7        0.056879            891
seed7/prg/spigotpi.sd7       0.037762             64
seed7/prg/sql7.sd7           0.043738            278
seed7/prg/startrek.sd7       0.060998            979
seed7/prg/sudoku7.sd7        0.102686           2657
seed7/prg/sydir7.sd7         0.047207            384
seed7/prg/syntaxhl.sd7       0.041531            177
seed7/prg/tak.sd7            0.036352             59
seed7/prg/tar7.sd7           0.039627            121
seed7/prg/tch.sd7            0.038304             55
seed7/prg/testfont.sd7       0.039674             95
seed7/prg/tet.sd7            0.046580            479
seed7/prg/tetg.sd7           0.047003            501
seed7/prg/toutf8.sd7         0.044615            240
seed7/prg/tst_cli.sd7        0.037324             40
seed7/prg/tst_srv.sd7        0.037955             47
seed7/prg/wator.sd7          0.053366            651
seed7/prg/which.sd7          0.038403             65
seed7/prg/wiz.sd7            0.107769           2833
seed7/prg/wordcnt.sd7        0.037120             54
seed7/prg/wrinum.sd7         0.036473             43
seed7/prg/wumpus.sd7         0.043422            372
seed7/lib/aes.s7i            0.112690           1144
seed7/lib/aes_gcm.s7i        0.048642            392
seed7/lib/ar.s7i             0.074902           1532
seed7/lib/arc4.s7i           0.038651            144
seed7/lib/archive.s7i        0.038586            143
seed7/lib/archive_base.s7i   0.039514            135
seed7/lib/array.s7i          0.054474            610
seed7/lib/asn1.s7i           0.050184            544
seed7/lib/asn1oid.s7i        0.044311            157
seed7/lib/basearray.s7i      0.052142            450
seed7/lib/bigfile.s7i        0.040008            136
seed7/lib/bigint.s7i         0.064151            824
seed7/lib/bigrat.s7i         0.056562            784
seed7/lib/bin16.s7i          0.053420            592
seed7/lib/bin32.s7i          0.050343            490
seed7/lib/bin64.s7i          0.051611            539
seed7/lib/bitdata.s7i        0.081576           1330
seed7/lib/bitmapfont.s7i     0.043185            215
seed7/lib/bitset.s7i         0.050801            593
seed7/lib/bitsetof.s7i       0.049461            431
seed7/lib/blowfish.s7i       0.058537            383
seed7/lib/bmp.s7i            0.063642            924
seed7/lib/boolean.s7i        0.053702            403
seed7/lib/browser.s7i        0.044202            280
seed7/lib/bstring.s7i        0.043989            227
seed7/lib/bytedata.s7i       0.052728            482
seed7/lib/bzip2.s7i          0.060169            887
seed7/lib/cards.s7i          0.069455           1342
seed7/lib/category.s7i       0.042076            209
seed7/lib/cc_conf.s7i        0.079831           1314
seed7/lib/ccittfax.s7i       0.069295           1022
seed7/lib/cgi.s7i            0.041749            109
seed7/lib/cgidialog.s7i      0.063072           1118
seed7/lib/char.s7i           0.046171            356
seed7/lib/charsets.s7i       0.085158           2024
seed7/lib/chartype.s7i       0.041708            121
seed7/lib/cipher.s7i         0.040389            146
seed7/lib/cli_cmds.s7i       0.071246           1360
seed7/lib/clib_file.s7i      0.045601            301
seed7/lib/color.s7i          0.042142            185
seed7/lib/complex.s7i        0.047257            464
seed7/lib/compress.s7i       0.040231            150
seed7/lib/console.s7i        0.040847            188
seed7/lib/cpio.s7i           0.083139           1708
seed7/lib/crc32.s7i          0.046173            193
seed7/lib/cronos16.s7i       0.097574           1173
seed7/lib/cronos27.s7i       0.121368           1464
seed7/lib/csv.s7i            0.042541            201
seed7/lib/db_prop.s7i        0.065673            991
seed7/lib/deflate.s7i        0.058206            740
seed7/lib/des.s7i            0.057581            444
seed7/lib/dialog.s7i         0.045336            311
seed7/lib/dir.s7i            0.039570            163
seed7/lib/draw.s7i           0.058358            854
seed7/lib/duration.s7i       0.063113           1038
seed7/lib/echo.s7i           0.039895            132
seed7/lib/editline.s7i       0.046687            398
seed7/lib/elf.s7i            0.088079           1560
seed7/lib/elliptic.s7i       0.053704            649
seed7/lib/enable_io.s7i      0.043909            312
seed7/lib/encoding.s7i       0.062681            931
seed7/lib/enumeration.s7i    0.042686            236
seed7/lib/environment.s7i    0.040892            175
seed7/lib/exif.s7i           0.041061            152
seed7/lib/external_file.s7i  0.044828            340
seed7/lib/field.s7i          0.042450            268
seed7/lib/file.s7i           0.045163            372
seed7/lib/filebits.s7i       0.037029             46
seed7/lib/filesys.s7i        0.050699            601
seed7/lib/fileutil.s7i       0.040219            144
seed7/lib/fixarray.s7i       0.045732            307
seed7/lib/float.s7i          0.060973            757
seed7/lib/font.s7i           0.043753            196
seed7/lib/font8x8.s7i        0.050953            998
seed7/lib/forloop.s7i        0.047516            449
seed7/lib/ftp.s7i            0.060053            969
seed7/lib/ftpserv.s7i        0.053070            631
seed7/lib/getf.s7i           0.038594            115
seed7/lib/gethttp.s7i        0.037988             41
seed7/lib/gethttps.s7i       0.037787             41
seed7/lib/gif.s7i            0.051978            561
seed7/lib/graph.s7i          0.049424            415
seed7/lib/graph_file.s7i     0.045374            399
seed7/lib/gtkserver.s7i      0.039126            161
seed7/lib/gzip.s7i           0.049745            573
seed7/lib/hash.s7i           0.051362            421
seed7/lib/hashsetof.s7i      0.050215            499
seed7/lib/hmac.s7i           0.040559            152
seed7/lib/html.s7i           0.038099             83
seed7/lib/html_ent.s7i       0.049045            476
seed7/lib/htmldom.s7i        0.044393            286
seed7/lib/http_request.s7i   0.053593            696
seed7/lib/http_srv_resp.s7i  0.047910            380
seed7/lib/https_request.s7i  0.042662            211
seed7/lib/httpserv.s7i       0.045283            345
seed7/lib/huffman.s7i        0.055862            644
seed7/lib/ico.s7i            0.042787            221
seed7/lib/idxarray.s7i       0.043712            232
seed7/lib/image.s7i          0.039811            156
seed7/lib/imagefile.s7i      0.040750            171
seed7/lib/inflate.s7i        0.049642            411
seed7/lib/inifile.s7i        0.039580            129
seed7/lib/integer.s7i        0.054804            663
seed7/lib/iobuffer.s7i       0.043072            289
seed7/lib/jpeg.s7i           0.086531           1761
seed7/lib/json.s7i           0.057515            891
seed7/lib/json_serde.s7i     0.053812            783
seed7/lib/keybd.s7i          0.058146            639
seed7/lib/keydescr.s7i       0.043207            192
seed7/lib/leb128.s7i         0.047782            218
seed7/lib/line.s7i           0.047012            164
seed7/lib/listener.s7i       0.046891            247
seed7/lib/logfile.s7i        0.043876             73
seed7/lib/lower.s7i          0.046779            142
seed7/lib/lzma.s7i           0.066659            934
seed7/lib/lzw.s7i            0.065690            861
seed7/lib/magic.s7i          0.051347            403
seed7/lib/mahjng32.s7i       0.070827           1500
seed7/lib/make.s7i           0.052898            544
seed7/lib/makedata.s7i       0.073105           1428
seed7/lib/math.s7i           0.041806            201
seed7/lib/mixarith.s7i       0.045193            249
seed7/lib/modern27.s7i       0.089906           1099
seed7/lib/more.s7i           0.038950            130
seed7/lib/msgdigest.s7i      0.080223           1222
seed7/lib/multiscr.s7i       0.040952             68
seed7/lib/null_file.s7i      0.049462            345
seed7/lib/osfiles.s7i        0.070587           1085
seed7/lib/pbm.s7i            0.041800            230
seed7/lib/pcx.s7i            0.054715            638
seed7/lib/pem.s7i            0.041505            185
seed7/lib/pgm.s7i            0.042050            238
seed7/lib/pic16.s7i          0.055230           1037
seed7/lib/pic32.s7i          0.085347           2060
seed7/lib/pic_util.s7i       0.041307            144
seed7/lib/pixelimage.s7i     0.044495            320
seed7/lib/pixmap_file.s7i    0.048690            459
seed7/lib/pixmapfont.s7i     0.042097            184
seed7/lib/pkcs1.s7i          0.061374            543
seed7/lib/png.s7i            0.068016           1064
seed7/lib/poll.s7i           0.045531            313
seed7/lib/ppm.s7i            0.046135            240
seed7/lib/process.s7i        0.055349            541
seed7/lib/progs.s7i          0.058469            789
seed7/lib/propertyfile.s7i   0.041487            155
seed7/lib/rational.s7i       0.054307            792
seed7/lib/ref_list.s7i       0.044242            252
seed7/lib/reference.s7i      0.040182            126
seed7/lib/reverse.s7i        0.039082             94
seed7/lib/rpm.s7i            0.150322           3487
seed7/lib/rpmext.s7i         0.044183            318
seed7/lib/scanfile.s7i       0.083053           1779
seed7/lib/scanjson.s7i       0.050901            413
seed7/lib/scanstri.s7i       0.084974           1814
seed7/lib/scantoml.s7i       0.076805           1603
seed7/lib/seed7_05.s7i       0.072142           1072
seed7/lib/set.s7i            0.039152             57
seed7/lib/shell.s7i          0.056823            615
seed7/lib/showtls.s7i        0.058253            678
seed7/lib/signature.s7i      0.040310            131
seed7/lib/smtp.s7i           0.042679            261
seed7/lib/sockbase.s7i       0.043539            217
seed7/lib/socket.s7i         0.047440            326
seed7/lib/sokoban1.s7i       0.061537           1519
seed7/lib/sql_base.s7i       0.070169           1000
seed7/lib/stars.s7i          0.142358           1705
seed7/lib/stdfont10.s7i      0.083896           3347
seed7/lib/stdfont12.s7i      0.095398           3928
seed7/lib/stdfont14.s7i      0.105791           4510
seed7/lib/stdfont16.s7i      0.121092           5092
seed7/lib/stdfont18.s7i      0.139587           5868
seed7/lib/stdfont20.s7i      0.154388           6449
seed7/lib/stdfont24.s7i      0.186483           7421
seed7/lib/stdfont8.s7i       0.081330           2960
seed7/lib/stdfont9.s7i       0.080368           3152
seed7/lib/stdio.s7i          0.044056            192
seed7/lib/strifile.s7i       0.050252            345
seed7/lib/string.s7i         0.061569            779
seed7/lib/stritext.s7i       0.045260            352
seed7/lib/struct.s7i         0.045808            266
seed7/lib/struct_elem.s7i    0.040649            129
seed7/lib/subfile.s7i        0.040805            174
seed7/lib/subrange.s7i       0.037840             78
seed7/lib/syntax.s7i         0.049583            294
seed7/lib/tar.s7i            0.087385           1880
seed7/lib/tar_cmds.s7i       0.060861            752
seed7/lib/tdes.s7i           0.039108            143
seed7/lib/tee.s7i            0.037728            143
seed7/lib/text.s7i           0.037974            135
seed7/lib/tga.s7i            0.053993            676
seed7/lib/tiff.s7i           0.127760           2771
seed7/lib/time.s7i           0.064357           1191
seed7/lib/tls.s7i            0.109842           2230
seed7/lib/unicode.s7i        0.054085            575
seed7/lib/unionfnd.s7i       0.043397            130
seed7/lib/upper.s7i          0.042899            142
seed7/lib/utf16.s7i          0.052961            540
seed7/lib/utf8.s7i           0.055683            234
seed7/lib/vecfont10.s7i      0.083928           1056
seed7/lib/vecfont18.s7i      0.094396           1119
seed7/lib/vector3d.s7i       0.044817            293
seed7/lib/vectorfont.s7i     0.043415            239
seed7/lib/wildcard.s7i       0.040621            140
seed7/lib/window.s7i         0.048723            455
seed7/lib/wrinum.s7i         0.042370            248
seed7/lib/x509cert.s7i       0.073874           1243
seed7/lib/xml_ent.s7i        0.038274             94
seed7/lib/xmldom.s7i         0.043193            303
seed7/lib/xz.s7i             0.047580            442
seed7/lib/zip.s7i            0.126608           2792
seed7/lib/zstd.s7i           0.076207           1263
============================ ================= ================

Statistical Summary — Mode C (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.092767        |
+-----------+-----------------+
| Minimum   | 0.036255        |
+-----------+-----------------+
| Maximum   | 2.597001        |
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
seed7/prg/addup.sd7          0.048079            190
seed7/prg/bas7.sd7           0.794633          11459
seed7/prg/bifurk.sd7         0.041362             73
seed7/prg/bigfiles.sd7       0.045058            129
seed7/prg/brainf7.sd7        0.041502             86
seed7/prg/calc7.sd7          0.042704            128
seed7/prg/carddemo.sd7       0.048892            190
seed7/prg/castle.sd7         0.229449           3148
seed7/prg/cat.sd7            0.040109             82
seed7/prg/cellauto.sd7       0.040856             85
seed7/prg/celsius.sd7        0.039199             42
seed7/prg/chk_all.sd7        0.084406            843
seed7/prg/chkarr.sd7         0.886990           8367
seed7/prg/chkbig.sd7         4.266189          29026
seed7/prg/chkbin.sd7         1.054493           6469
seed7/prg/chkbitdata.sd7     1.282834           6624
seed7/prg/chkbool.sd7        0.238557           3157
seed7/prg/chkbst.sd7         0.105658            722
seed7/prg/chkchr.sd7         0.501780           2809
seed7/prg/chkcmd.sd7         0.113411           1205
seed7/prg/chkdb.sd7          0.774768           7454
seed7/prg/chkdecl.sd7        0.095428            448
seed7/prg/chkenum.sd7        0.123057           1230
seed7/prg/chkerr.sd7         0.350251           4663
seed7/prg/chkexc.sd7         0.154817           2627
seed7/prg/chkfil.sd7         0.135081           1615
seed7/prg/chkflt.sd7         2.918974          20620
seed7/prg/chkhent.sd7        0.043506             54
seed7/prg/chkhsh.sd7         0.516734           4548
seed7/prg/chkidx.sd7         3.283932          19567
seed7/prg/chkint.sd7         5.733136          38129
seed7/prg/chkjson.sd7        0.197322           1764
seed7/prg/chkovf.sd7         1.249010           8216
seed7/prg/chkprc.sd7         0.723489          10111
seed7/prg/chkscan.sd7        0.098886            714
seed7/prg/chkset.sd7         1.812547          11974
seed7/prg/chkstr.sd7         3.539529          26952
seed7/prg/chktime.sd7        0.259539           2025
seed7/prg/chktoml.sd7        0.203921           1656
seed7/prg/clock.sd7          0.043252             47
seed7/prg/clock2.sd7         0.038359             43
seed7/prg/clock3.sd7         0.043755             95
seed7/prg/cmpfil.sd7         0.042016             84
seed7/prg/comanche.sd7       0.050037            180
seed7/prg/confval.sd7        0.057874            175
seed7/prg/db7.sd7            0.063921            417
seed7/prg/diff7.sd7          0.054570            263
seed7/prg/dirtst.sd7         0.038558             42
seed7/prg/dirx.sd7           0.048662            152
seed7/prg/dnafight.sd7       0.121731           1381
seed7/prg/dragon.sd7         0.041125             73
seed7/prg/echo.sd7           0.040868             39
seed7/prg/eliza.sd7          0.054193            302
seed7/prg/err.sd7            0.046508             96
seed7/prg/fannkuch.sd7       0.044949            131
seed7/prg/fib.sd7            0.039276             47
seed7/prg/find7.sd7          0.047551            133
seed7/prg/findchar.sd7       0.044570            149
seed7/prg/fractree.sd7       0.039187             55
seed7/prg/ftp7.sd7           0.056189            296
seed7/prg/ftpserv.sd7        0.041817             74
seed7/prg/gcd.sd7            0.042336            109
seed7/prg/gkbd.sd7           0.063180            358
seed7/prg/gtksvtst.sd7       0.040939             94
seed7/prg/hal.sd7            0.049407            250
seed7/prg/hamu.sd7           0.069235            573
seed7/prg/hanoi.sd7          0.038743             55
seed7/prg/hd.sd7             0.040455             79
seed7/prg/hello.sd7          0.037476             32
seed7/prg/hilbert.sd7        0.051180            108
seed7/prg/ide7.sd7           0.052366            196
seed7/prg/kbd.sd7            0.038704             49
seed7/prg/klondike.sd7       0.091436            883
seed7/prg/lander.sd7         0.136856           1551
seed7/prg/lst80bas.sd7       0.056278            344
seed7/prg/lst99bas.sd7       0.062854            401
seed7/prg/lstgwbas.sd7       0.075499            577
seed7/prg/mahjong.sd7        0.155116           1943
seed7/prg/make7.sd7          0.044369            121
seed7/prg/mandelbr.sd7       0.050343            237
seed7/prg/mind.sd7           0.062963            443
seed7/prg/mirror.sd7         0.048332            131
seed7/prg/ms.sd7             0.074993            641
seed7/prg/nicoma.sd7         0.045476            135
seed7/prg/pac.sd7            0.081968            726
seed7/prg/pairs.sd7          0.145156           2025
seed7/prg/panic.sd7          0.203860           2634
seed7/prg/percolation.sd7    0.060269            330
seed7/prg/planets.sd7        0.144397           1486
seed7/prg/portfwd7.sd7       0.048156            139
seed7/prg/prime.sd7          0.039495             74
seed7/prg/printpi1.sd7       0.046711             56
seed7/prg/printpi2.sd7       0.043126             54
seed7/prg/printpi3.sd7       0.040175             60
seed7/prg/pv7.sd7            0.065648            337
seed7/prg/queen.sd7          0.045500            149
seed7/prg/rand.sd7           0.043138            121
seed7/prg/raytrace.sd7       0.072178            538
seed7/prg/rever.sd7          0.091007            816
seed7/prg/roman.sd7          0.040083             38
seed7/prg/s7c.sd7            0.647746           9060
seed7/prg/s7check.sd7        0.041126             68
seed7/prg/savehd7.sd7        0.119044           1110
seed7/prg/self.sd7           0.040103             49
seed7/prg/shisen.sd7         0.126000           1423
seed7/prg/sl.sd7             0.105694           1029
seed7/prg/snake.sd7          0.070006            615
seed7/prg/sokoban.sd7        0.086598            891
seed7/prg/spigotpi.sd7       0.039067             64
seed7/prg/sql7.sd7           0.053040            278
seed7/prg/startrek.sd7       0.100711            979
seed7/prg/sudoku7.sd7        0.208265           2657
seed7/prg/sydir7.sd7         0.062609            384
seed7/prg/syntaxhl.sd7       0.049103            177
seed7/prg/tak.sd7            0.041755             59
seed7/prg/tar7.sd7           0.047370            121
seed7/prg/tch.sd7            0.039262             55
seed7/prg/testfont.sd7       0.046070             95
seed7/prg/tet.sd7            0.063034            479
seed7/prg/tetg.sd7           0.065919            501
seed7/prg/toutf8.sd7         0.057176            240
seed7/prg/tst_cli.sd7        0.041873             40
seed7/prg/tst_srv.sd7        0.040112             47
seed7/prg/wator.sd7          0.081845            651
seed7/prg/which.sd7          0.038339             65
seed7/prg/wiz.sd7            0.215788           2833
seed7/prg/wordcnt.sd7        0.040391             54
seed7/prg/wrinum.sd7         0.040573             43
seed7/prg/wumpus.sd7         0.056678            372
seed7/lib/aes.s7i            0.204990           1144
seed7/lib/aes_gcm.s7i        0.068672            392
seed7/lib/ar.s7i             0.129193           1532
seed7/lib/arc4.s7i           0.043745            144
seed7/lib/archive.s7i        0.045424            143
seed7/lib/archive_base.s7i   0.046838            135
seed7/lib/array.s7i          0.076976            610
seed7/lib/asn1.s7i           0.066644            544
seed7/lib/asn1oid.s7i        0.051376            157
seed7/lib/basearray.s7i      0.065930            450
seed7/lib/bigfile.s7i        0.044145            136
seed7/lib/bigint.s7i         0.084743            824
seed7/lib/bigrat.s7i         0.084347            784
seed7/lib/bin16.s7i          0.071755            592
seed7/lib/bin32.s7i          0.069242            490
seed7/lib/bin64.s7i          0.066821            539
seed7/lib/bitdata.s7i        0.129269           1330
seed7/lib/bitmapfont.s7i     0.048578            215
seed7/lib/bitset.s7i         0.066094            593
seed7/lib/bitsetof.s7i       0.064860            431
seed7/lib/blowfish.s7i       0.080662            383
seed7/lib/bmp.s7i            0.103358            924
seed7/lib/boolean.s7i        0.056330            403
seed7/lib/browser.s7i        0.057583            280
seed7/lib/bstring.s7i        0.048063            227
seed7/lib/bytedata.s7i       0.067784            482
seed7/lib/bzip2.s7i          0.091670            887
seed7/lib/cards.s7i          0.106483           1342
seed7/lib/category.s7i       0.051247            209
seed7/lib/cc_conf.s7i        0.124453           1314
seed7/lib/ccittfax.s7i       0.105158           1022
seed7/lib/cgi.s7i            0.042021            109
seed7/lib/cgidialog.s7i      0.100096           1118
seed7/lib/char.s7i           0.053470            356
seed7/lib/charsets.s7i       0.127155           2024
seed7/lib/chartype.s7i       0.048388            121
seed7/lib/cipher.s7i         0.042873            146
seed7/lib/cli_cmds.s7i       0.117144           1360
seed7/lib/clib_file.s7i      0.051795            301
seed7/lib/color.s7i          0.052945            185
seed7/lib/complex.s7i        0.068501            464
seed7/lib/compress.s7i       0.048478            150
seed7/lib/console.s7i        0.048219            188
seed7/lib/cpio.s7i           0.149238           1708
seed7/lib/crc32.s7i          0.055828            193
seed7/lib/cronos16.s7i       0.203481           1173
seed7/lib/cronos27.s7i       0.265854           1464
seed7/lib/csv.s7i            0.050074            201
seed7/lib/db_prop.s7i        0.103991            991
seed7/lib/deflate.s7i        0.091670            740
seed7/lib/des.s7i            0.084049            444
seed7/lib/dialog.s7i         0.058326            311
seed7/lib/dir.s7i            0.043205            163
seed7/lib/draw.s7i           0.091089            854
seed7/lib/duration.s7i       0.099721           1038
seed7/lib/echo.s7i           0.043948            132
seed7/lib/editline.s7i       0.063036            398
seed7/lib/elf.s7i            0.163674           1560
seed7/lib/elliptic.s7i       0.078046            649
seed7/lib/enable_io.s7i      0.053832            312
seed7/lib/encoding.s7i       0.098568            931
seed7/lib/enumeration.s7i    0.051638            236
seed7/lib/environment.s7i    0.044485            175
seed7/lib/exif.s7i           0.049587            152
seed7/lib/external_file.s7i  0.059170            340
seed7/lib/field.s7i          0.056996            268
seed7/lib/file.s7i           0.056010            372
seed7/lib/filebits.s7i       0.043111             46
seed7/lib/filesys.s7i        0.068750            601
seed7/lib/fileutil.s7i       0.045686            144
seed7/lib/fixarray.s7i       0.055928            307
seed7/lib/float.s7i          0.082767            757
seed7/lib/font.s7i           0.049860            196
seed7/lib/font8x8.s7i        0.069975            998
seed7/lib/forloop.s7i        0.063037            449
seed7/lib/ftp.s7i            0.089840            969
seed7/lib/ftpserv.s7i        0.077232            631
seed7/lib/getf.s7i           0.044812            115
seed7/lib/gethttp.s7i        0.039949             41
seed7/lib/gethttps.s7i       0.044611             41
seed7/lib/gif.s7i            0.073354            561
seed7/lib/graph.s7i          0.068542            415
seed7/lib/graph_file.s7i     0.061811            399
seed7/lib/gtkserver.s7i      0.046226            161
seed7/lib/gzip.s7i           0.071288            573
seed7/lib/hash.s7i           0.073258            421
seed7/lib/hashsetof.s7i      0.072146            499
seed7/lib/hmac.s7i           0.048225            152
seed7/lib/html.s7i           0.045693             83
seed7/lib/html_ent.s7i       0.066849            476
seed7/lib/htmldom.s7i        0.056097            286
seed7/lib/http_request.s7i   0.085044            696
seed7/lib/http_srv_resp.s7i  0.066314            380
seed7/lib/https_request.s7i  0.051269            211
seed7/lib/httpserv.s7i       0.057060            345
seed7/lib/huffman.s7i        0.077893            644
seed7/lib/ico.s7i            0.050625            221
seed7/lib/idxarray.s7i       0.053242            232
seed7/lib/image.s7i          0.044576            156
seed7/lib/imagefile.s7i      0.047523            171
seed7/lib/inflate.s7i        0.068482            411
seed7/lib/inifile.s7i        0.044304            129
seed7/lib/integer.s7i        0.073183            663
seed7/lib/iobuffer.s7i       0.054498            289
seed7/lib/jpeg.s7i           0.160305           1761
seed7/lib/json.s7i           0.083722            891
seed7/lib/json_serde.s7i     0.082153            783
seed7/lib/keybd.s7i          0.084579            639
seed7/lib/keydescr.s7i       0.051684            192
seed7/lib/leb128.s7i         0.048911            218
seed7/lib/line.s7i           0.047642            164
seed7/lib/listener.s7i       0.054424            247
seed7/lib/logfile.s7i        0.042224             73
seed7/lib/lower.s7i          0.049108            142
seed7/lib/lzma.s7i           0.101956            934
seed7/lib/lzw.s7i            0.093048            861
seed7/lib/magic.s7i          0.067770            403
seed7/lib/mahjng32.s7i       0.098618           1500
seed7/lib/make.s7i           0.072697            544
seed7/lib/makedata.s7i       0.128358           1428
seed7/lib/math.s7i           0.061166            201
seed7/lib/mixarith.s7i       0.054257            249
seed7/lib/modern27.s7i       0.181932           1099
seed7/lib/more.s7i           0.044522            130
seed7/lib/msgdigest.s7i      0.141831           1222
seed7/lib/multiscr.s7i       0.040197             68
seed7/lib/null_file.s7i      0.055085            345
seed7/lib/osfiles.s7i        0.100215           1085
seed7/lib/pbm.s7i            0.050989            230
seed7/lib/pcx.s7i            0.082496            638
seed7/lib/pem.s7i            0.046671            185
seed7/lib/pgm.s7i            0.049863            238
seed7/lib/pic16.s7i          0.070859           1037
seed7/lib/pic32.s7i          0.128589           2060
seed7/lib/pic_util.s7i       0.048261            144
seed7/lib/pixelimage.s7i     0.060012            320
seed7/lib/pixmap_file.s7i    0.066108            459
seed7/lib/pixmapfont.s7i     0.048233            184
seed7/lib/pkcs1.s7i          0.080311            543
seed7/lib/png.s7i            0.112308           1064
seed7/lib/poll.s7i           0.062538            313
seed7/lib/ppm.s7i            0.054939            240
seed7/lib/process.s7i        0.067993            541
seed7/lib/progs.s7i          0.086433            789
seed7/lib/propertyfile.s7i   0.052118            155
seed7/lib/rational.s7i       0.082640            792
seed7/lib/ref_list.s7i       0.051292            252
seed7/lib/reference.s7i      0.043567            126
seed7/lib/reverse.s7i        0.045118             94
seed7/lib/rpm.s7i            0.298008           3487
seed7/lib/rpmext.s7i         0.056838            318
seed7/lib/scanfile.s7i       0.138587           1779
seed7/lib/scanjson.s7i       0.069472            413
seed7/lib/scanstri.s7i       0.146226           1814
seed7/lib/scantoml.s7i       0.139543           1603
seed7/lib/seed7_05.s7i       0.117985           1072
seed7/lib/set.s7i            0.042930             57
seed7/lib/shell.s7i          0.074052            615
seed7/lib/showtls.s7i        0.091159            678
seed7/lib/signature.s7i      0.049904            131
seed7/lib/smtp.s7i           0.055523            261
seed7/lib/sockbase.s7i       0.050736            217
seed7/lib/socket.s7i         0.054734            326
seed7/lib/sokoban1.s7i       0.087903           1519
seed7/lib/sql_base.s7i       0.100103           1000
seed7/lib/stars.s7i          0.247730           1705
seed7/lib/stdfont10.s7i      0.152102           3347
seed7/lib/stdfont12.s7i      0.174814           3928
seed7/lib/stdfont14.s7i      0.198580           4510
seed7/lib/stdfont16.s7i      0.222783           5092
seed7/lib/stdfont18.s7i      0.259463           5868
seed7/lib/stdfont20.s7i      0.288338           6449
seed7/lib/stdfont24.s7i      0.347548           7421
seed7/lib/stdfont8.s7i       0.131694           2960
seed7/lib/stdfont9.s7i       0.141252           3152
seed7/lib/stdio.s7i          0.050292            192
seed7/lib/strifile.s7i       0.060410            345
seed7/lib/string.s7i         0.078304            779
seed7/lib/stritext.s7i       0.057934            352
seed7/lib/struct.s7i         0.059448            266
seed7/lib/struct_elem.s7i    0.043591            129
seed7/lib/subfile.s7i        0.046414            174
seed7/lib/subrange.s7i       0.041803             78
seed7/lib/syntax.s7i         0.065984            294
seed7/lib/tar.s7i            0.153383           1880
seed7/lib/tar_cmds.s7i       0.093948            752
seed7/lib/tdes.s7i           0.044658            143
seed7/lib/tee.s7i            0.042243            143
seed7/lib/text.s7i           0.043068            135
seed7/lib/tga.s7i            0.087395            676
seed7/lib/tiff.s7i           0.254328           2771
seed7/lib/time.s7i           0.109363           1191
seed7/lib/tls.s7i            0.210530           2230
seed7/lib/unicode.s7i        0.079271            575
seed7/lib/unionfnd.s7i       0.044602            130
seed7/lib/upper.s7i          0.044527            142
seed7/lib/utf16.s7i          0.069750            540
seed7/lib/utf8.s7i           0.052225            234
seed7/lib/vecfont10.s7i      0.167873           1056
seed7/lib/vecfont18.s7i      0.189285           1119
seed7/lib/vector3d.s7i       0.052337            293
seed7/lib/vectorfont.s7i     0.054146            239
seed7/lib/wildcard.s7i       0.045677            140
seed7/lib/window.s7i         0.065160            455
seed7/lib/wrinum.s7i         0.054340            248
seed7/lib/x509cert.s7i       0.126777           1243
seed7/lib/xml_ent.s7i        0.044385             94
seed7/lib/xmldom.s7i         0.055840            303
seed7/lib/xz.s7i             0.067971            442
seed7/lib/zip.s7i            0.241600           2792
seed7/lib/zstd.s7i           0.127808           1263
============================ ================= ================

Statistical Summary — Mode D (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.165701        |
+-----------+-----------------+
| Minimum   | 0.037476        |
+-----------+-----------------+
| Maximum   | 5.733136        |
+-----------+-----------------+

Cross-Mode Comparison
=====================

+------+-----------------+-----------------+-----------------+
| Mode | Average (s)     | Minimum (s)     | Maximum (s)     |
+======+=================+=================+=================+
| A    | 0.033800        | 0.031949        | 0.041438        |
+------+-----------------+-----------------+-----------------+
| B    | 0.038936        | 0.035116        | 0.047138        |
+------+-----------------+-----------------+-----------------+
| C    | 0.092767        | 0.036255        | 2.597001        |
+------+-----------------+-----------------+-----------------+
| D    | 0.165701        | 0.037476        | 5.733136        |
+------+-----------------+-----------------+-----------------+

Phase Timing Summary
====================

Wall-clock times include warm-up work and GC performed by the warm-up helper.

+----------+--------------+--------------+--------------+
| Phase    | Warm-up      | Timed pass   | Mode total   |
+==========+==============+==============+==============+
| Mode A   | 00:00:12.244 | 00:00:57.587 | 00:01:09.832 |
+----------+--------------+--------------+--------------+
| Mode B   | 00:00:14.873 | 00:01:06.391 | 00:01:21.265 |
+----------+--------------+--------------+--------------+
| Mode C   | 00:00:36.978 | 00:02:38.960 | 00:03:15.938 |
+----------+--------------+--------------+--------------+
| Mode D   | 00:01:03.887 | 00:04:43.236 | 00:05:47.124 |
+----------+--------------+--------------+--------------+
| Total    |              |              | 00:11:34.166 |
+----------+--------------+--------------+--------------+


.. ---------------------------------------------------------------------------
