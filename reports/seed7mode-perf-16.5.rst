=======================================================
GC-Controlled Benchmark Report: Seed7 Mode — Four Modes
=======================================================

:Running with: seed7-mode 2026-06-30T16:45:51+0000 W27-2
:Seed7 dir   : /Users/roup/my/dvo/seed7-repos/seed7/
:Started at: 12:48:41 local time
:Generated on: 2026-06-30 16:59:51 UTC
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
seed7/prg/addup.sd7          0.033279            190
seed7/prg/bas7.sd7           0.034008          11459
seed7/prg/bifurk.sd7         0.032740             73
seed7/prg/bigfiles.sd7       0.034277            129
seed7/prg/brainf7.sd7        0.034508             86
seed7/prg/calc7.sd7          0.033678            128
seed7/prg/carddemo.sd7       0.033858            190
seed7/prg/castle.sd7         0.034761           3148
seed7/prg/cat.sd7            0.033844             82
seed7/prg/cellauto.sd7       0.033685             85
seed7/prg/celsius.sd7        0.033862             42
seed7/prg/chk_all.sd7        0.033643            843
seed7/prg/chkarr.sd7         0.034499           8367
seed7/prg/chkbig.sd7         0.038646          29026
seed7/prg/chkbin.sd7         0.034432           6469
seed7/prg/chkbitdata.sd7     0.034359           6624
seed7/prg/chkbool.sd7        0.033916           3157
seed7/prg/chkbst.sd7         0.033742            722
seed7/prg/chkchr.sd7         0.033973           2809
seed7/prg/chkcmd.sd7         0.033651           1205
seed7/prg/chkdb.sd7          0.034725           7454
seed7/prg/chkdecl.sd7        0.033645            448
seed7/prg/chkenum.sd7        0.033580           1230
seed7/prg/chkerr.sd7         0.034194           4663
seed7/prg/chkexc.sd7         0.033469           2627
seed7/prg/chkfil.sd7         0.033004           1615
seed7/prg/chkflt.sd7         0.036848          20620
seed7/prg/chkhent.sd7        0.033482             54
seed7/prg/chkhsh.sd7         0.033773           4548
seed7/prg/chkidx.sd7         0.034848          19567
seed7/prg/chkint.sd7         0.039414          38129
seed7/prg/chkjson.sd7        0.034446           1764
seed7/prg/chkovf.sd7         0.033617           8216
seed7/prg/chkprc.sd7         0.033164          10111
seed7/prg/chkscan.sd7        0.036062            714
seed7/prg/chkset.sd7         0.038868          11974
seed7/prg/chkstr.sd7         0.038375          26952
seed7/prg/chktime.sd7        0.033929           2025
seed7/prg/chktoml.sd7        0.033882           1656
seed7/prg/clock.sd7          0.033271             47
seed7/prg/clock2.sd7         0.033577             43
seed7/prg/clock3.sd7         0.033358             95
seed7/prg/cmpfil.sd7         0.034007             84
seed7/prg/comanche.sd7       0.033666            180
seed7/prg/confval.sd7        0.033385            175
seed7/prg/db7.sd7            0.033664            417
seed7/prg/diff7.sd7          0.033997            263
seed7/prg/dirtst.sd7         0.033560             42
seed7/prg/dirx.sd7           0.033844            152
seed7/prg/dnafight.sd7       0.033916           1381
seed7/prg/dragon.sd7         0.033761             73
seed7/prg/echo.sd7           0.033804             39
seed7/prg/eliza.sd7          0.033931            302
seed7/prg/err.sd7            0.033657             96
seed7/prg/fannkuch.sd7       0.033722            131
seed7/prg/fib.sd7            0.033737             47
seed7/prg/find7.sd7          0.033609            133
seed7/prg/findchar.sd7       0.033682            149
seed7/prg/fractree.sd7       0.033770             55
seed7/prg/ftp7.sd7           0.033615            296
seed7/prg/ftpserv.sd7        0.032668             74
seed7/prg/gcd.sd7            0.033366            109
seed7/prg/gkbd.sd7           0.032721            358
seed7/prg/gtksvtst.sd7       0.032737             94
seed7/prg/hal.sd7            0.032864            250
seed7/prg/hamu.sd7           0.033659            573
seed7/prg/hanoi.sd7          0.034832             55
seed7/prg/hd.sd7             0.034037             79
seed7/prg/hello.sd7          0.033707             32
seed7/prg/hilbert.sd7        0.033424            108
seed7/prg/ide7.sd7           0.034145            196
seed7/prg/kbd.sd7            0.033760             49
seed7/prg/klondike.sd7       0.033873            883
seed7/prg/lander.sd7         0.034029           1551
seed7/prg/lst80bas.sd7       0.033879            344
seed7/prg/lst99bas.sd7       0.033815            401
seed7/prg/lstgwbas.sd7       0.033716            577
seed7/prg/mahjong.sd7        0.033796           1943
seed7/prg/make7.sd7          0.033454            121
seed7/prg/mandelbr.sd7       0.033798            237
seed7/prg/mind.sd7           0.033561            443
seed7/prg/mirror.sd7         0.033508            131
seed7/prg/ms.sd7             0.033850            641
seed7/prg/nicoma.sd7         0.033408            135
seed7/prg/pac.sd7            0.034080            726
seed7/prg/pairs.sd7          0.033604           2025
seed7/prg/panic.sd7          0.033730           2634
seed7/prg/percolation.sd7    0.033595            330
seed7/prg/planets.sd7        0.033802           1486
seed7/prg/portfwd7.sd7       0.033760            139
seed7/prg/prime.sd7          0.033710             74
seed7/prg/printpi1.sd7       0.033242             56
seed7/prg/printpi2.sd7       0.032765             54
seed7/prg/printpi3.sd7       0.032694             60
seed7/prg/pv7.sd7            0.032860            337
seed7/prg/queen.sd7          0.032651            149
seed7/prg/rand.sd7           0.032901            121
seed7/prg/raytrace.sd7       0.033964            538
seed7/prg/rever.sd7          0.033427            816
seed7/prg/roman.sd7          0.032894             38
seed7/prg/s7c.sd7            0.034156           9060
seed7/prg/s7check.sd7        0.033409             68
seed7/prg/savehd7.sd7        0.033906           1110
seed7/prg/self.sd7           0.033669             49
seed7/prg/shisen.sd7         0.033691           1423
seed7/prg/sl.sd7             0.033872           1029
seed7/prg/snake.sd7          0.033364            615
seed7/prg/sokoban.sd7        0.034018            891
seed7/prg/spigotpi.sd7       0.033609             64
seed7/prg/sql7.sd7           0.033804            278
seed7/prg/startrek.sd7       0.033814            979
seed7/prg/sudoku7.sd7        0.033849           2657
seed7/prg/sydir7.sd7         0.033795            384
seed7/prg/syntaxhl.sd7       0.033611            177
seed7/prg/tak.sd7            0.033680             59
seed7/prg/tar7.sd7           0.033434            121
seed7/prg/tch.sd7            0.033544             55
seed7/prg/testfont.sd7       0.033707             95
seed7/prg/tet.sd7            0.033705            479
seed7/prg/tetg.sd7           0.033907            501
seed7/prg/toutf8.sd7         0.033633            240
seed7/prg/tst_cli.sd7        0.033493             40
seed7/prg/tst_srv.sd7        0.033717             47
seed7/prg/wator.sd7          0.035849            651
seed7/prg/which.sd7          0.033363             65
seed7/prg/wiz.sd7            0.033575           2833
seed7/prg/wordcnt.sd7        0.032618             54
seed7/prg/wrinum.sd7         0.032664             43
seed7/prg/wumpus.sd7         0.032676            372
seed7/lib/aes.s7i            0.035138           1144
seed7/lib/aes_gcm.s7i        0.033925            392
seed7/lib/ar.s7i             0.033540           1532
seed7/lib/arc4.s7i           0.033803            144
seed7/lib/archive.s7i        0.033804            143
seed7/lib/archive_base.s7i   0.033703            135
seed7/lib/array.s7i          0.033613            610
seed7/lib/asn1.s7i           0.034118            544
seed7/lib/asn1oid.s7i        0.033497            157
seed7/lib/basearray.s7i      0.033795            450
seed7/lib/bigfile.s7i        0.033856            136
seed7/lib/bigint.s7i         0.033591            824
seed7/lib/bigrat.s7i         0.033825            784
seed7/lib/bin16.s7i          0.033441            592
seed7/lib/bin32.s7i          0.033343            490
seed7/lib/bin64.s7i          0.033840            539
seed7/lib/bitdata.s7i        0.033721           1330
seed7/lib/bitmapfont.s7i     0.032835            215
seed7/lib/bitset.s7i         0.032748            593
seed7/lib/bitsetof.s7i       0.032961            431
seed7/lib/blowfish.s7i       0.032917            383
seed7/lib/bmp.s7i            0.033144            924
seed7/lib/boolean.s7i        0.032793            403
seed7/lib/browser.s7i        0.033170            280
seed7/lib/bstring.s7i        0.032846            227
seed7/lib/bytedata.s7i       0.032770            482
seed7/lib/bzip2.s7i          0.033011            887
seed7/lib/cards.s7i          0.032769           1342
seed7/lib/category.s7i       0.033227            209
seed7/lib/cc_conf.s7i        0.032813           1314
seed7/lib/ccittfax.s7i       0.032539           1022
seed7/lib/cgi.s7i            0.033830            109
seed7/lib/cgidialog.s7i      0.035156           1118
seed7/lib/char.s7i           0.034037            356
seed7/lib/charsets.s7i       0.034378           2024
seed7/lib/chartype.s7i       0.033632            121
seed7/lib/cipher.s7i         0.033867            146
seed7/lib/cli_cmds.s7i       0.033899           1360
seed7/lib/clib_file.s7i      0.033959            301
seed7/lib/color.s7i          0.033891            185
seed7/lib/complex.s7i        0.033638            464
seed7/lib/compress.s7i       0.033535            150
seed7/lib/console.s7i        0.033967            188
seed7/lib/cpio.s7i           0.033660           1708
seed7/lib/crc32.s7i          0.033774            193
seed7/lib/cronos16.s7i       0.034104           1173
seed7/lib/cronos27.s7i       0.033815           1464
seed7/lib/csv.s7i            0.033703            201
seed7/lib/db_prop.s7i        0.034512            991
seed7/lib/deflate.s7i        0.033506            740
seed7/lib/des.s7i            0.033986            444
seed7/lib/dialog.s7i         0.034051            311
seed7/lib/dir.s7i            0.033621            163
seed7/lib/draw.s7i           0.033528            854
seed7/lib/duration.s7i       0.033945           1038
seed7/lib/echo.s7i           0.033763            132
seed7/lib/editline.s7i       0.033759            398
seed7/lib/elf.s7i            0.033286           1560
seed7/lib/elliptic.s7i       0.032857            649
seed7/lib/enable_io.s7i      0.032831            312
seed7/lib/encoding.s7i       0.032645            931
seed7/lib/enumeration.s7i    0.033017            236
seed7/lib/environment.s7i    0.032737            175
seed7/lib/exif.s7i           0.034320            152
seed7/lib/external_file.s7i  0.033866            340
seed7/lib/field.s7i          0.033623            268
seed7/lib/file.s7i           0.033804            372
seed7/lib/filebits.s7i       0.033641             46
seed7/lib/filesys.s7i        0.033708            601
seed7/lib/fileutil.s7i       0.033633            144
seed7/lib/fixarray.s7i       0.033704            307
seed7/lib/float.s7i          0.033609            757
seed7/lib/font.s7i           0.034233            196
seed7/lib/font8x8.s7i        0.033751            998
seed7/lib/forloop.s7i        0.033770            449
seed7/lib/ftp.s7i            0.033528            969
seed7/lib/ftpserv.s7i        0.033130            631
seed7/lib/getf.s7i           0.032512            115
seed7/lib/gethttp.s7i        0.032866             41
seed7/lib/gethttps.s7i       0.033007             41
seed7/lib/gif.s7i            0.032423            561
seed7/lib/graph.s7i          0.032770            415
seed7/lib/graph_file.s7i     0.032777            399
seed7/lib/gtkserver.s7i      0.032565            161
seed7/lib/gzip.s7i           0.033333            573
seed7/lib/hash.s7i           0.033734            421
seed7/lib/hashsetof.s7i      0.033581            499
seed7/lib/hmac.s7i           0.033833            152
seed7/lib/html.s7i           0.033560             83
seed7/lib/html_ent.s7i       0.033392            476
seed7/lib/htmldom.s7i        0.032782            286
seed7/lib/http_request.s7i   0.033189            696
seed7/lib/http_srv_resp.s7i  0.032936            380
seed7/lib/https_request.s7i  0.032933            211
seed7/lib/httpserv.s7i       0.032770            345
seed7/lib/huffman.s7i        0.034424            644
seed7/lib/ico.s7i            0.034152            221
seed7/lib/idxarray.s7i       0.033668            232
seed7/lib/image.s7i          0.033784            156
seed7/lib/imagefile.s7i      0.033924            171
seed7/lib/inflate.s7i        0.033616            411
seed7/lib/inifile.s7i        0.033829            129
seed7/lib/integer.s7i        0.033624            663
seed7/lib/iobuffer.s7i       0.033571            289
seed7/lib/jpeg.s7i           0.033902           1761
seed7/lib/json.s7i           0.033829            891
seed7/lib/json_serde.s7i     0.033630            783
seed7/lib/keybd.s7i          0.033771            639
seed7/lib/keydescr.s7i       0.033929            192
seed7/lib/leb128.s7i         0.033810            218
seed7/lib/line.s7i           0.033781            164
seed7/lib/listener.s7i       0.034244            247
seed7/lib/logfile.s7i        0.033476             73
seed7/lib/lower.s7i          0.033583            142
seed7/lib/lzma.s7i           0.033453            934
seed7/lib/lzw.s7i            0.033764            861
seed7/lib/magic.s7i          0.033930            403
seed7/lib/mahjng32.s7i       0.033831           1500
seed7/lib/make.s7i           0.033667            544
seed7/lib/makedata.s7i       0.033769           1428
seed7/lib/math.s7i           0.033811            201
seed7/lib/mixarith.s7i       0.032854            249
seed7/lib/modern27.s7i       0.033296           1099
seed7/lib/more.s7i           0.032621            130
seed7/lib/msgdigest.s7i      0.032566           1222
seed7/lib/multiscr.s7i       0.032626             68
seed7/lib/null_file.s7i      0.032740            345
seed7/lib/osfiles.s7i        0.035192           1085
seed7/lib/pbm.s7i            0.034552            230
seed7/lib/pcx.s7i            0.037061            638
seed7/lib/pem.s7i            0.035313            185
seed7/lib/pgm.s7i            0.033963            238
seed7/lib/pic16.s7i          0.034046           1037
seed7/lib/pic32.s7i          0.033745           2060
seed7/lib/pic_util.s7i       0.034201            144
seed7/lib/pixelimage.s7i     0.033888            320
seed7/lib/pixmap_file.s7i    0.033741            459
seed7/lib/pixmapfont.s7i     0.033854            184
seed7/lib/pkcs1.s7i          0.033686            543
seed7/lib/png.s7i            0.033985           1064
seed7/lib/poll.s7i           0.033724            313
seed7/lib/ppm.s7i            0.033583            240
seed7/lib/process.s7i        0.033529            541
seed7/lib/progs.s7i          0.034205            789
seed7/lib/propertyfile.s7i   0.033849            155
seed7/lib/rational.s7i       0.033825            792
seed7/lib/ref_list.s7i       0.033856            252
seed7/lib/reference.s7i      0.033909            126
seed7/lib/reverse.s7i        0.032931             94
seed7/lib/rpm.s7i            0.034210           3487
seed7/lib/rpmext.s7i         0.033823            318
seed7/lib/scanfile.s7i       0.033506           1779
seed7/lib/scanjson.s7i       0.032939            413
seed7/lib/scanstri.s7i       0.033106           1814
seed7/lib/scantoml.s7i       0.033023           1603
seed7/lib/seed7_05.s7i       0.032719           1072
seed7/lib/set.s7i            0.032485             57
seed7/lib/shell.s7i          0.032835            615
seed7/lib/showtls.s7i        0.034871            678
seed7/lib/signature.s7i      0.033847            131
seed7/lib/smtp.s7i           0.033383            261
seed7/lib/sockbase.s7i       0.033687            217
seed7/lib/socket.s7i         0.034002            326
seed7/lib/sokoban1.s7i       0.034066           1519
seed7/lib/sql_base.s7i       0.033828           1000
seed7/lib/stars.s7i          0.033616           1705
seed7/lib/stdfont10.s7i      0.033673           3347
seed7/lib/stdfont12.s7i      0.033930           3928
seed7/lib/stdfont14.s7i      0.034043           4510
seed7/lib/stdfont16.s7i      0.034001           5092
seed7/lib/stdfont18.s7i      0.034069           5868
seed7/lib/stdfont20.s7i      0.033999           6449
seed7/lib/stdfont24.s7i      0.034204           7421
seed7/lib/stdfont8.s7i       0.033727           2960
seed7/lib/stdfont9.s7i       0.033679           3152
seed7/lib/stdio.s7i          0.033879            192
seed7/lib/strifile.s7i       0.033568            345
seed7/lib/string.s7i         0.033712            779
seed7/lib/stritext.s7i       0.034123            352
seed7/lib/struct.s7i         0.033698            266
seed7/lib/struct_elem.s7i    0.033720            129
seed7/lib/subfile.s7i        0.033679            174
seed7/lib/subrange.s7i       0.032576             78
seed7/lib/syntax.s7i         0.032726            294
seed7/lib/tar.s7i            0.032968           1880
seed7/lib/tar_cmds.s7i       0.032903            752
seed7/lib/tdes.s7i           0.032786            143
seed7/lib/tee.s7i            0.033377            143
seed7/lib/text.s7i           0.033765            135
seed7/lib/tga.s7i            0.033701            676
seed7/lib/tiff.s7i           0.034323           2771
seed7/lib/time.s7i           0.033918           1191
seed7/lib/tls.s7i            0.033623           2230
seed7/lib/unicode.s7i        0.033633            575
seed7/lib/unionfnd.s7i       0.033707            130
seed7/lib/upper.s7i          0.033706            142
seed7/lib/utf16.s7i          0.033577            540
seed7/lib/utf8.s7i           0.033738            234
seed7/lib/vecfont10.s7i      0.033824           1056
seed7/lib/vecfont18.s7i      0.034159           1119
seed7/lib/vector3d.s7i       0.033429            293
seed7/lib/vectorfont.s7i     0.033813            239
seed7/lib/wildcard.s7i       0.033973            140
seed7/lib/window.s7i         0.033826            455
seed7/lib/wrinum.s7i         0.033613            248
seed7/lib/x509cert.s7i       0.033897           1243
seed7/lib/xml_ent.s7i        0.033637             94
seed7/lib/xmldom.s7i         0.033717            303
seed7/lib/xz.s7i             0.033793            442
seed7/lib/zip.s7i            0.033949           2792
seed7/lib/zstd.s7i           0.033548           1263
============================ ================= ================

Statistical Summary — Mode A (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.033717        |
+-----------+-----------------+
| Minimum   | 0.032423        |
+-----------+-----------------+
| Maximum   | 0.039414        |
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
seed7/prg/addup.sd7          0.039774            190
seed7/prg/bas7.sd7           0.041561          11459
seed7/prg/bifurk.sd7         0.035641             73
seed7/prg/bigfiles.sd7       0.037792            129
seed7/prg/brainf7.sd7        0.036725             86
seed7/prg/calc7.sd7          0.037036            128
seed7/prg/carddemo.sd7       0.037686            190
seed7/prg/castle.sd7         0.040464           3148
seed7/prg/cat.sd7            0.037737             82
seed7/prg/cellauto.sd7       0.037580             85
seed7/prg/celsius.sd7        0.036339             42
seed7/prg/chk_all.sd7        0.039364            843
seed7/prg/chkarr.sd7         0.039472           8367
seed7/prg/chkbig.sd7         0.044594          29026
seed7/prg/chkbin.sd7         0.040601           6469
seed7/prg/chkbitdata.sd7     0.040655           6624
seed7/prg/chkbool.sd7        0.039029           3157
seed7/prg/chkbst.sd7         0.039412            722
seed7/prg/chkchr.sd7         0.041214           2809
seed7/prg/chkcmd.sd7         0.038822           1205
seed7/prg/chkdb.sd7          0.040152           7454
seed7/prg/chkdecl.sd7        0.039375            448
seed7/prg/chkenum.sd7        0.038147           1230
seed7/prg/chkerr.sd7         0.040119           4663
seed7/prg/chkexc.sd7         0.038954           2627
seed7/prg/chkfil.sd7         0.038833           1615
seed7/prg/chkflt.sd7         0.042943          20620
seed7/prg/chkhent.sd7        0.036667             54
seed7/prg/chkhsh.sd7         0.040656           4548
seed7/prg/chkidx.sd7         0.040204          19567
seed7/prg/chkint.sd7         0.044760          38129
seed7/prg/chkjson.sd7        0.038212           1764
seed7/prg/chkovf.sd7         0.038342           8216
seed7/prg/chkprc.sd7         0.036903          10111
seed7/prg/chkscan.sd7        0.039634            714
seed7/prg/chkset.sd7         0.040667          11974
seed7/prg/chkstr.sd7         0.043823          26952
seed7/prg/chktime.sd7        0.040168           2025
seed7/prg/chktoml.sd7        0.039455           1656
seed7/prg/clock.sd7          0.036244             47
seed7/prg/clock2.sd7         0.035931             43
seed7/prg/clock3.sd7         0.038861             95
seed7/prg/cmpfil.sd7         0.037276             84
seed7/prg/comanche.sd7       0.039976            180
seed7/prg/confval.sd7        0.039845            175
seed7/prg/db7.sd7            0.038673            417
seed7/prg/diff7.sd7          0.039508            263
seed7/prg/dirtst.sd7         0.035832             42
seed7/prg/dirx.sd7           0.039148            152
seed7/prg/dnafight.sd7       0.039256           1381
seed7/prg/dragon.sd7         0.037084             73
seed7/prg/echo.sd7           0.035840             39
seed7/prg/eliza.sd7          0.038480            302
seed7/prg/err.sd7            0.041497             96
seed7/prg/fannkuch.sd7       0.038850            131
seed7/prg/fib.sd7            0.036270             47
seed7/prg/find7.sd7          0.038128            133
seed7/prg/findchar.sd7       0.037644            149
seed7/prg/fractree.sd7       0.036008             55
seed7/prg/ftp7.sd7           0.038263            296
seed7/prg/ftpserv.sd7        0.036636             74
seed7/prg/gcd.sd7            0.036600            109
seed7/prg/gkbd.sd7           0.041739            358
seed7/prg/gtksvtst.sd7       0.037901             94
seed7/prg/hal.sd7            0.038748            250
seed7/prg/hamu.sd7           0.039044            573
seed7/prg/hanoi.sd7          0.036748             55
seed7/prg/hd.sd7             0.037358             79
seed7/prg/hello.sd7          0.035914             32
seed7/prg/hilbert.sd7        0.038002            108
seed7/prg/ide7.sd7           0.039122            196
seed7/prg/kbd.sd7            0.036309             49
seed7/prg/klondike.sd7       0.038791            883
seed7/prg/lander.sd7         0.039758           1551
seed7/prg/lst80bas.sd7       0.038239            344
seed7/prg/lst99bas.sd7       0.040226            401
seed7/prg/lstgwbas.sd7       0.040155            577
seed7/prg/mahjong.sd7        0.039242           1943
seed7/prg/make7.sd7          0.039133            121
seed7/prg/mandelbr.sd7       0.038886            237
seed7/prg/mind.sd7           0.039333            443
seed7/prg/mirror.sd7         0.039784            131
seed7/prg/ms.sd7             0.038915            641
seed7/prg/nicoma.sd7         0.037802            135
seed7/prg/pac.sd7            0.037655            726
seed7/prg/pairs.sd7          0.038169           2025
seed7/prg/panic.sd7          0.038258           2634
seed7/prg/percolation.sd7    0.037776            330
seed7/prg/planets.sd7        0.039702           1486
seed7/prg/portfwd7.sd7       0.038787            139
seed7/prg/prime.sd7          0.037227             74
seed7/prg/printpi1.sd7       0.036577             56
seed7/prg/printpi2.sd7       0.036592             54
seed7/prg/printpi3.sd7       0.037002             60
seed7/prg/pv7.sd7            0.038872            337
seed7/prg/queen.sd7          0.038992            149
seed7/prg/rand.sd7           0.038755            121
seed7/prg/raytrace.sd7       0.039390            538
seed7/prg/rever.sd7          0.039329            816
seed7/prg/roman.sd7          0.036689             38
seed7/prg/s7c.sd7            0.039531           9060
seed7/prg/s7check.sd7        0.037286             68
seed7/prg/savehd7.sd7        0.039173           1110
seed7/prg/self.sd7           0.036279             49
seed7/prg/shisen.sd7         0.041451           1423
seed7/prg/sl.sd7             0.039802           1029
seed7/prg/snake.sd7          0.039184            615
seed7/prg/sokoban.sd7        0.038652            891
seed7/prg/spigotpi.sd7       0.036779             64
seed7/prg/sql7.sd7           0.038799            278
seed7/prg/startrek.sd7       0.039054            979
seed7/prg/sudoku7.sd7        0.038012           2657
seed7/prg/sydir7.sd7         0.037755            384
seed7/prg/syntaxhl.sd7       0.040387            177
seed7/prg/tak.sd7            0.035933             59
seed7/prg/tar7.sd7           0.037935            121
seed7/prg/tch.sd7            0.038407             55
seed7/prg/testfont.sd7       0.039394             95
seed7/prg/tet.sd7            0.039365            479
seed7/prg/tetg.sd7           0.039147            501
seed7/prg/toutf8.sd7         0.043206            240
seed7/prg/tst_cli.sd7        0.037136             40
seed7/prg/tst_srv.sd7        0.037197             47
seed7/prg/wator.sd7          0.039185            651
seed7/prg/which.sd7          0.037278             65
seed7/prg/wiz.sd7            0.039058           2833
seed7/prg/wordcnt.sd7        0.036434             54
seed7/prg/wrinum.sd7         0.036001             43
seed7/prg/wumpus.sd7         0.038856            372
seed7/lib/aes.s7i            0.044037           1144
seed7/lib/aes_gcm.s7i        0.045484            392
seed7/lib/ar.s7i             0.040273           1532
seed7/lib/arc4.s7i           0.039581            144
seed7/lib/archive.s7i        0.038679            143
seed7/lib/archive_base.s7i   0.038855            135
seed7/lib/array.s7i          0.039184            610
seed7/lib/asn1.s7i           0.037684            544
seed7/lib/asn1oid.s7i        0.043004            157
seed7/lib/basearray.s7i      0.039132            450
seed7/lib/bigfile.s7i        0.037736            136
seed7/lib/bigint.s7i         0.037570            824
seed7/lib/bigrat.s7i         0.037910            784
seed7/lib/bin16.s7i          0.038039            592
seed7/lib/bin32.s7i          0.039379            490
seed7/lib/bin64.s7i          0.039304            539
seed7/lib/bitdata.s7i        0.044821           1330
seed7/lib/bitmapfont.s7i     0.039013            215
seed7/lib/bitset.s7i         0.039150            593
seed7/lib/bitsetof.s7i       0.040467            431
seed7/lib/blowfish.s7i       0.042518            383
seed7/lib/bmp.s7i            0.039929            924
seed7/lib/boolean.s7i        0.038943            403
seed7/lib/browser.s7i        0.039189            280
seed7/lib/bstring.s7i        0.038695            227
seed7/lib/bytedata.s7i       0.038719            482
seed7/lib/bzip2.s7i          0.038890            887
seed7/lib/cards.s7i          0.037032           1342
seed7/lib/category.s7i       0.039605            209
seed7/lib/cc_conf.s7i        0.038793           1314
seed7/lib/ccittfax.s7i       0.039048           1022
seed7/lib/cgi.s7i            0.038259            109
seed7/lib/cgidialog.s7i      0.038783           1118
seed7/lib/char.s7i           0.038832            356
seed7/lib/charsets.s7i       0.039404           2024
seed7/lib/chartype.s7i       0.042110            121
seed7/lib/cipher.s7i         0.037894            146
seed7/lib/cli_cmds.s7i       0.038095           1360
seed7/lib/clib_file.s7i      0.037972            301
seed7/lib/color.s7i          0.038189            185
seed7/lib/complex.s7i        0.037927            464
seed7/lib/compress.s7i       0.038967            150
seed7/lib/console.s7i        0.038586            188
seed7/lib/cpio.s7i           0.039126           1708
seed7/lib/crc32.s7i          0.039847            193
seed7/lib/cronos16.s7i       0.042624           1173
seed7/lib/cronos27.s7i       0.043017           1464
seed7/lib/csv.s7i            0.038764            201
seed7/lib/db_prop.s7i        0.038943            991
seed7/lib/deflate.s7i        0.039921            740
seed7/lib/des.s7i            0.039876            444
seed7/lib/dialog.s7i         0.043154            311
seed7/lib/dir.s7i            0.040432            163
seed7/lib/draw.s7i           0.039447            854
seed7/lib/duration.s7i       0.039535           1038
seed7/lib/echo.s7i           0.038721            132
seed7/lib/editline.s7i       0.038663            398
seed7/lib/elf.s7i            0.040300           1560
seed7/lib/elliptic.s7i       0.039393            649
seed7/lib/enable_io.s7i      0.038666            312
seed7/lib/encoding.s7i       0.039629            931
seed7/lib/enumeration.s7i    0.038916            236
seed7/lib/environment.s7i    0.038345            175
seed7/lib/exif.s7i           0.039074            152
seed7/lib/external_file.s7i  0.038012            340
seed7/lib/field.s7i          0.037730            268
seed7/lib/file.s7i           0.037527            372
seed7/lib/filebits.s7i       0.035778             46
seed7/lib/filesys.s7i        0.038809            601
seed7/lib/fileutil.s7i       0.039257            144
seed7/lib/fixarray.s7i       0.040092            307
seed7/lib/float.s7i          0.039137            757
seed7/lib/font.s7i           0.038333            196
seed7/lib/font8x8.s7i        0.038187            998
seed7/lib/forloop.s7i        0.040117            449
seed7/lib/ftp.s7i            0.039164            969
seed7/lib/ftpserv.s7i        0.038804            631
seed7/lib/getf.s7i           0.038319            115
seed7/lib/gethttp.s7i        0.036483             41
seed7/lib/gethttps.s7i       0.036397             41
seed7/lib/gif.s7i            0.038927            561
seed7/lib/graph.s7i          0.040682            415
seed7/lib/graph_file.s7i     0.039152            399
seed7/lib/gtkserver.s7i      0.038087            161
seed7/lib/gzip.s7i           0.039364            573
seed7/lib/hash.s7i           0.040428            421
seed7/lib/hashsetof.s7i      0.040849            499
seed7/lib/hmac.s7i           0.038760            152
seed7/lib/html.s7i           0.037693             83
seed7/lib/html_ent.s7i       0.039261            476
seed7/lib/htmldom.s7i        0.038975            286
seed7/lib/http_request.s7i   0.038077            696
seed7/lib/http_srv_resp.s7i  0.037646            380
seed7/lib/https_request.s7i  0.037782            211
seed7/lib/httpserv.s7i       0.037696            345
seed7/lib/huffman.s7i        0.038374            644
seed7/lib/ico.s7i            0.040660            221
seed7/lib/idxarray.s7i       0.039035            232
seed7/lib/image.s7i          0.038434            156
seed7/lib/imagefile.s7i      0.038713            171
seed7/lib/inflate.s7i        0.039263            411
seed7/lib/inifile.s7i        0.038966            129
seed7/lib/integer.s7i        0.039413            663
seed7/lib/iobuffer.s7i       0.039328            289
seed7/lib/jpeg.s7i           0.038971           1761
seed7/lib/json.s7i           0.038197            891
seed7/lib/json_serde.s7i     0.037541            783
seed7/lib/keybd.s7i          0.037865            639
seed7/lib/keydescr.s7i       0.038864            192
seed7/lib/leb128.s7i         0.038285            218
seed7/lib/line.s7i           0.038413            164
seed7/lib/listener.s7i       0.037865            247
seed7/lib/logfile.s7i        0.036157             73
seed7/lib/lower.s7i          0.037313            142
seed7/lib/lzma.s7i           0.039597            934
seed7/lib/lzw.s7i            0.039198            861
seed7/lib/magic.s7i          0.040863            403
seed7/lib/mahjng32.s7i       0.038591           1500
seed7/lib/make.s7i           0.038324            544
seed7/lib/makedata.s7i       0.038025           1428
seed7/lib/math.s7i           0.037805            201
seed7/lib/mixarith.s7i       0.037575            249
seed7/lib/modern27.s7i       0.041605           1099
seed7/lib/more.s7i           0.038535            130
seed7/lib/msgdigest.s7i      0.040005           1222
seed7/lib/multiscr.s7i       0.037520             68
seed7/lib/null_file.s7i      0.040045            345
seed7/lib/osfiles.s7i        0.042140           1085
seed7/lib/pbm.s7i            0.039315            230
seed7/lib/pcx.s7i            0.039171            638
seed7/lib/pem.s7i            0.038904            185
seed7/lib/pgm.s7i            0.039126            238
seed7/lib/pic16.s7i          0.038242           1037
seed7/lib/pic32.s7i          0.037978           2060
seed7/lib/pic_util.s7i       0.038954            144
seed7/lib/pixelimage.s7i     0.038694            320
seed7/lib/pixmap_file.s7i    0.038637            459
seed7/lib/pixmapfont.s7i     0.040179            184
seed7/lib/pkcs1.s7i          0.044232            543
seed7/lib/png.s7i            0.038902           1064
seed7/lib/poll.s7i           0.038938            313
seed7/lib/ppm.s7i            0.039158            240
seed7/lib/process.s7i        0.040314            541
seed7/lib/progs.s7i          0.038894            789
seed7/lib/propertyfile.s7i   0.039390            155
seed7/lib/rational.s7i       0.042018            792
seed7/lib/ref_list.s7i       0.040787            252
seed7/lib/reference.s7i      0.038228            126
seed7/lib/reverse.s7i        0.036747             94
seed7/lib/rpm.s7i            0.038481           3487
seed7/lib/rpmext.s7i         0.039237            318
seed7/lib/scanfile.s7i       0.039193           1779
seed7/lib/scanjson.s7i       0.039907            413
seed7/lib/scanstri.s7i       0.039103           1814
seed7/lib/scantoml.s7i       0.039246           1603
seed7/lib/seed7_05.s7i       0.041095           1072
seed7/lib/set.s7i            0.037119             57
seed7/lib/shell.s7i          0.039097            615
seed7/lib/showtls.s7i        0.039545            678
seed7/lib/signature.s7i      0.039005            131
seed7/lib/smtp.s7i           0.038895            261
seed7/lib/sockbase.s7i       0.040442            217
seed7/lib/socket.s7i         0.039332            326
seed7/lib/sokoban1.s7i       0.038295           1519
seed7/lib/sql_base.s7i       0.038787           1000
seed7/lib/stars.s7i          0.041030           1705
seed7/lib/stdfont10.s7i      0.038041           3347
seed7/lib/stdfont12.s7i      0.039161           3928
seed7/lib/stdfont14.s7i      0.038599           4510
seed7/lib/stdfont16.s7i      0.038797           5092
seed7/lib/stdfont18.s7i      0.038951           5868
seed7/lib/stdfont20.s7i      0.039115           6449
seed7/lib/stdfont24.s7i      0.038375           7421
seed7/lib/stdfont8.s7i       0.036639           2960
seed7/lib/stdfont9.s7i       0.036451           3152
seed7/lib/stdio.s7i          0.037405            192
seed7/lib/strifile.s7i       0.037843            345
seed7/lib/string.s7i         0.037351            779
seed7/lib/stritext.s7i       0.040396            352
seed7/lib/struct.s7i         0.040734            266
seed7/lib/struct_elem.s7i    0.038995            129
seed7/lib/subfile.s7i        0.039065            174
seed7/lib/subrange.s7i       0.037966             78
seed7/lib/syntax.s7i         0.040230            294
seed7/lib/tar.s7i            0.039777           1880
seed7/lib/tar_cmds.s7i       0.038976            752
seed7/lib/tdes.s7i           0.038527            143
seed7/lib/tee.s7i            0.038779            143
seed7/lib/text.s7i           0.038676            135
seed7/lib/tga.s7i            0.040912            676
seed7/lib/tiff.s7i           0.041532           2771
seed7/lib/time.s7i           0.039273           1191
seed7/lib/tls.s7i            0.039132           2230
seed7/lib/unicode.s7i        0.041026            575
seed7/lib/unionfnd.s7i       0.038884            130
seed7/lib/upper.s7i          0.038653            142
seed7/lib/utf16.s7i          0.038445            540
seed7/lib/utf8.s7i           0.040103            234
seed7/lib/vecfont10.s7i      0.042395           1056
seed7/lib/vecfont18.s7i      0.042223           1119
seed7/lib/vector3d.s7i       0.038405            293
seed7/lib/vectorfont.s7i     0.038146            239
seed7/lib/wildcard.s7i       0.037743            140
seed7/lib/window.s7i         0.037648            455
seed7/lib/wrinum.s7i         0.038481            248
seed7/lib/x509cert.s7i       0.040673           1243
seed7/lib/xml_ent.s7i        0.038899             94
seed7/lib/xmldom.s7i         0.038764            303
seed7/lib/xz.s7i             0.038953            442
seed7/lib/zip.s7i            0.039001           2792
seed7/lib/zstd.s7i           0.039218           1263
============================ ================= ================

Statistical Summary — Mode B (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.038971        |
+-----------+-----------------+
| Minimum   | 0.035641        |
+-----------+-----------------+
| Maximum   | 0.045484        |
+-----------+-----------------+

Mode C — Mode Activation + Full-Buffer Fontification (font-lock-ensure)
-----------------------------------------------------------------------

Full-buffer fontification stress test.

File Load Times — Mode C (mean, GC-free)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

============================ ================= ================
File Name                    Mean Time (s)     Line count
============================ ================= ================
seed7/prg/addup.sd7          0.039796            190
seed7/prg/bas7.sd7           0.320540          11459
seed7/prg/bifurk.sd7         0.036679             73
seed7/prg/bigfiles.sd7       0.037522            129
seed7/prg/brainf7.sd7        0.036581             86
seed7/prg/calc7.sd7          0.037928            128
seed7/prg/carddemo.sd7       0.039091            190
seed7/prg/castle.sd7         0.107687           3148
seed7/prg/cat.sd7            0.036489             82
seed7/prg/cellauto.sd7       0.036197             85
seed7/prg/celsius.sd7        0.035504             42
seed7/prg/chk_all.sd7        0.058226            843
seed7/prg/chkarr.sd7         0.347293           8367
seed7/prg/chkbig.sd7         2.038781          29026
seed7/prg/chkbin.sd7         0.508225           6469
seed7/prg/chkbitdata.sd7     0.606488           6624
seed7/prg/chkbool.sd7        0.114479           3157
seed7/prg/chkbst.sd7         0.062669            722
seed7/prg/chkchr.sd7         0.214260           2809
seed7/prg/chkcmd.sd7         0.066601           1205
seed7/prg/chkdb.sd7          0.345706           7454
seed7/prg/chkdecl.sd7        0.056302            448
seed7/prg/chkenum.sd7        0.065252           1230
seed7/prg/chkerr.sd7         0.192562           4663
seed7/prg/chkexc.sd7         0.081920           2627
seed7/prg/chkfil.sd7         0.074546           1615
seed7/prg/chkflt.sd7         1.324083          20620
seed7/prg/chkhent.sd7        0.036826             54
seed7/prg/chkhsh.sd7         0.243302           4548
seed7/prg/chkidx.sd7         1.303414          19567
seed7/prg/chkint.sd7         2.496033          38129
seed7/prg/chkjson.sd7        0.099312           1764
seed7/prg/chkovf.sd7         0.552164           8216
seed7/prg/chkprc.sd7         0.318298          10111
seed7/prg/chkscan.sd7        0.058276            714
seed7/prg/chkset.sd7         0.645921          11974
seed7/prg/chkstr.sd7         1.360840          26952
seed7/prg/chktime.sd7        0.128465           2025
seed7/prg/chktoml.sd7        0.108550           1656
seed7/prg/clock.sd7          0.036140             47
seed7/prg/clock2.sd7         0.035350             43
seed7/prg/clock3.sd7         0.037091             95
seed7/prg/cmpfil.sd7         0.036180             84
seed7/prg/comanche.sd7       0.039797            180
seed7/prg/confval.sd7        0.042068            175
seed7/prg/db7.sd7            0.045966            417
seed7/prg/diff7.sd7          0.041202            263
seed7/prg/dirtst.sd7         0.034234             42
seed7/prg/dirx.sd7           0.036620            152
seed7/prg/dnafight.sd7       0.065114           1381
seed7/prg/dragon.sd7         0.036770             73
seed7/prg/echo.sd7           0.035572             39
seed7/prg/eliza.sd7          0.042800            302
seed7/prg/err.sd7            0.040573             96
seed7/prg/fannkuch.sd7       0.037142            131
seed7/prg/fib.sd7            0.035300             47
seed7/prg/find7.sd7          0.037657            133
seed7/prg/findchar.sd7       0.038359            149
seed7/prg/fractree.sd7       0.035888             55
seed7/prg/ftp7.sd7           0.043128            296
seed7/prg/ftpserv.sd7        0.036898             74
seed7/prg/gcd.sd7            0.035564            109
seed7/prg/gkbd.sd7           0.044293            358
seed7/prg/gtksvtst.sd7       0.035856             94
seed7/prg/hal.sd7            0.037986            250
seed7/prg/hamu.sd7           0.047022            573
seed7/prg/hanoi.sd7          0.035226             55
seed7/prg/hd.sd7             0.034972             79
seed7/prg/hello.sd7          0.033986             32
seed7/prg/hilbert.sd7        0.036744            108
seed7/prg/ide7.sd7           0.039325            196
seed7/prg/kbd.sd7            0.034888             49
seed7/prg/klondike.sd7       0.052704            883
seed7/prg/lander.sd7         0.069187           1551
seed7/prg/lst80bas.sd7       0.042145            344
seed7/prg/lst99bas.sd7       0.043484            401
seed7/prg/lstgwbas.sd7       0.051199            577
seed7/prg/mahjong.sd7        0.079705           1943
seed7/prg/make7.sd7          0.037710            121
seed7/prg/mandelbr.sd7       0.041419            237
seed7/prg/mind.sd7           0.044932            443
seed7/prg/mirror.sd7         0.038521            131
seed7/prg/ms.sd7             0.048547            641
seed7/prg/nicoma.sd7         0.037695            135
seed7/prg/pac.sd7            0.049233            726
seed7/prg/pairs.sd7          0.081833           2025
seed7/prg/panic.sd7          0.096885           2634
seed7/prg/percolation.sd7    0.042350            330
seed7/prg/planets.sd7        0.075766           1486
seed7/prg/portfwd7.sd7       0.038566            139
seed7/prg/prime.sd7          0.035249             74
seed7/prg/printpi1.sd7       0.037763             56
seed7/prg/printpi2.sd7       0.036366             54
seed7/prg/printpi3.sd7       0.035089             60
seed7/prg/pv7.sd7            0.042598            337
seed7/prg/queen.sd7          0.037680            149
seed7/prg/rand.sd7           0.038126            121
seed7/prg/raytrace.sd7       0.048133            538
seed7/prg/rever.sd7          0.053295            816
seed7/prg/roman.sd7          0.035340             38
seed7/prg/s7c.sd7            0.277142           9060
seed7/prg/s7check.sd7        0.037158             68
seed7/prg/savehd7.sd7        0.067762           1110
seed7/prg/self.sd7           0.036094             49
seed7/prg/shisen.sd7         0.071455           1423
seed7/prg/sl.sd7             0.061365           1029
seed7/prg/snake.sd7          0.046939            615
seed7/prg/sokoban.sd7        0.052899            891
seed7/prg/spigotpi.sd7       0.035280             64
seed7/prg/sql7.sd7           0.040000            278
seed7/prg/startrek.sd7       0.056921            979
seed7/prg/sudoku7.sd7        0.099315           2657
seed7/prg/sydir7.sd7         0.045437            384
seed7/prg/syntaxhl.sd7       0.041217            177
seed7/prg/tak.sd7            0.035753             59
seed7/prg/tar7.sd7           0.037391            121
seed7/prg/tch.sd7            0.035758             55
seed7/prg/testfont.sd7       0.036976             95
seed7/prg/tet.sd7            0.044078            479
seed7/prg/tetg.sd7           0.045988            501
seed7/prg/toutf8.sd7         0.042684            240
seed7/prg/tst_cli.sd7        0.035061             40
seed7/prg/tst_srv.sd7        0.035117             47
seed7/prg/wator.sd7          0.051669            651
seed7/prg/which.sd7          0.036033             65
seed7/prg/wiz.sd7            0.103900           2833
seed7/prg/wordcnt.sd7        0.035800             54
seed7/prg/wrinum.sd7         0.035815             43
seed7/prg/wumpus.sd7         0.042048            372
seed7/lib/aes.s7i            0.107429           1144
seed7/lib/aes_gcm.s7i        0.045456            392
seed7/lib/ar.s7i             0.070887           1532
seed7/lib/arc4.s7i           0.036908            144
seed7/lib/archive.s7i        0.038833            143
seed7/lib/archive_base.s7i   0.038153            135
seed7/lib/array.s7i          0.055528            610
seed7/lib/asn1.s7i           0.051307            544
seed7/lib/asn1oid.s7i        0.042126            157
seed7/lib/basearray.s7i      0.048249            450
seed7/lib/bigfile.s7i        0.038358            136
seed7/lib/bigint.s7i         0.055344            824
seed7/lib/bigrat.s7i         0.053664            784
seed7/lib/bin16.s7i          0.050713            592
seed7/lib/bin32.s7i          0.048206            490
seed7/lib/bin64.s7i          0.049547            539
seed7/lib/bitdata.s7i        0.076242           1330
seed7/lib/bitmapfont.s7i     0.039798            215
seed7/lib/bitset.s7i         0.048801            593
seed7/lib/bitsetof.s7i       0.047479            431
seed7/lib/blowfish.s7i       0.055327            383
seed7/lib/bmp.s7i            0.058149            924
seed7/lib/boolean.s7i        0.043013            403
seed7/lib/browser.s7i        0.041473            280
seed7/lib/bstring.s7i        0.039423            227
seed7/lib/bytedata.s7i       0.050196            482
seed7/lib/bzip2.s7i          0.058401            887
seed7/lib/cards.s7i          0.066276           1342
seed7/lib/category.s7i       0.041142            209
seed7/lib/cc_conf.s7i        0.078009           1314
seed7/lib/ccittfax.s7i       0.065417           1022
seed7/lib/cgi.s7i            0.038393            109
seed7/lib/cgidialog.s7i      0.060381           1118
seed7/lib/char.s7i           0.043687            356
seed7/lib/charsets.s7i       0.081323           2024
seed7/lib/chartype.s7i       0.039794            121
seed7/lib/cipher.s7i         0.037943            146
seed7/lib/cli_cmds.s7i       0.068563           1360
seed7/lib/clib_file.s7i      0.043429            301
seed7/lib/color.s7i          0.040815            185
seed7/lib/complex.s7i        0.045176            464
seed7/lib/compress.s7i       0.037106            150
seed7/lib/console.s7i        0.038076            188
seed7/lib/cpio.s7i           0.079210           1708
seed7/lib/crc32.s7i          0.042018            193
seed7/lib/cronos16.s7i       0.093756           1173
seed7/lib/cronos27.s7i       0.116094           1464
seed7/lib/csv.s7i            0.041097            201
seed7/lib/db_prop.s7i        0.062947            991
seed7/lib/deflate.s7i        0.055592            740
seed7/lib/des.s7i            0.055298            444
seed7/lib/dialog.s7i         0.043716            311
seed7/lib/dir.s7i            0.038807            163
seed7/lib/draw.s7i           0.056065            854
seed7/lib/duration.s7i       0.061209           1038
seed7/lib/echo.s7i           0.037800            132
seed7/lib/editline.s7i       0.045321            398
seed7/lib/elf.s7i            0.085055           1560
seed7/lib/elliptic.s7i       0.052262            649
seed7/lib/enable_io.s7i      0.042328            312
seed7/lib/encoding.s7i       0.059165            931
seed7/lib/enumeration.s7i    0.040140            236
seed7/lib/environment.s7i    0.037755            175
seed7/lib/exif.s7i           0.038658            152
seed7/lib/external_file.s7i  0.044355            340
seed7/lib/field.s7i          0.042342            268
seed7/lib/file.s7i           0.044704            372
seed7/lib/filebits.s7i       0.036638             46
seed7/lib/filesys.s7i        0.048498            601
seed7/lib/fileutil.s7i       0.038253            144
seed7/lib/fixarray.s7i       0.043482            307
seed7/lib/float.s7i          0.054847            757
seed7/lib/font.s7i           0.039342            196
seed7/lib/font8x8.s7i        0.048092            998
seed7/lib/forloop.s7i        0.044878            449
seed7/lib/ftp.s7i            0.057552            969
seed7/lib/ftpserv.s7i        0.051207            631
seed7/lib/getf.s7i           0.037326            115
seed7/lib/gethttp.s7i        0.074253             41
seed7/lib/gethttps.s7i       0.044141             41
seed7/lib/gif.s7i            0.054107            561
seed7/lib/graph.s7i          0.050681            415
seed7/lib/graph_file.s7i     0.043524            399
seed7/lib/gtkserver.s7i      0.038838            161
seed7/lib/gzip.s7i           0.049967            573
seed7/lib/hash.s7i           0.049338            421
seed7/lib/hashsetof.s7i      0.049146            499
seed7/lib/hmac.s7i           0.039053            152
seed7/lib/html.s7i           0.037622             83
seed7/lib/html_ent.s7i       0.047020            476
seed7/lib/htmldom.s7i        0.044149            286
seed7/lib/http_request.s7i   0.052089            696
seed7/lib/http_srv_resp.s7i  0.045850            380
seed7/lib/https_request.s7i  0.040680            211
seed7/lib/httpserv.s7i       0.043889            345
seed7/lib/huffman.s7i        0.053024            644
seed7/lib/ico.s7i            0.040776            221
seed7/lib/idxarray.s7i       0.043989            232
seed7/lib/image.s7i          0.037887            156
seed7/lib/imagefile.s7i      0.039373            171
seed7/lib/inflate.s7i        0.046893            411
seed7/lib/inifile.s7i        0.037710            129
seed7/lib/integer.s7i        0.051963            663
seed7/lib/iobuffer.s7i       0.041593            289
seed7/lib/jpeg.s7i           0.082191           1761
seed7/lib/json.s7i           0.054836            891
seed7/lib/json_serde.s7i     0.052223            783
seed7/lib/keybd.s7i          0.054415            639
seed7/lib/keydescr.s7i       0.043224            192
seed7/lib/leb128.s7i         0.041221            218
seed7/lib/line.s7i           0.039093            164
seed7/lib/listener.s7i       0.041499            247
seed7/lib/logfile.s7i        0.037016             73
seed7/lib/lower.s7i          0.038169            142
seed7/lib/lzma.s7i           0.059898            934
seed7/lib/lzw.s7i            0.058933            861
seed7/lib/magic.s7i          0.048613            403
seed7/lib/mahjng32.s7i       0.064028           1500
seed7/lib/make.s7i           0.049502            544
seed7/lib/makedata.s7i       0.070665           1428
seed7/lib/math.s7i           0.040481            201
seed7/lib/mixarith.s7i       0.041120            249
seed7/lib/modern27.s7i       0.087749           1099
seed7/lib/more.s7i           0.040495            130
seed7/lib/msgdigest.s7i      0.077597           1222
seed7/lib/multiscr.s7i       0.036406             68
seed7/lib/null_file.s7i      0.044404            345
seed7/lib/osfiles.s7i        0.063739           1085
seed7/lib/pbm.s7i            0.041052            230
seed7/lib/pcx.s7i            0.053431            638
seed7/lib/pem.s7i            0.040178            185
seed7/lib/pgm.s7i            0.041286            238
seed7/lib/pic16.s7i          0.048992           1037
seed7/lib/pic32.s7i          0.081059           2060
seed7/lib/pic_util.s7i       0.039200            144
seed7/lib/pixelimage.s7i     0.042997            320
seed7/lib/pixmap_file.s7i    0.046046            459
seed7/lib/pixmapfont.s7i     0.040507            184
seed7/lib/pkcs1.s7i          0.059790            543
seed7/lib/png.s7i            0.064677           1064
seed7/lib/poll.s7i           0.044522            313
seed7/lib/ppm.s7i            0.041074            240
seed7/lib/process.s7i        0.049504            541
seed7/lib/progs.s7i          0.056945            789
seed7/lib/propertyfile.s7i   0.039087            155
seed7/lib/rational.s7i       0.053072            792
seed7/lib/ref_list.s7i       0.041052            252
seed7/lib/reference.s7i      0.038676            126
seed7/lib/reverse.s7i        0.036335             94
seed7/lib/rpm.s7i            0.144171           3487
seed7/lib/rpmext.s7i         0.043455            318
seed7/lib/scanfile.s7i       0.084821           1779
seed7/lib/scanjson.s7i       0.047865            413
seed7/lib/scanstri.s7i       0.081274           1814
seed7/lib/scantoml.s7i       0.072039           1603
seed7/lib/seed7_05.s7i       0.067258           1072
seed7/lib/set.s7i            0.037693             57
seed7/lib/shell.s7i          0.053395            615
seed7/lib/showtls.s7i        0.055429            678
seed7/lib/signature.s7i      0.039270            131
seed7/lib/smtp.s7i           0.041841            261
seed7/lib/sockbase.s7i       0.043532            217
seed7/lib/socket.s7i         0.043721            326
seed7/lib/sokoban1.s7i       0.054266           1519
seed7/lib/sql_base.s7i       0.062512           1000
seed7/lib/stars.s7i          0.133873           1705
seed7/lib/stdfont10.s7i      0.080080           3347
seed7/lib/stdfont12.s7i      0.093340           3928
seed7/lib/stdfont14.s7i      0.102736           4510
seed7/lib/stdfont16.s7i      0.114913           5092
seed7/lib/stdfont18.s7i      0.130979           5868
seed7/lib/stdfont20.s7i      0.148573           6449
seed7/lib/stdfont24.s7i      0.178800           7421
seed7/lib/stdfont8.s7i       0.071740           2960
seed7/lib/stdfont9.s7i       0.075489           3152
seed7/lib/stdio.s7i          0.039184            192
seed7/lib/strifile.s7i       0.044836            345
seed7/lib/string.s7i         0.055210            779
seed7/lib/stritext.s7i       0.044840            352
seed7/lib/struct.s7i         0.044527            266
seed7/lib/struct_elem.s7i    0.038457            129
seed7/lib/subfile.s7i        0.038901            174
seed7/lib/subrange.s7i       0.037209             78
seed7/lib/syntax.s7i         0.046661            294
seed7/lib/tar.s7i            0.083853           1880
seed7/lib/tar_cmds.s7i       0.059596            752
seed7/lib/tdes.s7i           0.039494            143
seed7/lib/tee.s7i            0.038002            143
seed7/lib/text.s7i           0.038067            135
seed7/lib/tga.s7i            0.053158            676
seed7/lib/tiff.s7i           0.120446           2771
seed7/lib/time.s7i           0.062372           1191
seed7/lib/tls.s7i            0.104350           2230
seed7/lib/unicode.s7i        0.051193            575
seed7/lib/unionfnd.s7i       0.037838            130
seed7/lib/upper.s7i          0.037580            142
seed7/lib/utf16.s7i          0.050691            540
seed7/lib/utf8.s7i           0.041116            234
seed7/lib/vecfont10.s7i      0.080263           1056
seed7/lib/vecfont18.s7i      0.087671           1119
seed7/lib/vector3d.s7i       0.042013            293
seed7/lib/vectorfont.s7i     0.041037            239
seed7/lib/wildcard.s7i       0.038596            140
seed7/lib/window.s7i         0.045785            455
seed7/lib/wrinum.s7i         0.041784            248
seed7/lib/x509cert.s7i       0.071620           1243
seed7/lib/xml_ent.s7i        0.037650             94
seed7/lib/xmldom.s7i         0.041874            303
seed7/lib/xz.s7i             0.045608            442
seed7/lib/zip.s7i            0.118712           2792
seed7/lib/zstd.s7i           0.069493           1263
============================ ================= ================

Statistical Summary — Mode C (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.088590        |
+-----------+-----------------+
| Minimum   | 0.033986        |
+-----------+-----------------+
| Maximum   | 2.496033        |
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
seed7/prg/addup.sd7          0.046340            190
seed7/prg/bas7.sd7           0.758885          11459
seed7/prg/bifurk.sd7         0.039392             73
seed7/prg/bigfiles.sd7       0.043517            129
seed7/prg/brainf7.sd7        0.039539             86
seed7/prg/calc7.sd7          0.043399            128
seed7/prg/carddemo.sd7       0.048539            190
seed7/prg/castle.sd7         0.214801           3148
seed7/prg/cat.sd7            0.038890             82
seed7/prg/cellauto.sd7       0.039413             85
seed7/prg/celsius.sd7        0.037417             42
seed7/prg/chk_all.sd7        0.081593            843
seed7/prg/chkarr.sd7         0.857720           8367
seed7/prg/chkbig.sd7         4.114234          29026
seed7/prg/chkbin.sd7         1.027267           6469
seed7/prg/chkbitdata.sd7     1.248920           6624
seed7/prg/chkbool.sd7        0.229338           3157
seed7/prg/chkbst.sd7         0.099723            722
seed7/prg/chkchr.sd7         0.478684           2809
seed7/prg/chkcmd.sd7         0.111210           1205
seed7/prg/chkdb.sd7          0.746527           7454
seed7/prg/chkdecl.sd7        0.093475            448
seed7/prg/chkenum.sd7        0.121017           1230
seed7/prg/chkerr.sd7         0.338976           4663
seed7/prg/chkexc.sd7         0.147894           2627
seed7/prg/chkfil.sd7         0.126709           1615
seed7/prg/chkflt.sd7         2.798344          20620
seed7/prg/chkhent.sd7        0.039813             54
seed7/prg/chkhsh.sd7         0.494333           4548
seed7/prg/chkidx.sd7         3.160179          19567
seed7/prg/chkint.sd7         5.536954          38129
seed7/prg/chkjson.sd7        0.185116           1764
seed7/prg/chkovf.sd7         1.190923           8216
seed7/prg/chkprc.sd7         0.688980          10111
seed7/prg/chkscan.sd7        0.089044            714
seed7/prg/chkset.sd7         1.721742          11974
seed7/prg/chkstr.sd7         3.391772          26952
seed7/prg/chktime.sd7        0.245947           2025
seed7/prg/chktoml.sd7        0.194536           1656
seed7/prg/clock.sd7          0.036317             47
seed7/prg/clock2.sd7         0.036157             43
seed7/prg/clock3.sd7         0.041640             95
seed7/prg/cmpfil.sd7         0.038497             84
seed7/prg/comanche.sd7       0.046228            180
seed7/prg/confval.sd7        0.050141            175
seed7/prg/db7.sd7            0.062062            417
seed7/prg/diff7.sd7          0.051003            263
seed7/prg/dirtst.sd7         0.036780             42
seed7/prg/dirx.sd7           0.043694            152
seed7/prg/dnafight.sd7       0.116318           1381
seed7/prg/dragon.sd7         0.037373             73
seed7/prg/echo.sd7           0.036003             39
seed7/prg/eliza.sd7          0.051019            302
seed7/prg/err.sd7            0.043875             96
seed7/prg/fannkuch.sd7       0.042901            131
seed7/prg/fib.sd7            0.036327             47
seed7/prg/find7.sd7          0.042010            133
seed7/prg/findchar.sd7       0.047237            149
seed7/prg/fractree.sd7       0.042626             55
seed7/prg/ftp7.sd7           0.053331            296
seed7/prg/ftpserv.sd7        0.038752             74
seed7/prg/gcd.sd7            0.040571            109
seed7/prg/gkbd.sd7           0.061315            358
seed7/prg/gtksvtst.sd7       0.039292             94
seed7/prg/hal.sd7            0.047538            250
seed7/prg/hamu.sd7           0.066304            573
seed7/prg/hanoi.sd7          0.037027             55
seed7/prg/hd.sd7             0.039186             79
seed7/prg/hello.sd7          0.035893             32
seed7/prg/hilbert.sd7        0.041206            108
seed7/prg/ide7.sd7           0.047524            196
seed7/prg/kbd.sd7            0.038309             49
seed7/prg/klondike.sd7       0.086112            883
seed7/prg/lander.sd7         0.129955           1551
seed7/prg/lst80bas.sd7       0.055141            344
seed7/prg/lst99bas.sd7       0.058997            401
seed7/prg/lstgwbas.sd7       0.071995            577
seed7/prg/mahjong.sd7        0.145280           1943
seed7/prg/make7.sd7          0.041830            121
seed7/prg/mandelbr.sd7       0.048170            237
seed7/prg/mind.sd7           0.058889            443
seed7/prg/mirror.sd7         0.043537            131
seed7/prg/ms.sd7             0.068958            641
seed7/prg/nicoma.sd7         0.041884            135
seed7/prg/pac.sd7            0.070866            726
seed7/prg/pairs.sd7          0.140105           2025
seed7/prg/panic.sd7          0.195755           2634
seed7/prg/percolation.sd7    0.055525            330
seed7/prg/planets.sd7        0.136212           1486
seed7/prg/portfwd7.sd7       0.043652            139
seed7/prg/prime.sd7          0.037526             74
seed7/prg/printpi1.sd7       0.037698             56
seed7/prg/printpi2.sd7       0.037301             54
seed7/prg/printpi3.sd7       0.037449             60
seed7/prg/pv7.sd7            0.057814            337
seed7/prg/queen.sd7          0.042121            149
seed7/prg/rand.sd7           0.040568            121
seed7/prg/raytrace.sd7       0.066954            538
seed7/prg/rever.sd7          0.081001            816
seed7/prg/roman.sd7          0.036325             38
seed7/prg/s7c.sd7            0.615748           9060
seed7/prg/s7check.sd7        0.038177             68
seed7/prg/savehd7.sd7        0.109057           1110
seed7/prg/self.sd7           0.036586             49
seed7/prg/shisen.sd7         0.117880           1423
seed7/prg/sl.sd7             0.095041           1029
seed7/prg/snake.sd7          0.064859            615
seed7/prg/sokoban.sd7        0.081556            891
seed7/prg/spigotpi.sd7       0.037468             64
seed7/prg/sql7.sd7           0.051605            278
seed7/prg/startrek.sd7       0.092839            979
seed7/prg/sudoku7.sd7        0.196682           2657
seed7/prg/sydir7.sd7         0.059600            384
seed7/prg/syntaxhl.sd7       0.048460            177
seed7/prg/tak.sd7            0.037425             59
seed7/prg/tar7.sd7           0.042055            121
seed7/prg/tch.sd7            0.036896             55
seed7/prg/testfont.sd7       0.041145             95
seed7/prg/tet.sd7            0.061727            479
seed7/prg/tetg.sd7           0.061796            501
seed7/prg/toutf8.sd7         0.050789            240
seed7/prg/tst_cli.sd7        0.036144             40
seed7/prg/tst_srv.sd7        0.036799             47
seed7/prg/wator.sd7          0.078735            651
seed7/prg/which.sd7          0.037989             65
seed7/prg/wiz.sd7            0.209747           2833
seed7/prg/wordcnt.sd7        0.038398             54
seed7/prg/wrinum.sd7         0.039209             43
seed7/prg/wumpus.sd7         0.054071            372
seed7/lib/aes.s7i            0.195561           1144
seed7/lib/aes_gcm.s7i        0.062301            392
seed7/lib/ar.s7i             0.123157           1532
seed7/lib/arc4.s7i           0.042686            144
seed7/lib/archive.s7i        0.042806            143
seed7/lib/archive_base.s7i   0.042701            135
seed7/lib/array.s7i          0.074586            610
seed7/lib/asn1.s7i           0.063570            544
seed7/lib/asn1oid.s7i        0.057710            157
seed7/lib/basearray.s7i      0.066424            450
seed7/lib/bigfile.s7i        0.042483            136
seed7/lib/bigint.s7i         0.078641            824
seed7/lib/bigrat.s7i         0.080119            784
seed7/lib/bin16.s7i          0.068554            592
seed7/lib/bin32.s7i          0.061917            490
seed7/lib/bin64.s7i          0.063835            539
seed7/lib/bitdata.s7i        0.121342           1330
seed7/lib/bitmapfont.s7i     0.046849            215
seed7/lib/bitset.s7i         0.063025            593
seed7/lib/bitsetof.s7i       0.060935            431
seed7/lib/blowfish.s7i       0.076722            383
seed7/lib/bmp.s7i            0.099080            924
seed7/lib/boolean.s7i        0.055243            403
seed7/lib/browser.s7i        0.052980            280
seed7/lib/bstring.s7i        0.049358            227
seed7/lib/bytedata.s7i       0.066928            482
seed7/lib/bzip2.s7i          0.089468            887
seed7/lib/cards.s7i          0.108827           1342
seed7/lib/category.s7i       0.050105            209
seed7/lib/cc_conf.s7i        0.120532           1314
seed7/lib/ccittfax.s7i       0.101381           1022
seed7/lib/cgi.s7i            0.042180            109
seed7/lib/cgidialog.s7i      0.098231           1118
seed7/lib/char.s7i           0.051794            356
seed7/lib/charsets.s7i       0.122067           2024
seed7/lib/chartype.s7i       0.047935            121
seed7/lib/cipher.s7i         0.042575            146
seed7/lib/cli_cmds.s7i       0.114421           1360
seed7/lib/clib_file.s7i      0.052799            301
seed7/lib/color.s7i          0.046434            185
seed7/lib/complex.s7i        0.058505            464
seed7/lib/compress.s7i       0.043489            150
seed7/lib/console.s7i        0.048165            188
seed7/lib/cpio.s7i           0.142536           1708
seed7/lib/crc32.s7i          0.054968            193
seed7/lib/cronos16.s7i       0.193676           1173
seed7/lib/cronos27.s7i       0.253084           1464
seed7/lib/csv.s7i            0.047399            201
seed7/lib/db_prop.s7i        0.101690            991
seed7/lib/deflate.s7i        0.087900            740
seed7/lib/des.s7i            0.080182            444
seed7/lib/dialog.s7i         0.057342            311
seed7/lib/dir.s7i            0.043479            163
seed7/lib/draw.s7i           0.085536            854
seed7/lib/duration.s7i       0.097572           1038
seed7/lib/echo.s7i           0.043027            132
seed7/lib/editline.s7i       0.059490            398
seed7/lib/elf.s7i            0.154635           1560
seed7/lib/elliptic.s7i       0.075267            649
seed7/lib/enable_io.s7i      0.051381            312
seed7/lib/encoding.s7i       0.095882            931
seed7/lib/enumeration.s7i    0.048889            236
seed7/lib/environment.s7i    0.046128            175
seed7/lib/exif.s7i           0.046451            152
seed7/lib/external_file.s7i  0.052909            340
seed7/lib/field.s7i          0.052075            268
seed7/lib/file.s7i           0.054146            372
seed7/lib/filebits.s7i       0.038184             46
seed7/lib/filesys.s7i        0.064750            601
seed7/lib/fileutil.s7i       0.043615            144
seed7/lib/fixarray.s7i       0.053668            307
seed7/lib/float.s7i          0.083119            757
seed7/lib/font.s7i           0.046220            196
seed7/lib/font8x8.s7i        0.067405            998
seed7/lib/forloop.s7i        0.065892            449
seed7/lib/ftp.s7i            0.087179            969
seed7/lib/ftpserv.s7i        0.079323            631
seed7/lib/getf.s7i           0.053113            115
seed7/lib/gethttp.s7i        0.046899             41
seed7/lib/gethttps.s7i       0.041465             41
seed7/lib/gif.s7i            0.074600            561
seed7/lib/graph.s7i          0.063803            415
seed7/lib/graph_file.s7i     0.058985            399
seed7/lib/gtkserver.s7i      0.041358            161
seed7/lib/gzip.s7i           0.075833            573
seed7/lib/hash.s7i           0.065140            421
seed7/lib/hashsetof.s7i      0.066879            499
seed7/lib/hmac.s7i           0.045266            152
seed7/lib/html.s7i           0.039519             83
seed7/lib/html_ent.s7i       0.064881            476
seed7/lib/htmldom.s7i        0.053450            286
seed7/lib/http_request.s7i   0.077598            696
seed7/lib/http_srv_resp.s7i  0.058567            380
seed7/lib/https_request.s7i  0.048825            211
seed7/lib/httpserv.s7i       0.058368            345
seed7/lib/huffman.s7i        0.075382            644
seed7/lib/ico.s7i            0.058891            221
seed7/lib/idxarray.s7i       0.052069            232
seed7/lib/image.s7i          0.041958            156
seed7/lib/imagefile.s7i      0.045184            171
seed7/lib/inflate.s7i        0.064272            411
seed7/lib/inifile.s7i        0.043525            129
seed7/lib/integer.s7i        0.068765            663
seed7/lib/iobuffer.s7i       0.051485            289
seed7/lib/jpeg.s7i           0.153822           1761
seed7/lib/json.s7i           0.080672            891
seed7/lib/json_serde.s7i     0.079814            783
seed7/lib/keybd.s7i          0.079714            639
seed7/lib/keydescr.s7i       0.050522            192
seed7/lib/leb128.s7i         0.046092            218
seed7/lib/line.s7i           0.042726            164
seed7/lib/listener.s7i       0.047857            247
seed7/lib/logfile.s7i        0.037935             73
seed7/lib/lower.s7i          0.041274            142
seed7/lib/lzma.s7i           0.098342            934
seed7/lib/lzw.s7i            0.089035            861
seed7/lib/magic.s7i          0.064783            403
seed7/lib/mahjng32.s7i       0.093281           1500
seed7/lib/make.s7i           0.070056            544
seed7/lib/makedata.s7i       0.123006           1428
seed7/lib/math.s7i           0.045392            201
seed7/lib/mixarith.s7i       0.047547            249
seed7/lib/modern27.s7i       0.170420           1099
seed7/lib/more.s7i           0.043325            130
seed7/lib/msgdigest.s7i      0.135405           1222
seed7/lib/multiscr.s7i       0.038788             68
seed7/lib/null_file.s7i      0.050355            345
seed7/lib/osfiles.s7i        0.096699           1085
seed7/lib/pbm.s7i            0.047975            230
seed7/lib/pcx.s7i            0.078046            638
seed7/lib/pem.s7i            0.046356            185
seed7/lib/pgm.s7i            0.048944            238
seed7/lib/pic16.s7i          0.069586           1037
seed7/lib/pic32.s7i          0.122559           2060
seed7/lib/pic_util.s7i       0.044829            144
seed7/lib/pixelimage.s7i     0.053440            320
seed7/lib/pixmap_file.s7i    0.065570            459
seed7/lib/pixmapfont.s7i     0.048137            184
seed7/lib/pkcs1.s7i          0.078468            543
seed7/lib/png.s7i            0.106595           1064
seed7/lib/poll.s7i           0.051854            313
seed7/lib/ppm.s7i            0.049102            240
seed7/lib/process.s7i        0.063729            541
seed7/lib/progs.s7i          0.080543            789
seed7/lib/propertyfile.s7i   0.044850            155
seed7/lib/rational.s7i       0.079689            792
seed7/lib/ref_list.s7i       0.049383            252
seed7/lib/reference.s7i      0.042511            126
seed7/lib/reverse.s7i        0.039642             94
seed7/lib/rpm.s7i            0.285525           3487
seed7/lib/rpmext.s7i         0.053163            318
seed7/lib/scanfile.s7i       0.132552           1779
seed7/lib/scanjson.s7i       0.060522            413
seed7/lib/scanstri.s7i       0.134436           1814
seed7/lib/scantoml.s7i       0.129987           1603
seed7/lib/seed7_05.s7i       0.111653           1072
seed7/lib/set.s7i            0.038757             57
seed7/lib/shell.s7i          0.069626            615
seed7/lib/showtls.s7i        0.085531            678
seed7/lib/signature.s7i      0.043732            131
seed7/lib/smtp.s7i           0.049627            261
seed7/lib/sockbase.s7i       0.050549            217
seed7/lib/socket.s7i         0.052563            326
seed7/lib/sokoban1.s7i       0.081149           1519
seed7/lib/sql_base.s7i       0.094541           1000
seed7/lib/stars.s7i          0.237244           1705
seed7/lib/stdfont10.s7i      0.146785           3347
seed7/lib/stdfont12.s7i      0.166860           3928
seed7/lib/stdfont14.s7i      0.187840           4510
seed7/lib/stdfont16.s7i      0.212033           5092
seed7/lib/stdfont18.s7i      0.243762           5868
seed7/lib/stdfont20.s7i      0.270123           6449
seed7/lib/stdfont24.s7i      0.330330           7421
seed7/lib/stdfont8.s7i       0.127143           2960
seed7/lib/stdfont9.s7i       0.133914           3152
seed7/lib/stdio.s7i          0.044623            192
seed7/lib/strifile.s7i       0.054980            345
seed7/lib/string.s7i         0.075415            779
seed7/lib/stritext.s7i       0.054569            352
seed7/lib/struct.s7i         0.054063            266
seed7/lib/struct_elem.s7i    0.041491            129
seed7/lib/subfile.s7i        0.043724            174
seed7/lib/subrange.s7i       0.040125             78
seed7/lib/syntax.s7i         0.060599            294
seed7/lib/tar.s7i            0.146005           1880
seed7/lib/tar_cmds.s7i       0.085048            752
seed7/lib/tdes.s7i           0.043823            143
seed7/lib/tee.s7i            0.042326            143
seed7/lib/text.s7i           0.042927            135
seed7/lib/tga.s7i            0.080610            676
seed7/lib/tiff.s7i           0.245623           2771
seed7/lib/time.s7i           0.102027           1191
seed7/lib/tls.s7i            0.193588           2230
seed7/lib/unicode.s7i        0.073958            575
seed7/lib/unionfnd.s7i       0.044623            130
seed7/lib/upper.s7i          0.041954            142
seed7/lib/utf16.s7i          0.065809            540
seed7/lib/utf8.s7i           0.048400            234
seed7/lib/vecfont10.s7i      0.159930           1056
seed7/lib/vecfont18.s7i      0.180332           1119
seed7/lib/vector3d.s7i       0.049991            293
seed7/lib/vectorfont.s7i     0.048568            239
seed7/lib/wildcard.s7i       0.042993            140
seed7/lib/window.s7i         0.059905            455
seed7/lib/wrinum.s7i         0.049815            248
seed7/lib/x509cert.s7i       0.116278           1243
seed7/lib/xml_ent.s7i        0.040189             94
seed7/lib/xmldom.s7i         0.051537            303
seed7/lib/xz.s7i             0.061031            442
seed7/lib/zip.s7i            0.232629           2792
seed7/lib/zstd.s7i           0.117666           1263
============================ ================= ================

Statistical Summary — Mode D (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.158256        |
+-----------+-----------------+
| Minimum   | 0.035893        |
+-----------+-----------------+
| Maximum   | 5.536954        |
+-----------+-----------------+

Cross-Mode Comparison
=====================

+------+-----------------+-----------------+-----------------+
| Mode | Average (s)     | Minimum (s)     | Maximum (s)     |
+======+=================+=================+=================+
| A    | 0.033717        | 0.032423        | 0.039414        |
+------+-----------------+-----------------+-----------------+
| B    | 0.038971        | 0.035641        | 0.045484        |
+------+-----------------+-----------------+-----------------+
| C    | 0.088590        | 0.033986        | 2.496033        |
+------+-----------------+-----------------+-----------------+
| D    | 0.158256        | 0.035893        | 5.536954        |
+------+-----------------+-----------------+-----------------+

Phase Timing Summary
====================

Wall-clock times include warm-up work and GC performed by the warm-up helper.

+----------+--------------+--------------+--------------+
| Phase    | Warm-up      | Timed pass   | Mode total   |
+==========+==============+==============+==============+
| Mode A   | 00:00:12.362 | 00:00:57.433 | 00:01:09.795 |
+----------+--------------+--------------+--------------+
| Mode B   | 00:00:14.988 | 00:01:06.450 | 00:01:21.439 |
+----------+--------------+--------------+--------------+
| Mode C   | 00:00:35.868 | 00:02:31.767 | 00:03:07.635 |
+----------+--------------+--------------+--------------+
| Mode D   | 00:01:00.478 | 00:04:30.467 | 00:05:30.945 |
+----------+--------------+--------------+--------------+
| Total    |              |              | 00:11:09.823 |
+----------+--------------+--------------+--------------+


.. ---------------------------------------------------------------------------
