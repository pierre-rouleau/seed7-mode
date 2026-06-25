=======================================================
GC-Controlled Benchmark Report: Seed7 Mode — Four Modes
=======================================================

:Running with: seed7-mode 2026-06-24T21:10:39+0000 W26-3
:Seed7 dir   : /Users/roup/my/dvo/seed7-repos/seed7/
:Started at: 17:13:00 local time
:Generated on: 2026-06-24 21:24:16 UTC
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
seed7/prg/addup.sd7          0.034890            190
seed7/prg/bas7.sd7           0.036055          11459
seed7/prg/bifurk.sd7         0.034495             73
seed7/prg/bigfiles.sd7       0.034931            129
seed7/prg/brainf7.sd7        0.035688             86
seed7/prg/calc7.sd7          0.035036            128
seed7/prg/carddemo.sd7       0.034925            190
seed7/prg/castle.sd7         0.034387           3148
seed7/prg/cat.sd7            0.034881             82
seed7/prg/cellauto.sd7       0.034280             85
seed7/prg/celsius.sd7        0.033614             42
seed7/prg/chk_all.sd7        0.032607            843
seed7/prg/chkarr.sd7         0.033538           8367
seed7/prg/chkbig.sd7         0.037108          29026
seed7/prg/chkbin.sd7         0.033040           6469
seed7/prg/chkbitdata.sd7     0.035844           6624
seed7/prg/chkbool.sd7        0.033206           3157
seed7/prg/chkbst.sd7         0.033658            722
seed7/prg/chkchr.sd7         0.034632           2809
seed7/prg/chkcmd.sd7         0.034621           1205
seed7/prg/chkdb.sd7          0.035905           7454
seed7/prg/chkdecl.sd7        0.036326            448
seed7/prg/chkenum.sd7        0.034826           1230
seed7/prg/chkerr.sd7         0.033616           4663
seed7/prg/chkexc.sd7         0.033870           2627
seed7/prg/chkfil.sd7         0.033102           1615
seed7/prg/chkflt.sd7         0.036092          20620
seed7/prg/chkhent.sd7        0.033062             54
seed7/prg/chkhsh.sd7         0.033131           4548
seed7/prg/chkidx.sd7         0.034879          19567
seed7/prg/chkint.sd7         0.039045          38129
seed7/prg/chkjson.sd7        0.032553           1764
seed7/prg/chkovf.sd7         0.033150           8216
seed7/prg/chkprc.sd7         0.033084          10111
seed7/prg/chkscan.sd7        0.032950            714
seed7/prg/chkset.sd7         0.033663          11974
seed7/prg/chkstr.sd7         0.037370          26952
seed7/prg/chktime.sd7        0.033789           2025
seed7/prg/chktoml.sd7        0.037108           1656
seed7/prg/clock.sd7          0.034956             47
seed7/prg/clock2.sd7         0.035450             43
seed7/prg/clock3.sd7         0.033696             95
seed7/prg/cmpfil.sd7         0.036496             84
seed7/prg/comanche.sd7       0.037349            180
seed7/prg/confval.sd7        0.035560            175
seed7/prg/db7.sd7            0.036327            417
seed7/prg/diff7.sd7          0.035071            263
seed7/prg/dirtst.sd7         0.036487             42
seed7/prg/dirx.sd7           0.036913            152
seed7/prg/dnafight.sd7       0.034820           1381
seed7/prg/dragon.sd7         0.035648             73
seed7/prg/echo.sd7           0.035507             39
seed7/prg/eliza.sd7          0.033615            302
seed7/prg/err.sd7            0.033122             96
seed7/prg/fannkuch.sd7       0.033106            131
seed7/prg/fib.sd7            0.032831             47
seed7/prg/find7.sd7          0.032915            133
seed7/prg/findchar.sd7       0.032885            149
seed7/prg/fractree.sd7       0.033027             55
seed7/prg/ftp7.sd7           0.033394            296
seed7/prg/ftpserv.sd7        0.032969             74
seed7/prg/gcd.sd7            0.032824            109
seed7/prg/gkbd.sd7           0.033094            358
seed7/prg/gtksvtst.sd7       0.032892             94
seed7/prg/hal.sd7            0.034567            250
seed7/prg/hamu.sd7           0.033584            573
seed7/prg/hanoi.sd7          0.033191             55
seed7/prg/hd.sd7             0.033205             79
seed7/prg/hello.sd7          0.032897             32
seed7/prg/hilbert.sd7        0.033051            108
seed7/prg/ide7.sd7           0.032909            196
seed7/prg/kbd.sd7            0.032870             49
seed7/prg/klondike.sd7       0.032828            883
seed7/prg/lander.sd7         0.032476           1551
seed7/prg/lst80bas.sd7       0.032407            344
seed7/prg/lst99bas.sd7       0.032318            401
seed7/prg/lstgwbas.sd7       0.032648            577
seed7/prg/mahjong.sd7        0.033897           1943
seed7/prg/make7.sd7          0.033091            121
seed7/prg/mandelbr.sd7       0.032703            237
seed7/prg/mind.sd7           0.032489            443
seed7/prg/mirror.sd7         0.033034            131
seed7/prg/ms.sd7             0.032507            641
seed7/prg/nicoma.sd7         0.032723            135
seed7/prg/pac.sd7            0.033027            726
seed7/prg/pairs.sd7          0.032365           2025
seed7/prg/panic.sd7          0.032700           2634
seed7/prg/percolation.sd7    0.033042            330
seed7/prg/planets.sd7        0.033222           1486
seed7/prg/portfwd7.sd7       0.033800            139
seed7/prg/prime.sd7          0.035489             74
seed7/prg/printpi1.sd7       0.033400             56
seed7/prg/printpi2.sd7       0.033003             54
seed7/prg/printpi3.sd7       0.032940             60
seed7/prg/pv7.sd7            0.033138            337
seed7/prg/queen.sd7          0.033531            149
seed7/prg/rand.sd7           0.033940            121
seed7/prg/raytrace.sd7       0.038909            538
seed7/prg/rever.sd7          0.035547            816
seed7/prg/roman.sd7          0.033238             38
seed7/prg/s7c.sd7            0.033525           9060
seed7/prg/s7check.sd7        0.033080             68
seed7/prg/savehd7.sd7        0.032909           1110
seed7/prg/self.sd7           0.032816             49
seed7/prg/shisen.sd7         0.032615           1423
seed7/prg/sl.sd7             0.032608           1029
seed7/prg/snake.sd7          0.032677            615
seed7/prg/sokoban.sd7        0.032380            891
seed7/prg/spigotpi.sd7       0.032679             64
seed7/prg/sql7.sd7           0.034048            278
seed7/prg/startrek.sd7       0.033448            979
seed7/prg/sudoku7.sd7        0.033543           2657
seed7/prg/sydir7.sd7         0.033180            384
seed7/prg/syntaxhl.sd7       0.032936            177
seed7/prg/tak.sd7            0.033044             59
seed7/prg/tar7.sd7           0.032973            121
seed7/prg/tch.sd7            0.032973             55
seed7/prg/testfont.sd7       0.033011             95
seed7/prg/tet.sd7            0.033226            479
seed7/prg/tetg.sd7           0.032750            501
seed7/prg/toutf8.sd7         0.033239            240
seed7/prg/tst_cli.sd7        0.033064             40
seed7/prg/tst_srv.sd7        0.033148             47
seed7/prg/wator.sd7          0.033017            651
seed7/prg/which.sd7          0.033066             65
seed7/prg/wiz.sd7            0.033604           2833
seed7/prg/wordcnt.sd7        0.032838             54
seed7/prg/wrinum.sd7         0.033701             43
seed7/prg/wumpus.sd7         0.033069            372
seed7/lib/aes.s7i            0.033238           1144
seed7/lib/aes_gcm.s7i        0.033105            392
seed7/lib/ar.s7i             0.032929           1532
seed7/lib/arc4.s7i           0.033223            144
seed7/lib/archive.s7i        0.032948            143
seed7/lib/archive_base.s7i   0.034125            135
seed7/lib/array.s7i          0.038799            610
seed7/lib/asn1.s7i           0.035421            544
seed7/lib/asn1oid.s7i        0.032634            157
seed7/lib/basearray.s7i      0.033827            450
seed7/lib/bigfile.s7i        0.033402            136
seed7/lib/bigint.s7i         0.032437            824
seed7/lib/bigrat.s7i         0.034422            784
seed7/lib/bin16.s7i          0.032708            592
seed7/lib/bin32.s7i          0.032410            490
seed7/lib/bin64.s7i          0.033128            539
seed7/lib/bitdata.s7i        0.032927           1330
seed7/lib/bitmapfont.s7i     0.032239            215
seed7/lib/bitset.s7i         0.032172            593
seed7/lib/bitsetof.s7i       0.032843            431
seed7/lib/blowfish.s7i       0.033238            383
seed7/lib/bmp.s7i            0.033372            924
seed7/lib/boolean.s7i        0.034359            403
seed7/lib/browser.s7i        0.036048            280
seed7/lib/bstring.s7i        0.033067            227
seed7/lib/bytedata.s7i       0.033096            482
seed7/lib/bzip2.s7i          0.033718            887
seed7/lib/cards.s7i          0.033900           1342
seed7/lib/category.s7i       0.033148            209
seed7/lib/cc_conf.s7i        0.033216           1314
seed7/lib/ccittfax.s7i       0.033247           1022
seed7/lib/cgi.s7i            0.032978            109
seed7/lib/cgidialog.s7i      0.033375           1118
seed7/lib/char.s7i           0.033342            356
seed7/lib/charsets.s7i       0.033039           2024
seed7/lib/chartype.s7i       0.032782            121
seed7/lib/cipher.s7i         0.033144            146
seed7/lib/cli_cmds.s7i       0.033344           1360
seed7/lib/clib_file.s7i      0.032715            301
seed7/lib/color.s7i          0.032718            185
seed7/lib/complex.s7i        0.032719            464
seed7/lib/compress.s7i       0.032664            150
seed7/lib/console.s7i        0.032536            188
seed7/lib/cpio.s7i           0.033407           1708
seed7/lib/crc32.s7i          0.033674            193
seed7/lib/cronos16.s7i       0.033321           1173
seed7/lib/cronos27.s7i       0.034589           1464
seed7/lib/csv.s7i            0.033098            201
seed7/lib/db_prop.s7i        0.034290            991
seed7/lib/deflate.s7i        0.036723            740
seed7/lib/des.s7i            0.036012            444
seed7/lib/dialog.s7i         0.033037            311
seed7/lib/dir.s7i            0.033173            163
seed7/lib/draw.s7i           0.034510            854
seed7/lib/duration.s7i       0.033270           1038
seed7/lib/echo.s7i           0.033140            132
seed7/lib/editline.s7i       0.033761            398
seed7/lib/elf.s7i            0.033192           1560
seed7/lib/elliptic.s7i       0.033528            649
seed7/lib/enable_io.s7i      0.033159            312
seed7/lib/encoding.s7i       0.033066            931
seed7/lib/enumeration.s7i    0.033059            236
seed7/lib/environment.s7i    0.033272            175
seed7/lib/exif.s7i           0.033084            152
seed7/lib/external_file.s7i  0.033146            340
seed7/lib/field.s7i          0.032987            268
seed7/lib/file.s7i           0.033128            372
seed7/lib/filebits.s7i       0.035123             46
seed7/lib/filesys.s7i        0.034971            601
seed7/lib/fileutil.s7i       0.033097            144
seed7/lib/fixarray.s7i       0.032710            307
seed7/lib/float.s7i          0.032616            757
seed7/lib/font.s7i           0.032762            196
seed7/lib/font8x8.s7i        0.032417            998
seed7/lib/forloop.s7i        0.033812            449
seed7/lib/ftp.s7i            0.036452            969
seed7/lib/ftpserv.s7i        0.033187            631
seed7/lib/getf.s7i           0.033152            115
seed7/lib/gethttp.s7i        0.033236             41
seed7/lib/gethttps.s7i       0.033400             41
seed7/lib/gif.s7i            0.033325            561
seed7/lib/graph.s7i          0.033333            415
seed7/lib/graph_file.s7i     0.032945            399
seed7/lib/gtkserver.s7i      0.032957            161
seed7/lib/gzip.s7i           0.033162            573
seed7/lib/hash.s7i           0.032956            421
seed7/lib/hashsetof.s7i      0.033300            499
seed7/lib/hmac.s7i           0.033036            152
seed7/lib/html.s7i           0.032914             83
seed7/lib/html_ent.s7i       0.033231            476
seed7/lib/htmldom.s7i        0.033070            286
seed7/lib/http_request.s7i   0.033335            696
seed7/lib/http_srv_resp.s7i  0.033506            380
seed7/lib/https_request.s7i  0.033271            211
seed7/lib/httpserv.s7i       0.033644            345
seed7/lib/huffman.s7i        0.033301            644
seed7/lib/ico.s7i            0.033262            221
seed7/lib/idxarray.s7i       0.033000            232
seed7/lib/image.s7i          0.032996            156
seed7/lib/imagefile.s7i      0.032957            171
seed7/lib/inflate.s7i        0.032534            411
seed7/lib/inifile.s7i        0.032622            129
seed7/lib/integer.s7i        0.032542            663
seed7/lib/iobuffer.s7i       0.032392            289
seed7/lib/jpeg.s7i           0.033127           1761
seed7/lib/json.s7i           0.033762            891
seed7/lib/json_serde.s7i     0.033363            783
seed7/lib/keybd.s7i          0.033288            639
seed7/lib/keydescr.s7i       0.033117            192
seed7/lib/leb128.s7i         0.033115            218
seed7/lib/line.s7i           0.033535            164
seed7/lib/listener.s7i       0.033854            247
seed7/lib/logfile.s7i        0.034392             73
seed7/lib/lower.s7i          0.034992            142
seed7/lib/lzma.s7i           0.033331            934
seed7/lib/lzw.s7i            0.033274            861
seed7/lib/magic.s7i          0.035015            403
seed7/lib/mahjng32.s7i       0.039522           1500
seed7/lib/make.s7i           0.034490            544
seed7/lib/makedata.s7i       0.033223           1428
seed7/lib/math.s7i           0.033108            201
seed7/lib/mixarith.s7i       0.033848            249
seed7/lib/modern27.s7i       0.034181           1099
seed7/lib/more.s7i           0.033179            130
seed7/lib/msgdigest.s7i      0.035829           1222
seed7/lib/multiscr.s7i       0.040069             68
seed7/lib/null_file.s7i      0.035433            345
seed7/lib/osfiles.s7i        0.033054           1085
seed7/lib/pbm.s7i            0.033092            230
seed7/lib/pcx.s7i            0.033218            638
seed7/lib/pem.s7i            0.032471            185
seed7/lib/pgm.s7i            0.036792            238
seed7/lib/pic16.s7i          0.035491           1037
seed7/lib/pic32.s7i          0.033436           2060
seed7/lib/pic_util.s7i       0.033253            144
seed7/lib/pixelimage.s7i     0.033830            320
seed7/lib/pixmap_file.s7i    0.033058            459
seed7/lib/pixmapfont.s7i     0.033367            184
seed7/lib/pkcs1.s7i          0.033080            543
seed7/lib/png.s7i            0.033497           1064
seed7/lib/poll.s7i           0.033002            313
seed7/lib/ppm.s7i            0.033078            240
seed7/lib/process.s7i        0.033193            541
seed7/lib/progs.s7i          0.033224            789
seed7/lib/propertyfile.s7i   0.033242            155
seed7/lib/rational.s7i       0.033464            792
seed7/lib/ref_list.s7i       0.032982            252
seed7/lib/reference.s7i      0.033945            126
seed7/lib/reverse.s7i        0.033770             94
seed7/lib/rpm.s7i            0.036479           3487
seed7/lib/rpmext.s7i         0.035835            318
seed7/lib/scanfile.s7i       0.033620           1779
seed7/lib/scanjson.s7i       0.033448            413
seed7/lib/scanstri.s7i       0.033926           1814
seed7/lib/scantoml.s7i       0.039024           1603
seed7/lib/seed7_05.s7i       0.035372           1072
seed7/lib/set.s7i            0.033191             57
seed7/lib/shell.s7i          0.033025            615
seed7/lib/showtls.s7i        0.032835            678
seed7/lib/signature.s7i      0.033236            131
seed7/lib/smtp.s7i           0.032549            261
seed7/lib/sockbase.s7i       0.032932            217
seed7/lib/socket.s7i         0.032414            326
seed7/lib/sokoban1.s7i       0.032618           1519
seed7/lib/sql_base.s7i       0.034086           1000
seed7/lib/stars.s7i          0.033648           1705
seed7/lib/stdfont10.s7i      0.033338           3347
seed7/lib/stdfont12.s7i      0.033264           3928
seed7/lib/stdfont14.s7i      0.033295           4510
seed7/lib/stdfont16.s7i      0.033604           5092
seed7/lib/stdfont18.s7i      0.033557           5868
seed7/lib/stdfont20.s7i      0.033392           6449
seed7/lib/stdfont24.s7i      0.033127           7421
seed7/lib/stdfont8.s7i       0.034155           2960
seed7/lib/stdfont9.s7i       0.033577           3152
seed7/lib/stdio.s7i          0.033210            192
seed7/lib/strifile.s7i       0.034102            345
seed7/lib/string.s7i         0.033164            779
seed7/lib/stritext.s7i       0.033386            352
seed7/lib/struct.s7i         0.033259            266
seed7/lib/struct_elem.s7i    0.033248            129
seed7/lib/subfile.s7i        0.033170            174
seed7/lib/subrange.s7i       0.033269             78
seed7/lib/syntax.s7i         0.033843            294
seed7/lib/tar.s7i            0.032921           1880
seed7/lib/tar_cmds.s7i       0.033222            752
seed7/lib/tdes.s7i           0.033075            143
seed7/lib/tee.s7i            0.033093            143
seed7/lib/text.s7i           0.033149            135
seed7/lib/tga.s7i            0.033109            676
seed7/lib/tiff.s7i           0.033399           2771
seed7/lib/time.s7i           0.033935           1191
seed7/lib/tls.s7i            0.038834           2230
seed7/lib/unicode.s7i        0.034409            575
seed7/lib/unionfnd.s7i       0.032529            130
seed7/lib/upper.s7i          0.032140            142
seed7/lib/utf16.s7i          0.034826            540
seed7/lib/utf8.s7i           0.033238            234
seed7/lib/vecfont10.s7i      0.033277           1056
seed7/lib/vecfont18.s7i      0.033608           1119
seed7/lib/vector3d.s7i       0.033377            293
seed7/lib/vectorfont.s7i     0.033046            239
seed7/lib/wildcard.s7i       0.033051            140
seed7/lib/window.s7i         0.033131            455
seed7/lib/wrinum.s7i         0.033335            248
seed7/lib/x509cert.s7i       0.033281           1243
seed7/lib/xml_ent.s7i        0.032928             94
seed7/lib/xmldom.s7i         0.033099            303
seed7/lib/xz.s7i             0.033528            442
seed7/lib/zip.s7i            0.033343           2792
seed7/lib/zstd.s7i           0.033031           1263
============================ ================= ================

Statistical Summary — Mode A (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.033692        |
+-----------+-----------------+
| Minimum   | 0.032140        |
+-----------+-----------------+
| Maximum   | 0.040069        |
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
seed7/prg/addup.sd7          0.038483            190
seed7/prg/bas7.sd7           0.040703          11459
seed7/prg/bifurk.sd7         0.040563             73
seed7/prg/bigfiles.sd7       0.043376            129
seed7/prg/brainf7.sd7        0.038364             86
seed7/prg/calc7.sd7          0.037819            128
seed7/prg/carddemo.sd7       0.038317            190
seed7/prg/castle.sd7         0.047615           3148
seed7/prg/cat.sd7            0.039309             82
seed7/prg/cellauto.sd7       0.037651             85
seed7/prg/celsius.sd7        0.035759             42
seed7/prg/chk_all.sd7        0.038342            843
seed7/prg/chkarr.sd7         0.037426           8367
seed7/prg/chkbig.sd7         0.040800          29026
seed7/prg/chkbin.sd7         0.037941           6469
seed7/prg/chkbitdata.sd7     0.038821           6624
seed7/prg/chkbool.sd7        0.036807           3157
seed7/prg/chkbst.sd7         0.037851            722
seed7/prg/chkchr.sd7         0.038307           2809
seed7/prg/chkcmd.sd7         0.037325           1205
seed7/prg/chkdb.sd7          0.039730           7454
seed7/prg/chkdecl.sd7        0.038819            448
seed7/prg/chkenum.sd7        0.036775           1230
seed7/prg/chkerr.sd7         0.039086           4663
seed7/prg/chkexc.sd7         0.037259           2627
seed7/prg/chkfil.sd7         0.039765           1615
seed7/prg/chkflt.sd7         0.040586          20620
seed7/prg/chkhent.sd7        0.037116             54
seed7/prg/chkhsh.sd7         0.039030           4548
seed7/prg/chkidx.sd7         0.040727          19567
seed7/prg/chkint.sd7         0.047167          38129
seed7/prg/chkjson.sd7        0.037708           1764
seed7/prg/chkovf.sd7         0.037255           8216
seed7/prg/chkprc.sd7         0.037708          10111
seed7/prg/chkscan.sd7        0.038890            714
seed7/prg/chkset.sd7         0.040188          11974
seed7/prg/chkstr.sd7         0.041520          26952
seed7/prg/chktime.sd7        0.039639           2025
seed7/prg/chktoml.sd7        0.038445           1656
seed7/prg/clock.sd7          0.035457             47
seed7/prg/clock2.sd7         0.035367             43
seed7/prg/clock3.sd7         0.037683             95
seed7/prg/cmpfil.sd7         0.036539             84
seed7/prg/comanche.sd7       0.038484            180
seed7/prg/confval.sd7        0.040263            175
seed7/prg/db7.sd7            0.038356            417
seed7/prg/diff7.sd7          0.038961            263
seed7/prg/dirtst.sd7         0.035580             42
seed7/prg/dirx.sd7           0.038212            152
seed7/prg/dnafight.sd7       0.038532           1381
seed7/prg/dragon.sd7         0.036659             73
seed7/prg/echo.sd7           0.035388             39
seed7/prg/eliza.sd7          0.038115            302
seed7/prg/err.sd7            0.040572             96
seed7/prg/fannkuch.sd7       0.037550            131
seed7/prg/fib.sd7            0.035826             47
seed7/prg/find7.sd7          0.037967            133
seed7/prg/findchar.sd7       0.038286            149
seed7/prg/fractree.sd7       0.036065             55
seed7/prg/ftp7.sd7           0.038150            296
seed7/prg/ftpserv.sd7        0.037204             74
seed7/prg/gcd.sd7            0.037097            109
seed7/prg/gkbd.sd7           0.038737            358
seed7/prg/gtksvtst.sd7       0.037563             94
seed7/prg/hal.sd7            0.038004            250
seed7/prg/hamu.sd7           0.037323            573
seed7/prg/hanoi.sd7          0.035797             55
seed7/prg/hd.sd7             0.038643             79
seed7/prg/hello.sd7          0.041495             32
seed7/prg/hilbert.sd7        0.039361            108
seed7/prg/ide7.sd7           0.039497            196
seed7/prg/kbd.sd7            0.035546             49
seed7/prg/klondike.sd7       0.039692            883
seed7/prg/lander.sd7         0.038859           1551
seed7/prg/lst80bas.sd7       0.037767            344
seed7/prg/lst99bas.sd7       0.037661            401
seed7/prg/lstgwbas.sd7       0.037363            577
seed7/prg/mahjong.sd7        0.038525           1943
seed7/prg/make7.sd7          0.038274            121
seed7/prg/mandelbr.sd7       0.038324            237
seed7/prg/mind.sd7           0.037202            443
seed7/prg/mirror.sd7         0.037883            131
seed7/prg/ms.sd7             0.038224            641
seed7/prg/nicoma.sd7         0.038327            135
seed7/prg/pac.sd7            0.036626            726
seed7/prg/pairs.sd7          0.037541           2025
seed7/prg/panic.sd7          0.044296           2634
seed7/prg/percolation.sd7    0.040062            330
seed7/prg/planets.sd7        0.039086           1486
seed7/prg/portfwd7.sd7       0.038228            139
seed7/prg/prime.sd7          0.036223             74
seed7/prg/printpi1.sd7       0.037678             56
seed7/prg/printpi2.sd7       0.036116             54
seed7/prg/printpi3.sd7       0.036333             60
seed7/prg/pv7.sd7            0.037951            337
seed7/prg/queen.sd7          0.036186            149
seed7/prg/rand.sd7           0.036448            121
seed7/prg/raytrace.sd7       0.038036            538
seed7/prg/rever.sd7          0.037697            816
seed7/prg/roman.sd7          0.035254             38
seed7/prg/s7c.sd7            0.040993           9060
seed7/prg/s7check.sd7        0.044044             68
seed7/prg/savehd7.sd7        0.041032           1110
seed7/prg/self.sd7           0.035373             49
seed7/prg/shisen.sd7         0.037884           1423
seed7/prg/sl.sd7             0.036347           1029
seed7/prg/snake.sd7          0.036678            615
seed7/prg/sokoban.sd7        0.038400            891
seed7/prg/spigotpi.sd7       0.036682             64
seed7/prg/sql7.sd7           0.038625            278
seed7/prg/startrek.sd7       0.038848            979
seed7/prg/sudoku7.sd7        0.039016           2657
seed7/prg/sydir7.sd7         0.038569            384
seed7/prg/syntaxhl.sd7       0.039096            177
seed7/prg/tak.sd7            0.036195             59
seed7/prg/tar7.sd7           0.038386            121
seed7/prg/tch.sd7            0.038571             55
seed7/prg/testfont.sd7       0.041238             95
seed7/prg/tet.sd7            0.038299            479
seed7/prg/tetg.sd7           0.038671            501
seed7/prg/toutf8.sd7         0.038870            240
seed7/prg/tst_cli.sd7        0.035400             40
seed7/prg/tst_srv.sd7        0.035054             47
seed7/prg/wator.sd7          0.036587            651
seed7/prg/which.sd7          0.036033             65
seed7/prg/wiz.sd7            0.038132           2833
seed7/prg/wordcnt.sd7        0.035784             54
seed7/prg/wrinum.sd7         0.036299             43
seed7/prg/wumpus.sd7         0.038005            372
seed7/lib/aes.s7i            0.041709           1144
seed7/lib/aes_gcm.s7i        0.039264            392
seed7/lib/ar.s7i             0.038836           1532
seed7/lib/arc4.s7i           0.038659            144
seed7/lib/archive.s7i        0.038606            143
seed7/lib/archive_base.s7i   0.038597            135
seed7/lib/array.s7i          0.038923            610
seed7/lib/asn1.s7i           0.036736            544
seed7/lib/asn1oid.s7i        0.040929            157
seed7/lib/basearray.s7i      0.038731            450
seed7/lib/bigfile.s7i        0.038307            136
seed7/lib/bigint.s7i         0.038468            824
seed7/lib/bigrat.s7i         0.038308            784
seed7/lib/bin16.s7i          0.038344            592
seed7/lib/bin32.s7i          0.038502            490
seed7/lib/bin64.s7i          0.038548            539
seed7/lib/bitdata.s7i        0.042432           1330
seed7/lib/bitmapfont.s7i     0.038433            215
seed7/lib/bitset.s7i         0.038617            593
seed7/lib/bitsetof.s7i       0.038759            431
seed7/lib/blowfish.s7i       0.040688            383
seed7/lib/bmp.s7i            0.038451            924
seed7/lib/boolean.s7i        0.037474            403
seed7/lib/browser.s7i        0.038026            280
seed7/lib/bstring.s7i        0.037520            227
seed7/lib/bytedata.s7i       0.038381            482
seed7/lib/bzip2.s7i          0.040241            887
seed7/lib/cards.s7i          0.036759           1342
seed7/lib/category.s7i       0.038672            209
seed7/lib/cc_conf.s7i        0.038201           1314
seed7/lib/ccittfax.s7i       0.039019           1022
seed7/lib/cgi.s7i            0.038759            109
seed7/lib/cgidialog.s7i      0.038514           1118
seed7/lib/char.s7i           0.038936            356
seed7/lib/charsets.s7i       0.040122           2024
seed7/lib/chartype.s7i       0.042322            121
seed7/lib/cipher.s7i         0.038373            146
seed7/lib/cli_cmds.s7i       0.038180           1360
seed7/lib/clib_file.s7i      0.037827            301
seed7/lib/color.s7i          0.037702            185
seed7/lib/complex.s7i        0.036185            464
seed7/lib/compress.s7i       0.038057            150
seed7/lib/console.s7i        0.037318            188
seed7/lib/cpio.s7i           0.039066           1708
seed7/lib/crc32.s7i          0.038746            193
seed7/lib/cronos16.s7i       0.039502           1173
seed7/lib/cronos27.s7i       0.040685           1464
seed7/lib/csv.s7i            0.039443            201
seed7/lib/db_prop.s7i        0.039066            991
seed7/lib/deflate.s7i        0.038665            740
seed7/lib/des.s7i            0.040202            444
seed7/lib/dialog.s7i         0.038661            311
seed7/lib/dir.s7i            0.041484            163
seed7/lib/draw.s7i           0.039449            854
seed7/lib/duration.s7i       0.038747           1038
seed7/lib/echo.s7i           0.038478            132
seed7/lib/editline.s7i       0.038749            398
seed7/lib/elf.s7i            0.040148           1560
seed7/lib/elliptic.s7i       0.036760            649
seed7/lib/enable_io.s7i      0.038402            312
seed7/lib/encoding.s7i       0.038985            931
seed7/lib/enumeration.s7i    0.038631            236
seed7/lib/environment.s7i    0.038319            175
seed7/lib/exif.s7i           0.038452            152
seed7/lib/external_file.s7i  0.038249            340
seed7/lib/field.s7i          0.038783            268
seed7/lib/file.s7i           0.038170            372
seed7/lib/filebits.s7i       0.036499             46
seed7/lib/filesys.s7i        0.038323            601
seed7/lib/fileutil.s7i       0.039005            144
seed7/lib/fixarray.s7i       0.038291            307
seed7/lib/float.s7i          0.038664            757
seed7/lib/font.s7i           0.038172            196
seed7/lib/font8x8.s7i        0.037214            998
seed7/lib/forloop.s7i        0.038514            449
seed7/lib/ftp.s7i            0.038795            969
seed7/lib/ftpserv.s7i        0.038038            631
seed7/lib/getf.s7i           0.037702            115
seed7/lib/gethttp.s7i        0.035575             41
seed7/lib/gethttps.s7i       0.035762             41
seed7/lib/gif.s7i            0.037813            561
seed7/lib/graph.s7i          0.041442            415
seed7/lib/graph_file.s7i     0.038380            399
seed7/lib/gtkserver.s7i      0.038669            161
seed7/lib/gzip.s7i           0.038381            573
seed7/lib/hash.s7i           0.041907            421
seed7/lib/hashsetof.s7i      0.040499            499
seed7/lib/hmac.s7i           0.038109            152
seed7/lib/html.s7i           0.037475             83
seed7/lib/html_ent.s7i       0.038136            476
seed7/lib/htmldom.s7i        0.038387            286
seed7/lib/http_request.s7i   0.038466            696
seed7/lib/http_srv_resp.s7i  0.038332            380
seed7/lib/https_request.s7i  0.038348            211
seed7/lib/httpserv.s7i       0.038486            345
seed7/lib/huffman.s7i        0.039775            644
seed7/lib/ico.s7i            0.039016            221
seed7/lib/idxarray.s7i       0.038586            232
seed7/lib/image.s7i          0.036796            156
seed7/lib/imagefile.s7i      0.038571            171
seed7/lib/inflate.s7i        0.039067            411
seed7/lib/inifile.s7i        0.038867            129
seed7/lib/integer.s7i        0.038388            663
seed7/lib/iobuffer.s7i       0.038261            289
seed7/lib/jpeg.s7i           0.037955           1761
seed7/lib/json.s7i           0.037451            891
seed7/lib/json_serde.s7i     0.037802            783
seed7/lib/keybd.s7i          0.037571            639
seed7/lib/keydescr.s7i       0.039354            192
seed7/lib/leb128.s7i         0.039557            218
seed7/lib/line.s7i           0.037390            164
seed7/lib/listener.s7i       0.038951            247
seed7/lib/logfile.s7i        0.040916             73
seed7/lib/lower.s7i          0.042834            142
seed7/lib/lzma.s7i           0.039461            934
seed7/lib/lzw.s7i            0.038489            861
seed7/lib/magic.s7i          0.038950            403
seed7/lib/mahjng32.s7i       0.037894           1500
seed7/lib/make.s7i           0.038632            544
seed7/lib/makedata.s7i       0.038525           1428
seed7/lib/math.s7i           0.038260            201
seed7/lib/mixarith.s7i       0.038218            249
seed7/lib/modern27.s7i       0.039967           1099
seed7/lib/more.s7i           0.038322            130
seed7/lib/msgdigest.s7i      0.039320           1222
seed7/lib/multiscr.s7i       0.037012             68
seed7/lib/null_file.s7i      0.038107            345
seed7/lib/osfiles.s7i        0.040158           1085
seed7/lib/pbm.s7i            0.038884            230
seed7/lib/pcx.s7i            0.038711            638
seed7/lib/pem.s7i            0.038456            185
seed7/lib/pgm.s7i            0.037924            238
seed7/lib/pic16.s7i          0.036404           1037
seed7/lib/pic32.s7i          0.037247           2060
seed7/lib/pic_util.s7i       0.037475            144
seed7/lib/pixelimage.s7i     0.037534            320
seed7/lib/pixmap_file.s7i    0.038987            459
seed7/lib/pixmapfont.s7i     0.039697            184
seed7/lib/pkcs1.s7i          0.043644            543
seed7/lib/png.s7i            0.038650           1064
seed7/lib/poll.s7i           0.038345            313
seed7/lib/ppm.s7i            0.038512            240
seed7/lib/process.s7i        0.038537            541
seed7/lib/progs.s7i          0.038385            789
seed7/lib/propertyfile.s7i   0.038202            155
seed7/lib/rational.s7i       0.038793            792
seed7/lib/ref_list.s7i       0.038676            252
seed7/lib/reference.s7i      0.038351            126
seed7/lib/reverse.s7i        0.037652             94
seed7/lib/rpm.s7i            0.039102           3487
seed7/lib/rpmext.s7i         0.039097            318
seed7/lib/scanfile.s7i       0.038775           1779
seed7/lib/scanjson.s7i       0.039086            413
seed7/lib/scanstri.s7i       0.038613           1814
seed7/lib/scantoml.s7i       0.038482           1603
seed7/lib/seed7_05.s7i       0.040765           1072
seed7/lib/set.s7i            0.039576             57
seed7/lib/shell.s7i          0.038200            615
seed7/lib/showtls.s7i        0.038123            678
seed7/lib/signature.s7i      0.037617            131
seed7/lib/smtp.s7i           0.037796            261
seed7/lib/sockbase.s7i       0.040625            217
seed7/lib/socket.s7i         0.038050            326
seed7/lib/sokoban1.s7i       0.038519           1519
seed7/lib/sql_base.s7i       0.041171           1000
seed7/lib/stars.s7i          0.039159           1705
seed7/lib/stdfont10.s7i      0.037354           3347
seed7/lib/stdfont12.s7i      0.037295           3928
seed7/lib/stdfont14.s7i      0.037204           4510
seed7/lib/stdfont16.s7i      0.037346           5092
seed7/lib/stdfont18.s7i      0.037614           5868
seed7/lib/stdfont20.s7i      0.037406           6449
seed7/lib/stdfont24.s7i      0.038934           7421
seed7/lib/stdfont8.s7i       0.036794           2960
seed7/lib/stdfont9.s7i       0.036996           3152
seed7/lib/stdio.s7i          0.038224            192
seed7/lib/strifile.s7i       0.038417            345
seed7/lib/string.s7i         0.039100            779
seed7/lib/stritext.s7i       0.038769            352
seed7/lib/struct.s7i         0.038322            266
seed7/lib/struct_elem.s7i    0.037482            129
seed7/lib/subfile.s7i        0.037773            174
seed7/lib/subrange.s7i       0.036471             78
seed7/lib/syntax.s7i         0.037672            294
seed7/lib/tar.s7i            0.038377           1880
seed7/lib/tar_cmds.s7i       0.037885            752
seed7/lib/tdes.s7i           0.037851            143
seed7/lib/tee.s7i            0.037562            143
seed7/lib/text.s7i           0.037532            135
seed7/lib/tga.s7i            0.040083            676
seed7/lib/tiff.s7i           0.038680           2771
seed7/lib/time.s7i           0.038700           1191
seed7/lib/tls.s7i            0.039081           2230
seed7/lib/unicode.s7i        0.039283            575
seed7/lib/unionfnd.s7i       0.037289            130
seed7/lib/upper.s7i          0.038146            142
seed7/lib/utf16.s7i          0.038309            540
seed7/lib/utf8.s7i           0.038671            234
seed7/lib/vecfont10.s7i      0.039706           1056
seed7/lib/vecfont18.s7i      0.040162           1119
seed7/lib/vector3d.s7i       0.037272            293
seed7/lib/vectorfont.s7i     0.038242            239
seed7/lib/wildcard.s7i       0.038972            140
seed7/lib/window.s7i         0.038727            455
seed7/lib/wrinum.s7i         0.038481            248
seed7/lib/x509cert.s7i       0.038512           1243
seed7/lib/xml_ent.s7i        0.038123             94
seed7/lib/xmldom.s7i         0.036823            303
seed7/lib/xz.s7i             0.038246            442
seed7/lib/zip.s7i            0.043432           2792
seed7/lib/zstd.s7i           0.043289           1263
============================ ================= ================

Statistical Summary — Mode B (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.038499        |
+-----------+-----------------+
| Minimum   | 0.035054        |
+-----------+-----------------+
| Maximum   | 0.047615        |
+-----------+-----------------+

Mode C — Mode Activation + Full-Buffer Fontification (font-lock-ensure)
-----------------------------------------------------------------------

Full-buffer fontification stress test.

File Load Times — Mode C (mean, GC-free)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

============================ ================= ================
File Name                    Mean Time (s)     Line count
============================ ================= ================
seed7/prg/addup.sd7          0.038679            190
seed7/prg/bas7.sd7           0.323249          11459
seed7/prg/bifurk.sd7         0.035399             73
seed7/prg/bigfiles.sd7       0.036931            129
seed7/prg/brainf7.sd7        0.037032             86
seed7/prg/calc7.sd7          0.039975            128
seed7/prg/carddemo.sd7       0.039653            190
seed7/prg/castle.sd7         0.106436           3148
seed7/prg/cat.sd7            0.036122             82
seed7/prg/cellauto.sd7       0.035983             85
seed7/prg/celsius.sd7        0.035109             42
seed7/prg/chk_all.sd7        0.057966            843
seed7/prg/chkarr.sd7         0.402317           8367
seed7/prg/chkbig.sd7         2.079120          29026
seed7/prg/chkbin.sd7         0.517027           6469
seed7/prg/chkbitdata.sd7     0.617034           6624
seed7/prg/chkbool.sd7        0.121816           3157
seed7/prg/chkbst.sd7         0.071584            722
seed7/prg/chkchr.sd7         0.230757           2809
seed7/prg/chkcmd.sd7         0.078001           1205
seed7/prg/chkdb.sd7          0.352970           7454
seed7/prg/chkdecl.sd7        0.061227            448
seed7/prg/chkenum.sd7        0.068606           1230
seed7/prg/chkerr.sd7         0.197240           4663
seed7/prg/chkexc.sd7         0.090039           2627
seed7/prg/chkfil.sd7         0.085229           1615
seed7/prg/chkflt.sd7         1.359779          20620
seed7/prg/chkhent.sd7        0.041325             54
seed7/prg/chkhsh.sd7         0.249479           4548
seed7/prg/chkidx.sd7         1.330276          19567
seed7/prg/chkint.sd7         2.514514          38129
seed7/prg/chkjson.sd7        0.106982           1764
seed7/prg/chkovf.sd7         0.590183           8216
seed7/prg/chkprc.sd7         0.338350          10111
seed7/prg/chkscan.sd7        0.059884            714
seed7/prg/chkset.sd7         0.692561          11974
seed7/prg/chkstr.sd7         1.460050          26952
seed7/prg/chktime.sd7        0.131121           2025
seed7/prg/chktoml.sd7        0.106419           1656
seed7/prg/clock.sd7          0.035200             47
seed7/prg/clock2.sd7         0.034958             43
seed7/prg/clock3.sd7         0.037158             95
seed7/prg/cmpfil.sd7         0.035798             84
seed7/prg/comanche.sd7       0.039804            180
seed7/prg/confval.sd7        0.041424            175
seed7/prg/db7.sd7            0.045118            417
seed7/prg/diff7.sd7          0.041382            263
seed7/prg/dirtst.sd7         0.034238             42
seed7/prg/dirx.sd7           0.037105            152
seed7/prg/dnafight.sd7       0.068417           1381
seed7/prg/dragon.sd7         0.035312             73
seed7/prg/echo.sd7           0.034872             39
seed7/prg/eliza.sd7          0.042033            302
seed7/prg/err.sd7            0.039310             96
seed7/prg/fannkuch.sd7       0.036650            131
seed7/prg/fib.sd7            0.034861             47
seed7/prg/find7.sd7          0.037388            133
seed7/prg/findchar.sd7       0.038556            149
seed7/prg/fractree.sd7       0.035327             55
seed7/prg/ftp7.sd7           0.041896            296
seed7/prg/ftpserv.sd7        0.036532             74
seed7/prg/gcd.sd7            0.037826            109
seed7/prg/gkbd.sd7           0.047187            358
seed7/prg/gtksvtst.sd7       0.037468             94
seed7/prg/hal.sd7            0.044514            250
seed7/prg/hamu.sd7           0.053397            573
seed7/prg/hanoi.sd7          0.035735             55
seed7/prg/hd.sd7             0.036273             79
seed7/prg/hello.sd7          0.035212             32
seed7/prg/hilbert.sd7        0.036385            108
seed7/prg/ide7.sd7           0.038998            196
seed7/prg/kbd.sd7            0.034850             49
seed7/prg/klondike.sd7       0.053140            883
seed7/prg/lander.sd7         0.070283           1551
seed7/prg/lst80bas.sd7       0.043367            344
seed7/prg/lst99bas.sd7       0.044504            401
seed7/prg/lstgwbas.sd7       0.050031            577
seed7/prg/mahjong.sd7        0.079105           1943
seed7/prg/make7.sd7          0.037290            121
seed7/prg/mandelbr.sd7       0.039498            237
seed7/prg/mind.sd7           0.044065            443
seed7/prg/mirror.sd7         0.038030            131
seed7/prg/ms.sd7             0.051957            641
seed7/prg/nicoma.sd7         0.038496            135
seed7/prg/pac.sd7            0.049654            726
seed7/prg/pairs.sd7          0.082214           2025
seed7/prg/panic.sd7          0.094968           2634
seed7/prg/percolation.sd7    0.041613            330
seed7/prg/planets.sd7        0.074658           1486
seed7/prg/portfwd7.sd7       0.037469            139
seed7/prg/prime.sd7          0.039866             74
seed7/prg/printpi1.sd7       0.039960             56
seed7/prg/printpi2.sd7       0.035141             54
seed7/prg/printpi3.sd7       0.037369             60
seed7/prg/pv7.sd7            0.043882            337
seed7/prg/queen.sd7          0.037503            149
seed7/prg/rand.sd7           0.037332            121
seed7/prg/raytrace.sd7       0.048107            538
seed7/prg/rever.sd7          0.053056            816
seed7/prg/roman.sd7          0.034804             38
seed7/prg/s7c.sd7            0.279922           9060
seed7/prg/s7check.sd7        0.036672             68
seed7/prg/savehd7.sd7        0.071415           1110
seed7/prg/self.sd7           0.036352             49
seed7/prg/shisen.sd7         0.071259           1423
seed7/prg/sl.sd7             0.057836           1029
seed7/prg/snake.sd7          0.045451            615
seed7/prg/sokoban.sd7        0.053004            891
seed7/prg/spigotpi.sd7       0.035087             64
seed7/prg/sql7.sd7           0.040295            278
seed7/prg/startrek.sd7       0.058842            979
seed7/prg/sudoku7.sd7        0.099638           2657
seed7/prg/sydir7.sd7         0.044911            384
seed7/prg/syntaxhl.sd7       0.040682            177
seed7/prg/tak.sd7            0.038869             59
seed7/prg/tar7.sd7           0.039315            121
seed7/prg/tch.sd7            0.038739             55
seed7/prg/testfont.sd7       0.038972             95
seed7/prg/tet.sd7            0.043905            479
seed7/prg/tetg.sd7           0.044742            501
seed7/prg/toutf8.sd7         0.042029            240
seed7/prg/tst_cli.sd7        0.036526             40
seed7/prg/tst_srv.sd7        0.041656             47
seed7/prg/wator.sd7          0.053929            651
seed7/prg/which.sd7          0.035628             65
seed7/prg/wiz.sd7            0.104261           2833
seed7/prg/wordcnt.sd7        0.035684             54
seed7/prg/wrinum.sd7         0.034648             43
seed7/prg/wumpus.sd7         0.041707            372
seed7/lib/aes.s7i            0.107437           1144
seed7/lib/aes_gcm.s7i        0.045390            392
seed7/lib/ar.s7i             0.073969           1532
seed7/lib/arc4.s7i           0.037684            144
seed7/lib/archive.s7i        0.039426            143
seed7/lib/archive_base.s7i   0.038085            135
seed7/lib/array.s7i          0.053103            610
seed7/lib/asn1.s7i           0.047640            544
seed7/lib/asn1oid.s7i        0.041995            157
seed7/lib/basearray.s7i      0.048683            450
seed7/lib/bigfile.s7i        0.037674            136
seed7/lib/bigint.s7i         0.055460            824
seed7/lib/bigrat.s7i         0.054284            784
seed7/lib/bin16.s7i          0.052702            592
seed7/lib/bin32.s7i          0.049182            490
seed7/lib/bin64.s7i          0.051100            539
seed7/lib/bitdata.s7i        0.077884           1330
seed7/lib/bitmapfont.s7i     0.040062            215
seed7/lib/bitset.s7i         0.050862            593
seed7/lib/bitsetof.s7i       0.047526            431
seed7/lib/blowfish.s7i       0.054743            383
seed7/lib/bmp.s7i            0.058837            924
seed7/lib/boolean.s7i        0.043297            403
seed7/lib/browser.s7i        0.042537            280
seed7/lib/bstring.s7i        0.040683            227
seed7/lib/bytedata.s7i       0.050097            482
seed7/lib/bzip2.s7i          0.058696            887
seed7/lib/cards.s7i          0.066650           1342
seed7/lib/category.s7i       0.040684            209
seed7/lib/cc_conf.s7i        0.077810           1314
seed7/lib/ccittfax.s7i       0.065294           1022
seed7/lib/cgi.s7i            0.037325            109
seed7/lib/cgidialog.s7i      0.059666           1118
seed7/lib/char.s7i           0.042703            356
seed7/lib/charsets.s7i       0.087002           2024
seed7/lib/chartype.s7i       0.041509            121
seed7/lib/cipher.s7i         0.040464            146
seed7/lib/cli_cmds.s7i       0.069253           1360
seed7/lib/clib_file.s7i      0.046927            301
seed7/lib/color.s7i          0.041795            185
seed7/lib/complex.s7i        0.046427            464
seed7/lib/compress.s7i       0.040036            150
seed7/lib/console.s7i        0.039129            188
seed7/lib/cpio.s7i           0.086006           1708
seed7/lib/crc32.s7i          0.043920            193
seed7/lib/cronos16.s7i       0.094862           1173
seed7/lib/cronos27.s7i       0.119206           1464
seed7/lib/csv.s7i            0.040374            201
seed7/lib/db_prop.s7i        0.062331            991
seed7/lib/deflate.s7i        0.061534            740
seed7/lib/des.s7i            0.058571            444
seed7/lib/dialog.s7i         0.052172            311
seed7/lib/dir.s7i            0.042787            163
seed7/lib/draw.s7i           0.055750            854
seed7/lib/duration.s7i       0.060423           1038
seed7/lib/echo.s7i           0.037388            132
seed7/lib/editline.s7i       0.044837            398
seed7/lib/elf.s7i            0.083960           1560
seed7/lib/elliptic.s7i       0.057032            649
seed7/lib/enable_io.s7i      0.045556            312
seed7/lib/encoding.s7i       0.071283            931
seed7/lib/enumeration.s7i    0.051457            236
seed7/lib/environment.s7i    0.046019            175
seed7/lib/exif.s7i           0.047487            152
seed7/lib/external_file.s7i  0.049043            340
seed7/lib/field.s7i          0.057827            268
seed7/lib/file.s7i           0.043699            372
seed7/lib/filebits.s7i       0.037069             46
seed7/lib/filesys.s7i        0.050004            601
seed7/lib/fileutil.s7i       0.037349            144
seed7/lib/fixarray.s7i       0.043182            307
seed7/lib/float.s7i          0.055497            757
seed7/lib/font.s7i           0.038869            196
seed7/lib/font8x8.s7i        0.048636            998
seed7/lib/forloop.s7i        0.045226            449
seed7/lib/ftp.s7i            0.057092            969
seed7/lib/ftpserv.s7i        0.053085            631
seed7/lib/getf.s7i           0.039316            115
seed7/lib/gethttp.s7i        0.035287             41
seed7/lib/gethttps.s7i       0.034676             41
seed7/lib/gif.s7i            0.048831            561
seed7/lib/graph.s7i          0.047577            415
seed7/lib/graph_file.s7i     0.043474            399
seed7/lib/gtkserver.s7i      0.041439            161
seed7/lib/gzip.s7i           0.047912            573
seed7/lib/hash.s7i           0.049223            421
seed7/lib/hashsetof.s7i      0.048158            499
seed7/lib/hmac.s7i           0.038735            152
seed7/lib/html.s7i           0.042316             83
seed7/lib/html_ent.s7i       0.046326            476
seed7/lib/htmldom.s7i        0.042671            286
seed7/lib/http_request.s7i   0.051735            696
seed7/lib/http_srv_resp.s7i  0.044629            380
seed7/lib/https_request.s7i  0.039640            211
seed7/lib/httpserv.s7i       0.043396            345
seed7/lib/huffman.s7i        0.052283            644
seed7/lib/ico.s7i            0.040741            221
seed7/lib/idxarray.s7i       0.041694            232
seed7/lib/image.s7i          0.041514            156
seed7/lib/imagefile.s7i      0.042777            171
seed7/lib/inflate.s7i        0.045356            411
seed7/lib/inifile.s7i        0.036732            129
seed7/lib/integer.s7i        0.050695            663
seed7/lib/iobuffer.s7i       0.040671            289
seed7/lib/jpeg.s7i           0.081803           1761
seed7/lib/json.s7i           0.054605            891
seed7/lib/json_serde.s7i     0.052554            783
seed7/lib/keybd.s7i          0.054781            639
seed7/lib/keydescr.s7i       0.040686            192
seed7/lib/leb128.s7i         0.039629            218
seed7/lib/line.s7i           0.037722            164
seed7/lib/listener.s7i       0.040885            247
seed7/lib/logfile.s7i        0.037426             73
seed7/lib/lower.s7i          0.037232            142
seed7/lib/lzma.s7i           0.059591            934
seed7/lib/lzw.s7i            0.058705            861
seed7/lib/magic.s7i          0.047679            403
seed7/lib/mahjng32.s7i       0.063451           1500
seed7/lib/make.s7i           0.049133            544
seed7/lib/makedata.s7i       0.069659           1428
seed7/lib/math.s7i           0.039470            201
seed7/lib/mixarith.s7i       0.039675            249
seed7/lib/modern27.s7i       0.083561           1099
seed7/lib/more.s7i           0.037848            130
seed7/lib/msgdigest.s7i      0.083372           1222
seed7/lib/multiscr.s7i       0.036496             68
seed7/lib/null_file.s7i      0.041209            345
seed7/lib/osfiles.s7i        0.065997           1085
seed7/lib/pbm.s7i            0.039882            230
seed7/lib/pcx.s7i            0.051903            638
seed7/lib/pem.s7i            0.039026            185
seed7/lib/pgm.s7i            0.040304            238
seed7/lib/pic16.s7i          0.048144           1037
seed7/lib/pic32.s7i          0.079375           2060
seed7/lib/pic_util.s7i       0.038315            144
seed7/lib/pixelimage.s7i     0.042099            320
seed7/lib/pixmap_file.s7i    0.045471            459
seed7/lib/pixmapfont.s7i     0.039509            184
seed7/lib/pkcs1.s7i          0.060331            543
seed7/lib/png.s7i            0.067358           1064
seed7/lib/poll.s7i           0.045150            313
seed7/lib/ppm.s7i            0.040856            240
seed7/lib/process.s7i        0.048548            541
seed7/lib/progs.s7i          0.055670            789
seed7/lib/propertyfile.s7i   0.037766            155
seed7/lib/rational.s7i       0.052943            792
seed7/lib/ref_list.s7i       0.040680            252
seed7/lib/reference.s7i      0.037029            126
seed7/lib/reverse.s7i        0.038851             94
seed7/lib/rpm.s7i            0.150448           3487
seed7/lib/rpmext.s7i         0.043241            318
seed7/lib/scanfile.s7i       0.080868           1779
seed7/lib/scanjson.s7i       0.047829            413
seed7/lib/scanstri.s7i       0.080906           1814
seed7/lib/scantoml.s7i       0.070844           1603
seed7/lib/seed7_05.s7i       0.066010           1072
seed7/lib/set.s7i            0.036359             57
seed7/lib/shell.s7i          0.052068            615
seed7/lib/showtls.s7i        0.053924            678
seed7/lib/signature.s7i      0.037160            131
seed7/lib/smtp.s7i           0.040416            261
seed7/lib/sockbase.s7i       0.041594            217
seed7/lib/socket.s7i         0.042337            326
seed7/lib/sokoban1.s7i       0.053058           1519
seed7/lib/sql_base.s7i       0.062442           1000
seed7/lib/stars.s7i          0.136023           1705
seed7/lib/stdfont10.s7i      0.081230           3347
seed7/lib/stdfont12.s7i      0.091682           3928
seed7/lib/stdfont14.s7i      0.107723           4510
seed7/lib/stdfont16.s7i      0.118830           5092
seed7/lib/stdfont18.s7i      0.134915           5868
seed7/lib/stdfont20.s7i      0.150106           6449
seed7/lib/stdfont24.s7i      0.178904           7421
seed7/lib/stdfont8.s7i       0.071210           2960
seed7/lib/stdfont9.s7i       0.076429           3152
seed7/lib/stdio.s7i          0.038665            192
seed7/lib/strifile.s7i       0.043730            345
seed7/lib/string.s7i         0.055346            779
seed7/lib/stritext.s7i       0.048865            352
seed7/lib/struct.s7i         0.043897            266
seed7/lib/struct_elem.s7i    0.037804            129
seed7/lib/subfile.s7i        0.038491            174
seed7/lib/subrange.s7i       0.037443             78
seed7/lib/syntax.s7i         0.045572            294
seed7/lib/tar.s7i            0.082073           1880
seed7/lib/tar_cmds.s7i       0.062126            752
seed7/lib/tdes.s7i           0.039049            143
seed7/lib/tee.s7i            0.037313            143
seed7/lib/text.s7i           0.040104            135
seed7/lib/tga.s7i            0.053768            676
seed7/lib/tiff.s7i           0.121224           2771
seed7/lib/time.s7i           0.061560           1191
seed7/lib/tls.s7i            0.102010           2230
seed7/lib/unicode.s7i        0.050769            575
seed7/lib/unionfnd.s7i       0.038739            130
seed7/lib/upper.s7i          0.037857            142
seed7/lib/utf16.s7i          0.049504            540
seed7/lib/utf8.s7i           0.041152            234
seed7/lib/vecfont10.s7i      0.078749           1056
seed7/lib/vecfont18.s7i      0.087122           1119
seed7/lib/vector3d.s7i       0.039982            293
seed7/lib/vectorfont.s7i     0.040061            239
seed7/lib/wildcard.s7i       0.037953            140
seed7/lib/window.s7i         0.045388            455
seed7/lib/wrinum.s7i         0.049291            248
seed7/lib/x509cert.s7i       0.071728           1243
seed7/lib/xml_ent.s7i        0.037810             94
seed7/lib/xmldom.s7i         0.043920            303
seed7/lib/xz.s7i             0.053765            442
seed7/lib/zip.s7i            0.120334           2792
seed7/lib/zstd.s7i           0.072091           1263
============================ ================= ================

Statistical Summary — Mode C (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.090383        |
+-----------+-----------------+
| Minimum   | 0.034238        |
+-----------+-----------------+
| Maximum   | 2.514514        |
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
seed7/prg/addup.sd7          0.043945            190
seed7/prg/bas7.sd7           0.772484          11459
seed7/prg/bifurk.sd7         0.037808             73
seed7/prg/bigfiles.sd7       0.041745            129
seed7/prg/brainf7.sd7        0.039752             86
seed7/prg/calc7.sd7          0.040628            128
seed7/prg/carddemo.sd7       0.045379            190
seed7/prg/castle.sd7         0.214009           3148
seed7/prg/cat.sd7            0.039111             82
seed7/prg/cellauto.sd7       0.039617             85
seed7/prg/celsius.sd7        0.045326             42
seed7/prg/chk_all.sd7        0.084144            843
seed7/prg/chkarr.sd7         0.864372           8367
seed7/prg/chkbig.sd7         4.139529          29026
seed7/prg/chkbin.sd7         1.027855           6469
seed7/prg/chkbitdata.sd7     1.241921           6624
seed7/prg/chkbool.sd7        0.232510           3157
seed7/prg/chkbst.sd7         0.101803            722
seed7/prg/chkchr.sd7         0.484919           2809
seed7/prg/chkcmd.sd7         0.109883           1205
seed7/prg/chkdb.sd7          0.763593           7454
seed7/prg/chkdecl.sd7        0.097708            448
seed7/prg/chkenum.sd7        0.128289           1230
seed7/prg/chkerr.sd7         0.338839           4663
seed7/prg/chkexc.sd7         0.149150           2627
seed7/prg/chkfil.sd7         0.128392           1615
seed7/prg/chkflt.sd7         2.837895          20620
seed7/prg/chkhent.sd7        0.037578             54
seed7/prg/chkhsh.sd7         0.497973           4548
seed7/prg/chkidx.sd7         3.184359          19567
seed7/prg/chkint.sd7         5.555027          38129
seed7/prg/chkjson.sd7        0.192109           1764
seed7/prg/chkovf.sd7         1.215336           8216
seed7/prg/chkprc.sd7         0.700530          10111
seed7/prg/chkscan.sd7        0.090983            714
seed7/prg/chkset.sd7         1.753957          11974
seed7/prg/chkstr.sd7         3.427283          26952
seed7/prg/chktime.sd7        0.260510           2025
seed7/prg/chktoml.sd7        0.209597           1656
seed7/prg/clock.sd7          0.043057             47
seed7/prg/clock2.sd7         0.042938             43
seed7/prg/clock3.sd7         0.047648             95
seed7/prg/cmpfil.sd7         0.043114             84
seed7/prg/comanche.sd7       0.048668            180
seed7/prg/confval.sd7        0.056591            175
seed7/prg/db7.sd7            0.066719            417
seed7/prg/diff7.sd7          0.052630            263
seed7/prg/dirtst.sd7         0.035601             42
seed7/prg/dirx.sd7           0.041446            152
seed7/prg/dnafight.sd7       0.123662           1381
seed7/prg/dragon.sd7         0.038378             73
seed7/prg/echo.sd7           0.036829             39
seed7/prg/eliza.sd7          0.051308            302
seed7/prg/err.sd7            0.045218             96
seed7/prg/fannkuch.sd7       0.042251            131
seed7/prg/fib.sd7            0.037725             47
seed7/prg/find7.sd7          0.043414            133
seed7/prg/findchar.sd7       0.043711            149
seed7/prg/fractree.sd7       0.037335             55
seed7/prg/ftp7.sd7           0.053004            296
seed7/prg/ftpserv.sd7        0.038635             74
seed7/prg/gcd.sd7            0.040804            109
seed7/prg/gkbd.sd7           0.061671            358
seed7/prg/gtksvtst.sd7       0.039836             94
seed7/prg/hal.sd7            0.047361            250
seed7/prg/hamu.sd7           0.067230            573
seed7/prg/hanoi.sd7          0.037029             55
seed7/prg/hd.sd7             0.038227             79
seed7/prg/hello.sd7          0.036005             32
seed7/prg/hilbert.sd7        0.039976            108
seed7/prg/ide7.sd7           0.047510            196
seed7/prg/kbd.sd7            0.037673             49
seed7/prg/klondike.sd7       0.087104            883
seed7/prg/lander.sd7         0.134416           1551
seed7/prg/lst80bas.sd7       0.057470            344
seed7/prg/lst99bas.sd7       0.059817            401
seed7/prg/lstgwbas.sd7       0.074002            577
seed7/prg/mahjong.sd7        0.152393           1943
seed7/prg/make7.sd7          0.043384            121
seed7/prg/mandelbr.sd7       0.049133            237
seed7/prg/mind.sd7           0.059807            443
seed7/prg/mirror.sd7         0.043487            131
seed7/prg/ms.sd7             0.069676            641
seed7/prg/nicoma.sd7         0.043917            135
seed7/prg/pac.sd7            0.071323            726
seed7/prg/pairs.sd7          0.138916           2025
seed7/prg/panic.sd7          0.194736           2634
seed7/prg/percolation.sd7    0.055788            330
seed7/prg/planets.sd7        0.138248           1486
seed7/prg/portfwd7.sd7       0.043252            139
seed7/prg/prime.sd7          0.041755             74
seed7/prg/printpi1.sd7       0.042789             56
seed7/prg/printpi2.sd7       0.038273             54
seed7/prg/printpi3.sd7       0.038842             60
seed7/prg/pv7.sd7            0.058982            337
seed7/prg/queen.sd7          0.042054            149
seed7/prg/rand.sd7           0.042618            121
seed7/prg/raytrace.sd7       0.076321            538
seed7/prg/rever.sd7          0.084522            816
seed7/prg/roman.sd7          0.035555             38
seed7/prg/s7c.sd7            0.627658           9060
seed7/prg/s7check.sd7        0.038462             68
seed7/prg/savehd7.sd7        0.117006           1110
seed7/prg/self.sd7           0.037664             49
seed7/prg/shisen.sd7         0.121144           1423
seed7/prg/sl.sd7             0.096128           1029
seed7/prg/snake.sd7          0.065182            615
seed7/prg/sokoban.sd7        0.084208            891
seed7/prg/spigotpi.sd7       0.037647             64
seed7/prg/sql7.sd7           0.053510            278
seed7/prg/startrek.sd7       0.096789            979
seed7/prg/sudoku7.sd7        0.200664           2657
seed7/prg/sydir7.sd7         0.063487            384
seed7/prg/syntaxhl.sd7       0.050873            177
seed7/prg/tak.sd7            0.036973             59
seed7/prg/tar7.sd7           0.044075            121
seed7/prg/tch.sd7            0.037378             55
seed7/prg/testfont.sd7       0.041867             95
seed7/prg/tet.sd7            0.059825            479
seed7/prg/tetg.sd7           0.064612            501
seed7/prg/toutf8.sd7         0.053782            240
seed7/prg/tst_cli.sd7        0.038168             40
seed7/prg/tst_srv.sd7        0.038220             47
seed7/prg/wator.sd7          0.076787            651
seed7/prg/which.sd7          0.037085             65
seed7/prg/wiz.sd7            0.218477           2833
seed7/prg/wordcnt.sd7        0.038421             54
seed7/prg/wrinum.sd7         0.039186             43
seed7/prg/wumpus.sd7         0.054314            372
seed7/lib/aes.s7i            0.199847           1144
seed7/lib/aes_gcm.s7i        0.061061            392
seed7/lib/ar.s7i             0.126055           1532
seed7/lib/arc4.s7i           0.043156            144
seed7/lib/archive.s7i        0.042717            143
seed7/lib/archive_base.s7i   0.042816            135
seed7/lib/array.s7i          0.074575            610
seed7/lib/asn1.s7i           0.064008            544
seed7/lib/asn1oid.s7i        0.049110            157
seed7/lib/basearray.s7i      0.067707            450
seed7/lib/bigfile.s7i        0.041583            136
seed7/lib/bigint.s7i         0.078037            824
seed7/lib/bigrat.s7i         0.081323            784
seed7/lib/bin16.s7i          0.071257            592
seed7/lib/bin32.s7i          0.063840            490
seed7/lib/bin64.s7i          0.063912            539
seed7/lib/bitdata.s7i        0.123375           1330
seed7/lib/bitmapfont.s7i     0.047270            215
seed7/lib/bitset.s7i         0.065863            593
seed7/lib/bitsetof.s7i       0.062012            431
seed7/lib/blowfish.s7i       0.077623            383
seed7/lib/bmp.s7i            0.098637            924
seed7/lib/boolean.s7i        0.053685            403
seed7/lib/browser.s7i        0.051728            280
seed7/lib/bstring.s7i        0.047729            227
seed7/lib/bytedata.s7i       0.066277            482
seed7/lib/bzip2.s7i          0.089630            887
seed7/lib/cards.s7i          0.106350           1342
seed7/lib/category.s7i       0.061708            209
seed7/lib/cc_conf.s7i        0.122589           1314
seed7/lib/ccittfax.s7i       0.102406           1022
seed7/lib/cgi.s7i            0.043194            109
seed7/lib/cgidialog.s7i      0.097701           1118
seed7/lib/char.s7i           0.052013            356
seed7/lib/charsets.s7i       0.124321           2024
seed7/lib/chartype.s7i       0.046808            121
seed7/lib/cipher.s7i         0.042465            146
seed7/lib/cli_cmds.s7i       0.113938           1360
seed7/lib/clib_file.s7i      0.050602            301
seed7/lib/color.s7i          0.048084            185
seed7/lib/complex.s7i        0.061886            464
seed7/lib/compress.s7i       0.047001            150
seed7/lib/console.s7i        0.049508            188
seed7/lib/cpio.s7i           0.151979           1708
seed7/lib/crc32.s7i          0.052902            193
seed7/lib/cronos16.s7i       0.203136           1173
seed7/lib/cronos27.s7i       0.262156           1464
seed7/lib/csv.s7i            0.049714            201
seed7/lib/db_prop.s7i        0.103579            991
seed7/lib/deflate.s7i        0.092143            740
seed7/lib/des.s7i            0.086524            444
seed7/lib/dialog.s7i         0.061665            311
seed7/lib/dir.s7i            0.046379            163
seed7/lib/draw.s7i           0.086540            854
seed7/lib/duration.s7i       0.098111           1038
seed7/lib/echo.s7i           0.045019            132
seed7/lib/editline.s7i       0.061314            398
seed7/lib/elf.s7i            0.159543           1560
seed7/lib/elliptic.s7i       0.077945            649
seed7/lib/enable_io.s7i      0.058860            312
seed7/lib/encoding.s7i       0.106438            931
seed7/lib/enumeration.s7i    0.054634            236
seed7/lib/environment.s7i    0.045699            175
seed7/lib/exif.s7i           0.046188            152
seed7/lib/external_file.s7i  0.052734            340
seed7/lib/field.s7i          0.058208            268
seed7/lib/file.s7i           0.056290            372
seed7/lib/filebits.s7i       0.042278             46
seed7/lib/filesys.s7i        0.073374            601
seed7/lib/fileutil.s7i       0.045870            144
seed7/lib/fixarray.s7i       0.054795            307
seed7/lib/float.s7i          0.074880            757
seed7/lib/font.s7i           0.045859            196
seed7/lib/font8x8.s7i        0.069697            998
seed7/lib/forloop.s7i        0.062006            449
seed7/lib/ftp.s7i            0.089493            969
seed7/lib/ftpserv.s7i        0.082832            631
seed7/lib/getf.s7i           0.044042            115
seed7/lib/gethttp.s7i        0.037822             41
seed7/lib/gethttps.s7i       0.036643             41
seed7/lib/gif.s7i            0.071308            561
seed7/lib/graph.s7i          0.064156            415
seed7/lib/graph_file.s7i     0.060126            399
seed7/lib/gtkserver.s7i      0.042625            161
seed7/lib/gzip.s7i           0.071618            573
seed7/lib/hash.s7i           0.067204            421
seed7/lib/hashsetof.s7i      0.067352            499
seed7/lib/hmac.s7i           0.046895            152
seed7/lib/html.s7i           0.039958             83
seed7/lib/html_ent.s7i       0.067073            476
seed7/lib/htmldom.s7i        0.053384            286
seed7/lib/http_request.s7i   0.076922            696
seed7/lib/http_srv_resp.s7i  0.061149            380
seed7/lib/https_request.s7i  0.048718            211
seed7/lib/httpserv.s7i       0.059188            345
seed7/lib/huffman.s7i        0.080465            644
seed7/lib/ico.s7i            0.048034            221
seed7/lib/idxarray.s7i       0.049721            232
seed7/lib/image.s7i          0.041909            156
seed7/lib/imagefile.s7i      0.045967            171
seed7/lib/inflate.s7i        0.063426            411
seed7/lib/inifile.s7i        0.041688            129
seed7/lib/integer.s7i        0.069712            663
seed7/lib/iobuffer.s7i       0.050533            289
seed7/lib/jpeg.s7i           0.155922           1761
seed7/lib/json.s7i           0.083981            891
seed7/lib/json_serde.s7i     0.081495            783
seed7/lib/keybd.s7i          0.083316            639
seed7/lib/keydescr.s7i       0.049141            192
seed7/lib/leb128.s7i         0.045575            218
seed7/lib/line.s7i           0.043049            164
seed7/lib/listener.s7i       0.054339            247
seed7/lib/logfile.s7i        0.045830             73
seed7/lib/lower.s7i          0.043784            142
seed7/lib/lzma.s7i           0.099227            934
seed7/lib/lzw.s7i            0.096193            861
seed7/lib/magic.s7i          0.064505            403
seed7/lib/mahjng32.s7i       0.094422           1500
seed7/lib/make.s7i           0.071290            544
seed7/lib/makedata.s7i       0.132327           1428
seed7/lib/math.s7i           0.046959            201
seed7/lib/mixarith.s7i       0.046760            249
seed7/lib/modern27.s7i       0.169355           1099
seed7/lib/more.s7i           0.041338            130
seed7/lib/msgdigest.s7i      0.139546           1222
seed7/lib/multiscr.s7i       0.037662             68
seed7/lib/null_file.s7i      0.050947            345
seed7/lib/osfiles.s7i        0.097685           1085
seed7/lib/pbm.s7i            0.048863            230
seed7/lib/pcx.s7i            0.075716            638
seed7/lib/pem.s7i            0.044393            185
seed7/lib/pgm.s7i            0.048982            238
seed7/lib/pic16.s7i          0.065568           1037
seed7/lib/pic32.s7i          0.118682           2060
seed7/lib/pic_util.s7i       0.044436            144
seed7/lib/pixelimage.s7i     0.051663            320
seed7/lib/pixmap_file.s7i    0.061484            459
seed7/lib/pixmapfont.s7i     0.047368            184
seed7/lib/pkcs1.s7i          0.078225            543
seed7/lib/png.s7i            0.106234           1064
seed7/lib/poll.s7i           0.052001            313
seed7/lib/ppm.s7i            0.052596            240
seed7/lib/process.s7i        0.064948            541
seed7/lib/progs.s7i          0.079917            789
seed7/lib/propertyfile.s7i   0.045417            155
seed7/lib/rational.s7i       0.079239            792
seed7/lib/ref_list.s7i       0.048978            252
seed7/lib/reference.s7i      0.041726            126
seed7/lib/reverse.s7i        0.040464             94
seed7/lib/rpm.s7i            0.290002           3487
seed7/lib/rpmext.s7i         0.055489            318
seed7/lib/scanfile.s7i       0.136773           1779
seed7/lib/scanjson.s7i       0.062427            413
seed7/lib/scanstri.s7i       0.138703           1814
seed7/lib/scantoml.s7i       0.131006           1603
seed7/lib/seed7_05.s7i       0.113310           1072
seed7/lib/set.s7i            0.039203             57
seed7/lib/shell.s7i          0.069481            615
seed7/lib/showtls.s7i        0.090227            678
seed7/lib/signature.s7i      0.043691            131
seed7/lib/smtp.s7i           0.049642            261
seed7/lib/sockbase.s7i       0.049452            217
seed7/lib/socket.s7i         0.056025            326
seed7/lib/sokoban1.s7i       0.085126           1519
seed7/lib/sql_base.s7i       0.094584           1000
seed7/lib/stars.s7i          0.237696           1705
seed7/lib/stdfont10.s7i      0.146153           3347
seed7/lib/stdfont12.s7i      0.168985           3928
seed7/lib/stdfont14.s7i      0.192955           4510
seed7/lib/stdfont16.s7i      0.214532           5092
seed7/lib/stdfont18.s7i      0.246958           5868
seed7/lib/stdfont20.s7i      0.281384           6449
seed7/lib/stdfont24.s7i      0.332883           7421
seed7/lib/stdfont8.s7i       0.132560           2960
seed7/lib/stdfont9.s7i       0.146464           3152
seed7/lib/stdio.s7i          0.044019            192
seed7/lib/strifile.s7i       0.054733            345
seed7/lib/string.s7i         0.074500            779
seed7/lib/stritext.s7i       0.057013            352
seed7/lib/struct.s7i         0.062786            266
seed7/lib/struct_elem.s7i    0.042095            129
seed7/lib/subfile.s7i        0.043668            174
seed7/lib/subrange.s7i       0.038197             78
seed7/lib/syntax.s7i         0.060282            294
seed7/lib/tar.s7i            0.150866           1880
seed7/lib/tar_cmds.s7i       0.091163            752
seed7/lib/tdes.s7i           0.044376            143
seed7/lib/tee.s7i            0.041050            143
seed7/lib/text.s7i           0.041076            135
seed7/lib/tga.s7i            0.080024            676
seed7/lib/tiff.s7i           0.243751           2771
seed7/lib/time.s7i           0.099541           1191
seed7/lib/tls.s7i            0.196623           2230
seed7/lib/unicode.s7i        0.077147            575
seed7/lib/unionfnd.s7i       0.042842            130
seed7/lib/upper.s7i          0.043655            142
seed7/lib/utf16.s7i          0.066005            540
seed7/lib/utf8.s7i           0.046598            234
seed7/lib/vecfont10.s7i      0.162304           1056
seed7/lib/vecfont18.s7i      0.177109           1119
seed7/lib/vector3d.s7i       0.048187            293
seed7/lib/vectorfont.s7i     0.046466            239
seed7/lib/wildcard.s7i       0.041540            140
seed7/lib/window.s7i         0.059700            455
seed7/lib/wrinum.s7i         0.049538            248
seed7/lib/x509cert.s7i       0.119168           1243
seed7/lib/xml_ent.s7i        0.043115             94
seed7/lib/xmldom.s7i         0.050103            303
seed7/lib/xz.s7i             0.059692            442
seed7/lib/zip.s7i            0.234999           2792
seed7/lib/zstd.s7i           0.122285           1263
============================ ================= ================

Statistical Summary — Mode D (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.160272        |
+-----------+-----------------+
| Minimum   | 0.035555        |
+-----------+-----------------+
| Maximum   | 5.555027        |
+-----------+-----------------+

Cross-Mode Comparison
=====================

+------+-----------------+-----------------+-----------------+
| Mode | Average (s)     | Minimum (s)     | Maximum (s)     |
+======+=================+=================+=================+
| A    | 0.033692        | 0.032140        | 0.040069        |
+------+-----------------+-----------------+-----------------+
| B    | 0.038499        | 0.035054        | 0.047615        |
+------+-----------------+-----------------+-----------------+
| C    | 0.090383        | 0.034238        | 2.514514        |
+------+-----------------+-----------------+-----------------+
| D    | 0.160272        | 0.035555        | 5.555027        |
+------+-----------------+-----------------+-----------------+

Phase Timing Summary
====================

Wall-clock times include warm-up work and GC performed by the warm-up helper.

+----------+--------------+--------------+--------------+
| Phase    | Warm-up      | Timed pass   | Mode total   |
+==========+==============+==============+==============+
| Mode A   | 00:00:12.865 | 00:00:57.398 | 00:01:10.264 |
+----------+--------------+--------------+--------------+
| Mode B   | 00:00:14.814 | 00:01:05.641 | 00:01:20.455 |
+----------+--------------+--------------+--------------+
| Mode C   | 00:00:35.834 | 00:02:34.880 | 00:03:10.715 |
+----------+--------------+--------------+--------------+
| Mode D   | 00:01:01.245 | 00:04:33.985 | 00:05:35.230 |
+----------+--------------+--------------+--------------+
| Total    |              |              | 00:11:16.671 |
+----------+--------------+--------------+--------------+


.. ---------------------------------------------------------------------------
