=======================================================
GC-Controlled Benchmark Report: Seed7 Mode — Four Modes
=======================================================

:Running with: seed7-mode 2026-06-22T21:19:10+0000 W26-1
:Seed7 dir   : /Users/roup/my/dvo/seed7-repos/seed7/
:Started at: 22:04:43 local time
:Generated on: 2026-06-23 02:16:15 UTC
:N Iterations: 5  (mean of N timed opens per file)
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
seed7/prg/addup.sd7          0.034968            190
seed7/prg/bas7.sd7           0.035314          11459
seed7/prg/bifurk.sd7         0.034312             73
seed7/prg/bigfiles.sd7       0.034575            129
seed7/prg/brainf7.sd7        0.035140             86
seed7/prg/calc7.sd7          0.034128            128
seed7/prg/carddemo.sd7       0.035909            190
seed7/prg/castle.sd7         0.035275           3148
seed7/prg/cat.sd7            0.033955             82
seed7/prg/cellauto.sd7       0.034476             85
seed7/prg/celsius.sd7        0.035194             42
seed7/prg/chk_all.sd7        0.035926            843
seed7/prg/chkarr.sd7         0.037834           8367
seed7/prg/chkbig.sd7         0.039960          29026
seed7/prg/chkbin.sd7         0.034878           6469
seed7/prg/chkbitdata.sd7     0.035505           6624
seed7/prg/chkbool.sd7        0.034066           3157
seed7/prg/chkbst.sd7         0.034275            722
seed7/prg/chkchr.sd7         0.034835           2809
seed7/prg/chkcmd.sd7         0.034657           1205
seed7/prg/chkdb.sd7          0.035780           7454
seed7/prg/chkdecl.sd7        0.034423            448
seed7/prg/chkenum.sd7        0.034518           1230
seed7/prg/chkerr.sd7         0.035027           4663
seed7/prg/chkexc.sd7         0.036925           2627
seed7/prg/chkfil.sd7         0.034743           1615
seed7/prg/chkflt.sd7         0.037382          20620
seed7/prg/chkhent.sd7        0.034888             54
seed7/prg/chkhsh.sd7         0.034855           4548
seed7/prg/chkidx.sd7         0.036660          19567
seed7/prg/chkint.sd7         0.040946          38129
seed7/prg/chkjson.sd7        0.034676           1764
seed7/prg/chkovf.sd7         0.035350           8216
seed7/prg/chkprc.sd7         0.034995          10111
seed7/prg/chkscan.sd7        0.034596            714
seed7/prg/chkset.sd7         0.035633          11974
seed7/prg/chkstr.sd7         0.038576          26952
seed7/prg/chktime.sd7        0.034738           2025
seed7/prg/chktoml.sd7        0.034202           1656
seed7/prg/clock.sd7          0.034356             47
seed7/prg/clock2.sd7         0.035500             43
seed7/prg/clock3.sd7         0.035790             95
seed7/prg/cmpfil.sd7         0.037999             84
seed7/prg/comanche.sd7       0.035449            180
seed7/prg/confval.sd7        0.034199            175
seed7/prg/db7.sd7            0.034167            417
seed7/prg/diff7.sd7          0.034230            263
seed7/prg/dirtst.sd7         0.034331             42
seed7/prg/dirx.sd7           0.034403            152
seed7/prg/dnafight.sd7       0.034486           1381
seed7/prg/dragon.sd7         0.035197             73
seed7/prg/echo.sd7           0.033894             39
seed7/prg/eliza.sd7          0.032944            302
seed7/prg/err.sd7            0.033166             96
seed7/prg/fannkuch.sd7       0.033597            131
seed7/prg/fib.sd7            0.033661             47
seed7/prg/find7.sd7          0.033003            133
seed7/prg/findchar.sd7       0.032918            149
seed7/prg/fractree.sd7       0.033124             55
seed7/prg/ftp7.sd7           0.033071            296
seed7/prg/ftpserv.sd7        0.033137             74
seed7/prg/gcd.sd7            0.034118            109
seed7/prg/gkbd.sd7           0.033999            358
seed7/prg/gtksvtst.sd7       0.034280             94
seed7/prg/hal.sd7            0.034262            250
seed7/prg/hamu.sd7           0.034211            573
seed7/prg/hanoi.sd7          0.034807             55
seed7/prg/hd.sd7             0.034981             79
seed7/prg/hello.sd7          0.034008             32
seed7/prg/hilbert.sd7        0.034363            108
seed7/prg/ide7.sd7           0.034373            196
seed7/prg/kbd.sd7            0.035194             49
seed7/prg/klondike.sd7       0.035491            883
seed7/prg/lander.sd7         0.037401           1551
seed7/prg/lst80bas.sd7       0.035614            344
seed7/prg/lst99bas.sd7       0.034490            401
seed7/prg/lstgwbas.sd7       0.034761            577
seed7/prg/mahjong.sd7        0.034490           1943
seed7/prg/make7.sd7          0.034409            121
seed7/prg/mandelbr.sd7       0.034284            237
seed7/prg/mind.sd7           0.034576            443
seed7/prg/mirror.sd7         0.035055            131
seed7/prg/ms.sd7             0.034541            641
seed7/prg/nicoma.sd7         0.034595            135
seed7/prg/pac.sd7            0.034628            726
seed7/prg/pairs.sd7          0.034073           2025
seed7/prg/panic.sd7          0.034059           2634
seed7/prg/percolation.sd7    0.034504            330
seed7/prg/planets.sd7        0.034302           1486
seed7/prg/portfwd7.sd7       0.034696            139
seed7/prg/prime.sd7          0.034925             74
seed7/prg/printpi1.sd7       0.034364             56
seed7/prg/printpi2.sd7       0.034420             54
seed7/prg/printpi3.sd7       0.034420             60
seed7/prg/pv7.sd7            0.035068            337
seed7/prg/queen.sd7          0.034386            149
seed7/prg/rand.sd7           0.034008            121
seed7/prg/raytrace.sd7       0.034197            538
seed7/prg/rever.sd7          0.034254            816
seed7/prg/roman.sd7          0.034146             38
seed7/prg/s7c.sd7            0.035047           9060
seed7/prg/s7check.sd7        0.034648             68
seed7/prg/savehd7.sd7        0.036530           1110
seed7/prg/self.sd7           0.037451             49
seed7/prg/shisen.sd7         0.037539           1423
seed7/prg/sl.sd7             0.035402           1029
seed7/prg/snake.sd7          0.034797            615
seed7/prg/sokoban.sd7        0.034445            891
seed7/prg/spigotpi.sd7       0.034229             64
seed7/prg/sql7.sd7           0.034517            278
seed7/prg/startrek.sd7       0.034984            979
seed7/prg/sudoku7.sd7        0.034295           2657
seed7/prg/sydir7.sd7         0.033060            384
seed7/prg/syntaxhl.sd7       0.032883            177
seed7/prg/tak.sd7            0.032752             59
seed7/prg/tar7.sd7           0.033425            121
seed7/prg/tch.sd7            0.033150             55
seed7/prg/testfont.sd7       0.034040             95
seed7/prg/tet.sd7            0.034751            479
seed7/prg/tetg.sd7           0.034338            501
seed7/prg/toutf8.sd7         0.034197            240
seed7/prg/tst_cli.sd7        0.034325             40
seed7/prg/tst_srv.sd7        0.034338             47
seed7/prg/wator.sd7          0.034305            651
seed7/prg/which.sd7          0.034051             65
seed7/prg/wiz.sd7            0.034578           2833
seed7/prg/wordcnt.sd7        0.034363             54
seed7/prg/wrinum.sd7         0.034441             43
seed7/prg/wumpus.sd7         0.034324            372
seed7/lib/aes.s7i            0.034300           1144
seed7/lib/aes_gcm.s7i        0.034380            392
seed7/lib/ar.s7i             0.035649           1532
seed7/lib/arc4.s7i           0.035637            144
seed7/lib/archive.s7i        0.038011            143
seed7/lib/archive_base.s7i   0.035102            135
seed7/lib/array.s7i          0.034587            610
seed7/lib/asn1.s7i           0.033849            544
seed7/lib/asn1oid.s7i        0.034389            157
seed7/lib/basearray.s7i      0.034156            450
seed7/lib/bigfile.s7i        0.034273            136
seed7/lib/bigint.s7i         0.034385            824
seed7/lib/bigrat.s7i         0.034294            784
seed7/lib/bin16.s7i          0.034063            592
seed7/lib/bin32.s7i          0.034576            490
seed7/lib/bin64.s7i          0.034203            539
seed7/lib/bitdata.s7i        0.034530           1330
seed7/lib/bitmapfont.s7i     0.034464            215
seed7/lib/bitset.s7i         0.034782            593
seed7/lib/bitsetof.s7i       0.033743            431
seed7/lib/blowfish.s7i       0.034232            383
seed7/lib/bmp.s7i            0.034316            924
seed7/lib/boolean.s7i        0.034368            403
seed7/lib/browser.s7i        0.034437            280
seed7/lib/bstring.s7i        0.034359            227
seed7/lib/bytedata.s7i       0.034067            482
seed7/lib/bzip2.s7i          0.034219            887
seed7/lib/cards.s7i          0.034312           1342
seed7/lib/category.s7i       0.034308            209
seed7/lib/cc_conf.s7i        0.034433           1314
seed7/lib/ccittfax.s7i       0.034445           1022
seed7/lib/cgi.s7i            0.034813            109
seed7/lib/cgidialog.s7i      0.035809           1118
seed7/lib/char.s7i           0.036796            356
seed7/lib/charsets.s7i       0.036269           2024
seed7/lib/chartype.s7i       0.034406            121
seed7/lib/cipher.s7i         0.034367            146
seed7/lib/cli_cmds.s7i       0.034573           1360
seed7/lib/clib_file.s7i      0.034274            301
seed7/lib/color.s7i          0.034476            185
seed7/lib/complex.s7i        0.034043            464
seed7/lib/compress.s7i       0.034247            150
seed7/lib/console.s7i        0.034184            188
seed7/lib/cpio.s7i           0.033731           1708
seed7/lib/crc32.s7i          0.035534            193
seed7/lib/cronos16.s7i       0.046646           1173
seed7/lib/cronos27.s7i       0.039718           1464
seed7/lib/csv.s7i            0.035165            201
seed7/lib/db_prop.s7i        0.034997            991
seed7/lib/deflate.s7i        0.034495            740
seed7/lib/des.s7i            0.035095            444
seed7/lib/dialog.s7i         0.035520            311
seed7/lib/dir.s7i            0.040963            163
seed7/lib/draw.s7i           0.037520            854
seed7/lib/duration.s7i       0.034947           1038
seed7/lib/echo.s7i           0.035230            132
seed7/lib/editline.s7i       0.034861            398
seed7/lib/elf.s7i            0.035096           1560
seed7/lib/elliptic.s7i       0.035047            649
seed7/lib/enable_io.s7i      0.034941            312
seed7/lib/encoding.s7i       0.035114            931
seed7/lib/enumeration.s7i    0.036394            236
seed7/lib/environment.s7i    0.037435            175
seed7/lib/exif.s7i           0.036941            152
seed7/lib/external_file.s7i  0.035184            340
seed7/lib/field.s7i          0.034918            268
seed7/lib/file.s7i           0.034790            372
seed7/lib/filebits.s7i       0.035164             46
seed7/lib/filesys.s7i        0.035138            601
seed7/lib/fileutil.s7i       0.036447            144
seed7/lib/fixarray.s7i       0.037979            307
seed7/lib/float.s7i          0.041779            757
seed7/lib/font.s7i           0.034982            196
seed7/lib/font8x8.s7i        0.034838            998
seed7/lib/forloop.s7i        0.034819            449
seed7/lib/ftp.s7i            0.034321            969
seed7/lib/ftpserv.s7i        0.039340            631
seed7/lib/getf.s7i           0.038392            115
seed7/lib/gethttp.s7i        0.034981             41
seed7/lib/gethttps.s7i       0.034568             41
seed7/lib/gif.s7i            0.034526            561
seed7/lib/graph.s7i          0.034468            415
seed7/lib/graph_file.s7i     0.033973            399
seed7/lib/gtkserver.s7i      0.034464            161
seed7/lib/gzip.s7i           0.035018            573
seed7/lib/hash.s7i           0.037864            421
seed7/lib/hashsetof.s7i      0.036197            499
seed7/lib/hmac.s7i           0.037468            152
seed7/lib/html.s7i           0.037199             83
seed7/lib/html_ent.s7i       0.038697            476
seed7/lib/htmldom.s7i        0.039892            286
seed7/lib/http_request.s7i   0.040949            696
seed7/lib/http_srv_resp.s7i  0.038195            380
seed7/lib/https_request.s7i  0.038422            211
seed7/lib/httpserv.s7i       0.036124            345
seed7/lib/huffman.s7i        0.038317            644
seed7/lib/ico.s7i            0.040033            221
seed7/lib/idxarray.s7i       0.039024            232
seed7/lib/image.s7i          0.036906            156
seed7/lib/imagefile.s7i      0.034414            171
seed7/lib/inflate.s7i        0.033815            411
seed7/lib/inifile.s7i        0.034772            129
seed7/lib/integer.s7i        0.034551            663
seed7/lib/iobuffer.s7i       0.034583            289
seed7/lib/jpeg.s7i           0.034521           1761
seed7/lib/json.s7i           0.034473            891
seed7/lib/json_serde.s7i     0.034416            783
seed7/lib/keybd.s7i          0.034315            639
seed7/lib/keydescr.s7i       0.034446            192
seed7/lib/leb128.s7i         0.034576            218
seed7/lib/line.s7i           0.034503            164
seed7/lib/listener.s7i       0.034804            247
seed7/lib/logfile.s7i        0.034266             73
seed7/lib/lower.s7i          0.034003            142
seed7/lib/lzma.s7i           0.034357            934
seed7/lib/lzw.s7i            0.034194            861
seed7/lib/magic.s7i          0.034113            403
seed7/lib/mahjng32.s7i       0.034472           1500
seed7/lib/make.s7i           0.035104            544
seed7/lib/makedata.s7i       0.035808           1428
seed7/lib/math.s7i           0.036785            201
seed7/lib/mixarith.s7i       0.038566            249
seed7/lib/modern27.s7i       0.036768           1099
seed7/lib/more.s7i           0.034541            130
seed7/lib/msgdigest.s7i      0.034618           1222
seed7/lib/multiscr.s7i       0.034193             68
seed7/lib/null_file.s7i      0.033394            345
seed7/lib/osfiles.s7i        0.034334           1085
seed7/lib/pbm.s7i            0.034409            230
seed7/lib/pcx.s7i            0.033990            638
seed7/lib/pem.s7i            0.034662            185
seed7/lib/pgm.s7i            0.034225            238
seed7/lib/pic16.s7i          0.034181           1037
seed7/lib/pic32.s7i          0.034667           2060
seed7/lib/pic_util.s7i       0.034613            144
seed7/lib/pixelimage.s7i     0.034519            320
seed7/lib/pixmap_file.s7i    0.034636            459
seed7/lib/pixmapfont.s7i     0.034440            184
seed7/lib/pkcs1.s7i          0.034029            543
seed7/lib/png.s7i            0.034400           1064
seed7/lib/poll.s7i           0.034082            313
seed7/lib/ppm.s7i            0.034172            240
seed7/lib/process.s7i        0.034103            541
seed7/lib/progs.s7i          0.033746            789
seed7/lib/propertyfile.s7i   0.034075            155
seed7/lib/rational.s7i       0.034371            792
seed7/lib/ref_list.s7i       0.034902            252
seed7/lib/reference.s7i      0.034331            126
seed7/lib/reverse.s7i        0.034218             94
seed7/lib/rpm.s7i            0.035814           3487
seed7/lib/rpmext.s7i         0.035921            318
seed7/lib/scanfile.s7i       0.037686           1779
seed7/lib/scanjson.s7i       0.035625            413
seed7/lib/scanstri.s7i       0.035085           1814
seed7/lib/scantoml.s7i       0.034703           1603
seed7/lib/seed7_05.s7i       0.034216           1072
seed7/lib/set.s7i            0.034395             57
seed7/lib/shell.s7i          0.034500            615
seed7/lib/showtls.s7i        0.033222            678
seed7/lib/signature.s7i      0.033513            131
seed7/lib/smtp.s7i           0.033361            261
seed7/lib/sockbase.s7i       0.032992            217
seed7/lib/socket.s7i         0.033373            326
seed7/lib/sokoban1.s7i       0.032895           1519
seed7/lib/sql_base.s7i       0.033462           1000
seed7/lib/stars.s7i          0.033252           1705
seed7/lib/stdfont10.s7i      0.033065           3347
seed7/lib/stdfont12.s7i      0.034355           3928
seed7/lib/stdfont14.s7i      0.034388           4510
seed7/lib/stdfont16.s7i      0.034739           5092
seed7/lib/stdfont18.s7i      0.035071           5868
seed7/lib/stdfont20.s7i      0.036219           6449
seed7/lib/stdfont24.s7i      0.035064           7421
seed7/lib/stdfont8.s7i       0.034067           2960
seed7/lib/stdfont9.s7i       0.034259           3152
seed7/lib/stdio.s7i          0.034486            192
seed7/lib/strifile.s7i       0.034230            345
seed7/lib/string.s7i         0.034230            779
seed7/lib/stritext.s7i       0.034418            352
seed7/lib/struct.s7i         0.034363            266
seed7/lib/struct_elem.s7i    0.035722            129
seed7/lib/subfile.s7i        0.035979            174
seed7/lib/subrange.s7i       0.042805             78
seed7/lib/syntax.s7i         0.041551            294
seed7/lib/tar.s7i            0.034540           1880
seed7/lib/tar_cmds.s7i       0.034465            752
seed7/lib/tdes.s7i           0.034127            143
seed7/lib/tee.s7i            0.034522            143
seed7/lib/text.s7i           0.034485            135
seed7/lib/tga.s7i            0.034480            676
seed7/lib/tiff.s7i           0.034511           2771
seed7/lib/time.s7i           0.034446           1191
seed7/lib/tls.s7i            0.034103           2230
seed7/lib/unicode.s7i        0.034281            575
seed7/lib/unionfnd.s7i       0.034194            130
seed7/lib/upper.s7i          0.035172            142
seed7/lib/utf16.s7i          0.034559            540
seed7/lib/utf8.s7i           0.034259            234
seed7/lib/vecfont10.s7i      0.034111           1056
seed7/lib/vecfont18.s7i      0.034392           1119
seed7/lib/vector3d.s7i       0.034278            293
seed7/lib/vectorfont.s7i     0.034035            239
seed7/lib/wildcard.s7i       0.034201            140
seed7/lib/window.s7i         0.034133            455
seed7/lib/wrinum.s7i         0.034734            248
seed7/lib/x509cert.s7i       0.034548           1243
seed7/lib/xml_ent.s7i        0.034354             94
seed7/lib/xmldom.s7i         0.034199            303
seed7/lib/xz.s7i             0.034448            442
seed7/lib/zip.s7i            0.034489           2792
seed7/lib/zstd.s7i           0.035180           1263
============================ ================= ================

Statistical Summary — Mode A (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.035022        |
+-----------+-----------------+
| Minimum   | 0.032752        |
+-----------+-----------------+
| Maximum   | 0.046646        |
+-----------+-----------------+

Mode B — Mode Activation + Initial Visible jit-lock Pass
--------------------------------------------------------

Buffer displayed in a window; jit-lock-fontify-now triggers visible-region jit-lock.

File Load Times — Mode B (mean, GC-free)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

============================ ================= ================
File Name                    Mean Time (s)     Line count
============================ ================= ================
seed7/prg/addup.sd7          0.038263            190
seed7/prg/bas7.sd7           0.039236          11459
seed7/prg/bifurk.sd7         0.036548             73
seed7/prg/bigfiles.sd7       0.040821            129
seed7/prg/brainf7.sd7        0.040912             86
seed7/prg/calc7.sd7          0.038569            128
seed7/prg/carddemo.sd7       0.038917            190
seed7/prg/castle.sd7         0.039232           3148
seed7/prg/cat.sd7            0.037109             82
seed7/prg/cellauto.sd7       0.037575             85
seed7/prg/celsius.sd7        0.035875             42
seed7/prg/chk_all.sd7        0.038342            843
seed7/prg/chkarr.sd7         0.038355           8367
seed7/prg/chkbig.sd7         0.042125          29026
seed7/prg/chkbin.sd7         0.038193           6469
seed7/prg/chkbitdata.sd7     0.039993           6624
seed7/prg/chkbool.sd7        0.037488           3157
seed7/prg/chkbst.sd7         0.037421            722
seed7/prg/chkchr.sd7         0.038879           2809
seed7/prg/chkcmd.sd7         0.037393           1205
seed7/prg/chkdb.sd7          0.040101           7454
seed7/prg/chkdecl.sd7        0.039188            448
seed7/prg/chkenum.sd7        0.037460           1230
seed7/prg/chkerr.sd7         0.039576           4663
seed7/prg/chkexc.sd7         0.037281           2627
seed7/prg/chkfil.sd7         0.038931           1615
seed7/prg/chkflt.sd7         0.040724          20620
seed7/prg/chkhent.sd7        0.036462             54
seed7/prg/chkhsh.sd7         0.041893           4548
seed7/prg/chkidx.sd7         0.046778          19567
seed7/prg/chkint.sd7         0.047699          38129
seed7/prg/chkjson.sd7        0.041062           1764
seed7/prg/chkovf.sd7         0.040570           8216
seed7/prg/chkprc.sd7         0.038930          10111
seed7/prg/chkscan.sd7        0.039479            714
seed7/prg/chkset.sd7         0.040701          11974
seed7/prg/chkstr.sd7         0.042362          26952
seed7/prg/chktime.sd7        0.039700           2025
seed7/prg/chktoml.sd7        0.039273           1656
seed7/prg/clock.sd7          0.036296             47
seed7/prg/clock2.sd7         0.035861             43
seed7/prg/clock3.sd7         0.038714             95
seed7/prg/cmpfil.sd7         0.037360             84
seed7/prg/comanche.sd7       0.039435            180
seed7/prg/confval.sd7        0.039758            175
seed7/prg/db7.sd7            0.038856            417
seed7/prg/diff7.sd7          0.038985            263
seed7/prg/dirtst.sd7         0.036021             42
seed7/prg/dirx.sd7           0.039058            152
seed7/prg/dnafight.sd7       0.038827           1381
seed7/prg/dragon.sd7         0.037077             73
seed7/prg/echo.sd7           0.035413             39
seed7/prg/eliza.sd7          0.039121            302
seed7/prg/err.sd7            0.041167             96
seed7/prg/fannkuch.sd7       0.038192            131
seed7/prg/fib.sd7            0.038399             47
seed7/prg/find7.sd7          0.041815            133
seed7/prg/findchar.sd7       0.038740            149
seed7/prg/fractree.sd7       0.035383             55
seed7/prg/ftp7.sd7           0.037571            296
seed7/prg/ftpserv.sd7        0.036748             74
seed7/prg/gcd.sd7            0.036171            109
seed7/prg/gkbd.sd7           0.037879            358
seed7/prg/gtksvtst.sd7       0.036531             94
seed7/prg/hal.sd7            0.037781            250
seed7/prg/hamu.sd7           0.036753            573
seed7/prg/hanoi.sd7          0.036467             55
seed7/prg/hd.sd7             0.037857             79
seed7/prg/hello.sd7          0.035915             32
seed7/prg/hilbert.sd7        0.038538            108
seed7/prg/ide7.sd7           0.038943            196
seed7/prg/kbd.sd7            0.035925             49
seed7/prg/klondike.sd7       0.038430            883
seed7/prg/lander.sd7         0.039654           1551
seed7/prg/lst80bas.sd7       0.037901            344
seed7/prg/lst99bas.sd7       0.038091            401
seed7/prg/lstgwbas.sd7       0.038495            577
seed7/prg/mahjong.sd7        0.038960           1943
seed7/prg/make7.sd7          0.038542            121
seed7/prg/mandelbr.sd7       0.039064            237
seed7/prg/mind.sd7           0.037600            443
seed7/prg/mirror.sd7         0.042188            131
seed7/prg/ms.sd7             0.046629            641
seed7/prg/nicoma.sd7         0.042929            135
seed7/prg/pac.sd7            0.038377            726
seed7/prg/pairs.sd7          0.039336           2025
seed7/prg/panic.sd7          0.039699           2634
seed7/prg/percolation.sd7    0.039296            330
seed7/prg/planets.sd7        0.039776           1486
seed7/prg/portfwd7.sd7       0.038580            139
seed7/prg/prime.sd7          0.036763             74
seed7/prg/printpi1.sd7       0.036705             56
seed7/prg/printpi2.sd7       0.036490             54
seed7/prg/printpi3.sd7       0.041173             60
seed7/prg/pv7.sd7            0.043454            337
seed7/prg/queen.sd7          0.039005            149
seed7/prg/rand.sd7           0.038848            121
seed7/prg/raytrace.sd7       0.038801            538
seed7/prg/rever.sd7          0.038460            816
seed7/prg/roman.sd7          0.036250             38
seed7/prg/s7c.sd7            0.039282           9060
seed7/prg/s7check.sd7        0.037364             68
seed7/prg/savehd7.sd7        0.039845           1110
seed7/prg/self.sd7           0.036505             49
seed7/prg/shisen.sd7         0.039286           1423
seed7/prg/sl.sd7             0.038650           1029
seed7/prg/snake.sd7          0.037980            615
seed7/prg/sokoban.sd7        0.038756            891
seed7/prg/spigotpi.sd7       0.039012             64
seed7/prg/sql7.sd7           0.040932            278
seed7/prg/startrek.sd7       0.038853            979
seed7/prg/sudoku7.sd7        0.039343           2657
seed7/prg/sydir7.sd7         0.038589            384
seed7/prg/syntaxhl.sd7       0.040039            177
seed7/prg/tak.sd7            0.036845             59
seed7/prg/tar7.sd7           0.039091            121
seed7/prg/tch.sd7            0.036645             55
seed7/prg/testfont.sd7       0.038678             95
seed7/prg/tet.sd7            0.038414            479
seed7/prg/tetg.sd7           0.038818            501
seed7/prg/toutf8.sd7         0.039286            240
seed7/prg/tst_cli.sd7        0.035807             40
seed7/prg/tst_srv.sd7        0.035953             47
seed7/prg/wator.sd7          0.037135            651
seed7/prg/which.sd7          0.037404             65
seed7/prg/wiz.sd7            0.039861           2833
seed7/prg/wordcnt.sd7        0.036965             54
seed7/prg/wrinum.sd7         0.036019             43
seed7/prg/wumpus.sd7         0.038673            372
seed7/lib/aes.s7i            0.042862           1144
seed7/lib/aes_gcm.s7i        0.039473            392
seed7/lib/ar.s7i             0.038007           1532
seed7/lib/arc4.s7i           0.037756            144
seed7/lib/archive.s7i        0.037161            143
seed7/lib/archive_base.s7i   0.037500            135
seed7/lib/array.s7i          0.040457            610
seed7/lib/asn1.s7i           0.039183            544
seed7/lib/asn1oid.s7i        0.042584            157
seed7/lib/basearray.s7i      0.037809            450
seed7/lib/bigfile.s7i        0.038362            136
seed7/lib/bigint.s7i         0.038736            824
seed7/lib/bigrat.s7i         0.038977            784
seed7/lib/bin16.s7i          0.039013            592
seed7/lib/bin32.s7i          0.038611            490
seed7/lib/bin64.s7i          0.038752            539
seed7/lib/bitdata.s7i        0.043022           1330
seed7/lib/bitmapfont.s7i     0.038843            215
seed7/lib/bitset.s7i         0.039090            593
seed7/lib/bitsetof.s7i       0.039297            431
seed7/lib/blowfish.s7i       0.041737            383
seed7/lib/bmp.s7i            0.039116            924
seed7/lib/boolean.s7i        0.037235            403
seed7/lib/browser.s7i        0.038919            280
seed7/lib/bstring.s7i        0.037980            227
seed7/lib/bytedata.s7i       0.038577            482
seed7/lib/bzip2.s7i          0.039433            887
seed7/lib/cards.s7i          0.037031           1342
seed7/lib/category.s7i       0.039227            209
seed7/lib/cc_conf.s7i        0.038734           1314
seed7/lib/ccittfax.s7i       0.039138           1022
seed7/lib/cgi.s7i            0.038372            109
seed7/lib/cgidialog.s7i      0.038620           1118
seed7/lib/char.s7i           0.040127            356
seed7/lib/charsets.s7i       0.042241           2024
seed7/lib/chartype.s7i       0.042675            121
seed7/lib/cipher.s7i         0.038898            146
seed7/lib/cli_cmds.s7i       0.038888           1360
seed7/lib/clib_file.s7i      0.039784            301
seed7/lib/color.s7i          0.042297            185
seed7/lib/complex.s7i        0.037937            464
seed7/lib/compress.s7i       0.039125            150
seed7/lib/console.s7i        0.038920            188
seed7/lib/cpio.s7i           0.042416           1708
seed7/lib/crc32.s7i          0.038669            193
seed7/lib/cronos16.s7i       0.039191           1173
seed7/lib/cronos27.s7i       0.039683           1464
seed7/lib/csv.s7i            0.037510            201
seed7/lib/db_prop.s7i        0.037687            991
seed7/lib/deflate.s7i        0.039023            740
seed7/lib/des.s7i            0.046515            444
seed7/lib/dialog.s7i         0.040990            311
seed7/lib/dir.s7i            0.038834            163
seed7/lib/draw.s7i           0.039343            854
seed7/lib/duration.s7i       0.039309           1038
seed7/lib/echo.s7i           0.038815            132
seed7/lib/editline.s7i       0.038772            398
seed7/lib/elf.s7i            0.039736           1560
seed7/lib/elliptic.s7i       0.037161            649
seed7/lib/enable_io.s7i      0.037373            312
seed7/lib/encoding.s7i       0.037940            931
seed7/lib/enumeration.s7i    0.037776            236
seed7/lib/environment.s7i    0.037334            175
seed7/lib/exif.s7i           0.038482            152
seed7/lib/external_file.s7i  0.041491            340
seed7/lib/field.s7i          0.039003            268
seed7/lib/file.s7i           0.038937            372
seed7/lib/filebits.s7i       0.037010             46
seed7/lib/filesys.s7i        0.039469            601
seed7/lib/fileutil.s7i       0.038970            144
seed7/lib/fixarray.s7i       0.039203            307
seed7/lib/float.s7i          0.038905            757
seed7/lib/font.s7i           0.039823            196
seed7/lib/font8x8.s7i        0.037439            998
seed7/lib/forloop.s7i        0.042660            449
seed7/lib/ftp.s7i            0.039266            969
seed7/lib/ftpserv.s7i        0.038985            631
seed7/lib/getf.s7i           0.038671            115
seed7/lib/gethttp.s7i        0.036771             41
seed7/lib/gethttps.s7i       0.036698             41
seed7/lib/gif.s7i            0.038762            561
seed7/lib/graph.s7i          0.040681            415
seed7/lib/graph_file.s7i     0.038872            399
seed7/lib/gtkserver.s7i      0.038538            161
seed7/lib/gzip.s7i           0.038908            573
seed7/lib/hash.s7i           0.040530            421
seed7/lib/hashsetof.s7i      0.039355            499
seed7/lib/hmac.s7i           0.037738            152
seed7/lib/html.s7i           0.036579             83
seed7/lib/html_ent.s7i       0.039491            476
seed7/lib/htmldom.s7i        0.043451            286
seed7/lib/http_request.s7i   0.040407            696
seed7/lib/http_srv_resp.s7i  0.038978            380
seed7/lib/https_request.s7i  0.038962            211
seed7/lib/httpserv.s7i       0.038959            345
seed7/lib/huffman.s7i        0.038759            644
seed7/lib/ico.s7i            0.039043            221
seed7/lib/idxarray.s7i       0.039223            232
seed7/lib/image.s7i          0.037050            156
seed7/lib/imagefile.s7i      0.038576            171
seed7/lib/inflate.s7i        0.039153            411
seed7/lib/inifile.s7i        0.038771            129
seed7/lib/integer.s7i        0.039181            663
seed7/lib/iobuffer.s7i       0.038673            289
seed7/lib/jpeg.s7i           0.039112           1761
seed7/lib/json.s7i           0.039487            891
seed7/lib/json_serde.s7i     0.038710            783
seed7/lib/keybd.s7i          0.038573            639
seed7/lib/keydescr.s7i       0.038818            192
seed7/lib/leb128.s7i         0.039069            218
seed7/lib/line.s7i           0.037293            164
seed7/lib/listener.s7i       0.038476            247
seed7/lib/logfile.s7i        0.037471             73
seed7/lib/lower.s7i          0.037298            142
seed7/lib/lzma.s7i           0.037864            934
seed7/lib/lzw.s7i            0.037847            861
seed7/lib/magic.s7i          0.038956            403
seed7/lib/mahjng32.s7i       0.037154           1500
seed7/lib/make.s7i           0.040601            544
seed7/lib/makedata.s7i       0.039765           1428
seed7/lib/math.s7i           0.037944            201
seed7/lib/mixarith.s7i       0.038387            249
seed7/lib/modern27.s7i       0.042267           1099
seed7/lib/more.s7i           0.037523            130
seed7/lib/msgdigest.s7i      0.038330           1222
seed7/lib/multiscr.s7i       0.036042             68
seed7/lib/null_file.s7i      0.038188            345
seed7/lib/osfiles.s7i        0.040161           1085
seed7/lib/pbm.s7i            0.039618            230
seed7/lib/pcx.s7i            0.039238            638
seed7/lib/pem.s7i            0.039209            185
seed7/lib/pgm.s7i            0.040076            238
seed7/lib/pic16.s7i          0.037485           1037
seed7/lib/pic32.s7i          0.038298           2060
seed7/lib/pic_util.s7i       0.039379            144
seed7/lib/pixelimage.s7i     0.038917            320
seed7/lib/pixmap_file.s7i    0.038604            459
seed7/lib/pixmapfont.s7i     0.039104            184
seed7/lib/pkcs1.s7i          0.044002            543
seed7/lib/png.s7i            0.039819           1064
seed7/lib/poll.s7i           0.039645            313
seed7/lib/ppm.s7i            0.040873            240
seed7/lib/process.s7i        0.038769            541
seed7/lib/progs.s7i          0.038105            789
seed7/lib/propertyfile.s7i   0.037447            155
seed7/lib/rational.s7i       0.039325            792
seed7/lib/ref_list.s7i       0.038770            252
seed7/lib/reference.s7i      0.038571            126
seed7/lib/reverse.s7i        0.037680             94
seed7/lib/rpm.s7i            0.039716           3487
seed7/lib/rpmext.s7i         0.039084            318
seed7/lib/scanfile.s7i       0.039406           1779
seed7/lib/scanjson.s7i       0.040087            413
seed7/lib/scanstri.s7i       0.039111           1814
seed7/lib/scantoml.s7i       0.039312           1603
seed7/lib/seed7_05.s7i       0.041198           1072
seed7/lib/set.s7i            0.036640             57
seed7/lib/shell.s7i          0.037929            615
seed7/lib/showtls.s7i        0.039243            678
seed7/lib/signature.s7i      0.040263            131
seed7/lib/smtp.s7i           0.038937            261
seed7/lib/sockbase.s7i       0.038539            217
seed7/lib/socket.s7i         0.039940            326
seed7/lib/sokoban1.s7i       0.039963           1519
seed7/lib/sql_base.s7i       0.045797           1000
seed7/lib/stars.s7i          0.042561           1705
seed7/lib/stdfont10.s7i      0.040494           3347
seed7/lib/stdfont12.s7i      0.040030           3928
seed7/lib/stdfont14.s7i      0.037068           4510
seed7/lib/stdfont16.s7i      0.036967           5092
seed7/lib/stdfont18.s7i      0.036600           5868
seed7/lib/stdfont20.s7i      0.037669           6449
seed7/lib/stdfont24.s7i      0.037930           7421
seed7/lib/stdfont8.s7i       0.036457           2960
seed7/lib/stdfont9.s7i       0.042615           3152
seed7/lib/stdio.s7i          0.046878            192
seed7/lib/strifile.s7i       0.038539            345
seed7/lib/string.s7i         0.038582            779
seed7/lib/stritext.s7i       0.039858            352
seed7/lib/struct.s7i         0.039625            266
seed7/lib/struct_elem.s7i    0.038678            129
seed7/lib/subfile.s7i        0.039115            174
seed7/lib/subrange.s7i       0.037668             78
seed7/lib/syntax.s7i         0.039690            294
seed7/lib/tar.s7i            0.039330           1880
seed7/lib/tar_cmds.s7i       0.039336            752
seed7/lib/tdes.s7i           0.038845            143
seed7/lib/tee.s7i            0.046745            143
seed7/lib/text.s7i           0.040542            135
seed7/lib/tga.s7i            0.042075            676
seed7/lib/tiff.s7i           0.041359           2771
seed7/lib/time.s7i           0.042903           1191
seed7/lib/tls.s7i            0.041866           2230
seed7/lib/unicode.s7i        0.040023            575
seed7/lib/unionfnd.s7i       0.038862            130
seed7/lib/upper.s7i          0.048990            142
seed7/lib/utf16.s7i          0.041175            540
seed7/lib/utf8.s7i           0.038716            234
seed7/lib/vecfont10.s7i      0.039910           1056
seed7/lib/vecfont18.s7i      0.040237           1119
seed7/lib/vector3d.s7i       0.037661            293
seed7/lib/vectorfont.s7i     0.038557            239
seed7/lib/wildcard.s7i       0.039163            140
seed7/lib/window.s7i         0.039999            455
seed7/lib/wrinum.s7i         0.039198            248
seed7/lib/x509cert.s7i       0.039424           1243
seed7/lib/xml_ent.s7i        0.038774             94
seed7/lib/xmldom.s7i         0.037236            303
seed7/lib/xz.s7i             0.038830            442
seed7/lib/zip.s7i            0.038815           2792
seed7/lib/zstd.s7i           0.039079           1263
============================ ================= ================

Statistical Summary — Mode B (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.039087        |
+-----------+-----------------+
| Minimum   | 0.035383        |
+-----------+-----------------+
| Maximum   | 0.048990        |
+-----------+-----------------+

Mode C — Mode Activation + Full-Buffer Fontification (font-lock-ensure)
-----------------------------------------------------------------------

Full-buffer fontification stress test.

File Load Times — Mode C (mean, GC-free)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

============================ ================= ================
File Name                    Mean Time (s)     Line count
============================ ================= ================
seed7/prg/addup.sd7          0.040049            190
seed7/prg/bas7.sd7           0.325063          11459
seed7/prg/bifurk.sd7         0.036339             73
seed7/prg/bigfiles.sd7       0.037510            129
seed7/prg/brainf7.sd7        0.036253             86
seed7/prg/calc7.sd7          0.036805            128
seed7/prg/carddemo.sd7       0.037955            190
seed7/prg/castle.sd7         0.108672           3148
seed7/prg/cat.sd7            0.035851             82
seed7/prg/cellauto.sd7       0.036637             85
seed7/prg/celsius.sd7        0.035504             42
seed7/prg/chk_all.sd7        0.058911            843
seed7/prg/chkarr.sd7         0.353251           8367
seed7/prg/chkbig.sd7         2.042818          29026
seed7/prg/chkbin.sd7         0.503784           6469
seed7/prg/chkbitdata.sd7     0.603432           6624
seed7/prg/chkbool.sd7        0.114690           3157
seed7/prg/chkbst.sd7         0.060564            722
seed7/prg/chkchr.sd7         0.211411           2809
seed7/prg/chkcmd.sd7         0.064967           1205
seed7/prg/chkdb.sd7          0.346695           7454
seed7/prg/chkdecl.sd7        0.057924            448
seed7/prg/chkenum.sd7        0.066943           1230
seed7/prg/chkerr.sd7         0.192814           4663
seed7/prg/chkexc.sd7         0.080898           2627
seed7/prg/chkfil.sd7         0.078160           1615
seed7/prg/chkflt.sd7         1.325095          20620
seed7/prg/chkhent.sd7        0.035002             54
seed7/prg/chkhsh.sd7         0.244764           4548
seed7/prg/chkidx.sd7         1.301113          19567
seed7/prg/chkint.sd7         2.487883          38129
seed7/prg/chkjson.sd7        0.097569           1764
seed7/prg/chkovf.sd7         0.555950           8216
seed7/prg/chkprc.sd7         0.316042          10111
seed7/prg/chkscan.sd7        0.054696            714
seed7/prg/chkset.sd7         0.650049          11974
seed7/prg/chkstr.sd7         1.369372          26952
seed7/prg/chktime.sd7        0.125765           2025
seed7/prg/chktoml.sd7        0.106146           1656
seed7/prg/clock.sd7          0.035762             47
seed7/prg/clock2.sd7         0.037596             43
seed7/prg/clock3.sd7         0.037598             95
seed7/prg/cmpfil.sd7         0.036539             84
seed7/prg/comanche.sd7       0.040142            180
seed7/prg/confval.sd7        0.042945            175
seed7/prg/db7.sd7            0.046873            417
seed7/prg/diff7.sd7          0.042808            263
seed7/prg/dirtst.sd7         0.035998             42
seed7/prg/dirx.sd7           0.038289            152
seed7/prg/dnafight.sd7       0.066038           1381
seed7/prg/dragon.sd7         0.036038             73
seed7/prg/echo.sd7           0.035622             39
seed7/prg/eliza.sd7          0.042537            302
seed7/prg/err.sd7            0.039912             96
seed7/prg/fannkuch.sd7       0.036958            131
seed7/prg/fib.sd7            0.035733             47
seed7/prg/find7.sd7          0.037705            133
seed7/prg/findchar.sd7       0.038663            149
seed7/prg/fractree.sd7       0.036035             55
seed7/prg/ftp7.sd7           0.041658            296
seed7/prg/ftpserv.sd7        0.035594             74
seed7/prg/gcd.sd7            0.035288            109
seed7/prg/gkbd.sd7           0.044335            358
seed7/prg/gtksvtst.sd7       0.035624             94
seed7/prg/hal.sd7            0.037945            250
seed7/prg/hamu.sd7           0.047443            573
seed7/prg/hanoi.sd7          0.035089             55
seed7/prg/hd.sd7             0.035254             79
seed7/prg/hello.sd7          0.034748             32
seed7/prg/hilbert.sd7        0.035376            108
seed7/prg/ide7.sd7           0.040172            196
seed7/prg/kbd.sd7            0.036846             49
seed7/prg/klondike.sd7       0.056831            883
seed7/prg/lander.sd7         0.071223           1551
seed7/prg/lst80bas.sd7       0.043806            344
seed7/prg/lst99bas.sd7       0.044957            401
seed7/prg/lstgwbas.sd7       0.050317            577
seed7/prg/mahjong.sd7        0.078951           1943
seed7/prg/make7.sd7          0.038331            121
seed7/prg/mandelbr.sd7       0.040246            237
seed7/prg/mind.sd7           0.044598            443
seed7/prg/mirror.sd7         0.039067            131
seed7/prg/ms.sd7             0.048355            641
seed7/prg/nicoma.sd7         0.038024            135
seed7/prg/pac.sd7            0.047940            726
seed7/prg/pairs.sd7          0.079279           2025
seed7/prg/panic.sd7          0.094303           2634
seed7/prg/percolation.sd7    0.042337            330
seed7/prg/planets.sd7        0.076044           1486
seed7/prg/portfwd7.sd7       0.038296            139
seed7/prg/prime.sd7          0.036000             74
seed7/prg/printpi1.sd7       0.035661             56
seed7/prg/printpi2.sd7       0.035726             54
seed7/prg/printpi3.sd7       0.035864             60
seed7/prg/pv7.sd7            0.043701            337
seed7/prg/queen.sd7          0.044274            149
seed7/prg/rand.sd7           0.040366            121
seed7/prg/raytrace.sd7       0.046800            538
seed7/prg/rever.sd7          0.052410            816
seed7/prg/roman.sd7          0.033845             38
seed7/prg/s7c.sd7            0.274374           9060
seed7/prg/s7check.sd7        0.035956             68
seed7/prg/savehd7.sd7        0.064159           1110
seed7/prg/self.sd7           0.034732             49
seed7/prg/shisen.sd7         0.071024           1423
seed7/prg/sl.sd7             0.064195           1029
seed7/prg/snake.sd7          0.046990            615
seed7/prg/sokoban.sd7        0.055254            891
seed7/prg/spigotpi.sd7       0.036540             64
seed7/prg/sql7.sd7           0.041596            278
seed7/prg/startrek.sd7       0.058557            979
seed7/prg/sudoku7.sd7        0.102490           2657
seed7/prg/sydir7.sd7         0.044986            384
seed7/prg/syntaxhl.sd7       0.039708            177
seed7/prg/tak.sd7            0.034774             59
seed7/prg/tar7.sd7           0.036762            121
seed7/prg/tch.sd7            0.039669             55
seed7/prg/testfont.sd7       0.040328             95
seed7/prg/tet.sd7            0.043393            479
seed7/prg/tetg.sd7           0.045425            501
seed7/prg/toutf8.sd7         0.042308            240
seed7/prg/tst_cli.sd7        0.035835             40
seed7/prg/tst_srv.sd7        0.035605             47
seed7/prg/wator.sd7          0.051222            651
seed7/prg/which.sd7          0.034955             65
seed7/prg/wiz.sd7            0.102318           2833
seed7/prg/wordcnt.sd7        0.035354             54
seed7/prg/wrinum.sd7         0.034379             43
seed7/prg/wumpus.sd7         0.041455            372
seed7/lib/aes.s7i            0.107362           1144
seed7/lib/aes_gcm.s7i        0.044804            392
seed7/lib/ar.s7i             0.070418           1532
seed7/lib/arc4.s7i           0.038534            144
seed7/lib/archive.s7i        0.038468            143
seed7/lib/archive_base.s7i   0.038211            135
seed7/lib/array.s7i          0.053548            610
seed7/lib/asn1.s7i           0.047020            544
seed7/lib/asn1oid.s7i        0.042062            157
seed7/lib/basearray.s7i      0.048494            450
seed7/lib/bigfile.s7i        0.038148            136
seed7/lib/bigint.s7i         0.055597            824
seed7/lib/bigrat.s7i         0.054062            784
seed7/lib/bin16.s7i          0.056411            592
seed7/lib/bin32.s7i          0.050633            490
seed7/lib/bin64.s7i          0.048198            539
seed7/lib/bitdata.s7i        0.074489           1330
seed7/lib/bitmapfont.s7i     0.038441            215
seed7/lib/bitset.s7i         0.047021            593
seed7/lib/bitsetof.s7i       0.047698            431
seed7/lib/blowfish.s7i       0.056328            383
seed7/lib/bmp.s7i            0.060006            924
seed7/lib/boolean.s7i        0.044510            403
seed7/lib/browser.s7i        0.042618            280
seed7/lib/bstring.s7i        0.041146            227
seed7/lib/bytedata.s7i       0.050504            482
seed7/lib/bzip2.s7i          0.058905            887
seed7/lib/cards.s7i          0.067185           1342
seed7/lib/category.s7i       0.041008            209
seed7/lib/cc_conf.s7i        0.076212           1314
seed7/lib/ccittfax.s7i       0.063488           1022
seed7/lib/cgi.s7i            0.036153            109
seed7/lib/cgidialog.s7i      0.058493           1118
seed7/lib/char.s7i           0.042568            356
seed7/lib/charsets.s7i       0.078906           2024
seed7/lib/chartype.s7i       0.038692            121
seed7/lib/cipher.s7i         0.036958            146
seed7/lib/cli_cmds.s7i       0.073186           1360
seed7/lib/clib_file.s7i      0.042592            301
seed7/lib/color.s7i          0.040929            185
seed7/lib/complex.s7i        0.045097            464
seed7/lib/compress.s7i       0.038415            150
seed7/lib/console.s7i        0.039229            188
seed7/lib/cpio.s7i           0.080716           1708
seed7/lib/crc32.s7i          0.043740            193
seed7/lib/cronos16.s7i       0.092502           1173
seed7/lib/cronos27.s7i       0.116857           1464
seed7/lib/csv.s7i            0.040877            201
seed7/lib/db_prop.s7i        0.067112            991
seed7/lib/deflate.s7i        0.058738            740
seed7/lib/des.s7i            0.056844            444
seed7/lib/dialog.s7i         0.043808            311
seed7/lib/dir.s7i            0.038509            163
seed7/lib/draw.s7i           0.056877            854
seed7/lib/duration.s7i       0.065389           1038
seed7/lib/echo.s7i           0.038279            132
seed7/lib/editline.s7i       0.046925            398
seed7/lib/elf.s7i            0.083452           1560
seed7/lib/elliptic.s7i       0.050345            649
seed7/lib/enable_io.s7i      0.041928            312
seed7/lib/encoding.s7i       0.058757            931
seed7/lib/enumeration.s7i    0.040655            236
seed7/lib/environment.s7i    0.038224            175
seed7/lib/exif.s7i           0.038488            152
seed7/lib/external_file.s7i  0.045334            340
seed7/lib/field.s7i          0.042154            268
seed7/lib/file.s7i           0.045215            372
seed7/lib/filebits.s7i       0.036331             46
seed7/lib/filesys.s7i        0.048495            601
seed7/lib/fileutil.s7i       0.038254            144
seed7/lib/fixarray.s7i       0.043398            307
seed7/lib/float.s7i          0.054592            757
seed7/lib/font.s7i           0.039796            196
seed7/lib/font8x8.s7i        0.048442            998
seed7/lib/forloop.s7i        0.046583            449
seed7/lib/ftp.s7i            0.063358            969
seed7/lib/ftpserv.s7i        0.053050            631
seed7/lib/getf.s7i           0.037338            115
seed7/lib/gethttp.s7i        0.035693             41
seed7/lib/gethttps.s7i       0.035730             41
seed7/lib/gif.s7i            0.049481            561
seed7/lib/graph.s7i          0.048909            415
seed7/lib/graph_file.s7i     0.047840            399
seed7/lib/gtkserver.s7i      0.039915            161
seed7/lib/gzip.s7i           0.050579            573
seed7/lib/hash.s7i           0.051673            421
seed7/lib/hashsetof.s7i      0.051206            499
seed7/lib/hmac.s7i           0.039072            152
seed7/lib/html.s7i           0.036715             83
seed7/lib/html_ent.s7i       0.046374            476
seed7/lib/htmldom.s7i        0.043290            286
seed7/lib/http_request.s7i   0.051746            696
seed7/lib/http_srv_resp.s7i  0.045888            380
seed7/lib/https_request.s7i  0.041745            211
seed7/lib/httpserv.s7i       0.048829            345
seed7/lib/huffman.s7i        0.054117            644
seed7/lib/ico.s7i            0.044115            221
seed7/lib/idxarray.s7i       0.044540            232
seed7/lib/image.s7i          0.036453            156
seed7/lib/imagefile.s7i      0.038490            171
seed7/lib/inflate.s7i        0.045303            411
seed7/lib/inifile.s7i        0.037896            129
seed7/lib/integer.s7i        0.052519            663
seed7/lib/iobuffer.s7i       0.041598            289
seed7/lib/jpeg.s7i           0.084677           1761
seed7/lib/json.s7i           0.055639            891
seed7/lib/json_serde.s7i     0.052705            783
seed7/lib/keybd.s7i          0.055664            639
seed7/lib/keydescr.s7i       0.044262            192
seed7/lib/leb128.s7i         0.041373            218
seed7/lib/line.s7i           0.038197            164
seed7/lib/listener.s7i       0.041065            247
seed7/lib/logfile.s7i        0.036568             73
seed7/lib/lower.s7i          0.037810            142
seed7/lib/lzma.s7i           0.058654            934
seed7/lib/lzw.s7i            0.059102            861
seed7/lib/magic.s7i          0.048185            403
seed7/lib/mahjng32.s7i       0.063184           1500
seed7/lib/make.s7i           0.048543            544
seed7/lib/makedata.s7i       0.067270           1428
seed7/lib/math.s7i           0.038118            201
seed7/lib/mixarith.s7i       0.038533            249
seed7/lib/modern27.s7i       0.086327           1099
seed7/lib/more.s7i           0.038062            130
seed7/lib/msgdigest.s7i      0.079036           1222
seed7/lib/multiscr.s7i       0.036041             68
seed7/lib/null_file.s7i      0.042720            345
seed7/lib/osfiles.s7i        0.066036           1085
seed7/lib/pbm.s7i            0.040241            230
seed7/lib/pcx.s7i            0.052484            638
seed7/lib/pem.s7i            0.045816            185
seed7/lib/pgm.s7i            0.040658            238
seed7/lib/pic16.s7i          0.048924           1037
seed7/lib/pic32.s7i          0.078351           2060
seed7/lib/pic_util.s7i       0.037263            144
seed7/lib/pixelimage.s7i     0.042433            320
seed7/lib/pixmap_file.s7i    0.044506            459
seed7/lib/pixmapfont.s7i     0.038649            184
seed7/lib/pkcs1.s7i          0.058241            543
seed7/lib/png.s7i            0.062595           1064
seed7/lib/poll.s7i           0.043355            313
seed7/lib/ppm.s7i            0.038913            240
seed7/lib/process.s7i        0.051184            541
seed7/lib/progs.s7i          0.057410            789
seed7/lib/propertyfile.s7i   0.046414            155
seed7/lib/rational.s7i       0.054208            792
seed7/lib/ref_list.s7i       0.041597            252
seed7/lib/reference.s7i      0.037832            126
seed7/lib/reverse.s7i        0.036360             94
seed7/lib/rpm.s7i            0.141745           3487
seed7/lib/rpmext.s7i         0.042394            318
seed7/lib/scanfile.s7i       0.079486           1779
seed7/lib/scanjson.s7i       0.046942            413
seed7/lib/scanstri.s7i       0.078941           1814
seed7/lib/scantoml.s7i       0.070239           1603
seed7/lib/seed7_05.s7i       0.065589           1072
seed7/lib/set.s7i            0.035999             57
seed7/lib/shell.s7i          0.055322            615
seed7/lib/showtls.s7i        0.057778            678
seed7/lib/signature.s7i      0.038227            131
seed7/lib/smtp.s7i           0.040858            261
seed7/lib/sockbase.s7i       0.042183            217
seed7/lib/socket.s7i         0.043208            326
seed7/lib/sokoban1.s7i       0.053500           1519
seed7/lib/sql_base.s7i       0.064022           1000
seed7/lib/stars.s7i          0.139082           1705
seed7/lib/stdfont10.s7i      0.081607           3347
seed7/lib/stdfont12.s7i      0.092787           3928
seed7/lib/stdfont14.s7i      0.108946           4510
seed7/lib/stdfont16.s7i      0.113998           5092
seed7/lib/stdfont18.s7i      0.129666           5868
seed7/lib/stdfont20.s7i      0.143774           6449
seed7/lib/stdfont24.s7i      0.176280           7421
seed7/lib/stdfont8.s7i       0.071955           2960
seed7/lib/stdfont9.s7i       0.074058           3152
seed7/lib/stdio.s7i          0.039148            192
seed7/lib/strifile.s7i       0.042153            345
seed7/lib/string.s7i         0.053610            779
seed7/lib/stritext.s7i       0.041653            352
seed7/lib/struct.s7i         0.042679            266
seed7/lib/struct_elem.s7i    0.036181            129
seed7/lib/subfile.s7i        0.037979            174
seed7/lib/subrange.s7i       0.036166             78
seed7/lib/syntax.s7i         0.045604            294
seed7/lib/tar.s7i            0.080734           1880
seed7/lib/tar_cmds.s7i       0.055588            752
seed7/lib/tdes.s7i           0.037243            143
seed7/lib/tee.s7i            0.036442            143
seed7/lib/text.s7i           0.037594            135
seed7/lib/tga.s7i            0.058580            676
seed7/lib/tiff.s7i           0.122249           2771
seed7/lib/time.s7i           0.062710           1191
seed7/lib/tls.s7i            0.103215           2230
seed7/lib/unicode.s7i        0.051527            575
seed7/lib/unionfnd.s7i       0.037451            130
seed7/lib/upper.s7i          0.037758            142
seed7/lib/utf16.s7i          0.049123            540
seed7/lib/utf8.s7i           0.041311            234
seed7/lib/vecfont10.s7i      0.077970           1056
seed7/lib/vecfont18.s7i      0.086225           1119
seed7/lib/vector3d.s7i       0.039454            293
seed7/lib/vectorfont.s7i     0.038914            239
seed7/lib/wildcard.s7i       0.036611            140
seed7/lib/window.s7i         0.043728            455
seed7/lib/wrinum.s7i         0.039272            248
seed7/lib/x509cert.s7i       0.068903           1243
seed7/lib/xml_ent.s7i        0.035656             94
seed7/lib/xmldom.s7i         0.039621            303
seed7/lib/xz.s7i             0.046194            442
seed7/lib/zip.s7i            0.118356           2792
seed7/lib/zstd.s7i           0.069947           1263
============================ ================= ================

Statistical Summary — Mode C (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.088399        |
+-----------+-----------------+
| Minimum   | 0.033845        |
+-----------+-----------------+
| Maximum   | 2.487883        |
+-----------+-----------------+

Mode D — Mode Activation + Full Incremental jit-lock (Scroll Pass)
------------------------------------------------------------------

Scrolls through the full buffer and calls jit-lock-fontify-now for each visible region.

File Load Times — Mode D (mean, GC-free)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

============================ ================= ================
File Name                    Mean Time (s)     Line count
============================ ================= ================
seed7/prg/addup.sd7          0.045608            190
seed7/prg/bas7.sd7           0.801286          11459
seed7/prg/bifurk.sd7         0.038603             73
seed7/prg/bigfiles.sd7       0.042304            129
seed7/prg/brainf7.sd7        0.041512             86
seed7/prg/calc7.sd7          0.042515            128
seed7/prg/carddemo.sd7       0.047396            190
seed7/prg/castle.sd7         0.224108           3148
seed7/prg/cat.sd7            0.037292             82
seed7/prg/cellauto.sd7       0.038786             85
seed7/prg/celsius.sd7        0.035957             42
seed7/prg/chk_all.sd7        0.082840            843
seed7/prg/chkarr.sd7         0.889321           8367
seed7/prg/chkbig.sd7         4.261686          29026
seed7/prg/chkbin.sd7         1.054513           6469
seed7/prg/chkbitdata.sd7     1.279908           6624
seed7/prg/chkbool.sd7        0.238497           3157
seed7/prg/chkbst.sd7         0.109565            722
seed7/prg/chkchr.sd7         0.496204           2809
seed7/prg/chkcmd.sd7         0.112126           1205
seed7/prg/chkdb.sd7          0.781086           7454
seed7/prg/chkdecl.sd7        0.096532            448
seed7/prg/chkenum.sd7        0.124663           1230
seed7/prg/chkerr.sd7         0.350809           4663
seed7/prg/chkexc.sd7         0.155471           2627
seed7/prg/chkfil.sd7         0.133107           1615
seed7/prg/chkflt.sd7         2.896908          20620
seed7/prg/chkhent.sd7        0.038029             54
seed7/prg/chkhsh.sd7         0.524333           4548
seed7/prg/chkidx.sd7         3.316346          19567
seed7/prg/chkint.sd7         5.782150          38129
seed7/prg/chkjson.sd7        0.192811           1764
seed7/prg/chkovf.sd7         1.243834           8216
seed7/prg/chkprc.sd7         0.725670          10111
seed7/prg/chkscan.sd7        0.092599            714
seed7/prg/chkset.sd7         1.799965          11974
seed7/prg/chkstr.sd7         3.585487          26952
seed7/prg/chktime.sd7        0.259375           2025
seed7/prg/chktoml.sd7        0.202161           1656
seed7/prg/clock.sd7          0.039230             47
seed7/prg/clock2.sd7         0.036973             43
seed7/prg/clock3.sd7         0.042902             95
seed7/prg/cmpfil.sd7         0.040601             84
seed7/prg/comanche.sd7       0.046646            180
seed7/prg/confval.sd7        0.053416            175
seed7/prg/db7.sd7            0.063970            417
seed7/prg/diff7.sd7          0.052173            263
seed7/prg/dirtst.sd7         0.035350             42
seed7/prg/dirx.sd7           0.042471            152
seed7/prg/dnafight.sd7       0.122012           1381
seed7/prg/dragon.sd7         0.040876             73
seed7/prg/echo.sd7           0.039531             39
seed7/prg/eliza.sd7          0.053814            302
seed7/prg/err.sd7            0.045184             96
seed7/prg/fannkuch.sd7       0.042182            131
seed7/prg/fib.sd7            0.036920             47
seed7/prg/find7.sd7          0.042747            133
seed7/prg/findchar.sd7       0.044413            149
seed7/prg/fractree.sd7       0.037226             55
seed7/prg/ftp7.sd7           0.054310            296
seed7/prg/ftpserv.sd7        0.039205             74
seed7/prg/gcd.sd7            0.040844            109
seed7/prg/gkbd.sd7           0.068425            358
seed7/prg/gtksvtst.sd7       0.042069             94
seed7/prg/hal.sd7            0.047936            250
seed7/prg/hamu.sd7           0.066629            573
seed7/prg/hanoi.sd7          0.036114             55
seed7/prg/hd.sd7             0.037883             79
seed7/prg/hello.sd7          0.034709             32
seed7/prg/hilbert.sd7        0.039799            108
seed7/prg/ide7.sd7           0.046678            196
seed7/prg/kbd.sd7            0.035929             49
seed7/prg/klondike.sd7       0.088303            883
seed7/prg/lander.sd7         0.141864           1551
seed7/prg/lst80bas.sd7       0.057283            344
seed7/prg/lst99bas.sd7       0.060531            401
seed7/prg/lstgwbas.sd7       0.074845            577
seed7/prg/mahjong.sd7        0.152611           1943
seed7/prg/make7.sd7          0.042919            121
seed7/prg/mandelbr.sd7       0.049982            237
seed7/prg/mind.sd7           0.061283            443
seed7/prg/mirror.sd7         0.044674            131
seed7/prg/ms.sd7             0.071553            641
seed7/prg/nicoma.sd7         0.043061            135
seed7/prg/pac.sd7            0.074168            726
seed7/prg/pairs.sd7          0.144199           2025
seed7/prg/panic.sd7          0.206525           2634
seed7/prg/percolation.sd7    0.057874            330
seed7/prg/planets.sd7        0.141588           1486
seed7/prg/portfwd7.sd7       0.044239            139
seed7/prg/prime.sd7          0.037933             74
seed7/prg/printpi1.sd7       0.037738             56
seed7/prg/printpi2.sd7       0.037290             54
seed7/prg/printpi3.sd7       0.037641             60
seed7/prg/pv7.sd7            0.059978            337
seed7/prg/queen.sd7          0.043816            149
seed7/prg/rand.sd7           0.041744            121
seed7/prg/raytrace.sd7       0.071991            538
seed7/prg/rever.sd7          0.085466            816
seed7/prg/roman.sd7          0.036309             38
seed7/prg/s7c.sd7            0.662592           9060
seed7/prg/s7check.sd7        0.037655             68
seed7/prg/savehd7.sd7        0.119184           1110
seed7/prg/self.sd7           0.036118             49
seed7/prg/shisen.sd7         0.123472           1423
seed7/prg/sl.sd7             0.099814           1029
seed7/prg/snake.sd7          0.067577            615
seed7/prg/sokoban.sd7        0.084708            891
seed7/prg/spigotpi.sd7       0.040048             64
seed7/prg/sql7.sd7           0.055224            278
seed7/prg/startrek.sd7       0.096158            979
seed7/prg/sudoku7.sd7        0.204012           2657
seed7/prg/sydir7.sd7         0.062093            384
seed7/prg/syntaxhl.sd7       0.049615            177
seed7/prg/tak.sd7            0.037460             59
seed7/prg/tar7.sd7           0.042531            121
seed7/prg/tch.sd7            0.037684             55
seed7/prg/testfont.sd7       0.041734             95
seed7/prg/tet.sd7            0.060694            479
seed7/prg/tetg.sd7           0.064271            501
seed7/prg/toutf8.sd7         0.052077            240
seed7/prg/tst_cli.sd7        0.036760             40
seed7/prg/tst_srv.sd7        0.037036             47
seed7/prg/wator.sd7          0.078693            651
seed7/prg/which.sd7          0.038359             65
seed7/prg/wiz.sd7            0.222179           2833
seed7/prg/wordcnt.sd7        0.037656             54
seed7/prg/wrinum.sd7         0.036567             43
seed7/prg/wumpus.sd7         0.054107            372
seed7/lib/aes.s7i            0.201300           1144
seed7/lib/aes_gcm.s7i        0.065630            392
seed7/lib/ar.s7i             0.127790           1532
seed7/lib/arc4.s7i           0.041221            144
seed7/lib/archive.s7i        0.042215            143
seed7/lib/archive_base.s7i   0.042860            135
seed7/lib/array.s7i          0.076738            610
seed7/lib/asn1.s7i           0.063793            544
seed7/lib/asn1oid.s7i        0.053985            157
seed7/lib/basearray.s7i      0.067469            450
seed7/lib/bigfile.s7i        0.041442            136
seed7/lib/bigint.s7i         0.078862            824
seed7/lib/bigrat.s7i         0.080443            784
seed7/lib/bin16.s7i          0.069942            592
seed7/lib/bin32.s7i          0.063183            490
seed7/lib/bin64.s7i          0.065442            539
seed7/lib/bitdata.s7i        0.127981           1330
seed7/lib/bitmapfont.s7i     0.050519            215
seed7/lib/bitset.s7i         0.067483            593
seed7/lib/bitsetof.s7i       0.063821            431
seed7/lib/blowfish.s7i       0.077643            383
seed7/lib/bmp.s7i            0.102402            924
seed7/lib/boolean.s7i        0.055595            403
seed7/lib/browser.s7i        0.053541            280
seed7/lib/bstring.s7i        0.045999            227
seed7/lib/bytedata.s7i       0.065334            482
seed7/lib/bzip2.s7i          0.088754            887
seed7/lib/cards.s7i          0.106018           1342
seed7/lib/category.s7i       0.047934            209
seed7/lib/cc_conf.s7i        0.124969           1314
seed7/lib/ccittfax.s7i       0.102389           1022
seed7/lib/cgi.s7i            0.039719            109
seed7/lib/cgidialog.s7i      0.101748           1118
seed7/lib/char.s7i           0.053038            356
seed7/lib/charsets.s7i       0.135958           2024
seed7/lib/chartype.s7i       0.047779            121
seed7/lib/cipher.s7i         0.042676            146
seed7/lib/cli_cmds.s7i       0.116527           1360
seed7/lib/clib_file.s7i      0.050578            301
seed7/lib/color.s7i          0.046454            185
seed7/lib/complex.s7i        0.057975            464
seed7/lib/compress.s7i       0.044369            150
seed7/lib/console.s7i        0.045824            188
seed7/lib/cpio.s7i           0.150316           1708
seed7/lib/crc32.s7i          0.054724            193
seed7/lib/cronos16.s7i       0.200208           1173
seed7/lib/cronos27.s7i       0.258713           1464
seed7/lib/csv.s7i            0.048779            201
seed7/lib/db_prop.s7i        0.103618            991
seed7/lib/deflate.s7i        0.086532            740
seed7/lib/des.s7i            0.079321            444
seed7/lib/dialog.s7i         0.057459            311
seed7/lib/dir.s7i            0.044242            163
seed7/lib/draw.s7i           0.088616            854
seed7/lib/duration.s7i       0.100955           1038
seed7/lib/echo.s7i           0.047286            132
seed7/lib/editline.s7i       0.063737            398
seed7/lib/elf.s7i            0.161755           1560
seed7/lib/elliptic.s7i       0.076297            649
seed7/lib/enable_io.s7i      0.052787            312
seed7/lib/encoding.s7i       0.098878            931
seed7/lib/enumeration.s7i    0.047928            236
seed7/lib/environment.s7i    0.044686            175
seed7/lib/exif.s7i           0.044331            152
seed7/lib/external_file.s7i  0.051600            340
seed7/lib/field.s7i          0.053694            268
seed7/lib/file.s7i           0.054797            372
seed7/lib/filebits.s7i       0.037729             46
seed7/lib/filesys.s7i        0.065502            601
seed7/lib/fileutil.s7i       0.043876            144
seed7/lib/fixarray.s7i       0.054012            307
seed7/lib/float.s7i          0.076026            757
seed7/lib/font.s7i           0.043403            196
seed7/lib/font8x8.s7i        0.067605            998
seed7/lib/forloop.s7i        0.059064            449
seed7/lib/ftp.s7i            0.094293            969
seed7/lib/ftpserv.s7i        0.077862            631
seed7/lib/getf.s7i           0.040383            115
seed7/lib/gethttp.s7i        0.036692             41
seed7/lib/gethttps.s7i       0.035796             41
seed7/lib/gif.s7i            0.070269            561
seed7/lib/graph.s7i          0.065710            415
seed7/lib/graph_file.s7i     0.059965            399
seed7/lib/gtkserver.s7i      0.042794            161
seed7/lib/gzip.s7i           0.070036            573
seed7/lib/hash.s7i           0.068192            421
seed7/lib/hashsetof.s7i      0.067787            499
seed7/lib/hmac.s7i           0.044433            152
seed7/lib/html.s7i           0.039139             83
seed7/lib/html_ent.s7i       0.064187            476
seed7/lib/htmldom.s7i        0.054117            286
seed7/lib/http_request.s7i   0.079776            696
seed7/lib/http_srv_resp.s7i  0.060008            380
seed7/lib/https_request.s7i  0.048905            211
seed7/lib/httpserv.s7i       0.057158            345
seed7/lib/huffman.s7i        0.076097            644
seed7/lib/ico.s7i            0.049891            221
seed7/lib/idxarray.s7i       0.057706            232
seed7/lib/image.s7i          0.040789            156
seed7/lib/imagefile.s7i      0.045954            171
seed7/lib/inflate.s7i        0.064332            411
seed7/lib/inifile.s7i        0.040896            129
seed7/lib/integer.s7i        0.067956            663
seed7/lib/iobuffer.s7i       0.050445            289
seed7/lib/jpeg.s7i           0.159251           1761
seed7/lib/json.s7i           0.082903            891
seed7/lib/json_serde.s7i     0.080081            783
seed7/lib/keybd.s7i          0.081225            639
seed7/lib/keydescr.s7i       0.050040            192
seed7/lib/leb128.s7i         0.049907            218
seed7/lib/line.s7i           0.046187            164
seed7/lib/listener.s7i       0.051002            247
seed7/lib/logfile.s7i        0.038807             73
seed7/lib/lower.s7i          0.041606            142
seed7/lib/lzma.s7i           0.097455            934
seed7/lib/lzw.s7i            0.089636            861
seed7/lib/magic.s7i          0.063646            403
seed7/lib/mahjng32.s7i       0.094933           1500
seed7/lib/make.s7i           0.071565            544
seed7/lib/makedata.s7i       0.126335           1428
seed7/lib/math.s7i           0.045410            201
seed7/lib/mixarith.s7i       0.048034            249
seed7/lib/modern27.s7i       0.172025           1099
seed7/lib/more.s7i           0.042566            130
seed7/lib/msgdigest.s7i      0.142758           1222
seed7/lib/multiscr.s7i       0.038527             68
seed7/lib/null_file.s7i      0.051254            345
seed7/lib/osfiles.s7i        0.096469           1085
seed7/lib/pbm.s7i            0.046412            230
seed7/lib/pcx.s7i            0.078138            638
seed7/lib/pem.s7i            0.047109            185
seed7/lib/pgm.s7i            0.047499            238
seed7/lib/pic16.s7i          0.067591           1037
seed7/lib/pic32.s7i          0.126129           2060
seed7/lib/pic_util.s7i       0.044484            144
seed7/lib/pixelimage.s7i     0.053058            320
seed7/lib/pixmap_file.s7i    0.062918            459
seed7/lib/pixmapfont.s7i     0.049041            184
seed7/lib/pkcs1.s7i          0.079193            543
seed7/lib/png.s7i            0.112277           1064
seed7/lib/poll.s7i           0.053260            313
seed7/lib/ppm.s7i            0.048515            240
seed7/lib/process.s7i        0.063936            541
seed7/lib/progs.s7i          0.079250            789
seed7/lib/propertyfile.s7i   0.042704            155
seed7/lib/rational.s7i       0.080795            792
seed7/lib/ref_list.s7i       0.050056            252
seed7/lib/reference.s7i      0.042176            126
seed7/lib/reverse.s7i        0.041783             94
seed7/lib/rpm.s7i            0.301083           3487
seed7/lib/rpmext.s7i         0.060565            318
seed7/lib/scanfile.s7i       0.141499           1779
seed7/lib/scanjson.s7i       0.061548            413
seed7/lib/scanstri.s7i       0.140208           1814
seed7/lib/scantoml.s7i       0.132141           1603
seed7/lib/seed7_05.s7i       0.115004           1072
seed7/lib/set.s7i            0.037994             57
seed7/lib/shell.s7i          0.071249            615
seed7/lib/showtls.s7i        0.087366            678
seed7/lib/signature.s7i      0.044204            131
seed7/lib/smtp.s7i           0.050279            261
seed7/lib/sockbase.s7i       0.051410            217
seed7/lib/socket.s7i         0.053434            326
seed7/lib/sokoban1.s7i       0.082878           1519
seed7/lib/sql_base.s7i       0.098102           1000
seed7/lib/stars.s7i          0.248093           1705
seed7/lib/stdfont10.s7i      0.148202           3347
seed7/lib/stdfont12.s7i      0.173466           3928
seed7/lib/stdfont14.s7i      0.197345           4510
seed7/lib/stdfont16.s7i      0.225024           5092
seed7/lib/stdfont18.s7i      0.255876           5868
seed7/lib/stdfont20.s7i      0.290819           6449
seed7/lib/stdfont24.s7i      0.345660           7421
seed7/lib/stdfont8.s7i       0.131241           2960
seed7/lib/stdfont9.s7i       0.136400           3152
seed7/lib/stdio.s7i          0.045247            192
seed7/lib/strifile.s7i       0.053914            345
seed7/lib/string.s7i         0.079782            779
seed7/lib/stritext.s7i       0.058222            352
seed7/lib/struct.s7i         0.055305            266
seed7/lib/struct_elem.s7i    0.041952            129
seed7/lib/subfile.s7i        0.051730            174
seed7/lib/subrange.s7i       0.044946             78
seed7/lib/syntax.s7i         0.063324            294
seed7/lib/tar.s7i            0.151141           1880
seed7/lib/tar_cmds.s7i       0.086290            752
seed7/lib/tdes.s7i           0.044324            143
seed7/lib/tee.s7i            0.042391            143
seed7/lib/text.s7i           0.042721            135
seed7/lib/tga.s7i            0.082232            676
seed7/lib/tiff.s7i           0.254513           2771
seed7/lib/time.s7i           0.103231           1191
seed7/lib/tls.s7i            0.203196           2230
seed7/lib/unicode.s7i        0.074887            575
seed7/lib/unionfnd.s7i       0.042744            130
seed7/lib/upper.s7i          0.041674            142
seed7/lib/utf16.s7i          0.067429            540
seed7/lib/utf8.s7i           0.047844            234
seed7/lib/vecfont10.s7i      0.161276           1056
seed7/lib/vecfont18.s7i      0.180015           1119
seed7/lib/vector3d.s7i       0.050194            293
seed7/lib/vectorfont.s7i     0.048874            239
seed7/lib/wildcard.s7i       0.042884            140
seed7/lib/window.s7i         0.060770            455
seed7/lib/wrinum.s7i         0.048905            248
seed7/lib/x509cert.s7i       0.121038           1243
seed7/lib/xml_ent.s7i        0.050190             94
seed7/lib/xmldom.s7i         0.051823            303
seed7/lib/xz.s7i             0.061924            442
seed7/lib/zip.s7i            0.240407           2792
seed7/lib/zstd.s7i           0.119444           1263
============================ ================= ================

Statistical Summary — Mode D (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.163662        |
+-----------+-----------------+
| Minimum   | 0.034709        |
+-----------+-----------------+
| Maximum   | 5.782150        |
+-----------+-----------------+

Cross-Mode Comparison
=====================

+------+-----------------+-----------------+-----------------+
| Mode | Average (s)     | Minimum (s)     | Maximum (s)     |
+======+=================+=================+=================+
| A    | 0.035022        | 0.032752        | 0.046646        |
+------+-----------------+-----------------+-----------------+
| B    | 0.039087        | 0.035383        | 0.048990        |
+------+-----------------+-----------------+-----------------+
| C    | 0.088399        | 0.033845        | 2.487883        |
+------+-----------------+-----------------+-----------------+
| D    | 0.163662        | 0.034709        | 5.782150        |
+------+-----------------+-----------------+-----------------+

Phase Timing Summary
====================

Wall-clock times include warm-up work and GC performed by the warm-up helper.

+----------+--------------+--------------+--------------+
| Phase    | Warm-up      | Timed pass   | Mode total   |
+==========+==============+==============+==============+
| Mode A   | 00:00:12.608 | 00:00:59.720 | 00:01:12.329 |
+----------+--------------+--------------+--------------+
| Mode B   | 00:00:15.156 | 00:01:07.240 | 00:01:22.396 |
+----------+--------------+--------------+--------------+
| Mode C   | 00:00:35.965 | 00:02:32.037 | 00:03:08.003 |
+----------+--------------+--------------+--------------+
| Mode D   | 00:01:08.578 | 00:04:41.306 | 00:05:49.885 |
+----------+--------------+--------------+--------------+
| Total    |              |              | 00:11:32.631 |
+----------+--------------+--------------+--------------+


.. ---------------------------------------------------------------------------
