=======================================================
GC-Controlled Benchmark Report: Seed7 Mode — Four Modes
=======================================================

:Running with: seed7-mode 2026-06-22T21:19:10+0000 W26-1
:Seed7 dir   : /Users/roup/my/dvo/seed7-repos/seed7/
:Started at: 17:38:09 local time
:Generated on: 2026-06-22 21:49:21 UTC
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
seed7/prg/addup.sd7          0.033102            190
seed7/prg/bas7.sd7           0.033415          11459
seed7/prg/bifurk.sd7         0.032332             73
seed7/prg/bigfiles.sd7       0.033415            129
seed7/prg/brainf7.sd7        0.040235             86
seed7/prg/calc7.sd7          0.035827            128
seed7/prg/carddemo.sd7       0.033601            190
seed7/prg/castle.sd7         0.033829           3148
seed7/prg/cat.sd7            0.033708             82
seed7/prg/cellauto.sd7       0.033695             85
seed7/prg/celsius.sd7        0.033824             42
seed7/prg/chk_all.sd7        0.033604            843
seed7/prg/chkarr.sd7         0.034019           8367
seed7/prg/chkbig.sd7         0.038410          29026
seed7/prg/chkbin.sd7         0.034500           6469
seed7/prg/chkbitdata.sd7     0.034276           6624
seed7/prg/chkbool.sd7        0.033514           3157
seed7/prg/chkbst.sd7         0.033987            722
seed7/prg/chkchr.sd7         0.033971           2809
seed7/prg/chkcmd.sd7         0.033540           1205
seed7/prg/chkdb.sd7          0.034038           7454
seed7/prg/chkdecl.sd7        0.032624            448
seed7/prg/chkenum.sd7        0.032599           1230
seed7/prg/chkerr.sd7         0.033420           4663
seed7/prg/chkexc.sd7         0.032753           2627
seed7/prg/chkfil.sd7         0.032684           1615
seed7/prg/chkflt.sd7         0.037548          20620
seed7/prg/chkhent.sd7        0.033555             54
seed7/prg/chkhsh.sd7         0.034305           4548
seed7/prg/chkidx.sd7         0.036027          19567
seed7/prg/chkint.sd7         0.040504          38129
seed7/prg/chkjson.sd7        0.033874           1764
seed7/prg/chkovf.sd7         0.034391           8216
seed7/prg/chkprc.sd7         0.034509          10111
seed7/prg/chkscan.sd7        0.033605            714
seed7/prg/chkset.sd7         0.034812          11974
seed7/prg/chkstr.sd7         0.037949          26952
seed7/prg/chktime.sd7        0.033745           2025
seed7/prg/chktoml.sd7        0.033702           1656
seed7/prg/clock.sd7          0.033692             47
seed7/prg/clock2.sd7         0.033554             43
seed7/prg/clock3.sd7         0.033330             95
seed7/prg/cmpfil.sd7         0.034229             84
seed7/prg/comanche.sd7       0.033573            180
seed7/prg/confval.sd7        0.033715            175
seed7/prg/db7.sd7            0.033770            417
seed7/prg/diff7.sd7          0.033857            263
seed7/prg/dirtst.sd7         0.033624             42
seed7/prg/dirx.sd7           0.033611            152
seed7/prg/dnafight.sd7       0.033680           1381
seed7/prg/dragon.sd7         0.033429             73
seed7/prg/echo.sd7           0.033246             39
seed7/prg/eliza.sd7          0.032603            302
seed7/prg/err.sd7            0.032765             96
seed7/prg/fannkuch.sd7       0.032532            131
seed7/prg/fib.sd7            0.033247             47
seed7/prg/find7.sd7          0.032466            133
seed7/prg/findchar.sd7       0.033840            149
seed7/prg/fractree.sd7       0.033514             55
seed7/prg/ftp7.sd7           0.033538            296
seed7/prg/ftpserv.sd7        0.033616             74
seed7/prg/gcd.sd7            0.033576            109
seed7/prg/gkbd.sd7           0.033854            358
seed7/prg/gtksvtst.sd7       0.033804             94
seed7/prg/hal.sd7            0.033698            250
seed7/prg/hamu.sd7           0.033758            573
seed7/prg/hanoi.sd7          0.033572             55
seed7/prg/hd.sd7             0.033549             79
seed7/prg/hello.sd7          0.033708             32
seed7/prg/hilbert.sd7        0.033327            108
seed7/prg/ide7.sd7           0.033724            196
seed7/prg/kbd.sd7            0.033855             49
seed7/prg/klondike.sd7       0.033726            883
seed7/prg/lander.sd7         0.033884           1551
seed7/prg/lst80bas.sd7       0.033296            344
seed7/prg/lst99bas.sd7       0.033635            401
seed7/prg/lstgwbas.sd7       0.033716            577
seed7/prg/mahjong.sd7        0.033612           1943
seed7/prg/make7.sd7          0.033456            121
seed7/prg/mandelbr.sd7       0.033566            237
seed7/prg/mind.sd7           0.033432            443
seed7/prg/mirror.sd7         0.033690            131
seed7/prg/ms.sd7             0.033594            641
seed7/prg/nicoma.sd7         0.032887            135
seed7/prg/pac.sd7            0.032393            726
seed7/prg/pairs.sd7          0.032709           2025
seed7/prg/panic.sd7          0.032654           2634
seed7/prg/percolation.sd7    0.032621            330
seed7/prg/planets.sd7        0.032709           1486
seed7/prg/portfwd7.sd7       0.034007            139
seed7/prg/prime.sd7          0.033544             74
seed7/prg/printpi1.sd7       0.033486             56
seed7/prg/printpi2.sd7       0.033219             54
seed7/prg/printpi3.sd7       0.033628             60
seed7/prg/pv7.sd7            0.033506            337
seed7/prg/queen.sd7          0.033608            149
seed7/prg/rand.sd7           0.033568            121
seed7/prg/raytrace.sd7       0.033578            538
seed7/prg/rever.sd7          0.033510            816
seed7/prg/roman.sd7          0.033611             38
seed7/prg/s7c.sd7            0.034319           9060
seed7/prg/s7check.sd7        0.033529             68
seed7/prg/savehd7.sd7        0.033715           1110
seed7/prg/self.sd7           0.033838             49
seed7/prg/shisen.sd7         0.033839           1423
seed7/prg/sl.sd7             0.033591           1029
seed7/prg/snake.sd7          0.033632            615
seed7/prg/sokoban.sd7        0.033669            891
seed7/prg/spigotpi.sd7       0.033786             64
seed7/prg/sql7.sd7           0.034836            278
seed7/prg/startrek.sd7       0.033512            979
seed7/prg/sudoku7.sd7        0.033285           2657
seed7/prg/sydir7.sd7         0.033671            384
seed7/prg/syntaxhl.sd7       0.033634            177
seed7/prg/tak.sd7            0.033450             59
seed7/prg/tar7.sd7           0.032243            121
seed7/prg/tch.sd7            0.032681             55
seed7/prg/testfont.sd7       0.032754             95
seed7/prg/tet.sd7            0.032375            479
seed7/prg/tetg.sd7           0.032350            501
seed7/prg/toutf8.sd7         0.033673            240
seed7/prg/tst_cli.sd7        0.033754             40
seed7/prg/tst_srv.sd7        0.033444             47
seed7/prg/wator.sd7          0.033591            651
seed7/prg/which.sd7          0.033092             65
seed7/prg/wiz.sd7            0.033844           2833
seed7/prg/wordcnt.sd7        0.033579             54
seed7/prg/wrinum.sd7         0.033488             43
seed7/prg/wumpus.sd7         0.033612            372
seed7/lib/aes.s7i            0.033575           1144
seed7/lib/aes_gcm.s7i        0.033471            392
seed7/lib/ar.s7i             0.033856           1532
seed7/lib/arc4.s7i           0.033556            144
seed7/lib/archive.s7i        0.033745            143
seed7/lib/archive_base.s7i   0.033561            135
seed7/lib/array.s7i          0.033881            610
seed7/lib/asn1.s7i           0.033738            544
seed7/lib/asn1oid.s7i        0.033642            157
seed7/lib/basearray.s7i      0.033868            450
seed7/lib/bigfile.s7i        0.033554            136
seed7/lib/bigint.s7i         0.033580            824
seed7/lib/bigrat.s7i         0.033713            784
seed7/lib/bin16.s7i          0.033663            592
seed7/lib/bin32.s7i          0.033921            490
seed7/lib/bin64.s7i          0.033401            539
seed7/lib/bitdata.s7i        0.032657           1330
seed7/lib/bitmapfont.s7i     0.032610            215
seed7/lib/bitset.s7i         0.033010            593
seed7/lib/bitsetof.s7i       0.032647            431
seed7/lib/blowfish.s7i       0.032658            383
seed7/lib/bmp.s7i            0.033652            924
seed7/lib/boolean.s7i        0.033756            403
seed7/lib/browser.s7i        0.033696            280
seed7/lib/bstring.s7i        0.033892            227
seed7/lib/bytedata.s7i       0.033704            482
seed7/lib/bzip2.s7i          0.033639            887
seed7/lib/cards.s7i          0.033632           1342
seed7/lib/category.s7i       0.033562            209
seed7/lib/cc_conf.s7i        0.033676           1314
seed7/lib/ccittfax.s7i       0.033390           1022
seed7/lib/cgi.s7i            0.033413            109
seed7/lib/cgidialog.s7i      0.033897           1118
seed7/lib/char.s7i           0.033640            356
seed7/lib/charsets.s7i       0.032775           2024
seed7/lib/chartype.s7i       0.032709            121
seed7/lib/cipher.s7i         0.032732            146
seed7/lib/cli_cmds.s7i       0.033016           1360
seed7/lib/clib_file.s7i      0.032925            301
seed7/lib/color.s7i          0.032492            185
seed7/lib/complex.s7i        0.033561            464
seed7/lib/compress.s7i       0.032597            150
seed7/lib/console.s7i        0.032848            188
seed7/lib/cpio.s7i           0.032971           1708
seed7/lib/crc32.s7i          0.033697            193
seed7/lib/cronos16.s7i       0.033821           1173
seed7/lib/cronos27.s7i       0.032546           1464
seed7/lib/csv.s7i            0.032527            201
seed7/lib/db_prop.s7i        0.033090            991
seed7/lib/deflate.s7i        0.032570            740
seed7/lib/des.s7i            0.032437            444
seed7/lib/dialog.s7i         0.033914            311
seed7/lib/dir.s7i            0.033620            163
seed7/lib/draw.s7i           0.033877            854
seed7/lib/duration.s7i       0.033830           1038
seed7/lib/echo.s7i           0.033611            132
seed7/lib/editline.s7i       0.033587            398
seed7/lib/elf.s7i            0.033832           1560
seed7/lib/elliptic.s7i       0.033767            649
seed7/lib/enable_io.s7i      0.033794            312
seed7/lib/encoding.s7i       0.033691            931
seed7/lib/enumeration.s7i    0.033488            236
seed7/lib/environment.s7i    0.034217            175
seed7/lib/exif.s7i           0.033744            152
seed7/lib/external_file.s7i  0.033738            340
seed7/lib/field.s7i          0.033663            268
seed7/lib/file.s7i           0.033323            372
seed7/lib/filebits.s7i       0.033625             46
seed7/lib/filesys.s7i        0.033668            601
seed7/lib/fileutil.s7i       0.033708            144
seed7/lib/fixarray.s7i       0.033804            307
seed7/lib/float.s7i          0.033663            757
seed7/lib/font.s7i           0.033643            196
seed7/lib/font8x8.s7i        0.033771            998
seed7/lib/forloop.s7i        0.033672            449
seed7/lib/ftp.s7i            0.033363            969
seed7/lib/ftpserv.s7i        0.032550            631
seed7/lib/getf.s7i           0.032502            115
seed7/lib/gethttp.s7i        0.032482             41
seed7/lib/gethttps.s7i       0.032396             41
seed7/lib/gif.s7i            0.032503            561
seed7/lib/graph.s7i          0.033847            415
seed7/lib/graph_file.s7i     0.033740            399
seed7/lib/gtkserver.s7i      0.033638            161
seed7/lib/gzip.s7i           0.033453            573
seed7/lib/hash.s7i           0.033683            421
seed7/lib/hashsetof.s7i      0.033656            499
seed7/lib/hmac.s7i           0.034098            152
seed7/lib/html.s7i           0.033386             83
seed7/lib/html_ent.s7i       0.033739            476
seed7/lib/htmldom.s7i        0.033516            286
seed7/lib/http_request.s7i   0.033864            696
seed7/lib/http_srv_resp.s7i  0.033825            380
seed7/lib/https_request.s7i  0.033789            211
seed7/lib/httpserv.s7i       0.033653            345
seed7/lib/huffman.s7i        0.032710            644
seed7/lib/ico.s7i            0.032795            221
seed7/lib/idxarray.s7i       0.036014            232
seed7/lib/image.s7i          0.032898            156
seed7/lib/imagefile.s7i      0.032476            171
seed7/lib/inflate.s7i        0.033669            411
seed7/lib/inifile.s7i        0.033432            129
seed7/lib/integer.s7i        0.032884            663
seed7/lib/iobuffer.s7i       0.032488            289
seed7/lib/jpeg.s7i           0.032588           1761
seed7/lib/json.s7i           0.032831            891
seed7/lib/json_serde.s7i     0.032767            783
seed7/lib/keybd.s7i          0.032668            639
seed7/lib/keydescr.s7i       0.032750            192
seed7/lib/leb128.s7i         0.032444            218
seed7/lib/line.s7i           0.032755            164
seed7/lib/listener.s7i       0.035189            247
seed7/lib/logfile.s7i        0.040371             73
seed7/lib/lower.s7i          0.035277            142
seed7/lib/lzma.s7i           0.034066            934
seed7/lib/lzw.s7i            0.033816            861
seed7/lib/magic.s7i          0.033843            403
seed7/lib/mahjng32.s7i       0.033733           1500
seed7/lib/make.s7i           0.033653            544
seed7/lib/makedata.s7i       0.033553           1428
seed7/lib/math.s7i           0.033708            201
seed7/lib/mixarith.s7i       0.033728            249
seed7/lib/modern27.s7i       0.034076           1099
seed7/lib/more.s7i           0.033708            130
seed7/lib/msgdigest.s7i      0.033591           1222
seed7/lib/multiscr.s7i       0.033728             68
seed7/lib/null_file.s7i      0.033906            345
seed7/lib/osfiles.s7i        0.033566           1085
seed7/lib/pbm.s7i            0.033529            230
seed7/lib/pcx.s7i            0.033634            638
seed7/lib/pem.s7i            0.033477            185
seed7/lib/pgm.s7i            0.033979            238
seed7/lib/pic16.s7i          0.033930           1037
seed7/lib/pic32.s7i          0.033726           2060
seed7/lib/pic_util.s7i       0.033842            144
seed7/lib/pixelimage.s7i     0.033820            320
seed7/lib/pixmap_file.s7i    0.033573            459
seed7/lib/pixmapfont.s7i     0.032748            184
seed7/lib/pkcs1.s7i          0.032606            543
seed7/lib/png.s7i            0.032727           1064
seed7/lib/poll.s7i           0.032498            313
seed7/lib/ppm.s7i            0.032389            240
seed7/lib/process.s7i        0.033878            541
seed7/lib/progs.s7i          0.033940            789
seed7/lib/propertyfile.s7i   0.033273            155
seed7/lib/rational.s7i       0.032960            792
seed7/lib/ref_list.s7i       0.032558            252
seed7/lib/reference.s7i      0.032423            126
seed7/lib/reverse.s7i        0.033057             94
seed7/lib/rpm.s7i            0.033075           3487
seed7/lib/rpmext.s7i         0.033098            318
seed7/lib/scanfile.s7i       0.033260           1779
seed7/lib/scanjson.s7i       0.033841            413
seed7/lib/scanstri.s7i       0.033675           1814
seed7/lib/scantoml.s7i       0.033611           1603
seed7/lib/seed7_05.s7i       0.033463           1072
seed7/lib/set.s7i            0.033921             57
seed7/lib/shell.s7i          0.033736            615
seed7/lib/showtls.s7i        0.033695            678
seed7/lib/signature.s7i      0.033560            131
seed7/lib/smtp.s7i           0.033381            261
seed7/lib/sockbase.s7i       0.033589            217
seed7/lib/socket.s7i         0.033764            326
seed7/lib/sokoban1.s7i       0.033637           1519
seed7/lib/sql_base.s7i       0.034217           1000
seed7/lib/stars.s7i          0.033828           1705
seed7/lib/stdfont10.s7i      0.033783           3347
seed7/lib/stdfont12.s7i      0.041943           3928
seed7/lib/stdfont14.s7i      0.034730           4510
seed7/lib/stdfont16.s7i      0.033046           5092
seed7/lib/stdfont18.s7i      0.032688           5868
seed7/lib/stdfont20.s7i      0.036484           6449
seed7/lib/stdfont24.s7i      0.043732           7421
seed7/lib/stdfont8.s7i       0.038165           2960
seed7/lib/stdfont9.s7i       0.034597           3152
seed7/lib/stdio.s7i          0.034422            192
seed7/lib/strifile.s7i       0.034060            345
seed7/lib/string.s7i         0.034692            779
seed7/lib/stritext.s7i       0.034589            352
seed7/lib/struct.s7i         0.037337            266
seed7/lib/struct_elem.s7i    0.035380            129
seed7/lib/subfile.s7i        0.034908            174
seed7/lib/subrange.s7i       0.034820             78
seed7/lib/syntax.s7i         0.034146            294
seed7/lib/tar.s7i            0.036058           1880
seed7/lib/tar_cmds.s7i       0.035713            752
seed7/lib/tdes.s7i           0.035597            143
seed7/lib/tee.s7i            0.036160            143
seed7/lib/text.s7i           0.035211            135
seed7/lib/tga.s7i            0.036111            676
seed7/lib/tiff.s7i           0.036671           2771
seed7/lib/time.s7i           0.036182           1191
seed7/lib/tls.s7i            0.036146           2230
seed7/lib/unicode.s7i        0.036831            575
seed7/lib/unionfnd.s7i       0.034456            130
seed7/lib/upper.s7i          0.033561            142
seed7/lib/utf16.s7i          0.033660            540
seed7/lib/utf8.s7i           0.033427            234
seed7/lib/vecfont10.s7i      0.033277           1056
seed7/lib/vecfont18.s7i      0.032557           1119
seed7/lib/vector3d.s7i       0.032547            293
seed7/lib/vectorfont.s7i     0.033122            239
seed7/lib/wildcard.s7i       0.032421            140
seed7/lib/window.s7i         0.032328            455
seed7/lib/wrinum.s7i         0.033562            248
seed7/lib/x509cert.s7i       0.033567           1243
seed7/lib/xml_ent.s7i        0.036225             94
seed7/lib/xmldom.s7i         0.036920            303
seed7/lib/xz.s7i             0.035901            442
seed7/lib/zip.s7i            0.036859           2792
seed7/lib/zstd.s7i           0.036596           1263
============================ ================= ================

Statistical Summary — Mode A (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.033817        |
+-----------+-----------------+
| Minimum   | 0.032243        |
+-----------+-----------------+
| Maximum   | 0.043732        |
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
seed7/prg/addup.sd7          0.043379            190
seed7/prg/bas7.sd7           0.049257          11459
seed7/prg/bifurk.sd7         0.043237             73
seed7/prg/bigfiles.sd7       0.044846            129
seed7/prg/brainf7.sd7        0.039548             86
seed7/prg/calc7.sd7          0.038358            128
seed7/prg/carddemo.sd7       0.038727            190
seed7/prg/castle.sd7         0.038419           3148
seed7/prg/cat.sd7            0.037130             82
seed7/prg/cellauto.sd7       0.038655             85
seed7/prg/celsius.sd7        0.035821             42
seed7/prg/chk_all.sd7        0.040186            843
seed7/prg/chkarr.sd7         0.046090           8367
seed7/prg/chkbig.sd7         0.043728          29026
seed7/prg/chkbin.sd7         0.043701           6469
seed7/prg/chkbitdata.sd7     0.043012           6624
seed7/prg/chkbool.sd7        0.037717           3157
seed7/prg/chkbst.sd7         0.037930            722
seed7/prg/chkchr.sd7         0.040882           2809
seed7/prg/chkcmd.sd7         0.037906           1205
seed7/prg/chkdb.sd7          0.042198           7454
seed7/prg/chkdecl.sd7        0.043005            448
seed7/prg/chkenum.sd7        0.040568           1230
seed7/prg/chkerr.sd7         0.040234           4663
seed7/prg/chkexc.sd7         0.037278           2627
seed7/prg/chkfil.sd7         0.038994           1615
seed7/prg/chkflt.sd7         0.045446          20620
seed7/prg/chkhent.sd7        0.037209             54
seed7/prg/chkhsh.sd7         0.040445           4548
seed7/prg/chkidx.sd7         0.041862          19567
seed7/prg/chkint.sd7         0.046930          38129
seed7/prg/chkjson.sd7        0.042673           1764
seed7/prg/chkovf.sd7         0.042949           8216
seed7/prg/chkprc.sd7         0.040945          10111
seed7/prg/chkscan.sd7        0.044382            714
seed7/prg/chkset.sd7         0.046049          11974
seed7/prg/chkstr.sd7         0.043901          26952
seed7/prg/chktime.sd7        0.039990           2025
seed7/prg/chktoml.sd7        0.039517           1656
seed7/prg/clock.sd7          0.036216             47
seed7/prg/clock2.sd7         0.035430             43
seed7/prg/clock3.sd7         0.037836             95
seed7/prg/cmpfil.sd7         0.036013             84
seed7/prg/comanche.sd7       0.038038            180
seed7/prg/confval.sd7        0.040162            175
seed7/prg/db7.sd7            0.039153            417
seed7/prg/diff7.sd7          0.039108            263
seed7/prg/dirtst.sd7         0.035970             42
seed7/prg/dirx.sd7           0.038552            152
seed7/prg/dnafight.sd7       0.038945           1381
seed7/prg/dragon.sd7         0.040893             73
seed7/prg/echo.sd7           0.038095             39
seed7/prg/eliza.sd7          0.041521            302
seed7/prg/err.sd7            0.044154             96
seed7/prg/fannkuch.sd7       0.038617            131
seed7/prg/fib.sd7            0.036033             47
seed7/prg/find7.sd7          0.039526            133
seed7/prg/findchar.sd7       0.039443            149
seed7/prg/fractree.sd7       0.037275             55
seed7/prg/ftp7.sd7           0.039738            296
seed7/prg/ftpserv.sd7        0.038204             74
seed7/prg/gcd.sd7            0.038279            109
seed7/prg/gkbd.sd7           0.039623            358
seed7/prg/gtksvtst.sd7       0.039091             94
seed7/prg/hal.sd7            0.037162            250
seed7/prg/hamu.sd7           0.039884            573
seed7/prg/hanoi.sd7          0.039722             55
seed7/prg/hd.sd7             0.043992             79
seed7/prg/hello.sd7          0.036380             32
seed7/prg/hilbert.sd7        0.037416            108
seed7/prg/ide7.sd7           0.037921            196
seed7/prg/kbd.sd7            0.035628             49
seed7/prg/klondike.sd7       0.039275            883
seed7/prg/lander.sd7         0.039177           1551
seed7/prg/lst80bas.sd7       0.038201            344
seed7/prg/lst99bas.sd7       0.038400            401
seed7/prg/lstgwbas.sd7       0.038056            577
seed7/prg/mahjong.sd7        0.040497           1943
seed7/prg/make7.sd7          0.039306            121
seed7/prg/mandelbr.sd7       0.037962            237
seed7/prg/mind.sd7           0.038562            443
seed7/prg/mirror.sd7         0.037309            131
seed7/prg/ms.sd7             0.037559            641
seed7/prg/nicoma.sd7         0.037635            135
seed7/prg/pac.sd7            0.037104            726
seed7/prg/pairs.sd7          0.038177           2025
seed7/prg/panic.sd7          0.039038           2634
seed7/prg/percolation.sd7    0.038683            330
seed7/prg/planets.sd7        0.039144           1486
seed7/prg/portfwd7.sd7       0.038593            139
seed7/prg/prime.sd7          0.036643             74
seed7/prg/printpi1.sd7       0.036619             56
seed7/prg/printpi2.sd7       0.036008             54
seed7/prg/printpi3.sd7       0.036071             60
seed7/prg/pv7.sd7            0.038501            337
seed7/prg/queen.sd7          0.037807            149
seed7/prg/rand.sd7           0.038335            121
seed7/prg/raytrace.sd7       0.038047            538
seed7/prg/rever.sd7          0.037516            816
seed7/prg/roman.sd7          0.037839             38
seed7/prg/s7c.sd7            0.047356           9060
seed7/prg/s7check.sd7        0.039999             68
seed7/prg/savehd7.sd7        0.039390           1110
seed7/prg/self.sd7           0.036391             49
seed7/prg/shisen.sd7         0.039542           1423
seed7/prg/sl.sd7             0.038575           1029
seed7/prg/snake.sd7          0.038298            615
seed7/prg/sokoban.sd7        0.038822            891
seed7/prg/spigotpi.sd7       0.037954             64
seed7/prg/sql7.sd7           0.038951            278
seed7/prg/startrek.sd7       0.038377            979
seed7/prg/sudoku7.sd7        0.038960           2657
seed7/prg/sydir7.sd7         0.038649            384
seed7/prg/syntaxhl.sd7       0.042214            177
seed7/prg/tak.sd7            0.036820             59
seed7/prg/tar7.sd7           0.039149            121
seed7/prg/tch.sd7            0.036193             55
seed7/prg/testfont.sd7       0.038342             95
seed7/prg/tet.sd7            0.038448            479
seed7/prg/tetg.sd7           0.037988            501
seed7/prg/toutf8.sd7         0.039024            240
seed7/prg/tst_cli.sd7        0.034738             40
seed7/prg/tst_srv.sd7        0.036098             47
seed7/prg/wator.sd7          0.038509            651
seed7/prg/which.sd7          0.037072             65
seed7/prg/wiz.sd7            0.041883           2833
seed7/prg/wordcnt.sd7        0.035988             54
seed7/prg/wrinum.sd7         0.036178             43
seed7/prg/wumpus.sd7         0.038199            372
seed7/lib/aes.s7i            0.042508           1144
seed7/lib/aes_gcm.s7i        0.039192            392
seed7/lib/ar.s7i             0.040264           1532
seed7/lib/arc4.s7i           0.041508            144
seed7/lib/archive.s7i        0.045664            143
seed7/lib/archive_base.s7i   0.041613            135
seed7/lib/array.s7i          0.039267            610
seed7/lib/asn1.s7i           0.038533            544
seed7/lib/asn1oid.s7i        0.043399            157
seed7/lib/basearray.s7i      0.039107            450
seed7/lib/bigfile.s7i        0.039606            136
seed7/lib/bigint.s7i         0.038878            824
seed7/lib/bigrat.s7i         0.039810            784
seed7/lib/bin16.s7i          0.040237            592
seed7/lib/bin32.s7i          0.038899            490
seed7/lib/bin64.s7i          0.039860            539
seed7/lib/bitdata.s7i        0.044287           1330
seed7/lib/bitmapfont.s7i     0.042731            215
seed7/lib/bitset.s7i         0.041403            593
seed7/lib/bitsetof.s7i       0.039698            431
seed7/lib/blowfish.s7i       0.040924            383
seed7/lib/bmp.s7i            0.039857            924
seed7/lib/boolean.s7i        0.040327            403
seed7/lib/browser.s7i        0.038266            280
seed7/lib/bstring.s7i        0.038792            227
seed7/lib/bytedata.s7i       0.039715            482
seed7/lib/bzip2.s7i          0.039549            887
seed7/lib/cards.s7i          0.040445           1342
seed7/lib/category.s7i       0.040603            209
seed7/lib/cc_conf.s7i        0.038456           1314
seed7/lib/ccittfax.s7i       0.039034           1022
seed7/lib/cgi.s7i            0.038072            109
seed7/lib/cgidialog.s7i      0.038782           1118
seed7/lib/char.s7i           0.039407            356
seed7/lib/charsets.s7i       0.039544           2024
seed7/lib/chartype.s7i       0.047030            121
seed7/lib/cipher.s7i         0.044443            146
seed7/lib/cli_cmds.s7i       0.040701           1360
seed7/lib/clib_file.s7i      0.039218            301
seed7/lib/color.s7i          0.038714            185
seed7/lib/complex.s7i        0.038760            464
seed7/lib/compress.s7i       0.040499            150
seed7/lib/console.s7i        0.037993            188
seed7/lib/cpio.s7i           0.038876           1708
seed7/lib/crc32.s7i          0.046179            193
seed7/lib/cronos16.s7i       0.049518           1173
seed7/lib/cronos27.s7i       0.046308           1464
seed7/lib/csv.s7i            0.040479            201
seed7/lib/db_prop.s7i        0.039643            991
seed7/lib/deflate.s7i        0.038943            740
seed7/lib/des.s7i            0.040458            444
seed7/lib/dialog.s7i         0.040080            311
seed7/lib/dir.s7i            0.039037            163
seed7/lib/draw.s7i           0.041536            854
seed7/lib/duration.s7i       0.039322           1038
seed7/lib/echo.s7i           0.039573            132
seed7/lib/editline.s7i       0.039007            398
seed7/lib/elf.s7i            0.040622           1560
seed7/lib/elliptic.s7i       0.039549            649
seed7/lib/enable_io.s7i      0.038547            312
seed7/lib/encoding.s7i       0.044437            931
seed7/lib/enumeration.s7i    0.045197            236
seed7/lib/environment.s7i    0.046044            175
seed7/lib/exif.s7i           0.045771            152
seed7/lib/external_file.s7i  0.047440            340
seed7/lib/field.s7i          0.048703            268
seed7/lib/file.s7i           0.047652            372
seed7/lib/filebits.s7i       0.041928             46
seed7/lib/filesys.s7i        0.042448            601
seed7/lib/fileutil.s7i       0.040445            144
seed7/lib/fixarray.s7i       0.044662            307
seed7/lib/float.s7i          0.043037            757
seed7/lib/font.s7i           0.041768            196
seed7/lib/font8x8.s7i        0.036799            998
seed7/lib/forloop.s7i        0.039692            449
seed7/lib/ftp.s7i            0.040829            969
seed7/lib/ftpserv.s7i        0.044496            631
seed7/lib/getf.s7i           0.043093            115
seed7/lib/gethttp.s7i        0.039266             41
seed7/lib/gethttps.s7i       0.042851             41
seed7/lib/gif.s7i            0.042346            561
seed7/lib/graph.s7i          0.045575            415
seed7/lib/graph_file.s7i     0.041293            399
seed7/lib/gtkserver.s7i      0.038090            161
seed7/lib/gzip.s7i           0.040277            573
seed7/lib/hash.s7i           0.038956            421
seed7/lib/hashsetof.s7i      0.039450            499
seed7/lib/hmac.s7i           0.038868            152
seed7/lib/html.s7i           0.037726             83
seed7/lib/html_ent.s7i       0.039285            476
seed7/lib/htmldom.s7i        0.040038            286
seed7/lib/http_request.s7i   0.039492            696
seed7/lib/http_srv_resp.s7i  0.038469            380
seed7/lib/https_request.s7i  0.038750            211
seed7/lib/httpserv.s7i       0.038769            345
seed7/lib/huffman.s7i        0.039859            644
seed7/lib/ico.s7i            0.039562            221
seed7/lib/idxarray.s7i       0.045886            232
seed7/lib/image.s7i          0.041828            156
seed7/lib/imagefile.s7i      0.039913            171
seed7/lib/inflate.s7i        0.044762            411
seed7/lib/inifile.s7i        0.044651            129
seed7/lib/integer.s7i        0.041415            663
seed7/lib/iobuffer.s7i       0.039654            289
seed7/lib/jpeg.s7i           0.038817           1761
seed7/lib/json.s7i           0.038341            891
seed7/lib/json_serde.s7i     0.041605            783
seed7/lib/keybd.s7i          0.041081            639
seed7/lib/keydescr.s7i       0.041873            192
seed7/lib/leb128.s7i         0.039826            218
seed7/lib/line.s7i           0.038633            164
seed7/lib/listener.s7i       0.038537            247
seed7/lib/logfile.s7i        0.036837             73
seed7/lib/lower.s7i          0.038708            142
seed7/lib/lzma.s7i           0.039322            934
seed7/lib/lzw.s7i            0.038729            861
seed7/lib/magic.s7i          0.040111            403
seed7/lib/mahjng32.s7i       0.037847           1500
seed7/lib/make.s7i           0.037484            544
seed7/lib/makedata.s7i       0.037924           1428
seed7/lib/math.s7i           0.037755            201
seed7/lib/mixarith.s7i       0.042484            249
seed7/lib/modern27.s7i       0.047015           1099
seed7/lib/more.s7i           0.039919            130
seed7/lib/msgdigest.s7i      0.039093           1222
seed7/lib/multiscr.s7i       0.036334             68
seed7/lib/null_file.s7i      0.037058            345
seed7/lib/osfiles.s7i        0.039303           1085
seed7/lib/pbm.s7i            0.038347            230
seed7/lib/pcx.s7i            0.039447            638
seed7/lib/pem.s7i            0.038685            185
seed7/lib/pgm.s7i            0.038834            238
seed7/lib/pic16.s7i          0.036949           1037
seed7/lib/pic32.s7i          0.037831           2060
seed7/lib/pic_util.s7i       0.038780            144
seed7/lib/pixelimage.s7i     0.038356            320
seed7/lib/pixmap_file.s7i    0.038878            459
seed7/lib/pixmapfont.s7i     0.040679            184
seed7/lib/pkcs1.s7i          0.043968            543
seed7/lib/png.s7i            0.039250           1064
seed7/lib/poll.s7i           0.038458            313
seed7/lib/ppm.s7i            0.039748            240
seed7/lib/process.s7i        0.038239            541
seed7/lib/progs.s7i          0.038373            789
seed7/lib/propertyfile.s7i   0.038907            155
seed7/lib/rational.s7i       0.038849            792
seed7/lib/ref_list.s7i       0.038260            252
seed7/lib/reference.s7i      0.039038            126
seed7/lib/reverse.s7i        0.037512             94
seed7/lib/rpm.s7i            0.039236           3487
seed7/lib/rpmext.s7i         0.038812            318
seed7/lib/scanfile.s7i       0.038270           1779
seed7/lib/scanjson.s7i       0.044955            413
seed7/lib/scanstri.s7i       0.039064           1814
seed7/lib/scantoml.s7i       0.038227           1603
seed7/lib/seed7_05.s7i       0.039628           1072
seed7/lib/set.s7i            0.037939             57
seed7/lib/shell.s7i          0.039613            615
seed7/lib/showtls.s7i        0.038892            678
seed7/lib/signature.s7i      0.038940            131
seed7/lib/smtp.s7i           0.038963            261
seed7/lib/sockbase.s7i       0.038933            217
seed7/lib/socket.s7i         0.039060            326
seed7/lib/sokoban1.s7i       0.036964           1519
seed7/lib/sql_base.s7i       0.040097           1000
seed7/lib/stars.s7i          0.044284           1705
seed7/lib/stdfont10.s7i      0.043523           3347
seed7/lib/stdfont12.s7i      0.045692           3928
seed7/lib/stdfont14.s7i      0.038703           4510
seed7/lib/stdfont16.s7i      0.037385           5092
seed7/lib/stdfont18.s7i      0.038915           5868
seed7/lib/stdfont20.s7i      0.038463           6449
seed7/lib/stdfont24.s7i      0.038773           7421
seed7/lib/stdfont8.s7i       0.037759           2960
seed7/lib/stdfont9.s7i       0.037841           3152
seed7/lib/stdio.s7i          0.038395            192
seed7/lib/strifile.s7i       0.038408            345
seed7/lib/string.s7i         0.037439            779
seed7/lib/stritext.s7i       0.037837            352
seed7/lib/struct.s7i         0.038774            266
seed7/lib/struct_elem.s7i    0.037211            129
seed7/lib/subfile.s7i        0.038349            174
seed7/lib/subrange.s7i       0.037759             78
seed7/lib/syntax.s7i         0.037793            294
seed7/lib/tar.s7i            0.038054           1880
seed7/lib/tar_cmds.s7i       0.037870            752
seed7/lib/tdes.s7i           0.037549            143
seed7/lib/tee.s7i            0.040147            143
seed7/lib/text.s7i           0.043515            135
seed7/lib/tga.s7i            0.045094            676
seed7/lib/tiff.s7i           0.041370           2771
seed7/lib/time.s7i           0.039264           1191
seed7/lib/tls.s7i            0.039353           2230
seed7/lib/unicode.s7i        0.041671            575
seed7/lib/unionfnd.s7i       0.039112            130
seed7/lib/upper.s7i          0.038197            142
seed7/lib/utf16.s7i          0.038404            540
seed7/lib/utf8.s7i           0.039780            234
seed7/lib/vecfont10.s7i      0.039079           1056
seed7/lib/vecfont18.s7i      0.042175           1119
seed7/lib/vector3d.s7i       0.039130            293
seed7/lib/vectorfont.s7i     0.038670            239
seed7/lib/wildcard.s7i       0.038481            140
seed7/lib/window.s7i         0.037671            455
seed7/lib/wrinum.s7i         0.038088            248
seed7/lib/x509cert.s7i       0.038158           1243
seed7/lib/xml_ent.s7i        0.037476             94
seed7/lib/xmldom.s7i         0.039455            303
seed7/lib/xz.s7i             0.038553            442
seed7/lib/zip.s7i            0.038819           2792
seed7/lib/zstd.s7i           0.039007           1263
============================ ================= ================

Statistical Summary — Mode B (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.039833        |
+-----------+-----------------+
| Minimum   | 0.034738        |
+-----------+-----------------+
| Maximum   | 0.049518        |
+-----------+-----------------+

Mode C — Mode Activation + Full-Buffer Fontification (font-lock-ensure)
-----------------------------------------------------------------------

Full-buffer fontification stress test.

File Load Times — Mode C (mean, GC-free)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

============================ ================= ================
File Name                    Mean Time (s)     Line count
============================ ================= ================
seed7/prg/addup.sd7          0.038534            190
seed7/prg/bas7.sd7           0.321400          11459
seed7/prg/bifurk.sd7         0.036376             73
seed7/prg/bigfiles.sd7       0.037632            129
seed7/prg/brainf7.sd7        0.036672             86
seed7/prg/calc7.sd7          0.037844            128
seed7/prg/carddemo.sd7       0.039306            190
seed7/prg/castle.sd7         0.107945           3148
seed7/prg/cat.sd7            0.036517             82
seed7/prg/cellauto.sd7       0.036271             85
seed7/prg/celsius.sd7        0.035411             42
seed7/prg/chk_all.sd7        0.057653            843
seed7/prg/chkarr.sd7         0.350052           8367
seed7/prg/chkbig.sd7         2.036307          29026
seed7/prg/chkbin.sd7         0.505274           6469
seed7/prg/chkbitdata.sd7     0.612199           6624
seed7/prg/chkbool.sd7        0.114430           3157
seed7/prg/chkbst.sd7         0.062430            722
seed7/prg/chkchr.sd7         0.213886           2809
seed7/prg/chkcmd.sd7         0.066127           1205
seed7/prg/chkdb.sd7          0.345416           7454
seed7/prg/chkdecl.sd7        0.055895            448
seed7/prg/chkenum.sd7        0.065085           1230
seed7/prg/chkerr.sd7         0.192778           4663
seed7/prg/chkexc.sd7         0.081751           2627
seed7/prg/chkfil.sd7         0.074360           1615
seed7/prg/chkflt.sd7         1.322483          20620
seed7/prg/chkhent.sd7        0.036692             54
seed7/prg/chkhsh.sd7         0.242308           4548
seed7/prg/chkidx.sd7         1.307364          19567
seed7/prg/chkint.sd7         2.498127          38129
seed7/prg/chkjson.sd7        0.101076           1764
seed7/prg/chkovf.sd7         0.552403           8216
seed7/prg/chkprc.sd7         0.317584          10111
seed7/prg/chkscan.sd7        0.054923            714
seed7/prg/chkset.sd7         0.647573          11974
seed7/prg/chkstr.sd7         1.373200          26952
seed7/prg/chktime.sd7        0.127154           2025
seed7/prg/chktoml.sd7        0.106300           1656
seed7/prg/clock.sd7          0.035579             47
seed7/prg/clock2.sd7         0.034991             43
seed7/prg/clock3.sd7         0.036699             95
seed7/prg/cmpfil.sd7         0.036178             84
seed7/prg/comanche.sd7       0.039904            180
seed7/prg/confval.sd7        0.042618            175
seed7/prg/db7.sd7            0.046347            417
seed7/prg/diff7.sd7          0.040871            263
seed7/prg/dirtst.sd7         0.035784             42
seed7/prg/dirx.sd7           0.036760            152
seed7/prg/dnafight.sd7       0.064470           1381
seed7/prg/dragon.sd7         0.036531             73
seed7/prg/echo.sd7           0.034840             39
seed7/prg/eliza.sd7          0.040961            302
seed7/prg/err.sd7            0.038654             96
seed7/prg/fannkuch.sd7       0.037329            131
seed7/prg/fib.sd7            0.035185             47
seed7/prg/find7.sd7          0.037551            133
seed7/prg/findchar.sd7       0.038876            149
seed7/prg/fractree.sd7       0.035694             55
seed7/prg/ftp7.sd7           0.041853            296
seed7/prg/ftpserv.sd7        0.036460             74
seed7/prg/gcd.sd7            0.036422            109
seed7/prg/gkbd.sd7           0.045635            358
seed7/prg/gtksvtst.sd7       0.036880             94
seed7/prg/hal.sd7            0.039577            250
seed7/prg/hamu.sd7           0.048453            573
seed7/prg/hanoi.sd7          0.035706             55
seed7/prg/hd.sd7             0.036667             79
seed7/prg/hello.sd7          0.035047             32
seed7/prg/hilbert.sd7        0.036434            108
seed7/prg/ide7.sd7           0.039727            196
seed7/prg/kbd.sd7            0.035386             49
seed7/prg/klondike.sd7       0.054177            883
seed7/prg/lander.sd7         0.068941           1551
seed7/prg/lst80bas.sd7       0.042100            344
seed7/prg/lst99bas.sd7       0.043623            401
seed7/prg/lstgwbas.sd7       0.048899            577
seed7/prg/mahjong.sd7        0.080226           1943
seed7/prg/make7.sd7          0.037916            121
seed7/prg/mandelbr.sd7       0.040165            237
seed7/prg/mind.sd7           0.044622            443
seed7/prg/mirror.sd7         0.038832            131
seed7/prg/ms.sd7             0.048402            641
seed7/prg/nicoma.sd7         0.038258            135
seed7/prg/pac.sd7            0.049110            726
seed7/prg/pairs.sd7          0.082104           2025
seed7/prg/panic.sd7          0.096100           2634
seed7/prg/percolation.sd7    0.043160            330
seed7/prg/planets.sd7        0.075734           1486
seed7/prg/portfwd7.sd7       0.038530            139
seed7/prg/prime.sd7          0.035878             74
seed7/prg/printpi1.sd7       0.036283             56
seed7/prg/printpi2.sd7       0.035519             54
seed7/prg/printpi3.sd7       0.035249             60
seed7/prg/pv7.sd7            0.042808            337
seed7/prg/queen.sd7          0.036873            149
seed7/prg/rand.sd7           0.036306            121
seed7/prg/raytrace.sd7       0.046477            538
seed7/prg/rever.sd7          0.053455            816
seed7/prg/roman.sd7          0.035213             38
seed7/prg/s7c.sd7            0.278561           9060
seed7/prg/s7check.sd7        0.036319             68
seed7/prg/savehd7.sd7        0.066348           1110
seed7/prg/self.sd7           0.035936             49
seed7/prg/shisen.sd7         0.071824           1423
seed7/prg/sl.sd7             0.058866           1029
seed7/prg/snake.sd7          0.046560            615
seed7/prg/sokoban.sd7        0.054386            891
seed7/prg/spigotpi.sd7       0.035694             64
seed7/prg/sql7.sd7           0.041194            278
seed7/prg/startrek.sd7       0.058495            979
seed7/prg/sudoku7.sd7        0.097496           2657
seed7/prg/sydir7.sd7         0.044546            384
seed7/prg/syntaxhl.sd7       0.041394            177
seed7/prg/tak.sd7            0.041118             59
seed7/prg/tar7.sd7           0.039730            121
seed7/prg/tch.sd7            0.037530             55
seed7/prg/testfont.sd7       0.037758             95
seed7/prg/tet.sd7            0.044429            479
seed7/prg/tetg.sd7           0.045459            501
seed7/prg/toutf8.sd7         0.042291            240
seed7/prg/tst_cli.sd7        0.035462             40
seed7/prg/tst_srv.sd7        0.035398             47
seed7/prg/wator.sd7          0.051795            651
seed7/prg/which.sd7          0.036193             65
seed7/prg/wiz.sd7            0.104220           2833
seed7/prg/wordcnt.sd7        0.035963             54
seed7/prg/wrinum.sd7         0.035123             43
seed7/prg/wumpus.sd7         0.042035            372
seed7/lib/aes.s7i            0.109240           1144
seed7/lib/aes_gcm.s7i        0.047489            392
seed7/lib/ar.s7i             0.072229           1532
seed7/lib/arc4.s7i           0.037166            144
seed7/lib/archive.s7i        0.037021            143
seed7/lib/archive_base.s7i   0.037276            135
seed7/lib/array.s7i          0.051920            610
seed7/lib/asn1.s7i           0.047631            544
seed7/lib/asn1oid.s7i        0.042268            157
seed7/lib/basearray.s7i      0.048715            450
seed7/lib/bigfile.s7i        0.038186            136
seed7/lib/bigint.s7i         0.054988            824
seed7/lib/bigrat.s7i         0.053840            784
seed7/lib/bin16.s7i          0.050324            592
seed7/lib/bin32.s7i          0.047560            490
seed7/lib/bin64.s7i          0.049535            539
seed7/lib/bitdata.s7i        0.076581           1330
seed7/lib/bitmapfont.s7i     0.040317            215
seed7/lib/bitset.s7i         0.048496            593
seed7/lib/bitsetof.s7i       0.047401            431
seed7/lib/blowfish.s7i       0.055845            383
seed7/lib/bmp.s7i            0.060050            924
seed7/lib/boolean.s7i        0.044835            403
seed7/lib/browser.s7i        0.043713            280
seed7/lib/bstring.s7i        0.044108            227
seed7/lib/bytedata.s7i       0.060617            482
seed7/lib/bzip2.s7i          0.079641            887
seed7/lib/cards.s7i          0.073884           1342
seed7/lib/category.s7i       0.044154            209
seed7/lib/cc_conf.s7i        0.083005           1314
seed7/lib/ccittfax.s7i       0.076095           1022
seed7/lib/cgi.s7i            0.041744            109
seed7/lib/cgidialog.s7i      0.064668           1118
seed7/lib/char.s7i           0.046618            356
seed7/lib/charsets.s7i       0.085413           2024
seed7/lib/chartype.s7i       0.043255            121
seed7/lib/cipher.s7i         0.041974            146
seed7/lib/cli_cmds.s7i       0.072394           1360
seed7/lib/clib_file.s7i      0.046524            301
seed7/lib/color.s7i          0.043743            185
seed7/lib/complex.s7i        0.048629            464
seed7/lib/compress.s7i       0.042416            150
seed7/lib/console.s7i        0.042951            188
seed7/lib/cpio.s7i           0.086320           1708
seed7/lib/crc32.s7i          0.047370            193
seed7/lib/cronos16.s7i       0.098600           1173
seed7/lib/cronos27.s7i       0.122728           1464
seed7/lib/csv.s7i            0.044183            201
seed7/lib/db_prop.s7i        0.068698            991
seed7/lib/deflate.s7i        0.060340            740
seed7/lib/des.s7i            0.057901            444
seed7/lib/dialog.s7i         0.045054            311
seed7/lib/dir.s7i            0.039332            163
seed7/lib/draw.s7i           0.058408            854
seed7/lib/duration.s7i       0.064520           1038
seed7/lib/echo.s7i           0.039511            132
seed7/lib/editline.s7i       0.047663            398
seed7/lib/elf.s7i            0.087895           1560
seed7/lib/elliptic.s7i       0.054613            649
seed7/lib/enable_io.s7i      0.046449            312
seed7/lib/encoding.s7i       0.062023            931
seed7/lib/enumeration.s7i    0.043815            236
seed7/lib/environment.s7i    0.040893            175
seed7/lib/exif.s7i           0.040947            152
seed7/lib/external_file.s7i  0.045004            340
seed7/lib/field.s7i          0.042948            268
seed7/lib/file.s7i           0.044930            372
seed7/lib/filebits.s7i       0.037935             46
seed7/lib/filesys.s7i        0.050157            601
seed7/lib/fileutil.s7i       0.039758            144
seed7/lib/fixarray.s7i       0.044277            307
seed7/lib/float.s7i          0.056565            757
seed7/lib/font.s7i           0.041874            196
seed7/lib/font8x8.s7i        0.051408            998
seed7/lib/forloop.s7i        0.048362            449
seed7/lib/ftp.s7i            0.060622            969
seed7/lib/ftpserv.s7i        0.052880            631
seed7/lib/getf.s7i           0.038988            115
seed7/lib/gethttp.s7i        0.037483             41
seed7/lib/gethttps.s7i       0.036282             41
seed7/lib/gif.s7i            0.052125            561
seed7/lib/graph.s7i          0.051969            415
seed7/lib/graph_file.s7i     0.045500            399
seed7/lib/gtkserver.s7i      0.040269            161
seed7/lib/gzip.s7i           0.049151            573
seed7/lib/hash.s7i           0.050450            421
seed7/lib/hashsetof.s7i      0.050364            499
seed7/lib/hmac.s7i           0.040758            152
seed7/lib/html.s7i           0.038318             83
seed7/lib/html_ent.s7i       0.050716            476
seed7/lib/htmldom.s7i        0.048404            286
seed7/lib/http_request.s7i   0.054181            696
seed7/lib/http_srv_resp.s7i  0.047792            380
seed7/lib/https_request.s7i  0.041499            211
seed7/lib/httpserv.s7i       0.045513            345
seed7/lib/huffman.s7i        0.056168            644
seed7/lib/ico.s7i            0.044032            221
seed7/lib/idxarray.s7i       0.043681            232
seed7/lib/image.s7i          0.038859            156
seed7/lib/imagefile.s7i      0.040003            171
seed7/lib/inflate.s7i        0.047417            411
seed7/lib/inifile.s7i        0.038684            129
seed7/lib/integer.s7i        0.054522            663
seed7/lib/iobuffer.s7i       0.042321            289
seed7/lib/jpeg.s7i           0.087398           1761
seed7/lib/json.s7i           0.056715            891
seed7/lib/json_serde.s7i     0.055634            783
seed7/lib/keybd.s7i          0.057924            639
seed7/lib/keydescr.s7i       0.042680            192
seed7/lib/leb128.s7i         0.044121            218
seed7/lib/line.s7i           0.041640            164
seed7/lib/listener.s7i       0.047245            247
seed7/lib/logfile.s7i        0.038438             73
seed7/lib/lower.s7i          0.040638            142
seed7/lib/lzma.s7i           0.062983            934
seed7/lib/lzw.s7i            0.061156            861
seed7/lib/magic.s7i          0.052198            403
seed7/lib/mahjng32.s7i       0.067660           1500
seed7/lib/make.s7i           0.050758            544
seed7/lib/makedata.s7i       0.072188           1428
seed7/lib/math.s7i           0.043544            201
seed7/lib/mixarith.s7i       0.045264            249
seed7/lib/modern27.s7i       0.087385           1099
seed7/lib/more.s7i           0.039048            130
seed7/lib/msgdigest.s7i      0.078576           1222
seed7/lib/multiscr.s7i       0.035761             68
seed7/lib/null_file.s7i      0.041563            345
seed7/lib/osfiles.s7i        0.063468           1085
seed7/lib/pbm.s7i            0.039096            230
seed7/lib/pcx.s7i            0.051690            638
seed7/lib/pem.s7i            0.039507            185
seed7/lib/pgm.s7i            0.040617            238
seed7/lib/pic16.s7i          0.048998           1037
seed7/lib/pic32.s7i          0.079354           2060
seed7/lib/pic_util.s7i       0.038877            144
seed7/lib/pixelimage.s7i     0.042159            320
seed7/lib/pixmap_file.s7i    0.045842            459
seed7/lib/pixmapfont.s7i     0.039705            184
seed7/lib/pkcs1.s7i          0.059170            543
seed7/lib/png.s7i            0.063890           1064
seed7/lib/poll.s7i           0.043686            313
seed7/lib/ppm.s7i            0.040730            240
seed7/lib/process.s7i        0.049116            541
seed7/lib/progs.s7i          0.057161            789
seed7/lib/propertyfile.s7i   0.038983            155
seed7/lib/rational.s7i       0.053293            792
seed7/lib/ref_list.s7i       0.041422            252
seed7/lib/reference.s7i      0.036977            126
seed7/lib/reverse.s7i        0.035685             94
seed7/lib/rpm.s7i            0.140808           3487
seed7/lib/rpmext.s7i         0.042252            318
seed7/lib/scanfile.s7i       0.082238           1779
seed7/lib/scanjson.s7i       0.047012            413
seed7/lib/scanstri.s7i       0.080280           1814
seed7/lib/scantoml.s7i       0.071623           1603
seed7/lib/seed7_05.s7i       0.066451           1072
seed7/lib/set.s7i            0.036141             57
seed7/lib/shell.s7i          0.053703            615
seed7/lib/showtls.s7i        0.055236            678
seed7/lib/signature.s7i      0.038670            131
seed7/lib/smtp.s7i           0.041365            261
seed7/lib/sockbase.s7i       0.042317            217
seed7/lib/socket.s7i         0.042959            326
seed7/lib/sokoban1.s7i       0.053681           1519
seed7/lib/sql_base.s7i       0.064057           1000
seed7/lib/stars.s7i          0.135968           1705
seed7/lib/stdfont10.s7i      0.079384           3347
seed7/lib/stdfont12.s7i      0.091294           3928
seed7/lib/stdfont14.s7i      0.101206           4510
seed7/lib/stdfont16.s7i      0.112433           5092
seed7/lib/stdfont18.s7i      0.129990           5868
seed7/lib/stdfont20.s7i      0.146723           6449
seed7/lib/stdfont24.s7i      0.176765           7421
seed7/lib/stdfont8.s7i       0.072383           2960
seed7/lib/stdfont9.s7i       0.078711           3152
seed7/lib/stdio.s7i          0.039174            192
seed7/lib/strifile.s7i       0.042087            345
seed7/lib/string.s7i         0.060540            779
seed7/lib/stritext.s7i       0.043547            352
seed7/lib/struct.s7i         0.045535            266
seed7/lib/struct_elem.s7i    0.038368            129
seed7/lib/subfile.s7i        0.039070            174
seed7/lib/subrange.s7i       0.036743             78
seed7/lib/syntax.s7i         0.045639            294
seed7/lib/tar.s7i            0.082204           1880
seed7/lib/tar_cmds.s7i       0.056069            752
seed7/lib/tdes.s7i           0.038565            143
seed7/lib/tee.s7i            0.037361            143
seed7/lib/text.s7i           0.037742            135
seed7/lib/tga.s7i            0.053732            676
seed7/lib/tiff.s7i           0.120223           2771
seed7/lib/time.s7i           0.061462           1191
seed7/lib/tls.s7i            0.101733           2230
seed7/lib/unicode.s7i        0.051035            575
seed7/lib/unionfnd.s7i       0.036760            130
seed7/lib/upper.s7i          0.036837            142
seed7/lib/utf16.s7i          0.048156            540
seed7/lib/utf8.s7i           0.040233            234
seed7/lib/vecfont10.s7i      0.077775           1056
seed7/lib/vecfont18.s7i      0.087179           1119
seed7/lib/vector3d.s7i       0.041015            293
seed7/lib/vectorfont.s7i     0.040399            239
seed7/lib/wildcard.s7i       0.037980            140
seed7/lib/window.s7i         0.044867            455
seed7/lib/wrinum.s7i         0.041034            248
seed7/lib/x509cert.s7i       0.071550           1243
seed7/lib/xml_ent.s7i        0.037783             94
seed7/lib/xmldom.s7i         0.041123            303
seed7/lib/xz.s7i             0.045919            442
seed7/lib/zip.s7i            0.118527           2792
seed7/lib/zstd.s7i           0.069525           1263
============================ ================= ================

Statistical Summary — Mode C (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.089197        |
+-----------+-----------------+
| Minimum   | 0.034840        |
+-----------+-----------------+
| Maximum   | 2.498127        |
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
seed7/prg/addup.sd7          0.045211            190
seed7/prg/bas7.sd7           0.760673          11459
seed7/prg/bifurk.sd7         0.037084             73
seed7/prg/bigfiles.sd7       0.040871            129
seed7/prg/brainf7.sd7        0.038353             86
seed7/prg/calc7.sd7          0.040483            128
seed7/prg/carddemo.sd7       0.044990            190
seed7/prg/castle.sd7         0.212242           3148
seed7/prg/cat.sd7            0.037581             82
seed7/prg/cellauto.sd7       0.040965             85
seed7/prg/celsius.sd7        0.039301             42
seed7/prg/chk_all.sd7        0.083063            843
seed7/prg/chkarr.sd7         0.858365           8367
seed7/prg/chkbig.sd7         4.088659          29026
seed7/prg/chkbin.sd7         1.022469           6469
seed7/prg/chkbitdata.sd7     1.236734           6624
seed7/prg/chkbool.sd7        0.227047           3157
seed7/prg/chkbst.sd7         0.101102            722
seed7/prg/chkchr.sd7         0.476161           2809
seed7/prg/chkcmd.sd7         0.107868           1205
seed7/prg/chkdb.sd7          0.750186           7454
seed7/prg/chkdecl.sd7        0.094330            448
seed7/prg/chkenum.sd7        0.120707           1230
seed7/prg/chkerr.sd7         0.335255           4663
seed7/prg/chkexc.sd7         0.145093           2627
seed7/prg/chkfil.sd7         0.127042           1615
seed7/prg/chkflt.sd7         2.802747          20620
seed7/prg/chkhent.sd7        0.037146             54
seed7/prg/chkhsh.sd7         0.497350           4548
seed7/prg/chkidx.sd7         3.149360          19567
seed7/prg/chkint.sd7         5.509252          38129
seed7/prg/chkjson.sd7        0.185532           1764
seed7/prg/chkovf.sd7         1.197408           8216
seed7/prg/chkprc.sd7         0.691198          10111
seed7/prg/chkscan.sd7        0.089385            714
seed7/prg/chkset.sd7         1.730447          11974
seed7/prg/chkstr.sd7         3.394607          26952
seed7/prg/chktime.sd7        0.243987           2025
seed7/prg/chktoml.sd7        0.192599           1656
seed7/prg/clock.sd7          0.035828             47
seed7/prg/clock2.sd7         0.035280             43
seed7/prg/clock3.sd7         0.040192             95
seed7/prg/cmpfil.sd7         0.038462             84
seed7/prg/comanche.sd7       0.047468            180
seed7/prg/confval.sd7        0.050625            175
seed7/prg/db7.sd7            0.062762            417
seed7/prg/diff7.sd7          0.052708            263
seed7/prg/dirtst.sd7         0.036670             42
seed7/prg/dirx.sd7           0.043485            152
seed7/prg/dnafight.sd7       0.117390           1381
seed7/prg/dragon.sd7         0.038637             73
seed7/prg/echo.sd7           0.036196             39
seed7/prg/eliza.sd7          0.051791            302
seed7/prg/err.sd7            0.044630             96
seed7/prg/fannkuch.sd7       0.041837            131
seed7/prg/fib.sd7            0.037014             47
seed7/prg/find7.sd7          0.042220            133
seed7/prg/findchar.sd7       0.043602            149
seed7/prg/fractree.sd7       0.037309             55
seed7/prg/ftp7.sd7           0.052069            296
seed7/prg/ftpserv.sd7        0.037284             74
seed7/prg/gcd.sd7            0.039062            109
seed7/prg/gkbd.sd7           0.060244            358
seed7/prg/gtksvtst.sd7       0.038448             94
seed7/prg/hal.sd7            0.046905            250
seed7/prg/hamu.sd7           0.066164            573
seed7/prg/hanoi.sd7          0.036656             55
seed7/prg/hd.sd7             0.038090             79
seed7/prg/hello.sd7          0.036123             32
seed7/prg/hilbert.sd7        0.041407            108
seed7/prg/ide7.sd7           0.047012            196
seed7/prg/kbd.sd7            0.035743             49
seed7/prg/klondike.sd7       0.085259            883
seed7/prg/lander.sd7         0.129086           1551
seed7/prg/lst80bas.sd7       0.053409            344
seed7/prg/lst99bas.sd7       0.057599            401
seed7/prg/lstgwbas.sd7       0.075773            577
seed7/prg/mahjong.sd7        0.146471           1943
seed7/prg/make7.sd7          0.041127            121
seed7/prg/mandelbr.sd7       0.047602            237
seed7/prg/mind.sd7           0.058172            443
seed7/prg/mirror.sd7         0.043366            131
seed7/prg/ms.sd7             0.070200            641
seed7/prg/nicoma.sd7         0.042029            135
seed7/prg/pac.sd7            0.072331            726
seed7/prg/pairs.sd7          0.138229           2025
seed7/prg/panic.sd7          0.191793           2634
seed7/prg/percolation.sd7    0.055263            330
seed7/prg/planets.sd7        0.135702           1486
seed7/prg/portfwd7.sd7       0.044810            139
seed7/prg/prime.sd7          0.039450             74
seed7/prg/printpi1.sd7       0.036430             56
seed7/prg/printpi2.sd7       0.035670             54
seed7/prg/printpi3.sd7       0.036695             60
seed7/prg/pv7.sd7            0.055745            337
seed7/prg/queen.sd7          0.041138            149
seed7/prg/rand.sd7           0.041350            121
seed7/prg/raytrace.sd7       0.069125            538
seed7/prg/rever.sd7          0.084636            816
seed7/prg/roman.sd7          0.040470             38
seed7/prg/s7c.sd7            0.625962           9060
seed7/prg/s7check.sd7        0.038111             68
seed7/prg/savehd7.sd7        0.107513           1110
seed7/prg/self.sd7           0.036059             49
seed7/prg/shisen.sd7         0.118466           1423
seed7/prg/sl.sd7             0.095730           1029
seed7/prg/snake.sd7          0.065598            615
seed7/prg/sokoban.sd7        0.081974            891
seed7/prg/spigotpi.sd7       0.037411             64
seed7/prg/sql7.sd7           0.051270            278
seed7/prg/startrek.sd7       0.093315            979
seed7/prg/sudoku7.sd7        0.193847           2657
seed7/prg/sydir7.sd7         0.059841            384
seed7/prg/syntaxhl.sd7       0.048959            177
seed7/prg/tak.sd7            0.037565             59
seed7/prg/tar7.sd7           0.041736            121
seed7/prg/tch.sd7            0.036030             55
seed7/prg/testfont.sd7       0.040137             95
seed7/prg/tet.sd7            0.057913            479
seed7/prg/tetg.sd7           0.060900            501
seed7/prg/toutf8.sd7         0.050838            240
seed7/prg/tst_cli.sd7        0.036489             40
seed7/prg/tst_srv.sd7        0.036395             47
seed7/prg/wator.sd7          0.076748            651
seed7/prg/which.sd7          0.036113             65
seed7/prg/wiz.sd7            0.205170           2833
seed7/prg/wordcnt.sd7        0.036489             54
seed7/prg/wrinum.sd7         0.035275             43
seed7/prg/wumpus.sd7         0.052668            372
seed7/lib/aes.s7i            0.196073           1144
seed7/lib/aes_gcm.s7i        0.060492            392
seed7/lib/ar.s7i             0.121350           1532
seed7/lib/arc4.s7i           0.042094            144
seed7/lib/archive.s7i        0.041568            143
seed7/lib/archive_base.s7i   0.042477            135
seed7/lib/array.s7i          0.076981            610
seed7/lib/asn1.s7i           0.067492            544
seed7/lib/asn1oid.s7i        0.049336            157
seed7/lib/basearray.s7i      0.064370            450
seed7/lib/bigfile.s7i        0.041838            136
seed7/lib/bigint.s7i         0.077106            824
seed7/lib/bigrat.s7i         0.078991            784
seed7/lib/bin16.s7i          0.067222            592
seed7/lib/bin32.s7i          0.060759            490
seed7/lib/bin64.s7i          0.063211            539
seed7/lib/bitdata.s7i        0.123265           1330
seed7/lib/bitmapfont.s7i     0.045878            215
seed7/lib/bitset.s7i         0.061522            593
seed7/lib/bitsetof.s7i       0.059929            431
seed7/lib/blowfish.s7i       0.075749            383
seed7/lib/bmp.s7i            0.098021            924
seed7/lib/boolean.s7i        0.054638            403
seed7/lib/browser.s7i        0.052649            280
seed7/lib/bstring.s7i        0.047228            227
seed7/lib/bytedata.s7i       0.066021            482
seed7/lib/bzip2.s7i          0.088929            887
seed7/lib/cards.s7i          0.101957           1342
seed7/lib/category.s7i       0.048273            209
seed7/lib/cc_conf.s7i        0.119185           1314
seed7/lib/ccittfax.s7i       0.100003           1022
seed7/lib/cgi.s7i            0.040757            109
seed7/lib/cgidialog.s7i      0.094247           1118
seed7/lib/char.s7i           0.050310            356
seed7/lib/charsets.s7i       0.122180           2024
seed7/lib/chartype.s7i       0.047650            121
seed7/lib/cipher.s7i         0.041541            146
seed7/lib/cli_cmds.s7i       0.112397           1360
seed7/lib/clib_file.s7i      0.051423            301
seed7/lib/color.s7i          0.047906            185
seed7/lib/complex.s7i        0.058634            464
seed7/lib/compress.s7i       0.043314            150
seed7/lib/console.s7i        0.045439            188
seed7/lib/cpio.s7i           0.143231           1708
seed7/lib/crc32.s7i          0.053837            193
seed7/lib/cronos16.s7i       0.193106           1173
seed7/lib/cronos27.s7i       0.247490           1464
seed7/lib/csv.s7i            0.050956            201
seed7/lib/db_prop.s7i        0.099946            991
seed7/lib/deflate.s7i        0.084437            740
seed7/lib/des.s7i            0.080246            444
seed7/lib/dialog.s7i         0.055956            311
seed7/lib/dir.s7i            0.042624            163
seed7/lib/draw.s7i           0.084733            854
seed7/lib/duration.s7i       0.097120           1038
seed7/lib/echo.s7i           0.042009            132
seed7/lib/editline.s7i       0.059050            398
seed7/lib/elf.s7i            0.153156           1560
seed7/lib/elliptic.s7i       0.073322            649
seed7/lib/enable_io.s7i      0.050319            312
seed7/lib/encoding.s7i       0.095864            931
seed7/lib/enumeration.s7i    0.049604            236
seed7/lib/environment.s7i    0.043881            175
seed7/lib/exif.s7i           0.045583            152
seed7/lib/external_file.s7i  0.051508            340
seed7/lib/field.s7i          0.052299            268
seed7/lib/file.s7i           0.053743            372
seed7/lib/filebits.s7i       0.037470             46
seed7/lib/filesys.s7i        0.063767            601
seed7/lib/fileutil.s7i       0.043012            144
seed7/lib/fixarray.s7i       0.052900            307
seed7/lib/float.s7i          0.072830            757
seed7/lib/font.s7i           0.043753            196
seed7/lib/font8x8.s7i        0.066022            998
seed7/lib/forloop.s7i        0.057319            449
seed7/lib/ftp.s7i            0.084929            969
seed7/lib/ftpserv.s7i        0.074389            631
seed7/lib/getf.s7i           0.040008            115
seed7/lib/gethttp.s7i        0.035971             41
seed7/lib/gethttps.s7i       0.036853             41
seed7/lib/gif.s7i            0.071400            561
seed7/lib/graph.s7i          0.063910            415
seed7/lib/graph_file.s7i     0.057401            399
seed7/lib/gtkserver.s7i      0.041580            161
seed7/lib/gzip.s7i           0.066490            573
seed7/lib/hash.s7i           0.065136            421
seed7/lib/hashsetof.s7i      0.065019            499
seed7/lib/hmac.s7i           0.043624            152
seed7/lib/html.s7i           0.038684             83
seed7/lib/html_ent.s7i       0.062799            476
seed7/lib/htmldom.s7i        0.052668            286
seed7/lib/http_request.s7i   0.078094            696
seed7/lib/http_srv_resp.s7i  0.058994            380
seed7/lib/https_request.s7i  0.047929            211
seed7/lib/httpserv.s7i       0.054631            345
seed7/lib/huffman.s7i        0.072279            644
seed7/lib/ico.s7i            0.047853            221
seed7/lib/idxarray.s7i       0.049077            232
seed7/lib/image.s7i          0.041171            156
seed7/lib/imagefile.s7i      0.044503            171
seed7/lib/inflate.s7i        0.062751            411
seed7/lib/inifile.s7i        0.042109            129
seed7/lib/integer.s7i        0.067133            663
seed7/lib/iobuffer.s7i       0.050559            289
seed7/lib/jpeg.s7i           0.154138           1761
seed7/lib/json.s7i           0.080022            891
seed7/lib/json_serde.s7i     0.078643            783
seed7/lib/keybd.s7i          0.080246            639
seed7/lib/keydescr.s7i       0.049613            192
seed7/lib/leb128.s7i         0.046380            218
seed7/lib/line.s7i           0.043043            164
seed7/lib/listener.s7i       0.047445            247
seed7/lib/logfile.s7i        0.037034             73
seed7/lib/lower.s7i          0.040617            142
seed7/lib/lzma.s7i           0.094082            934
seed7/lib/lzw.s7i            0.088329            861
seed7/lib/magic.s7i          0.063271            403
seed7/lib/mahjng32.s7i       0.091681           1500
seed7/lib/make.s7i           0.069661            544
seed7/lib/makedata.s7i       0.120364           1428
seed7/lib/math.s7i           0.044884            201
seed7/lib/mixarith.s7i       0.046647            249
seed7/lib/modern27.s7i       0.167296           1099
seed7/lib/more.s7i           0.042220            130
seed7/lib/msgdigest.s7i      0.138744           1222
seed7/lib/multiscr.s7i       0.037865             68
seed7/lib/null_file.s7i      0.049426            345
seed7/lib/osfiles.s7i        0.093223           1085
seed7/lib/pbm.s7i            0.046832            230
seed7/lib/pcx.s7i            0.077717            638
seed7/lib/pem.s7i            0.045526            185
seed7/lib/pgm.s7i            0.048184            238
seed7/lib/pic16.s7i          0.066279           1037
seed7/lib/pic32.s7i          0.121379           2060
seed7/lib/pic_util.s7i       0.043707            144
seed7/lib/pixelimage.s7i     0.052740            320
seed7/lib/pixmap_file.s7i    0.062029            459
seed7/lib/pixmapfont.s7i     0.047825            184
seed7/lib/pkcs1.s7i          0.077563            543
seed7/lib/png.s7i            0.108002           1064
seed7/lib/poll.s7i           0.052264            313
seed7/lib/ppm.s7i            0.048326            240
seed7/lib/process.s7i        0.063173            541
seed7/lib/progs.s7i          0.077836            789
seed7/lib/propertyfile.s7i   0.042657            155
seed7/lib/rational.s7i       0.079800            792
seed7/lib/ref_list.s7i       0.049008            252
seed7/lib/reference.s7i      0.041875            126
seed7/lib/reverse.s7i        0.040324             94
seed7/lib/rpm.s7i            0.285354           3487
seed7/lib/rpmext.s7i         0.051896            318
seed7/lib/scanfile.s7i       0.131965           1779
seed7/lib/scanjson.s7i       0.060672            413
seed7/lib/scanstri.s7i       0.137295           1814
seed7/lib/scantoml.s7i       0.127647           1603
seed7/lib/seed7_05.s7i       0.109318           1072
seed7/lib/set.s7i            0.038567             57
seed7/lib/shell.s7i          0.068225            615
seed7/lib/showtls.s7i        0.083412            678
seed7/lib/signature.s7i      0.041447            131
seed7/lib/smtp.s7i           0.048419            261
seed7/lib/sockbase.s7i       0.050396            217
seed7/lib/socket.s7i         0.051698            326
seed7/lib/sokoban1.s7i       0.080348           1519
seed7/lib/sql_base.s7i       0.094241           1000
seed7/lib/stars.s7i          0.235491           1705
seed7/lib/stdfont10.s7i      0.140825           3347
seed7/lib/stdfont12.s7i      0.162967           3928
seed7/lib/stdfont14.s7i      0.188868           4510
seed7/lib/stdfont16.s7i      0.215943           5092
seed7/lib/stdfont18.s7i      0.243680           5868
seed7/lib/stdfont20.s7i      0.268399           6449
seed7/lib/stdfont24.s7i      0.327697           7421
seed7/lib/stdfont8.s7i       0.126749           2960
seed7/lib/stdfont9.s7i       0.133285           3152
seed7/lib/stdio.s7i          0.044111            192
seed7/lib/strifile.s7i       0.053726            345
seed7/lib/string.s7i         0.076441            779
seed7/lib/stritext.s7i       0.055510            352
seed7/lib/struct.s7i         0.055913            266
seed7/lib/struct_elem.s7i    0.042121            129
seed7/lib/subfile.s7i        0.042525            174
seed7/lib/subrange.s7i       0.037744             78
seed7/lib/syntax.s7i         0.058583            294
seed7/lib/tar.s7i            0.143493           1880
seed7/lib/tar_cmds.s7i       0.081799            752
seed7/lib/tdes.s7i           0.042580            143
seed7/lib/tee.s7i            0.040746            143
seed7/lib/text.s7i           0.040221            135
seed7/lib/tga.s7i            0.078006            676
seed7/lib/tiff.s7i           0.243954           2771
seed7/lib/time.s7i           0.102225           1191
seed7/lib/tls.s7i            0.194645           2230
seed7/lib/unicode.s7i        0.070432            575
seed7/lib/unionfnd.s7i       0.040726            130
seed7/lib/upper.s7i          0.040101            142
seed7/lib/utf16.s7i          0.066447            540
seed7/lib/utf8.s7i           0.047658            234
seed7/lib/vecfont10.s7i      0.159588           1056
seed7/lib/vecfont18.s7i      0.175497           1119
seed7/lib/vector3d.s7i       0.049339            293
seed7/lib/vectorfont.s7i     0.047688            239
seed7/lib/wildcard.s7i       0.042393            140
seed7/lib/window.s7i         0.061365            455
seed7/lib/wrinum.s7i         0.048981            248
seed7/lib/x509cert.s7i       0.117114           1243
seed7/lib/xml_ent.s7i        0.040199             94
seed7/lib/xmldom.s7i         0.048850            303
seed7/lib/xz.s7i             0.058835            442
seed7/lib/zip.s7i            0.230432           2792
seed7/lib/zstd.s7i           0.117422           1263
============================ ================= ================

Statistical Summary — Mode D (GC-free, mean-of-N)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+-----------------+
| Metric    | Mean Time (s)   |
+===========+=================+
| Average   | 0.157104        |
+-----------+-----------------+
| Minimum   | 0.035275        |
+-----------+-----------------+
| Maximum   | 5.509252        |
+-----------+-----------------+

Cross-Mode Comparison
=====================

+------+-----------------+-----------------+-----------------+
| Mode | Average (s)     | Minimum (s)     | Maximum (s)     |
+======+=================+=================+=================+
| A    | 0.033817        | 0.032243        | 0.043732        |
+------+-----------------+-----------------+-----------------+
| B    | 0.039833        | 0.034738        | 0.049518        |
+------+-----------------+-----------------+-----------------+
| C    | 0.089197        | 0.034840        | 2.498127        |
+------+-----------------+-----------------+-----------------+
| D    | 0.157104        | 0.035275        | 5.509252        |
+------+-----------------+-----------------+-----------------+

Phase Timing Summary
====================

Wall-clock times include warm-up work and GC performed by the warm-up helper.

+----------+--------------+--------------+--------------+
| Phase    | Warm-up      | Timed pass   | Mode total   |
+==========+==============+==============+==============+
| Mode A   | 00:00:12.259 | 00:00:57.606 | 00:01:09.866 |
+----------+--------------+--------------+--------------+
| Mode B   | 00:00:15.812 | 00:01:07.922 | 00:01:23.735 |
+----------+--------------+--------------+--------------+
| Mode C   | 00:00:35.670 | 00:02:32.828 | 00:03:08.498 |
+----------+--------------+--------------+--------------+
| Mode D   | 00:01:01.271 | 00:04:28.529 | 00:05:29.801 |
+----------+--------------+--------------+--------------+
| Total    |              |              | 00:11:11.908 |
+----------+--------------+--------------+--------------+


.. ---------------------------------------------------------------------------
