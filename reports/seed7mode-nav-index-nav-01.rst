=============================
Seed7 Navigation Entity Index
=============================

- Running with: seed7-mode 2026-06-23T16:07:25+0000 W26-2
- Seed7 dir: /Users/roup/my/dvo/seed7-repos/seed7/
- Generated on: 2026-06-23T15:34:54-0400
- Files indexed: 340

.. sectnum::
   :depth: 2

.. contents:: Table of Contents
   :depth: 2

.. ---------------------------------------------------------------------------

prg
===

prg/addup.sd7
-------------

:Lines:    190
:Entities: 4

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
39     procedure   ``human_move``
71     procedure   ``comp_move``
85     procedure   ``game``
122    procedure   ``main``
====== =========== ==================================================

prg/bas7.sd7
------------

:Lines:    11459
:Entities: 240

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
101    set         ``basic_name_char``
102    set         ``number_suffix``
103    set         ``numeric_var_suffix``
105    structure   ``lineType``
113    array       ``prg``
154    set         ``defint_var``
155    set         ``deflng_var``
156    set         ``defsng_var``
157    set         ``defdbl_var``
158    set         ``defstr_var``
160    structure   ``defFnType``
202    structure   ``forLoopDescrType``
210    array       ``forLoop``
212    structure   ``whileLoopDescrType``
217    array       ``whileLoop``
219    structure   ``doLoopDescrType``
224    array       ``doLoop``
231    structure   ``gosubReturnDescrType``
241    array       ``gosubReturn``
243    structure   ``boundsType``
260    set         ``numeric_functions``
269    set         ``not_allowed_as_label``
281    array       ``loresColor``
299    array       ``hiresColor``
310    procedure   ``sleep``
319    procedure   ``delay``
332    procedure   ``listProg``
407    function    ``get_symbol``
685    function    ``endOfStatement``
689    function    ``ignoreRestOfLine``
693    function    ``isStringExpr``
700    function    ``isStringVar``
707    function    ``isNumericExpr``
720    function    ``isNumericVar``
731    function    ``isIntegerVar``
738    function    ``isLongVar``
745    function    ``isSingleVar``
752    function    ``isDoubleVar``
759    procedure   ``error_marker``
773    function    ``getNumericVar``
788    procedure   ``setNumericVar``
795    function    ``getStringVar``
805    procedure   ``setStringVar``
811    function    ``varptr``
823    function    ``varptrStri``
834    function    ``varname``
844    function    ``getFileValue``
854    procedure   ``setFileValue``
860    procedure   ``closeAllFiles``
871    procedure   ``addDoLoopHeader``
887    function    ``doLoopHeaderPresent``
902    procedure   ``line_marker``
917    procedure   ``line_marker``
936    procedure   ``error_marker``
942    procedure   ``error_expect``
950    procedure   ``error_expect2``
958    procedure   ``error_expect3``
967    procedure   ``expect``
977    function    ``label_or_linenum``
981    procedure   ``goto_label_or_linenum``
1020   procedure   ``set_return_position``
1034   procedure   ``set_sub_entry_position``
1040   procedure   ``check_loop_stacks_before_return``
1079   procedure   ``do_return``
1089   procedure   ``goto_on_error``
1100   procedure   ``do_resume_next``
1116   procedure   ``do_resume_same``
1132   function    ``color_num``
1157   function    ``get_data_line``
1206   procedure   ``data_statement_in_line``
1227   function    ``get_data_field``
1313   procedure   ``skip_space_cr_lf``
1325   function    ``read_input_string``
1349   function    ``read_input_number``
1373   procedure   ``assign_input_number``
1391   procedure   ``assign_input_string``
1399   procedure   ``set_function``
1412   procedure   ``define_function``
1457   procedure   ``getBoundsFromIndexPart``
1485   procedure   ``getBounds``
1506   function    ``exec_lbound``
1516   function    ``exec_ubound``
1526   function    ``getFirstIndex``
1561   function    ``exec_expr``
1564   procedure   ``append_index``
1600   function    ``get_name``
1610   procedure   ``skip_parenthesized_stri``
1635   function    ``is_let_statement``
1655   function    ``exec_str_expr``
1661   function    ``exec_str_function``
1670   array       ``str_value_backup``
1671   array       ``num_value_backup``
1721   function    ``extendedKeyCode``
1825   function    ``keyboardScanCode``
1918   procedure   ``execLines``
1921   function    ``exec_str_primary``
2331   function    ``exec_str_mult``
2356   function    ``exec_str_expr``
2374   function    ``exec_function``
2383   array       ``str_value_backup``
2384   array       ``num_value_backup``
2434   function    ``exec_primary``
3099   function    ``exec_exponentation``
3118   function    ``exec_negation``
3134   function    ``exec_multdiv``
3198   function    ``exec_addsub``
3219   function    ``exec_comparison``
3280   function    ``exec_cond_not``
3296   function    ``binary_and``
3312   function    ``exec_cond_and``
3337   function    ``binary_or``
3353   function    ``binary_xor``
3369   function    ``exec_cond_or``
3400   function    ``binary_eqv``
3416   function    ``exec_cond_eqv``
3433   function    ``binary_imp``
3449   function    ``exec_expr``
3466   function    ``getNameList``
3483   function    ``nameInList``
3500   function    ``removeNameFromList``
3530   function    ``next_symbol``
3552   function    ``find_then``
3578   procedure   ``find_else``
3618   function    ``find_next``
3676   function    ``find_next``
3702   function    ``find_wend``
3720   function    ``find_do``
3741   function    ``find_loop``
3759   function    ``find_end_sub``
3788   function    ``find_end_function``
3817   function    ``find_end_select``
3846   function    ``find_end_if``
3888   function    ``find_else_elseif_or_end_if``
3933   function    ``find_case_or_end_select``
3965   procedure   ``exec_elseif_else_chain``
4017   procedure   ``advance_after_statement``
4032   procedure   ``exec_goto``
4091   procedure   ``exec_gosub``
4155   procedure   ``typeMismatch``
4180   procedure   ``assignmentExpected``
4206   procedure   ``variableExpected``
4230   procedure   ``exec_let``
4285   procedure   ``exec_mid_statement``
4380   procedure   ``exec_next_decision``
4436   procedure   ``exec_if``
4597   procedure   ``exec_else``
4620   procedure   ``exec_for``
4683   procedure   ``exec_next``
4751   procedure   ``exec_on``
4866   procedure   ``exec_do``
4942   procedure   ``exec_loop``
5083   procedure   ``exec_select``
5267   function    ``continueWithPrintStatement``
5286   procedure   ``exec_print_using``
5505   procedure   ``illegalFunctionCall``
5529   procedure   ``exec_print``
5665   procedure   ``exec_print``
5769   procedure   ``exec_lprint``
5777   procedure   ``exec_print_to_file``
5815   procedure   ``exec_write_to_file``
5867   procedure   ``exec_write``
5904   procedure   ``exec_read``
5974   procedure   ``exec_input_from_file``
6047   procedure   ``read_input``
6075   procedure   ``exec_input``
6139   procedure   ``exec_line_input_from_file``
6184   procedure   ``exec_line_input``
6236   procedure   ``exec_linput_from_file``
6281   procedure   ``exec_linput``
6330   procedure   ``exec_accept``
6332   set         ``accept_keywords``
6430   procedure   ``exec_display``
6432   set         ``display_keywords``
6504   function    ``basicOpen``
6508   array       ``pathElems``
6511   array       ``directoryContent``
6571   procedure   ``exec_open``
6765   procedure   ``exec_close``
6806   procedure   ``exec_file_put``
6931   procedure   ``exec_file_get``
7104   procedure   ``exec_seek``
7149   procedure   ``clearProgram``
7162   procedure   ``exec_clear``
7174   function    ``parseType``
7186   procedure   ``deallocateArray``
7210   procedure   ``initArray``
7239   procedure   ``exec_dim``
7369   procedure   ``exec_defType_numeric``
7420   procedure   ``exec_defstr``
7466   procedure   ``exec_type``
7493   function    ``readVarNameFromBloadFile``
7522   procedure   ``exec_bload``
7646   procedure   ``exec_bsave``
7680   procedure   ``exec_files``
7701   function    ``getCga2ImageFromBytes``
7742   function    ``getCga4ImageFromBytes``
7755   array       ``palette1``
7783   function    ``getEgaImageFromBytes``
7826   function    ``getVgaImageFromBytes``
7863   function    ``getBytesFromArray``
7904   function    ``getImageFromArray``
7930   function    ``getImageFromArray``
7953   procedure   ``exec_screen``
8060   procedure   ``exec_pset``
8092   procedure   ``exec_preset``
8124   procedure   ``exec_line``
8206   procedure   ``exec_circle``
8263   procedure   ``exec_put``
8305   procedure   ``exec_get``
8368   function    ``getChar``
8377   function    ``getSign``
8390   procedure   ``exec_draw``
8575   procedure   ``exec_plot``
8592   procedure   ``exec_hplot``
8628   procedure   ``setup_graphic``
8648   procedure   ``exec_call_char``
8653   array       ``charPicture``
8705   procedure   ``exec_call_clear``
8716   procedure   ``exec_call_color``
8757   procedure   ``exec_call_hchar``
8793   procedure   ``exec_call_key``
8941   procedure   ``exec_call_screen``
8955   procedure   ``exec_call_sound``
8980   procedure   ``exec_call_vchar``
9016   procedure   ``exec_let``
9172   function    ``runOrChain``
9176   function    ``execCmd``
10878  procedure   ``execLines``
10901  procedure   ``runProg``
10908  procedure   ``logLine``
10919  procedure   ``prepareLoops``
10991  procedure   ``preprocessLine``
11026  procedure   ``checkLabels``
11072  function    ``loadProg``
11075  procedure   ``loadProg``
11283  function    ``runOrChain``
11318  function    ``loadProg``
11338  procedure   ``interactiveMode``
11382  procedure   ``main``
11384  array       ``arg_v``
====== =========== ==================================================

prg/bifurk.sd7
--------------

:Lines:    73
:Entities: 2

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
38     procedure   ``bifurk``
64     procedure   ``main``
====== =========== ==================================================

prg/bigfiles.sd7
----------------

:Lines:    129
:Entities: 5

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
33     function    ``searchDir``
38     array       ``dirElements``
70     procedure   ``showResults``
73     array       ``lengthList``
93     procedure   ``main``
====== =========== ==================================================

prg/brainf7.sd7
---------------

:Lines:    86
:Entities: 3

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
29     procedure   ``brainF``
31     array       ``memory``
71     procedure   ``main``
====== =========== ==================================================

prg/calc7.sd7
-------------

:Lines:    128
:Entities: 1

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
33     procedure   ``main``
====== =========== ==================================================

prg/carddemo.sd7
----------------

:Lines:    190
:Entities: 5

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
40     structure   ``cardType``
52     procedure   ``put``
70     procedure   ``move``
94     function    ``select_card``
112    procedure   ``main``
====== =========== ==================================================

prg/castle.sd7
--------------

:Lines:    3148
:Entities: 126

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
63     enumeration ``directionType``
67     function    ``str``
70     function    ``parse``
76     enumeration ``itemType``
88     enumeration ``monsterType``
150    function    ``str``
153    function    ``parse``
159    structure   ``graphObj``
162    array       ``picture``
171    structure   ``itemObj``
176    structure   ``monsterObj``
185    structure   ``playerObj``
193    structure   ``roomType``
194    array       ``data``
195    array       ``description``
196    array       ``exits``
199    structure   ``scoreType``
206    array       ``item``
207    array       ``monster``
209    array       ``room``
214    array       ``message``
260    array       ``query_pic``
271    array       ``wall_pic``
290    array       ``colored_wall1_pic``
309    array       ``colored_wall2_pic``
328    array       ``vertical_pic``
347    array       ``horizontal_pic``
366    array       ``left_upper_pic``
385    array       ``right_upper_pic``
404    array       ``left_lower_pic``
423    array       ``right_lower_pic``
442    array       ``railing_pic``
461    array       ``gate_pic``
480    array       ``small_bush_pic``
499    array       ``big_bush_pic``
518    array       ``wood_pic``
537    array       ``stone_pic``
556    array       ``gray_pic``
575    array       ``water_pic``
594    array       ``bed_left_pic``
613    array       ``bed_right_pic``
632    array       ``bed_top_pic``
651    array       ``bed_bottom_pic``
670    array       ``bed_white_middle_pic``
689    array       ``bed_red_middle_pic``
708    array       ``bed_blue_middle_pic``
727    array       ``bed_purple_middle_pic``
746    array       ``bed_yellow_middle_pic``
765    array       ``chain_pic``
784    array       ``k_pic``
795    array       ``b_pic``
806    array       ``up_pic``
841    array       ``down_pic``
876    array       ``big_spider_pic``
895    array       ``small_spider_pic``
914    array       ``fairy_pic``
949    array       ``large_gem_pic``
968    array       ``floor_pic``
979    array       ``player_pic``
998    function    ``createPixmap``
1005   procedure   ``initRoomData``
1028   procedure   ``initMonster``
1054   procedure   ``initMonsterData``
1074   function    ``conv``
1087   function    ``conv``
1091   procedure   ``initItem``
1112   procedure   ``initItemData``
1139   procedure   ``initPlayerData``
1155   procedure   ``loadHighScore``
1171   procedure   ``saveHighScore``
1185   procedure   ``initGame``
1245   procedure   ``displayBox``
1261   function    ``getPixmap``
1273   procedure   ``displayRoom``
1346   procedure   ``displayObj``
1359   procedure   ``displayMonster``
1371   procedure   ``objectLegend``
1383   procedure   ``displayLegend``
1415   function    ``sizeOfLegend``
1443   procedure   ``collectedTreasures``
1469   procedure   ``killedMonsters``
1511   function    ``getInventoryStartLine``
1522   procedure   ``displayInventoryItem``
1538   procedure   ``displayInventoryList``
1559   procedure   ``displayInventory``
1572   procedure   ``displayMessage``
1584   procedure   ``displayCommands``
1610   procedure   ``displayAll``
1624   procedure   ``floodTrap``
1651   procedure   ``checkRoom``
1695   procedure   ``hideItem``
1708   procedure   ``showItem``
1721   procedure   ``loadGame``
1769   procedure   ``saveGame``
1809   function    ``searchMonster``
1827   function    ``hitMonster``
1844   function    ``selectItem``
1939   procedure   ``dropItem``
1988   procedure   ``dropItem``
1999   procedure   ``useItem``
2081   procedure   ``useItem``
2092   function    ``getMonsterMessage``
2094   array       ``message``
2116   function    ``getItemMessage``
2118   array       ``message``
2176   function    ``getScore``
2205   procedure   ``showStatus``
2242   procedure   ``lookItemInRoom``
2455   procedure   ``lookItem``
2475   procedure   ``welcomeScreen``
2494   procedure   ``endOfGame``
2513   function    ``getField``
2543   procedure   ``checkField``
2580   function    ``hitWall``
2589   function    ``checkCollision``
2600   function    ``hitPlayer``
2611   procedure   ``moveMonsters``
2709   procedure   ``enterRoom``
2716   procedure   ``moveDelta``
2805   procedure   ``move``
2856   procedure   ``moveTo``
2911   procedure   ``takeItem``
2942   procedure   ``takeItem``
3064   procedure   ``quitGame``
3072   procedure   ``playGame``
3132   procedure   ``main``
====== =========== ==================================================

prg/cat.sd7
-----------

:Lines:    82
:Entities: 2

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
31     procedure   ``cat``
59     procedure   ``main``
====== =========== ==================================================

prg/cellauto.sd7
----------------

:Lines:    85
:Entities: 3

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
34     function    ``nextGeneration``
52     procedure   ``drawGeneration``
65     procedure   ``main``
====== =========== ==================================================

prg/celsius.sd7
---------------

:Lines:    42
:Entities: 1

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
29     procedure   ``main``
====== =========== ==================================================

prg/chk_all.sd7
---------------

:Lines:    843
:Entities: 10

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
528    procedure   ``ignoreFileError``
538    function    ``equalFiles``
569    function    ``cmdOutput``
575    array       ``parameters``
600    function    ``checkInterpreter``
622    function    ``checkCompiler``
736    procedure   ``check``
738    array       ``minimalTestOptions``
739    array       ``options``
770    procedure   ``main``
====== =========== ==================================================

prg/chkarr.sd7
--------------

:Lines:    8367
:Entities: 384

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
33     array       ``testNumArr``
34     array       ``testStriArr``
36     array       ``fltArr1``
37     array       ``fltArr2``
38     array       ``fltArr3``
39     array       ``fltArr4``
41     array       ``arr1``
43     array       ``arr2``
45     array       ``arr3``
47     array       ``arr4``
50     function    ``boolExpr``
54     function    ``intExpr``
58     function    ``bigintExpr``
66     function    ``floatExpr``
70     function    ``complexExpr``
74     function    ``charExpr``
78     function    ``striExpr``
82     function    ``bstriExpr``
86     function    ``setExpr``
90     procedure   ``DECLARE_ARR_EXPR``
92     function    ``arrExpr``
108    function    ``raisesIndexError``
121    procedure   ``DECLARE_RAISES_INDEX_ERROR``
124    function    ``raisesIndexError``
138    function    ``raisesIndexError``
152    function    ``raisesIndexError``
156    array       ``exprResult``
180    procedure   ``DECLARE_RAISES_RANGE_ERROR``
183    function    ``raisesRangeError``
187    array       ``exprResult``
211    procedure   ``subprog1``
213    array       ``arr3``
219    procedure   ``testSpecial``
256    function    ``striRand``
271    procedure   ``testArrayCopy``
278    array       ``stringArray``
280    array       ``destArray``
301    procedure   ``testInitialization``
304    array       ``blnArr1Const``
305    array       ``intArr1Const``
306    array       ``bigArr1Const``
307    array       ``fltArr1Const``
308    array       ``cpxArr1Const``
309    array       ``chrArr1Const``
310    array       ``strArr1Const``
311    array       ``bstArr1Const``
312    array       ``setArr1Const``
313    array       ``blnArr2Const``
314    array       ``intArr2Const``
315    array       ``bigArr2Const``
316    array       ``fltArr2Const``
317    array       ``cpxArr2Const``
318    array       ``chrArr2Const``
319    array       ``strArr2Const``
320    array       ``bstArr2Const``
321    array       ``setArr2Const``
322    array       ``blnArr3Const``
323    array       ``intArr3Const``
324    array       ``bigArr3Const``
325    array       ``fltArr3Const``
326    array       ``cpxArr3Const``
327    array       ``chrArr3Const``
328    array       ``strArr3Const``
329    array       ``bstArr3Const``
330    array       ``setArr3Const``
331    array       ``blnArr1``
332    array       ``intArr1``
333    array       ``bigArr1``
334    array       ``fltArr1``
335    array       ``cpxArr1``
336    array       ``chrArr1``
337    array       ``strArr1``
338    array       ``bstArr1``
339    array       ``setArr1``
340    array       ``blnArr2``
341    array       ``intArr2``
342    array       ``bigArr2``
343    array       ``fltArr2``
344    array       ``cpxArr2``
345    array       ``chrArr2``
346    array       ``strArr2``
347    array       ``bstArr2``
348    array       ``setArr2``
349    array       ``blnArr3``
350    array       ``intArr3``
351    array       ``bigArr3``
352    array       ``fltArr3``
353    array       ``cpxArr3``
354    array       ``chrArr3``
355    array       ``strArr3``
356    array       ``bstArr3``
357    array       ``setArr3``
439    procedure   ``testCompare``
442    array       ``blnArr``
443    array       ``intArr``
444    array       ``bigArr``
445    array       ``fltArr``
446    array       ``cpxArr``
448    array       ``chrArr``
449    array       ``strArr``
450    array       ``bstArr``
452    array       ``setArr``
453    array       ``blnArr2``
454    array       ``intArr2``
455    array       ``bigArr2``
456    array       ``fltArr2``
457    array       ``cpxArr2``
459    array       ``chrArr2``
460    array       ``strArr2``
461    array       ``bstArr2``
463    array       ``setArr2``
464    array       ``blnArrConst``
465    array       ``intArrConst``
466    array       ``bigArrConst``
467    array       ``fltArrConst``
468    array       ``cpxArrConst``
470    array       ``chrArrConst``
471    array       ``strArrConst``
472    array       ``bstArrConst``
474    array       ``setArrConst``
807    procedure   ``definePeelFirstElement``
810    function    ``peelFirstElement``
833    procedure   ``testAssign``
836    array       ``blnArr``
837    array       ``intArr``
838    array       ``bigArr``
839    array       ``fltArr``
840    array       ``cpxArr``
842    array       ``chrArr``
843    array       ``strArr``
844    array       ``bstArr``
846    array       ``setArr``
847    array       ``blnArr2``
848    array       ``intArr2``
849    array       ``bigArr2``
850    array       ``fltArr2``
851    array       ``cpxArr2``
852    array       ``chrArr2``
853    array       ``strArr2``
854    array       ``bstArr2``
855    array       ``setArr2``
856    array       ``blnArrArr``
857    array       ``intArrArr``
858    array       ``bigArrArr``
859    array       ``fltArrArr``
860    array       ``cpxArrArr``
861    array       ``chrArrArr``
862    array       ``strArrArr``
863    array       ``bstArrArr``
864    array       ``setArrArr``
1542   procedure   ``testAppend``
1545   array       ``blnArr``
1546   array       ``intArr``
1547   array       ``bigArr``
1548   array       ``fltArr``
1549   array       ``cpxArr``
1551   array       ``chrArr``
1552   array       ``strArr``
1553   array       ``bstArr``
1555   array       ``setArr``
1679   procedure   ``testPush``
1682   array       ``blnArr``
1683   array       ``intArr``
1684   array       ``bigArr``
1685   array       ``fltArr``
1686   array       ``cpxArr``
1688   array       ``chrArr``
1689   array       ``strArr``
1690   array       ``bstArr``
1692   array       ``setArr``
1812   procedure   ``testLength``
1815   array       ``blnArrConst``
1816   array       ``intArrConst``
1817   array       ``bigArrConst``
1818   array       ``fltArrConst``
1819   array       ``cpxArrConst``
1821   array       ``chrArrConst``
1822   array       ``strArrConst``
1823   array       ``bstArrConst``
1825   array       ``setArrConst``
1826   array       ``blnArrConst0``
1827   array       ``intArrConst0``
1828   array       ``bigArrConst0``
1829   array       ``fltArrConst0``
1830   array       ``cpxArrConst0``
1832   array       ``chrArrConst0``
1833   array       ``strArrConst0``
1834   array       ``bstArrConst0``
1836   array       ``setArrConst0``
1837   array       ``blnArr``
1838   array       ``intArr``
1839   array       ``bigArr``
1840   array       ``fltArr``
1841   array       ``cpxArr``
1843   array       ``chrArr``
1844   array       ``strArr``
1845   array       ``bstArr``
1847   array       ``setArr``
1848   array       ``blnArr0``
1849   array       ``intArr0``
1850   array       ``bigArr0``
1851   array       ``fltArr0``
1852   array       ``cpxArr0``
1854   array       ``chrArr0``
1855   array       ``strArr0``
1856   array       ``bstArr0``
1858   array       ``setArr0``
1908   function    ``testIndex1``
1942   function    ``testIndex2``
1987   function    ``testFixArrayIndex``
2099   function    ``testBaseArrayIndex``
2245   function    ``returnFileArray``
2249   function    ``testFileArray``
2288   function    ``testIndex1``
2292   array       ``blnArrConst``
2293   array       ``intArrConst``
2294   array       ``bigArrConst``
2295   array       ``fltArrConst``
2296   array       ``blnArrConst0``
2297   array       ``intArrConst0``
2298   array       ``bigArrConst0``
2299   array       ``fltArrConst0``
2300   array       ``blnArr``
2301   array       ``intArr``
2302   array       ``bigArr``
2303   array       ``fltArr``
2304   array       ``emptyBlnArr``
2305   array       ``emptyIntArr``
2306   array       ``emptyBigArr``
2307   array       ``emptyFltArr``
2627   function    ``testIndex2``
2631   array       ``cpxArrConst``
2633   array       ``chrArrConst``
2634   array       ``strArrConst``
2635   array       ``bstArrConst``
2637   array       ``setArrConst``
2638   array       ``cpxArrConst0``
2640   array       ``chrArrConst0``
2641   array       ``strArrConst0``
2642   array       ``bstArrConst0``
2644   array       ``setArrConst0``
2645   array       ``cpxArr``
2647   array       ``chrArr``
2648   array       ``strArr``
2649   array       ``bstArr``
2651   array       ``setArr``
2652   array       ``emptyCpxArr``
2653   array       ``emptyChrArr``
2654   array       ``emptyStrArr``
2655   array       ``emptyBstArr``
2656   array       ``emptySetArr``
3055   procedure   ``testIndex``
3081   procedure   ``testTimes``
3365   procedure   ``testHeadAndTail``
3368   array       ``numArr``
3369   array       ``striArr``
3425   procedure   ``testHead``
3428   array       ``intArr``
3429   array       ``bigArr``
3430   array       ``fltArr``
3431   array       ``cpxArr``
3433   array       ``chrArr``
3434   array       ``strArr``
3435   array       ``bstArr``
3437   array       ``setArr``
3438   array       ``intHead``
3439   array       ``bigHead``
3440   array       ``fltHead``
3441   array       ``cpxHead``
3442   array       ``chrHead``
3443   array       ``strHead``
3444   array       ``bstHead``
3445   array       ``setHead``
4094   procedure   ``testTail``
4097   array       ``intArr``
4098   array       ``bigArr``
4099   array       ``fltArr``
4100   array       ``cpxArr``
4102   array       ``chrArr``
4103   array       ``strArr``
4104   array       ``bstArr``
4106   array       ``setArr``
4107   array       ``intTail``
4108   array       ``bigTail``
4109   array       ``fltTail``
4110   array       ``cpxTail``
4111   array       ``chrTail``
4112   array       ``strTail``
4113   array       ``bstTail``
4114   array       ``setTail``
4600   procedure   ``testRange``
4603   array       ``intArr``
4604   array       ``bigArr``
4605   array       ``fltArr``
4606   array       ``cpxArr``
4609   array       ``chrArr``
4610   array       ``strArr``
4612   array       ``bstArr``
4615   array       ``setArr``
4616   array       ``intRange``
4617   array       ``bigRange``
4618   array       ``fltRange``
4619   array       ``cpxRange``
4620   array       ``chrRange``
4621   array       ``strRange``
4622   array       ``bstRange``
4623   array       ``setRange``
5751   procedure   ``testSubarr``
5754   array       ``intArr``
5755   array       ``bigArr``
5756   array       ``fltArr``
5757   array       ``cpxArr``
5760   array       ``chrArr``
5761   array       ``strArr``
5763   array       ``bstArr``
5766   array       ``setArr``
5767   array       ``intSubarr``
5768   array       ``bigSubarr``
5769   array       ``fltSubarr``
5770   array       ``cpxSubarr``
5771   array       ``chrSubarr``
5772   array       ``strSubarr``
5773   array       ``bstSubarr``
5774   array       ``setSubarr``
6902   procedure   ``testInsertElement``
6906   array       ``intArr``
6907   array       ``bigArr``
6908   array       ``fltArr``
6909   array       ``cpxArr``
6910   array       ``chrArr``
6911   array       ``strArr``
6912   array       ``bstArr``
6913   array       ``setArr``
6914   array       ``arrArr``
7135   procedure   ``testInsertArray``
7139   array       ``intArr``
7140   array       ``bigArr``
7141   array       ``fltArr``
7142   array       ``cpxArr``
7143   array       ``chrArr``
7144   array       ``strArr``
7145   array       ``bstArr``
7146   array       ``setArr``
7147   array       ``intArr2``
7148   array       ``bigArr2``
7149   array       ``fltArr2``
7150   array       ``cpxArr2``
7151   array       ``chrArr2``
7152   array       ``strArr2``
7153   array       ``bstArr2``
7154   array       ``setArr2``
7622   procedure   ``testRemoveElement``
7625   array       ``intArr``
7626   array       ``bigArr``
7627   array       ``fltArr``
7628   array       ``cpxArr``
7630   array       ``chrArr``
7631   array       ``strArr``
7632   array       ``bstArr``
7634   array       ``setArr``
7882   procedure   ``testRemoveArray``
7885   array       ``intArr``
7886   array       ``bigArr``
7887   array       ``fltArr``
7888   array       ``cpxArr``
7890   array       ``chrArr``
7891   array       ``strArr``
7892   array       ``bstArr``
7894   array       ``setArr``
7895   array       ``intRemoved``
7896   array       ``bigRemoved``
7897   array       ``fltRemoved``
7898   array       ``cpxRemoved``
7899   array       ``chrRemoved``
7900   array       ``strRemoved``
7901   array       ``bstRemoved``
7902   array       ``setRemoved``
8206   procedure   ``testArraySort``
8208   array       ``intArr``
8209   array       ``bigArr``
8210   array       ``fltArr``
8211   array       ``chrArr``
8212   array       ``strArr``
8343   procedure   ``main``
====== =========== ==================================================

prg/chkbig.sd7
--------------

:Lines:    29026
:Entities: 154

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
36     function    ``bigintExpr``
44     function    ``intExpr``
48     function    ``striExpr``
52     function    ``boolExpr``
56     procedure   ``DECLARE_RAISES_RANGE_ERROR``
59     function    ``raisesRangeError``
80     procedure   ``DECLARE_RAISES_NUMERIC_ERROR``
83     function    ``raisesNumericError``
104    procedure   ``chkLiteral``
357    function    ``chkEq``
1163   function    ``chkNe``
1969   function    ``chkLt``
2775   function    ``chkLe``
3581   function    ``chkGe``
4387   function    ``chkGt``
5193   procedure   ``chkComparisons``
5222   function    ``chkParse``
5800   procedure   ``chkStringConv``
6476   function    ``chkRadix_1``
6497   function    ``chkRadix_2``
6893   function    ``chkRadix_3``
7298   function    ``chkRadix_4``
7464   procedure   ``chkRadix``
7493   procedure   ``chkOrd``
7676   procedure   ``chkBigOrdWithBigMod``
8141   procedure   ``chkConv``
8502   procedure   ``chkBytesBe``
8876   procedure   ``chkBytesLe``
9250   procedure   ``chkBytesBe2BigInt``
9707   procedure   ``chkBytesLe2BigInt``
10164  function    ``chkNegate``
10175  function    ``chkNegate_1``
10515  function    ``chkNegate_2``
10855  procedure   ``chkNegate``
10876  procedure   ``chkPlus``
11223  procedure   ``chkSucc``
11570  procedure   ``chkPred``
11917  function    ``testIncr``
11926  procedure   ``chkIncr``
12273  function    ``testDecr``
12282  procedure   ``chkDecr``
12629  procedure   ``chkAddition``
13058  function    ``chkAddAssignLiteral1``
13154  function    ``chkAddAssignLiteral2``
13291  function    ``chkAddAssignLiteral3``
13450  function    ``chkAddAssignLiteral4``
13546  function    ``chkAddAssignExpression1``
13642  function    ``chkAddAssignExpression2``
13779  function    ``chkAddAssignExpression3``
13938  function    ``chkAddAssignExpression4``
14034  function    ``chkAddAssignWithItself``
14089  procedure   ``chkAddAssign``
14108  procedure   ``chkSubtraction``
14531  function    ``chkSubtractAssignLiteral1``
14627  function    ``chkSubtractAssignLiteral2``
14765  function    ``chkSubtractAssignLiteral3``
14924  function    ``chkSubtractAssignLiteral4``
15020  function    ``chkSubtractAssignExpression1``
15116  function    ``chkSubtractAssignExpression2``
15254  function    ``chkSubtractAssignExpression3``
15413  function    ``chkSubtractAssignExpression4``
15509  function    ``chkSubtractAssignWithItself``
15564  procedure   ``chkSubtractAssign``
15583  function    ``chkMultiplicationWithConst1``
16044  function    ``chkMultiplicationWithConst2``
16505  function    ``chkMultiplicationWithConst3``
16901  procedure   ``chkMultiplication``
17419  function    ``chkMultAssignLiteral1``
17536  function    ``chkMultAssignLiteral2``
17663  function    ``chkMultAssignLiteral3``
17798  function    ``chkMultAssignLiteral4``
17915  function    ``chkMultAssignExpression1``
18032  function    ``chkMultAssignExpression2``
18159  function    ``chkMultAssignExpression3``
18294  function    ``chkMultAssignExpression4``
18411  function    ``chkMultAssign1``
18596  procedure   ``chkMultAssign``
18705  function    ``chkDivision_1``
19063  function    ``chkDivision_2``
19109  function    ``chkDivision_3``
19219  function    ``chkDivision_4``
19401  function    ``chkDivision_5``
19484  procedure   ``chkDivision``
19517  function    ``chkRemainder_1``
19875  function    ``chkRemainder_2``
19929  function    ``chkRemainder_3``
20039  function    ``chkRemainder_4``
20221  function    ``chkRemainder_5``
20283  procedure   ``chkRemainder``
20316  function    ``chkDivRem_1``
20410  function    ``chkDivRem_2``
20504  function    ``chkDivRem_3``
20598  function    ``chkDivRem_4``
20692  function    ``chkDivRem_5``
20746  function    ``chkDivRem_6``
20808  procedure   ``chkDivRem``
20845  function    ``chkModDivision_1``
21203  function    ``chkModDivision_2``
21285  function    ``chkModDivision_3``
21395  function    ``chkModDivision_4``
21577  function    ``chkModDivision_5``
21660  procedure   ``chkModDivision``
21693  function    ``chkModulo_1``
22051  function    ``chkModulo_2``
22133  function    ``chkModulo_3``
22243  function    ``chkModulo_4``
22425  function    ``chkModulo_5``
22671  function    ``chkModulo_6``
22750  procedure   ``chkModulo``
22787  function    ``chkPower_1``
23011  function    ``chkPower_2``
23096  function    ``chkPower_3``
23306  function    ``chkPower_4``
23342  function    ``chkPower_5``
23552  function    ``chkPower_6``
23588  function    ``chkPower_7``
23798  function    ``chkPower_8``
23834  function    ``chkPower_9``
24045  procedure   ``chkPower``
24094  procedure   ``chkFactorial``
24138  procedure   ``chkBinom``
24177  function    ``chkCompare_1``
24383  function    ``chkCompare_2``
24589  function    ``chkCompare_3``
24795  function    ``chkCompare_4``
25001  procedure   ``chkCompare``
25030  procedure   ``chkAbs``
25711  procedure   ``chkOdd``
25731  procedure   ``chkRand``
25751  procedure   ``chkGcd``
25768  procedure   ``chkLog2``
26594  procedure   ``chkLog10``
26830  procedure   ``chkBitLength``
26933  procedure   ``chkLowestSetBit``
27165  function    ``chkLShift``
27189  function    ``chkLShift``
27197  procedure   ``chkLShift``
27279  procedure   ``chkLShift``
27583  function    ``chkRShift``
27607  function    ``chkRShift``
27615  procedure   ``chkRShift``
27697  procedure   ``chkRShift``
28001  procedure   ``chkSqrt``
28144  procedure   ``checkSci``
28723  procedure   ``chkConstants``
28820  procedure   ``do_assign``
28836  function    ``peelLowestByte``
28846  procedure   ``chkAssign``
28851  array       ``bigNumArr``
28875  procedure   ``checkTernary``
28930  function    ``randDigitStri``
28944  procedure   ``testConversion``
28963  procedure   ``testConversion``
28974  procedure   ``main``
====== =========== ==================================================

prg/chkbin.sd7
--------------

:Lines:    6469
:Entities: 44

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
33     function    ``intExpr``
37     function    ``bin32Expr``
41     function    ``bin64Expr``
45     function    ``striExpr``
49     function    ``floatExpr``
53     function    ``boolExpr``
57     function    ``bigintExpr``
65     procedure   ``DECLARE_RAISES_RANGE_ERROR``
68     function    ``raisesRangeError``
91     procedure   ``checkShift``
179    procedure   ``checkAnd``
350    procedure   ``checkOr``
393    procedure   ``checkXor``
436    function    ``checkBytesBeWithLength_1``
791    function    ``checkBytesBeWithLength_2``
1146   function    ``checkBytesBeWithLength_3``
1176   procedure   ``checkBytesBeWithLength``
1201   function    ``checkBytesLeWithLength_1``
1556   function    ``checkBytesLeWithLength_2``
1911   function    ``checkBytesLeWithLength_3``
1941   procedure   ``checkBytesLeWithLength``
1966   function    ``checkBytesBe2Bin64_1``
2321   function    ``checkBytesBe2Bin64_2``
2676   function    ``checkBytesBe2Bin64_3``
2734   procedure   ``checkBytesBe2Bin64``
2759   function    ``checkBytesLe2Bin64_1``
3114   function    ``checkBytesLe2Bin64_2``
3469   function    ``checkBytesLe2Bin64_3``
3527   procedure   ``checkBytesLe2Bin64``
3552   procedure   ``checkFloat2Bin16``
3710   procedure   ``checkBin16ToFloat``
3774   procedure   ``checkFloat2Bin32``
3860   procedure   ``checkBin32ToFloat``
3996   procedure   ``checkFloat2Bin64``
4082   procedure   ``checkBin64ToFloat``
4172   procedure   ``checkFloat2MbfBits``
4251   procedure   ``checkMbfBits2Float``
4317   procedure   ``checkBinBinaryWithBigMod``
4751   procedure   ``checkStr``
4973   procedure   ``checkRadix``
5919   procedure   ``checkBitLength``
6193   procedure   ``checkLowestSetBit``
6341   procedure   ``checkTernary``
6442   procedure   ``main``
====== =========== ==================================================

prg/chkbitdata.sd7
------------------

:Lines:    6624
:Entities: 39

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
30     function    ``chkGetBitLsb``
72     procedure   ``chkGetBitLsb``
128    function    ``chkGetBitsLsb``
170    procedure   ``chkGetBitsLsb``
796    function    ``chkPeekBitsLsb``
837    procedure   ``chkPeekBitsLsb``
1474   function    ``chkEofLsb``
1554   procedure   ``chkEofLsb``
2172   function    ``chkGetsLsb``
2221   procedure   ``chkGetsLsb``
2280   function    ``chkCloseLsb``
2312   procedure   ``chkCloseLsb``
2371   function    ``chkGetBitMsb``
2413   procedure   ``chkGetBitMsb``
2451   function    ``chkGetBitsMsb``
2493   procedure   ``chkGetBitsMsb``
3119   function    ``chkPeekBitsMsb``
3160   procedure   ``chkPeekBitsMsb``
3797   function    ``chkEofMsb``
3889   procedure   ``chkEofMsb``
4507   function    ``chkGetsMsb``
4560   procedure   ``chkGetsMsb``
4619   function    ``chkCloseMsb``
4651   procedure   ``chkCloseMsb``
4710   procedure   ``initBitStream``
4722   function    ``chkPutBitLsb``
4745   procedure   ``chkPutBitLsb``
4782   function    ``chkPutBitsLsb``
4806   procedure   ``chkPutBitsLsb``
5464   function    ``chkLengthTruncateLsb``
5496   procedure   ``chkLengthTruncateLsb``
5656   procedure   ``initBitStream``
5668   function    ``chkPutBitMsb``
5691   procedure   ``chkPutBitMsb``
5728   function    ``chkPutBitsMsb``
5752   procedure   ``chkPutBitsMsb``
6410   function    ``chkLengthTruncateMsb``
6442   procedure   ``chkLengthTruncateMsb``
6602   procedure   ``main``
====== =========== ==================================================

prg/chkbool.sd7
---------------

:Lines:    3157
:Entities: 98

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
37     function    ``boolExpr``
45     function    ``intExpr``
49     function    ``raisesNumericError``
64     procedure   ``check_not``
118    procedure   ``check_and``
261    procedure   ``check_and_assign``
283    procedure   ``check_or``
402    procedure   ``check_or_assign``
424    procedure   ``check_xor_assign``
446    procedure   ``check_eq_expr``
468    procedure   ``check_eq_ref_param``
490    procedure   ``check_eq_in_param``
512    procedure   ``check_eq_local_var``
537    procedure   ``check_eq_global_var``
559    procedure   ``check_eq_global_const``
581    procedure   ``check_eq_literal``
603    procedure   ``check_eq``
621    procedure   ``check_ne_expr``
643    procedure   ``check_ne_ref_param``
665    procedure   ``check_ne_in_param``
687    procedure   ``check_ne_local_var``
712    procedure   ``check_ne_global_var``
734    procedure   ``check_ne_global_const``
756    procedure   ``check_ne_literal``
778    procedure   ``check_ne``
796    procedure   ``check_gt_expr``
818    procedure   ``check_gt_ref_param``
840    procedure   ``check_gt_in_param``
862    procedure   ``check_gt_local_var``
887    procedure   ``check_gt_global_var``
909    procedure   ``check_gt_global_const``
931    procedure   ``check_gt_literal``
953    procedure   ``check_gt``
971    procedure   ``check_ge_expr``
993    procedure   ``check_ge_ref_param``
1015   procedure   ``check_ge_in_param``
1037   procedure   ``check_ge_local_var``
1062   procedure   ``check_ge_global_var``
1084   procedure   ``check_ge_global_const``
1106   procedure   ``check_ge_literal``
1128   procedure   ``check_ge``
1146   procedure   ``check_lt_expr``
1168   procedure   ``check_lt_ref_param``
1190   procedure   ``check_lt_in_param``
1212   procedure   ``check_lt_local_var``
1237   procedure   ``check_lt_global_var``
1259   procedure   ``check_lt_global_const``
1281   procedure   ``check_lt_literal``
1303   procedure   ``check_lt``
1321   procedure   ``check_le_expr``
1343   procedure   ``check_le_ref_param``
1365   procedure   ``check_le_in_param``
1387   procedure   ``check_le_local_var``
1412   procedure   ``check_le_global_var``
1434   procedure   ``check_le_global_const``
1456   procedure   ``check_le_literal``
1478   procedure   ``check_le``
1496   procedure   ``check_relations``
1507   procedure   ``check_compare``
1529   procedure   ``check_ord_expr``
1543   procedure   ``check_ord_ref_param``
1557   procedure   ``check_ord_in_param``
1571   procedure   ``check_ord_local_var``
1588   procedure   ``check_ord_global_var``
1602   procedure   ``check_ord_global_const``
1616   procedure   ``check_ord_literal``
1630   procedure   ``check_ord``
1648   procedure   ``check_conv_literal``
1670   procedure   ``check_conv_local_const``
1695   procedure   ``check_conv_local_var``
1720   procedure   ``check_conv_ref_param``
1742   procedure   ``check_conv_in_param``
1764   procedure   ``check_conv``
1914   procedure   ``check_str_expr``
1928   procedure   ``check_str_ref_param``
1942   procedure   ``check_str_in_param``
1956   procedure   ``check_str_local_var``
1973   procedure   ``check_str_global_var``
1987   procedure   ``check_str_global_const``
2001   procedure   ``check_str_literal``
2015   procedure   ``check_str``
2033   procedure   ``check_succ_in_param``
2056   procedure   ``check_succ_in_var_param``
2079   procedure   ``check_succ_val_param``
2102   procedure   ``check_succ_ref_param``
2125   procedure   ``check_succ_inout_param``
2148   procedure   ``check_succ``
2248   procedure   ``check_pred_in_param``
2271   procedure   ``check_pred_in_var_param``
2294   procedure   ``check_pred_val_param``
2317   procedure   ``check_pred_ref_param``
2340   procedure   ``check_pred_inout_param``
2363   procedure   ``check_pred``
2463   function    ``is_odd``
2466   function    ``is_even``
2470   function    ``is_odd``
2474   procedure   ``check_ternary``
3138   procedure   ``main``
====== =========== ==================================================

prg/chkbst.sd7
--------------

:Lines:    722
:Entities: 19

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
29     procedure   ``&:=``
32     function    ``bstriExpr``
36     function    ``intExpr``
40     function    ``boolExpr``
44     function    ``raisesIndexError``
59     function    ``raisesRangeError``
74     procedure   ``check_bstring_parse``
144    procedure   ``check_bstring_length``
178    procedure   ``do_assign``
194    function    ``peelFirstChar``
204    procedure   ``check_bstring_assign``
209    array       ``bstriArr``
231    procedure   ``do_append``
237    procedure   ``check_bstring_append``
268    procedure   ``check_bstring_comparisons``
469    procedure   ``check_bstring_index``
629    procedure   ``check_bstring_constants``
653    procedure   ``check_bstring_ternary``
710    procedure   ``main``
====== =========== ==================================================

prg/chkchr.sd7
--------------

:Lines:    2809
:Entities: 24

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
30     function    ``charExpr``
34     function    ``boolExpr``
38     function    ``intExpr``
42     function    ``raisesRangeError``
57     procedure   ``check_literal``
168    procedure   ``check_conversions``
226    procedure   ``check_functions``
272    procedure   ``check_isLetter``
802    function    ``check_width_1``
1128   function    ``check_width_2``
1457   procedure   ``check_width``
1474   function    ``check_literal_function_1``
1672   function    ``check_literal_function_2``
1778   function    ``check_literal_function_3``
1884   function    ``literalFunction``
1888   function    ``check_literal_function_4``
2086   function    ``check_c_literal_function_1``
2284   function    ``check_c_literal_function_2``
2390   function    ``check_c_literal_function_3``
2496   function    ``cLiteralFunction``
2500   function    ``check_c_literal_function_4``
2698   procedure   ``check_literal_function``
2742   procedure   ``check_ternary``
2797   procedure   ``main``
====== =========== ==================================================

prg/chkcmd.sd7
--------------

:Lines:    1205
:Entities: 22

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
31     function    ``raisesFileError``
45     function    ``raisesFileError``
59     function    ``raisesFileError``
73     function    ``raisesFileError``
87     function    ``raisesFileError``
101    function    ``raisesFileError``
113    structure   ``fileProperties``
123    function    ``=``
132    function    ``<>``
141    function    ``getFileProperties``
154    procedure   ``checkRemoveFile``
191    procedure   ``checkRemoveTree``
233    procedure   ``checkCopyFile``
422    procedure   ``checkCloneFile``
661    procedure   ``checkMoveFile``
861    function    ``checkDanglingSymlink``
932    function    ``checkSymlinkToRegularFile``
1062   procedure   ``checkSymlink``
1079   function    ``randomString``
1091   function    ``randomNameNotInEnvironment``
1110   procedure   ``check_environment``
1191   procedure   ``main``
====== =========== ==================================================

prg/chkdb.sd7
-------------

:Lines:    7454
:Entities: 117

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
36     structure   ``connectData``
45     function    ``connectData``
58     structure   ``testState``
67     function    ``testState``
82     function    ``raisesRangeError``
94     function    ``raisesDatabaseError``
106    procedure   ``DECL_INSERT``
109    procedure   ``insert``
127    procedure   ``insert``
148    procedure   ``insInf``
175    procedure   ``insNaN``
202    procedure   ``insert``
236    procedure   ``insert``
255    procedure   ``insert``
276    procedure   ``insert``
295    procedure   ``create``
334    procedure   ``testPrepareAndExecute``
674    function    ``changeParameter``
691    procedure   ``testAssignDatabase``
694    array       ``dbArr``
723    function    ``changeParameter``
734    procedure   ``testAssignStatement``
737    array       ``statementArr``
748    procedure   ``testFieldNames``
848    procedure   ``testWithAutoCommit``
932    procedure   ``testAutoCommit``
1002   procedure   ``testWithoutAutoCommit``
1116   procedure   ``testTransactions``
1194   procedure   ``testBooleanField``
1197   array       ``expect``
1324   procedure   ``testInt8Field``
1327   array       ``expect``
1510   procedure   ``testInt16Field``
1513   array       ``expect``
1673   procedure   ``testInt32Field``
1676   array       ``expect``
1838   procedure   ``testInt64Field``
1841   array       ``expect``
2041   procedure   ``testBigIntField``
2044   array       ``expect``
2161   procedure   ``testFloatField``
2164   array       ``expect``
2315   procedure   ``testDoubleField``
2318   array       ``expect``
2469   procedure   ``testBigRatField``
2472   array       ``expect``
2760   procedure   ``testAnyIntegerField``
2763   array       ``expect``
3072   procedure   ``testDecimalIntField``
3090   procedure   ``testNumericIntField``
3108   procedure   ``testAnyRationalField``
3113   array       ``expect``
3306   procedure   ``testDecimalField``
3329   procedure   ``testNumericField``
3352   procedure   ``testChar1AsciiControlField``
3355   array       ``expect``
3513   procedure   ``testChar1AsciiField``
3516   array       ``expect``
3696   procedure   ``testChar1Latin1C1ControlField``
3699   array       ``expect``
3831   procedure   ``testChar1Latin1Field``
3834   array       ``expect``
4062   procedure   ``testCodePageField``
4066   array       ``expect``
4336   procedure   ``testChar1Field``
4339   array       ``expect``
4658   procedure   ``testCharField``
4661   array       ``expect``
4827   procedure   ``testCharField2``
4830   array       ``expect``
4940   procedure   ``testVarcharField``
4943   array       ``expect``
5111   procedure   ``testBlobField``
5114   array       ``expect``
5240   procedure   ``testClobField``
5243   array       ``expect``
5347   procedure   ``testDateField``
5350   array       ``expect``
5523   procedure   ``testTimeField``
5526   array       ``expect``
5616   procedure   ``testDateTimeField``
5619   array       ``expect``
5792   procedure   ``testTimeStampField``
5795   array       ``expect``
6018   procedure   ``testTimeStampTzField``
6021   array       ``expect``
6254   procedure   ``testPositiveYearMonthDurationField``
6257   array       ``expect``
6320   procedure   ``testNegativeYearMonthDurationField``
6323   array       ``expect``
6384   procedure   ``testPositiveDurationField``
6387   array       ``expect``
6459   procedure   ``testNegativeDurationField``
6462   array       ``expect``
6530   procedure   ``testPositiveFractionDurationField``
6533   array       ``expect``
6623   procedure   ``testNegativeFractionDurationField``
6626   array       ``expect``
6716   procedure   ``testCombinedDurationField``
6719   array       ``expect``
6797   procedure   ``testAdvancedDurationField``
6800   array       ``expect``
6878   procedure   ``testFloatField2``
6913   procedure   ``testBigRatField2``
6947   procedure   ``testBlobField2``
7006   procedure   ``testTimeField2``
7009   array       ``expect``
7063   procedure   ``testDurationField2``
7066   array       ``expect``
7120   procedure   ``testNumericField2``
7177   procedure   ``testVarcharField2``
7180   array       ``expect``
7257   procedure   ``testDb``
7316   function    ``failed``
7333   procedure   ``testDatabase``
7360   procedure   ``main``
7362   array       ``dbConnectDataList``
====== =========== ==================================================

prg/chkdecl.sd7
---------------

:Lines:    448
:Entities: 9

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
59     procedure   ``XX``
74     procedure   ``YY``
88     procedure   ``WW``
94     procedure   ``UU``
98     procedure   ``ZZ``
282    procedure   ``AA``
286    procedure   ``BB``
373    procedure   ``recursive``
398    procedure   ``main``
====== =========== ==================================================

prg/chkenum.sd7
---------------

:Lines:    1230
:Entities: 21

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
28     enumeration ``directionType``
33     function    ``enumExpr``
37     function    ``boolExpr``
41     function    ``intExpr``
45     procedure   ``DECLARE_RAISES_RANGE_ERROR``
48     function    ``raisesRangeError``
69     function    ``checkEq``
191    function    ``checkNe``
313    function    ``checkLt``
435    function    ``checkLe``
557    function    ``checkGe``
679    function    ``checkGt``
801    procedure   ``checkComparisons``
830    procedure   ``checkCompare``
956    procedure   ``checkOrd``
984    procedure   ``checkConv``
1041   procedure   ``checkSuccAndPred``
1095   procedure   ``checkIncrAndDecr``
1137   procedure   ``checkLiteralFunction``
1165   procedure   ``checkTernary``
1218   procedure   ``main``
====== =========== ==================================================

prg/chkerr.sd7
--------------

:Lines:    4663
:Entities: 92

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
30     procedure   ``show``
41     function    ``checkError``
77     function    ``checkError1``
84     function    ``checkError2``
91     function    ``checkError``
125    function    ``checkError1``
132    function    ``checkError2``
139    function    ``checkEofEncountered``
174    function    ``checkCharIllegal``
312    function    ``checkCommentOpen``
337    function    ``checkScannerErrors``
352    function    ``checkFileNotFound``
399    function    ``checkWrongPathDelimiter``
423    function    ``checkEssentialIncludeFailed``
438    function    ``checkIllegalPragma``
461    function    ``checkIncludeAndPragmaErrors``
477    function    ``checkWrongAction``
493    function    ``checkWrongSystem``
508    function    ``checkDollarValueWrong``
554    function    ``checkDollarTypeWrong``
600    function    ``checkSystemMainMissing``
615    function    ``checkBasicDeclarationErrors``
632    function    ``checkCardDecimalTooBig``
691    function    ``checkNegativeExponent``
726    function    ``checkDigitExpected``
781    function    ``checkCardWithExponentTooBig``
824    function    ``checkBase2To36Allowed``
859    function    ``checkExtDigitExpected``
1010   function    ``checkIllegalBaseDigit``
1293   function    ``checkCardBasedTooBig``
1584   function    ``checkNumericLiteralErrors``
1604   function    ``checkApostrophExpected``
1627   function    ``checkCharExceeds``
1654   function    ``checkWrongQuotationRepresentation``
1669   function    ``checkStringEscape``
1868   function    ``checkWrongNumericalEscape``
1911   function    ``checkNumericalEscapeTooBig``
1942   function    ``checkBackslashExpected``
2005   function    ``checkStringExceeds``
2048   function    ``checkStringAndCharLiteralErrors``
2068   function    ``checkNameExpected``
2103   function    ``checkCardExpected``
2214   function    ``checkStriExpected``
2250   function    ``checkIdentExpected``
2308   function    ``checkTypeExpected``
2375   function    ``checkProcExpected``
2624   function    ``checkParamSpecifierExpected``
2640   function    ``checkParamDeclOrSymbolExpected``
2664   function    ``checkExceptionExpected``
2720   function    ``checkExprExpected``
2744   function    ``checkExpectedSymbol``
2842   function    ``checkExpectedErrors``
2865   function    ``checkParamDeclFailed``
2898   function    ``checkDeclFailed``
2968   function    ``checkObjTwiceDeclared``
3002   function    ``checkPreviousDeclaration``
3036   function    ``checkExceptionRaised``
3110   function    ``checkDeclarationErrors``
3127   function    ``checkIllegalAssociativity``
3183   function    ``checkIllegalPriority``
3210   function    ``checkTwoParameterSyntax``
3241   function    ``checkSyntaxDeclaredTwice``
3264   function    ``checkDotExprExpected``
3280   function    ``checkRedeclaredInfixPriority``
3303   function    ``checkRedeclaredPrefixPriority``
3332   function    ``checkWrongExprParamPriority``
3361   function    ``checkWrongPrefixPriority``
3378   function    ``checkDotExprIllegal``
3395   function    ``checkSyntaxDeclarationErrors``
3417   function    ``checkNoMatch``
3582   function    ``checkWrongAccessRight``
3699   function    ``checkLiteralTypeUndefined``
3730   function    ``checkKindOfParamUndefined``
3753   function    ``checkSemanticErrors``
3769   function    ``checkOverlongUtf8Encoding``
3900   function    ``checkUtf16SurrogateChar``
3959   function    ``checkCharNotUnicode``
4042   function    ``checkUTF8ContinuationByte1``
4085   function    ``checkUTF8ContinuationByte2``
4160   function    ``checkUTF8ContinuationByte3``
4203   function    ``checkUTF8ContinuationByte4``
4230   function    ``checkUTF8ContinuationByte5``
4305   function    ``checkUTF8ContinuationByte6``
4348   function    ``checkUTF8ContinuationByte7``
4375   function    ``checkUTF8ContinuationByte8``
4402   function    ``checkUTF8ContinuationByte9``
4477   function    ``checkUTF8ContinuationByte10``
4520   function    ``checkUnexpectedUtf8ContinuationByte``
4547   function    ``checkSolitaryUtf8StartByte``
4598   function    ``checkUtf16ByteOrderMarkFound``
4617   function    ``checkUnicodeErrors``
4645   procedure   ``main``
====== =========== ==================================================

prg/chkexc.sd7
--------------

:Lines:    2627
:Entities: 19

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
36     procedure   ``f1``
42     procedure   ``f2``
48     procedure   ``f3``
54     procedure   ``f4``
60     function    ``test_func``
64     function    ``intExpr``
68     procedure   ``check_ord_and_conv``
118    procedure   ``check_integer_exponentiation``
439    procedure   ``check_integer``
840    function    ``bigIntExpr``
844    procedure   ``check_bigInteger_exponentiation``
1165   procedure   ``check_bigInteger``
1309   procedure   ``check_float``
1523   procedure   ``check_string``
1759   procedure   ``check_array``
1765   array       ``arr``
1766   array       ``constantArray``
2074   procedure   ``check_file``
2445   procedure   ``main``
====== =========== ==================================================

prg/chkfil.sd7
--------------

:Lines:    1615
:Entities: 37

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
56     procedure   ``DECLARE_RAISES_RANGE_ERROR``
59     function    ``raisesRangeError``
81     procedure   ``DECLARE_RAISES_FILE_ERROR``
84     function    ``raisesFileError``
109    procedure   ``check_clib_file``
113    array       ``fileArr``
150    function    ``genTempClibFile``
180    function    ``changeParameter``
191    procedure   ``check_clib_file_assign``
198    array       ``fileArr``
234    function    ``genTempFile``
264    function    ``changeParameter``
275    procedure   ``check_file_assign``
282    array       ``fileArr``
318    procedure   ``check_file_open``
368    function    ``check_file_io_1``
483    function    ``check_file_io_2``
598    function    ``check_file_io_3``
717    procedure   ``check_file_io``
751    procedure   ``check_file_seek``
885    procedure   ``check_file_append``
934    procedure   ``check_automatic_close``
983    function    ``check_use_after_close1``
1030   function    ``check_use_after_close2``
1101   procedure   ``check_use_after_close``
1142   procedure   ``check_utf8_file_open``
1192   procedure   ``check_utf8_io``
1273   procedure   ``check_utf8_seek``
1376   procedure   ``check_automatic_close_utf8``
1427   procedure   ``check_use_after_close_utf8``
1468   structure   ``myFile``
1473   function    ``openMyFile``
1483   procedure   ``write``
1489   procedure   ``check_my_file``
1498   procedure   ``check_null_file``
1576   procedure   ``check_keybd_and_console``
1593   procedure   ``main``
====== =========== ==================================================

prg/chkflt.sd7
--------------

:Lines:    20620
:Entities: 91

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
36     function    ``striExpr``
40     function    ``floatExpr``
44     function    ``intExpr``
48     function    ``boolExpr``
52     procedure   ``DECLARE_RAISES_RANGE_ERROR``
55     function    ``raisesRangeError``
77     procedure   ``check_literal``
360    function    ``check_equal_and_not_equal``
910    function    ``check_less_than_and_greater_than``
1460   function    ``check_less_equal_and_greater_equal``
2010   procedure   ``check_comparison``
2100   procedure   ``check_compare``
2452   function    ``check_parse_operator``
2993   function    ``check_parse_function``
3534   procedure   ``check_parse``
3553   function    ``check_str_1``
4204   function    ``check_str_scientific``
4794   function    ``check_digits``
4908   function    ``check_sci``
5144   procedure   ``check_str``
5172   procedure   ``check_conversion``
5636   procedure   ``check_trunc``
5830   function    ``check_round_1``
5995   function    ``check_round_2``
6160   procedure   ``check_round``
6190   function    ``changeParameter``
6199   procedure   ``check_assign``
6204   array       ``intArr``
6222   procedure   ``check_add``
6277   function    ``check_mult_with_ipower_base_minus_infinity``
6575   function    ``check_mult_with_ipower_base_minus_0``
6885   function    ``check_mult_with_ipower_base_0``
7195   function    ``check_mult_with_ipower_base_1``
7451   function    ``check_mult_with_ipower_base_2``
7716   function    ``check_mult_with_ipower_base_4``
7981   function    ``check_mult_with_ipower_base_infinity``
8291   function    ``check_mult_minus_infinity_with_ipower_base_2``
8479   function    ``check_mult_minus_zero_with_ipower_base_2``
8667   function    ``check_mult_zero_with_ipower_base_2``
8855   function    ``check_mult_infinity_with_ipower_base_2``
9043   function    ``check_mult_nan_with_ipower_base_2``
9231   function    ``check_mult_minus_infinity_with_ipower_base_4``
9419   function    ``check_mult_minus_zero_with_ipower_base_4``
9607   function    ``check_mult_zero_with_ipower_base_4``
9795   function    ``check_mult_infinity_with_ipower_base_4``
9983   function    ``check_mult_nan_with_ipower_base_4``
10171  procedure   ``check_mult``
10374  procedure   ``check_division``
10399  procedure   ``check_remainder``
10823  procedure   ``check_modulo``
11247  function    ``check_power_base_minus_0``
11617  function    ``check_power_base_0``
11987  function    ``check_power_base_1``
12085  function    ``check_power_base_2``
12247  procedure   ``check_power``
12796  function    ``check_ipower_base_minus_infinity``
12878  function    ``check_ipower_base_minus_2``
13114  function    ``check_ipower_base_minus_1``
13196  function    ``check_ipower_base_minus_0``
13608  function    ``check_ipower_base_0``
14020  function    ``check_ipower_base_1``
14102  function    ``check_ipower_base_2``
14322  function    ``check_ipower_base_4``
14534  function    ``check_ipower_base_infinity``
14616  function    ``check_ipower_base_nan``
14958  procedure   ``check_ipower``
15121  function    ``check_lshift_minus_infinity``
15327  function    ``check_lshift_minus_0``
15533  function    ``check_lshift_0``
15739  function    ``check_lshift_infinity``
15945  function    ``check_lshift_nan``
16151  procedure   ``check_lshift``
16545  function    ``check_rshift_minus_infinity``
16751  function    ``check_rshift_minus_0``
16957  function    ``check_rshift_0``
17163  function    ``check_rshift_infinity``
17369  function    ``check_rshift_nan``
17575  procedure   ``check_rshift``
17969  procedure   ``check_functions``
18050  procedure   ``check_negative_zero``
18075  function    ``check_inf_1``
18162  function    ``check_inf_2``
18216  function    ``check_inf_3``
18260  function    ``check_inf_4``
18509  function    ``check_inf_5``
18900  procedure   ``check_inf``
18932  procedure   ``check_nan``
20245  procedure   ``check_rand``
20416  procedure   ``check_decompose``
20461  procedure   ``check_ternary``
20590  procedure   ``main``
====== =========== ==================================================

prg/chkhent.sd7
---------------

:Lines:    54
:Entities: 2

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
31     procedure   ``main``
33     array       ``fileNames``
====== =========== ==================================================

prg/chkhsh.sd7
--------------

:Lines:    4548
:Entities: 27

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
36     enumeration ``enumerationType``
249    procedure   ``DECLARE_RAISES_INDEX_ERROR``
252    function    ``raisesIndexError``
286    procedure   ``DECLARE_RAISES_RANGE_ERROR``
289    function    ``raisesRangeError``
309    procedure   ``chkBooleanHash``
613    procedure   ``chkIntegerHash``
918    procedure   ``chkBigIntegerHash``
1223   procedure   ``chkRationalHash``
1528   procedure   ``chkBigRationalHash``
1833   procedure   ``chkFloatHash``
2138   procedure   ``chkComplexHash``
2443   procedure   ``chkCharHash``
2748   procedure   ``chkStringHash``
3053   procedure   ``chkBstringHash``
3358   procedure   ``chkBitsetHash``
3663   procedure   ``chkTimeHash``
3968   procedure   ``chkEnumerationTypeHash``
4273   procedure   ``chkKeysFunction``
4277   array       ``hashKeys``
4303   procedure   ``chkValuesFunction``
4307   array       ``hashValues``
4333   procedure   ``chkForLoop``
4379   procedure   ``chkAssignmentToItself``
4397   procedure   ``chkHashLiteral``
4508   procedure   ``chkInlineHashKeys``
4525   procedure   ``main``
====== =========== ==================================================

prg/chkidx.sd7
--------------

:Lines:    19567
:Entities: 56

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
30     function    ``striExpr``
34     function    ``intExpr``
38     function    ``charExpr``
42     procedure   ``DECLARE_RAISES_INDEX_ERROR``
45     function    ``raisesIndexError``
68     procedure   ``check_string_index``
312    function    ``headOfEmptyString``
316    function    ``headOfString1``
320    function    ``headFunction``
324    function    ``headFunction``
328    procedure   ``check_string_head``
652    function    ``tailOfEmptyString``
656    function    ``tailOfString1``
660    function    ``tailFunction``
664    function    ``tailFunction``
668    procedure   ``check_string_tail``
1010   function    ``check_string_range_expr_1``
1880   function    ``check_string_range_expr_2``
2750   function    ``check_string_range_expr_3``
2913   function    ``check_string_range_assign_1``
3785   function    ``check_string_range_assign_2``
4657   function    ``check_string_range_assign_3``
4822   function    ``rangeFunction``
4826   function    ``check_string_range_func_1``
5696   function    ``check_string_range_func_2``
6566   procedure   ``check_string_range``
6868   function    ``check_string_substr_expr_1``
7706   function    ``check_string_substr_expr_2``
8544   function    ``check_string_substr_assign_1``
9356   function    ``check_string_substr_assign_2``
10168  function    ``substrFunction``
10172  function    ``substrFunction``
10176  function    ``check_string_substr_func_1``
11048  function    ``check_string_substr_func_2``
11455  function    ``check_string_substr_func_3``
11862  procedure   ``check_string_substr``
11968  function    ``check_string_substr_fixLen_empty_1``
12278  function    ``check_string_substr_fixLen_empty_2``
12588  function    ``check_string_substr_fixLen_empty_3``
12900  function    ``check_string_substr_fixLen_empty_4``
13212  function    ``check_string_substr_fixLen_expr_1``
13986  function    ``check_string_substr_fixLen_expr_2``
14760  function    ``check_string_substr_fixLen_assign_1``
15536  function    ``check_string_substr_fixLen_assign_2``
16312  function    ``substrFixLen``
16316  function    ``substrFixLen``
16320  function    ``check_string_substr_fixLen_func_1``
17500  function    ``check_string_substr_fixLen_func_2``
18576  procedure   ``check_string_substr_fixLen``
18642  function    ``check_string_eq_of_substr_fixlen_1``
18736  function    ``check_string_eq_of_substr_fixlen_2``
18829  function    ``check_string_eq_of_substr_fixlen_3``
18942  function    ``check_string_eq_of_substr_fixlen_4``
19036  procedure   ``check_string_eq_of_substr_fixlen``
19058  procedure   ``check_string_assign_at``
19552  procedure   ``main``
====== =========== ==================================================

prg/chkint.sd7
--------------

:Lines:    38129
:Entities: 193

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
38     function    ``intExpr``
42     function    ``striExpr``
46     function    ``charExpr``
50     function    ``boolExpr``
54     procedure   ``DECLARE_RAISES_RANGE_ERROR``
57     function    ``raisesRangeError``
78     procedure   ``DECLARE_RAISES_NUMERIC_ERROR``
81     function    ``raisesNumericError``
102    function    ``raisesIndexError``
117    procedure   ``check_exponent_integer_literal``
178    procedure   ``check_based_integer_literal``
359    procedure   ``check_compare``
712    procedure   ``check_bytesBe``
1241   procedure   ``check_bytesLe``
1770   function    ``check_bytesSignedBeWithLength``
2214   function    ``check_bytesUnsignedBeWithLength_1``
2597   function    ``check_bytesUnsignedBeWithLength_2``
3078   procedure   ``check_bytesBeWithLength``
3101   function    ``check_bytesSignedLeWithLength``
3545   function    ``check_bytesUnsignedLeWithLength_1``
3928   function    ``check_bytesUnsignedLeWithLength_2``
4409   procedure   ``check_bytesLeWithLength``
4432   function    ``check_bytes2IntSignedBe_len_optimization``
4792   function    ``check_bytes2IntSignedBe_fixLen_optimization``
5152   function    ``check_bytes2IntSignedBe``
5393   function    ``check_bytes2IntUnsignedBe_len_optimization``
5717   function    ``check_bytes2IntUnsignedBe_fixLen_optimization``
6041   function    ``check_bytes2IntUnsignedBe``
6254   procedure   ``check_bytes2IntBe``
6275   function    ``check_bytes2IntSignedLe_len_optimization``
6675   function    ``check_bytes2IntSignedLe_fixLen_optimization``
7075   function    ``check_bytes2IntSignedLe``
7317   function    ``check_bytes2IntUnsignedLe_len_optimization``
7641   function    ``check_bytes2IntUnsignedLe_fixLen_optimization``
7965   function    ``check_bytes2IntUnsignedLe``
8179   procedure   ``check_bytes2IntLe``
8200   procedure   ``check_leb128``
8608   function    ``changeParameter``
8617   procedure   ``check_assign``
8622   array       ``intArr``
8640   procedure   ``check_negation``
8677   function    ``check_reduced_overflow_checking_for_sums_1``
9083   function    ``check_reduced_overflow_checking_for_sums_2``
9334   function    ``check_reduced_overflow_checking_for_sums_3``
9559   procedure   ``check_add``
10272  function    ``check_add_assign_minimum_result``
10278  array       ``arr``
10481  function    ``check_add_assign_maximum_result``
10487  array       ``arr``
10690  procedure   ``check_add_assign``
10695  array       ``arr``
10805  procedure   ``check_subtract``
11414  function    ``check_subtract_assign_minimum_result``
11420  array       ``arr``
11619  function    ``check_subtract_assign_maximum_result``
11625  array       ``arr``
11828  procedure   ``check_subtract_assign``
11833  array       ``arr``
11950  procedure   ``check_succ``
12016  procedure   ``check_pred``
12082  procedure   ``check_incr``
12085  array       ``arr``
12147  procedure   ``check_decr``
12150  array       ``arr``
12212  function    ``check_mult_positive_product``
12418  function    ``check_mult_negative_product``
12608  function    ``check_mult_minimum_product``
12846  function    ``check_mult_minimum_plus_1_product``
13076  function    ``check_mult_maximum_product``
13310  function    ``check_reduced_overflow_checking_for_mult``
13452  function    ``check_mult_with_power_base_0``
13908  function    ``check_mult_with_power_base_1``
14344  function    ``check_mult_with_power_base_2``
14970  procedure   ``check_mult``
15086  function    ``check_mult_assign_positive_product``
15092  array       ``arr``
15311  function    ``check_mult_assign_negative_product``
15317  array       ``arr``
15520  function    ``check_mult_assign_minimum_product_1``
15652  function    ``check_mult_assign_minimum_product_2``
15657  array       ``arr``
15784  function    ``check_mult_assign_minimum_plus_1_product_1``
15912  function    ``check_mult_assign_minimum_plus_1_product_2``
15917  array       ``arr``
16040  function    ``check_mult_assign_maximum_product_1``
16168  function    ``check_mult_assign_maximum_product_2``
16173  array       ``arr``
16296  procedure   ``check_mult_assign``
16301  array       ``arr``
16452  procedure   ``check_division``
16535  function    ``check_div_with_small_numbers_1``
16755  function    ``check_div_with_small_numbers_2``
16975  function    ``check_div_with_small_numbers_3``
17195  function    ``check_div_with_small_numbers_4``
17415  function    ``check_div_min_max``
17585  function    ``check_div_by_zero``
17652  function    ``check_div_optimization_1``
17992  function    ``check_div_optimization_2``
18184  function    ``check_div_optimization_3``
18227  function    ``check_div_optimization_4``
18513  function    ``check_div_division_check_optimization``
19070  function    ``check_div_of_product_optimization``
19291  function    ``check_mdiv_of_product_optimization``
19512  procedure   ``check_div``
19576  function    ``check_rem_with_small_numbers_1``
19796  function    ``check_rem_with_small_numbers_2``
20016  function    ``check_rem_with_small_numbers_3``
20236  function    ``check_rem_with_small_numbers_4``
20456  function    ``check_rem_min_max``
20626  function    ``check_rem_by_zero``
20701  function    ``check_rem_optimization_1``
21041  function    ``check_rem_optimization_2``
21233  function    ``check_rem_optimization_3``
21382  function    ``check_rem_optimization_4``
21518  function    ``check_rem_division_check_optimization``
21680  procedure   ``check_rem``
21736  function    ``check_mdiv_with_small_numbers_1``
21956  function    ``check_mdiv_with_small_numbers_2``
22176  function    ``check_mdiv_with_small_numbers_3``
22396  function    ``check_mdiv_with_small_numbers_4``
22616  function    ``check_mdiv_min_max``
22786  function    ``check_mdiv_powers_of_two``
23068  function    ``check_mdiv_by_zero``
23135  function    ``check_mdiv_optimization_1``
23475  function    ``check_mdiv_optimization_2``
23667  function    ``check_mdiv_optimization_3``
23946  function    ``check_mdiv_optimization_4``
24232  function    ``check_mdiv_division_check_optimization``
24789  procedure   ``check_mdiv``
24849  function    ``check_mod_with_small_numbers_1``
25069  function    ``check_mod_with_small_numbers_2``
25289  function    ``check_mod_with_small_numbers_3``
25509  function    ``check_mod_with_small_numbers_4``
25729  function    ``check_mod_min_max``
26147  function    ``check_mod_by_power_of_two_optimization``
26546  function    ``check_mod_by_computed_power_of_two_optimization``
26992  function    ``check_mod_optimization_1``
27332  function    ``check_mod_optimization_2``
27524  function    ``check_mod_optimization_3``
27803  function    ``check_mod_by_zero``
27870  function    ``check_mod_division_check_optimization``
28427  procedure   ``check_mod``
28487  procedure   ``check_odd``
28507  procedure   ``check_fact``
28550  procedure   ``check_binom``
30308  function    ``check_power_1``
31642  function    ``check_power_with_constant_base``
31980  function    ``check_power_with_constant_exponent``
32096  function    ``check_power_optimization_with_ranges``
32194  procedure   ``check_power``
32283  procedure   ``check_abs``
32326  procedure   ``check_rand``
32413  procedure   ``check_sqrt``
32710  procedure   ``check_log2``
32980  procedure   ``check_log10``
33074  function    ``check_reduced_overflow_checking_for_lshift``
33208  function    ``check_lshift_by_sum``
33254  function    ``chkLShift``
33270  function    ``chkLShift``
33289  procedure   ``chkLShift``
33361  function    ``check_lShift_assign_1``
33459  function    ``check_lShift_assign_2``
33464  array       ``arr``
33512  function    ``check_lShift_assign_3``
33517  array       ``arr``
33565  function    ``check_lShift_assign_4``
33665  function    ``check_lShift_assign_5``
33670  array       ``arr``
33719  function    ``check_lShift_assign_6``
33724  array       ``arr``
33773  procedure   ``check_lShift``
34171  function    ``chkRShift``
34187  function    ``chkRShift``
34201  procedure   ``chkRShift``
34253  function    ``check_rShift_assign_1``
34315  function    ``check_rShift_assign_2``
34320  array       ``arr``
34377  procedure   ``check_rShift``
34405  function    ``check_radix``
34800  function    ``strFunction``
34804  procedure   ``check_str``
35177  function    ``check_sci_negative``
35457  function    ``check_sci_positive``
35746  procedure   ``check_sci``
35776  procedure   ``check_parse``
36639  procedure   ``check_lpad0``
37195  function    ``check_bitLength_1``
37457  function    ``check_bitLength_2``
37719  procedure   ``check_bitLength``
37737  procedure   ``check_lowestSetBit``
37828  function    ``decrementAndCheck``
37837  procedure   ``check_ternary``
38079  procedure   ``main``
====== =========== ==================================================

prg/chkjson.sd7
---------------

:Lines:    1764
:Entities: 27

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
35     enumeration ``directionType``
82     structure   ``stringIntegerStruct2``
87     function    ``mapJsonElementName``
91     function    ``mapStructElementName``
98     procedure   ``DECLARE_RAISES_RANGE_ERROR``
101    function    ``raisesRangeError``
127    procedure   ``DECLARE_RAISES_ILLEGAL_ACTION``
130    function    ``raisesIllegalAction``
154    function    ``initScan``
163    function    ``getJsonStringOkay``
217    function    ``getJsonStringRaisesRangeError``
240    function    ``checkGetJsonString1``
396    procedure   ``checkGetJsonString``
410    function    ``getJsonNumberOkay``
464    function    ``getJsonNumberRaisesRangeError``
487    function    ``checkGetJsonNumber1``
848    procedure   ``checkGetJsonNumber``
862    procedure   ``checkGetJsonSymbol``
904    procedure   ``checkJsonDom``
1148   procedure   ``checkStructElementFunctions``
1193   procedure   ``DECLARE_CHECK_PARSE_JSON``
1196   function    ``checkParseJson``
1233   function    ``parseJsonRaisesRangeError``
1279   procedure   ``checkParseJson``
1335   procedure   ``checkFromJson``
1548   procedure   ``checkToJson``
1752   procedure   ``main``
====== =========== ==================================================

prg/chkovf.sd7
--------------

:Lines:    8216
:Entities: 66

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
36     function    ``intExpr``
40     function    ``striExpr``
44     function    ``charExpr``
48     function    ``boolExpr``
52     function    ``raisesOverflowError``
67     function    ``raisesOverflowError``
82     function    ``raisesOverflowError``
95     function    ``exceptionName``
110    function    ``exceptionName``
123    procedure   ``check_negation``
147    function    ``check_reduced_overflow_checking_for_sums``
503    procedure   ``check_add``
975    function    ``check_add_assign_underflow``
981    array       ``arr``
1176   function    ``check_add_assign_overflow``
1182   array       ``arr``
1377   procedure   ``check_add_assign``
1382   array       ``arr``
1469   procedure   ``check_subtract``
1889   function    ``check_subtract_assign_underflow``
1895   array       ``arr``
2086   function    ``check_subtract_assign_overflow``
2092   array       ``arr``
2291   procedure   ``check_subtract_assign``
2296   array       ``arr``
2357   procedure   ``check_succ``
2375   procedure   ``check_pred``
2393   procedure   ``check_incr``
2396   array       ``arr``
2410   procedure   ``check_decr``
2413   array       ``arr``
2431   function    ``check_reduced_overflow_checking_for_mult``
2600   function    ``check_mult_with_power_base_2``
3135   procedure   ``check_mult``
3846   function    ``check_mult_assign_underflow_1``
4042   function    ``check_mult_assign_underflow_2``
4047   array       ``arr``
4238   function    ``check_mult_assign_overflow_1``
4436   function    ``check_mult_assign_overflow_2``
4441   array       ``arr``
4634   procedure   ``check_mult_assign``
4639   array       ``arr``
4689   procedure   ``check_div``
4735   procedure   ``check_rem``
4768   procedure   ``check_mdiv``
4814   procedure   ``check_mod``
4976   function    ``check_reduced_overflow_checking_for_lshift``
5050   function    ``check_lshift_by_sum``
5120   procedure   ``check_lShift``
5872   function    ``check_lShift_assign_1``
6082   function    ``check_lShift_assign_2``
6087   array       ``arr``
6292   procedure   ``check_lShift_assign``
6312   procedure   ``check_rShift``
6440   procedure   ``check_rShift_assign``
6445   array       ``arr``
6540   procedure   ``check_ulShift``
6668   procedure   ``check_ulShift_assign``
6673   array       ``arr``
6768   procedure   ``check_urShift``
6896   procedure   ``check_urShift_assign``
6901   array       ``arr``
6996   procedure   ``check_power``
7340   procedure   ``check_binom``
8164   procedure   ``check_abs``
8185   procedure   ``main``
====== =========== ==================================================

prg/chkprc.sd7
--------------

:Lines:    10111
:Entities: 40

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
40     enumeration ``enumerationType``
50     function    ``boolExpr``
58     function    ``enumExpr``
62     function    ``striExpr``
66     function    ``bstriExpr``
70     function    ``intExpr``
74     function    ``charExpr``
78     function    ``bigintExpr``
86     function    ``floatExpr``
90     function    ``rationalExpr``
94     function    ``setExpr``
98     function    ``typeExpr``
102    procedure   ``check_if``
946    procedure   ``check_while``
950    array       ``array_loop_control``
1164   procedure   ``check_repeat``
1168   array       ``array_loop_control``
1351   procedure   ``check_for_int_to``
1581   procedure   ``check_for_int_downto``
1811   procedure   ``check_for_int_to_until``
2060   procedure   ``check_for_int_downto_until``
2313   procedure   ``check_for_char_to``
2477   procedure   ``check_for_char_downto``
2640   procedure   ``check_for_char_to_until``
2889   procedure   ``check_for_char_downto_until``
3142   procedure   ``check_for_bool_to``
3205   procedure   ``check_for_bool_downto``
3268   procedure   ``check_for_big_to``
3313   procedure   ``check_case_boolean``
3601   procedure   ``check_case_int``
4267   procedure   ``check_case_enum``
4851   procedure   ``check_case_char``
5503   procedure   ``check_case_string``
6155   procedure   ``check_case_bstring``
6807   procedure   ``check_case_bigint``
7459   procedure   ``check_case_float``
8111   procedure   ``check_case_rational``
8777   procedure   ``check_case_bitset``
9429   procedure   ``check_case_type``
10081  procedure   ``main``
====== =========== ==================================================

prg/chkscan.sd7
---------------

:Lines:    714
:Entities: 20

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
31     function    ``initScan``
40     procedure   ``checkGetDigits``
72     procedure   ``checkGetHexDigits``
104    procedure   ``checkGetInteger``
138    procedure   ``checkGetNumber``
178    procedure   ``checkGetNonDigits``
210    procedure   ``checkGetQuotedText``
246    procedure   ``checkGetCommandLineWord``
302    procedure   ``checkGetSimpleStringLiteral``
348    procedure   ``checkGetLetters``
380    procedure   ``checkGetName``
412    procedure   ``checkSkipSpace``
442    procedure   ``checkSkipSpaceOrTab``
472    procedure   ``checkSkipWhiteSpace``
502    procedure   ``checkGetWhiteSpace``
532    procedure   ``checkGetWord``
584    procedure   ``checkSkipLine``
612    procedure   ``checkGetLine``
640    procedure   ``checkGetXmlTagHeadOrContent``
692    procedure   ``main``
====== =========== ==================================================

prg/chkset.sd7
--------------

:Lines:    11974
:Entities: 157

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
31     set         ``uc_letter_char``
32     set         ``lc_letter_char``
33     set         ``letter_char``
34     set         ``digit_char``
35     set         ``alphanum_char``
36     set         ``special_char``
40     set         ``name_start_char``
41     set         ``name_char``
48     function    ``intExpr``
52     function    ``charExpr``
56     function    ``striExpr``
60     function    ``boolExpr``
64     function    ``genSet``
74     set         ``aSet``
80     function    ``genSet``
88     procedure   ``DECLARE_RAISES_RANGE_ERROR``
91     function    ``raisesRangeError``
113    procedure   ``check_literal``
185    procedure   ``check_conv``
337    procedure   ``check_card``
345    set         ``test_set2``
346    set         ``constStriSet1``
347    set         ``constStriSet2``
689    procedure   ``check_rand``
697    set         ``test_set2``
698    set         ``constStriSet1``
699    set         ``constStriSet2``
814    procedure   ``check_incl``
821    array       ``compare_set``
861    procedure   ``check_excl``
896    function    ``compareTest``
902    procedure   ``check_compare``
1782   function    ``relationTest``
1794   function    ``relationTests1``
2077   function    ``relationTests2``
2480   function    ``relationTests3``
2883   function    ``relationTests4``
3286   procedure   ``check_relations``
3445   function    ``check_union_1``
3650   function    ``check_union_2``
3968   function    ``check_union_3``
4201   function    ``check_union_4``
4238   function    ``check_union_5``
4304   procedure   ``check_union``
4337   function    ``check_union_assign_1``
4460   function    ``check_union_assign_2``
4555   function    ``check_union_assign_3``
4706   function    ``check_union_assign_4``
4886   function    ``check_union_assign_5``
5009   function    ``check_union_assign_6``
5160   function    ``check_union_assign_7``
5228   procedure   ``check_union_assign``
5269   function    ``check_symdiff_1``
5474   function    ``check_symdiff_2``
5791   function    ``check_symdiff_3``
6024   function    ``check_symdiff_4``
6061   procedure   ``check_symdiff``
6090   function    ``check_intersection_1``
6295   function    ``check_intersection_2``
6612   function    ``check_intersection_3``
6845   function    ``check_intersection_4``
6882   function    ``check_intersection_5``
6948   procedure   ``check_intersection``
6981   function    ``check_intersection_assign_1``
7104   function    ``check_intersection_assign_2``
7199   function    ``check_intersection_assign_3``
7350   function    ``check_intersection_assign_4``
7529   function    ``check_intersection_assign_5``
7652   function    ``check_intersection_assign_6``
7803   function    ``check_intersection_assign_7``
7871   procedure   ``check_intersection_assign``
7912   function    ``check_difference_1``
8117   function    ``check_difference_2``
8434   function    ``check_difference_3``
8667   function    ``check_difference_4``
8704   function    ``check_difference_5``
8770   function    ``check_difference_6``
8790   procedure   ``check_difference``
8827   function    ``check_difference_assign_1``
8950   function    ``check_difference_assign_2``
9045   function    ``check_difference_assign_3``
9196   function    ``check_difference_assign_4``
9375   function    ``check_difference_assign_5``
9498   function    ``check_difference_assign_6``
9649   function    ``check_difference_assign_7``
9717   procedure   ``check_difference_assign``
9758   function    ``check_elem1``
10012  function    ``check_elem2``
10238  function    ``check_elem3``
10291  function    ``check_elem_with_range_0_1``
10386  function    ``check_elem_with_range_0_63``
10581  procedure   ``check_elem``
10793  procedure   ``check_elem_char``
10797  set         ``set_a``
10798  set         ``set_b``
10799  set         ``set_a_b``
10800  set         ``set_b_c``
10801  set         ``set_a_b_c``
10802  set         ``set_b_c_d``
10803  set         ``set_a_e_g``
10804  set         ``set_b_e_g``
10805  set         ``set_a_b_c_d``
10806  set         ``set_b_c_d_e``
10807  set         ``set_a_c_e_g``
10808  set         ``set_b_c_e_g``
10809  set         ``set_a_b_c_d_e``
10810  set         ``set_b_c_d_e_f``
10811  set         ``set_a_c_e_g_k``
10812  set         ``set_b_c_e_g_k``
10813  set         ``set_a_unicode``
10814  set         ``set_b_unicode``
10815  set         ``set_1_3_5``
10816  set         ``set_2_3_5``
10817  set         ``set_1_3_5_7``
10818  set         ``set_2_3_5_7``
10819  set         ``set_1_3_5_7_9``
10820  set         ``set_2_3_5_7_9``
10821  set         ``set_1_unicode``
10822  set         ``set_2_unicode``
11096  procedure   ``check_elem_string``
11100  set         ``set_a``
11101  set         ``set_b``
11102  set         ``set_a_b``
11103  set         ``set_b_c``
11104  set         ``set_a_b_c``
11105  set         ``set_b_c_d``
11106  set         ``set_a_e_g``
11107  set         ``set_b_e_g``
11108  set         ``set_a_b_c_d``
11109  set         ``set_b_c_d_e``
11110  set         ``set_a_c_e_g``
11111  set         ``set_b_c_e_g``
11112  set         ``set_a_b_c_d_e``
11113  set         ``set_b_c_d_e_f``
11114  set         ``set_a_c_e_g_k``
11115  set         ``set_b_c_e_g_k``
11116  set         ``set_a_unicode``
11117  set         ``set_b_unicode``
11118  set         ``set_1_3_5``
11119  set         ``set_2_3_5``
11120  set         ``set_1_3_5_7``
11121  set         ``set_2_3_5_7``
11122  set         ``set_1_3_5_7_9``
11123  set         ``set_2_3_5_7_9``
11124  set         ``set_1_unicode``
11125  set         ``set_2_unicode``
11398  procedure   ``check_to_array``
11424  procedure   ``check_min``
11475  procedure   ``check_max``
11526  procedure   ``check_next``
11594  procedure   ``check_str``
11645  function    ``peelMinimum``
11658  procedure   ``check_assign``
11662  array       ``arr_1``
11709  procedure   ``check_charset``
11711  array       ``special_char_list``
11944  procedure   ``main``
====== =========== ==================================================

prg/chkstr.sd7
--------------

:Lines:    26952
:Entities: 156

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
33     function    ``striExpr``
37     function    ``intExpr``
41     function    ``charExpr``
45     function    ``boolExpr``
49     procedure   ``DECLARE_RAISES_RANGE_ERROR``
52     function    ``raisesRangeError``
74     procedure   ``check_string_literal``
326    function    ``check_string_eq``
500    function    ``check_string_ne``
674    function    ``check_string_gt``
1088   function    ``check_string_ge``
1502   function    ``check_string_lt``
1916   function    ``check_string_le``
2330   function    ``check_string_comparisons_1``
2357   function    ``check_string_compare``
2361   array       ``arr1``
2362   array       ``arr2``
2487   function    ``check_eq_of_substr_fixlen``
2639   procedure   ``check_string_comparisons``
2685   procedure   ``check_string_length``
3139   procedure   ``check_string_index``
3314   function    ``check_string_head_assign``
3499   function    ``headOfEmptyString``
3503   function    ``headOfString1``
3507   function    ``headFunction``
3511   function    ``headFunction1``
3515   function    ``headFunction2``
3519   function    ``headFunction``
3523   function    ``check_string_head_func``
3739   procedure   ``check_string_head``
4009   function    ``check_string_tail_assign``
4182   function    ``tailOfEmptyString``
4186   function    ``tailOfString1``
4190   function    ``tailFunction``
4194   function    ``tailFunction``
4198   function    ``tailFunction``
4202   function    ``check_string_tail_func``
4390   procedure   ``check_string_tail``
4637   function    ``check_string_range_expr_1``
4939   function    ``check_string_range_expr_2``
5241   function    ``check_string_range_expr_3``
5617   function    ``check_string_range_assign_1``
5929   function    ``check_string_range_assign_2``
6241   function    ``check_string_range_assign_3``
6550   function    ``check_string_range_assign_4``
6634   function    ``rangeFunction``
6638   function    ``check_string_range_func_1``
6940   function    ``check_string_range_func_2``
7242   procedure   ``check_string_range``
7288   function    ``check_string_substr_expr_1``
7734   function    ``check_string_substr_expr_2``
8180   function    ``check_string_substr_expr_3``
8466   function    ``check_string_substr_expr_4``
8612   function    ``check_string_substr_assign_1``
8844   function    ``check_string_substr_assign_2``
9076   function    ``check_string_substr_assign_3``
9308   function    ``check_string_substr_assign_4``
9540   function    ``check_string_substr_assign_5``
9692   function    ``check_string_substr_assign_6``
9844   function    ``check_string_substr_assign_7``
9996   function    ``substrFunction``
10000  function    ``substrFunction``
10004  function    ``check_string_substr_func_1``
10090  function    ``check_string_substr_func_2``
10316  function    ``check_string_substr_func_3``
10542  function    ``check_string_substr_func_4``
10768  function    ``check_string_substr_func_5``
10994  procedure   ``check_string_substr``
11068  function    ``check_string_substr_fixLen_expr_1``
11206  function    ``check_string_substr_fixLen_expr_2``
11344  function    ``check_string_substr_fixLen_assign_1``
11492  function    ``check_string_substr_fixLen_assign_2``
11640  function    ``substrFixLen``
11644  function    ``substrFixLen``
11648  function    ``check_string_substr_fixLen_func_1``
11826  function    ``check_string_substr_fixLen_func_2``
11964  procedure   ``check_string_substr_fixLen``
12022  function    ``check_string_concat_1``
12396  function    ``check_string_concat_2``
12770  procedure   ``check_string_concat``
12920  function    ``check_string_mult_1``
13401  function    ``check_string_mult_2``
13882  function    ``check_string_mult_3``
14363  function    ``check_string_mult_4``
14844  function    ``check_string_mult_5``
15086  function    ``check_string_mult_6``
15163  procedure   ``check_string_mult``
15197  function    ``check_string_lpad``
15369  function    ``check_string_lpad0``
15729  function    ``check_string_rpad``
15901  procedure   ``check_string_ops``
15923  function    ``check_string_pos_1``
16274  function    ``check_string_pos_2``
16625  function    ``check_string_pos_3``
16976  function    ``check_string_pos_4``
17327  function    ``check_string_pos_5``
17811  procedure   ``check_string_pos``
17841  function    ``check_string_char_pos_1``
17951  function    ``check_string_char_pos_2``
18061  function    ``check_string_char_pos_3``
18171  function    ``check_string_char_pos_4``
18281  function    ``check_string_char_pos_5``
18446  procedure   ``check_string_char_pos``
18476  procedure   ``check_string_pos_idx``
19412  function    ``check_string_rpos_1``
19580  function    ``check_string_rpos_2``
19748  function    ``check_string_rpos_3``
19916  function    ``check_string_rpos_4``
20084  procedure   ``check_string_rpos``
20175  procedure   ``check_string_char_rpos``
20558  procedure   ``check_string_rpos_idx``
21609  procedure   ``check_string_replace``
21875  function    ``check_utf8_conversions``
22388  procedure   ``check_string_unicode_conversons``
22512  procedure   ``check_string_case_conversions``
23027  function    ``check_string_trim``
23239  function    ``check_string_ltrim``
23453  function    ``check_string_rtrim``
23667  function    ``check_string_str``
23679  procedure   ``check_string_funcs``
23705  procedure   ``check_string_split``
24324  procedure   ``check_string_join``
24596  procedure   ``do_assign``
24602  function    ``stringFromChar``
24616  function    ``peelFirstChar``
24626  procedure   ``check_string_assign``
24632  array       ``striArr``
24737  procedure   ``do_append``
24743  function    ``check_string_append_self_ref1``
24758  procedure   ``test1``
24789  procedure   ``do_append``
24795  function    ``check_string_append_self_ref2_short``
24850  function    ``check_string_append_self_ref2_long``
24905  function    ``check_string_append_to_empty``
24950  function    ``check_string_append_mult``
25002  function    ``check_string_append_1``
25196  function    ``check_string_append_2``
25412  function    ``returnStringWithCapacity``
25421  function    ``check_string_append_3``
25427  array       ``arr``
25487  procedure   ``check_string_append``
25529  function    ``check_string_assign_at_with_char``
25535  array       ``arr``
25688  function    ``check_string_assign_at_with_string_1``
25860  function    ``check_string_assign_at_with_string_2``
25951  function    ``check_string_assign_at_with_string_3``
26084  function    ``check_string_assign_at_with_string_4``
26089  array       ``arr``
26174  function    ``check_string_assign_at_with_string_5``
26179  array       ``arr``
26264  function    ``check_string_assign_at_with_string_mult_1``
26436  function    ``check_string_assign_at_with_string_mult_2``
26608  procedure   ``check_string_assign_at``
26650  procedure   ``check_string_for``
26840  procedure   ``check_string_ternary``
26916  procedure   ``main``
====== =========== ==================================================

prg/chktime.sd7
---------------

:Lines:    2025
:Entities: 16

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
29     procedure   ``DECLARE_RAISES_RANGE_ERROR``
32     function    ``raisesRangeError``
53     procedure   ``checkISO8601ToTimeConversion``
461    function    ``checkTimeToStringConversion_1``
612    function    ``checkTimeToStringConversion_2``
763    function    ``checkTimeToStringConversion_3``
1070   procedure   ``checkTimeToStringConversion``
1092   procedure   ``checkLeapYearFunctions``
1212   procedure   ``checkTimestamp1970``
1282   procedure   ``checkTimestamp1601``
1350   procedure   ``checkJulianDayNumber``
1408   procedure   ``checkTrunc``
1448   procedure   ``checkWeekDate``
1482   procedure   ``checkAwait``
1623   procedure   ``checkSetLocalTZ``
2011   procedure   ``main``
====== =========== ==================================================

prg/chktoml.sd7
---------------

:Lines:    1656
:Entities: 17

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
30     procedure   ``DECLARE_RAISES_RANGE_ERROR``
33     function    ``raisesRangeError``
53     function    ``initScan``
62     function    ``getTomlSymbolOkay``
116    function    ``getTomlSymbolRaisesRangeError``
139    function    ``checkGetTomlBasicString``
368    function    ``checkGetTomlMultiLineBasicString``
629    function    ``checkGetTomlLiteralString``
841    function    ``checkGetTomlMultiLineLiteralString``
954    procedure   ``checkGetTomlString``
1036   procedure   ``checkGetTomlInteger``
1231   procedure   ``checkGetTomlFloat``
1378   procedure   ``checkGetTomlDate``
1479   procedure   ``checkGetTomlSymbol``
1505   function    ``getTomlKeyOkay``
1559   procedure   ``checkGetTomlKey``
1646   procedure   ``main``
====== =========== ==================================================

prg/clock.sd7
-------------

:Lines:    47
:Entities: 1

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
31     procedure   ``main``
====== =========== ==================================================

prg/clock2.sd7
--------------

:Lines:    43
:Entities: 1

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
32     procedure   ``main``
====== =========== ==================================================

prg/clock3.sd7
--------------

:Lines:    95
:Entities: 1

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
38     procedure   ``main``
====== =========== ==================================================

prg/cmpfil.sd7
--------------

:Lines:    84
:Entities: 1

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
28     procedure   ``main``
====== =========== ==================================================

prg/comanche.sd7
----------------

:Lines:    180
:Entities: 2

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
39     procedure   ``main``
45     array       ``args``
====== =========== ==================================================

prg/confval.sd7
---------------

:Lines:    175
:Entities: 3

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
31     function    ``noEmptyArray``
33     array       ``resultArray``
41     procedure   ``main``
====== =========== ==================================================

prg/db7.sd7
-----------

:Lines:    417
:Entities: 11

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
43     function    ``failed``
63     function    ``fetchLines``
98     procedure   ``displayMessage``
119    procedure   ``displayMessage``
138    procedure   ``displayResult``
188    procedure   ``doExecute``
219    procedure   ``doCatalog``
221    array       ``tableNames``
293    procedure   ``doStatements``
343    function    ``doLogin``
401    procedure   ``main``
====== =========== ==================================================

prg/diff7.sd7
-------------

:Lines:    263
:Entities: 7

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
30     procedure   ``showLines``
41     procedure   ``changeMessage``
56     procedure   ``diffContent``
58     array       ``lines1``
59     array       ``lines2``
161    procedure   ``diff``
228    procedure   ``main``
====== =========== ==================================================

prg/dirtst.sd7
--------------

:Lines:    42
:Entities: 1

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
29     procedure   ``main``
====== =========== ==================================================

prg/dirx.sd7
------------

:Lines:    152
:Entities: 3

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
31     function    ``wildcard_match``
71     procedure   ``main``
75     array       ``names``
====== =========== ==================================================

prg/dnafight.sd7
----------------

:Lines:    1381
:Entities: 67

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
82     structure   ``lifeForm``
89     structure   ``bacterium``
98     function    ``.``
99     function    ``.``
100    function    ``.``
101    function    ``.``
102    function    ``.``
103    function    ``.``
104    function    ``.``
105    function    ``.``
107    structure   ``position``
113    array       ``area``
114    array       ``animates``
115    array       ``children``
120    enumeration ``killReason``
147    function    ``str``
151    structure   ``statRecord``
159    array       ``statValues``
162    array       ``killarray``
165    array       ``REASON``
195    function    ``display_color``
197    function    ``upper_char``
204    function    ``lower_char``
215    array       ``fieldWin``
218    array       ``diffx``
220    array       ``diffy``
227    procedure   ``resetStat``
236    procedure   ``incrKillStat``
243    function    ``continue``
263    procedure   ``setclass``
289    procedure   ``writeParameters``
300    procedure   ``initScreen``
372    procedure   ``readLimits``
376    array       ``intValue``
428    function    ``charCol``
455    procedure   ``initDisplay``
468    function    ``ranDir``
479    function    ``shrinkSize``
490    function    ``nextSize``
506    function    ``newBacterium``
523    procedure   ``create``
534    procedure   ``setBact``
543    procedure   ``die``
557    procedure   ``move``
585    procedure   ``digest``
611    procedure   ``eatat``
623    function    ``strength``
639    function    ``view``
648    function    ``food``
657    function    ``hunger``
666    procedure   ``doWait``
679    procedure   ``eat``
702    procedure   ``kill``
738    procedure   ``split``
779    procedure   ``setAllBacterials``
803    procedure   ``writeInfo``
829    function    ``readCommand``
848    procedure   ``InitAnimates``
980    procedure   ``initArea``
1010   procedure   ``statistics``
1124   procedure   ``finalStatistics``
1133   array       ``colField``
1134   array       ``valueField``
1201   procedure   ``initStatistics``
1238   procedure   ``execute``
1272   procedure   ``generation``
1293   procedure   ``main``
====== =========== ==================================================

prg/dragon.sd7
--------------

:Lines:    73
:Entities: 4

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
35     procedure   ``turn``
40     procedure   ``forward``
52     procedure   ``dragon``
65     procedure   ``main``
====== =========== ==================================================

prg/echo.sd7
------------

:Lines:    39
:Entities: 1

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
29     procedure   ``main``
====== =========== ==================================================

prg/eliza.sd7
-------------

:Lines:    302
:Entities: 6

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
29     array       ``keyword_table``
38     array       ``replace_words``
48     array       ``example_sentence``
191    array       ``keyword_assignment``
198    array       ``current_sentence``
201    procedure   ``main``
====== =========== ==================================================

prg/err.sd7
-----------

:Lines:    96
:Entities: 0

(none)


prg/fannkuch.sd7
----------------

:Lines:    131
:Entities: 5

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
31     procedure   ``fannkuch``
44     array       ``p``
45     array       ``q``
46     array       ``s``
118    procedure   ``main``
====== =========== ==================================================

prg/fib.sd7
-----------

:Lines:    47
:Entities: 2

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
28     function    ``fib``
39     procedure   ``main``
====== =========== ==================================================

prg/find7.sd7
-------------

:Lines:    133
:Entities: 4

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
31     procedure   ``writeMatchingLines``
55     procedure   ``searchDir``
58     array       ``dirElements``
97     procedure   ``main``
====== =========== ==================================================

prg/findchar.sd7
----------------

:Lines:    149
:Entities: 5

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
33     procedure   ``doFind``
71     procedure   ``doFindRecursive``
75     array       ``dir``
92     procedure   ``main``
94     array       ``arg_v``
====== =========== ==================================================

prg/fractree.sd7
----------------

:Lines:    55
:Entities: 2

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
33     procedure   ``drawTree``
47     procedure   ``main``
====== =========== ==================================================

prg/ftp7.sd7
------------

:Lines:    296
:Entities: 12

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
35     procedure   ``writeHelp``
44     procedure   ``longFileListing``
46     array       ``dir``
55     array       ``monthName``
99     procedure   ``fileListing``
101    array       ``dir``
122    procedure   ``directoryListing``
124    array       ``dir``
145    procedure   ``copyFtpFile``
171    procedure   ``execute``
173    array       ``dir``
241    procedure   ``main``
====== =========== ==================================================

prg/ftpserv.sd7
---------------

:Lines:    74
:Entities: 1

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
30     procedure   ``main``
====== =========== ==================================================

prg/gcd.sd7
-----------

:Lines:    109
:Entities: 3

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
29     function    ``gcd``
44     function    ``binaryGcd``
91     procedure   ``main``
====== =========== ==================================================

prg/gkbd.sd7
------------

:Lines:    358
:Entities: 5

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
47     array       ``buttonColor``
51     set         ``mouseKeys``
58     procedure   ``displayKey``
174    procedure   ``checkKey``
186    procedure   ``main``
====== =========== ==================================================

prg/gtksvtst.sd7
----------------

:Lines:    94
:Entities: 3

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
30     procedure   ``show_message``
40     procedure   ``main_window``
81     procedure   ``main``
====== =========== ==================================================

prg/hal.sd7
-----------

:Lines:    250
:Entities: 14

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
42     procedure   ``write_camera``
58     procedure   ``focus_0``
68     procedure   ``focus_1``
78     procedure   ``focus_2``
88     procedure   ``focus_3``
98     procedure   ``focus_4``
108    procedure   ``write_sys_0``
122    procedure   ``write_sys_1``
134    procedure   ``write_shp``
149    procedure   ``write_antenna``
164    procedure   ``write_com``
178    procedure   ``write_lrn_0``
192    procedure   ``write_lrn_1``
203    procedure   ``main``
====== =========== ==================================================

prg/hamu.sd7
------------

:Lines:    573
:Entities: 17

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
42     procedure   ``read_number``
74     procedure   ``title``
94     procedure   ``think_it_over``
103    procedure   ``init_game``
120    procedure   ``state_of_the_nation``
154    procedure   ``buy_soil``
185    procedure   ``sell_soil``
214    procedure   ``support_population``
242    procedure   ``cultivate_soil``
288    procedure   ``budget``
344    procedure   ``work_on_soil``
367    procedure   ``population_growth``
425    procedure   ``statistics``
441    procedure   ``evaluation``
504    procedure   ``final_report``
530    procedure   ``game``
554    procedure   ``main``
====== =========== ==================================================

prg/hanoi.sd7
-------------

:Lines:    55
:Entities: 3

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
29     procedure   ``hanoi``
39     procedure   ``hanoi``
48     procedure   ``main``
====== =========== ==================================================

prg/hd.sd7
----------

:Lines:    79
:Entities: 1

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
29     procedure   ``main``
====== =========== ==================================================

prg/hello.sd7
-------------

:Lines:    32
:Entities: 1

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
28     procedure   ``main``
====== =========== ==================================================

prg/hilbert.sd7
---------------

:Lines:    108
:Entities: 7

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
31     procedure   ``drawDown``
32     procedure   ``drawUp``
34     procedure   ``drawRight``
50     procedure   ``drawLeft``
66     procedure   ``drawDown``
82     procedure   ``drawUp``
98     procedure   ``main``
====== =========== ==================================================

prg/ide7.sd7
------------

:Lines:    196
:Entities: 4

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
83     procedure   ``loadFile``
89     array       ``dirContent``
118    procedure   ``saveFileAs``
144    procedure   ``main``
====== =========== ==================================================

prg/kbd.sd7
-----------

:Lines:    49
:Entities: 1

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
31     procedure   ``main``
====== =========== ==================================================

prg/klondike.sd7
----------------

:Lines:    883
:Entities: 56

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
50     function    ``suit``
51     function    ``rank``
52     function    ``visible``
53     procedure   ``setVisible``
54     procedure   ``putCard``
60     structure   ``cardType``
73     function    ``suit``
77     function    ``rank``
81     function    ``visible``
85     procedure   ``setVisible``
96     procedure   ``putCard``
110    structure   ``cardDeck``
111    array       ``cards``
116    function    ``cardsInDeck``
120    function    ``genCardImage``
135    function    ``createCardDeck``
151    procedure   ``shuffle``
153    array       ``shuffled``
163    function    ``dealCard``
172    function    ``selectCard``
186    function    ``cardBelow``
226    structure   ``stateType``
235    function    ``dealCard``
245    procedure   ``dealCards``
269    procedure   ``recycleWaste``
282    function    ``cardIndex``
301    function    ``searchCard``
321    function    ``getSubPile``
336    procedure   ``toTop``
346    function    ``isAtPlace``
365    function    ``isAtDestPlace``
369    function    ``atDestPlace``
384    function    ``cardFitsToDest``
390    procedure   ``removeFromDest``
399    procedure   ``appendToDest``
410    function    ``isFinished``
427    function    ``isAtPilePlace``
433    function    ``atFreePilePlace``
449    function    ``cardFitsToPile``
456    procedure   ``removeFromPile``
463    procedure   ``appendToPile``
479    procedure   ``removeFromOldPlace``
505    procedure   ``move``
530    procedure   ``move``
561    procedure   ``move``
585    procedure   ``move``
613    function    ``selectCard``
624    procedure   ``processMouseClick``
710    function    ``findCard``
726    procedure   ``createSolution``
728    array       ``wholeSuit``
748    function    ``jumpingCard``
751    function    ``jumpingCard``
794    procedure   ``victoryAnimation``
815    function    ``playGame``
871    procedure   ``main``
====== =========== ==================================================

prg/lander.sd7
--------------

:Lines:    1551
:Entities: 57

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
53     structure   ``rocket_type``
74     structure   ``point``
79     array       ``landscape``
80     array       ``advanced_landscape``
82     array       ``explosion_data``
96     array       ``COS_ANG``
97     array       ``SIN_ANG``
110    structure   ``lineCoordinates``
117    array       ``ship``
118    array       ``flames``
120    structure   ``tonetype``
132    function    ``frequency``
145    function    ``tone``
155    array       ``BLUE``
309    array       ``STAR``
394    procedure   ``sound``
400    procedure   ``nosound``
406    procedure   ``delay``
412    procedure   ``doBeep``
418    procedure   ``welcome``
479    procedure   ``turn_sound_on``
485    procedure   ``play_soundstep``
512    procedure   ``pause_game``
547    procedure   ``check_keyboard``
594    procedure   ``readHiScore``
609    procedure   ``writeHiScore``
622    function    ``genLine``
634    procedure   ``addNoiseToLandscape``
652    procedure   ``load``
786    procedure   ``calculate_height``
800    procedure   ``init_display``
847    procedure   ``drawLandscape``
850    array       ``pointxy``
877    procedure   ``setup``
930    procedure   ``drawLine``
944    procedure   ``ship_picture``
966    procedure   ``display_gauges``
1034   procedure   ``display_ship``
1059   procedure   ``drawLogo``
1062   array       ``logo``
1081   procedure   ``setupAdvancedLander``
1139   procedure   ``prepare_message``
1148   procedure   ``too_high``
1160   procedure   ``outside_operating_area``
1172   procedure   ``revise_control``
1228   procedure   ``anounceAdvancedLander``
1274   procedure   ``end_of_flight``
1309   procedure   ``play_stars_and_stripes``
1328   procedure   ``crash_sound``
1347   procedure   ``explosion``
1366   procedure   ``crash_landing``
1386   procedure   ``good_landing``
1440   procedure   ``fast_landing``
1462   procedure   ``tilt_landing``
1477   procedure   ``ground_contact``
1498   procedure   ``crash_test``
1521   procedure   ``main``
====== =========== ==================================================

prg/lst80bas.sd7
----------------

:Lines:    344
:Entities: 5

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
31     array       ``tokenTable``
158    procedure   ``appendToken``
175    function    ``decodeTokenizedLine``
296    procedure   ``writeLines``
317    procedure   ``main``
====== =========== ==================================================

prg/lst99bas.sd7
----------------

:Lines:    401
:Entities: 8

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
31     array       ``tokenTable``
163    procedure   ``appendToken``
180    function    ``decodeTokenizedLine``
248    function    ``convertIntVar254``
278    function    ``searchStartOfBasicFile``
305    procedure   ``writeLines``
356    procedure   ``writeLines``
374    procedure   ``main``
====== =========== ==================================================

prg/lstgwbas.sd7
----------------

:Lines:    577
:Entities: 12

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
33     array       ``tokenTable1``
151    array       ``tokenTable2``
164    array       ``tokenTable3``
206    array       ``tokenTable4``
246    procedure   ``appendToken``
256    function    ``floatLiteral``
302    function    ``decodeTokenizedLine``
492    procedure   ``unprotect``
494    array       ``patterns1``
498    array       ``patterns2``
526    procedure   ``writeLines``
550    procedure   ``main``
====== =========== ==================================================

prg/mahjong.sd7
---------------

:Lines:    1943
:Entities: 82

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
75     function    ``colorScale``
78     array       ``colorScale``
97     array       ``leftFrameColor``
98     array       ``rightFrameColor``
99     array       ``nearFrameColor``
100    array       ``farFrameColor``
102    array       ``nearLeftEdgeColor``
107    array       ``digit_pixmap``
109    structure   ``tileType``
110    array       ``pattern``
116    structure   ``fieldType``
124    structure   ``positionType``
130    structure   ``moveType``
137    array       ``field``
138    array       ``demoMoves``
139    array       ``playerMoves``
297    array       ``zero``
316    array       ``one``
335    array       ``two``
354    array       ``three``
373    array       ``four``
391    array       ``five``
410    array       ``six``
429    array       ``seven``
448    array       ``eight``
467    array       ``nine``
486    procedure   ``writeButtons``
508    function    ``getCommand``
540    function    ``genTile``
548    array       ``tiles``
599    procedure   ``draw``
615    procedure   ``putFrame``
659    procedure   ``drawLeftSide``
682    procedure   ``drawNearSide``
710    procedure   ``drawTile``
748    procedure   ``drawBoardWithFlush``
775    procedure   ``drawBoard``
800    function    ``getMaximumLevelLeftward``
821    function    ``getMaximumLevelDownward``
842    procedure   ``markFrame``
890    procedure   ``unmarkFrame``
903    procedure   ``mark``
913    procedure   ``unmark``
923    procedure   ``unmarkAll``
943    procedure   ``refresh``
979    procedure   ``remove``
991    procedure   ``insert``
1000   procedure   ``draw_number``
1022   function    ``covered``
1046   function    ``getFirstAccessibleTileColumn``
1075   function    ``getLastAccessibleTileColumn``
1104   function    ``getAccessibleTilePositions``
1106   array       ``accessibleTilePositions``
1127   function    ``countPossibleMoves``
1131   array       ``accessibleTilePositions``
1154   array       ``accessibleTilePositions``
1180   procedure   ``showPossibleMatches``
1182   array       ``accessibleTilePositions``
1203   procedure   ``updateNumbers``
1210   procedure   ``showDemo``
1212   array       ``backupField``
1255   procedure   ``locateTile``
1289   function    ``legalMove``
1320   procedure   ``playerMove``
1384   procedure   ``playerTurn``
1450   procedure   ``initLayout``
1469   function    ``getFirstFreeColumn``
1505   function    ``getLastFreeColumn``
1543   function    ``lineIsFree``
1563   function    ``getAnyFreeColumn``
1596   function    ``getFreeColumn``
1625   function    ``getFreePlaceColumn``
1665   procedure   ``getFreePlace``
1678   procedure   ``dealOneTile``
1689   procedure   ``removeOneTile``
1712   procedure   ``dealTilePair``
1757   procedure   ``initTiles``
1783   procedure   ``dealTiles``
1820   procedure   ``dealTiles2``
1842   procedure   ``startNewGame``
1863   procedure   ``writeCentered``
1870   procedure   ``main``
====== =========== ==================================================

prg/make7.sd7
-------------

:Lines:    121
:Entities: 2

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
31     procedure   ``main``
39     array       ``targets``
====== =========== ==================================================

prg/mandelbr.sd7
----------------

:Lines:    237
:Entities: 6

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
47     array       ``colorTable``
52     function    ``iterate``
68     procedure   ``displayMandelbrotSet``
83     procedure   ``showHelp``
137    procedure   ``doCommand``
170    procedure   ``main``
====== =========== ==================================================

prg/mind.sd7
------------

:Lines:    443
:Entities: 11

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
30     structure   ``guess``
41     array       ``guesses``
44     procedure   ``read_number``
75     function    ``question``
96     procedure   ``compare``
176    function    ``legal``
279    procedure   ``search_try``
296    procedure   ``verify``
327    procedure   ``round_computer_guesses``
372    procedure   ``round_human_guesses``
408    procedure   ``main``
====== =========== ==================================================

prg/mirror.sd7
--------------

:Lines:    131
:Entities: 3

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
32     structure   ``rectangularArea``
40     function    ``selectArea``
97     procedure   ``main``
====== =========== ==================================================

prg/ms.sd7
----------

:Lines:    641
:Entities: 34

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
53     structure   ``field_place``
64     array       ``area``
67     procedure   ``init_area``
75     procedure   ``display_place``
95     procedure   ``show_place``
104    procedure   ``incr_place``
113    procedure   ``init_mines``
153    function    ``place_is_free``
171    procedure   ``display_nonfree_place``
200    procedure   ``show_free_NW``
201    procedure   ``show_free_NE``
202    procedure   ``show_free_SW``
203    procedure   ``show_free_SE``
206    procedure   ``show_free_N``
218    procedure   ``show_free_S``
230    procedure   ``show_free_W``
242    procedure   ``show_free_E``
254    procedure   ``show_free_NW``
268    procedure   ``show_free_NE``
282    procedure   ``show_free_SE``
296    procedure   ``show_free_SW``
310    procedure   ``show_free_area``
327    procedure   ``show_explosion``
340    procedure   ``show_wrong_mark``
347    procedure   ``show_area_around``
393    procedure   ``show_solution``
410    procedure   ``read_command``
424    procedure   ``congratulation``
450    procedure   ``select_round``
494    procedure   ``init_round``
518    procedure   ``shut_round``
527    procedure   ``init``
539    procedure   ``play_round``
625    procedure   ``main``
====== =========== ==================================================

prg/nicoma.sd7
--------------

:Lines:    135
:Entities: 4

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
34     procedure   ``verify``
69     function    ``read_remainder``
90     procedure   ``guess_number``
120    procedure   ``main``
====== =========== ==================================================

prg/pac.sd7
-----------

:Lines:    726
:Entities: 39

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
38     enumeration ``placeType``
42     function    ``str``
47     enumeration ``directType``
51     structure   ``pacmanType``
62     structure   ``ghostType``
75     array       ``ghostList``
84     array       ``labyrinthMap``
85     array       ``map``
89     array       ``field``
120    procedure   ``showLifes``
137    procedure   ``left``
150    procedure   ``right``
163    procedure   ``up``
176    procedure   ``down``
189    procedure   ``sendHome``
203    procedure   ``readCommand``
231    procedure   ``writeMapPosition``
239    procedure   ``pacmanCought``
267    procedure   ``ghostCought``
283    procedure   ``someoneCought``
294    procedure   ``eat``
300    procedure   ``eat``
312    procedure   ``eat``
332    procedure   ``checkGhost``
341    procedure   ``checkAllGhosts``
352    procedure   ``movePacman``
382    function    ``selectDirection``
390    array       ``possible``
417    procedure   ``turn``
420    procedure   ``turn``
428    procedure   ``turn``
436    procedure   ``turn``
444    procedure   ``turn``
452    procedure   ``move``
574    procedure   ``moveGhosts``
586    procedure   ``readMap``
611    procedure   ``showMap``
631    procedure   ``mainControl``
658    procedure   ``main``
====== =========== ==================================================

prg/pairs.sd7
-------------

:Lines:    2025
:Entities: 78

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
55     array       ``digit_pixmap``
58     structure   ``cardType``
59     array       ``picture``
64     enumeration ``visibleType``
68     structure   ``fieldType``
74     structure   ``gameObj``
86     array       ``size_2_5``
121    array       ``size_3_4``
156    array       ``size_4_4``
191    array       ``size_4_5``
226    array       ``size_4_6``
261    array       ``size_5_6``
296    array       ``size_6_6``
331    array       ``size_6_7``
366    array       ``size_6_8``
401    array       ``size_6_10``
436    array       ``size_6_12``
471    array       ``size_6_14``
506    array       ``panic_monster1_pic``
525    array       ``panic_monster2_pic``
544    array       ``panic_monster3_pic``
563    array       ``dig_right_pic``
582    array       ``big_bush_pic``
617    array       ``chain_pic``
636    array       ``large_gem_pic``
671    array       ``fairy_pic``
738    array       ``computer_pic``
805    array       ``sea_pic``
872    array       ``woman_pic``
939    array       ``zero``
958    array       ``one``
977    array       ``two``
996    array       ``three``
1015   array       ``four``
1033   array       ``five``
1052   array       ``six``
1071   array       ``seven``
1090   array       ``eight``
1109   array       ``nine``
1128   function    ``genCard``
1138   array       ``cards``
1183   array       ``size_descr``
1198   array       ``size_list``
1214   array       ``field``
1217   procedure   ``draw``
1230   procedure   ``put``
1237   procedure   ``prepare``
1268   procedure   ``show``
1278   procedure   ``hide``
1292   procedure   ``remove``
1302   procedure   ``hideAll``
1315   procedure   ``draw_number``
1337   procedure   ``showHit``
1354   function    ``countCardBacks``
1371   procedure   ``randomField``
1396   procedure   ``randomNotVisited``
1431   procedure   ``firstOfPair``
1472   procedure   ``secondOfPair``
1494   procedure   ``computerLevel0``
1508   procedure   ``computerLevel1``
1529   procedure   ``computerLevel2``
1550   procedure   ``computerLevel3``
1571   procedure   ``computerLevel4``
1592   procedure   ``computerLevel5``
1611   procedure   ``computerLevel6``
1636   procedure   ``computerLevel7``
1661   procedure   ``computerLevel8``
1686   procedure   ``computerLevel9``
1709   procedure   ``computerTurn``
1746   procedure   ``checkHit``
1763   procedure   ``mouseCommand``
1782   procedure   ``playerMove``
1841   procedure   ``playerTurn``
1851   procedure   ``dealCards``
1875   procedure   ``initializeGame``
1887   procedure   ``selectSize``
1929   function    ``createButton``
1939   procedure   ``main``
====== =========== ==================================================

prg/panic.sd7
-------------

:Lines:    2634
:Entities: 123

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
58     enumeration ``holeType``
64     function    ``str``
69     array       ``round_description``
121    array       ``layout_description``
132    array       ``monster_pixmap``
133    array       ``player_left_pixmap``
134    array       ``player_right_pixmap``
135    array       ``player_up_pixmap``
136    array       ``player_down_pixmap``
137    array       ``player_falling_pixmap``
138    array       ``player_dig_left_pixmap``
139    array       ``player_dig_right_pixmap``
140    array       ``digit_pixmap``
148    structure   ``screenObj``
164    structure   ``playerObj``
172    structure   ``monsterObj``
182    structure   ``gameObj``
195    array       ``monster``
199    array       ``level_line``
203    array       ``field``
204    array       ``hole_status``
209    array       ``player_right_1``
228    array       ``player_right_2``
247    array       ``player_left_1``
266    array       ``player_left_2``
285    array       ``player_falling``
304    array       ``player_dig_right_1``
323    array       ``player_dig_right_2``
342    array       ``player_dig_left_1``
361    array       ``player_dig_left_2``
379    array       ``player_up_down_1``
398    array       ``player_up_down_2``
417    array       ``monster1_1``
430    array       ``monster1_2``
443    array       ``monster1_3``
456    array       ``monster1_4``
469    array       ``monster1_5``
482    array       ``monster2_1``
495    array       ``monster2_2``
508    array       ``monster3_1``
521    array       ``monster3_2``
534    array       ``player_reserve``
545    array       ``score_text``
555    array       ``bonus_text``
565    array       ``hiscore_text``
575    array       ``zero``
585    array       ``one``
595    array       ``two``
605    array       ``three``
615    array       ``four``
625    array       ``five``
635    array       ``six``
645    array       ``seven``
655    array       ``eight``
665    array       ``nine``
675    function    ``createPixmap``
679    procedure   ``init_pictures``
802    procedure   ``visible``
810    procedure   ``invisible``
816    procedure   ``allObjectsInvisible``
831    procedure   ``pause_game``
865    procedure   ``game_command``
883    procedure   ``draw_number``
905    function    ``number_pixmap``
928    procedure   ``draw_level``
968    procedure   ``draw_base_level``
978    procedure   ``mark_level``
988    procedure   ``draw_level_piece``
1037   procedure   ``draw_hole``
1111   procedure   ``draw_ladder``
1140   procedure   ``mark_ladder``
1151   procedure   ``set_ladder``
1159   procedure   ``init_ladders``
1171   procedure   ``change_direction``
1223   function    ``collision``
1236   function    ``collision2``
1253   procedure   ``avoid_monster_collision``
1302   procedure   ``move``
1322   procedure   ``place``
1336   procedure   ``die``
1348   procedure   ``mark_hole``
1386   procedure   ``fall_at_monsters``
1414   procedure   ``check_falling``
1480   procedure   ``set_direction``
1509   procedure   ``player_collision``
1556   procedure   ``check_hole``
1620   procedure   ``check_direction``
1664   procedure   ``place_monster``
1684   procedure   ``draw_reserve``
1706   procedure   ``init_round``
1801   procedure   ``dig_hole``
1820   procedure   ``shut_hole``
1845   procedure   ``do_dig``
1865   function    ``hole_position_ok``
1891   function    ``ladder_up_direction``
1912   function    ``ladder_down_direction``
1933   function    ``level_direction``
1954   function    ``hole_direction``
1975   procedure   ``stop``
1984   procedure   ``left``
2002   procedure   ``right``
2020   procedure   ``up``
2038   procedure   ``down``
2056   procedure   ``dig_left``
2074   procedure   ``dig_right``
2092   procedure   ``shut_left``
2110   procedure   ``shut_right``
2128   procedure   ``go_horizontally``
2138   procedure   ``go_vertically``
2148   procedure   ``player_falling``
2185   procedure   ``process_command``
2242   procedure   ``stop_at_hole``
2273   procedure   ``process``
2302   function    ``getLevel``
2321   function    ``getLevel``
2334   function    ``clickedAtLadder``
2362   function    ``clickedAboveHole``
2376   function    ``clickedAtMonsterInHole``
2390   procedure   ``get_command``
2482   procedure   ``game_round``
2547   procedure   ``play_game``
2568   procedure   ``writeCentered``
2575   procedure   ``main``
====== =========== ==================================================

prg/percolation.sd7
-------------------

:Lines:    330
:Entities: 21

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
47     function    ``open``
49     function    ``dimensions``
51     function    ``percolates``
53     function    ``isOpen``
55     function    ``isFull``
57     structure   ``virtualGrid``
62     array       ``status``
69     procedure   ``initVirtualGrid``
93     procedure   ``connect``
99     function    ``coordsTo1D``
109    function    ``open``
150    function    ``dimensions``
157    function    ``percolates``
164    function    ``isOpen``
174    function    ``isFull``
192    procedure   ``delay``
204    procedure   ``draw_update``
241    procedure   ``open_random``
258    function    ``build_stats``
275    procedure   ``main``
281    array       ``all_simulations``
====== =========== ==================================================

prg/planets.sd7
---------------

:Lines:    1486
:Entities: 61

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
61     structure   ``orbitType``
78     array       ``orbitDescr``
83     structure   ``heliocentricPlanetType``
97     structure   ``eclipticalCoordinateType``
102    structure   ``equatorialCoordinateType``
107    structure   ``horizontalCoordinateType``
112    structure   ``earthCoordinateType``
117    structure   ``geocentricPlanetType``
131    function    ``toDegrees``
135    function    ``toRadians``
139    function    ``sinD``
143    function    ``cosD``
147    function    ``tanD``
151    function    ``asinD``
155    function    ``acosD``
159    function    ``atanD``
163    function    ``atan2D``
167    function    ``frac``
174    function    ``julianDay``
179    function    ``minutes``
183    function    ``seconds``
187    function    ``angleAsDegrees``
210    function    ``angleAsHours``
233    function    ``anomaly``
250    function    ``anomaly2``
268    function    ``eclipticalCoordinatesToRightAscension``
281    function    ``eclipticalCoordinatesToDeclination``
285    procedure   ``degreesToRange``
296    procedure   ``hoursToRange``
307    function    ``localSiderealToCivilTime``
334    procedure   ``showLocalTime``
371    function    ``calculateGreenwichMeanSiderealTime``
390    procedure   ``showTime``
426    function    ``locatePositionOfPlanetInItsOrbitalPlane``
452    procedure   ``plotPlanet``
454    function    ``computeRadiusVector``
490    procedure   ``showMenu``
534    procedure   ``setup``
564    procedure   ``plotInnerPlanets``
589    procedure   ``plotOuterPlanets``
613    procedure   ``changeDateTime``
644    procedure   ``changeLongLat``
657    function    ``computeProjectedHeliocentricLongitude``
674    procedure   ``ProjectPlanetOntoEclipticalPlane``
683    function    ``calculateEclipticalCoordinates``
710    function    ``calculateEquatorialCoordinates``
724    function    ``calculateHourAngle``
736    function    ``calculateHorizontalCoordinates``
754    function    ``genGeocentricPos``
768    function    ``panoramaXPos``
772    function    ``panoramaYPos``
776    procedure   ``drawAllPlanets``
796    procedure   ``drawSun``
809    procedure   ``drawHorizont``
813    array       ``name``
839    procedure   ``writePoint``
903    procedure   ``drawStars``
933    procedure   ``locatePlanet``
1150   procedure   ``showName``
1268   procedure   ``initOrbitDescr``
1407   procedure   ``main``
====== =========== ==================================================

prg/portfwd7.sd7
----------------

:Lines:    139
:Entities: 2

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
33     procedure   ``connectPort``
84     procedure   ``main``
====== =========== ==================================================

prg/prime.sd7
-------------

:Lines:    74
:Entities: 2

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
32     function    ``isPrime``
54     procedure   ``main``
====== =========== ==================================================

prg/printpi1.sd7
----------------

:Lines:    56
:Entities: 2

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
40     function    ``compute_pi_machin``
52     procedure   ``main``
====== =========== ==================================================

prg/printpi2.sd7
----------------

:Lines:    54
:Entities: 2

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
36     function    ``compute_pi_bailey_borwein_plouffe``
50     procedure   ``main``
====== =========== ==================================================

prg/printpi3.sd7
----------------

:Lines:    60
:Entities: 2

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
40     procedure   ``main``
45     array       ``arr``
====== =========== ==================================================

prg/pv7.sd7
-----------

:Lines:    337
:Entities: 13

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
36     procedure   ``writeMessage``
54     procedure   ``nextImageFile``
64     procedure   ``previousImageFile``
74     procedure   ``findFileWithName``
86     function    ``isGraphicFile``
90     set         ``imageMagic``
108    structure   ``sectionData``
116    procedure   ``displayImage``
150    procedure   ``move``
179    procedure   ``zoomIn``
205    procedure   ``zoomOut``
231    procedure   ``main``
236    array       ``fileList``
====== =========== ==================================================

prg/queen.sd7
-------------

:Lines:    149
:Entities: 5

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
36     procedure   ``initialize``
50     procedure   ``display``
80     function    ``canPlace``
121    procedure   ``place``
138    procedure   ``main``
====== =========== ==================================================

prg/rand.sd7
------------

:Lines:    121
:Entities: 4

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
30     procedure   ``rand_test_1``
49     procedure   ``rand_test_2``
68     procedure   ``rand_test_3``
87     procedure   ``main``
====== =========== ==================================================

prg/raytrace.sd7
----------------

:Lines:    538
:Entities: 43

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
33     function    ``*``
38     function    ``str``
43     procedure   ``add``
60     structure   ``ray``
65     function    ``ray``
74     structure   ``view``
87     function    ``calcDirection``
91     structure   ``light``
97     function    ``light``
106    function    ``pos``
109    function    ``colour``
112    function    ``castsShadows``
116    structure   ``surface``
126    function    ``surface``
146    function    ``dull``
158    function    ``shiny``
174    structure   ``baseRayTraceObject``
180    function    ``rayTraceObject``
187    function    ``surfaceAt``
191    structure   ``intersect``
197    function    ``intersect``
204    procedure   ``setValue``
212    function    ``intersect``
213    function    ``surfaceAt``
216    structure   ``sphere``
223    function    ``sphere``
232    function    ``intersect``
268    function    ``surfaceAt``
277    structure   ``infinitePlane``
284    function    ``infinitePlane``
293    function    ``intersect``
318    function    ``surfaceAt``
327    structure   ``canvas``
333    structure   ``rayTraceCanvas``
334    array       ``scene``
341    function    ``intersect``
354    function    ``shadow``
369    function    ``trace``
371    function    ``shade``
451    function    ``trace``
465    procedure   ``trace``
501    procedure   ``main``
503    array       ``scene``
====== =========== ==================================================

prg/rever.sd7
-------------

:Lines:    816
:Entities: 28

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
41     enumeration ``direction_type``
46     array       ``ALL_DIRECTIONS``
55     structure   ``moveType``
67     array       ``TVAL``
68     array       ``field_counter``
78     function    ``line_delta``
88     function    ``column_delta``
91     procedure   ``rules``
122    procedure   ``count_fields``
141    procedure   ``write_move``
148    procedure   ``show_board``
180    procedure   ``show_board``
218    procedure   ``turn``
263    procedure   ``turn_evaluation``
300    procedure   ``move_value``
348    procedure   ``calculate_move``
358    array       ``best_move``
512    procedure   ``computer_move``
532    procedure   ``read_column``
557    procedure   ``read_line``
593    procedure   ``read_move``
618    procedure   ``human_move``
640    procedure   ``first_move``
669    procedure   ``final_result``
689    procedure   ``set_tvals``
718    procedure   ``generate_start_board``
746    procedure   ``start``
797    procedure   ``main``
====== =========== ==================================================

prg/roman.sd7
-------------

:Lines:    38
:Entities: 1

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
30     procedure   ``main``
====== =========== ==================================================

prg/s7c.sd7
-----------

:Lines:    9060
:Entities: 227

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
67     array       ``libraryDirs``
97     array       ``elementNameTable``
106    function    ``isFuncParamData``
110    procedure   ``count_declarations``
123    procedure   ``process_value_hashCode_declaration``
143    procedure   ``process_value_cpy_declaration``
164    procedure   ``process_value_create_declaration``
189    procedure   ``process_value_destr_declaration``
208    procedure   ``process_value_cmp_declaration``
231    procedure   ``process_big_create_call``
245    procedure   ``process_str_create_call``
286    procedure   ``getAnyParamToTempAssigns``
302    procedure   ``getAnyParamToTempAssigns``
353    function    ``enum_value``
382    procedure   ``ref_list_value``
408    function    ``getExprValue``
425    function    ``isPointerParam``
430    function    ``isCopyParam``
435    function    ``isInOutParam``
439    function    ``canTakeAddress``
470    procedure   ``process_constenumobject``
479    function    ``param_list_okay``
498    function    ``containsFunctionCall``
529    function    ``recursiveFunctionCall``
548    function    ``identical_values``
605    function    ``canUseArrTimes``
639    procedure   ``assignArrayValue``
757    procedure   ``process_local_declaration``
1073   procedure   ``process_local_var_declaration``
1087   procedure   ``determineDataForActualFuncParam``
1110   function    ``determineDataForActualFuncParam``
1119   procedure   ``defineFunctype``
1189   procedure   ``defineActualFuncParam``
1249   procedure   ``defineFuncValue``
1288   procedure   ``callActualFuncParam``
1305   procedure   ``processFuncValue``
1338   procedure   ``processFuncParam``
1349   procedure   ``checkParameterAliasing``
1385   procedure   ``call_params``
1482   procedure   ``process_prototype_declaration``
1486   procedure   ``process_const_func_call``
1561   procedure   ``process_func_call``
1595   procedure   ``process_call``
1598   procedure   ``process_indirect_func_call``
1621   procedure   ``process_call``
1731   procedure   ``process_match``
1781   procedure   ``optimize_constant_expressions``
1822   procedure   ``process_expr``
2139   procedure   ``process_call_by_name_expr``
2188   procedure   ``declare_types_of_params``
2203   procedure   ``process_param_declaration``
2263   procedure   ``process_param_list_declaration``
2295   procedure   ``process_result_declaration``
2324   procedure   ``process_return``
2336   procedure   ``process_return_value``
2369   procedure   ``process_local_consts``
2373   procedure   ``process_const_func_declaration``
2617   procedure   ``process_library_initialisation``
2637   procedure   ``declare_exception_name``
2662   procedure   ``process_main_declaration``
2880   procedure   ``process_var_func_declaration``
2904   procedure   ``process_func_declaration``
2916   procedure   ``process_prototype_declaration``
2953   procedure   ``process_forward_declaration``
2968   procedure   ``addImplementationToInterface``
2984   procedure   ``process_type_declaration``
3028   procedure   ``process_int_declaration``
3059   procedure   ``process_bigint_declaration``
3077   procedure   ``process_char_declaration``
3092   procedure   ``process_stri_declaration``
3110   procedure   ``process_bstri_declaration``
3129   procedure   ``process_float_declaration``
3144   procedure   ``action_address``
3302   procedure   ``block_address``
3359   procedure   ``object_address``
3392   procedure   ``process_reference_declaration``
3420   procedure   ``process_ref_list_declaration``
3463   procedure   ``process_file_declaration``
3476   procedure   ``process_socket_declaration``
3487   procedure   ``process_poll_declaration``
3502   procedure   ``process_array_declaration``
3535   procedure   ``process_hash_declaration``
3570   procedure   ``process_set_declaration``
3590   procedure   ``process_struct_declaration``
3631   procedure   ``process_interface_declaration``
3654   procedure   ``process_win_declaration``
3673   procedure   ``process_plist_declaration``
3692   procedure   ``process_process_declaration``
3705   procedure   ``process_prog_declaration``
3718   procedure   ``process_database_declaration``
3729   procedure   ``process_sql_stmt_declaration``
3740   procedure   ``process_enum_declaration``
3766   procedure   ``process_enum_literal_declaration``
3796   procedure   ``print_parameter_list``
3871   procedure   ``process_dynamic_parameter_list``
3923   procedure   ``process_dynamic_function_call``
3970   procedure   ``process_dynamic_action_call``
4004   function    ``process_dynamic_call``
4064   function    ``process_dynamic_condition``
4069   function    ``process_dynamic_param_implements``
4157   function    ``process_dynamic_param_enumeration``
4246   function    ``process_dynamic_param_struct_elem``
4368   function    ``process_dynamic_condition``
4447   procedure   ``process_dynamic_decision``
4503   procedure   ``process_dynamic_decisions``
4514   procedure   ``process_dynamic_declaration``
4546   procedure   ``declare_literal_function_of_enum``
4585   procedure   ``declare_literal_function_of_enums``
4597   procedure   ``process_hashcode``
4612   function    ``keyCreateObj``
4627   function    ``keyCompareObj``
4642   function    ``dataCreateObj``
4657   function    ``dataCopyObj``
4672   procedure   ``process_arr_cpy_declaration``
4697   procedure   ``process_arr_create_declaration``
4722   procedure   ``process_arr_destr_declaration``
4747   procedure   ``process_arr_gen_declaration``
4774   procedure   ``process_arr_idx_declaration``
4801   procedure   ``process_arr_times_declaration``
4862   procedure   ``defineParam1TypeCategory``
4877   procedure   ``process_itf_next_file_declaration``
4889   procedure   ``process_hsh_cpy_declaration``
4904   procedure   ``process_hsh_create_declaration``
4919   procedure   ``process_hsh_destr_declaration``
4934   procedure   ``addStructElem``
4956   procedure   ``process_sct_cpy_declaration``
4983   procedure   ``process_sct_create_declaration``
4996   procedure   ``process_sct_destr_declaration``
5009   procedure   ``process_sct_select_declaration``
5037   procedure   ``process_var_action_declaration``
5059   procedure   ``process_action_declaration``
5197   procedure   ``process_object_declaration``
5281   procedure   ``replaceLocalsFromOutside``
5320   procedure   ``changeCallsOfLocalFunction``
5349   procedure   ``changeCallsFromSubFunctions``
5368   procedure   ``adjustParamsToAdd``
5403   function    ``fixLocalFunction``
5433   procedure   ``processLocalFunctions``
5455   procedure   ``addTypeCategoryForLocalVars``
5477   procedure   ``process_local_consts``
5500   procedure   ``process_object``
5524   procedure   ``declare_failed_inline_functions``
5538   procedure   ``write_file_head``
6042   procedure   ``declareExtern``
6053   procedure   ``write_prototypes``
6142   procedure   ``initPollOperations``
6155   function    ``determine_multiple_array_elements``
6192   procedure   ``walk_const_list``
6287   procedure   ``prepare_func_literal``
6303   procedure   ``process_func_literal``
6315   procedure   ``process_pollData_literal``
6323   procedure   ``init_const_value``
6431   function    ``int32AsFourBytes``
6443   function    ``int64AsEightBytes``
6455   function    ``int64AsTwoInt32``
6474   procedure   ``init_bigint_constants``
6492   procedure   ``assign_bigint_constants``
6528   function    ``pixelEncodingIdentical``
6537   function    ``pixelEncodingWithoutAlphaChannel``
6544   function    ``pixelEncodingWithRedAndBlueSwapped``
6553   function    ``pixelEncodingWithRedAndBlueSwappedWithoutAlphaChannel``
6560   function    ``swapRedAndBlue``
6586   array       ``pixelArray``
6586   function    ``fixPixels``
6620   procedure   ``init_win_constants``
6677   procedure   ``assign_win_constants``
6716   function    ``pointListEncodingIdentical``
6725   function    ``toPointListAbsolute``
6730   array       ``xyArray``
6762   function    ``toPointListRelative16``
6767   array       ``xyArray``
6798   function    ``toPointListRelative32``
6803   array       ``xyArray``
6834   function    ``toTargetPointListBstring``
6855   procedure   ``init_plist_constants``
6875   procedure   ``assign_plist_constants``
6905   procedure   ``write_striChars``
6928   procedure   ``write_str_table``
6973   procedure   ``handleOverlappingStrings``
7027   procedure   ``init_string_constants_with_slices``
7034   array       ``lengthList``
7037   array       ``stringPosition``
7084   procedure   ``init_string_constants_no_slices``
7136   procedure   ``init_string_constants``
7150   procedure   ``write_bstriChars``
7187   procedure   ``write_bst_table``
7225   procedure   ``init_bstri_constants_with_slices``
7232   array       ``lengthList``
7235   array       ``stringPosition``
7282   procedure   ``init_bstri_constants_no_slices``
7349   procedure   ``init_bstri_constants``
7363   procedure   ``init_set_constants``
7425   procedure   ``init_type_constants``
7468   procedure   ``enter_ref_constant_types``
7481   procedure   ``init_ref_constants``
7544   procedure   ``init_array_constants``
7603   procedure   ``malloc_struct``
7623   procedure   ``init_struct_constants``
7661   procedure   ``init_hash_constants``
7726   procedure   ``init_interface_constants``
7753   procedure   ``init_nan_constants``
7776   procedure   ``initCaseLabelsOfWhen``
7824   procedure   ``initCaseLabelsOfCase``
7840   procedure   ``initCaseLabels``
7860   procedure   ``initElementNameLabelsOfCase``
7887   procedure   ``initElementNameLabels``
7907   procedure   ``walk_const_list``
7942   procedure   ``init_values``
8052   procedure   ``declare_rtlRaiseError``
8106   procedure   ``declare_raise_error2``
8148   procedure   ``init_globals``
8160   procedure   ``process_global_declarations``
8229   procedure   ``init_systypes``
8283   function    ``temp_name``
8296   procedure   ``pass_1``
8432   procedure   ``pass_2``
8538   procedure   ``importEnvironment``
8556   procedure   ``appendLibrary``
8572   procedure   ``appendLibrary``
8582   procedure   ``logProgram``
8600   procedure   ``execProgramScript``
8622   procedure   ``execProgram``
8652   procedure   ``pass_3``
8674   array       ``compileParams``
8676   array       ``linkParams``
8937   procedure   ``writeHelp``
8983   procedure   ``main``
====== =========== ==================================================

prg/s7check.sd7
---------------

:Lines:    68
:Entities: 1

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
33     procedure   ``main``
====== =========== ==================================================

prg/savehd7.sd7
---------------

:Lines:    1110
:Entities: 31

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
40     enumeration ``phaseType``
44     function    ``str``
47     function    ``parse``
60     structure   ``stateType``
83     procedure   ``showProgress``
118    function    ``loadState``
158    procedure   ``saveState``
189    procedure   ``checkSumOfBadBytes``
204    function    ``countBadBytesInAreasForward``
220    procedure   ``listBadAreas``
232    function    ``confirmSave``
399    procedure   ``nextPhase``
434    function    ``bigLength``
438    function    ``afterMaximumBadArea``
453    function    ``searchPossibleAreaCombine``
471    procedure   ``combineBadAreas``
503    procedure   ``addBadArea``
529    function    ``searchPossibleAreaShrink``
546    procedure   ``shrinkBadAreas``
588    procedure   ``removeBadArea``
603    procedure   ``copyFile``
680    function    ``repairBlock``
710    procedure   ``repairChunk``
739    procedure   ``rereadFile``
789    procedure   ``determineBadAreaToProcess``
813    procedure   ``processAreaBackward``
853    procedure   ``fixOrImproveFile``
910    procedure   ``processAreaForward``
951    procedure   ``examineFile``
1028   procedure   ``writeHelp``
1065   procedure   ``main``
====== =========== ==================================================

prg/self.sd7
------------

:Lines:    49
:Entities: 2

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
26     array       ``prog``
39     procedure   ``main``
====== =========== ==================================================

prg/shisen.sd7
--------------

:Lines:    1423
:Entities: 53

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
57     array       ``digit_pixmap``
59     structure   ``cardType``
60     array       ``picture``
65     enumeration ``visibleType``
69     structure   ``fieldType``
79     array       ``big_bush_pic``
114    array       ``large_gem_pic``
149    array       ``fairy_pic``
216    array       ``computer_pic``
283    array       ``sea_pic``
350    array       ``zero``
369    array       ``one``
388    array       ``two``
407    array       ``three``
426    array       ``four``
444    array       ``five``
463    array       ``six``
482    array       ``seven``
501    array       ``eight``
520    array       ``nine``
539    function    ``genCard``
547    array       ``cards``
574    array       ``field``
577    procedure   ``draw``
590    procedure   ``put``
597    procedure   ``show``
604    procedure   ``mark``
625    procedure   ``unmark``
646    procedure   ``remove``
656    procedure   ``unmarkAll``
669    function    ``countCards``
686    procedure   ``showHit``
699    procedure   ``horizontal``
711    procedure   ``vertical``
723    function    ``line_free``
739    function    ``column_free``
755    procedure   ``upper_way``
782    procedure   ``lower_way``
809    procedure   ``left_way``
836    procedure   ``right_way``
863    procedure   ``way_down_right``
885    procedure   ``way_right_down``
907    procedure   ``way_down_left``
929    procedure   ``way_left_down``
951    function    ``find_way``
1085   procedure   ``readHelpCommand``
1102   procedure   ``help``
1178   procedure   ``playerMove``
1238   procedure   ``playerTurn``
1276   function    ``solvable``
1333   procedure   ``dealCards``
1365   procedure   ``writeCentered``
1372   procedure   ``main``
====== =========== ==================================================

prg/sl.sd7
----------

:Lines:    1029
:Entities: 28

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
57     structure   ``coord``
68     structure   ``workplace``
75     array       ``workspace``
76     array       ``listlength``
92     procedure   ``info1``
119    procedure   ``info2``
150    procedure   ``nextDecimal``
162    procedure   ``writeHeader``
178    procedure   ``editInfo``
206    procedure   ``clearScreen``
219    procedure   ``writeScreen``
232    procedure   ``writeMap``
239    array       ``mapfield``
304    procedure   ``redraw``
317    procedure   ``shiftField``
351    procedure   ``mark``
365    procedure   ``erase``
402    procedure   ``clearField``
423    procedure   ``readFilename``
463    procedure   ``load``
520    procedure   ``save``
576    procedure   ``pressakey``
592    procedure   ``editmode``
771    procedure   ``runinfo``
789    procedure   ``init``
802    procedure   ``markcell``
814    procedure   ``nextgeneration``
960    procedure   ``main``
====== =========== ==================================================

prg/snake.sd7
-------------

:Lines:    615
:Entities: 25

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
36     enumeration ``direction_type``
46     array       ``FIELD``
48     structure   ``screen_object``
54     structure   ``apple_object``
60     structure   ``snake_object``
67     array       ``position``
75     procedure   ``move``
83     procedure   ``beep``
91     procedure   ``set_position``
111    procedure   ``show_status``
119    procedure   ``turn``
131    procedure   ``turn``
143    procedure   ``turn``
155    procedure   ``turn``
167    procedure   ``move``
178    procedure   ``move``
189    procedure   ``move``
200    procedure   ``move``
211    procedure   ``move``
218    procedure   ``move``
228    procedure   ``enlarge``
244    procedure   ``show``
282    function    ``play``
356    procedure   ``init_level``
578    procedure   ``main``
====== =========== ==================================================

prg/sokoban.sd7
---------------

:Lines:    891
:Entities: 33

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
54     enumeration ``categoryType``
58     structure   ``fieldType``
64     array       ``levelMap``
85     enumeration ``moveMode``
89     enumeration ``moveDirection``
93     structure   ``moveType``
98     array       ``playerMoves``
102    array       ``player_pic``
137    array       ``goal_pic``
172    array       ``wall_pic``
207    array       ``packet_pic``
242    array       ``player_at_goal_pic``
277    array       ``packet_at_goal_pic``
312    procedure   ``introduction``
339    function    ``createButton``
356    procedure   ``loadPixmaps``
373    procedure   ``readLevel``
411    procedure   ``recognizeFieldsOutside``
431    procedure   ``recognizeFieldsOutside``
451    procedure   ``generateLevelMap``
500    procedure   ``readLevelMap``
506    procedure   ``writeStatus``
517    procedure   ``drawMap``
565    procedure   ``assignDxDy``
583    procedure   ``moveDxDy``
595    procedure   ``pushDxDy``
620    procedure   ``pullDxDy``
645    procedure   ``undoMove``
670    procedure   ``redoMove``
695    function    ``doMove``
732    procedure   ``doMove``
775    procedure   ``playLevel``
875    procedure   ``main``
====== =========== ==================================================

prg/spigotpi.sd7
----------------

:Lines:    64
:Entities: 1

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
28     procedure   ``main``
====== =========== ==================================================

prg/sql7.sd7
------------

:Lines:    278
:Entities: 6

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
34     procedure   ``writeHelp``
60     function    ``failed``
77     procedure   ``displayResult``
105    procedure   ``doExecute``
142    function    ``getSqlStatement``
196    procedure   ``main``
====== =========== ==================================================

prg/startrek.sd7
----------------

:Lines:    979
:Entities: 43

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
42     array       ``description``
47     array       ``klingonRow``
48     array       ``klingonColumn``
49     array       ``klingonEnergy``
51     array       ``quad``
58     array       ``sect``
68     array       ``damage``
73     procedure   ``title``
81     procedure   ``help_course``
91     procedure   ``help_sector``
100    procedure   ``help_quadrant``
110    procedure   ``help_warp_engines``
132    procedure   ``help_short_range_sensors``
143    procedure   ``help_long_range_sensors``
153    procedure   ``help_phasers``
165    procedure   ``help_photon_torpedoes``
176    procedure   ``help_galactic_records``
186    procedure   ``help_commands``
198    procedure   ``help_game``
214    procedure   ``help_quit``
221    procedure   ``help``
250    procedure   ``fix_damage``
260    procedure   ``find_free_sector``
269    procedure   ``init``
320    procedure   ``enter_quadrant``
367    function    ``get_condition``
398    procedure   ``write_phaser_hit``
407    function    ``klingon_distance``
416    procedure   ``hits_from_klingons``
439    procedure   ``time_for_repair``
446    procedure   ``show_damage``
453    procedure   ``move_ship``
528    procedure   ``short_range_sensors``
557    procedure   ``warp_engines``
664    function    ``quadrant_description``
673    procedure   ``long_range_sensors``
699    procedure   ``phasers``
754    procedure   ``torpedo_track``
818    procedure   ``photon_torpedoes``
862    procedure   ``galactic_records``
891    procedure   ``write_stardate``
898    procedure   ``game``
958    procedure   ``main``
====== =========== ==================================================

prg/sudoku7.sd7
---------------

:Lines:    2657
:Entities: 139

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
64     array       ``blue_digits``
65     array       ``red_digits``
66     array       ``small_digits``
73     array       ``field``
74     array       ``candidates``
75     array       ``user_input``
94     array       ``blue_zero``
113    array       ``blue_one``
132    array       ``blue_two``
151    array       ``blue_three``
170    array       ``blue_four``
188    array       ``blue_five``
207    array       ``blue_six``
226    array       ``blue_seven``
245    array       ``blue_eight``
264    array       ``blue_nine``
283    array       ``red_zero``
302    array       ``red_one``
321    array       ``red_two``
340    array       ``red_three``
359    array       ``red_four``
377    array       ``red_five``
396    array       ``red_six``
415    array       ``red_seven``
434    array       ``red_eight``
453    array       ``red_nine``
472    array       ``blue_single``
490    array       ``blue_double``
509    array       ``blue_triple``
527    array       ``candidates_pic``
545    procedure   ``initCandidates``
566    procedure   ``initGrid``
592    procedure   ``clearField``
600    procedure   ``markField``
608    procedure   ``clearDigit``
619    procedure   ``setRedDigit``
632    procedure   ``setBlueDigit``
645    procedure   ``writeSmallDigit``
656    procedure   ``excludeInRow``
668    procedure   ``excludeInColumn``
680    procedure   ``excludeInBox``
695    procedure   ``excludeDigit``
703    procedure   ``excludeFields``
718    procedure   ``showAllCandidates``
747    procedure   ``checkSingles``
769    procedure   ``checkHiddenSinglesInRow``
795    procedure   ``checkHiddenSinglesInColumn``
821    procedure   ``checkHiddenSinglesInBox``
852    procedure   ``checkHiddenSingles``
871    procedure   ``checkLockedCandidatesInRow``
906    procedure   ``checkLockedCandidatesInColumn``
941    procedure   ``checkLockedCandidatesInBox``
993    procedure   ``checkLockedCandidates``
1012   procedure   ``checkNakedPairsInRow``
1018   set         ``pairSet``
1039   procedure   ``checkNakedPairsInColumn``
1045   set         ``pairSet``
1066   procedure   ``checkNakedPairsInBox``
1075   set         ``pairSet``
1104   procedure   ``checkNakedPairs``
1123   procedure   ``checkNakedTriplesInRow``
1130   set         ``tripleSet``
1159   procedure   ``checkNakedTriplesInColumn``
1166   set         ``tripleSet``
1195   procedure   ``checkNakedTriplesInBox``
1206   set         ``tripleSet``
1248   procedure   ``checkNakedTriples``
1267   procedure   ``checkNakedQuadsInRow``
1275   set         ``quadSet``
1310   procedure   ``checkNakedQuadsInColumn``
1318   set         ``quadSet``
1353   procedure   ``checkNakedQuadsInBox``
1366   set         ``quadSet``
1420   procedure   ``checkNakedQuads``
1439   procedure   ``checkHiddenPairsInRow``
1445   array       ``columnsWithDigit``
1446   set         ``pairColumns``
1447   set         ``pairSet``
1473   procedure   ``checkHiddenPairsInColumn``
1479   array       ``rowsWithDigit``
1480   set         ``pairRows``
1481   set         ``pairSet``
1507   procedure   ``checkHiddenPairsInBox``
1514   array       ``cellsWithDigit``
1515   set         ``pairCells``
1516   set         ``pairSet``
1548   procedure   ``checkHiddenPairs``
1567   procedure   ``checkHiddenTriplesInRow``
1574   array       ``columnsWithDigit``
1575   set         ``tripleColumns``
1576   set         ``tripleSet``
1611   procedure   ``checkHiddenTriplesInColumn``
1618   array       ``rowsWithDigit``
1619   set         ``tripleRows``
1620   set         ``tripleSet``
1655   procedure   ``checkHiddenTriplesInBox``
1663   array       ``cellsWithDigit``
1664   set         ``tripleCells``
1665   set         ``tripleSet``
1706   procedure   ``checkHiddenTriples``
1725   procedure   ``checkHiddenQuadsInRow``
1733   array       ``columnsWithDigit``
1734   set         ``quadColumns``
1735   set         ``quadSet``
1776   procedure   ``checkHiddenQuadsInColumn``
1784   array       ``rowsWithDigit``
1785   set         ``quadRows``
1786   set         ``quadSet``
1827   procedure   ``checkHiddenQuadsInBox``
1836   array       ``cellsWithDigit``
1837   set         ``quadCells``
1838   set         ``quadSet``
1885   procedure   ``checkHiddenQuads``
1904   procedure   ``checkXWingForDigit``
1913   array       ``rowsInColumn``
1914   array       ``columnsInRow``
1915   set         ``xWingColumns``
1916   set         ``xWingRows``
1971   procedure   ``checkXWing``
1981   procedure   ``checkSwordfishForDigit``
1992   array       ``rowsInColumn``
1993   array       ``columnsInRow``
1994   set         ``swordfishColumns``
1995   set         ``swordfishRows``
2066   procedure   ``checkSwordfish``
2076   procedure   ``solve``
2120   procedure   ``blueChanges``
2130   function    ``readCommand``
2142   procedure   ``showSolved``
2160   procedure   ``showNumberOfCandidates``
2180   procedure   ``showCandidatesDigit``
2200   procedure   ``showCandidates``
2222   procedure   ``showStrategyCheckboxes``
2277   procedure   ``hideStrategyCheckboxes``
2283   procedure   ``showButtons``
2318   procedure   ``loadField``
2336   procedure   ``processCommand``
2591   procedure   ``writeCentered``
2598   procedure   ``main``
====== =========== ==================================================

prg/sydir7.sd7
--------------

:Lines:    384
:Entities: 9

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
32     structure   ``syncFlags``
43     procedure   ``syncFile``
47     procedure   ``syncDir``
50     array       ``sourceContent``
51     array       ``destContent``
111    function    ``equalFileContent``
140    procedure   ``syncFile``
147    array       ``dirContent``
288    procedure   ``main``
====== =========== ==================================================

prg/syntaxhl.sd7
----------------

:Lines:    177
:Entities: 6

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
105    array       ``emptyArray``
110    enumeration ``myEnum``
113    structure   ``myStruct``
116    function    ``test``
117    function    ``test``
120    procedure   ``main``
====== =========== ==================================================

prg/tak.sd7
-----------

:Lines:    59
:Entities: 2

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
35     function    ``tak``
49     procedure   ``main``
====== =========== ==================================================

prg/tar7.sd7
------------

:Lines:    121
:Entities: 3

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
32     procedure   ``main``
40     array       ``arg_list``
41     array       ``fileList``
====== =========== ==================================================

prg/tch.sd7
-----------

:Lines:    55
:Entities: 1

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
29     procedure   ``main``
====== =========== ==================================================

prg/testfont.sd7
----------------

:Lines:    95
:Entities: 2

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
45     array       ``fontList``
50     procedure   ``main``
====== =========== ==================================================

prg/tet.sd7
-----------

:Lines:    479
:Entities: 22

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
47     array       ``occupied``
49     enumeration ``rot_position``
53     enumeration ``tetromino_type``
57     function    ``score``
67     array       ``tetromino_list``
72     function    ``PATTERN``
185    structure   ``piece``
194    procedure   ``next``
204    procedure   ``prev``
214    procedure   ``show``
232    procedure   ``hide``
250    function    ``is_occupied``
269    procedure   ``do_occupie``
285    procedure   ``left``
296    procedure   ``right``
307    procedure   ``rotate``
318    procedure   ``down``
331    procedure   ``position``
341    procedure   ``drop``
355    procedure   ``set_piece``
405    procedure   ``remove_full_lines``
428    procedure   ``main``
====== =========== ==================================================

prg/tetg.sd7
------------

:Lines:    501
:Entities: 23

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
53     array       ``occupied``
55     enumeration ``rot_position``
59     enumeration ``tetromino_type``
63     function    ``score``
73     function    ``color``
83     array       ``tetromino_list``
88     function    ``PATTERN``
201    structure   ``piece``
210    procedure   ``next``
220    procedure   ``prev``
230    procedure   ``show``
247    procedure   ``hide``
264    function    ``is_occupied``
283    procedure   ``do_occupie``
299    procedure   ``left``
310    procedure   ``right``
321    procedure   ``rotate``
332    procedure   ``down``
345    procedure   ``position``
355    procedure   ``drop``
369    procedure   ``set_piece``
421    procedure   ``remove_full_lines``
447    procedure   ``main``
====== =========== ==================================================

prg/toutf8.sd7
--------------

:Lines:    240
:Entities: 1

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
34     procedure   ``main``
====== =========== ==================================================

prg/tst_cli.sd7
---------------

:Lines:    40
:Entities: 1

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
30     procedure   ``main``
====== =========== ==================================================

prg/tst_srv.sd7
---------------

:Lines:    47
:Entities: 1

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
30     procedure   ``main``
====== =========== ==================================================

prg/wator.sd7
-------------

:Lines:    651
:Entities: 16

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
61     structure   ``cellType``
73     array       ``sumContent``
79     procedure   ``introduction``
122    procedure   ``display``
153    procedure   ``writeInfo``
203    procedure   ``initInfo``
223    procedure   ``readNumber``
253    procedure   ``initialize``
306    procedure   ``moveFish``
322    procedure   ``moveAllFish``
333    array       ``moveopts``
413    procedure   ``killFish``
430    procedure   ``moveShark``
447    procedure   ``moveAllSharks``
457    array       ``moveopts``
563    procedure   ``main``
====== =========== ==================================================

prg/which.sd7
-------------

:Lines:    65
:Entities: 1

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
29     procedure   ``main``
====== =========== ==================================================

prg/wiz.sd7
-----------

:Lines:    2833
:Entities: 132

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
43     enumeration ``speciesType``
47     function    ``str``
53     enumeration ``directType``
57     function    ``str``
65     enumeration ``objectType``
70     function    ``str``
83     enumeration ``animateType``
88     function    ``str``
94     enumeration ``armorType``
98     function    ``str``
104    enumeration ``weaponType``
108    function    ``str``
114    enumeration ``commandType``
120    enumeration ``contentType``
125    enumeration ``transferType``
129    enumeration ``occurType``
134    structure   ``playerType``
159    structure   ``fightStateType``
170    structure   ``roomType``
187    procedure   ``writePos``
188    procedure   ``enterRoom``
190    procedure   ``incident``
192    procedure   ``teleportTo``
194    procedure   ``writeFightState``
196    procedure   ``executeCommand``
198    procedure   ``removeFromRoom``
212    function    ``rangeLaby``
216    function    ``rangeLevel``
220    function    ``range18``
224    function    ``rand``
228    function    ``rand``
232    procedure   ``startText``
261    procedure   ``writeHelp``
288    procedure   ``randomRoom``
297    procedure   ``findUninhabitedRoom``
308    procedure   ``findEmptyRoom``
322    procedure   ``initRoom``
332    procedure   ``initEntrance``
340    procedure   ``initRoomConnections``
342    array       ``restrictedConnections``
392    procedure   ``initRoomProperties``
442    procedure   ``initRoomTransfers``
470    function    ``aOrAn``
484    procedure   ``DECLARE_A_OR_AN``
486    function    ``aOrAn``
495    function    ``anyFood``
500    function    ``anyAdjective``
506    function    ``numberName``
510    function    ``sexName``
514    function    ``titleName``
518    function    ``countOwnedObjects``
522    function    ``countOwnedTreasures``
526    function    ``ownedTreasure``
530    function    ``treasureNumber``
534    procedure   ``removeFromRoom``
541    procedure   ``writePos``
547    procedure   ``findGoldPieces``
558    procedure   ``findFlares``
569    function    ``roomAdjective``
575    function    ``roomIdInLevel``
579    function    ``roomId``
583    procedure   ``writeRoomDescription``
656    procedure   ``writeConnections``
724    procedure   ``writeThings``
737    procedure   ``writeAnimates``
745    procedure   ``writeObjects``
772    procedure   ``writeRoomDetails``
781    procedure   ``look``
794    function    ``readChar``
807    function    ``readChoice``
817    procedure   ``readNumber``
841    procedure   ``readSpecies``
866    procedure   ``readSex``
889    procedure   ``riseAttribute``
922    procedure   ``readAttributes``
961    procedure   ``buyAttribute``
999    procedure   ``buyArmor``
1053   procedure   ``buyWeapon``
1113   procedure   ``buyLamp``
1144   procedure   ``buyFlares``
1178   procedure   ``buyTreasures``
1216   procedure   ``buy``
1257   procedure   ``sellTreasures``
1294   procedure   ``sell``
1309   procedure   ``checkArmor``
1334   procedure   ``vendorDies``
1362   procedure   ``monsterDies``
1396   procedure   ``monsterAttacks``
1420   procedure   ``attackMonster``
1453   procedure   ``castSpell``
1498   procedure   ``bribeMonster``
1538   procedure   ``meetMonster``
1571   procedure   ``retreatFromMonster``
1599   procedure   ``attack``
1616   procedure   ``cast``
1636   procedure   ``bribe``
1656   procedure   ``meetVendor``
1667   procedure   ``enterRoom``
1721   procedure   ``readCoordinate``
1747   procedure   ``teleportTo``
1769   procedure   ``teleport``
1797   procedure   ``go``
1800   array       ``delta_x``
1801   array       ``delta_y``
1802   array       ``delta_z``
1830   procedure   ``status``
1848   procedure   ``listInventory``
1881   procedure   ``inventory``
1889   procedure   ``contentInfo``
1936   procedure   ``map``
1960   procedure   ``flare``
1992   procedure   ``shineIntoRoom``
2004   procedure   ``lamp``
2038   procedure   ``drink``
2093   procedure   ``read``
2139   procedure   ``viewRoomWithOrb``
2151   procedure   ``gazeIntoOrb``
2238   procedure   ``gaze``
2276   procedure   ``openChest``
2342   procedure   ``open``
2354   procedure   ``quit``
2367   procedure   ``wait``
2374   procedure   ``illegal``
2381   function    ``readCommand``
2451   procedure   ``executeCommand``
2485   procedure   ``writeFightState``
2507   procedure   ``incident``
2603   procedure   ``curesAndDissolves``
2633   procedure   ``writeRemark``
2724   procedure   ``die``
2746   procedure   ``exitCastle``
2773   procedure   ``main``
====== =========== ==================================================

prg/wordcnt.sd7
---------------

:Lines:    54
:Entities: 1

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
32     procedure   ``main``
====== =========== ==================================================

prg/wrinum.sd7
--------------

:Lines:    43
:Entities: 1

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
34     procedure   ``main``
====== =========== ==================================================

prg/wumpus.sd7
--------------

:Lines:    372
:Entities: 15

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
38     array       ``loc``
39     array       ``save``
41     enumeration ``status_type``
47     array       ``cave``
70     procedure   ``print_instructions``
117    procedure   ``check_hazards``
146    function    ``shoot_or_move``
166    procedure   ``check_shot``
178    procedure   ``move_wumpus``
193    procedure   ``shoot_arrow``
199    array       ``path``
246    function    ``readmove``
277    procedure   ``move_player``
302    procedure   ``init_setup``
323    procedure   ``main``
====== =========== ==================================================

lib
===

lib/aes.s7i
-----------

:Lines:    1144
:Entities: 19

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
39     structure   ``aesState``
40     array       ``encryptionSubKey``
41     array       ``decryptionSubKey``
56     array       ``Te``
384    array       ``Td``
712    array       ``rcon``
720    procedure   ``expandAesKey128``
742    procedure   ``expandAesKey192``
768    procedure   ``expandAesKey256``
801    function    ``setAesKey``
803    array       ``encryptKey``
826    function    ``genAesDecryptKey``
828    array       ``decryptKey``
873    function    ``setAesKey``
890    function    ``setCipherKey``
895    function    ``encodeAesBlock``
995    function    ``decodeAesBlock``
1099   function    ``encode``
1125   function    ``decode``
====== =========== ==================================================

lib/aes_gcm.s7i
---------------

:Lines:    392
:Entities: 17

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
36     structure   ``factorHType``
42     function    ``computeFactorH``
78     array       ``last4``
89     function    ``gcmMult``
134    structure   ``aesGcmState``
166    function    ``setAesGcmKey``
189    function    ``setCipherKey``
200    procedure   ``initAead``
213    function    ``getComputedMac``
222    function    ``getMac``
229    procedure   ``incrementGcmNonceCounter``
243    function    ``aesGcmEncode``
281    function    ``aesGcmDecode``
319    procedure   ``initializeComputedMac``
331    procedure   ``finalizeComputedMac``
356    function    ``encode``
376    function    ``decode``
====== =========== ==================================================

lib/ar.s7i
----------

:Lines:    1532
:Entities: 49

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
47     structure   ``arHeader``
64     procedure   ``showHeader``
79     procedure   ``assignFilePath``
113    function    ``arHeader``
129    procedure   ``readHead``
145    procedure   ``readMinimumOfHead``
161    function    ``str``
188    procedure   ``writeHead``
194    function    ``getLongName``
221    function    ``addLongName``
272    structure   ``arArchive``
286    function    ``openAr``
345    function    ``openAr``
359    procedure   ``close``
365    function    ``addToCatalog``
377    function    ``addImplicitDir``
389    function    ``followSymlink``
431    function    ``followSymlink``
445    procedure   ``fixRegisterAndCatalog``
464    procedure   ``setHeaderFileName``
522    function    ``readDir``
533    function    ``readDir``
548    function    ``fileType``
609    function    ``fileTypeSL``
655    function    ``getFileMode``
674    procedure   ``setFileMode``
705    function    ``fileSize``
727    function    ``getMTime``
751    procedure   ``setMTime``
784    function    ``getOwner``
806    procedure   ``setOwner``
842    function    ``getGroup``
864    procedure   ``setGroup``
901    function    ``getFileMode``
939    function    ``getMTime``
978    procedure   ``setMTime``
1020   function    ``getOwner``
1059   procedure   ``setOwner``
1104   function    ``getGroup``
1143   procedure   ``setGroup``
1186   function    ``readLink``
1228   procedure   ``makeLink``
1278   function    ``getFile``
1305   procedure   ``putFile``
1384   procedure   ``makeDir``
1442   procedure   ``removeFile``
1488   procedure   ``for``
1498   function    ``openFileInAr``
1529   function    ``open``
====== =========== ==================================================

lib/arc4.s7i
------------

:Lines:    144
:Entities: 7

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
38     structure   ``arc4State``
39     array       ``s``
59     function    ``setArc4Key``
90     function    ``setCipherKey``
95     function    ``getArc4PseudoRandomByte``
114    function    ``encode``
132    function    ``decode``
====== =========== ==================================================

lib/archive.s7i
---------------

:Lines:    143
:Entities: 3

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
48     function    ``openArchive``
102    function    ``openArchive``
118    function    ``openArchiveByExtension``
====== =========== ==================================================

lib/archive_base.s7i
--------------------

:Lines:    135
:Entities: 5

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
52     function    ``readDir``
54     array       ``fileNames``
58     set         ``fileNameSet``
105    function    ``implicitDir``
123    function    ``isEmptyDir``
====== =========== ==================================================

lib/array.s7i
-------------

:Lines:    610
:Entities: 49

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
29     structure   ``ARRAY_IDX_RANGE``
34     function    ``..``
43     function    ``len``
56     function    ``array``
74     procedure   ``:=``
81     procedure   ``&:=``
88     procedure   ``&:=``
99     function    ``&``
117    function    ``..``
134    function    ``..``
144    function    ``len``
159    procedure   ``insert``
170    procedure   ``insert``
186    function    ``remove``
197    function    ``remove``
207    function    ``length``
216    function    ``minIdx``
225    function    ``maxIdx``
236    function    ``times``
238    function    ``.``
241    function    ``conv``
258    function    ``times``
266    procedure   ``for``
281    procedure   ``for``
295    procedure   ``for``
310    procedure   ``for``
333    procedure   ``for``
361    procedure   ``for``
372    procedure   ``for``
390    procedure   ``for``
413    procedure   ``for``
442    function    ``rand``
448    function    ``=``
464    function    ``<>``
485    procedure   ``insert``
504    function    ``compare``
524    function    ``<``
527    function    ``>``
530    function    ``<=``
533    function    ``>=``
538    function    ``SORT``
540    function    ``SORT_REVERSE``
555    function    ``sort``
558    function    ``sort``
574    function    ``sort``
577    function    ``sort``
601    procedure   ``ENABLE_SORT``
605    function    ``SORT``
607    function    ``sort``
====== =========== ==================================================

lib/asn1.s7i
------------

:Lines:    544
:Entities: 26

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
33     array       ``classTagName``
71     enumeration ``asn1TagType``
106    enumeration ``asn1TagClass``
114    structure   ``asn1DataElement``
125    function    ``.``
127    function    ``.``
131    function    ``encodeObjectIdentifier``
161    function    ``decodeObjectIdentifier``
163    array       ``oid``
184    function    ``objectIdentifier``
218    function    ``getAsn1DataElement``
264    function    ``getData``
279    procedure   ``skipData``
285    function    ``genAsn1Length``
303    function    ``asn1TagTypeOfString``
307    set         ``visibleChar``
308    set         ``printableChar``
337    function    ``genAsn1Element``
353    function    ``genAsn1Integer``
372    function    ``genAsn1String``
392    function    ``genAsn1Sequence``
408    function    ``genAsn1Set``
423    function    ``genExplicitAsn1Tag``
434    procedure   ``printAsn1``
522    procedure   ``printAsn1``
532    procedure   ``searchLengthByte``
====== =========== ==================================================

lib/asn1oid.s7i
---------------

:Lines:    157
:Entities: 3

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
102    structure   ``algorithmIdentifierType``
109    function    ``getAlgorithmIdentifier``
143    function    ``genAlgorithmIdentifier``
====== =========== ==================================================

lib/basearray.s7i
-----------------

:Lines:    450
:Entities: 35

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
42     function    ``array``
58     procedure   ``:=``
65     procedure   ``&:=``
72     procedure   ``&:=``
78     function    ``&``
96     function    ``..``
113    function    ``..``
123    function    ``len``
134    procedure   ``insert``
145    procedure   ``insert``
157    function    ``remove``
168    function    ``remove``
175    function    ``length``
195    function    ``maxIdx``
197    function    ``SET_MIN_IDX``
199    function    ``SET_MIN_IDX``
201    function    ``SET_MIN_IDX``
203    function    ``TIMES``
211    function    ``;``
218    procedure   ``:=``
220    function    ``ord``
222    function    ``conv``
227    function    ``times``
234    procedure   ``for``
249    procedure   ``for``
263    procedure   ``for``
278    procedure   ``for``
301    procedure   ``for``
329    procedure   ``for``
340    procedure   ``for``
358    procedure   ``for``
381    procedure   ``for``
410    function    ``rand``
416    function    ``=``
431    function    ``<>``
====== =========== ==================================================

lib/bigfile.s7i
---------------

:Lines:    136
:Entities: 12

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
35     function    ``bigLength``
36     procedure   ``seek``
37     function    ``bigTell``
42     function    ``bigLength``
43     procedure   ``seek``
44     function    ``bigTell``
58     function    ``bigLength``
73     procedure   ``seek``
87     function    ``bigTell``
106    function    ``bigLength``
119    procedure   ``seek``
134    function    ``bigTell``
====== =========== ==================================================

lib/bigint.s7i
--------------

:Lines:    824
:Entities: 60

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
51     procedure   ``:=``
64     function    ``+``
71     function    ``-``
78     function    ``+``
85     function    ``-``
92     function    ``*``
107    function    ``div``
119    function    ``rem``
135    function    ``mdiv``
147    function    ``mod``
159    function    ``**``
172    function    ``<<``
182    function    ``>>``
188    procedure   ``+:=``
194    procedure   ``-:=``
200    procedure   ``*:=``
207    procedure   ``<<:=``
214    procedure   ``>>:=``
222    function    ``=``
230    function    ``<>``
238    function    ``<``
246    function    ``>``
254    function    ``<=``
262    function    ``>=``
271    function    ``compare``
278    function    ``hashCode``
281    structure   ``quotRem``
287    function    ``quotRem``
296    function    ``=``
301    function    ``<>``
312    function    ``divRem``
320    function    ``succ``
328    function    ``pred``
335    function    ``abs``
349    function    ``log10``
363    function    ``log2``
371    function    ``odd``
379    function    ``ord``
387    function    ``integer``
395    function    ``gcd``
403    function    ``bigInteger``
411    function    ``conv``
421    function    ``str``
429    function    ``literal``
445    function    ``radix``
460    function    ``RADIX``
483    function    ``bigInteger``
506    function    ``parse``
530    function    ``bigInteger``
540    function    ``rand``
556    function    ``bitLength``
571    function    ``lowestSetBit``
580    procedure   ``incr``
589    procedure   ``decr``
621    function    ``sci``
666    function    ``!``
692    function    ``!``
725    function    ``sqrt``
749    function    ``modInverse``
799    function    ``modPow``
====== =========== ==================================================

lib/bigrat.s7i
--------------

:Lines:    784
:Entities: 42

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
41     structure   ``bigRational``
47     procedure   ``normalize``
56     procedure   ``reduce``
72     function    ``/``
87     function    ``+``
95     function    ``-``
108    function    ``+``
126    function    ``-``
144    function    ``*``
163    function    ``/``
181    procedure   ``+:=``
196    procedure   ``-:=``
211    procedure   ``*:=``
222    procedure   ``/:=``
235    function    ``**``
255    function    ``=``
265    function    ``<>``
275    function    ``<``
285    function    ``>``
295    function    ``<=``
305    function    ``>=``
316    function    ``compare``
325    function    ``hashCode``
332    function    ``rat``
344    function    ``bigRational``
356    function    ``conv``
368    function    ``bigRational``
380    function    ``conv``
393    function    ``abs``
405    function    ``floor``
412    function    ``ceil``
419    function    ``trunc``
428    function    ``round``
445    function    ``round10``
479    function    ``str``
524    function    ``literal``
533    function    ``fraction``
560    function    ``digits``
594    function    ``decimalExponent``
631    function    ``sci``
700    function    ``bigRational``
773    function    ``parse``
====== =========== ==================================================

lib/bin16.s7i
-------------

:Lines:    592
:Entities: 39

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
44     procedure   ``:=``
51     function    ``bin16``
65     function    ``bin16``
81     function    ``conv``
88     function    ``conv``
95     function    ``ord``
102    function    ``integer``
109    function    ``char``
117    function    ``bin64``
124    function    ``bin16``
133    function    ``compare``
140    function    ``hashCode``
148    function    ``rand``
160    function    ``bitLength``
172    function    ``lowestSetBit``
181    function    ``str``
194    function    ``radix``
207    function    ``RADIX``
225    function    ``bytes``
243    function    ``bytes``
251    function    ``=``
259    function    ``<>``
267    function    ``&``
275    function    ``|``
283    function    ``><``
291    function    ``~``
303    function    ``<<``
314    function    ``>>``
322    procedure   ``<<:=``
331    procedure   ``>>:=``
337    procedure   ``&:=``
343    procedure   ``|:=``
349    procedure   ``><:=``
361    function    ``rotLeft``
374    function    ``rotRight``
406    function    ``float``
472    function    ``bin16``
570    function    ``bin16``
579    function    ``bin16``
====== =========== ==================================================

lib/bin32.s7i
-------------

:Lines:    490
:Entities: 39

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
43     procedure   ``:=``
50     function    ``bin32``
63     function    ``bin32``
76     function    ``bin32``
83     function    ``conv``
90     function    ``conv``
97     function    ``ord``
104    function    ``integer``
111    function    ``char``
124    function    ``float``
133    function    ``compare``
140    function    ``hashCode``
148    function    ``rand``
160    function    ``bitLength``
172    function    ``lowestSetBit``
181    function    ``str``
194    function    ``radix``
207    function    ``RADIX``
225    function    ``bytes``
243    function    ``bytes``
251    function    ``=``
259    function    ``<>``
267    function    ``&``
275    function    ``|``
283    function    ``><``
291    function    ``~``
303    function    ``<<``
314    function    ``>>``
322    procedure   ``<<:=``
331    procedure   ``>>:=``
337    procedure   ``&:=``
343    procedure   ``|:=``
349    procedure   ``><:=``
361    function    ``rotLeft``
374    function    ``rotRight``
392    function    ``float2MbfBits``
437    function    ``mbfBits2Float``
464    function    ``bin32``
475    function    ``bin32``
====== =========== ==================================================

lib/bin64.s7i
-------------

:Lines:    539
:Entities: 43

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
44     procedure   ``:=``
56     function    ``bin64``
77     function    ``bin64``
84     function    ``bin64``
97     function    ``bin64``
104    function    ``conv``
111    function    ``conv``
118    function    ``ord``
125    function    ``integer``
133    function    ``big``
141    function    ``bigInteger``
148    function    ``char``
161    function    ``float``
170    function    ``compare``
177    function    ``hashCode``
185    function    ``rand``
197    function    ``bitLength``
209    function    ``lowestSetBit``
219    function    ``str``
232    function    ``radix``
245    function    ``RADIX``
263    function    ``bytes``
281    function    ``bytes``
289    function    ``=``
297    function    ``<>``
305    function    ``&``
313    function    ``|``
321    function    ``><``
329    function    ``~``
342    function    ``<<``
353    function    ``>>``
362    procedure   ``<<:=``
371    procedure   ``>>:=``
377    procedure   ``&:=``
383    procedure   ``|:=``
389    procedure   ``><:=``
401    function    ``rotLeft``
414    function    ``rotRight``
422    function    ``getBinary``
440    function    ``float2MbfBits``
482    function    ``mbfBits2Float``
515    function    ``bin64``
528    function    ``bin64``
====== =========== ==================================================

lib/bitdata.s7i
---------------

:Lines:    1330
:Entities: 52

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
41     array       ``reverseBits``
109    function    ``reverseBits``
123    function    ``reverseByteBits``
146    structure   ``lsbInBitStream``
161    procedure   ``show``
176    procedure   ``fillStriBuffer``
195    procedure   ``fillBuffer``
209    function    ``openLsbInBitStream``
224    function    ``openLsbInBitStream``
245    procedure   ``close``
282    function    ``getBit``
327    function    ``getBits``
378    function    ``peekBits``
399    procedure   ``skipBits``
430    function    ``eof``
463    function    ``gets``
500    structure   ``msbInBitStream``
515    procedure   ``show``
530    procedure   ``fillStriBuffer``
549    procedure   ``fillBuffer``
563    function    ``openMsbInBitStream``
578    function    ``openMsbInBitStream``
599    procedure   ``close``
638    function    ``getBit``
683    function    ``getBits``
726    function    ``peekBits``
747    procedure   ``skipBits``
775    function    ``eof``
808    function    ``gets``
850    structure   ``lsbOutBitStream``
878    procedure   ``putBit``
908    procedure   ``putBits``
924    procedure   ``&:=``
941    function    ``length``
954    procedure   ``truncate``
988    procedure   ``flush``
1007   function    ``getBytes``
1022   procedure   ``write``
1040   structure   ``msbOutBitStream``
1068   procedure   ``putBit``
1099   procedure   ``putBits``
1122   procedure   ``&:=``
1139   function    ``length``
1152   procedure   ``truncate``
1188   procedure   ``flush``
1207   function    ``getBytes``
1222   procedure   ``write``
1234   structure   ``reverseBitStream``
1243   function    ``reverseBitStream``
1257   function    ``bitsStillInStream``
1266   function    ``bitsRead``
1277   function    ``getBits``
====== =========== ==================================================

lib/bitmapfont.s7i
------------------

:Lines:    215
:Entities: 8

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
45     structure   ``bitmapFont``
57     function    ``width``
84     function    ``numOfCharsInWidth``
117    function    ``genPixmap``
149    function    ``genPixmapFont``
176    function    ``getFontCharPixmap``
191    procedure   ``setFont``
203    function    ``columnWidth``
====== =========== ==================================================

lib/bitset.s7i
--------------

:Lines:    593
:Entities: 39

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
39     procedure   ``:=``
46     function    ``_GENERATE_EMPTY_SET``
64     function    ``|``
73     function    ``&``
82     function    ``><``
91     function    ``-``
98     procedure   ``|:=``
105    procedure   ``&:=``
112    procedure   ``-:=``
128    function    ``=``
144    function    ``<>``
164    function    ``<``
184    function    ``>``
204    function    ``<=``
224    function    ``>=``
249    function    ``compare``
256    function    ``hashCode``
267    function    ``in``
278    function    ``not``
286    procedure   ``incl``
293    procedure   ``excl``
302    procedure   ``@:=``
319    function    ``card``
329    function    ``rand``
342    function    ``min``
355    function    ``max``
368    function    ``next``
400    function    ``integer``
410    function    ``conv``
420    function    ``bitset``
430    function    ``conv``
436    procedure   ``for``
457    procedure   ``for``
480    procedure   ``for``
494    function    ``toArray``
496    array       ``values``
526    function    ``str``
550    function    ``bitset``
580    function    ``parse``
====== =========== ==================================================

lib/bitsetof.s7i
----------------

:Lines:    431
:Entities: 37

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
35     function    ``bitset``
52     procedure   ``:=``
56     function    ``bitset``
57     function    ``conv``
58     function    ``varConv``
59     function    ``conv``
73     function    ``|``
81     function    ``&``
89     function    ``><``
97     function    ``-``
103    procedure   ``|:=``
109    procedure   ``&:=``
115    procedure   ``-:=``
122    function    ``=``
129    function    ``<>``
139    function    ``<``
149    function    ``>``
159    function    ``<=``
169    function    ``>=``
183    function    ``compare``
189    function    ``hashCode``
199    function    ``in``
210    function    ``not``
218    procedure   ``incl``
227    procedure   ``excl``
238    procedure   ``@:=``
253    function    ``card``
261    function    ``rand``
273    function    ``min``
285    function    ``max``
297    function    ``next``
323    procedure   ``for``
344    procedure   ``for``
367    procedure   ``for``
381    function    ``toArray``
383    array       ``values``
412    function    ``str``
====== =========== ==================================================

lib/blowfish.s7i
----------------

:Lines:    383
:Entities: 12

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
38     structure   ``blowfishState``
39     array       ``pBox``
40     array       ``sBox``
53     array       ``pBoxInit``
60     array       ``sBoxInit``
235    function    ``f``
242    procedure   ``encrypt``
264    procedure   ``decrypt``
291    function    ``setBlowfishKey``
336    function    ``setCipherKey``
345    function    ``encode``
367    function    ``decode``
====== =========== ==================================================

lib/bmp.s7i
-----------

:Lines:    924
:Entities: 37

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
64     structure   ``bmpColorBitfield``
70     structure   ``bmpHeader``
90     array       ``colorSpaceEndpoints``
99     function    ``bmpColorBitfield``
110    function    ``str``
116    function    ``toColor``
120    procedure   ``readBitMasks``
144    procedure   ``showHeader``
169    procedure   ``readDibHeader``
236    procedure   ``readHeader``
255    procedure   ``computeNumberOfPaletteColors``
267    procedure   ``readPaletteData``
293    procedure   ``readPalette``
314    procedure   ``readBmpImageLine1Bit``
344    procedure   ``readBmpImage1Bit``
363    procedure   ``readBmpImageLine2Bit``
389    procedure   ``readBmpImage2Bit``
408    procedure   ``readBmpImageLine4Bit``
427    procedure   ``readBmpImage4Bit``
446    procedure   ``readBmpImageLine8Bit``
463    procedure   ``readBmpImage8Bit``
482    procedure   ``readBmpBitfieldImageLine16Bit``
501    procedure   ``readBmpNoBitfieldImageLine16Bit``
520    procedure   ``readBmpImage16Bit``
541    procedure   ``readBmpImageLine24Bit``
558    procedure   ``readBmpImage24Bit``
571    procedure   ``readBmpBitfieldImageLine32Bit``
590    procedure   ``readBmpNoBitfieldImageLine32Bit``
607    procedure   ``readBmpImage32Bit``
628    procedure   ``processHuffman1d``
646    procedure   ``readBmpImageRle4``
706    procedure   ``readBmpImageRle8``
757    function    ``readBmp``
827    function    ``readBmp``
851    function    ``readBmp``
870    function    ``str``
914    procedure   ``writeBmp``
====== =========== ==================================================

lib/boolean.s7i
---------------

:Lines:    403
:Entities: 39

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
39     procedure   ``:=``
83     function    ``not``
94     function    ``and``
95     function    ``and``
96     function    ``and``
107    function    ``or``
108    function    ``or``
109    function    ``or``
117    procedure   ``&:=``
125    procedure   ``|:=``
133    procedure   ``><:=``
141    function    ``=``
150    function    ``<>``
158    function    ``<``
166    function    ``>``
174    function    ``<=``
182    function    ``>=``
191    function    ``compare``
198    function    ``hashCode``
206    function    ``ord``
214    function    ``integer``
222    function    ``conv``
231    function    ``boolean``
240    function    ``conv``
243    function    ``varConv``
257    function    ``succ``
267    function    ``pred``
273    procedure   ``incr``
282    procedure   ``decr``
295    function    ``str``
305    function    ``literal``
314    function    ``parse``
323    function    ``boolean``
334    function    ``rand``
337    procedure   ``DECLARE_TERNARY``
345    function    ``?``
347    function    ``?``
349    function    ``?``
351    function    ``?``
====== =========== ==================================================

lib/browser.s7i
---------------

:Lines:    280
:Entities: 7

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
39     structure   ``browserConnection``
47     function    ``processHttpRequest``
146    function    ``openBrowser``
192    procedure   ``close``
203    procedure   ``acceptNewSock``
219    procedure   ``sendClientError``
245    procedure   ``display``
====== =========== ==================================================

lib/bstring.s7i
---------------

:Lines:    227
:Entities: 17

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
43     procedure   ``:=``
45     function    ``_GENERATE_EMPTY_BSTRING``
59     function    ``str``
67     function    ``literal``
76     function    ``string``
85     function    ``bstring``
94     function    ``parse``
112    function    ``length``
120    function    ``=``
128    function    ``<>``
137    function    ``compare``
144    function    ``hashCode``
150    procedure   ``for``
173    function    ``bStriBe2BigInt``
187    function    ``bStriLe2BigInt``
204    function    ``bStriBe``
221    function    ``bStriLe``
====== =========== ==================================================

lib/bytedata.s7i
----------------

:Lines:    482
:Entities: 41

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
41     enumeration ``signedness``
53     enumeration ``endianness``
64     function    ``fromAsciiz``
90     function    ``getAsciiz``
113    function    ``getAsciiz``
134    function    ``hex``
159    function    ``hex2Bytes``
176    function    ``bytes``
178    function    ``bytes``
180    function    ``bytes``
182    function    ``bytes``
203    function    ``bytes``
207    function    ``bytes``
209    function    ``bytes``
211    function    ``bytes``
213    function    ``bytes``
250    function    ``bytes``
254    function    ``bytes``
257    function    ``bytes``
260    function    ``bytes``
263    function    ``bytes``
290    function    ``bytes``
294    function    ``bytes``
307    function    ``bytes``
320    function    ``bytes``
337    function    ``bytes``
381    function    ``bytes``
385    function    ``bytes2Int``
387    function    ``bytes2Int``
389    function    ``bytes2Int``
391    function    ``bytes2Int``
413    function    ``bytes2Int``
417    function    ``bytes2BigInt``
420    function    ``bytes2BigInt``
423    function    ``bytes2BigInt``
426    function    ``bytes2BigInt``
452    function    ``bytes2BigInt``
459    function    ``getUInt16Le``
466    function    ``getUInt32Le``
473    function    ``getUInt16Be``
480    function    ``getUInt32Be``
====== =========== ==================================================

lib/bzip2.s7i
-------------

:Lines:    887
:Entities: 35

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
97     structure   ``bzip2Header``
103    procedure   ``showHeader``
110    function    ``readBzip2Header``
123    function    ``readBzip2Header``
139    structure   ``bzip2BlockHeader``
149    procedure   ``showHeader``
159    procedure   ``readMappingTable``
188    procedure   ``readHuffmanGroups``
199    structure   ``bzip2HuffmanDecoder``
205    structure   ``bzip2DecoderState``
212    function    ``getHuffmanSymbol``
227    procedure   ``readSelectors``
234    array       ``mtfSelector``
268    procedure   ``readHuffmanDecoders``
273    array       ``codeLengths``
313    procedure   ``initMoveToFront``
339    procedure   ``moveMtfDataToTheEnd``
384    function    ``decodeMoveToFront``
447    procedure   ``processInputData``
494    function    ``computeFrequenciesSum``
510    function    ``inverseBurrowsWheelerTransform``
538    function    ``getBwtByte``
555    function    ``runLengthDecoding``
612    function    ``decompressBzip2Block``
638    function    ``bzip2Decompress``
670    function    ``bzip2Decompress``
699    structure   ``bzip2File``
716    function    ``openBzip2File``
744    function    ``getc``
770    function    ``gets``
804    function    ``eof``
814    function    ``hasNext``
837    function    ``length``
869    procedure   ``seek``
885    function    ``tell``
====== =========== ==================================================

lib/cards.s7i
-------------

:Lines:    1342
:Entities: 59

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
32     array       ``black_A_sign``
46     array       ``black_K_sign``
61     array       ``black_Q_sign``
78     array       ``black_J_sign``
93     array       ``black_10_sign``
108    array       ``black_9_sign``
123    array       ``black_8_sign``
138    array       ``black_7_sign``
153    array       ``black_6_sign``
168    array       ``black_5_sign``
183    array       ``black_4_sign``
198    array       ``black_3_sign``
213    array       ``black_2_sign``
228    array       ``red_A_sign``
242    array       ``red_K_sign``
257    array       ``red_Q_sign``
274    array       ``red_J_sign``
289    array       ``red_10_sign``
304    array       ``red_9_sign``
319    array       ``red_8_sign``
334    array       ``red_7_sign``
349    array       ``red_6_sign``
364    array       ``red_5_sign``
379    array       ``red_4_sign``
394    array       ``red_3_sign``
409    array       ``red_2_sign``
424    array       ``small_spades_sign``
436    array       ``small_hearts_sign``
449    array       ``small_diamonds_sign``
462    array       ``small_clubs_sign``
475    array       ``big_spades_sign``
495    array       ``big_hearts_sign``
515    array       ``big_diamonds_sign``
535    array       ``big_clubs_sign``
555    array       ``king_of_spades_pic``
594    array       ``queen_of_spades_pic``
633    array       ``jack_of_spades_pic``
672    array       ``king_of_hearts_pic``
711    array       ``queen_of_hearts_pic``
750    array       ``jack_of_hearts_pic``
789    array       ``king_of_diamonds_pic``
828    array       ``queen_of_diamonds_pic``
867    array       ``jack_of_diamonds_pic``
906    array       ``king_of_clubs_pic``
945    array       ``queen_of_clubs_pic``
984    array       ``jack_of_clubs_pic``
1023   array       ``backside_pic``
1072   procedure   ``drawPic``
1094   procedure   ``drawPic2``
1123   function    ``genPixmap``
1159   function    ``genPixmap``
1202   enumeration ``cardSuit``
1206   function    ``str``
1209   set         ``blackCardSuit``
1210   set         ``redCardSuit``
1216   enumeration ``cardRank``
1220   function    ``str``
1228   function    ``cardPixmap``
1316   function    ``cardBackside``
====== =========== ==================================================

lib/category.s7i
----------------

:Lines:    209
:Entities: 13

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
40     procedure   ``:=``
49     function    ``category``
62     function    ``=``
69     function    ``<>``
78     function    ``compare``
85     function    ``hashCode``
92     function    ``ord``
99     function    ``conv``
106    function    ``category``
115    function    ``str``
124    function    ``parse``
188    procedure   ``for``
202    procedure   ``for``
====== =========== ==================================================

lib/cc_conf.s7i
---------------

:Lines:    1314
:Entities: 20

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
37     structure   ``ccConfigType``
699    array       ``CC_OPT_DEBUG_INFO``
715    array       ``CC_OPT_OPTIMIZE_1``
720    array       ``CC_OPT_OPTIMIZE_2``
725    array       ``CC_OPT_OPTIMIZE_3``
749    array       ``CC_FLAGS``
818    array       ``LINKER_FLAGS``
834    array       ``SYSTEM_LIBS``
841    array       ``SYSTEM_BIGINT_LIBS``
848    array       ``SYSTEM_CONSOLE_LIBS``
855    array       ``SYSTEM_DATABASE_LIBS``
862    array       ``SYSTEM_DRAW_LIBS``
869    array       ``SYSTEM_MATH_LIBS``
954    function    ``configValue``
961    function    ``getBuiltInConfig``
1128   procedure   ``assignConfigValue``
1267   function    ``readConfig``
1282   function    ``determineCCVersion``
1286   array       ``redirection``
1312   function    ``ccVersionIsOkay``
====== =========== ==================================================

lib/ccittfax.s7i
----------------

:Lines:    1022
:Entities: 35

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
36     function    ``createMsbHuffmanDecoder``
46     procedure   ``addCode``
65     function    ``createLsbHuffmanDecoder``
76     procedure   ``addCode``
93     structure   ``huffmanSymbolBits``
99     function    ``huffmanSymbolBits``
108    array       ``whiteHuffmanSymbolBits``
216    array       ``blackHuffmanSymbolBits``
324    procedure   ``addHuffmanSymbols``
335    procedure   ``addHuffmanSymbols``
346    function    ``genWhiteMsbHuffmanDecoder``
355    function    ``genBlackMsbHuffmanDecoder``
364    function    ``genWhiteLsbHuffmanDecoder``
373    function    ``genBlackLsbHuffmanDecoder``
400    array       ``t4HuffmanSymbolBits``
414    function    ``genT4MsbHuffmanDecoder``
423    function    ``genT4LsbHuffmanDecoder``
436    procedure   ``DECLARE_CcittModifiedGroup3Fax_FUNCTIONS``
441    function    ``getWhiteBits``
463    function    ``getBlackBits``
485    procedure   ``skipToStart``
502    procedure   ``skipEol``
512    procedure   ``processCcittFaxRow``
568    procedure   ``processCcittModifiedGroup3FaxMsb``
602    procedure   ``processCcittModifiedGroup3FaxLsb``
623    procedure   ``DECLARE_CcittT6Fax_FUNCTIONS``
628    procedure   ``processCcittT4Fax2dRow``
784    procedure   ``processCcittT6FaxMsb``
815    procedure   ``processCcittT6FaxLsb``
847    procedure   ``processCcittT4Fax1dMsb``
880    procedure   ``processCcittT4Fax1dLsb``
898    procedure   ``DECLARE_CcittT4Fax2d_FUNCTIONS``
903    procedure   ``processCcittT4Fax1dRow``
960    procedure   ``processCcittT4Fax2dMsb``
999    procedure   ``processCcittT4Fax2dLsb``
====== =========== ==================================================

lib/cgi.s7i
-----------

:Lines:    109
:Entities: 1

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
53     array       ``paramArray``
====== =========== ==================================================

lib/cgidialog.s7i
-----------------

:Lines:    1118
:Entities: 102

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
47     structure   ``emptyDialog``
56     function    ``genDialogElementName``
64     structure   ``webPage``
70     procedure   ``assignName``
71     procedure   ``update``
72     function    ``toHtml``
75     function    ``webPage``
83     function    ``webForm``
92     procedure   ``update``
103    procedure   ``send``
138    structure   ``header``
149    function    ``header``
160    function    ``toHtml``
170    structure   ``label``
180    function    ``label``
190    function    ``toHtml``
199    structure   ``image``
212    function    ``image``
226    procedure   ``assignName``
234    function    ``toHtml``
243    structure   ``space``
253    function    ``space``
263    function    ``toHtml``
278    structure   ``vspace``
288    function    ``vspace``
298    function    ``toHtml``
309    structure   ``script``
315    function    ``script``
325    function    ``toHtml``
336    structure   ``textField``
348    function    ``textField``
357    procedure   ``assignName``
363    procedure   ``update``
370    function    ``toHtml``
384    structure   ``passwordField``
396    function    ``passwordField``
405    procedure   ``assignName``
411    procedure   ``update``
418    function    ``toHtml``
432    structure   ``textArea``
445    function    ``textArea``
454    procedure   ``assignName``
460    procedure   ``update``
467    function    ``toHtml``
479    structure   ``checkbox``
490    function    ``checkbox``
499    procedure   ``update``
504    function    ``toHtml``
517    structure   ``radioButton``
519    array       ``labelList``
529    function    ``radioButton``
537    procedure   ``assignName``
543    procedure   ``update``
554    function    ``toHtml``
571    structure   ``resetButton``
584    function    ``resetButton``
596    function    ``resetButton``
608    function    ``resetButton``
616    procedure   ``assignName``
624    function    ``toHtml``
647    structure   ``submitButton``
661    function    ``submitButton``
673    function    ``submitButton``
685    function    ``submitButton``
693    procedure   ``assignName``
699    procedure   ``update``
704    function    ``toHtml``
727    structure   ``closeButton``
739    function    ``closeButton``
747    procedure   ``assignName``
753    procedure   ``update``
758    function    ``toHtml``
767    structure   ``selection``
770    array       ``labelList``
780    function    ``selection``
789    procedure   ``assignName``
795    procedure   ``update``
806    function    ``toHtml``
827    structure   ``dialogSequenceBase``
828    array       ``elementList``
837    function    ``&``
848    function    ``&``
857    procedure   ``&:=``
862    procedure   ``assignName``
871    procedure   ``update``
881    structure   ``dialogSequence``
890    function    ``dialogSequence``
904    function    ``dialogSequence``
914    function    ``toHtml``
927    structure   ``dialogColumn``
936    function    ``dialogColumn``
946    function    ``toHtml``
965    structure   ``dialogRow``
974    function    ``dialogRow``
984    function    ``toHtml``
1005   structure   ``dialogTable``
1007   array       ``elementTable``
1018   function    ``dialogTable``
1045   function    ``dialogTable``
1067   procedure   ``assignName``
1079   procedure   ``update``
1091   function    ``toHtml``
====== =========== ==================================================

lib/char.s7i
------------

:Lines:    356
:Entities: 30

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
41     procedure   ``:=``
67     function    ``=``
75     function    ``<>``
83     function    ``<``
91     function    ``>``
99     function    ``<=``
107    function    ``>=``
116    function    ``compare``
123    function    ``hashCode``
133    function    ``ord``
143    function    ``integer``
154    function    ``chr``
165    function    ``char``
176    function    ``conv``
179    function    ``varConv``
186    function    ``succ``
193    function    ``pred``
200    function    ``str``
213    function    ``upper``
226    function    ``lower``
249    function    ``isLetter``
263    function    ``width``
271    procedure   ``incr``
279    procedure   ``decr``
282    function    ``c_literal``
292    function    ``rand``
313    function    ``parse``
326    function    ``char``
342    function    ``trimValue``
352    function    ``literal``
====== =========== ==================================================

lib/charsets.s7i
----------------

:Lines:    2024
:Entities: 2

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
1917   procedure   ``conv2unicode``
1940   procedure   ``conv2unicodeByName``
====== =========== ==================================================

lib/chartype.s7i
----------------

:Lines:    121
:Entities: 37

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
29     set         ``letter_char``
30     set         ``digit_char``
31     set         ``hexdigit_char``
32     set         ``octdigit_char``
33     set         ``alphanum_char``
34     set         ``control_char``
35     set         ``ascii_control_char``
36     set         ``special_char``
40     set         ``extended_special_char``
41     set         ``left_angle_bracket``
42     set         ``right_angle_bracket``
43     set         ``special_html_char``
44     set         ``single_quotation_char``
45     set         ``double_quotation_char``
46     set         ``sharp_char``
47     set         ``hyphen_char``
48     set         ``slash_char``
49     set         ``special_sql_char``
50     set         ``paren_char``
51     set         ``left_paren_char``
52     set         ``other_paren_char``
53     set         ``name_start_char``
54     set         ``name_char``
55     set         ``space_or_tab``
56     set         ``line_end_char``
57     set         ``white_space_char``
58     set         ``white_space_or_end_tag``
59     set         ``white_space_or_printable``
60     set         ``equals_or_end_tag``
61     set         ``special_comment_char``
62     set         ``html_name_start_char``
63     set         ``html_name_char``
64     set         ``http_separators``
67     set         ``http_token_char``
69     set         ``no_escape_char``
83     array       ``esc_tab``
108    function    ``pos``
====== =========== ==================================================

lib/cipher.s7i
--------------

:Lines:    146
:Entities: 12

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
34     enumeration ``cipherAlgorithm``
53     function    ``blockSize``
61     function    ``setCipherKey``
69     procedure   ``initAead``
79     function    ``getComputedMac``
88     function    ``getMac``
95     function    ``encode``
102    function    ``decode``
112    structure   ``noCipherState``
125    function    ``setCipherKey``
140    function    ``encode``
144    function    ``decode``
====== =========== ==================================================

lib/cli_cmds.s7i
----------------

:Lines:    1360
:Entities: 45

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
37     set         ``parameter_char``
38     set         ``dos_parameter_char``
50     procedure   ``doRemoveCmd``
92     array       ``fileList``
92     procedure   ``doCopyCmd``
181    array       ``fileList``
181    procedure   ``doMoveCmd``
254    procedure   ``doMkdirCmd``
287    function    ``getCommandParameter``
394    function    ``getUnixCommandParameter``
467    function    ``getDosCommandParameter``
562    function    ``getDosEchoParameter``
608    function    ``doOneCommand``
613    function    ``execCommand``
623    function    ``execBacktickCommands``
647    procedure   ``addToFileList``
670    procedure   ``doRm``
677    array       ``fileList``
710    procedure   ``doDel``
714    array       ``fileList``
739    procedure   ``doCp``
747    array       ``fileList``
782    procedure   ``doCopy``
786    array       ``fileList``
811    procedure   ``doXCopy``
817    array       ``fileList``
846    procedure   ``doMv``
852    array       ``fileList``
884    procedure   ``doMove``
888    array       ``fileList``
913    procedure   ``doMkdir``
919    array       ``fileList``
950    procedure   ``doMd``
953    array       ``fileList``
976    function    ``doPwd``
997    function    ``doEcho``
1032   procedure   ``doCd``
1052   procedure   ``appendToFile``
1066   function    ``oneExternalCommand``
1072   array       ``parameterArray``
1160   function    ``oneExternalCommand``
1166   array       ``parameterArray``
1216   procedure   ``doMake``
1219   function    ``doOneCommand``
1312   function    ``processCommand``
====== =========== ==================================================

lib/clib_file.s7i
-----------------

:Lines:    301
:Entities: 27

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
44     procedure   ``:=``
52     function    ``=``
60     function    ``<>``
63     function    ``_GENERATE_EMPTY_CLIB_FILE``
76     function    ``CLIB_INPUT``
77     function    ``CLIB_OUTPUT``
78     function    ``CLIB_ERROR``
113    function    ``openClibFile``
116    function    ``openNullDeviceClibFile``
117    procedure   ``pipe``
124    procedure   ``close``
133    function    ``eof``
141    function    ``hasNext``
151    function    ``inputReady``
158    procedure   ``flush``
165    function    ``getc``
175    function    ``gets``
195    function    ``terminated_read``
212    function    ``word_read``
226    function    ``line_read``
236    procedure   ``write``
248    function    ``length``
263    procedure   ``truncate``
272    function    ``seekable``
284    procedure   ``seek``
297    function    ``tell``
300    function    ``literal``
====== =========== ==================================================

lib/color.s7i
-------------

:Lines:    185
:Entities: 8

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
33     structure   ``color``
44     function    ``=``
54     function    ``<>``
64     function    ``+``
79     function    ``color``
94     function    ``gray``
110    function    ``compare``
134    function    ``hashCode``
====== =========== ==================================================

lib/complex.s7i
---------------

:Lines:    464
:Entities: 31

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
39     structure   ``complex``
49     function    ``complex``
62     function    ``complex``
75     function    ``polar``
88     function    ``complex``
101    function    ``conv``
114    function    ``conv``
127    function    ``=``
135    function    ``<>``
143    function    ``+``
151    function    ``-``
164    function    ``conj``
177    function    ``+``
190    function    ``-``
203    function    ``*``
216    function    ``/``
231    procedure   ``+:=``
241    procedure   ``-:=``
251    procedure   ``*:=``
264    procedure   ``/:=``
280    function    ``abs``
296    function    ``sqrAbs``
305    function    ``arg``
313    function    ``**``
333    function    ``compare``
348    function    ``hashCode``
360    function    ``str``
380    function    ``complex``
408    function    ``parse``
424    function    ``digits``
449    function    ``sci``
====== =========== ==================================================

lib/compress.s7i
----------------

:Lines:    150
:Entities: 4

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
34     function    ``toPackBits``
90     function    ``fromPackBits``
120    function    ``toPackBitsPdf``
129    function    ``fromPackBitsPdf``
====== =========== ==================================================

lib/console.s7i
---------------

:Lines:    188
:Entities: 18

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
40     structure   ``console_file``
60     procedure   ``CON_OPEN``
68     function    ``open``
81     procedure   ``flush``
88     function    ``height``
95     function    ``width``
101    procedure   ``setPos``
109    function    ``column``
116    function    ``line``
119    procedure   ``cursor``
120    procedure   ``clearArea``
129    procedure   ``clear``
140    procedure   ``clear``
146    procedure   ``v_scroll``
149    procedure   ``h_scroll``
167    procedure   ``write``
170    procedure   ``moveLeft``
176    procedure   ``erase``
====== =========== ==================================================

lib/cpio.s7i
------------

:Lines:    1708
:Entities: 46

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
51     structure   ``cpioHeader``
73     function    ``computeCheck``
85     procedure   ``showHeader``
108    procedure   ``readHead``
235    procedure   ``readMinimumOfHead``
321    function    ``str``
393    procedure   ``writeHead``
399    procedure   ``writeTrailer``
431    structure   ``cpioArchive``
449    function    ``openCpio``
498    function    ``openCpio``
512    procedure   ``close``
518    function    ``addToCatalog``
532    function    ``addImplicitDir``
544    function    ``followSymlink``
590    function    ``followSymlink``
604    procedure   ``fixRegisterAndCatalog``
634    function    ``readDir``
645    function    ``readDir``
660    function    ``fileType``
721    function    ``fileTypeSL``
767    function    ``getFileMode``
787    procedure   ``setFileMode``
819    function    ``fileSize``
840    function    ``getMTime``
863    procedure   ``setMTime``
896    function    ``getOwner``
918    procedure   ``setOwner``
954    function    ``getGroup``
976    procedure   ``setGroup``
1013   function    ``getFileMode``
1051   function    ``getMTime``
1090   procedure   ``setMTime``
1132   function    ``getOwner``
1171   procedure   ``setOwner``
1216   function    ``getGroup``
1255   procedure   ``setGroup``
1298   function    ``readLink``
1340   procedure   ``makeLink``
1415   function    ``getFile``
1443   procedure   ``putFile``
1545   procedure   ``makeDir``
1622   procedure   ``removeFile``
1669   procedure   ``for``
1679   function    ``openFileInCpio``
1705   function    ``open``
====== =========== ==================================================

lib/crc32.s7i
-------------

:Lines:    193
:Entities: 6

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
35     function    ``createCrc32Table``
63     function    ``crc32``
79     function    ``crc32``
93     array       ``bzip2Crc32Table``
163    function    ``bzip2Crc32``
180    function    ``bzip2Crc32``
====== =========== ==================================================

lib/cronos16.s7i
----------------

:Lines:    1173
:Entities: 1

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
32     function    ``genCronos16``
====== =========== ==================================================

lib/cronos27.s7i
----------------

:Lines:    1464
:Entities: 1

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
32     function    ``genCronos27``
====== =========== ==================================================

lib/csv.s7i
-----------

:Lines:    201
:Entities: 5

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
40     function    ``toCsvLine``
81     function    ``fromCsvLine``
83     array       ``data``
153    function    ``readCsvLine``
155    array       ``data``
====== =========== ==================================================

lib/db_prop.s7i
---------------

:Lines:    991
:Entities: 47

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
34     set         ``forbidden``
48     function    ``allowedInFieldName``
52     function    ``ddlStatementsNeedCommit``
68     function    ``transactionLocks``
89     function    ``int8Type``
110    function    ``maxInt8Value``
130    function    ``int64Type``
146    function    ``withRangeCheck``
162    function    ``bigIntType``
178    function    ``sqlIntLiteralBits``
194    function    ``bigIntTypeBits``
214    function    ``floatType``
230    function    ``compareFloatAsDecimalString``
250    function    ``doubleType``
266    function    ``compareDoubleAsDecimalString``
282    function    ``bigRatType``
302    function    ``decimalIntType``
322    function    ``maxDecimalPrecision``
338    function    ``maxDecimalLiteralPrecision``
358    function    ``numericIntType``
378    function    ``maxNumericPrecision``
394    function    ``maxNumericLiteralPrecision``
414    function    ``decimalType``
434    function    ``minDecimalScale``
456    function    ``maxDecimalScale``
476    function    ``numericType``
496    function    ``minNumericScale``
518    function    ``maxNumericScale``
534    function    ``maxChar1FieldCharacter``
550    function    ``charFieldPreservesTrailingSpaces``
566    function    ``nullAllowedInStringLiteral``
582    function    ``nullAllowedInString``
602    function    ``varcharType``
622    function    ``blobType``
642    function    ``clobType``
658    function    ``minDate``
674    function    ``toDate``
725    function    ``timeType``
741    function    ``toTime``
765    function    ``dateTimeType``
781    function    ``toDateTime``
840    function    ``timeStampType``
856    function    ``minTimeStamp``
872    function    ``toTimeStamp``
944    function    ``timeStampTzType``
960    function    ``durationType``
976    function    ``supportsFloatingDecimals``
====== =========== ==================================================

lib/deflate.s7i
---------------

:Lines:    740
:Entities: 30

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
36     procedure   ``putLiteralOrLength``
51     procedure   ``putLength``
79     procedure   ``putDistance``
133    structure   ``deflateData``
137    array       ``literalOrLengthSymbolCount``
138    array       ``distanceSymbolCount``
142    procedure   ``deflateFixed``
214    function    ``encodeLz77Length``
258    procedure   ``addLz77Length``
273    function    ``encodeLz77Distance``
356    procedure   ``addLz77Distance``
371    function    ``compressWithLz77``
449    procedure   ``huffmanEncodeLz77``
474    procedure   ``checkMaximumCodeLength``
489    function    ``processCombinedData``
554    procedure   ``huffmanEncodeCombinedData``
582    function    ``mapCombinedDataCodeLengths``
584    array       ``mappedCodeLengths``
586    array       ``mapFromOrderedLengths``
601    procedure   ``deflateDynamic``
605    array       ``literalOrLengthCodeLengths``
607    array       ``distanceCodeLengths``
609    array       ``combinedData``
611    array       ``combinedDataSymbolCount``
612    array       ``combinedDataCodeLengths``
614    array       ``mappedCodeLengths``
644    procedure   ``uncompressedBlock``
669    procedure   ``deflateBlock``
707    procedure   ``deflate``
730    function    ``deflate``
====== =========== ==================================================

lib/des.s7i
-----------

:Lines:    444
:Entities: 18

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
44     structure   ``desState``
60     function    ``setDesKey``
64     array       ``pc1``
69     array       ``pc2``
74     array       ``bytebit``
77     array       ``totrot``
79     array       ``pc1m``
80     array       ``pcr``
81     array       ``ks``
126    function    ``reverseKeyScheduleOrder``
145    function    ``setDesKey``
161    function    ``setCipherKey``
166    array       ``spBox``
297    procedure   ``initialPermutation``
325    procedure   ``finalPermutation``
353    function    ``processDesBlock``
399    function    ``encode``
425    function    ``decode``
====== =========== ==================================================

lib/dialog.s7i
--------------

:Lines:    311
:Entities: 11

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
34     function    ``clickedWindow``
38     function    ``buttonWindow``
41     function    ``clickedWindow``
45     function    ``buttonWindow``
48     procedure   ``writeButton``
60     procedure   ``writeButton``
71     function    ``isOkay``
181    function    ``isOkay``
189    procedure   ``messageWindow``
270    procedure   ``messageWindow``
276    procedure   ``bossMode``
====== =========== ==================================================

lib/dir.s7i
-----------

:Lines:    163
:Entities: 9

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
36     structure   ``dirFile``
38     array       ``dirArray``
54     function    ``openDir``
73     function    ``openDirPath``
102    function    ``getln``
124    function    ``getwd``
140    function    ``gets``
152    function    ``eof``
161    function    ``hasNext``
====== =========== ==================================================

lib/draw.s7i
------------

:Lines:    854
:Entities: 60

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
44     function    ``colorPixel``
51     function    ``pixelToColor``
62     procedure   ``screen``
80     procedure   ``point``
91     procedure   ``point``
102    procedure   ``line``
115    procedure   ``line``
128    procedure   ``lineTo``
142    procedure   ``lineTo``
154    procedure   ``lineToAngle``
169    procedure   ``lineToAngle``
178    procedure   ``hline``
186    procedure   ``hline``
193    procedure   ``vline``
201    procedure   ``vline``
213    procedure   ``rect``
226    procedure   ``rect``
233    procedure   ``rect``
255    procedure   ``rectTo``
280    procedure   ``rectTo``
304    procedure   ``box``
323    procedure   ``box``
341    procedure   ``boxTo``
360    procedure   ``boxTo``
377    procedure   ``circle``
388    procedure   ``circle``
398    procedure   ``fcircle``
409    procedure   ``fcircle``
420    procedure   ``arc``
433    procedure   ``arc``
445    procedure   ``arc``
459    procedure   ``arc``
467    procedure   ``chord``
479    procedure   ``pieslice``
486    procedure   ``fellipse``
494    procedure   ``fellipse``
504    procedure   ``clear``
513    procedure   ``clear``
522    procedure   ``clear``
532    procedure   ``polyLine``
543    procedure   ``polyLine``
554    procedure   ``fpolyLine``
566    procedure   ``fpolyLine``
572    procedure   ``paint``
586    procedure   ``put``
597    procedure   ``put``
614    procedure   ``put``
624    function    ``newPixmap``
636    function    ``getPixmap``
648    function    ``getPixmap``
658    function    ``getPixmap``
670    function    ``getPixmap``
728    function    ``getPixmap``
779    function    ``getPixmap``
804    procedure   ``setTransparentColor``
814    function    ``getPixelColor``
826    function    ``getPixelColor``
838    procedure   ``palette``
844    procedure   ``palette``
850    procedure   ``sound``
====== =========== ==================================================

lib/duration.s7i
----------------

:Lines:    1038
:Entities: 50

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
38     structure   ``duration``
57     function    ``getYears``
66     function    ``getMonths``
77     function    ``getDays``
85     function    ``getHours``
93     function    ``getMinutes``
101    function    ``getSeconds``
109    function    ``getMicroSeconds``
120    function    ``str``
174    function    ``literal``
184    function    ``duration``
312    function    ``parse``
323    function    ``=``
338    function    ``<>``
354    function    ``<=``
395    function    ``>=``
436    function    ``<``
445    function    ``>``
454    function    ``compare``
496    function    ``hashCode``
508    function    ``toYears``
517    function    ``toMonths``
529    function    ``toDays``
543    function    ``toHours``
551    function    ``toMinutes``
561    function    ``toSeconds``
572    function    ``toMicroSeconds``
580    procedure   ``NORMALIZE_DUR_TIME``
593    procedure   ``NORMALIZE``
603    function    ``.``
612    function    ``.``
621    function    ``.``
630    function    ``.``
639    function    ``.``
648    function    ``.``
657    function    ``.``
670    function    ``+``
692    function    ``-``
714    function    ``+``
737    function    ``-``
760    function    ``*``
783    function    ``*``
805    procedure   ``+:=``
825    procedure   ``-:=``
845    procedure   ``+:=``
861    procedure   ``-:=``
878    function    ``+``
899    function    ``-``
920    function    ``-``
1031   procedure   ``wait``
====== =========== ==================================================

lib/echo.s7i
------------

:Lines:    132
:Entities: 4

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
32     structure   ``echoFile``
45     function    ``openEcho``
67     function    ``getc``
113    function    ``gets``
====== =========== ==================================================

lib/editline.s7i
----------------

:Lines:    398
:Entities: 12

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
37     structure   ``editLineFile``
43     array       ``history``
61     function    ``openEditLine``
88     function    ``openEditLineLatin1``
115    function    ``getln``
122    array       ``history``
257    function    ``getc``
281    function    ``gets``
314    function    ``eof``
324    function    ``hasNext``
328    function    ``readPassword``
331    function    ``readPassword``
====== =========== ==================================================

lib/elf.s7i
-----------

:Lines:    1560
:Entities: 86

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
258    structure   ``elfHeader``
281    procedure   ``show``
305    procedure   ``readHeader32Le``
323    procedure   ``readHeader32Be``
341    procedure   ``readHeader64Le``
359    procedure   ``readHeader64Be``
377    procedure   ``readHeader``
433    structure   ``elfProgramHeader``
447    procedure   ``show``
462    procedure   ``readProgramHeader32Le``
476    procedure   ``readProgramHeader32Be``
490    procedure   ``readProgramHeader64Le``
504    procedure   ``readProgramHeader64Be``
518    procedure   ``readProgramHeader``
554    structure   ``elfSectionHeader``
571    function    ``sectionTypeName``
609    procedure   ``show``
627    procedure   ``readSectionHeader32Le``
643    procedure   ``readSectionHeader32Be``
659    procedure   ``readSectionHeader64Le``
675    procedure   ``readSectionHeader64Be``
691    procedure   ``readSectionHeader``
729    structure   ``elfData``
732    array       ``programHeaders``
733    array       ``sectionHeaders``
742    function    ``openElf``
790    function    ``readSectionNames``
799    function    ``getSection``
812    function    ``getSectionData``
826    structure   ``elfGnuHashHeader``
831    array       ``bloom_filter``
832    array       ``buckets``
833    array       ``values``
837    procedure   ``show``
846    procedure   ``readGnuHashHeaderLe``
862    procedure   ``readGnuHashHeaderBe``
878    function    ``readGnuHashHeader``
896    structure   ``elfNoteHeader``
905    procedure   ``show``
915    procedure   ``readNoteHeaderLe``
923    procedure   ``readNoteHeaderBe``
931    function    ``readNoteHeader``
970    function    ``getNote``
997    function    ``getBuildId``
1004   function    ``getNotes``
1028   structure   ``elfSym``
1038   procedure   ``show``
1049   procedure   ``readSym32Le``
1060   procedure   ``readSym32Be``
1071   procedure   ``readSym64Le``
1082   procedure   ``readSym64Be``
1093   function    ``readSym``
1135   function    ``getDynsymNames``
1138   array       ``dynsymNames``
1167   function    ``getDynsymNames``
1176   function    ``getDynsymNames``
1180   structure   ``elfDyn``
1186   procedure   ``show``
1193   procedure   ``readDyn32Le``
1200   procedure   ``readDyn32Be``
1207   procedure   ``readDyn64Le``
1214   procedure   ``readDyn64Be``
1221   function    ``readDyn``
1263   function    ``getDynamicNeeds``
1266   array       ``dynNames``
1298   function    ``getDynamicNeeds``
1307   function    ``getDynamicNeeds``
1311   structure   ``elfVerDAux``
1317   procedure   ``show``
1324   procedure   ``readVerDAuxLe``
1331   procedure   ``readVerDAuxBe``
1338   function    ``readVerDAux``
1366   structure   ``elfVerNAux``
1376   procedure   ``show``
1387   procedure   ``readVerNAuxLe``
1397   procedure   ``readVerNAuxBe``
1407   function    ``readVerNAux``
1425   structure   ``elfVerNeed``
1435   procedure   ``show``
1446   procedure   ``readVerNeedLe``
1456   procedure   ``readVerNeedBe``
1466   function    ``readVerNeed``
1492   function    ``getVerNeeds``
1495   array       ``verNeedNames``
1549   function    ``getVerNeeds``
1558   function    ``getVerNeeds``
====== =========== ==================================================

lib/elliptic.s7i
----------------

:Lines:    649
:Entities: 33

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
37     structure   ``ecPoint``
47     function    ``ecPoint``
56     function    ``literal``
64     function    ``getNeutralEcPoint``
83     function    ``=``
93     function    ``<>``
103    structure   ``ellipticCurve``
125    function    ``ellipticCurve``
231    function    ``getSizeInBytes``
238    function    ``element``
247    function    ``double``
271    function    ``add``
301    function    ``mult``
321    structure   ``jacobianPoint``
334    function    ``toJacobian``
353    function    ``fromJacobian``
370    function    ``double``
397    function    ``add``
442    function    ``mult``
462    function    ``multFast``
470    function    ``multAddFast``
479    function    ``ecPointCompress``
486    function    ``ecPointEncode``
494    function    ``ecPointDecode``
522    structure   ``eccKeyPair``
529    function    ``eccKeyPair``
550    function    ``genPrivateKey``
557    function    ``genEccKeyPair``
570    function    ``verifyKeyPair``
577    function    ``truncate``
592    structure   ``ecdsaSignatureType``
602    function    ``sign``
628    function    ``verify``
====== =========== ==================================================

lib/enable_io.s7i
-----------------

:Lines:    312
:Entities: 19

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
53     procedure   ``enable_input``
66     procedure   ``read``
82     procedure   ``read``
104    procedure   ``readln``
119    procedure   ``readln``
142    procedure   ``read``
158    procedure   ``read``
172    procedure   ``readln``
187    procedure   ``readln``
212    procedure   ``enable_output``
218    procedure   ``write``
226    procedure   ``writeln``
234    procedure   ``write``
242    procedure   ``writeln``
252    function    ``lpad``
260    function    ``rpad``
268    function    ``<&``
276    function    ``<&``
298    procedure   ``enable_io``
====== =========== ==================================================

lib/encoding.s7i
----------------

:Lines:    931
:Entities: 28

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
44     function    ``toBase64``
102    function    ``fromBase64``
106    array       ``decode``
168    function    ``toBase64Url``
220    function    ``fromBase64Url``
224    array       ``decode``
289    function    ``toQuotedPrintable``
344    function    ``fromQuotedPrintable``
389    function    ``toUuencoded``
445    function    ``fromUuencoded``
500    function    ``toPercentEncoded``
504    set         ``unreservedChars``
530    function    ``fromPercentEncoded``
564    function    ``toUrlEncoded``
568    set         ``unreservedChars``
599    function    ``fromUrlEncoded``
639    function    ``toAscii85``
692    function    ``fromAscii85``
696    set         ``whiteSpace``
735    function    ``toAsciiHex``
763    function    ``fromAsciiHex``
768    set         ``whiteSpace``
798    function    ``toBase``
824    function    ``fromBaseToBigInt``
856    function    ``toBase``
885    function    ``fromBase``
916    function    ``toBase58``
929    function    ``fromBase58``
====== =========== ==================================================

lib/enumeration.s7i
-------------------

:Lines:    236
:Entities: 24

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
29     function    ``expr_to_list``
45     function    ``new``
62     procedure   ``:=``
69     function    ``literal``
71     function    ``getValue``
73     function    ``ICONV2``
76     function    ``ORD2``
113    function    ``=``
120    function    ``<>``
128    function    ``conv``
136    function    ``ord``
144    function    ``integer``
151    function    ``hashCode``
160    function    ``compare``
168    function    ``conv``
177    function    ``succ``
186    function    ``pred``
193    procedure   ``incr``
202    procedure   ``decr``
214    function    ``rand``
217    function    ``<``
220    function    ``<=``
223    function    ``>``
226    function    ``>=``
====== =========== ==================================================

lib/environment.s7i
-------------------

:Lines:    175
:Entities: 12

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
34     function    ``argv``
46     function    ``name``
56     function    ``path``
65     function    ``dir``
86     function    ``file``
100    function    ``sourceLine``
110    procedure   ``exit``
120    procedure   ``exit``
139    function    ``getenv``
154    procedure   ``setenv``
166    procedure   ``unsetenv``
174    function    ``environment``
====== =========== ==================================================

lib/exif.s7i
------------

:Lines:    152
:Entities: 3

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
73     structure   ``exifDataType``
81     procedure   ``readExifData``
133    procedure   ``changeOrientation``
====== =========== ==================================================

lib/external_file.s7i
---------------------

:Lines:    340
:Entities: 22

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
41     structure   ``external_file``
82     function    ``open``
98     function    ``openNullDevice``
117    procedure   ``close``
128    procedure   ``flush``
140    procedure   ``write``
146    procedure   ``moveLeft``
152    procedure   ``erase``
162    function    ``getc``
173    function    ``gets``
192    function    ``getTerminatedString``
209    function    ``getwd``
224    function    ``getln``
234    function    ``eof``
243    function    ``hasNext``
254    function    ``inputReady``
267    function    ``length``
283    procedure   ``truncate``
295    function    ``seekable``
308    procedure   ``seek``
324    function    ``tell``
328    function    ``fileOpenSucceeds``
====== =========== ==================================================

lib/field.s7i
-------------

:Lines:    268
:Entities: 7

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
36     structure   ``fieldFile``
61     function    ``openField``
88     function    ``getwd``
162    function    ``getln``
229    function    ``getc``
241    function    ``gets``
257    procedure   ``write``
====== =========== ==================================================

lib/file.s7i
------------

:Lines:    372
:Entities: 35

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
45     procedure   ``write``
54     procedure   ``writeln``
66     procedure   ``writeln``
69     procedure   ``moveLeft``
70     procedure   ``erase``
73     procedure   ``backSpace``
81     procedure   ``cursorOn``
82     procedure   ``cursorOff``
88     procedure   ``close``
96     procedure   ``flush``
103    function    ``getc``
111    function    ``gets``
127    function    ``getTerminatedString``
140    function    ``getwd``
151    function    ``getln``
154    function    ``getk``
155    function    ``eoln``
164    function    ``eof``
173    function    ``hasNext``
183    function    ``inputReady``
196    function    ``length``
211    procedure   ``truncate``
220    function    ``seekable``
233    procedure   ``seek``
247    function    ``tell``
263    function    ``compare``
270    function    ``hashCode``
273    function    ``.``
274    function    ``.``
285    procedure   ``read``
300    procedure   ``read``
317    procedure   ``readln``
331    procedure   ``readln``
347    procedure   ``readln``
361    procedure   ``skip``
====== =========== ==================================================

lib/filebits.s7i
----------------

:Lines:    46
:Entities: 0

(none)


lib/filesys.s7i
---------------

:Lines:    601
:Entities: 51

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
60     enumeration ``filePermission``
80     function    ``integer``
83     function    ``conv``
86     function    ``fileMode``
89     function    ``conv``
92     function    ``str``
119    function    ``removeDotFiles``
177    function    ``symlinkDestination``
214    procedure   ``close``
225    function    ``readDir``
235    function    ``readDir``
243    function    ``readDir``
251    function    ``readDir``
262    function    ``fileType``
273    function    ``fileTypeSL``
282    function    ``fileSize``
291    function    ``bigFileSize``
299    function    ``getFileMode``
303    function    ``fileMode``
312    procedure   ``setFileMode``
321    function    ``getMTime``
328    procedure   ``setMTime``
337    function    ``getOwner``
344    procedure   ``setOwner``
353    function    ``getGroup``
360    procedure   ``setGroup``
370    function    ``getFileMode``
379    function    ``getMTime``
387    procedure   ``setMTime``
397    function    ``getOwner``
405    procedure   ``setOwner``
415    function    ``getGroup``
423    procedure   ``setGroup``
430    function    ``open``
438    function    ``getFile``
446    procedure   ``putFile``
454    function    ``readLink``
466    procedure   ``makeLink``
477    procedure   ``removeFile``
484    procedure   ``removeTree``
491    procedure   ``moveFile``
501    procedure   ``makeDir``
505    procedure   ``mkdir``
515    procedure   ``rmdir``
523    function    ``getcwd``
530    procedure   ``chdir``
536    structure   ``emptyFileSys``
559    function    ``readDir``
562    array       ``filePaths``
564    array       ``pathsInDir``
599    function    ``bigFileSize``
====== =========== ==================================================

lib/fileutil.s7i
----------------

:Lines:    144
:Entities: 4

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
34     procedure   ``copyFile``
52     function    ``copyFile``
73     procedure   ``insertArea``
114    procedure   ``deleteArea``
====== =========== ==================================================

lib/fixarray.s7i
----------------

:Lines:    307
:Entities: 15

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
43     function    ``array``
53     procedure   ``:=``
55     function    ``SET_MIN_IDX``
57     function    ``TIMES``
69     function    ``times``
128    procedure   ``for``
143    procedure   ``for``
157    procedure   ``for``
172    procedure   ``for``
193    procedure   ``for``
219    procedure   ``for``
230    procedure   ``for``
248    procedure   ``for``
268    procedure   ``for``
305    function    ``array``
====== =========== ==================================================

lib/float.s7i
-------------

:Lines:    757
:Entities: 50

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
53     procedure   ``:=``
60     function    ``+``
67     function    ``-``
74     function    ``+``
81     function    ``-``
88     function    ``*``
98     function    ``/``
115    function    ``rem``
134    function    ``mod``
149    function    ``**``
167    function    ``**``
180    function    ``<<``
193    function    ``>>``
199    procedure   ``+:=``
205    procedure   ``-:=``
211    procedure   ``*:=``
217    procedure   ``/:=``
227    function    ``=``
237    function    ``<>``
249    function    ``<``
261    function    ``>``
273    function    ``<=``
285    function    ``>=``
320    function    ``compare``
327    function    ``hashCode``
347    function    ``str``
367    function    ``string``
392    function    ``str``
408    function    ``literal``
438    function    ``float``
457    function    ``parse``
484    function    ``digits``
512    function    ``sci``
534    function    ``exp``
559    function    ``flt``
566    function    ``float``
573    function    ``conv``
580    function    ``abs``
589    function    ``floor``
598    function    ``ceil``
615    function    ``round``
634    function    ``round10``
679    function    ``trunc``
688    function    ``isNaN``
701    function    ``isNegativeZero``
709    function    ``isPositiveZero``
719    function    ``rand``
722    procedure   ``decompose``
726    structure   ``floatElements``
742    function    ``decompose``
====== =========== ==================================================

lib/font.s7i
------------

:Lines:    196
:Entities: 22

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
42     function    ``xHeight``
50     function    ``capHeight``
59     function    ``lineHeight``
68     function    ``ascent``
77     function    ``descent``
80     function    ``baseLineDelta``
89     function    ``columnWidth``
97     function    ``characterSpacing``
104    function    ``width``
116    function    ``numOfCharsInWidth``
133    function    ``compare``
140    function    ``hashCode``
146    structure   ``emptyFont``
158    structure   ``fontProperties``
173    function    ``xHeight``
176    function    ``capHeight``
179    function    ``lineHeight``
182    function    ``ascent``
185    function    ``descent``
188    function    ``baseLineDelta``
191    function    ``columnWidth``
194    function    ``characterSpacing``
====== =========== ==================================================

lib/font8x8.s7i
---------------

:Lines:    998
:Entities: 1

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
32     function    ``genFont8x8``
====== =========== ==================================================

lib/forloop.s7i
---------------

:Lines:    449
:Entities: 27

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
38     procedure   ``for``
52     procedure   ``for``
66     procedure   ``for``
81     procedure   ``for``
94     procedure   ``FOR_STEP_DECLS``
98     procedure   ``for``
112    procedure   ``for``
136    procedure   ``FOR_UNTIL_DECLS``
138    procedure   ``for``
158    procedure   ``for``
178    procedure   ``for``
198    procedure   ``for``
228    procedure   ``FOR_DECLS``
231    procedure   ``for``
248    procedure   ``for``
275    procedure   ``FOR_ENUM_DECLS``
278    procedure   ``for``
295    procedure   ``for``
310    procedure   ``for``
318    procedure   ``for``
326    procedure   ``for``
336    procedure   ``for``
357    procedure   ``for``
383    procedure   ``for``
395    procedure   ``for``
412    procedure   ``for``
431    procedure   ``for``
====== =========== ==================================================

lib/ftp.s7i
-----------

:Lines:    969
:Entities: 50

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
54     function    ``listDir``
55     function    ``getActiveMode``
56     procedure   ``setActiveMode``
57     function    ``getAsciiTransfer``
58     procedure   ``setAsciiTransfer``
59     procedure   ``closeFtpData``
60     procedure   ``ftpResponse``
67     structure   ``ftpConnection``
83     procedure   ``ftpCommand``
90     procedure   ``ftpResponse``
111    function    ``allowUtf8``
141    function    ``openFtp``
189    function    ``openFtp``
216    function    ``openFtp``
268    function    ``openFtp``
275    procedure   ``close``
286    procedure   ``openActiveFtpData``
301    procedure   ``openPassiveFtpData``
305    array       ``addrAndPort``
326    procedure   ``openFtpData``
336    procedure   ``prepareFtpDataCommunication``
346    procedure   ``closeFtpData``
361    function    ``readDir``
363    array       ``fileNames``
423    function    ``listDir``
425    array       ``directoryListing``
463    procedure   ``chdir``
482    function    ``getcwd``
512    function    ``fileType``
561    function    ``fileTypeSL``
570    function    ``fileSize``
587    function    ``bigFileSize``
605    function    ``getMTime``
638    function    ``getActiveMode``
646    procedure   ``setActiveMode``
657    function    ``getAsciiTransfer``
665    procedure   ``setAsciiTransfer``
687    structure   ``ftpDataFile``
692    function    ``ftpDataFile``
703    procedure   ``close``
729    function    ``open``
781    function    ``getFile``
822    procedure   ``putFile``
851    procedure   ``removeFile``
865    procedure   ``moveFile``
885    procedure   ``makeDir``
899    procedure   ``rmdir``
909    procedure   ``splitFtpLocation``
934    function    ``getFtp``
958    function    ``getFtp``
====== =========== ==================================================

lib/ftpserv.s7i
---------------

:Lines:    631
:Entities: 24

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
39     structure   ``ftpServer``
48     structure   ``ftpServerConnection``
63     procedure   ``ftpResponse``
70     function    ``openActiveData``
74     array       ``addrAndPort``
87     function    ``openPassiveData``
116    function    ``getPathArgument``
132    function    ``toQuotedUtf8``
136    procedure   ``listOneFile``
143    array       ``monthName``
175    procedure   ``listFiles``
179    array       ``dirContent``
220    procedure   ``nameListFiles``
223    array       ``dirContent``
251    procedure   ``mlsdFileList``
253    array       ``dirContent``
292    procedure   ``retrieveFile``
310    procedure   ``storeFile``
328    procedure   ``openFtpSession``
341    procedure   ``closeFtpSession``
348    procedure   ``processFtpRequest``
581    procedure   ``processFtpRequest``
595    procedure   ``processFtpRequest``
613    procedure   ``runServer``
====== =========== ==================================================

lib/getf.s7i
------------

:Lines:    115
:Entities: 5

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
37     function    ``getf``
55     procedure   ``putf``
76     function    ``readf``
78     array       ``data``
100    procedure   ``writef``
====== =========== ==================================================

lib/gethttp.s7i
---------------

:Lines:    41
:Entities: 1

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
39     function    ``getHttp``
====== =========== ==================================================

lib/gethttps.s7i
----------------

:Lines:    41
:Entities: 1

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
39     function    ``getHttps``
====== =========== ==================================================

lib/gif.s7i
-----------

:Lines:    561
:Entities: 25

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
50     structure   ``gifHeader``
60     structure   ``gifImageHeader``
72     structure   ``gifGraphicControlExtension``
80     structure   ``gifApplicationExtension``
86     procedure   ``readGifColorMap``
106    procedure   ``showHeader``
117    procedure   ``readHeader``
139    procedure   ``showImageHeader``
152    procedure   ``readImageHeader``
176    function    ``readSubBlockSeries``
190    procedure   ``showExtension``
200    procedure   ``readGraphicControlExtension``
226    procedure   ``showExtension``
233    procedure   ``readApplicationExtension``
252    procedure   ``fillGifImageLine8Bit``
267    procedure   ``fillGifImage8Bit``
287    procedure   ``fillGifImageLine8Bit``
305    procedure   ``fillGifImage8Bit``
326    procedure   ``fillGifImage8BitInterlaced``
368    procedure   ``fillGifImage8BitInterlaced``
412    procedure   ``readImage``
462    structure   ``gifData``
473    procedure   ``getImage``
521    function    ``readGif``
549    function    ``readGif``
====== =========== ==================================================

lib/graph.s7i
-------------

:Lines:    415
:Entities: 71

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
41     procedure   ``:=``
42     function    ``=``
43     function    ``<>``
44     function    ``_GENERATE_EMPTY_WINDOW``
53     procedure   ``:=``
54     function    ``=``
55     function    ``<>``
56     function    ``conv``
57     function    ``pixel``
58     function    ``conv``
59     function    ``ord``
60     function    ``compare``
61     function    ``hashCode``
66     procedure   ``DRAW_PPOINT``
69     procedure   ``DRAW_PLINE``
73     procedure   ``DRAW_PRECT``
77     procedure   ``DRAW_CIRCLE``
80     procedure   ``DRAW_CLEAR``
82     procedure   ``FILL_CIRCLE``
85     procedure   ``DRAW_ARC``
89     procedure   ``DRAW_ARC``
93     procedure   ``FILL_ARCCHORD``
97     procedure   ``FILL_ARCPIESLICE``
101    procedure   ``FILL_ELLIPSE``
105    function    ``PRIMITIVE_GRAPHIC_OPEN``
116    procedure   ``flushGraphic``
136    function    ``openSubWindow``
146    procedure   ``setWindowName``
155    procedure   ``setCursorVisible``
170    function    ``capturePixmap``
176    function    ``getPixelData``
178    function    ``getPixelData``
180    function    ``getPixel``
183    function    ``getPixel``
201    procedure   ``copyArea``
207    procedure   ``SET_TRANSPARENTCOLOR``
209    function    ``hashCode``
210    function    ``compare``
212    procedure   ``setContent``
220    function    ``rgbPixel``
224    procedure   ``DRAW_PIXEL_TO_RGB``
227    procedure   ``DRAW_TEXT``
234    function    ``screenHeight``
239    function    ``screenWidth``
246    function    ``height``
253    function    ``width``
264    function    ``xPos``
275    function    ``yPos``
285    procedure   ``setPos``
291    procedure   ``setSize``
297    procedure   ``toBottom``
302    procedure   ``toTop``
310    function    ``getBorder``
319    function    ``pointerXPos``
328    function    ``pointerYPos``
337    procedure   ``setPointerPos``
356    procedure   ``selectInput``
366    procedure   ``:=``
368    function    ``_GENERATE_EMPTY_POINT_LIST``
371    function    ``bstring``
372    function    ``pointList``
374    function    ``=``
375    function    ``<>``
377    function    ``hashCode``
378    function    ``compare``
387    function    ``genPointList``
393    function    ``xyArray``
395    function    ``scale``
399    array       ``coordinates``
409    procedure   ``DRAW_POLYLINE``
412    procedure   ``FILL_POLYLINE``
====== =========== ==================================================

lib/graph_file.s7i
------------------

:Lines:    399
:Entities: 27

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
38     structure   ``graph_file``
64     function    ``open``
88     function    ``open``
113    function    ``open``
133    function    ``open``
155    procedure   ``close``
165    procedure   ``flush``
174    procedure   ``color``
183    procedure   ``color``
194    function    ``height``
202    function    ``width``
210    function    ``line``
218    function    ``column``
227    procedure   ``clear``
247    procedure   ``clear``
256    procedure   ``v_scroll``
286    procedure   ``h_scroll``
297    procedure   ``setPos``
311    procedure   ``setPosXY``
324    procedure   ``setLine``
334    procedure   ``setColumn``
344    procedure   ``write``
357    procedure   ``writeln``
367    procedure   ``moveLeft``
374    procedure   ``erase``
383    procedure   ``cursorOn``
390    procedure   ``cursorOff``
====== =========== ==================================================

lib/gtkserver.s7i
-----------------

:Lines:    161
:Entities: 6

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
48     procedure   ``set_gtk_logging``
54     procedure   ``gtk_log``
88     procedure   ``gtk_start``
104    procedure   ``gtk_stop``
129    function    ``gtk``
155    procedure   ``gtk_exec``
====== =========== ==================================================

lib/gzip.s7i
------------

:Lines:    573
:Entities: 23

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
41     structure   ``gzipHeader``
59     procedure   ``showHeader``
78     function    ``readGzipHeader``
130    function    ``readGzipHeader``
191    function    ``gzuncompress``
220    function    ``gzcompress``
242    function    ``gunzip``
273    structure   ``gzipFile``
293    function    ``openGzipFile``
318    function    ``getc``
341    function    ``gets``
372    function    ``eof``
381    function    ``hasNext``
399    function    ``length``
426    procedure   ``seek``
442    function    ``tell``
453    function    ``gzip``
477    structure   ``gzipWriteFile``
498    function    ``openGzipFile``
519    procedure   ``close``
536    procedure   ``write``
561    function    ``length``
571    function    ``tell``
====== =========== ==================================================

lib/hash.s7i
------------

:Lines:    421
:Entities: 37

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
37     function    ``hash``
64     procedure   ``CREATE``
67     procedure   ``DESTROY``
69     procedure   ``COPY``
72     procedure   ``FOR_DATA``
74     procedure   ``FOR_KEY``
76     procedure   ``FOR_DATA_KEY``
79     function    ``KEYS``
81     function    ``VALUES``
95     procedure   ``:=``
105    function    ``length``
107    function    ``INDEX``
110    function    ``INDEX``
114    function    ``INDEX2``
119    procedure   ``INCL``
123    procedure   ``EXCL``
126    function    ``UPDATE``
130    function    ``CONTAINS``
133    function    ``GEN_HASH``
142    function    ``.``
153    function    ``;``
164    function    ``default``
174    function    ``in``
183    function    ``not``
193    procedure   ``incl``
202    procedure   ``excl``
215    procedure   ``@:=``
233    function    ``update``
246    function    ``:``
248    function    ``,``
282    procedure   ``for``
298    procedure   ``for``
318    procedure   ``for``
337    function    ``keys``
352    function    ``values``
357    function    ``=``
375    function    ``<>``
====== =========== ==================================================

lib/hashsetof.s7i
-----------------

:Lines:    499
:Entities: 34

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
35     function    ``hashset``
59     procedure   ``CREATE``
62     procedure   ``DESTROY``
64     procedure   ``COPY``
67     procedure   ``FOR_KEY``
69     function    ``KEYS``
83     procedure   ``:=``
91     function    ``.``
104    function    ``card``
112    function    ``rand``
114    procedure   ``INCL``
118    procedure   ``EXCL``
121    function    ``CONTAINS``
133    function    ``in``
144    function    ``not``
152    procedure   ``incl``
162    procedure   ``excl``
174    procedure   ``@:=``
188    procedure   ``for``
199    function    ``toArray``
208    function    ``|``
226    function    ``&``
245    function    ``><``
269    function    ``-``
285    procedure   ``|:=``
298    procedure   ``&:=``
315    procedure   ``-:=``
329    function    ``=``
352    function    ``<>``
363    function    ``<``
391    function    ``>``
419    function    ``<=``
440    function    ``>=``
480    function    ``str``
====== =========== ==================================================

lib/hmac.s7i
------------

:Lines:    152
:Entities: 6

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
42     function    ``><``
65     function    ``hmac``
93     function    ``p_hash``
110    function    ``pseudoRandomFunction``
123    function    ``keyBlockFunction``
140    function    ``mgf1Sha1``
====== =========== ==================================================

lib/html.s7i
------------

:Lines:    83
:Entities: 4

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
35     set         ``voidHtmlElements``
40     function    ``getValueOfHtmlAttribute``
58     function    ``getHtmlLinks``
60     array       ``htmlLinks``
====== =========== ==================================================

lib/html_ent.s7i
----------------

:Lines:    476
:Entities: 9

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
29     structure   ``htmlEntityType``
34     function    ``genHtmlEntity``
42     array       ``htmlEntityList``
312    function    ``genHtmlEntityHash``
327    function    ``genHtmlCharNameHash``
346    function    ``decodeHtmlEntities``
399    function    ``encodeHtmlEntities``
425    function    ``encodeHtmlContent``
457    function    ``quoteHtmlAttrValue``
====== =========== ==================================================

lib/htmldom.s7i
---------------

:Lines:    286
:Entities: 14

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
37     structure   ``htmlDocument``
39     array       ``doctypeArguments``
47     function    ``getHtmlRoot``
54     function    ``getDoctypeName``
61     function    ``getDoctypeParameter``
65     structure   ``htmlDomParserState``
72     function    ``readHtmlNode``
76     function    ``readHtmlContainerSubNodes``
82     set         ``alternateEndTags``
141    function    ``readHtmlNode``
199    function    ``readHtml``
252    function    ``readHtml``
266    procedure   ``writeHtml``
282    procedure   ``writeHtml``
====== =========== ==================================================

lib/http_request.s7i
--------------------

:Lines:    696
:Entities: 35

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
51     structure   ``httpLocation``
58     array       ``cookies``
62     procedure   ``show``
73     structure   ``httpBody``
79     structure   ``httpCreds``
86     function    ``basicAuth``
96     function    ``bearerAuth``
107    structure   ``httpRequest``
115    function    ``httpGetRequest``
126    structure   ``httpResponse``
136    procedure   ``setProxy``
143    function    ``getHttpLocation``
202    function    ``toHttpAscii``
228    procedure   ``sendHttp``
279    function    ``openHttp``
292    function    ``openHttp``
303    function    ``getHttpStatusCode``
329    structure   ``httpHeader``
336    array       ``cookies``
340    function    ``getHttpHeader``
392    function    ``getHttpBody``
465    function    ``getHttp``
476    function    ``getHttpLocation``
515    function    ``sendHttp``
564    function    ``sendHttp``
575    function    ``http``
594    function    ``http``
609    function    ``http``
622    function    ``http``
629    function    ``http``
643    function    ``http``
650    function    ``http``
665    function    ``http``
672    function    ``http``
694    function    ``http``
====== =========== ==================================================

lib/http_srv_resp.s7i
---------------------

:Lines:    380
:Entities: 12

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
42     structure   ``httpResponseData``
58     function    ``httpResponseData``
70     procedure   ``sendHttpResponse``
139    procedure   ``failExpectation``
152    procedure   ``failExpectation``
163    procedure   ``sendHttpContinue``
173    procedure   ``sendClientError``
199    function    ``callCgi``
288    procedure   ``processGet``
293    array       ``cgiHeader``
354    procedure   ``processPost``
358    array       ``cgiHeader``
====== =========== ==================================================

lib/https_request.s7i
---------------------

:Lines:    211
:Entities: 14

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
33     function    ``openHttps``
48     function    ``openHttps``
59     function    ``sendHttps``
108    function    ``sendHttps``
112    function    ``https``
131    function    ``https``
135    function    ``https``
148    function    ``https``
152    function    ``https``
165    function    ``https``
169    function    ``https``
183    function    ``https``
187    function    ``https``
209    function    ``https``
====== =========== ==================================================

lib/httpserv.s7i
----------------

:Lines:    345
:Entities: 12

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
48     structure   ``httpServerRequest``
63     structure   ``httpServerConnection``
80     structure   ``httpServer``
97     function    ``expecting``
108    procedure   ``resetRequest``
118    function    ``getHttpRequest``
221    procedure   ``openHttpSession``
250    procedure   ``closeHttpSession``
258    function    ``getHttpRequest``
281    procedure   ``cleanSessions``
303    function    ``openHttpServer``
322    function    ``getHttpRequest``
====== =========== ==================================================

lib/huffman.s7i
---------------

:Lines:    644
:Entities: 29

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
44     function    ``computeSymbolsWithCodeLength``
51     array       ``numberOfCodesWithLength``
52     array       ``valueIndex``
90     structure   ``msbHuffmanDecoder``
109    function    ``createMsbHuffmanDecoder``
148    function    ``createHuffmanTableMsb``
153    function    ``createMsbHuffmanDecoder``
224    function    ``createMsbHuffmanDecoder``
244    function    ``getHuffmanSymbol``
261    structure   ``lsbHuffmanDecoder``
264    array       ``codeLengths``
268    function    ``createLsbHuffmanDecoder``
333    function    ``createLsbHuffmanDecoder``
346    function    ``createHuffmanTableLsb``
359    function    ``getHuffmanSymbol``
375    structure   ``huffmanEncoding``
396    procedure   ``putHuffmanSymbol``
409    procedure   ``putHuffmanSymbol``
415    structure   ``huffmanSymbolNode``
421    function    ``compare``
427    structure   ``huffmanTreeNode``
437    function    ``getHuffmanSymbolNodes``
455    procedure   ``getLengthsFromTree``
474    procedure   ``getLengthsFromTree``
494    function    ``getHuffmanCodeLengths``
496    array       ``codeLengths``
542    procedure   ``reduceMaximumHuffmanCodeLength``
604    function    ``createHuffmanEncoder``
634    function    ``createHuffmanEncoder``
====== =========== ==================================================

lib/ico.s7i
-----------

:Lines:    221
:Entities: 7

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
39     structure   ``icoDirEntry``
50     procedure   ``showDirEntry``
62     procedure   ``readDirEntry``
90     function    ``readIco``
129    function    ``readIco``
149    function    ``str``
211    procedure   ``writeIco``
====== =========== ==================================================

lib/idxarray.s7i
----------------

:Lines:    232
:Entities: 25

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
32     function    ``array``
51     procedure   ``:=``
58     procedure   ``&:=``
65     procedure   ``&:=``
71     function    ``&``
78     function    ``length``
85     function    ``minIntIdx``
92     function    ``maxIntIdx``
94     function    ``conv``
95     function    ``conv``
96     function    ``conv``
97     function    ``conv``
98     function    ``.``
102    function    ``;``
108    function    ``;``
130    function    ``..``
139    function    ``;``
154    function    ``len``
162    function    ``minIdx``
170    function    ``maxIdx``
176    procedure   ``for``
191    procedure   ``for``
203    procedure   ``for``
216    function    ``times``
230    function    ``array``
====== =========== ==================================================

lib/image.s7i
-------------

:Lines:    156
:Entities: 19

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
44     function    ``width``
51     function    ``height``
58     function    ``xPos``
65     function    ``yPos``
73     function    ``window``
79     procedure   ``setPos``
85     procedure   ``toTop``
91     procedure   ``toBottom``
98     function    ``str``
104    structure   ``baseImage``
116    function    ``width``
120    function    ``height``
124    function    ``xPos``
128    function    ``yPos``
132    function    ``window``
136    procedure   ``setPos``
142    procedure   ``toTop``
148    procedure   ``toBottom``
154    function    ``str``
====== =========== ==================================================

lib/imagefile.s7i
-----------------

:Lines:    171
:Entities: 3

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
50     function    ``hasImageExtension``
83     function    ``readImage``
159    function    ``readImage``
====== =========== ==================================================

lib/inflate.s7i
---------------

:Lines:    411
:Entities: 19

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
38     procedure   ``getNonCompressedBlock``
56     function    ``getLiteralOrLength``
79     function    ``getDistance``
83     function    ``decodeLength``
124    function    ``decodeDistance``
165    procedure   ``decodeFixedHuffmanCodes``
197    procedure   ``decodeDynamicHuffmanCodes``
231    procedure   ``decodeDynamicHuffmanCodes``
234    array       ``mapToOrderedLengths``
239    array       ``combinedDataCodeLengths``
241    array       ``combinedData``
242    array       ``literalOrLengthCodeLengths``
244    array       ``distanceCodeLengths``
287    procedure   ``processCompressedBlock``
314    function    ``inflate``
336    function    ``inflate``
350    procedure   ``processCompressedBlock64``
377    function    ``inflate64``
399    function    ``inflate64``
====== =========== ==================================================

lib/inifile.s7i
---------------

:Lines:    129
:Entities: 5

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
40     function    ``getParamValue``
53     procedure   ``getKeyValuePair``
69     function    ``getIniSection``
87     function    ``readIniFile``
117    function    ``readIniFile``
====== =========== ==================================================

lib/integer.s7i
---------------

:Lines:    663
:Entities: 55

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
33     procedure   ``:=``
58     function    ``+``
66     function    ``-``
75     function    ``!``
83     function    ``+``
91     function    ``-``
102    function    ``*``
118    function    ``div``
131    function    ``rem``
148    function    ``mdiv``
161    function    ``mod``
174    function    ``**``
185    function    ``<<``
195    function    ``>>``
202    procedure   ``+:=``
209    procedure   ``-:=``
216    procedure   ``*:=``
225    procedure   ``<<:=``
233    procedure   ``>>:=``
248    function    ``!``
256    function    ``=``
264    function    ``<>``
272    function    ``<``
280    function    ``>``
288    function    ``<=``
296    function    ``>=``
305    function    ``compare``
312    function    ``hashCode``
320    function    ``succ``
328    function    ``pred``
336    function    ``abs``
346    function    ``sqrt``
360    function    ``log10``
374    function    ``log2``
382    function    ``odd``
389    function    ``ord``
396    function    ``conv``
407    function    ``str``
418    function    ``string``
429    function    ``literal``
446    function    ``radix``
463    function    ``RADIX``
482    function    ``lpad0``
492    procedure   ``incr``
502    procedure   ``decr``
512    function    ``rand``
527    function    ``bitLength``
542    function    ``lowestSetBit``
565    function    ``integer``
588    function    ``parse``
612    function    ``integer``
638    function    ``sci``
641    procedure   ``DECLARE_MIN_MAX``
648    function    ``min``
655    function    ``max``
====== =========== ==================================================

lib/iobuffer.s7i
----------------

:Lines:    289
:Entities: 17

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
29     enumeration ``bufferModeType``
34     procedure   ``setbuf``
42     structure   ``bufferFile``
58     function    ``openBufferFile``
74     procedure   ``close``
84     procedure   ``flush``
94     procedure   ``setbuf``
104    procedure   ``write``
128    procedure   ``moveLeft``
145    function    ``getc``
167    function    ``gets``
215    function    ``eof``
226    function    ``hasNext``
236    function    ``length``
246    function    ``seekable``
258    procedure   ``seek``
278    function    ``tell``
====== =========== ==================================================

lib/jpeg.s7i
------------

:Lines:    1761
:Entities: 51

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
66     structure   ``jpegComponent``
70     structure   ``jpegScan``
83     structure   ``jpegHeader``
114    structure   ``jpegMinimumCodedUnit``
123    procedure   ``showHeader``
163    procedure   ``readStartOfFrame``
217    procedure   ``readDefineHuffmanTable``
222    array       ``numberOfCodesWithLength``
275    procedure   ``readStartOfScan``
338    procedure   ``readDefineQuantizationTable``
384    procedure   ``readDefineRestartInterval``
403    procedure   ``readApplicationSegment``
430    procedure   ``readComment``
453    function    ``getSymbol``
474    procedure   ``readBlock``
506    function    ``unzigzag``
510    array       ``zigzag``
534    procedure   ``fastIdct8``
616    procedure   ``idct8x8``
651    procedure   ``processBlock``
669    function    ``clampColor``
680    function    ``setPixel``
686    procedure   ``colorMinimumCodedUnit11``
713    procedure   ``colorMinimumCodedUnit12``
751    procedure   ``colorMinimumCodedUnit21``
784    procedure   ``colorMinimumCodedUnit22``
837    procedure   ``setupQuantization``
861    function    ``readEntropyCodedSegment``
877    procedure   ``loadMonochromeImage``
930    procedure   ``loadColorImage``
945    array       ``luma``
999    procedure   ``loadImage``
1037   function    ``loadSequential``
1055   procedure   ``readDcValue``
1073   procedure   ``readLumaDcOfAllBlocks``
1109   procedure   ``readDcValuesOfAllBlocks``
1163   procedure   ``refineDcValuesOfAllBlocks``
1214   procedure   ``readBlockAc``
1256   procedure   ``readLumaAcOfAllBlocks``
1296   procedure   ``readChromaAcOfAllBlocks``
1335   procedure   ``refineNonZeroes``
1369   procedure   ``refineBlockAc``
1423   procedure   ``refineLumaAcOfAllBlocks``
1460   procedure   ``refineChromaAcOfAllBlocks``
1495   procedure   ``loadProgressive``
1568   procedure   ``processBlock``
1580   procedure   ``colorMinimumCodedUnit``
1627   function    ``colorAllMinimumCodedUnits``
1664   function    ``readJpeg``
1673   array       ``mcuImage``
1749   function    ``readJpeg``
====== =========== ==================================================

lib/json.s7i
------------

:Lines:    891
:Entities: 80

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
40     enumeration ``jsonCategory``
49     function    ``str``
73     function    ``readJson``
90     function    ``category``
107    function    ``type``
123    function    ``void``
139    function    ``boolean``
154    function    ``integer``
169    function    ``bigInteger``
182    function    ``float``
202    function    ``string``
227    function    ``length``
238    function    ``minIdx``
249    function    ``maxIdx``
273    function    ``in``
283    function    ``not``
294    function    ``keys``
305    function    ``values``
313    function    ``str``
325    procedure   ``for``
344    procedure   ``for``
364    procedure   ``for``
375    structure   ``jsonBase``
390    structure   ``jsonNull``
409    function    ``jsonValue``
419    function    ``readJsonNull``
428    function    ``category``
429    function    ``type``
430    function    ``void``
431    function    ``str``
442    structure   ``jsonBoolean``
455    function    ``jsonValue``
466    function    ``readJsonBoolean``
481    function    ``category``
482    function    ``type``
483    function    ``boolean``
484    function    ``str``
495    structure   ``jsonNumber``
506    function    ``jsonNumber``
522    function    ``jsonValue``
530    function    ``jsonValue``
538    function    ``jsonValue``
541    function    ``readJsonNumber``
550    function    ``category``
553    function    ``type``
569    function    ``integer``
570    function    ``bigInteger``
571    function    ``float``
572    function    ``str``
583    structure   ``jsonString``
596    function    ``jsonValue``
607    function    ``readJsonString``
616    function    ``category``
617    function    ``type``
618    function    ``string``
619    function    ``str``
630    structure   ``jsonArray``
643    function    ``jsonValue``
654    function    ``readJsonArray``
677    function    ``category``
678    function    ``type``
680    function    ``length``
681    function    ``minIdx``
682    function    ``maxIdx``
683    function    ``values``
686    function    ``str``
713    structure   ``jsonObject``
714    array       ``elementNames``
726    procedure   ``@:=``
740    function    ``jsonValue``
748    function    ``readJsonObject``
796    function    ``category``
797    function    ``type``
799    function    ``in``
800    function    ``not``
801    function    ``keys``
804    function    ``str``
825    function    ``readJson``
863    function    ``readJson``
882    function    ``readJson``
====== =========== ==================================================

lib/json_serde.s7i
------------------

:Lines:    783
:Entities: 60

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
34     function    ``key_type``
35     function    ``base_type``
41     function    ``toJson``
70     function    ``toJson``
77     function    ``toJson``
81     procedure   ``DECLARE_TO_JSON_ARRAY``
82     procedure   ``DECLARE_TO_JSON_HASH``
83     procedure   ``DECLARE_TO_JSON_SET``
84     procedure   ``DECLARE_TO_JSON_ENUM``
85     procedure   ``DECLARE_TO_JSON_STRUCT``
92     procedure   ``declare_to_json``
106    function    ``toJson``
117    procedure   ``DECLARE_TO_JSON_ARRAY``
124    function    ``toJson``
145    procedure   ``DECLARE_TO_JSON_HASH``
153    function    ``toJson``
176    procedure   ``DECLARE_TO_JSON_SET``
183    function    ``toJson``
204    procedure   ``DECLARE_TO_JSON_ENUM``
209    function    ``toJson``
215    procedure   ``DECLARE_STRUCT_TO_JSON``
221    function    ``toJson``
227    procedure   ``DECLARE_TO_JSON_STRUCT``
231    function    ``toJson``
238    function    ``mapStructElementName``
245    function    ``toJson``
268    procedure   ``DECLARE_PARSE_JSON_ARRAY``
269    procedure   ``DECLARE_PARSE_JSON_HASH``
270    procedure   ``DECLARE_PARSE_JSON_SET``
271    procedure   ``DECLARE_PARSE_JSON_ENUM``
272    procedure   ``DECLARE_PARSE_JSON_STRUCT``
275    procedure   ``DECLARE_PARSE_JSON_BOOLEAN``
284    function    ``parseJson``
300    procedure   ``DECLARE_PARSE_JSON_OTHER``
309    function    ``parseJson``
331    procedure   ``declare_parse_json``
364    procedure   ``DECLARE_PARSE_JSON_ARRAY``
374    function    ``parseJson``
408    procedure   ``DECLARE_PARSE_JSON_HASH``
420    function    ``parseJson``
464    procedure   ``DECLARE_PARSE_JSON_SET``
475    function    ``parseJson``
509    procedure   ``DECLARE_PARSE_JSON_ENUM``
519    function    ``parseJson``
536    function    ``parseJson``
554    procedure   ``DECLARE_PARSE_STRUCT_ELEMENT_JSON``
562    procedure   ``parseStructElementJson``
570    procedure   ``DECLARE_PARSE_JSON_STRUCT``
574    procedure   ``parseStructElementJson``
582    function    ``mapStructElementName``
587    function    ``genMapJsonElementName``
609    function    ``parseJson``
665    function    ``fromJson``
683    function    ``fromJson``
702    function    ``fromJson``
717    procedure   ``DECLARE_FROM_JSON_OTHER``
723    function    ``fromJson``
745    procedure   ``declare_from_json``
756    function    ``fromJson``
778    procedure   ``declare_json_serde``
====== =========== ==================================================

lib/keybd.s7i
-------------

:Lines:    639
:Entities: 27

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
74     function    ``buttonPressed``
84     function    ``clickedXPos``
94     function    ``clickedYPos``
97     structure   ``console_keybd_file``
116    function    ``getc``
124    function    ``gets``
127    function    ``raw_getc``
128    function    ``line_read``
130    function    ``word_read``
140    function    ``inputReady``
154    function    ``getk``
175    function    ``getwd``
192    function    ``getln``
200    structure   ``graph_keybd_file``
220    function    ``getc``
228    function    ``gets``
261    function    ``buttonPressed``
271    function    ``clickedXPos``
281    function    ``clickedYPos``
284    function    ``raw_getc``
285    function    ``line_read``
287    function    ``word_read``
297    function    ``inputReady``
311    function    ``getk``
332    function    ``getwd``
349    function    ``getln``
637    function    ``getc``
====== =========== ==================================================

lib/keydescr.s7i
----------------

:Lines:    192
:Entities: 1

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
35     function    ``genKeyDescription``
====== =========== ==================================================

lib/leb128.s7i
--------------

:Lines:    218
:Entities: 7

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
35     function    ``getLeb128``
66     function    ``leb128ToInt``
104    function    ``leb128ToInt``
126    function    ``uLeb128ToInt``
157    function    ``uLeb128ToInt``
172    function    ``leb128``
199    function    ``uLeb128``
====== =========== ==================================================

lib/line.s7i
------------

:Lines:    164
:Entities: 8

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
32     structure   ``lineFile``
44     function    ``openLine``
58     procedure   ``write``
67     procedure   ``writeln``
75     procedure   ``flush``
91     function    ``getln``
127    function    ``getc``
148    function    ``gets``
====== =========== ==================================================

lib/listener.s7i
----------------

:Lines:    247
:Entities: 11

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
34     structure   ``inetListener``
68     function    ``openInetListener``
96     procedure   ``close``
117    procedure   ``signOn``
127    procedure   ``signOff``
139    procedure   ``listen``
158    function    ``accept``
176    procedure   ``waitForRequest``
203    function    ``getExistingConnection``
207    function    ``getNewConnection``
240    procedure   ``waitForRequest``
====== =========== ==================================================

lib/logfile.s7i
---------------

:Lines:    73
:Entities: 2

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
46     procedure   ``write``
64     procedure   ``writeln``
====== =========== ==================================================

lib/lower.s7i
-------------

:Lines:    142
:Entities: 10

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
36     structure   ``lowerFile``
66     function    ``openLower``
81     procedure   ``write``
90     procedure   ``writeln``
100    procedure   ``writeln``
106    procedure   ``moveLeft``
112    procedure   ``erase``
118    procedure   ``cursorOn``
124    procedure   ``cursorOff``
136    function    ``gets``
====== =========== ==================================================

lib/lzma.s7i
------------

:Lines:    934
:Entities: 43

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
32     structure   ``lzmaRangeDecoderState``
42     function    ``getc``
61     function    ``gets``
79     procedure   ``resetRangeDecoder``
99     function    ``isFinishedOK``
103    procedure   ``normalize``
111    function    ``getBit``
134    function    ``getDirectBit``
152    function    ``getDirectBits``
177    function    ``lzmaBitTreeDecoder``
190    procedure   ``init``
195    function    ``decode``
207    function    ``reverseDecode``
238    structure   ``lzmaLenDecoder``
247    procedure   ``init``
263    function    ``decodeLen``
277    function    ``updateState_Literal``
291    function    ``updateState_Match``
303    function    ``updateState_Rep``
315    function    ``updateState_ShortRep``
344    structure   ``lzmaDecoder``
353    array       ``litProbs``
372    procedure   ``showLzmaDecoder``
402    function    ``decodeProperties``
428    procedure   ``resetDictionary``
435    procedure   ``decodeLiteral``
466    procedure   ``initDist``
478    function    ``bitTreeReverseDecode``
497    function    ``decodeDistance``
525    procedure   ``resetPropabilities``
543    procedure   ``resetState``
561    procedure   ``copyMatch``
586    function    ``decodePacket``
683    function    ``lzmaDecompress``
726    structure   ``lzmaFile``
746    function    ``openLzmaFile``
781    function    ``getc``
811    function    ``gets``
850    function    ``eof``
861    function    ``hasNext``
886    function    ``length``
916    procedure   ``seek``
932    function    ``tell``
====== =========== ==================================================

lib/lzw.s7i
-----------

:Lines:    861
:Entities: 13

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
50     function    ``lzwCompressLsb``
128    function    ``lzwDecompress``
216    function    ``lzwDecompressLsb``
241    function    ``lzwCompressMsb``
320    function    ``lzwDecompress``
409    function    ``lzwDecompressMsb``
435    function    ``lzwCompressMsbEarlyChange``
515    function    ``lzwDecompressEarlyChange``
605    function    ``lzwDecompressMsbEarlyChange``
632    function    ``lzwDecompressEarlyChange``
723    function    ``lzwDecompressMsbEarlyChange``
745    function    ``unshrinkPartialClear``
782    function    ``lzwDecompressShrink``
====== =========== ==================================================

lib/magic.s7i
-------------

:Lines:    403
:Entities: 3

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
93     function    ``getMagic``
210    function    ``getMagic``
343    function    ``magicDescription``
====== =========== ==================================================

lib/mahjng32.s7i
----------------

:Lines:    1500
:Entities: 42

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
32     array       ``dot1_pic``
67     array       ``dot2_pic``
102    array       ``dot3_pic``
137    array       ``dot4_pic``
172    array       ``dot5_pic``
207    array       ``dot6_pic``
242    array       ``dot7_pic``
277    array       ``dot8_pic``
312    array       ``dot9_pic``
347    array       ``bamboo1_pic``
382    array       ``bamboo2_pic``
417    array       ``bamboo3_pic``
452    array       ``bamboo4_pic``
487    array       ``bamboo5_pic``
522    array       ``bamboo6_pic``
557    array       ``bamboo7_pic``
592    array       ``bamboo8_pic``
627    array       ``bamboo9_pic``
662    array       ``character1_pic``
697    array       ``character2_pic``
732    array       ``character3_pic``
767    array       ``character4_pic``
802    array       ``character5_pic``
837    array       ``character6_pic``
872    array       ``character7_pic``
907    array       ``character8_pic``
942    array       ``character9_pic``
977    array       ``north_pic``
1012   array       ``south_pic``
1047   array       ``east_pic``
1082   array       ``west_pic``
1117   array       ``middle_pic``
1152   array       ``green_pic``
1187   array       ``white_pic``
1222   array       ``plum_pic``
1257   array       ``orchid_pic``
1292   array       ``chrysanthemum_pic``
1327   array       ``bamboo_pic``
1362   array       ``spring_pic``
1397   array       ``summer_pic``
1432   array       ``autumn_pic``
1467   array       ``winter_pic``
====== =========== ==================================================

lib/make.s7i
------------

:Lines:    544
:Entities: 19

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
36     enumeration ``makeFlag``
43     procedure   ``make``
46     procedure   ``doMake``
50     array       ``targets``
91     function    ``expandInternalMacro``
119    function    ``applyInternalMacros``
123    set         ``iternalMacroDesignator``
127    set         ``dependencySet``
128    array       ``strictDependencies``
189    procedure   ``processCommands``
230    function    ``pattern_match``
251    procedure   ``processRule``
254    procedure   ``processRule``
299    procedure   ``processPatternRule``
381    procedure   ``processRule``
412    procedure   ``make``
505    procedure   ``make``
521    procedure   ``make``
540    procedure   ``make``
====== =========== ==================================================

lib/makedata.s7i
----------------

:Lines:    1428
:Entities: 55

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
36     set         ``target_name_char``
37     set         ``macro_name_char``
42     structure   ``ruleType``
44     array       ``dependencies``
45     array       ``commands``
57     structure   ``makeDataType``
69     function    ``patternMatches``
86     function    ``patternMatches``
98     function    ``patternSubstitute``
127    function    ``replaceSuffixes``
151    function    ``getMacroName``
165    function    ``getMacroParameter``
189    function    ``applyMacros``
193    function    ``replaceSuffixes``
214    function    ``strip``
230    function    ``subst``
256    function    ``patsubst``
282    function    ``dir``
313    function    ``notdir``
344    function    ``suffix``
376    function    ``basename``
412    function    ``addprefix``
444    function    ``addsuffix``
476    function    ``filter``
486    array       ``patternList``
514    function    ``filterOut``
524    array       ``patternList``
552    function    ``foreach``
592    function    ``call``
620    function    ``sort``
628    array       ``wordArray``
653    function    ``shell``
667    function    ``wildcard``
673    array       ``matches``
700    function    ``applyMacros``
824    function    ``getMacroParameter``
859    function    ``getMakeLine``
909    function    ``getMakeTarget``
922    procedure   ``find_endif_directive``
943    procedure   ``find_endif_or_else_directive``
964    procedure   ``execIf``
988    procedure   ``find_endif``
1003   procedure   ``find_endif_or_else``
1018   procedure   ``execIfeq``
1049   procedure   ``execIfdef``
1071   function    ``getMakeDependency``
1130   function    ``readRule``
1181   function    ``patternRulePresent``
1206   procedure   ``addPatternRule``
1221   procedure   ``addRule``
1243   procedure   ``includeMakefile``
1246   procedure   ``readMakefile``
1250   array       ``alternateTargets``
1379   procedure   ``includeMakefile``
1410   procedure   ``readMakefile``
====== =========== ==================================================

lib/math.s7i
------------

:Lines:    201
:Entities: 17

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
50     function    ``sin``
57     function    ``cos``
64     function    ``tan``
72     function    ``asin``
80     function    ``acos``
88     function    ``atan``
99     function    ``atan2``
107    function    ``sinh``
115    function    ``cosh``
123    function    ``tanh``
130    function    ``exp``
139    function    ``expm1``
151    function    ``log``
165    function    ``log1p``
177    function    ``log10``
189    function    ``log2``
200    function    ``sqrt``
====== =========== ==================================================

lib/mixarith.s7i
----------------

:Lines:    249
:Entities: 25

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
37     function    ``+``
45     function    ``+``
53     function    ``-``
61     function    ``-``
69     function    ``*``
77     function    ``*``
88     function    ``/``
99     function    ``/``
107    function    ``+``
115    function    ``+``
123    function    ``-``
131    function    ``-``
139    function    ``*``
147    function    ``*``
158    function    ``/``
169    function    ``/``
177    function    ``float``
185    function    ``+``
193    function    ``+``
201    function    ``-``
209    function    ``-``
217    function    ``*``
225    function    ``*``
236    function    ``/``
247    function    ``/``
====== =========== ==================================================

lib/modern27.s7i
----------------

:Lines:    1099
:Entities: 1

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
32     function    ``genModern27``
====== =========== ==================================================

lib/more.s7i
------------

:Lines:    130
:Entities: 4

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
32     structure   ``moreFile``
58     function    ``openMore``
83     procedure   ``writeln``
112    procedure   ``write``
====== =========== ==================================================

lib/msgdigest.s7i
-----------------

:Lines:    1222
:Entities: 47

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
50     function    ``md4``
55     array       ``shiftAmount``
59     array       ``idx``
133    function    ``createMd5Table``
135    array       ``k``
152    function    ``md5``
157    array       ``shiftAmount``
162    array       ``k``
244    function    ``ripemd160``
248    array       ``k1``
249    array       ``k2``
250    array       ``r1``
256    array       ``r2``
262    array       ``s1``
268    array       ``s2``
277    array       ``x``
379    function    ``sha1``
476    function    ``sha224``
482    array       ``k``
601    function    ``sha256``
607    array       ``k``
727    function    ``sha384``
734    array       ``k``
873    function    ``sha512``
880    array       ``k``
1020   procedure   ``keccakf``
1023   array       ``roundConstant``
1036   array       ``permute``
1040   array       ``rotationCount``
1089   function    ``keccak1600``
1131   function    ``sha3_224``
1140   function    ``sha3_256``
1149   function    ``sha3_384``
1158   function    ``sha3_512``
1166   enumeration ``digestAlgorithm``
1177   function    ``msgDigest``
1179   function    ``msgDigest``
1180   function    ``msgDigest``
1181   function    ``msgDigest``
1182   function    ``msgDigest``
1183   function    ``msgDigest``
1184   function    ``msgDigest``
1185   function    ``msgDigest``
1186   function    ``msgDigest``
1187   function    ``msgDigest``
1194   function    ``blockSize``
1211   function    ``digestSize``
====== =========== ==================================================

lib/multiscr.s7i
----------------

:Lines:    68
:Entities: 1

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
46     procedure   ``INIT_MULTI``
====== =========== ==================================================

lib/null_file.s7i
-----------------

:Lines:    345
:Entities: 14

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
48     structure   ``null_file``
89     procedure   ``writeln``
101    procedure   ``writeln``
114    function    ``gets``
131    function    ``getc``
163    function    ``getTerminatedString``
194    function    ``getwd``
227    function    ``getln``
263    function    ``eoln``
275    function    ``length``
299    procedure   ``seek``
313    function    ``tell``
321    procedure   ``moveLeft``
327    procedure   ``erase``
====== =========== ==================================================

lib/osfiles.s7i
---------------

:Lines:    1085
:Entities: 84

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
36     procedure   ``GET_ATIME``
41     procedure   ``GET_CTIME``
46     procedure   ``GET_MTIME``
51     procedure   ``SET_ATIME``
55     procedure   ``SET_MTIME``
59     procedure   ``GET_ATIME_OF_SYMLINK``
64     procedure   ``GET_MTIME_OF_SYMLINK``
69     procedure   ``SET_MTIME_OF_SYMLINK``
91     function    ``readDir``
111    function    ``fileType``
128    function    ``fileTypeSL``
144    function    ``fileSize``
159    function    ``bigFileSize``
174    function    ``getFileMode``
178    function    ``fileMode``
193    procedure   ``setFileMode``
208    function    ``getATime``
232    procedure   ``setATime``
252    function    ``getCTime``
275    function    ``getMTime``
300    procedure   ``setMTime``
320    function    ``getOwner``
334    procedure   ``setOwner``
349    function    ``getGroup``
363    procedure   ``setGroup``
380    function    ``getFileMode``
397    function    ``getATime``
423    function    ``getMTime``
450    procedure   ``setMTime``
470    function    ``getOwner``
486    procedure   ``setOwner``
501    function    ``getGroup``
517    procedure   ``setGroup``
540    function    ``readLink``
564    function    ``readLink``
584    function    ``finalPath``
600    procedure   ``makeLink``
605    procedure   ``symlink``
624    procedure   ``removeFile``
637    procedure   ``removeTree``
659    procedure   ``copyFile``
676    procedure   ``cloneFile``
697    procedure   ``moveFile``
711    procedure   ``makeDir``
715    procedure   ``mkdir``
728    function    ``getcwd``
740    procedure   ``chdir``
754    function    ``homeDir``
772    function    ``toAbsPath``
798    function    ``getParentDir``
821    function    ``getFileName``
846    procedure   ``makeParentDirs``
886    function    ``convDosPath``
910    function    ``toStdPath``
927    structure   ``osFileSys``
934    function    ``openOsFileSys``
952    function    ``readDir``
955    function    ``fileType``
958    function    ``fileTypeSL``
961    function    ``fileSize``
964    function    ``bigFileSize``
967    function    ``getFileMode``
970    procedure   ``setFileMode``
976    function    ``getMTime``
979    procedure   ``setMTime``
985    function    ``getOwner``
988    procedure   ``setOwner``
994    function    ``getGroup``
997    procedure   ``setGroup``
1003   function    ``getFileMode``
1006   function    ``getMTime``
1009   procedure   ``setMTime``
1015   function    ``getOwner``
1018   procedure   ``setOwner``
1024   function    ``getGroup``
1027   procedure   ``setGroup``
1033   function    ``open``
1036   function    ``getFile``
1051   procedure   ``putFile``
1064   function    ``readLink``
1067   procedure   ``makeLink``
1073   procedure   ``makeDir``
1078   function    ``getcwd``
1081   procedure   ``chdir``
====== =========== ==================================================

lib/pbm.s7i
-----------

:Lines:    230
:Entities: 7

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
40     procedure   ``readPbmAsciiImage``
63     procedure   ``readPbmBinaryImageLine``
92     procedure   ``readPbmBinaryImage``
111    function    ``readPbm``
157    function    ``readPbm``
176    function    ``str``
220    procedure   ``writePbm``
====== =========== ==================================================

lib/pcx.s7i
-----------

:Lines:    638
:Entities: 27

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
58     structure   ``pcxHeader``
81     procedure   ``showHeader``
104    procedure   ``readHeader``
143    function    ``isPcxMagic``
154    function    ``fromPcxRunLengthEncoding``
191    procedure   ``readPcxEgaPalette``
208    procedure   ``readPcxVgaPalette``
225    procedure   ``readPcxImageLineVga``
238    procedure   ``readPcxImageVga``
252    procedure   ``readPcxGrayscaleImageLineVga``
267    procedure   ``readPcxGrayscaleImageVga``
280    procedure   ``readPcxTrueColorImageLine``
295    procedure   ``readPcxTrueColorImage``
310    procedure   ``readPcxImageLine16Colors``
324    procedure   ``readPcxImage16Colors``
338    procedure   ``readPcxImageLineCga4``
354    procedure   ``readPcxImageCga4``
368    procedure   ``readPcxImageLineCga2``
388    procedure   ``readPcxImageCga2``
402    procedure   ``readPcxImageLine2Planes``
425    procedure   ``readPcxImage2Planes``
439    procedure   ``readPcxImageLine3Planes``
462    procedure   ``readPcxImage3Planes``
476    procedure   ``readPcxImageLineEga``
498    procedure   ``readPcxImageEga``
522    function    ``readPcx``
626    function    ``readPcx``
====== =========== ==================================================

lib/pem.s7i
-----------

:Lines:    185
:Entities: 5

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
42     structure   ``pemObject``
55     function    ``pemObject``
74     function    ``str``
90     function    ``pemObject``
139    function    ``readPemObject``
====== =========== ==================================================

lib/pgm.s7i
-----------

:Lines:    238
:Entities: 8

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
40     procedure   ``readPgmAsciiImage``
60     procedure   ``readPgmBinaryImageLine8``
77     procedure   ``readPgmBinaryImageLine16``
95     procedure   ``readPgmBinaryImage``
123    function    ``readPgm``
179    function    ``readPgm``
198    function    ``str``
228    procedure   ``writePgm``
====== =========== ==================================================

lib/pic16.s7i
-------------

:Lines:    1037
:Entities: 53

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
32     array       ``bat_pic``
51     array       ``book_pic``
70     array       ``cancel_pic``
89     array       ``card_back_pic``
108    array       ``checkmark_pic``
127    array       ``clear_pic``
146    array       ``crown_pic``
165    array       ``crystal_ball_pic``
184    array       ``demon_pic``
203    array       ``diamond_pic``
222    array       ``drop_pic``
241    array       ``execute_pic``
260    array       ``exit_pic``
279    array       ``eye_pic``
298    array       ``flask_pic``
317    array       ``folder_pic``
336    array       ``fountain_pic``
355    array       ``glasses_pic``
374    array       ``goblet_pic``
393    array       ``goldbar_pic``
412    array       ``grating_pic``
431    array       ``hand_pic``
450    array       ``harp_pic``
469    array       ``helmet_pic``
488    array       ``holy_cross_pic``
507    array       ``hourglass_pic``
526    array       ``hut_pic``
545    array       ``jade_figurine_pic``
564    array       ``key_pic``
583    array       ``lamp_pic``
602    array       ``load_pic``
621    array       ``magic_wand_pic``
640    array       ``magnifier_pic``
659    array       ``necklace_pic``
678    array       ``ogre_pic``
697    array       ``on_off_pic``
716    array       ``reset_pic``
735    array       ``return_pic``
754    array       ``right_arrow_pic``
773    array       ``ruby_pic``
792    array       ``save_pic``
811    array       ``save_as_pic``
830    array       ``scepter_pic``
849    array       ``seed7_include_pic``
868    array       ``seed7_source_pic``
887    array       ``silver_bars_pic``
906    array       ``snake_pic``
925    array       ``statue_pic``
944    array       ``sword_pic``
963    array       ``take_pic``
982    array       ``terminate_pic``
1001   array       ``tree_pic``
1020   array       ``vampire_pic``
====== =========== ==================================================

lib/pic32.s7i
-------------

:Lines:    2060
:Entities: 58

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
32     array       ``bat_pic``
67     array       ``book_pic``
102    array       ``cancel_pic``
137    array       ``card_back_pic``
172    array       ``checkmark_pic``
207    array       ``clear_pic``
242    array       ``crown_pic``
277    array       ``crystal_ball_pic``
312    array       ``demon_pic``
347    array       ``diamond_pic``
382    array       ``drop_pic``
417    array       ``execute_pic``
452    array       ``exit_pic``
487    array       ``eye_pic``
522    array       ``flask_pic``
557    array       ``folder_pic``
592    array       ``fountain_pic``
627    array       ``glasses_pic``
662    array       ``goblet_pic``
697    array       ``goldbar_pic``
732    array       ``grating_pic``
767    array       ``hand_pic``
802    array       ``harp_pic``
837    array       ``helmet_pic``
872    array       ``holy_cross_pic``
907    array       ``hourglass_pic``
942    array       ``hut_pic``
977    array       ``jade_figurine_pic``
1012   array       ``key_pic``
1047   array       ``lamp_pic``
1082   array       ``left_arrow_pic``
1117   array       ``load_pic``
1152   array       ``magic_wand_pic``
1187   array       ``magnifier_pic``
1222   array       ``necklace_pic``
1257   array       ``next_pic``
1292   array       ``ogre_pic``
1327   array       ``on_off_pic``
1362   array       ``previous_pic``
1397   array       ``reset_pic``
1432   array       ``redo_pic``
1467   array       ``return_pic``
1502   array       ``right_arrow_pic``
1537   array       ``ruby_pic``
1572   array       ``save_pic``
1607   array       ``save_as_pic``
1642   array       ``scepter_pic``
1677   array       ``seed7_include_pic``
1712   array       ``seed7_source_pic``
1747   array       ``silver_bars_pic``
1782   array       ``snake_pic``
1817   array       ``statue_pic``
1852   array       ``sword_pic``
1887   array       ``take_pic``
1922   array       ``terminate_pic``
1957   array       ``tree_pic``
1992   array       ``undo_pic``
2027   array       ``vampire_pic``
====== =========== ==================================================

lib/pic_util.s7i
----------------

:Lines:    144
:Entities: 3

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
38     function    ``charColor``
79     function    ``createPixmap``
109    procedure   ``drawPattern``
====== =========== ==================================================

lib/pixelimage.s7i
------------------

:Lines:    320
:Entities: 13

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
44     procedure   ``setPixels``
61     function    ``getPixmap``
80     function    ``getPixelImage``
86     function    ``getRotated90``
115    procedure   ``rotate90``
124    function    ``getRotated180``
154    procedure   ``rotate180``
183    function    ``getRotated270``
210    procedure   ``rotate270``
219    procedure   ``mirrorHorizontally``
245    procedure   ``mirrorVertically``
270    function    ``getRotated90AndMirroredHorizontally``
301    function    ``getRotated270AndMirroredHorizontally``
====== =========== ==================================================

lib/pixmap_file.s7i
-------------------

:Lines:    459
:Entities: 28

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
41     structure   ``pixmapFontFile``
66     function    ``openPixmapFontFile``
83     function    ``openPixmapFontFile``
105    procedure   ``flush``
114    procedure   ``setFont``
120    function    ``getFont``
123    function    ``getFont``
134    function    ``height``
142    function    ``width``
150    function    ``line``
158    function    ``column``
167    procedure   ``clear``
186    procedure   ``clear``
201    procedure   ``v_scroll``
234    procedure   ``setPos``
252    procedure   ``setPosXY``
270    procedure   ``setLine``
281    procedure   ``setColumn``
292    procedure   ``color``
302    procedure   ``color``
309    procedure   ``scale``
312    procedure   ``scale``
322    procedure   ``write``
355    procedure   ``writeln``
369    procedure   ``moveLeft``
388    procedure   ``erase``
420    procedure   ``cursorOn``
442    procedure   ``cursorOff``
====== =========== ==================================================

lib/pixmapfont.s7i
------------------

:Lines:    184
:Entities: 29

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
37     function    ``.``
38     function    ``.``
39     function    ``.``
40     function    ``.``
41     function    ``fontSize``
42     function    ``scale``
43     function    ``foreground``
44     function    ``background``
45     function    ``baseLineDelta``
46     function    ``line_delta``
47     function    ``column_delta``
48     function    ``characterSpacing``
55     structure   ``pixmapFontType``
73     function    ``genPixmapFont``
76     function    ``getFontCharPixmap``
80     function    ``fontSize``
81     function    ``scale``
82     function    ``foreground``
83     function    ``background``
84     function    ``baseLineDelta``
85     function    ``line_delta``
86     function    ``column_delta``
87     function    ``characterSpacing``
98     structure   ``fontKeyType``
106    function    ``hashCode``
110    function    ``compare``
137    function    ``genPixmapFont``
159    function    ``getFont``
181    function    ``getFontCharPixmap``
====== =========== ==================================================

lib/pkcs1.s7i
-------------

:Lines:    543
:Entities: 26

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
34     structure   ``rsaKey``
48     structure   ``rsaKeyPair``
57     function    ``rsaKey``
69     function    ``literal``
77     function    ``rsaKeyPair``
89     function    ``rsaKeyPair``
103    function    ``literal``
114    function    ``isProbablyPrime``
129    function    ``getProbablyPrime``
149    function    ``genRsaKeyPair``
174    function    ``int2Octets``
183    function    ``octets2int``
191    function    ``emeOaepEncoding``
224    function    ``emeOaepDecoding``
264    function    ``emePkcs1V15Encoding``
284    function    ``emePkcs1V15Decoding``
308    function    ``emsaPkcs1V15Encoding``
325    function    ``emsaPkcs1V15Decoding``
351    function    ``rsaEncrypt``
369    function    ``rsaDecrypt``
391    function    ``rsaesOaepEncrypt``
416    function    ``rsaesOaepDecrypt``
444    function    ``rsaesPkcs1V15Encrypt``
470    function    ``rsaesPkcs1V15Decrypt``
498    function    ``rsassaPkcs1V15Encrypt``
525    function    ``rsassaPkcs1V15Decrypt``
====== =========== ==================================================

lib/png.s7i
-----------

:Lines:    1064
:Entities: 43

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
48     structure   ``pngHeader``
62     procedure   ``showHeader``
76     function    ``pngHeader``
95     function    ``str``
105    function    ``isOkay``
117    procedure   ``computeBytesPerPixel``
133    procedure   ``computeBytesPerScanline``
147    function    ``readPngChunk``
175    function    ``paethPredictor``
197    procedure   ``filterPngData``
269    procedure   ``fillPngImageLine1Bit``
301    procedure   ``fillPngImage1Bit``
313    procedure   ``fillPngImageLine2Bit``
341    procedure   ``fillPngImage2Bit``
353    procedure   ``fillPngImageLine4Bit``
373    procedure   ``fillPngImage4Bit``
385    procedure   ``fillPngImageLine8Bit``
398    procedure   ``fillPngImage8Bit``
410    procedure   ``fillPngImageLine8Bit``
425    procedure   ``fillPngImage8Bit``
437    procedure   ``fillPngImageLine16Bit``
452    procedure   ``fillPngImage16Bit``
464    procedure   ``fillPngImageLine24Bit``
479    procedure   ``fillPngImage24Bit``
491    procedure   ``fillPngImageLine48Bit``
506    procedure   ``fillPngImage48Bit``
518    function    ``pixelDataToImage``
586    function    ``interlaceToImage``
678    function    ``readPng``
748    function    ``readPng``
762    function    ``imageToPixelData``
782    procedure   ``doFilter0``
799    procedure   ``doFilter1``
818    procedure   ``doFilter2``
837    procedure   ``doFilter3``
856    procedure   ``doFilter3``
886    procedure   ``doFilter4``
906    procedure   ``doFilter4``
939    procedure   ``setPngFilter``
1007   procedure   ``setPngFilter``
1017   function    ``genPngChunk``
1028   function    ``str``
1054   procedure   ``writePng``
====== =========== ==================================================

lib/poll.s7i
------------

:Lines:    313
:Entities: 22

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
57     procedure   ``:=``
59     function    ``_GENERATE_EMPTY_POLL_DATA``
73     procedure   ``clear``
92     procedure   ``addCheck``
96     procedure   ``addCheck``
117    procedure   ``addCheck``
133    procedure   ``removeCheck``
137    procedure   ``removeCheck``
156    procedure   ``removeCheck``
172    function    ``getCheck``
176    function    ``getCheck``
192    function    ``getCheck``
206    procedure   ``poll``
222    function    ``getFinding``
226    function    ``getFinding``
243    function    ``getFinding``
257    procedure   ``iterChecks``
271    procedure   ``iterFindings``
279    function    ``hasNext``
282    function    ``nextFile``
294    function    ``nextFile``
304    procedure   ``for``
====== =========== ==================================================

lib/ppm.s7i
-----------

:Lines:    240
:Entities: 8

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
40     procedure   ``readPpmAsciiImage``
66     procedure   ``readPpmBinaryImageLine8``
83     procedure   ``readPpmBinaryImageLine16``
101    procedure   ``readPpmBinaryImage``
129    function    ``readPpm``
185    function    ``readPpm``
204    function    ``str``
230    procedure   ``writePpm``
====== =========== ==================================================

lib/process.s7i
---------------

:Lines:    541
:Entities: 33

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
44     procedure   ``:=``
45     function    ``_GENERATE_EMPTY_PROCESS``
61     function    ``=``
70     function    ``<>``
84     function    ``compare``
91     function    ``hashCode``
100    function    ``str``
108    function    ``isAlive``
111    function    ``startProcess``
137    function    ``startProcess``
179    function    ``startProcess``
199    function    ``startProcess``
205    array       ``parameters``
235    function    ``startPipe``
255    function    ``startPipe``
261    array       ``parameters``
273    function    ``childStdInClibFile``
274    function    ``childStdOutClibFile``
275    function    ``childStdErrClibFile``
285    function    ``childStdIn``
307    function    ``childStdOut``
329    function    ``childStdErr``
348    procedure   ``kill``
356    procedure   ``waitFor``
365    function    ``exitValue``
373    function    ``getSearchPath``
385    procedure   ``setSearchPath``
393    function    ``commandPath``
440    function    ``commandDir``
477    procedure   ``pipe2``
498    procedure   ``pipe2``
518    procedure   ``pty``
523    procedure   ``pty``
====== =========== ==================================================

lib/progs.s7i
-------------

:Lines:    789
:Entities: 100

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
50     procedure   ``:=``
52     function    ``_GENERATE_EMPTY_PROG``
66     function    ``=``
73     function    ``<>``
82     function    ``compare``
89     function    ``hashCode``
100    function    ``name``
108    function    ``path``
111    enumeration ``singleParseOption``
128    function    ``parseFile``
133    function    ``parseFile``
139    function    ``parseFile``
158    function    ``parseFile``
175    function    ``parseFile``
191    function    ``parseFile``
205    function    ``parseFile``
209    function    ``parseStri``
214    function    ``parseStri``
220    function    ``parseStri``
235    function    ``parseStri``
248    function    ``parseStri``
260    function    ``parseStri``
270    function    ``parseStri``
274    function    ``parseStri``
279    function    ``parseStri``
285    function    ``parseStri``
300    function    ``parseStri``
313    function    ``parseStri``
325    function    ``parseStri``
335    function    ``parseStri``
343    function    ``evaluate``
352    function    ``evaluate``
360    procedure   ``execute``
368    procedure   ``execute``
378    procedure   ``execute``
387    procedure   ``execute``
398    function    ``sysVar``
405    function    ``errorCount``
411    enumeration ``errorType``
481    function    ``str``
492    structure   ``parseError``
502    procedure   ``DO_GET_ERROR``
514    function    ``getError``
534    function    ``globalObjects``
537    function    ``structSymbols``
546    function    ``syobject``
553    function    ``match``
556    function    ``matchExpr``
565    function    ``isTemp``
573    function    ``isVar``
580    procedure   ``setVar``
588    function    ``category``
607    procedure   ``setCategory``
618    function    ``formalParams``
626    procedure   ``setFormalParams``
635    procedure   ``appendFormalParams``
644    function    ``resultVar``
653    function    ``resultInitValue``
663    function    ``localConsts``
673    function    ``localVars``
682    function    ``body``
691    function    ``arrayMinIdx``
700    function    ``arrayMaxIdx``
709    function    ``arrayLength``
713    function    ``arrayToList``
714    function    ``structToList``
715    function    ``hashDataToList``
716    function    ``hashKeysToList``
717    function    ``hashLength``
718    function    ``interfaceToStruct``
727    function    ``file``
736    function    ``path``
744    function    ``line``
753    function    ``objNumber``
756    function    ``allocList``
757    function    ``alloc``
759    function    ``alloc``
760    function    ``alloc``
762    function    ``getValue``
763    function    ``getValue``
764    function    ``getValue``
765    function    ``getValue``
766    function    ``getValue``
767    function    ``getValue``
768    function    ``getValue``
769    function    ``getValue``
770    function    ``getValue``
771    function    ``getValue``
772    function    ``getValue``
774    function    ``getValue``
775    function    ``getValue``
776    function    ``getValue``
777    function    ``getValue``
778    function    ``getValue``
780    function    ``getValue``
781    function    ``getValue``
784    procedure   ``setValue``
786    function    ``typeNumber``
787    function    ``typeObject``
788    function    ``interfaces``
====== =========== ==================================================

lib/propertyfile.s7i
--------------------

:Lines:    155
:Entities: 7

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
32     set         ``propertyWhiteSpace``
41     function    ``readPropertyNameOrValue``
84     function    ``readPropertyFile``
88     set         ``keyTerminator``
89     set         ``valueTerminator``
126    function    ``readPropertyFile``
143    function    ``readPropertyFile8``
====== =========== ==================================================

lib/rational.s7i
----------------

:Lines:    792
:Entities: 41

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
37     structure   ``rational``
43     procedure   ``normalize``
52     procedure   ``reduce``
72     function    ``gcd1``
87     function    ``gcd2``
112    function    ``/``
127    function    ``+``
135    function    ``-``
148    function    ``+``
166    function    ``-``
184    function    ``*``
203    function    ``/``
221    procedure   ``+:=``
236    procedure   ``-:=``
251    procedure   ``*:=``
262    procedure   ``/:=``
275    function    ``**``
295    function    ``=``
305    function    ``<>``
315    function    ``<``
325    function    ``>``
335    function    ``<=``
345    function    ``>=``
356    function    ``compare``
365    function    ``hashCode``
372    function    ``rat``
384    function    ``rational``
396    function    ``conv``
409    function    ``abs``
421    function    ``floor``
428    function    ``ceil``
435    function    ``trunc``
444    function    ``round``
461    function    ``round10``
495    function    ``str``
541    function    ``fraction``
568    function    ``digits``
602    function    ``decimalExponent``
639    function    ``sci``
708    function    ``rational``
781    function    ``parse``
====== =========== ==================================================

lib/ref_list.s7i
----------------

:Lines:    252
:Entities: 24

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
39     procedure   ``:=``
41     function    ``_GENERATE_EMPTY_REFLIST``
56     procedure   ``&:=``
68     procedure   ``@:=``
77     function    ``make_list``
85     function    ``=``
93     function    ``<>``
111    function    ``..``
129    function    ``..``
138    function    ``&``
147    function    ``in``
156    function    ``not``
165    function    ``pos``
184    function    ``pos``
188    procedure   ``incl``
189    procedure   ``excl``
196    function    ``length``
199    procedure   ``TRACE_LIST``
205    procedure   ``for``
213    procedure   ``for``
231    procedure   ``for``
241    procedure   ``for``
246    procedure   ``for``
249    procedure   ``for``
====== =========== ==================================================

lib/reference.s7i
-----------------

:Lines:    126
:Entities: 16

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
39     procedure   ``:=``
41     function    ``_GENERATE_NIL``
61     function    ``=``
69     function    ``<>``
78     function    ``compare``
85     function    ``hashCode``
93     function    ``str``
100    function    ``getType``
107    procedure   ``setType``
110    function    ``is_symb``
111    function    ``symb``
112    function    ``getValue``
113    function    ``getfunc``
114    function    ``getobj``
116    procedure   ``TRACE_REF``
118    function    ``get_type``
====== =========== ==================================================

lib/reverse.s7i
---------------

:Lines:    94
:Entities: 6

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
29     structure   ``reverse_file``
35     function    ``openReverse``
55     procedure   ``write``
72     procedure   ``writeln``
79     procedure   ``moveLeft``
89     procedure   ``erase``
====== =========== ==================================================

lib/rpm.s7i
-----------

:Lines:    3487
:Entities: 114

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
54     enumeration ``rpmPackageType``
58     structure   ``rpmLead``
69     structure   ``rpmHeader``
78     structure   ``rpmIndexEntry``
84     array       ``arrayValue``
460    function    ``sigtagName``
495    function    ``rpmtagName``
794    function    ``rpmDependencyFlagsString``
837    procedure   ``show``
852    procedure   ``show``
866    procedure   ``show``
877    function    ``getUtf8z``
893    function    ``getStriValue``
931    function    ``getArrayValue``
934    array       ``arrayValue``
949    function    ``getIntValue``
967    procedure   ``initLead``
980    procedure   ``readLead``
1005   function    ``str``
1018   procedure   ``writeLead``
1024   procedure   ``readHeader``
1039   function    ``str``
1047   function    ``str``
1054   structure   ``rpmDependency``
1061   function    ``=``
1067   function    ``<>``
1073   function    ``compare``
1087   function    ``findDependency``
1101   procedure   ``addDependency``
1115   structure   ``rpmCatalogEntry``
1140   array       ``dependencies``
1145   procedure   ``write``
1177   structure   ``rpmSection``
1200   structure   ``rpmArchive``
1211   array       ``dirNames``
1212   array       ``classDict``
1213   array       ``provided``
1214   array       ``required``
1215   array       ``dependsDict``
1222   function    ``getStringArray``
1224   array       ``stringArray``
1242   procedure   ``createMinimumOfCatalog``
1300   function    ``getInt``
1335   procedure   ``readCatalogEntry``
1367   procedure   ``updateIndexEntry``
1395   procedure   ``updateIndexEntry``
1424   procedure   ``updateIndexEntry``
1444   procedure   ``updateIndexEntry``
1482   procedure   ``updateIndexEntry``
1509   procedure   ``updateProvisions``
1513   array       ``provideName``
1514   array       ``provideVersion``
1528   procedure   ``updateRequirements``
1532   array       ``requireName``
1533   array       ``requireVersion``
1547   procedure   ``updateDependencies``
1577   procedure   ``updateHeader``
1629   procedure   ``updateHeader``
1650   procedure   ``update``
1669   function    ``isDirty``
1687   procedure   ``fillTagMap``
1702   procedure   ``readSection``
1716   function    ``sectionStri``
1748   procedure   ``assignIndexValues``
1770   procedure   ``updateStore``
1818   function    ``checkHeaderDigest``
1846   procedure   ``doSettings``
1873   procedure   ``readDependencies``
1920   function    ``getDigest``
1939   procedure   ``checkPayloadDigest``
1979   procedure   ``checkUncompressedDigest``
2003   procedure   ``getArchive``
2046   function    ``archiveFilePath``
2065   function    ``openRpm``
2108   function    ``openRpm``
2122   procedure   ``close``
2215   function    ``addImplicitDir``
2227   function    ``followSymlink``
2265   function    ``followSymlink``
2279   procedure   ``fixRegisterAndCatalog``
2298   function    ``findNameIndex``
2312   function    ``getNameIndex``
2325   function    ``fileClass``
2344   procedure   ``setDependencies``
2370   procedure   ``setDirIndexAndBaseName``
2399   function    ``readDir``
2410   function    ``readDir``
2425   function    ``fileType``
2484   function    ``fileTypeSL``
2531   function    ``getFileMode``
2551   procedure   ``setFileMode``
2587   function    ``fileSize``
2608   function    ``getMTime``
2631   procedure   ``setMTime``
2667   function    ``getOwner``
2694   procedure   ``setOwner``
2736   function    ``getGroup``
2763   procedure   ``setGroup``
2806   function    ``getFileMode``
2845   function    ``getMTime``
2885   procedure   ``setMTime``
2933   function    ``getOwner``
2975   procedure   ``setOwner``
3030   function    ``getGroup``
3072   procedure   ``setGroup``
3125   function    ``readLink``
3160   procedure   ``makeLink``
3216   function    ``getFile``
3252   procedure   ``putFile``
3346   procedure   ``makeDir``
3412   procedure   ``removeFile``
3453   procedure   ``for``
3463   function    ``openFileInRpm``
3484   function    ``open``
====== =========== ==================================================

lib/rpmext.s7i
--------------

:Lines:    318
:Entities: 30

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
32     function    ``getName``
34     procedure   ``setName``
36     function    ``getPackageType``
38     procedure   ``setPackageType``
40     procedure   ``addProvision``
43     procedure   ``addRequirement``
46     function    ``getTagValue``
48     function    ``getTagValue``
50     procedure   ``setTagValue``
53     procedure   ``setTagValue``
56     procedure   ``setTagValue``
59     procedure   ``getFileFlags``
61     procedure   ``getFileFlags``
63     procedure   ``setFileFlags``
66     procedure   ``setFileFlags``
73     function    ``getName``
80     procedure   ``setName``
89     function    ``getPackageType``
96     procedure   ``setPackageType``
105    procedure   ``addProvision``
120    procedure   ``addRequirement``
135    function    ``getTagValue``
148    function    ``getTagValue``
161    procedure   ``setTagValue``
171    procedure   ``setTagValue``
181    procedure   ``setTagValue``
201    function    ``getFileFlags``
221    procedure   ``setFileFlags``
251    function    ``getFileFlags``
290    procedure   ``setFileFlags``
====== =========== ==================================================

lib/scanfile.s7i
----------------

:Lines:    1779
:Entities: 42

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
41     procedure   ``skipComment``
80     function    ``getComment``
125    procedure   ``skipClassicComment``
148    procedure   ``skipLineComment``
170    function    ``getLineComment``
198    function    ``getDigits``
228    function    ``getHexDigits``
262    function    ``getInteger``
296    function    ``getNumber``
375    function    ``getNonDigits``
409    function    ``getQuotedText``
452    function    ``getSimpleStringLiteral``
488    procedure   ``getEscapeSequence``
527    function    ``getCharLiteral``
575    function    ``getStringLiteral``
625    function    ``getLetters``
657    function    ``getName``
684    procedure   ``skipSpace``
706    procedure   ``skipSpaceOrTab``
724    procedure   ``skipWhiteSpace``
739    procedure   ``skipWhiteSpace``
759    function    ``getWhiteSpace``
783    function    ``getWord``
817    function    ``getWord``
845    procedure   ``skipLine``
868    function    ``getLine``
903    function    ``getSymbolOrComment``
965    function    ``getSymbol``
1030   function    ``getSymbolWithHtmlEntities``
1109   function    ``getHtmlTagSymbolOrComment``
1187   procedure   ``skipXmlComment``
1226   function    ``getXmlTagOrContent``
1253   function    ``getXmlCharacterReference``
1312   function    ``getXmlCdataContent``
1389   function    ``getXmlTagHeadOrContent``
1480   function    ``getSymbolInXmlTag``
1529   procedure   ``skipXmlTag``
1545   procedure   ``skipXmlTag``
1589   procedure   ``getNextXmlAttribute``
1640   function    ``getHtmlAttributeValue``
1703   procedure   ``getNextHtmlAttribute``
1740   function    ``getSimpleSymbol``
====== =========== ==================================================

lib/scanjson.s7i
----------------

:Lines:    413
:Entities: 8

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
49     function    ``getJsonString``
53     set         ``special_stri_char``
114    function    ``getJsonString``
118    set         ``special_stri_char``
178    function    ``getJsonNumber``
247    function    ``getJsonNumber``
326    function    ``getJsonSymbol``
385    function    ``getJsonSymbol``
====== =========== ==================================================

lib/scanstri.s7i
----------------

:Lines:    1814
:Entities: 41

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
40     procedure   ``skipComment``
78     function    ``getComment``
119    procedure   ``skipClassicComment``
144    procedure   ``skipLineComment``
168    function    ``getLineComment``
197    function    ``getDigits``
226    function    ``getHexDigits``
259    function    ``getInteger``
294    function    ``getNumber``
358    function    ``getNonDigits``
390    function    ``getQuotedText``
433    function    ``getCommandLineWord``
495    function    ``getSimpleStringLiteral``
533    procedure   ``getEscapeSequence``
583    function    ``getCharLiteral``
631    function    ``getStringLiteral``
678    function    ``getCStringLiteralText``
791    function    ``getLetters``
821    function    ``getName``
850    procedure   ``skipSpace``
875    procedure   ``skipSpaceOrTab``
900    procedure   ``skipWhiteSpace``
927    function    ``getWhiteSpace``
958    function    ``getWord``
997    function    ``getWord``
1031   procedure   ``skipLine``
1056   function    ``getLine``
1087   function    ``getSymbolOrComment``
1160   function    ``getSymbol``
1230   procedure   ``skipXmlComment``
1270   function    ``getXmlTagOrContent``
1309   function    ``getXmlCdataContent``
1382   function    ``getXmlTagHeadOrContent``
1471   function    ``getSymbolInXmlTag``
1525   procedure   ``skipXmlTag``
1541   procedure   ``skipXmlTag``
1585   procedure   ``getNextXmlAttribute``
1636   function    ``getHtmlAttributeValue``
1709   procedure   ``getNextHtmlAttribute``
1747   function    ``getHttpSymbol``
1798   function    ``getValueOfHeaderAttribute``
====== =========== ==================================================

lib/scantoml.s7i
----------------

:Lines:    1603
:Entities: 59

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
33     set         ``tomlBareKeyChar``
36     function    ``getTomlUnicode4``
48     function    ``getTomlUnicode8``
64     function    ``getTomlBasicString``
68     set         ``illegalControlChar``
70     set         ``specialStriChar``
124    function    ``getTomlMultiLineBasicString``
128    set         ``illegalControlChar``
130    set         ``specialStriChar``
245    function    ``getTomlLiteralString``
249    set         ``illegalControlChar``
251    set         ``specialStriChar``
269    function    ``getTomlMultiLineLiteralString``
273    set         ``illegalControlChar``
275    set         ``specialStriChar``
310    function    ``getTomlString``
328    function    ``getTomlHexInteger``
358    function    ``getTomlOctInteger``
388    function    ``getTomlBinInteger``
418    function    ``getTomlDecInteger``
465    function    ``getTomlInteger``
497    function    ``getTomlFloat``
557    function    ``getTomlName``
578    function    ``getTomlMonth``
597    function    ``getTomlTwoDigits``
620    function    ``getTomlDate``
700    function    ``getTomlTime``
743    function    ``getTomlNumberOrDate``
771    function    ``getTomlSymbol``
797    procedure   ``skipTomlSpaceTabNlAndComments``
824    function    ``getTomlKey``
848    function    ``getTomlBasicString``
852    set         ``illegalControlChar``
854    set         ``specialStriChar``
905    function    ``getTomlMultiLineBasicString``
909    set         ``illegalControlChar``
911    set         ``specialStriChar``
1031   function    ``getTomlLiteralString``
1035   set         ``illegalControlChar``
1037   set         ``specialStriChar``
1055   function    ``getTomlMultiLineLiteralString``
1059   set         ``illegalControlChar``
1061   set         ``specialStriChar``
1105   function    ``getTomlString``
1123   function    ``getTomlHexInteger``
1148   function    ``getTomlOctInteger``
1173   function    ``getTomlBinInteger``
1198   function    ``getTomlDecInteger``
1240   function    ``getTomlInteger``
1267   function    ``getTomlFloat``
1325   function    ``getTomlName``
1340   function    ``getTomlMonth``
1361   function    ``getTomlTwoDigits``
1383   function    ``getTomlDate``
1475   function    ``getTomlTime``
1513   function    ``getTomlNumberOrDate``
1541   function    ``getTomlSymbol``
1561   procedure   ``skipTomlSpaceTabNlAndComments``
1588   function    ``getTomlKey``
====== =========== ==================================================

lib/seed7_05.s7i
----------------

:Lines:    1072
:Entities: 164

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
57     procedure   ``const``
58     procedure   ``var``
59     procedure   ``var``
60     procedure   ``syntax``
62     function    ``ref``
63     function    ``val``
64     function    ``val``
65     function    ``in``
66     function    ``in``
67     function    ``in``
68     function    ``in``
69     function    ``inout``
70     function    ``inout``
71     function    ``attr``
72     function    ``attr``
73     procedure   ``global``
80     procedure   ``:=``
83     function    ``func``
84     function    ``func``
85     function    ``func``
87     function    ``return``
88     function    ``return``
93     procedure   ``:=``
94     procedure   ``noop``
95     procedure   ``:=``
96     procedure   ``;``
99     procedure   ``PRINT``
101    procedure   ``IN_PARAM_IS_VALUE``
102    procedure   ``IN_PARAM_IS_REFERENCE``
114    procedure   ``BASIC_TYPE_DECLS``
118    procedure   ``TRACE``
126    procedure   ``:=``
127    procedure   ``:=``
138    function    ``func``
147    function    ``func``
156    function    ``func``
163    function    ``func``
170    function    ``return``
171    function    ``return``
172    function    ``return``
173    function    ``return``
175    function    ``return``
176    function    ``return``
177    function    ``return``
178    function    ``return``
204    procedure   ``var``
211    function    ``str``
212    function    ``gentype``
213    function    ``gensub``
215    function    ``newtype``
226    function    ``subtype``
236    function    ``func``
245    function    ``func``
254    function    ``func``
261    function    ``func``
268    function    ``return``
269    function    ``return``
270    function    ``return``
271    function    ``return``
309    procedure   ``:=``
315    procedure   ``:=``
344    procedure   ``raise``
345    function    ``str``
346    function    ``conv``
347    function    ``ord``
352    procedure   ``TRACE_OPTIONS``
353    procedure   ``TRACE_OBJ``
354    procedure   ``TRACE_PROC``
372    function    ``ord``
373    function    ``ACTION``
374    function    ``conv``
375    function    ``str``
376    function    ``=``
377    function    ``<>``
378    function    ``compare``
379    function    ``hashCode``
387    function    ``=``
388    function    ``<>``
389    function    ``compare``
390    function    ``hashCode``
391    function    ``isFunc``
392    function    ``isVarfunc``
393    function    ``resultType``
394    function    ``isDerived``
395    function    ``meta``
396    procedure   ``addInterface``
401    function    ``=``
402    function    ``<>``
419    procedure   ``if``
423    procedure   ``if``
428    procedure   ``if``
459    procedure   ``while``
460    procedure   ``while``
461    procedure   ``while``
463    procedure   ``while``
464    procedure   ``while``
465    procedure   ``while``
467    procedure   ``repeat``
468    procedure   ``repeat``
469    procedure   ``repeat``
471    procedure   ``repeat``
472    procedure   ``repeat``
473    procedure   ``repeat``
480    function    ``conv``
483    function    ``rand``
486    function    ``compare``
503    function    ``parse``
514    function    ``trimValue``
524    function    ``parse``
539    function    ``literal``
567    function    ``width``
579    function    ``reverse``
599    function    ``noCtrlChars``
619    function    ``tuple``
631    function    ``,``
632    function    ``,``
677    function    ``split``
678    function    ``split``
680    function    ``join``
693    function    ``join``
706    function    ``noEmptyStrings``
708    array       ``noEmptyStrings``
719    function    ``isDigitString``
733    function    ``isDigitString``
779    function    ``integer``
783    array       ``digitval``
823    function    ``sci``
865    function    ``new``
874    procedure   ``:=``
875    function    ``=``
876    function    ``<>``
880    function    ``sub``
889    procedure   ``:=``
890    function    ``=``
891    function    ``<>``
896    procedure   ``type_implements_interface``
899    procedure   ``:=``
900    function    ``conv``
901    function    ``toInterface``
903    function    ``conv``
904    function    ``conv``
909    function    ``create``
918    function    ``create``
929    procedure   ``CASE_DECLS``
940    procedure   ``case``
948    procedure   ``case``
951    procedure   ``case``
956    procedure   ``case``
959    procedure   ``case``
977    procedure   ``TRACE``
985    procedure   ``BLOCK_DECLS``
994    procedure   ``block``
997    procedure   ``block``
1001   procedure   ``block``
1012   function    ``succeeds``
1023   function    ``=``
1024   function    ``<>``
1030   structure   ``runError``
1036   procedure   ``GET_RUN_ERROR``
1043   function    ``getRunError``
1058   procedure   ``heapstat``
1059   function    ``heapsize``
1061   procedure   ``include``
1067   procedure   ``main``
====== =========== ==================================================

lib/set.s7i
-----------

:Lines:    57
:Entities: 1

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
38     function    ``set``
====== =========== ==================================================

lib/shell.s7i
-------------

:Lines:    615
:Entities: 25

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
55     function    ``shell``
78     function    ``shell``
109    function    ``shell``
118    array       ``parameterArray``
150    function    ``shell``
175    function    ``shell``
208    procedure   ``shellCmd``
236    procedure   ``shellCmd``
256    function    ``shellEscape``
275    function    ``toOsPath``
292    function    ``toShellPath``
296    function    ``shellParameters``
311    function    ``popenClibFile``
318    structure   ``popenFile``
347    function    ``popen``
391    function    ``popen``
397    array       ``parameterArray``
442    function    ``popen``
460    procedure   ``close``
469    structure   ``popen8File``
498    function    ``popen8``
542    function    ``popen8``
548    array       ``parameterArray``
593    function    ``popen8``
611    procedure   ``close``
====== =========== ==================================================

lib/showtls.s7i
---------------

:Lines:    678
:Entities: 19

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
32     procedure   ``showChangeCipherSpec``
39     procedure   ``showAlert``
83     procedure   ``showExtensions``
205    procedure   ``showClientHello``
249    procedure   ``showServerHello``
286    procedure   ``showX509Cert``
339    procedure   ``showCertificate``
346    array       ``certList``
382    procedure   ``showServerKeyExchange``
436    procedure   ``showCertificateRequest``
481    procedure   ``showServerHelloDone``
495    procedure   ``showCertificateVerify``
511    procedure   ``showClientKeyExchange``
527    procedure   ``showFinished``
547    procedure   ``showHandshakeMsg``
578    procedure   ``showHandshake``
600    procedure   ``showApplicationData``
614    procedure   ``showTlsMsg``
640    procedure   ``showTlsMsgType``
====== =========== ==================================================

lib/signature.s7i
-----------------

:Lines:    131
:Entities: 5

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
34     structure   ``rsaSignatureType``
40     function    ``genRsaSignature``
58     function    ``getRsaSignature``
92     function    ``genSignature``
116    function    ``verifySignature``
====== =========== ==================================================

lib/smtp.s7i
------------

:Lines:    261
:Entities: 13

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
37     structure   ``smtpConnection``
47     structure   ``smtpMessage``
49     array       ``toAddrs``
50     array       ``ccAddrs``
51     array       ``bccAddrs``
57     function    ``str``
79     procedure   ``smtpCommand``
86     procedure   ``smtpResponse``
111    procedure   ``close``
137    function    ``openSmtp``
186    procedure   ``login``
213    procedure   ``send``
256    procedure   ``send``
====== =========== ==================================================

lib/sockbase.s7i
----------------

:Lines:    217
:Entities: 38

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
39     procedure   ``:=``
41     function    ``_GENERATE_EMPTY_SOCKET_ADDRESS``
44     function    ``=``
45     function    ``<>``
47     function    ``addrFamily``
56     function    ``compare``
63     function    ``hashCode``
74     function    ``numericAddress``
77     function    ``service``
95     function    ``inetSocketAddress``
106    function    ``inetSocketAddress``
116    function    ``inetListenerAddress``
124    function    ``getHostname``
133    procedure   ``:=``
135    function    ``=``
136    function    ``<>``
138    function    ``_GENERATE_EMPTY_PRIMITIVE_SOCKET``
142    procedure   ``close``
143    function    ``localAddress``
144    function    ``peerAddress``
145    function    ``ord``
146    function    ``getc``
148    function    ``gets``
150    function    ``hasNext``
151    function    ``word_read``
153    function    ``line_read``
155    procedure   ``write``
156    function    ``recv``
158    function    ``recvfrom``
161    function    ``send``
163    function    ``sendto``
165    procedure   ``setSockOpt``
169    function    ``PRIMITIVE_SOCKET``
178    procedure   ``connect``
192    function    ``accept``
201    procedure   ``bind``
211    procedure   ``listen``
215    function    ``inputReady``
====== =========== ==================================================

lib/socket.s7i
--------------

:Lines:    326
:Entities: 38

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
46     procedure   ``close``
47     procedure   ``listen``
48     function    ``accept``
49     procedure   ``signOn``
50     procedure   ``signOff``
51     procedure   ``waitForRequest``
52     function    ``getExistingConnection``
53     function    ``getNewConnection``
56     structure   ``baseListener``
74     structure   ``socket``
84     function    ``localAddress``
85     function    ``peerAddress``
94     function    ``localAddress``
104    function    ``peerAddress``
108    function    ``service``
109    function    ``service``
112    function    ``port``
113    function    ``port``
116    function    ``ord``
117    function    ``ord``
121    function    ``inputReady``
122    function    ``inputReady``
133    function    ``openSocket``
166    function    ``openInetSocket``
182    function    ``openInetSocket``
193    procedure   ``close``
203    procedure   ``release``
206    procedure   ``release``
216    procedure   ``flush``
229    procedure   ``write``
243    procedure   ``writeln``
253    function    ``getc``
263    function    ``gets``
278    function    ``getwd``
291    function    ``getln``
303    function    ``eof``
314    function    ``hasNext``
324    function    ``inputReady``
====== =========== ==================================================

lib/sokoban1.s7i
----------------

:Lines:    1519
:Entities: 1

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
29     array       ``levels``
====== =========== ==================================================

lib/sql_base.s7i
----------------

:Lines:    1000
:Entities: 75

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
51     enumeration ``dbCategory``
57     function    ``str``
61     function    ``dbCategory``
100    function    ``parse``
117    procedure   ``:=``
119    function    ``_GENERATE_EMPTY_DATABASE``
133    function    ``=``
141    function    ``<>``
144    function    ``DRIVER_NUM``
147    function    ``DB_CATEGORY_NUM``
160    procedure   ``:=``
162    function    ``_GENERATE_EMPTY_STATEMENT``
176    function    ``=``
184    function    ``<>``
187    procedure   ``BIND_BIG_RAT``
190    procedure   ``BIND_TIME``
195    procedure   ``BIND_DURATION``
200    procedure   ``COLUMN_BIG_RAT``
203    procedure   ``COLUMN_TIME``
209    procedure   ``COLUMN_DURATION``
215    function    ``openDatabase``
219    function    ``openDatabase``
223    function    ``openDatabase``
227    function    ``openDatabase``
231    function    ``openDatabase``
235    function    ``openDatabase``
239    function    ``openDatabase``
243    function    ``openDatabase``
263    function    ``openDatabase``
282    function    ``openDatabase``
304    function    ``openDatabase``
310    function    ``openDatabase``
367    function    ``openDatabase``
462    function    ``openDatabase``
497    procedure   ``close``
508    function    ``prepare``
521    procedure   ``bind``
534    procedure   ``bind``
551    procedure   ``bind``
564    procedure   ``bind``
577    procedure   ``bind``
590    procedure   ``bind``
601    procedure   ``bind``
614    procedure   ``bind``
627    procedure   ``bind``
646    procedure   ``bind``
662    procedure   ``execute``
676    function    ``fetch``
693    function    ``column``
711    function    ``column``
736    function    ``column``
754    function    ``column``
772    function    ``column``
798    function    ``column``
816    function    ``column``
834    function    ``column``
852    function    ``column``
875    function    ``isNull``
881    function    ``getAutoCommit``
887    procedure   ``setAutoCommit``
893    procedure   ``commit``
899    procedure   ``rollback``
908    function    ``columnCount``
918    function    ``columnName``
921    procedure   ``execute``
930    function    ``libFunction``
931    function    ``dbFunction``
932    function    ``errCode``
933    function    ``errMessage``
936    function    ``driver``
940    function    ``dbCategory``
944    function    ``quoteTableNames``
961    function    ``tableNamesCommand``
983    function    ``getTableNames``
986    array       ``tableNames``
====== =========== ==================================================

lib/stars.s7i
-------------

:Lines:    1705
:Entities: 3

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
36     structure   ``starDescr``
44     function    ``genStarDescr``
60     array       ``stars``
====== =========== ==================================================

lib/stdfont10.s7i
-----------------

:Lines:    3347
:Entities: 1

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
32     function    ``genStdFont10``
====== =========== ==================================================

lib/stdfont12.s7i
-----------------

:Lines:    3928
:Entities: 1

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
32     function    ``genStdFont12``
====== =========== ==================================================

lib/stdfont14.s7i
-----------------

:Lines:    4510
:Entities: 1

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
32     function    ``genStdFont14``
====== =========== ==================================================

lib/stdfont16.s7i
-----------------

:Lines:    5092
:Entities: 1

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
32     function    ``genStdFont16``
====== =========== ==================================================

lib/stdfont18.s7i
-----------------

:Lines:    5868
:Entities: 1

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
32     function    ``genStdFont18``
====== =========== ==================================================

lib/stdfont20.s7i
-----------------

:Lines:    6449
:Entities: 1

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
32     function    ``genStdFont20``
====== =========== ==================================================

lib/stdfont24.s7i
-----------------

:Lines:    7421
:Entities: 1

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
32     function    ``genStdFont24``
====== =========== ==================================================

lib/stdfont8.s7i
----------------

:Lines:    2960
:Entities: 1

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
32     function    ``genStdFont8``
====== =========== ==================================================

lib/stdfont9.s7i
----------------

:Lines:    3152
:Entities: 1

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
32     function    ``genStdFont9``
====== =========== ==================================================

lib/stdio.s7i
-------------

:Lines:    192
:Entities: 9

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
35     function    ``INIT_STD_FILE``
94     procedure   ``write``
106    procedure   ``writeln``
118    procedure   ``writeln``
132    procedure   ``read``
147    procedure   ``read``
160    procedure   ``readln``
174    procedure   ``readln``
186    procedure   ``readln``
====== =========== ==================================================

lib/strifile.s7i
----------------

:Lines:    345
:Entities: 18

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
36     structure   ``striFile``
48     function    ``openStriFile``
63     function    ``openStriFile``
76     procedure   ``write``
96     procedure   ``moveLeft``
110    function    ``getc``
128    function    ``gets``
159    function    ``getTerminatedString``
189    function    ``getwd``
193    set         ``space_or_tab``
194    set         ``space_tab_or_nl``
239    function    ``getln``
269    function    ``eof``
279    function    ``hasNext``
288    function    ``length``
299    procedure   ``truncate``
327    procedure   ``seek``
343    function    ``tell``
====== =========== ==================================================

lib/string.s7i
--------------

:Lines:    779
:Entities: 55

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
31     procedure   ``:=``
49     function    ``mult``
56     function    ``lpad``
71     function    ``lpad0``
78     function    ``rpad``
87     function    ``&``
99     function    ``<&``
130    function    ``..``
170    function    ``..``
191    function    ``len``
214    function    ``fixLen``
223    procedure   ``&:=``
231    procedure   ``&:=``
247    procedure   ``@:=``
266    procedure   ``@:=``
274    function    ``=``
282    function    ``<>``
290    function    ``<``
298    function    ``>``
306    function    ``<=``
314    function    ``>=``
323    function    ``compare``
330    function    ``hashCode``
337    function    ``length``
348    function    ``startsWith``
360    function    ``endsWith``
372    function    ``equalAtIndex``
386    function    ``pos``
397    function    ``pos``
417    function    ``pos``
429    function    ``pos``
443    function    ``rpos``
454    function    ``rpos``
473    function    ``rpos``
485    function    ``rpos``
505    function    ``replace``
514    function    ``replace1``
543    function    ``replaceN``
593    function    ``replace2``
628    function    ``upper``
641    function    ``lower``
652    function    ``width``
660    function    ``reverse``
669    function    ``trim``
678    function    ``ltrim``
687    function    ``rtrim``
700    function    ``trimValue``
712    function    ``trimValue``
720    function    ``str``
731    function    ``literal``
734    function    ``c_literal``
737    function    ``getint``
753    function    ``gets``
766    function    ``string``
774    function    ``parse``
====== =========== ==================================================

lib/stritext.s7i
----------------

:Lines:    352
:Entities: 22

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
36     structure   ``striText``
37     array       ``content``
49     function    ``openStriText``
63     procedure   ``write``
80     procedure   ``writeln``
98     procedure   ``moveLeft``
117    function    ``getc``
141    function    ``gets``
176    function    ``getwd``
192    function    ``getln``
214    function    ``eof``
224    function    ``hasNext``
233    function    ``length``
245    procedure   ``seek``
257    function    ``tell``
274    function    ``height``
282    function    ``width``
300    function    ``line``
308    function    ``column``
315    procedure   ``setPos``
325    procedure   ``setLine``
334    procedure   ``setColumn``
====== =========== ==================================================

lib/struct.s7i
--------------

:Lines:    266
:Entities: 30

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
37     procedure   ``:=``
38     function    ``length``
39     function    ``&``
40     procedure   ``incl``
41     function    ``empty``
42     function    ``declare_elements``
53     function    ``new``
71     procedure   ``:=``
72     function    ``conv``
73     function    ``conv``
94     function    ``new``
105    procedure   ``:=``
106    function    ``conv``
107    function    ``conv``
124    function    ``new``
141    procedure   ``:=``
142    function    ``conv``
143    function    ``conv``
166    function    ``new``
176    procedure   ``:=``
177    function    ``conv``
178    function    ``conv``
201    function    ``sub``
218    procedure   ``:=``
219    function    ``conv``
220    function    ``conv``
249    function    ``sub``
259    procedure   ``:=``
260    function    ``conv``
261    function    ``conv``
====== =========== ==================================================

lib/struct_elem.s7i
-------------------

:Lines:    129
:Entities: 10

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
38     procedure   ``:=``
45     function    ``structElement``
58     function    ``=``
65     function    ``<>``
74     function    ``compare``
81     function    ``hashCode``
91     function    ``getName``
101    function    ``getType``
112    function    ``symb``
128    function    ``elements``
====== =========== ==================================================

lib/subfile.s7i
---------------

:Lines:    174
:Entities: 9

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
36     structure   ``subFile``
53     function    ``openSubFile``
72     function    ``getc``
91     function    ``gets``
118    function    ``eof``
128    function    ``hasNext``
137    function    ``length``
156    procedure   ``seek``
172    function    ``tell``
====== =========== ==================================================

lib/subrange.s7i
----------------

:Lines:    78
:Entities: 4

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
33     procedure   ``SUBRANGE_TYPES``
39     function    ``subrange``
64     function    ``conv``
67     function    ``conv``
====== =========== ==================================================

lib/syntax.s7i
--------------

:Lines:    294
:Entities: 0

(none)


lib/tar.s7i
-----------

:Lines:    1880
:Entities: 55

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
62     structure   ``tarHeader``
88     procedure   ``showHeader``
115    function    ``gets0``
130    function    ``gets0Spc``
150    function    ``getOct``
170    function    ``getMetaData``
189    procedure   ``puts0``
198    procedure   ``putSpc``
204    procedure   ``putOct``
210    function    ``tarChksum``
223    function    ``tarHeader``
247    function    ``readHeadBlock``
270    procedure   ``readHead``
357    function    ``readMinimumOfHeadBlock``
381    procedure   ``readMinimumOfHead``
449    function    ``str``
479    procedure   ``writeHead``
573    structure   ``tarArchive``
587    function    ``openTar``
633    function    ``openTar``
647    procedure   ``close``
653    function    ``addToCatalog``
667    function    ``addImplicitDir``
682    function    ``followSymlink``
725    function    ``followSymlink``
739    procedure   ``fixRegisterAndCatalog``
769    function    ``readDir``
780    function    ``readDir``
795    function    ``fileType``
858    function    ``fileTypeSL``
905    function    ``getFileMode``
925    procedure   ``setFileMode``
957    function    ``fileSize``
980    function    ``getMTime``
1003   procedure   ``setMTime``
1036   function    ``getOwner``
1063   procedure   ``setOwner``
1099   function    ``getGroup``
1126   procedure   ``setGroup``
1163   function    ``getFileMode``
1201   function    ``getMTime``
1240   procedure   ``setMTime``
1282   function    ``getOwner``
1323   procedure   ``setOwner``
1368   function    ``getGroup``
1409   procedure   ``setGroup``
1452   function    ``readLink``
1486   procedure   ``makeLink``
1621   function    ``getFile``
1649   procedure   ``putFile``
1733   procedure   ``makeDir``
1799   procedure   ``removeFile``
1841   procedure   ``for``
1851   function    ``openFileInTar``
1877   function    ``open``
====== =========== ==================================================

lib/tar_cmds.s7i
----------------

:Lines:    752
:Entities: 23

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
41     procedure   ``setUpHead``
65     function    ``openTarFileWithMagic``
117    function    ``openTarFileByExtension``
151    function    ``filePathIsInTarMemberList``
184    procedure   ``tarTell``
230    procedure   ``archiveTell``
234    array       ``nameList``
274    procedure   ``tarTell``
311    procedure   ``tarXtract``
318    array       ``dirHeaderList``
408    procedure   ``archiveXtract``
411    array       ``nameList``
414    array       ``dirPathList``
503    procedure   ``tarXtract``
536    procedure   ``tarXtract``
552    procedure   ``tarXtract``
576    procedure   ``tarCreate``
580    array       ``dirContent``
620    procedure   ``archiveCreate``
624    array       ``dirContent``
685    procedure   ``tarCreate``
732    procedure   ``tarCreate``
748    procedure   ``tarCreate``
====== =========== ==================================================

lib/tdes.s7i
------------

:Lines:    143
:Entities: 5

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
39     structure   ``tdesState``
65     function    ``setTdesKey``
85     function    ``setCipherKey``
94     function    ``encode``
122    function    ``decode``
====== =========== ==================================================

lib/tee.s7i
-----------

:Lines:    143
:Entities: 11

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
32     structure   ``teeFile``
33     array       ``destFiles``
42     function    ``openTee``
59     function    ``openTee``
67     procedure   ``write``
81     procedure   ``writeln``
95     procedure   ``flush``
105    procedure   ``moveLeft``
115    procedure   ``erase``
125    procedure   ``cursorOn``
135    procedure   ``cursorOff``
====== =========== ==================================================

lib/text.s7i
------------

:Lines:    135
:Entities: 19

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
49     function    ``height``
56     function    ``width``
63     function    ``line``
70     function    ``column``
78     procedure   ``clear``
85     procedure   ``clear``
88     procedure   ``v_scroll``
89     procedure   ``v_scroll``
91     procedure   ``h_scroll``
92     procedure   ``h_scroll``
99     procedure   ``color``
105    procedure   ``color``
111    procedure   ``setPos``
117    procedure   ``setPosXY``
123    procedure   ``setLine``
129    procedure   ``setColumn``
132    procedure   ``cursor``
133    procedure   ``box``
134    procedure   ``clear_box``
====== =========== ==================================================

lib/tga.s7i
-----------

:Lines:    676
:Entities: 26

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
52     structure   ``tgaHeader``
76     procedure   ``showHeader``
97     procedure   ``readPalette``
138    function    ``isTgaHeader``
151    procedure   ``readHeader``
199    procedure   ``readTgaColorMappedImageLine8``
212    procedure   ``readTgaColorMappedImage8``
232    procedure   ``readTgaUncompressedColorMapped``
253    procedure   ``readTgaTrueColorImageLine16``
270    procedure   ``readTgaTrueColorImage16``
290    procedure   ``readTgaTrueColorImageLine24``
305    procedure   ``readTgaTrueColorImage24``
325    procedure   ``readTgaTrueColorImageLine32``
340    procedure   ``readTgaTrueColorImage32``
360    procedure   ``readTgaUncompressedTrueColor``
397    procedure   ``readTgaGrayscaleImageLine8``
412    procedure   ``readTgaGrayscaleImage8``
432    procedure   ``readTgaGrayscaleImageLine16``
447    procedure   ``readTgaGrayscaleImage16``
467    procedure   ``readTgaUncompressedGrayscale``
496    function    ``fromTgaRunLengthEncoding``
528    procedure   ``readTgaRleColorMapped``
549    procedure   ``readTgaRleTrueColor``
586    procedure   ``readTgaRleGrayscale``
623    function    ``readTga``
664    function    ``readTga``
====== =========== ==================================================

lib/tiff.s7i
------------

:Lines:    2771
:Entities: 65

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
155    structure   ``tiffHeader``
165    array       ``bitsPerSample``
168    array       ``stripOffsets``
169    array       ``stripByteCounts``
170    array       ``tileOffsets``
171    array       ``tileByteCounts``
181    procedure   ``showHeader``
211    procedure   ``readHeader``
229    function    ``tagName``
299    function    ``fieldTypeName``
322    function    ``tagValueAsString``
426    function    ``tagValueAsArray``
429    array       ``tagValues``
540    procedure   ``readIDFEntry``
591    procedure   ``readImageFileDirectory``
623    procedure   ``readColorMap``
625    array       ``colorMapData``
646    procedure   ``differencingPredictor``
693    procedure   ``floatPredictor``
737    procedure   ``removeUnusedData``
758    procedure   ``processJpegSegments``
834    function    ``readJpeg``
919    procedure   ``setupJpegHeader``
946    procedure   ``loadJpegSegment``
956    procedure   ``assignDecoder``
960    array       ``numberOfCodesWithLength``
982    function    ``readOldJpeg``
1121   procedure   ``processCcittModifiedGroup3Fax``
1149   procedure   ``processCcittT6Fax``
1174   procedure   ``processCcittT4Fax1d``
1202   procedure   ``processCcittT4Fax2d``
1227   procedure   ``processRow``
1296   procedure   ``processRowWithColorMap``
1327   function    ``clampColor2``
1331   procedure   ``processRowWithGrayscale``
1419   procedure   ``processRowWithGrayscaleReversed``
1458   procedure   ``processRowWithGrayscaleAlpha``
1497   procedure   ``processRowWithGrayscaleAlphaReversed``
1536   procedure   ``processRowWithRGB``
1638   procedure   ``processRowWithRGBA``
1714   procedure   ``processRowWithCMYK``
1781   procedure   ``processRowWithCMYKA``
1848   procedure   ``processRowWithYCbCr``
1856   array       ``luminance``
1894   procedure   ``processRowWithYCbCr2``
1903   array       ``luminance``
1941   procedure   ``processRow``
2007   procedure   ``decompressStripOrTile``
2056   procedure   ``readThunderScan``
2066   array       ``twoBitDeltas``
2067   array       ``threeBitDeltas``
2140   procedure   ``readThunderScan``
2188   procedure   ``nextTile``
2211   procedure   ``readStripOrTile``
2331   function    ``readTiff``
2384   procedure   ``processRow``
2410   procedure   ``processRowRGB``
2467   procedure   ``processRowWithCMYK``
2509   procedure   ``processRow``
2524   procedure   ``readStripOrTile``
2534   array       ``stripData``
2537   array       ``stripDataStream``
2602   function    ``readTiffPlanarFormat``
2655   function    ``readTiff``
2759   function    ``readTiff``
====== =========== ==================================================

lib/time.s7i
------------

:Lines:    1191
:Entities: 65

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
43     structure   ``time``
56     procedure   ``GET_TIME``
61     procedure   ``AWAIT_TIME``
65     procedure   ``FROM_TIMESTAMP``
70     procedure   ``SET_LOCAL_TZ``
79     function    ``isLeapYear``
87     function    ``daysInYear``
103    function    ``daysInMonth``
107    set         ``monthsOfLength31``
129    function    ``daysInMonth``
137    function    ``strDate``
147    function    ``strTime``
160    function    ``strTimeZone``
179    function    ``str_yyyy_mm_dd``
185    function    ``str_yy_mm_dd``
191    function    ``str_mm_dd_yyyy``
197    function    ``str_mm_dd_yy``
203    function    ``str_dd_mm_yyyy``
209    function    ``str_dd_mm_yy``
215    function    ``str_d_m_yyyy``
221    function    ``str_d_m_yy``
227    function    ``str_hh_mm``
232    function    ``str_hh_mm_ss``
245    function    ``strDateTime``
262    function    ``str``
272    function    ``literal``
286    function    ``time``
386    function    ``parse``
397    function    ``=``
412    function    ``<>``
428    function    ``<=``
467    function    ``>=``
506    function    ``<``
515    function    ``>``
525    function    ``compare``
565    function    ``hashCode``
575    function    ``truncToSecond``
588    function    ``truncToMinute``
602    function    ``truncToHour``
617    function    ``truncToDay``
633    function    ``truncToMonth``
650    function    ``truncToYear``
668    function    ``dayOfWeek``
690    function    ``dayOfYear``
710    function    ``weekOfYear``
730    function    ``weekOfYear``
742    function    ``weekDateYear``
767    function    ``weekDateWeek``
781    procedure   ``NORMALIZE``
822    function    ``toUTC``
842    function    ``toLocalTZ``
867    function    ``setLocalTZ``
884    function    ``julianDayNumber``
910    function    ``julianDayNumToTime``
937    function    ``timestamp1970``
964    function    ``timestamp1970ToTime``
981    function    ``timestamp1601``
999    function    ``timestamp1601ToTime``
1068   function    ``rand``
1098   function    ``time``
1112   function    ``time``
1129   function    ``time``
1148   function    ``timeInTimeZone``
1166   function    ``date``
1182   procedure   ``await``
====== =========== ==================================================

lib/tls.s7i
-----------

:Lines:    2230
:Entities: 71

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
63     array       ``supportedCiphers``
84     enumeration ``keyExchangeAlgorithm``
88     structure   ``tlsParameters``
225    array       ``curveByNumber``
231    array       ``signatureHashByNumber``
243    array       ``signatureSchemes``
246    array       ``serverSignatureSchemesRsa``
250    array       ``serverSignatureSchemesEcdsa``
253    structure   ``tlsParseState``
268    structure   ``tlsFile``
275    structure   ``clientSession``
298    function    ``getEllipticCurveNumber``
322    procedure   ``storeCipherSuite``
395    procedure   ``computeMasterSecret``
413    procedure   ``storeKeys``
476    function    ``verifySignature``
516    function    ``genSignature``
545    procedure   ``validateCertificates``
595    procedure   ``processCertificate``
601    array       ``cert``
636    procedure   ``processClientCertificate``
642    array       ``cert``
668    procedure   ``processCertificateVerify``
683    procedure   ``processEllipticCurvesExtension``
704    procedure   ``processSignatureAlgorithmsExtension``
736    procedure   ``processClientExtensions``
760    procedure   ``processClientHello``
826    procedure   ``processServerHello``
867    procedure   ``processServerKeyExchange``
929    procedure   ``processCertificateRequest``
944    procedure   ``processServerHelloDone``
958    procedure   ``processClientKeyExchange``
1009   procedure   ``processChangeCipherSpec``
1016   procedure   ``processFinished``
1077   procedure   ``getTlsMsgRecord``
1116   procedure   ``loadCompleteHandshakeMsg``
1156   function    ``genExtension``
1161   function    ``serverNameExtension``
1166   function    ``genEllipticCurvesExtension``
1179   function    ``int16BeArrayExtension``
1192   function    ``genClientExtensions``
1207   function    ``genClientHello``
1241   function    ``genServerHello``
1272   function    ``genCertificate``
1298   function    ``genServerKeyExchange``
1337   function    ``genCertificateRequest``
1372   function    ``genServerHelloDone``
1389   function    ``genClientKeyExchange``
1439   function    ``genChangeCipherSpec``
1454   function    ``genFinished``
1507   function    ``genAlert``
1523   function    ``tlsEncryptRecord``
1580   function    ``tlsDecryptRecord``
1648   procedure   ``sendAlertAndClose``
1668   procedure   ``updateClientCache``
1682   function    ``negotiateSecurityParameters``
1769   function    ``openNewTlsSocket``
1798   function    ``openTlsSocket``
1887   function    ``openTlsSocket``
1909   function    ``openTlsSocket``
1923   function    ``openTlsSocket``
1942   function    ``openServerTls``
2065   procedure   ``close``
2079   function    ``eof``
2083   function    ``getApplicationData``
2119   procedure   ``write``
2145   procedure   ``writeln``
2158   function    ``gets``
2197   function    ``getln``
2225   function    ``getServerCertificate``
2228   function    ``getServerCertificate``
====== =========== ==================================================

lib/unicode.s7i
---------------

:Lines:    575
:Entities: 15

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
43     function    ``toUtf8``
61     function    ``fromUtf8``
71     function    ``toUtf16Be``
110    function    ``fromUtf16Be``
164    function    ``toUtf16Le``
203    function    ``fromUtf16Le``
264    function    ``replaceUtf16SurrogatePairs``
306    function    ``fromNullTerminatedUtf16Be``
334    function    ``fromNullTerminatedUtf16Le``
366    function    ``getNullTerminatedUtf16Be``
393    function    ``getNullTerminatedUtf16Be``
436    function    ``getNullTerminatedUtf16Le``
463    function    ``getNullTerminatedUtf16Le``
498    function    ``fromUtf7``
502    array       ``decode``
====== =========== ==================================================

lib/unionfnd.s7i
----------------

:Lines:    130
:Entities: 9

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
35     structure   ``unionfind``
36     array       ``items``
37     array       ``ranks``
42     procedure   ``init_unionfind``
53     function    ``num_groups``
60     function    ``rank``
77     function    ``group``
92     procedure   ``union``
116    function    ``is_connected``
====== =========== ==================================================

lib/upper.s7i
-------------

:Lines:    142
:Entities: 10

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
36     structure   ``upperFile``
66     function    ``openUpper``
81     procedure   ``write``
90     procedure   ``writeln``
100    procedure   ``writeln``
106    procedure   ``moveLeft``
112    procedure   ``erase``
118    procedure   ``cursorOn``
124    procedure   ``cursorOff``
136    function    ``gets``
====== =========== ==================================================

lib/utf16.s7i
-------------

:Lines:    540
:Entities: 19

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
40     structure   ``utf16File``
50     procedure   ``close``
60     procedure   ``flush``
72     function    ``eof``
81     function    ``hasNext``
94     function    ``length``
110    procedure   ``truncate``
122    function    ``seekable``
133    procedure   ``seek``
149    function    ``tell``
161    structure   ``utf16leFile``
203    function    ``openUtf16le``
227    procedure   ``write``
259    function    ``gets``
324    structure   ``utf16beFile``
366    function    ``openUtf16be``
390    procedure   ``write``
422    function    ``gets``
515    function    ``openUtf16``
====== =========== ==================================================

lib/utf8.s7i
------------

:Lines:    234
:Entities: 15

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
40     structure   ``utf8File``
47     function    ``utf8_getc``
48     function    ``utf8_gets``
50     function    ``utf8_word_read``
52     function    ``utf8_line_read``
54     procedure   ``utf8_write``
55     procedure   ``utf8_seek``
91     function    ``openUtf8``
111    procedure   ``write``
122    function    ``getc``
132    function    ``gets``
149    function    ``getwd``
165    function    ``getln``
181    procedure   ``seek``
190    function    ``INIT_STD_UTF8_FILE``
====== =========== ==================================================

lib/vecfont10.s7i
-----------------

:Lines:    1056
:Entities: 1

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
32     function    ``genVecFont10``
====== =========== ==================================================

lib/vecfont18.s7i
-----------------

:Lines:    1119
:Entities: 1

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
32     function    ``genVecFont18``
====== =========== ==================================================

lib/vector3d.s7i
----------------

:Lines:    293
:Entities: 22

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
37     structure   ``vector3d``
48     function    ``vector3d``
63     function    ``=``
72     function    ``<>``
80     function    ``-``
94     function    ``+``
108    function    ``-``
122    function    ``*``
136    function    ``/``
149    procedure   ``+:=``
160    procedure   ``-:=``
171    procedure   ``*:=``
182    procedure   ``/:=``
194    function    ``abs``
202    function    ``sqrAbs``
210    function    ``dot``
218    function    ``cross``
233    function    ``reflect``
241    function    ``unitVector``
260    function    ``compare``
278    function    ``hashCode``
288    function    ``str``
====== =========== ==================================================

lib/vectorfont.s7i
------------------

:Lines:    239
:Entities: 13

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
35     structure   ``charVectorType``
37     array       ``points``
47     structure   ``vectorFont``
58     function    ``width``
85     function    ``numOfCharsInWidth``
117    function    ``genPixmap``
149    function    ``genPixmapFont``
176    function    ``getFontCharPixmap``
191    procedure   ``setFont``
203    function    ``pline``
207    function    ``fillp``
211    procedure   ``incl``
227    function    ``columnWidth``
====== =========== ==================================================

lib/wildcard.s7i
----------------

:Lines:    140
:Entities: 5

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
40     function    ``wildcardMatch``
87     function    ``findMatchingFiles``
90     array       ``matchingFiles``
94     array       ``dirContent``
138    function    ``findMatchingFiles``
====== =========== ==================================================

lib/window.s7i
--------------

:Lines:    455
:Entities: 27

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
38     structure   ``window_file``
57     function    ``openWindow``
87     function    ``height``
95     function    ``width``
103    function    ``line``
111    function    ``column``
119    procedure   ``flush``
133    procedure   ``box``
172    procedure   ``clear_box``
219    procedure   ``clear``
235    procedure   ``clear``
246    procedure   ``v_scroll``
255    procedure   ``v_scroll``
267    procedure   ``h_scroll``
276    procedure   ``h_scroll``
288    procedure   ``color``
294    procedure   ``color``
303    procedure   ``setPos``
313    procedure   ``setLine``
322    procedure   ``setColumn``
331    procedure   ``write``
359    procedure   ``writeln``
373    procedure   ``moveLeft``
388    procedure   ``erase``
412    procedure   ``backSpace``
429    procedure   ``cursorOn``
443    procedure   ``cursorOff``
====== =========== ==================================================

lib/wrinum.s7i
--------------

:Lines:    248
:Entities: 17

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
33     function    ``str``
38     function    ``str_0_to_9``
71     function    ``str``
75     array       ``STR_0_TO_19``
81     array       ``STR_20_TO_90``
85     function    ``str_99``
99     function    ``str_1E3``
117    function    ``str_1E6``
135    function    ``str_1E9``
160    function    ``str``
164    array       ``STR_0_TO_19``
170    array       ``STR_0_TO_9``
174    array       ``STR_20_TO_90``
178    function    ``str_99``
193    function    ``str_1E3``
211    function    ``str_1E6``
229    function    ``str_1E9``
====== =========== ==================================================

lib/x509cert.s7i
----------------

:Lines:    1243
:Entities: 51

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
43     structure   ``x509Validity``
48     structure   ``subjectPublicKeyInfoType``
56     structure   ``x509Extension``
62     structure   ``tbsCertificateType``
70     array       ``extensions``
83     structure   ``x509Cert``
95     structure   ``certAndKey``
96     array       ``certList``
113    function    ``getRsaKey``
137    function    ``genX509RsaKey``
147    function    ``getEcdsaSignature``
176    function    ``getEllipticCurveFromOid``
200    function    ``getEllipticCurveOid``
224    function    ``getEllipticCurve``
283    function    ``getPublicKeyInfo``
334    function    ``genX509PublicKeyInfo``
355    function    ``getX509Name``
394    function    ``x509Name``
418    function    ``genX509Name``
436    function    ``getTime_yymmddhhmmssZ``
480    function    ``getTime_yyyymmddhhmmssfffZ``
494    function    ``x509Validity``
503    function    ``getX509Validity``
531    function    ``genX509Validity``
543    function    ``getTbsCertificate``
579    function    ``getPkcs7SignedDataCert``
642    function    ``getTbsCertificate``
662    function    ``toAsn1``
675    function    ``genX509TbsCertificate``
702    function    ``getDigestOidFromAlgorithm``
716    function    ``getDigestAlgorithm``
734    function    ``getDigestFromSignatureAlgorithm``
755    function    ``getDigestOidFromSignatureAlgorithm``
776    procedure   ``showSignatureAlgorithm``
804    function    ``getX509Cert``
834    function    ``genX509Cert``
859    function    ``genX509Cert``
890    function    ``validateSignature``
937    function    ``createX509Cert``
967    function    ``createX509Cert``
1003   function    ``createX509Cert``
1035   function    ``createX509Cert``
1073   function    ``x509KeyUsage``
1091   function    ``x509BasicConstraints``
1106   function    ``x509BasicConstraints``
1127   procedure   ``addExtension``
1135   function    ``certAndKey``
1144   function    ``certAndKey``
1163   function    ``selfSignedX509Cert``
1189   function    ``selfSignedX509Cert``
1218   function    ``selfSignedX509Cert``
====== =========== ==================================================

lib/xml_ent.s7i
---------------

:Lines:    94
:Entities: 2

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
31     function    ``genPredeclaredXmlEntities``
50     function    ``decodeXmlEntities``
====== =========== ==================================================

lib/xmldom.s7i
--------------

:Lines:    303
:Entities: 25

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
48     function    ``getAttrValue``
54     function    ``getAttributes``
60     function    ``getSubNodes``
66     function    ``getContent``
69     procedure   ``writeXml``
70     function    ``.``
74     procedure   ``writeXml``
83     structure   ``xmlBaseNode``
91     function    ``getSubNodes``
94     procedure   ``writeXml``
107    structure   ``xmlText``
114    function    ``getContent``
117    procedure   ``writeXml``
137    structure   ``xmlElement``
145    function    ``getAttrValue``
148    function    ``getAttributes``
151    procedure   ``writeXml``
168    structure   ``xmlContainer``
169    array       ``subNodes``
175    function    ``getSubNodes``
178    procedure   ``writeXml``
202    function    ``readXmlNode``
264    function    ``readXml``
284    function    ``readXml``
295    procedure   ``for``
====== =========== ==================================================

lib/xz.s7i
----------

:Lines:    442
:Entities: 18

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
41     structure   ``xzFilterFlags``
47     function    ``xzFilterFlags``
63     function    ``xzDictionarySize``
79     function    ``xzPacket``
147    structure   ``xzBlockHeader``
152    array       ``filterFlags``
156    function    ``readXzBlockHeader``
198    procedure   ``readXzBlockHeader``
237    function    ``xzDecompress``
269    structure   ``xzFile``
287    function    ``openXzFile``
320    function    ``getc``
342    function    ``gets``
372    function    ``eof``
381    function    ``hasNext``
398    function    ``length``
424    procedure   ``seek``
440    function    ``tell``
====== =========== ==================================================

lib/zip.s7i
-----------

:Lines:    2792
:Entities: 91

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
104    function    ``rposOfMagic``
129    function    ``getExtraFieldMap``
157    function    ``extraFieldFromMap``
171    procedure   ``writeExtraField``
197    structure   ``local_file_header``
216    procedure   ``write``
235    procedure   ``considerZip64ExtraField``
253    function    ``get_local_header``
300    function    ``str``
320    procedure   ``writeHead``
326    structure   ``zip64_end_of_central_dir_locator``
334    procedure   ``write``
343    function    ``get_zip64_end_of_central_dir_locator``
361    structure   ``zip64_end_of_central_directory``
376    procedure   ``write``
392    function    ``get_zip64_end_of_central_directory``
419    structure   ``end_of_central_directory``
432    procedure   ``write``
446    function    ``get_end_of_central_directory``
470    function    ``str``
486    function    ``readEndOfCentralDir``
543    structure   ``central_file_header``
569    procedure   ``write``
596    procedure   ``considerZip64ExtraField``
624    function    ``get_central_header``
680    function    ``str``
707    procedure   ``writeHead``
713    function    ``getCentralHeaderFilePath``
749    function    ``toLocalHeader``
768    procedure   ``updateLocalHeader``
781    procedure   ``initLastModFileTime``
802    procedure   ``assignLastModFileTime``
833    procedure   ``assignLastModFileTime``
864    procedure   ``assignUserId``
886    procedure   ``assignUserId``
908    procedure   ``assignGroupId``
932    procedure   ``assignGroupId``
972    structure   ``zipArchive``
991    function    ``openZip``
1042   function    ``openZip``
1053   function    ``openZip``
1070   function    ``openZip``
1077   procedure   ``close``
1083   function    ``addToCatalog``
1093   function    ``addImplicitDir``
1105   function    ``isRegularFile``
1121   function    ``isSymlink``
1127   function    ``followSymlink``
1178   function    ``followSymlink``
1192   procedure   ``fixRegisterAndCatalog``
1219   procedure   ``initializeFileReferenceMap``
1245   function    ``getReferencePaths``
1248   array       ``filePathList``
1275   function    ``readDir``
1286   function    ``readDir``
1301   function    ``fileType``
1379   function    ``fileTypeSL``
1430   function    ``getFileMode``
1484   procedure   ``setFileMode``
1513   function    ``fileSize``
1535   function    ``getMTime``
1588   procedure   ``setMTime``
1620   function    ``getOwner``
1663   procedure   ``setOwner``
1701   function    ``getGroup``
1746   procedure   ``setGroup``
1785   function    ``getFileMode``
1823   function    ``getMTime``
1891   procedure   ``setMTime``
1936   function    ``getOwner``
1993   procedure   ``setOwner``
2044   function    ``getGroup``
2103   procedure   ``setGroup``
2152   function    ``readLink``
2211   procedure   ``makeLink``
2272   function    ``lzwDecompressShrink``
2286   function    ``getFile``
2289   function    ``getReference``
2321   function    ``crc32``
2325   procedure   ``updateKeys``
2334   procedure   ``initializeEncryptionKeys``
2347   function    ``decryptByte``
2358   function    ``decrypt``
2373   function    ``openEncryptFile``
2397   procedure   ``readDataDescriptor``
2445   function    ``getFile``
2515   procedure   ``putFile``
2636   procedure   ``makeDir``
2713   procedure   ``removeFile``
2767   function    ``getZipContent``
2784   procedure   ``for``
====== =========== ==================================================

lib/zstd.s7i
------------

:Lines:    1263
:Entities: 69

====== =========== ==================================================
 Line   Type        Name
====== =========== ==================================================
36     structure   ``zstdFrameHeader``
44     function    ``zstdWindowSize``
57     procedure   ``readFrameHeader``
123    structure   ``fseWeightsType``
128    structure   ``fseTableEntry``
135    structure   ``fseDecodingType``
137    array       ``decodingTable``
141    function    ``getFseCompressedHuffmanWeights``
200    function    ``buildDecodingTable``
204    array       ``nextStateOfSymbol``
266    function    ``repeatingFseDecodingTable``
277    procedure   ``symbolTranslation``
293    function    ``peekSymbol``
297    procedure   ``nextState``
309    function    ``getAdditionalBits``
313    function    ``decodeInterleavedFseStreams``
316    array       ``weights``
319    array       ``fseState``
351    structure   ``zstdHuffmanDecoder``
353    array       ``numberOfBits``
354    array       ``symbols``
358    function    ``createZstdHuffmanDecoder``
368    array       ``rankIdx``
400    function    ``createZstdHuffmanDecoder``
411    array       ``numBits``
412    array       ``rankCount``
454    function    ``readZstdHuffmanTreeWeights``
456    array       ``weights``
492    function    ``decodeSymbol``
507    function    ``decodeStream``
537    structure   ``zstdLiteralSectionHeader``
545    function    ``readZstdLiteralsSectionHeader``
603    structure   ``zstdSequencesSectionType``
610    structure   ``zstdBlockStateType``
611    array       ``offsetHistory``
617    function    ``readCompressedLiteralsBlock``
624    array       ``weights``
625    array       ``streamSize``
626    array       ``compressedStream``
662    function    ``readLiteralsSection``
685    structure   ``zstdSequencesSectionHeader``
693    function    ``readZstdSequencesSectionHeader``
721    structure   ``zstdSequenceState``
727    structure   ``zstdSequenceType``
733    array       ``zstdLiteralLengthBaseValueTranslation``
741    array       ``zstdLiteralLengthExtraBits``
746    array       ``zstdMatchLengthBaseValueTranslation``
753    array       ``zstdMatchLengthsExtraBits``
759    function    ``buildZstdLiteralLengthsTable``
764    array       ``literalLengthDefaultDistributions``
782    function    ``buildZstdOffsetTable``
787    array       ``offsetDefaultDistribution``
802    function    ``buildZstdMatchLengthsTable``
807    array       ``matchLengthDefaultDistribution``
825    procedure   ``initDecodeTables``
888    function    ``decodeSequence``
918    procedure   ``decodeSequences``
1012   procedure   ``readCompressedBlock``
1035   function    ``zstdBlock``
1071   function    ``zstdDecompress``
1095   structure   ``zstdFile``
1113   function    ``openZstdFile``
1141   function    ``getc``
1163   function    ``gets``
1193   function    ``eof``
1202   function    ``hasNext``
1219   function    ``length``
1245   procedure   ``seek``
1261   function    ``tell``
====== =========== ==================================================


.. ---------------------------------------------------------------------------
