{-# OPTIONS_GHC -w #-}
module Parse where
import Data.Char
import Data.List
import Common
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.11

data HappyAbsSyn t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 t22
	| HappyAbsSyn23 t23
	| HappyAbsSyn24 t24
	| HappyAbsSyn25 t25
	| HappyAbsSyn26 t26
	| HappyAbsSyn27 t27
	| HappyAbsSyn28 t28
	| HappyAbsSyn29 t29
	| HappyAbsSyn30 t30
	| HappyAbsSyn31 t31
	| HappyAbsSyn32 t32
	| HappyAbsSyn33 t33
	| HappyAbsSyn34 t34

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,260) ([0,0,924,996,0,49152,16441,62,0,39936,58371,3,0,14784,15936,0,0,0,0,0,0,0,0,0,7168,4132,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,49152,1,0,0,7168,0,0,0,448,0,0,0,28,996,0,49152,16385,62,0,7168,58368,3,0,448,15936,0,0,28,996,0,49152,16385,62,0,0,0,0,0,0,0,0,0,28,1012,0,0,0,0,0,24576,4096,0,0,0,0,0,0,0,0,0,0,32768,1,0,0,512,0,0,0,32,0,0,0,2,0,0,70,64,0,0,0,0,0,0,0,0,0,0,0,0,49152,16385,62,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7168,0,0,0,320,0,0,0,28,996,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,16,0,0,64,0,0,0,264,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,32768,16,0,0,0,0,0,0,0,0,0,3072,0,0,0,448,0,0,0,0,0,0,32768,4,64,0,5120,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,264,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,1,0,0,0,0,0,23552,16,4,0,192,0,0,0,28,0,0,32768,4,64,0,0,0,0,0,32768,8,0,0,0,0,0,0,1024,0,0,0,0,0,0,32768,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,23552,16,4,0,448,2,0,0,28,0,0,49152,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,2176,0,0,0,0,0,0,448,0,0,0,0,1,0,49152,1,0,0,0,1024,0,0,448,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,4172,1024,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,7168,0,0,0,0,16,0,0,0,0,0,49152,261,64,0,0,256,0,0,448,0,0,0,0,0,0,49152,261,64,0,0,256,0,0,448,0,0,0,0,8,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_buildUpAPs","%start_buildUpAP","MultipleComm","SingleComm","CommDef","CommExp","GI","N","T","TransGs","TransG","Exp","Ap","AP","S","Sigma","Gamma","Transs","Trans","IS","Ac","Symbol","SymbolV","CadenaPila","CadenasGra","CadenaV","Cadena","Terminals","CadenaNonTerm","ListaCadena","NaturalsZ","Naturals","SYMBOLU","SYMBOLL","STRING","NAT","NATUNIT","DEFA","DEFG","DEFE","';'","'|'","'/'","'_'","'->'","'-'","','","'='","'('","')'","UNION","KS","KP","POT","REVERSE","CONCAT","ZERO","%eof"]
        bit_start = st * 60
        bit_end = (st + 1) * 60
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..59]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (35) = happyShift action_10
action_0 (36) = happyShift action_11
action_0 (37) = happyShift action_12
action_0 (40) = happyShift action_13
action_0 (41) = happyShift action_14
action_0 (42) = happyShift action_15
action_0 (51) = happyShift action_16
action_0 (54) = happyShift action_17
action_0 (55) = happyShift action_18
action_0 (56) = happyShift action_19
action_0 (57) = happyShift action_20
action_0 (58) = happyShift action_21
action_0 (5) = happyGoto action_23
action_0 (6) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 (8) = happyGoto action_5
action_0 (14) = happyGoto action_6
action_0 (15) = happyGoto action_7
action_0 (24) = happyGoto action_8
action_0 (29) = happyGoto action_9
action_0 _ = happyReduce_3

action_1 (35) = happyShift action_10
action_1 (36) = happyShift action_11
action_1 (37) = happyShift action_12
action_1 (40) = happyShift action_13
action_1 (41) = happyShift action_14
action_1 (42) = happyShift action_15
action_1 (51) = happyShift action_16
action_1 (54) = happyShift action_17
action_1 (55) = happyShift action_18
action_1 (56) = happyShift action_19
action_1 (57) = happyShift action_20
action_1 (58) = happyShift action_21
action_1 (6) = happyGoto action_22
action_1 (7) = happyGoto action_4
action_1 (8) = happyGoto action_5
action_1 (14) = happyGoto action_6
action_1 (15) = happyGoto action_7
action_1 (24) = happyGoto action_8
action_1 (29) = happyGoto action_9
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (35) = happyShift action_10
action_2 (36) = happyShift action_11
action_2 (37) = happyShift action_12
action_2 (40) = happyShift action_13
action_2 (41) = happyShift action_14
action_2 (42) = happyShift action_15
action_2 (51) = happyShift action_16
action_2 (54) = happyShift action_17
action_2 (55) = happyShift action_18
action_2 (56) = happyShift action_19
action_2 (57) = happyShift action_20
action_2 (58) = happyShift action_21
action_2 (6) = happyGoto action_3
action_2 (7) = happyGoto action_4
action_2 (8) = happyGoto action_5
action_2 (14) = happyGoto action_6
action_2 (15) = happyGoto action_7
action_2 (24) = happyGoto action_8
action_2 (29) = happyGoto action_9
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (35) = happyShift action_10
action_3 (36) = happyShift action_11
action_3 (37) = happyShift action_12
action_3 (40) = happyShift action_13
action_3 (41) = happyShift action_14
action_3 (42) = happyShift action_15
action_3 (51) = happyShift action_16
action_3 (54) = happyShift action_17
action_3 (55) = happyShift action_18
action_3 (56) = happyShift action_19
action_3 (57) = happyShift action_20
action_3 (58) = happyShift action_21
action_3 (5) = happyGoto action_38
action_3 (6) = happyGoto action_3
action_3 (7) = happyGoto action_4
action_3 (8) = happyGoto action_5
action_3 (14) = happyGoto action_6
action_3 (15) = happyGoto action_7
action_3 (24) = happyGoto action_8
action_3 (29) = happyGoto action_9
action_3 _ = happyReduce_3

action_4 _ = happyReduce_5

action_5 _ = happyReduce_4

action_6 (35) = happyShift action_10
action_6 (36) = happyShift action_11
action_6 (37) = happyShift action_12
action_6 (43) = happyShift action_35
action_6 (46) = happyShift action_36
action_6 (53) = happyShift action_37
action_6 (24) = happyGoto action_8
action_6 (28) = happyGoto action_33
action_6 (29) = happyGoto action_34
action_6 _ = happyFail (happyExpListPerState 6)

action_7 _ = happyReduce_26

action_8 _ = happyReduce_59

action_9 _ = happyReduce_28

action_10 _ = happyReduce_44

action_11 _ = happyReduce_45

action_12 _ = happyReduce_58

action_13 (35) = happyShift action_10
action_13 (36) = happyShift action_11
action_13 (37) = happyShift action_12
action_13 (24) = happyGoto action_8
action_13 (29) = happyGoto action_32
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (35) = happyShift action_10
action_14 (36) = happyShift action_11
action_14 (37) = happyShift action_12
action_14 (24) = happyGoto action_8
action_14 (29) = happyGoto action_31
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (35) = happyShift action_10
action_15 (36) = happyShift action_11
action_15 (37) = happyShift action_12
action_15 (24) = happyGoto action_8
action_15 (29) = happyGoto action_30
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (35) = happyShift action_10
action_16 (36) = happyShift action_11
action_16 (37) = happyShift action_12
action_16 (51) = happyShift action_16
action_16 (54) = happyShift action_17
action_16 (55) = happyShift action_18
action_16 (56) = happyShift action_19
action_16 (57) = happyShift action_20
action_16 (58) = happyShift action_21
action_16 (14) = happyGoto action_29
action_16 (15) = happyGoto action_7
action_16 (24) = happyGoto action_8
action_16 (29) = happyGoto action_9
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (35) = happyShift action_10
action_17 (36) = happyShift action_11
action_17 (37) = happyShift action_12
action_17 (51) = happyShift action_16
action_17 (54) = happyShift action_17
action_17 (55) = happyShift action_18
action_17 (56) = happyShift action_19
action_17 (57) = happyShift action_20
action_17 (58) = happyShift action_21
action_17 (14) = happyGoto action_28
action_17 (15) = happyGoto action_7
action_17 (24) = happyGoto action_8
action_17 (29) = happyGoto action_9
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (35) = happyShift action_10
action_18 (36) = happyShift action_11
action_18 (37) = happyShift action_12
action_18 (51) = happyShift action_16
action_18 (54) = happyShift action_17
action_18 (55) = happyShift action_18
action_18 (56) = happyShift action_19
action_18 (57) = happyShift action_20
action_18 (58) = happyShift action_21
action_18 (14) = happyGoto action_27
action_18 (15) = happyGoto action_7
action_18 (24) = happyGoto action_8
action_18 (29) = happyGoto action_9
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (35) = happyShift action_10
action_19 (36) = happyShift action_11
action_19 (37) = happyShift action_12
action_19 (51) = happyShift action_16
action_19 (54) = happyShift action_17
action_19 (55) = happyShift action_18
action_19 (56) = happyShift action_19
action_19 (57) = happyShift action_20
action_19 (58) = happyShift action_21
action_19 (14) = happyGoto action_26
action_19 (15) = happyGoto action_7
action_19 (24) = happyGoto action_8
action_19 (29) = happyGoto action_9
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (35) = happyShift action_10
action_20 (36) = happyShift action_11
action_20 (37) = happyShift action_12
action_20 (51) = happyShift action_16
action_20 (54) = happyShift action_17
action_20 (55) = happyShift action_18
action_20 (56) = happyShift action_19
action_20 (57) = happyShift action_20
action_20 (58) = happyShift action_21
action_20 (14) = happyGoto action_25
action_20 (15) = happyGoto action_7
action_20 (24) = happyGoto action_8
action_20 (29) = happyGoto action_9
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (35) = happyShift action_10
action_21 (36) = happyShift action_11
action_21 (37) = happyShift action_12
action_21 (51) = happyShift action_16
action_21 (54) = happyShift action_17
action_21 (55) = happyShift action_18
action_21 (56) = happyShift action_19
action_21 (57) = happyShift action_20
action_21 (58) = happyShift action_21
action_21 (14) = happyGoto action_24
action_21 (15) = happyGoto action_7
action_21 (24) = happyGoto action_8
action_21 (29) = happyGoto action_9
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (60) = happyAccept
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (60) = happyAccept
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (35) = happyShift action_10
action_24 (36) = happyShift action_11
action_24 (37) = happyShift action_12
action_24 (51) = happyShift action_16
action_24 (53) = happyShift action_37
action_24 (54) = happyShift action_17
action_24 (55) = happyShift action_18
action_24 (56) = happyShift action_19
action_24 (57) = happyShift action_20
action_24 (58) = happyShift action_21
action_24 (14) = happyGoto action_51
action_24 (15) = happyGoto action_7
action_24 (24) = happyGoto action_8
action_24 (29) = happyGoto action_9
action_24 _ = happyFail (happyExpListPerState 24)

action_25 _ = happyReduce_25

action_26 (38) = happyShift action_42
action_26 (39) = happyShift action_43
action_26 (53) = happyShift action_37
action_26 (34) = happyGoto action_50
action_26 _ = happyFail (happyExpListPerState 26)

action_27 _ = happyReduce_22

action_28 _ = happyReduce_21

action_29 (52) = happyShift action_49
action_29 (53) = happyShift action_37
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (50) = happyShift action_48
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (50) = happyShift action_47
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (50) = happyShift action_46
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (38) = happyShift action_42
action_33 (39) = happyShift action_43
action_33 (43) = happyShift action_44
action_33 (59) = happyShift action_45
action_33 (33) = happyGoto action_40
action_33 (34) = happyGoto action_41
action_33 _ = happyFail (happyExpListPerState 33)

action_34 _ = happyReduce_57

action_35 _ = happyReduce_11

action_36 _ = happyReduce_56

action_37 (35) = happyShift action_10
action_37 (36) = happyShift action_11
action_37 (37) = happyShift action_12
action_37 (51) = happyShift action_16
action_37 (54) = happyShift action_17
action_37 (55) = happyShift action_18
action_37 (56) = happyShift action_19
action_37 (57) = happyShift action_20
action_37 (58) = happyShift action_21
action_37 (14) = happyGoto action_39
action_37 (15) = happyGoto action_7
action_37 (24) = happyGoto action_8
action_37 (29) = happyGoto action_9
action_37 _ = happyFail (happyExpListPerState 37)

action_38 _ = happyReduce_2

action_39 _ = happyReduce_20

action_40 (43) = happyShift action_61
action_40 _ = happyFail (happyExpListPerState 40)

action_41 _ = happyReduce_68

action_42 _ = happyReduce_70

action_43 _ = happyReduce_69

action_44 _ = happyReduce_10

action_45 _ = happyReduce_67

action_46 (35) = happyShift action_10
action_46 (36) = happyShift action_11
action_46 (37) = happyShift action_12
action_46 (16) = happyGoto action_58
action_46 (17) = happyGoto action_59
action_46 (24) = happyGoto action_8
action_46 (29) = happyGoto action_60
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (35) = happyShift action_56
action_47 (37) = happyShift action_57
action_47 (9) = happyGoto action_53
action_47 (10) = happyGoto action_54
action_47 (31) = happyGoto action_55
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (35) = happyShift action_10
action_48 (36) = happyShift action_11
action_48 (37) = happyShift action_12
action_48 (51) = happyShift action_16
action_48 (54) = happyShift action_17
action_48 (55) = happyShift action_18
action_48 (56) = happyShift action_19
action_48 (57) = happyShift action_20
action_48 (58) = happyShift action_21
action_48 (14) = happyGoto action_52
action_48 (15) = happyGoto action_7
action_48 (24) = happyGoto action_8
action_48 (29) = happyGoto action_9
action_48 _ = happyFail (happyExpListPerState 48)

action_49 _ = happyReduce_27

action_50 _ = happyReduce_24

action_51 _ = happyReduce_23

action_52 (43) = happyShift action_68
action_52 (53) = happyShift action_37
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (43) = happyShift action_67
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (44) = happyShift action_65
action_54 (49) = happyShift action_66
action_54 _ = happyFail (happyExpListPerState 54)

action_55 _ = happyReduce_14

action_56 _ = happyReduce_64

action_57 _ = happyReduce_63

action_58 (43) = happyShift action_64
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (44) = happyShift action_62
action_59 (49) = happyShift action_63
action_59 _ = happyFail (happyExpListPerState 59)

action_60 _ = happyReduce_31

action_61 _ = happyReduce_9

action_62 (35) = happyShift action_10
action_62 (36) = happyShift action_11
action_62 (18) = happyGoto action_76
action_62 (24) = happyGoto action_77
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (35) = happyShift action_10
action_63 (36) = happyShift action_11
action_63 (37) = happyShift action_12
action_63 (24) = happyGoto action_8
action_63 (29) = happyGoto action_75
action_63 _ = happyFail (happyExpListPerState 63)

action_64 _ = happyReduce_6

action_65 (36) = happyShift action_72
action_65 (39) = happyShift action_73
action_65 (59) = happyShift action_74
action_65 (11) = happyGoto action_70
action_65 (30) = happyGoto action_71
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (35) = happyShift action_56
action_66 (37) = happyShift action_57
action_66 (31) = happyGoto action_69
action_66 _ = happyFail (happyExpListPerState 66)

action_67 _ = happyReduce_8

action_68 _ = happyReduce_7

action_69 _ = happyReduce_13

action_70 (44) = happyShift action_80
action_70 (49) = happyShift action_81
action_70 _ = happyFail (happyExpListPerState 70)

action_71 _ = happyReduce_16

action_72 _ = happyReduce_60

action_73 _ = happyReduce_61

action_74 _ = happyReduce_62

action_75 _ = happyReduce_30

action_76 (44) = happyShift action_78
action_76 (49) = happyShift action_79
action_76 _ = happyFail (happyExpListPerState 76)

action_77 _ = happyReduce_33

action_78 (35) = happyShift action_10
action_78 (36) = happyShift action_11
action_78 (37) = happyShift action_12
action_78 (39) = happyShift action_90
action_78 (45) = happyShift action_91
action_78 (59) = happyShift action_92
action_78 (19) = happyGoto action_87
action_78 (24) = happyGoto action_8
action_78 (26) = happyGoto action_88
action_78 (29) = happyGoto action_89
action_78 _ = happyReduce_36

action_79 (35) = happyShift action_10
action_79 (36) = happyShift action_11
action_79 (24) = happyGoto action_86
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (35) = happyShift action_10
action_80 (36) = happyShift action_11
action_80 (37) = happyShift action_12
action_80 (12) = happyGoto action_83
action_80 (13) = happyGoto action_84
action_80 (24) = happyGoto action_8
action_80 (29) = happyGoto action_85
action_80 _ = happyFail (happyExpListPerState 80)

action_81 (36) = happyShift action_72
action_81 (39) = happyShift action_73
action_81 (59) = happyShift action_74
action_81 (30) = happyGoto action_82
action_81 _ = happyFail (happyExpListPerState 81)

action_82 _ = happyReduce_15

action_83 (44) = happyShift action_96
action_83 (48) = happyShift action_97
action_83 _ = happyFail (happyExpListPerState 83)

action_84 _ = happyReduce_18

action_85 (47) = happyShift action_95
action_85 _ = happyFail (happyExpListPerState 85)

action_86 _ = happyReduce_32

action_87 (44) = happyShift action_93
action_87 (49) = happyShift action_94
action_87 _ = happyFail (happyExpListPerState 87)

action_88 _ = happyReduce_35

action_89 _ = happyReduce_50

action_90 _ = happyReduce_52

action_91 _ = happyReduce_51

action_92 _ = happyReduce_53

action_93 (51) = happyShift action_107
action_93 (20) = happyGoto action_105
action_93 (21) = happyGoto action_106
action_93 _ = happyReduce_39

action_94 (35) = happyShift action_10
action_94 (36) = happyShift action_11
action_94 (37) = happyShift action_12
action_94 (39) = happyShift action_90
action_94 (45) = happyShift action_91
action_94 (59) = happyShift action_92
action_94 (24) = happyGoto action_8
action_94 (26) = happyGoto action_104
action_94 (29) = happyGoto action_89
action_94 _ = happyFail (happyExpListPerState 94)

action_95 (35) = happyShift action_10
action_95 (36) = happyShift action_11
action_95 (37) = happyShift action_12
action_95 (46) = happyShift action_103
action_95 (24) = happyGoto action_8
action_95 (27) = happyGoto action_100
action_95 (29) = happyGoto action_101
action_95 (32) = happyGoto action_102
action_95 _ = happyFail (happyExpListPerState 95)

action_96 (35) = happyShift action_10
action_96 (36) = happyShift action_11
action_96 (37) = happyShift action_12
action_96 (24) = happyGoto action_8
action_96 (29) = happyGoto action_99
action_96 _ = happyFail (happyExpListPerState 96)

action_97 (35) = happyShift action_10
action_97 (36) = happyShift action_11
action_97 (37) = happyShift action_12
action_97 (13) = happyGoto action_98
action_97 (24) = happyGoto action_8
action_97 (29) = happyGoto action_85
action_97 _ = happyFail (happyExpListPerState 97)

action_98 _ = happyReduce_17

action_99 _ = happyReduce_12

action_100 _ = happyReduce_19

action_101 _ = happyReduce_66

action_102 (49) = happyShift action_111
action_102 _ = happyReduce_55

action_103 _ = happyReduce_54

action_104 _ = happyReduce_34

action_105 (44) = happyShift action_109
action_105 (48) = happyShift action_110
action_105 _ = happyFail (happyExpListPerState 105)

action_106 _ = happyReduce_38

action_107 (35) = happyShift action_10
action_107 (36) = happyShift action_11
action_107 (37) = happyShift action_12
action_107 (24) = happyGoto action_8
action_107 (29) = happyGoto action_108
action_107 _ = happyFail (happyExpListPerState 107)

action_108 (49) = happyShift action_116
action_108 _ = happyFail (happyExpListPerState 108)

action_109 (35) = happyShift action_10
action_109 (36) = happyShift action_11
action_109 (37) = happyShift action_12
action_109 (22) = happyGoto action_114
action_109 (24) = happyGoto action_8
action_109 (29) = happyGoto action_115
action_109 _ = happyFail (happyExpListPerState 109)

action_110 (51) = happyShift action_107
action_110 (21) = happyGoto action_113
action_110 _ = happyFail (happyExpListPerState 110)

action_111 (35) = happyShift action_10
action_111 (36) = happyShift action_11
action_111 (37) = happyShift action_12
action_111 (24) = happyGoto action_8
action_111 (29) = happyGoto action_112
action_111 _ = happyFail (happyExpListPerState 111)

action_112 _ = happyReduce_65

action_113 _ = happyReduce_37

action_114 (44) = happyShift action_122
action_114 _ = happyFail (happyExpListPerState 114)

action_115 _ = happyReduce_41

action_116 (35) = happyShift action_10
action_116 (36) = happyShift action_11
action_116 (39) = happyShift action_119
action_116 (45) = happyShift action_120
action_116 (59) = happyShift action_121
action_116 (24) = happyGoto action_117
action_116 (25) = happyGoto action_118
action_116 _ = happyFail (happyExpListPerState 116)

action_117 _ = happyReduce_47

action_118 (49) = happyShift action_125
action_118 _ = happyFail (happyExpListPerState 118)

action_119 _ = happyReduce_48

action_120 _ = happyReduce_46

action_121 _ = happyReduce_49

action_122 (35) = happyShift action_10
action_122 (36) = happyShift action_11
action_122 (37) = happyShift action_12
action_122 (23) = happyGoto action_123
action_122 (24) = happyGoto action_8
action_122 (29) = happyGoto action_124
action_122 _ = happyFail (happyExpListPerState 122)

action_123 (49) = happyShift action_127
action_123 _ = happyReduce_29

action_124 _ = happyReduce_43

action_125 (35) = happyShift action_10
action_125 (36) = happyShift action_11
action_125 (37) = happyShift action_12
action_125 (39) = happyShift action_90
action_125 (45) = happyShift action_91
action_125 (59) = happyShift action_92
action_125 (24) = happyGoto action_8
action_125 (26) = happyGoto action_126
action_125 (29) = happyGoto action_89
action_125 _ = happyFail (happyExpListPerState 125)

action_126 (49) = happyShift action_129
action_126 _ = happyFail (happyExpListPerState 126)

action_127 (35) = happyShift action_10
action_127 (36) = happyShift action_11
action_127 (37) = happyShift action_12
action_127 (24) = happyGoto action_8
action_127 (29) = happyGoto action_128
action_127 _ = happyFail (happyExpListPerState 127)

action_128 _ = happyReduce_42

action_129 (35) = happyShift action_10
action_129 (36) = happyShift action_11
action_129 (37) = happyShift action_12
action_129 (39) = happyShift action_90
action_129 (45) = happyShift action_91
action_129 (59) = happyShift action_92
action_129 (24) = happyGoto action_8
action_129 (26) = happyGoto action_130
action_129 (29) = happyGoto action_89
action_129 _ = happyFail (happyExpListPerState 129)

action_130 (49) = happyShift action_131
action_130 _ = happyFail (happyExpListPerState 130)

action_131 (35) = happyShift action_10
action_131 (36) = happyShift action_11
action_131 (37) = happyShift action_12
action_131 (24) = happyGoto action_8
action_131 (29) = happyGoto action_132
action_131 _ = happyFail (happyExpListPerState 131)

action_132 (52) = happyShift action_133
action_132 _ = happyFail (happyExpListPerState 132)

action_133 _ = happyReduce_40

happyReduce_2 = happySpecReduce_2  5 happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1 : happy_var_2
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_0  5 happyReduction_3
happyReduction_3  =  HappyAbsSyn5
		 ([]
	)

happyReduce_4 = happySpecReduce_1  6 happyReduction_4
happyReduction_4 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  6 happyReduction_5
happyReduction_5 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happyReduce 5 7 happyReduction_6
happyReduction_6 (_ `HappyStk`
	(HappyAbsSyn16  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn29  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (DefAp (AP happy_var_2 happy_var_4)
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 5 7 happyReduction_7
happyReduction_7 (_ `HappyStk`
	(HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn29  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (DefExp happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_8 = happyReduce 5 7 happyReduction_8
happyReduction_8 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn29  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (DefG (GI happy_var_2 happy_var_4)
	) `HappyStk` happyRest

happyReduce_9 = happyReduce 4 8 happyReduction_9
happyReduction_9 (_ `HappyStk`
	(HappyAbsSyn33  happy_var_3) `HappyStk`
	(HappyAbsSyn28  happy_var_2) `HappyStk`
	(HappyAbsSyn14  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (ApEval happy_var_1 happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_10 = happySpecReduce_3  8 happyReduction_10
happyReduction_10 _
	(HappyAbsSyn28  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn8
		 (ApEval happy_var_1 happy_var_2 4096
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_2  8 happyReduction_11
happyReduction_11 _
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn8
		 (ApExp happy_var_1
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happyReduce 7 9 happyReduction_12
happyReduction_12 ((HappyAbsSyn29  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Gi happy_var_1 happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_13 = happySpecReduce_3  10 happyReduction_13
happyReduction_13 (HappyAbsSyn31  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_3 : happy_var_1
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  10 happyReduction_14
happyReduction_14 (HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn10
		 ([happy_var_1]
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  11 happyReduction_15
happyReduction_15 (HappyAbsSyn30  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_3 : happy_var_1
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  11 happyReduction_16
happyReduction_16 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  12 happyReduction_17
happyReduction_17 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_3 : happy_var_1
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  12 happyReduction_18
happyReduction_18 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 ([happy_var_1]
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  13 happyReduction_19
happyReduction_19 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn13
		 (R happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  14 happyReduction_20
happyReduction_20 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (OpUnion happy_var_1 happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_2  14 happyReduction_21
happyReduction_21 (HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (OpKs happy_var_2
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_2  14 happyReduction_22
happyReduction_22 (HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (OpKp happy_var_2
	)
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  14 happyReduction_23
happyReduction_23 (HappyAbsSyn14  happy_var_3)
	(HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (OpConcat happy_var_2 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  14 happyReduction_24
happyReduction_24 (HappyAbsSyn34  happy_var_3)
	(HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (OpPot happy_var_2 happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_2  14 happyReduction_25
happyReduction_25 (HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (OpReverse happy_var_2
	)
happyReduction_25 _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  14 happyReduction_26
happyReduction_26 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  15 happyReduction_27
happyReduction_27 _
	(HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (happy_var_2
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  15 happyReduction_28
happyReduction_28 (HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn15
		 (Auto happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happyReduce 11 16 happyReduction_29
happyReduction_29 ((HappyAbsSyn23  happy_var_11) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_9) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (Ap happy_var_1 happy_var_3 happy_var_5 happy_var_7 happy_var_9 happy_var_11
	) `HappyStk` happyRest

happyReduce_30 = happySpecReduce_3  17 happyReduction_30
happyReduction_30 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_3 : happy_var_1
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  17 happyReduction_31
happyReduction_31 (HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn17
		 ([happy_var_1]
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  18 happyReduction_32
happyReduction_32 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_3 : happy_var_1
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  18 happyReduction_33
happyReduction_33 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn18
		 ([happy_var_1]
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  19 happyReduction_34
happyReduction_34 (HappyAbsSyn26  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_3 : happy_var_1
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  19 happyReduction_35
happyReduction_35 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn19
		 ([happy_var_1]
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_0  19 happyReduction_36
happyReduction_36  =  HappyAbsSyn19
		 ([]
	)

happyReduce_37 = happySpecReduce_3  20 happyReduction_37
happyReduction_37 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_3 : happy_var_1
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  20 happyReduction_38
happyReduction_38 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn20
		 ([happy_var_1]
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_0  20 happyReduction_39
happyReduction_39  =  HappyAbsSyn20
		 ([]
	)

happyReduce_40 = happyReduce 11 21 happyReduction_40
happyReduction_40 (_ `HappyStk`
	(HappyAbsSyn29  happy_var_10) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn26  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn26  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn29  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (T happy_var_2 happy_var_4 happy_var_6 happy_var_8 happy_var_10
	) `HappyStk` happyRest

happyReduce_41 = happySpecReduce_1  22 happyReduction_41
happyReduction_41 (HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn22
		 (IS happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  23 happyReduction_42
happyReduction_42 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_3 : happy_var_1
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  23 happyReduction_43
happyReduction_43 (HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn23
		 ([happy_var_1]
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  24 happyReduction_44
happyReduction_44 (HappyTerminal (TokenSymU happy_var_1))
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  24 happyReduction_45
happyReduction_45 (HappyTerminal (TokenSymL happy_var_1))
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  25 happyReduction_46
happyReduction_46 _
	 =  HappyAbsSyn25
		 ('/'
	)

happyReduce_47 = happySpecReduce_1  25 happyReduction_47
happyReduction_47 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1
	)
happyReduction_47 _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  25 happyReduction_48
happyReduction_48 (HappyTerminal (TokenNUnit happy_var_1))
	 =  HappyAbsSyn25
		 (head (show happy_var_1)
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  25 happyReduction_49
happyReduction_49 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn25
		 (head (show happy_var_1)
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_1  26 happyReduction_50
happyReduction_50 (HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_1  26 happyReduction_51
happyReduction_51 _
	 =  HappyAbsSyn26
		 ("/"
	)

happyReduce_52 = happySpecReduce_1  26 happyReduction_52
happyReduction_52 (HappyTerminal (TokenNUnit happy_var_1))
	 =  HappyAbsSyn26
		 (show happy_var_1
	)
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_1  26 happyReduction_53
happyReduction_53 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn26
		 (show happy_var_1
	)
happyReduction_53 _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_1  27 happyReduction_54
happyReduction_54 _
	 =  HappyAbsSyn27
		 ([]
	)

happyReduce_55 = happySpecReduce_1  27 happyReduction_55
happyReduction_55 (HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_1
	)
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  28 happyReduction_56
happyReduction_56 _
	 =  HappyAbsSyn28
		 (""
	)

happyReduce_57 = happySpecReduce_1  28 happyReduction_57
happyReduction_57 (HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn28
		 (happy_var_1
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_1  29 happyReduction_58
happyReduction_58 (HappyTerminal (TokenSTRING happy_var_1))
	 =  HappyAbsSyn29
		 (happy_var_1
	)
happyReduction_58 _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_1  29 happyReduction_59
happyReduction_59 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn29
		 ([happy_var_1]
	)
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_1  30 happyReduction_60
happyReduction_60 (HappyTerminal (TokenSymL happy_var_1))
	 =  HappyAbsSyn30
		 (happy_var_1:[]
	)
happyReduction_60 _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_1  30 happyReduction_61
happyReduction_61 (HappyTerminal (TokenNUnit happy_var_1))
	 =  HappyAbsSyn30
		 ((show happy_var_1)
	)
happyReduction_61 _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_1  30 happyReduction_62
happyReduction_62 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn30
		 ((show happy_var_1)
	)
happyReduction_62 _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_1  31 happyReduction_63
happyReduction_63 (HappyTerminal (TokenSTRING happy_var_1))
	 =  HappyAbsSyn31
		 (happy_var_1
	)
happyReduction_63 _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_1  31 happyReduction_64
happyReduction_64 (HappyTerminal (TokenSymU happy_var_1))
	 =  HappyAbsSyn31
		 ([happy_var_1]
	)
happyReduction_64 _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_3  32 happyReduction_65
happyReduction_65 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn32
		 (happy_var_3 : happy_var_1
	)
happyReduction_65 _ _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_1  32 happyReduction_66
happyReduction_66 (HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn32
		 ([happy_var_1]
	)
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_1  33 happyReduction_67
happyReduction_67 _
	 =  HappyAbsSyn33
		 (0
	)

happyReduce_68 = happySpecReduce_1  33 happyReduction_68
happyReduction_68 (HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn33
		 (happy_var_1
	)
happyReduction_68 _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_1  34 happyReduction_69
happyReduction_69 (HappyTerminal (TokenNUnit happy_var_1))
	 =  HappyAbsSyn34
		 (happy_var_1
	)
happyReduction_69 _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_1  34 happyReduction_70
happyReduction_70 (HappyTerminal (TokenNAT happy_var_1))
	 =  HappyAbsSyn34
		 (happy_var_1
	)
happyReduction_70 _  = notHappyAtAll 

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	TEOF -> action 60 60 tk (HappyState action) sts stk;
	TokenSymU happy_dollar_dollar -> cont 35;
	TokenSymL happy_dollar_dollar -> cont 36;
	TokenSTRING happy_dollar_dollar -> cont 37;
	TokenNAT happy_dollar_dollar -> cont 38;
	TokenNUnit happy_dollar_dollar -> cont 39;
	TokenDefA -> cont 40;
	TokenDefG -> cont 41;
	TokenDefE -> cont 42;
	TokenT -> cont 43;
	TokenDiv -> cont 44;
	TokenBar -> cont 45;
	TokenDown -> cont 46;
	TokenArrow -> cont 47;
	TokenMinus -> cont 48;
	TokenComma -> cont 49;
	TokenEqual -> cont 50;
	TokenOB -> cont 51;
	TokenCB -> cont 52;
	TokenU -> cont 53;
	TokenKs -> cont 54;
	TokenKp -> cont 55;
	TokenPot -> cont 56;
	TokenReverse -> cont 57;
	TokenConc -> cont 58;
	TokenZero -> cont 59;
	_ -> happyError' (tk, [])
	})

happyError_ explist 60 tk = happyError' (tk, explist)
happyError_ explist _ tk = happyError' (tk, explist)

happyThen :: () => P a -> (a -> P b) -> P b
happyThen = (thenP)
happyReturn :: () => a -> P a
happyReturn = (returnP)
happyThen1 :: () => P a -> (a -> P b) -> P b
happyThen1 = happyThen
happyReturn1 :: () => a -> P a
happyReturn1 = happyReturn
happyError' :: () => ((Token), [String]) -> P a
happyError' tk = (\(tokens, explist) -> happyError) tk
buildUpAPs = happySomeParser where
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn5 z -> happyReturn z; _other -> notHappyAtAll })

buildUpAP = happySomeParser where
 happySomeParser = happyThen (happyParse action_1) (\x -> case x of {HappyAbsSyn6 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


---VER EL TEMA DE LA PRECEDENCIA QUE CREO QUE ES AL PEDO


data ParseResult a = Ok a | Failed String  deriving Show

type LineNumber = Int

type P a = String -> LineNumber -> ParseResult a

getLineNo :: P LineNumber
getLineNo = \s l -> Ok l

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s l-> case m s l of
                         Ok a     -> k a s l
                         Failed e -> Failed e

returnP :: a -> P a
returnP a = \s l-> Ok a

failP :: String -> P a
failP err = \s l -> Failed err

catchP :: P a -> (String -> P a) -> P a
catchP m k = \s l -> case m s l of
                        Ok a     -> Ok a
                        Failed e -> k e s l
                        
--parseError :: Token -> P a
--parseError t = getLineNo `thenP` \line -> failP ("Línea "++ show line ++ ": parse error" ) 

happyError :: P a
happyError = \ s i -> Failed $ "Línea "++(show (i::LineNumber))++": Error de parseo\n"++(s)  --no se que onda con esto ajaj

data Token
      = TokenSymU Char
      | TokenSymL Char 
      | TokenSTRING String
      | TokenNAT Int
      | TokenPlus
      | TokenMinus
      | TokenComma 
      | TokenOB
      | TokenCB
      | TokenDefA 
      | TokenDefE 
      | TokenDefG 
      | TokenEqual
      | TokenU
      | TokenKs
      | TokenKp
      | TokenReverse
      | TokenDiv
      | TokenConc
      | TEOF
      | TokenArrow
      | TokenT
      | TokenBar
      | TokenPot 
      | TokenDown
      | TokenZero
      | TokenNUnit Int
      
 deriving Show


lexer :: (Token -> P a) -> P a
lexer cont s = case s of
		[] -> cont TEOF []
		('\n':s)  ->  \line -> lexer cont s (line + 1)
		(c:cs)
      		      | isSpace c -> lexer cont cs
      		      | isAlpha c -> lexS (c:cs)
      		      | isDigit c -> lexD (c:cs)
      		('-':('-':cs)) -> lexer cont $ dropWhile ((/=) '\n') cs
      		('{':('-':cs)) -> comentarios 0 0 cont cs
      		('-':('}':cs)) -> \ line -> Failed $ "Línea "++(show line)++": Comentario no abierto"
      		('(':cs) -> cont TokenOB cs
      		(')':cs) -> cont TokenCB cs
      		( ',':cs) -> cont TokenComma cs
      		( '-':('>':cs)) -> cont TokenArrow cs
      		( '-':cs) -> cont TokenMinus cs
      		( '*':cs) -> cont TokenKs cs
      		( ';':cs) -> cont TokenT cs
      		( '_':cs) -> cont TokenDown cs
      		( '/':cs) -> cont TokenBar cs
      		( '+':cs) -> cont TokenKp cs
      		( '|':cs) -> cont TokenDiv cs
      		('=':cs) -> cont TokenEqual cs
      		unknown -> \line -> Failed $
                     "Línea "++(show line)++": No se puede reconocer "++(show $ take 10 unknown)++ "..." 
               where lexD cs = let (number,rest) = span isDigit cs
                               in if (read number :: Int) < 10
                               then if (read number :: Int) == 0
                                    then cont TokenZero rest
                                    else cont (TokenNUnit (read number :: Int )) rest
                               else cont (TokenNAT (read number :: Int )) rest
                     lexS cs = case span isAlpha cs of
                                ("defA",rest)     -> cont TokenDefA rest
                                ("defE",rest)     -> cont TokenDefE rest
                                ("defG",rest)     -> cont TokenDefG rest
                                ("Union",rest)     -> cont TokenU rest   
                                ("Potency",rest)     -> cont TokenPot rest
                                ("Concat",rest)     -> cont TokenConc rest
                                ("Reverse",rest)     -> cont TokenReverse rest
                                (something,rest) -> if (length something) > 1 
                                                     then cont (TokenSTRING something) rest
                                                     else if isUpper (head something)
                                                          then cont (TokenSymU (head something)) rest
                                                          else cont (TokenSymL (head something)) rest
                     comentarios anidado cl cont s = case s of
                                ('-':('-':cs)) -> comentarios anidado cl cont $ dropWhile ((/=) '\n') cs
                                ('{':('-':cs)) -> comentarios (anidado+1) cl cont cs
                                ('-':('}':cs)) -> case anidado of
                                                  0 -> \line -> lexer cont cs (line+cl)
                                                  _ -> comentarios (anidado-1) cl cont cs
                                ('\n':cs) -> comentarios anidado (cl+1) cont cs
                                (_:cs) -> comentarios anidado cl cont cs

		   
buildUpAPs' s = buildUpAPs s 1
buildUpAP' s = buildUpAP s 1
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}







# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4











































{-# LINE 7 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}















{-# LINE 7 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc8336_0/ghc_2.h" #-}
































































































































































































{-# LINE 7 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 









{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList







{-# LINE 65 "templates/GenericTemplate.hs" #-}

{-# LINE 75 "templates/GenericTemplate.hs" #-}

{-# LINE 84 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 137 "templates/GenericTemplate.hs" #-}

{-# LINE 147 "templates/GenericTemplate.hs" #-}
indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 267 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 333 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
