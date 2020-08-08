{-# LANGUAGE Safe #-}

module SDL2.Enumerations where


import Data.Word (Word32)

newtype SDLEventType = SDLEventType { unSDLEventType :: Word32 }
    deriving (Eq)
newtype SDLKeycode = SDLKeycode { unSDLKeycode :: Word32 }
    deriving (Eq)
newtype SDLScancode = SDLScancode { unSDLScancode :: Word32 }
    deriving (Eq)

-- SDLEventType
sdlFirstEvent :: SDLEventType
sdlFirstEvent = SDLEventType 0
sdlQuit :: SDLEventType
sdlQuit = SDLEventType 256
sdlWindowEvent :: SDLEventType
sdlWindowEvent = SDLEventType 512
sdlMouseMotion :: SDLEventType
sdlMouseMotion = SDLEventType 1024

-- SDLScancode
sdlScancodeUnknown :: SDLScancode
sdlScancodeUnknown = SDLScancode 0
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 1
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 2
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 3
sdlScancodeA :: SDLScancode
sdlScancodeA = SDLScancode 4
sdlScancodeB :: SDLScancode
sdlScancodeB = SDLScancode 5
sdlScancodeC :: SDLScancode
sdlScancodeC = SDLScancode 6
sdlScancodeD :: SDLScancode
sdlScancodeD = SDLScancode 7
sdlScancodeE :: SDLScancode
sdlScancodeE = SDLScancode 8
sdlScancodeF :: SDLScancode
sdlScancodeF = SDLScancode 9
sdlScancodeG :: SDLScancode
sdlScancodeG = SDLScancode 10
sdlScancodeH :: SDLScancode
sdlScancodeH = SDLScancode 11
sdlScancodeI :: SDLScancode
sdlScancodeI = SDLScancode 12
sdlScancodeJ :: SDLScancode
sdlScancodeJ = SDLScancode 13
sdlScancodeK :: SDLScancode
sdlScancodeK = SDLScancode 14
sdlScancodeL :: SDLScancode
sdlScancodeL = SDLScancode 15
sdlScancodeM :: SDLScancode
sdlScancodeM = SDLScancode 16
sdlScancodeN :: SDLScancode
sdlScancodeN = SDLScancode 17
sdlScancodeO :: SDLScancode
sdlScancodeO = SDLScancode 18
sdlScancodeP :: SDLScancode
sdlScancodeP = SDLScancode 19
sdlScancodeQ :: SDLScancode
sdlScancodeQ = SDLScancode 20
sdlScancodeR :: SDLScancode
sdlScancodeR = SDLScancode 21
sdlScancodeS :: SDLScancode
sdlScancodeS = SDLScancode 22
sdlScancodeT :: SDLScancode
sdlScancodeT = SDLScancode 23
sdlScancodeU :: SDLScancode
sdlScancodeU = SDLScancode 24
sdlScancodeV :: SDLScancode
sdlScancodeV = SDLScancode 25
sdlScancodeW :: SDLScancode
sdlScancodeW = SDLScancode 26
sdlScancodeX :: SDLScancode
sdlScancodeX = SDLScancode 27
sdlScancodeY :: SDLScancode
sdlScancodeY = SDLScancode 28
sdlScancodeZ :: SDLScancode
sdlScancodeZ = SDLScancode 29
sdlScancode1 :: SDLScancode
sdlScancode1 = SDLScancode 30
sdlScancode2 :: SDLScancode
sdlScancode2 = SDLScancode 31
sdlScancode3 :: SDLScancode
sdlScancode3 = SDLScancode 32
sdlScancode4 :: SDLScancode
sdlScancode4 = SDLScancode 33
sdlScancode5 :: SDLScancode
sdlScancode5 = SDLScancode 34
sdlScancode6 :: SDLScancode
sdlScancode6 = SDLScancode 35
sdlScancode7 :: SDLScancode
sdlScancode7 = SDLScancode 36
sdlScancode8 :: SDLScancode
sdlScancode8 = SDLScancode 37
sdlScancode9 :: SDLScancode
sdlScancode9 = SDLScancode 38
sdlScancode0 :: SDLScancode
sdlScancode0 = SDLScancode 39
sdlScancodeReturn :: SDLScancode
sdlScancodeReturn = SDLScancode 40
sdlScancodeEscape :: SDLScancode
sdlScancodeEscape = SDLScancode 41
sdlScancodeBackspace :: SDLScancode
sdlScancodeBackspace = SDLScancode 42
sdlScancodeTab :: SDLScancode
sdlScancodeTab = SDLScancode 43
sdlScancodeSpace :: SDLScancode
sdlScancodeSpace = SDLScancode 44
sdlScancodeMinus :: SDLScancode
sdlScancodeMinus = SDLScancode 45
sdlScancodeEquals :: SDLScancode
sdlScancodeEquals = SDLScancode 46
sdlScancodeLeftBracket :: SDLScancode
sdlScancodeLeftBracket = SDLScancode 47
sdlScancodeRightBracket :: SDLScancode
sdlScancodeRightBracket = SDLScancode 48
sdlScancodeBackSlash :: SDLScancode
sdlScancodeBackSlash = SDLScancode 49
sdlScancodeNonUSHash :: SDLScancode
sdlScancodeNonUSHash = SDLScancode 50
sdlScancodeSemiColon :: SDLScancode
sdlScancodeSemiColon = SDLScancode 51
sdlScancodeApostrophe :: SDLScancode
sdlScancodeApostrophe = SDLScancode 52
sdlScancodeGrave :: SDLScancode
sdlScancodeGrave = SDLScancode 53
sdlScancodeComma :: SDLScancode
sdlScancodeComma = SDLScancode 54
sdlScancodePeriod :: SDLScancode
sdlScancodePeriod = SDLScancode 55
sdlScancodeSlash :: SDLScancode
sdlScancodeSlash = SDLScancode 56
sdlScancodeCapsLock :: SDLScancode
sdlScancodeCapsLock = SDLScancode 57
sdlScancodeF1 :: SDLScancode
sdlScancodeF1 = SDLScancode 58
sdlScancodeF2 :: SDLScancode
sdlScancodeF2 = SDLScancode 59
sdlScancodeF3 :: SDLScancode
sdlScancodeF3 = SDLScancode 60
sdlScancodeF4 :: SDLScancode
sdlScancodeF4 = SDLScancode 61
sdlScancodeF5 :: SDLScancode
sdlScancodeF5 = SDLScancode 62
sdlScancodeF6 :: SDLScancode
sdlScancodeF6 = SDLScancode 63
sdlScancodeF7 :: SDLScancode
sdlScancodeF7 = SDLScancode 64
sdlScancodeF8 :: SDLScancode
sdlScancodeF8 = SDLScancode 65
sdlScancodeF9 :: SDLScancode
sdlScancodeF9 = SDLScancode 66
sdlScancodeF10 :: SDLScancode
sdlScancodeF10 = SDLScancode 67
sdlScancodeF11 :: SDLScancode
sdlScancodeF11 = SDLScancode 68
sdlScancodeF12 :: SDLScancode
sdlScancodeF12 = SDLScancode 69
sdlScancodePrintScreen :: SDLScancode
sdlScancodePrintScreen = SDLScancode 70
sdlScancodeScrollLock :: SDLScancode
sdlScancodeScrollLock = SDLScancode 71
sdlScancodePause :: SDLScancode
sdlScancodePause = SDLScancode 72
sdlScancodeInsert :: SDLScancode
sdlScancodeInsert = SDLScancode 73
sdlScancodeHome :: SDLScancode
sdlScancodeHome = SDLScancode 74
sdlScancodePageUp :: SDLScancode
sdlScancodePageUp = SDLScancode 75
sdlScancodeDelete :: SDLScancode
sdlScancodeDelete = SDLScancode 76
sdlScancodeEnd :: SDLScancode
sdlScancodeEnd = SDLScancode 77
sdlScancodePageDown :: SDLScancode
sdlScancodePageDown = SDLScancode 78
sdlScancodeRight :: SDLScancode
sdlScancodeRight = SDLScancode 79
sdlScancodeLeft :: SDLScancode
sdlScancodeLeft = SDLScancode 80
sdlScancodeDown :: SDLScancode
sdlScancodeDown = SDLScancode 81
sdlScancodeUp :: SDLScancode
sdlScancodeUp = SDLScancode 82
sdlScancodeNumLockClear :: SDLScancode
sdlScancodeNumLockClear = SDLScancode 83
sdlScancodeKPDivide :: SDLScancode
sdlScancodeKPDivide = SDLScancode 84
sdlScancodeKPMultiply :: SDLScancode
sdlScancodeKPMultiply = SDLScancode 85
sdlScancodeKPMinus :: SDLScancode
sdlScancodeKPMinus = SDLScancode 86
sdlScancodeKPPlus :: SDLScancode
sdlScancodeKPPlus = SDLScancode 87
sdlScancodeKPEnter :: SDLScancode
sdlScancodeKPEnter = SDLScancode 88
sdlScancodeKP1 :: SDLScancode
sdlScancodeKP1 = SDLScancode 89
sdlScancodeKP2 :: SDLScancode
sdlScancodeKP2 = SDLScancode 90
sdlScancodeKP3 :: SDLScancode
sdlScancodeKP3 = SDLScancode 91
sdlScancodeKP4 :: SDLScancode
sdlScancodeKP4 = SDLScancode 92
sdlScancodeKP5 :: SDLScancode
sdlScancodeKP5 = SDLScancode 93
sdlScancodeKP6 :: SDLScancode
sdlScancodeKP6 = SDLScancode 94
sdlScancodeKP7 :: SDLScancode
sdlScancodeKP7 = SDLScancode 95
sdlScancodeKP8 :: SDLScancode
sdlScancodeKP8 = SDLScancode 96
sdlScancodeKP9 :: SDLScancode
sdlScancodeKP9 = SDLScancode 97
sdlScancodeKP0 :: SDLScancode
sdlScancodeKP0 = SDLScancode 98
sdlScancodeKPPeriod :: SDLScancode
sdlScancodeKPPeriod = SDLScancode 99
sdlScancodeNonUSBackSlash :: SDLScancode
sdlScancodeNonUSBackSlash = SDLScancode 100
sdlScancodeApplication :: SDLScancode
sdlScancodeApplication = SDLScancode 101
sdlScancodePower :: SDLScancode
sdlScancodePower = SDLScancode 102
sdlScancodeKPEquals :: SDLScancode
sdlScancodeKPEquals = SDLScancode 103
sdlScancodeF13 :: SDLScancode
sdlScancodeF13 = SDLScancode 104
sdlScancodeF14 :: SDLScancode
sdlScancodeF14 = SDLScancode 105
sdlScancodeF15 :: SDLScancode
sdlScancodeF15 = SDLScancode 106
sdlScancodeF16 :: SDLScancode
sdlScancodeF16 = SDLScancode 107
sdlScancodeF17 :: SDLScancode
sdlScancodeF17 = SDLScancode 108
sdlScancodeF18 :: SDLScancode
sdlScancodeF18 = SDLScancode 109
sdlScancodeF19 :: SDLScancode
sdlScancodeF19 = SDLScancode 110
sdlScancodeF20 :: SDLScancode
sdlScancodeF20 = SDLScancode 111
sdlScancodeF21 :: SDLScancode
sdlScancodeF21 = SDLScancode 112
sdlScancodeF22 :: SDLScancode
sdlScancodeF22 = SDLScancode 113
sdlScancodeF23 :: SDLScancode
sdlScancodeF23 = SDLScancode 114
sdlScancodeF24 :: SDLScancode
sdlScancodeF24 = SDLScancode 115
sdlScancodeExecute :: SDLScancode
sdlScancodeExecute = SDLScancode 116
sdlScancodeHelp :: SDLScancode
sdlScancodeHelp = SDLScancode 117
sdlScancodeMenu :: SDLScancode
sdlScancodeMenu = SDLScancode 118
sdlScancodeSelect :: SDLScancode
sdlScancodeSelect = SDLScancode 119
sdlScancodeStop :: SDLScancode
sdlScancodeStop = SDLScancode 120
sdlScancodeAgain :: SDLScancode
sdlScancodeAgain = SDLScancode 121
sdlScancodeUndo :: SDLScancode
sdlScancodeUndo = SDLScancode 122
sdlScancodeCut :: SDLScancode
sdlScancodeCut = SDLScancode 123
sdlScancodeCopy :: SDLScancode
sdlScancodeCopy = SDLScancode 124
sdlScancodePaste :: SDLScancode
sdlScancodePaste = SDLScancode 125
sdlScancodeFind :: SDLScancode
sdlScancodeFind = SDLScancode 126
sdlScancodeMute :: SDLScancode
sdlScancodeMute = SDLScancode 127
sdlScancodeVolumeUp :: SDLScancode
sdlScancodeVolumeUp = SDLScancode 128
sdlScancodeVolumeDown :: SDLScancode
sdlScancodeVolumeDown = SDLScancode 129
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 130
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 131
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 132
sdlScancodeKPComma :: SDLScancode
sdlScancodeKPComma = SDLScancode 133
sdlScancodeKPEqualsAs400 :: SDLScancode
sdlScancodeKPEqualsAs400 = SDLScancode 134
sdlScancodeInternational1 :: SDLScancode
sdlScancodeInternational1 = SDLScancode 135
sdlScancodeInternational2 :: SDLScancode
sdlScancodeInternational2 = SDLScancode 136
sdlScancodeInternational3 :: SDLScancode
sdlScancodeInternational3 = SDLScancode 137
sdlScancodeInternational4 :: SDLScancode
sdlScancodeInternational4 = SDLScancode 138
sdlScancodeInternational5 :: SDLScancode
sdlScancodeInternational5 = SDLScancode 139
sdlScancodeInternational6 :: SDLScancode
sdlScancodeInternational6 = SDLScancode 140
sdlScancodeInternational7 :: SDLScancode
sdlScancodeInternational7 = SDLScancode 141
sdlScancodeInternational8 :: SDLScancode
sdlScancodeInternational8 = SDLScancode 142
sdlScancodeInternational9 :: SDLScancode
sdlScancodeInternational9 = SDLScancode 143
sdlScancodeLang1 :: SDLScancode
sdlScancodeLang1 = SDLScancode 144
sdlScancodeLang2 :: SDLScancode
sdlScancodeLang2 = SDLScancode 145
sdlScancodeLang3 :: SDLScancode
sdlScancodeLang3 = SDLScancode 146
sdlScancodeLang4 :: SDLScancode
sdlScancodeLang4 = SDLScancode 147
sdlScancodeLang5 :: SDLScancode
sdlScancodeLang5 = SDLScancode 148
sdlScancodeLang6 :: SDLScancode
sdlScancodeLang6 = SDLScancode 149
sdlScancodeLang7 :: SDLScancode
sdlScancodeLang7 = SDLScancode 150
sdlScancodeLang8 :: SDLScancode
sdlScancodeLang8 = SDLScancode 151
sdlScancodeLang9 :: SDLScancode
sdlScancodeLang9 = SDLScancode 152
sdlScancodeAltErease :: SDLScancode
sdlScancodeAltErease = SDLScancode 153
sdlScancodeSysReq :: SDLScancode
sdlScancodeSysReq = SDLScancode 154
sdlScancodeCancel :: SDLScancode
sdlScancodeCancel = SDLScancode 155
sdlScancodeClear :: SDLScancode
sdlScancodeClear = SDLScancode 156
sdlScancodePrior :: SDLScancode
sdlScancodePrior = SDLScancode 157
sdlScancodeReturn2 :: SDLScancode
sdlScancodeReturn2 = SDLScancode 158
sdlScancodeSeparator :: SDLScancode
sdlScancodeSeparator = SDLScancode 159
sdlScancodeOut :: SDLScancode
sdlScancodeOut = SDLScancode 160
sdlScancodeOper :: SDLScancode
sdlScancodeOper = SDLScancode 161
sdlScancodeClearAgain :: SDLScancode
sdlScancodeClearAgain = SDLScancode 162
sdlScancodeCrSel :: SDLScancode
sdlScancodeCrSel = SDLScancode 163
sdlScancodeExSel :: SDLScancode
sdlScancodeExSel = SDLScancode 164
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 165
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 166
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 167
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 168
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 169
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 170
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 171
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 172
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 173
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 174
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 175
sdlScancodeKP00 :: SDLScancode
sdlScancodeKP00 = SDLScancode 176
sdlScancodeKP000 :: SDLScancode
sdlScancodeKP000 = SDLScancode 177
sdlScancodeThousansSeparator :: SDLScancode
sdlScancodeThousansSeparator = SDLScancode 178
sdlScancodeDecimalSeparator :: SDLScancode
sdlScancodeDecimalSeparator = SDLScancode 179
sdlScancodeCurrencyUnit :: SDLScancode
sdlScancodeCurrencyUnit = SDLScancode 180
sdlScancodeCurrencySubUnit :: SDLScancode
sdlScancodeCurrencySubUnit = SDLScancode 181
sdlScancodeKPLeftParen :: SDLScancode
sdlScancodeKPLeftParen = SDLScancode 182
sdlScancodeKPRightParen :: SDLScancode
sdlScancodeKPRightParen = SDLScancode 183
sdlScancodeKPLeftBrace :: SDLScancode
sdlScancodeKPLeftBrace = SDLScancode 184
sdlScancodeKPRightBrace :: SDLScancode
sdlScancodeKPRightBrace = SDLScancode 185
sdlScancodeKPTab :: SDLScancode
sdlScancodeKPTab = SDLScancode 186
sdlScancodeKPBackSpace :: SDLScancode
sdlScancodeKPBackSpace = SDLScancode 187
sdlScancodeKPA :: SDLScancode
sdlScancodeKPA = SDLScancode 188
sdlScancodeKPB :: SDLScancode
sdlScancodeKPB = SDLScancode 189
sdlScancodeKPC :: SDLScancode
sdlScancodeKPC = SDLScancode 190
sdlScancodeKPD :: SDLScancode
sdlScancodeKPD = SDLScancode 191
sdlScancodeKPE :: SDLScancode
sdlScancodeKPE = SDLScancode 192
sdlScancodeKPF :: SDLScancode
sdlScancodeKPF = SDLScancode 193
sdlScancodeKPXor :: SDLScancode
sdlScancodeKPXor = SDLScancode 194
sdlScancodeKPPower :: SDLScancode
sdlScancodeKPPower = SDLScancode 195
sdlScancodeKPPercent :: SDLScancode
sdlScancodeKPPercent = SDLScancode 196
sdlScancodeKPLess :: SDLScancode
sdlScancodeKPLess = SDLScancode 197
sdlScancodeKPGreater :: SDLScancode
sdlScancodeKPGreater = SDLScancode 198
sdlScancodeKPAmpersand :: SDLScancode
sdlScancodeKPAmpersand = SDLScancode 199
sdlScancodeKPDBLAmpersand :: SDLScancode
sdlScancodeKPDBLAmpersand = SDLScancode 200
sdlScancodeKPVerticalBar :: SDLScancode
sdlScancodeKPVerticalBar = SDLScancode 201
sdlScancodeKPDBLVerticalBar :: SDLScancode
sdlScancodeKPDBLVerticalBar = SDLScancode 202
sdlScancodeKPColon :: SDLScancode
sdlScancodeKPColon = SDLScancode 203
sdlScancodeKPHash :: SDLScancode
sdlScancodeKPHash = SDLScancode 204
sdlScancodeKPSpace :: SDLScancode
sdlScancodeKPSpace = SDLScancode 205
sdlScancodeKPAt :: SDLScancode
sdlScancodeKPAt = SDLScancode 206
sdlScancodeKPExclam :: SDLScancode
sdlScancodeKPExclam = SDLScancode 207
sdlScancodeKPMemStore :: SDLScancode
sdlScancodeKPMemStore = SDLScancode 208
sdlScancodeKPMemRecall :: SDLScancode
sdlScancodeKPMemRecall = SDLScancode 209
sdlScancodeKPMemClear :: SDLScancode
sdlScancodeKPMemClear = SDLScancode 210
sdlScancodeKPMemAdd :: SDLScancode
sdlScancodeKPMemAdd = SDLScancode 211
sdlScancodeKPMemSubtract :: SDLScancode
sdlScancodeKPMemSubtract = SDLScancode 212
sdlScancodeKPMemMultiply :: SDLScancode
sdlScancodeKPMemMultiply = SDLScancode 213
sdlScancodeKPMemDivide :: SDLScancode
sdlScancodeKPMemDivide = SDLScancode 214
sdlScancodeKPPlusMinus :: SDLScancode
sdlScancodeKPPlusMinus = SDLScancode 215
sdlScancodeKPClear :: SDLScancode
sdlScancodeKPClear = SDLScancode 216
sdlScancodeKPClearEntry :: SDLScancode
sdlScancodeKPClearEntry = SDLScancode 217
sdlScancodeKPBinary :: SDLScancode
sdlScancodeKPBinary = SDLScancode 218
sdlScancodeKPOctal :: SDLScancode
sdlScancodeKPOctal = SDLScancode 219
sdlScancodeKPDecimal :: SDLScancode
sdlScancodeKPDecimal = SDLScancode 220
sdlScancodeKPHexaDecimal :: SDLScancode
sdlScancodeKPHexaDecimal = SDLScancode 221
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 222
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 223
sdlScancodeLCTRL :: SDLScancode
sdlScancodeLCTRL = SDLScancode 224
sdlScancodeLShift :: SDLScancode
sdlScancodeLShift = SDLScancode 225
sdlScancodeLAlt :: SDLScancode
sdlScancodeLAlt = SDLScancode 226
sdlScancodeLGUI :: SDLScancode
sdlScancodeLGUI = SDLScancode 227
sdlScancodeRCTRL :: SDLScancode
sdlScancodeRCTRL = SDLScancode 228
sdlScancodeRShift :: SDLScancode
sdlScancodeRShift = SDLScancode 229
sdlScancodeRAlt :: SDLScancode
sdlScancodeRAlt = SDLScancode 230
sdlScancodeRGUI :: SDLScancode
sdlScancodeRGUI = SDLScancode 231
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 232
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 233
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 234
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 235
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 236
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 237
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 238
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 239
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 240
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 241
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 242
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 243
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 244
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 245
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 246
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 247
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 248
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 249
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 250
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 251
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 252
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 253
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 254
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 255
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 256
sdlScancodeMode :: SDLScancode
sdlScancodeMode = SDLScancode 257
sdlScancodeAudioNext :: SDLScancode
sdlScancodeAudioNext = SDLScancode 258
sdlScancodeAudioPrev :: SDLScancode
sdlScancodeAudioPrev = SDLScancode 259
sdlScancodeAudioStop :: SDLScancode
sdlScancodeAudioStop = SDLScancode 260
sdlScancodeAudioPlay :: SDLScancode
sdlScancodeAudioPlay = SDLScancode 261
sdlScancodeAudioMute :: SDLScancode
sdlScancodeAudioMute = SDLScancode 262
sdlScancodeMediaSelect :: SDLScancode
sdlScancodeMediaSelect = SDLScancode 263
sdlScancodeWWW :: SDLScancode
sdlScancodeWWW = SDLScancode 264
sdlScancodeMail :: SDLScancode
sdlScancodeMail = SDLScancode 265
sdlScancodeCalculator :: SDLScancode
sdlScancodeCalculator = SDLScancode 266
sdlScancodeComputer :: SDLScancode
sdlScancodeComputer = SDLScancode 267
sdlScancodeACSearch :: SDLScancode
sdlScancodeACSearch = SDLScancode 268
sdlScancodeACHome :: SDLScancode
sdlScancodeACHome = SDLScancode 269
sdlScancodeACBack :: SDLScancode
sdlScancodeACBack = SDLScancode 270
sdlScancodeACForward :: SDLScancode
sdlScancodeACForward = SDLScancode 271
sdlScancodeACStop :: SDLScancode
sdlScancodeACStop = SDLScancode 272
sdlScancodeACRefresh :: SDLScancode
sdlScancodeACRefresh = SDLScancode 273
sdlScancodeACBookmarks :: SDLScancode
sdlScancodeACBookmarks = SDLScancode 274
sdlScancodeBrightnessDown :: SDLScancode
sdlScancodeBrightnessDown = SDLScancode 275
sdlScancodeBrightnessUp :: SDLScancode
sdlScancodeBrightnessUp = SDLScancode 276
sdlScancodeDisplaySwitch :: SDLScancode
sdlScancodeDisplaySwitch = SDLScancode 277
sdlScancodeKPDillumToggle :: SDLScancode
sdlScancodeKPDillumToggle = SDLScancode 278
sdlScancodeKPDillumDown :: SDLScancode
sdlScancodeKPDillumDown = SDLScancode 279
sdlScancodeKPDillumUp :: SDLScancode
sdlScancodeKPDillumUp = SDLScancode 280
sdlScancodeEject :: SDLScancode
sdlScancodeEject = SDLScancode 281
sdlScancodeSleep :: SDLScancode
sdlScancodeSleep = SDLScancode 282
sdlScancodeApp1 :: SDLScancode
sdlScancodeApp1 = SDLScancode 283
sdlScancodeApp2 :: SDLScancode
sdlScancodeApp2 = SDLScancode 284
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 285
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 286
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 287
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 288
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 289
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 290
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 291
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 292
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 293
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 294
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 295
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 296
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 297
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 298
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 299
-- sdlScancode :: SDLScancode
-- sdlScancode = SDLScancode 300