module Factory.Input exposing (formula)

import Factory.Factory exposing (Reactions, parse)


input =
    """1 ZQVND => 2 MBZM
       2 KZCVX, 1 SZBQ => 7 HQFB
       1 PFSQF => 9 RSVN
       2 PJXQB => 4 FSNZ
       20 JVDKQ, 2 LSQFK, 8 SDNCK, 1 MQJNV, 13 LBTV, 3 KPBRX => 5 QBPC
       131 ORE => 8 WDQSL
       19 BRGJH, 2 KNVN, 3 CRKW => 9 MQJNV
       16 DNPM, 1 VTVBF, 11 JSGM => 1 BWVJ
       3 KNVN, 1 JQRML => 7 HGQJ
       1 MRQJ, 2 HQFB, 1 MQJNV => 5 VQLP
       1 PLGH => 5 DMGF
       12 DMGF, 3 DNPM, 1 CRKW => 1 CLML
       1 JSGM, 1 RSVN => 5 TMNKH
       1 RFJLG, 3 CFWC => 2 ZJMC
       1 BRGJH => 5 KPBRX
       1 SZBQ, 17 GBVJF => 4 ZHGL
       2 PLGH => 5 CFWC
       4 FCBZS, 2 XQWHB => 8 JSGM
       2 PFSQF => 2 KNVN
       12 CRKW, 9 GBVJF => 1 KRCB
       1 ZHGL => 8 PJMFP
       198 ORE => 2 XQWHB
       2 BWVJ, 7 CFWC, 17 DPMWN => 3 KZCVX
       4 WXBF => 6 JVDKQ
       2 SWMTK, 1 JQRML => 7 QXGZ
       1 JSGM, 1 LFSFJ => 4 LSQFK
       73 KNVN, 65 VQLP, 12 QBPC, 4 XGTL, 10 SWMTK, 51 ZJMC, 4 JMCPR, 1 VNHT => 1 FUEL
       1 BWVJ, 7 MBZM => 5 JXZT
       10 CFWC => 2 DPMWN
       13 LQDLN => 3 LBTV
       1 PFZW, 3 LQDLN => 5 PJXQB
       2 RSVN, 2 PFSQF => 5 CRKW
       1 HGQJ, 3 SMNGJ, 36 JXZT, 10 FHKG, 3 KPBRX, 2 CLML => 3 JMCPR
       126 ORE => 4 FCBZS
       1 DNPM, 13 MBZM => 5 PLGH
       2 XQWHB, 10 FCBZS => 9 LFSFJ
       1 DPMWN => 9 PFZW
       1 ZJMC, 3 TMNKH => 2 SWMTK
       7 TZCK, 1 XQWHB => 5 ZQVND
       4 CFWC, 1 ZLWN, 5 RSVN => 2 WXBF
       1 BRGJH, 2 CLML => 6 LQDLN
       26 BWVJ => 2 GBVJF
       16 PJXQB, 20 SDNCK, 3 HQFB, 7 QXGZ, 2 KNVN, 9 KZCVX => 8 XGTL
       8 PJMFP, 3 BRGJH, 19 MRQJ => 5 SMNGJ
       7 DNPM => 2 SZBQ
       2 JQRML, 14 SDNCK => 8 FHKG
       1 FSNZ, 6 RFJLG, 2 CRKW => 8 SDNCK
       2 CLML, 4 SWMTK, 16 KNVN => 4 JQRML
       8 TZCK, 18 WDQSL => 2 PFSQF
       1 LSQFK => 8 VTVBF
       18 BRGJH, 8 ZHGL, 2 KRCB => 7 VNHT
       3 TZCK => 4 DNPM
       14 PFZW, 1 PFSQF => 7 BRGJH
       21 PLGH, 6 VTVBF, 2 RSVN => 1 ZLWN
       149 ORE => 2 TZCK
       3 JSGM => 1 RFJLG
       4 PFSQF, 4 DMGF => 3 MRQJ"""


formula : Reactions
formula =
    parse input
