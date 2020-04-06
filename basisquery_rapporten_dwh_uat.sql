

select top(1000) 
  ContractID = 'CONTRACT_QUOTE.CONTRACT_QUOTE_ID'
, Klant = r.Customer
, r.Project
, VerantwoordelijkLabo = 'PROJECT.OWNER_LOCATION'
, LaboLimsGroep = 'PROJECT.GROUP_NAME'
, LimsStaalNummer = s.LIMSSampleNumber
, ExternSampleID = s.FieldSampleID
, LaboCode = s.LabSampleID
, OrigineelStaal = s.LIMSOriginalSampleNumber 
, SampleProduct = 'SAMPLE.PRODUCT'
, ProductGrade = s.MatrixDetail
, SamplingPoint = s.SamplingPoint
, Matrix = 'SAMPLE.C_SAMPLE_MATRIX'
, Monsternamedatum = s.FieldSamplingDate
, Monsternemer = s.FieldObserver
, Toestand = 'SAMPLE.C_CONDITION'
, VoorbehandelingExtern = s.SamplePreparation
, Opmerking = s.FieldSampleRemark
, LimsAnalyseNaam = r.LimsAnalysisName
, LimsAnalyseVersie = a.AnalysisVersion
, SapCode = a.SAPcode
, AnalyseNaam = a.AnalysisLabName
, r.Component
, Gerapporteerd = r.IsReportable
, TestReplicaat = 'TEST.REPLICATE_COUNT'
, ResultaatReplicaat = r.ResultReplicate
, r.Instrument
, Batch = 'TEST.BATCH'
, WaardRuw = r.Result
, WaardeGeformatteerd = r.ResultFormatted
, Eenheid = r.LimsUnit 
, BinnenSpecs = r.IsInSpec
, BenedenLOQ = r.isBelowLOQ
, BovenMaxLOQ = r.IsAboveLOQ
, ResultaatType = 'RESULT.RESULT_TYPE'
, NumeriekeWaarde = r.ResultFormattedNumeric
, NumeriekeRuweWaarde = r.ResultNumeric
, VisueleMatrix = 'C_VISUAL_MATRIX'
, Plaat = 'SAMPLE.C_PLATE_MAIN'
, PlaatPositie = 'SAMPLE.C_PLATE_MAIN_POS'
, ArchiefStaal = 'SAMPLE.C_ARCHIVE_SAMPLE'
, Xcoord = s.SampleLambertX
, Ycoord = s.SampleLambertY
, Diepte = s.SampleDepth
, Toponiem = s.SampleToponym
from dimSample s
left join factResult r on r.SampleKey = s.SampleKey
left join dimAnalysis a on a.AnalysisKey = r.AnalysisKey
--left join dimContract c on r.ContractKey = c.ContractKey, ofwel rechtstreeks in dimProject of dimResult invullen
where r.PROJECT = '<<INSERT_PROJECT_NAME>>'
and s.SampleStatus = 'A' 
and r.IsReportable <> 0


