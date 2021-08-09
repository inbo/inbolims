select top(1000000) 
  ContractID =  c.[Contract]
, Klant = r.Customer
, r.Project
, VerantwoordelijkLabo = p.VerantwoordelijkLabo
, LaboLimsGroep = p.ProjectGroupName
, LimsStaalNummer = s.LIMSSampleNumber
, ExternSampleID = s.FieldSampleID
, LaboCode = s.LabSampleID
, OrigineelStaal = s.LIMSOriginalSampleNumber 
, SampleProduct = s.Product
, ProductGrade = s.MatrixDetail
, SamplingPoint = s.SamplingPoint
, Matrix = s.Matrix
, Monsternamedatum = s.FieldSamplingDate
, Monsternemer = s.FieldObserver
, Toestand = s.SampleCondition
, VoorbehandelingExtern = s.SamplePreparation
, Opmerking = s.FieldSampleRemark
, LimsAnalyseNaam = r.LimsAnalysisName
, LimsAnalyseVersie = a.AnalysisVersion
, SapCode = a.SAPcode
, AnalyseNaam = a.AnalysisLabName
, r.Component
, Gerapporteerd = r.IsReportable
, TestReplicaat = r.TestReplicateCount
, ResultaatReplicaat = r.ResultReplicate
, r.Instrument
, Batch = r.Batch
, WaardeRuw = r.Result
, WaardeGeformatteerd = r.ResultFormatted
, Eenheid = r.LimsUnit 
, BinnenSpecs = r.IsInSpec
, BenedenLOQ = r.isBelowLOQ
, BovenMaxLOQ = r.IsAboveLOQ
, ResultaatType = r.ResultType
, NumeriekeWaarde = r.ResultFormattedNumeric
, NumeriekeRuweWaarde = r.ResultNumeric
, VisueleMatrix = s.VisualMatrix
, Plaat = s.samplePlate
, PlaatPositie = s.SamplePlatePosition
, ArchiefStaal = s.IsArchiefstaal
, Xcoord = s.SampleLambertX
, Ycoord = s.SampleLambertY
, Diepte = s.SampleDepth
, Toponiem = s.SampleToponym
, r.contractKey
from dimSample s
left join factResult r on r.SampleKey = s.SampleKey
inner join dimAnalysis a on a.AnalysisKey = r.AnalysisKey
inner join dimProject p on r.ProjectKey = p.ProjectKey
left join dimContract c on r.ContractKey = c.ContractKey
where r.PROJECT in '<<INSERT_PROJECT_NAME>>'
and s.SampleStatus = 'A' 
and (s.SampleType <> 'DUP' or s.SampleType is null)
and r.IsReportable <> 0 


