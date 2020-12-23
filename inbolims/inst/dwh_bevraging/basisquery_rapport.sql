select top(100000) 
  ContractID = c.Contract
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
, WaardRuw = r.Result
, WaardeGeformatteerd = r.ResultFormatted
, Eenheid = r.LimsUnit 
, BinnenSpecs = r.IsInSpec
, BenedenLOQ = r.isBelowLOQ
, BovenMaxLOQ = r.IsAboveLOQ
, ResultaatType = r.ResultType
, NumeriekeWaarde = r.ResultFormattedNumeric
, NumeriekeRuweWaarde = r.ResultNumeric
, VisueleMatrix = s.VisualMatrix
, Plaat = s.SamplePlate
, PlaatPositie = s.SamplePlatePosition
, ArchiefStaal = s.IsArchiefStaal
, Xcoord = s.SampleLambertX
, Ycoord = s.SampleLambertY
, Diepte = s.SampleDepth
, Toponiem = s.SampleToponym
from dimSample s
inner join factResult r on r.SampleKey = s.SampleKey
inner join dimAnalysis a on a.AnalysisKey = r.AnalysisKey
inner join dimContract c on s.ContractKey = c.ContractKey
inner join dimProject p on p.ProjectKey = s.ProjectKey
where r.IsReportable <> 0 
and s.SampleStatus = 'A' 
<<PROJECTCRITERIUM>>
<<EXTRA CRITERIA>>
