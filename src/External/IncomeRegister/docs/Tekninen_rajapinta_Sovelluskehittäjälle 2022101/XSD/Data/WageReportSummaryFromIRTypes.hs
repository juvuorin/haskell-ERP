{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.WageReportSummaryFromIRTypes'xsd
  ( module Data.WageReportSummaryFromIRTypes'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import Text.XML.HaXml.OneOfN
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
import Data.IRCommonTypes'xsd as Irct
import Data.Xmldsig'core'schema'xsd as Ds
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
data AccidentInsurance = AccidentInsurance
        { accidInsur_accInsProvId :: Maybe Id
        , accidInsur_accInsPolicyNo :: Maybe Irct.String20
        , accidInsur_noAccInsPolicyNo :: Maybe Irct.True
        }
        deriving (Eq,Show)
instance SchemaType AccidentInsurance where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return AccidentInsurance
            `apply` optional (parseSchemaType "AccInsProvId")
            `apply` optional (parseSchemaType "AccInsPolicyNo")
            `apply` optional (parseSchemaType "NoAccInsPolicyNo")
    schemaTypeToXML s x@AccidentInsurance{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "AccInsProvId") $ accidInsur_accInsProvId x
            , maybe [] (schemaTypeToXML "AccInsPolicyNo") $ accidInsur_accInsPolicyNo x
            , maybe [] (schemaTypeToXML "NoAccInsPolicyNo") $ accidInsur_noAccInsPolicyNo x
            ]
 
data AccidentInsuranceIncome = AccidentInsuranceIncome
        { accidInsurIncome_subToAccInsContribution :: Irct.Decimal2
        , accidInsurIncome_recoverySubToAccInsContribution :: Irct.Decimal2
        }
        deriving (Eq,Show)
instance SchemaType AccidentInsuranceIncome where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return AccidentInsuranceIncome
            `apply` parseSchemaType "SubToAccInsContribution"
            `apply` parseSchemaType "RecoverySubToAccInsContribution"
    schemaTypeToXML s x@AccidentInsuranceIncome{} =
        toXMLElement s []
            [ schemaTypeToXML "SubToAccInsContribution" $ accidInsurIncome_subToAccInsContribution x
            , schemaTypeToXML "RecoverySubToAccInsContribution" $ accidInsurIncome_recoverySubToAccInsContribution x
            ]
 
data AccInsPolicyNumbersSummary = AccInsPolicyNumbersSummary
        { accInsPolicyNumbersSummary_accInsPolicyNumberSummary :: [AccInsPolicyNumberSummary]
        }
        deriving (Eq,Show)
instance SchemaType AccInsPolicyNumbersSummary where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return AccInsPolicyNumbersSummary
            `apply` many1 (parseSchemaType "AccInsPolicyNumberSummary")
    schemaTypeToXML s x@AccInsPolicyNumbersSummary{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "AccInsPolicyNumberSummary") $ accInsPolicyNumbersSummary_accInsPolicyNumberSummary x
            ]
 
data AccInsPolicyNumberSummary = AccInsPolicyNumberSummary
        { accInsPolicyNumberSummary_accidentInsurance :: AccidentInsurance
        , accInsPolicyNumberSummary_accidentInsuranceIncome :: AccidentInsuranceIncome
        }
        deriving (Eq,Show)
instance SchemaType AccInsPolicyNumberSummary where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return AccInsPolicyNumberSummary
            `apply` parseSchemaType "AccidentInsurance"
            `apply` parseSchemaType "AccidentInsuranceIncome"
    schemaTypeToXML s x@AccInsPolicyNumberSummary{} =
        toXMLElement s []
            [ schemaTypeToXML "AccidentInsurance" $ accInsPolicyNumberSummary_accidentInsurance x
            , schemaTypeToXML "AccidentInsuranceIncome" $ accInsPolicyNumberSummary_accidentInsuranceIncome x
            ]
 
data Id = Id
        { id_type :: Xs.Int
        , id_code :: Irct.String30
        , id_countryCode :: Maybe Irct.String2
        , id_countryName :: Maybe Irct.String70
        }
        deriving (Eq,Show)
instance SchemaType Id where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Id
            `apply` parseSchemaType "Type"
            `apply` parseSchemaType "Code"
            `apply` optional (parseSchemaType "CountryCode")
            `apply` optional (parseSchemaType "CountryName")
    schemaTypeToXML s x@Id{} =
        toXMLElement s []
            [ schemaTypeToXML "Type" $ id_type x
            , schemaTypeToXML "Code" $ id_code x
            , maybe [] (schemaTypeToXML "CountryCode") $ id_countryCode x
            , maybe [] (schemaTypeToXML "CountryName") $ id_countryName x
            ]
 
data Income = Income
        { income_total :: Maybe Irct.Decimal2
        , income_subToPensionInsContribution :: Maybe Irct.Decimal2
        , income_subToAccInsContribution :: Maybe Irct.Decimal2
        , income_subToUnemploymentInsContribution :: Maybe Irct.Decimal2
        , income_subToHealthInsContribution :: Maybe Irct.Decimal2
        , income_employeePensionInsContribution :: Maybe Irct.Decimal2
        , income_employeeUnemploymentInsContribution :: Maybe Irct.Decimal2
        , income_employeeHealthInsContribution :: Maybe Irct.Decimal2
        , income_withholding :: Maybe Irct.Decimal2
        , income_taxAtSource :: Maybe Irct.Decimal2
        }
        deriving (Eq,Show)
instance SchemaType Income where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Income
            `apply` optional (parseSchemaType "IncomeTotal")
            `apply` optional (parseSchemaType "SubToPensionInsContribution")
            `apply` optional (parseSchemaType "SubToAccInsContribution")
            `apply` optional (parseSchemaType "SubToUnemploymentInsContribution")
            `apply` optional (parseSchemaType "SubToHealthInsContribution")
            `apply` optional (parseSchemaType "EmployeePensionInsContribution")
            `apply` optional (parseSchemaType "EmployeeUnemploymentInsContribution")
            `apply` optional (parseSchemaType "EmployeeHealthInsContribution")
            `apply` optional (parseSchemaType "Withholding")
            `apply` optional (parseSchemaType "TaxAtSource")
    schemaTypeToXML s x@Income{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "IncomeTotal") $ income_total x
            , maybe [] (schemaTypeToXML "SubToPensionInsContribution") $ income_subToPensionInsContribution x
            , maybe [] (schemaTypeToXML "SubToAccInsContribution") $ income_subToAccInsContribution x
            , maybe [] (schemaTypeToXML "SubToUnemploymentInsContribution") $ income_subToUnemploymentInsContribution x
            , maybe [] (schemaTypeToXML "SubToHealthInsContribution") $ income_subToHealthInsContribution x
            , maybe [] (schemaTypeToXML "EmployeePensionInsContribution") $ income_employeePensionInsContribution x
            , maybe [] (schemaTypeToXML "EmployeeUnemploymentInsContribution") $ income_employeeUnemploymentInsContribution x
            , maybe [] (schemaTypeToXML "EmployeeHealthInsContribution") $ income_employeeHealthInsContribution x
            , maybe [] (schemaTypeToXML "Withholding") $ income_withholding x
            , maybe [] (schemaTypeToXML "TaxAtSource") $ income_taxAtSource x
            ]
 
data IncomeEarner = IncomeEarner
        { incomeEarner_id :: Maybe Id
        , incomeEarner_birthDate :: Maybe Xsd.Date
        , incomeEarner_missingIdentification :: Maybe Irct.True
        }
        deriving (Eq,Show)
instance SchemaType IncomeEarner where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return IncomeEarner
            `apply` optional (parseSchemaType "IncomeEarnerId")
            `apply` optional (parseSchemaType "BirthDate")
            `apply` optional (parseSchemaType "MissingIdentification")
    schemaTypeToXML s x@IncomeEarner{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "IncomeEarnerId") $ incomeEarner_id x
            , maybe [] (schemaTypeToXML "BirthDate") $ incomeEarner_birthDate x
            , maybe [] (schemaTypeToXML "MissingIdentification") $ incomeEarner_missingIdentification x
            ]
 
data IncomeEarnerIncome = IncomeEarnerIncome
        { incomeEarnerIncome_noIncome :: Maybe Irct.True
        , incomeEarnerIncome_incomeSummary :: Maybe IncomeSummary
        }
        deriving (Eq,Show)
instance SchemaType IncomeEarnerIncome where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return IncomeEarnerIncome
            `apply` optional (parseSchemaType "NoIncome")
            `apply` optional (parseSchemaType "IncomeSummary")
    schemaTypeToXML s x@IncomeEarnerIncome{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "NoIncome") $ incomeEarnerIncome_noIncome x
            , maybe [] (schemaTypeToXML "IncomeSummary") $ incomeEarnerIncome_incomeSummary x
            ]
 
data IncomeEarnersSummary = IncomeEarnersSummary
        { incomeEarnersSummary_incomeEarnerSummary :: [IncomeEarnerSummary]
        }
        deriving (Eq,Show)
instance SchemaType IncomeEarnersSummary where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return IncomeEarnersSummary
            `apply` many1 (parseSchemaType "IncomeEarnerSummary")
    schemaTypeToXML s x@IncomeEarnersSummary{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "IncomeEarnerSummary") $ incomeEarnersSummary_incomeEarnerSummary x
            ]
 
data IncomeEarnerSummary = IncomeEarnerSummary
        { incomeEarnerSummary_incomeEarner :: IncomeEarner
        , incomeEarnerSummary_incomeEarnerIncome :: IncomeEarnerIncome
        }
        deriving (Eq,Show)
instance SchemaType IncomeEarnerSummary where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return IncomeEarnerSummary
            `apply` parseSchemaType "IncomeEarner"
            `apply` parseSchemaType "IncomeEarnerIncome"
    schemaTypeToXML s x@IncomeEarnerSummary{} =
        toXMLElement s []
            [ schemaTypeToXML "IncomeEarner" $ incomeEarnerSummary_incomeEarner x
            , schemaTypeToXML "IncomeEarnerIncome" $ incomeEarnerSummary_incomeEarnerIncome x
            ]
 
data IncomeSummary = IncomeSummary
        { incomeSummary_reporterSubTypeIncome :: ReporterSubTypeIncome
        }
        deriving (Eq,Show)
instance SchemaType IncomeSummary where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return IncomeSummary
            `apply` parseSchemaType "ReporterSubTypeIncome"
    schemaTypeToXML s x@IncomeSummary{} =
        toXMLElement s []
            [ schemaTypeToXML "ReporterSubTypeIncome" $ incomeSummary_reporterSubTypeIncome x
            ]
 
data Payer = Payer
        { payer_id :: Id
        , payer_subOrg :: Maybe SubOrg
        }
        deriving (Eq,Show)
instance SchemaType Payer where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Payer
            `apply` parseSchemaType "PayerId"
            `apply` optional (parseSchemaType "SubOrg")
    schemaTypeToXML s x@Payer{} =
        toXMLElement s []
            [ schemaTypeToXML "PayerId" $ payer_id x
            , maybe [] (schemaTypeToXML "SubOrg") $ payer_subOrg x
            ]
 
data PaymentDateSummary = PaymentDateSummary
        { paymentDateSummary_paymentDate :: Xsd.Date
        , paymentDateSummary_wageReportCount :: Xs.Int
        , paymentDateSummary_incomeTotal :: Irct.Decimal2
        , paymentDateSummary_subToPensionInsContribution :: Irct.Decimal2
        , paymentDateSummary_subToAccInsContribution :: Irct.Decimal2
        , paymentDateSummary_subToUnemploymentInsContribution :: Irct.Decimal2
        , paymentDateSummary_subToHealthInsContribution :: Irct.Decimal2
        , paymentDateSummary_employeePensionInsContribution :: Irct.Decimal2
        , paymentDateSummary_employeeUnemploymentInsContribution :: Irct.Decimal2
        , paymentDateSummary_employeeHealthInsContribution :: Irct.Decimal2
        , paymentDateSummary_withholding :: Irct.Decimal2
        , paymentDateSummary_taxAtSource :: Irct.Decimal2
        , paymentDateSummary_unjustEnrichmentTotal :: Irct.Decimal2
        , paymentDateSummary_recoveryTotal :: Irct.Decimal2
        , paymentDateSummary_recoverySubToPensionInsContribution :: Irct.Decimal2
        , paymentDateSummary_recoverySubToAccInsContribution :: Irct.Decimal2
        , paymentDateSummary_recoverySubToUnemploymentInsContribution :: Irct.Decimal2
        , paymentDateSummary_recoverySubToHealthInsContribution :: Irct.Decimal2
        , paymentDateSummary_incomeEarnersSummary :: IncomeEarnersSummary
        , paymentDateSummary_pensionPolicyNumbersSummary :: PensionPolicyNumbersSummary
        , paymentDateSummary_accInsPolicyNumbersSummary :: AccInsPolicyNumbersSummary
        }
        deriving (Eq,Show)
instance SchemaType PaymentDateSummary where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return PaymentDateSummary
            `apply` parseSchemaType "PaymentDate"
            `apply` parseSchemaType "WageReportCount"
            `apply` parseSchemaType "IncomeTotal"
            `apply` parseSchemaType "SubToPensionInsContribution"
            `apply` parseSchemaType "SubToAccInsContribution"
            `apply` parseSchemaType "SubToUnemploymentInsContribution"
            `apply` parseSchemaType "SubToHealthInsContribution"
            `apply` parseSchemaType "EmployeePensionInsContribution"
            `apply` parseSchemaType "EmployeeUnemploymentInsContribution"
            `apply` parseSchemaType "EmployeeHealthInsContribution"
            `apply` parseSchemaType "Withholding"
            `apply` parseSchemaType "TaxAtSource"
            `apply` parseSchemaType "UnjustEnrichmentTotal"
            `apply` parseSchemaType "RecoveryTotal"
            `apply` parseSchemaType "RecoverySubToPensionInsContribution"
            `apply` parseSchemaType "RecoverySubToAccInsContribution"
            `apply` parseSchemaType "RecoverySubToUnemploymentInsContribution"
            `apply` parseSchemaType "RecoverySubToHealthInsContribution"
            `apply` parseSchemaType "IncomeEarnersSummary"
            `apply` parseSchemaType "PensionPolicyNumbersSummary"
            `apply` parseSchemaType "AccInsPolicyNumbersSummary"
    schemaTypeToXML s x@PaymentDateSummary{} =
        toXMLElement s []
            [ schemaTypeToXML "PaymentDate" $ paymentDateSummary_paymentDate x
            , schemaTypeToXML "WageReportCount" $ paymentDateSummary_wageReportCount x
            , schemaTypeToXML "IncomeTotal" $ paymentDateSummary_incomeTotal x
            , schemaTypeToXML "SubToPensionInsContribution" $ paymentDateSummary_subToPensionInsContribution x
            , schemaTypeToXML "SubToAccInsContribution" $ paymentDateSummary_subToAccInsContribution x
            , schemaTypeToXML "SubToUnemploymentInsContribution" $ paymentDateSummary_subToUnemploymentInsContribution x
            , schemaTypeToXML "SubToHealthInsContribution" $ paymentDateSummary_subToHealthInsContribution x
            , schemaTypeToXML "EmployeePensionInsContribution" $ paymentDateSummary_employeePensionInsContribution x
            , schemaTypeToXML "EmployeeUnemploymentInsContribution" $ paymentDateSummary_employeeUnemploymentInsContribution x
            , schemaTypeToXML "EmployeeHealthInsContribution" $ paymentDateSummary_employeeHealthInsContribution x
            , schemaTypeToXML "Withholding" $ paymentDateSummary_withholding x
            , schemaTypeToXML "TaxAtSource" $ paymentDateSummary_taxAtSource x
            , schemaTypeToXML "UnjustEnrichmentTotal" $ paymentDateSummary_unjustEnrichmentTotal x
            , schemaTypeToXML "RecoveryTotal" $ paymentDateSummary_recoveryTotal x
            , schemaTypeToXML "RecoverySubToPensionInsContribution" $ paymentDateSummary_recoverySubToPensionInsContribution x
            , schemaTypeToXML "RecoverySubToAccInsContribution" $ paymentDateSummary_recoverySubToAccInsContribution x
            , schemaTypeToXML "RecoverySubToUnemploymentInsContribution" $ paymentDateSummary_recoverySubToUnemploymentInsContribution x
            , schemaTypeToXML "RecoverySubToHealthInsContribution" $ paymentDateSummary_recoverySubToHealthInsContribution x
            , schemaTypeToXML "IncomeEarnersSummary" $ paymentDateSummary_incomeEarnersSummary x
            , schemaTypeToXML "PensionPolicyNumbersSummary" $ paymentDateSummary_pensionPolicyNumbersSummary x
            , schemaTypeToXML "AccInsPolicyNumbersSummary" $ paymentDateSummary_accInsPolicyNumbersSummary x
            ]
 
data PensionInsurance = PensionInsurance
        { pensionInsur_pensionProvIdCode :: Maybe Xs.Int
        , pensionInsur_pensionPolicyNo :: Maybe Irct.PensionPolicyNo
        , pensionInsur_noPensionPolicyNo :: Maybe Irct.True
        }
        deriving (Eq,Show)
instance SchemaType PensionInsurance where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return PensionInsurance
            `apply` optional (parseSchemaType "PensionProvIdCode")
            `apply` optional (parseSchemaType "PensionPolicyNo")
            `apply` optional (parseSchemaType "NoPensionPolicyNo")
    schemaTypeToXML s x@PensionInsurance{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "PensionProvIdCode") $ pensionInsur_pensionProvIdCode x
            , maybe [] (schemaTypeToXML "PensionPolicyNo") $ pensionInsur_pensionPolicyNo x
            , maybe [] (schemaTypeToXML "NoPensionPolicyNo") $ pensionInsur_noPensionPolicyNo x
            ]
 
data PensionInsuranceIncome = PensionInsuranceIncome
        { pensionInsurIncome_subToPensionInsContribution :: Irct.Decimal2
        , pensionInsurIncome_recoverySubToPensionInsContribution :: Irct.Decimal2
        }
        deriving (Eq,Show)
instance SchemaType PensionInsuranceIncome where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return PensionInsuranceIncome
            `apply` parseSchemaType "SubToPensionInsContribution"
            `apply` parseSchemaType "RecoverySubToPensionInsContribution"
    schemaTypeToXML s x@PensionInsuranceIncome{} =
        toXMLElement s []
            [ schemaTypeToXML "SubToPensionInsContribution" $ pensionInsurIncome_subToPensionInsContribution x
            , schemaTypeToXML "RecoverySubToPensionInsContribution" $ pensionInsurIncome_recoverySubToPensionInsContribution x
            ]
 
data PensionPolicyNumbersSummary = PensionPolicyNumbersSummary
        { pensionPolicyNumbersSummary_pensionPolicyNumberSummary :: [PensionPolicyNumberSummary]
        }
        deriving (Eq,Show)
instance SchemaType PensionPolicyNumbersSummary where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return PensionPolicyNumbersSummary
            `apply` many1 (parseSchemaType "PensionPolicyNumberSummary")
    schemaTypeToXML s x@PensionPolicyNumbersSummary{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "PensionPolicyNumberSummary") $ pensionPolicyNumbersSummary_pensionPolicyNumberSummary x
            ]
 
data PensionPolicyNumberSummary = PensionPolicyNumberSummary
        { pensionPolicyNumberSummary_pensionInsurance :: PensionInsurance
        , pensionPolicyNumberSummary_pensionInsuranceIncome :: PensionInsuranceIncome
        }
        deriving (Eq,Show)
instance SchemaType PensionPolicyNumberSummary where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return PensionPolicyNumberSummary
            `apply` parseSchemaType "PensionInsurance"
            `apply` parseSchemaType "PensionInsuranceIncome"
    schemaTypeToXML s x@PensionPolicyNumberSummary{} =
        toXMLElement s []
            [ schemaTypeToXML "PensionInsurance" $ pensionPolicyNumberSummary_pensionInsurance x
            , schemaTypeToXML "PensionInsuranceIncome" $ pensionPolicyNumberSummary_pensionInsuranceIncome x
            ]
 
data Query = Query
        { query_iRQueryId :: Irct.Guid
        , query_timestamp :: Xs.DateTime
        }
        deriving (Eq,Show)
instance SchemaType Query where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Query
            `apply` parseSchemaType "IRQueryId"
            `apply` parseSchemaType "QueryTimestamp"
    schemaTypeToXML s x@Query{} =
        toXMLElement s []
            [ schemaTypeToXML "IRQueryId" $ query_iRQueryId x
            , schemaTypeToXML "QueryTimestamp" $ query_timestamp x
            ]
 
data Recovery = Recovery
        { recovery_total :: Maybe Irct.Decimal2
        , recovery_subToPensionInsContribution :: Maybe Irct.Decimal2
        , recovery_subToAccInsContribution :: Maybe Irct.Decimal2
        , recovery_subToUnemploymentInsContribution :: Maybe Irct.Decimal2
        , recovery_subToHealthInsContribution :: Maybe Irct.Decimal2
        }
        deriving (Eq,Show)
instance SchemaType Recovery where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Recovery
            `apply` optional (parseSchemaType "RecoveryTotal")
            `apply` optional (parseSchemaType "RecoverySubToPensionInsContribution")
            `apply` optional (parseSchemaType "RecoverySubToAccInsContribution")
            `apply` optional (parseSchemaType "RecoverySubToUnemploymentInsContribution")
            `apply` optional (parseSchemaType "RecoverySubToHealthInsContribution")
    schemaTypeToXML s x@Recovery{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "RecoveryTotal") $ recovery_total x
            , maybe [] (schemaTypeToXML "RecoverySubToPensionInsContribution") $ recovery_subToPensionInsContribution x
            , maybe [] (schemaTypeToXML "RecoverySubToAccInsContribution") $ recovery_subToAccInsContribution x
            , maybe [] (schemaTypeToXML "RecoverySubToUnemploymentInsContribution") $ recovery_subToUnemploymentInsContribution x
            , maybe [] (schemaTypeToXML "RecoverySubToHealthInsContribution") $ recovery_subToHealthInsContribution x
            ]
 
data ReporterSubTypeIncome = ReporterSubTypeIncome
        { reportSubTypeIncome_reporterSubType :: Xs.Int
        , reportSubTypeIncome_income :: Maybe Income
        , reportSubTypeIncome_unjustEnrichment :: Maybe UnjustEnrichment
        , reportSubTypeIncome_recovery :: Maybe Recovery
        }
        deriving (Eq,Show)
instance SchemaType ReporterSubTypeIncome where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return ReporterSubTypeIncome
            `apply` parseSchemaType "ReporterSubType"
            `apply` optional (parseSchemaType "Income")
            `apply` optional (parseSchemaType "UnjustEnrichment")
            `apply` optional (parseSchemaType "Recovery")
    schemaTypeToXML s x@ReporterSubTypeIncome{} =
        toXMLElement s []
            [ schemaTypeToXML "ReporterSubType" $ reportSubTypeIncome_reporterSubType x
            , maybe [] (schemaTypeToXML "Income") $ reportSubTypeIncome_income x
            , maybe [] (schemaTypeToXML "UnjustEnrichment") $ reportSubTypeIncome_unjustEnrichment x
            , maybe [] (schemaTypeToXML "Recovery") $ reportSubTypeIncome_recovery x
            ]
 
data SubOrg = SubOrg
        { subOrg_type :: Xs.Int
        , subOrg_code :: Irct.String20
        }
        deriving (Eq,Show)
instance SchemaType SubOrg where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return SubOrg
            `apply` parseSchemaType "Type"
            `apply` parseSchemaType "Code"
    schemaTypeToXML s x@SubOrg{} =
        toXMLElement s []
            [ schemaTypeToXML "Type" $ subOrg_type x
            , schemaTypeToXML "Code" $ subOrg_code x
            ]
 
data Subscription = Subscription
        { subscription_queryDataType :: Xs.Int
        , subscription_productionEnvironment :: Xsd.Boolean
        , subscription_iRMainSubscriptionId :: Irct.Guid
        , subscription_iRSubscriptionId :: Irct.Guid
        , subscription_mainSubscriptionId :: Irct.String40
        , subscription_id :: Irct.String40
        }
        deriving (Eq,Show)
instance SchemaType Subscription where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Subscription
            `apply` parseSchemaType "QueryDataType"
            `apply` parseSchemaType "ProductionEnvironment"
            `apply` parseSchemaType "IRMainSubscriptionId"
            `apply` parseSchemaType "IRSubscriptionId"
            `apply` parseSchemaType "MainSubscriptionId"
            `apply` parseSchemaType "SubscriptionId"
    schemaTypeToXML s x@Subscription{} =
        toXMLElement s []
            [ schemaTypeToXML "QueryDataType" $ subscription_queryDataType x
            , schemaTypeToXML "ProductionEnvironment" $ subscription_productionEnvironment x
            , schemaTypeToXML "IRMainSubscriptionId" $ subscription_iRMainSubscriptionId x
            , schemaTypeToXML "IRSubscriptionId" $ subscription_iRSubscriptionId x
            , schemaTypeToXML "MainSubscriptionId" $ subscription_mainSubscriptionId x
            , schemaTypeToXML "SubscriptionId" $ subscription_id x
            ]
 
data Summary = Summary
        { summary_nrOfReports :: Xs.Int
        }
        deriving (Eq,Show)
instance SchemaType Summary where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Summary
            `apply` parseSchemaType "NrOfReports"
    schemaTypeToXML s x@Summary{} =
        toXMLElement s []
            [ schemaTypeToXML "NrOfReports" $ summary_nrOfReports x
            ]
 
data UnjustEnrichment = UnjustEnrichment
        { unjustEnrich_total :: Irct.Decimal2
        }
        deriving (Eq,Show)
instance SchemaType UnjustEnrichment where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return UnjustEnrichment
            `apply` parseSchemaType "UnjustEnrichmentTotal"
    schemaTypeToXML s x@UnjustEnrichment{} =
        toXMLElement s []
            [ schemaTypeToXML "UnjustEnrichmentTotal" $ unjustEnrich_total x
            ]
 
data WageReportPaymentDatesSummary = WageReportPaymentDatesSummary
        { wageReportPaymentDatesSummary_paymentDateSummary :: [PaymentDateSummary]
        }
        deriving (Eq,Show)
instance SchemaType WageReportPaymentDatesSummary where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return WageReportPaymentDatesSummary
            `apply` many1 (parseSchemaType "PaymentDateSummary")
    schemaTypeToXML s x@WageReportPaymentDatesSummary{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "PaymentDateSummary") $ wageReportPaymentDatesSummary_paymentDateSummary x
            ]
 
data WageReportSummaryFromIR = WageReportSummaryFromIR
        { wageReportSummaryFromIR_subscription :: Subscription
        , wageReportSummaryFromIR_query :: Query
        , wageReportSummaryFromIR_summary :: Summary
        , wageReportSummaryFromIR_payer :: Payer
        , wageReportSummaryFromIR_wageReportPaymentDatesSummary :: Maybe WageReportPaymentDatesSummary
        , wageReportSummaryFromIR_signature :: SignatureType
        }
        deriving (Eq,Show)
instance SchemaType WageReportSummaryFromIR where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return WageReportSummaryFromIR
            `apply` parseSchemaType "Subscription"
            `apply` parseSchemaType "Query"
            `apply` parseSchemaType "Summary"
            `apply` parseSchemaType "Payer"
            `apply` optional (parseSchemaType "WageReportPaymentDatesSummary")
            `apply` elementSignature
    schemaTypeToXML s x@WageReportSummaryFromIR{} =
        toXMLElement s []
            [ schemaTypeToXML "Subscription" $ wageReportSummaryFromIR_subscription x
            , schemaTypeToXML "Query" $ wageReportSummaryFromIR_query x
            , schemaTypeToXML "Summary" $ wageReportSummaryFromIR_summary x
            , schemaTypeToXML "Payer" $ wageReportSummaryFromIR_payer x
            , maybe [] (schemaTypeToXML "WageReportPaymentDatesSummary") $ wageReportSummaryFromIR_wageReportPaymentDatesSummary x
            , elementToXMLSignature $ wageReportSummaryFromIR_signature x
            ]
