@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'interface for material transfer'
@Metadata.allowExtensions: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define root view entity ZI_GRN_PRINT
  as select from    ZI_MATERIALDOCUMENT_N as a
    left outer join ztb_grn_print              as b on a.Materialdocument = b.materialdocument
//    inner join      I_MaterialDocumentItem_2   as c on c.MaterialDocumentYear = a.MaterialDocumentYear



{
  key a.Materialdocument,
  key a.MaterialDocumentYear,
        key a.MaterialDocumentItem,
      a.PostingDate,
      a.StorageLocation,
      a.Material,
      a.EntryUnit,
      @Semantics.quantity.unitOfMeasure: 'EntryUnit'
      a.QuantityInEntryUnit,
      b.base64_3,
      b.m_ind
}
