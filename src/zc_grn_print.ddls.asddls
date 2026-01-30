@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Consumption GRN_Print'
@Metadata.allowExtensions: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define root view entity ZC_GRN_PRINT
  provider contract transactional_query
  as projection on ZI_GRN_PRINT

{
  key Materialdocument,
  key MaterialDocumentYear,
  key MaterialDocumentItem,
      PostingDate,
      StorageLocation,
      Material,
      //      @Consumption.hidden: true
      EntryUnit,
      @Semantics.quantity.unitOfMeasure: 'EntryUnit'
      QuantityInEntryUnit,
      base64_3,
      m_ind



}
