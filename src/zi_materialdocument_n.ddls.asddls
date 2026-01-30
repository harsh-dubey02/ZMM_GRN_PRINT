@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'New CDS MT'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
//define view entity ZI_MATERIALDOCUMENT_N
//as  select distinct from I_MaterialDocumentHeader_2 as a
//       left outer join I_MaterialDocumentItem_2 as c
//      on  a.MaterialDocument = c.MaterialDocument
//      inner join      I_PurchaseOrderAPI01       as d on d.PurchaseOrder = c.PurchaseOrder 
//{
//    key a.MaterialDocument as Materialdocument,
//      key a.MaterialDocumentYear,
//      key c.MaterialDocumentItem,
//        a.PostingDate,
//        a.StorageLocation,
//        c.Material,
//        c.EntryUnit,
//      @Semantics.quantity.unitOfMeasure: 'EntryUnit'
//        c.QuantityInEntryUnit,
//        d.PurchaseOrder,
//         d.PurchaseOrderType
////    base64 as Base64,
////    base64_1 as Base641,
////    base64_2 as Base642,
////    base64_3 as Base643,
////    m_ind as MInd
//}
//where d.PurchaseOrderType = 'ZIMP'
//   or d.PurchaseOrderType = 'ZDOM'
//   or c.GoodsMovementType = '101'

define view entity ZI_MATERIALDOCUMENT_N
as select distinct from I_MaterialDocumentHeader_2 as a

inner join ZI_MD_FIRST_ITEM as f
    on  a.MaterialDocument     = f.MaterialDocument
    and a.MaterialDocumentYear = f.MaterialDocumentYear

inner join I_MaterialDocumentItem_2 as c
    on  c.MaterialDocument     = f.MaterialDocument
    and c.MaterialDocumentYear = f.MaterialDocumentYear
    and c.MaterialDocumentItem = f.MaterialDocumentItem

inner join I_PurchaseOrderAPI01 as d
    on d.PurchaseOrder = c.PurchaseOrder
{
    key a.MaterialDocument as Materialdocument,
    key a.MaterialDocumentYear,
    key c.MaterialDocumentItem,

    a.PostingDate,
    a.StorageLocation,
    c.Material,
    c.EntryUnit,

    @Semantics.quantity.unitOfMeasure: 'EntryUnit'
    c.QuantityInEntryUnit,

    d.PurchaseOrder,
    d.PurchaseOrderType
}
where
 d.PurchaseOrderType = 'ZIMP'
   or d.PurchaseOrderType = 'ZDOM'
   or c.GoodsMovementType = '101'

   
  
