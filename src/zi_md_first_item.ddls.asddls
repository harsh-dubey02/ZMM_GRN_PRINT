@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.ignorePropagatedAnnotations: true
@EndUserText.label: 'First Item per Material Document'
define view entity ZI_MD_FIRST_ITEM
as select from I_MaterialDocumentItem_2
{
    MaterialDocument,
    MaterialDocumentYear,
    min( MaterialDocumentItem ) as MaterialDocumentItem
}
group by
    MaterialDocument,
    MaterialDocumentYear;
