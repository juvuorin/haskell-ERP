module Event.EventTypes where 

data EventType =  EventTypePostPurchaseInvoice
                | EventTypeInsertPurchaseInvoice 
                | EventTypePostSalesInvoice
                | EventTypePostVatReport
                | EventTypeUpdateDocument
                | EventTypeDeleteDocument
                | EventTypeInsertDocument
