// package org.quasigroup.ibclient.response.readers

// import org.quasigroup.ibclient.IBClient
// import org.quasigroup.ibclient.IBClient.*
// import org.quasigroup.ibclient.decoder.Decoder.{DecoderState, read, readNothing}
// import org.quasigroup.ibclient.response.ResponseMsg.ErrorDetail
// import org.quasigroup.ibclient.exceptions.EClientErrors

// object ErrorDetailReader {

//   def create(using serverVersion: IBClient.ServerVersion): DecoderState[ErrorDetail] =
//     for
//       version <- read[Int]
//       error <-
//         if version < 2 then
//           read[String].map(msg => ErrorDetail(EClientErrors.NO_VALID_ID, EClientErrors.UNKNOWN_ID.code, msg, ""))
//         else
//           for
//             id <- read[Int]
//             errorCode <- read[Int]
//             errorMsg <- read[String].map(str =>
//               if serverVersion >= IBClient.MIN_SERVER_VER_ENCODE_MSG_ASCII7 then decodeUnicodeEscapedString(str)
//               else str
//             )
//             advancedOrderRejectJson <- read[String].map(str =>
//               if serverVersion >= IBClient.MIN_SERVER_VER_ADVANCED_ORDER_REJECT then decodeUnicodeEscapedString(str)
//               else str
//             )
//           yield ErrorDetail(id, errorCode, errorMsg, advancedOrderRejectJson)
//     yield error

// }
