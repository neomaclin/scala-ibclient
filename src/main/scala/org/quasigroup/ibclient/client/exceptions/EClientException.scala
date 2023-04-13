package org.quasigroup.ibclient.client.exceptions

import java.io.IOException

final class EClientException(err: EClientErrors.CodeMsgPair, text: String)
    extends IOException
