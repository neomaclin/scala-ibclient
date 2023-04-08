package org.quasigroup.ibclient.client.exceptions

import org.quasigroup.ibclient.client.EClientErrors

import java.io.IOException

final class EClientException(err: EClientErrors.CodeMsgPair, text: String)
    extends IOException
