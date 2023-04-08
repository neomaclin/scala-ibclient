package org.quasigroup.ibclient.client.exceptions

import java.io.IOException

final class InvalidMessageLengthException(message: String)
    extends IOException(message)
