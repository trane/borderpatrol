package com.lookout.borderpatrol

import com.lookout.borderpatrol.sessionx.Types

/**
 * The `crypto` module includes primitives for:
 *  - generating random data [[com.lookout.borderpatrol.crypto.Generator Generator]]
 *  - creating cryptographic signers [[com.lookout.borderpatrol.crypto.Signer Signer]]
 *  - an implementation of encryption for session data [[com.lookout.borderpatrol.crypto.CryptKey CryptKey]]
 *  - an encrypted session store [[com.lookout.borderpatrol.crypto.EncryptedSessionStore EncryptedSessionStore]]
 *
 * It also includes Type Classes for interfacing with backends that would like to encrypt data at rest.
 */
package object crypto extends Types
