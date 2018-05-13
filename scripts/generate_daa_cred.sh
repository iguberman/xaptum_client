#!/usr/bin/env bash
set -x

ID=$1

## This script relies on following ecdaa executables in your path:

##      ecdaa_member_request_join
##      ecdaa_issuer_respond_to_join_request

NONCE="nonce-$ID"
PK_BIN="MEMBER/pk.bin"
SK_BIN="MEMBER/sk.bin"

## TODO
if [[ -f $PK_BIN && -f $SK_BIN ]]; then
   echo "File $PK_BIN and $SK_BIN exist"
else
    ecdaa_member_request_join $NONCE $PK_BIN $SK_BIN || exit 1
fi

CRED_BIN="MEMBER/cred$1.bin"
CRED_SIG_BIN="MEMBER/cred_sig$1.bin"

## GENERATE CREDENTIAL
ecdaa_issuer_respond_to_join_request $PK_BIN $ISK_BIN $CRED_BIN $CRED_SIGN_BIN $NONCE || exit 1

SIG_BIN="MEMBER/sig$1.bin"
MSG_BIN="MEMBER/message.bin"
BASENAME_BIN="GROUP/basename.bin"
SK_REV_LIST_BIN="MEMBER/sk_revocation_list.bin"
BSN_REV_LIST_BIN="GROUP/bsn_revocation_list.bin"

## SIGN AND VERIFY
ecdaa_member_sign $SK_BIN $CRED_BIN $SIG_BIN $MSG_BIN $BASENAME_BIN || exit 1

SK_REV_LIST_COUNT=`wc -l $SK_REV_LIST_BIN | awk '{ print $1 }'`
BSN_REV_LIST_COUNT=`wc -l $BSN_REV_LIST_BIN | awk '{ print $1 }'`

ecdaa_verify \
$MSG_BIN $SIG_BIN $GPK_BIN \
$SK_REV_LIST_BIN $SK_REV_LIST_COUNT \
$BSN_REV_LIST_BIN $BSN_REV_LIST_COUNT \
$BASENAME_BIN  || exit 1