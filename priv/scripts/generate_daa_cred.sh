#!/usr/bin/env bash
set -x

## This script generates credentials for a single endpoint
## It can be used on its own, or run as dependency of generate_multi_daa_creds.sh script
## which takes care of generating group stuff (common for multiple endpoints)
#  It relies on the following:
#  1. ecdaa executables are in your path:
##      ecdaa_member_request_join
##      ecdaa_issuer_respond_to_join_request

## 2. GROUP_DIR is already populated with isk.bin adn gpk.bin (most likely but not necessarily
#     by generate_multi_daa_creds.sh scripts)
##  If they are not, this script should simply be run as dependency of generate_multi_daa_creds.sh
##  which takes care of generating group stuff.

## 3. The following files are populated
##    (this would be always true if run as dependency of generate_multi_daa_creds.sh):
##    $BASEDIR/GROUP/message.bin"
##    $BASEDIR/GROUP/basename.bin"
##    $BASEDIR/GROUP/sk_revocation_list.bin"
##    $BASEDIR/GROUP/bsn_revocation_list.bin"


ID=$1
BASEDIR=${2:-`pwd`}

MEMBER_DIR="$BASEDIR/MEMBER$ID"

GROUP_DIR="$BASEDIR/GROUP"

mkdir -p $MEMBER_DIR

mkdir -p $GROUP_DIR


NONCE="nonce-$ID"
PK_BIN="$MEMBER_DIR/pk.bin"
SK_BIN="$MEMBER_DIR/sk.bin"

member_request_join $NONCE $PK_BIN $SK_BIN || exit 1

CRED_BIN="$MEMBER_DIR/cred.bin"
CRED_SIG_BIN="$MEMBER_DIR/cred_sig.bin"
ISK_BIN="$GROUP_DIR/isk.bin"
GPK_BIN="$GROUP_DIR/gpk.bin"

## GENERATE CREDENTIAL
issuer_respond_to_join_request $PK_BIN $ISK_BIN $CRED_BIN $CRED_SIG_BIN $NONCE || exit 1

SIG_BIN="$MEMBER_DIR/sig.bin"
MSG_BIN="$GROUP_DIR/message.bin"
BASENAME_BIN="$GROUP_DIR/basename.bin"
SK_REV_LIST_BIN="$GROUP_DIR/sk_revocation_list.bin"
BSN_REV_LIST_BIN="$GROUP_DIR/bsn_revocation_list.bin"

## SIGN AND VERIFY
member_sign $SK_BIN $CRED_BIN $SIG_BIN $MSG_BIN $BASENAME_BIN || exit 1

SK_REV_LIST_COUNT=`wc -l $SK_REV_LIST_BIN | awk '{ print $1 }'`
BSN_REV_LIST_COUNT=`wc -l $BSN_REV_LIST_BIN | awk '{ print $1 }'`

verify \
$MSG_BIN $SIG_BIN $GPK_BIN \
$SK_REV_LIST_BIN $SK_REV_LIST_COUNT \
$BSN_REV_LIST_BIN $BSN_REV_LIST_COUNT \
$BASENAME_BIN  || exit 1