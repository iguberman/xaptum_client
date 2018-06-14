#!/usr/bin/env bash

CREDS_RANGE_START=$1
CREDS_RANGE_END=$2

BASEDIR=${3:-`pwd`}
GROUP_DIR="$BASEDIR/GROUP"
mkdir -p $GROUP_DIR

CUR_DIR="$(dirname "$0")"

## This script relies on following ecdaa executables in your path:

##      OPTIONAL:
##      (needed if files GROUP/isk.bin and GROUP/gpk.bin aren't already populated
##      which will be populated the first time this script is run or by explicitly copying them here)
##      ecdaa_issuer_create_group
##      ecdaa_extract_group_public_key


GPK_BIN="$GROUP_DIR/gpk.bin"
ISK_BIN="$GROUP_DIR/isk.bin"

if [[ -f $ISK_BIN && -f $GPK_BIN ]]; then
    echo "File $ISK_BIN and $GPK_BIN already exist"
else
    IPK_BIN="$GROUP_DIR/ipk.bin"
    ## Create ISSUER public and private key
    ecdaa_issuer_create_group $IPK_BIN $ISK_BIN || exit 1
    ## Extract group public key
    ecdaa_extract_group_public_key $IPK_BIN $GPK_BIN || exit 1
    rm $IPK_BIN
    echo "Successfully generated $ISK_BIN and $GPK_BIN"
fi

MSG_BIN="$GROUP_DIR/message.bin"
BASENAME_BIN="$GROUP_DIR/basename.bin"
SK_REV_LIST_BIN="$GROUP_DIR/sk_revocation_list.bin"
BSN_REV_LIST_BIN="$GROUP_DIR/bsn_revocation_list.bin"

## Copy default files to requested BASEDIR if it doesn't have them already

if [ ! -f $MSG_BIN ]; then
    cp "$CUR_DIR/DEFAULTS/message.bin" $MSG_BIN
fi

if [ ! -f $BASENAME_BIN ]; then
    cp "$CUR_DIR/DEFAULTS/basename.bin" $BASENAME_BIN
fi

if [ ! -f $SK_REV_LIST_BIN ]; then
    cp "$CUR_DIR/DEFAULTS/sk_revocation_list.bin" $SK_REV_LIST_BIN
fi

if [ ! -f $BSN_REV_LIST_BIN ]; then
    cp "$CUR_DIR/DEFAULTS/bsn_revocation_list.bin" $BSN_REV_LIST_BIN
fi


for i in $(seq $CREDS_RANGE_START $CREDS_RANGE_END);
do echo "Generating cred $i"
"$CUR_DIR"/generate_daa_cred.sh $i $BASEDIR || exit 1
done

