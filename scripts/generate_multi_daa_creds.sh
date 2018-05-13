#!/usr/bin/env bash

CREDS_RANGE_START=$1
CREDS_RANGE_END=$2

## This script relies on following ecdaa executables in your path:

##      OPTIONAL:
##      (needed if files GROUP/isk.bin and GROUP/gpk.bin aren't already populated
##      which will be populated the first time this script is run or by explicitly copying them here)
##      ecdaa_issuer_create_group
##      ecdaa_extract_group_public_key

GPK_BIN="GROUP/gpk.bin"
ISK_BIN="GROUP/isk.bin"

if [[ -f $ISK_BIN && -f $GPK_BIN ]]; then
    echo "File $ISK_BIN and $GPK_BIN already exist"
else
    IPK_BIN="GROUP/ipk.bin"
    ## Create ISSUER public and private key
    ecdaa_issuer_create_group $IPK_BIN $ISK_BIN || exit 1
    ## Extract group public key
    ecdaa_extract_group_public_key $IPK_BIN $GPK_BIN || exit 1
    rm $IPK_BIN
    echo "Successfully generated $ISK_BIN and $GPK_BIN"
fi


for i in $(seq $CREDS_RANGE_START $CREDS_RANGE_END);
do echo "Generating cred $i"
./generate_daa_cred.sh $i || exit 1
done

