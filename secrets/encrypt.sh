#!/usr/bin/env bash
set -euxo pipefail

secrets_root="$(dirname -- "$0")"
encrypted_dir="$secrets_root/encrypted"
plaintext_dir="$secrets_root/plaintext"

[ -f "$secrets_root/trustees" ]
[ -d "$plaintext_dir" ]

rm $encrypted_dir/*

for file in $(cd $plaintext_dir && find -type f); do
  age --encrypt --recipients-file "$secrets_root/trustees" -o "$encrypted_dir/$file" "$plaintext_dir/$file"
done
