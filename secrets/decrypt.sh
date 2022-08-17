#!/usr/bin/env bash
set -euxo pipefail

secrets_root="$(dirname -- "$0")"
encrypted_dir="$secrets_root/encrypted"
plaintext_dir="$secrets_root/plaintext"

[ -f "$secrets_root/trustees" ]
[ -d "$encrypted_dir" ]

mkdir -p $plaintext_dir
rm -f $plaintext_dir/*

for file in $(cd $encrypted_dir && find -type f); do
  age --decrypt $@ -o "$plaintext_dir/$file" "$encrypted_dir/$file"
done
