"""Binplz DNS update script

Usage: python update_dns.py <root IP address>

Where <root IP address> is the IP address that the A record for the binplz.dev root domain should point to.

This script talks to the transip API to update the binplz.dev settings.
To do so, we
  1. Request an API token, signing the request with our transip account's private key
  2. Request a DNS record update, passing the API token
"""
import random
import requests
import json
import datetime
import base64
import OpenSSL.crypto as ssl
import sys
import ipaddress

ip = ipaddress.ip_address(sys.argv[1])

# API docs: https://api.transip.nl/rest/docs.html
api_root = "https://api.transip.nl/v6"
username = "jonascarpay"

print(f"Starting update of DNS record to {ip}...")

print("Preparing and signing Token request...")

timestamp: str = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
token_request_body = {
    "login": username,
    "nonce": str(random.randint(0, 999999999)),
    "read_only": False,
    "expiration_time": "30 minutes",
    "label": f"Binplz DNS update key {timestamp}",
    "global_key": True,
}
token_request_body_ascii = json.dumps(token_request_body).encode("ascii")

with open("../secrets/plaintext/transip_key.pem", "r") as f:
    private_key: str = f.read().strip()

signature = ssl.sign(
    pkey=ssl.load_privatekey(ssl.FILETYPE_PEM, private_key),
    data=token_request_body_ascii,
    digest="sha512",
)

print("Requesting new API token...")
token_response = requests.post(
    url=api_root + "/auth",
    data=token_request_body_ascii,
    headers={"Signature": base64.b64encode(signature).decode("ascii")},
)

assert token_response.status_code == 201

token = token_response.json()["token"]

dns_update_request_body = {
    "dnsEntries": [
        {
            "name": "@",
            "expire": 86400,  # 1 day
            "type": "A",
            "content": str(ip),
        },
        {
            "name": "docs",
            "expire": 86400,
            "type": "CNAME",
            "content": "binplz.github.io.",
        },
    ]
}

print("Updating DNS Entries...")
dns_update_response = requests.put(
    url=api_root + "/domains/binplz.dev/dns",
    data=json.dumps(dns_update_request_body),
    headers={"Authorization": f"Bearer {token}"},
)

assert dns_update_response.status_code == 204
print("Success!")
