#!/usr/bin/env python
from django.utils.crypto import pbkdf2
import base64, hashlib
# pbkdf2(password, salt, iterations, dklen=0, digest=None)
hash = pbkdf2("test_password","myHash", 10000, 32, hashlib.sha256)
actual_password='sachin'
salt = "px4WJMvOyTRX"
iters = 12000
dklen=32
pwd = base64.b64encode( pbkdf2(actual_password,salt, iters, dklen, hashlib.sha256))
passwd_stored_in_django='pbkdf2_sha256$12000$px4WJMvOyTRX$6W2lVN8mEqGc1ReeXevuV9v6DZ59OzZiz4jI9XpmDS0='