#!/usr/bin/env python

from urllib.request import FancyURLopener

email = 'Email Enter Here' # @gmail.com can be left out
password  = 'Password Enter Here'

url = 'https://%s:%s@mail.google.com/mail/feed/atom' % (email, password)

opener = FancyURLopener()
page = opener.open(url)

contents = page.read().decode('utf-8')

ifrom = contents.index('<fullcount>') + 11
ito   = contents.index('</fullcount>')

fullcount = contents[ifrom:ito]

print(fullcount)
