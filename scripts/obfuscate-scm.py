import os, sys
import re
import base64
import random
import string
import json

DEBUG = False

### Begin RC4 implementation from https://gist.github.com/cdleary/188393 ###
def initialize(key):
    """Produce a 256-entry list based on `key` (a sequence of numbers)
    as the first step in RC4.
    Note: indices in key greater than 255 will be ignored.
    """
    k = list(range(256))
    j = 0
    for i in range(256):
        j = (j + k[i] + key[i % len(key)]) % 256
        k[i], k[j] = k[j], k[i]
    return k

def gen_random_bytes(k):
    """Yield a pseudo-random stream of bytes based on 256-byte array `k`."""
    i = 0
    j = 0
    while True:
        i = (i + 1) % 256
        j = (j + k[i]) % 256
        k[i], k[j] = k[j], k[i]
        yield k[(k[i] + k[j]) % 256]

def run_rc4(k, text):
    cipher_chars = []
    random_byte_gen = gen_random_bytes(k)
    ret = ""
    for char in text:
        byte = ord(char)
        cipher_byte = byte ^ next(random_byte_gen)
        ret += chr(cipher_byte)
    return ret

### End RC4 implementation ###

mangle_map = {}
def mangle(name):
	if name in mangle_map:
		return mangle_map[name]
	key = ''.join(random.choice(string.ascii_lowercase) for x in range(256)) # from https://stackoverflow.com/questions/1957273/how-do-i-generate-a-random-string-of-length-x-a-z-only-in-python
	k = initialize([ord(char) for char in key])
	if not DEBUG:
		mangled = base64.b64encode(bytes(run_rc4(k, name), "utf8")).decode('utf8')
	else:
		mangled = name + "123"
	mangle_map[name] = mangled
	return mangle_map[name]

def strip_empty_lines(data):
	return "\n".join([line for line in data.split("\n") if len(line.strip())])

def transform(data):
	# Strip comments
	data = re.sub(r";.*", "", data, flags=re.MULTILINE)

	# Flatten structure
	if not DEBUG:
		data = data.replace("\n", " ")
		while "  " in data:
			data = data.replace("  ", " ")

	# Perform name mangling for (define) statements
	allNames = set()
	def mangle_define_statement(group):
		name = group.groups()[1]
		allNames.add(name)
		return group.group().replace(name, mangle(name))
	def unmangle_name_ref(group):
		name = group.group()
		if name not in allNames:
			return name
		return mangle(name)
	data = re.sub(r"\((define|set!)\s+([^\(\s\)]+)", mangle_define_statement, data, flags=re.MULTILINE)
	data = re.sub(r"\((define|set!)\s+\(([^\(\s\)]+)[\s\)]", mangle_define_statement, data, flags=re.MULTILINE)

	# Ensure all references to mangled names are consistent
	data = re.sub(r"((?<!\')[^\(\s0-9\)][^\(\s\)]*)", unmangle_name_ref, data, flags=re.MULTILINE)

	# Ensure we know where critical functions are located
	crits = ["derive-infix"]
	for crit in crits:
		assert crit in mangle_map
		data += " (define __mangled__{} '{})".format(crit, mangle(crit))
	# Return transformed input
	return strip_empty_lines(data)


def main():
	global DEBUG
	# Load file given in STDIN
	assert len(sys.argv) > 2, "Input and output filenames are needed"
	DEBUG = "--debug" in sys.argv
	with open(sys.argv[1], 'r') as fp:
		data = fp.read()

	# Transform file data
	data = transform(data)

	# Write new file where specified
	with open(sys.argv[2], 'w') as fp:
		fp.write(data)
	if DEBUG:
		with open('mangled.json', 'w') as fp:
			json.dump(mangle_map, fp)

if __name__ == '__main__':
	main()
