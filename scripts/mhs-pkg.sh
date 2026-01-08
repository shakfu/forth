#!/usr/bin/env bash

# Note -P <entry> must be of form <name>-<semver>

PKG_VER=0.1.0
PKG_NAME=music
THIRDPARTY=`pwd`/thirdparty
MHS_MIDI=`pwd`/projects/mhs-midi

MHSDIR=${THIRDPARTY}/MicroHs ${THIRDPARTY}/MicroHs/bin/mhs -P${PKG_NAME}-${PKG_VER} -i${MHS_MIDI}/lib \
	Async Midi MidiPerform Music MusicPerform \
	-o ${PKG_NAME}-${PKG_VER}.pkg
