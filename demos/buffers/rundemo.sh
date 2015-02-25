#!/bin/sh

nfloader hello.nf buf0 buf1
nfloader world.nf buf0 buf1
sleep 2
nfloader hello.nf buf1 buf0
nfloader world.nf buf1 buf0

