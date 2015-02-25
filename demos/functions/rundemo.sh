#!/bin/sh

nfloader beat.nf beat beat
nfloader k.nf beat beat
sleep 5
nfloader fnwriter.nf beat beat
sleep 5
nfloader fnreader.nf beat beat
