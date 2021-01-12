#!/bin/bash
#> @file
#> @brief Test script to test gettab.x utility
#> @author aerorahul @date 2020-12-20

set -eu

cmd=$1
outfile=$2
reffile=$3

rc="-1"
$cmd > $outfile && cmp $outfile $reffile
rc=${?}

exit $rc

