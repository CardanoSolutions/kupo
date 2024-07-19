#!/usr/bin/env bash 

for nixlib in $(otool -L "$1" | awk '/nix\/store/{ print $1 }'); do
    case "$nixlib" in
    *libiconv.dylib) install_name_tool -change "$nixlib" /usr/lib/libiconv.dylib "$1" ;;
    *libffi.*.dylib) install_name_tool -change "$nixlib" /usr/lib/libffi.dylib   "$1" ;;
    *libc++.*.dylib) install_name_tool -change "$nixlib" /usr/lib/libc++.dylib   "$1" ;;
    *libz.dylib)     install_name_tool -change "$nixlib" /usr/lib/libz.dylib     "$1" ;;
    *) ;;
    esac
done
