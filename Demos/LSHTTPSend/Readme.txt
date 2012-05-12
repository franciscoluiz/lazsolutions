This solution is ideal for you work with HTTP protocol.

Steps to run this demo with SSL support (to test with HTTPs protocol):

In Windows, copy the DLLs "libeay32.dll" and "ssleay32.dll" from "lazsolutions\media\lib\win32" to this folder, and try run the demo again.

In Linux, install the libs of OpenSSL(*) projetct, and try run the demo again.

(*)
  OpenSSL in openSUSE:
    # zypper in libopenssl-devel 

  OpenSSL in Mint:
    $ sudo apt-get install libssl-dev

  OpenSSL in Fedora:
    $ su
    # cd /usr/lib
    # ln -s libcrypto.so.1.0.0 libcrypto.so
    # ln -s libssl.so.1.0.0 libssl.so

Enjoy!