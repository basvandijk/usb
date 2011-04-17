This library allows you to communicate with USB devices from
userspace. It is implemented as a high-level wrapper around
[bindings-libusb] which is a low-level binding to the C library:
[libusb-1.*].

This documentation assumes knowledge of how to operate USB devices
from a software standpoint (descriptors, configurations, interfaces,
endpoints, control/bulk/interrupt/isochronous transfers, etc). Full
information can be found in the [USB 2.0 Specification][USB-2.0-spec].

For an example how to use this library see the [ls-usb] package.

Also see the [usb-safe] package which wraps this package and provides
some strong safety guarantees for working with USB devices.

Finally have a look at the [usb-enumerator] package which provides
iteratee enumerators for enumerating bulk and interrupt endpoints.

Besides the API documentation the following sources might be
interesting:

* [The libusb 1.0 documentation](http://libusb.sourceforge.net/api-1.0/)

* [The USB 2.0 specification][USB-2.0-spec]

* [The `bindings-libusb` documentation][bindings-libusb]

* ["USB in a NutShell"](http://www.beyondlogic.org/usbnutshell/usb1.htm)
 
[bindings-libusb]: http://hackage.haskell.org/package/bindings-libusb
[ls-usb]:          http://hackage.haskell.org/package/ls-usb
[usb-safe]:        http://hackage.haskell.org/package/usb-safe         
[usb-enumerator]:  http://hackage.haskell.org/package/usb-enumerator
[libusb-1.*]:      http://libusb.org/wiki/libusb-1.0
[USB-2.0-spec]:    http://www.usb.org/developers/docs/
