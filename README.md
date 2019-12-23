# clj-ssdp

[![Clojars Project](https://img.shields.io/clojars/v/clj-ssdp.svg)](https://clojars.org/clj-ssdp)

## Usage

```clojure
(require '[clj-ssdp.client :refer :all])

;discover all the ssdp devices on your network
(discover)
;you can set a timeout in milliseconds
(discover 1000)
;you can set a timeout and a search target as well
(discover 1000 "urn:schemas-upnp-org:device:MediaRenderer:1")


;same thing for discover-one, discover-one returns the first ssdp device
(discover-one)
(discover-one 1000)
(discover-one 1000 "urn:schemas-upnp-org:device:MediaRenderer:1")
```

## License

Copyright © 2019 FIXME

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
