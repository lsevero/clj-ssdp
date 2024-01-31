(ns clj-ssdp.client
  (:import [java.net DatagramPacket DatagramSocket
            InetAddress SocketTimeoutException])
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [clojure.repl :as repl]
            ))

(defn parse-device
  [device]
  (let [lines (->> (str/split device #"\r\n")
                   (remove empty?))]
    (->> (mapv (fn [line] (let [match (re-find #"(.*): (.*)" line)]
                            (when (= 3 (count match))
                              [(match 1) (match 2)])
                            )) lines)
         (remove nil?)
         (into {})
         walk/keywordize-keys)))

(comment (parse-device "HTTP/1.1 200 OK\r\nST: urn:schemas-upnp-org:device:Basic:1\r\nUSN: uuid:d541c15f-a2fc-42de-8d1e-1e4432767487::urn:schemas-upnp-org:device:Basic:1\r\nLOCATION: http://192.168.1.40/path/to/description.xml\r\nCACHE-CONTROL: max-age=1800\r\nDATE: Mon, 23 Dec 2019 01:53:17 GMT\r\nSERVER: node.js/13.5.0 UPnP/1.1 @achingbrain/ssdp/2.1.2\r\nEXT: \r\n\r\n"))

(defn discover-one
  ([^long timeout ^String search-target]
   {:pre [(pos-int? timeout)]}
   (let [msearch (str "M-SEARCH * HTTP/1.1\nHost: 239.255.255.250:1900\nMAN: \"ssdp:discover\"\nST: " search-target "\n")
         msearch (str msearch "MX: " (long (/ timeout 1000)) "\n\r\n")
         send-data (.getBytes msearch)
         receive-data (byte-array 1024)
         ^DatagramPacket send-packet (DatagramPacket. send-data (alength send-data) (InetAddress/getByName "239.255.255.250") 1900)
         ^DatagramSocket client-socket (doto (DatagramSocket.)
                                         (.setSoTimeout timeout)
                                         (.send send-packet))]
     (try
       (let [^DatagramPacket receive-packet (DatagramPacket. receive-data (alength receive-data))
             _ (.receive client-socket receive-packet)
             device (String. (.getData receive-packet))]
         (if (= search-target "ssdp:all")
           (parse-device device)
           (when (.contains device search-target)
             (parse-device device))))
       (catch SocketTimeoutException e (do
                                         (.close client-socket)
                                         nil)))))
  ([^long timeout]
   (discover-one timeout "ssdp:all"))
  ([]
   (discover-one 100 "ssdp:all")))

(defn discover
  ([^long timeout ^String search-target]
   {:pre [(pos-int? timeout)]}
   (let [msearch (str "M-SEARCH * HTTP/1.1\nHost: 239.255.255.250:1900\nMAN: \"ssdp:discover\"\nST: " search-target "\n")
         msearch (str msearch "MX: " (long (/ timeout 1000)) "\n\r\n")
         ^bytes send-data (.getBytes msearch)
         ^bytes receive-data (byte-array 1024)
         ^DatagramPacket send-packet (DatagramPacket. send-data (alength send-data) (InetAddress/getByName "239.255.255.250") 1900)
         ^DatagramSocket client-socket (doto (DatagramSocket.)
                                         (.setSoTimeout timeout)
                                         (.send send-packet))]
     (letfn [(get-devices
               ([] (get-devices []))
               ([devices]
                (try
                  (let [^DatagramPacket receive-packet (DatagramPacket. receive-data (alength receive-data))
                        _ (.receive client-socket receive-packet)
                        device  (-> (.getData receive-packet)
                                    String.)]
                    (do
                      (if (.contains device search-target)
                        (get-devices (conj devices (parse-device device)))
                        (if (= search-target "ssdp:all")
                          (get-devices (conj devices (parse-device device)))
                          (get-devices devices)))))
                  (catch SocketTimeoutException e (do
                                                    (.close client-socket)
                                                    devices)))))]
       (get-devices))))

  ([^long timeout]
   (discover timeout "ssdp:all"))
  ([]
   (discover 100 "ssdp:all")))

(comment 
  (discover)
  (discover 1000 "urn:schemas-upnp-org:device:MediaRenderer:1")
  (discover 100 "urn:schemas-upnp-org:device:Basic:1")
  )

