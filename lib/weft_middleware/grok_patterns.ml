(* Standard grok pattern library — compiled to regex *)

let base_patterns = [
  "USERNAME", {|[a-zA-Z0-9._-]+|};
  "USER", {|%{USERNAME}|};
  "INT", {|(?:[+-]?(?:[0-9]+))|};
  "BASE10NUM", {|(?:[+-]?(?:(?:[0-9]+(?:\.[0-9]+)?)|\.[0-9]+))|};
  "NUMBER", {|(?:%{BASE10NUM})|};
  "BASE16NUM", {|(?:0[xX]?[0-9a-fA-F]+)|};
  "BASE16FLOAT", {|(?:\b(?:[+-]?(?:0x)?(?:(?:[0-9a-fA-F]+(?:\.[0-9a-fA-F]*)?)|\.[0-9a-fA-F]+)(?:[pP][+-]?[0-9]+)?)\b)|};
  "POSINT", {|(?:[1-9][0-9]*)|};
  "NONNEGINT", {|(?:[0-9]+)|};
  "WORD", {|\b\w+\b|};
  "NOTSPACE", {|\S+|};
  "SPACE", {|\s*|};
  "DATA", {|.*?|};
  "GREEDYDATA", {|.*|};
  "QUOTEDSTRING", {|"(?:[^"\\]|\\.)*"|'(?:[^'\\]|\\.)*'|};
  "UUID", {|[A-Fa-f0-9]{8}-(?:[A-Fa-f0-9]{4}-){3}[A-Fa-f0-9]{12}|};

  (* Networking *)
  "MAC", {|(?:%{CISCOMAC}|%{WINDOWSMAC}|%{COMMONMAC})|};
  "CISCOMAC", {|(?:(?:[A-Fa-f0-9]{4}\.){2}[A-Fa-f0-9]{4})|};
  "WINDOWSMAC", {|(?:(?:[A-Fa-f0-9]{2}-){5}[A-Fa-f0-9]{2})|};
  "COMMONMAC", {|(?:(?:[A-Fa-f0-9]{2}:){5}[A-Fa-f0-9]{2})|};
  "IPV6", {|(?:(?:[0-9A-Fa-f]{1,4}:){7}[0-9A-Fa-f]{1,4}|(?:[0-9A-Fa-f]{1,4}:){1,7}:|::(?:[0-9A-Fa-f]{1,4}:){0,5}[0-9A-Fa-f]{1,4})|};
  "IPV4", {|(?:(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?))|};
  "IP", {|(?:%{IPV6}|%{IPV4})|};
  "HOSTNAME", {|\b(?:[0-9A-Za-z][0-9A-Za-z-]{0,62})(?:\.(?:[0-9A-Za-z][0-9A-Za-z-]{0,62}))*(?:\.?|\b)|};
  "HOST", {|%{HOSTNAME}|};
  "IPORHOST", {|(?:%{IP}|%{HOSTNAME})|};
  "HOSTPORT", {|%{IPORHOST}:%{POSINT}|};

  (* Paths *)
  "PATH", {|(?:%{UNIXPATH}|%{WINPATH})|};
  "UNIXPATH", {|(?:/[\w_%!$@:.,-]?/[\w_%!$@:.,~-]*)+|};
  "WINPATH", {|(?:[A-Za-z]+:|\\)(?:\\[^\\?*]*)+|};
  "TTY", {|(?:/dev/(?:pts|tty(?:[pq])?)(?:\w+)?/?(?:[0-9]+))|};
  "URIPROTO", {|[A-Za-z]+(?:\+[A-Za-z+]+)?|};
  "URIHOST", {|%{IPORHOST}(?::%{POSINT:port})?|};
  "URIPATH", {|(?:/[A-Za-z0-9$.+!*'(){},~:;=@#%_\-]*)+|};
  "URIPARAM", {|\?[A-Za-z0-9$.+!*'|(){},~@#%&/=:;_?\-\[\]<>]*|};
  "URIPATHPARAM", {|%{URIPATH}(?:%{URIPARAM})?|};
  "URI", {|%{URIPROTO}://(?:%{USER}(?::[^@]*)?@)?(?:%{URIHOST})?(?:%{URIPATH}(?:%{URIPARAM})?)?|};

  (* Dates *)
  "MONTH", {|(?:Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|Apr(?:il)?|May|Jun(?:e)?|Jul(?:y)?|Aug(?:ust)?|Sep(?:tember)?|Oct(?:ober)?|Nov(?:ember)?|Dec(?:ember)?)|};
  "MONTHNUM", {|(?:0[1-9]|1[0-2])|};
  "MONTHDAY", {|(?:(?:0[1-9])|(?:[12][0-9])|(?:3[01])|[1-9])|};
  "DAY", {|(?:Mon(?:day)?|Tue(?:sday)?|Wed(?:nesday)?|Thu(?:rsday)?|Fri(?:day)?|Sat(?:urday)?|Sun(?:day)?)|};
  "YEAR", {|(?:\d\d){1,2}|};
  "HOUR", {|(?:2[0123]|[01]?[0-9])|};
  "MINUTE", {|(?:[0-5][0-9])|};
  "SECOND", {|(?:(?:[0-5]?[0-9]|60)(?:[:.,][0-9]+)?)|};
  "TIME", {|(?:%{HOUR}:%{MINUTE}(?::%{SECOND}))|};
  "DATE_US", {|%{MONTHNUM}[/-]%{MONTHDAY}[/-]%{YEAR}|};
  "DATE_EU", {|%{MONTHDAY}[./-]%{MONTHNUM}[./-]%{YEAR}|};
  "ISO8601_TIMEZONE", {|(?:Z|[+-]%{HOUR}(?::?%{MINUTE}))|};
  "ISO8601_SECOND", {|(?:%{SECOND}|60)|};
  "TIMESTAMP_ISO8601", {|%{YEAR}-%{MONTHNUM}-%{MONTHDAY}[T ]%{HOUR}:?%{MINUTE}(?::?%{SECOND})?%{ISO8601_TIMEZONE}?|};
  "DATE", {|%{DATE_US}|%{DATE_EU}|};
  "DATESTAMP", {|%{DATE}[- ]%{TIME}|};
  "TZ", {|(?:[PMCE][SD]T|UTC)|};
  "DATESTAMP_RFC822", {|%{DAY} %{MONTH} %{MONTHDAY} %{YEAR} %{TIME} %{TZ}|};
  "DATESTAMP_RFC2822", {|%{DAY}, %{MONTHDAY} %{MONTH} %{YEAR} %{TIME} %{ISO8601_TIMEZONE}|};
  "DATESTAMP_OTHER", {|%{DAY} %{MONTH} %{MONTHDAY} %{TIME} %{TZ} %{YEAR}|};
  "DATESTAMP_EVENTLOG", {|%{YEAR}%{MONTHNUM}%{MONTHDAY}%{HOUR}%{MINUTE}%{SECOND}|};

  (* Syslog *)
  "SYSLOGTIMESTAMP", {|%{MONTH} +%{MONTHDAY} %{TIME}|};
  "PROG", {|[\x21-\x5a\x5c\x5e-\x7e]+|};
  "SYSLOGPROG", {|%{PROG:program}(?:\[%{POSINT:pid}\])?|};
  "SYSLOGHOST", {|%{IPORHOST}|};
  "SYSLOGFACILITY", {|<%{NONNEGINT:facility}.%{NONNEGINT:priority}>|};
  "HTTPDATE", {|%{MONTHDAY}/%{MONTH}/%{YEAR}:%{TIME} %{INT}|};

  (* Log levels *)
  "LOGLEVEL", {|(?:[Aa]lert|ALERT|[Tt]race|TRACE|[Dd]ebug|DEBUG|[Nn]otice|NOTICE|[Ii]nfo|INFO|[Ww]arn(?:ing)?|WARN(?:ING)?|[Ee]rr(?:or)?|ERR(?:OR)?|[Cc]rit(?:ical)?|CRIT(?:ICAL)?|[Ff]atal|FATAL|[Ss]evere|SEVERE|EMERG(?:ENCY)?|[Ee]merg(?:ency)?)|};
]

let pattern_table : (string, string) Hashtbl.t =
  let tbl = Hashtbl.create 128 in
  List.iter (fun (name, pat) -> Hashtbl.replace tbl name pat) base_patterns;
  tbl

let lookup name =
  Hashtbl.find_opt pattern_table name
