;;; Copyright © 2015  Amirouche Boubekki <amirouche@hypermove.net>
;;; Copyright © 2014  David Thompson <davet@gnu.org>
;;;
;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU Affero General Public License
;;; as published by the Free Software Foundation, either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Affero General Public License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.
(define-module (hypermove))

(use-modules (srfi srfi-1))
(use-modules (srfi srfi-19))
(use-modules (srfi srfi-26))

(use-modules (rnrs bytevectors))

(use-modules (ice-9 iconv))
(use-modules (ice-9 match))
(use-modules (ice-9 receive))
(use-modules (ice-9 binary-ports))

(use-modules (web server))
(use-modules (web http))
(use-modules (web request))
(use-modules (web response))
(use-modules (web server))
(use-modules (web uri))

(use-modules (sxml simple))
(use-modules ((sxml xpath) #:select (sxpath)))

(use-modules ((htmlprag) #:select (html->sxml)))

(use-modules (html))
(use-modules (text))
(use-modules (http))

(use-modules (wiredtiger))
(use-modules (wiredtigerz))
(use-modules (wsh))

(use-modules (ice-9 hash-table))
(use-modules (ice-9 regex))


(setlocale LC_ALL "")


(define (string-replace string char value)
  (string-map (lambda (<>) (if (eq? <> char) value <>)) string))


(define (spacy string) (string-replace string #\+ #\space))


(define (hex->char a b)
  (integer->char (string->number (list->string (list a b)) 16)))


(define (unhex string)
  (if (not (string-index string #\%))
      string
      (let loop ((chars (string->list string))
                 (out '()))
        (match chars
          ((#\% a b . rest) (loop rest (cons (hex->char a b) out)))
          ((a . rest) (loop rest (cons a out)))
          (() (list->string (reverse out)))))))

(define decode (compose unhex spacy))

(define (acons-list k v alist)
  (let ((value (assoc-ref alist k)))
    (if value
        (let ((alist (alist-delete k alist)))
          (acons k (cons v value) alist))
        (acons k (list v) alist))))

(define (list->alist lst)
  (let next ((lst lst)
             (out '()))
    (if (null? lst)
        out
        (next (cdr lst) (acons-list (caar lst) (cdar lst) out)))))


(define-public (querystring bv)
  ;; semi-colon and amp can be used as pair separator
  (define string (utf8->string bv))
  (define pairs (map (cut string-split <> #\=)
                     (append-map (cut string-split <> #\;) (string-split string #\&))))
  (list->alist (map (match-lambda ((key value) (cons (decode key) (decode value)))) pairs)))



(define %mime-types
  (alist->hash-table
   '(("ez" . application/andrew-inset)
     ("anx" . application/annodex)
     ("atom" . application/atom+xml)
     ("atomcat" . application/atomcat+xml)
     ("atomsrv" . application/atomserv+xml)
     ("lin" . application/bbolin)
     ("cap" . application/cap)
     ("pcap" . application/cap)
     ("cu" . application/cu-seeme)
     ("davmount" . application/davmount+xml)
     ("tsp" . application/dsptype)
     ("es" . application/ecmascript)
     ("spl" . application/futuresplash)
     ("hta" . application/hta)
     ("jar" . application/java-archive)
     ("ser" . application/java-serialized-object)
     ("class" . application/java-vm)
     ("js" . application/javascript)
     ("m3g" . application/m3g)
     ("hqx" . application/mac-binhex40)
     ("cpt" . application/mac-compactpro)
     ("nb" . application/mathematica)
     ("nbp" . application/mathematica)
     ("mdb" . application/msaccess)
     ("doc" . application/msword)
     ("dot" . application/msword)
     ("mxf" . application/mxf)
     ("bin" . application/octet-stream)
     ("oda" . application/oda)
     ("ogx" . application/ogg)
     ("pdf" . application/pdf)
     ("key" . application/pgp-keys)
     ("pgp" . application/pgp-signature)
     ("prf" . application/pics-rules)
     ("ps" . application/postscript)
     ("ai" . application/postscript)
     ("eps" . application/postscript)
     ("epsi" . application/postscript)
     ("epsf" . application/postscript)
     ("eps2" . application/postscript)
     ("eps3" . application/postscript)
     ("rar" . application/rar)
     ("rdf" . application/rdf+xml)
     ("rss" . application/rss+xml)
     ("rtf" . application/rtf)
     ("smi" . application/smil)
     ("smil" . application/smil)
     ("xhtml" . application/xhtml+xml)
     ("xht" . application/xhtml+xml)
     ("xml" . application/xml)
     ("xsl" . application/xml)
     ("xsd" . application/xml)
     ("xspf" . application/xspf+xml)
     ("zip" . application/zip)
     ("apk" . application/vnd.android.package-archive)
     ("cdy" . application/vnd.cinderella)
     ("kml" . application/vnd.google-earth.kml+xml)
     ("kmz" . application/vnd.google-earth.kmz)
     ("xul" . application/vnd.mozilla.xul+xml)
     ("xls" . application/vnd.ms-excel)
     ("xlb" . application/vnd.ms-excel)
     ("xlt" . application/vnd.ms-excel)
     ("cat" . application/vnd.ms-pki.seccat)
     ("stl" . application/vnd.ms-pki.stl)
     ("ppt" . application/vnd.ms-powerpoint)
     ("pps" . application/vnd.ms-powerpoint)
     ("odc" . application/vnd.oasis.opendocument.chart)
     ("odb" . application/vnd.oasis.opendocument.database)
     ("odf" . application/vnd.oasis.opendocument.formula)
     ("odg" . application/vnd.oasis.opendocument.graphics)
     ("otg" . application/vnd.oasis.opendocument.graphics-template)
     ("odi" . application/vnd.oasis.opendocument.image)
     ("odp" . application/vnd.oasis.opendocument.presentation)
     ("otp" . application/vnd.oasis.opendocument.presentation-template)
     ("ods" . application/vnd.oasis.opendocument.spreadsheet)
     ("ots" . application/vnd.oasis.opendocument.spreadsheet-template)
     ("odt" . application/vnd.oasis.opendocument.text)
     ("odm" . application/vnd.oasis.opendocument.text-master)
     ("ott" . application/vnd.oasis.opendocument.text-template)
     ("oth" . application/vnd.oasis.opendocument.text-web)
     ("xlsx" . application/vnd.openxmlformats-officedocument.spreadsheetml.sheet)
     ("xltx" . application/vnd.openxmlformats-officedocument.spreadsheetml.template)
     ("pptx" . application/vnd.openxmlformats-officedocument.presentationml.presentation)
     ("ppsx" . application/vnd.openxmlformats-officedocument.presentationml.slideshow)
     ("potx" . application/vnd.openxmlformats-officedocument.presentationml.template)
     ("docx" . application/vnd.openxmlformats-officedocument.wordprocessingml.document)
     ("dotx" . application/vnd.openxmlformats-officedocument.wordprocessingml.template)
     ("cod" . application/vnd.rim.cod)
     ("mmf" . application/vnd.smaf)
     ("sdc" . application/vnd.stardivision.calc)
     ("sds" . application/vnd.stardivision.chart)
     ("sda" . application/vnd.stardivision.draw)
     ("sdd" . application/vnd.stardivision.impress)
     ("sdf" . application/vnd.stardivision.math)
     ("sdw" . application/vnd.stardivision.writer)
     ("sgl" . application/vnd.stardivision.writer-global)
     ("sxc" . application/vnd.sun.xml.calc)
     ("stc" . application/vnd.sun.xml.calc.template)
     ("sxd" . application/vnd.sun.xml.draw)
     ("std" . application/vnd.sun.xml.draw.template)
     ("sxi" . application/vnd.sun.xml.impress)
     ("sti" . application/vnd.sun.xml.impress.template)
     ("sxm" . application/vnd.sun.xml.math)
     ("sxw" . application/vnd.sun.xml.writer)
     ("sxg" . application/vnd.sun.xml.writer.global)
     ("stw" . application/vnd.sun.xml.writer.template)
     ("sis" . application/vnd.symbian.install)
     ("vsd" . application/vnd.visio)
     ("wbxml" . application/vnd.wap.wbxml)
     ("wmlc" . application/vnd.wap.wmlc)
     ("wmlsc" . application/vnd.wap.wmlscriptc)
     ("wpd" . application/vnd.wordperfect)
     ("wp5" . application/vnd.wordperfect5.1)
     ("wk" . application/x-123)
     ("7z" . application/x-7z-compressed)
     ("bz2" . application/x-bzip2)
     ("gz" . application/x-gzip)
     ("abw" . application/x-abiword)
     ("dmg" . application/x-apple-diskimage)
     ("bcpio" . application/x-bcpio)
     ("torrent" . application/x-bittorrent)
     ("cab" . application/x-cab)
     ("cbr" . application/x-cbr)
     ("cbz" . application/x-cbz)
     ("cdf" . application/x-cdf)
     ("cda" . application/x-cdf)
     ("vcd" . application/x-cdlink)
     ("pgn" . application/x-chess-pgn)
     ("cpio" . application/x-cpio)
     ("csh" . application/x-csh)
     ("deb" . application/x-debian-package)
     ("udeb" . application/x-debian-package)
     ("dcr" . application/x-director)
     ("dir" . application/x-director)
     ("dxr" . application/x-director)
     ("dms" . application/x-dms)
     ("wad" . application/x-doom)
     ("dvi" . application/x-dvi)
     ("rhtml" . application/x-httpd-eruby)
     ("pfa" . application/x-font)
     ("pfb" . application/x-font)
     ("gsf" . application/x-font)
     ("pcf" . application/x-font)
     ("pcf.Z" . application/x-font)
     ("mm" . application/x-freemind)
     ("spl" . application/x-futuresplash)
     ("gnumeric" . application/x-gnumeric)
     ("sgf" . application/x-go-sgf)
     ("gcf" . application/x-graphing-calculator)
     ("gtar" . application/x-gtar)
     ("tgz" . application/x-gtar)
     ("taz" . application/x-gtar)
     ("tar.gz" . application/x-gtar)
     ("tar.bz2" . application/x-gtar)
     ("tbz2" . application/x-gtar)
     ("hdf" . application/x-hdf)
     ("phtml" . application/x-httpd-php)
     ("pht" . application/x-httpd-php)
     ("php" . application/x-httpd-php)
     ("phps" . application/x-httpd-php-source)
     ("php3" . application/x-httpd-php3)
     ("php3p" . application/x-httpd-php3-preprocessed)
     ("php4" . application/x-httpd-php4)
     ("php5" . application/x-httpd-php5)
     ("ica" . application/x-ica)
     ("info" . application/x-info)
     ("ins" . application/x-internet-signup)
     ("isp" . application/x-internet-signup)
     ("iii" . application/x-iphone)
     ("iso" . application/x-iso9660-image)
     ("jam" . application/x-jam)
     ("jnlp" . application/x-java-jnlp-file)
     ("jmz" . application/x-jmol)
     ("chrt" . application/x-kchart)
     ("kil" . application/x-killustrator)
     ("skp" . application/x-koan)
     ("skd" . application/x-koan)
     ("skt" . application/x-koan)
     ("skm" . application/x-koan)
     ("kpr" . application/x-kpresenter)
     ("kpt" . application/x-kpresenter)
     ("ksp" . application/x-kspread)
     ("kwd" . application/x-kword)
     ("kwt" . application/x-kword)
     ("latex" . application/x-latex)
     ("lha" . application/x-lha)
     ("lyx" . application/x-lyx)
     ("lzh" . application/x-lzh)
     ("lzx" . application/x-lzx)
     ("frm" . application/x-maker)
     ("maker" . application/x-maker)
     ("frame" . application/x-maker)
     ("fm" . application/x-maker)
     ("fb" . application/x-maker)
     ("book" . application/x-maker)
     ("fbdoc" . application/x-maker)
     ("mif" . application/x-mif)
     ("wmd" . application/x-ms-wmd)
     ("wmz" . application/x-ms-wmz)
     ("com" . application/x-msdos-program)
     ("exe" . application/x-msdos-program)
     ("bat" . application/x-msdos-program)
     ("dll" . application/x-msdos-program)
     ("msi" . application/x-msi)
     ("nc" . application/x-netcdf)
     ("pac" . application/x-ns-proxy-autoconfig)
     ("dat" . application/x-ns-proxy-autoconfig)
     ("nwc" . application/x-nwc)
     ("o" . application/x-object)
     ("oza" . application/x-oz-application)
     ("p7r" . application/x-pkcs7-certreqresp)
     ("crl" . application/x-pkcs7-crl)
     ("pyc" . application/x-python-code)
     ("pyo" . application/x-python-code)
     ("qgs" . application/x-qgis)
     ("shp" . application/x-qgis)
     ("shx" . application/x-qgis)
     ("qtl" . application/x-quicktimeplayer)
     ("rpm" . application/x-redhat-package-manager)
     ("rb" . application/x-ruby)
     ("sh" . application/x-sh)
     ("shar" . application/x-shar)
     ("swf" . application/x-shockwave-flash)
     ("swfl" . application/x-shockwave-flash)
     ("scr" . application/x-silverlight)
     ("sit" . application/x-stuffit)
     ("sitx" . application/x-stuffit)
     ("sv4cpio" . application/x-sv4cpio)
     ("sv4crc" . application/x-sv4crc)
     ("tar" . application/x-tar)
     ("tcl" . application/x-tcl)
     ("gf" . application/x-tex-gf)
     ("pk" . application/x-tex-pk)
     ("texinfo" . application/x-texinfo)
     ("texi" . application/x-texinfo)
     ("~" . application/x-trash)
     ("%" . application/x-trash)
     ("bak" . application/x-trash)
     ("old" . application/x-trash)
     ("sik" . application/x-trash)
     ("t" . application/x-troff)
     ("tr" . application/x-troff)
     ("roff" . application/x-troff)
     ("man" . application/x-troff-man)
     ("me" . application/x-troff-me)
     ("ms" . application/x-troff-ms)
     ("ustar" . application/x-ustar)
     ("src" . application/x-wais-source)
     ("wz" . application/x-wingz)
     ("crt" . application/x-x509-ca-cert)
     ("xcf" . application/x-xcf)
     ("fig" . application/x-xfig)
     ("xpi" . application/x-xpinstall)
     ("amr" . audio/amr)
     ("awb" . audio/amr-wb)
     ("amr" . audio/amr)
     ("awb" . audio/amr-wb)
     ("axa" . audio/annodex)
     ("au" . audio/basic)
     ("snd" . audio/basic)
     ("flac" . audio/flac)
     ("mid" . audio/midi)
     ("midi" . audio/midi)
     ("kar" . audio/midi)
     ("mpga" . audio/mpeg)
     ("mpega" . audio/mpeg)
     ("mp2" . audio/mpeg)
     ("mp3" . audio/mpeg)
     ("m4a" . audio/mpeg)
     ("m3u" . audio/mpegurl)
     ("oga" . audio/ogg)
     ("ogg" . audio/ogg)
     ("spx" . audio/ogg)
     ("sid" . audio/prs.sid)
     ("aif" . audio/x-aiff)
     ("aiff" . audio/x-aiff)
     ("aifc" . audio/x-aiff)
     ("gsm" . audio/x-gsm)
     ("m3u" . audio/x-mpegurl)
     ("wma" . audio/x-ms-wma)
     ("wax" . audio/x-ms-wax)
     ("ra" . audio/x-pn-realaudio)
     ("rm" . audio/x-pn-realaudio)
     ("ram" . audio/x-pn-realaudio)
     ("ra" . audio/x-realaudio)
     ("pls" . audio/x-scpls)
     ("sd2" . audio/x-sd2)
     ("wav" . audio/x-wav)
     ("alc" . chemical/x-alchemy)
     ("cac" . chemical/x-cache)
     ("cache" . chemical/x-cache)
     ("csf" . chemical/x-cache-csf)
     ("cbin" . chemical/x-cactvs-binary)
     ("cascii" . chemical/x-cactvs-binary)
     ("ctab" . chemical/x-cactvs-binary)
     ("cdx" . chemical/x-cdx)
     ("cer" . chemical/x-cerius)
     ("c3d" . chemical/x-chem3d)
     ("chm" . chemical/x-chemdraw)
     ("cif" . chemical/x-cif)
     ("cmdf" . chemical/x-cmdf)
     ("cml" . chemical/x-cml)
     ("cpa" . chemical/x-compass)
     ("bsd" . chemical/x-crossfire)
     ("csml" . chemical/x-csml)
     ("csm" . chemical/x-csml)
     ("ctx" . chemical/x-ctx)
     ("cxf" . chemical/x-cxf)
     ("cef" . chemical/x-cxf)
     ("emb" . chemical/x-embl-dl-nucleotide)
     ("embl" . chemical/x-embl-dl-nucleotide)
     ("spc" . chemical/x-galactic-spc)
     ("inp" . chemical/x-gamess-input)
     ("gam" . chemical/x-gamess-input)
     ("gamin" . chemical/x-gamess-input)
     ("fch" . chemical/x-gaussian-checkpoint)
     ("fchk" . chemical/x-gaussian-checkpoint)
     ("cub" . chemical/x-gaussian-cube)
     ("gau" . chemical/x-gaussian-input)
     ("gjc" . chemical/x-gaussian-input)
     ("gjf" . chemical/x-gaussian-input)
     ("gal" . chemical/x-gaussian-log)
     ("gcg" . chemical/x-gcg8-sequence)
     ("gen" . chemical/x-genbank)
     ("hin" . chemical/x-hin)
     ("istr" . chemical/x-isostar)
     ("ist" . chemical/x-isostar)
     ("jdx" . chemical/x-jcamp-dx)
     ("dx" . chemical/x-jcamp-dx)
     ("kin" . chemical/x-kinemage)
     ("mcm" . chemical/x-macmolecule)
     ("mmd" . chemical/x-macromodel-input)
     ("mmod" . chemical/x-macromodel-input)
     ("mol" . chemical/x-mdl-molfile)
     ("rd" . chemical/x-mdl-rdfile)
     ("rxn" . chemical/x-mdl-rxnfile)
     ("sd" . chemical/x-mdl-sdfile)
     ("sdf" . chemical/x-mdl-sdfile)
     ("tgf" . chemical/x-mdl-tgf)
     ("mcif" . chemical/x-mmcif)
     ("mol2" . chemical/x-mol2)
     ("b" . chemical/x-molconn-Z)
     ("gpt" . chemical/x-mopac-graph)
     ("mop" . chemical/x-mopac-input)
     ("mopcrt" . chemical/x-mopac-input)
     ("mpc" . chemical/x-mopac-input)
     ("zmt" . chemical/x-mopac-input)
     ("moo" . chemical/x-mopac-out)
     ("mvb" . chemical/x-mopac-vib)
     ("asn" . chemical/x-ncbi-asn1)
     ("prt" . chemical/x-ncbi-asn1-ascii)
     ("ent" . chemical/x-ncbi-asn1-ascii)
     ("val" . chemical/x-ncbi-asn1-binary)
     ("aso" . chemical/x-ncbi-asn1-binary)
     ("asn" . chemical/x-ncbi-asn1-spec)
     ("pdb" . chemical/x-pdb)
     ("ent" . chemical/x-pdb)
     ("ros" . chemical/x-rosdal)
     ("sw" . chemical/x-swissprot)
     ("vms" . chemical/x-vamas-iso14976)
     ("vmd" . chemical/x-vmd)
     ("xtel" . chemical/x-xtel)
     ("xyz" . chemical/x-xyz)
     ("gif" . image/gif)
     ("ief" . image/ief)
     ("jpeg" . image/jpeg)
     ("jpg" . image/jpeg)
     ("jpe" . image/jpeg)
     ("pcx" . image/pcx)
     ("png" . image/png)
     ("svg" . image/svg+xml)
     ("svgz" . image/svg+xml)
     ("tiff" . image/tiff)
     ("tif" . image/tiff)
     ("djvu" . image/vnd.djvu)
     ("djv" . image/vnd.djvu)
     ("wbmp" . image/vnd.wap.wbmp)
     ("cr2" . image/x-canon-cr2)
     ("crw" . image/x-canon-crw)
     ("ras" . image/x-cmu-raster)
     ("cdr" . image/x-coreldraw)
     ("pat" . image/x-coreldrawpattern)
     ("cdt" . image/x-coreldrawtemplate)
     ("cpt" . image/x-corelphotopaint)
     ("erf" . image/x-epson-erf)
     ("ico" . image/x-icon)
     ("art" . image/x-jg)
     ("jng" . image/x-jng)
     ("bmp" . image/x-ms-bmp)
     ("nef" . image/x-nikon-nef)
     ("orf" . image/x-olympus-orf)
     ("psd" . image/x-photoshop)
     ("pnm" . image/x-portable-anymap)
     ("pbm" . image/x-portable-bitmap)
     ("pgm" . image/x-portable-graymap)
     ("ppm" . image/x-portable-pixmap)
     ("rgb" . image/x-rgb)
     ("xbm" . image/x-xbitmap)
     ("xpm" . image/x-xpixmap)
     ("xwd" . image/x-xwindowdump)
     ("eml" . message/rfc822)
     ("igs" . model/iges)
     ("iges" . model/iges)
     ("msh" . model/mesh)
     ("mesh" . model/mesh)
     ("silo" . model/mesh)
     ("wrl" . model/vrml)
     ("vrml" . model/vrml)
     ("x3dv" . model/x3d+vrml)
     ("x3d" . model/x3d+xml)
     ("x3db" . model/x3d+binary)
     ("manifest" . text/cache-manifest)
     ("ics" . text/calendar)
     ("icz" . text/calendar)
     ("css" . text/css)
     ("csv" . text/csv)
     ("323" . text/h323)
     ("html" . text/html)
     ("htm" . text/html)
     ("shtml" . text/html)
     ("uls" . text/iuls)
     ("mml" . text/mathml)
     ("asc" . text/plain)
     ("txt" . text/plain)
     ("text" . text/plain)
     ("pot" . text/plain)
     ("brf" . text/plain)
     ("rtx" . text/richtext)
     ("sct" . text/scriptlet)
     ("wsc" . text/scriptlet)
     ("tm" . text/texmacs)
     ("ts" . text/texmacs)
     ("tsv" . text/tab-separated-values)
     ("jad" . text/vnd.sun.j2me.app-descriptor)
     ("wml" . text/vnd.wap.wml)
     ("wmls" . text/vnd.wap.wmlscript)
     ("bib" . text/x-bibtex)
     ("boo" . text/x-boo)
     ("h++" . text/x-c++hdr)
     ("hpp" . text/x-c++hdr)
     ("hxx" . text/x-c++hdr)
     ("hh" . text/x-c++hdr)
     ("c++" . text/x-c++src)
     ("cpp" . text/x-c++src)
     ("cxx" . text/x-c++src)
     ("cc" . text/x-c++src)
     ("h" . text/x-chdr)
     ("htc" . text/x-component)
     ("csh" . text/x-csh)
     ("c" . text/x-csrc)
     ("d" . text/x-dsrc)
     ("diff" . text/x-diff)
     ("patch" . text/x-diff)
     ("hs" . text/x-haskell)
     ("java" . text/x-java)
     ("lhs" . text/x-literate-haskell)
     ("moc" . text/x-moc)
     ("p" . text/x-pascal)
     ("pas" . text/x-pascal)
     ("gcd" . text/x-pcs-gcd)
     ("pl" . text/x-perl)
     ("pm" . text/x-perl)
     ("py" . text/x-python)
     ("scala" . text/x-scala)
     ("etx" . text/x-setext)
     ("sh" . text/x-sh)
     ("tcl" . text/x-tcl)
     ("tk" . text/x-tcl)
     ("tex" . text/x-tex)
     ("ltx" . text/x-tex)
     ("sty" . text/x-tex)
     ("cls" . text/x-tex)
     ("vcs" . text/x-vcalendar)
     ("vcf" . text/x-vcard)
     ("json" . text/javascript)
     ("3gp" . video/3gpp)
     ("axv" . video/annodex)
     ("dl" . video/dl)
     ("dif" . video/dv)
     ("dv" . video/dv)
     ("fli" . video/fli)
     ("gl" . video/gl)
     ("mpeg" . video/mpeg)
     ("mpg" . video/mpeg)
     ("mpe" . video/mpeg)
     ("mp4" . video/mp4)
     ("qt" . video/quicktime)
     ("mov" . video/quicktime)
     ("ogv" . video/ogg)
     ("mxu" . video/vnd.mpegurl)
     ("flv" . video/x-flv)
     ("lsf" . video/x-la-asf)
     ("lsx" . video/x-la-asf)
     ("mng" . video/x-mng)
     ("asf" . video/x-ms-asf)
     ("asx" . video/x-ms-asf)
     ("wm" . video/x-ms-wm)
     ("wmv" . video/x-ms-wmv)
     ("wmx" . video/x-ms-wmx)
     ("wvx" . video/x-ms-wvx)
     ("avi" . video/x-msvideo)
     ("movie" . video/x-sgi-movie)
     ("mpv" . video/x-matroska)
     ("mkv" . video/x-matroska)
     ("ice" . x-conference/x-cooltalk)
     ("sisx" . x-epoc/x-sisx-app)
     ("vrm" . x-world/x-vrml)
     ("vrml" . x-world/x-vrml)
     ("wrl" . x-world/x-vrml))))

(define %file-ext-regexp
  (make-regexp "(\\.(.*)|[~%])$"))

(define (file-extension file-name)
  "Return the file extension for FILE-NAME, or #f if one is not
found."
  (and=> (regexp-exec %file-ext-regexp file-name)
         (lambda (match)
           (or (match:substring match 2)
               (match:substring match 1)))))

(define (mime-type file-name)
  "Guess the MIME type for FILE-NAME based upon its file extension."
  (or (hash-ref %mime-types (file-extension file-name))
      'text/plain))

;;; date/time

(define (now)
  (time-second (current-time)))

(define (seconds->date seconds)
  (time-utc->date (make-time time-utc 0 seconds)))

(define (format-date date)
  (date->string date "~5"))

(define format-seconds (compose format-date seconds->date))

;;; web

(define (request-path-components request)
  "Split the URI path of REQUEST into a list of component strings.  For
example: \"/foo/bar\" yields '(\"foo\" \"bar\")."
  (split-and-decode-uri-path (uri-path (request-uri request))))

(define (render-html sxml)
  (values '((content-type . (text/html)))
          (lambda (port)
            (sxml->html sxml port))))

(define (not-found uri)
  (values (build-response #:code 404)
          (string-append "Resource not found: " uri)))

(define (redirect uri)
  (values (build-response #:code 303 #:headers `((Location . ,uri))) ""))

(define (error)
  (values (build-response #:code 500)))


;;;
;;; template
;;;

(define (template body-class body)
  `((doctype "html")
    (html
     (head
      (meta (@ (charset "utf-8")))
      (title "hypermove")
      (link (@ (rel "stylesheet") (href "/static/normalize.css")))
      (link (@ (rel "stylesheet") (href "/static/main.css"))))
     (body (@ (class ,body-class))
           (img (@ (id "logo") (width "601") (height "1243") (src "static/her-ipod-style_by_aneglus_2002-2003.jpg")))
           (div (@ (id "container"))
                (div (@ (id "container"))
                     ,body)
                (footer (@ (class "text-center"))
                        (p (small "Copyright © 2016 Amirouche Boubekki"))))))))

;;;
;;; static assets rendering
;;;

(define (directory? filename)
  (string=? filename (dirname filename)))

(define (render-static-asset path)
  (let ((filename (string-join (cons* (dirname (current-filename)) "static" path) "/")))
    (if (and (file-exists? filename) (not (directory? filename)))
        (values `((content-type ,(mime-type filename)))
                (call-with-input-file filename get-bytevector-all))
        (not-found (string-join (cons "static" path) "/" 'prefix)))))


;;; route context

(define (make-context request body)
  "Create a route context"
  (let ((context `((request . ,request))))
    (let ((context (if body (acons 'POST (querystring body) context) context)))
      (if (uri-query (request-uri request))
          (acons 'GET (querystring (string->utf8 (uri-query (request-uri request)))) context)
          context))))


(define (context-post context key)
  "Get KEY from CONTEXT"
  (assoc-ref (assoc-ref context 'POST) key))

(define (context-get context key)
  "Get KEY from CONTEXT"
  (assoc-ref (assoc-ref context 'GET) key))

(define (context-method context)
  "get request method from CONTEXT"
  (request-method (assoc-ref context 'request)))

;;; db and model

(define (template:result hits)
  (match (document-ref (car hits))
    ((title snippet) `(a (@ (href ,(car hits)))
                         (p ,snippet)
                         (p (@ (class "link")) ,(car hits))))))


(define (template:index-view query hits)
  (template "index"
            `(div (form (@ (id "search") (method "GET"))
                        (input (@ (type "text") (name "query") (value ,query)))
                        (input (@ (type "submit") (value "hypermove"))))
                  (div (@ (id "hits"))
                       ,(map template:result hits)))))

(define (view:index context)
  (case (context-method context)
    ((GET) (let ((q (context-get context "query")))
             (if q 
                 (let* ((hits (search* (query (car q)))))
                   (render-html (template:index-view (car q) hits)))
                 (render-html (template:index-view "" '())))))
    (else (error))))

;;; index url

(define (remove-newlines string)
  (string-map (lambda (char) (if (equal? char #\newline) #\space char)) string))

(define (index* url)
  (let* ((response (curl url))
         (body (utf8->string (read-response-body (call-with-input-string response read-response)))))
    (let* ((string (html2text body))
           (snippet (if (< 200 (string-length string)) (remove-newlines (string-take string 200)) string)))
      (let* ((html (html->sxml body))
             (title (car ((sxpath '(// title *text*)) html))))
        (add-document url title snippet)
        (index url body)
        html))))

(define (view:add context)
  (let* ((url (car (context-get context "url"))))
    (index* url)
    (render-html (template "add" "ok"))))

;;; index domain

(define uri-domain (compose uri-host string->uri))

(define (extract-href sxml)
  (map cadr ((sxpath '(// a @ href)) sxml)))

(define (unsupported-href href)
  (not (or (string-prefix? "#" href)
           (string-prefix? "mailto:" href))))

(define (url-domain url)
  (string-take url
               (cond
                ((string-prefix? "http://" url)
                 (let ((has-slash (string-index (string-drop url 7) #\/)))
                   (if has-slash
                       (+ 7 has-slash)
                       (string-length url))))
                ((string-prefix? "https://" url)
                 (let ((has-slash (string-index (string-drop url 8) #\/)))
                   (if has-slash
                       (+ 8 has-slash)
                       (string-length url))))
                (else (string-length url)))))

(define (proprify url)
  "Remove extract / in URL path"
  (let* ((path (string-split (uri-path (string->uri url)) #\/))
         (clean (lambda (string)
                  (not (or (equal? string "") (equal? string ".")))))
         (path (filter clean path))
         (join (lambda (lst) (string-join lst "/")))
         (make-url (lambda (path)
                     (string-append (url-domain url) "/" (join path)))))
    (make-url (let loop ((path path) (out '()))
                (if (null? path)
                    (reverse out)
                    (if (equal? (car path) "..")
                        (loop (cdr path) (cdr out))
                        (loop (cdr path) (cons (car path) out))))))))

(define* ((href->url original-document-url) href)
  (proprify (cond
             ((string-prefix? "http" href) href)
             ((string-prefix? "//" href)
              (if (string-prefix? "http://" original-document-url)
                  (string-append "http:" href)
                  (string-append "https:" href)))
             ((string-prefix? "/" href)
              (string-append (url-domain original-document-url) href))
             ;; ./foo/bar/baz and foo/bar/baz
             (else
              (if (string-suffix? "/" original-document-url)
                  (string-append original-document-url href)
                  (string-append (url-domain original-document-url)
                                 "/"
                                 (dirname (uri-path (string->uri original-document-url)))
                                 "/"
                                 href))))))

(define (extract-links original-document-url html)
  (let ((hrefs (extract-href html)))
    (delete-duplicates
     (map (href->url original-document-url)
          (filter unsupported-href hrefs)))))

(define (index-domain url)
  (let loop ((urls (list url)))
    (unless (null? urls)
      (if (document-ref (car urls))
          (loop (cdr urls))
          (catch #true
            (lambda () 
              (let* ((html (index* (car urls)))
                     (links (extract-links url html))
                     (new (filter (lambda (link) (equal? (uri-domain url) (uri-domain link))) links)))
                (loop (delete-duplicates (lset-union equal? new (cdr urls))))))
            (lambda (key . args)
              (loop (cdr urls))))))))
    
(define (view:add-domain context)
  (let* ((url (car (context-get context "url"))))
    (index-domain url)
    (render-html (template "add" "ok"))))

(define (handler request body)
  (define context (make-context request body))
  (match (request-path-components request)
    (() (view:index context))
    (("add") (view:add context))
    (("add" "domain") (view:add-domain context))
    (("static" path ...) (render-static-asset path))
    (_ (render-html (template "dunno" "dunno")))))


(with-env (env-open* "/tmp/wt" *wsh*)
  (run-server handler))