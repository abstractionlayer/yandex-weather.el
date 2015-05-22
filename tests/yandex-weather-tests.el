;;; yandex-weather-tests.el -- Regression tests.

;; Copyright (C) 2013-2015 Whitesquall

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;; Commentary:

;; Tests for the package.

;;
;; emacs -batch -Q -L .. -l yandex-weather-tests.el -f ert-run-tests-batch-and-exit
;;

;;; Code:


(require 'ert)
(require 'rng-loc)
(require 'yandex-weather)
(require 'org-yandex-weather)

(defvar yandex-weather-test-data-file "27612.xml"
  "The file with test data.")

(defvar yandex-weather-test-icon-base64-data
  "iVBORw0KGgoAAAANSUhEUgAAAAMAAAACCAYAAACddGYaAAAAAXNSR0IArs4c6QAAAAZiS0dEAO8A
UQBRItXOlAAAAAlwSFlzAAALEwAACxMBAJqcGAAAAAd0SU1FB9gJDxcIBl8Z3A0AAAAZdEVYdENv
bW1lbnQAQ3JlYXRlZCB3aXRoIEdJTVBXgQ4XAAAAC0lEQVQI12NgwAUAABoAASRETuUAAAAASUVO
RK5CYII="
  "The simple png picture.")

(defvar yandex-weather-test-get-icon-header
  "HTTP/1.1 200 OK
Server: nginx/1.7.10
Date: Sun, 26 Apr 2015 09:36:35 GMT
Content-Type: image/png
Content-Length: 583
Last-Modified: Wed, 06 Feb 2013 11:20:43 GMT
Connection: keep-alive
ETag: \"51123c8b-247\"
Expires: Thu, 31 Dec 2037 23:55:55 GMT
Cache-Control: max-age=315360000
Cache-Control: public
Access-Control-Allow-Origin: *
Accept-Ranges: bytes

"
  "The header of the GET response returned `url-retrieve-synchronously'.")

(ert-deftest yandex-weather-build-url-test ()
  "Test the mail url building."
  :tags '(yandex-weather)
  (should
   (let ((yandex-weather-use-https nil))
     (string-equal
      (yandex-weather-build-url "27612")
      "http://export.yandex.ru/weather-ng/forecasts/27612.xml")))

  (should
   (let ((yandex-weather-use-https t))
     (string-equal
      (yandex-weather-build-url "27612")
      "https://export.yandex.ru/weather-ng/forecasts/27612.xml")))
  )

(ert-deftest yandex-weather-build-icon-url-test ()
  "Test the url building."
  :tags '(yandex-weather)
  (should
   (let ((yandex-weather-use-https nil))
     (string-equal
      (yandex-weather-build-icon-url "bkn_n_+2")
      "http://yandex.st/weather/1.1.86/i/icons/22x22/bkn_n_+2.png")))

  (should
   (let ((yandex-weather-use-https t))
     (string-equal
      (yandex-weather-build-icon-url "bkn_n_+2")
      "https://yandex.st/weather/1.1.86/i/icons/22x22/bkn_n_+2.png")))
  )

(defun yandex-weather-find-file-with-test-data ()
  "Find the file with xml data response from the server.
You can run ert manually or using makefile."
  (if (file-exists-p yandex-weather-test-data-file)
      yandex-weather-test-data-file
    (concat "tests/" yandex-weather-test-data-file)))

(defun yandex-weather-get-test-data ()
  "Return mocked test xml data."
  (with-current-buffer
      (yandex-weather-open-xml-file-suppress-msg-what-schema-is-used)
    (let ((data (xml-parse-region (point-min)
                                  (point-max))))
      (kill-buffer (current-buffer))
      data)))

(defun yandex-weather-open-xml-file-suppress-msg-what-schema-is-used ()
  "Open file with test data without printing message what schema is used.
Running tests looks more cleaner."
  (cl-letf (((symbol-function 'rng-what-schema)
             (lambda ()
               (interactive))))
           (find-file (yandex-weather-find-file-with-test-data))))

(defun yandex-weather-get-data-fixture (body date format data)
  (unwind-protect
      (progn
        (let ((org-yandex-weather-display-icon-p nil)
              (org-yandex-weather-location "27612")
              (org-yandex-weather-format format))
          (cl-letf (((symbol-function 'yandex-weather-get-data)
                     (lambda (location expire-time)
                       data))
                    ((symbol-function 'org-yandex-weather-check-interval)
                     (lambda (date)
                       t)))
            (funcall body))))))

(defun yandex-weather-build-server-response ()
  "Return the response of the server with the test icon data."
  (concat yandex-weather-test-get-icon-header
          (base64-decode-string yandex-weather-test-icon-base64-data)))

(defun yandex-weather-get-icon-fixture (body date format data)
  (unwind-protect
      (progn
        (let ((org-yandex-weather-display-icon-p t)
              (org-yandex-weather-format format))
          (cl-letf (((symbol-function 'url-retrieve-synchronously)
                     (lambda (url)
                       (let ((buffer nil))
                         (switch-to-buffer " *tmp*")
                         (insert (yandex-weather-build-server-response))
                         (setq buffer (current-buffer))
                         buffer)))
                    ((symbol-function 'yandex-weather-get-data)
                     (lambda (location expire-time)
                       data))
                    ((symbol-function 'org-yandex-weather-check-interval)
                     (lambda (date)
                       t)))
            (funcall body))))))

(defun yandex-weather-extract-icon-data-from-propertized-string (string)
  (plist-get (cdr (get-text-property 0 'display string))
             :data))

(ert-deftest org-yandex-weather-test ()
  "Test the org entry with the weather for location."
  :tags '(yandex-weather)
  (let ((data (yandex-weather-get-test-data)))
    (yandex-weather-get-data-fixture
     (lambda ()
       (should
        (string-equal (org-yandex-weather)
                      "Москва:  облачно, [-1,2]°C, →4.7 746 93")))
     (list 1 15 2015)
     "%C: %i %c, [%l,%h]%s, %d%w %p %H"
     data)

    (yandex-weather-get-data-fixture
     (lambda ()
       (should
        (equal (org-yandex-weather) nil)))
     (list 1 14 2015)
     "%C: %i %c, [%l,%h]%s, %d%w %p %H"
     data)

    (yandex-weather-get-data-fixture
     (lambda ()
       (should
        (equal (org-yandex-weather) nil)))
     (list 1 25 2015)
     "%C: %i %c, [%l,%h]%s, %d%w %p %H"
     data)))

(ert-deftest org-yandex-weather-icon-data-test ()
  "Test the icon in the forecast."
  :tags '(yandex-weather)
  (let ((data (yandex-weather-get-test-data)))
    (yandex-weather-get-icon-fixture
     (lambda ()
       (should
        (string-equal
         (yandex-weather-extract-icon-data-from-propertized-string
          (org-yandex-weather))
         (base64-decode-string yandex-weather-test-icon-base64-data))))
     (list 1 15 2015)
     "%i"
     data)))

(ert-deftest yandex-weather-forecast->icon-name-test ()
  "Test the icon name from the forecast data."
  :tags '(yandex-weather)
  (let ((data (yandex-weather-get-test-data)))
    (should
     (string-equal
      (yandex-weather-forecast->icon
       (yandex-weather-data->forecast-by-date data (list 1 15 2015)))
      "ovc_+2"))

    (should-not
     (yandex-weather-forecast->icon
      (yandex-weather-data->forecast-by-date data (list 1 14 2015))))
    ))

(ert-deftest yandex-weather-forecast->wind-direction-test ()
  "Test the wind direction from the forecast data."
  :tags '(yandex-weather)
  (let ((data (yandex-weather-get-test-data)))
    (should
     (string-equal
      (yandex-weather-forecast->wind-direction
       (yandex-weather-data->forecast-by-date data (list 1 15 2015)))
      "w"))

    (should-not
     (yandex-weather-forecast->wind-direction
      (yandex-weather-data->forecast-by-date data (list 1 14 2015))))
    ))

(ert-deftest yandex-weather-forecast->wind-speed-test ()
  "Test the wind speed from the forecast data."
  :tags '(yandex-weather)
  (let ((data (yandex-weather-get-test-data)))
    (should
     (string-equal
      (yandex-weather-forecast->wind-speed
       (yandex-weather-data->forecast-by-date data (list 1 15 2015)))
      "4.7"))

    (should-not
     (yandex-weather-forecast->wind-direction
      (yandex-weather-data->forecast-by-date data (list 1 14 2015))))
    ))

(ert-deftest yandex-weather-forecast->pressure-test ()
 "Test the pressure from the forecast data."
 :tags '(yandex-weather)
 (let ((data (yandex-weather-get-test-data)))
   (should
    (string-equal
     (yandex-weather-forecast->pressure
      (yandex-weather-data->forecast-by-date data (list 1 15 2015)))
     "746"))

   (should-not
    (yandex-weather-forecast->pressure
     (yandex-weather-data->forecast-by-date data (list 1 14 2015))))
   ))

(ert-deftest yandex-weather-forecast->humidity-test ()
 "Test the humidity from the forecast data."
 :tags '(yandex-weather)
 (let ((data (yandex-weather-get-test-data)))
   (should
    (string-equal
     (yandex-weather-forecast->humidity
      (yandex-weather-data->forecast-by-date data (list 1 15 2015)))
     "93"))

   (should-not
    (yandex-weather-forecast->humidity
     (yandex-weather-data->forecast-by-date data (list 1 14 2015))))
   ))

(ert-deftest yandex-weather-forecast->condition-test ()
 "Test the condition from the forecast data."
 :tags '(yandex-weather)
 (let ((data (yandex-weather-get-test-data)))
   (should
    (string-equal
     (yandex-weather-forecast->condition
      (yandex-weather-data->forecast-by-date data (list 1 15 2015)))
     "облачно"))

   (should-not
    (yandex-weather-forecast->condition
     (yandex-weather-data->forecast-by-date data (list 1 14 2015))))
   ))

(ert-deftest yandex-weather-forecast->avg-night-temperature-test ()
 "Test the average night temperature from the forecast data."
 :tags '(yandex-weather)
 (let ((data (yandex-weather-get-test-data)))
   (should
    (string-equal
     (yandex-weather-forecast->avg-night-temperature
      (yandex-weather-data->forecast-by-date data (list 1 15 2015)))
     "-1"))

   (should-not
    (yandex-weather-forecast->avg-night-temperature
     (yandex-weather-data->forecast-by-date data (list 1 14 2015))))
   ))

(ert-deftest yandex-weather-forecast->avg-day-temperature-test ()
 "Test the average day temperature from the forecast data."
 :tags '(yandex-weather)
 (let ((data (yandex-weather-get-test-data)))
   (should
    (string-equal
     (yandex-weather-forecast->avg-day-temperature
      (yandex-weather-data->forecast-by-date data (list 1 15 2015)))
     "2"))

   (should-not
    (yandex-weather-forecast->avg-day-temperature
     (yandex-weather-data->forecast-by-date data (list 1 14 2015))))
   ))


;;; yandex-weather-tests.el ends here
