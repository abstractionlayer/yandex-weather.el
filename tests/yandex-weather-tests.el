;;; yandex-weather-tests.el -- Regression tests.

;; Copyright (C) 2013-2016 Whitesquall

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
  "The simplest PNG picture.")

(ert-deftest yandex-weather-build-forecast-url-test ()
  "Test the mail url building."
  :tags '(yandex-weather)
  (should
   (let ((yandex-weather-use-https nil))
     (string-equal
      (yandex-weather-build-forecast-url "27612")
      "http://export.yandex.ru/weather-ng/forecasts/27612.xml")))

  (should
   (let ((yandex-weather-use-https t))
     (string-equal
      (yandex-weather-build-forecast-url "27612")
      "https://export.yandex.ru/weather-ng/forecasts/27612.xml")))
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

(defun yandex-weather-get-icon-fixture (body date format data)
  (unwind-protect
      (progn
        (let ((org-yandex-weather-display-icon-p t)
              (org-yandex-weather-format format))
          (cl-letf (((symbol-function 'find-file-noselect)
                     (lambda (file-path warn rawfile)
                       (let ((buffer nil))
                         (generate-new-buffer
                          (generate-new-buffer-name " *tmp*"))
                         (insert (base64-decode-string
                                  yandex-weather-test-icon-base64-data))
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
                      "Москва:  ясно, [13,23]°C, ↓3.5 758 42")))
     (list 8 21 2015)
     "%C: %i %c, [%l,%h]%s, %d%w %p %H"
     data)

    (yandex-weather-get-data-fixture
     (lambda ()
       (should
        (equal (org-yandex-weather) nil)))
     (list 8 20 2015)
     "%C: %i %c, [%l,%h]%s, %d%w %p %H"
     data)

    (yandex-weather-get-data-fixture
     (lambda ()
       (should
        (equal (org-yandex-weather) nil)))
     (list 9 1 2015)
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
     (list 8 21 2015)
     "%i"
     data)))

(defun org-yandex-weather-check-interval-fixture (body)
  (unwind-protect
      (progn
        (cl-letf (((symbol-function 'current-time)
                   (lambda ()
                     '(21861 31875 628007 217000))))
          (funcall body)))))

(ert-deftest org-yandex-weather-check-interval-test ()
  "Test interval of dates for forecasts. The function should return t
if DATE between current day and current day plus 10 days. Else return nil."
  :tags '( yandex-weather)
  (org-yandex-weather-check-interval-fixture
   (lambda ()
     (should
      (equal (org-yandex-weather-check-interval (list 5 27 2015)) t))

     (should-not
      (org-yandex-weather-check-interval (list 5 26 2015)))

     (should
      (equal (org-yandex-weather-check-interval (list 5 31 2015)) t))

     (should
      (equal (org-yandex-weather-check-interval (list 6 5 2015)) t))

     (should-not
      (org-yandex-weather-check-interval (list 6 6 2015)))
     )))

(ert-deftest yandex-weather-forecast->icon-name-test ()
  "Test the icon name from the forecast data."
  :tags '(yandex-weather)
  (let ((data (yandex-weather-get-test-data)))
    (should
     (string-equal
      (yandex-weather-forecast->icon
       (yandex-weather-data->forecast-by-date data (list 8 21 2015)))
      "skc_d_+24"))

    (should-not
     (yandex-weather-forecast->icon
      (yandex-weather-data->forecast-by-date data (list 8 20 2015))))
    ))

(ert-deftest yandex-weather-forecast->wind-direction-test ()
  "Test the wind direction from the forecast data."
  :tags '(yandex-weather)
  (let ((data (yandex-weather-get-test-data)))
    (should
     (string-equal
      (yandex-weather-forecast->wind-direction
       (yandex-weather-data->forecast-by-date data (list 8 21 2015)))
      "n"))

    (should-not
     (yandex-weather-forecast->wind-direction
      (yandex-weather-data->forecast-by-date data (list 8 20 2015))))
    ))

(ert-deftest yandex-weather-forecast->wind-speed-test ()
  "Test the wind speed from the forecast data."
  :tags '(yandex-weather)
  (let ((data (yandex-weather-get-test-data)))
    (should
     (string-equal
      (yandex-weather-forecast->wind-speed
       (yandex-weather-data->forecast-by-date data (list 8 21 2015)))
      "3.5"))

    (should-not
     (yandex-weather-forecast->wind-direction
      (yandex-weather-data->forecast-by-date data (list 8 20 2015))))
    ))

(ert-deftest yandex-weather-forecast->pressure-test ()
 "Test the pressure from the forecast data."
 :tags '(yandex-weather)
 (let ((data (yandex-weather-get-test-data)))
   (should
    (string-equal
     (yandex-weather-forecast->pressure
      (yandex-weather-data->forecast-by-date data (list 8 21 2015)))
     "758"))

   (should-not
    (yandex-weather-forecast->pressure
     (yandex-weather-data->forecast-by-date data (list 8 20 2015))))
   ))

(ert-deftest yandex-weather-forecast->humidity-test ()
 "Test the humidity from the forecast data."
 :tags '(yandex-weather)
 (let ((data (yandex-weather-get-test-data)))
   (should
    (string-equal
     (yandex-weather-forecast->humidity
      (yandex-weather-data->forecast-by-date data (list 8 21 2015)))
     "42"))

   (should-not
    (yandex-weather-forecast->humidity
     (yandex-weather-data->forecast-by-date data (list 8 20 2015))))
   ))

(ert-deftest yandex-weather-forecast->condition-test ()
 "Test the condition from the forecast data."
 :tags '(yandex-weather)
 (let ((data (yandex-weather-get-test-data)))
   (should
    (string-equal
     (yandex-weather-forecast->condition
      (yandex-weather-data->forecast-by-date data (list 8 21 2015)))
     "ясно"))

   (should-not
    (yandex-weather-forecast->condition
     (yandex-weather-data->forecast-by-date data (list 8 20 2015))))
   ))

(ert-deftest yandex-weather-forecast->avg-night-temperature-test ()
 "Test the average night temperature from the forecast data."
 :tags '(yandex-weather)
 (let ((data (yandex-weather-get-test-data)))
   (should
    (string-equal
     (yandex-weather-forecast->avg-night-temperature
      (yandex-weather-data->forecast-by-date data (list 8 21 2015)))
     "13"))

   (should-not
    (yandex-weather-forecast->avg-night-temperature
     (yandex-weather-data->forecast-by-date data (list 8 20 2015))))
   ))

(ert-deftest yandex-weather-forecast->avg-day-temperature-test ()
 "Test the average day temperature from the forecast data."
 :tags '(yandex-weather)
 (let ((data (yandex-weather-get-test-data)))
   (should
    (string-equal
     (yandex-weather-forecast->avg-day-temperature
      (yandex-weather-data->forecast-by-date data (list 8 21 2015)))
     "23"))

   (should-not
    (yandex-weather-forecast->avg-day-temperature
     (yandex-weather-data->forecast-by-date data (list 8 20 2015))))
   ))


;;; yandex-weather-tests.el ends here
