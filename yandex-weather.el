;;; yandex-weather.el --- Fetch Yandex Weather forecasts.

;; This script based on google-weather.el originally written by Julien Danjou.
;; http://git.naquadah.org/?p=google-weather-el.git;a=summary

;; Copyright (C) 2013 Whitesquall

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

(require 'url)
(require 'url-cache)
(require 'xml)
(require 'time-date)
(require 'cl)

(defgroup yandex-weather nil
  "Yandex Weather."
  :group 'comm)

(defcustom yandex-weather-use-https nil
  "Default protocol to use to access the Yandex Weather API."
  :group 'yandex-weather
  :type 'boolean)

(defconst yandex-weather-url
  "export.yandex.ru/weather-ng/forecasts/"
  "URL of the API.")

(defconst yandex-weather-icon-url
  "yandex.st/weather/1.1.86/i/icons/22x22/"
  "URL of the icons.")

(defconst yandex-weather-temperature-symbol "°C"
  "Temperature symbol.")

(defun yandex-weather-cache-expired (url expire-time)
  "Check if URL is cached for more than EXPIRE-TIME."
  (cond (url-standalone-mode
         (not (file-exists-p (url-cache-create-filename url))))
        (t (let ((cache-time (url-is-cached url)))
             (if cache-time 
                 (time-less-p
                  (time-add 
                   cache-time
                   (seconds-to-time expire-time))
                  (current-time))
               t)))))

(defun yandex-weather-cache-fetch (url)
  "Fetch URL from the cache."
  (with-current-buffer (generate-new-buffer " *temp*")
    (url-cache-extract (url-cache-create-filename url))
    (current-buffer)))

(defun yandex-weather-retrieve-data-raw (url &optional expire-time)
  "Retrieve URL and return its data as string.
If EXPIRE-TIME is set, the data will be fetched from the cache if
their are not older than EXPIRE-TIME seconds. Otherwise, they
will be fetched and then cached. Therefore, setting EXPIRE-TIME
to 0 force a cache renewal."
  (let* ((expired (if expire-time
                      (yandex-weather-cache-expired url expire-time)
                    t))
         (buffer (if expired
                     (url-retrieve-synchronously url)
                   (yandex-weather-cache-fetch url)))
         data)
    (when (and expired expire-time)
      (url-store-in-cache buffer))
    buffer))

(defun yandex-weather-retrieve-data (url &optional expire-time)
  "Retrieve URL and return its data as string.
If EXPIRE-TIME is set, the data will be fetched from the cache if
their are not older than EXPIRE-TIME seconds. Otherwise, they
will be fetched and then cached. Therefore, setting EXPIRE-TIME
to 0 force a cache renewal."
  (with-current-buffer (yandex-weather-retrieve-data-raw
                        url expire-time)
    (goto-char (point-min))
    (unless (search-forward "\n\n" nil t)
      (error "Data not found."))
    (decode-coding-region
     (point) (point-max)
     (detect-coding-region (point) (point-max) t))
    (set-buffer-multibyte t)
    (let ((data (xml-parse-region (point) (point-max))))
      (kill-buffer (current-buffer))
      data)))

(defun yandex-weather-build-url (location)
  "Build URL to retrieve weather for LOCATION.
LOCATION can be finded http://weather.yandex.ru/static/cities.xml .
We need 'id' field in the 'city' tag."
  (concat "http" (when yandex-weather-use-https "s")
          "://" yandex-weather-url location ".xml"))

(defun yandex-weather-build-icon-url (icon-num)
  "Build URL to retrieve icon for weather."
  (concat "http" (when yandex-weather-use-https "s")
          "://" yandex-weather-icon-url icon-num ".png"))

(defun yandex-weather-get-data (location &optional expire-time)
  "Get weather data for LOCATION.
See `yandex-weather-retrieve-data' for the use of EXPIRE-TIME."
  (yandex-weather-retrieve-data
   (yandex-weather-build-url location) expire-time))

(defun yandex-weather-data->all-info (data)
  "Return all weather information from DATA."
  (cdr (assq 'forecast data)))

(defun yandex-weather-data->city (data)
  "Return the city where the DATA come from."
  (cdr (assq 'city (car (yandex-weather-data->all-info data)))))

(defun yandex-weather-data->forecasts (data)
  "Return forecasts for all days from the DATA."
  (xml-get-children (yandex-weather-data->all-info data) 'day))

(defun yandex-weather-data->forecast-by-date (data date)
  "Return the forecast of the weather for the DATA for the DATE."
  (let ((forecast-date (format "%.4d-%.2d-%.2d"
                               (nth 2 date)
                               (nth 0 date)
                               (nth 1 date)))
        (forecasts (yandex-weather-data->forecasts data))
        (retvalue nil))
    ; Now we got the formated date and forecasts for all days.
    (mapcar (lambda (x)
              (when (equal (cdr (car (car (cdr x)))) forecast-date)
                (setq retvalue x)))
            forecasts)
    retvalue))

(defun yandex-weather-forecast->day-part (forecast day-part)
  "Return required DAY-PART for the FORECAST."
  (let ((retvalue nil))
    (mapcar (lambda (x) 
              (when (equal (cdr (car (cdr (car (cdr x))))) day-part)
                (setq retvalue x)))
            (xml-get-children forecast 'day_part))
    retvalue))

(defun yandex-weather-forecast->avg-night-temperature (forecast)
  "Return the average night temperature for the FORECAST."
  (nth 2 (car (xml-get-children
               (car 
                (xml-get-children 
                 (yandex-weather-forecast->day-part forecast "night")
                 'temperature-data))
               'avg))))

(defun yandex-weather-forecast->avg-day-temperature (forecast)
  "Return the average day temperature for the FORECAST."
  (nth 2 (car (cdr (cdr (cdr (car 
   (xml-get-children 
    (yandex-weather-forecast->day-part forecast "day")
    'temperature-data))))))))

(defun yandex-weather-forecast->condition (forecast)
  "Return the condition for the FORECAST."
  (nth 2 (car
   (xml-get-children
    (yandex-weather-forecast->day-part forecast "day")
    'weather_type_short))))

(defun yandex-weather-forecast->icon (forecast)
  "Return the name of the icon for the FORECAST."
  (nth 2 (car
          (xml-get-children
           (yandex-weather-forecast->day-part forecast "day")
           'image-v2))))

(provide 'yandex-weather)

;;; yandex-weather.el ends here