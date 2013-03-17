;;; org-yandex-weather.el -- Show Yandex Weather forecasts in Org Agenda.

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

(require 'yandex-weather)
(require 'image)
(require 'format-spec)
(require 'solar)
(require 'parse-time)

(defgroup org-yandex-weather nil
  "Yandex Weather for Org mode."
  :group 'comm
  :group 'org)

(defcustom org-yandex-weather-location "27612"
  "Default location for org-yandex-weather."
  :group 'org-yandex-weather)

(defcustom org-yandex-weather-format "%C: %i %c, [%l,%h]%s"
  "String to return to describe the weather.
Valid %-sequences are:
   - %i the icon;
   - %c means the weather condition;
   - %L the supplied location;
   - %C the city the weather is for;
   - %l the lower temperature;
   - %h the higher temperature;
   - %s the temperature unit symbol.")

(defcustom org-yandex-weather-cache-time 7200
  "Define how many seconds we should cache the weather."
  :group 'org-yandex-weather)

(defcustom org-yandex-weather-display-icon-p t
  "Display icons."
  :group 'org-yandex-weather)

(defun org-yandex-weather-get-icon (url)
  (with-current-buffer
      (yandex-weather-retrieve-data-raw url org-yandex-weather-cache-time)
    (goto-char (point-min))
    (unless (search-forward "\n\n" nil t)
      (error "Data not found."))     
    (set-buffer-multibyte nil)
    (let ((data (buffer-substring (point) (point-max))))
      (kill-buffer (current-buffer))
      data)))

(defun org-yandex-weather-check-interval (date)
  "Return t if DATE places between current day and current day 
plus 10 days. Else return nil."
  (let* ((low-days (time-to-days (current-time)))
         (high-days (+ low-days 10))
         (days-of-date 
          (calendar-absolute-from-gregorian
           date)))
    (and
     (>= days-of-date low-days)
     (< days-of-date high-days))))

;;;###autoload
(defun org-yandex-weather (&optional location)
  "Return Org entry with the weather for LOCATION.
If LOCATION isn't set, use org-yandex-weather-location."
  (when (org-yandex-weather-check-interval date)
    (let* ((location (or location org-yandex-weather-location))
           (data (ignore-errors
                   (yandex-weather-get-data location 
                                            org-yandex-weather-cache-time)))
           (forecast (when data
                       (yandex-weather-data->forecast-by-date data date))))
      (when forecast
        (let ((condition (yandex-weather-forecast->condition forecast))
              (low (yandex-weather-forecast->avg-night-temperature 
                    forecast))
              (high (yandex-weather-forecast->avg-day-temperature
                     forecast))
              (city (yandex-weather-data->city data))
              (icon 
               (when org-yandex-weather-display-icon-p
                 (create-image 
                  (org-yandex-weather-get-icon
                   (yandex-weather-build-icon-url
                    (yandex-weather-forecast->icon forecast)))
                  'png t))))
          (format-spec org-yandex-weather-format
                       `((?i . ,(if icon
                                    (propertize "icon"
                                                'display
                                                (append
                                                 icon '(:ascent center))
                                                'rear-nonsticky '(display))
                                  ""))
                         (?c . ,condition)
                         (?l . ,low)
                         (?h . ,high)
                         (?C . ,city)
                         (?s . ,yandex-weather-temperature-symbol))))))))
  
(provide 'org-yandex-weather)

;;; org-yandex-weather.el ends here