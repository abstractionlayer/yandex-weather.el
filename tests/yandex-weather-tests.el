;;; yandex-weather-tests.el -- Regression tests.
;;
;; emacs -batch -Q -L .. -l yandex-weather-tests.el -f ert-run-tests-batch-and-exit
;;


(require 'cl)
(require 'ert)
(require 'yandex-weather)
(require 'org-yandex-weather)

(defvar yandex-weather-test-data-file "27612.xml"
  "The file with test data.")

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

(defun yandex-weather-get-data-fixture (body date format)
  (unwind-protect
      (progn
        (let ((org-yandex-weather-display-icon-p nil)
              (org-yandex-weather-location "27612")
              (org-yandex-weather-format format))
          (flet ((yandex-weather-get-data
                  (location expire-time)
                  (with-current-buffer
                      (find-file ( yandex-weather-find-file-with-test-data))
                    (let ((data (xml-parse-region (point-min)
                                                  (point-max))))
                      (kill-buffer (current-buffer))
                      data)))

                 (org-yandex-weather-check-interval
                  (date)
                  t))

            (funcall body))))))

(ert-deftest org-yandex-weather-test ()
  "Test the org entry with the weather for location."
  :tags '(yandex-weather)

  (yandex-weather-get-data-fixture
   (lambda ()
     (should
      (string-equal (org-yandex-weather)
                    "Москва:  облачно, [-1,2]°C, →4.7 746 93")))
   (list 1 15 2015)
   "%C: %i %c, [%l,%h]%s, %d%w %p %H")

  (yandex-weather-get-data-fixture
   (lambda ()
     (should
      (equal (org-yandex-weather) nil)))
   (list 1 14 2015)
   "%C: %i %c, [%l,%h]%s, %d%w %p %H")

  (yandex-weather-get-data-fixture
   (lambda ()
     (should
      (equal (org-yandex-weather) nil)))
   (list 1 25 2015)
   "%C: %i %c, [%l,%h]%s, %d%w %p %H")
  )


;;; yandex-weather-tests.el ends here
