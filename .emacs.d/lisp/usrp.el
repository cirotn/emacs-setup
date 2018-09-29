(require 'ido)

(defun uhd-image-loader ()
  "Inserts UHD image loader command line."
  (interactive)
  (let* ((type (read-string "type: " "x300" nil nil))
         (addr (read-string "addr: " "192.168.40.2" nil nil))
         (file (ido-read-file-name "bitfile: "))
         (buffer-name "uhd_image_loader")
         (command (concat "uhd_image_loader --args=\"type=" type ",addr=" addr "\" --fpga-path=" file))
         )
    (insert command)))

;; uhd_image_loader --args="type=x300, addr=192.168.40.2" --fpga-path=~/fpgadev/usrp3/top/x300/build/usrp_x310_fpga_RFNOC_HG.bit

(defun reset-x300 ()
  "Inserts reset usrp command line."
  (interactive)
  (let* ((root-path (ido-read-directory-name "uhddev path: " "~/uhddev" ))
         (addr (read-string "addr: " "192.168.40.2" nil nil))
         (command (concat "python " root-path "/firmware/usrp3/x300/x300_debug.py --addr="addr " --poke=0x100058 --data=1"))
         )
    (insert command)))

;; python ~/uhddev/firmware/usrp3/x300/x300_debug.py --addr=192.168.40.2 --poke=0x100058 --data=1

(defun uhd-image-builder()
  "Inserts uhd_image_builder command line."
  (interactive)
  (let* ((root-path (ido-read-directory-name "fpgadev path: " "~/fpgadev" ))
         (yaml-path (ido-read-file-name "yaml path: " "~"))
         (device (read-string "device: " "x310" nil nil))
         (target (read-string "target: " "X310_RFNOC_HG" nil nil))
         )
    (insert (concat root-path "/usrp3/tools/scripts/uhd_image_builder.py -y " yaml-path " -d " device " -t " target))))

;; ~/fpgadev/usrp3/tools/scripts/uhd_image_builder.py -y ~/ciro.yml -d x310 -t X310_RFNOC_HG

(provide 'usrp)
