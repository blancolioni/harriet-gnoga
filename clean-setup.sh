#!/bin/sh
./bin/harriet-driver.exe --create
./bin/harriet-driver.exe --add-faction --account-name=root --faction-name=Republic --faction-adjective=Republican --faction-plural=Republicans --faction-color=#003153 
./bin/harriet-driver.exe --add-faction --faction-names-file=faction-names.config --faction-colors-file=faction-colors.config
