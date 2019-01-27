#!/bin/sh
./bin/harriet-driver --create
./bin/harriet-driver --add-faction --account-name=root --faction-name=Republic --faction-adjective=Republican --faction-plural=Republicans --faction-color=#003153 
./bin/harriet-driver --add-faction --faction-names-file=faction-names.config --faction-colors-file=faction-colors.config
