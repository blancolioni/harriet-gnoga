#!/bin/sh
./bin/harriet-driver --create
./bin/harriet-driver --add-faction --account-name=root --faction-name=Republic --faction-adjective=Republican --faction-plural=Republicans --faction-color=#003153 --faction-setup-path=capital
./bin/harriet-driver --add-faction --faction-names-file=faction-names.config --faction-colors-file=faction-colors.config
echo Saving initial state
cp harriet.db.marlowe harriet.db.init.marlowe